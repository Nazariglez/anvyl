use super::rir::{
    RirCallArg, RirCallTarget, RirCellRef, RirCellStorage, RirCollectionLoanScope,
    RirCollectionRootKind, RirExternKind, RirFunction, RirLambdaStorage, RirMutPlaceArg,
    RirOperand, RirParamAbi, RirPlace, RirPlaceRoot, RirProgram, RirProjection, RirRValue, RirStmt,
    RirStringifyReqKind, RirStruct, RirStructuredBlock, RirType, RirTypeId,
};

pub(super) fn fallible_functions(program: &RirProgram) -> Vec<bool> {
    let mut fallible = vec![false; program.functions.len()];
    for global in &program.globals {
        if let Some(slot) = fallible.get_mut(global.init.index()) {
            *slot = true;
        }
    }
    loop {
        let mut changed = false;
        for function in &program.functions {
            let is_fallible = block_calls_fallible(program, &fallible, function, &function.body);
            let slot = &mut fallible[function.id.index()];
            if is_fallible && !*slot {
                *slot = true;
                changed = true;
            }
        }
        if !changed {
            return fallible;
        }
    }
}

fn block_calls_fallible(
    program: &RirProgram,
    fallible: &[bool],
    function: &RirFunction,
    block: &RirStructuredBlock,
) -> bool {
    block
        .stmts
        .iter()
        .any(|stmt| stmt_calls_fallible(program, fallible, function, stmt))
        || term_calls_fallible(program, function, &block.term)
}

fn stmt_calls_fallible(
    program: &RirProgram,
    fallible: &[bool],
    function: &RirFunction,
    stmt: &RirStmt,
) -> bool {
    match stmt {
        RirStmt::Init { value, .. } | RirStmt::CellInit { value, .. } | RirStmt::Eval(value) => {
            rvalue_calls_fallible(program, fallible, function, value)
        }
        RirStmt::GlobalEnsure { .. } => true,
        RirStmt::GlobalSetRoot { .. } => true,
        RirStmt::Assign { dst, value } => {
            place_is_mut_place_param(function, dst)
                || place_has_indexed_collection_write(program, function, dst)
                || program.collection_replace_ty(dst.ty)
                || rvalue_calls_fallible(program, fallible, function, value)
        }
        RirStmt::CellSet { .. } | RirStmt::ScopedPlaceCellSet { .. } => true,
        RirStmt::CollectionLoanScope(scope) => {
            block_calls_fallible(program, fallible, function, &scope.body)
                || loan_scope_is_fallible(program, scope)
        }
        RirStmt::DataRefSet { object, value, .. } => {
            operand_uses_mut_place_param(function, object)
                || operand_uses_mut_place_param(function, value)
                || operand_ty(program, value).is_some_and(|ty| program.collection_replace_ty(ty))
        }
        RirStmt::MapValueSet { .. } => true,
        _ => stmt_child_blocks_any(stmt, |block| {
            block_calls_fallible(program, fallible, function, block)
        }),
    }
}

fn term_calls_fallible(
    program: &RirProgram,
    function: &RirFunction,
    term: &super::rir::RirTerm,
) -> bool {
    match term {
        super::rir::RirTerm::Return(Some(operand)) => {
            operand_has_fallible_place(program, function, operand)
        }
        super::rir::RirTerm::None
        | super::rir::RirTerm::Return(None)
        | super::rir::RirTerm::Break(_)
        | super::rir::RirTerm::Continue(_)
        | super::rir::RirTerm::Unreachable => false,
    }
}

fn rvalue_calls_fallible(
    program: &RirProgram,
    fallible: &[bool],
    function: &RirFunction,
    value: &RirRValue,
) -> bool {
    rvalue_uses_mut_place_param(function, value)
        || rvalue_uses_fallible_place(program, function, value)
        || match value {
            RirRValue::ListPush { .. }
            | RirRValue::RangeListCopy { .. }
            | RirRValue::MapGet { .. }
            | RirRValue::MapInsert { .. }
            | RirRValue::MapRemove { .. }
            | RirRValue::MapEntryAt { .. }
            | RirRValue::MapValueAt { .. }
            | RirRValue::SliceView { .. } => true,
            RirRValue::Call { callee, args, .. } => {
                args.iter()
                    .any(|arg| call_arg_preparation_fallible(program, arg))
                    || match callee {
                        RirCallTarget::Function(id) => fallible[id.index()],
                        RirCallTarget::Extern(id) => match &program.externs[id.index()].kind {
                            RirExternKind::Native(native) => native.abi.fallible,
                        },
                        RirCallTarget::LambdaValue { sig, .. } => program
                            .lambdas_for_sig(*sig)
                            .any(|lambda| fallible[lambda.function.index()]),
                    }
            }
            RirRValue::CellGetCopy { .. } | RirRValue::ScopedPlaceCellGet { .. } => true,
            RirRValue::Stringify { source_ty, .. } => {
                stringify_calls_fallible(program, fallible, *source_ty)
            }
            _ => false,
        }
}

fn rvalue_uses_fallible_place(
    program: &RirProgram,
    function: &RirFunction,
    value: &RirRValue,
) -> bool {
    match value {
        RirRValue::Use(operand)
        | RirRValue::Unary { value: operand, .. }
        | RirRValue::Cast { value: operand, .. }
        | RirRValue::OptionalSome { value: operand, .. }
        | RirRValue::Stringify { value: operand, .. }
        | RirRValue::Format { value: operand, .. }
        | RirRValue::ListPush { value: operand, .. }
        | RirRValue::MapGet { key: operand, .. }
        | RirRValue::MapRemove { key: operand, .. } => {
            operand_has_fallible_place(program, function, operand)
        }
        RirRValue::Binary { lhs, rhs, .. } | RirRValue::SharedRefEq { lhs, rhs, .. } => {
            operand_has_fallible_place(program, function, lhs)
                || operand_has_fallible_place(program, function, rhs)
        }
        RirRValue::Struct { fields, .. }
        | RirRValue::Tuple { fields, .. }
        | RirRValue::DataRefAlloc { fields, .. }
        | RirRValue::Array { elems: fields, .. }
        | RirRValue::List { elems: fields, .. }
        | RirRValue::EnumVariant { fields, .. }
        | RirRValue::StringConcat { parts: fields } => fields
            .iter()
            .any(|operand| operand_has_fallible_place(program, function, operand)),
        RirRValue::Map { entries, .. } => entries.iter().any(|(key, value)| {
            operand_has_fallible_place(program, function, key)
                || operand_has_fallible_place(program, function, value)
        }),
        RirRValue::MapInsert { key, value, .. } => {
            operand_has_fallible_place(program, function, key)
                || operand_has_fallible_place(program, function, value)
        }
        RirRValue::Call { args, .. } => args
            .iter()
            .any(|arg| call_arg_has_fallible_place(program, function, arg)),
        RirRValue::Lambda { captures, .. } => captures.iter().any(|capture| match capture {
            super::rir::RirLambdaCaptureArg::Readonly { value } => {
                operand_has_fallible_place(program, function, value)
            }
            super::rir::RirLambdaCaptureArg::Scoped { place } => {
                place_has_fallible_projection(program, function, place)
            }
            super::rir::RirLambdaCaptureArg::StackCell { .. }
            | super::rir::RirLambdaCaptureArg::HeapCell { .. }
            | super::rir::RirLambdaCaptureArg::ScopedPlaceCell { .. } => false,
        }),
        RirRValue::DataRefGet { object, .. } => {
            operand_has_fallible_place(program, function, object)
        }
        RirRValue::Len { source }
        | RirRValue::SliceView { source, .. }
        | RirRValue::RangeListCopy { source, .. } => {
            place_has_fallible_projection(program, function, source)
        }
        RirRValue::MapEntryAt { map, .. } | RirRValue::MapValueAt { map, .. } => {
            place_has_fallible_projection(program, function, map)
        }
        RirRValue::CellGetCopy { .. } | RirRValue::ScopedPlaceCellGet { .. } => false,
    }
}

fn operand_has_fallible_place(
    program: &RirProgram,
    function: &RirFunction,
    operand: &RirOperand,
) -> bool {
    match operand {
        RirOperand::Place(place) => {
            matches!(place.root, RirPlaceRoot::Global(_))
                || place_has_fallible_projection(program, function, place)
        }
        RirOperand::Const(_) => false,
    }
}

fn call_arg_has_fallible_place(
    program: &RirProgram,
    function: &RirFunction,
    arg: &RirCallArg,
) -> bool {
    match arg {
        RirCallArg::Value(operand)
        | RirCallArg::ScopedLambda {
            callee: operand, ..
        } => operand_has_fallible_place(program, function, operand),
        RirCallArg::SharedBorrow(place) | RirCallArg::MutBorrow(place) => {
            place_has_fallible_projection(program, function, place)
        }
        RirCallArg::SharedStringConst(_) | RirCallArg::MutPlace(_) => false,
    }
}

fn place_has_indexed_collection_write(
    program: &RirProgram,
    function: &RirFunction,
    place: &RirPlace,
) -> bool {
    projection_facts(program, function, place).is_some_and(|facts| facts.indexed_collection_write)
}

fn place_has_fallible_projection(
    program: &RirProgram,
    function: &RirFunction,
    place: &RirPlace,
) -> bool {
    projection_facts(program, function, place).is_some_and(|facts| facts.fallible_projection)
}

struct ProjectionFacts {
    fallible_projection: bool,
    indexed_collection_write: bool,
}

fn projection_facts(
    program: &RirProgram,
    function: &RirFunction,
    place: &RirPlace,
) -> Option<ProjectionFacts> {
    let RirPlaceRoot::Local(local) = place.root else {
        return None;
    };
    let mut ty = function.locals.get(local.index())?.ty;
    let mut facts = ProjectionFacts {
        fallible_projection: false,
        indexed_collection_write: false,
    };
    for projection in &place.projections {
        match projection {
            RirProjection::Field(field) => {
                let RirType::Struct(id) = program.types.get(ty.index())? else {
                    return None;
                };
                ty = program
                    .structs
                    .get(id.index())?
                    .fields
                    .get(field.index())?
                    .ty;
            }
            RirProjection::TupleField(field) => {
                let RirType::Tuple(id) = program.types.get(ty.index())? else {
                    return None;
                };
                ty = program
                    .tuples
                    .get(id.index())?
                    .fields
                    .get(field.index())?
                    .ty;
            }
            RirProjection::Index(_) => match program.types.get(ty.index())? {
                RirType::Array { elem, .. } => ty = *elem,
                RirType::List(elem) | RirType::Slice(elem) => {
                    facts.fallible_projection = true;
                    facts.indexed_collection_write = true;
                    ty = *elem;
                }
                _ => return None,
            },
            RirProjection::MapIndex(_) => {
                let RirType::Map { value, .. } = program.types.get(ty.index())? else {
                    return None;
                };
                facts.fallible_projection = true;
                facts.indexed_collection_write = true;
                ty = program
                    .types
                    .iter()
                    .position(|ty| matches!(ty, RirType::Option(inner) if inner == value))
                    .map(RirTypeId::from_index)?;
            }
        }
    }
    Some(facts)
}

fn operand_ty(program: &RirProgram, operand: &RirOperand) -> Option<RirTypeId> {
    match operand {
        RirOperand::Place(place) => Some(place.ty),
        RirOperand::Const(id) => program.consts.get(id.index()).map(|konst| konst.ty),
    }
}

fn loan_scope_is_fallible(program: &RirProgram, scope: &RirCollectionLoanScope) -> bool {
    matches!(
        (scope.root_kind, program.types[scope.root.ty.index()]),
        (RirCollectionRootKind::List, RirType::List(_))
            | (RirCollectionRootKind::Map, RirType::Map { .. })
    )
}

fn stringify_calls_fallible(program: &RirProgram, fallible: &[bool], ty: RirTypeId) -> bool {
    program
        .stringify_reqs
        .iter()
        .find(|req| req.ty == ty)
        .is_some_and(|req| match req.kind {
            RirStringifyReqKind::Override { function, .. } => fallible[function.index()],
            RirStringifyReqKind::Structural(_) => false,
        })
}

#[derive(Debug, Clone, Copy, Default)]
pub(super) struct ContextUse {
    pub rt: bool,
    pub types: bool,
    pub globals: bool,
}

impl ContextUse {
    fn union(self, other: Self) -> Self {
        Self {
            rt: self.rt || other.rt,
            types: self.types || other.types,
            globals: self.globals || other.globals,
        }
    }

    fn rt() -> Self {
        Self {
            rt: true,
            ..Self::default()
        }
    }

    fn rt_types() -> Self {
        Self {
            rt: true,
            types: true,
            ..Self::default()
        }
    }

    fn globals() -> Self {
        Self {
            globals: true,
            ..Self::default()
        }
    }

    fn generated_call() -> Self {
        Self {
            rt: true,
            types: true,
            globals: true,
        }
    }
}

pub(super) fn function_context_use(program: &RirProgram, function: &RirFunction) -> ContextUse {
    block_context_use(program, function, &function.body)
}

pub(super) fn stringify_helper_context_use(program: &RirProgram, strukt: &RirStruct) -> ContextUse {
    if strukt
        .fields
        .iter()
        .any(|field| matches!(program.types[field.ty.index()], RirType::Struct(_)))
    {
        ContextUse::rt_types()
    } else {
        ContextUse::default()
    }
}

fn block_context_use(
    program: &RirProgram,
    function: &RirFunction,
    block: &RirStructuredBlock,
) -> ContextUse {
    let stmts = block
        .stmts
        .iter()
        .fold(ContextUse::default(), |uses, stmt| {
            uses.union(stmt_context_use(program, function, stmt))
        });
    stmts.union(term_context_use(program, function, &block.term))
}

fn stmt_context_use(program: &RirProgram, function: &RirFunction, stmt: &RirStmt) -> ContextUse {
    match stmt {
        RirStmt::Init { value, .. } | RirStmt::Eval(value) => {
            rvalue_context_use(program, function, value)
        }
        RirStmt::GlobalEnsure { .. } => ContextUse::generated_call(),
        RirStmt::GlobalSetRoot { value, .. } => {
            ContextUse::globals().union(rvalue_context_use(program, function, value))
        }
        RirStmt::CellInit { cell, value } | RirStmt::CellSet { cell, value } => {
            cell_context_use(program, *cell).union(rvalue_context_use(program, function, value))
        }
        RirStmt::Assign { dst, value } => place_context_use(program, function, dst)
            .union(rvalue_context_use(program, function, value)),
        RirStmt::ScopedPlaceCellSet { value, .. } => {
            ContextUse::rt().union(rvalue_context_use(program, function, value))
        }
        RirStmt::DataRefSet { object, value, .. } => ContextUse::rt_types()
            .union(operand_context_use(program, function, object))
            .union(operand_context_use(program, function, value)),
        RirStmt::MapValueSet { .. } => ContextUse::rt(),
        RirStmt::CollectionLoanScope(scope) => {
            let root = if place_is_mut_place_param(function, &scope.root) {
                ContextUse::rt()
            } else {
                ContextUse::default()
            };
            root.union(block_context_use(program, function, &scope.body))
        }
        RirStmt::CollectionSlotScope(block) => block_context_use(program, function, block),
        _ => stmt_child_blocks_context_use(program, function, stmt),
    }
}

fn stmt_child_blocks_context_use(
    program: &RirProgram,
    function: &RirFunction,
    stmt: &RirStmt,
) -> ContextUse {
    let mut uses = ContextUse::default();
    let mut merge = |block: &RirStructuredBlock| {
        uses = uses.union(block_context_use(program, function, block));
    };
    stmt_child_blocks_any(stmt, |block| {
        merge(block);
        false
    });
    uses
}

fn rvalue_context_use(
    program: &RirProgram,
    function: &RirFunction,
    value: &RirRValue,
) -> ContextUse {
    let mut uses = if rvalue_uses_mut_place_param(function, value) {
        ContextUse::rt()
    } else {
        ContextUse::default()
    };
    uses = uses.union(rvalue_operand_context_use(program, function, value));
    match value {
        RirRValue::Call { callee, args, .. } => {
            uses = uses.union(match callee {
                RirCallTarget::Function(_) | RirCallTarget::LambdaValue { .. } => {
                    ContextUse::generated_call()
                }
                RirCallTarget::Extern(id) => extern_context_use(program, *id),
            });
            for arg in args {
                uses = uses.union(call_arg_context_use(program, function, arg));
            }
            uses
        }
        RirRValue::DataRefAlloc { .. }
        | RirRValue::DataRefGet { .. }
        | RirRValue::List { .. }
        | RirRValue::Map { .. }
        | RirRValue::RangeListCopy { .. } => uses.union(ContextUse::rt_types()),
        RirRValue::ListPush { .. }
        | RirRValue::MapGet { .. }
        | RirRValue::MapInsert { .. }
        | RirRValue::MapRemove { .. }
        | RirRValue::MapEntryAt { .. }
        | RirRValue::MapValueAt { .. } => uses.union(ContextUse::rt()),
        RirRValue::CellGetCopy { cell, .. } => uses.union(cell_context_use(program, *cell)),
        RirRValue::ScopedPlaceCellGet { .. } => uses.union(ContextUse::rt()),
        RirRValue::Stringify { source_ty, .. }
            if matches!(program.types[source_ty.index()], RirType::Struct(_)) =>
        {
            uses.union(stringify_context_use(program, *source_ty))
        }
        RirRValue::Lambda { lambda, .. }
            if program.lambdas.get(lambda.index()).is_some_and(|lambda| {
                matches!(lambda.storage, RirLambdaStorage::HeapEnv { .. })
            }) =>
        {
            uses.union(ContextUse::rt_types())
        }
        _ => uses,
    }
}

fn rvalue_operand_context_use(
    program: &RirProgram,
    function: &RirFunction,
    value: &RirRValue,
) -> ContextUse {
    match value {
        RirRValue::Use(operand)
        | RirRValue::Unary { value: operand, .. }
        | RirRValue::Cast { value: operand, .. }
        | RirRValue::OptionalSome { value: operand, .. }
        | RirRValue::Stringify { value: operand, .. }
        | RirRValue::Format { value: operand, .. } => {
            operand_context_use(program, function, operand)
        }
        RirRValue::Binary { lhs, rhs, .. } | RirRValue::SharedRefEq { lhs, rhs, .. } => {
            operands_context_use(program, function, [lhs, rhs])
        }
        RirRValue::StringConcat { parts } => operands_context_use(program, function, parts),
        RirRValue::Struct { fields, .. }
        | RirRValue::Tuple { fields, .. }
        | RirRValue::DataRefAlloc { fields, .. }
        | RirRValue::Array { elems: fields, .. }
        | RirRValue::List { elems: fields, .. }
        | RirRValue::EnumVariant { fields, .. } => operands_context_use(program, function, fields),
        RirRValue::Map { entries, .. } => {
            entries.iter().fold(ContextUse::default(), |uses, entry| {
                uses.union(operands_context_use(
                    program,
                    function,
                    [&entry.0, &entry.1],
                ))
            })
        }
        RirRValue::DataRefGet { object, .. } => operand_context_use(program, function, object),
        RirRValue::Call { .. } => ContextUse::default(),
        RirRValue::Len { source }
        | RirRValue::SliceView { source, .. }
        | RirRValue::RangeListCopy { source, .. } => place_context_use(program, function, source),
        RirRValue::ListPush { list, value } => place_context_use(program, function, list)
            .union(operand_context_use(program, function, value)),
        RirRValue::MapGet { map, key, .. } | RirRValue::MapRemove { map, key, .. } => {
            place_context_use(program, function, map)
                .union(operand_context_use(program, function, key))
        }
        RirRValue::MapInsert { map, key, value } => place_context_use(program, function, map)
            .union(operands_context_use(program, function, [key, value])),
        RirRValue::MapEntryAt { map, .. } | RirRValue::MapValueAt { map, .. } => {
            place_context_use(program, function, map)
        }
        RirRValue::Lambda { .. }
        | RirRValue::CellGetCopy { .. }
        | RirRValue::ScopedPlaceCellGet { .. } => ContextUse::default(),
    }
}

fn term_context_use(
    program: &RirProgram,
    function: &RirFunction,
    term: &super::rir::RirTerm,
) -> ContextUse {
    match term {
        super::rir::RirTerm::Return(Some(operand)) => {
            operand_context_use(program, function, operand)
        }
        super::rir::RirTerm::None
        | super::rir::RirTerm::Return(None)
        | super::rir::RirTerm::Break(_)
        | super::rir::RirTerm::Continue(_)
        | super::rir::RirTerm::Unreachable => ContextUse::default(),
    }
}

fn extern_context_use(program: &RirProgram, id: super::rir::RirExternId) -> ContextUse {
    match &program.externs[id.index()].kind {
        RirExternKind::Native(native) => match native.abi.ctx {
            anvyx_runtime::RustWrapperCtx::HiddenRuntime => ContextUse::rt(),
            anvyx_runtime::RustWrapperCtx::None => ContextUse::default(),
        },
    }
}

fn stringify_context_use(program: &RirProgram, ty: RirTypeId) -> ContextUse {
    let Some(req) = program.stringify_reqs.iter().find(|req| req.ty == ty) else {
        return ContextUse::generated_call();
    };
    match req.kind {
        RirStringifyReqKind::Structural(_) => ContextUse::rt_types(),
        RirStringifyReqKind::Override { .. } => ContextUse::generated_call(),
    }
}

fn call_arg_context_use(
    program: &RirProgram,
    function: &RirFunction,
    arg: &RirCallArg,
) -> ContextUse {
    match arg {
        RirCallArg::Value(operand) => operand_context_use(program, function, operand),
        RirCallArg::ScopedLambda { callee, .. } => {
            ContextUse::generated_call().union(operand_context_use(program, function, callee))
        }
        RirCallArg::SharedBorrow(place) | RirCallArg::MutBorrow(place) => {
            place_context_use(program, function, place)
        }
        RirCallArg::SharedStringConst(_) => ContextUse::default(),
        RirCallArg::MutPlace(arg) => mut_place_context_use(program, function, arg),
    }
}

fn mut_place_context_use(
    program: &RirProgram,
    _function: &RirFunction,
    arg: &RirMutPlaceArg,
) -> ContextUse {
    let Some(mut ty) = arg.root.ty() else {
        return ContextUse::rt_types();
    };
    let mut uses = ContextUse::default();
    for projection in &arg.projections {
        match (program.types.get(ty.index()), projection) {
            (Some(RirType::List(elem) | RirType::Slice(elem)), RirProjection::Index(_)) => {
                uses = uses.union(ContextUse::rt());
                ty = *elem;
            }
            (Some(RirType::Array { elem, .. }), RirProjection::Index(_)) => ty = *elem,
            (Some(RirType::Struct(id)), RirProjection::Field(field)) => {
                let Some(next) = program
                    .structs
                    .get(id.index())
                    .and_then(|strukt| strukt.fields.get(field.index()))
                    .map(|field| field.ty)
                else {
                    return uses;
                };
                ty = next;
            }
            (Some(RirType::Tuple(id)), RirProjection::TupleField(field)) => {
                let Some(next) = program
                    .tuples
                    .get(id.index())
                    .and_then(|tuple| tuple.fields.get(field.index()))
                    .map(|field| field.ty)
                else {
                    return uses;
                };
                ty = next;
            }
            (Some(RirType::Map { value, .. }), RirProjection::MapIndex(_)) => {
                uses = uses.union(ContextUse::rt());
                let Some(next) = program.option_ty(*value) else {
                    return uses;
                };
                ty = next;
            }
            _ => return uses,
        }
    }
    uses
}

fn operands_context_use<'a>(
    program: &RirProgram,
    function: &RirFunction,
    operands: impl IntoIterator<Item = &'a RirOperand>,
) -> ContextUse {
    operands
        .into_iter()
        .fold(ContextUse::default(), |uses, operand| {
            uses.union(operand_context_use(program, function, operand))
        })
}

fn operand_context_use(
    program: &RirProgram,
    function: &RirFunction,
    operand: &RirOperand,
) -> ContextUse {
    match operand {
        RirOperand::Place(place) => place_context_use(program, function, place),
        RirOperand::Const(_) => ContextUse::default(),
    }
}

fn place_context_use(program: &RirProgram, function: &RirFunction, place: &RirPlace) -> ContextUse {
    if matches!(place.root, RirPlaceRoot::Global(_)) {
        return ContextUse::generated_call();
    }
    if place_is_mut_place_param(function, place)
        || projection_facts(program, function, place).is_some_and(|facts| facts.fallible_projection)
    {
        ContextUse::rt()
    } else {
        ContextUse::default()
    }
}

fn cell_context_use(program: &RirProgram, cell: RirCellRef) -> ContextUse {
    if cell_uses_ctx(program, cell) {
        ContextUse::rt_types()
    } else {
        ContextUse::default()
    }
}

fn stmt_child_blocks_any(
    stmt: &RirStmt,
    mut block_matches: impl FnMut(&RirStructuredBlock) -> bool,
) -> bool {
    match stmt {
        RirStmt::If(branch) => {
            block_matches(&branch.then_block)
                || branch.else_block.as_ref().is_some_and(block_matches)
        }
        RirStmt::Loop(loop_) => block_matches(&loop_.body),
        RirStmt::CollectionLoanScope(scope) => block_matches(&scope.body),
        RirStmt::CollectionSlotScope(block) => block_matches(block),
        RirStmt::OptionMatch(match_) => {
            block_matches(&match_.some_block) || block_matches(&match_.none_block)
        }
        RirStmt::EnumMatch(match_) => {
            match_.arms.iter().any(|arm| block_matches(&arm.block))
                || match_.else_block.as_ref().is_some_and(block_matches)
        }
        RirStmt::Init { .. }
        | RirStmt::GlobalEnsure { .. }
        | RirStmt::GlobalSetRoot { .. }
        | RirStmt::Assign { .. }
        | RirStmt::CellInit { .. }
        | RirStmt::CellSet { .. }
        | RirStmt::ScopedPlaceCellSet { .. }
        | RirStmt::Eval(_)
        | RirStmt::DataRefSet { .. }
        | RirStmt::MapValueSet { .. } => false,
    }
}

fn call_arg_preparation_fallible(program: &RirProgram, arg: &RirCallArg) -> bool {
    let RirCallArg::MutPlace(arg) = arg else {
        return false;
    };
    let Some(mut ty) = arg.root.ty() else {
        return true;
    };
    for projection in &arg.projections {
        match (program.types.get(ty.index()), projection) {
            (Some(RirType::List(_)), RirProjection::Index(_)) => return true,
            (Some(RirType::Struct(id)), RirProjection::Field(field)) => {
                let Some(next) = program
                    .structs
                    .get(id.index())
                    .and_then(|strukt| strukt.fields.get(field.index()))
                    .map(|field| field.ty)
                else {
                    return false;
                };
                ty = next;
            }
            (Some(RirType::Tuple(id)), RirProjection::TupleField(field)) => {
                let Some(next) = program
                    .tuples
                    .get(id.index())
                    .and_then(|tuple| tuple.fields.get(field.index()))
                    .map(|field| field.ty)
                else {
                    return false;
                };
                ty = next;
            }
            (Some(RirType::Array { elem, .. } | RirType::Slice(elem)), RirProjection::Index(_)) => {
                ty = *elem;
            }
            (Some(RirType::Map { value, .. }), RirProjection::MapIndex(_)) => {
                let Some(next) = program.option_ty(*value) else {
                    return false;
                };
                ty = next;
            }
            _ => return false,
        }
    }
    false
}

fn call_arg_uses_mut_place_param(function: &RirFunction, arg: &RirCallArg) -> bool {
    match arg {
        RirCallArg::Value(operand)
        | RirCallArg::ScopedLambda {
            callee: operand, ..
        } => operand_uses_mut_place_param(function, operand),
        RirCallArg::SharedBorrow(place) | RirCallArg::MutBorrow(place) => {
            place_is_mut_place_param(function, place)
        }
        RirCallArg::SharedStringConst(_) | RirCallArg::MutPlace(_) => false,
    }
}

fn operands_use_mut_place_param<'a>(
    function: &RirFunction,
    operands: impl IntoIterator<Item = &'a RirOperand>,
) -> bool {
    operands
        .into_iter()
        .any(|operand| operand_uses_mut_place_param(function, operand))
}

fn operand_uses_mut_place_param(function: &RirFunction, operand: &RirOperand) -> bool {
    match operand {
        RirOperand::Place(place) => place_is_mut_place_param(function, place),
        RirOperand::Const(_) => false,
    }
}

fn place_is_mut_place_param(function: &RirFunction, place: &RirPlace) -> bool {
    let RirPlaceRoot::Local(local) = place.root else {
        return false;
    };
    local_is_mut_place_param(function, local)
}

fn local_is_mut_place_param(function: &RirFunction, local: super::rir::RirLocalId) -> bool {
    function
        .params
        .iter()
        .any(|param| param.local == local && param.abi == RirParamAbi::MutPlace)
}

fn cell_uses_ctx(program: &RirProgram, cell: RirCellRef) -> bool {
    let id = match cell {
        RirCellRef::Owner(id) | RirCellRef::Capture { cell: id, .. } => id,
    };
    program
        .cells
        .get(id.index())
        .is_some_and(|cell| cell.storage == RirCellStorage::Heap)
}

fn rvalue_uses_mut_place_param(function: &RirFunction, value: &RirRValue) -> bool {
    match value {
        RirRValue::Use(operand)
        | RirRValue::Unary { value: operand, .. }
        | RirRValue::Cast { value: operand, .. }
        | RirRValue::OptionalSome { value: operand, .. }
        | RirRValue::Stringify { value: operand, .. }
        | RirRValue::Format { value: operand, .. } => {
            operand_uses_mut_place_param(function, operand)
        }
        RirRValue::Binary { lhs, rhs, .. } | RirRValue::SharedRefEq { lhs, rhs, .. } => {
            operands_use_mut_place_param(function, [lhs, rhs])
        }
        RirRValue::StringConcat { parts } => parts
            .iter()
            .any(|operand| operand_uses_mut_place_param(function, operand)),
        RirRValue::Struct { fields, .. }
        | RirRValue::Tuple { fields, .. }
        | RirRValue::DataRefAlloc { fields, .. }
        | RirRValue::Array { elems: fields, .. }
        | RirRValue::List { elems: fields, .. }
        | RirRValue::EnumVariant { fields, .. } => fields
            .iter()
            .any(|operand| operand_uses_mut_place_param(function, operand)),
        RirRValue::Map { entries, .. } => entries.iter().any(|(key, value)| {
            operand_uses_mut_place_param(function, key)
                || operand_uses_mut_place_param(function, value)
        }),
        RirRValue::DataRefGet { object, .. } => operand_uses_mut_place_param(function, object),
        RirRValue::Call { args, .. } => args
            .iter()
            .any(|arg| call_arg_uses_mut_place_param(function, arg)),
        RirRValue::Len { source }
        | RirRValue::SliceView { source, .. }
        | RirRValue::RangeListCopy { source, .. } => place_is_mut_place_param(function, source),
        RirRValue::ListPush { list, value } => {
            place_is_mut_place_param(function, list)
                || operand_uses_mut_place_param(function, value)
        }
        RirRValue::MapGet { map, key, .. } | RirRValue::MapRemove { map, key, .. } => {
            place_is_mut_place_param(function, map) || operand_uses_mut_place_param(function, key)
        }
        RirRValue::MapInsert { map, key, value } => {
            place_is_mut_place_param(function, map)
                || operands_use_mut_place_param(function, [key, value])
        }
        RirRValue::MapEntryAt { map, .. } | RirRValue::MapValueAt { map, .. } => {
            place_is_mut_place_param(function, map)
        }
        RirRValue::Lambda { .. }
        | RirRValue::CellGetCopy { .. }
        | RirRValue::ScopedPlaceCellGet { .. } => false,
    }
}
