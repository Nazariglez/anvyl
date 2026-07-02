use super::{
    native_call::NativeArgBoundary,
    place::{mut_place_dynamic_facts, place_dynamic_facts},
    rir::{
        RirCallArg, RirCallTarget, RirCellRef, RirCellStorage, RirCollectionAccess,
        RirCollectionLoanScope, RirCollectionRootKind, RirExternKind, RirFunction,
        RirLambdaStorage, RirMutPlaceAccess, RirMutPlaceArg, RirMutPlaceHandle, RirOperand,
        RirOptionSubject, RirParamAbi, RirPlace, RirPlaceRoot, RirProgram, RirRValue, RirStmt,
        RirStringifyReqKind, RirStruct, RirStructuredBlock, RirType, RirTypeId, native_arg_facts,
        native_return_adopts_resource, stmt_child_blocks_any,
    },
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
        RirStmt::GlobalEnsure { .. }
        | RirStmt::GlobalSetRoot { .. }
        | RirStmt::GlobalUpdateRoot { .. }
        | RirStmt::MutPlaceSet { .. }
        | RirStmt::CellSet { .. }
        | RirStmt::ScopedPlaceCellSet { .. }
        | RirStmt::SequenceSlotSet { .. }
        | RirStmt::MapValueSet { .. }
        | RirStmt::MapEntryMatch(_) => true,
        RirStmt::Assign { dst, value } => {
            place_is_mut_place_param(function, dst)
                || place_has_fallible_projection(program, function, dst)
                || program.collection_replace_ty(dst.ty)
                || rvalue_calls_fallible(program, fallible, function, value)
        }
        RirStmt::CollectionLoanScope(scope) => {
            block_calls_fallible(program, fallible, function, &scope.body)
                || loan_scope_is_fallible(program, scope)
        }
        RirStmt::DataRefSet { object, value, .. } => {
            operand_uses_mut_place_param(function, object)
                || operand_uses_mut_place_param(function, value)
                || operand_has_fallible_place(program, function, object)
                || operand_has_fallible_place(program, function, value)
                || operand_ty(program, value).is_some_and(|ty| program.collection_replace_ty(ty))
        }
        RirStmt::OptionMatch(match_) => {
            match_.payload_ref
                || option_subject_fallible(program, function, &match_.subject)
                || stmt_child_blocks_any(stmt, |block| {
                    block_calls_fallible(program, fallible, function, block)
                })
        }
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
            | RirRValue::SequenceSlotAt { .. }
            | RirRValue::SliceView { .. }
            | RirRValue::CellGetCopy { .. }
            | RirRValue::ScopedPlaceCellGet { .. }
            | RirRValue::MutPlaceGetCopy { .. } => true,
            RirRValue::Call { callee, args, .. } => {
                args.iter()
                    .any(|arg| call_arg_preparation_fallible(program, arg))
                    || match callee {
                        RirCallTarget::Function(id) => fallible[id.index()],
                        RirCallTarget::Extern(id) => match &program.externs[id.index()].kind {
                            RirExternKind::Native(native) => {
                                native
                                    .call_plan(program.has_retained_callbacks())
                                    .provider_entry()
                                    .suspends_runtime_entry()
                                    || native.abi.fallible
                                    || native_ref_borrow_conversion_fallible(program, *id, args)
                            }
                        },
                        RirCallTarget::LambdaValue { sig, .. } => program
                            .lambdas_for_sig(*sig)
                            .any(|lambda| fallible[lambda.function.index()]),
                    }
            }
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
        | RirRValue::FunctionValue { value: operand, .. }
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
        RirRValue::Len { source } => place_has_fallible_projection(program, function, source),
        RirRValue::CollectionLen { source } => {
            collection_access_fallible(program, function, source)
        }
        RirRValue::SequenceSlotAt { collection, .. } => {
            collection_access_fallible(program, function, collection)
        }
        RirRValue::SliceView { source, .. } | RirRValue::RangeListCopy { source, .. } => {
            place_has_fallible_projection(program, function, source)
        }
        RirRValue::MapEntryAt { map, .. } | RirRValue::MapValueAt { map, .. } => {
            collection_access_fallible(program, function, map)
        }
        RirRValue::CellGetCopy { .. } | RirRValue::ScopedPlaceCellGet { .. } => false,
        RirRValue::MutPlaceGetCopy { place, .. } => mut_place_preparation_fallible(program, place),
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
        | RirCallArg::InitFieldProvided(operand)
        | RirCallArg::ScopedLambda {
            callee: operand, ..
        }
        | RirCallArg::EscapingLambda {
            callee: operand, ..
        } => operand_has_fallible_place(program, function, operand),
        RirCallArg::SharedBorrow(place) | RirCallArg::MutBorrow(place) => {
            place_has_fallible_projection(program, function, place)
        }
        RirCallArg::SharedStringConst(_)
        | RirCallArg::MutPlace(_)
        | RirCallArg::InitFieldOmitted => false,
    }
}

fn place_has_fallible_projection(
    program: &RirProgram,
    function: &RirFunction,
    place: &RirPlace,
) -> bool {
    place_dynamic_facts(program, function, place).is_some_and(|facts| facts.fallible_projection)
}

fn collection_access_fallible(
    program: &RirProgram,
    function: &RirFunction,
    access: &RirCollectionAccess,
) -> bool {
    match access {
        RirCollectionAccess::Direct(place) => {
            matches!(place.root, RirPlaceRoot::Global(_))
                || place_has_fallible_projection(program, function, place)
        }
        RirCollectionAccess::MutPlace(_) => true,
    }
}

fn operand_ty(program: &RirProgram, operand: &RirOperand) -> Option<RirTypeId> {
    match operand {
        RirOperand::Place(place) => Some(place.ty),
        RirOperand::Const(id) => program.consts.get(id.index()).map(|konst| konst.ty),
    }
}

fn loan_scope_is_fallible(program: &RirProgram, scope: &RirCollectionLoanScope) -> bool {
    scope.root_kind.tracks_shape_loan()
        && matches!(
            (scope.root_kind, program.types[scope.root.ty().index()]),
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
        RirStmt::GlobalSetRoot { value, .. } | RirStmt::GlobalUpdateRoot { value, .. } => {
            ContextUse::globals().union(rvalue_context_use(program, function, value))
        }
        RirStmt::MutPlaceSet { place, value } => ContextUse::rt()
            .union(mut_place_context_use(program, function, place))
            .union(rvalue_context_use(program, function, value)),
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
        RirStmt::SequenceSlotSet {
            collection, value, ..
        } => ContextUse::rt()
            .union(collection_access_context_use(program, function, collection))
            .union(operand_context_use(program, function, value)),
        RirStmt::MapValueSet { map, value, .. } => ContextUse::rt()
            .union(collection_access_context_use(program, function, map))
            .union(operand_context_use(program, function, value)),
        RirStmt::CollectionLoanScope(scope) => collection_loan_context_use(
            program, function, scope,
        )
        .union(block_context_use(program, function, &scope.body)),
        RirStmt::CollectionSlotScope(block) => block_context_use(program, function, block),
        RirStmt::OptionMatch(match_) => {
            let subject = option_subject_context_use(program, function, &match_.subject);
            let payload = match_.payload_ref.then(ContextUse::rt).unwrap_or_default();
            subject
                .union(payload)
                .union(stmt_child_blocks_context_use(program, function, stmt))
        }
        RirStmt::MapEntryMatch(match_) => mut_place_context_use(program, function, &match_.map)
            .union(ContextUse::rt())
            .union(operand_context_use(program, function, &match_.key))
            .union(stmt_child_blocks_context_use(program, function, stmt)),
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

fn collection_access_context_use(
    program: &RirProgram,
    function: &RirFunction,
    access: &RirCollectionAccess,
) -> ContextUse {
    match access {
        RirCollectionAccess::Direct(place) => place_context_use(program, function, place),
        RirCollectionAccess::MutPlace(place) => mut_place_context_use(program, function, place),
    }
}

fn collection_loan_context_use(
    program: &RirProgram,
    function: &RirFunction,
    scope: &RirCollectionLoanScope,
) -> ContextUse {
    match &scope.root {
        RirCollectionAccess::Direct(place) => match place.root {
            RirPlaceRoot::Global(_) => ContextUse::generated_call(),
            RirPlaceRoot::Local(_) if place_is_mut_place_param(function, place) => ContextUse::rt(),
            RirPlaceRoot::Local(_) => ContextUse::default(),
        },
        RirCollectionAccess::MutPlace(place) => mut_place_context_use(program, function, place),
    }
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
                RirCallTarget::Extern(id) => extern_context_use(program, *id)
                    .union(native_ref_borrow_context_use(program, *id, args)),
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
        | RirRValue::MapValueAt { .. }
        | RirRValue::ScopedPlaceCellGet { .. } => uses.union(ContextUse::rt()),
        RirRValue::MutPlaceGetCopy { place, .. } => uses
            .union(ContextUse::rt())
            .union(mut_place_context_use(program, function, place)),
        RirRValue::CellGetCopy { cell, .. } => uses.union(cell_context_use(program, *cell)),
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
        | RirRValue::FunctionValue { value: operand, .. }
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
        RirRValue::Len { source } => place_context_use(program, function, source),
        RirRValue::CollectionLen { source } => {
            collection_access_context_use(program, function, source)
        }
        RirRValue::SequenceSlotAt { collection, .. } => {
            ContextUse::rt().union(collection_access_context_use(program, function, collection))
        }
        RirRValue::SliceView { source, .. } | RirRValue::RangeListCopy { source, .. } => {
            place_context_use(program, function, source)
        }
        RirRValue::ListPush { list, value } => {
            collection_access_context_use(program, function, list)
                .union(operand_context_use(program, function, value))
        }
        RirRValue::MapGet { map, key, .. } => collection_access_context_use(program, function, map)
            .union(operand_context_use(program, function, key)),
        RirRValue::MapRemove { map, key, .. } => {
            collection_access_context_use(program, function, map)
                .union(operand_context_use(program, function, key))
        }
        RirRValue::MapInsert {
            map, key, value, ..
        } => collection_access_context_use(program, function, map).union(operands_context_use(
            program,
            function,
            [key, value],
        )),
        RirRValue::MapEntryAt { map, .. } | RirRValue::MapValueAt { map, .. } => {
            collection_access_context_use(program, function, map)
        }
        RirRValue::Call { .. }
        | RirRValue::Lambda { .. }
        | RirRValue::CellGetCopy { .. }
        | RirRValue::ScopedPlaceCellGet { .. }
        | RirRValue::MutPlaceGetCopy { .. } => ContextUse::default(),
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
        RirExternKind::Native(native) => {
            let base = match native.abi.ctx {
                anvyx_runtime::RustWrapperCtx::HiddenRuntime => ContextUse::rt(),
                anvyx_runtime::RustWrapperCtx::None => ContextUse::default(),
            };
            if native_return_adopts_resource(
                program,
                program.externs[id.index()].ret,
                &native.abi.ret,
            ) {
                base.union(ContextUse::rt())
            } else {
                base
            }
        }
    }
}

fn native_ref_borrow_context_use(
    program: &RirProgram,
    ext: super::rir::RirExternId,
    args: &[RirCallArg],
) -> ContextUse {
    if native_ref_borrow_conversion_fallible(program, ext, args) {
        ContextUse::rt()
    } else {
        ContextUse::default()
    }
}

fn native_ref_borrow_conversion_fallible(
    program: &RirProgram,
    ext: super::rir::RirExternId,
    args: &[RirCallArg],
) -> bool {
    let RirExternKind::Native(native) = &program.externs[ext.index()].kind;
    let plan = native.call_plan(program.has_retained_callbacks());
    program.externs[ext.index()]
        .params
        .iter()
        .zip(args)
        .enumerate()
        .any(|(index, (param, arg))| {
            matches!(
                plan.arg_boundary(index, native_arg_facts(program, param.ty, arg)),
                NativeArgBoundary::NativeRefBorrow { .. }
            )
        })
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
        RirCallArg::Value(operand) | RirCallArg::InitFieldProvided(operand) => {
            operand_context_use(program, function, operand)
        }
        RirCallArg::ScopedLambda { callee, .. } | RirCallArg::EscapingLambda { callee, .. } => {
            ContextUse::generated_call().union(operand_context_use(program, function, callee))
        }
        RirCallArg::SharedBorrow(place) | RirCallArg::MutBorrow(place) => {
            place_context_use(program, function, place)
        }
        RirCallArg::SharedStringConst(_) | RirCallArg::InitFieldOmitted => ContextUse::default(),
        RirCallArg::MutPlace(arg) => mut_place_context_use(program, function, arg),
    }
}

fn mut_place_context_use(
    program: &RirProgram,
    _function: &RirFunction,
    arg: &RirMutPlaceArg,
) -> ContextUse {
    let root = mut_place_root_context_use(&arg.access);
    let Some(facts) = mut_place_dynamic_facts(program, arg) else {
        return root.union(ContextUse::rt_types());
    };
    if facts.fallible_projection {
        root.union(ContextUse::rt())
    } else {
        root
    }
}

fn mut_place_root_context_use(access: &RirMutPlaceAccess) -> ContextUse {
    match access {
        RirMutPlaceAccess::Handle(RirMutPlaceHandle::Global { .. }) => ContextUse::generated_call(),
        RirMutPlaceAccess::DataRef { .. } => ContextUse::rt_types(),
        RirMutPlaceAccess::Handle(
            RirMutPlaceHandle::Local { .. }
            | RirMutPlaceHandle::Param { .. }
            | RirMutPlaceHandle::StackCell { .. }
            | RirMutPlaceHandle::HeapCell { .. }
            | RirMutPlaceHandle::ScopedPlaceCell { .. },
        ) => ContextUse::default(),
    }
}

fn option_subject_context_use(
    program: &RirProgram,
    function: &RirFunction,
    subject: &RirOptionSubject,
) -> ContextUse {
    match subject {
        RirOptionSubject::Place(place) => place_context_use(program, function, place),
        RirOptionSubject::MutPlace(place) => {
            ContextUse::rt().union(mut_place_context_use(program, function, place))
        }
    }
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
        || place_dynamic_facts(program, function, place)
            .is_some_and(|facts| facts.fallible_projection)
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

fn call_arg_preparation_fallible(program: &RirProgram, arg: &RirCallArg) -> bool {
    let RirCallArg::MutPlace(arg) = arg else {
        return false;
    };
    mut_place_preparation_fallible(program, arg)
}

fn option_subject_fallible(
    program: &RirProgram,
    function: &RirFunction,
    subject: &RirOptionSubject,
) -> bool {
    match subject {
        RirOptionSubject::Place(place) => place_has_fallible_projection(program, function, place),
        RirOptionSubject::MutPlace(_) => true,
    }
}

fn mut_place_preparation_fallible(program: &RirProgram, arg: &RirMutPlaceArg) -> bool {
    match arg.access {
        RirMutPlaceAccess::Handle(RirMutPlaceHandle::Global { .. })
        | RirMutPlaceAccess::DataRef { .. } => true,
        RirMutPlaceAccess::Handle(
            RirMutPlaceHandle::Local { .. }
            | RirMutPlaceHandle::Param { .. }
            | RirMutPlaceHandle::StackCell { .. }
            | RirMutPlaceHandle::HeapCell { .. }
            | RirMutPlaceHandle::ScopedPlaceCell { .. },
        ) => mut_place_dynamic_facts(program, arg).is_none_or(|facts| facts.fallible_projection),
    }
}

fn mut_place_uses_mut_place_param(function: &RirFunction, arg: &RirMutPlaceArg) -> bool {
    match arg.access {
        RirMutPlaceAccess::Handle(RirMutPlaceHandle::Param { local, .. }) => function
            .params
            .iter()
            .any(|param| param.local == local && param.abi == RirParamAbi::MutPlace),
        _ => false,
    }
}

fn call_arg_uses_mut_place_param(function: &RirFunction, arg: &RirCallArg) -> bool {
    match arg {
        RirCallArg::Value(operand)
        | RirCallArg::InitFieldProvided(operand)
        | RirCallArg::ScopedLambda {
            callee: operand, ..
        }
        | RirCallArg::EscapingLambda {
            callee: operand, ..
        } => operand_uses_mut_place_param(function, operand),
        RirCallArg::SharedBorrow(place) | RirCallArg::MutBorrow(place) => {
            place_is_mut_place_param(function, place)
        }
        RirCallArg::SharedStringConst(_)
        | RirCallArg::MutPlace(_)
        | RirCallArg::InitFieldOmitted => false,
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

fn collection_access_uses_mut_place_param(
    function: &RirFunction,
    access: &RirCollectionAccess,
) -> bool {
    match access {
        RirCollectionAccess::Direct(place) => place_is_mut_place_param(function, place),
        RirCollectionAccess::MutPlace(place) => mut_place_uses_mut_place_param(function, place),
    }
}

fn rvalue_uses_mut_place_param(function: &RirFunction, value: &RirRValue) -> bool {
    match value {
        RirRValue::Use(operand)
        | RirRValue::FunctionValue { value: operand, .. }
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
        RirRValue::Len { source } => place_is_mut_place_param(function, source),
        RirRValue::CollectionLen { source } => {
            collection_access_uses_mut_place_param(function, source)
        }
        RirRValue::SequenceSlotAt { collection, .. } => {
            collection_access_uses_mut_place_param(function, collection)
        }
        RirRValue::SliceView { source, .. } | RirRValue::RangeListCopy { source, .. } => {
            place_is_mut_place_param(function, source)
        }
        RirRValue::ListPush { list, value } => {
            collection_access_uses_mut_place_param(function, list)
                || operand_uses_mut_place_param(function, value)
        }
        RirRValue::MapGet { map, key, .. } | RirRValue::MapRemove { map, key, .. } => {
            collection_access_uses_mut_place_param(function, map)
                || operand_uses_mut_place_param(function, key)
        }
        RirRValue::MapInsert {
            map, key, value, ..
        } => {
            collection_access_uses_mut_place_param(function, map)
                || operands_use_mut_place_param(function, [key, value])
        }
        RirRValue::MapEntryAt { map, .. } | RirRValue::MapValueAt { map, .. } => {
            collection_access_uses_mut_place_param(function, map)
        }
        RirRValue::Lambda { .. }
        | RirRValue::CellGetCopy { .. }
        | RirRValue::ScopedPlaceCellGet { .. } => false,
        RirRValue::MutPlaceGetCopy { place, .. } => mut_place_uses_mut_place_param(function, place),
    }
}
