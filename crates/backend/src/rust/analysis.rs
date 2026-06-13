use super::rir::{
    RirCallArg, RirCallTarget, RirCellRef, RirCellStorage, RirCollectionLoanScope,
    RirCollectionRootKind, RirExternKind, RirFunction, RirLambdaStorage, RirOperand, RirParamAbi,
    RirPlace, RirProgram, RirProjection, RirRValue, RirStmt, RirStringifyReqKind, RirStruct,
    RirStructuredBlock, RirType, RirTypeId,
};

pub(super) fn fallible_functions(program: &RirProgram) -> Vec<bool> {
    let mut fallible = vec![false; program.functions.len()];
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
        RirStmt::Assign { dst, value } => {
            place_is_mut_place_param(function, dst)
                || place_has_indexed_collection_write(program, function, dst)
                || collection_replace_ty(program, dst.ty)
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
                || operand_ty(program, value).is_some_and(|ty| collection_replace_ty(program, ty))
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
            operand_has_slice_index(program, function, operand)
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
            | RirRValue::MapInsert { .. }
            | RirRValue::MapRemove { .. }
            | RirRValue::MapEntryAt { .. }
            | RirRValue::MapValueAt { .. }
            | RirRValue::SliceView { .. } => true,
            RirRValue::RangeListCopy { source, .. } => {
                matches!(program.types[source.ty.index()], RirType::Slice(_))
            }
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
            operand_has_slice_index(program, function, operand)
        }
        RirRValue::Binary { lhs, rhs, .. } | RirRValue::SharedRefEq { lhs, rhs, .. } => {
            operand_has_slice_index(program, function, lhs)
                || operand_has_slice_index(program, function, rhs)
        }
        RirRValue::Struct { fields, .. }
        | RirRValue::Tuple { fields, .. }
        | RirRValue::DataRefAlloc { fields, .. }
        | RirRValue::Array { elems: fields, .. }
        | RirRValue::List { elems: fields, .. }
        | RirRValue::EnumVariant { fields, .. }
        | RirRValue::StringConcat { parts: fields } => fields
            .iter()
            .any(|operand| operand_has_slice_index(program, function, operand)),
        RirRValue::Map { entries, .. } => entries.iter().any(|(key, value)| {
            operand_has_slice_index(program, function, key)
                || operand_has_slice_index(program, function, value)
        }),
        RirRValue::MapInsert { key, value, .. } => {
            operand_has_slice_index(program, function, key)
                || operand_has_slice_index(program, function, value)
        }
        RirRValue::Call { args, .. } => args
            .iter()
            .any(|arg| call_arg_has_slice_index(program, function, arg)),
        RirRValue::Lambda { captures, .. } => captures.iter().any(|capture| match capture {
            super::rir::RirLambdaCaptureArg::Readonly { value } => {
                operand_has_slice_index(program, function, value)
            }
            super::rir::RirLambdaCaptureArg::Scoped { place } => {
                place_has_slice_index(program, function, place)
            }
            super::rir::RirLambdaCaptureArg::StackCell { .. }
            | super::rir::RirLambdaCaptureArg::HeapCell { .. }
            | super::rir::RirLambdaCaptureArg::ScopedPlaceCell { .. } => false,
        }),
        RirRValue::DataRefGet { object, .. } => operand_has_slice_index(program, function, object),
        RirRValue::Len { source }
        | RirRValue::SliceView { source, .. }
        | RirRValue::RangeListCopy { source, .. } => {
            place_has_slice_index(program, function, source)
        }
        RirRValue::MapEntryAt { map, .. } | RirRValue::MapValueAt { map, .. } => {
            place_has_slice_index(program, function, map)
        }
        RirRValue::CellGetCopy { .. } | RirRValue::ScopedPlaceCellGet { .. } => false,
    }
}

fn operand_has_slice_index(
    program: &RirProgram,
    function: &RirFunction,
    operand: &RirOperand,
) -> bool {
    match operand {
        RirOperand::Place(place) => place_has_slice_index(program, function, place),
        RirOperand::Const(_) => false,
    }
}

fn call_arg_has_slice_index(
    program: &RirProgram,
    function: &RirFunction,
    arg: &RirCallArg,
) -> bool {
    match arg {
        RirCallArg::Value(operand) => operand_has_slice_index(program, function, operand),
        RirCallArg::SharedBorrow(place) | RirCallArg::MutBorrow(place) => {
            place_has_slice_index(program, function, place)
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

fn place_has_slice_index(program: &RirProgram, function: &RirFunction, place: &RirPlace) -> bool {
    projection_facts(program, function, place).is_some_and(|facts| facts.slice_index)
}

struct ProjectionFacts {
    slice_index: bool,
    indexed_collection_write: bool,
}

fn projection_facts(
    program: &RirProgram,
    function: &RirFunction,
    place: &RirPlace,
) -> Option<ProjectionFacts> {
    let mut ty = function.locals.get(place.local.index())?.ty;
    let mut facts = ProjectionFacts {
        slice_index: false,
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
                RirType::List(elem) => {
                    facts.indexed_collection_write = true;
                    ty = *elem;
                }
                RirType::Slice(elem) => {
                    facts.slice_index = true;
                    facts.indexed_collection_write = true;
                    ty = *elem;
                }
                _ => return None,
            },
            RirProjection::MapIndex(_) => {
                let RirType::Map { value, .. } = program.types.get(ty.index())? else {
                    return None;
                };
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

fn collection_replace_ty(program: &RirProgram, ty: RirTypeId) -> bool {
    matches!(
        program.types[ty.index()],
        RirType::List(_) | RirType::Map { .. }
    )
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

pub(super) fn function_uses_ctx(program: &RirProgram, function: &RirFunction) -> bool {
    block_uses_ctx(program, function, &function.body)
}

pub(super) fn stringify_helper_uses_ctx(program: &RirProgram, strukt: &RirStruct) -> bool {
    strukt
        .fields
        .iter()
        .any(|field| matches!(program.types[field.ty.index()], RirType::Struct(_)))
}

fn block_uses_ctx(
    program: &RirProgram,
    function: &RirFunction,
    block: &RirStructuredBlock,
) -> bool {
    block
        .stmts
        .iter()
        .any(|stmt| stmt_uses_ctx(program, function, stmt))
}

fn stmt_uses_ctx(program: &RirProgram, function: &RirFunction, stmt: &RirStmt) -> bool {
    match stmt {
        RirStmt::Init { value, .. } | RirStmt::Eval(value) => {
            rvalue_uses_ctx(program, function, value)
        }
        RirStmt::CellInit { cell, value } => {
            cell_uses_ctx(program, *cell) || rvalue_uses_ctx(program, function, value)
        }
        RirStmt::Assign { dst, value } => {
            place_is_mut_place_param(function, dst) || rvalue_uses_ctx(program, function, value)
        }
        RirStmt::CellSet { cell, value } => {
            cell_uses_ctx(program, *cell) || rvalue_uses_ctx(program, function, value)
        }
        RirStmt::ScopedPlaceCellSet { .. } => true,
        RirStmt::DataRefSet { .. } | RirStmt::MapValueSet { .. } => true,
        RirStmt::CollectionLoanScope(scope) => {
            place_is_mut_place_param(function, &scope.root)
                || block_uses_ctx(program, function, &scope.body)
        }
        RirStmt::CollectionSlotScope(block) => block_uses_ctx(program, function, block),
        _ => stmt_child_blocks_any(stmt, |block| block_uses_ctx(program, function, block)),
    }
}

fn stmt_child_blocks_any(
    stmt: &RirStmt,
    mut block_matches: impl FnMut(&RirStructuredBlock) -> bool,
) -> bool {
    match stmt {
        RirStmt::If(branch) => {
            block_matches(&branch.then_block)
                || match &branch.else_block {
                    Some(block) => block_matches(block),
                    None => false,
                }
        }
        RirStmt::Loop(loop_) => block_matches(&loop_.body),
        RirStmt::CollectionLoanScope(scope) => block_matches(&scope.body),
        RirStmt::CollectionSlotScope(block) => block_matches(block),
        RirStmt::OptionMatch(match_) => {
            block_matches(&match_.some_block) || block_matches(&match_.none_block)
        }
        RirStmt::EnumMatch(match_) => {
            match_.arms.iter().any(|arm| block_matches(&arm.block))
                || match &match_.else_block {
                    Some(block) => block_matches(block),
                    None => false,
                }
        }
        RirStmt::Init { .. }
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
                let Some(next) = option_ty(program, *value) else {
                    return false;
                };
                ty = next;
            }
            _ => return false,
        }
    }
    false
}

fn option_ty(program: &RirProgram, inner: RirTypeId) -> Option<RirTypeId> {
    program
        .types
        .iter()
        .position(|ty| matches!(ty, RirType::Option(found) if *found == inner))
        .map(RirTypeId::from_index)
}

fn call_arg_uses_mut_place_param(function: &RirFunction, arg: &RirCallArg) -> bool {
    match arg {
        RirCallArg::Value(operand) => operand_uses_mut_place_param(function, operand),
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
    local_is_mut_place_param(function, place.local)
}

fn local_is_mut_place_param(function: &RirFunction, local: super::rir::RirLocalId) -> bool {
    function
        .params
        .iter()
        .any(|param| param.local == local && param.abi == RirParamAbi::MutPlace)
}

fn rvalue_uses_ctx(program: &RirProgram, function: &RirFunction, value: &RirRValue) -> bool {
    rvalue_uses_mut_place_param(function, value)
        || match value {
            RirRValue::Call { .. }
            | RirRValue::DataRefAlloc { .. }
            | RirRValue::DataRefGet { .. } => true,
            RirRValue::CellGetCopy { cell, .. } => cell_uses_ctx(program, *cell),
            RirRValue::ScopedPlaceCellGet { .. } => true,
            RirRValue::Stringify { source_ty, .. } => {
                matches!(program.types[source_ty.index()], RirType::Struct(_))
            }
            RirRValue::Lambda { lambda, .. } => program
                .lambdas
                .get(lambda.index())
                .is_some_and(|lambda| matches!(lambda.storage, RirLambdaStorage::HeapEnv { .. })),
            _ => false,
        }
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
