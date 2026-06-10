use super::rir::{
    RirCallArg, RirCallTarget, RirCellRef, RirCellStorage, RirExternKind, RirFunction,
    RirLambdaStorage, RirOperand, RirParamAbi, RirPlace, RirProgram, RirRValue, RirStmt,
    RirStringifyReqKind, RirStruct, RirStructuredBlock, RirType, RirTypeId,
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
                || rvalue_calls_fallible(program, fallible, function, value)
        }
        RirStmt::CellSet { .. } | RirStmt::ScopedPlaceCellSet { .. } => true,
        RirStmt::DataRefSet { object, value, .. } => {
            operand_uses_mut_place_param(function, object)
                || operand_uses_mut_place_param(function, value)
        }
        _ => stmt_child_blocks_any(stmt, |block| {
            block_calls_fallible(program, fallible, function, block)
        }),
    }
}

fn rvalue_calls_fallible(
    program: &RirProgram,
    fallible: &[bool],
    function: &RirFunction,
    value: &RirRValue,
) -> bool {
    rvalue_uses_mut_place_param(function, value)
        || match value {
            RirRValue::Call { callee, args, .. } => {
                args.iter().any(call_arg_erases_dataref)
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
        RirStmt::DataRefSet { .. } => true,
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
        | RirStmt::DataRefSet { .. } => false,
    }
}

fn call_arg_erases_dataref(arg: &RirCallArg) -> bool {
    matches!(
        arg,
        RirCallArg::MutPlace(super::rir::RirMutPlaceArg::DataRefProjection { .. })
    )
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
        | RirRValue::ListSlice { source, .. } => place_is_mut_place_param(function, source),
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
        RirRValue::Lambda { .. }
        | RirRValue::CellGetCopy { .. }
        | RirRValue::ScopedPlaceCellGet { .. } => false,
    }
}
