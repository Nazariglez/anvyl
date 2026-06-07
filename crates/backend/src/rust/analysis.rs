use super::rir::{
    RirCallTarget, RirExternKind, RirFunction, RirProgram, RirRValue, RirStmt, RirStringifyReqKind,
    RirStruct, RirStructuredBlock, RirType, RirTypeId,
};

pub(super) fn fallible_functions(program: &RirProgram) -> Vec<bool> {
    let mut fallible = vec![false; program.functions.len()];
    loop {
        let mut changed = false;
        for function in &program.functions {
            let is_fallible = block_calls_fallible(program, &fallible, &function.body);
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
    block: &RirStructuredBlock,
) -> bool {
    block
        .stmts
        .iter()
        .any(|stmt| stmt_calls_fallible(program, fallible, stmt))
}

fn stmt_calls_fallible(program: &RirProgram, fallible: &[bool], stmt: &RirStmt) -> bool {
    match stmt {
        RirStmt::Init { value, .. } | RirStmt::Assign { value, .. } | RirStmt::Eval(value) => {
            rvalue_calls_fallible(program, fallible, value)
        }
        RirStmt::DataRefSet { .. } => false,
        _ => stmt_child_blocks_any(stmt, |block| block_calls_fallible(program, fallible, block)),
    }
}

fn rvalue_calls_fallible(program: &RirProgram, fallible: &[bool], value: &RirRValue) -> bool {
    match value {
        RirRValue::Call { callee, .. } => match callee {
            RirCallTarget::Function(id) => fallible[id.index()],
            RirCallTarget::Extern(id) => match &program.externs[id.index()].kind {
                RirExternKind::Native(native) => native.abi.fallible,
            },
            RirCallTarget::LambdaValue { sig, .. } => program
                .lambdas_for_sig(*sig)
                .any(|lambda| fallible[lambda.function.index()]),
        },
        RirRValue::Use(_)
        | RirRValue::Unary { .. }
        | RirRValue::Binary { .. }
        | RirRValue::SharedRefEq { .. }
        | RirRValue::Cast { .. }
        | RirRValue::OptionalSome { .. }
        | RirRValue::StringConcat { .. }
        | RirRValue::Format { .. }
        | RirRValue::Len { .. }
        | RirRValue::ListPush { .. }
        | RirRValue::SliceView { .. }
        | RirRValue::Array { .. }
        | RirRValue::List { .. }
        | RirRValue::Map { .. }
        | RirRValue::MapGet { .. }
        | RirRValue::MapInsert { .. }
        | RirRValue::MapRemove { .. }
        | RirRValue::ListSlice { .. }
        | RirRValue::Lambda { .. }
        | RirRValue::Struct { .. }
        | RirRValue::Tuple { .. }
        | RirRValue::EnumVariant { .. }
        | RirRValue::DataRefAlloc { .. }
        | RirRValue::DataRefGet { .. } => false,
        RirRValue::Stringify { source_ty, .. } => {
            stringify_calls_fallible(program, fallible, *source_ty)
        }
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
    block_uses_ctx(program, &function.body)
}

pub(super) fn stringify_helper_uses_ctx(program: &RirProgram, strukt: &RirStruct) -> bool {
    strukt
        .fields
        .iter()
        .any(|field| matches!(program.types[field.ty.index()], RirType::Struct(_)))
}

fn block_uses_ctx(program: &RirProgram, block: &RirStructuredBlock) -> bool {
    block.stmts.iter().any(|stmt| stmt_uses_ctx(program, stmt))
}

fn stmt_uses_ctx(program: &RirProgram, stmt: &RirStmt) -> bool {
    match stmt {
        RirStmt::Init { value, .. } | RirStmt::Assign { value, .. } | RirStmt::Eval(value) => {
            rvalue_uses_ctx(program, value)
        }
        RirStmt::DataRefSet { .. } => true,
        _ => stmt_child_blocks_any(stmt, |block| block_uses_ctx(program, block)),
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
        | RirStmt::Eval(_)
        | RirStmt::DataRefSet { .. } => false,
    }
}

fn rvalue_uses_ctx(program: &RirProgram, value: &RirRValue) -> bool {
    match value {
        RirRValue::Call { .. } => true,
        RirRValue::Stringify { source_ty, .. } => {
            matches!(program.types[source_ty.index()], RirType::Struct(_))
        }
        RirRValue::Use(_)
        | RirRValue::Unary { .. }
        | RirRValue::Binary { .. }
        | RirRValue::SharedRefEq { .. }
        | RirRValue::Cast { .. }
        | RirRValue::OptionalSome { .. }
        | RirRValue::StringConcat { .. }
        | RirRValue::Format { .. }
        | RirRValue::Len { .. }
        | RirRValue::ListPush { .. }
        | RirRValue::SliceView { .. }
        | RirRValue::Array { .. }
        | RirRValue::List { .. }
        | RirRValue::Map { .. }
        | RirRValue::MapGet { .. }
        | RirRValue::MapInsert { .. }
        | RirRValue::MapRemove { .. }
        | RirRValue::ListSlice { .. }
        | RirRValue::Lambda { .. }
        | RirRValue::Struct { .. }
        | RirRValue::Tuple { .. }
        | RirRValue::EnumVariant { .. } => false,
        RirRValue::DataRefAlloc { .. } | RirRValue::DataRefGet { .. } => true,
    }
}
