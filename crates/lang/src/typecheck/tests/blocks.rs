use super::helpers::{
    assert_expr_type, block_expr, expr_stmt, fn_decl, get_expr_id, ident_expr, let_binding,
    lit_int, program, reset_expr_ids, run_err, run_ok,
};
use crate::{ast::Type, typecheck::error::DiagnosticKind};

// ---- block expression type tests ----

#[test]
fn test_block_expr_empty_is_void() {
    reset_expr_ids();
    // { }
    let block = block_expr(vec![], None);
    let block_id = get_expr_id(&block);
    let prog = program(vec![fn_decl(
        "main",
        vec![],
        Type::Void,
        vec![expr_stmt(block)],
    )]);

    let tcx = run_ok(prog);
    assert_expr_type(&tcx, block_id, &Type::Void);
}

#[test]
fn test_block_expr_trailing_int() {
    reset_expr_ids();
    // { 1 }
    let one = lit_int(1);
    let block = block_expr(vec![], Some(one));
    let block_id = get_expr_id(&block);
    let prog = program(vec![fn_decl(
        "main",
        vec![],
        Type::Int,
        vec![expr_stmt(block)],
    )]);

    let tcx = run_ok(prog);
    assert_expr_type(&tcx, block_id, &Type::Int);
}

#[test]
fn test_block_expr_let_then_ident() {
    reset_expr_ids();
    // { let x: int = 1; x }
    let x_val = lit_int(1);
    let x_binding = let_binding("x", Some(Type::Int), x_val);
    let x_ref = ident_expr("x");
    let x_ref_id = get_expr_id(&x_ref);
    let block = block_expr(vec![x_binding], Some(x_ref));
    let block_id = get_expr_id(&block);
    let prog = program(vec![fn_decl(
        "main",
        vec![],
        Type::Int,
        vec![expr_stmt(block)],
    )]);

    let tcx = run_ok(prog);
    assert_expr_type(&tcx, block_id, &Type::Int);
    assert_expr_type(&tcx, x_ref_id, &Type::Int);
}

// ---- let-binding with block tests ----

#[test]
fn test_let_binding_block_infers_int() {
    reset_expr_ids();
    // let x = { 1 };
    let one = lit_int(1);
    let one_id = get_expr_id(&one);
    let block = block_expr(vec![], Some(one));
    let block_id = get_expr_id(&block);
    let prog = program(vec![fn_decl(
        "main",
        vec![],
        Type::Void,
        vec![let_binding("x", None, block)],
    )]);

    let tcx = run_ok(prog);
    assert_expr_type(&tcx, one_id, &Type::Int);
    assert_expr_type(&tcx, block_id, &Type::Int);
}

#[test]
fn test_let_binding_block_annotated_int_ok() {
    reset_expr_ids();
    // let x: int = { 1 };
    let one = lit_int(1);
    let block = block_expr(vec![], Some(one));
    let block_id = get_expr_id(&block);
    let prog = program(vec![fn_decl(
        "main",
        vec![],
        Type::Void,
        vec![let_binding("x", Some(Type::Int), block)],
    )]);

    let tcx = run_ok(prog);
    assert_expr_type(&tcx, block_id, &Type::Int);
}

#[test]
fn test_let_binding_block_type_mismatch() {
    reset_expr_ids();
    // let x: string = { 1 };
    let one = lit_int(1);
    let block = block_expr(vec![], Some(one));
    let prog = program(vec![fn_decl(
        "main",
        vec![],
        Type::Void,
        vec![let_binding("x", Some(Type::String), block)],
    )]);

    let errors = run_err(prog);
    assert!(errors.iter().any(|e| matches!(
        &e.kind,
        DiagnosticKind::MismatchedTypes { expected, found }
        if *expected == Type::String && *found == Type::Int
    )));
}
