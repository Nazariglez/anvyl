use super::helpers::{
    array_literal, assert_expr_type, assign_expr, call_expr, expr_stmt, fn_decl, get_expr_id,
    ident_expr, let_binding, lit_bool, lit_int, lit_nil, opt_type, program, reset_expr_ids,
    return_stmt, run_err, run_ok, var_binding,
};
use crate::{
    ast::{ArrayLen, AssignOp, Type},
    typecheck::error::DiagnosticKind,
};

#[test]
fn test_binding_annotated_success() {
    reset_expr_ids();

    // let x: int = 1;
    let value_expr = lit_int(1);
    let value_id = get_expr_id(&value_expr);
    let prog = program(vec![let_binding("x", Some(Type::Int), value_expr)]);

    let tcx = run_ok(prog);
    assert_expr_type(&tcx, value_id, &Type::Int);
}

#[test]
fn test_binding_annotated_mismatch() {
    reset_expr_ids();

    // let x: int = true;
    let prog = program(vec![let_binding("x", Some(Type::Int), lit_bool(true))]);

    let errors = run_err(prog);
    // should have at least one mismatched types error between int and bool
    assert!(!errors.is_empty());
    assert!(
        errors.iter().any(|e| matches!(
            &e.kind,
            DiagnosticKind::MismatchedTypes { expected, found }
            if (*expected == Type::Int && *found == Type::Bool) ||
               (*expected == Type::Bool && *found == Type::Int)
        )),
        "Expected MismatchedTypes error (int/bool mismatch), got: {errors:?}"
    );
}

#[test]
fn test_binding_unannotated_simple_inference() {
    reset_expr_ids();

    // let x = 1;
    let value_expr = lit_int(1);
    let value_id = get_expr_id(&value_expr);
    let prog = program(vec![let_binding("x", None, value_expr)]);

    let tcx = run_ok(prog);
    assert_expr_type(&tcx, value_id, &Type::Int);
}

#[test]
fn test_binding_unannotated_unresolved_infer() {
    reset_expr_ids();

    // let x = nil; (no other uses)
    let prog = program(vec![let_binding("x", None, lit_nil())]);

    let errors = run_err(prog);
    assert!(
        errors
            .iter()
            .any(|e| matches!(&e.kind, DiagnosticKind::UnresolvedInfer))
    );
}

#[test]
fn test_binding_chained_inference() {
    reset_expr_ids();

    // let x: int = 1; let y = x;
    // first binding needs type annotation so x is in scope for second binding
    let x_val_expr = lit_int(1);
    let x_val_id = get_expr_id(&x_val_expr);
    let y_val_expr = ident_expr("x");
    let y_val_id = get_expr_id(&y_val_expr);
    let prog = program(vec![
        let_binding("x", Some(Type::Int), x_val_expr),
        let_binding("y", None, y_val_expr),
    ]);

    let tcx = run_ok(prog);
    assert_expr_type(&tcx, x_val_id, &Type::Int);
    assert_expr_type(&tcx, y_val_id, &Type::Int);
}

#[test]
fn test_constraint_chain_resolves() {
    reset_expr_ids();
    // let a: int? = nil; let b: int? = a;
    let a_expr = lit_nil();
    let a_id = get_expr_id(&a_expr);
    let b_expr = ident_expr("a");
    let b_id = get_expr_id(&b_expr);
    let prog = program(vec![
        let_binding("a", Some(opt_type(Type::Int)), a_expr),
        let_binding("b", Some(opt_type(Type::Int)), b_expr),
    ]);

    let tcx = run_ok(prog);
    assert_expr_type(&tcx, a_id, &opt_type(Type::Int));
    assert_expr_type(&tcx, b_id, &opt_type(Type::Int));
}

#[test]
fn test_leftover_infer() {
    reset_expr_ids();
    // let a = nil;
    let prog = program(vec![let_binding("a", None, lit_nil())]);

    let errors = run_err(prog);
    assert!(
        errors
            .iter()
            .any(|e| matches!(&e.kind, DiagnosticKind::UnresolvedInfer))
    );
}

#[test]
fn test_constraint_through_function_call() {
    reset_expr_ids();
    // fn id(x: int) -> int { return x; }
    // let a: int = id(1);
    let fn_def = fn_decl(
        "id",
        vec![("x", Type::Int)],
        Type::Int,
        vec![return_stmt(Some(ident_expr("x")))],
    );
    let a_val = call_expr(ident_expr("id"), vec![lit_int(1)]);
    let a_val_id = get_expr_id(&a_val);
    let prog = program(vec![fn_def, let_binding("a", Some(Type::Int), a_val)]);

    let tcx = run_ok(prog);
    assert_expr_type(&tcx, a_val_id, &Type::Int);
}

#[test]
fn test_assignability_int_to_optional_int() {
    reset_expr_ids();
    // let x: int? = 10;
    let value_expr = lit_int(10);
    let value_id = get_expr_id(&value_expr);
    let prog = program(vec![let_binding(
        "x",
        Some(opt_type(Type::Int)),
        value_expr,
    )]);

    let tcx = run_ok(prog);
    assert_expr_type(&tcx, value_id, &Type::Int);
}

#[test]
fn test_binding_type_tracks_effective_optional_annotation() {
    reset_expr_ids();

    let value_expr = lit_int(10);
    let value_id = get_expr_id(&value_expr);
    let prog = program(vec![let_binding(
        "x",
        Some(opt_type(Type::Int)),
        value_expr,
    )]);

    let tcx = run_ok(prog);
    assert_eq!(
        tcx.binding_type(value_id).cloned(),
        Some(opt_type(Type::Int))
    );
}

#[test]
fn test_binding_type_resolves_array_len_infer_annotation() {
    reset_expr_ids();

    let value_expr = array_literal(vec![lit_int(1), lit_int(2)]);
    let value_id = get_expr_id(&value_expr);
    let prog = program(vec![let_binding(
        "xs",
        Some(Type::Array {
            elem: Type::Int.boxed(),
            len: ArrayLen::Infer,
        }),
        value_expr,
    )]);

    let tcx = run_ok(prog);
    assert_eq!(
        tcx.binding_type(value_id).cloned(),
        Some(Type::Array {
            elem: Type::Int.boxed(),
            len: ArrayLen::Fixed(2),
        })
    );
}

#[test]
fn test_assignability_nil_to_optional_int() {
    reset_expr_ids();
    // let x: int? = nil;
    let value_expr = lit_nil();
    let value_id = get_expr_id(&value_expr);
    let prog = program(vec![let_binding(
        "x",
        Some(opt_type(Type::Int)),
        value_expr,
    )]);

    let tcx = run_ok(prog);
    assert_expr_type(&tcx, value_id, &opt_type(Type::Int));
}

#[test]
fn test_assignability_optional_to_non_optional_fails() {
    reset_expr_ids();
    // let a: int? = nil; let b: int = a;
    let a_expr = lit_nil();
    let b_expr = ident_expr("a");
    let prog = program(vec![
        let_binding("a", Some(opt_type(Type::Int)), a_expr),
        let_binding("b", Some(Type::Int), b_expr),
    ]);

    let errors = run_err(prog);
    assert!(errors.iter().any(|e| matches!(
        &e.kind,
        DiagnosticKind::MismatchedTypes { expected, found }
        if *expected == Type::Int && *found == opt_type(Type::Int)
    )));
}

#[test]
fn test_multiple_optional_assignments() {
    reset_expr_ids();
    // var e: int? = nil; e = 10;
    let nil_expr = lit_nil();
    let ten_expr = lit_int(10);
    let ten_id = get_expr_id(&ten_expr);
    let assign = assign_expr(ident_expr("e"), AssignOp::Assign, ten_expr);
    let prog = program(vec![
        var_binding("e", Some(opt_type(Type::Int)), nil_expr),
        expr_stmt(assign),
    ]);

    let tcx = run_ok(prog);
    assert_expr_type(&tcx, ten_id, &Type::Int);
}
