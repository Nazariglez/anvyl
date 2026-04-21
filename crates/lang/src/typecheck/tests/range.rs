use super::helpers::{
    assert_expr_type, call_expr, dummy_ident, expr_stmt, generic_fn_decl, get_expr_id, ident_expr,
    let_binding, lit_float, lit_int, program, range_expr, reset_expr_ids, run_err, run_ok,
};
use crate::{
    ast::{Type, TypeParam, TypeVarId},
    typecheck::{
        error::DiagnosticKind,
        range::{range_inclusive_type, range_type},
    },
};

#[test]
fn range_expr_of_ints_has_range_int_type() {
    reset_expr_ids();
    let range = range_expr(lit_int(0), false, lit_int(10));
    let range_id = get_expr_id(&range);
    let prog = program(vec![expr_stmt(range.clone())]);
    let tcx = run_ok(prog);
    assert_expr_type(&tcx, range_id, &range_type(Type::Int));
}

#[test]
fn inclusive_range_expr_of_floats_has_range_inclusive_float_type() {
    reset_expr_ids();
    let range = range_expr(lit_float(0.0), true, lit_float(10.0));
    let range_id = get_expr_id(&range);
    let prog = program(vec![expr_stmt(range.clone())]);
    let tcx = run_ok(prog);
    assert_expr_type(&tcx, range_id, &range_inclusive_type(Type::Float));
}

#[test]
fn range_expr_bounds_must_unify() {
    reset_expr_ids();
    let range = range_expr(lit_int(0), false, lit_float(1.0));
    let prog = program(vec![expr_stmt(range)]);
    let errors = run_err(prog);
    assert!(
        errors
            .iter()
            .any(|e| matches!(&e.kind, DiagnosticKind::MismatchedTypes { .. })),
        "expected mismatched types error, got: {errors:?}"
    );
}

#[test]
fn range_exprs_work_with_generics() {
    reset_expr_ids();

    let t_id = TypeVarId(0);
    let t_type = Type::Var(t_id);
    let type_params = vec![TypeParam {
        name: dummy_ident("T"),
        id: t_id,
    }];
    let wrap_fn = generic_fn_decl(
        "wrap",
        type_params,
        vec![
            ("value", t_type.clone()),
            ("range", range_type(t_type.clone())),
        ],
        t_type.clone(),
        vec![expr_stmt(ident_expr("value"))],
    );

    let call = call_expr(
        ident_expr("wrap"),
        vec![lit_int(5), range_expr(lit_int(0), false, lit_int(10))],
    );
    let call_id = get_expr_id(&call);
    let binding = let_binding("result", None, call);

    let prog = program(vec![wrap_fn, binding]);
    let tcx = run_ok(prog);
    assert_expr_type(&tcx, call_id, &Type::Int);
}
