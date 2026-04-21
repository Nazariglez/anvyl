use super::helpers::{
    assert_expr_type, assign_expr, binary_expr, cast_expr_node, expr_part, expr_stmt, get_expr_id,
    ident_expr, let_binding, lit_bool, lit_float, lit_int, lit_nil, lit_string, opt_type, program,
    reset_expr_ids, run_err, run_ok, string_interp_expr, struct_decl, struct_literal_expr,
    text_part, unary_expr, var_binding,
};
use crate::{
    ast::{AssignOp, BinaryOp, Type, UnaryOp},
    typecheck::error::DiagnosticKind,
};

// ---- binary arithmetic tests ----

#[test]
fn test_binary_arithmetic_int() {
    reset_expr_ids();
    // 1 + 2
    let expr = binary_expr(lit_int(1), BinaryOp::Add, lit_int(2));
    let expr_id = get_expr_id(&expr);
    let prog = program(vec![expr_stmt(expr)]);

    let tcx = run_ok(prog);
    assert_expr_type(&tcx, expr_id, &Type::Int);
}

#[test]
fn test_binary_arithmetic_float() {
    reset_expr_ids();
    // 1.0 + 2.0
    let expr = binary_expr(lit_float(1.0), BinaryOp::Add, lit_float(2.0));
    let expr_id = get_expr_id(&expr);
    let prog = program(vec![expr_stmt(expr)]);

    let tcx = run_ok(prog);
    assert_expr_type(&tcx, expr_id, &Type::Float);
}

#[test]
fn test_binary_arithmetic_mismatch() {
    reset_expr_ids();
    // 1 + true
    let prog = program(vec![expr_stmt(binary_expr(
        lit_int(1),
        BinaryOp::Add,
        lit_bool(true),
    ))]);

    let errors = run_err(prog);
    assert!(
        errors
            .iter()
            .any(|e| matches!(&e.kind, DiagnosticKind::MismatchedTypes { .. }))
    );
}

// ---- binary logical tests ----

#[test]
fn test_binary_logical_ok() {
    reset_expr_ids();
    // true && false
    let expr = binary_expr(lit_bool(true), BinaryOp::And, lit_bool(false));
    let expr_id = get_expr_id(&expr);
    let prog = program(vec![expr_stmt(expr)]);

    let tcx = run_ok(prog);
    assert_expr_type(&tcx, expr_id, &Type::Bool);
}

#[test]
fn test_binary_logical_invalid_operand() {
    reset_expr_ids();
    // 1 && 2
    let prog = program(vec![expr_stmt(binary_expr(
        lit_int(1),
        BinaryOp::And,
        lit_int(2),
    ))]);

    let errors = run_err(prog);
    assert!(errors.iter().any(|e| matches!(
        &e.kind,
        DiagnosticKind::InvalidOperand { operand_type, .. }
        if *operand_type == Type::Int
    )));
}

// ---- binary comparison tests ----

#[test]
fn test_binary_comparison_ok() {
    reset_expr_ids();
    // 1 < 2
    let expr = binary_expr(lit_int(1), BinaryOp::LessThan, lit_int(2));
    let expr_id = get_expr_id(&expr);
    let prog = program(vec![expr_stmt(expr)]);

    let tcx = run_ok(prog);
    assert_expr_type(&tcx, expr_id, &Type::Bool);
}

#[test]
fn test_binary_comparison_mismatch() {
    reset_expr_ids();
    // 1 < true
    let prog = program(vec![expr_stmt(binary_expr(
        lit_int(1),
        BinaryOp::LessThan,
        lit_bool(true),
    ))]);

    let errors = run_err(prog);
    assert!(
        errors
            .iter()
            .any(|e| matches!(&e.kind, DiagnosticKind::MismatchedTypes { .. }))
    );
}

#[test]
fn test_binary_string_comparison_ok() {
    reset_expr_ids();
    // "a" < "b"
    let expr = binary_expr(lit_string("a"), BinaryOp::LessThan, lit_string("b"));
    let expr_id = get_expr_id(&expr);
    let prog = program(vec![expr_stmt(expr)]);

    let tcx = run_ok(prog);
    assert_expr_type(&tcx, expr_id, &Type::Bool);
}

#[test]
fn test_binary_string_comparison_all_ops_ok() {
    reset_expr_ids();
    // "a" < "b", "z" > "a", "abc" <= "abd", "xyz" >= "xyz"
    let lt = binary_expr(lit_string("a"), BinaryOp::LessThan, lit_string("b"));
    let gt = binary_expr(lit_string("z"), BinaryOp::GreaterThan, lit_string("a"));
    let lte = binary_expr(lit_string("abc"), BinaryOp::LessThanEq, lit_string("abd"));
    let gte = binary_expr(
        lit_string("xyz"),
        BinaryOp::GreaterThanEq,
        lit_string("xyz"),
    );
    let lt_id = get_expr_id(&lt);
    let gt_id = get_expr_id(&gt);
    let lte_id = get_expr_id(&lte);
    let gte_id = get_expr_id(&gte);
    let prog = program(vec![
        expr_stmt(lt),
        expr_stmt(gt),
        expr_stmt(lte),
        expr_stmt(gte),
    ]);

    let tcx = run_ok(prog);
    assert_expr_type(&tcx, lt_id, &Type::Bool);
    assert_expr_type(&tcx, gt_id, &Type::Bool);
    assert_expr_type(&tcx, lte_id, &Type::Bool);
    assert_expr_type(&tcx, gte_id, &Type::Bool);
}

#[test]
fn test_binary_string_comparison_mismatch() {
    reset_expr_ids();
    // "a" < 1
    let prog = program(vec![expr_stmt(binary_expr(
        lit_string("a"),
        BinaryOp::LessThan,
        lit_int(1),
    ))]);

    let errors = run_err(prog);
    assert!(errors.iter().any(|e| matches!(
        &e.kind,
        DiagnosticKind::MismatchedTypes { expected, found }
        if *expected == Type::String && *found == Type::Int
    )));
}

// ---- unary tests ----

#[test]
fn test_unary_neg_int() {
    reset_expr_ids();
    // -1
    let expr = unary_expr(UnaryOp::Neg, lit_int(1));
    let expr_id = get_expr_id(&expr);
    let prog = program(vec![expr_stmt(expr)]);

    let tcx = run_ok(prog);
    assert_expr_type(&tcx, expr_id, &Type::Int);
}

#[test]
fn test_unary_neg_float() {
    reset_expr_ids();
    // -1.0
    let expr = unary_expr(UnaryOp::Neg, lit_float(1.0));
    let expr_id = get_expr_id(&expr);
    let prog = program(vec![expr_stmt(expr)]);

    let tcx = run_ok(prog);
    assert_expr_type(&tcx, expr_id, &Type::Float);
}

#[test]
fn test_unary_not_bool() {
    reset_expr_ids();
    // !true
    let expr = unary_expr(UnaryOp::Not, lit_bool(true));
    let expr_id = get_expr_id(&expr);
    let prog = program(vec![expr_stmt(expr)]);

    let tcx = run_ok(prog);
    assert_expr_type(&tcx, expr_id, &Type::Bool);
}

#[test]
fn test_unary_neg_invalid() {
    reset_expr_ids();
    // -true
    let prog = program(vec![expr_stmt(unary_expr(UnaryOp::Neg, lit_bool(true)))]);

    let errors = run_err(prog);
    assert!(errors.iter().any(|e| matches!(
        &e.kind,
        DiagnosticKind::InvalidOperand { operand_type, .. }
        if *operand_type == Type::Bool
    )));
}

#[test]
fn test_unary_not_invalid() {
    reset_expr_ids();
    // !1
    let prog = program(vec![expr_stmt(unary_expr(UnaryOp::Not, lit_int(1)))]);

    let errors = run_err(prog);
    assert!(errors.iter().any(|e| matches!(
        &e.kind,
        DiagnosticKind::InvalidOperand { operand_type, .. }
        if *operand_type == Type::Int
    )));
}

// ---- assignment tests ----

#[test]
fn test_assignment_plain_ok() {
    reset_expr_ids();
    // var x: int = 1; x = 2;
    let assign = assign_expr(ident_expr("x"), AssignOp::Assign, lit_int(2));
    let assign_id = get_expr_id(&assign);
    let prog = program(vec![
        var_binding("x", Some(Type::Int), lit_int(1)),
        expr_stmt(assign),
    ]);

    let tcx = run_ok(prog);
    assert_expr_type(&tcx, assign_id, &Type::Void);
}

#[test]
fn test_assignment_plain_mismatch() {
    reset_expr_ids();
    // var x: int = 1; x = true;
    let prog = program(vec![
        var_binding("x", Some(Type::Int), lit_int(1)),
        expr_stmt(assign_expr(
            ident_expr("x"),
            AssignOp::Assign,
            lit_bool(true),
        )),
    ]);

    let errors = run_err(prog);
    assert!(errors.iter().any(|e| matches!(
        &e.kind,
        DiagnosticKind::MismatchedTypes { expected, found }
        if *expected == Type::Int && *found == Type::Bool
    )));
}

#[test]
fn test_assignment_compound_ok() {
    reset_expr_ids();
    // var x: int = 1; x += 2;
    let assign = assign_expr(ident_expr("x"), AssignOp::AddAssign, lit_int(2));
    let assign_id = get_expr_id(&assign);
    let prog = program(vec![
        var_binding("x", Some(Type::Int), lit_int(1)),
        expr_stmt(assign),
    ]);

    let tcx = run_ok(prog);
    assert_expr_type(&tcx, assign_id, &Type::Void);
}

#[test]
fn test_assignment_string_add_assign_int_ok() {
    reset_expr_ids();
    // var x: string = "a"; x += 1;
    let assign = assign_expr(ident_expr("x"), AssignOp::AddAssign, lit_int(1));
    let assign_id = get_expr_id(&assign);
    let prog = program(vec![
        var_binding("x", Some(Type::String), lit_string("a")),
        expr_stmt(assign),
    ]);

    let stmts = run_ok(prog);
    assert_expr_type(&stmts, assign_id, &Type::Void);
}

#[test]
fn test_assignment_int_to_optional_var() {
    reset_expr_ids();
    // var x: int? = nil; x = 10;
    let nil_expr = lit_nil();
    let ten_expr = lit_int(10);
    let ten_id = get_expr_id(&ten_expr);
    let assign = assign_expr(ident_expr("x"), AssignOp::Assign, ten_expr);
    let prog = program(vec![
        var_binding("x", Some(opt_type(Type::Int)), nil_expr),
        expr_stmt(assign),
    ]);

    let tcx = run_ok(prog);
    assert_expr_type(&tcx, ten_id, &Type::Int);
}

#[test]
fn test_assignment_string_to_optional_string() {
    reset_expr_ids();
    // var c: string? = nil; c = "whatever";
    let nil_expr = lit_nil();
    let str_expr = lit_string("whatever");
    let str_id = get_expr_id(&str_expr);
    let assign = assign_expr(ident_expr("c"), AssignOp::Assign, str_expr);
    let prog = program(vec![
        var_binding("c", Some(opt_type(Type::String)), nil_expr),
        expr_stmt(assign),
    ]);

    let tcx = run_ok(prog);
    assert_expr_type(&tcx, str_id, &Type::String);
}

#[test]
fn test_assignment_float_to_optional_float() {
    reset_expr_ids();
    // var d: float? = 10.0; d = nil;
    let float_expr = lit_float(10.0);
    let nil_expr = lit_nil();
    let assign = assign_expr(ident_expr("d"), AssignOp::Assign, nil_expr);
    let prog = program(vec![
        var_binding("d", Some(opt_type(Type::Float)), float_expr),
        expr_stmt(assign),
    ]);

    let _tcx = run_ok(prog);
}

// ---- coalesce tests ----

#[test]
fn test_coalesce_optional_with_concrete_fallback() {
    reset_expr_ids();
    // let a: int? = nil;
    // let x: int = a ?? 10;
    let a_expr = lit_nil();
    let a_binding = let_binding("a", Some(opt_type(Type::Int)), a_expr);
    let coalesce = binary_expr(ident_expr("a"), BinaryOp::Coalesce, lit_int(10));
    let coalesce_id = get_expr_id(&coalesce);
    let x_binding = let_binding("x", Some(Type::Int), coalesce);
    let prog = program(vec![a_binding, x_binding]);

    let tcx = run_ok(prog);
    assert_expr_type(&tcx, coalesce_id, &Type::Int);
}

#[test]
fn test_coalesce_non_optional_left_error() {
    reset_expr_ids();
    // let x = 10 ?? 20;
    let coalesce = binary_expr(lit_int(10), BinaryOp::Coalesce, lit_int(20));
    let prog = program(vec![let_binding("x", None, coalesce)]);

    let errors = run_err(prog);
    assert!(errors.iter().any(|e| matches!(
        &e.kind,
        DiagnosticKind::InvalidOperand { op, operand_type }
        if op == "??" && *operand_type == Type::Int
    )));
}

#[test]
fn test_coalesce_mismatched_types() {
    reset_expr_ids();
    // let x: int? = nil;
    // let y = x ?? "s"; // int? ?? string should error
    let x_binding = let_binding("x", Some(opt_type(Type::Int)), lit_nil());
    let coalesce = binary_expr(ident_expr("x"), BinaryOp::Coalesce, lit_string("s"));
    let y_binding = let_binding("y", None, coalesce);
    let prog = program(vec![x_binding, y_binding]);

    let errors = run_err(prog);
    assert!(errors.iter().any(|e| matches!(
        &e.kind,
        DiagnosticKind::MismatchedTypes { expected, found }
        if *expected == Type::Int && *found == Type::String
    )));
}

#[test]
fn test_coalesce_nil_with_int() {
    reset_expr_ids();
    // let a: int = nil ?? 10;
    let nil_expr = lit_nil();
    let nil_id = get_expr_id(&nil_expr);
    let ten_expr = lit_int(10);
    let coalesce = binary_expr(nil_expr, BinaryOp::Coalesce, ten_expr);
    let coalesce_id = get_expr_id(&coalesce);
    let prog = program(vec![let_binding("a", Some(Type::Int), coalesce)]);

    let tcx = run_ok(prog);
    assert_expr_type(&tcx, nil_id, &opt_type(Type::Int));
    assert_expr_type(&tcx, coalesce_id, &Type::Int);
}

#[test]
fn test_coalesce_string_with_string_error() {
    reset_expr_ids();
    // let b = "nice" ?? "other";
    let nice_expr = lit_string("nice");
    let other_expr = lit_string("other");
    let coalesce = binary_expr(nice_expr, BinaryOp::Coalesce, other_expr);
    let prog = program(vec![let_binding("b", None, coalesce)]);

    let errors = run_err(prog);
    assert!(errors.iter().any(|e| matches!(
        &e.kind,
        DiagnosticKind::InvalidOperand { op, operand_type }
        if op == "??" && *operand_type == Type::String
    )));
}

#[test]
fn test_coalesce_mismatched_inner_types() {
    reset_expr_ids();
    // let a: int? = nil ?? true;
    let nil_expr = lit_nil();
    let bool_expr = lit_bool(true);
    let coalesce = binary_expr(nil_expr, BinaryOp::Coalesce, bool_expr);
    let prog = program(vec![let_binding("a", Some(opt_type(Type::Int)), coalesce)]);

    let errors = run_err(prog);
    assert!(
        errors
            .iter()
            .any(|e| matches!(&e.kind, DiagnosticKind::MismatchedTypes { .. }))
    );
}

#[test]
fn test_coalesce_optional_string_with_string() {
    reset_expr_ids();
    // let a: string? = nil;
    // let b: string = a ?? "fallback";
    let a_expr = lit_nil();
    let a_binding = let_binding("a", Some(opt_type(Type::String)), a_expr);
    let coalesce = binary_expr(ident_expr("a"), BinaryOp::Coalesce, lit_string("fallback"));
    let coalesce_id = get_expr_id(&coalesce);
    let b_binding = let_binding("b", Some(Type::String), coalesce);
    let prog = program(vec![a_binding, b_binding]);

    let tcx = run_ok(prog);
    assert_expr_type(&tcx, coalesce_id, &Type::String);
}

#[test]
fn test_coalesce_optional_int_with_float_error() {
    reset_expr_ids();
    // let a: int? = nil;
    // let b = a ?? 1.5;  // error: int? ?? float mismatch
    let a_binding = let_binding("a", Some(opt_type(Type::Int)), lit_nil());
    let coalesce = binary_expr(ident_expr("a"), BinaryOp::Coalesce, lit_float(1.5));
    let b_binding = let_binding("b", None, coalesce);
    let prog = program(vec![a_binding, b_binding]);

    let errors = run_err(prog);
    assert!(errors.iter().any(|e| matches!(
        &e.kind,
        DiagnosticKind::MismatchedTypes { expected, found }
        if *expected == Type::Int && *found == Type::Float
    )));
}

// ---- string concat tests ----

#[test]
fn test_binary_string_concat() {
    reset_expr_ids();
    // "a" + "b"
    let expr = binary_expr(lit_string("a"), BinaryOp::Add, lit_string("b"));
    let expr_id = get_expr_id(&expr);
    let prog = program(vec![expr_stmt(expr)]);

    let tcx = run_ok(prog);
    assert_expr_type(&tcx, expr_id, &Type::String);
}

#[test]
fn test_binary_string_sub_err() {
    reset_expr_ids();
    // "a" - "b"
    let prog = program(vec![expr_stmt(binary_expr(
        lit_string("a"),
        BinaryOp::Sub,
        lit_string("b"),
    ))]);

    let errors = run_err(prog);
    assert!(errors.iter().any(|e| matches!(
        &e.kind,
        DiagnosticKind::InvalidOperand { op, operand_type }
        if op == "-" && *operand_type == Type::String
    )));
}

#[test]
fn test_binary_string_add_int_ok() {
    reset_expr_ids();
    // "a" + 1
    let expr = binary_expr(lit_string("a"), BinaryOp::Add, lit_int(1));
    let expr_id = get_expr_id(&expr);
    let prog = program(vec![expr_stmt(expr)]);

    let stmts = run_ok(prog);
    assert_expr_type(&stmts, expr_id, &Type::String);
}

#[test]
fn test_assignment_string_add_assign_ok() {
    reset_expr_ids();
    // var s: string = "x"; s += "y";
    let assign = assign_expr(ident_expr("s"), AssignOp::AddAssign, lit_string("y"));
    let assign_id = get_expr_id(&assign);
    let prog = program(vec![
        var_binding("s", Some(Type::String), lit_string("x")),
        expr_stmt(assign),
    ]);

    let tcx = run_ok(prog);
    assert_expr_type(&tcx, assign_id, &Type::Void);
}

#[test]
fn test_assignment_string_sub_assign_err() {
    reset_expr_ids();
    // var s: string = "x"; s -= "y";
    let prog = program(vec![
        var_binding("s", Some(Type::String), lit_string("x")),
        expr_stmt(assign_expr(
            ident_expr("s"),
            AssignOp::SubAssign,
            lit_string("y"),
        )),
    ]);

    let errors = run_err(prog);
    assert!(errors.iter().any(|e| matches!(
        &e.kind,
        DiagnosticKind::InvalidOperand { op, operand_type }
        if op == "-=" && *operand_type == Type::String
    )));
}

#[test]
fn test_binary_string_add_float_ok() {
    reset_expr_ids();
    // "val: " + 3.14
    let expr = binary_expr(
        lit_string("val: "),
        BinaryOp::Add,
        lit_float(std::f64::consts::PI),
    );
    let expr_id = get_expr_id(&expr);
    let prog = program(vec![expr_stmt(expr)]);

    let stmts = run_ok(prog);
    assert_expr_type(&stmts, expr_id, &Type::String);
}

#[test]
fn test_binary_string_add_bool_ok() {
    reset_expr_ids();
    // "flag: " + true
    let expr = binary_expr(lit_string("flag: "), BinaryOp::Add, lit_bool(true));
    let expr_id = get_expr_id(&expr);
    let prog = program(vec![expr_stmt(expr)]);

    let stmts = run_ok(prog);
    assert_expr_type(&stmts, expr_id, &Type::String);
}

#[test]
fn test_binary_int_add_string_ok() {
    reset_expr_ids();
    // 10 + " points"
    let expr = binary_expr(lit_int(10), BinaryOp::Add, lit_string(" points"));
    let expr_id = get_expr_id(&expr);
    let prog = program(vec![expr_stmt(expr)]);

    let stmts = run_ok(prog);
    assert_expr_type(&stmts, expr_id, &Type::String);
}

#[test]
fn test_binary_string_add_struct_err() {
    reset_expr_ids();
    // struct Point { x: int } let a = "pos: " + Point { x: 0 };
    let point_decl = struct_decl("Point", vec![("x", Type::Int)], vec![]);
    let prog = program(vec![
        point_decl,
        expr_stmt(binary_expr(
            lit_string("pos: "),
            BinaryOp::Add,
            struct_literal_expr("Point", vec![("x", lit_int(0))]),
        )),
    ]);

    let errors = run_err(prog);
    assert!(errors.iter().any(|e| matches!(
        &e.kind,
        DiagnosticKind::InvalidOperand { op, .. }
        if op == "+"
    )));
}

#[test]
fn test_assignment_string_add_assign_int_ok_var() {
    reset_expr_ids();
    // var s = "x"; s += 10;
    let assign = assign_expr(ident_expr("s"), AssignOp::AddAssign, lit_int(10));
    let assign_id = get_expr_id(&assign);
    let prog = program(vec![
        var_binding("s", Some(Type::String), lit_string("x")),
        expr_stmt(assign),
    ]);

    let tcx = run_ok(prog);
    assert_expr_type(&tcx, assign_id, &Type::Void);
}

#[test]
fn test_assignment_string_add_assign_bool_ok() {
    reset_expr_ids();
    // var s = "x"; s += true;
    let assign = assign_expr(ident_expr("s"), AssignOp::AddAssign, lit_bool(true));
    let assign_id = get_expr_id(&assign);
    let prog = program(vec![
        var_binding("s", Some(Type::String), lit_string("x")),
        expr_stmt(assign),
    ]);

    let tcx = run_ok(prog);
    assert_expr_type(&tcx, assign_id, &Type::Void);
}

// ---- string interpolation tests ----

#[test]
fn test_string_interp_text_only_ok() {
    reset_expr_ids();
    let interp = string_interp_expr(vec![text_part("hello")]);
    let interp_id = get_expr_id(&interp);
    let prog = program(vec![expr_stmt(interp)]);

    let tcx = run_ok(prog);
    assert_expr_type(&tcx, interp_id, &Type::String);
}

#[test]
fn test_string_interp_with_string_var_ok() {
    reset_expr_ids();
    let interp = string_interp_expr(vec![text_part("val: "), expr_part(ident_expr("x"))]);
    let interp_id = get_expr_id(&interp);
    let prog = program(vec![
        let_binding("x", Some(Type::String), lit_string("a")),
        expr_stmt(interp),
    ]);

    let tcx = run_ok(prog);
    assert_expr_type(&tcx, interp_id, &Type::String);
}

#[test]
fn test_string_interp_with_int_ok() {
    reset_expr_ids();
    let interp = string_interp_expr(vec![text_part("n="), expr_part(lit_int(42))]);
    let interp_id = get_expr_id(&interp);
    let prog = program(vec![expr_stmt(interp)]);

    let tcx = run_ok(prog);
    assert_expr_type(&tcx, interp_id, &Type::String);
}

#[test]
fn test_string_interp_with_float_ok() {
    reset_expr_ids();
    let interp = string_interp_expr(vec![
        text_part("f="),
        expr_part(lit_float(std::f64::consts::PI)),
    ]);
    let interp_id = get_expr_id(&interp);
    let prog = program(vec![expr_stmt(interp)]);

    let tcx = run_ok(prog);
    assert_expr_type(&tcx, interp_id, &Type::String);
}

#[test]
fn test_string_interp_with_bool_ok() {
    reset_expr_ids();
    let interp = string_interp_expr(vec![expr_part(lit_bool(true))]);
    let interp_id = get_expr_id(&interp);
    let prog = program(vec![expr_stmt(interp)]);

    let tcx = run_ok(prog);
    assert_expr_type(&tcx, interp_id, &Type::String);
}

#[test]
fn test_string_interp_with_struct_ok() {
    reset_expr_ids();
    let interp = string_interp_expr(vec![expr_part(ident_expr("p"))]);
    let interp_id = get_expr_id(&interp);
    let prog = program(vec![
        struct_decl("Point", vec![("x", Type::Int), ("y", Type::Int)], vec![]),
        let_binding(
            "p",
            None,
            struct_literal_expr("Point", vec![("x", lit_int(1)), ("y", lit_int(2))]),
        ),
        expr_stmt(interp),
    ]);

    let tcx = run_ok(prog);
    assert_expr_type(&tcx, interp_id, &Type::String);
}

// ---- cast expression tests ----

#[test]
fn test_cast_int_to_float_ok() {
    reset_expr_ids();
    let expr = cast_expr_node(lit_int(42), Type::Float);
    let expr_id = get_expr_id(&expr);
    let prog = program(vec![expr_stmt(expr)]);
    let tcx = run_ok(prog);
    assert_expr_type(&tcx, expr_id, &Type::Float);
}

#[test]
fn test_cast_float_to_int_ok() {
    reset_expr_ids();
    let expr = cast_expr_node(lit_float(std::f64::consts::PI), Type::Int);
    let expr_id = get_expr_id(&expr);
    let prog = program(vec![expr_stmt(expr)]);
    let tcx = run_ok(prog);
    assert_expr_type(&tcx, expr_id, &Type::Int);
}

#[test]
fn test_cast_same_type_int_ok() {
    reset_expr_ids();
    let expr = cast_expr_node(lit_int(10), Type::Int);
    let expr_id = get_expr_id(&expr);
    let prog = program(vec![expr_stmt(expr)]);
    let tcx = run_ok(prog);
    assert_expr_type(&tcx, expr_id, &Type::Int);
}

#[test]
fn test_cast_same_type_float_ok() {
    reset_expr_ids();
    let expr = cast_expr_node(lit_float(1.0), Type::Float);
    let expr_id = get_expr_id(&expr);
    let prog = program(vec![expr_stmt(expr)]);
    let tcx = run_ok(prog);
    assert_expr_type(&tcx, expr_id, &Type::Float);
}

#[test]
fn test_cast_bool_to_int_err() {
    reset_expr_ids();
    let prog = program(vec![expr_stmt(cast_expr_node(lit_bool(true), Type::Int))]);
    let errors = run_err(prog);
    assert!(errors.iter().any(|e| matches!(
        &e.kind,
        DiagnosticKind::InvalidCast { from, to }
        if *from == Type::Bool && *to == Type::Int
    )));
}

#[test]
fn test_cast_string_to_int_err() {
    reset_expr_ids();
    let prog = program(vec![expr_stmt(cast_expr_node(lit_string("hi"), Type::Int))]);
    let errors = run_err(prog);
    assert!(errors.iter().any(|e| matches!(
        &e.kind,
        DiagnosticKind::InvalidCast { from, to }
        if *from == Type::String && *to == Type::Int
    )));
}

#[test]
fn test_cast_int_to_bool_err() {
    reset_expr_ids();
    let prog = program(vec![expr_stmt(cast_expr_node(lit_int(1), Type::Bool))]);
    let errors = run_err(prog);
    assert!(errors.iter().any(|e| matches!(
        &e.kind,
        DiagnosticKind::InvalidCast { from, to }
        if *from == Type::Int && *to == Type::Bool
    )));
}

#[test]
fn test_cast_int_to_string_err() {
    reset_expr_ids();
    let prog = program(vec![expr_stmt(cast_expr_node(lit_int(1), Type::String))]);
    let errors = run_err(prog);
    assert!(errors.iter().any(|e| matches!(
        &e.kind,
        DiagnosticKind::InvalidCast { from, to }
        if *from == Type::Int && *to == Type::String
    )));
}
