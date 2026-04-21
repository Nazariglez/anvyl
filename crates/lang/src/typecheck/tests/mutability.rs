use super::helpers::{
    assign_expr, call_expr, dummy_ident, expr_stmt, field_expr, fn_decl, fn_decl_var_params,
    ident_expr, let_binding, lit_int, method, program, reset_expr_ids, run_err, run_ok,
    struct_decl, struct_literal_expr, var_binding,
};
use crate::{
    ast::{AssignOp, MethodReceiver, Type},
    typecheck::error::DiagnosticKind,
};

// ---- assignment mutability ----

#[test]
fn test_let_assign_ident_err() {
    reset_expr_ids();
    // let x = 10; x = 20;
    let prog = program(vec![
        let_binding("x", None, lit_int(10)),
        expr_stmt(assign_expr(ident_expr("x"), AssignOp::Assign, lit_int(20))),
    ]);

    let errors = run_err(prog);
    assert!(
        errors.iter().any(|e| matches!(
            &e.kind,
            DiagnosticKind::ImmutableAssignment { name } if name == &dummy_ident("x")
        )),
        "expected ImmutableAssignment for x, got: {errors:?}"
    );
}

#[test]
fn test_var_assign_ident_ok() {
    reset_expr_ids();
    // var x = 10; x = 20;
    let prog = program(vec![
        var_binding("x", None, lit_int(10)),
        expr_stmt(assign_expr(ident_expr("x"), AssignOp::Assign, lit_int(20))),
    ]);

    run_ok(prog);
}

#[test]
fn test_let_compound_assign_err() {
    reset_expr_ids();
    // let x = 10; x += 5;
    let prog = program(vec![
        let_binding("x", None, lit_int(10)),
        expr_stmt(assign_expr(
            ident_expr("x"),
            AssignOp::AddAssign,
            lit_int(5),
        )),
    ]);

    let errors = run_err(prog);
    assert!(
        errors.iter().any(|e| matches!(
            &e.kind,
            DiagnosticKind::ImmutableAssignment { name } if name == &dummy_ident("x")
        )),
        "expected ImmutableAssignment for x, got: {errors:?}"
    );
}

#[test]
fn test_var_compound_assign_ok() {
    reset_expr_ids();
    // var x = 10; x += 5;
    let prog = program(vec![
        var_binding("x", None, lit_int(10)),
        expr_stmt(assign_expr(
            ident_expr("x"),
            AssignOp::AddAssign,
            lit_int(5),
        )),
    ]);

    run_ok(prog);
}

#[test]
fn test_let_field_assign_err() {
    reset_expr_ids();
    // struct Point { x: int } let p = Point { x: 0 }; p.x = 10;
    let point_decl = struct_decl("Point", vec![("x", Type::Int)], vec![]);
    let p_binding = let_binding(
        "p",
        None,
        struct_literal_expr("Point", vec![("x", lit_int(0))]),
    );
    let assign = expr_stmt(assign_expr(
        field_expr(ident_expr("p"), "x"),
        AssignOp::Assign,
        lit_int(10),
    ));
    let prog = program(vec![point_decl, p_binding, assign]);

    let errors = run_err(prog);
    assert!(
        errors.iter().any(|e| matches!(
            &e.kind,
            DiagnosticKind::ImmutableAssignment { name } if name == &dummy_ident("p")
        )),
        "expected ImmutableAssignment for p, got: {errors:?}"
    );
}

#[test]
fn test_var_field_assign_ok() {
    reset_expr_ids();
    // struct Point { x: int } var p = Point { x: 0 }; p.x = 10;
    let point_decl = struct_decl("Point", vec![("x", Type::Int)], vec![]);
    let p_binding = var_binding(
        "p",
        None,
        struct_literal_expr("Point", vec![("x", lit_int(0))]),
    );
    let assign = expr_stmt(assign_expr(
        field_expr(ident_expr("p"), "x"),
        AssignOp::Assign,
        lit_int(10),
    ));
    let prog = program(vec![point_decl, p_binding, assign]);

    run_ok(prog);
}

// ---- readonly/var param assignment ----

#[test]
fn test_readonly_param_assign_err() {
    reset_expr_ids();
    // fn f(x: int) { x = 10; }
    let fn_def = fn_decl(
        "f",
        vec![("x", Type::Int)],
        Type::Void,
        vec![expr_stmt(assign_expr(
            ident_expr("x"),
            AssignOp::Assign,
            lit_int(10),
        ))],
    );
    let prog = program(vec![fn_def]);

    let errors = run_err(prog);
    assert!(
        errors.iter().any(|e| matches!(
            &e.kind,
            DiagnosticKind::ImmutableAssignment { name } if name == &dummy_ident("x")
        )),
        "expected ImmutableAssignment for x, got: {errors:?}"
    );
}

#[test]
fn test_var_param_assign_ok() {
    reset_expr_ids();
    // fn f(var x: int) { x = 10; }
    let fn_def = fn_decl_var_params(
        "f",
        vec![("x", Type::Int, true)],
        Type::Void,
        vec![expr_stmt(assign_expr(
            ident_expr("x"),
            AssignOp::Assign,
            lit_int(10),
        ))],
    );
    let prog = program(vec![fn_def]);

    run_ok(prog);
}

// ---- call-site var param enforcement ----

#[test]
fn test_var_param_var_arg_ok() {
    reset_expr_ids();
    // fn f(var x: int) {} var n = 1; f(n);
    let fn_def = fn_decl_var_params("f", vec![("x", Type::Int, true)], Type::Void, vec![]);
    let prog = program(vec![
        fn_def,
        var_binding("n", None, lit_int(1)),
        expr_stmt(call_expr(ident_expr("f"), vec![ident_expr("n")])),
    ]);

    run_ok(prog);
}

#[test]
fn test_var_param_let_arg_err() {
    reset_expr_ids();
    // fn f(var x: int) {} let n = 1; f(n);
    let fn_def = fn_decl_var_params("f", vec![("x", Type::Int, true)], Type::Void, vec![]);
    let prog = program(vec![
        fn_def,
        let_binding("n", None, lit_int(1)),
        expr_stmt(call_expr(ident_expr("f"), vec![ident_expr("n")])),
    ]);

    let errors = run_err(prog);
    assert!(
        errors.iter().any(|e| matches!(
            &e.kind,
            DiagnosticKind::VarParamImmutableBinding { param, binding }
                if param == &dummy_ident("x") && binding == &dummy_ident("n")
        )),
        "expected VarParamImmutableBinding for x/n, got: {errors:?}"
    );
}

#[test]
fn test_var_param_literal_err() {
    reset_expr_ids();
    // fn f(var x: int) {} f(10);
    let fn_def = fn_decl_var_params("f", vec![("x", Type::Int, true)], Type::Void, vec![]);
    let prog = program(vec![
        fn_def,
        expr_stmt(call_expr(ident_expr("f"), vec![lit_int(10)])),
    ]);

    let errors = run_err(prog);
    assert!(
        errors.iter().any(|e| matches!(
            &e.kind,
            DiagnosticKind::VarParamNotLvalue { param } if param == &dummy_ident("x")
        )),
        "expected VarParamNotLvalue for x, got: {errors:?}"
    );
}

#[test]
fn test_var_param_expr_err() {
    use super::helpers::binary_expr;
    use crate::ast::BinaryOp;
    reset_expr_ids();
    // fn f(var x: int) {} var a = 1; f(a + 1);

    let fn_def = fn_decl_var_params("f", vec![("x", Type::Int, true)], Type::Void, vec![]);
    let add_expr = binary_expr(ident_expr("a"), BinaryOp::Add, lit_int(1));
    let prog = program(vec![
        fn_def,
        var_binding("a", None, lit_int(1)),
        expr_stmt(call_expr(ident_expr("f"), vec![add_expr])),
    ]);

    let errors = run_err(prog);
    assert!(
        errors.iter().any(|e| matches!(
            &e.kind,
            DiagnosticKind::VarParamNotLvalue { param } if param == &dummy_ident("x")
        )),
        "expected VarParamNotLvalue for x, got: {errors:?}"
    );
}

// ---- method receiver mutability ----

#[test]
fn test_readonly_self_mutation_err() {
    use super::helpers::return_stmt;
    reset_expr_ids();
    // struct Counter { value: int
    //   fn get(self) -> int { self.value = 10; return self.value; }
    // }
    // let c = Counter { value: 0 };
    // c.get();

    let get_body = vec![
        expr_stmt(assign_expr(
            field_expr(ident_expr("self"), "value"),
            AssignOp::Assign,
            lit_int(10),
        )),
        return_stmt(Some(field_expr(ident_expr("self"), "value"))),
    ];
    let counter_decl = struct_decl(
        "Counter",
        vec![("value", Type::Int)],
        vec![method(
            "get",
            Some(MethodReceiver::Value),
            vec![],
            Type::Int,
            get_body,
        )],
    );
    let c_binding = let_binding(
        "c",
        None,
        struct_literal_expr("Counter", vec![("value", lit_int(0))]),
    );
    let call = expr_stmt(call_expr(field_expr(ident_expr("c"), "get"), vec![]));
    let prog = program(vec![counter_decl, c_binding, call]);

    let errors = run_err(prog);
    assert!(
        errors
            .iter()
            .any(|e| matches!(&e.kind, DiagnosticKind::ReadonlySelfMutation { .. })),
        "expected ReadonlySelfMutation, got: {errors:?}"
    );
}

#[test]
fn test_var_self_mutation_ok() {
    use super::helpers::binary_expr;
    use crate::ast::BinaryOp;
    reset_expr_ids();
    // struct Counter { value: int
    //   fn increment(var self) { self.value = self.value + 1; }
    // }
    // var c = Counter { value: 0 };
    // c.increment();

    let increment_body = vec![expr_stmt(assign_expr(
        field_expr(ident_expr("self"), "value"),
        AssignOp::Assign,
        binary_expr(
            field_expr(ident_expr("self"), "value"),
            BinaryOp::Add,
            lit_int(1),
        ),
    ))];
    let counter_decl = struct_decl(
        "Counter",
        vec![("value", Type::Int)],
        vec![method(
            "increment",
            Some(MethodReceiver::Var),
            vec![],
            Type::Void,
            increment_body,
        )],
    );
    let c_binding = var_binding(
        "c",
        None,
        struct_literal_expr("Counter", vec![("value", lit_int(0))]),
    );
    let call = expr_stmt(call_expr(field_expr(ident_expr("c"), "increment"), vec![]));
    let prog = program(vec![counter_decl, c_binding, call]);

    run_ok(prog);
}

#[test]
fn test_var_self_on_let_binding_err() {
    use super::helpers::binary_expr;
    use crate::ast::BinaryOp;
    reset_expr_ids();
    // struct Counter { value: int
    //   fn increment(var self) { self.value = self.value + 1; }
    // }
    // let c = Counter { value: 0 };
    // c.increment();   -- should fail: c is immutable

    let increment_body = vec![expr_stmt(assign_expr(
        field_expr(ident_expr("self"), "value"),
        AssignOp::Assign,
        binary_expr(
            field_expr(ident_expr("self"), "value"),
            BinaryOp::Add,
            lit_int(1),
        ),
    ))];
    let counter_decl = struct_decl(
        "Counter",
        vec![("value", Type::Int)],
        vec![method(
            "increment",
            Some(MethodReceiver::Var),
            vec![],
            Type::Void,
            increment_body,
        )],
    );
    let c_binding = let_binding(
        "c",
        None,
        struct_literal_expr("Counter", vec![("value", lit_int(0))]),
    );
    let call = expr_stmt(call_expr(field_expr(ident_expr("c"), "increment"), vec![]));
    let prog = program(vec![counter_decl, c_binding, call]);

    let errors = run_err(prog);
    assert!(
        errors
            .iter()
            .any(|e| matches!(&e.kind, DiagnosticKind::MutatingMethodOnImmutable { .. })),
        "expected MutatingMethodOnImmutable, got: {errors:?}"
    );
}

#[test]
fn test_var_self_on_var_binding_ok() {
    use super::helpers::binary_expr;
    use crate::ast::BinaryOp;
    reset_expr_ids();
    // struct Counter { value: int
    //   fn increment(var self) { self.value = self.value + 1; }
    // }
    // var c = Counter { value: 0 };
    // c.increment();   -- ok: c is mutable

    let increment_body = vec![expr_stmt(assign_expr(
        field_expr(ident_expr("self"), "value"),
        AssignOp::Assign,
        binary_expr(
            field_expr(ident_expr("self"), "value"),
            BinaryOp::Add,
            lit_int(1),
        ),
    ))];
    let counter_decl = struct_decl(
        "Counter",
        vec![("value", Type::Int)],
        vec![method(
            "increment",
            Some(MethodReceiver::Var),
            vec![],
            Type::Void,
            increment_body,
        )],
    );
    let c_binding = var_binding(
        "c",
        None,
        struct_literal_expr("Counter", vec![("value", lit_int(0))]),
    );
    let call = expr_stmt(call_expr(field_expr(ident_expr("c"), "increment"), vec![]));
    let prog = program(vec![counter_decl, c_binding, call]);

    run_ok(prog);
}
