use super::helpers::{
    assert_expr_type, binary_expr, call_expr, call_expr_with_type_args, dummy_ident, expr_stmt,
    fn_decl, generic_fn_decl, get_expr_id, ident_expr, let_binding, lit_bool, lit_float, lit_int,
    lit_string, program, reset_expr_ids, return_stmt, run_err, run_ok,
};
use crate::{
    ast::{BinaryOp, FuncParam, Type, TypeParam, TypeVarId},
    typecheck::error::DiagnosticKind,
};

#[test]
fn test_call_happy_path() {
    reset_expr_ids();
    // fn f(a: int, b: bool) -> string { return "ok"; }
    // f(1, true);
    let fn_def = fn_decl(
        "f",
        vec![("a", Type::Int), ("b", Type::Bool)],
        Type::String,
        vec![return_stmt(Some(lit_string("ok")))],
    );
    let call_expr_node = call_expr(ident_expr("f"), vec![lit_int(1), lit_bool(true)]);
    let call_id = get_expr_id(&call_expr_node);
    let prog = program(vec![fn_def, expr_stmt(call_expr_node)]);

    let tcx = run_ok(prog);
    assert_expr_type(&tcx, call_id, &Type::String);
}

#[test]
fn test_call_arity_mismatch_too_few() {
    reset_expr_ids();
    // fn f(a: int, b: bool) -> string { return "ok"; }
    // f(1);
    let fn_def = fn_decl(
        "f",
        vec![("a", Type::Int), ("b", Type::Bool)],
        Type::String,
        vec![return_stmt(Some(lit_string("ok")))],
    );
    let prog = program(vec![
        fn_def,
        expr_stmt(call_expr(ident_expr("f"), vec![lit_int(1)])),
    ]);

    let errors = run_err(prog);
    assert!(
        errors
            .iter()
            .any(|e| matches!(&e.kind, DiagnosticKind::MismatchedTypes { .. }))
    );
}

#[test]
fn test_call_arity_mismatch_too_many() {
    reset_expr_ids();
    // fn f(a: int, b: bool) -> string { return "ok"; }
    // f(1, true, 3);
    let fn_def = fn_decl(
        "f",
        vec![("a", Type::Int), ("b", Type::Bool)],
        Type::String,
        vec![return_stmt(Some(lit_string("ok")))],
    );
    let prog = program(vec![
        fn_def,
        expr_stmt(call_expr(
            ident_expr("f"),
            vec![lit_int(1), lit_bool(true), lit_int(3)],
        )),
    ]);

    let errors = run_err(prog);
    assert!(
        errors
            .iter()
            .any(|e| matches!(&e.kind, DiagnosticKind::MismatchedTypes { .. }))
    );
}

#[test]
fn test_call_argument_type_mismatch() {
    reset_expr_ids();
    // fn f(a: int, b: bool) -> string { return "ok"; }
    // f("nope", true);
    let fn_def = fn_decl(
        "f",
        vec![("a", Type::Int), ("b", Type::Bool)],
        Type::String,
        vec![return_stmt(Some(lit_string("ok")))],
    );
    let prog = program(vec![
        fn_def,
        expr_stmt(call_expr(
            ident_expr("f"),
            vec![lit_string("nope"), lit_bool(true)],
        )),
    ]);

    let errors = run_err(prog);
    assert!(!errors.is_empty());
    assert!(
        errors.iter().any(|e| matches!(
            &e.kind,
            DiagnosticKind::MismatchedTypes { expected, found }
            if (*expected == Type::Int && *found == Type::String) ||
               (*expected == Type::String && *found == Type::Int)
        )),
        "Expected MismatchedTypes error (int/string mismatch), got: {errors:?}"
    );
}

#[test]
fn test_function_call_through_variable() {
    reset_expr_ids();
    // fn f(a: int) -> int { return a; }
    // let g: fn(int) -> int = f;
    // g(42);
    let fn_def = fn_decl(
        "f",
        vec![("a", Type::Int)],
        Type::Int,
        vec![return_stmt(Some(ident_expr("a")))],
    );
    let fn_type = Type::Func {
        params: vec![FuncParam::immut(Type::Int)],
        ret: Box::new(Type::Int),
    };
    let g_binding = let_binding("g", Some(fn_type), ident_expr("f"));
    let call_expr_node = call_expr(ident_expr("g"), vec![lit_int(42)]);
    let call_id = get_expr_id(&call_expr_node);
    let prog = program(vec![fn_def, g_binding, expr_stmt(call_expr_node)]);

    let tcx = run_ok(prog);
    assert_expr_type(&tcx, call_id, &Type::Int);
}

// ---- generic function tests ----

#[test]
fn test_template_generic_add_with_int_ok() {
    reset_expr_ids();

    // fn add<T>(a: T, b: T) -> T { a + b }
    // let x = add(1, 2);
    let t_id = TypeVarId(0);
    let t_type = Type::Var(t_id);
    let type_params = vec![TypeParam {
        name: dummy_ident("T"),
        id: t_id,
    }];

    let add_fn = generic_fn_decl(
        "add",
        type_params,
        vec![("a", t_type.clone()), ("b", t_type.clone())],
        t_type.clone(),
        vec![expr_stmt(binary_expr(
            ident_expr("a"),
            BinaryOp::Add,
            ident_expr("b"),
        ))],
    );

    let call = call_expr(ident_expr("add"), vec![lit_int(1), lit_int(2)]);
    let call_id = get_expr_id(&call);
    let binding = let_binding("x", None, call);

    let prog = program(vec![add_fn, binding]);
    let tcx = run_ok(prog);
    assert_expr_type(&tcx, call_id, &Type::Int);
}

#[test]
fn test_template_generic_add_with_float_ok() {
    reset_expr_ids();

    // fn add<T>(a: T, b: T) -> T { a + b }
    // let x = add(1.0, 2.0);
    let t_id = TypeVarId(0);
    let t_type = Type::Var(t_id);
    let type_params = vec![TypeParam {
        name: dummy_ident("T"),
        id: t_id,
    }];

    let add_fn = generic_fn_decl(
        "add",
        type_params,
        vec![("a", t_type.clone()), ("b", t_type.clone())],
        t_type.clone(),
        vec![expr_stmt(binary_expr(
            ident_expr("a"),
            BinaryOp::Add,
            ident_expr("b"),
        ))],
    );

    let call = call_expr(ident_expr("add"), vec![lit_float(1.0), lit_float(2.0)]);
    let call_id = get_expr_id(&call);
    let binding = let_binding("x", None, call);

    let prog = program(vec![add_fn, binding]);
    let tcx = run_ok(prog);
    assert_expr_type(&tcx, call_id, &Type::Float);
}

#[test]
fn test_template_generic_add_with_bool_err() {
    reset_expr_ids();

    // fn add<T>(a: T, b: T) -> T { a + b }
    // let x = add(true, false); // Error: bool + bool is invalid
    let t_id = TypeVarId(0);
    let t_type = Type::Var(t_id);
    let type_params = vec![TypeParam {
        name: dummy_ident("T"),
        id: t_id,
    }];

    let add_fn = generic_fn_decl(
        "add",
        type_params,
        vec![("a", t_type.clone()), ("b", t_type.clone())],
        t_type.clone(),
        vec![expr_stmt(binary_expr(
            ident_expr("a"),
            BinaryOp::Add,
            ident_expr("b"),
        ))],
    );

    let call = call_expr(ident_expr("add"), vec![lit_bool(true), lit_bool(false)]);
    let binding = let_binding("x", None, call);

    let prog = program(vec![add_fn, binding]);
    let errors = run_err(prog);

    assert!(!errors.is_empty());
    assert!(
        errors.iter().any(|e| matches!(
            &e.kind,
            DiagnosticKind::InvalidOperand { op, operand_type }
            if op == "+" && *operand_type == Type::Bool
        )),
        "Expected InvalidOperand error, got: {errors:?}"
    );
}

#[test]
fn test_template_generic_explicit_type_args_ok() {
    reset_expr_ids();

    // fn add<T>(a: T, b: T) -> T { a + b }
    // let x = add<int>(1, 2);
    let t_id = TypeVarId(0);
    let t_type = Type::Var(t_id);
    let type_params = vec![TypeParam {
        name: dummy_ident("T"),
        id: t_id,
    }];

    let add_fn = generic_fn_decl(
        "add",
        type_params,
        vec![("a", t_type.clone()), ("b", t_type.clone())],
        t_type.clone(),
        vec![expr_stmt(binary_expr(
            ident_expr("a"),
            BinaryOp::Add,
            ident_expr("b"),
        ))],
    );

    let call = call_expr_with_type_args(
        ident_expr("add"),
        vec![lit_int(1), lit_int(2)],
        vec![Type::Int],
    );
    let call_id = get_expr_id(&call);
    let binding = let_binding("x", None, call);

    let prog = program(vec![add_fn, binding]);
    let tcx = run_ok(prog);
    assert_expr_type(&tcx, call_id, &Type::Int);
}

#[test]
fn test_template_generic_explicit_type_args_bool_err() {
    reset_expr_ids();

    // fn add<T>(a: T, b: T) -> T { a + b }
    // let x = add<bool>(true, false); // Error: bool + bool is invalid
    let t_id = TypeVarId(0);
    let t_type = Type::Var(t_id);
    let type_params = vec![TypeParam {
        name: dummy_ident("T"),
        id: t_id,
    }];

    let add_fn = generic_fn_decl(
        "add",
        type_params,
        vec![("a", t_type.clone()), ("b", t_type.clone())],
        t_type.clone(),
        vec![expr_stmt(binary_expr(
            ident_expr("a"),
            BinaryOp::Add,
            ident_expr("b"),
        ))],
    );

    let call = call_expr_with_type_args(
        ident_expr("add"),
        vec![lit_bool(true), lit_bool(false)],
        vec![Type::Bool],
    );
    let binding = let_binding("x", None, call);

    let prog = program(vec![add_fn, binding]);
    let errors = run_err(prog);
    assert!(!errors.is_empty());
    assert!(
        errors.iter().any(|e| matches!(
            &e.kind,
            DiagnosticKind::InvalidOperand { op, operand_type }
            if op == "+" && *operand_type == Type::Bool
        )),
        "Expected InvalidOperand error, got: {errors:?}"
    );
}

#[test]
fn test_template_generic_specialization_cache() {
    reset_expr_ids();

    // fn add<T>(a: T, b: T) -> T { a + b }
    // let x = add(1, 2);
    // let y = add(10, 20); // same instantiation should use cache
    let t_id = TypeVarId(0);
    let t_type = Type::Var(t_id);
    let type_params = vec![TypeParam {
        name: dummy_ident("T"),
        id: t_id,
    }];

    let add_fn = generic_fn_decl(
        "add",
        type_params,
        vec![("a", t_type.clone()), ("b", t_type.clone())],
        t_type.clone(),
        vec![expr_stmt(binary_expr(
            ident_expr("a"),
            BinaryOp::Add,
            ident_expr("b"),
        ))],
    );

    let call1 = call_expr(ident_expr("add"), vec![lit_int(1), lit_int(2)]);
    let call1_id = get_expr_id(&call1);
    let binding1 = let_binding("x", None, call1);

    let call2 = call_expr(ident_expr("add"), vec![lit_int(10), lit_int(20)]);
    let call2_id = get_expr_id(&call2);
    let binding2 = let_binding("y", None, call2);

    let prog = program(vec![add_fn, binding1, binding2]);
    let tcx = run_ok(prog);

    assert_expr_type(&tcx, call1_id, &Type::Int);
    assert_expr_type(&tcx, call2_id, &Type::Int);
}

#[test]
fn test_template_generic_multiple_instantiations() {
    reset_expr_ids();

    // fn add<T>(a: T, b: T) -> T { a + b }
    // let x = add(1, 2);       // T = int
    // let y = add(1.0, 2.0);   // T = float
    let t_id = TypeVarId(0);
    let t_type = Type::Var(t_id);
    let type_params = vec![TypeParam {
        name: dummy_ident("T"),
        id: t_id,
    }];

    let add_fn = generic_fn_decl(
        "add",
        type_params,
        vec![("a", t_type.clone()), ("b", t_type.clone())],
        t_type.clone(),
        vec![expr_stmt(binary_expr(
            ident_expr("a"),
            BinaryOp::Add,
            ident_expr("b"),
        ))],
    );

    let call1 = call_expr(ident_expr("add"), vec![lit_int(1), lit_int(2)]);
    let call1_id = get_expr_id(&call1);
    let binding1 = let_binding("x", None, call1);

    let call2 = call_expr(ident_expr("add"), vec![lit_float(1.0), lit_float(2.0)]);
    let call2_id = get_expr_id(&call2);
    let binding2 = let_binding("y", None, call2);

    let prog = program(vec![add_fn, binding1, binding2]);
    let tcx = run_ok(prog);

    // first call should have type int
    assert_expr_type(&tcx, call1_id, &Type::Int);
    // second call should have type float
    assert_expr_type(&tcx, call2_id, &Type::Float);
}

#[test]
fn test_template_generic_identity_ok() {
    reset_expr_ids();

    // fn identity<T>(x: T) -> T { x }
    // let a = identity(42);
    // let b = identity(true);
    let t_id = TypeVarId(0);
    let t_type = Type::Var(t_id);
    let type_params = vec![TypeParam {
        name: dummy_ident("T"),
        id: t_id,
    }];

    let identity_fn = generic_fn_decl(
        "identity",
        type_params,
        vec![("x", t_type.clone())],
        t_type.clone(),
        vec![expr_stmt(ident_expr("x"))],
    );

    let call1 = call_expr(ident_expr("identity"), vec![lit_int(42)]);
    let call1_id = get_expr_id(&call1);
    let binding1 = let_binding("a", None, call1);

    let call2 = call_expr(ident_expr("identity"), vec![lit_bool(true)]);
    let call2_id = get_expr_id(&call2);
    let binding2 = let_binding("b", None, call2);

    let prog = program(vec![identity_fn, binding1, binding2]);
    let tcx = run_ok(prog);

    // first call should have type int
    assert_expr_type(&tcx, call1_id, &Type::Int);
    // second call should have type bool
    assert_expr_type(&tcx, call2_id, &Type::Bool);
}

#[test]
fn test_println_builtin_ok() {
    reset_expr_ids();
    // println("hello") at file scope
    let call = call_expr(ident_expr("println"), vec![lit_string("hello")]);
    let call_id = get_expr_id(&call);
    let prog = program(vec![expr_stmt(call)]);
    let tcx = run_ok(prog);
    assert_expr_type(&tcx, call_id, &Type::Void);
}

#[test]
fn test_println_builtin_int_ok() {
    reset_expr_ids();
    let call = call_expr(ident_expr("println"), vec![lit_int(42)]);
    let call_id = get_expr_id(&call);
    let prog = program(vec![expr_stmt(call)]);
    let tcx = run_ok(prog);
    assert_expr_type(&tcx, call_id, &Type::Void);
}

#[test]
fn test_println_builtin_bool_ok() {
    reset_expr_ids();
    let call = call_expr(ident_expr("println"), vec![lit_bool(true)]);
    let call_id = get_expr_id(&call);
    let prog = program(vec![expr_stmt(call)]);
    let tcx = run_ok(prog);
    assert_expr_type(&tcx, call_id, &Type::Void);
}
