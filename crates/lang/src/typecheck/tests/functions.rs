use super::helpers::{
    assert_expr_type, call_expr, check_src, expr_stmt, fn_decl, get_expr_id, ident_expr,
    let_binding, lit_bool, lit_int, program, reset_expr_ids, return_stmt, run_err, run_ok,
};
use crate::{
    ast::{FuncParam, Type},
    typecheck::error::{Diagnostic, DiagnosticKind},
};

fn assert_has_mismatch(errors: &[Diagnostic], a: &Type, b: &Type) {
    let found = errors.iter().any(|e| {
        matches!(&e.kind, DiagnosticKind::MismatchedTypes { expected, found }
            if (*expected == *a && *found == *b) || (*expected == *b && *found == *a))
    });
    assert!(
        found,
        "Expected MismatchedTypes error with {a:?} and {b:?}, got: {errors:?}"
    );
}

// ---- return tests ----

#[test]
fn test_return_void_function_ok() {
    reset_expr_ids();
    // fn main() { return; }
    let prog = program(vec![fn_decl(
        "main",
        vec![],
        Type::Void,
        vec![return_stmt(None)],
    )]);

    let _tcx = run_ok(prog);
}

#[test]
fn test_return_void_function_returning_value() {
    reset_expr_ids();
    // fn main() { return 1; }
    let prog = program(vec![fn_decl(
        "main",
        vec![],
        Type::Void,
        vec![return_stmt(Some(lit_int(1)))],
    )]);

    let errors = run_err(prog);
    assert!(!errors.is_empty());
    assert_has_mismatch(&errors, &Type::Void, &Type::Int);
}

#[test]
fn test_return_non_void_function_correct() {
    reset_expr_ids();
    // fn f() -> int { return 1; }
    let value_expr = lit_int(1);
    let value_id = get_expr_id(&value_expr);
    let prog = program(vec![fn_decl(
        "f",
        vec![],
        Type::Int,
        vec![return_stmt(Some(value_expr))],
    )]);

    let tcx = run_ok(prog);
    assert_expr_type(&tcx, value_id, &Type::Int);
}

#[test]
fn test_return_non_void_wrong_type() {
    reset_expr_ids();
    // fn f() -> int { return true; }
    let prog = program(vec![fn_decl(
        "f",
        vec![],
        Type::Int,
        vec![return_stmt(Some(lit_bool(true)))],
    )]);

    let errors = run_err(prog);
    assert!(!errors.is_empty());
    assert_has_mismatch(&errors, &Type::Int, &Type::Bool);
}

#[test]
fn test_return_non_void_without_value() {
    reset_expr_ids();
    // fn f() -> int { return; }
    let prog = program(vec![fn_decl(
        "f",
        vec![],
        Type::Int,
        vec![return_stmt(None)],
    )]);

    let errors = run_err(prog);
    assert_has_mismatch(&errors, &Type::Int, &Type::Void);
}

// ---- function as value tests ----

#[test]
fn test_function_as_value() {
    reset_expr_ids();
    // fn f(a: int) -> int { return a; }
    // let g: fn(int) -> int = f;
    let fn_def = fn_decl(
        "f",
        vec![("a", Type::Int)],
        Type::Int,
        vec![return_stmt(Some(ident_expr("a")))],
    );
    let g_val = ident_expr("f");
    let g_val_id = get_expr_id(&g_val);
    let expected_fn_type = Type::Func {
        params: vec![FuncParam::immut(Type::Int)],
        ret: Box::new(Type::Int),
    };
    let prog = program(vec![
        fn_def,
        let_binding("g", Some(expected_fn_type.clone()), g_val),
    ]);

    let tcx = run_ok(prog);
    assert_expr_type(&tcx, g_val_id, &expected_fn_type);
}

// ---- nested function / scope tests ----

#[test]
fn test_nested_function_scope() {
    reset_expr_ids();
    // fn outer() -> int {
    //   fn inner() -> int { return 10; }
    //   return inner();
    // }
    let inner_fn = fn_decl(
        "inner",
        vec![],
        Type::Int,
        vec![return_stmt(Some(lit_int(10)))],
    );
    let call_inner = call_expr(ident_expr("inner"), vec![]);
    let outer_fn = fn_decl(
        "outer",
        vec![],
        Type::Int,
        vec![inner_fn, return_stmt(Some(call_inner))],
    );
    let prog = program(vec![outer_fn]);

    let _tcx = run_ok(prog);
}

#[test]
fn test_function_forward_reference() {
    reset_expr_ids();
    // fn a() -> int { return b(); }
    // fn b() -> int { return 1; }
    let a_fn = fn_decl(
        "a",
        vec![],
        Type::Int,
        vec![return_stmt(Some(call_expr(ident_expr("b"), vec![])))],
    );
    let b_fn = fn_decl("b", vec![], Type::Int, vec![return_stmt(Some(lit_int(1)))]);
    let prog = program(vec![a_fn, b_fn]);

    let _tcx = run_ok(prog);
}

#[test]
fn test_function_mutual_recursion() {
    reset_expr_ids();
    // fn even(n: int) -> bool {
    //   return odd(n);
    // }
    // fn odd(n: int) -> bool {
    //   return even(n);
    // }
    let even_fn = fn_decl(
        "even",
        vec![("n", Type::Int)],
        Type::Bool,
        vec![return_stmt(Some(call_expr(
            ident_expr("odd"),
            vec![ident_expr("n")],
        )))],
    );
    let odd_fn = fn_decl(
        "odd",
        vec![("n", Type::Int)],
        Type::Bool,
        vec![return_stmt(Some(call_expr(
            ident_expr("even"),
            vec![ident_expr("n")],
        )))],
    );
    let prog = program(vec![even_fn, odd_fn]);

    let _tcx = run_ok(prog);
}

// ---- implicit return function tests ----

#[test]
fn test_implicit_return_simple_int() {
    reset_expr_ids();
    // fn f() -> int { 1 }
    let one = lit_int(1);
    let one_id = get_expr_id(&one);
    let prog = program(vec![fn_decl("f", vec![], Type::Int, vec![expr_stmt(one)])]);

    let tcx = run_ok(prog);
    assert_expr_type(&tcx, one_id, &Type::Int);
}

#[test]
fn test_implicit_return_let_then_ident() {
    reset_expr_ids();
    // fn f() -> int { let x: int = 1; x }
    let x_val = lit_int(1);
    let x_binding = let_binding("x", Some(Type::Int), x_val);
    let x_ref = ident_expr("x");
    let x_ref_id = get_expr_id(&x_ref);
    let prog = program(vec![fn_decl(
        "f",
        vec![],
        Type::Int,
        vec![x_binding, expr_stmt(x_ref)],
    )]);

    let tcx = run_ok(prog);
    assert_expr_type(&tcx, x_ref_id, &Type::Int);
}

#[test]
fn test_explicit_return_still_works() {
    reset_expr_ids();
    // fn f() -> int { return 1; }
    let one = lit_int(1);
    let one_id = get_expr_id(&one);
    let prog = program(vec![fn_decl(
        "f",
        vec![],
        Type::Int,
        vec![return_stmt(Some(one))],
    )]);

    let tcx = run_ok(prog);
    assert_expr_type(&tcx, one_id, &Type::Int);
}

#[test]
fn test_implicit_return_empty_body_non_void_error() {
    reset_expr_ids();
    // fn f() -> int { }
    let prog = program(vec![fn_decl("f", vec![], Type::Int, vec![])]);

    let errors = run_err(prog);
    assert_has_mismatch(&errors, &Type::Int, &Type::Void);
}

#[test]
fn test_implicit_return_wrong_type_error() {
    reset_expr_ids();
    // fn f() -> int { true }
    let prog = program(vec![fn_decl(
        "f",
        vec![],
        Type::Int,
        vec![expr_stmt(lit_bool(true))],
    )]);

    let errors = run_err(prog);
    assert_has_mismatch(&errors, &Type::Int, &Type::Bool);
}

#[test]
fn test_void_fn_trailing_value_error() {
    reset_expr_ids();
    // fn f() { 1 }
    // void functions should error on trailing non-void expressions
    let prog = program(vec![fn_decl(
        "f",
        vec![],
        Type::Void,
        vec![expr_stmt(lit_int(1))],
    )]);

    let errors = run_err(prog);
    assert_has_mismatch(&errors, &Type::Void, &Type::Int);
}

#[test]
fn test_void_fn_empty_body_ok() {
    reset_expr_ids();
    // fn f() { }
    let prog = program(vec![fn_decl("f", vec![], Type::Void, vec![])]);

    let _tcx = run_ok(prog);
}

#[test]
fn test_nested_fn_implicit_return() {
    reset_expr_ids();
    // fn outer() -> int {
    //   fn inner() -> int { 1 }
    //   inner()
    // }
    let inner_fn = fn_decl("inner", vec![], Type::Int, vec![expr_stmt(lit_int(1))]);
    let call_inner = call_expr(ident_expr("inner"), vec![]);
    let call_id = get_expr_id(&call_inner);
    let outer_fn = fn_decl(
        "outer",
        vec![],
        Type::Int,
        vec![inner_fn, expr_stmt(call_inner)],
    );
    let prog = program(vec![outer_fn]);

    let tcx = run_ok(prog);
    assert_expr_type(&tcx, call_id, &Type::Int);
}

// ---- extern fn typechecker tests ----

#[test]
fn extern_fn_call_correct_types_ok() {
    let result =
        check_src("extern fn add(a: int, b: int) -> int;\nfn main() { let x = add(1, 2); }");
    assert!(
        result.is_ok(),
        "expected typecheck to pass, got: {:?}",
        result.err()
    );
}

#[test]
fn extern_fn_call_wrong_arg_type_typecheck_err() {
    let result = check_src("extern fn add(a: int, b: int) -> int;\nfn main() { add(true, 2); }");
    assert!(result.is_err(), "expected type error for wrong arg type");
}

#[test]
fn extern_fn_call_wrong_arity_typecheck_err() {
    let result = check_src("extern fn add(a: int, b: int) -> int;\nfn main() { add(1); }");
    assert!(result.is_err(), "expected type error for wrong arity");
}

#[test]
fn extern_fn_void_return_ok() {
    let result = check_src("extern fn tick();\nfn main() { tick(); }");
    assert!(
        result.is_ok(),
        "expected typecheck to pass, got: {:?}",
        result.err()
    );
}

#[test]
fn extern_type_used_in_extern_fn_ok() {
    let src = "
extern type Sprite;
extern fn create() -> Sprite;
extern fn draw(s: Sprite);
fn main() {
    let s = create();
    draw(s);
}";
    let result = check_src(src);
    assert!(
        result.is_ok(),
        "expected typecheck to pass, got: {:?}",
        result.err()
    );
}

#[test]
fn extern_type_mismatch_err() {
    let src = "
extern type Sprite;
extern type Sound;
extern fn play(s: Sound);
fn main() {
    let spr = create_sprite();
    play(spr);
}
extern fn create_sprite() -> Sprite;";
    let result = check_src(src);
    assert!(result.is_err(), "expected type error for wrong extern type");
}

#[test]
fn extern_type_not_assignable_to_int_err() {
    let src = "
extern type Sprite;
extern fn create() -> Sprite;
fn main() {
    let x: int = create();
}";
    let result = check_src(src);
    assert!(result.is_err(), "expected type error: Sprite is not int");
}

#[test]
fn int_not_assignable_to_extern_type_err() {
    let src = "
extern type Sprite;
extern fn draw(s: Sprite);
fn main() {
    draw(42);
}";
    let result = check_src(src);
    assert!(result.is_err(), "expected type error: int is not Sprite");
}

#[test]
fn extern_type_optional_ok() {
    let src = "
extern type Sprite;
extern fn find() -> Sprite?;
fn main() {
    let s = find();
}";
    let result = check_src(src);
    assert!(
        result.is_ok(),
        "expected typecheck to pass, got: {:?}",
        result.err()
    );
}
