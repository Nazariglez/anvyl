use super::support::{
    assert_calls, assert_calls_with_modules, assert_err_count, assert_no_infer_vars_in_result,
    assert_single_error, assert_type, assert_type_with_modules, typecheck, typecheck_with_modules,
};
use crate::{
    ast::{Ident, Type},
    typecheck::{CallTarget, ConstDiagnostic, ModuleScope, TypeError},
};

fn option_type(inner: Type) -> Type {
    Type::Enum {
        name: Ident::new("Option"),
        type_args: vec![inner],
        const_args: vec![],
        origin: None,
    }
}

#[test]
fn direct_call_typechecks() {
    assert_type(
        "fn foo() -> int { 0 } fn main() -> int { foo() }",
        Type::Int,
    );
}

#[test]
fn direct_call_target() {
    assert_calls("fn foo() -> int { 0 } fn main() { foo(); }", 1);
}

#[test]
fn generic_fn_inferred() {
    assert_type(
        "fn id<T>(x: T) -> T { x } fn main() -> int { id(1) }",
        Type::Int,
    );
}

#[test]
fn generic_fn_explicit() {
    assert_type(
        "fn id<T>(x: T) -> T { x } fn main() -> string { id<string>(\"ok\") }",
        Type::String,
    );
}

#[test]
fn generic_fn_call_target() {
    let result = typecheck("fn id<T>(x: T) -> T { x } fn main() { id(1); }").unwrap();
    let target = result.calls().values().next().expect("missing call target");
    assert_eq!(
        target,
        &CallTarget::GenericDirect {
            module: ModuleScope::Root,
            name: Ident::new("id"),
            type_args: vec![Type::Int],
            const_args: vec![],
        }
    );
}

#[test]
fn generic_fn_repeated_param_conflict() {
    let result = typecheck("fn same<T>(a: T, b: T) -> T { a } fn main() { same(1, true); }");
    assert!(result.is_err(), "expected repeated type param conflict");
}

#[test]
fn generic_fn_body_err() {
    let result = typecheck(
        "fn mul2(x: int) -> int { x * 2 } fn duplicate<T>(x: T) -> T { mul2(x) } fn main() { duplicate<string>(\"x\"); }",
    );
    assert!(result.is_err(), "expected specialized body error");
}

#[test]
fn explicit_optional_nil() {
    assert_type(
        "fn id<T>(x: T) -> T { x } fn main() { let x = id<int?>(nil); x; }",
        Type::option_of(Type::Int),
    );
}

#[test]
fn explicit_plain_nil_err() {
    assert_err_count("fn id<T>(x: T) -> T { x } fn main() { id<int>(nil); }", 1);
}

#[test]
fn arg_conflict_mismatch() {
    assert_single_error(
        "fn id<T>(x: T) -> T { x } fn main() { id<int>(1.5); }",
        |err| matches!(err, TypeError::TypeMismatch { .. }),
    );
}

#[test]
fn expected_nil_arg() {
    assert_type(
        "fn id<T>(x: T) -> T { x } fn main() { let x: int? = id(nil); x; }",
        Type::option_of(Type::Int),
    );
}

#[test]
fn expected_binding() {
    assert_type(
        "enum Option<T> { Some(T), None } fn none<T>() -> Option<T> { nil } fn main() { let x: Option<int> = none(); x; }",
        option_type(Type::Int),
    );
}

#[test]
fn expected_return() {
    assert_type(
        "enum Option<T> { Some(T), None } fn none<T>() -> Option<T> { nil } fn main() -> Option<int> { none() }",
        option_type(Type::Int),
    );
}

#[test]
fn expected_return_no_leak() {
    let checked = typecheck(
        "enum Option<T> { Some(T), None } fn none<T>() -> Option<T> { nil } fn main() -> Option<int> { none() }",
    )
    .expect("typecheck failed");
    assert_no_infer_vars_in_result(&checked);
}

#[test]
fn return_only_unbound() {
    assert_err_count(
        "enum Option<T> { Some(T), None } fn none<T>() -> Option<T> { nil } fn main() { none(); }",
        1,
    );
}

#[test]
fn return_unbound_variant() {
    assert_single_error(
        "enum Option<T> { Some(T), None } fn none<T>() -> Option<T> { nil } fn main() { none(); }",
        |err| matches!(err, TypeError::UnboundGenericParam { .. }),
    );
}

#[test]
fn expected_explicit_mismatch() {
    assert_err_count(
        "enum Option<T> { Some(T), None } fn none<T>() -> Option<T> { nil } fn main() { let x: Option<int> = none<string>(); }",
        1,
    );
}

#[test]
fn generic_const_array_inference() {
    assert_type(
        "fn len<T, N: int>(xs: [T; N]) -> int { N } fn main(xs: [int; 3]) -> int { len(xs) }",
        Type::Int,
    );
}

#[test]
fn const_target() {
    let result =
        typecheck("fn len<T, N: int>(xs: [T; N]) -> int { N } fn main() { len([1, 2, 3]); }")
            .unwrap();
    let target = result.calls().values().next().expect("missing call target");
    assert_eq!(
        target,
        &CallTarget::GenericDirect {
            module: ModuleScope::Root,
            name: Ident::new("len"),
            type_args: vec![Type::Int],
            const_args: vec![3],
        }
    );
}

#[test]
fn const_conflict() {
    assert_single_error(
        "fn same<T, N: int>(a: [T; N], b: [T; N]) -> T { a[0] } fn main() { same([1, 2, 3], [4, 5]); }",
        |err| {
            matches!(
                err,
                TypeError::ConstMismatch {
                    expected: ConstDiagnostic::Value(_),
                    found: ConstDiagnostic::Value(_),
                    ..
                }
            )
        },
    );
}

#[test]
fn generic_const_array_explicit_named() {
    assert_type(
        "const CAP = 3; fn len<T, N: int>(xs: [T; N]) -> int { 0 } fn main(xs: [int; 3]) -> int { len<int, CAP>(xs) }",
        Type::Int,
    );
}

#[test]
fn generic_const_call_target() {
    let result = typecheck(
        "fn len<T, N: int>(xs: [T; N]) -> int { 0 } fn main(xs: [int; 3]) { len<int, 3>(xs); }",
    )
    .unwrap();
    let target = result.calls().values().next().expect("missing call target");
    assert_eq!(
        target,
        &CallTarget::GenericDirect {
            module: ModuleScope::Root,
            name: Ident::new("len"),
            type_args: vec![Type::Int],
            const_args: vec![3],
        }
    );
}

#[test]
fn generic_const_arg_kind_mismatch() {
    let result = typecheck(
        "fn len<T, N: int>(xs: [T; N]) -> int { 0 } fn main(xs: [int; 3]) { len<3, int>(xs); }",
    );
    assert!(result.is_err(), "expected generic arg kind mismatch");
}

#[test]
fn generic_method_const_return() {
    assert_type(
        "struct Arrays { fn len<T, N: int>(xs: [T; N]) -> int { N } } fn main(xs: [int; 4]) -> int { Arrays.len<int, 4>(xs) }",
        Type::Int,
    );
}

#[test]
fn module_function_call() {
    let dep = "pub fn init() -> int { 0 }";
    let root = "
        import gamekit as gk;
        fn main() -> int { gk.init() }
    ";
    assert_type_with_modules(root, dep, Type::Int);
}

#[test]
fn module_function_call_target() {
    let dep = "pub fn init() -> int { 0 }";
    let root = "
        import gamekit as gk;
        fn main() { gk.init(); }
    ";
    assert_calls_with_modules(root, dep, 1);
}

#[test]
fn module_function_wrong_arg() {
    let dep = "pub fn init(x: int) -> int { x }";
    let root = "
        import gamekit as gk;
        fn main() -> int { gk.init(true) }
    ";
    let result = typecheck_with_modules(root, dep);
    assert!(result.is_err(), "expected error for wrong arg type");
}

#[test]
fn unknown_module_member() {
    let dep = "pub fn init() -> int { 0 }";
    let root = "
        import gamekit as gk;
        fn main() -> int { gk.unknown() }
    ";
    let result = typecheck_with_modules(root, dep);
    assert!(result.is_err(), "expected error for unknown module member");
}
