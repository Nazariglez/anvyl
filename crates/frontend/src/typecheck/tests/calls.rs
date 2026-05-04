use super::support::{
    assert_calls, assert_calls_with_modules, assert_err_count, assert_single_error, assert_ty,
    assert_ty_mods, assert_typecheck_closed, check, check_mods,
};
use crate::{
    ast::{Ident, NominalKind, Type},
    span::Span,
    typecheck::{
        ArityError, CallTarget, CallableId, ConstDiagnostic, GenericArgs, ModuleScope, TypeError,
        call_target_closure_facts,
        const_term::{ConstInferVarId, ConstTerm},
    },
};

fn option_type(inner: Type) -> Type {
    Type::nominal(
        NominalKind::Enum,
        Ident::new("Option"),
        vec![inner],
        vec![],
        None,
    )
}

#[test]
fn direct_call_typechecks() {
    assert_ty(
        "fn foo() -> int { 0 } fn main() -> int { foo() }",
        Type::Int,
    );
}

#[test]
fn non_callable_call() {
    assert_single_error("fn main() { 1(); }", |err| {
        matches!(err, TypeError::NotCallable { ty: Type::Int, .. })
    });
}

#[test]
fn builtin_calls_typecheck() {
    check(
        r#"
        fn main() {
            println("ok");
            println(1);
            assert(true);
            assert_msg(true, "ok");
        }
        "#,
    )
    .unwrap();
}

#[test]
fn builtin_calls_in_named_modules() {
    check_mods(
        "import gamekit { run }; fn main() { run(); }",
        "pub fn run() { println(1); }",
    )
    .unwrap();
}

#[test]
fn direct_call_target() {
    assert_calls("fn foo() -> int { 0 } fn main() { foo(); }", 1);
}

#[test]
fn generic_fn_inferred() {
    assert_ty(
        "fn id<T>(x: T) -> T { x } fn main() -> int { id(1) }",
        Type::Int,
    );
}

#[test]
fn generic_fn_explicit() {
    assert_ty(
        "fn id<T>(x: T) -> T { x } fn main() -> string { id<string>(\"ok\") }",
        Type::String,
    );
}

#[test]
fn generic_fn_call_target() {
    let result = check("fn id<T>(x: T) -> T { x } fn main() { id(1); }").unwrap();
    let target = result.calls().values().next().expect("missing call target");
    assert_eq!(
        target,
        &CallTarget::new(
            CallableId::function(ModuleScope::Root, Ident::new("id")),
            GenericArgs {
                type_args: vec![Type::Int],
                const_args: vec![],
            }
        )
    );
}

#[test]
fn explicit_prefix_call_target() {
    let result = check(
        "enum Option<T> { Some(T), None } fn make<T, U>(x: T) -> Option<U> { nil } fn main() -> Option<string> { make<int>(1) }",
    )
    .unwrap();
    let target = result.calls().values().next().expect("missing call target");
    assert_eq!(
        target,
        &CallTarget::new(
            CallableId::function(ModuleScope::Root, Ident::new("make")),
            GenericArgs {
                type_args: vec![Type::Int, Type::String],
                const_args: vec![],
            }
        )
    );
}

#[test]
fn generic_fn_repeated_param_conflict() {
    let result = check("fn same<T>(a: T, b: T) -> T { a } fn main() { same(1, true); }");
    assert!(result.is_err(), "expected repeated type param conflict");
}

#[test]
fn generic_fn_body_err() {
    let result = check(
        "fn mul2(x: int) -> int { x * 2 } fn duplicate<T>(x: T) -> T { mul2(x) } fn main() { duplicate<string>(\"x\"); }",
    );
    assert!(result.is_err(), "expected specialized body error");
}

#[test]
fn too_many_explicit_args_err() {
    assert_single_error(
        "fn id<T>(x: T) -> T { x } fn main() { id<int, string>(1); }",
        |err| {
            matches!(
                err,
                TypeError::GenericArity(ArityError::TypeArgs {
                    expected: 1,
                    found: 2,
                })
            )
        },
    );
}

#[test]
fn const_arg_in_type_slot_err() {
    assert_single_error("fn id<T>(x: T) -> T { x } fn main() { id<3>(1); }", |err| {
        matches!(
            err,
            TypeError::GenericArgKindMismatch {
                expected: "type",
                ..
            }
        )
    });
}

#[test]
fn explicit_optional_nil() {
    assert_ty(
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
    assert_ty(
        "fn id<T>(x: T) -> T { x } fn main() { let x: int? = id(nil); x; }",
        Type::option_of(Type::Int),
    );
}

#[test]
fn expected_binding() {
    assert_ty(
        "enum Option<T> { Some(T), None } fn none<T>() -> Option<T> { nil } fn main() { let x: Option<int> = none(); x; }",
        option_type(Type::Int),
    );
}

#[test]
fn expected_return() {
    assert_ty(
        "enum Option<T> { Some(T), None } fn none<T>() -> Option<T> { nil } fn main() -> Option<int> { none() }",
        option_type(Type::Int),
    );
}

#[test]
fn expected_return_no_leak() {
    let checked = check(
        "enum Option<T> { Some(T), None } fn none<T>() -> Option<T> { nil } fn main() -> Option<int> { none() }",
    )
    .expect("typecheck failed");
    assert_typecheck_closed(&checked);
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
    assert_ty(
        "fn len<T, N: int>(xs: [T; N]) -> int { N } fn main(xs: [int; 3]) -> int { len(xs) }",
        Type::Int,
    );
}

#[test]
fn const_target() {
    let result =
        check("fn len<T, N: int>(xs: [T; N]) -> int { N } fn main() { len([1, 2, 3]); }").unwrap();
    let target = result.calls().values().next().expect("missing call target");
    assert_eq!(
        target,
        &CallTarget::new(
            CallableId::function(ModuleScope::Root, Ident::new("len")),
            GenericArgs {
                type_args: vec![Type::Int],
                const_args: vec![ConstTerm::from_usize(3)],
            }
        )
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
    assert_ty(
        "const CAP = 3; fn len<T, N: int>(xs: [T; N]) -> int { 0 } fn main(xs: [int; 3]) -> int { len<int, CAP>(xs) }",
        Type::Int,
    );
}

#[test]
fn generic_const_call_target() {
    let result = check(
        "fn len<T, N: int>(xs: [T; N]) -> int { 0 } fn main(xs: [int; 3]) { len<int, 3>(xs); }",
    )
    .unwrap();
    let target = result.calls().values().next().expect("missing call target");
    assert_eq!(
        target,
        &CallTarget::new(
            CallableId::function(ModuleScope::Root, Ident::new("len")),
            GenericArgs {
                type_args: vec![Type::Int],
                const_args: vec![ConstTerm::from_usize(3)],
            }
        )
    );
}

#[test]
fn generic_const_named_target() {
    let result = check(
        "const CAP = 3; fn len<T, N: int>(xs: [T; N]) -> int { 0 } fn main(xs: [int; 3]) { len<int, CAP>(xs); }",
    )
    .unwrap();
    let target = result.calls().values().next().expect("missing call target");
    assert_eq!(
        target,
        &CallTarget::new(
            CallableId::function(ModuleScope::Root, Ident::new("len")),
            GenericArgs {
                type_args: vec![Type::Int],
                const_args: vec![ConstTerm::from_usize(3)],
            }
        )
    );
}

#[test]
fn non_bare_const_arg_err() {
    assert_single_error(
        "fn take<T, N: int>(xs: [T; N]) {} fn main(xs: [int; 3]) { take<int, [int]>(xs); }",
        |err| {
            matches!(
                err,
                TypeError::GenericArgKindMismatch {
                    expected: "const",
                    ..
                }
            )
        },
    );
}

#[test]
fn generic_const_unknown_name_arg_err() {
    assert_single_error(
        "fn take<T, N: int>(xs: [T; N]) {} fn main(xs: [int; 3]) { take<int, N>(xs); }",
        |err| matches!(err, TypeError::UnknownConst { name, .. } if *name == Ident::new("N")),
    );
}

#[test]
fn call_target_facts_distinguish_const_infer() {
    let facts = call_target_closure_facts(&CallTarget::new(
        CallableId::function(ModuleScope::Root, Ident::new("take")),
        GenericArgs {
            type_args: vec![],
            const_args: vec![ConstTerm::Infer(ConstInferVarId(0))],
        },
    ));

    assert!(!facts.types.contains_infer);
    assert!(facts.types.first_unresolved.is_none());
    assert!(!facts.contains_unresolved_const());
    assert!(facts.consts.contains_infer);
}

#[test]
fn call_target_const_infer_error() {
    let target = CallTarget::new(
        CallableId::function(ModuleScope::Root, Ident::new("take")),
        GenericArgs {
            type_args: vec![],
            const_args: vec![ConstTerm::Infer(ConstInferVarId(0))],
        },
    );
    let span = Span::new(1, 2);
    let mut errors = vec![];

    super::super::push_call_target_closure_error(&mut errors, &target, span);

    assert_eq!(errors, vec![TypeError::CannotInferConst { span }]);
}

#[test]
fn generic_const_arg_kind_mismatch() {
    let result = check(
        "fn len<T, N: int>(xs: [T; N]) -> int { 0 } fn main(xs: [int; 3]) { len<3, int>(xs); }",
    );
    assert!(result.is_err(), "expected generic arg kind mismatch");
}

#[test]
fn bool_const_arg_err() {
    assert_single_error(
        "fn take<N: int>(xs: [int; N]) {} fn main() { take<true>([]); }",
        |err| {
            matches!(
                err,
                TypeError::ExpectedIntConst {
                    found: Type::Bool,
                    ..
                }
            )
        },
    );
}

#[test]
fn negative_const_arg_err() {
    assert_single_error(
        "const NEG = -1; fn take<N: int>(xs: [int; N]) {} fn main() { take<NEG>([]); }",
        |err| matches!(err, TypeError::NegativeArrayLength { value: -1, .. }),
    );
}

#[test]
fn generic_method_const_return() {
    assert_ty(
        "struct Arrays { fn len<T, N: int>(xs: [T; N]) -> int { N } } fn main(xs: [int; 4]) -> int { Arrays.len<int, 4>(xs) }",
        Type::Int,
    );
}

#[test]
fn method_args_bind_method_generics() {
    assert_ty(
        "struct Box<T> { value: T, fn keep<U>(self, x: U) -> U { x } } fn main(b: Box<int>) -> string { b.keep<string>(\"ok\") }",
        Type::String,
    );
}

#[test]
fn generic_method_named_const_arg() {
    assert_ty(
        "const CAP = 3; struct Arrays { fn len<T, N: int>(xs: [T; N]) -> int { N } } fn main(xs: [int; 3]) -> int { Arrays.len<int, CAP>(xs) }",
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
    assert_ty_mods(root, dep, Type::Int);
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
    let result = check_mods(root, dep);
    assert!(result.is_err(), "expected error for wrong arg type");
}

#[test]
fn unknown_module_member() {
    let dep = "pub fn init() -> int { 0 }";
    let root = "
        import gamekit as gk;
        fn main() -> int { gk.unknown() }
    ";
    let result = check_mods(root, dep);
    assert!(result.is_err(), "expected error for unknown module member");
}
