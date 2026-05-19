use super::support::{
    assert_calls, assert_calls_with_modules, assert_deprecated_warning, assert_err_count,
    assert_expected_projection, assert_expr_type, assert_single_error, assert_ty, assert_ty_mods,
    assert_typecheck_closed, check, check_mods, nominal_struct,
};
use crate::{
    ast::{Ident, NominalKind, Type},
    typecheck::{
        CallTarget, CallableId, ConstDiagnostic, DeprecatedUseKind, GenericArgs, ModuleScope,
        TypeError, call_target_closure_facts,
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
fn deprecated_generic_call_warns_once() {
    let result = check(
        "@deprecated(\"use newer\") fn old<T>(value: T) -> T { value }
         fn main() { old(1); }",
    )
    .unwrap();
    assert_deprecated_warning(
        &result,
        DeprecatedUseKind::Function,
        "old",
        Some("use newer"),
    );
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
fn cached_generic_specialization_restores_call_targets() {
    let result = check(
        r#"
        fn id<T>(x: T) -> T { x }
        fn wrap<T>(x: T) -> T { id(x) }
        fn main() {
            wrap(1);
            wrap("x");
            wrap(2);
        }
        "#,
    )
    .unwrap();
    let id = CallableId::function(ModuleScope::Root, Ident::new("id"));
    let target = result
        .calls()
        .values()
        .find(|target| target.id == id)
        .expect("missing nested call target");
    assert_eq!(
        target,
        &CallTarget::new(
            id,
            GenericArgs {
                type_args: vec![Type::Int],
                const_args: vec![],
            }
        )
    );
}

#[test]
fn projected_var_arg_records_expected_projection() {
    let result = check(
        r"
        struct Entity { x: int }
        struct Enemy { @as embed entity: Entity }
        fn move_entity(var entity: Entity) { entity.x += 1; }
        fn main() {
            var enemy = Enemy { entity: Entity { x: 1 } };
            move_entity(enemy);
        }
        ",
    )
    .unwrap();
    let entity_ty = nominal_struct("Entity");
    let expr_id = assert_expected_projection(&result, &["entity"], entity_ty.clone());
    assert_expr_type(&result, expr_id, &entity_ty);
}

#[test]
fn dependent_projected_var_arg_records_specialized_expected_projection() {
    let result = check(
        r"
        struct Entity { x: int }
        struct Box<T> { @as embed value: T }
        fn move_entity(var entity: Entity) { entity.x += 1; }
        fn move_box<T>(var box: Box<T>) { move_entity(box); }
        fn main() {
            var box = Box<Entity> { value: Entity { x: 1 } };
            move_box<Entity>(box);
            move_box<Entity>(box);
        }
        ",
    )
    .unwrap();
    let entity_ty = nominal_struct("Entity");
    let expr_id = assert_expected_projection(&result, &["value"], entity_ty.clone());
    assert_expr_type(&result, expr_id, &entity_ty);
}

#[test]
fn exact_var_arg_records_no_projection() {
    let result = check(
        r"
        struct Entity { x: int }
        struct Enemy { @as embed entity: Entity }
        fn move_entity(var entity: Entity) { entity.x += 1; }
        fn main() {
            var enemy = Enemy { entity: Entity { x: 1 } };
            move_entity(enemy.entity);
        }
        ",
    )
    .unwrap();

    assert!(result.expected_projections().is_empty());
}

#[test]
fn value_arg_records_expected_projection() {
    let result = check(
        r"
        struct Entity { x: int }
        struct Enemy { @as embed entity: Entity }
        fn take(entity: Entity) {}
        fn main() {
            let enemy = Enemy { entity: Entity { x: 1 } };
            take(enemy);
        }
        ",
    )
    .unwrap();
    assert_expected_projection(&result, &["entity"], nominal_struct("Entity"));
}

#[test]
fn unconstrained_generic_arg_records_no_projection() {
    let result = check(
        r"
        struct Entity { x: int }
        struct Enemy { @as embed entity: Entity }
        fn id<T>(x: T) -> T { x }
        fn main() {
            let enemy = Enemy { entity: Entity { x: 1 } };
            id(enemy);
        }
        ",
    )
    .unwrap();

    assert!(result.expected_projections().is_empty());
}

#[test]
fn cast_accept_arg_projected_field_casts_to_target() {
    let result = check(
        r"
        struct Raw { x: int }
        struct Entity { x: int }
        struct Enemy { @as embed raw: Raw }
        extend Entity {
            cast from(raw: Raw) { Entity { x: raw.x } }
        }
        fn take(entity: as Entity) {}
        fn main() {
            let enemy = Enemy { raw: Raw { x: 1 } };
            take(enemy);
        }
        ",
    )
    .unwrap();
    assert_expected_projection(&result, &["raw"], nominal_struct("Raw"));
}

#[test]
fn cast_accept_arg_source_cast_precedes_projection() {
    let result = check(
        r"
        struct Entity { x: int }
        struct Enemy { @as embed entity: Entity }
        extend Entity {
            cast from(enemy: Enemy) { enemy.entity }
        }
        fn take(entity: as Entity) {}
        fn main() {
            let enemy = Enemy { entity: Entity { x: 1 } };
            take(enemy);
        }
        ",
    )
    .unwrap();

    assert!(result.expected_projections().is_empty());
}

#[test]
fn explicit_cast_projection_records_operand_fact() {
    let result = check(
        r"
        struct Raw { x: int }
        struct Entity { x: int }
        struct Enemy { @as embed raw: Raw }
        extend Entity {
            cast from(raw: Raw) { Entity { x: raw.x } }
        }
        fn main() {
            let enemy = Enemy { raw: Raw { x: 1 } };
            let entity: Entity = enemy as Entity;
        }
        ",
    )
    .unwrap();
    assert_expected_projection(&result, &["raw"], nominal_struct("Raw"));
}

#[test]
fn explicit_cast_records_no_projection() {
    let result = check(
        r"
        struct Entity { x: int }
        struct Enemy { @as embed entity: Entity }
        extend Entity {
            cast from(enemy: Enemy) { enemy.entity }
        }
        fn main() {
            let enemy = Enemy { entity: Entity { x: 1 } };
            let entity: Entity = enemy as Entity;
        }
        ",
    )
    .unwrap();

    assert!(result.expected_projections().is_empty());
}

#[test]
fn projected_generic_return_records_call_target() {
    let result = check(
        r"
        struct Entity { x: int }
        struct Actor { @as embed entity: Entity }
        struct Enemy { @as embed actor: Actor }
        fn id<T>(x: T) -> T { x }
        fn main() -> Entity {
            let enemy = Enemy { actor: Actor { entity: Entity { x: 1 } } };
            id(enemy)
        }
        ",
    )
    .unwrap();
    assert_expected_projection(&result, &["actor", "entity"], nominal_struct("Entity"));
    let target = result
        .calls()
        .values()
        .find(|target| target.id == CallableId::function(ModuleScope::Root, Ident::new("id")))
        .expect("missing projected generic call target");
    assert_eq!(
        target,
        &CallTarget::new(
            CallableId::function(ModuleScope::Root, Ident::new("id")),
            GenericArgs {
                type_args: vec![nominal_struct("Enemy")],
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
fn generic_const_array_explicit_named() {
    assert_ty(
        "const CAP = 3; fn len<T, N: int>(xs: [T; N]) -> int { 0 } fn main(xs: [int; 3]) -> int { len<int, CAP>(xs) }",
        Type::Int,
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
fn generic_method_named_const_arg() {
    assert_ty(
        "const CAP = 3; struct Arrays { fn len<T, N: int>(xs: [T; N]) -> int { N } } fn main(xs: [int; 3]) -> int { Arrays.len<int, CAP>(xs) }",
        Type::Int,
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
fn call_target_facts_distinguish_const_infer() {
    let facts = call_target_closure_facts(&CallTarget::new(
        CallableId::function(ModuleScope::Root, Ident::new("take")),
        GenericArgs {
            type_args: vec![],
            const_args: vec![ConstTerm::Infer(ConstInferVarId(0))],
        },
    ));

    assert!(!facts.types.infer.contains_type);
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
    let span = None;
    let mut errors = vec![];

    super::super::push_call_target_closure_error(&mut errors, &target, span);

    assert_eq!(errors, vec![TypeError::CannotInferConst { span }]);
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
