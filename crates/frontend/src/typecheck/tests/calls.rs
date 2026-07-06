use super::support::{
    assert_calls, assert_calls_with_modules, assert_deprecated_warning, assert_err_count,
    assert_expected_projection, assert_expr_type, assert_single_error, assert_ty, assert_ty_mods,
    assert_typecheck_closed, check, check_mods, core_option, generic_body, nominal_struct, output,
};
use crate::{
    ast::{Ident, Type},
    lint::LintId,
    typecheck::{
        CallTarget, CallableId, ConstDiagnostic, DeprecatedUseKind, GenericArgs, ModuleScope,
        TypeError, call_target_closure_facts,
        const_term::{ConstInferVarId, ConstTerm},
    },
};

#[test]
fn failed_output_keeps_lints() {
    let (errors, warnings, lint_events, _, facts) = output(
        "@deprecated fn old() {}
         fn main() { old(); missing; }",
    )
    .into_parts();

    assert!(!errors.is_empty());
    assert!(warnings.is_empty());
    assert!(facts.is_none());
    assert_eq!(lint_events.len(), 1);
    assert_eq!(lint_events[0].id, LintId::Deprecated);
}

#[test]
fn direct_typecheck_has_no_magic_builtins() {
    assert_single_error("fn main() { println(\"ok\"); }", |err| {
        matches!(err, TypeError::UndefinedVariable { .. })
    });
    assert_single_error("fn main() { assert(true); }", |err| {
        matches!(err, TypeError::UndefinedVariable { .. })
    });
    assert_single_error("fn main() { assert_msg(true, \"ok\"); }", |err| {
        matches!(err, TypeError::UndefinedVariable { .. })
    });
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
    let targets = result
        .bodies()
        .flat_map(|body| body.calls.values())
        .filter(|target| target.id == id)
        .collect::<Vec<_>>();
    assert!(targets.contains(&&CallTarget::new(
        id.clone(),
        GenericArgs {
            type_args: vec![Type::Int],
            const_args: vec![],
        }
    )));
    assert!(targets.contains(&&CallTarget::new(
        id,
        GenericArgs {
            type_args: vec![Type::String],
            const_args: vec![],
        }
    )));
    assert_eq!(targets.len(), 2);

    let int_key = generic_body("wrap", vec![Type::Int]);
    let string_key = generic_body("wrap", vec![Type::String]);
    let int_body = result.expect_body(&int_key);
    let string_body = result.expect_body(&string_key);
    let has_qualified_type = int_body.expr_types.iter().any(|(expr, int_fact)| {
        int_fact.ty.as_ref() == Some(&Type::Int)
            && string_body
                .expr_types
                .get(expr)
                .and_then(|fact| fact.ty.as_ref())
                == Some(&Type::String)
    });
    assert!(
        has_qualified_type,
        "int: {:?}\nstring: {:?}",
        int_body.expr_types, string_body.expr_types
    );
}

#[test]
fn dependent_projected_ref_arg_records_specialized_expected_projection() {
    let result = check(
        r"
        struct Entity { x: int }
        struct Box<T> { @as embed value: T }
        fn move_entity(ref entity: Entity) { entity.x += 1; }
        fn move_box<T>(ref box: Box<T>) { move_entity(box); }
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

    let body = result.expect_body(&generic_body("move_box", vec![entity_ty.clone()]));
    let fact = body
        .expected_projections
        .get(&expr_id)
        .expect("projection fact should stay in specialized body");
    assert_eq!(fact.target_ty, entity_ty);
}

#[test]
fn generic_arg_without_expected_target_records_no_projection() {
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
fn projection_dyn_arg_emits_one_deprecated_contract_warning() {
    let result = check(
        r#"
        @deprecated("use New") contract Old { fn f(self); }
        struct Entity { fn f(self) {} }
        struct Enemy { @as embed entity: Entity }
        fn take(value: dyn Old) {}
        fn main() {
            let enemy = Enemy { entity: Entity {} };
            take(enemy);
        }
        "#,
    )
    .unwrap();

    assert_deprecated_warning(&result, DeprecatedUseKind::Contract, "Old", Some("use New"));
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
    let result =
        check("fn make<T, U>(x: T) -> U? { nil } fn main() -> Option<string> { make<int>(1) }")
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
    let checked = check("fn none<T>() -> T? { nil } fn main() -> Option<int> { none() }")
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
fn module_function_call_target() {
    let dep = "pub fn init() -> int { 0 }";
    let root = "
        import gamekit as gk;
        fn main() { gk.init(); }
    ";
    assert_calls_with_modules(root, dep, 1);
}
