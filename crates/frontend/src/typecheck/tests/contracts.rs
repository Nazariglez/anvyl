use super::support::{assert_deprecated_warning, assert_typecheck_closed, check, errors};
use crate::{
    ast::{ContractRef, Ident, Type},
    externs,
    span::Span,
    test_support::{empty_resolved, module_path, parse_program, resolved_modules},
    typecheck::{
        DeclError, DeprecatedUseKind, ModuleScope, TypeError, TypecheckConfig, contracts,
        contracts::{ContractMatchError, ContractSlotTarget, RequirementError},
        typechecker_for_modules,
    },
};

fn contract(name: &str) -> ContractRef {
    ContractRef::Named {
        qualifier: None,
        name: Ident::new(name),
        origin: None,
    }
}

fn checker(source: &str) -> crate::typecheck::TypeChecker {
    let program = parse_program(source);
    let resolved = empty_resolved();
    let raw_externs = externs::collect_source_externs(&program, &resolved).unwrap();
    typechecker_for_modules(&program, &resolved, raw_externs, TypecheckConfig::default())
        .expect("typecheck failed")
}

fn checker_with_modules(source: &str, modules: &[(&str, &str)]) -> crate::typecheck::TypeChecker {
    let program = parse_program(source);
    let resolved = resolved_modules(&program, modules);
    let raw_externs = externs::collect_source_externs(&program, &resolved).unwrap();
    typechecker_for_modules(&program, &resolved, raw_externs, TypecheckConfig::default())
        .expect("typecheck failed")
}

fn root_type(tc: &mut crate::typecheck::TypeChecker, name: &str) -> Type {
    tc.resolve_type_for_tc_at(&Type::UnresolvedName(Ident::new(name)), Span::default())
}

fn module(name: &str) -> ModuleScope {
    ModuleScope::Named(module_path(name))
}

#[test]
fn duplicate_direct_requirement_is_error() {
    let errors = errors(
        "contract Drawable {
            fn draw(self);
            fn draw(self);
        }",
    );

    assert_eq!(errors.len(), 1);
    assert!(matches!(
        &errors[0],
        TypeError::Decl(DeclError::DuplicateContractRequirement { name, .. })
            if name.as_str() == "draw"
    ));
}

#[test]
fn conflicting_direct_requirement_is_error() {
    let errors = errors(
        "contract Drawable {
            fn draw(self);
            fn draw(self, layer: int);
        }",
    );

    assert_eq!(errors.len(), 1);
    assert!(
        matches!(&errors[0], TypeError::CompileError { message, .. } if message.contains("conflicting contract requirement 'draw' in contract 'Drawable'"))
    );
}

#[test]
fn inclusion_requirements_match() {
    let matched = assert_matches(
        "contract Updatable { fn update(var self, dt: float); }
        contract Drawable { fn draw(self); }
        contract Actor { Updatable; Drawable; }
        struct Enemy {
            fn update(var self, dt: float) {}
            fn draw(self) {}
        }",
        "Enemy",
        "Actor",
    );

    assert_eq!(matched.slots.len(), 2);
}

#[test]
fn inclusion_duplicate_requirement_collapses() {
    let result = check(
        "contract A { fn draw(self); }
        contract B { fn draw(self); }
        contract C { A; B; }
        struct Sprite { fn draw(self) {} }
        fn render(sprite: Sprite) {
            let actor: dyn C = sprite;
            actor.draw();
        }",
    )
    .expect("typecheck failed");

    let contract = result
        .decls()
        .contract(&crate::typecheck::ContractKey {
            module: ModuleScope::Root,
            name: Ident::new("C"),
        })
        .expect("missing contract");
    assert_eq!(contract.requirements.len(), 1);
}

#[test]
fn inclusion_conflict_is_error() {
    let errors = errors(
        "contract A { fn draw(self); }
        contract B { fn draw(self, layer: int); }
        contract C { A; B; }",
    );

    assert!(errors.iter().any(|error| matches!(
        error,
        TypeError::CompileError { message, .. }
            if message.contains("conflicting contract requirement 'draw' in contract 'C'")
    )));
}

#[test]
fn inclusion_cycle_is_error() {
    let errors = errors("contract A { B; } contract B { A; }");

    assert!(errors.iter().any(|error| matches!(
        error,
        TypeError::CompileError { message, .. }
            if message.contains("contract inclusion cycle")
    )));
}

#[test]
fn dynamic_intersection_matches_and_canonicalizes_order() {
    let result = check(
        "contract A { fn a(self); }
        contract B { fn b(self); }
        struct Both { fn a(self) {} fn b(self) {} }
        fn first(x: Both) { let y: dyn A + B = x; y.a(); y.b(); }
        fn second(x: Both) { let y: dyn B + A = x; y.a(); y.b(); }",
    )
    .expect("typecheck failed");

    let dyn_types = result
        .types()
        .filter_map(|(_, (_, ty))| matches!(ty, Type::Dyn(_)).then_some(ty))
        .collect::<Vec<_>>();
    assert!(dyn_types.windows(2).all(|pair| pair[0] == pair[1]));
}

#[test]
fn deprecated_contract_inclusion_warns() {
    let result = check(
        "@deprecated(\"use New\") contract Old { fn f(self); }
        contract New { Old; }",
    )
    .expect("typecheck failed");

    assert_deprecated_warning(&result, DeprecatedUseKind::Contract, "Old", Some("use New"));
}

#[test]
fn static_bound_accepts_satisfying_concrete_arg() {
    check(
        "contract A { fn a(self) -> int; }
        struct Thing { fn a(self) -> int { 1 } }
        fn add<T: A>(value: T) -> int { value.a() }
        fn main() { let x = add(Thing {}); }",
    )
    .expect("typecheck failed");
}

#[test]
fn static_bound_uses_declaration_module() {
    checker_with_modules(
        "import api { take };
        import thing { Thing };
        fn main() { take(Thing {}); }",
        &[
            ("thing", "pub struct Thing { fn a(self) {} }"),
            (
                "api",
                "pub contract A { fn a(self); }
                pub fn take<T: A>(value: T) {}",
            ),
        ],
    );
}

#[test]
fn deprecated_contract_bound_warns() {
    let result = check(
        "@deprecated(\"use New\") contract Old { fn f(self); }
        fn take<T: Old>(value: T) {}",
    )
    .expect("typecheck failed");

    assert_deprecated_warning(&result, DeprecatedUseKind::Contract, "Old", Some("use New"));
}

#[test]
fn static_bound_rejects_unsatisfied_concrete_arg() {
    let errors = errors(
        "contract A { fn a(self) -> int; }
        struct Thing {}
        fn add<T: A>(value: T) -> int { 1 }
        fn main() { let x = add(Thing {}); }",
    );

    assert!(errors.iter().any(|error| matches!(
        error,
        TypeError::CompileError { message, .. }
            if message.contains("does not satisfy contract bound 'A'")
    )));
}

#[test]
fn static_bound_accepts_dynamic_subset() {
    check(
        "contract A { fn a(self); }
        contract B { fn b(self); }
        struct Both { fn a(self) {} fn b(self) {} }
        fn take<T: A>(value: T) {}
        fn main() {
            let value: dyn A + B = Both {};
            take(value);
        }",
    )
    .expect("typecheck failed");
}

#[test]
fn static_bound_rejects_dynamic_missing_requirement() {
    let errors = errors(
        "contract A { fn a(self); }
        contract B { fn b(self); }
        struct OnlyA { fn a(self) {} }
        fn take<T: A + B>(value: T) {}
        fn main() {
            let value: dyn A = OnlyA {};
            take(value);
        }",
    );

    assert!(errors.iter().any(|error| matches!(
        error,
        TypeError::CompileError { message, .. }
            if message.contains("does not satisfy contract bound 'B'")
    )));
}

#[test]
fn nominal_literal_checks_owner_bounds() {
    let errors = errors(
        "contract Named { fn name(self) -> string; }
        struct Rock {}
        struct Box<T: Named> { value: T }
        fn main() { let box = Box<Rock> { value: Rock {} }; }",
    );

    assert!(errors.iter().any(|error| matches!(
        error,
        TypeError::CompileError { message, .. }
            if message.contains("does not satisfy contract bound 'Named'")
    )));
}

#[test]
fn static_bound_does_not_create_witness_for_concrete_arg() {
    let result = check(
        "contract A { fn a(self); }
        struct Thing { fn a(self) {} }
        fn take<T: A>(value: T) {}
        fn main() { take(Thing {}); }",
    )
    .expect("typecheck failed");

    assert!(result.contract_witnesses().is_empty());
    assert!(result.dyn_conversions().is_empty());
}

#[test]
fn anonymous_dynamic_contract_validates_requirement_types() {
    let errors = errors(
        "contract A { fn a(self); }
        type Bad = dyn { fn f(self, values: [dyn A: int]); };",
    );

    assert!(
        errors
            .iter()
            .any(|error| matches!(error, TypeError::NonKeyableMapKey { .. }))
    );
}

#[test]
fn inferred_dynamic_local_collects_method() {
    checker(
        "struct Actor { fn draw(self) {} }
        fn main() {
            let actor: dyn _ = Actor {};
            actor.draw();
        }",
    );
}

#[test]
fn inferred_dynamic_receiver_mutability_uses_access() {
    checker(
        "struct Actor { fn update(var self, dt: float) {} }
        fn main() {
            var actor: dyn _ = Actor {};
            actor.update(1.0);
        }",
    );
}

#[test]
fn inferred_dynamic_expected_context_solves_empty_surface() {
    checker(
        "contract Drawable { fn draw(self); }
        struct Actor { fn draw(self) {} }
        fn take(actor: dyn Drawable) {}
        fn main() {
            let actor: dyn _ = Actor {};
            take(actor);
        }",
    );
}

#[test]
fn inferred_dynamic_function_param_and_return_solve_from_body() {
    checker(
        "struct Actor { fn draw(self) {} }
        fn use_actor(actor: dyn _) { actor.draw(); }
        fn make_actor() -> dyn _ { Actor {} }
        fn main() {
            use_actor(Actor {});
            let actor = make_actor();
            actor.draw();
        }",
    );
}

#[test]
fn inferred_dynamic_empty_hole_is_error() {
    let errors = errors(
        "struct Actor {}
        fn main() { let actor: dyn _ = Actor {}; }",
    );

    assert!(errors.iter().any(|error| matches!(
        error,
        TypeError::CompileError { message, .. }
            if message.contains("cannot infer empty dynamic contract")
    )));
}

#[test]
fn inferred_dynamic_rejects_conflicting_requirements() {
    let errors = errors(
        "struct Actor {
            fn f(self, x: int) {}
        }
        fn main() {
            let actor: dyn _ = Actor {};
            actor.f(1);
            actor.f(\"x\");
        }",
    );

    assert!(errors.iter().any(|error| matches!(
        error,
        TypeError::CompileError { message, .. }
            if message.contains("conflicting inferred dynamic requirement 'f'")
    )));
}

#[test]
fn inferred_dynamic_rejects_unknown_argument_type() {
    let errors = errors(
        "struct Actor { fn f(self, x: int) {} }
        fn main() {
            let actor: dyn _ = Actor {};
            actor.f(nil);
        }",
    );

    assert!(errors.iter().any(|error| matches!(
        error,
        TypeError::CompileError { message, .. }
            if message.contains("cannot infer parameter type for dynamic method 'f'")
    )));
}

#[test]
fn inferred_dynamic_rejects_unsolved_return_type() {
    let errors = errors(
        "struct Actor { fn count(self) -> int { 1 } }
        fn main() {
            let actor: dyn _ = Actor {};
            let count = actor.count();
        }",
    );

    assert!(errors.iter().any(|error| matches!(
        error,
        TypeError::CompileError { message, .. }
            if message.contains("cannot infer return type for dynamic method 'count'")
    )));
}

#[test]
fn inferred_dynamic_rejects_stored_positions() {
    let errors = errors("struct Box { actor: dyn _ }");

    assert!(errors.iter().any(|error| matches!(
        error,
        TypeError::CompileError { message, .. }
            if message.contains("stored or ownerless type positions")
    )));
}

#[test]
fn anonymous_dynamic_contract_converts_and_calls() {
    let result = check(
        "struct Thing { fn draw(self) {} }
        fn main() {
            let value: dyn { fn draw(self); } = Thing {};
            value.draw();
        }",
    )
    .expect("typecheck failed");

    assert_eq!(result.contract_witnesses().len(), 1);
    assert_eq!(result.dyn_conversions().len(), 1);
    assert_eq!(result.dyn_calls().len(), 1);
}

#[test]
fn anonymous_dynamic_contract_rejects_missing_method() {
    let errors = errors(
        "struct Thing {}
        fn main() {
            let value: dyn { fn draw(self); } = Thing {};
        }",
    );

    assert!(errors.iter().any(|error| matches!(
        error,
        TypeError::ContractUnsatisfied { detail, .. } if detail.contains("missing method 'draw'")
    )));
}

#[test]
fn anonymous_dynamic_contract_weakens_from_named_surface() {
    check(
        "contract A { fn a(self); }
        contract B { fn b(self); }
        struct Both { fn a(self) {} fn b(self) {} }
        fn take(value: dyn { fn a(self); }) {}
        fn main() {
            let value: dyn A + B = Both {};
            take(value);
        }",
    )
    .expect("typecheck failed");
}

#[test]
fn dynamic_weakening_records_fact() {
    let result = check(
        "contract A { fn a(self); }
        contract B { fn b(self); }
        struct Both { fn a(self) {} fn b(self) {} }
        fn main() {
            let both: dyn A + B = Both {};
            let a: dyn A = both;
        }",
    )
    .expect("typecheck failed");

    assert_eq!(result.dyn_weakenings().len(), 1);
}

#[test]
fn dynamic_strengthening_is_rejected() {
    let errors = errors(
        "contract A { fn a(self); }
        contract B { fn b(self); }
        struct OnlyA { fn a(self) {} }
        fn main() {
            let a: dyn A = OnlyA {};
            let both: dyn A + B = a;
        }",
    );

    assert!(errors.iter().any(|error| matches!(
        error,
        TypeError::CompileError { message, .. }
            if message.contains("implicit dynamic strengthening is not allowed")
    )));
}

#[test]
fn deprecated_contract_declaration_does_not_warn() {
    let result = check("@deprecated contract Old { fn f(self); }").expect("typecheck failed");

    assert!(result.warnings().is_empty());
}

#[test]
fn deprecated_contract_reference_warns() {
    let result = check(
        "@deprecated(\"use New\") contract Old { fn f(self); }
        fn takes_old(x: dyn Old) {}",
    )
    .expect("typecheck failed");

    assert_deprecated_warning(&result, DeprecatedUseKind::Contract, "Old", Some("use New"));
}

#[test]
fn deprecated_contract_reference_warns_once_for_dynamic_call() {
    let result = check(
        "@deprecated contract Old { fn f(self); }
        struct Thing { fn f(self) {} }
        fn takes_old(x: dyn Old) { x.f(); }
        fn main() { takes_old(Thing {}); }",
    )
    .expect("typecheck failed");

    assert_deprecated_warning(&result, DeprecatedUseKind::Contract, "Old", None);
}

fn assert_matches(source: &str, ty_name: &str, contract_name: &str) -> contracts::ContractMatch {
    let mut tc = checker(source);
    let ty = root_type(&mut tc, ty_name);
    match contracts::match_contract(&mut tc, &ty, &contract(contract_name), Span::default()) {
        Ok(matched) => matched,
        Err(ContractMatchError::UnknownContract)
        | Err(ContractMatchError::ConflictingRequirement(_))
        | Err(ContractMatchError::Unsatisfied(_)) => panic!("contract should match"),
    }
}

fn mismatch(source: &str, ty_name: &str, contract_name: &str) -> RequirementError {
    let mut tc = checker(source);
    let ty = root_type(&mut tc, ty_name);
    let Err(ContractMatchError::Unsatisfied(err)) =
        contracts::match_contract(&mut tc, &ty, &contract(contract_name), Span::default())
    else {
        panic!("contract should not match");
    };
    err.reason
}

#[test]
fn direct_method_matches() {
    let matched = assert_matches(
        "contract Drawable { fn draw(self) -> int; }
        struct Sprite { fn draw(self) -> int { 1 } }",
        "Sprite",
        "Drawable",
    );

    assert_eq!(matched.slots.len(), 1);
    assert!(matches!(
        matched.slots[0].target,
        ContractSlotTarget::Direct(_)
    ));
}

#[test]
fn mutating_requirement_accepts_readonly_method() {
    assert_matches(
        "contract Drawable { fn draw(var self); }
        struct Sprite { fn draw(self) {} }",
        "Sprite",
        "Drawable",
    );
}

#[test]
fn readonly_requirement_rejects_mutating_method() {
    assert!(matches!(
        mismatch(
            "contract Drawable { fn draw(self); }
            struct Sprite { fn draw(var self) {} }",
            "Sprite",
            "Drawable",
        ),
        RequirementError::Receiver { .. }
    ));
}

#[test]
fn parameter_type_mismatch_is_specific() {
    assert!(matches!(
        mismatch(
            "contract Ticks { fn tick(var self, dt: float); }
            struct Timer { fn tick(var self, dt: int) {} }",
            "Timer",
            "Ticks",
        ),
        RequirementError::Param { index: 0, .. }
    ));
}

#[test]
fn return_type_mismatch_is_specific() {
    assert!(matches!(
        mismatch(
            "contract Named { fn name(self) -> string; }
            struct Enemy { fn name(self) -> int { 1 } }",
            "Enemy",
            "Named",
        ),
        RequirementError::Return { .. }
    ));
}

#[test]
fn default_parameter_does_not_reduce_arity() {
    assert!(matches!(
        mismatch(
            "contract Drawable { fn draw(self); }
            struct Sprite { fn draw(self, layer: int = 0) {} }",
            "Sprite",
            "Drawable",
        ),
        RequirementError::Arity {
            expected: 0,
            found: 1
        }
    ));
}

#[test]
fn concrete_defaults_do_not_change_matching_arity() {
    assert_matches(
        "contract Drawable { fn draw(self, layer: int); }
        struct Sprite { fn draw(self, layer: int = 0) {} }",
        "Sprite",
        "Drawable",
    );
}

#[test]
fn imported_extension_method_matches() {
    let mut tc = checker_with_modules(
        "import ai { Enemy };
        contract Updatable { fn update(var self, dt: float); }",
        &[
            ("enemy", "pub struct Enemy { hp: int }"),
            (
                "ai",
                "pub import enemy { Enemy };
                pub extend Enemy { fn update(var self, dt: float) {} }",
            ),
        ],
    );
    let ty = root_type(&mut tc, "Enemy");
    let matched =
        match contracts::match_contract(&mut tc, &ty, &contract("Updatable"), Span::default()) {
            Ok(matched) => matched,
            Err(ContractMatchError::UnknownContract)
            | Err(ContractMatchError::ConflictingRequirement(_))
            | Err(ContractMatchError::Unsatisfied(_)) => panic!("extension should match"),
        };

    assert!(matches!(
        matched.slots[0].target,
        ContractSlotTarget::Extend(_)
    ));
}

#[test]
fn missing_extension_import_is_missing_method() {
    let mut tc = checker_with_modules(
        "import enemy { Enemy };
        contract Updatable { fn update(var self, dt: float); }",
        &[
            ("enemy", "pub struct Enemy { hp: int }"),
            (
                "ai",
                "pub import enemy { Enemy };
                pub extend Enemy { fn update(var self, dt: float) {} }",
            ),
        ],
    );
    let ty = root_type(&mut tc, "Enemy");
    let Err(ContractMatchError::Unsatisfied(err)) =
        contracts::match_contract(&mut tc, &ty, &contract("Updatable"), Span::default())
    else {
        panic!("missing import should not match");
    };

    assert_eq!(err.reason, RequirementError::Missing);
}

#[test]
fn ambiguous_extension_method_is_ambiguous() {
    let mut tc = checker_with_modules(
        "import enemy { Enemy };
        import aggressive;
        import passive;
        contract Updatable { fn update(var self, dt: float); }",
        &[
            ("enemy", "pub struct Enemy { hp: int }"),
            (
                "aggressive",
                "pub import enemy { Enemy };
                pub extend Enemy { fn update(var self, dt: float) {} }",
            ),
            (
                "passive",
                "pub import enemy { Enemy };
                pub extend Enemy { fn update(var self, dt: float) {} }",
            ),
        ],
    );
    let ty = root_type(&mut tc, "Enemy");
    let Err(ContractMatchError::Unsatisfied(err)) =
        contracts::match_contract(&mut tc, &ty, &contract("Updatable"), Span::default())
    else {
        panic!("ambiguous extensions should not match");
    };

    assert_eq!(err.reason, RequirementError::Ambiguous);
}

#[test]
fn promoted_method_matches() {
    let matched = assert_matches(
        "contract Updatable { fn update(var self, dt: float); }
        struct Entity { fn update(var self, dt: float) {} }
        struct Enemy { embed entity: Entity }",
        "Enemy",
        "Updatable",
    );

    assert!(matches!(
        matched.slots[0].target,
        ContractSlotTarget::Promoted(_)
    ));
}

#[test]
fn extern_method_matches() {
    let matched = assert_matches(
        "contract Movable { fn move_by(var self, dx: float); }
        extern type Point { fn move_by(var self, dx: float); }",
        "Point",
        "Movable",
    );

    assert!(matches!(
        matched.slots[0].target,
        ContractSlotTarget::Extern(_)
    ));
}

#[test]
fn generic_method_rejected() {
    assert!(matches!(
        mismatch(
            "contract Boxed { fn get(self) -> int; }
            struct Box { fn get<T>(self) -> int { 1 } }",
            "Box",
            "Boxed",
        ),
        RequirementError::GenericMethod
    ));
}

#[test]
fn witness_key_includes_selected_extension() {
    let mut tc = checker_with_modules(
        "",
        &[
            (
                "api",
                "pub contract Updatable { fn update(var self, dt: float); }",
            ),
            ("enemy", "pub struct Enemy { hp: int }"),
            (
                "aggressive_ext",
                "pub import enemy { Enemy };
                pub extend Enemy { fn update(var self, dt: float) {} }",
            ),
            (
                "passive_ext",
                "pub import enemy { Enemy };
                pub extend Enemy { fn update(var self, dt: float) {} }",
            ),
            (
                "aggressive_use",
                "pub import api { Updatable };
                pub import aggressive_ext { Enemy };",
            ),
            (
                "passive_use",
                "pub import api { Updatable };
                pub import passive_ext { Enemy };",
            ),
        ],
    );

    let aggressive = module("aggressive_use");
    let aggressive_id = tc.with_current_module(&aggressive, |tc| {
        let ty = root_type(tc, "Enemy");
        let matched =
            match contracts::match_contract(tc, &ty, &contract("Updatable"), Span::default()) {
                Ok(matched) => matched,
                Err(ContractMatchError::UnknownContract)
                | Err(ContractMatchError::ConflictingRequirement(_))
                | Err(ContractMatchError::Unsatisfied(_)) => {
                    panic!("aggressive witness should match")
                }
            };
        contracts::plan_witness(tc, &matched, Span::default())
    });
    let passive = module("passive_use");
    let passive_id = tc.with_current_module(&passive, |tc| {
        let ty = root_type(tc, "Enemy");
        let matched =
            match contracts::match_contract(tc, &ty, &contract("Updatable"), Span::default()) {
                Ok(matched) => matched,
                Err(ContractMatchError::UnknownContract)
                | Err(ContractMatchError::ConflictingRequirement(_))
                | Err(ContractMatchError::Unsatisfied(_)) => {
                    panic!("passive witness should match")
                }
            };
        contracts::plan_witness(tc, &matched, Span::default())
    });

    assert_ne!(aggressive_id, passive_id);
    assert_eq!(tc.contract_witnesses.len(), 2);
}

#[test]
fn ordinary_check_still_closes_contract_requirement_types() {
    let result = check(
        "type Seconds = float;
        contract Ticks { fn tick(var self, dt: Seconds); }
        struct Timer { fn tick(var self, dt: float) {} }",
    )
    .expect("typecheck failed");

    assert_typecheck_closed(&result);
}
