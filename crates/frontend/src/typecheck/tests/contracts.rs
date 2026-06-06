use super::support::{assert_typecheck_closed, check, generic_body, nominal_struct};
use crate::{
    ast::{ContractRef, Ident, Type},
    externs,
    span::Span,
    test_support::{empty_resolved, module_path, parse_program, resolved_modules},
    typecheck::{
        ModuleScope, TypecheckConfig, contracts,
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
    let raw_externs =
        externs::prepare_raw_externs(externs::RawExterns::default(), &program, &resolved).unwrap();
    typechecker_for_modules(&program, &resolved, raw_externs, TypecheckConfig::default())
        .expect("typecheck failed")
}

fn checker_with_modules(source: &str, modules: &[(&str, &str)]) -> crate::typecheck::TypeChecker {
    let program = parse_program(source);
    let resolved = resolved_modules(&program, modules);
    let raw_externs =
        externs::prepare_raw_externs(externs::RawExterns::default(), &program, &resolved).unwrap();
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
fn cached_generic_specialization_restores_dyn_conversion_facts() {
    let result = check(
        "contract Drawable { fn draw(self); }
        struct Enemy { fn draw(self) {} }
        struct Label { fn draw(self) {} }
        fn use_actor(actor: dyn Drawable) {}
        fn wrap<T>(actor: T) { use_actor(actor); }
        fn main() {
            wrap(Enemy {});
            wrap(Label {});
            wrap(Enemy {});
        }",
    )
    .expect("typecheck failed");

    let witnesses = result.contract_witnesses();
    let concrete = result
        .bodies()
        .flat_map(|body| body.dyn_conversions.values())
        .map(|conversion| &witnesses[&conversion.witness].key.concrete_ty)
        .collect::<Vec<_>>();
    assert!(concrete.contains(&&nominal_struct("Enemy")));
    assert!(concrete.contains(&&nominal_struct("Label")));
    assert_eq!(concrete.len(), 2);
}

#[test]
fn cached_generic_specialization_restores_dyn_call_facts() {
    let result = check(
        "struct Enemy { fn draw(self) {} }
        struct Label { fn draw(self) {} }
        fn use_actor(actor: dyn _) { actor.draw(); }
        fn wrap<T>(actor: T) { use_actor(actor); }
        fn main() {
            wrap(Enemy {});
            wrap(Label {});
            wrap(Enemy {});
        }",
    )
    .expect("typecheck failed");

    assert_eq!(result.dyn_calls().len(), 1);
    assert!(
        result
            .dyn_calls()
            .values()
            .all(|fact| fact.method == Ident::new("draw") && !fact.requires_mutable)
    );
}

#[test]
fn cached_generic_specialization_restores_dyn_downcast_facts() {
    let result = check(
        "struct Enemy { fn draw(self) {} fn attack(self) {} }
        struct Label { fn draw(self) {} }
        fn use_actor(actor: dyn _) {
            actor.draw();
            if let enemy = actor as? Enemy {
                enemy.attack();
            }
        }
        fn wrap<T>(actor: T) { use_actor(actor); }
        fn main() {
            wrap(Enemy {});
            wrap(Label {});
            wrap(Enemy {});
        }",
    )
    .expect("typecheck failed");

    let enemy = nominal_struct("Enemy");
    assert_eq!(result.dyn_downcasts().len(), 1);
    assert!(
        result.dyn_downcasts().values().all(|fact| {
            fact.target == enemy && !fact.mutable && fact.expr_id != fact.source_id
        })
    );
}

#[test]
fn inferred_dyn_pending_facts_keep_specialized_body() {
    let result = check(
        "struct Enemy { fn draw(self) {} }
        struct Label { fn draw(self) {} }
        fn use_actor(actor: dyn _) { actor.draw(); }
        fn wrap<T>(actor: T) { use_actor(actor); }
        fn main() {
            wrap(Enemy {});
            wrap(Label {});
            wrap(Enemy {});
        }",
    )
    .expect("typecheck failed");

    for ty in [nominal_struct("Enemy"), nominal_struct("Label")] {
        let key = generic_body("wrap", vec![ty]);
        let body = result.expect_body(&key);
        assert_eq!(body.dyn_conversions.len(), 1);
    }
    assert_typecheck_closed(&result);
}

#[test]
fn dynamic_collection_literal_records_element_conversions() {
    let result = check(
        "contract Drawable { fn draw(self); }
        struct Sprite { fn draw(self) {} }
        struct Label { fn draw(self) {} }
        fn main() {
            let items: [dyn Drawable] = [Sprite {}, Label {}];
        }",
    )
    .expect("typecheck failed");

    assert_eq!(result.dyn_conversions().len(), 2);
}

#[test]
fn indexed_dynamic_call_records_fact() {
    let result = check(
        "contract Drawable { fn draw(self); }
        struct Sprite { fn draw(self) {} }
        fn main() {
            let items: [dyn Drawable] = [Sprite {}];
            items[0].draw();
        }",
    )
    .expect("typecheck failed");

    assert_eq!(result.dyn_calls().len(), 1);
}

#[test]
fn for_var_dynamic_call_records_fact() {
    let result = check(
        "contract Updatable { fn update(var self); }
        struct Enemy { fn update(var self) {} }
        fn main() {
            var items: [dyn Updatable] = [Enemy {}];
            for var item in items {
                item.update();
            }
        }",
    )
    .expect("typecheck failed");

    assert_eq!(result.dyn_calls().len(), 1);
}

fn assert_matches(source: &str, ty_name: &str, contract_name: &str) -> contracts::ContractMatch {
    let mut tc = checker(source);
    let ty = root_type(&mut tc, ty_name);
    match contracts::match_contract(&mut tc, &ty, &contract(contract_name), Span::default()) {
        Ok(matched) => matched,
        Err(
            ContractMatchError::UnknownContract
            | ContractMatchError::ConflictingRequirement(_)
            | ContractMatchError::Unsatisfied(_),
        ) => panic!("contract should match"),
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
            Err(
                ContractMatchError::UnknownContract
                | ContractMatchError::ConflictingRequirement(_)
                | ContractMatchError::Unsatisfied(_),
            ) => panic!("extension should match"),
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
                Err(
                    ContractMatchError::UnknownContract
                    | ContractMatchError::ConflictingRequirement(_)
                    | ContractMatchError::Unsatisfied(_),
                ) => panic!("aggressive witness should match"),
            };
        contracts::plan_witness(tc, &matched, Span::default())
    });
    let passive = module("passive_use");
    let passive_id = tc.with_current_module(&passive, |tc| {
        let ty = root_type(tc, "Enemy");
        let matched =
            match contracts::match_contract(tc, &ty, &contract("Updatable"), Span::default()) {
                Ok(matched) => matched,
                Err(
                    ContractMatchError::UnknownContract
                    | ContractMatchError::ConflictingRequirement(_)
                    | ContractMatchError::Unsatisfied(_),
                ) => panic!("passive witness should match"),
            };
        contracts::plan_witness(tc, &matched, Span::default())
    });

    assert_ne!(aggressive_id, passive_id);
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
