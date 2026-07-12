use super::support::{assert_typecheck_closed, check, check_modules, generic_body, nominal_struct};
use crate::{
    ast::{ContractRef, Ident, Type},
    externs,
    span::Span,
    test_support::{empty_resolved, module_path, parse_program, resolved_modules},
    typecheck::{
        ModuleScope, TypecheckConfig,
        contract_surface::{ContractReturnSchema, ContractTypeSchema},
        contracts,
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
fn inclusion_duplicates_collapse() {
    let result = check(
        "contract A { fn draw(self); }
        contract B { fn draw(self); }
        contract C { A; B; }",
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
fn contract_surface_slots_preserve_modes() {
    let result = check(
        "contract Service {
            fn z(ref self, ref count: int, callback: escaping fn()) -> int;
            fn a(self);
        }",
    )
    .expect("typecheck failed");

    let surface = result.contract_surface(&contract("Service"));
    assert_eq!(surface.slots.len(), 2);
    assert_eq!(surface.slots[0].name, Ident::new("a"));
    assert_eq!(surface.slots[1].name, Ident::new("z"));
    assert_eq!(surface.slots[1].receiver, crate::ast::MethodReceiver::Ref);
    assert!(surface.slots[1].params[0].mutable);
    assert_eq!(
        surface.slots[1].params[1].escape,
        crate::ast::EscapeMode::Escaping
    );
    assert_eq!(
        surface.slots[1].ret,
        ContractReturnSchema::Value(ContractTypeSchema::Int)
    );
}

#[test]
fn contract_surface_preserves_iter_return() {
    let result = check("contract Stream { fn items(self) -> iter; }").expect("typecheck failed");

    let surface = result.contract_surface(&contract("Stream"));
    assert_eq!(surface.slots[0].ret, ContractReturnSchema::Iter);
}

#[test]
fn specialization_type_arguments_seed_surfaces() {
    let result = check(
        "contract A { fn a(self); }
        contract B { fn b(self); }
        fn marker<T>() {}
        fn main() { marker<dyn A + B>(); }",
    )
    .expect("typecheck failed");

    let composed = ContractRef::Intersection(vec![contract("A"), contract("B")]);
    assert_eq!(result.contract_surface(&composed).slots.len(), 2);
}

#[test]
fn recursive_contract_surfaces_canonicalize() {
    let result = check(
        "contract Left { fn next(self) -> (dyn Left)?; }
        contract Right { fn next(self) -> (dyn Right)?; }
        contract A { fn next(self) -> dyn B; }
        contract B { fn prev(self) -> dyn A; }
        contract X { fn next(self) -> dyn Y; }
        contract Y { fn prev(self) -> dyn X; }
        contract Different { fn next(self) -> int; }",
    )
    .expect("typecheck failed");

    assert_eq!(
        result.contract_surface_id(&contract("Left")),
        result.contract_surface_id(&contract("Right"))
    );
    assert_eq!(
        result.contract_surface_id(&contract("A")),
        result.contract_surface_id(&contract("X"))
    );
    assert_eq!(
        result.contract_surface_id(&contract("B")),
        result.contract_surface_id(&contract("Y"))
    );
    let left_id = result.contract_surface_id(&contract("Left"));
    assert_ne!(left_id, result.contract_surface_id(&contract("Different")));
    let left = result.contract_surface(&contract("Left"));
    let ContractReturnSchema::Value(ret) = &left.slots[0].ret else {
        panic!("expected value return");
    };
    let recursive_edge = match ret {
        ContractTypeSchema::Optional(inner) => inner.as_ref(),
        ContractTypeSchema::Nominal { type_args, .. } => &type_args[0],
        other => panic!("expected optional recursive return, found {other:?}"),
    };
    assert_eq!(*recursive_edge, ContractTypeSchema::Dyn(left_id));
    let b_id = result.contract_surface_id(&contract("B"));
    let a = result.contract_surface(&contract("A"));
    assert_eq!(
        a.slots[0].ret,
        ContractReturnSchema::Value(ContractTypeSchema::Dyn(b_id))
    );
}

#[test]
fn inferred_surface_dependencies_are_canonical() {
    let result = check(
        "fn connect(left: dyn _, right: dyn _) {
            left.link(right);
            right.draw();
        }",
    )
    .expect("typecheck failed");

    let function = result
        .function_facts()
        .iter()
        .find(|function| function.name == Ident::new("connect"))
        .expect("missing function");
    let Type::Dyn(left) = &function.params[0].ty else {
        panic!("expected left dynamic parameter");
    };
    let Type::Dyn(right) = &function.params[1].ty else {
        panic!("expected right dynamic parameter");
    };
    let right_id = result.contract_surface_id(right);
    let left = result.contract_surface(left);
    assert_eq!(
        left.slots[0].params[0].ty,
        ContractTypeSchema::Dyn(right_id)
    );
}

#[test]
fn mutually_inferred_surfaces_terminate() {
    let result = check(
        "fn connect(left: dyn _, right: dyn _) {
            left.link(right);
            right.link(left);
        }",
    )
    .expect("typecheck failed");

    let function = result
        .function_facts()
        .iter()
        .find(|function| function.name == Ident::new("connect"))
        .expect("missing function");
    let Type::Dyn(left) = &function.params[0].ty else {
        panic!("expected left dynamic parameter");
    };
    let Type::Dyn(right) = &function.params[1].ty else {
        panic!("expected right dynamic parameter");
    };
    let left_id = result.contract_surface_id(left);
    assert_eq!(left_id, result.contract_surface_id(right));
    let left = result.contract_surface(left);
    assert_eq!(left.slots[0].params[0].ty, ContractTypeSchema::Dyn(left_id));
}

#[test]
fn contract_surface_ids_ignore_declaration_order() {
    let forward = check(
        "contract A { fn next(self) -> dyn B; }
        contract B { fn prev(self) -> dyn A; }",
    )
    .expect("typecheck failed");
    let reverse = check(
        "contract B { fn prev(self) -> dyn A; }
        contract A { fn next(self) -> dyn B; }",
    )
    .expect("typecheck failed");

    assert_eq!(
        forward.contract_surface_id(&contract("A")),
        reverse.contract_surface_id(&contract("A"))
    );
    assert_eq!(
        forward.contract_surface_id(&contract("B")),
        reverse.contract_surface_id(&contract("B"))
    );
}

#[test]
fn storage_only_composed_surface_is_interned() {
    let result = check(
        "contract A { fn a(self); }
        contract B { fn b(self); }
        struct Holder { item: dyn A + B }",
    )
    .expect("typecheck failed");

    let composed = ContractRef::Intersection(vec![contract("A"), contract("B")]);
    let surface = result.contract_surface(&composed);
    assert_eq!(surface.slots.len(), 2);
}

#[test]
fn dynamic_intersection_matches_and_canonicalizes_order() {
    let result = check(
        "contract A { fn a(self); }
        contract B { fn b(self); }
        contract Included { A; B; }
        struct Both { fn a(self) {} fn b(self) {} }
        fn inferred(value: dyn _) { value.a(); value.b(); }
        fn first(x: Both) { let y: dyn A + B = x; y.a(); y.b(); }
        fn second(x: Both) { let y: dyn B + A = x; y.a(); y.b(); }",
    )
    .expect("typecheck failed");

    let composed = ContractRef::Intersection(vec![contract("A"), contract("B")]);
    let reversed = ContractRef::Intersection(vec![contract("B"), contract("A")]);
    let included = result.contract_surface_id(&contract("Included"));
    assert_eq!(result.contract_surface_id(&composed), included);
    assert_eq!(result.contract_surface_id(&reversed), included);

    let inferred = result
        .function_facts()
        .iter()
        .find(|function| function.name == Ident::new("inferred"))
        .expect("missing inferred function");
    let Type::Dyn(inferred) = &inferred.params[0].ty else {
        panic!("expected inferred dynamic parameter");
    };
    assert_eq!(result.contract_surface_id(inferred), included);
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
            let b: dyn B = both;
            let reversed: dyn B + A = Both {};
            let other: dyn B = reversed;
        }",
    )
    .expect("typecheck failed");

    let source = result.contract_surface(&ContractRef::Intersection(vec![
        contract("A"),
        contract("B"),
    ]));
    let target = result.contract_surface(&contract("B"));
    assert_eq!(result.dyn_weakenings().len(), 2);
    assert!(result.dyn_weakenings().values().all(|weakening| {
        weakening.source == source.id
            && weakening.target == target.id
            && weakening.target_to_source == [source.slots[1].id]
    }));
}

#[test]
fn equal_dynamic_surfaces_do_not_weaken() {
    let result = check(
        "contract A { fn a(self); }
        contract Same { fn a(self); }
        struct Item { fn a(self) {} }
        fn main() {
            let item: dyn A + Same = Item {};
            let same: dyn Same = item;
        }",
    )
    .expect("typecheck failed");

    assert!(result.dyn_weakenings().is_empty());
    assert_eq!(
        result.contract_surface_id(&contract("A")),
        result.contract_surface_id(&contract("Same"))
    );
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
            .all(|fact| !fact.requires_mutable)
    );
}

#[test]
fn cached_generic_specialization_restores_dyn_downcast_facts() {
    let result = check(
        "struct Enemy { fn draw(self) {} fn attack(self) {} }
        struct Label { fn draw(self) {} }
        fn use_actor(actor: dyn _) {
            actor.draw();
            if let enemy? = actor as? Enemy {
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

    let surface = result.contract_surface(&contract("Drawable"));
    let fact = result.dyn_calls().values().next().expect("missing call");
    assert_eq!(fact.surface, surface.id);
    assert_eq!(fact.slot, surface.slots[0].id);
    assert_ne!(fact.call_id, fact.receiver_id);
}

#[test]
fn for_ref_dynamic_call_records_fact() {
    let result = check(
        "contract Updatable { fn update(ref self); }
        struct Enemy { fn update(ref self) {} }
        fn main() {
            var items: [dyn Updatable] = [Enemy {}];
            for ref item in items {
                item.update();
            }
        }",
    )
    .expect("typecheck failed");

    let surface = result.contract_surface(&contract("Updatable"));
    let fact = result.dyn_calls().values().next().expect("missing call");
    assert_eq!(fact.surface, surface.id);
    assert_eq!(fact.slot, surface.slots[0].id);
    assert!(fact.requires_mutable);
    assert_ne!(fact.call_id, fact.receiver_id);
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
fn resolved_target_families_are_preserved() {
    let direct = assert_matches(
        "contract Drawable { fn draw(self) -> int; }
        struct Sprite { fn draw(self) -> int { 1 } }",
        "Sprite",
        "Drawable",
    );

    let mut tc = checker_with_modules(
        "import ai { Enemy };
        contract Updatable { fn update(ref self, dt: float); }",
        &[
            ("enemy", "pub struct Enemy { hp: int }"),
            (
                "ai",
                "pub import enemy { Enemy };
                pub extend Enemy { fn update(ref self, dt: float) {} }",
            ),
        ],
    );
    let ty = root_type(&mut tc, "Enemy");
    let Ok(extended) =
        contracts::match_contract(&mut tc, &ty, &contract("Updatable"), Span::default())
    else {
        panic!("extension should match");
    };

    let promoted = assert_matches(
        "contract Updatable { fn update(ref self, dt: float); }
        struct Entity { fn update(ref self, dt: float) {} }
        struct Enemy { embed entity: Entity }",
        "Enemy",
        "Updatable",
    );
    let external = assert_matches(
        "contract Movable { fn move_by(ref self, dx: float); }
        extern type Point { fn move_by(ref self, dx: float); }",
        "Point",
        "Movable",
    );

    assert!(matches!(
        direct.slots[0].target,
        ContractSlotTarget::Direct(_)
    ));
    assert!(matches!(
        extended.slots[0].target,
        ContractSlotTarget::Extend(_)
    ));
    assert!(matches!(
        promoted.slots[0].target,
        ContractSlotTarget::Promoted(_)
    ));
    assert!(matches!(
        external.slots[0].target,
        ContractSlotTarget::Extern(_)
    ));
}

#[test]
fn signature_mismatches_are_classified() {
    let cases = [
        (
            "contract Drawable { fn draw(self); }
            struct Sprite { fn draw(ref self) {} }",
            "Sprite",
            "Drawable",
            "receiver",
        ),
        (
            "contract Ticks { fn tick(ref self, dt: float); }
            struct Timer { fn tick(ref self, dt: int) {} }",
            "Timer",
            "Ticks",
            "param",
        ),
        (
            "contract Named { fn name(self) -> string; }
            struct Enemy { fn name(self) -> int { 1 } }",
            "Enemy",
            "Named",
            "return",
        ),
        (
            "contract Drawable { fn draw(self); }
            struct Sprite { fn draw(self, layer: int = 0) {} }",
            "Sprite",
            "Drawable",
            "arity",
        ),
        (
            "contract Boxed { fn get(self) -> int; }
            struct Box { fn get<T>(self) -> int { 1 } }",
            "Box",
            "Boxed",
            "generic",
        ),
    ];

    for (source, ty, contract, expected) in cases {
        let err = mismatch(source, ty, contract);
        let classified = match expected {
            "receiver" => matches!(err, RequirementError::Receiver { .. }),
            "param" => matches!(err, RequirementError::Param { index: 0, .. }),
            "return" => matches!(err, RequirementError::Return { .. }),
            "arity" => matches!(
                err,
                RequirementError::Arity {
                    expected: 0,
                    found: 1
                }
            ),
            "generic" => matches!(err, RequirementError::GenericMethod),
            _ => unreachable!(),
        };
        assert!(classified, "expected {expected}, found {err:?}");
    }
}

#[test]
fn witness_structural_keys_dedupe_spans() {
    let result = check(
        "contract A { fn a(self); }
        struct Item { fn a(self) {} }
        fn take(item: dyn A) {}
        fn main() {
            take(Item {});
            take(Item {});
        }",
    )
    .expect("typecheck failed");

    assert_eq!(result.contract_witnesses().len(), 1);
    let key = result
        .witness_structural_keys()
        .values()
        .next()
        .expect("missing structural key");
    assert_eq!(key.surface, result.contract_surface_id(&contract("A")));
    assert_eq!(key.slots.len(), 1);
}

#[test]
fn witness_structural_keys_ignore_declaration_order() {
    let forward = check(
        "contract A { fn a(self); }
        struct Item { fn a(self) {} }
        fn main() { let item: dyn A = Item {}; }",
    )
    .expect("typecheck failed");
    let reverse = check(
        "struct Item { fn a(self) {} }
        contract A { fn a(self); }
        fn main() { let item: dyn A = Item {}; }",
    )
    .expect("typecheck failed");

    assert_eq!(
        forward
            .witness_structural_keys()
            .values()
            .next()
            .expect("missing forward key"),
        reverse
            .witness_structural_keys()
            .values()
            .next()
            .expect("missing reverse key")
    );
}

#[test]
fn witness_structural_keys_preserve_lexical_targets() {
    let result = check_modules(
        "import aggressive_use;
        import passive_use;",
        &[
            ("api", "pub contract A { fn a(self); }"),
            ("item", "pub struct Item {}"),
            (
                "aggressive_ext",
                "pub import item { Item };
                pub extend Item { fn a(self) {} }",
            ),
            (
                "passive_ext",
                "pub import item { Item };
                pub extend Item { fn a(self) {} }",
            ),
            (
                "aggressive_use",
                "pub import api { A };
                pub import aggressive_ext { Item };
                pub fn make(item: Item) -> dyn A { item }",
            ),
            (
                "passive_use",
                "pub import api { A };
                pub import passive_ext { Item };
                pub fn make(item: Item) -> dyn A { item }",
            ),
        ],
    )
    .expect("typecheck failed");

    let keys = result
        .witness_structural_keys()
        .values()
        .collect::<Vec<_>>();
    assert_eq!(keys.len(), 2);
    assert_eq!(keys[0].concrete_ty, keys[1].concrete_ty);
    assert_eq!(keys[0].surface, keys[1].surface);
    assert_ne!(keys[0].slots, keys[1].slots);
}

#[test]
fn witness_key_includes_selected_extension() {
    let mut tc = checker_with_modules(
        "",
        &[
            (
                "api",
                "pub contract Updatable { fn update(ref self, dt: float); }",
            ),
            ("enemy", "pub struct Enemy { hp: int }"),
            (
                "aggressive_ext",
                "pub import enemy { Enemy };
                pub extend Enemy { fn update(ref self, dt: float) {} }",
            ),
            (
                "passive_ext",
                "pub import enemy { Enemy };
                pub extend Enemy { fn update(ref self, dt: float) {} }",
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
        contract Ticks { fn tick(ref self, dt: Seconds); }
        struct Timer { fn tick(ref self, dt: float) {} }",
    )
    .expect("typecheck failed");

    assert_typecheck_closed(&result);
}
