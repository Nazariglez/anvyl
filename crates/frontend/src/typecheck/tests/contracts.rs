use super::support::{assert_typecheck_closed, check};
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

fn assert_matches(source: &str, ty_name: &str, contract_name: &str) -> contracts::ContractMatch {
    let mut tc = checker(source);
    let ty = root_type(&mut tc, ty_name);
    match contracts::match_contract(&mut tc, &ty, &contract(contract_name), Span::default()) {
        Ok(matched) => matched,
        Err(_) => panic!("contract should match"),
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
            Err(_) => panic!("extension should match"),
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
                Err(_) => panic!("aggressive witness should match"),
            };
        contracts::plan_witness(tc, &matched, Span::default())
    });
    let passive = module("passive_use");
    let passive_id = tc.with_current_module(&passive, |tc| {
        let ty = root_type(tc, "Enemy");
        let matched =
            match contracts::match_contract(tc, &ty, &contract("Updatable"), Span::default()) {
                Ok(matched) => matched,
                Err(_) => panic!("passive witness should match"),
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
