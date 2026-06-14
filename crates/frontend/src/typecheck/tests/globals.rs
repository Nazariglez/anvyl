use super::support::{check, check_named};
use crate::{
    ast::Ident,
    typecheck::{
        BodyInstanceKey, CaptureStorage, GlobalAccessFact, GlobalAccessMode, GlobalInitEffect,
        GlobalKey, LambdaEscapeKind, ModuleScope,
    },
};

fn root_global(name: &str) -> BodyInstanceKey {
    BodyInstanceKey::Global(GlobalKey {
        module: ModuleScope::Root,
        name: Ident::new(name),
    })
}

fn modes(
    result: &super::support::TypecheckTestResult,
) -> Vec<(GlobalAccessMode, GlobalInitEffect)> {
    let mut facts = result
        .global_accesses()
        .values()
        .map(|fact| (fact.mode, fact.init_effect))
        .collect::<Vec<_>>();
    facts.sort();
    facts
}

#[test]
fn records_global_access_modes() {
    let result = check(
        "struct Counter { value: int }
         extend Counter { fn reset(var self) { self.value = 0; } }
         lazy var Count: Counter = Counter { value: 0 };
         fn set(var x: Counter) { x.value = 1; }
         fn main() {
             let x = Count;
             Count = Counter { value: 2 };
             Count.value = 3;
             set(Count);
             Count.reset();
         }",
    )
    .expect("typecheck");

    assert_eq!(
        modes(&result),
        vec![
            (GlobalAccessMode::Read, GlobalInitEffect::InitializeFirst),
            (
                GlobalAccessMode::RootAssign,
                GlobalInitEffect::StoreWithoutInit,
            ),
            (
                GlobalAccessMode::ProjectedAssign,
                GlobalInitEffect::InitializeFirst,
            ),
            (
                GlobalAccessMode::VarArgument,
                GlobalInitEffect::InitializeFirst,
            ),
            (
                GlobalAccessMode::MutReceiver,
                GlobalInitEffect::InitializeFirst,
            ),
        ]
    );
}

#[test]
fn records_projected_global_var_argument() {
    let result = check(
        "struct Health { @as embed hp: int }
         lazy var Player: Health = Health { hp: 1 };
         fn bump(var hp: int) { hp += 1; }
         fn main() { bump(Player); }",
    )
    .expect("typecheck");

    assert_eq!(
        modes(&result),
        vec![(
            GlobalAccessMode::VarArgument,
            GlobalInitEffect::InitializeFirst,
        )]
    );
}

#[test]
fn records_extern_receiver_access_modes() {
    let result = check(
        "extern type Counter {
             fn bump(var self);
             fn get(self) -> int;
         }
         extern fn make_counter() -> Counter;
         lazy var Count: Counter = make_counter();
         fn main() {
             Count.bump();
             let value = Count.get();
         }",
    )
    .expect("typecheck");

    assert_eq!(
        modes(&result),
        vec![
            (GlobalAccessMode::Read, GlobalInitEffect::InitializeFirst),
            (
                GlobalAccessMode::MutReceiver,
                GlobalInitEffect::InitializeFirst,
            ),
        ]
    );
}

#[test]
fn global_initializer_facts_are_keyed_by_global() {
    let result = check(
        "fn id(x: int) -> int { x }
         lazy let a = id(1);
         lazy let b = id(2);",
    )
    .expect("typecheck");

    assert_eq!(result.expect_body(&root_global("a")).calls.len(), 1);
    assert_eq!(result.expect_body(&root_global("b")).calls.len(), 1);
    assert!(
        result
            .body(&BodyInstanceKey::Module(ModuleScope::Root))
            .is_none_or(|body| body.calls.is_empty())
    );
}

#[test]
fn global_initializer_read_fact_uses_initializer_body() {
    let result = check(
        "lazy let other = 1;
         lazy let value = other + 1;",
    )
    .expect("typecheck");

    let body = result.expect_body(&root_global("value"));
    let facts = body.global_accesses.values().collect::<Vec<_>>();
    assert!(matches!(
        facts.as_slice(),
        [GlobalAccessFact {
            key: GlobalKey { module: ModuleScope::Root, name },
            mode: GlobalAccessMode::Read,
            init_effect: GlobalInitEffect::InitializeFirst,
            ..
        }] if *name == Ident::new("other")
    ));
    assert!(
        result
            .body(&BodyInstanceKey::Module(ModuleScope::Root))
            .is_none_or(|body| body.global_accesses.is_empty())
    );
}

#[test]
fn global_initializer_block_locals_are_recorded() {
    let result = check(
        "lazy let other = 1;
         lazy let value = { let y = other; y };",
    )
    .expect("typecheck");

    let body = result.expect_body(&root_global("value"));
    assert_eq!(body.locals.defs.len(), 1);
    assert_eq!(body.locals.binding_defs.len(), 1);
    assert_eq!(body.locals.uses.len(), 1);
    assert_eq!(body.global_accesses.len(), 1);
}

#[test]
fn lambda_inside_global_initializer_uses_lambda_body() {
    let result = check("lazy let make = || 1;").expect("typecheck");

    let global_body = result.expect_body(&root_global("make"));
    assert_eq!(global_body.function_values.len(), 1);
    assert!(global_body.calls.is_empty());
    let lambda_bodies = result
        .body_entries()
        .filter(|(key, _)| matches!(key, BodyInstanceKey::Lambda(_)))
        .collect::<Vec<_>>();
    assert_eq!(lambda_bodies.len(), 1);
    assert!(!lambda_bodies[0].1.expr_types.is_empty());
}

#[test]
fn global_initializer_lambda_value_escapes() {
    let result = check("lazy let make = { var y = 1; || { y += 1; y } };").expect("typecheck");

    assert!(
        result
            .lambda_escapes()
            .values()
            .any(|fact| fact.escape == LambdaEscapeKind::Escaping)
    );
    assert!(
        result
            .lambda_captures()
            .values()
            .any(|capture| capture.name == Ident::new("y")
                && capture.storage == CaptureStorage::OwnedMutableUpvalue)
    );
}

#[test]
fn imported_global_initializer_fact_keeps_declaring_identity() {
    let result = check_named(
        "import assets;
         lazy let count: int = assets.atlas + 1;",
        &[("assets", "pub lazy let atlas: int = 1;")],
    )
    .expect("typecheck");

    let body = result.expect_body(&root_global("count"));
    let facts = body.global_accesses.values().collect::<Vec<_>>();
    assert!(matches!(
        facts.as_slice(),
        [GlobalAccessFact {
            key: GlobalKey { module, name },
            mode: GlobalAccessMode::Read,
            init_effect: GlobalInitEffect::InitializeFirst,
            ..
        }] if module.to_string() == "assets" && *name == Ident::new("atlas")
    ));
}

#[test]
fn projected_global_fact_keeps_root_expr_id() {
    let result = check(
        "struct Box { value: int }
         lazy var State: Box = Box { value: 0 };
         fn main() -> int { State.value }",
    )
    .expect("typecheck");

    let facts = result.global_accesses().values().collect::<Vec<_>>();
    assert!(facts.iter().any(|fact| {
        fact.key.name == Ident::new("State")
            && fact.mode == GlobalAccessMode::Read
            && fact.root_expr_id != fact.expr_id
    }));
}

#[test]
fn records_qualified_global_root_assignment() {
    let result = check_named(
        "import services;
         fn main() { services.Count = 1; }",
        &[("services", "pub lazy var Count = 0;")],
    )
    .expect("typecheck");

    let facts = result.global_accesses().values().collect::<Vec<_>>();
    assert!(matches!(
        facts.as_slice(),
        [GlobalAccessFact {
            mode: GlobalAccessMode::RootAssign,
            init_effect: GlobalInitEffect::StoreWithoutInit,
            ..
        }]
    ));
}
