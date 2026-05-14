use super::support::{assert_deprecated_warning, check, check_named, errors};
use crate::{
    ast::Ident,
    typecheck::{
        DeprecatedUseKind, GlobalAccessFact, GlobalAccessMode, GlobalInitEffect, TypeError,
    },
};

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
         fn bump(var hp: as int) { hp += 1; }
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

#[test]
fn infers_forward_global_reference() {
    check(
        "lazy let A = B;
         lazy let B = 1;
         fn main() { let x: int = A; }",
    )
    .expect("typecheck");
}

#[test]
fn rejects_runtime_global_in_const_position() {
    let errors = errors("lazy let Size = 4; fn main() { let xs = [0; Size]; }");
    assert!(errors.iter().any(|error| matches!(
        error,
        TypeError::RuntimeGlobalInConstPosition { global, .. }
            if global.name == Ident::new("Size")
    )));
}

#[test]
fn warns_on_deprecated_global_read() {
    let result = check(
        "@deprecated(\"use Other\")
         lazy let Old = 1;
         fn main() { let x = Old; }",
    )
    .expect("typecheck");

    assert_deprecated_warning(&result, DeprecatedUseKind::Global, "Old", Some("use Other"));
}
