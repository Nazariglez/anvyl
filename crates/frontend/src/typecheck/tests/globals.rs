use super::support::{check, check_named};
use crate::typecheck::{GlobalAccessFact, GlobalAccessMode, GlobalInitEffect};

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
