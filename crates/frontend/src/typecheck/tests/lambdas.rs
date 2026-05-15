use super::support::{TypecheckTestResult, check};
use crate::{
    ast::Type,
    typecheck::{
        CaptureAccess, CaptureStorage, CaptureStorageOrigin, LambdaCaptureFact, LambdaEscapeKind,
    },
};

fn capture<'a>(result: &'a TypecheckTestResult, name: &str) -> &'a LambdaCaptureFact {
    result
        .lambda_captures()
        .values()
        .find(|capture| capture.name.as_str() == name)
        .unwrap_or_else(|| panic!("{name} capture fact"))
}

fn promoted(result: &TypecheckTestResult, capture: &LambdaCaptureFact) -> bool {
    result
        .binding_promotions()
        .contains_key(&capture.binding_id)
}

fn checked(source: &str) -> TypecheckTestResult {
    check(source).expect("program should typecheck")
}

#[test]
fn shadowed_captures_use_distinct_binding_ids() {
    let result = checked(
        r#"
        fn main() {
            var x = 1;
            let a = || { x = 2; };
            {
                var x = 3;
                let b = || { x = 4; };
            }
        }
        "#,
    );

    let captures = result
        .lambda_captures()
        .values()
        .filter(|capture| capture.name.as_str() == "x")
        .collect::<Vec<_>>();
    assert_eq!(captures.len(), 2);
    assert_ne!(captures[0].binding_id, captures[1].binding_id);
}

#[test]
fn escaping_mutable_capture_records_promotion() {
    let result = checked(
        r#"
        fn make() -> fn() {
            var x = 1;
            || { x = 2; }
        }
        "#,
    );

    let capture = capture(&result, "x");
    assert_eq!(capture.origin, CaptureStorageOrigin::Owned);
    assert_eq!(capture.storage, CaptureStorage::OwnedMutableUpvalue);
    assert!(promoted(&result, capture));
    assert!(
        result
            .lambda_escapes()
            .values()
            .any(|fact| fact.escape == LambdaEscapeKind::Escaping)
    );
}

#[test]
fn local_function_alias_stays_non_escaping() {
    let result = checked(
        r#"
        fn main() {
            var x = 1;
            let f = || { x = 2; };
            f();
        }
        "#,
    );

    let capture = capture(&result, "x");
    assert_eq!(capture.storage, CaptureStorage::OwnedMutableScoped);
    assert!(!promoted(&result, capture));
    assert!(
        result
            .lambda_escapes()
            .values()
            .all(|fact| fact.escape == LambdaEscapeKind::NonEscaping)
    );
}

#[test]
fn escaping_read_of_mutable_binding_records_promotion() {
    let result = checked(
        r#"
        fn make() -> fn() {
            var x = 1;
            || { let y = x; }
        }
        "#,
    );

    let capture = capture(&result, "x");
    assert_eq!(capture.access, CaptureAccess::Read);
    assert_eq!(capture.storage, CaptureStorage::OwnedMutableUpvalue);
    assert!(promoted(&result, capture));
}

#[test]
fn projected_var_argument_records_mutable_capture_access() {
    let result = checked(
        r#"
        struct Point { x: int }

        fn bump(var x: int) {}

        fn main() {
            var p = Point { x: 0 };
            let f = || { bump(p.x); };
            f();
        }
        "#,
    );

    assert_eq!(capture(&result, "p").access, CaptureAccess::Mutable);
}

#[test]
fn indexed_mut_receiver_records_mutable_capture_access() {
    let result = checked(
        r#"
        struct Player {
            hp: int,

            fn reset(var self) {
                self.hp = 0;
            }
        }

        fn main() {
            var players: [Player] = [Player { hp: 1 }];
            let f = || { players[0].reset(); };
            f();
        }
        "#,
    );

    assert_eq!(capture(&result, "players").access, CaptureAccess::Mutable);
}

#[test]
fn branch_alias_to_distinct_lambdas_marks_both_escaping() {
    let result = checked(
        r#"
        fn make(cond: bool) -> fn() {
            var x = 1;
            var y = 2;
            var f = || { let a = x; };
            if cond {
                f = || { let b = y; };
            }
            f
        }
        "#,
    );

    let promoted = result
        .lambda_captures()
        .values()
        .filter(|capture| matches!(capture.name.as_str(), "x" | "y"))
        .filter(|capture| promoted(&result, capture))
        .count();
    assert_eq!(promoted, 2);
}

#[test]
fn escaping_capture_of_local_closure_marks_closure_escaping() {
    let result = checked(
        r#"
        fn make() -> fn() {
            var x = 1;
            let inner = || { x = 2; };
            || { inner(); }
        }
        "#,
    );

    let capture = capture(&result, "x");
    assert_eq!(capture.storage, CaptureStorage::OwnedMutableUpvalue);
    assert!(promoted(&result, capture));
}

#[test]
fn generic_specialization_preserves_capture_facts() {
    let result = checked(
        r#"
        fn make<T>(x: T) -> fn() {
            || { let y = x; }
        }

        fn main() {
            let f = make(1);
            let g = make("x");
        }
        "#,
    );

    let captures = result
        .lambda_captures()
        .values()
        .filter(|capture| capture.name.as_str() == "x")
        .collect::<Vec<_>>();
    assert_eq!(captures.len(), 2);
    assert!(captures.iter().all(|capture| {
        capture.origin == CaptureStorageOrigin::Owned
            && capture.storage == CaptureStorage::OwnedReadonly
    }));
    assert!(captures.iter().any(|capture| capture.ty == Type::Int));
    assert!(captures.iter().any(|capture| capture.ty == Type::String));
}

#[test]
fn non_escaping_borrowed_capture_records_scoped_storage() {
    let result = checked(
        r#"
        fn touch(var x: int) {
            let f = || { x = 1; };
            f();
        }
        "#,
    );

    let capture = capture(&result, "x");
    assert_eq!(capture.origin, CaptureStorageOrigin::BorrowedParam);
    assert_eq!(capture.storage, CaptureStorage::BorrowedScoped);
    assert!(!promoted(&result, capture));
}
