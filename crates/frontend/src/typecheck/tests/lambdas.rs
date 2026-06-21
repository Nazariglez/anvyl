use super::support::{TypecheckTestResult, check};
use crate::{
    ast::Type,
    typecheck::{
        CaptureAccess, CaptureStorage, CaptureStorageOrigin, LambdaCaptureFact, LambdaEscapeKind,
    },
};

fn captures<'a>(result: &'a TypecheckTestResult, name: &str) -> Vec<&'a LambdaCaptureFact> {
    result
        .lambda_captures()
        .values()
        .filter(|capture| capture.name.as_str() == name)
        .collect()
}

fn capture<'a>(result: &'a TypecheckTestResult, name: &str) -> &'a LambdaCaptureFact {
    captures(result, name)
        .into_iter()
        .next()
        .unwrap_or_else(|| panic!("{name} capture fact"))
}

fn requires_cell(result: &TypecheckTestResult, capture: &LambdaCaptureFact) -> bool {
    result
        .capture_cell_requirements()
        .contains_key(&capture.binding_id)
}

fn escaping_lambdas(result: &TypecheckTestResult) -> usize {
    result
        .lambda_escapes()
        .values()
        .filter(|fact| fact.escape == LambdaEscapeKind::Escaping)
        .count()
}

fn function_capture<'a>(
    result: &'a TypecheckTestResult,
    name: &str,
    storage: CaptureStorage,
) -> &'a LambdaCaptureFact {
    let capture = capture(result, name);
    assert!(matches!(capture.ty, Type::Func { .. }));
    assert_eq!(capture.storage, storage);
    capture
}

fn checked(source: &str) -> TypecheckTestResult {
    check(source).expect("program should typecheck")
}

fn assert_borrowed_scoped_capture(result: &TypecheckTestResult, name: &str) {
    let capture = capture(result, name);
    assert_eq!(capture.origin, CaptureStorageOrigin::BorrowedParam);
    assert_eq!(capture.storage, CaptureStorage::BorrowedScoped);
    assert!(!requires_cell(result, capture));
}

#[test]
fn non_escaping_mutable_captures_share_one_cell_requirement() {
    let result = checked(
        r"
        fn main() {
            var x = 0;
            let a = || { x = 1; };
            let b = || { x = 2; };
            a();
            b();
        }
        ",
    );

    let captures = captures(&result, "x");
    assert_eq!(captures.len(), 2);
    assert!(
        captures
            .iter()
            .all(|capture| requires_cell(&result, capture))
    );
    let binding = captures[0].binding_id;
    assert!(captures.iter().all(|capture| capture.binding_id == binding));
    assert_eq!(result.capture_cell_requirements().len(), 1);
}

#[test]
fn shadowed_captures_use_distinct_cell_requirements() {
    let result = checked(
        r"
        fn main() {
            var x = 1;
            let a = || { x = 2; };
            {
                var x = 3;
                let b = || { x = 4; };
            }
        }
        ",
    );

    let captures = captures(&result, "x");
    assert_eq!(captures.len(), 2);
    assert!(
        captures
            .iter()
            .all(|capture| requires_cell(&result, capture))
    );
    assert_ne!(captures[0].binding_id, captures[1].binding_id);
    assert_eq!(result.capture_cell_requirements().len(), 2);
}

#[test]
fn returned_counter_records_cell_requirement() {
    let result = checked(
        r"
        fn make_counter() -> fn() -> int {
            var count = 0;
            || {
                count = count + 1;
                count
            }
        }
        ",
    );

    let capture = capture(&result, "count");
    assert_eq!(capture.origin, CaptureStorageOrigin::Owned);
    assert_eq!(capture.storage, CaptureStorage::OwnedMutableUpvalue);
    assert!(requires_cell(&result, capture));
    assert!(
        result
            .lambda_escapes()
            .values()
            .any(|fact| fact.escape == LambdaEscapeKind::Escaping)
    );
}

#[test]
fn returned_lambdas_share_one_mutable_cell_requirement() {
    let result = checked(
        r"
        fn make(cond: bool) -> fn() -> int {
            var count = 0;
            let inc = || {
                count = count + 1;
                count
            };
            let get = || count;
            if cond { inc } else { get }
        }
        ",
    );

    let captures = captures(&result, "count");
    assert_eq!(captures.len(), 2);
    assert!(captures.iter().all(|capture| {
        capture.storage == CaptureStorage::OwnedMutableUpvalue && requires_cell(&result, capture)
    }));
    let binding = captures[0].binding_id;
    assert!(captures.iter().all(|capture| capture.binding_id == binding));
    assert_eq!(result.capture_cell_requirements().len(), 1);
}

#[test]
fn nested_escaping_mutable_capture_records_cell_requirement() {
    let result = checked(
        r"
        fn make() -> fn() -> fn() -> int {
            var count = 0;
            || {
                || {
                    count = count + 1;
                    count
                }
            }
        }
        ",
    );

    let capture = capture(&result, "count");
    assert_eq!(capture.storage, CaptureStorage::OwnedMutableUpvalue);
    assert!(requires_cell(&result, capture));
    assert_eq!(result.capture_cell_requirements().len(), 1);
}

#[test]
fn readonly_and_writer_lambdas_share_mutable_cell_requirement() {
    let result = checked(
        r"
        fn make(cond: bool) -> fn() -> int {
            var count = 0;
            let read = || count;
            let write = || {
                count = count + 1;
                count
            };
            if cond { read } else { write }
        }
        ",
    );

    let captures = captures(&result, "count");
    assert_eq!(captures.len(), 2);
    assert!(
        captures
            .iter()
            .any(|capture| capture.access == CaptureAccess::Read)
    );
    assert!(
        captures
            .iter()
            .any(|capture| capture.access == CaptureAccess::Mutable)
    );
    let binding = captures[0].binding_id;
    assert!(captures.iter().all(|capture| {
        capture.binding_id == binding
            && capture.storage == CaptureStorage::OwnedMutableUpvalue
            && requires_cell(&result, capture)
    }));
    assert_eq!(result.capture_cell_requirements().len(), 1);
}

#[test]
fn local_function_alias_stays_non_escaping() {
    let result = checked(
        r"
        fn main() {
            var x = 1;
            let f = || { x = 2; };
            f();
        }
        ",
    );

    let capture = capture(&result, "x");
    assert_eq!(capture.storage, CaptureStorage::OwnedMutableScoped);
    assert!(requires_cell(&result, capture));
    assert!(
        result
            .lambda_escapes()
            .values()
            .all(|fact| fact.escape == LambdaEscapeKind::NonEscaping)
    );
}

#[test]
fn escaping_read_of_mutable_binding_records_cell_requirement() {
    let result = checked(
        r"
        fn make() -> fn() {
            var x = 1;
            || { let y = x; }
        }
        ",
    );

    let capture = capture(&result, "x");
    assert_eq!(capture.access, CaptureAccess::Read);
    assert_eq!(capture.storage, CaptureStorage::OwnedMutableUpvalue);
    assert!(requires_cell(&result, capture));
}

#[test]
fn projected_var_argument_records_mutable_capture_access() {
    let result = checked(
        r"
        struct Point { x: int }

        fn bump(var x: int) {}

        fn main() {
            var p = Point { x: 0 };
            let f = || { bump(p.x); };
            f();
        }
        ",
    );

    assert_eq!(capture(&result, "p").access, CaptureAccess::Mutable);
}

#[test]
fn indexed_mut_receiver_records_mutable_capture_access() {
    let result = checked(
        r"
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
        ",
    );

    assert_eq!(capture(&result, "players").access, CaptureAccess::Mutable);
}

#[test]
fn branch_alias_to_distinct_lambdas_marks_both_escaping() {
    let result = checked(
        r"
        fn make(cond: bool) -> fn() {
            var x = 1;
            var y = 2;
            var f = || { let a = x; };
            if cond {
                f = || { let b = y; };
            }
            f
        }
        ",
    );

    let required = result
        .lambda_captures()
        .values()
        .filter(|capture| matches!(capture.name.as_str(), "x" | "y"))
        .filter(|capture| requires_cell(&result, capture))
        .count();
    assert_eq!(required, 2);
}

#[test]
fn escaping_capture_of_local_closure_marks_closure_escaping() {
    let result = checked(
        r"
        fn make() -> fn() {
            var x = 1;
            let inner = || { x = 2; };
            || { inner(); }
        }
        ",
    );

    let capture = capture(&result, "x");
    assert_eq!(capture.storage, CaptureStorage::OwnedMutableUpvalue);
    assert!(requires_cell(&result, capture));
}

#[test]
fn nested_readonly_function_value_capture_records_ordinary_capture() {
    let result = checked(
        r"
        fn main() {
            let seed = 3;
            let f = || seed;
            let g = || f();
            g();
        }
        ",
    );

    let function_capture = function_capture(&result, "f", CaptureStorage::OwnedReadonly);
    assert_eq!(function_capture.origin, CaptureStorageOrigin::Owned);
    assert!(!requires_cell(&result, function_capture));
    assert_eq!(
        capture(&result, "seed").storage,
        CaptureStorage::OwnedReadonly
    );
    assert_eq!(escaping_lambdas(&result), 0);
}

#[test]
fn escaping_function_value_capture_marks_captured_lambda_escaping() {
    let result = checked(
        r"
        fn make(seed: int) -> fn() -> int {
            let f = || seed;
            || f()
        }
        ",
    );

    function_capture(&result, "f", CaptureStorage::OwnedReadonly);
    assert_eq!(
        capture(&result, "seed").storage,
        CaptureStorage::OwnedReadonly
    );
    assert_eq!(escaping_lambdas(&result), 2);
}

#[test]
fn assignment_after_function_value_capture_updates_escaping_flow() {
    let result = checked(
        r"
        fn make(cond: bool) -> fn() -> int {
            let seed = 1;
            var f = || seed;
            let g = || f();
            if cond {
                f = || 2;
            }
            g
        }
        ",
    );

    let function_capture = function_capture(&result, "f", CaptureStorage::OwnedMutableUpvalue);
    assert!(requires_cell(&result, function_capture));
    assert_eq!(
        capture(&result, "seed").storage,
        CaptureStorage::OwnedReadonly
    );
    assert_eq!(escaping_lambdas(&result), 3);
}

#[test]
fn function_call_return_capture_is_escaping_safe() {
    let result = checked(
        r"
        fn make(seed: int) -> fn() -> int {
            || seed
        }

        fn outer(seed: int) -> fn() -> int {
            let f = make(seed);
            || f()
        }
        ",
    );

    let function_capture = function_capture(&result, "f", CaptureStorage::OwnedReadonly);
    assert!(!requires_cell(&result, function_capture));
    assert_eq!(escaping_lambdas(&result), 2);
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

    let captures = captures(&result, "x");
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
        r"
        fn touch(var x: int) {
            let f = || { x = 1; };
            f();
        }
        ",
    );

    assert_borrowed_scoped_capture(&result, "x");
}

#[test]
fn slice_param_captures_are_borrowed_scoped() {
    let result = checked(
        r"
        fn use_slice_now(xs: slice[int]) -> int {
            let f = || xs[0];
            f()
        }

        fn use_tuple_now(pair: (slice[int], int)) -> int {
            let f = || pair.0[0];
            f()
        }
        ",
    );

    assert_borrowed_scoped_capture(&result, "xs");
    assert_borrowed_scoped_capture(&result, "pair");
}

#[test]
fn slice_signature_callback_capture_stays_owned() {
    let result = checked(
        r"
        fn use_now(cb: fn(slice[int]) -> int) {
            let f = || { let g = cb; };
            f();
        }
        ",
    );

    let capture = capture(&result, "cb");
    assert_eq!(capture.origin, CaptureStorageOrigin::Owned);
    assert_eq!(capture.storage, CaptureStorage::OwnedReadonly);
}
