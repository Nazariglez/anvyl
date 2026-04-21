use super::helpers::{opt_type, type_var};
use crate::ast::{FuncParam, Type};

// ---- type variable display tests ----

#[test]
fn test_type_var_display() {
    let t = type_var(0);
    assert_eq!(format!("{t}"), "$0");

    let u = type_var(42);
    assert_eq!(format!("{u}"), "$42");
}

#[test]
fn test_optional_type_var_display() {
    let t = type_var(0);
    let opt_t = opt_type(t);
    assert_eq!(format!("{opt_t}"), "$0?");
}

#[test]
fn test_func_type_var_display() {
    let t = type_var(0);
    let u = type_var(1);
    let func_type = Type::Func {
        params: vec![FuncParam::immut(t)],
        ret: u.boxed(),
    };
    assert_eq!(format!("{func_type}"), "fn($0) -> $1");
}

// ---- type variable predicates tests ----

#[test]
fn test_is_type_var() {
    let t = type_var(0);
    assert!(t.is_type_var());
    assert!(!Type::Int.is_type_var());
    assert!(!Type::Infer.is_type_var());
}

#[test]
fn test_type_var_is_not_inferred() {
    // type variables are not considered infer
    let t = type_var(0);
    assert!(!t.is_infer());
    assert!(Type::Infer.is_infer());
}

#[test]
fn test_type_var_is_not_num_bool_etc() {
    let t = type_var(0);
    assert!(!t.is_num());
    assert!(!t.is_bool());
    assert!(!t.is_str());
    assert!(!t.is_void());
    assert!(!t.is_optional());
    assert!(!t.is_func());
}
