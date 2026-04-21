use super::helpers::{
    dummy_ident, enum_decl, ident_expr, let_binding, lit_float, lit_int, lit_string,
    map_literal_expr, program, reset_expr_ids, run_err, run_ok, struct_decl, struct_literal_expr,
};
use crate::{
    ast::{Type, VariantKind},
    typecheck::error::DiagnosticKind,
};

// ---- sanity: int key still works ----

#[test]
fn test_map_int_key_still_ok() {
    reset_expr_ids();

    let map = map_literal_expr(vec![(lit_int(1), lit_string("a"))]);
    let binding = let_binding("m", None, map);
    let prog = program(vec![binding]);
    let _ = run_ok(prog);
}

// ---- float key in map literal ----

#[test]
fn test_map_float_key_err() {
    reset_expr_ids();

    let map = map_literal_expr(vec![(lit_float(1.0), lit_string("a"))]);
    let binding = let_binding("m", None, map);
    let prog = program(vec![binding]);
    let errors = run_err(prog);
    assert!(!errors.is_empty());
    assert!(
        errors
            .iter()
            .any(|e| matches!(&e.kind, DiagnosticKind::MapKeyFloat)),
        "Expected MapKeyFloat, got: {errors:?}"
    );
}

// ---- float key via type annotation (stmt.rs path) ----

#[test]
fn test_map_annotation_float_key_err() {
    reset_expr_ids();

    let map_ty = Type::Map {
        key: Type::Float.boxed(),
        value: Type::String.boxed(),
    };
    let map = map_literal_expr(vec![]);
    let binding = let_binding("m", Some(map_ty), map);
    let prog = program(vec![binding]);
    let errors = run_err(prog);
    assert!(!errors.is_empty());
    assert!(
        errors
            .iter()
            .any(|e| matches!(&e.kind, DiagnosticKind::MapKeyFloat)),
        "Expected MapKeyFloat, got: {errors:?}"
    );
}

// ---- struct with int fields as key ----

#[test]
fn test_map_struct_int_fields_key_ok() {
    reset_expr_ids();

    // struct Point { x: int, y: int }
    // let p = Point { x: 1, y: 2 };
    // let m = [p: "origin"];
    let point_decl = struct_decl("Point", vec![("x", Type::Int), ("y", Type::Int)], vec![]);
    let p_binding = let_binding(
        "p",
        None,
        struct_literal_expr("Point", vec![("x", lit_int(1)), ("y", lit_int(2))]),
    );
    let map = map_literal_expr(vec![(ident_expr("p"), lit_string("origin"))]);
    let m_binding = let_binding("m", None, map);
    let prog = program(vec![point_decl, p_binding, m_binding]);
    let _ = run_ok(prog);
}

// ---- struct with float field as key -> error ----

#[test]
fn test_map_struct_float_field_key_err() {
    reset_expr_ids();

    // struct Vec2 { x: float, y: float }
    // let v = Vec2 { x: 1.0, y: 2.0 };
    // let m = [v: 0];   // error: Vec2 not keyable (float field)
    let vec2_decl = struct_decl("Vec2", vec![("x", Type::Float), ("y", Type::Float)], vec![]);
    let v_binding = let_binding(
        "v",
        None,
        struct_literal_expr("Vec2", vec![("x", lit_float(1.0)), ("y", lit_float(2.0))]),
    );
    let map = map_literal_expr(vec![(ident_expr("v"), lit_int(0))]);
    let m_binding = let_binding("m", None, map);
    let prog = program(vec![vec2_decl, v_binding, m_binding]);
    let errors = run_err(prog);
    assert!(!errors.is_empty());
    assert!(
        errors
            .iter()
            .any(|e| matches!(&e.kind, DiagnosticKind::MapKeyNotKeyable { .. })),
        "Expected MapKeyNotKeyable, got: {errors:?}"
    );
}

// ---- enum with keyable payloads as key ----

#[test]
fn test_map_enum_keyable_payload_ok() {
    reset_expr_ids();

    // enum Cell { Empty, Coord(int, int) }
    // let m: [Cell: int] = [:];
    let cell_decl = enum_decl(
        "Cell",
        vec![
            ("Empty", VariantKind::Unit),
            ("Coord", VariantKind::Tuple(vec![Type::Int, Type::Int])),
        ],
    );
    let map_ty = Type::Map {
        key: Type::Enum {
            name: dummy_ident("Cell"),
            type_args: vec![],
            origin: None,
        }
        .boxed(),
        value: Type::Int.boxed(),
    };
    let map = map_literal_expr(vec![]);
    let m_binding = let_binding("m", Some(map_ty), map);
    let prog = program(vec![cell_decl, m_binding]);
    let _ = run_ok(prog);
}

// ---- enum with non-keyable payload as key ----

#[test]
fn test_map_enum_non_keyable_payload_err() {
    reset_expr_ids();

    // enum Wrapper { Hold([int]) }
    // [Wrapper.Hold(...): 0]  ->  error: non-keyable payload
    let wrapper_decl = enum_decl(
        "Wrapper",
        vec![(
            "Hold",
            VariantKind::Tuple(vec![Type::List {
                elem: Type::Int.boxed(),
            }]),
        )],
    );
    let map_ty = Type::Map {
        key: Type::Enum {
            name: dummy_ident("Wrapper"),
            type_args: vec![],
            origin: None,
        }
        .boxed(),
        value: Type::Int.boxed(),
    };
    let map = map_literal_expr(vec![]);
    let binding = let_binding("m", Some(map_ty), map);
    let prog = program(vec![wrapper_decl, binding]);
    let errors = run_err(prog);
    assert!(!errors.is_empty());
    assert!(
        errors
            .iter()
            .any(|e| matches!(&e.kind, DiagnosticKind::MapKeyNotKeyable { .. })),
        "Expected MapKeyNotKeyable, got: {errors:?}"
    );
}

// ---- reason notes ----

#[test]
fn test_map_struct_float_field_reason_note() {
    reset_expr_ids();
    // struct Pos { x: float, y: float }; let p = Pos{...}; [p: 0] -> MapKeyNotKeyable
    // note should mention field 'x' and 'not keyable'
    let pos_decl = struct_decl("Pos", vec![("x", Type::Float), ("y", Type::Float)], vec![]);
    let p_binding = let_binding(
        "p",
        None,
        struct_literal_expr("Pos", vec![("x", lit_float(1.0)), ("y", lit_float(2.0))]),
    );
    let map = map_literal_expr(vec![(ident_expr("p"), lit_int(0))]);
    let m_binding = let_binding("m", None, map);
    let prog = program(vec![pos_decl, p_binding, m_binding]);
    let errors = run_err(prog);
    let not_keyable_err = errors
        .iter()
        .find(|e| matches!(&e.kind, DiagnosticKind::MapKeyNotKeyable { .. }))
        .expect("Expected MapKeyNotKeyable error");
    assert!(
        not_keyable_err
            .notes
            .iter()
            .any(|n| n.contains("field 'x'") && n.contains("not keyable")),
        "Expected note mentioning field 'x' and 'not keyable', got: {:?}",
        not_keyable_err.notes
    );
}

#[test]
fn test_map_enum_list_payload_reason_note() {
    reset_expr_ids();
    // enum Wrapper { Hold([int]) }; let m: [Wrapper: int] = [:] -> MapKeyNotKeyable
    // note should mention variant 'Hold' and 'not keyable'
    let wrapper_decl = enum_decl(
        "Wrapper",
        vec![(
            "Hold",
            VariantKind::Tuple(vec![Type::List {
                elem: Type::Int.boxed(),
            }]),
        )],
    );
    let map_ty = Type::Map {
        key: Type::Enum {
            name: dummy_ident("Wrapper"),
            type_args: vec![],
            origin: None,
        }
        .boxed(),
        value: Type::Int.boxed(),
    };
    let binding = let_binding("m", Some(map_ty), map_literal_expr(vec![]));
    let prog = program(vec![wrapper_decl, binding]);
    let errors = run_err(prog);
    let not_keyable_err = errors
        .iter()
        .find(|e| matches!(&e.kind, DiagnosticKind::MapKeyNotKeyable { .. }))
        .expect("Expected MapKeyNotKeyable error");
    assert!(
        not_keyable_err
            .notes
            .iter()
            .any(|n| n.contains("variant 'Hold'") && n.contains("not keyable")),
        "Expected note mentioning variant 'Hold' and 'not keyable', got: {:?}",
        not_keyable_err.notes
    );
}
