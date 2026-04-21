use super::helpers::{
    array_literal, assert_expr_type, binary_expr, call_expr, enum_decl, field_expr, fn_decl,
    get_expr_id, ident_expr, let_binding, lit_bool, lit_float, lit_int, lit_nil, lit_string,
    map_literal_expr, opt_type, program, reset_expr_ids, return_stmt, run_err, run_ok, struct_decl,
    struct_literal_expr,
};
use crate::{
    ast::{ArrayLen, BinaryOp, Type, VariantKind},
    typecheck::error::DiagnosticKind,
};

// ---- equatable primitives ----

#[test]
fn test_eq_int_ok() {
    reset_expr_ids();
    let expr = binary_expr(lit_int(1), BinaryOp::Eq, lit_int(2));
    let expr_id = get_expr_id(&expr);
    let prog = program(vec![super::helpers::expr_stmt(expr)]);
    let tcx = run_ok(prog);
    assert_expr_type(&tcx, expr_id, &Type::Bool);
}

#[test]
fn test_eq_float_ok() {
    reset_expr_ids();
    let expr = binary_expr(lit_float(1.0), BinaryOp::Eq, lit_float(2.0));
    let expr_id = get_expr_id(&expr);
    let prog = program(vec![super::helpers::expr_stmt(expr)]);
    let tcx = run_ok(prog);
    assert_expr_type(&tcx, expr_id, &Type::Bool);
}

#[test]
fn test_neq_bool_ok() {
    reset_expr_ids();
    let expr = binary_expr(lit_bool(true), BinaryOp::NotEq, lit_bool(false));
    let expr_id = get_expr_id(&expr);
    let prog = program(vec![super::helpers::expr_stmt(expr)]);
    let tcx = run_ok(prog);
    assert_expr_type(&tcx, expr_id, &Type::Bool);
}

#[test]
fn test_eq_string_ok() {
    reset_expr_ids();
    let expr = binary_expr(lit_string("a"), BinaryOp::Eq, lit_string("b"));
    let expr_id = get_expr_id(&expr);
    let prog = program(vec![super::helpers::expr_stmt(expr)]);
    let tcx = run_ok(prog);
    assert_expr_type(&tcx, expr_id, &Type::Bool);
}

// ---- equatable structs ----

#[test]
fn test_eq_struct_with_int_fields_ok() {
    reset_expr_ids();

    // struct Point { x: int, y: int }
    // let a = Point { x: 1, y: 2 };
    // let b = Point { x: 1, y: 2 };
    // a == b  ->  bool
    let point_decl = struct_decl("Point", vec![("x", Type::Int), ("y", Type::Int)], vec![]);
    let a_binding = let_binding(
        "a",
        None,
        struct_literal_expr("Point", vec![("x", lit_int(1)), ("y", lit_int(2))]),
    );
    let b_binding = let_binding(
        "b",
        None,
        struct_literal_expr("Point", vec![("x", lit_int(1)), ("y", lit_int(2))]),
    );
    let eq = binary_expr(ident_expr("a"), BinaryOp::Eq, ident_expr("b"));
    let eq_id = get_expr_id(&eq);
    let prog = program(vec![
        point_decl,
        a_binding,
        b_binding,
        super::helpers::expr_stmt(eq),
    ]);
    let tcx = run_ok(prog);
    assert_expr_type(&tcx, eq_id, &Type::Bool);
}

#[test]
fn test_eq_struct_with_float_fields_ok() {
    reset_expr_ids();

    // struct Vec2 { x: float, y: float }
    // float is equatable, so Vec2 is equatable
    let vec2_decl = struct_decl("Vec2", vec![("x", Type::Float), ("y", Type::Float)], vec![]);
    let a_binding = let_binding(
        "a",
        None,
        struct_literal_expr("Vec2", vec![("x", lit_float(1.0)), ("y", lit_float(2.0))]),
    );
    let b_binding = let_binding(
        "b",
        None,
        struct_literal_expr("Vec2", vec![("x", lit_float(3.0)), ("y", lit_float(4.0))]),
    );
    let eq = binary_expr(ident_expr("a"), BinaryOp::Eq, ident_expr("b"));
    let eq_id = get_expr_id(&eq);
    let prog = program(vec![
        vec2_decl,
        a_binding,
        b_binding,
        super::helpers::expr_stmt(eq),
    ]);
    let tcx = run_ok(prog);
    assert_expr_type(&tcx, eq_id, &Type::Bool);
}

// ---- equatable: function types (identity equality) ----

#[test]
fn test_eq_fn_type_errors() {
    reset_expr_ids();

    // fn foo() -> int { 0 }
    // fn bar() -> int { 1 }
    // let a = foo;  let b = bar;
    // a == b  ->  bool (identity equality)
    let foo_decl = fn_decl(
        "foo",
        vec![],
        Type::Int,
        vec![return_stmt(Some(lit_int(0)))],
    );
    let bar_decl = fn_decl(
        "bar",
        vec![],
        Type::Int,
        vec![return_stmt(Some(lit_int(1)))],
    );
    let fn_ty = Type::Func {
        params: vec![],
        ret: Type::Int.boxed(),
    };
    let a_binding = let_binding("a", Some(fn_ty.clone()), ident_expr("foo"));
    let b_binding = let_binding("b", Some(fn_ty), ident_expr("bar"));
    let eq = binary_expr(ident_expr("a"), BinaryOp::Eq, ident_expr("b"));
    let eq_id = get_expr_id(&eq);
    let prog = program(vec![
        foo_decl,
        bar_decl,
        a_binding,
        b_binding,
        super::helpers::expr_stmt(eq),
    ]);
    let tcx = run_ok(prog);
    assert_expr_type(&tcx, eq_id, &Type::Bool);
}

// ---- equatable: lists ----

#[test]
fn test_eq_list_ok() {
    reset_expr_ids();

    // let a: [int] = [1, 2];
    // let b: [int] = [3, 4];
    // a == b  ->  bool
    let list_int = Type::List {
        elem: Type::Int.boxed(),
    };
    let a_binding = let_binding(
        "a",
        Some(list_int.clone()),
        array_literal(vec![lit_int(1), lit_int(2)]),
    );
    let b_binding = let_binding(
        "b",
        Some(list_int),
        array_literal(vec![lit_int(3), lit_int(4)]),
    );
    let eq = binary_expr(ident_expr("a"), BinaryOp::Eq, ident_expr("b"));
    let eq_id = get_expr_id(&eq);
    let prog = program(vec![a_binding, b_binding, super::helpers::expr_stmt(eq)]);
    let tcx = run_ok(prog);
    assert_expr_type(&tcx, eq_id, &Type::Bool);
}

// ---- equatable: maps ----

#[test]
fn test_eq_map_ok() {
    reset_expr_ids();

    // let a: [int: string] = [1: "a"];
    // let b: [int: string] = [2: "b"];
    // a == b  ->  bool
    let map_ty = Type::Map {
        key: Type::Int.boxed(),
        value: Type::String.boxed(),
    };
    let a_binding = let_binding(
        "a",
        Some(map_ty.clone()),
        map_literal_expr(vec![(lit_int(1), lit_string("a"))]),
    );
    let b_binding = let_binding(
        "b",
        Some(map_ty),
        map_literal_expr(vec![(lit_int(2), lit_string("b"))]),
    );
    let eq = binary_expr(ident_expr("a"), BinaryOp::Eq, ident_expr("b"));
    let eq_id = get_expr_id(&eq);
    let prog = program(vec![a_binding, b_binding, super::helpers::expr_stmt(eq)]);
    let tcx = run_ok(prog);
    assert_expr_type(&tcx, eq_id, &Type::Bool);
}

// ---- equatable: struct with fn field (identity equality on fn fields) ----

#[test]
fn test_eq_struct_with_fn_field_errors() {
    reset_expr_ids();

    // struct Handler { callback: fn() -> int }
    // Handler is equatable because fn fields use identity equality
    let fn_ty = Type::Func {
        params: vec![],
        ret: Type::Int.boxed(),
    };
    let handler_decl = struct_decl("Handler", vec![("callback", fn_ty.clone())], vec![]);

    let foo_decl = fn_decl(
        "foo",
        vec![],
        Type::Int,
        vec![return_stmt(Some(lit_int(0)))],
    );
    let a_binding = let_binding(
        "a",
        None,
        struct_literal_expr("Handler", vec![("callback", ident_expr("foo"))]),
    );
    let b_binding = let_binding(
        "b",
        None,
        struct_literal_expr("Handler", vec![("callback", ident_expr("foo"))]),
    );
    let eq = binary_expr(ident_expr("a"), BinaryOp::Eq, ident_expr("b"));
    let eq_id = get_expr_id(&eq);
    let prog = program(vec![
        handler_decl,
        foo_decl,
        a_binding,
        b_binding,
        super::helpers::expr_stmt(eq),
    ]);
    let tcx = run_ok(prog);
    assert_expr_type(&tcx, eq_id, &Type::Bool);
}

// ---- enum equality ----

#[test]
fn test_eq_enum_unit_variants_ok() {
    reset_expr_ids();
    // enum Dir { N, S, E, W }
    // let a = Dir.N; let b = Dir.S; a == b  ->  ok
    let dir_decl = enum_decl(
        "Dir",
        vec![
            ("N", VariantKind::Unit),
            ("S", VariantKind::Unit),
            ("E", VariantKind::Unit),
            ("W", VariantKind::Unit),
        ],
    );
    let a_val = field_expr(ident_expr("Dir"), "N");
    let b_val = field_expr(ident_expr("Dir"), "S");
    let a_binding = let_binding("a", None, a_val);
    let b_binding = let_binding("b", None, b_val);
    let eq = binary_expr(ident_expr("a"), BinaryOp::Eq, ident_expr("b"));
    let prog = program(vec![
        dir_decl,
        a_binding,
        b_binding,
        super::helpers::expr_stmt(eq),
    ]);
    let _ = run_ok(prog);
}

#[test]
fn test_eq_enum_int_payload_ok() {
    reset_expr_ids();
    // enum Bar { X(int), Y(string) }
    // let a = Bar.X(1); let b = Bar.X(2); a == b  ->  ok
    let bar_decl = enum_decl(
        "Bar",
        vec![
            ("X", VariantKind::Tuple(vec![Type::Int])),
            ("Y", VariantKind::Tuple(vec![Type::String])),
        ],
    );
    let a_val = call_expr(field_expr(ident_expr("Bar"), "X"), vec![lit_int(1)]);
    let b_val = call_expr(field_expr(ident_expr("Bar"), "X"), vec![lit_int(2)]);
    let a_binding = let_binding("a", None, a_val);
    let b_binding = let_binding("b", None, b_val);
    let eq = binary_expr(ident_expr("a"), BinaryOp::Eq, ident_expr("b"));
    let prog = program(vec![
        bar_decl,
        a_binding,
        b_binding,
        super::helpers::expr_stmt(eq),
    ]);
    let _ = run_ok(prog);
}

#[test]
fn test_eq_enum_fn_payload_err() {
    reset_expr_ids();
    // fn my_fn() -> void {}
    // enum Foo { A(fn() -> void) }
    // let a = Foo.A(my_fn); let b = Foo.A(my_fn); a == b  ->  bool (identity equality)
    let my_fn_decl = fn_decl("my_fn", vec![], Type::Void, vec![]);
    let foo_decl = enum_decl(
        "Foo",
        vec![(
            "A",
            VariantKind::Tuple(vec![Type::Func {
                params: vec![],
                ret: Type::Void.boxed(),
            }]),
        )],
    );
    let a_val = call_expr(
        field_expr(ident_expr("Foo"), "A"),
        vec![ident_expr("my_fn")],
    );
    let b_val = call_expr(
        field_expr(ident_expr("Foo"), "A"),
        vec![ident_expr("my_fn")],
    );
    let a_binding = let_binding("a", None, a_val);
    let b_binding = let_binding("b", None, b_val);
    let eq = binary_expr(ident_expr("a"), BinaryOp::Eq, ident_expr("b"));
    let eq_id = get_expr_id(&eq);
    let prog = program(vec![
        my_fn_decl,
        foo_decl,
        a_binding,
        b_binding,
        super::helpers::expr_stmt(eq),
    ]);
    let tcx = run_ok(prog);
    assert_expr_type(&tcx, eq_id, &Type::Bool);
}

#[test]
fn test_eq_enum_list_payload_ok() {
    reset_expr_ids();
    // enum Baz { M([int]) }
    // let a = Baz.M(xs); let b = Baz.M(ys); a == b  ->  bool
    let list_int = Type::List {
        elem: Type::Int.boxed(),
    };
    let baz_decl = enum_decl(
        "Baz",
        vec![("M", VariantKind::Tuple(vec![list_int.clone()]))],
    );
    let xs_binding = let_binding(
        "xs",
        Some(list_int.clone()),
        array_literal(vec![lit_int(1)]),
    );
    let ys_binding = let_binding("ys", Some(list_int), array_literal(vec![lit_int(2)]));
    let a_val = call_expr(field_expr(ident_expr("Baz"), "M"), vec![ident_expr("xs")]);
    let b_val = call_expr(field_expr(ident_expr("Baz"), "M"), vec![ident_expr("ys")]);
    let a_binding = let_binding("a", None, a_val);
    let b_binding = let_binding("b", None, b_val);
    let eq = binary_expr(ident_expr("a"), BinaryOp::Eq, ident_expr("b"));
    let eq_id = get_expr_id(&eq);
    let prog = program(vec![
        baz_decl,
        xs_binding,
        ys_binding,
        a_binding,
        b_binding,
        super::helpers::expr_stmt(eq),
    ]);
    let tcx = run_ok(prog);
    assert_expr_type(&tcx, eq_id, &Type::Bool);
}

#[test]
fn test_eq_struct_fn_field_reason_note() {
    reset_expr_ids();
    // struct with fn field is equatable; no error expected
    let my_fn_decl = fn_decl("my_fn", vec![], Type::Void, vec![]);
    let handler_decl = struct_decl(
        "Handler",
        vec![(
            "callback",
            Type::Func {
                params: vec![],
                ret: Type::Void.boxed(),
            },
        )],
        vec![],
    );
    let a_binding = let_binding(
        "a",
        None,
        struct_literal_expr("Handler", vec![("callback", ident_expr("my_fn"))]),
    );
    let b_binding = let_binding(
        "b",
        None,
        struct_literal_expr("Handler", vec![("callback", ident_expr("my_fn"))]),
    );
    let eq = binary_expr(ident_expr("a"), BinaryOp::Eq, ident_expr("b"));
    let eq_id = get_expr_id(&eq);
    let prog = program(vec![
        my_fn_decl,
        handler_decl,
        a_binding,
        b_binding,
        super::helpers::expr_stmt(eq),
    ]);
    let tcx = run_ok(prog);
    assert_expr_type(&tcx, eq_id, &Type::Bool);
}

#[test]
fn test_eq_enum_fn_payload_reason_note() {
    reset_expr_ids();
    // enum with fn payload is equatable; no error expected
    let my_fn_decl = fn_decl("my_fn", vec![], Type::Void, vec![]);
    let foo_decl = enum_decl(
        "Foo",
        vec![(
            "A",
            VariantKind::Tuple(vec![Type::Func {
                params: vec![],
                ret: Type::Void.boxed(),
            }]),
        )],
    );
    let a_val = call_expr(
        field_expr(ident_expr("Foo"), "A"),
        vec![ident_expr("my_fn")],
    );
    let b_val = call_expr(
        field_expr(ident_expr("Foo"), "A"),
        vec![ident_expr("my_fn")],
    );
    let a_binding = let_binding("a", None, a_val);
    let b_binding = let_binding("b", None, b_val);
    let eq = binary_expr(ident_expr("a"), BinaryOp::Eq, ident_expr("b"));
    let eq_id = get_expr_id(&eq);
    let prog = program(vec![
        my_fn_decl,
        foo_decl,
        a_binding,
        b_binding,
        super::helpers::expr_stmt(eq),
    ]);
    let tcx = run_ok(prog);
    assert_expr_type(&tcx, eq_id, &Type::Bool);
}

// ---- equatable containers: fixed arrays ----

#[test]
fn test_eq_array_fixed_ok() {
    reset_expr_ids();

    // let a: [int; 3] = [1, 2, 3];
    // let b: [int; 3] = [4, 5, 6];
    // a == b  ->  bool
    let arr_ty = Type::Array {
        elem: Type::Int.boxed(),
        len: ArrayLen::Fixed(3),
    };
    let a_binding = let_binding(
        "a",
        Some(arr_ty.clone()),
        array_literal(vec![lit_int(1), lit_int(2), lit_int(3)]),
    );
    let b_binding = let_binding(
        "b",
        Some(arr_ty),
        array_literal(vec![lit_int(4), lit_int(5), lit_int(6)]),
    );
    let eq = binary_expr(ident_expr("a"), BinaryOp::Eq, ident_expr("b"));
    let eq_id = get_expr_id(&eq);
    let prog = program(vec![a_binding, b_binding, super::helpers::expr_stmt(eq)]);
    let tcx = run_ok(prog);
    assert_expr_type(&tcx, eq_id, &Type::Bool);
}

// ---- equatable containers: list of structs (gamedev: comparing position lists) ----

#[test]
fn test_eq_list_of_structs_ok() {
    reset_expr_ids();

    // struct Point { x: int, y: int }
    // let a: [Point] = [...];
    // let b: [Point] = [...];
    // a == b  ->  bool
    let point_decl = struct_decl("Point", vec![("x", Type::Int), ("y", Type::Int)], vec![]);
    let point_ty = Type::Struct {
        name: super::helpers::dummy_ident("Point"),
        type_args: vec![],
        origin: None,
    };
    let list_ty = Type::List {
        elem: point_ty.clone().boxed(),
    };
    let a_binding = let_binding(
        "a",
        Some(list_ty.clone()),
        array_literal(vec![struct_literal_expr(
            "Point",
            vec![("x", lit_int(0)), ("y", lit_int(0))],
        )]),
    );
    let b_binding = let_binding(
        "b",
        Some(list_ty),
        array_literal(vec![struct_literal_expr(
            "Point",
            vec![("x", lit_int(1)), ("y", lit_int(1))],
        )]),
    );
    let eq = binary_expr(ident_expr("a"), BinaryOp::Eq, ident_expr("b"));
    let eq_id = get_expr_id(&eq);
    let prog = program(vec![
        point_decl,
        a_binding,
        b_binding,
        super::helpers::expr_stmt(eq),
    ]);
    let tcx = run_ok(prog);
    assert_expr_type(&tcx, eq_id, &Type::Bool);
}

// ---- equatable containers: map with string key (gamedev: inventory-style maps) ----

#[test]
fn test_eq_map_string_int_ok() {
    reset_expr_ids();

    // let a: [string: int] = ["hp": 100];
    // let b: [string: int] = ["hp": 50];
    // a == b  ->  bool
    let map_ty = Type::Map {
        key: Type::String.boxed(),
        value: Type::Int.boxed(),
    };
    let a_binding = let_binding(
        "a",
        Some(map_ty.clone()),
        map_literal_expr(vec![(lit_string("hp"), lit_int(100))]),
    );
    let b_binding = let_binding(
        "b",
        Some(map_ty),
        map_literal_expr(vec![(lit_string("hp"), lit_int(50))]),
    );
    let eq = binary_expr(ident_expr("a"), BinaryOp::Eq, ident_expr("b"));
    let eq_id = get_expr_id(&eq);
    let prog = program(vec![a_binding, b_binding, super::helpers::expr_stmt(eq)]);
    let tcx = run_ok(prog);
    assert_expr_type(&tcx, eq_id, &Type::Bool);
}

// ---- equatable containers: enum with list payload now equatable ----

#[test]
fn test_eq_enum_with_list_payload_ok() {
    reset_expr_ids();

    // enum Wrapper { Items([int]) }
    // a == b  ->  bool  (because [int] is now equatable)
    // Annotated let bindings are used so the array literal resolves to [int] (List), not [int; N].
    let list_int = Type::List {
        elem: Type::Int.boxed(),
    };
    let wrapper_decl = enum_decl(
        "Wrapper",
        vec![("Items", VariantKind::Tuple(vec![list_int.clone()]))],
    );
    let xs_binding = let_binding(
        "xs",
        Some(list_int.clone()),
        array_literal(vec![lit_int(1)]),
    );
    let ys_binding = let_binding("ys", Some(list_int), array_literal(vec![lit_int(2)]));
    let a_val = call_expr(
        field_expr(ident_expr("Wrapper"), "Items"),
        vec![ident_expr("xs")],
    );
    let b_val = call_expr(
        field_expr(ident_expr("Wrapper"), "Items"),
        vec![ident_expr("ys")],
    );
    let a_binding = let_binding("a", None, a_val);
    let b_binding = let_binding("b", None, b_val);
    let eq = binary_expr(ident_expr("a"), BinaryOp::Eq, ident_expr("b"));
    let eq_id = get_expr_id(&eq);
    let prog = program(vec![
        wrapper_decl,
        xs_binding,
        ys_binding,
        a_binding,
        b_binding,
        super::helpers::expr_stmt(eq),
    ]);
    let tcx = run_ok(prog);
    assert_expr_type(&tcx, eq_id, &Type::Bool);
}

// ---- non-equatable containers: list of fn ----

#[test]
fn test_eq_list_of_fn_errors() {
    reset_expr_ids();

    // let a: [fn() -> void] = [foo];
    // let b: [fn() -> void] = [foo];
    // a == b  ->  bool (fn is equatable via identity)
    let fn_ty = Type::Func {
        params: vec![],
        ret: Type::Void.boxed(),
    };
    let foo_decl = fn_decl("foo", vec![], Type::Void, vec![]);
    let list_ty = Type::List {
        elem: fn_ty.boxed(),
    };
    let a_binding = let_binding(
        "a",
        Some(list_ty.clone()),
        array_literal(vec![ident_expr("foo")]),
    );
    let b_binding = let_binding("b", Some(list_ty), array_literal(vec![ident_expr("foo")]));
    let eq = binary_expr(ident_expr("a"), BinaryOp::Eq, ident_expr("b"));
    let eq_id = get_expr_id(&eq);
    let prog = program(vec![
        foo_decl,
        a_binding,
        b_binding,
        super::helpers::expr_stmt(eq),
    ]);
    let tcx = run_ok(prog);
    assert_expr_type(&tcx, eq_id, &Type::Bool);
}

// ---- equatable containers: map with fn value (identity equality on fn values) ----

#[test]
fn test_eq_map_fn_value_errors() {
    reset_expr_ids();

    // let a: [int: fn() -> void] = [...];
    // let b: [int: fn() -> void] = [...];
    // a == b  ->  bool (fn is equatable via identity)
    let fn_ty = Type::Func {
        params: vec![],
        ret: Type::Void.boxed(),
    };
    let foo_decl = fn_decl("foo", vec![], Type::Void, vec![]);
    let map_ty = Type::Map {
        key: Type::Int.boxed(),
        value: fn_ty.boxed(),
    };
    let a_binding = let_binding(
        "a",
        Some(map_ty.clone()),
        map_literal_expr(vec![(lit_int(1), ident_expr("foo"))]),
    );
    let b_binding = let_binding(
        "b",
        Some(map_ty),
        map_literal_expr(vec![(lit_int(2), ident_expr("foo"))]),
    );
    let eq = binary_expr(ident_expr("a"), BinaryOp::Eq, ident_expr("b"));
    let eq_id = get_expr_id(&eq);
    let prog = program(vec![
        foo_decl,
        a_binding,
        b_binding,
        super::helpers::expr_stmt(eq),
    ]);
    let tcx = run_ok(prog);
    assert_expr_type(&tcx, eq_id, &Type::Bool);
}

// ---- non-equatable containers: slice stays non-equatable ----

#[test]
fn test_eq_slice_errors() {
    reset_expr_ids();

    // fn compare(a: [int; ..], b: [int; ..]) -> bool { a == b }
    // Slice is not equatable
    let slice_ty = Type::Slice {
        elem: Type::Int.boxed(),
    };
    let fn_body = vec![return_stmt(Some(binary_expr(
        ident_expr("a"),
        BinaryOp::Eq,
        ident_expr("b"),
    )))];
    let compare_fn = fn_decl(
        "compare",
        vec![("a", slice_ty.clone()), ("b", slice_ty)],
        Type::Bool,
        fn_body,
    );
    let prog = program(vec![compare_fn]);
    let errors = run_err(prog);
    assert!(!errors.is_empty());
    assert!(
        errors
            .iter()
            .any(|e| matches!(&e.kind, DiagnosticKind::NotEquatable { .. })),
        "Expected NotEquatable for Slice, got: {errors:?}"
    );
}

// ---- diagnostic reason notes for containers ----

#[test]
fn test_eq_list_fn_elem_reason_note() {
    reset_expr_ids();

    // list of fn is equatable; no error expected
    let fn_ty = Type::Func {
        params: vec![],
        ret: Type::Void.boxed(),
    };
    let foo_decl = fn_decl("foo", vec![], Type::Void, vec![]);
    let list_ty = Type::List {
        elem: fn_ty.boxed(),
    };
    let a_binding = let_binding(
        "a",
        Some(list_ty.clone()),
        array_literal(vec![ident_expr("foo")]),
    );
    let b_binding = let_binding("b", Some(list_ty), array_literal(vec![ident_expr("foo")]));
    let eq = binary_expr(ident_expr("a"), BinaryOp::Eq, ident_expr("b"));
    let eq_id = get_expr_id(&eq);
    let prog = program(vec![
        foo_decl,
        a_binding,
        b_binding,
        super::helpers::expr_stmt(eq),
    ]);
    let tcx = run_ok(prog);
    assert_expr_type(&tcx, eq_id, &Type::Bool);
}

#[test]
fn test_eq_map_fn_value_reason_note() {
    reset_expr_ids();

    // map with fn value is equatable; no error expected
    let fn_ty = Type::Func {
        params: vec![],
        ret: Type::Void.boxed(),
    };
    let foo_decl = fn_decl("foo", vec![], Type::Void, vec![]);
    let map_ty = Type::Map {
        key: Type::Int.boxed(),
        value: fn_ty.boxed(),
    };
    let a_binding = let_binding(
        "a",
        Some(map_ty.clone()),
        map_literal_expr(vec![(lit_int(1), ident_expr("foo"))]),
    );
    let b_binding = let_binding(
        "b",
        Some(map_ty),
        map_literal_expr(vec![(lit_int(2), ident_expr("foo"))]),
    );
    let eq = binary_expr(ident_expr("a"), BinaryOp::Eq, ident_expr("b"));
    let eq_id = get_expr_id(&eq);
    let prog = program(vec![
        foo_decl,
        a_binding,
        b_binding,
        super::helpers::expr_stmt(eq),
    ]);
    let tcx = run_ok(prog);
    assert_expr_type(&tcx, eq_id, &Type::Bool);
}

// ---- nil equality unification ----

#[test]
fn test_eq_optional_int_vs_nil_ok() {
    reset_expr_ids();
    // let v: int? = 10; v == nil  ->  bool
    let v_binding = let_binding("v", Some(opt_type(Type::Int)), lit_int(10));
    let nil_expr = lit_nil();
    let nil_id = get_expr_id(&nil_expr);
    let eq = binary_expr(ident_expr("v"), BinaryOp::Eq, nil_expr);
    let eq_id = get_expr_id(&eq);
    let prog = program(vec![v_binding, super::helpers::expr_stmt(eq)]);
    let tcx = run_ok(prog);
    assert_expr_type(&tcx, eq_id, &Type::Bool);
    assert_expr_type(&tcx, nil_id, &opt_type(Type::Int));
}

#[test]
fn test_neq_optional_int_vs_nil_ok() {
    reset_expr_ids();
    // let v: int? = 10; v != nil  ->  bool
    let v_binding = let_binding("v", Some(opt_type(Type::Int)), lit_int(10));
    let eq = binary_expr(ident_expr("v"), BinaryOp::NotEq, lit_nil());
    let eq_id = get_expr_id(&eq);
    let prog = program(vec![v_binding, super::helpers::expr_stmt(eq)]);
    let tcx = run_ok(prog);
    assert_expr_type(&tcx, eq_id, &Type::Bool);
}

#[test]
fn test_eq_nil_vs_optional_string_ok() {
    reset_expr_ids();
    // let v: string? = nil; nil == v  ->  bool (reversed order)
    let v_binding = let_binding("v", Some(opt_type(Type::String)), lit_nil());
    let eq = binary_expr(lit_nil(), BinaryOp::Eq, ident_expr("v"));
    let eq_id = get_expr_id(&eq);
    let prog = program(vec![v_binding, super::helpers::expr_stmt(eq)]);
    let tcx = run_ok(prog);
    assert_expr_type(&tcx, eq_id, &Type::Bool);
}

#[test]
fn test_eq_int_vs_nil_mismatch() {
    reset_expr_ids();
    // 1 == nil  ->  MismatchedTypes (non-optional int cannot unify with <infer>?)
    let eq = binary_expr(lit_int(1), BinaryOp::Eq, lit_nil());
    let prog = program(vec![super::helpers::expr_stmt(eq)]);
    let errors = run_err(prog);
    assert!(
        errors
            .iter()
            .any(|e| matches!(&e.kind, DiagnosticKind::MismatchedTypes { .. })),
        "Expected MismatchedTypes, got: {errors:?}"
    );
}
