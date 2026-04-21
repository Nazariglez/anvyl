use super::helpers::{
    array_literal, assert_expr_type, binary_expr, call_expr, call_expr_with_type_args, dummy_ident,
    dummy_span, field_expr, fn_decl, generic_method, generic_struct_decl, get_expr_id, ident_expr,
    let_binding, lit_int, lit_string, map_literal_expr, program, reset_expr_ids, return_stmt,
    run_err, run_ok, struct_decl, struct_literal_expr, type_param,
};
use crate::{
    ast::{
        BinaryOp, EnumDecl, EnumDeclNode, EnumVariant, MethodReceiver, Stmt, StmtNode, Type,
        TypeParam, TypeVarId, VariantKind, Visibility,
    },
    typecheck::error::DiagnosticKind,
};

// ---- list constrain_assignable tests ----

#[test]
fn test_constrain_list_passthrough_ok() {
    reset_expr_ids();

    // fn get_ints(xs: [int]) -> [int] { return xs; }
    // let a: [int] = [1, 2, 3];
    // let b: [int] = get_ints(a);
    let list_int = Type::List {
        elem: Type::Int.boxed(),
    };
    let get_ints = fn_decl(
        "get_ints",
        vec![("xs", list_int.clone())],
        list_int.clone(),
        vec![return_stmt(Some(ident_expr("xs")))],
    );

    let arr = array_literal(vec![lit_int(1), lit_int(2), lit_int(3)]);
    let a_binding = let_binding("a", Some(list_int.clone()), arr);
    let call = call_expr(ident_expr("get_ints"), vec![ident_expr("a")]);
    let call_id = get_expr_id(&call);
    let b_binding = let_binding("b", Some(list_int.clone()), call);

    let prog = program(vec![get_ints, a_binding, b_binding]);
    let tcx = run_ok(prog);
    assert_expr_type(&tcx, call_id, &list_int);
}

#[test]
fn test_constrain_list_annotation_then_assign_ok() {
    reset_expr_ids();

    // let a: [int] = [1, 2, 3];
    // let b: [int] = a;
    let list_int = Type::List {
        elem: Type::Int.boxed(),
    };

    let arr = array_literal(vec![lit_int(1), lit_int(2), lit_int(3)]);
    let a_binding = let_binding("a", Some(list_int.clone()), arr);

    let b_val = ident_expr("a");
    let b_id = get_expr_id(&b_val);
    let b_binding = let_binding("b", Some(list_int.clone()), b_val);

    let prog = program(vec![a_binding, b_binding]);
    let tcx = run_ok(prog);
    assert_expr_type(&tcx, b_id, &list_int);
}

#[test]
fn test_constrain_list_type_mismatch_errors() {
    reset_expr_ids();

    // fn make_strings() -> [string] { ... }
    // let x: [int] = make_strings(); // ERROR
    let make_strings = fn_decl(
        "make_strings",
        vec![],
        Type::List {
            elem: Type::String.boxed(),
        },
        vec![return_stmt(Some(ident_expr("make_strings")))],
    );

    let call = call_expr(ident_expr("make_strings"), vec![]);
    let binding = let_binding(
        "x",
        Some(Type::List {
            elem: Type::Int.boxed(),
        }),
        call,
    );

    let prog = program(vec![make_strings, binding]);
    let errors = run_err(prog);
    assert!(!errors.is_empty());
    assert!(
        errors
            .iter()
            .any(|e| matches!(&e.kind, DiagnosticKind::MismatchedTypes { .. })),
        "Expected MismatchedTypes, got: {errors:?}"
    );
}

// ---- map constrain_assignable tests ----

#[test]
fn test_constrain_map_infer_flows_to_annotated() {
    reset_expr_ids();

    // fn make_map() -> [int: string] { ... }
    // let x: [int: string] = make_map();
    let map_ty = Type::Map {
        key: Type::Int.boxed(),
        value: Type::String.boxed(),
    };
    let make_map = fn_decl(
        "make_map",
        vec![],
        map_ty.clone(),
        vec![return_stmt(Some(map_literal_expr(vec![(
            lit_int(1),
            lit_string("a"),
        )])))],
    );

    let call = call_expr(ident_expr("make_map"), vec![]);
    let call_id = get_expr_id(&call);
    let binding = let_binding("x", Some(map_ty.clone()), call);

    let prog = program(vec![make_map, binding]);
    let tcx = run_ok(prog);
    assert_expr_type(&tcx, call_id, &map_ty);
}

#[test]
fn test_constrain_map_type_mismatch_errors() {
    reset_expr_ids();

    // fn make_map() -> [int: string] { ... }
    // let x: [int: int] = make_map(); // ERROR
    let make_map = fn_decl(
        "make_map",
        vec![],
        Type::Map {
            key: Type::Int.boxed(),
            value: Type::String.boxed(),
        },
        vec![return_stmt(Some(map_literal_expr(vec![])))],
    );

    let call = call_expr(ident_expr("make_map"), vec![]);
    let binding = let_binding(
        "x",
        Some(Type::Map {
            key: Type::Int.boxed(),
            value: Type::Int.boxed(),
        }),
        call,
    );

    let prog = program(vec![make_map, binding]);
    let errors = run_err(prog);
    assert!(!errors.is_empty());
    assert!(
        errors
            .iter()
            .any(|e| matches!(&e.kind, DiagnosticKind::MismatchedTypes { .. })),
        "Expected MismatchedTypes, got: {errors:?}"
    );
}

// ---- struct constrain_assignable tests ----

#[test]
fn test_constrain_struct_passthrough_ok() {
    reset_expr_ids();

    // struct Point { x: int, y: int }
    // fn make_point(x: int, y: int) -> Point { Point { x: x, y: y } }
    // let p: Point = make_point(1, 2);
    let point_ty = Type::Struct {
        name: dummy_ident("Point"),
        type_args: vec![],
        origin: None,
    };
    let point_decl = struct_decl("Point", vec![("x", Type::Int), ("y", Type::Int)], vec![]);
    let make_point = fn_decl(
        "make_point",
        vec![("px", Type::Int), ("py", Type::Int)],
        point_ty.clone(),
        vec![return_stmt(Some(struct_literal_expr(
            "Point",
            vec![("x", ident_expr("px")), ("y", ident_expr("py"))],
        )))],
    );

    let call = call_expr(ident_expr("make_point"), vec![lit_int(1), lit_int(2)]);
    let call_id = get_expr_id(&call);
    let binding = let_binding("p", Some(point_ty.clone()), call);

    let prog = program(vec![point_decl, make_point, binding]);
    let tcx = run_ok(prog);
    assert_expr_type(&tcx, call_id, &point_ty);
}

#[test]
fn test_constrain_generic_struct_name_mismatch_errors() {
    reset_expr_ids();

    // struct Foo { x: int }
    // struct Bar { x: int }
    // fn make_foo() -> Foo { Foo { x: 1 } }
    // let b: Bar = make_foo(); // ERROR
    let foo_decl = struct_decl("Foo", vec![("x", Type::Int)], vec![]);
    let bar_decl = struct_decl("Bar", vec![("x", Type::Int)], vec![]);

    let make_foo = fn_decl(
        "make_foo",
        vec![],
        Type::Struct {
            name: dummy_ident("Foo"),
            type_args: vec![],
            origin: None,
        },
        vec![return_stmt(Some(struct_literal_expr(
            "Foo",
            vec![("x", lit_int(1))],
        )))],
    );

    let call = call_expr(ident_expr("make_foo"), vec![]);
    let binding = let_binding(
        "b",
        Some(Type::Struct {
            name: dummy_ident("Bar"),
            type_args: vec![],
            origin: None,
        }),
        call,
    );

    let prog = program(vec![foo_decl, bar_decl, make_foo, binding]);
    let errors = run_err(prog);
    assert!(!errors.is_empty());
    assert!(
        errors
            .iter()
            .any(|e| matches!(&e.kind, DiagnosticKind::MismatchedTypes { .. })),
        "Expected MismatchedTypes, got: {errors:?}"
    );
}

// ---- list literal annotation flowing through binding ----

#[test]
fn test_constrain_list_literal_annotated_binding() {
    reset_expr_ids();

    // let x: [string] = ["hello", "world"];
    let arr = array_literal(vec![lit_string("hello"), lit_string("world")]);
    let arr_id = get_expr_id(&arr);
    let binding = let_binding(
        "x",
        Some(Type::List {
            elem: Type::String.boxed(),
        }),
        arr,
    );

    let prog = program(vec![binding]);
    let tcx = run_ok(prog);
    assert_expr_type(
        &tcx,
        arr_id,
        &Type::List {
            elem: Type::String.boxed(),
        },
    );
}

#[test]
fn test_constrain_map_literal_annotated_binding() {
    reset_expr_ids();

    // let m: [int: string] = [1: "a"];
    let map = map_literal_expr(vec![(lit_int(1), lit_string("a"))]);
    let map_id = get_expr_id(&map);
    let binding = let_binding(
        "m",
        Some(Type::Map {
            key: Type::Int.boxed(),
            value: Type::String.boxed(),
        }),
        map,
    );

    let prog = program(vec![binding]);
    let tcx = run_ok(prog);
    assert_expr_type(
        &tcx,
        map_id,
        &Type::Map {
            key: Type::Int.boxed(),
            value: Type::String.boxed(),
        },
    );
}

// ---- generic method declaration tests ----

#[test]
fn test_generic_method_type_param_shadows_struct_errors() {
    reset_expr_ids();

    // struct Foo<T> {}
    // with fn bar<T>(self, x: T) -> T { ... }
    // T on the method shadows T on the struct — should error.
    let struct_t_id = TypeVarId(0);
    let method_t_id = TypeVarId(1);
    let method_t_type = Type::Var(method_t_id);
    let gm = generic_method(
        "bar",
        vec![type_param("T", 1)],
        Some(MethodReceiver::Value),
        vec![("x", method_t_type.clone())],
        method_t_type,
        vec![return_stmt(Some(ident_expr("x")))],
    );
    let foo_decl = generic_struct_decl("Foo", vec![type_param("T", 0)], vec![], vec![gm]);

    let prog = program(vec![foo_decl]);
    let errors = run_err(prog);
    assert!(!errors.is_empty());
    assert!(
        errors
            .iter()
            .any(|e| matches!(&e.kind, DiagnosticKind::MethodTypeParamShadowsStruct { .. })),
        "Expected MethodTypeParamShadowsStruct, got: {errors:?}"
    );
    let _ = struct_t_id; // used indirectly via type_param
}

#[test]
fn test_generic_method_declaration_ok() {
    reset_expr_ids();

    // struct Foo {}
    // with fn bar<T>(self, x: T) -> T { ... }
    // Generic method on a non-generic struct — no shadowing, should be accepted.
    let t_type = Type::Var(TypeVarId(0));
    let gm = generic_method(
        "bar",
        vec![type_param("T", 0)],
        Some(MethodReceiver::Value),
        vec![("x", t_type.clone())],
        t_type,
        vec![return_stmt(Some(ident_expr("x")))],
    );
    let foo_decl = struct_decl("Foo", vec![], vec![gm]);

    let prog = program(vec![foo_decl]);
    let _ = run_ok(prog);
}

#[test]
fn test_generic_method_on_generic_struct_no_shadow_ok() {
    reset_expr_ids();

    // struct Wrapper<T> {}
    // with fn convert<U>(self, x: U) -> U { ... }
    // T and U are different names — no shadowing, should be accepted.
    let u_type = Type::Var(TypeVarId(1));
    let gm = generic_method(
        "convert",
        vec![type_param("U", 1)],
        Some(MethodReceiver::Value),
        vec![("x", u_type.clone())],
        u_type,
        vec![return_stmt(Some(ident_expr("x")))],
    );
    let wrapper_decl = generic_struct_decl("Wrapper", vec![type_param("T", 0)], vec![], vec![gm]);

    let prog = program(vec![wrapper_decl]);
    let _ = run_ok(prog);
}

// ---- generic method call tests ----

#[test]
fn test_generic_method_call_on_non_generic_struct() {
    reset_expr_ids();

    // struct Foo {}
    // with fn bar<T>(self, x: T) -> T { x }
    // let f = Foo {};
    // let result = f.bar(42);  -- should infer T = int, result type = int
    let t_type = Type::Var(TypeVarId(0));
    let gm = generic_method(
        "bar",
        vec![type_param("T", 0)],
        Some(MethodReceiver::Value),
        vec![("x", t_type.clone())],
        t_type,
        vec![return_stmt(Some(ident_expr("x")))],
    );
    let foo_decl = struct_decl("Foo", vec![], vec![gm]);

    let f_binding = let_binding("f", None, struct_literal_expr("Foo", vec![]));
    let call = call_expr(field_expr(ident_expr("f"), "bar"), vec![lit_int(42)]);
    let call_id = get_expr_id(&call);
    let result_binding = let_binding("result", None, call);

    let prog = program(vec![foo_decl, f_binding, result_binding]);
    let tcx = run_ok(prog);
    assert_expr_type(&tcx, call_id, &Type::Int);
}

#[test]
fn test_generic_method_call_on_generic_struct() {
    reset_expr_ids();

    // struct Wrapper<T> { value: T }
    // with fn convert<U>(self, x: U) -> U { x }
    // let w = Wrapper { value: 42 };
    // let result = w.convert("hello");  -- infers T=int (from struct), U=string (from arg)
    let struct_t_id = TypeVarId(0);
    let method_u_id = TypeVarId(1);
    let method_u_type = Type::Var(method_u_id);
    let gm = generic_method(
        "convert",
        vec![type_param("U", 1)],
        Some(MethodReceiver::Value),
        vec![("x", method_u_type.clone())],
        method_u_type,
        vec![return_stmt(Some(ident_expr("x")))],
    );
    let wrapper_decl = generic_struct_decl(
        "Wrapper",
        vec![type_param("T", 0)],
        vec![("value", Type::Var(struct_t_id))],
        vec![gm],
    );

    let w_binding = let_binding(
        "w",
        None,
        struct_literal_expr("Wrapper", vec![("value", lit_int(42))]),
    );
    let call = call_expr(
        field_expr(ident_expr("w"), "convert"),
        vec![lit_string("hello")],
    );
    let call_id = get_expr_id(&call);
    let result_binding = let_binding("result", None, call);

    let prog = program(vec![wrapper_decl, w_binding, result_binding]);
    let tcx = run_ok(prog);
    assert_expr_type(&tcx, call_id, &Type::String);
}

#[test]
fn test_generic_method_call_explicit_type_args() {
    reset_expr_ids();

    // struct Foo {}
    // with fn bar<T>(self, x: T) -> T { x }
    // let f = Foo {};
    // let result = f.bar<int>(42);  -- explicit T = int
    let t_type = Type::Var(TypeVarId(0));
    let gm = generic_method(
        "bar",
        vec![type_param("T", 0)],
        Some(MethodReceiver::Value),
        vec![("x", t_type.clone())],
        t_type,
        vec![return_stmt(Some(ident_expr("x")))],
    );
    let foo_decl = struct_decl("Foo", vec![], vec![gm]);

    let f_binding = let_binding("f", None, struct_literal_expr("Foo", vec![]));
    let call = call_expr_with_type_args(
        field_expr(ident_expr("f"), "bar"),
        vec![lit_int(42)],
        vec![Type::Int],
    );
    let call_id = get_expr_id(&call);
    let result_binding = let_binding("result", None, call);

    let prog = program(vec![foo_decl, f_binding, result_binding]);
    let tcx = run_ok(prog);
    assert_expr_type(&tcx, call_id, &Type::Int);
}

#[test]
fn test_generic_method_call_explicit_type_arg_arity_mismatch() {
    reset_expr_ids();

    // struct Foo {}
    // with fn bar<T>(self, x: T) -> T { x }
    // let f = Foo {};
    // f.bar<int, string>(42);  -- too many type args => GenericArgNumMismatch
    let t_type = Type::Var(TypeVarId(0));
    let gm = generic_method(
        "bar",
        vec![type_param("T", 0)],
        Some(MethodReceiver::Value),
        vec![("x", t_type.clone())],
        t_type,
        vec![return_stmt(Some(ident_expr("x")))],
    );
    let foo_decl = struct_decl("Foo", vec![], vec![gm]);

    let f_binding = let_binding("f", None, struct_literal_expr("Foo", vec![]));
    let call = call_expr_with_type_args(
        field_expr(ident_expr("f"), "bar"),
        vec![lit_int(42)],
        vec![Type::Int, Type::String],
    );
    let result_binding = let_binding("result", None, call);

    let prog = program(vec![foo_decl, f_binding, result_binding]);
    let errors = run_err(prog);
    assert!(
        errors
            .iter()
            .any(|e| matches!(&e.kind, DiagnosticKind::GenericArgNumMismatch { .. })),
        "Expected GenericArgNumMismatch, got: {errors:?}"
    );
}

#[test]
fn test_static_generic_method_inferred() {
    reset_expr_ids();

    // struct Foo {}
    // with static fn make<T>(x: T) -> T { x }
    // let result = Foo.make(42);  -- infers T = int
    let t_type = Type::Var(TypeVarId(0));
    let gm = generic_method(
        "make",
        vec![type_param("T", 0)],
        None,
        vec![("x", t_type.clone())],
        t_type,
        vec![return_stmt(Some(ident_expr("x")))],
    );
    let foo_decl = struct_decl("Foo", vec![], vec![gm]);

    let call = call_expr(field_expr(ident_expr("Foo"), "make"), vec![lit_int(42)]);
    let call_id = get_expr_id(&call);
    let result_binding = let_binding("result", None, call);

    let prog = program(vec![foo_decl, result_binding]);
    let tcx = run_ok(prog);
    assert_expr_type(&tcx, call_id, &Type::Int);
}

#[test]
fn test_static_generic_method_on_generic_struct_inferred() {
    reset_expr_ids();

    // struct Wrapper<T> { value: T }
    // with static fn with_extra<U>(val: T, extra: U) -> U { extra }
    // let result = Wrapper.with_extra(42, "hello");  -- infers T=int, U=string
    let struct_t_id = TypeVarId(0);
    let method_u_id = TypeVarId(1);
    let method_u_type = Type::Var(method_u_id);
    let gm = generic_method(
        "with_extra",
        vec![type_param("U", 1)],
        None,
        vec![
            ("val", Type::Var(struct_t_id)),
            ("extra", method_u_type.clone()),
        ],
        method_u_type,
        vec![return_stmt(Some(ident_expr("extra")))],
    );
    let wrapper_decl = generic_struct_decl(
        "Wrapper",
        vec![type_param("T", 0)],
        vec![("value", Type::Var(struct_t_id))],
        vec![gm],
    );

    let call = call_expr(
        field_expr(ident_expr("Wrapper"), "with_extra"),
        vec![lit_int(42), lit_string("hello")],
    );
    let call_id = get_expr_id(&call);
    let result_binding = let_binding("result", None, call);

    let prog = program(vec![wrapper_decl, result_binding]);
    let tcx = run_ok(prog);
    assert_expr_type(&tcx, call_id, &Type::String);
}

#[test]
fn test_generic_method_multiple_type_params() {
    reset_expr_ids();

    // struct Foo {}
    // with fn pair<A, B>(self, a: A, b: B) -> B { b }
    // let f = Foo {};
    // let result = f.pair(42, "hello");  -- infers A=int, B=string, result type = string
    let a_type = Type::Var(TypeVarId(0));
    let b_type = Type::Var(TypeVarId(1));
    let gm = generic_method(
        "pair",
        vec![type_param("A", 0), type_param("B", 1)],
        Some(MethodReceiver::Value),
        vec![("a", a_type), ("b", b_type.clone())],
        b_type,
        vec![return_stmt(Some(ident_expr("b")))],
    );
    let foo_decl = struct_decl("Foo", vec![], vec![gm]);

    let f_binding = let_binding("f", None, struct_literal_expr("Foo", vec![]));
    let call = call_expr(
        field_expr(ident_expr("f"), "pair"),
        vec![lit_int(42), lit_string("hello")],
    );
    let call_id = get_expr_id(&call);
    let result_binding = let_binding("result", None, call);

    let prog = program(vec![foo_decl, f_binding, result_binding]);
    let tcx = run_ok(prog);
    assert_expr_type(&tcx, call_id, &Type::String);
}

#[test]
fn test_generic_method_returns_struct_instantiation() {
    reset_expr_ids();

    // struct Container<T> { value: T }
    // with fn wrap<U>(self, x: U) -> Container<U> { Container { value: x } }
    // let c = Container { value: 42 };
    // let result = c.wrap("hello");  -- infers T=int (struct), U=string (method), result = Container<string>
    let struct_t_id = TypeVarId(0);
    let method_u_id = TypeVarId(1);
    let container_of_u = Type::Struct {
        name: dummy_ident("Container"),
        type_args: vec![Type::Var(method_u_id)],
        origin: None,
    };
    let gm = generic_method(
        "wrap",
        vec![type_param("U", 1)],
        Some(MethodReceiver::Value),
        vec![("x", Type::Var(method_u_id))],
        container_of_u.clone(),
        vec![return_stmt(Some(struct_literal_expr(
            "Container",
            vec![("value", ident_expr("x"))],
        )))],
    );
    let container_decl = generic_struct_decl(
        "Container",
        vec![type_param("T", 0)],
        vec![("value", Type::Var(struct_t_id))],
        vec![gm],
    );

    let c_binding = let_binding(
        "c",
        None,
        struct_literal_expr("Container", vec![("value", lit_int(42))]),
    );
    let call = call_expr(
        field_expr(ident_expr("c"), "wrap"),
        vec![lit_string("hello")],
    );
    let call_id = get_expr_id(&call);
    let result_binding = let_binding("result", None, call);

    let prog = program(vec![container_decl, c_binding, result_binding]);
    let tcx = run_ok(prog);
    let expected = Type::Struct {
        name: dummy_ident("Container"),
        type_args: vec![Type::String],
        origin: None,
    };
    assert_expr_type(&tcx, call_id, &expected);
}

#[test]
fn test_generic_method_body_type_error() {
    reset_expr_ids();

    // struct Foo {}
    // with fn bad<T>(self, x: T) -> T { x - x }  (Sub only valid for numeric types)
    // let f = Foo {};
    // f.bad("oops");  -- T=string, body tries string - string => type error
    let t_type = Type::Var(TypeVarId(0));
    let body_sub = binary_expr(ident_expr("x"), BinaryOp::Sub, ident_expr("x"));
    let gm = generic_method(
        "bad",
        vec![type_param("T", 0)],
        Some(MethodReceiver::Value),
        vec![("x", t_type.clone())],
        t_type,
        vec![return_stmt(Some(body_sub))],
    );
    let foo_decl = struct_decl("Foo", vec![], vec![gm]);

    let f_binding = let_binding("f", None, struct_literal_expr("Foo", vec![]));
    let call = call_expr(field_expr(ident_expr("f"), "bad"), vec![lit_string("oops")]);
    let result_binding = let_binding("result", None, call);

    let prog = program(vec![foo_decl, f_binding, result_binding]);
    let errors = run_err(prog);
    assert!(
        !errors.is_empty(),
        "Expected body type error, got no errors"
    );
}

// ---- constrain_assignable: new container branches ----

#[test]
fn test_constrain_list_inferred_elem_from_annotation() {
    reset_expr_ids();

    // let xs: [int] = [1, 2, 3];
    // -- list literal starts as [Infer], annotation drives elem to int
    let arr = array_literal(vec![lit_int(1), lit_int(2), lit_int(3)]);
    let arr_id = get_expr_id(&arr);
    let binding = let_binding(
        "xs",
        Some(Type::List {
            elem: Type::Int.boxed(),
        }),
        arr,
    );

    let prog = program(vec![binding]);
    let tcx = run_ok(prog);
    assert_expr_type(
        &tcx,
        arr_id,
        &Type::List {
            elem: Type::Int.boxed(),
        },
    );
}

#[test]
fn test_constrain_list_inferred_elem_mismatch_errors() {
    reset_expr_ids();

    // fn get_strings() -> [string] { ["a", "b"] }
    // let xs: [int] = get_strings();  -- ERROR
    let get_strings = fn_decl(
        "get_strings",
        vec![],
        Type::List {
            elem: Type::String.boxed(),
        },
        vec![return_stmt(Some(array_literal(vec![lit_string("a")])))],
    );

    let call = call_expr(ident_expr("get_strings"), vec![]);
    let binding = let_binding(
        "xs",
        Some(Type::List {
            elem: Type::Int.boxed(),
        }),
        call,
    );

    let prog = program(vec![get_strings, binding]);
    let errors = run_err(prog);
    assert!(
        errors
            .iter()
            .any(|e| matches!(&e.kind, DiagnosticKind::MismatchedTypes { .. })),
        "Expected MismatchedTypes, got: {errors:?}"
    );
}

#[test]
fn test_constrain_map_inferred_key_value_from_annotation() {
    reset_expr_ids();

    // let m: [string: int] = ["a": 1];
    let map = map_literal_expr(vec![(lit_string("a"), lit_int(1))]);
    let map_id = get_expr_id(&map);
    let binding = let_binding(
        "m",
        Some(Type::Map {
            key: Type::String.boxed(),
            value: Type::Int.boxed(),
        }),
        map,
    );

    let prog = program(vec![binding]);
    let tcx = run_ok(prog);
    assert_expr_type(
        &tcx,
        map_id,
        &Type::Map {
            key: Type::String.boxed(),
            value: Type::Int.boxed(),
        },
    );
}

#[test]
fn test_constrain_map_inferred_value_mismatch_errors() {
    reset_expr_ids();

    // fn get_map() -> [int: string] { [1: "a"] }
    // let m: [int: int] = get_map();  -- ERROR
    let get_map = fn_decl(
        "get_map",
        vec![],
        Type::Map {
            key: Type::Int.boxed(),
            value: Type::String.boxed(),
        },
        vec![return_stmt(Some(map_literal_expr(vec![(
            lit_int(1),
            lit_string("a"),
        )])))],
    );

    let call = call_expr(ident_expr("get_map"), vec![]);
    let binding = let_binding(
        "m",
        Some(Type::Map {
            key: Type::Int.boxed(),
            value: Type::Int.boxed(),
        }),
        call,
    );

    let prog = program(vec![get_map, binding]);
    let errors = run_err(prog);
    assert!(
        errors
            .iter()
            .any(|e| matches!(&e.kind, DiagnosticKind::MismatchedTypes { .. })),
        "Expected MismatchedTypes, got: {errors:?}"
    );
}

#[test]
fn test_constrain_generic_struct_type_arg_inferred_from_annotation() {
    reset_expr_ids();

    // struct Wrapper<T> { value: T }
    // fn make() -> Wrapper<int> { Wrapper { value: 42 } }
    // let w: Wrapper<int> = make();  -- Wrapper<Infer> constrained to Wrapper<int>
    let t_id = TypeVarId(0);
    let wrapper_decl = generic_struct_decl(
        "Wrapper",
        vec![type_param("T", 0)],
        vec![("value", Type::Var(t_id))],
        vec![],
    );

    let wrapper_int = Type::Struct {
        name: dummy_ident("Wrapper"),
        type_args: vec![Type::Int],
        origin: None,
    };
    let make_fn = fn_decl(
        "make",
        vec![],
        wrapper_int.clone(),
        vec![return_stmt(Some(struct_literal_expr(
            "Wrapper",
            vec![("value", lit_int(42))],
        )))],
    );

    let call = call_expr(ident_expr("make"), vec![]);
    let call_id = get_expr_id(&call);
    let binding = let_binding("w", Some(wrapper_int.clone()), call);

    let prog = program(vec![wrapper_decl, make_fn, binding]);
    let tcx = run_ok(prog);
    assert_expr_type(&tcx, call_id, &wrapper_int);
}

#[test]
fn test_constrain_generic_struct_type_arg_mismatch_errors() {
    reset_expr_ids();

    // struct Wrapper<T> { value: T }
    // fn make_int() -> Wrapper<int> { Wrapper { value: 1 } }
    // let w: Wrapper<string> = make_int();  -- ERROR
    let t_id = TypeVarId(0);
    let wrapper_decl = generic_struct_decl(
        "Wrapper",
        vec![type_param("T", 0)],
        vec![("value", Type::Var(t_id))],
        vec![],
    );

    let make_int = fn_decl(
        "make_int",
        vec![],
        Type::Struct {
            name: dummy_ident("Wrapper"),
            type_args: vec![Type::Int],
            origin: None,
        },
        vec![return_stmt(Some(struct_literal_expr(
            "Wrapper",
            vec![("value", lit_int(1))],
        )))],
    );

    let call = call_expr(ident_expr("make_int"), vec![]);
    let binding = let_binding(
        "w",
        Some(Type::Struct {
            name: dummy_ident("Wrapper"),
            type_args: vec![Type::String],
            origin: None,
        }),
        call,
    );

    let prog = program(vec![wrapper_decl, make_int, binding]);
    let errors = run_err(prog);
    assert!(
        errors
            .iter()
            .any(|e| matches!(&e.kind, DiagnosticKind::MismatchedTypes { .. })),
        "Expected MismatchedTypes, got: {errors:?}"
    );
}

#[test]
fn test_constrain_generic_enum_type_arg_assignable() {
    reset_expr_ids();

    // enum Box<T> { Wrap(T) }
    // fn use_box(x: Box<int>) { let b: Box<int> = x; } - Box<int> assignable to Box<int>
    let t_id = TypeVarId(0);
    let box_decl = StmtNode {
        node: Stmt::Enum(EnumDeclNode {
            node: EnumDecl {
                annotations: vec![],
                doc: None,
                name: dummy_ident("Box"),
                visibility: Visibility::Public,
                type_params: vec![TypeParam {
                    name: dummy_ident("T"),
                    id: t_id,
                }],
                const_params: vec![],
                variants: vec![EnumVariant {
                    annotations: vec![],
                    name: dummy_ident("Wrap"),
                    kind: VariantKind::Tuple(vec![Type::Var(t_id)]),
                    doc: None,
                }],
            },
            span: dummy_span(),
        }),
        span: dummy_span(),
    };

    let box_int = Type::Enum {
        name: dummy_ident("Box"),
        type_args: vec![Type::Int],
        origin: None,
    };
    let inner_binding = let_binding("b", Some(box_int.clone()), ident_expr("x"));
    let use_box_fn = fn_decl(
        "use_box",
        vec![("x", box_int)],
        Type::Void,
        vec![inner_binding],
    );

    let prog = program(vec![box_decl, use_box_fn]);
    let _ = run_ok(prog);
}

#[test]
fn test_constrain_generic_enum_type_arg_mismatch_errors() {
    reset_expr_ids();

    // enum Box<T> { Wrap(T) }
    // fn make_int() -> Box<int> { ... }
    // let b: Box<string> = make_int();  -- ERROR
    let t_id = TypeVarId(0);
    let box_decl = StmtNode {
        node: Stmt::Enum(EnumDeclNode {
            node: EnumDecl {
                annotations: vec![],
                doc: None,
                name: dummy_ident("Box"),
                visibility: Visibility::Public,
                type_params: vec![TypeParam {
                    name: dummy_ident("T"),
                    id: t_id,
                }],
                const_params: vec![],
                variants: vec![EnumVariant {
                    annotations: vec![],
                    name: dummy_ident("Wrap"),
                    kind: VariantKind::Tuple(vec![Type::Var(t_id)]),
                    doc: None,
                }],
            },
            span: dummy_span(),
        }),
        span: dummy_span(),
    };

    let box_int = Type::Enum {
        name: dummy_ident("Box"),
        type_args: vec![Type::Int],
        origin: None,
    };
    let box_string = Type::Enum {
        name: dummy_ident("Box"),
        type_args: vec![Type::String],
        origin: None,
    };
    let inner_binding = let_binding("b", Some(box_string), ident_expr("x"));
    let use_box_fn = fn_decl(
        "use_box",
        vec![("x", box_int)],
        Type::Void,
        vec![inner_binding],
    );

    let prog = program(vec![box_decl, use_box_fn]);
    let errors = run_err(prog);
    assert!(
        errors
            .iter()
            .any(|e| matches!(&e.kind, DiagnosticKind::MismatchedTypes { .. })),
        "Expected MismatchedTypes, got: {errors:?}"
    );
}
