use super::support::{check, check_named, generic_body};
use crate::{
    ast::{ArrayLen, ConstValue, EscapeMode, Ident, Type},
    typecheck::{
        BodyInstanceKey, FunctionValueKind, FunctionValueOrigin, GenericArgs, LambdaBodyKey,
        LocalDefKind, LocalUseMode, semantic_use::ConstValueMap,
    },
};

fn assert_single_const(values: &ConstValueMap, value: ConstValue) {
    assert_eq!(values.len(), 1);
    assert_eq!(values.values().next(), Some(&value));
}

fn assert_main_mut_borrows(source: &str, name: &str) {
    let result = check(source).expect("typecheck failed");
    let body = result.expect_body(&result.function_body("main"));
    let local = body
        .locals
        .defs
        .values()
        .find(|fact| fact.name.as_str() == name)
        .expect("missing binding");
    assert!(
        body.locals
            .uses
            .values()
            .any(|fact| fact.local == local.id && fact.mode == LocalUseMode::MutBorrow)
    );
}

#[test]
fn records_params_bindings_and_uses() {
    let source = r"
fn f(a: int, var b: int) -> int {
    let x = a;
    var y = x;
    y = b;
    y += 1;
    y
}
";
    let result = check(source).expect("typecheck failed");
    let body_key = result.function_body("f");
    let body = result.expect_body(&body_key);
    let locals = &body.locals;

    let a = locals.param_defs[&0];
    let b = locals.param_defs[&1];
    assert_eq!(locals.defs[&a].name.as_str(), "a");
    assert_eq!(locals.defs[&a].kind, LocalDefKind::Parameter);
    assert!(!locals.defs[&a].mutable);
    assert_eq!(locals.defs[&b].name.as_str(), "b");
    assert!(locals.defs[&b].mutable);

    let binding = |name: &str| {
        locals
            .defs
            .values()
            .find(|fact| fact.name.as_str() == name)
            .expect("missing binding fact")
    };
    let x = binding("x");
    let y = binding("y");
    assert_eq!(locals.binding_defs.len(), 2);
    assert!(locals.binding_defs.values().any(|&local| local == x.id));
    assert!(locals.binding_defs.values().any(|&local| local == y.id));
    assert_eq!(x.kind, LocalDefKind::Binding);
    assert!(!x.mutable);
    assert!(y.mutable);

    let has_use = |local, mode| {
        locals
            .uses
            .values()
            .any(|fact| fact.local == local && fact.mode == mode)
    };
    assert!(has_use(y.id, LocalUseMode::Assign));
    assert!(has_use(y.id, LocalUseMode::CompoundAssign));
    assert!(has_use(a, LocalUseMode::Read));
    assert!(has_use(b, LocalUseMode::Read));
}

#[test]
fn wildcard_creates_no_local() {
    let result = check("fn f() { let _ = 1; }").expect("typecheck failed");
    let body_key = result.function_body("f");
    let locals = &result.expect_body(&body_key).locals;
    assert!(locals.defs.is_empty());
    assert!(locals.binding_defs.is_empty());
}

#[test]
fn mutating_receiver_records_mut_borrow_use() {
    assert_main_mut_borrows(
        r"
struct Counter { value: int }
extend Counter { fn reset(var self) { self.value = 0; } }
fn main() { var counter = Counter { value: 1 }; counter.reset(); }
",
        "counter",
    );
}

#[test]
fn map_entry_payload_receiver_records_payload_alias_use() {
    assert_main_mut_borrows(
        r#"
extend [int] { fn add(var self, _value: int) {} }
fn main() {
    var groups: [string: [int]] = ["a": [1, 2]];
    if var xs? = groups["a"] {
        xs.add(3);
    }
}
"#,
        "xs",
    );
}

#[test]
fn map_entry_payload_place_return_preserves_payload_alias_use() {
    assert_main_mut_borrows(
        r#"
extend [int] {
    fn id(var self) -> var [int] { self }
    fn add(var self, _value: int) {}
}
fn main() {
    var groups: [string: [int]] = ["a": [1, 2]];
    if var xs? = groups["a"] {
        xs.id().add(3);
    }
}
"#,
        "xs",
    );
}

#[test]
fn function_facts_include_default_params() {
    let result = check(r#"fn f(a: int, message: string = "ok") -> bool { true }"#)
        .expect("typecheck failed");
    let fact = result
        .function_facts()
        .iter()
        .find(|fact| fact.name == Ident::new("f"))
        .expect("missing function fact");

    assert_eq!(fact.args, GenericArgs::default());
    assert_eq!(fact.params.len(), 2);
    assert_eq!(fact.params[0].name.as_str(), "a");
    assert_eq!(fact.params[0].ty, Type::Int);
    assert_eq!(fact.params[0].escape, EscapeMode::NonEscaping);
    assert_eq!(fact.params[1].name.as_str(), "message");
    assert_eq!(fact.params[1].ty, Type::String);
    assert_eq!(fact.ret.ty, Type::Bool);
}

#[test]
fn function_facts_preserve_param_escape() {
    let result = check("fn f(cb: escaping fn()) {}").expect("typecheck failed");
    let fact = result
        .function_facts()
        .iter()
        .find(|fact| fact.name == Ident::new("f"))
        .expect("missing function fact");

    assert_eq!(fact.params[0].escape, EscapeMode::Escaping);
}

#[test]
fn omitted_default_args_are_recorded() {
    let result = check(r#"fn f(a: int, message: string = "ok") {} fn main() { f(1); }"#)
        .expect("typecheck failed");
    assert_eq!(result.default_args().len(), 1);
    let (&call, defaults) = result
        .default_args()
        .iter()
        .next()
        .expect("missing default arg facts");
    assert!(result.calls().contains_key(&call));
    assert_eq!(defaults.len(), 1);
    let fact = &defaults[0];
    assert_eq!(fact.call, call);
    assert_eq!(fact.param_index, 1);
    assert_eq!(fact.ty, Type::String);
    assert_eq!(fact.callee.args, GenericArgs::default());
    assert!(result.types().any(|(expr, _)| *expr == fact.default.expr));
}

#[test]
fn explicit_default_param_records_no_default_arg() {
    let result = check(r#"fn f(a: int, message: string = "ok") {} fn main() { f(1, "x"); }"#)
        .expect("typecheck failed");
    assert!(result.default_args().is_empty());
}

#[test]
fn top_level_const_value_records_one_fact() {
    let result =
        check("const BASE: int = 10; fn main() { let value = BASE; }").expect("typecheck failed");
    assert_single_const(result.const_values(), ConstValue::Int(10));
}

#[test]
fn imported_const_value_records_fact() {
    let result = check_named(
        "import helper { BASE }; fn main() { let value = BASE; }",
        &[("helper", "pub const BASE: int = 10;")],
    )
    .expect("typecheck failed");
    assert_single_const(result.const_values(), ConstValue::Int(10));
}

#[test]
fn module_qualified_const_value_records_fact() {
    let result = check_named(
        "import helper as h; fn main() { let value = h.BASE; }",
        &[("helper", "pub const BASE: int = 10;")],
    )
    .expect("typecheck failed");
    assert_single_const(result.const_values(), ConstValue::Int(10));
}

#[test]
fn module_qualified_global_records_no_const_fact() {
    let result = check_named(
        "import helper as h; fn main() { let value = h.count; }",
        &[("helper", "pub lazy var count = 10;")],
    )
    .expect("typecheck failed");
    assert!(result.const_values().is_empty());
    assert_eq!(result.global_accesses().len(), 1);
}

#[test]
fn local_const_value_records_fact_without_local_use() {
    let result =
        check("fn main() { const BASE: int = 10; let value = BASE; }").expect("typecheck failed");
    let body = result.expect_body(&result.function_body("main"));
    assert_single_const(&body.const_values, ConstValue::Int(10));
    assert!(body.locals.uses.is_empty());
}

#[test]
fn lambda_local_const_value_records_fact_without_capture() {
    let result = check("fn main() { const N: int = 10; let f: fn() -> int = || { N + 1 }; f(); }")
        .expect("typecheck failed");
    let lambda = result
        .lambda_escapes()
        .keys()
        .next()
        .copied()
        .expect("missing lambda fact");
    let body = result.expect_body(&BodyInstanceKey::Lambda(LambdaBodyKey {
        expr: lambda,
        specialization: GenericArgs::default(),
    }));
    assert_single_const(&body.const_values, ConstValue::Int(10));
    assert!(body.locals.uses.is_empty());
    assert!(result.lambda_captures().is_empty());
}

#[test]
fn non_const_local_value_records_no_const_fact() {
    let result =
        check("fn main() { let value = 10; let copy = value; }").expect("typecheck failed");
    assert!(result.const_values().is_empty());
}

#[test]
fn const_facts_remain_body_scoped_when_flattened() {
    let result =
        check("const BASE: int = 10; fn value() -> int { BASE } fn main() { let value = BASE; }")
            .expect("typecheck failed");
    let value_body = result.function_body("value");
    let main_body = result.function_body("main");
    assert_eq!(result.expect_body(&value_body).const_values.len(), 1);
    assert_eq!(result.expect_body(&main_body).const_values.len(), 1);
    assert_eq!(result.const_values().len(), 2);
}

#[test]
fn omitted_struct_defaults_are_recorded_in_declaration_order() {
    let result = check(
        "struct Point { x: int = 1, y: int = 2, z: int } fn main() { let p = Point { z: 3 }; }",
    )
    .expect("typecheck failed");
    let defaults = result.default_fields();
    assert_eq!(defaults.len(), 1);
    let fields = defaults
        .values()
        .next()
        .expect("missing default field facts");
    assert_eq!(fields.len(), 2);
    assert_eq!(fields[0].field.as_str(), "x");
    assert_eq!(fields[0].slot, 0);
    assert_eq!(fields[0].ty, Type::Int);
    assert_eq!(fields[1].field.as_str(), "y");
    assert_eq!(fields[1].slot, 1);
    assert_eq!(fields[1].ty, Type::Int);
}

#[test]
fn explicit_fields_record_no_default_fields() {
    let result = check("struct Point { x: int = 1 } fn main() { let p = Point { x: 2 }; }")
        .expect("typecheck failed");
    assert!(result.default_fields().is_empty());
}

#[test]
fn dataref_defaults_are_recorded() {
    let result = check("dataref Box { x: int = 1 } fn main() { let b = Box {}; }")
        .expect("typecheck failed");
    let fields = result
        .default_fields()
        .values()
        .next()
        .expect("missing dataref default field facts");
    assert_eq!(fields.len(), 1);
    assert_eq!(fields[0].field.as_str(), "x");
    assert_eq!(fields[0].slot, 0);
    assert_eq!(fields[0].ty, Type::Int);
}

#[test]
fn generic_default_field_records_instantiated_owner_and_type() {
    let result =
        check("struct Box<T> { value: T? = nil } fn main() { let b: Box<int> = Box<int> {}; }")
            .expect("typecheck failed");
    let field = result
        .default_fields()
        .values()
        .next()
        .and_then(|fields| fields.first())
        .expect("missing generic default field fact");
    assert_eq!(field.field.as_str(), "value");
    assert!(!matches!(field.ty, Type::Var(_)));
    let Type::Nominal(owner) = &field.owner else {
        panic!("default owner is not nominal");
    };
    assert_eq!(owner.type_args, vec![Type::Int]);
}

#[test]
fn declaration_generic_default_call_does_not_export_template_specialization() {
    let result = check(
        "fn default_value<T>() -> T? { nil } struct Box<T> { value: T? = default_value<T>() } fn main() { let b: Box<int> = Box<int> {}; }",
    )
    .expect("typecheck failed");
    assert!(result.function_facts().iter().all(|fact| {
        fact.id.name != Ident::new("default_value")
            || fact
                .args
                .type_args
                .iter()
                .all(|arg| !matches!(arg, Type::Var(_)))
    }));
}

#[test]
fn enum_variant_and_extern_literals_record_no_default_fields() {
    let result = check(
        "enum E { A { x: int } } extern type Point rep inline { init; x: int; } fn main() { let e = E.A { x: 1 }; let p = Point { x: 2 }; }",
    )
    .expect("typecheck failed");
    assert!(result.default_fields().is_empty());
}

#[test]
fn function_value_origins_are_specific() {
    let cases = [
        (
            "fn tick() {} fn main() { let f: fn() = tick; f(); }",
            FunctionValueOrigin::KnownLocal,
        ),
        (
            "struct Holder { f: fn() } fn main(h: Holder) { let f: fn() = h.f; }",
            FunctionValueOrigin::AggregateField,
        ),
        (
            "fn main(pair: (fn(), int)) { let f: fn() = pair.0; }",
            FunctionValueOrigin::TupleField,
        ),
        (
            "dataref Holder { f: fn() } fn main(h: Holder) { let f: fn() = h.f; }",
            FunctionValueOrigin::DataRefProjection,
        ),
        (
            "fn main(xs: [fn(); 2]) { let f: fn() = xs[0]; }",
            FunctionValueOrigin::FixedArrayElement,
        ),
        (
            "fn main(xs: [fn()]) { let f: fn() = xs[0]; }",
            FunctionValueOrigin::ListElement,
        ),
        (
            "fn main(xs: [string: fn()]) { xs[\"a\"]?(); }",
            FunctionValueOrigin::MapValue,
        ),
        (
            "fn main(f: fn()?) { f?(); }",
            FunctionValueOrigin::UnknownProjection,
        ),
        (
            "fn tick() {} lazy let f: fn() = tick; fn main() { let g: fn() = f; }",
            FunctionValueOrigin::GlobalRoot,
        ),
        (
            "struct Holder { f: fn() } fn tick() {} lazy let h: Holder = Holder { f: tick }; fn main() { let f: fn() = h.f; }",
            FunctionValueOrigin::GlobalProjection,
        ),
        (
            "fn tick() {} fn make() -> fn() { tick } fn main() { make()(); }",
            FunctionValueOrigin::CallReturn,
        ),
    ];

    for (source, origin) in cases {
        let result = check(source).expect(source);
        let body = result.expect_body(&result.function_body("main"));
        assert!(
            body.function_values
                .values()
                .any(|fact| fact.kind == FunctionValueKind::Storage(origin)),
            "missing {origin:?} for {source}; facts: {:?}",
            body.function_values
        );
    }
}

#[test]
fn records_named_function_value_and_value_call() {
    let result =
        check("fn tick() {} fn main() { let f: fn() = tick; f(); }").expect("typecheck failed");

    let body = result.expect_body(&result.function_body("main"));
    assert!(
        body.function_values
            .values()
            .any(|fact| matches!(fact.kind, FunctionValueKind::Named(_)))
    );
    let (&call_expr, call) = body
        .function_value_calls
        .iter()
        .next()
        .expect("missing function-value call fact");
    assert_eq!(call_expr, call.expr);
    assert_eq!(call.args.len(), 0);
    assert!(matches!(call.sig, Type::Func { .. }));
    assert!(body.function_values.contains_key(&call.callee));
    assert!(!body.calls.contains_key(&call.expr));
}

#[test]
fn records_lambda_function_value_and_call() {
    let result = check("fn main() { let f: fn() = || {}; f(); }").expect("typecheck failed");

    let body = result.expect_body(&result.function_body("main"));
    assert!(body.function_values.values().any(|fact| {
        matches!(fact.kind, FunctionValueKind::Lambda { lambda_expr } if lambda_expr == fact.expr)
    }));
    let call = body
        .function_value_calls
        .values()
        .next()
        .expect("missing function-value call fact");
    assert!(body.function_values.contains_key(&call.callee));
    assert!(!body.calls.contains_key(&call.expr));
}

#[test]
fn direct_named_call_is_not_function_value_call() {
    let result = check("fn tick() {} fn main() { tick(); }").expect("typecheck failed");

    let body = result.expect_body(&result.function_body("main"));
    assert_eq!(body.calls.len(), 1);
    assert!(body.function_value_calls.is_empty());
    assert!(body.function_values.is_empty());
}

#[test]
fn function_value_call_preserves_parameter_escape_modes() {
    let result = check(
        r"
fn main(non: fn(fn()), esc: fn(escaping fn()), cb: escaping fn()) {
    non(cb);
    esc(cb);
}
",
    )
    .expect("typecheck failed");
    let body = result.expect_body(&result.function_body("main"));
    assert!(body.calls.is_empty());
    assert_eq!(body.function_value_calls.len(), 2);
    let mut escapes = body
        .function_value_calls
        .values()
        .map(|fact| fact.args[0].escape)
        .collect::<Vec<_>>();
    escapes.sort_by_key(|escape| escape.is_escaping());

    assert_eq!(escapes, vec![EscapeMode::NonEscaping, EscapeMode::Escaping]);
}

#[test]
fn optional_function_call_has_unwrapped_callee_fact() {
    let result = check(
        r"
fn tick() {}
fn main(cond: bool) {
    let f: fn()? = if cond { tick } else { nil };
    f?();
}
",
    )
    .expect("typecheck failed");
    let body = result.expect_body(&result.function_body("main"));
    let call = body
        .function_value_calls
        .values()
        .next()
        .expect("missing function-value call fact");

    assert!(body.function_values.contains_key(&call.callee));
    assert_eq!(body.function_values[&call.callee].ty, call.sig);
    assert!(!body.calls.contains_key(&call.expr));
}

#[test]
fn immediately_called_returned_function_has_callee_fact() {
    let result = check("fn tick() {} fn make() -> fn() { tick } fn main() { make()(); }")
        .expect("typecheck failed");
    let body = result.expect_body(&result.function_body("main"));
    let call = body
        .function_value_calls
        .values()
        .next()
        .expect("missing function-value call fact");

    assert!(body.function_values.contains_key(&call.callee));
    assert!(matches!(
        body.function_values[&call.callee].kind,
        FunctionValueKind::Storage(FunctionValueOrigin::CallReturn)
    ));
    assert!(!body.calls.contains_key(&call.expr));
}

#[test]
fn branch_joined_function_value_is_local_or_place_fact() {
    let result = check(
        r"
fn a() {}
fn b() {}
fn main(cond: bool) {
    let f: fn() = if cond { a } else { b };
    f();
}
",
    )
    .expect("typecheck failed");

    let body = result.expect_body(&result.function_body("main"));
    let call = body
        .function_value_calls
        .values()
        .next()
        .expect("missing function-value call fact");
    assert!(matches!(
        body.function_values
            .get(&call.callee)
            .expect("missing callee function-value fact")
            .kind,
        FunctionValueKind::Storage(FunctionValueOrigin::KnownLocal)
    ));
    assert!(!body.calls.contains_key(&call.expr));
}

#[test]
fn records_stringify_fact() {
    let result = check("fn f() { let s: string = #stringify(1); }").expect("typecheck failed");
    let body = result.expect_body(&result.function_body("f"));

    assert_eq!(body.stringifies.len(), 1);
    let (&call, fact) = body
        .stringifies
        .iter()
        .next()
        .expect("missing stringify fact");
    assert!(body.expr_types.contains_key(&call));
    assert_eq!(fact.source_ty, Type::Int);
    assert!(body.expr_types.contains_key(&fact.arg));
}

fn generic_args(type_args: Vec<Type>) -> GenericArgs {
    GenericArgs {
        type_args,
        const_args: vec![],
    }
}

#[test]
fn records_body_qualified_generic_stringify_facts() {
    let result = check(
        r#"
fn f<T>(x: T) { let s = #stringify(x); }
fn main() { f(1); f("x"); }
"#,
    )
    .expect("typecheck failed");

    let int_body = result.expect_body(&generic_body("f", vec![Type::Int]));
    let string_body = result.expect_body(&generic_body("f", vec![Type::String]));
    let int_fact = int_body
        .stringifies
        .values()
        .next()
        .expect("missing int stringify fact");
    let string_fact = string_body
        .stringifies
        .values()
        .next()
        .expect("missing string stringify fact");

    assert_eq!(int_body.stringifies.len(), 1);
    assert_eq!(string_body.stringifies.len(), 1);
    assert_eq!(int_fact.source_ty, Type::Int);
    assert_eq!(string_fact.source_ty, Type::String);
}

#[test]
fn records_stringify_inside_lambda() {
    let result =
        check("fn f() { let cb: fn() -> string = || #stringify(1); }").expect("typecheck failed");
    let lambda = result
        .lambda_escapes()
        .keys()
        .next()
        .copied()
        .expect("missing lambda fact");
    let body = result.expect_body(&BodyInstanceKey::Lambda(LambdaBodyKey {
        expr: lambda,
        specialization: GenericArgs::default(),
    }));

    assert_eq!(body.stringifies.len(), 1);
    assert_eq!(
        body.stringifies
            .values()
            .next()
            .expect("missing stringify fact")
            .source_ty,
        Type::Int
    );
}

#[test]
fn records_distinct_lambda_body_facts() {
    let result = check("fn f() { let a: fn() -> int = || 1; let b: fn() -> int = || 2; }")
        .expect("typecheck failed");
    let lambda_bodies = result
        .lambda_escapes()
        .keys()
        .map(|expr| {
            result.expect_body(&BodyInstanceKey::Lambda(LambdaBodyKey {
                expr: *expr,
                specialization: GenericArgs::default(),
            }))
        })
        .collect::<Vec<_>>();

    assert_eq!(lambda_bodies.len(), 2);
    assert!(lambda_bodies.iter().all(|body| !body.expr_types.is_empty()));
}

#[test]
fn generic_lambda_body_records_specialized_facts() {
    let result = check(
        r#"
fn f<T>(x: T) { let cb: fn() -> string = || #stringify(x); }
fn main() { f(1); f("x"); }
"#,
    )
    .expect("typecheck failed");
    let lambda = result
        .lambda_escapes()
        .keys()
        .next()
        .copied()
        .expect("missing lambda fact");
    let int_body = result.expect_body(&BodyInstanceKey::Lambda(LambdaBodyKey {
        expr: lambda,
        specialization: generic_args(vec![Type::Int]),
    }));
    let string_body = result.expect_body(&BodyInstanceKey::Lambda(LambdaBodyKey {
        expr: lambda,
        specialization: generic_args(vec![Type::String]),
    }));

    assert_eq!(
        int_body.stringifies.values().next().unwrap().source_ty,
        Type::Int
    );
    assert_eq!(
        string_body.stringifies.values().next().unwrap().source_ty,
        Type::String
    );
}

#[test]
fn generic_body_records_concrete_param_local() {
    let result = check("fn id<T>(x: T) -> T { x } fn main() { id(1); }").expect("typecheck failed");
    let body = result.expect_body(&generic_body("id", vec![Type::Int]));
    let locals = &body.locals;
    let local = locals.param_defs[&0];
    let def = &locals.defs[&local];

    assert_eq!(def.name.as_str(), "x");
    assert_eq!(def.kind, LocalDefKind::Parameter);
    assert_eq!(def.ty, Type::Int);
    assert!(
        locals
            .uses
            .values()
            .any(|fact| fact.local == local && fact.mode == LocalUseMode::Read)
    );
}

#[test]
fn generic_body_records_concrete_binding_local() {
    let result =
        check("fn f<T>(x: T) -> T { let y = x; y } fn main() { f(1); }").expect("typecheck failed");
    let body = result.expect_body(&generic_body("f", vec![Type::Int]));
    let y = body
        .locals
        .defs
        .values()
        .find(|fact| fact.name.as_str() == "y")
        .expect("missing generic binding fact");

    assert_eq!(y.kind, LocalDefKind::Binding);
    assert_eq!(y.ty, Type::Int);
    assert!(
        body.locals
            .binding_defs
            .values()
            .any(|&local| local == y.id)
    );
    assert!(
        body.locals
            .uses
            .values()
            .any(|fact| fact.local == y.id && fact.mode == LocalUseMode::Read)
    );
}

#[test]
fn repeated_generic_specialization_keeps_single_local_fact_body() {
    let result = check("fn f<T>(x: T) -> T { let y = x; y } fn main() { f(1); f(2); }")
        .expect("typecheck failed");
    let body = result.expect_body(&generic_body("f", vec![Type::Int]));

    assert_eq!(body.locals.param_defs.len(), 1);
    assert_eq!(body.locals.binding_defs.len(), 1);
    assert_eq!(body.locals.defs.len(), 2);
    assert!(body.locals.defs.values().all(|fact| fact.ty == Type::Int));
}

#[test]
fn generic_function_facts_are_concrete_and_ordered() {
    let result = check(
        r#"
fn id<T>(x: T) -> T { x }
fn main() { id("x"); id(1); id(2); }
"#,
    )
    .expect("typecheck failed");
    let id_facts = result
        .function_facts()
        .iter()
        .filter(|fact| fact.name == Ident::new("id"))
        .collect::<Vec<_>>();

    assert_eq!(id_facts.len(), 2);
    assert_eq!(id_facts[0].args.type_args, vec![Type::Int]);
    assert_eq!(id_facts[1].args.type_args, vec![Type::String]);
    for fact in id_facts {
        let ty = fact.args.type_args[0].clone();
        assert_eq!(fact.params.len(), 1);
        assert_eq!(fact.params[0].name.as_str(), "x");
        assert_eq!(fact.params[0].ty, ty);
        assert_eq!(fact.ret.ty, ty);
        let body = result.expect_body(&fact.body);
        let local = body.locals.param_defs[&0];
        assert_eq!(body.locals.defs[&local].ty, fact.params[0].ty);
    }
}

#[test]
fn uncalled_generic_function_has_no_fact() {
    let result = check("fn id<T>(x: T) -> T { x } fn main() {}").expect("typecheck failed");

    assert!(
        result
            .function_facts()
            .iter()
            .all(|fact| fact.name != Ident::new("id"))
    );
}

#[test]
fn generic_inferred_return_function_fact_is_concrete() {
    let result = check("fn id<T>(x: T) -> _ { x } fn main() { id(1); }").expect("typecheck failed");
    let fact = result
        .function_facts()
        .iter()
        .find(|fact| fact.name == Ident::new("id"))
        .expect("missing generic function fact");

    assert_eq!(fact.args.type_args, vec![Type::Int]);
    assert_eq!(fact.ret.ty, Type::Int);
}

#[test]
fn const_generic_function_fact_uses_concrete_array_len() {
    let result = check(
        "const CAP = 3; fn id<T, N: int>(x: [T; N]) -> [T; N] { x } fn main(x: [int; CAP]) { id<int, CAP>(x); }",
    )
    .expect("typecheck failed");
    let fact = result
        .function_facts()
        .iter()
        .find(|fact| fact.name == Ident::new("id"))
        .expect("missing generic function fact");
    let expected = Type::Array {
        elem: Box::new(Type::Int),
        len: ArrayLen::Fixed(3),
    };

    assert_eq!(fact.args.const_args.len(), 1);
    assert_eq!(fact.params[0].ty, expected);
    assert_eq!(fact.ret.ty, expected);
}

#[test]
fn generic_function_fact_order_distinguishes_same_named_types() {
    let result = check_named(
        r"
import a;
import b;
fn id<T>(x: T) -> T { x }
fn main(a_id: a.Id, b_id: b.Id) { id(b_id); id(a_id); }
",
        &[("a", "pub struct Id {}"), ("b", "pub struct Id {}")],
    )
    .expect("typecheck failed");
    let id_facts = result
        .function_facts()
        .iter()
        .filter(|fact| fact.name == Ident::new("id"))
        .collect::<Vec<_>>();

    assert_eq!(id_facts.len(), 2);
    let origins = id_facts
        .iter()
        .map(|fact| match &fact.args.type_args[0] {
            Type::Nominal(nominal) => nominal.origin.clone(),
            ty => panic!("expected nominal type, found {ty:?}"),
        })
        .collect::<Vec<_>>();
    assert_ne!(origins[0], origins[1]);
    assert!(format!("{:?}", origins[0]).contains('a'));
    assert!(format!("{:?}", origins[1]).contains('b'));
}

#[test]
fn generic_function_fact_order_distinguishes_function_type_flags() {
    let result = check(
        r"
fn id<T>(x: escaping T) -> T { x }
fn plain(x: int) -> int { x }
fn borrowed(var x: int) -> var int { x }
fn main(var x: int) {
    let plain_fn: fn(int) -> int = plain;
    let borrowed_fn: fn(var int) -> var int = borrowed;
    id(borrowed_fn);
    id(plain_fn);
}
",
    )
    .expect("typecheck failed");
    let id_facts = result
        .function_facts()
        .iter()
        .filter(|fact| fact.name == Ident::new("id"))
        .collect::<Vec<_>>();

    assert_eq!(id_facts.len(), 2);
    let type_args = id_facts
        .iter()
        .map(|fact| fact.args.type_args[0].clone())
        .collect::<Vec<_>>();
    assert_ne!(type_args[0], type_args[1]);
    assert!(
        matches!(&type_args[0], Type::Func { params, ret } if !params[0].mutable && ret.access == crate::ast::ReturnAccess::Value)
    );
    assert!(
        matches!(&type_args[1], Type::Func { params, ret } if params[0].mutable && ret.access == crate::ast::ReturnAccess::Place)
    );
}
