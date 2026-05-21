use super::support::{check, generic_body};
use crate::{
    ast::{ArrayLen, Ident, Type},
    typecheck::{GenericArgs, LocalDefKind, LocalUseMode},
};

#[test]
fn records_params_bindings_and_uses() {
    let source = r#"
fn f(a: int, var b: int) -> int {
    let x = a;
    var y = x;
    y = b;
    y += 1;
    y
}
"#;
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
    assert_eq!(fact.params[1].name.as_str(), "message");
    assert_eq!(fact.params[1].ty, Type::String);
    assert_eq!(fact.return_ty, Type::Bool);
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
    let body = result.expect_body(&result.function_body("f"));

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
        assert_eq!(fact.return_ty, ty);
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
    assert_eq!(fact.return_ty, Type::Int);
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
    assert_eq!(fact.return_ty, expected);
}

#[test]
fn generic_function_fact_order_distinguishes_same_named_types() {
    let result = super::support::check_named(
        r#"
import a;
import b;
fn id<T>(x: T) -> T { x }
fn main(a_id: a.Id, b_id: b.Id) { id(b_id); id(a_id); }
"#,
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
