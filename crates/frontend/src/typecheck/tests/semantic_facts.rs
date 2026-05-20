use super::support::check;
use crate::typecheck::{LocalDefKind, LocalUseMode};

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
