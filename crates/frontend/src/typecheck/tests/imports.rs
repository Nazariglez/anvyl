use super::support::{TypecheckTestResult, check_named};
use crate::{
    ast::{Ident, Type},
    resolve::ModulePath,
    typecheck::{
        CallForm, CallTarget, GenericArgs, TypeError,
        decls::{CallableId, DeclError, ExtendId, MethodSurface, ModuleScope},
    },
};

fn expect_errors(
    result: Result<TypecheckTestResult, Vec<TypeError>>,
    message: &str,
) -> Vec<TypeError> {
    let Err(errors) = result else {
        panic!("{message}");
    };
    errors
}

fn expect_single_error(
    result: Result<TypecheckTestResult, Vec<TypeError>>,
    matches: impl FnOnce(&TypeError) -> bool,
) {
    let errors = expect_errors(result, "expected one typecheck error");
    assert_eq!(errors.len(), 1, "unexpected errors: {errors:?}");
    assert!(matches(&errors[0]), "unexpected error: {:?}", errors[0]);
}

#[test]
fn reexport_alias_call_target() {
    let root = "
        import facade { dup };
        fn use_it() -> int { dup(1) }
    ";
    let modules = [
        ("tools", "pub fn id<T>(x: T) -> T { x }"),
        ("facade", "pub import tools { id as dup };"),
    ];
    let result = check_named(root, &modules).expect("typecheck failed");
    let target = result.calls().values().next().expect("missing call target");
    assert_id_target(target, "tools", "id");
}

#[test]
fn qualified_reexport_call_target() {
    let root = "
        import facade;
        fn use_it() -> int { facade.dup(1) }
    ";
    let modules = [
        ("tools", "pub fn id<T>(x: T) -> T { x }"),
        ("facade", "pub import tools { id as dup };"),
    ];
    let result = check_named(root, &modules).expect("typecheck failed");
    let target = result.calls().values().next().expect("missing call target");
    assert_id_target(target, "tools", "id");
}

#[test]
fn hidden_provider_conflict_stays_ambiguous() {
    let root = "
        import facade;
        fn use_it() -> int { 1.pick() }
    ";
    let modules = [
        ("facade", "pub import a { * }; pub import b { * };"),
        ("a", "pub extend int { fn pick(self) -> int { 1 } }"),
        ("b", "pub extend int { fn pick(self) -> int { 2 } }"),
    ];

    expect_single_error(
        check_named(root, &modules),
        |error| matches!(error, TypeError::AmbiguousExtendMethod { name, .. } if *name == Ident::new("pick")),
    );
}

#[test]
fn public_private_member_reports_once() {
    let root = "pub import tools { hidden };";
    let modules = [
        ("source", "fn hidden() -> int { 1 }"),
        ("tools", "pub import source { hidden };"),
    ];

    expect_single_error(
        check_named(root, &modules),
        |error| matches!(error, TypeError::Decl(DeclError::PrivateImportMember { name, .. }) if *name == Ident::new("hidden")),
    );
}

#[test]
fn qualified_extend_call_target_preserves_provider() {
    let root = "
        import facade;
        fn use_it() -> int { facade.a.pick(1) }
    ";
    let modules = [
        ("facade", "pub import a;"),
        ("a", "pub extend int { fn pick(self) -> int { 1 } }"),
    ];

    let result = check_named(root, &modules).expect("typecheck failed");
    let target = result.calls().values().next().expect("missing call target");
    assert_qualified_pick_target(target, "a");
}

#[test]
fn barrel_qualified_extend_call_target_preserves_provider() {
    let root = "
        import facade;
        fn use_it() -> int { facade.pick(1) }
    ";
    let modules = [
        ("facade", "pub import a { * };"),
        ("a", "pub extend int { fn pick(self) -> int { 1 } }"),
    ];

    let result = check_named(root, &modules).expect("typecheck failed");
    let target = result.calls().values().next().expect("missing call target");
    assert_qualified_pick_target(target, "a");
}

fn assert_id_target(target: &CallTarget, module: &str, name: &str) {
    assert_eq!(
        target,
        &CallTarget::new(
            CallableId::function(named_scope(module), Ident::new(name)),
            GenericArgs {
                type_args: vec![Type::Int],
                const_args: vec![],
            },
        )
    );
}

fn assert_qualified_pick_target(target: &CallTarget, module: &str) {
    assert_eq!(target.id, pick_id(module));
    assert_eq!(target.args, GenericArgs::default());
    assert!(matches!(target.form, CallForm::QualifiedExtend { .. }));
}

fn pick_id(module: &str) -> CallableId {
    CallableId::extend_method(
        ExtendId {
            module: named_scope(module),
            index: 0,
        },
        Ident::new("pick"),
        MethodSurface::Instance,
    )
}

fn named_scope(module: &str) -> ModuleScope {
    ModuleScope::Named(ModulePath::new(vec![module.to_string()]).unwrap())
}
