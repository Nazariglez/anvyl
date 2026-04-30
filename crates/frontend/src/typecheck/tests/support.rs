use std::collections::HashSet;

use crate::{
    ast::Type,
    externs::RawExterns,
    lexer::tokenize,
    parser,
    resolve::{ModuleKey, ModulePath, ResolveResult, ResolvedModule},
    typecheck::{self, ModuleScope, TypeError},
};

pub(crate) fn assert_typecheck_closed(result: &typecheck::TypecheckResult) {
    for ty in result.types().map(|(_, (_, ty))| ty) {
        assert!(
            !typecheck::type_contains_infer(ty),
            "result contains inferred type: {ty:?}"
        );
        assert!(
            !typecheck::type_contains_unresolved_ref(ty),
            "result contains unresolved type ref: {ty:?}"
        );
    }
    for target in result.calls().values() {
        assert!(
            !typecheck::call_target_contains_infer(target),
            "call target contains inferred type: {target:?}"
        );
        assert!(
            !typecheck::call_target_contains_unresolved_ref(target),
            "call target contains unresolved type ref: {target:?}"
        );
    }
}

fn parse(source: &str) -> crate::ast::Program {
    let tokens = tokenize(source).expect("lexer error");
    parser::parse_ast(&tokens).expect("parse error")
}

pub(crate) fn check(source: &str) -> Result<typecheck::TypecheckResult, Vec<TypeError>> {
    let program = parse(source);
    let resolved = ResolveResult {
        module_groups: vec![],
    };
    typecheck::check_with_modules(&program, &resolved, HashSet::new(), RawExterns::default())
}

pub(crate) fn errors(source: &str) -> Vec<TypeError> {
    match check(source) {
        Ok(_) => panic!("expected typecheck errors in: {source}"),
        Err(errors) => errors,
    }
}

pub(crate) fn assert_single_error(source: &str, matches: impl FnOnce(&TypeError) -> bool) {
    let errors = errors(source);
    assert_eq!(errors.len(), 1);
    assert!(matches(&errors[0]), "unexpected error: {:?}", errors[0]);
}

pub(crate) fn check_mods(
    root_source: &str,
    dep_source: &str,
) -> Result<typecheck::TypecheckResult, Vec<TypeError>> {
    check_named(root_source, &[("gamekit", dep_source)])
}

pub(crate) fn check_named(
    root_source: &str,
    modules: &[(&str, &str)],
) -> Result<typecheck::TypecheckResult, Vec<TypeError>> {
    check_with_active(root_source, modules, &[])
}

fn module_path(name: &str) -> ModulePath {
    ModulePath::new(name.split('.').map(str::to_string).collect()).unwrap()
}

fn resolved_modules(modules: &[(&str, &str)]) -> ResolveResult {
    ResolveResult {
        module_groups: vec![
            modules
                .iter()
                .map(|(name, source)| ResolvedModule {
                    key: ModuleKey::Named(module_path(name)),
                    program: parse(source),
                })
                .collect(),
        ],
    }
}

pub(crate) fn check_active(
    root_source: &str,
    modules: &[(&str, &str)],
    always_active: &[&str],
) -> Result<typecheck::TypecheckResult, Vec<TypeError>> {
    check_with_active(root_source, modules, always_active)
}

fn check_with_active(
    root_source: &str,
    modules: &[(&str, &str)],
    always_active: &[&str],
) -> Result<typecheck::TypecheckResult, Vec<TypeError>> {
    let root = parse(root_source);
    let resolved = resolved_modules(modules);
    let always_active_modules = always_active
        .iter()
        .map(|name| ModuleScope::Named(module_path(name)))
        .collect();
    typecheck::check_with_modules(
        &root,
        &resolved,
        always_active_modules,
        RawExterns::default(),
    )
}

fn last_expr_type(result: &typecheck::TypecheckResult) -> Option<Type> {
    result
        .types()
        .max_by(|(_, (left, _)), (_, (right, _))| {
            left.end
                .cmp(&right.end)
                .then_with(|| right.start.cmp(&left.start))
        })
        .map(|(_, (_, ty))| ty.clone())
}

pub(crate) fn ty_of(source: &str) -> Type {
    let result = check(source).expect("typecheck failed");
    last_expr_type(&result).unwrap_or(Type::Void)
}

pub(crate) fn assert_ty(source: &str, expected: Type) {
    let ty = ty_of(source);
    assert_eq!(ty, expected, "source: {source}");
}

pub(crate) fn assert_err(source: &str) {
    let result = check(source);
    assert!(result.is_err(), "expected error but got Ok in: {source}");
}

pub(crate) fn assert_err_count(source: &str, count: usize) {
    match check(source) {
        Ok(_) if count == 0 => {}
        Ok(_) => panic!("expected {count} errors in: {source}, got Ok"),
        Err(errors) => assert_eq!(
            errors.len(),
            count,
            "expected {count} errors in: {source}, got {errors:?}"
        ),
    }
}

pub(crate) fn assert_ty_mods(root: &str, dep: &str, expected: Type) {
    assert_ty_named(root, &[("gamekit", dep)], expected);
}

pub(crate) fn assert_ty_named(root: &str, modules: &[(&str, &str)], expected: Type) {
    let result = check_named(root, modules).expect("typecheck failed");
    let ty = last_expr_type(&result).unwrap_or(Type::Void);
    assert_eq!(ty, expected, "root: {root}");
}

pub(crate) fn assert_calls(source: &str, count: usize) {
    let result = check(source).expect("typecheck failed");
    assert_eq!(
        result.calls().len(),
        count,
        "expected {count} call targets in: {source}"
    );
}

pub(crate) fn assert_calls_with_modules(root: &str, dep: &str, count: usize) {
    let result = check_mods(root, dep).expect("typecheck failed");
    assert_eq!(
        result.calls().len(),
        count,
        "expected {count} call targets in: {root}"
    );
}
