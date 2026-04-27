use crate::{
    ast::Type,
    lexer::tokenize,
    parser,
    resolve::{ModuleKey, ModulePath, ResolveResult, ResolvedModule},
    typecheck::{self, TypeError},
};

pub(crate) fn assert_no_infer_vars_in_result(result: &typecheck::TypecheckResult) {
    for ty in result.types().map(|(_, (_, ty))| ty) {
        assert!(
            !typecheck::type_contains_infer(ty),
            "result contains inferred type: {ty:?}"
        );
    }
    for target in result.calls().values() {
        assert!(
            !typecheck::call_target_contains_infer(target),
            "call target contains inferred type: {target:?}"
        );
    }
}

fn parse(source: &str) -> crate::ast::Program {
    let tokens = tokenize(source).expect("lexer error");
    parser::parse_ast(&tokens).expect("parse error")
}

pub(crate) fn typecheck(source: &str) -> Result<typecheck::TypecheckResult, Vec<TypeError>> {
    let program = parse(source);
    typecheck::check(&program)
}

pub(crate) fn errors(source: &str) -> Vec<TypeError> {
    match typecheck(source) {
        Ok(_) => panic!("expected typecheck errors in: {source}"),
        Err(errors) => errors,
    }
}

pub(crate) fn assert_single_error(source: &str, matches: impl FnOnce(&TypeError) -> bool) {
    let errors = errors(source);
    assert_eq!(errors.len(), 1);
    assert!(matches(&errors[0]), "unexpected error: {:?}", errors[0]);
}

pub(crate) fn typecheck_with_modules(
    root_source: &str,
    dep_source: &str,
) -> Result<typecheck::TypecheckResult, Vec<TypeError>> {
    typecheck_with_named_modules(root_source, &[("gamekit", dep_source)])
}

pub(crate) fn typecheck_with_named_modules(
    root_source: &str,
    modules: &[(&str, &str)],
) -> Result<typecheck::TypecheckResult, Vec<TypeError>> {
    let root = parse(root_source);
    let resolved = ResolveResult {
        module_groups: vec![
            modules
                .iter()
                .map(|(name, source)| ResolvedModule {
                    key: ModuleKey::Named(ModulePath::new(vec![(*name).to_string()])),
                    program: parse(source),
                })
                .collect(),
        ],
    };
    typecheck::check_with_modules(&root, &resolved)
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

pub(crate) fn expr_type(source: &str) -> Type {
    let result = typecheck(source).expect("typecheck failed");
    last_expr_type(&result).unwrap_or(Type::Void)
}

pub(crate) fn assert_type(source: &str, expected: Type) {
    let ty = expr_type(source);
    assert_eq!(ty, expected, "source: {source}");
}

pub(crate) fn assert_err(source: &str) {
    let result = typecheck(source);
    assert!(result.is_err(), "expected error but got Ok in: {source}");
}

pub(crate) fn assert_err_count(source: &str, count: usize) {
    match typecheck(source) {
        Ok(_) if count == 0 => {}
        Ok(_) => panic!("expected {count} errors in: {source}, got Ok"),
        Err(errors) => assert_eq!(
            errors.len(),
            count,
            "expected {count} errors in: {source}, got {:?}",
            errors
        ),
    }
}

pub(crate) fn assert_type_with_modules(root: &str, dep: &str, expected: Type) {
    assert_type_with_named_modules(root, &[("gamekit", dep)], expected);
}

pub(crate) fn assert_type_with_named_modules(root: &str, modules: &[(&str, &str)], expected: Type) {
    let result = typecheck_with_named_modules(root, modules).expect("typecheck failed");
    let ty = last_expr_type(&result).unwrap_or(Type::Void);
    assert_eq!(ty, expected, "root: {root}");
}

pub(crate) fn assert_calls(source: &str, count: usize) {
    let result = typecheck(source).expect("typecheck failed");
    assert_eq!(
        result.calls().len(),
        count,
        "expected {count} call targets in: {source}"
    );
}

pub(crate) fn assert_calls_with_modules(root: &str, dep: &str, count: usize) {
    let result = typecheck_with_modules(root, dep).expect("typecheck failed");
    assert_eq!(
        result.calls().len(),
        count,
        "expected {count} call targets in: {root}"
    );
}
