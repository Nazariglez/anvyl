use std::collections::HashMap;

use crate::{
    ast::{ExprId, Ident, Type},
    externs::{self, RawExterns, catalog::ExternCatalog},
    span::Span,
    test_support::{empty_resolved, parse_program, resolved_modules},
    typecheck::{
        self, CallMap, DeprecatedUseKind, ExternUseMap, TypeError, TypeWarning,
        decls::DeclarationIndex,
    },
};

pub(crate) struct TypecheckTestResult {
    types: HashMap<ExprId, (Span, Type)>,
    calls: CallMap,
    extern_uses: ExternUseMap,
    warnings: Vec<TypeWarning>,
    decls: DeclarationIndex,
    externs: ExternCatalog,
}

impl TypecheckTestResult {
    pub(crate) fn types(&self) -> impl Iterator<Item = (&ExprId, &(Span, Type))> {
        self.types.iter()
    }

    pub(crate) fn calls(&self) -> &CallMap {
        &self.calls
    }

    pub(crate) fn extern_uses(&self) -> &ExternUseMap {
        &self.extern_uses
    }

    pub(crate) fn warnings(&self) -> &[TypeWarning] {
        &self.warnings
    }

    pub(crate) fn decls(&self) -> &DeclarationIndex {
        &self.decls
    }

    pub(crate) fn externs(&self) -> &ExternCatalog {
        &self.externs
    }
}

pub(crate) fn assert_deprecated_warning(
    result: &TypecheckTestResult,
    kind: DeprecatedUseKind,
    name: &str,
    reason: Option<&str>,
) {
    let warnings = result.warnings();
    assert_eq!(warnings.len(), 1);
    assert!(matches!(
        &warnings[0],
        TypeWarning::DeprecatedAccess {
            kind: warning_kind,
            name: warning_name,
            reason: warning_reason,
            ..
        } if *warning_kind == kind
            && *warning_name == Ident::new(name)
            && warning_reason.as_deref() == reason
    ));
}

pub(crate) fn assert_typecheck_closed(result: &TypecheckTestResult) {
    for ty in result.types().map(|(_, (_, ty))| ty) {
        assert_closed_type(ty, "result");
    }
    for target in result.calls().values() {
        let facts = typecheck::call_target_closure_facts(target);
        assert!(
            !facts.types.infer.contains_type,
            "call target contains inferred type: {target:?}"
        );
        assert!(
            facts.types.first_unresolved.is_none(),
            "call target contains unresolved type ref: {target:?}"
        );
        assert!(
            !facts.contains_unresolved_const(),
            "call target contains unresolved const: {target:?}"
        );
        assert!(
            !facts.consts.contains_infer,
            "call target contains inferred const: {target:?}"
        );
    }
    result.externs().for_each_resolved_ty(|ty, _| {
        assert_closed_type(&ty.ty, "extern");
    });
}

fn assert_closed_type(ty: &Type, label: &str) {
    let facts = typecheck::type_closure_facts(ty);
    assert!(
        !facts.infer.contains_type,
        "{label} type contains inferred type: {ty:?}"
    );
    assert!(
        facts.first_unresolved.is_none(),
        "{label} type contains unresolved type ref: {ty:?}"
    );
    assert!(
        !facts.contains_unresolved_const,
        "{label} type contains unresolved const: {ty:?}"
    );
}

pub(crate) fn check(source: &str) -> Result<TypecheckTestResult, Vec<TypeError>> {
    let program = parse_program(source);
    let resolved = empty_resolved();
    let raw_externs = externs::collect_source_externs(&program, &resolved).unwrap();
    check_with_raw_externs(&program, &resolved, raw_externs)
}

pub(crate) fn check_with_raw_externs(
    program: &crate::ast::Program,
    resolved: &crate::resolve::ResolveResult,
    raw_externs: RawExterns,
) -> Result<TypecheckTestResult, Vec<TypeError>> {
    let mut tc = typecheck::typechecker_for_modules(
        program,
        resolved,
        raw_externs,
        typecheck::TypecheckConfig::default(),
    )?;
    let types = tc.finish()?;
    Ok(TypecheckTestResult {
        types,
        calls: tc.calls,
        extern_uses: tc.extern_uses,
        warnings: tc.warnings,
        decls: tc.decls,
        externs: tc.externs,
    })
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
) -> Result<TypecheckTestResult, Vec<TypeError>> {
    check_named(root_source, &[("gamekit", dep_source)])
}

pub(crate) fn check_named(
    root_source: &str,
    modules: &[(&str, &str)],
) -> Result<TypecheckTestResult, Vec<TypeError>> {
    let root = parse_program(root_source);
    let resolved = resolved_modules(&root, modules);
    let raw_externs = externs::collect_source_externs(&root, &resolved).unwrap();
    check_with_raw_externs(&root, &resolved, raw_externs)
}

fn last_expr_type(result: &TypecheckTestResult) -> Option<Type> {
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
