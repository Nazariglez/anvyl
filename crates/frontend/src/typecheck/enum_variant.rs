use super::{
    DeprecatedUseKind, GenericArgs, GenericParams, TypeChecker, TypeError, VariantShape,
    annotation::AccessPolicy,
    decls::{NominalKey, VariantSchema, nominal_generic_args, nominal_type},
    deprecated_lint,
};
use crate::{
    ast::{Ident, NominalKind, Type},
    span::Span,
};

#[derive(Clone)]
pub(super) struct ResolvedEnumVariant {
    pub(super) key: NominalKey,
    pub(super) variant: Ident,
    pub(super) generics: GenericParams,
    pub(super) schema: VariantSchema,
}

impl ResolvedEnumVariant {
    pub(super) fn owner_ty(&self) -> Type {
        nominal_type(&self.key)
    }

    pub(super) fn owner_args_from_expected(
        &self,
        expected: &Type,
        tc: &TypeChecker,
    ) -> Option<GenericArgs> {
        if tc.decls.key_for_type(expected).as_ref() != Some(&self.key) {
            return None;
        }
        nominal_generic_args(expected)
    }
}

pub(super) fn resolve_use(
    tc: &mut TypeChecker,
    key: &NominalKey,
    variant: Ident,
    span: Span,
) -> Option<ResolvedEnumVariant> {
    let schema = tc.decls.enum_schema(key)?;
    let Some(variant_schema) = schema.variants.get(&variant).cloned() else {
        tc.push_error(TypeError::UnknownEnumVariant {
            enum_name: key.name,
            variant,
            span: tc.error_span(span),
        });
        return None;
    };
    let generics = schema.generics.clone();
    let warning = resolved_use_lint(
        key.name,
        variant,
        &schema.policy,
        &variant_schema.policy,
        tc.source_span(span),
    );
    if let Some(event) = warning {
        tc.push_lint_event(event);
    }
    Some(ResolvedEnumVariant {
        key: key.clone(),
        variant,
        generics,
        schema: variant_schema,
    })
}

fn resolved_use_lint(
    enum_name: Ident,
    variant: Ident,
    enum_policy: &AccessPolicy,
    variant_policy: &AccessPolicy,
    span: crate::span::SourceSpan,
) -> Option<crate::lint::LintEvent> {
    if variant_policy.has_deprecated() {
        return Some(deprecated_lint(
            DeprecatedUseKind::EnumVariant,
            variant,
            variant_policy.deprecated_reason().map(str::to_string),
            span,
        ));
    }
    enum_policy.has_deprecated().then(|| {
        deprecated_lint(
            DeprecatedUseKind::Enum,
            enum_name,
            enum_policy.deprecated_reason().map(str::to_string),
            span,
        )
    })
}

pub(super) fn resolve_pattern(
    tc: &mut TypeChecker,
    qualifier: Option<Ident>,
    variant: Ident,
    span: Span,
    expected: &Type,
) -> Option<ResolvedEnumVariant> {
    let expected_key = tc
        .decls
        .key_for_type(expected)
        .filter(|key| key.kind == NominalKind::Enum);
    let key = match qualifier {
        Some(name) => resolve_explicit_pattern(tc, name, expected_key.as_ref(), expected, span)?,
        None => resolve_inferred_pattern(tc, expected_key, span)?,
    };
    resolve_use(tc, &key, variant, span)
}

fn resolve_explicit_pattern(
    tc: &mut TypeChecker,
    name: Ident,
    expected_key: Option<&NominalKey>,
    expected: &Type,
    span: Span,
) -> Option<NominalKey> {
    let visible_ty = tc.visible_type_subject(name, span);
    let visible = visible_ty
        .as_ref()
        .and_then(|ty| tc.decls.key_for_type(ty))
        .filter(|key| key.kind == NominalKind::Enum);
    if let Some(visible) = visible {
        if let Some(expected_key) = expected_key {
            if visible != *expected_key {
                tc.push_error(TypeError::EnumPatternTypeMismatch {
                    expected: nominal_type(expected_key),
                    found: nominal_type(&visible),
                    span: tc.error_span(span),
                });
                return None;
            }
            return Some(visible);
        }
        if matches!(expected, Type::Infer) {
            return Some(visible);
        }
        tc.push_error(TypeError::EnumPatternTypeMismatch {
            expected: expected.clone(),
            found: nominal_type(&visible),
            span: tc.error_span(span),
        });
        return None;
    }

    if let Some(key) = expected_key.filter(|key| key.name == name) {
        return Some(key.clone());
    }

    tc.push_error(TypeError::UnknownType {
        qualifier: None,
        name,
        span: tc.error_span(span),
    });
    None
}

fn resolve_inferred_pattern(
    tc: &mut TypeChecker,
    expected_key: Option<NominalKey>,
    span: Span,
) -> Option<NominalKey> {
    match expected_key {
        Some(key) => Some(key),
        None => {
            tc.push_error(TypeError::CannotInferEnum {
                span: tc.error_span(span),
            });
            None
        }
    }
}

pub(super) fn push_shape_mismatch(
    tc: &mut TypeChecker,
    resolved: &ResolvedEnumVariant,
    expected: VariantShape,
    span: Span,
) {
    tc.push_error(TypeError::EnumVariantShapeMismatch {
        enum_name: resolved.key.name,
        variant: resolved.variant,
        expected,
        span: tc.error_span(span),
    });
}

pub(super) fn push_arg_count_mismatch(
    tc: &mut TypeChecker,
    enum_name: Ident,
    variant: Ident,
    expected: usize,
    found: usize,
    span: Span,
) {
    tc.push_error(TypeError::EnumVariantArgCount {
        enum_name,
        variant,
        expected,
        found,
        span: tc.error_span(span),
    });
}
