use super::{
    DeprecatedUseKind, FieldSchema, GenericArgs, GenericParams, ResolvedNominal, TypeChecker,
    TypeError, VariantShape,
    annotation::{AccessPolicy, deprecated_lint},
    const_term::ConstTerm,
    decls::{
        CallableDef, CallableId, CallableRef, CallableSig, NamedSchemas, NominalKey,
        VariantPayload, VariantSchema, nominal_generic_args, nominal_type, nominal_type_with_args,
        owner_template,
    },
    generic_bind::GenericSolveSession,
    infer::GenericSolverSeeds,
};
use crate::{
    ast::{FuncParam, Ident, NominalKind, ReturnSpec, Type},
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

    pub(super) fn owner_args_from_type(&self, ty: &Type, tc: &TypeChecker) -> Option<GenericArgs> {
        if tc.decls.key_for_type(ty).as_ref() != Some(&self.key) {
            return None;
        }
        let args = nominal_generic_args(ty)?;
        let has_type_args = args.type_args.len() == self.generics.type_params.len();
        let has_const_args = args.const_args.len() == self.generics.const_params.len();
        (has_type_args && has_const_args).then_some(args)
    }

    fn owner_ty_from_args(&self, args: &GenericArgs) -> Option<Type> {
        let const_args = ConstTerm::to_args_no_infer(&args.const_args)?;
        Some(nominal_type_with_args(
            &self.key,
            &args.type_args,
            &const_args,
        ))
    }
}

pub(super) enum VariantPayloadRef<'a> {
    Unit,
    Tuple(&'a [Type]),
    Struct(&'a NamedSchemas<FieldSchema>),
}

pub(super) fn expect_shape<'a>(
    tc: &mut TypeChecker,
    resolved: &'a ResolvedEnumVariant,
    expected: VariantShape,
    span: Span,
) -> Option<VariantPayloadRef<'a>> {
    let payload = match (&resolved.schema.payload, expected) {
        (VariantPayload::Unit, VariantShape::Unit) => VariantPayloadRef::Unit,
        (VariantPayload::Tuple(types), VariantShape::Tuple) => VariantPayloadRef::Tuple(types),
        (VariantPayload::Struct(fields), VariantShape::Struct) => VariantPayloadRef::Struct(fields),
        _ => {
            push_shape_mismatch(tc, resolved, expected, span);
            return None;
        }
    };
    Some(payload)
}

pub(super) fn expect_unit(
    tc: &mut TypeChecker,
    resolved: &ResolvedEnumVariant,
    span: Span,
) -> bool {
    expect_shape(tc, resolved, VariantShape::Unit, span).is_some()
}

pub(super) fn expect_tuple<'a>(
    tc: &mut TypeChecker,
    resolved: &'a ResolvedEnumVariant,
    span: Span,
) -> Option<&'a [Type]> {
    match expect_shape(tc, resolved, VariantShape::Tuple, span) {
        Some(VariantPayloadRef::Tuple(types)) => Some(types),
        _ => None,
    }
}

pub(super) fn tuple_callable_ref(
    resolved: &ResolvedEnumVariant,
    payload_types: &[Type],
    owner_args: GenericArgs,
) -> CallableRef {
    let params = payload_types
        .iter()
        .cloned()
        .map(FuncParam::immut)
        .collect();
    let ret = owner_template(&resolved.key, &resolved.generics);

    CallableRef {
        def: CallableDef {
            id: CallableId::enum_variant(resolved.key.clone(), resolved.variant),
            sig: CallableSig {
                owner_generics: resolved.generics.clone(),
                generics: GenericParams::default(),
                params,
                default_sites: vec![],
                required_params: payload_types.len(),
                ret: ReturnSpec::value(ret),
            },
        },
        receiver_ty: None,
        owner_args,
        is_stringify_override: false,
    }
}

pub(super) fn expect_struct<'a>(
    tc: &mut TypeChecker,
    resolved: &'a ResolvedEnumVariant,
    span: Span,
) -> Option<&'a NamedSchemas<FieldSchema>> {
    match expect_shape(tc, resolved, VariantShape::Struct, span) {
        Some(VariantPayloadRef::Struct(fields)) => Some(fields),
        _ => None,
    }
}

pub(super) fn solve_unit_owner_ty(
    tc: &mut TypeChecker,
    resolved: &ResolvedEnumVariant,
    explicit_args: Option<&GenericArgs>,
    expected: Option<&Type>,
    span: Span,
) -> Option<Type> {
    let seeds = explicit_args.map_or_else(GenericSolverSeeds::default, |args| {
        GenericSolverSeeds::from_args(&resolved.generics, args)
    });
    let session = GenericSolveSession::new(tc, &resolved.generics, &seeds, span);

    if let Some(expected) =
        expected.filter(|ty| tc.decls.key_for_type(ty).as_ref() == Some(&resolved.key))
    {
        let template = owner_template(&resolved.key, &resolved.generics);
        let template = tc
            .solver
            .instantiate_generic_type(&template, session.vars());
        let expected = TypeChecker::type_handle(expected);
        tc.expect_equal(span, template, expected);
    }
    if tc.solve_constraints() {
        return None;
    }
    let args = session.finish(tc)?;
    resolved.owner_ty_from_args(&args)
}

pub(super) fn resolve_use(
    tc: &mut TypeChecker,
    key: &NominalKey,
    variant: Ident,
    span: Span,
) -> Option<ResolvedEnumVariant> {
    let nominal = tc.resolve_nominal(&nominal_type(key))?;
    let variants = nominal.variants()?;
    let Some(variant_schema) = variants.get(variant).cloned() else {
        tc.push_error(TypeError::UnknownEnumVariant {
            enum_name: key.name,
            variant,
            span: tc.error_span(span),
        });
        return None;
    };
    let ResolvedNominal::Enum { schema, .. } = nominal else {
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
            variant_policy.deprecated_reason(),
            span,
        ));
    }
    enum_policy.has_deprecated().then(|| {
        deprecated_lint(
            DeprecatedUseKind::Enum,
            enum_name,
            enum_policy.deprecated_reason(),
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

fn push_shape_mismatch(
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
