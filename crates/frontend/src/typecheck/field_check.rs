use std::collections::HashSet;

use super::{
    MemberAccessKind, TypeChecker, TypeError,
    annotation::AccessPolicy,
    decls::{FieldDefault, FieldSchema, NamedSchemas, NominalKey},
    member,
};
use crate::{
    ast::{Ident, Type},
    externs::catalog::{ExternField, ExternType},
    span::Span,
};

pub(super) struct FieldUse {
    pub(super) name: Ident,
    pub(super) span: Span,
    pub(super) index: usize,
}

pub(super) struct CheckedField {
    pub(super) name: Ident,
    pub(super) index: usize,
    pub(super) slot: usize,
    pub(super) ty: Type,
    pub(super) policy: AccessPolicy,
}

pub(super) struct OmittedDefaultField {
    pub(super) name: Ident,
    pub(super) slot: usize,
    pub(super) ty: Type,
    pub(super) default: FieldDefault,
}

pub(super) enum FieldOwner {
    Nominal(Type),
    Variant { key: NominalKey, variant: Ident },
}

pub(super) fn extern_field_schema(ty: &ExternType) -> NamedSchemas<FieldSchema> {
    extern_fields(ty, |_| true)
}

pub(super) fn extern_readable_field_schema(ty: &ExternType) -> NamedSchemas<FieldSchema> {
    extern_fields(ty, |field| field.readable)
}

fn extern_fields(
    ty: &ExternType,
    include: impl Fn(&ExternField) -> bool,
) -> NamedSchemas<FieldSchema> {
    let mut schema = NamedSchemas::default();
    for field in &ty.fields {
        if !include(field) {
            continue;
        }
        schema
            .insert(
                field.name,
                FieldSchema {
                    ty: field.ty.ty.clone(),
                    default: None,
                    policy: AccessPolicy::default(),
                    span: None,
                    embed: None,
                },
            )
            .expect("extern fields are unique");
    }
    schema
}

#[derive(Clone, Copy)]
pub(super) enum MissingFields<'a> {
    None,
    RequireAll,
    RequireOnly(&'a [Ident]),
    AllowDefaults,
    AllowRest { has_rest: bool },
}

#[derive(Default)]
pub(super) struct FieldShape {
    pub(super) fields: Vec<CheckedField>,
    pub(super) default_fields: Vec<OmittedDefaultField>,
    pub(super) invalid_indices: Vec<usize>,
    pub(super) failed: bool,
}

pub(super) fn check_named<T>(
    fields: &[(Ident, T)],
    schema: &NamedSchemas<FieldSchema>,
    owner: &FieldOwner,
    missing: MissingFields<'_>,
    span: Option<Span>,
    field_span: impl Fn(&T) -> Span,
    tc: &mut TypeChecker,
) -> FieldShape {
    let uses = fields
        .iter()
        .enumerate()
        .map(|(index, (name, field))| FieldUse {
            name: *name,
            span: field_span(field),
            index,
        })
        .collect::<Vec<_>>();
    check(&uses, schema, owner, missing, span, tc)
}

pub(super) fn check(
    uses: &[FieldUse],
    schema: &NamedSchemas<FieldSchema>,
    owner: &FieldOwner,
    missing: MissingFields<'_>,
    span: Option<Span>,
    tc: &mut TypeChecker,
) -> FieldShape {
    let mut seen = HashSet::new();
    let mut failed = false;
    let mut fields = Vec::with_capacity(uses.len());
    let mut invalid_indices = vec![];

    for field in uses {
        if !seen.insert(field.name) {
            tc.push_error(TypeError::DuplicateField {
                name: field.name,
                span: tc.error_span(field.span),
            });
            invalid_indices.push(field.index);
            failed = true;
            continue;
        }
        match schema.get(field.name) {
            Some(field_schema) => fields.push(CheckedField {
                name: field.name,
                index: field.index,
                slot: schema.slot(field.name).expect("field schema exists"),
                ty: field_schema.ty.clone(),
                policy: field_schema.policy.clone(),
            }),
            None => {
                invalid_indices.push(field.index);
                push_unknown(owner, field.name, field.span, tc);
                failed = true;
            }
        }
    }

    let mut default_fields = vec![];
    if let Some(span) = span {
        for (slot, (name, field)) in schema.iter().enumerate() {
            if seen.contains(&name) {
                continue;
            }
            match (missing, &field.default) {
                (MissingFields::AllowDefaults, Some(default)) => {
                    default_fields.push(OmittedDefaultField {
                        name,
                        slot,
                        ty: field.ty.clone(),
                        default: default.clone(),
                    });
                }
                _ if missing_required(missing, name, field) => {
                    push_missing(owner, name, span, tc);
                    failed = true;
                }
                _ => {}
            }
        }
    }

    FieldShape {
        fields,
        default_fields,
        invalid_indices,
        failed,
    }
}

fn missing_required(missing: MissingFields<'_>, name: Ident, field: &FieldSchema) -> bool {
    match missing {
        MissingFields::None => false,
        MissingFields::RequireAll => true,
        MissingFields::RequireOnly(required) => required.contains(&name),
        MissingFields::AllowDefaults => !field.has_default(),
        MissingFields::AllowRest { has_rest } => !has_rest,
    }
}

fn push_unknown(owner: &FieldOwner, name: Ident, span: Span, tc: &mut TypeChecker) {
    match owner {
        FieldOwner::Nominal(ty) => {
            if let Some(paths) = member::promoted_field_paths(ty, name, tc) {
                tc.push_error(TypeError::PromotedFieldNotStored {
                    ty: ty.clone(),
                    field: name,
                    paths,
                    span: tc.error_span(span),
                });
                return;
            }
            tc.push_error(TypeError::UnknownMember {
                ty: ty.clone(),
                member: name,
                kind: MemberAccessKind::Field,
                span: tc.error_span(span),
            });
        }
        FieldOwner::Variant { key, variant } => {
            tc.push_error(TypeError::UnknownVariantField {
                enum_name: key.name,
                variant: *variant,
                field: name,
                span: tc.error_span(span),
            });
        }
    }
}

fn push_missing(owner: &FieldOwner, name: Ident, span: Span, tc: &mut TypeChecker) {
    match owner {
        FieldOwner::Nominal(_) => tc.push_error(TypeError::MissingField {
            name,
            span: tc.error_span(span),
        }),
        FieldOwner::Variant { key, variant } => {
            tc.push_error(TypeError::MissingVariantField {
                enum_name: key.name,
                variant: *variant,
                field: name,
                span: tc.error_span(span),
            });
        }
    }
}
