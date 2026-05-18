use std::collections::{HashMap, HashSet};

use super::{
    MemberAccessKind, TypeChecker, TypeError,
    annotation::AccessPolicy,
    decls::{FieldSchema, NominalKey},
    member,
};
use crate::{
    ast::{Ident, Type},
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
    pub(super) ty: Type,
    pub(super) policy: AccessPolicy,
}

pub(super) enum FieldOwner {
    Nominal(Type),
    Variant { key: NominalKey, variant: Ident },
}

#[derive(Clone, Copy)]
pub(super) enum MissingFields {
    None,
    RequireAll,
    AllowDefaults,
    AllowRest { has_rest: bool },
}

#[derive(Default)]
pub(super) struct FieldShape {
    pub(super) fields: Vec<CheckedField>,
    pub(super) invalid_indices: Vec<usize>,
    pub(super) failed: bool,
}

pub(super) fn check_named<T>(
    fields: &[(Ident, T)],
    schema: &HashMap<Ident, FieldSchema>,
    owner: &FieldOwner,
    missing: MissingFields,
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
    schema: &HashMap<Ident, FieldSchema>,
    owner: &FieldOwner,
    missing: MissingFields,
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
        match schema.get(&field.name) {
            Some(schema) => fields.push(CheckedField {
                name: field.name,
                index: field.index,
                ty: schema.ty.clone(),
                policy: schema.policy.clone(),
            }),
            None => {
                invalid_indices.push(field.index);
                push_unknown(owner, field.name, field.span, tc);
                failed = true;
            }
        }
    }

    if missing_fields_enabled(missing) {
        for (name, field) in schema {
            if seen.contains(name) || missing_default_ok(missing, field) {
                continue;
            }
            let Some(span) = span else {
                continue;
            };
            push_missing(owner, *name, span, tc);
            failed = true;
        }
    }

    FieldShape {
        fields,
        invalid_indices,
        failed,
    }
}

fn missing_fields_enabled(missing: MissingFields) -> bool {
    match missing {
        MissingFields::None => false,
        MissingFields::RequireAll | MissingFields::AllowDefaults => true,
        MissingFields::AllowRest { has_rest } => !has_rest,
    }
}

fn missing_default_ok(missing: MissingFields, field: &FieldSchema) -> bool {
    matches!(missing, MissingFields::AllowDefaults) && field.has_default
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
