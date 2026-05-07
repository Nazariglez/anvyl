use std::collections::{HashMap, HashSet};

use super::{
    MemberAccessKind, TypeChecker, TypeError,
    annotation::AccessPolicy,
    decls::{FieldSchema, NominalKey},
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
    AllowDefaults,
    AllowRest { has_rest: bool },
}

#[derive(Default)]
pub(super) struct FieldShape {
    pub(super) fields: Vec<CheckedField>,
    pub(super) failed: bool,
}

pub(super) fn check(
    uses: &[FieldUse],
    schema: &HashMap<Ident, FieldSchema>,
    owner: &FieldOwner,
    missing: MissingFields,
    span: Span,
    tc: &mut TypeChecker,
) -> FieldShape {
    let mut seen = HashSet::new();
    let mut failed = false;
    let mut fields = Vec::with_capacity(uses.len());

    for field in uses {
        if !seen.insert(field.name) {
            tc.push_error(TypeError::DuplicateField {
                name: field.name,
                span: field.span,
            });
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
            push_missing(owner, *name, span, tc);
            failed = true;
        }
    }

    FieldShape { fields, failed }
}

fn missing_fields_enabled(missing: MissingFields) -> bool {
    match missing {
        MissingFields::None => false,
        MissingFields::AllowDefaults => true,
        MissingFields::AllowRest { has_rest } => !has_rest,
    }
}

fn missing_default_ok(missing: MissingFields, field: &FieldSchema) -> bool {
    matches!(missing, MissingFields::AllowDefaults) && field.has_default
}

fn push_unknown(owner: &FieldOwner, name: Ident, span: Span, tc: &mut TypeChecker) {
    match owner {
        FieldOwner::Nominal(ty) => tc.push_error(TypeError::UnknownMember {
            ty: ty.clone(),
            member: name,
            kind: MemberAccessKind::Field,
            span,
        }),
        FieldOwner::Variant { key, variant } => {
            tc.push_error(TypeError::UnknownVariantField {
                enum_name: key.name,
                variant: *variant,
                field: name,
                span,
            });
        }
    }
}

fn push_missing(owner: &FieldOwner, name: Ident, span: Span, tc: &mut TypeChecker) {
    match owner {
        FieldOwner::Nominal(_) => tc.push_error(TypeError::MissingField { name, span }),
        FieldOwner::Variant { key, variant } => {
            tc.push_error(TypeError::MissingVariantField {
                enum_name: key.name,
                variant: *variant,
                field: name,
                span,
            });
        }
    }
}
