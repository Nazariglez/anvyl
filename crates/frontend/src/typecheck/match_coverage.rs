use std::collections::HashSet;

use super::{
    TypeChecker, TypeError,
    decls::{NominalKey, VariantPayload, nominal_generic_args},
    pattern::PatternCheckResult,
    semantic_use::{CheckedEnumPayload, CheckedPattern},
    substitute, substitute_aggregate_member,
};
use crate::{
    ast::{Ident, NominalKind, Type},
    span::Span,
};

enum MatchSubject {
    Finite(Shape),
    Open,
    Unknown,
    Unsupported(Type),
}

#[derive(Clone)]
enum Shape {
    Empty,
    Bool,
    Enum {
        key: NominalKey,
        variants: Vec<Ident>,
    },
    Optional,
    Tuple(usize),
    Struct(Vec<usize>),
    Open,
}

pub(super) fn check(
    scrutinee: &Type,
    heads: &[PatternCheckResult],
    span: Span,
    tc: &mut TypeChecker,
) {
    let subject = classify(scrutinee, tc);
    if let MatchSubject::Unsupported(found) = &subject {
        tc.push_error(TypeError::UnsupportedMatchScrutinee {
            found: found.clone(),
            span: tc.error_span(span),
        });
        return;
    }
    if matches!(subject, MatchSubject::Unknown) || heads.iter().any(|head| head.outcome.had_error) {
        return;
    }

    let hint = match subject {
        MatchSubject::Finite(shape) => Some(shape),
        MatchSubject::Open => Some(Shape::Open),
        MatchSubject::Unknown | MatchSubject::Unsupported(_) => unreachable!(),
    };
    let rows = heads
        .iter()
        .map(|head| vec![head.checked.clone()])
        .collect();
    if !matrix_exhaustive(rows, hint, tc) {
        tc.push_error(TypeError::NonExhaustiveMatch {
            span: tc.error_span(span),
        });
    }
}

fn classify(scrutinee: &Type, tc: &TypeChecker) -> MatchSubject {
    if type_uninhabited(scrutinee, tc, &mut HashSet::new()) {
        return MatchSubject::Finite(Shape::Empty);
    }
    if tc.decls.semantic_option_inner(scrutinee).is_some() {
        return MatchSubject::Finite(Shape::Optional);
    }
    match scrutinee {
        Type::Infer => MatchSubject::Unknown,
        Type::Bool => MatchSubject::Finite(Shape::Bool),
        Type::Int | Type::Float | Type::String | Type::Char => MatchSubject::Open,
        Type::Tuple(fields) => MatchSubject::Finite(Shape::Tuple(fields.len())),
        Type::Optional { .. } => MatchSubject::Finite(Shape::Optional),
        ty => match tc
            .decls
            .key_for_type(ty)
            .filter(|key| key.kind == NominalKind::Enum)
        {
            Some(key) => {
                let schema = tc.decls.enum_schema(&key);
                if schema.is_some_and(|schema| schema.body.kind.flag().is_some()) {
                    MatchSubject::Open
                } else {
                    let variants = inhabited_variants(ty, &key, tc, &mut HashSet::new());
                    MatchSubject::Finite(Shape::Enum { key, variants })
                }
            }
            None => MatchSubject::Unsupported(ty.clone()),
        },
    }
}

fn type_uninhabited(ty: &Type, tc: &TypeChecker, seen: &mut HashSet<NominalKey>) -> bool {
    if tc.decls.semantic_option_inner(ty).is_some() {
        return false;
    }
    match ty {
        Type::Void => true,
        Type::Tuple(fields) => fields.iter().any(|field| type_uninhabited(field, tc, seen)),
        Type::Array { elem, len } => {
            matches!(len, crate::ast::ArrayLen::Fixed(len) if *len.value() > 0)
                && type_uninhabited(elem, tc, seen)
        }
        Type::Nominal(_) => {
            let Some(key) = tc.decls.key_for_type(ty) else {
                return false;
            };
            match key.kind {
                NominalKind::Enum => {
                    let is_flag = tc
                        .decls
                        .enum_schema(&key)
                        .is_some_and(|schema| schema.body.kind.flag().is_some());
                    !is_flag && inhabited_variants(ty, &key, tc, seen).is_empty()
                }
                NominalKind::Struct => {
                    if !seen.insert(key.clone()) {
                        return false;
                    }
                    let uninhabited = tc.decls.aggregate(&key).is_some_and(|aggregate| {
                        let generics = aggregate.all_generics();
                        aggregate.fields.values().any(|field| {
                            let field_ty = substitute_aggregate_member(ty, &generics, &field.ty);
                            type_uninhabited(&field_ty, tc, seen)
                        })
                    });
                    seen.remove(&key);
                    uninhabited
                }
                NominalKind::DataRef | NominalKind::Extern => false,
            }
        }
        Type::Infer
        | Type::InferReturn
        | Type::Any
        | Type::Int
        | Type::Float
        | Type::Bool
        | Type::String
        | Type::Char
        | Type::Func { .. }
        | Type::Dyn(_)
        | Type::Var(_)
        | Type::UnresolvedName(_)
        | Type::UnresolvedNominal { .. }
        | Type::List { .. }
        | Type::Map { .. }
        | Type::Slice { .. }
        | Type::Optional { .. } => false,
    }
}

fn inhabited_variants(
    owner: &Type,
    key: &NominalKey,
    tc: &TypeChecker,
    seen: &mut HashSet<NominalKey>,
) -> Vec<Ident> {
    let Some(schema) = tc.decls.enum_schema(key) else {
        return vec![];
    };
    if !seen.insert(key.clone()) {
        return schema.body.variants.names().collect();
    }
    let Some(args) = nominal_generic_args(owner) else {
        seen.remove(key);
        return schema.body.variants.names().collect();
    };
    let (type_subst, const_subst) = schema.all_generics().substitutions(&args);
    let variants = schema
        .body
        .variants
        .iter()
        .filter_map(|(name, variant)| {
            let inhabited = match &variant.payload {
                VariantPayload::Unit => true,
                VariantPayload::Tuple(fields) => fields.iter().all(|field| {
                    let field = substitute(field, &type_subst, &const_subst);
                    !type_uninhabited(&field, tc, seen)
                }),
                VariantPayload::Struct(fields) => fields.values().all(|field| {
                    let field = substitute(&field.ty, &type_subst, &const_subst);
                    !type_uninhabited(&field, tc, seen)
                }),
            };
            inhabited.then_some(name)
        })
        .collect();
    seen.remove(key);
    variants
}

fn matrix_exhaustive(
    mut rows: Vec<Vec<CheckedPattern>>,
    hint: Option<Shape>,
    tc: &TypeChecker,
) -> bool {
    if rows.is_empty() {
        return matches!(hint, Some(Shape::Empty))
            || matches!(hint, Some(Shape::Enum { ref variants, .. }) if variants.is_empty());
    }
    if rows[0].is_empty() {
        return true;
    }

    if rows.iter().all(|row| pattern_irrefutable(&row[0], tc)) {
        return matrix_exhaustive(rows.into_iter().map(drop_first).collect(), None, tc);
    }
    rows = expand_first_or(rows);

    let shape = hint
        .or_else(|| infer_shape(&rows, tc))
        .unwrap_or(Shape::Open);
    match shape {
        Shape::Empty => true,
        Shape::Bool => [false, true].into_iter().all(|value| {
            let rows = specialize(&rows, |pattern| bool_fields(pattern, value));
            matrix_exhaustive(rows, None, tc)
        }),
        Shape::Enum { key, variants } => variants.into_iter().all(|variant| {
            let field_shape = enum_field_shape(&rows, &key, variant);
            let rows = specialize(&rows, |pattern| {
                enum_fields(pattern, &key, variant, &field_shape)
            });
            matrix_exhaustive(rows, None, tc)
        }),
        Shape::Optional => [false, true].into_iter().all(|some| {
            let rows = specialize(&rows, |pattern| optional_fields(pattern, some));
            matrix_exhaustive(rows, None, tc)
        }),
        Shape::Tuple(len) => {
            let rows = specialize(&rows, |pattern| tuple_fields(pattern, len));
            matrix_exhaustive(rows, None, tc)
        }
        Shape::Struct(slots) => {
            let rows = specialize(&rows, |pattern| struct_fields(pattern, &slots));
            matrix_exhaustive(rows, None, tc)
        }
        Shape::Open => {
            let rows = specialize(&rows, |pattern| irrefutable(pattern).then(Vec::new));
            matrix_exhaustive(rows, None, tc)
        }
    }
}

fn expand_first_or(rows: Vec<Vec<CheckedPattern>>) -> Vec<Vec<CheckedPattern>> {
    let mut expanded = vec![];
    for row in rows {
        match &row[0] {
            CheckedPattern::Or(alternatives) => {
                for alternative in alternatives {
                    let mut branch = Vec::with_capacity(row.len());
                    branch.push(alternative.pattern.clone());
                    branch.extend_from_slice(&row[1..]);
                    expanded.push(branch);
                }
            }
            _ => expanded.push(row),
        }
    }
    expanded
}

fn drop_first(row: Vec<CheckedPattern>) -> Vec<CheckedPattern> {
    row.into_iter().skip(1).collect()
}

fn specialize(
    rows: &[Vec<CheckedPattern>],
    fields: impl Fn(&CheckedPattern) -> Option<Vec<CheckedPattern>>,
) -> Vec<Vec<CheckedPattern>> {
    rows.iter()
        .filter_map(|row| {
            let mut specialized = fields(&row[0])?;
            specialized.extend_from_slice(&row[1..]);
            Some(specialized)
        })
        .collect()
}

fn irrefutable(pattern: &CheckedPattern) -> bool {
    matches!(
        pattern,
        CheckedPattern::Wildcard | CheckedPattern::Binding(_)
    )
}

fn pattern_irrefutable(pattern: &CheckedPattern, tc: &TypeChecker) -> bool {
    if irrefutable(pattern) {
        return true;
    }
    let CheckedPattern::Or(alternatives) = pattern else {
        return false;
    };
    let rows = alternatives
        .iter()
        .map(|alternative| vec![alternative.pattern.clone()])
        .collect();
    matrix_exhaustive(rows, None, tc)
}

fn infer_shape(rows: &[Vec<CheckedPattern>], tc: &TypeChecker) -> Option<Shape> {
    for pattern in rows.iter().map(|row| &row[0]) {
        match pattern {
            CheckedPattern::Literal(literal) if literal.ty == Type::Bool => {
                return Some(Shape::Bool);
            }
            CheckedPattern::OptionalSome(_) | CheckedPattern::Nil => return Some(Shape::Optional),
            CheckedPattern::Tuple(fields) => return Some(Shape::Tuple(fields.len())),
            CheckedPattern::Struct { fields, .. } => {
                return Some(Shape::Struct(struct_slots(rows, fields)));
            }
            CheckedPattern::Enum { owner, .. } => {
                let variants = inhabited_variants(&owner.ty, &owner.key, tc, &mut HashSet::new());
                return Some(Shape::Enum {
                    key: owner.key.clone(),
                    variants,
                });
            }
            CheckedPattern::Literal(_)
            | CheckedPattern::FlagValue { .. }
            | CheckedPattern::Unsupported => return Some(Shape::Open),
            CheckedPattern::Wildcard | CheckedPattern::Binding(_) | CheckedPattern::Or(_) => {}
        }
    }
    None
}

fn bool_fields(pattern: &CheckedPattern, value: bool) -> Option<Vec<CheckedPattern>> {
    if irrefutable(pattern) {
        return Some(vec![]);
    }
    match pattern {
        CheckedPattern::Literal(literal) => {
            matches!(literal.value, crate::ast::ConstValue::Bool(found) if found == value)
                .then(Vec::new)
        }
        _ => None,
    }
}

#[derive(Clone)]
enum FieldShape {
    Unit,
    Tuple(usize),
    Struct(Vec<usize>),
}

impl FieldShape {
    fn len(&self) -> usize {
        match self {
            Self::Unit => 0,
            Self::Tuple(len) => *len,
            Self::Struct(slots) => slots.len(),
        }
    }
}

fn enum_field_shape(rows: &[Vec<CheckedPattern>], key: &NominalKey, variant: Ident) -> FieldShape {
    let mut shape = FieldShape::Unit;
    for pattern in rows.iter().map(|row| &row[0]) {
        let CheckedPattern::Enum {
            owner,
            variant: found,
            payload,
        } = pattern
        else {
            continue;
        };
        if &owner.key != key || *found != variant {
            continue;
        }
        match payload {
            CheckedEnumPayload::Unit => {}
            CheckedEnumPayload::Tuple(fields) => return FieldShape::Tuple(fields.len()),
            CheckedEnumPayload::Struct(fields) => {
                let fields = fields.iter().map(|field| field.slot);
                match &mut shape {
                    FieldShape::Struct(slots) => {
                        for slot in fields {
                            if !slots.contains(&slot) {
                                slots.push(slot);
                            }
                        }
                    }
                    _ => shape = FieldShape::Struct(fields.collect()),
                }
            }
        }
    }
    if let FieldShape::Struct(slots) = &mut shape {
        slots.sort_unstable();
    }
    shape
}

fn enum_fields(
    pattern: &CheckedPattern,
    key: &NominalKey,
    variant: Ident,
    shape: &FieldShape,
) -> Option<Vec<CheckedPattern>> {
    if irrefutable(pattern) {
        return Some(vec![CheckedPattern::Wildcard; shape.len()]);
    }
    let CheckedPattern::Enum {
        owner,
        variant: found,
        payload,
    } = pattern
    else {
        return None;
    };
    if &owner.key != key || *found != variant {
        return None;
    }
    match (payload, shape) {
        (CheckedEnumPayload::Unit, FieldShape::Unit) => Some(vec![]),
        (CheckedEnumPayload::Tuple(fields), FieldShape::Tuple(_)) => Some(fields.clone()),
        (CheckedEnumPayload::Struct(fields), FieldShape::Struct(slots)) => Some(
            slots
                .iter()
                .map(|slot| {
                    fields
                        .iter()
                        .find(|field| field.slot == *slot)
                        .map_or(CheckedPattern::Wildcard, |field| field.pattern.clone())
                })
                .collect(),
        ),
        _ => None,
    }
}

fn optional_fields(pattern: &CheckedPattern, some: bool) -> Option<Vec<CheckedPattern>> {
    if irrefutable(pattern) {
        return Some(if some {
            vec![CheckedPattern::Wildcard]
        } else {
            vec![]
        });
    }
    match (some, pattern) {
        (true, CheckedPattern::OptionalSome(inner)) => Some(vec![(**inner).clone()]),
        (false, CheckedPattern::Nil) => Some(vec![]),
        (
            true,
            CheckedPattern::Enum {
                variant, payload, ..
            },
        ) if *variant == Ident::new("Some") => {
            let CheckedEnumPayload::Tuple(fields) = payload else {
                return None;
            };
            (fields.len() == 1).then(|| fields.clone())
        }
        (false, CheckedPattern::Enum { variant, .. }) if *variant == Ident::new("None") => {
            Some(vec![])
        }
        _ => None,
    }
}

fn tuple_fields(pattern: &CheckedPattern, len: usize) -> Option<Vec<CheckedPattern>> {
    if irrefutable(pattern) {
        return Some(vec![CheckedPattern::Wildcard; len]);
    }
    match pattern {
        CheckedPattern::Tuple(fields) if fields.len() == len => Some(fields.clone()),
        _ => None,
    }
}

fn struct_slots(
    rows: &[Vec<CheckedPattern>],
    seed: &[super::semantic_use::CheckedStructField],
) -> Vec<usize> {
    let mut slots = seed.iter().map(|field| field.slot).collect::<Vec<_>>();
    for pattern in rows.iter().map(|row| &row[0]) {
        if let CheckedPattern::Struct { fields, .. } = pattern {
            for field in fields {
                if !slots.contains(&field.slot) {
                    slots.push(field.slot);
                }
            }
        }
    }
    slots.sort_unstable();
    slots
}

fn struct_fields(pattern: &CheckedPattern, slots: &[usize]) -> Option<Vec<CheckedPattern>> {
    if irrefutable(pattern) {
        return Some(vec![CheckedPattern::Wildcard; slots.len()]);
    }
    let CheckedPattern::Struct { fields, .. } = pattern else {
        return None;
    };
    Some(
        slots
            .iter()
            .map(|slot| {
                fields
                    .iter()
                    .find(|field| field.slot == *slot)
                    .map_or(CheckedPattern::Wildcard, |field| field.pattern.clone())
            })
            .collect(),
    )
}
