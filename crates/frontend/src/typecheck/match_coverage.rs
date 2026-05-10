use super::{
    TypeChecker, TypeError,
    decls::NominalKey,
    pattern::{PatternCover, PatternOutcome},
};
use crate::{
    ast::{Ident, NominalKind, Type},
    span::Span,
};

enum MatchSubject {
    Bool,
    Int,
    Float,
    String,
    Enum {
        key: NominalKey,
        variants: Vec<Ident>,
    },
    Tuple,
    Unknown,
    Unsupported(Type),
}

pub(super) fn check(
    scrutinee: &Type,
    outcomes: &[PatternOutcome],
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
    if matches!(subject, MatchSubject::Unknown) || outcomes.iter().any(|outcome| outcome.had_error)
    {
        return;
    }
    if outcomes
        .iter()
        .any(|outcome| cover_is_catch_all(&outcome.cover))
    {
        return;
    }
    match subject {
        MatchSubject::Bool => check_bool(outcomes, span, tc),
        MatchSubject::Enum { key, variants } => check_enum(&key, &variants, outcomes, span, tc),
        MatchSubject::Int | MatchSubject::Float | MatchSubject::String => {
            tc.push_error(TypeError::NonExhaustiveMatch {
                span: tc.error_span(span),
            });
        }
        MatchSubject::Tuple => {}
        MatchSubject::Unknown | MatchSubject::Unsupported(_) => {}
    }
}

fn classify(scrutinee: &Type, tc: &TypeChecker) -> MatchSubject {
    match scrutinee {
        Type::Infer => MatchSubject::Unknown,
        Type::Bool => MatchSubject::Bool,
        Type::Int => MatchSubject::Int,
        Type::Float => MatchSubject::Float,
        Type::String => MatchSubject::String,
        Type::Tuple(_) => MatchSubject::Tuple,
        ty => match tc
            .decls
            .key_for_type(ty)
            .filter(|key| key.kind == NominalKind::Enum)
        {
            Some(key) => {
                let variants = tc
                    .decls
                    .enum_schema(&key)
                    .map(|schema| schema.variants.keys().copied().collect())
                    .unwrap_or_default();
                MatchSubject::Enum { key, variants }
            }
            None => MatchSubject::Unsupported(ty.clone()),
        },
    }
}

fn check_bool(outcomes: &[PatternOutcome], span: Span, tc: &mut TypeChecker) {
    let has_true = covers_bool(outcomes, true);
    let has_false = covers_bool(outcomes, false);
    if !has_true || !has_false {
        tc.push_error(TypeError::NonExhaustiveMatch {
            span: tc.error_span(span),
        });
    }
}

fn cover_is_catch_all(cover: &PatternCover) -> bool {
    match cover {
        PatternCover::CatchAll => true,
        PatternCover::Or(covers) => covers.iter().any(cover_is_catch_all),
        _ => false,
    }
}

fn covers_bool(outcomes: &[PatternOutcome], value: bool) -> bool {
    outcomes
        .iter()
        .any(|outcome| cover_matches_bool(&outcome.cover, value))
}

fn cover_matches_bool(cover: &PatternCover, value: bool) -> bool {
    match cover {
        PatternCover::Bool(v) => *v == value,
        PatternCover::Or(covers) => covers.iter().any(|cover| cover_matches_bool(cover, value)),
        _ => false,
    }
}

fn check_enum(
    key: &NominalKey,
    variants: &[Ident],
    outcomes: &[PatternOutcome],
    span: Span,
    tc: &mut TypeChecker,
) {
    for variant in variants {
        let covered = outcomes
            .iter()
            .any(|outcome| cover_matches_enum(&outcome.cover, key, *variant));
        if !covered {
            tc.push_error(TypeError::NonExhaustiveMatch {
                span: tc.error_span(span),
            });
            return;
        }
    }
}

fn cover_matches_enum(cover: &PatternCover, key: &NominalKey, variant: Ident) -> bool {
    match cover {
        PatternCover::EnumVariant {
            key: covered_key,
            variant: covered,
        } => covered_key == key && *covered == variant,
        PatternCover::Or(covers) => covers
            .iter()
            .any(|cover| cover_matches_enum(cover, key, variant)),
        _ => false,
    }
}
