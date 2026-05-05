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
            span,
        });
        return;
    }
    if matches!(subject, MatchSubject::Unknown) || outcomes.iter().any(|outcome| outcome.had_error)
    {
        return;
    }
    if outcomes
        .iter()
        .any(|outcome| matches!(outcome.cover, PatternCover::CatchAll))
    {
        return;
    }
    match subject {
        MatchSubject::Bool => check_bool(outcomes, span, tc),
        MatchSubject::Enum { key, variants } => check_enum(&key, &variants, outcomes, span, tc),
        MatchSubject::Int | MatchSubject::Float | MatchSubject::String => {
            tc.push_error(TypeError::NonExhaustiveMatch { span });
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
        tc.push_error(TypeError::NonExhaustiveMatch { span });
    }
}

fn covers_bool(outcomes: &[PatternOutcome], value: bool) -> bool {
    outcomes
        .iter()
        .any(|outcome| matches!(outcome.cover, PatternCover::Bool(v) if v == value))
}

fn check_enum(
    key: &NominalKey,
    variants: &[Ident],
    outcomes: &[PatternOutcome],
    span: Span,
    tc: &mut TypeChecker,
) {
    for variant in variants {
        let covered = outcomes.iter().any(|outcome| {
            matches!(&outcome.cover, PatternCover::EnumVariant { key: covered_key, variant: covered }
                if covered_key == key && covered == variant)
        });
        if !covered {
            tc.push_error(TypeError::NonExhaustiveMatch { span });
            return;
        }
    }
}
