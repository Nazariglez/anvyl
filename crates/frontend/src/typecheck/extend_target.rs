use super::{
    GenericArgs, GenericParams, Specificity, generic_template_type, match_generic_template_args,
};
use crate::{
    ast::{ExtendTargetConstraint, Ident, RawEnumBackingConstraint, Type},
    typecheck::decls::{DeclarationIndex, EnumRepr, ExtendSchema},
};

pub(crate) struct ExtendTargetPattern<'a> {
    pub(crate) target: &'a Type,
    pub(crate) generics: &'a GenericParams,
    pub(crate) target_constraint: Option<ExtendTargetConstraint>,
}

impl<'a> From<&'a ExtendSchema> for ExtendTargetPattern<'a> {
    fn from(extend: &'a ExtendSchema) -> Self {
        Self {
            target: &extend.target,
            generics: &extend.generics,
            target_constraint: extend.target_constraint,
        }
    }
}

pub(crate) struct ExtendTargetMatch {
    pub(crate) templated_target: Type,
    pub(crate) receiver_ty: Type,
    pub(crate) owner_args: Result<GenericArgs, Vec<Ident>>,
}

pub(crate) fn match_exact_target(
    decls: &DeclarationIndex,
    pattern: &ExtendTargetPattern<'_>,
    subject: &Type,
    static_nominal_family: bool,
) -> Option<ExtendTargetMatch> {
    let target = generic_template_type(pattern.target, pattern.generics);
    let owner_args = if static_nominal_family && static_nominal_family_match(&target, subject) {
        Ok(GenericArgs::default())
    } else {
        match_generic_template_args(pattern.generics, &target, subject)?
    };
    target_satisfies_constraint(decls, pattern, subject).then_some(ExtendTargetMatch {
        templated_target: target,
        receiver_ty: subject.clone(),
        owner_args,
    })
}

fn target_satisfies_constraint(
    decls: &DeclarationIndex,
    pattern: &ExtendTargetPattern<'_>,
    subject: &Type,
) -> bool {
    let Some(constraint) = pattern.target_constraint else {
        return true;
    };
    if broad_constraint(pattern).is_some() && !saturated_nominal(decls, subject) {
        return false;
    }
    type_satisfies_constraint(decls, subject, constraint)
}

fn type_satisfies_constraint(
    decls: &DeclarationIndex,
    ty: &Type,
    constraint: ExtendTargetConstraint,
) -> bool {
    decls.key_for_type(ty).is_some_and(|key| {
        key.kind == constraint.nominal_kind() && backing_matches(decls, &key, constraint.backing())
    })
}

fn backing_matches(
    decls: &DeclarationIndex,
    key: &crate::typecheck::decls::NominalKey,
    backing: Option<RawEnumBackingConstraint>,
) -> bool {
    match backing {
        None => true,
        Some(RawEnumBackingConstraint::Int) => {
            decls.enum_repr_for_key(key) == Some(EnumRepr::RawInt)
        }
        Some(RawEnumBackingConstraint::String) => {
            decls.enum_repr_for_key(key) == Some(EnumRepr::RawString)
        }
    }
}

pub(crate) fn validate_constrained_target(
    decls: &DeclarationIndex,
    pattern: &ExtendTargetPattern<'_>,
) -> bool {
    match pattern.target_constraint {
        None => true,
        Some(constraint) => {
            broad_constraint(pattern).is_some()
                || type_satisfies_constraint(decls, pattern.target, constraint)
        }
    }
}

fn saturated_nominal(decls: &DeclarationIndex, ty: &Type) -> bool {
    let Some(key) = decls.key_for_type(ty) else {
        return false;
    };
    let Some(args) = super::decls::nominal_generic_args(ty) else {
        return false;
    };
    decls.split_nominal_args(&key, &args).is_some()
}

pub(crate) fn permits_receiver_conversion(pattern: &ExtendTargetPattern<'_>) -> bool {
    pattern.target_constraint.is_none()
}

pub(crate) fn same_target_pattern(
    a: &ExtendTargetPattern<'_>,
    b: &ExtendTargetPattern<'_>,
) -> bool {
    match (broad_constraint(a), broad_constraint(b)) {
        (Some(a_constraint), Some(b_constraint)) => a_constraint == b_constraint,
        (Some(_), None) | (None, Some(_)) => false,
        (None, None) => super::same_extend_target(a.target, a.generics, b.target, b.generics),
    }
}

fn broad_constraint(pattern: &ExtendTargetPattern<'_>) -> Option<ExtendTargetConstraint> {
    let constraint = pattern.target_constraint?;
    let Type::Var(id) = pattern.target else {
        return None;
    };
    pattern
        .generics
        .type_params
        .iter()
        .any(|param| param.id == *id)
        .then_some(constraint)
}

pub(crate) struct MatchedTargetPattern<'a> {
    pub(crate) pattern: ExtendTargetPattern<'a>,
    pub(crate) target: &'a Type,
}

pub(crate) fn most_specific_target_match(candidates: &[MatchedTargetPattern<'_>]) -> Option<usize> {
    match candidates.len() {
        0 => None,
        1 => Some(0),
        _ => {
            let winner = (1..candidates.len()).fold(0, |best, i| {
                if target_match_specificity(&candidates[i], &candidates[best])
                    == Specificity::MoreSpecific
                {
                    i
                } else {
                    best
                }
            });
            candidates
                .iter()
                .enumerate()
                .all(|(i, candidate)| {
                    i == winner
                        || target_match_specificity(&candidates[winner], candidate)
                            == Specificity::MoreSpecific
                })
                .then_some(winner)
        }
    }
}

fn target_match_specificity(
    a: &MatchedTargetPattern<'_>,
    b: &MatchedTargetPattern<'_>,
) -> Specificity {
    compare_target_specificity(&a.pattern, a.target, &b.pattern, b.target)
}

pub(crate) fn compare_target_specificity(
    a: &ExtendTargetPattern<'_>,
    a_target: &Type,
    b: &ExtendTargetPattern<'_>,
    b_target: &Type,
) -> Specificity {
    match (broad_constraint(a), broad_constraint(b)) {
        (Some(a_constraint), Some(b_constraint)) => {
            return compare_constraint_specificity(a_constraint, b_constraint);
        }
        (Some(_), None) => {
            if universal_target(b) {
                return Specificity::MoreSpecific;
            }
            return matches!(b_target, Type::Nominal(_))
                .then_some(Specificity::LessSpecific)
                .unwrap_or(Specificity::Incomparable);
        }
        (None, Some(_)) => {
            if universal_target(a) {
                return Specificity::LessSpecific;
            }
            return matches!(a_target, Type::Nominal(_))
                .then_some(Specificity::MoreSpecific)
                .unwrap_or(Specificity::Incomparable);
        }
        (None, None) => {}
    }
    super::compare_specificity(a_target, b_target)
}

fn compare_constraint_specificity(
    a: ExtendTargetConstraint,
    b: ExtendTargetConstraint,
) -> Specificity {
    match (a, b) {
        (a, b) if a == b => Specificity::Equal,
        (
            ExtendTargetConstraint::Enum { backing: Some(_) },
            ExtendTargetConstraint::Enum { backing: None },
        ) => Specificity::MoreSpecific,
        (
            ExtendTargetConstraint::Enum { backing: None },
            ExtendTargetConstraint::Enum { backing: Some(_) },
        ) => Specificity::LessSpecific,
        _ => Specificity::Incomparable,
    }
}

fn universal_target(pattern: &ExtendTargetPattern<'_>) -> bool {
    if pattern.target_constraint.is_some() || !pattern.generics.const_params.is_empty() {
        return false;
    }
    let [param] = pattern.generics.type_params.as_slice() else {
        return false;
    };
    *pattern.target == Type::Var(param.id)
}

fn static_nominal_family_match(target: &Type, subject: &Type) -> bool {
    let (Type::Nominal(target), Type::Nominal(subject)) = (target, subject) else {
        return false;
    };
    target.id == subject.id && subject.type_args.is_empty() && subject.const_args.is_empty()
}
