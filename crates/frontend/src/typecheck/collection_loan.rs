use super::{
    ModuleScope, TypeError,
    place::PlaceIdentity,
    type_ops::{contains_nested_stored_slice_view, contains_stored_slice_view},
};
use crate::{
    ast::{Ident, Type},
    collection_effect::{self, CollectionKind, CollectionStructuralEffect},
    span::SourceSpan,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum CollectionRootKind {
    List,
    Array,
    Slice,
    Map,
}

impl CollectionRootKind {
    pub(super) fn from_type(ty: &Type) -> Option<Self> {
        match ty {
            Type::List { .. } => Some(Self::List),
            Type::Array { .. } => Some(Self::Array),
            Type::Slice { .. } => Some(Self::Slice),
            Type::Map { .. } => Some(Self::Map),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum CollectionExposureKind {
    Sequence,
    Map,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) struct ActiveCollectionLoan {
    pub(super) identity: PlaceIdentity,
    pub(super) exposure: CollectionExposureKind,
}

pub(super) fn classify_method_effect(
    receiver_ty: &Type,
    name: Ident,
    origin: &ModuleScope,
) -> Option<CollectionStructuralEffect> {
    if !origin.is_core_module("collections") {
        return None;
    }
    let kind = match CollectionRootKind::from_type(receiver_ty)? {
        CollectionRootKind::List | CollectionRootKind::Array | CollectionRootKind::Slice => {
            CollectionKind::Sequence
        }
        CollectionRootKind::Map => CollectionKind::Map,
    };
    collection_effect::classify_structural_method(kind, name)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum ActiveSequenceMatch {
    Exact,
    RebindConflict,
    ElementProjection,
    Distinct,
    Unknown,
}

pub(super) fn classify_for_loan(
    iterable_ty: &Type,
    identity: PlaceIdentity,
) -> Option<ActiveCollectionLoan> {
    let exposure = match CollectionRootKind::from_type(iterable_ty)? {
        CollectionRootKind::List | CollectionRootKind::Array | CollectionRootKind::Slice => {
            CollectionExposureKind::Sequence
        }
        CollectionRootKind::Map => CollectionExposureKind::Map,
    };
    Some(ActiveCollectionLoan { identity, exposure })
}

pub(super) fn active_sequence_match(
    active_loans: &[ActiveCollectionLoan],
    identity: &PlaceIdentity,
) -> ActiveSequenceMatch {
    let mut saw_unknown = false;
    for loan in active_loans
        .iter()
        .rev()
        .filter(|loan| loan.exposure == CollectionExposureKind::Sequence)
    {
        match sequence_match_kind(identity, &loan.identity) {
            ActiveSequenceMatch::Exact => return ActiveSequenceMatch::Exact,
            ActiveSequenceMatch::RebindConflict => return ActiveSequenceMatch::RebindConflict,
            ActiveSequenceMatch::ElementProjection => {
                return ActiveSequenceMatch::ElementProjection;
            }
            ActiveSequenceMatch::Distinct => {}
            ActiveSequenceMatch::Unknown => saw_unknown = true,
        }
    }
    if saw_unknown {
        ActiveSequenceMatch::Unknown
    } else {
        ActiveSequenceMatch::Distinct
    }
}

pub(super) fn structural_method_error(
    active_loans: &[ActiveCollectionLoan],
    effect: CollectionStructuralEffect,
    receiver_identity: &PlaceIdentity,
    span: Option<SourceSpan>,
) -> Option<TypeError> {
    if effect.sequence() {
        return matches!(
            active_sequence_match(active_loans, receiver_identity),
            ActiveSequenceMatch::Exact
        )
        .then_some(TypeError::SequenceStructuralMutationDuringLoan { span });
    }
    active_loans
        .iter()
        .any(|loan| {
            loan.exposure == CollectionExposureKind::Map
                && proven_exact_match(receiver_identity, &loan.identity)
        })
        .then_some(TypeError::MapStructuralMutationDuringLoan { span })
}

pub(super) fn root_rebind_error(
    active_loans: &[ActiveCollectionLoan],
    identity: &PlaceIdentity,
    span: Option<SourceSpan>,
) -> Option<TypeError> {
    for loan in active_loans.iter().rev() {
        match sequence_match_kind(identity, &loan.identity) {
            ActiveSequenceMatch::Exact | ActiveSequenceMatch::RebindConflict => {
                return Some(TypeError::ActiveCollectionRebind { span });
            }
            ActiveSequenceMatch::ElementProjection
            | ActiveSequenceMatch::Distinct
            | ActiveSequenceMatch::Unknown => {}
        }
    }
    None
}

pub(super) fn mutable_collection_arg_error(
    active_loans: &[ActiveCollectionLoan],
    identity: &PlaceIdentity,
    arg_ty: &Type,
    param_ty: &Type,
    span: Option<SourceSpan>,
) -> Option<TypeError> {
    if matches!(
        CollectionRootKind::from_type(param_ty),
        Some(CollectionRootKind::Slice)
    ) {
        return None;
    }
    match CollectionRootKind::from_type(arg_ty).or_else(|| CollectionRootKind::from_type(param_ty))
    {
        Some(CollectionRootKind::List | CollectionRootKind::Array | CollectionRootKind::Slice) => {
            match active_sequence_match(active_loans, identity) {
                ActiveSequenceMatch::Exact => Some(TypeError::ActiveCollectionMutableArg { span }),
                ActiveSequenceMatch::RebindConflict
                | ActiveSequenceMatch::ElementProjection
                | ActiveSequenceMatch::Distinct
                | ActiveSequenceMatch::Unknown => None,
            }
        }
        Some(CollectionRootKind::Map) => active_loans
            .iter()
            .find(|loan| {
                loan.exposure == CollectionExposureKind::Map
                    && proven_exact_match(identity, &loan.identity)
            })
            .map(|_| TypeError::ActiveCollectionMutableArg { span }),
        None => None,
    }
}

pub(super) fn stored_slice_local_error(ty: &Type, span: Option<SourceSpan>) -> Option<TypeError> {
    contains_stored_slice_view(ty).then_some(TypeError::StoredSliceLocal { span })
}

pub(super) fn stored_nested_slice_error(ty: &Type, span: Option<SourceSpan>) -> Option<TypeError> {
    contains_nested_stored_slice_view(ty).then_some(TypeError::StoredSliceLocal { span })
}

fn sequence_match_kind(identity: &PlaceIdentity, active: &PlaceIdentity) -> ActiveSequenceMatch {
    if proven_exact_match(identity, active) {
        return ActiveSequenceMatch::Exact;
    }
    if proven_element_projection(identity, active) {
        return ActiveSequenceMatch::ElementProjection;
    }
    if proven_rebind_conflict(identity, active) {
        return ActiveSequenceMatch::RebindConflict;
    }
    if identity.conflicts_with(active) {
        ActiveSequenceMatch::Unknown
    } else {
        ActiveSequenceMatch::Distinct
    }
}

fn proven_exact_match(left: &PlaceIdentity, right: &PlaceIdentity) -> bool {
    match (left, right) {
        (PlaceIdentity::Single(left), PlaceIdentity::Single(right)) => left == right,
        (PlaceIdentity::ConstIndex(left), PlaceIdentity::ConstIndex(right)) => left == right,
        (
            PlaceIdentity::IndexedDerived {
                base: left_base,
                index: left_index,
            },
            PlaceIdentity::IndexedDerived {
                base: right_base,
                index: right_index,
            },
        ) => left_base == right_base && proven_exact_match(left_index, right_index),
        (PlaceIdentity::Alternatives { alternatives, .. }, other)
        | (other, PlaceIdentity::Alternatives { alternatives, .. }) => {
            !alternatives.is_empty()
                && alternatives
                    .iter()
                    .all(|alternative| proven_exact_match(alternative, other))
        }
        _ => false,
    }
}

fn proven_element_projection(target: &PlaceIdentity, active: &PlaceIdentity) -> bool {
    matches!(
        (target.indexed_base_path(), active),
        (Some(base), PlaceIdentity::Single(active)) if base.starts_with(active)
    )
}

fn proven_rebind_conflict(target: &PlaceIdentity, active: &PlaceIdentity) -> bool {
    if proven_exact_match(target, active) {
        return true;
    }
    match (target, active) {
        (PlaceIdentity::Single(_), PlaceIdentity::Single(_)) => target.conflicts_with(active),
        (PlaceIdentity::Single(target), PlaceIdentity::IndexedDerived { base, .. }) => {
            target.conflicts_with(base)
        }
        (PlaceIdentity::Alternatives { alternatives, .. }, other)
        | (other, PlaceIdentity::Alternatives { alternatives, .. }) => {
            !alternatives.is_empty()
                && alternatives
                    .iter()
                    .all(|alternative| proven_rebind_conflict(alternative, other))
        }
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        ast::Ident,
        typecheck::{SemanticLocalId, place::PlaceRoot},
    };

    fn loan(identity: PlaceIdentity) -> ActiveCollectionLoan {
        ActiveCollectionLoan {
            identity,
            exposure: CollectionExposureKind::Sequence,
        }
    }

    fn local(id: u32) -> PlaceIdentity {
        PlaceIdentity::root(PlaceRoot::Local(SemanticLocalId::new(id)))
    }

    #[test]
    fn matches_same_indexed_collection_root() {
        let index = local(2);
        let loan = loan(local(1).index_by(index.clone()));
        assert!(matches!(
            active_sequence_match(&[loan], &local(1).index_by(index)),
            ActiveSequenceMatch::Exact
        ));
    }

    #[test]
    fn matches_same_literal_indexed_collection_root() {
        let index = PlaceIdentity::const_index(0);
        let loan = loan(local(1).index_by(index.clone()));
        assert!(matches!(
            active_sequence_match(&[loan], &local(1).index_by(index)),
            ActiveSequenceMatch::Exact
        ));
    }

    #[test]
    fn cannot_prove_different_indexed_collection_root() {
        let loan = loan(local(1).index_by(local(2)));
        assert!(matches!(
            active_sequence_match(&[loan], &local(1).index_by(local(3))),
            ActiveSequenceMatch::Unknown
        ));
    }

    #[test]
    fn matches_indexed_projection_from_projected_root() {
        let loan = loan(local(1).field(Ident::new("left")));
        assert!(matches!(
            active_sequence_match(&[loan], &local(1).field(Ident::new("left")).index()),
            ActiveSequenceMatch::ElementProjection
        ));
    }

    #[test]
    fn distinguishes_sibling_projection() {
        let loan = loan(local(1).field(Ident::new("left")));
        let sibling = local(1).field(Ident::new("right"));
        assert!(matches!(
            active_sequence_match(&[loan], &sibling),
            ActiveSequenceMatch::Distinct
        ));
    }

    #[test]
    fn distinguishes_indexed_sibling_projection() {
        let loan = loan(local(1).field(Ident::new("left")));
        let sibling = local(1).field(Ident::new("right")).index();
        assert!(matches!(
            active_sequence_match(&[loan], &sibling),
            ActiveSequenceMatch::Distinct
        ));
    }
}
