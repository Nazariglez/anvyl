use crate::ast::Ident;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum CollectionKind {
    Sequence,
    Map,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum SequenceMethod {
    Push,
    ForEach,
    Retain,
    RemoveWhere,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum MapMethod {
    Insert,
    Remove,
    Retain,
    RemoveWhere,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum SequenceStructuralEffect {
    Push,
    Retain,
    RemoveWhere,
    InternalPop,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum MapStructuralEffect {
    Insert,
    Remove,
    Retain,
    RemoveWhere,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum CollectionStructuralEffect {
    Sequence(SequenceStructuralEffect),
    Map(MapStructuralEffect),
}

impl CollectionStructuralEffect {
    pub(crate) fn name(self) -> &'static str {
        match self {
            Self::Sequence(SequenceStructuralEffect::Push) => "ListPush",
            Self::Sequence(SequenceStructuralEffect::Retain) => "ListRetain",
            Self::Sequence(SequenceStructuralEffect::RemoveWhere) => "ListRemoveWhere",
            Self::Sequence(SequenceStructuralEffect::InternalPop) => "ListPop",
            Self::Map(MapStructuralEffect::Insert) => "MapInsert",
            Self::Map(MapStructuralEffect::Remove) => "MapRemove",
            Self::Map(MapStructuralEffect::Retain) => "MapRetain",
            Self::Map(MapStructuralEffect::RemoveWhere) => "MapRemoveWhere",
        }
    }

    pub(crate) fn sequence(self) -> bool {
        matches!(self, Self::Sequence(_))
    }

    pub(crate) fn map(self) -> bool {
        matches!(self, Self::Map(_))
    }
}

impl SequenceMethod {
    fn structural_effect(self) -> Option<SequenceStructuralEffect> {
        match self {
            Self::Push => Some(SequenceStructuralEffect::Push),
            Self::Retain => Some(SequenceStructuralEffect::Retain),
            Self::RemoveWhere => Some(SequenceStructuralEffect::RemoveWhere),
            Self::ForEach => None,
        }
    }

    fn remove_matches(self) -> Option<bool> {
        match self {
            Self::Retain => Some(false),
            Self::RemoveWhere => Some(true),
            Self::Push | Self::ForEach => None,
        }
    }

    fn storage_value_arg_indices(self) -> &'static [usize] {
        match self {
            Self::Push => &[0],
            Self::ForEach | Self::Retain | Self::RemoveWhere => &[],
        }
    }
}

impl MapMethod {
    fn structural_effect(self) -> MapStructuralEffect {
        match self {
            Self::Insert => MapStructuralEffect::Insert,
            Self::Remove => MapStructuralEffect::Remove,
            Self::Retain => MapStructuralEffect::Retain,
            Self::RemoveWhere => MapStructuralEffect::RemoveWhere,
        }
    }

    fn remove_matches(self) -> Option<bool> {
        match self {
            Self::Retain => Some(false),
            Self::RemoveWhere => Some(true),
            Self::Insert | Self::Remove => None,
        }
    }

    fn storage_value_arg_indices(self) -> &'static [usize] {
        match self {
            Self::Insert => &[1],
            Self::Remove | Self::Retain | Self::RemoveWhere => &[],
        }
    }
}

pub(crate) fn classify_sequence_method(name: Ident) -> Option<SequenceMethod> {
    Some(match name.as_str() {
        "push" => SequenceMethod::Push,
        "for_each" => SequenceMethod::ForEach,
        "retain" => SequenceMethod::Retain,
        "remove_where" => SequenceMethod::RemoveWhere,
        _ => return None,
    })
}

pub(crate) fn classify_map_method(name: Ident) -> Option<MapMethod> {
    Some(match name.as_str() {
        "insert" => MapMethod::Insert,
        "remove" => MapMethod::Remove,
        "retain" => MapMethod::Retain,
        "remove_where" => MapMethod::RemoveWhere,
        _ => return None,
    })
}

pub(crate) fn classify_structural_method(
    kind: CollectionKind,
    name: Ident,
) -> Option<CollectionStructuralEffect> {
    match kind {
        CollectionKind::Sequence => classify_sequence_method(name)
            .and_then(SequenceMethod::structural_effect)
            .map(CollectionStructuralEffect::Sequence),
        CollectionKind::Map => classify_map_method(name)
            .map(MapMethod::structural_effect)
            .map(CollectionStructuralEffect::Map),
    }
}

pub(crate) fn filter_remove_matches(kind: CollectionKind, name: Ident) -> Option<bool> {
    match kind {
        CollectionKind::Sequence => {
            classify_sequence_method(name).and_then(SequenceMethod::remove_matches)
        }
        CollectionKind::Map => classify_map_method(name).and_then(MapMethod::remove_matches),
    }
}

pub(crate) fn storage_value_arg_indices(kind: CollectionKind, name: Ident) -> &'static [usize] {
    match kind {
        CollectionKind::Sequence => {
            classify_sequence_method(name).map_or(&[], SequenceMethod::storage_value_arg_indices)
        }
        CollectionKind::Map => {
            classify_map_method(name).map_or(&[], MapMethod::storage_value_arg_indices)
        }
    }
}

pub(crate) fn has_lowered_stub(name: Ident) -> bool {
    classify_sequence_method(name).is_some() || classify_map_method(name).is_some()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn id(name: &str) -> Ident {
        Ident::new(name)
    }

    #[test]
    fn classifies_core_collection_methods() {
        assert_eq!(
            classify_sequence_method(id("push")),
            Some(SequenceMethod::Push)
        );
        assert_eq!(
            classify_sequence_method(id("for_each")),
            Some(SequenceMethod::ForEach)
        );
        assert_eq!(
            classify_sequence_method(id("retain")),
            Some(SequenceMethod::Retain)
        );
        assert_eq!(
            classify_sequence_method(id("remove_where")),
            Some(SequenceMethod::RemoveWhere)
        );
        assert_eq!(classify_map_method(id("insert")), Some(MapMethod::Insert));
        assert_eq!(classify_map_method(id("remove")), Some(MapMethod::Remove));
        assert_eq!(classify_map_method(id("retain")), Some(MapMethod::Retain));
        assert_eq!(
            classify_map_method(id("remove_where")),
            Some(MapMethod::RemoveWhere)
        );
    }

    #[test]
    fn callback_methods_have_collection_roles() {
        assert_eq!(
            classify_structural_method(CollectionKind::Sequence, id("for_each")),
            None
        );
        assert_eq!(
            filter_remove_matches(CollectionKind::Sequence, id("retain")),
            Some(false)
        );
        assert_eq!(
            filter_remove_matches(CollectionKind::Sequence, id("remove_where")),
            Some(true)
        );
        assert_eq!(
            filter_remove_matches(CollectionKind::Map, id("retain")),
            Some(false)
        );
        assert_eq!(
            filter_remove_matches(CollectionKind::Map, id("remove_where")),
            Some(true)
        );
    }

    #[test]
    fn structural_methods_record_storage_values() {
        assert_eq!(
            storage_value_arg_indices(CollectionKind::Sequence, id("push")),
            &[0]
        );
        assert_eq!(
            storage_value_arg_indices(CollectionKind::Map, id("insert")),
            &[1]
        );
        assert!(storage_value_arg_indices(CollectionKind::Map, id("remove")).is_empty());
    }
}
