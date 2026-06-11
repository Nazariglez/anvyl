use crate::ast::Ident;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum SequenceStructuralEffect {
    Push,
    Pop,
    SortBy,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum MapStructuralEffect {
    Insert,
    Remove,
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
            Self::Sequence(SequenceStructuralEffect::Pop) => "ListPop",
            Self::Sequence(SequenceStructuralEffect::SortBy) => "ListSortBy",
            Self::Map(MapStructuralEffect::Insert) => "MapInsert",
            Self::Map(MapStructuralEffect::Remove) => "MapRemove",
        }
    }

    pub(crate) fn sequence(self) -> bool {
        matches!(self, Self::Sequence(_))
    }

    pub(crate) fn map(self) -> bool {
        matches!(self, Self::Map(_))
    }
}

pub(crate) fn classify_sequence_method(name: Ident) -> Option<SequenceStructuralEffect> {
    Some(match name.as_str() {
        "push" => SequenceStructuralEffect::Push,
        "sort_by" => SequenceStructuralEffect::SortBy,
        _ => return None,
    })
}

pub(crate) fn classify_map_method(name: Ident) -> Option<MapStructuralEffect> {
    Some(match name.as_str() {
        "insert" => MapStructuralEffect::Insert,
        "remove" => MapStructuralEffect::Remove,
        _ => return None,
    })
}
