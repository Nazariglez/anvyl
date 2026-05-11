use std::collections::HashMap;

use crate::{
    ast::{Ident, Type},
    span::SourceSpan,
};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub(crate) struct CanonicalTypeKey(pub(crate) Type);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum Exposure {
    Explicit,
    Implicit,
}

impl Exposure {
    pub(crate) fn from_selector(has_selector: bool) -> Self {
        if has_selector {
            Self::Explicit
        } else {
            Self::Implicit
        }
    }
}

#[derive(Clone, Debug)]
pub(crate) struct PromotedAlias {
    pub(crate) path: Vec<Ident>,
    pub(crate) origin: CanonicalTypeKey,
    pub(crate) origin_member: Ident,
    pub(crate) exposure: Exposure,
    pub(crate) selector_span: Option<SourceSpan>,
}

pub(crate) type PromotedFieldAlias = PromotedAlias;
pub(crate) type PromotedMethodAlias = PromotedAlias;

impl PromotedAlias {
    pub(crate) fn new(
        path: Vec<Ident>,
        origin: CanonicalTypeKey,
        origin_member: Ident,
        exposure: Exposure,
        selector_span: Option<SourceSpan>,
    ) -> Self {
        Self {
            path,
            origin,
            origin_member,
            exposure,
            selector_span,
        }
    }

    pub(crate) fn with_prefix(
        &self,
        prefix: Ident,
        exposure: Exposure,
        selector_span: Option<SourceSpan>,
    ) -> Self {
        Self::new(
            prefixed_path(prefix, &self.path),
            self.origin.clone(),
            self.origin_member,
            exposure,
            selector_span,
        )
    }

    pub(crate) fn path_len(&self) -> usize {
        self.path.len()
    }
}

#[derive(Clone, Debug)]
pub(crate) struct SurfaceSlot<T> {
    pub(crate) aliases: Vec<T>,
    pub(crate) ambiguous: bool,
}

impl<T> Default for SurfaceSlot<T> {
    fn default() -> Self {
        Self {
            aliases: vec![],
            ambiguous: false,
        }
    }
}

#[derive(Clone, Debug, Default)]
pub(crate) struct PromotedSurface {
    pub(crate) fields: HashMap<Ident, SurfaceSlot<PromotedFieldAlias>>,
    pub(crate) methods: HashMap<Ident, SurfaceSlot<PromotedMethodAlias>>,
}

impl PromotedSurface {
    pub(crate) fn insert_field(&mut self, name: Ident, alias: PromotedFieldAlias) {
        insert_alias(&mut self.fields, name, alias);
    }

    pub(crate) fn insert_method(&mut self, name: Ident, alias: PromotedMethodAlias) {
        insert_alias(&mut self.methods, name, alias);
    }

    pub(crate) fn sort(&mut self) {
        for slot in self.fields.values_mut() {
            slot.aliases.sort_by_key(alias_sort_key);
        }
        for slot in self.methods.values_mut() {
            slot.aliases.sort_by_key(alias_sort_key);
        }
    }

    pub(crate) fn invariants_hold(&self) -> bool {
        slots_valid(&self.fields, field_alias_valid)
            && slots_valid(&self.methods, method_alias_valid)
    }
}

fn insert_alias<T>(map: &mut HashMap<Ident, SurfaceSlot<T>>, name: Ident, alias: T) {
    let slot = map.entry(name).or_default();
    slot.aliases.push(alias);
    slot.ambiguous = slot.aliases.len() > 1;
}

fn slots_valid<T>(map: &HashMap<Ident, SurfaceSlot<T>>, valid: fn(&T) -> bool) -> bool {
    map.values().all(|slot| {
        !slot.aliases.is_empty()
            && slot.ambiguous == (slot.aliases.len() > 1)
            && slot.aliases.iter().all(valid)
    })
}

fn prefixed_path(prefix: Ident, path: &[Ident]) -> Vec<Ident> {
    let mut prefixed = Vec::with_capacity(path.len() + 1);
    prefixed.push(prefix);
    prefixed.extend(path.iter().copied());
    prefixed
}

fn alias_sort_key(alias: &PromotedAlias) -> (String, String, usize, String) {
    (
        alias.origin.0.to_string(),
        alias.origin_member.to_string(),
        alias.path_len(),
        render_path(&alias.path),
    )
}

fn field_alias_valid(alias: &PromotedAlias) -> bool {
    alias.path.last() == Some(&alias.origin_member)
        && alias.path_len() > 1
        && alias_valid_common(alias)
}

fn method_alias_valid(alias: &PromotedAlias) -> bool {
    alias.path_len() > 0 && alias_valid_common(alias)
}

fn alias_valid_common(alias: &PromotedAlias) -> bool {
    let origin_is_canonical = !matches!(
        &alias.origin.0,
        Type::Infer | Type::InferReturn | Type::UnresolvedName(_) | Type::UnresolvedNominal { .. }
    );
    let exposure_has_span = match alias.exposure {
        Exposure::Explicit => alias.selector_span.is_some(),
        Exposure::Implicit => alias.selector_span.is_none(),
    };
    origin_is_canonical && exposure_has_span
}

fn render_path(path: &[Ident]) -> String {
    path.iter()
        .map(ToString::to_string)
        .collect::<Vec<_>>()
        .join(".")
}
