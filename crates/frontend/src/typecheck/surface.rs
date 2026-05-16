use std::collections::{HashMap, HashSet};

use super::{
    TypeChecker, TypeError,
    annotation::AccessPolicy,
    decls::{
        AggregateSchema, DeclError, DeclarationIndex, DependentEmbedTemplate, EmbedFieldSchema,
        FieldSchema, MethodKey, MethodSurface, NominalKey, ProjectionEntry, nominal_key_sort_key,
    },
    substitute_aggregate_member,
    type_ops::type_depends_on_generics,
};
use crate::{
    ast::{self, ConstArg, Ident, NominalKind, Type},
    externs::catalog::ExternCatalog,
    span::SourceSpan,
};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub(crate) struct CanonicalTypeKey(pub(crate) Type);

impl TypeChecker {
    pub(super) fn promoted_surface_for(&mut self, receiver: &Type) -> Option<PromotedSurface> {
        let key = CanonicalTypeKey(receiver.clone());
        if let Some(surface) = self.promoted_surfaces.get(&key) {
            return Some(surface.clone());
        }
        let (surface, errors) = self.decls.promoted_surface_for(receiver, &self.externs)?;
        for error in errors {
            self.push_error_once(TypeError::Decl(error));
        }
        self.promoted_surfaces.insert(key, surface.clone());
        Some(surface)
    }
}

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
    pub(crate) fn with_prefix(
        &self,
        prefix: Ident,
        exposure: Exposure,
        selector_span: Option<SourceSpan>,
    ) -> Self {
        Self {
            path: prefixed_path(prefix, &self.path),
            origin: self.origin.clone(),
            origin_member: self.origin_member,
            exposure,
            selector_span,
        }
    }

    pub(crate) fn path_len(&self) -> usize {
        self.path.len()
    }
}

fn promoted_alias(
    origin: &Type,
    path: Vec<Ident>,
    origin_member: Ident,
    exposure: Exposure,
    selector_span: Option<SourceSpan>,
) -> PromotedAlias {
    PromotedAlias {
        path,
        origin: CanonicalTypeKey(origin.clone()),
        origin_member,
        exposure,
        selector_span,
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

impl DeclarationIndex {
    pub(crate) fn build_promoted_surfaces(&mut self, externs: &ExternCatalog) -> Vec<DeclError> {
        build_dependent_embed_templates(self);
        let mut builder = SurfaceBuilder::new(self, externs);
        let mut keys = self
            .aggregates()
            .map(|(key, _)| key.clone())
            .collect::<Vec<_>>();
        keys.sort_by_key(nominal_key_sort_key);
        for key in &keys {
            builder.build(key, None);
        }
        let SurfaceBuilder {
            surfaces, errors, ..
        } = builder;
        for (key, surface) in surfaces {
            if let Some(aggregate) = self.aggregate_mut(&key) {
                aggregate.promoted = surface;
            }
        }
        errors
    }

    pub(crate) fn promoted_surface_for(
        &self,
        receiver: &Type,
        externs: &ExternCatalog,
    ) -> Option<(PromotedSurface, Vec<DeclError>)> {
        let key = self.key_for_type(receiver)?;
        let aggregate = self.aggregate(&key)?;
        if aggregate.dependent_embeds.is_empty() {
            return Some((aggregate.promoted.clone(), vec![]));
        }
        let mut builder = SurfaceBuilder::new(self, externs);
        let surface = builder.build_type(receiver, None);
        Some((surface, builder.errors))
    }
}

fn build_dependent_embed_templates(decls: &mut DeclarationIndex) {
    let mut keys = decls
        .aggregates()
        .map(|(key, _)| key.clone())
        .collect::<Vec<_>>();
    keys.sort_by_key(nominal_key_sort_key);
    for key in keys {
        let Some(schema) = decls.aggregate(&key) else {
            continue;
        };
        let mut templates = vec![];
        let mut fields = schema.fields.iter().collect::<Vec<_>>();
        fields.sort_by(|(left, _), (right, _)| left.as_str().cmp(right.as_str()));
        for (name, field) in fields {
            let Some(embed) = &field.embed else {
                continue;
            };
            if !type_depends_on_generics(&field.ty) {
                continue;
            }
            templates.push(DependentEmbedTemplate {
                field_path: vec![*name],
                target_ty: field.ty.clone(),
                selector: embed.selector.clone(),
                exposure: embed.exposure,
                span: embed.span,
            });
        }
        if let Some(schema) = decls.aggregate_mut(&key) {
            schema.dependent_embeds = templates;
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum SurfaceState {
    Visiting,
    Done,
}

struct SurfaceBuilder<'a> {
    decls: &'a DeclarationIndex,
    externs: &'a ExternCatalog,
    states: HashMap<NominalKey, SurfaceState>,
    surfaces: HashMap<NominalKey, PromotedSurface>,
    type_states: HashMap<CanonicalTypeKey, SurfaceState>,
    type_surfaces: HashMap<CanonicalTypeKey, PromotedSurface>,
    reported_cycles: HashSet<NominalKey>,
    errors: Vec<DeclError>,
}

impl<'a> SurfaceBuilder<'a> {
    fn new(decls: &'a DeclarationIndex, externs: &'a ExternCatalog) -> Self {
        Self {
            decls,
            externs,
            states: HashMap::new(),
            surfaces: HashMap::new(),
            type_states: HashMap::new(),
            type_surfaces: HashMap::new(),
            reported_cycles: HashSet::new(),
            errors: vec![],
        }
    }

    fn build(&mut self, key: &NominalKey, cycle_span: Option<SourceSpan>) -> PromotedSurface {
        match self.states.get(key).copied() {
            Some(SurfaceState::Done) => {
                return self.surfaces.get(key).cloned().unwrap_or_default();
            }
            Some(SurfaceState::Visiting) => {
                if self.reported_cycles.insert(key.clone()) {
                    self.errors.push(DeclError::EmbedSurfaceCycle {
                        owner: key.clone(),
                        target: key.clone(),
                        span: cycle_span,
                    });
                }
                return PromotedSurface::default();
            }
            None => {}
        }

        self.states.insert(key.clone(), SurfaceState::Visiting);
        let mut surface = PromotedSurface::default();
        let Some(schema) = self.decls.aggregate(key) else {
            self.states.insert(key.clone(), SurfaceState::Done);
            self.surfaces.insert(key.clone(), surface.clone());
            return surface;
        };

        let mut fields = schema.fields.iter().collect::<Vec<_>>();
        fields.sort_by(|(left, _), (right, _)| left.as_str().cmp(right.as_str()));
        for (name, field) in fields {
            self.add_embed_members(&mut surface, *name, field);
        }
        self.merge_surface(key, schema, &mut surface);
        surface.sort();
        self.states.insert(key.clone(), SurfaceState::Done);
        self.surfaces.insert(key.clone(), surface.clone());
        surface
    }

    fn build_type(&mut self, ty: &Type, cycle_span: Option<SourceSpan>) -> PromotedSurface {
        let cache_key = CanonicalTypeKey(ty.clone());
        match self.type_states.get(&cache_key).copied() {
            Some(SurfaceState::Done) => {
                return self
                    .type_surfaces
                    .get(&cache_key)
                    .cloned()
                    .unwrap_or_default();
            }
            Some(SurfaceState::Visiting) => {
                if let Some(key) = self.decls.key_for_type(ty)
                    && self.reported_cycles.insert(key.clone())
                {
                    self.errors.push(DeclError::EmbedSurfaceCycle {
                        owner: key.clone(),
                        target: key,
                        span: cycle_span,
                    });
                }
                return PromotedSurface::default();
            }
            None => {}
        }

        let Some(key) = self.decls.key_for_type(ty) else {
            return PromotedSurface::default();
        };
        let Some(schema) = self.decls.aggregate(&key) else {
            return PromotedSurface::default();
        };

        self.type_states
            .insert(cache_key.clone(), SurfaceState::Visiting);
        let mut surface = self.build(&key, cycle_span);
        for template in &schema.dependent_embeds {
            self.add_dependent_embed(&mut surface, ty, schema, template);
        }
        self.merge_surface(&key, schema, &mut surface);
        surface.sort();
        self.type_states
            .insert(cache_key.clone(), SurfaceState::Done);
        self.type_surfaces.insert(cache_key, surface.clone());
        surface
    }

    fn add_dependent_embed(
        &mut self,
        surface: &mut PromotedSurface,
        owner_ty: &Type,
        schema: &AggregateSchema,
        template: &DependentEmbedTemplate,
    ) {
        let Some(embed_name) = template.field_path.first().copied() else {
            return;
        };
        let target_ty =
            substitute_aggregate_member(owner_ty, &schema.generics, &template.target_ty);
        if !concrete_surface_type(&target_ty) {
            return;
        }
        let field = FieldSchema {
            ty: target_ty,
            has_default: false,
            policy: AccessPolicy::default(),
            span: Some(template.span),
            embed: Some(EmbedFieldSchema {
                selector: template.selector.clone(),
                exposure: template.exposure,
                as_projection: false,
                span: template.span,
            }),
        };
        self.add_embed_members(surface, embed_name, &field);
    }

    fn add_embed_members(
        &mut self,
        surface: &mut PromotedSurface,
        embed_name: Ident,
        field: &FieldSchema,
    ) {
        let Some(embed) = &field.embed else {
            return;
        };
        if !concrete_surface_type(&field.ty) {
            return;
        }

        let Some(target_key) = self.decls.key_for_type(&field.ty) else {
            if let Some(selector) = &embed.selector {
                self.add_unknown_selector_errors(field, selector);
            }
            return;
        };
        match &embed.selector {
            Some(selector) => {
                self.add_selected_members(surface, embed_name, field, &target_key, selector);
            }
            None => match target_key.kind {
                NominalKind::Struct | NominalKind::DataRef => {
                    self.add_aggregate_fields(surface, embed_name, field, &target_key);
                    self.add_aggregate_methods(surface, embed_name, field, &target_key);
                }
                NominalKind::Extern => {
                    self.add_extern_fields(surface, embed_name, field, &target_key);
                    self.add_extern_methods(surface, embed_name, field, &target_key);
                }
                NominalKind::Enum => {}
            },
        }
    }

    fn add_unknown_selector_errors(&mut self, field: &FieldSchema, selector: &ast::EmbedSelector) {
        for item in &selector.items {
            let span = SourceSpan::from_byte_span(
                field.span.expect("embedded field has span").source,
                item.span,
            );
            match item.kind {
                ast::EmbedSelectorKind::Field => {
                    self.errors.push(DeclError::UnknownEmbedFieldSelector {
                        name: item.name,
                        span: Some(span),
                    });
                }
                ast::EmbedSelectorKind::Method => {
                    self.errors.push(DeclError::UnknownEmbedMethodSelector {
                        name: item.name,
                        span: Some(span),
                    });
                }
            }
        }
    }

    fn merge_surface(
        &mut self,
        owner: &NominalKey,
        schema: &AggregateSchema,
        surface: &mut PromotedSurface,
    ) {
        surface.sort();
        self.merge_field_surface(owner, schema, surface);
        self.merge_method_surface(owner, schema, surface);
    }

    fn merge_field_surface(
        &mut self,
        owner: &NominalKey,
        schema: &AggregateSchema,
        surface: &mut PromotedSurface,
    ) {
        let direct_fields = schema.fields.keys().copied().collect::<HashSet<_>>();
        let mut names = surface.fields.keys().copied().collect::<Vec<_>>();
        names.sort_by(|left, right| left.as_str().cmp(right.as_str()));
        for name in names {
            let Some(slot) = surface.fields.remove(&name) else {
                continue;
            };
            let explicit = explicit_aliases(&slot);
            if direct_fields.contains(&name) {
                for alias in explicit {
                    self.errors.push(DeclError::EmbedFieldConflictsWithDirect {
                        owner: owner.clone(),
                        name,
                        span: alias.selector_span,
                    });
                }
                continue;
            }

            let aliases = merged_aliases(slot, |alias| {
                self.errors.push(DeclError::DuplicateExplicitEmbedField {
                    owner: owner.clone(),
                    name,
                    span: alias.selector_span,
                });
            });
            insert_merged_aliases(&mut surface.fields, name, aliases);
        }
    }

    fn merge_method_surface(
        &mut self,
        owner: &NominalKey,
        schema: &AggregateSchema,
        surface: &mut PromotedSurface,
    ) {
        let direct_methods = schema
            .methods
            .keys()
            .filter(|key| key.surface == MethodSurface::Instance)
            .map(|key| key.name)
            .collect::<HashSet<_>>();
        let mut names = surface.methods.keys().copied().collect::<Vec<_>>();
        names.sort_by(|left, right| left.as_str().cmp(right.as_str()));
        for name in names {
            let Some(slot) = surface.methods.remove(&name) else {
                continue;
            };
            let explicit = explicit_aliases(&slot);
            if direct_methods.contains(&name) {
                for alias in explicit {
                    self.errors.push(DeclError::EmbedMethodConflictsWithDirect {
                        owner: owner.clone(),
                        name,
                        span: alias.selector_span,
                    });
                }
                continue;
            }

            let aliases = merged_aliases(slot, |alias| {
                self.errors.push(DeclError::DuplicateExplicitEmbedMethod {
                    owner: owner.clone(),
                    name,
                    span: alias.selector_span,
                });
            });
            insert_merged_aliases(&mut surface.methods, name, aliases);
        }
    }

    fn add_selected_members(
        &mut self,
        surface: &mut PromotedSurface,
        embed_name: Ident,
        field: &FieldSchema,
        target_key: &NominalKey,
        selector: &ast::EmbedSelector,
    ) {
        for item in &selector.items {
            let exposed = item.alias.unwrap_or(item.name);
            match item.kind {
                ast::EmbedSelectorKind::Field => {
                    if let Some(alias) =
                        self.selected_field_alias(embed_name, field, target_key, item)
                    {
                        surface.insert_field(exposed, alias);
                    }
                }
                ast::EmbedSelectorKind::Method => {
                    if let Some(alias) =
                        self.selected_method_alias(embed_name, field, target_key, item)
                    {
                        surface.insert_method(exposed, alias);
                    }
                }
            }
        }
    }

    fn selected_field_alias(
        &mut self,
        embed_name: Ident,
        field: &FieldSchema,
        target_key: &NominalKey,
        item: &ast::EmbedSelectorItem,
    ) -> Option<PromotedFieldAlias> {
        let span = SourceSpan::from_byte_span(
            field.span.expect("embedded field has span").source,
            item.span,
        );
        match target_key.kind {
            NominalKind::Struct | NominalKind::DataRef => {
                self.selected_aggregate_field_alias(embed_name, field, target_key, item, span)
            }
            NominalKind::Extern => {
                self.selected_extern_field_alias(embed_name, field, target_key, item, span)
            }
            NominalKind::Enum => {
                self.errors.push(DeclError::UnknownEmbedFieldSelector {
                    name: item.name,
                    span: Some(span),
                });
                None
            }
        }
    }

    fn selected_aggregate_field_alias(
        &mut self,
        embed_name: Ident,
        field: &FieldSchema,
        target_key: &NominalKey,
        item: &ast::EmbedSelectorItem,
        span: SourceSpan,
    ) -> Option<PromotedFieldAlias> {
        let target_surface = self.build_type(&field.ty, field.span);
        let Some(target) = self.decls.aggregate(target_key) else {
            self.errors.push(DeclError::UnknownEmbedFieldSelector {
                name: item.name,
                span: Some(span),
            });
            return None;
        };
        if target.fields.contains_key(&item.name) {
            return Some(promoted_alias(
                &field.ty,
                vec![embed_name, item.name],
                item.name,
                Exposure::Explicit,
                Some(span),
            ));
        }
        if let Some(slot) = target_surface.fields.get(&item.name) {
            if slot.ambiguous || slot.aliases.len() != 1 {
                self.errors.push(DeclError::AmbiguousEmbedFieldSelector {
                    name: item.name,
                    span: Some(span),
                });
                return None;
            }
            return Some(slot.aliases[0].with_prefix(embed_name, Exposure::Explicit, Some(span)));
        }
        if aggregate_has_method_name(target, &target_surface, item.name) {
            self.errors.push(DeclError::EmbedFieldSelectorNamesMethod {
                name: item.name,
                span: Some(span),
            });
            return None;
        }
        self.errors.push(DeclError::UnknownEmbedFieldSelector {
            name: item.name,
            span: Some(span),
        });
        None
    }

    fn selected_extern_field_alias(
        &mut self,
        embed_name: Ident,
        field: &FieldSchema,
        target_key: &NominalKey,
        item: &ast::EmbedSelectorItem,
        span: SourceSpan,
    ) -> Option<PromotedFieldAlias> {
        let Some(owner) = self.externs.type_by_nominal(target_key) else {
            self.errors.push(DeclError::UnknownEmbedFieldSelector {
                name: item.name,
                span: Some(span),
            });
            return None;
        };
        if self.externs.field(owner, item.name).is_some() {
            return Some(promoted_alias(
                &field.ty,
                vec![embed_name, item.name],
                item.name,
                Exposure::Explicit,
                Some(span),
            ));
        }
        if self.externs.method(owner, item.name).is_some()
            || self.externs.static_method(owner, item.name).is_some()
        {
            self.errors.push(DeclError::EmbedFieldSelectorNamesMethod {
                name: item.name,
                span: Some(span),
            });
            return None;
        }
        self.errors.push(DeclError::UnknownEmbedFieldSelector {
            name: item.name,
            span: Some(span),
        });
        None
    }

    fn selected_method_alias(
        &mut self,
        embed_name: Ident,
        field: &FieldSchema,
        target_key: &NominalKey,
        item: &ast::EmbedSelectorItem,
    ) -> Option<PromotedMethodAlias> {
        let span = SourceSpan::from_byte_span(
            field.span.expect("embedded field has span").source,
            item.span,
        );
        if item.name == to_string_ident() {
            self.errors
                .push(DeclError::EmbedMethodSelectorNamesToString { span: Some(span) });
            return None;
        }
        match target_key.kind {
            NominalKind::Struct | NominalKind::DataRef => {
                self.selected_aggregate_method_alias(embed_name, field, target_key, item, span)
            }
            NominalKind::Extern => {
                self.selected_extern_method_alias(embed_name, field, target_key, item, span)
            }
            NominalKind::Enum => {
                self.errors.push(DeclError::UnknownEmbedMethodSelector {
                    name: item.name,
                    span: Some(span),
                });
                None
            }
        }
    }

    fn selected_aggregate_method_alias(
        &mut self,
        embed_name: Ident,
        field: &FieldSchema,
        target_key: &NominalKey,
        item: &ast::EmbedSelectorItem,
        span: SourceSpan,
    ) -> Option<PromotedMethodAlias> {
        let target_surface = self.build_type(&field.ty, field.span);
        let Some(target) = self.decls.aggregate(target_key) else {
            self.errors.push(DeclError::UnknownEmbedMethodSelector {
                name: item.name,
                span: Some(span),
            });
            return None;
        };
        if target.methods.contains_key(&MethodKey::instance(item.name)) {
            return Some(promoted_alias(
                &field.ty,
                vec![embed_name],
                item.name,
                Exposure::Explicit,
                Some(span),
            ));
        }
        if let Some(slot) = target_surface.methods.get(&item.name) {
            if slot.ambiguous || slot.aliases.len() != 1 {
                self.errors.push(DeclError::AmbiguousEmbedMethodSelector {
                    name: item.name,
                    span: Some(span),
                });
                return None;
            }
            return Some(slot.aliases[0].with_prefix(embed_name, Exposure::Explicit, Some(span)));
        }
        if target.methods.contains_key(&MethodKey::static_(item.name)) {
            self.errors.push(DeclError::EmbedMethodSelectorNamesStatic {
                name: item.name,
                span: Some(span),
            });
            return None;
        }
        if aggregate_has_field_name(target, &target_surface, item.name) {
            self.errors.push(DeclError::EmbedMethodSelectorNamesField {
                name: item.name,
                span: Some(span),
            });
            return None;
        }
        self.errors.push(DeclError::UnknownEmbedMethodSelector {
            name: item.name,
            span: Some(span),
        });
        None
    }

    fn selected_extern_method_alias(
        &mut self,
        embed_name: Ident,
        field: &FieldSchema,
        target_key: &NominalKey,
        item: &ast::EmbedSelectorItem,
        span: SourceSpan,
    ) -> Option<PromotedMethodAlias> {
        let Some(owner) = self.externs.type_by_nominal(target_key) else {
            self.errors.push(DeclError::UnknownEmbedMethodSelector {
                name: item.name,
                span: Some(span),
            });
            return None;
        };
        if self.externs.method(owner, item.name).is_some() {
            return Some(promoted_alias(
                &field.ty,
                vec![embed_name],
                item.name,
                Exposure::Explicit,
                Some(span),
            ));
        }
        if self.externs.static_method(owner, item.name).is_some() {
            self.errors.push(DeclError::EmbedMethodSelectorNamesStatic {
                name: item.name,
                span: Some(span),
            });
            return None;
        }
        if self.externs.field(owner, item.name).is_some() {
            self.errors.push(DeclError::EmbedMethodSelectorNamesField {
                name: item.name,
                span: Some(span),
            });
            return None;
        }
        self.errors.push(DeclError::UnknownEmbedMethodSelector {
            name: item.name,
            span: Some(span),
        });
        None
    }

    fn add_aggregate_fields(
        &mut self,
        surface: &mut PromotedSurface,
        embed_name: Ident,
        field: &FieldSchema,
        target_key: &NominalKey,
    ) {
        let target_surface = self.build_type(&field.ty, field.span);
        let Some(target) = self.decls.aggregate(target_key) else {
            return;
        };

        let mut direct_fields = target.fields.keys().copied().collect::<Vec<_>>();
        direct_fields.sort_by(|left, right| left.as_str().cmp(right.as_str()));
        for name in direct_fields {
            let alias = promoted_alias(
                &field.ty,
                vec![embed_name, name],
                name,
                Exposure::Implicit,
                None,
            );
            surface.insert_field(name, alias);
        }

        for (name, slot) in target_surface.fields {
            for alias in slot.aliases {
                surface.insert_field(
                    name,
                    alias.with_prefix(embed_name, Exposure::Implicit, None),
                );
            }
        }
    }

    fn add_aggregate_methods(
        &mut self,
        surface: &mut PromotedSurface,
        embed_name: Ident,
        field: &FieldSchema,
        target_key: &NominalKey,
    ) {
        let target_surface = self.build_type(&field.ty, field.span);
        let Some(target) = self.decls.aggregate(target_key) else {
            return;
        };

        let mut direct_methods = target
            .methods
            .keys()
            .filter(|key| key.surface == MethodSurface::Instance && key.name != to_string_ident())
            .map(|key| key.name)
            .collect::<Vec<_>>();
        direct_methods.sort_by(|left, right| left.as_str().cmp(right.as_str()));
        for name in direct_methods {
            let alias = promoted_alias(&field.ty, vec![embed_name], name, Exposure::Implicit, None);
            surface.insert_method(name, alias);
        }

        for (name, slot) in target_surface.methods {
            for alias in slot.aliases {
                surface.insert_method(
                    name,
                    alias.with_prefix(embed_name, Exposure::Implicit, None),
                );
            }
        }
    }

    fn add_extern_fields(
        &self,
        surface: &mut PromotedSurface,
        embed_name: Ident,
        field: &FieldSchema,
        target_key: &NominalKey,
    ) {
        let Some(owner) = self.externs.type_by_nominal(target_key) else {
            return;
        };
        let mut fields = self.externs.ty(owner).fields.iter().collect::<Vec<_>>();
        fields.sort_by(|left, right| left.name.as_str().cmp(right.name.as_str()));
        for extern_field in fields {
            let name = extern_field.name;
            let alias = promoted_alias(
                &field.ty,
                vec![embed_name, name],
                name,
                Exposure::Implicit,
                None,
            );
            surface.insert_field(name, alias);
        }
    }

    fn add_extern_methods(
        &self,
        surface: &mut PromotedSurface,
        embed_name: Ident,
        field: &FieldSchema,
        target_key: &NominalKey,
    ) {
        let Some(owner) = self.externs.type_by_nominal(target_key) else {
            return;
        };
        let mut methods = self.externs.ty(owner).methods.iter().collect::<Vec<_>>();
        methods.sort_by(|left, right| left.name.as_str().cmp(right.name.as_str()));
        for extern_method in methods {
            let name = extern_method.name;
            if name == to_string_ident() {
                continue;
            }
            let alias = promoted_alias(&field.ty, vec![embed_name], name, Exposure::Implicit, None);
            surface.insert_method(name, alias);
        }
    }
}

fn explicit_aliases(slot: &SurfaceSlot<PromotedAlias>) -> Vec<PromotedAlias> {
    slot.aliases
        .iter()
        .filter(|alias| alias.exposure == Exposure::Explicit)
        .cloned()
        .collect()
}

fn merged_aliases(
    slot: SurfaceSlot<PromotedAlias>,
    mut duplicate_explicit: impl FnMut(&PromotedAlias),
) -> Vec<PromotedAlias> {
    let explicit = explicit_aliases(&slot);
    match explicit.len() {
        0 => shortest_implicit_aliases(slot.aliases),
        1 => explicit,
        _ => {
            for alias in explicit.iter().skip(1) {
                duplicate_explicit(alias);
            }
            explicit
        }
    }
}

fn insert_merged_aliases(
    map: &mut HashMap<Ident, SurfaceSlot<PromotedAlias>>,
    name: Ident,
    aliases: Vec<PromotedAlias>,
) {
    if !aliases.is_empty() {
        map.insert(
            name,
            SurfaceSlot {
                ambiguous: aliases.len() > 1,
                aliases,
            },
        );
    }
}

fn shortest_implicit_aliases(aliases: Vec<PromotedAlias>) -> Vec<PromotedAlias> {
    let Some(path_len) = aliases.iter().map(PromotedAlias::path_len).min() else {
        return vec![];
    };
    aliases
        .into_iter()
        .filter(|alias| alias.path_len() == path_len)
        .collect()
}

fn aggregate_has_field_name(
    aggregate: &AggregateSchema,
    surface: &PromotedSurface,
    name: Ident,
) -> bool {
    aggregate.fields.contains_key(&name) || surface.fields.contains_key(&name)
}

fn aggregate_has_method_name(
    aggregate: &AggregateSchema,
    surface: &PromotedSurface,
    name: Ident,
) -> bool {
    aggregate.methods.keys().any(|key| key.name == name) || surface.methods.contains_key(&name)
}

fn to_string_ident() -> Ident {
    Ident::new("to_string")
}

pub(super) fn dependent_embed_template_valid(template: &DependentEmbedTemplate) -> bool {
    template.field_path.len() == 1
        && type_depends_on_generics(&template.target_ty)
        && template.span.span.start <= template.span.span.end
        && template.exposure == Exposure::from_selector(template.selector.is_some())
}

pub(super) fn projection_entry_valid(entry: &ProjectionEntry) -> bool {
    entry.target.0 == entry.target_ty
        && entry.field_path.len() == 1
        && entry.field_span.span.start <= entry.field_span.span.end
}

fn concrete_surface_type(ty: &Type) -> bool {
    match ty {
        Type::Infer
        | Type::InferReturn
        | Type::Var(_)
        | Type::UnresolvedName(_)
        | Type::UnresolvedNominal { .. } => false,
        Type::Func { params, ret } => {
            params.iter().all(|param| concrete_surface_type(&param.ty))
                && concrete_surface_type(&ret.ty)
        }
        Type::Dyn(_) => true,
        Type::Tuple(elems) => elems.iter().all(concrete_surface_type),
        Type::Nominal(nominal) => {
            nominal.type_args.iter().all(concrete_surface_type)
                && nominal.const_args.iter().all(concrete_const_arg)
        }
        Type::List { elem } | Type::Slice { elem } | Type::Array { elem, .. } => {
            concrete_surface_type(elem)
        }
        Type::Map { key, value } => concrete_surface_type(key) && concrete_surface_type(value),
        Type::Any | Type::Int | Type::Float | Type::Bool | Type::String | Type::Void => true,
    }
}

fn concrete_const_arg(arg: &ConstArg) -> bool {
    matches!(arg, ConstArg::Value(_))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        ast::{Ident, NominalKind, Program, Type},
        test_support::{parse_program, resolved_modules},
        typecheck::{
            DeclarationIndex, DependentEmbedTemplate, ModuleScope, ProjectionEntry,
            TypecheckConfig, typechecker_for_modules,
        },
    };

    fn ident(name: &str) -> Ident {
        Ident::new(name)
    }

    fn parse(source: &str) -> Program {
        parse_program(source)
    }

    fn checked_index(root: &str, modules: &[(&str, &str)]) -> DeclarationIndex {
        let root = parse(root);
        let resolved = resolved_modules(&root, modules);
        let externs = crate::externs::collect_source_externs(&root, &resolved).unwrap();
        let mut tc = typechecker_for_modules(&root, &resolved, externs, TypecheckConfig::default())
            .expect("typecheck failed");
        tc.finish().expect("typecheck failed");
        tc.decls.clone()
    }

    fn promoted_slot<'a>(
        index: &'a DeclarationIndex,
        owner: &str,
        field: &str,
    ) -> Option<&'a SurfaceSlot<PromotedFieldAlias>> {
        let key = index
            .local_nominal_type(&ModuleScope::Root, ident(owner))
            .expect("missing owner");
        let aggregate = index.aggregate(&key).expect("missing aggregate");
        aggregate.promoted.fields.get(&ident(field))
    }

    fn promoted_paths(index: &DeclarationIndex, owner: &str, field: &str) -> Vec<Vec<Ident>> {
        promoted_slot(index, owner, field)
            .map(|slot| {
                slot.aliases
                    .iter()
                    .map(|alias| alias.path.clone())
                    .collect()
            })
            .unwrap_or_default()
    }

    fn promoted_method_slot<'a>(
        index: &'a DeclarationIndex,
        owner: &str,
        method: &str,
    ) -> Option<&'a SurfaceSlot<PromotedMethodAlias>> {
        let key = index
            .local_nominal_type(&ModuleScope::Root, ident(owner))
            .expect("missing owner");
        let aggregate = index.aggregate(&key).expect("missing aggregate");
        aggregate.promoted.methods.get(&ident(method))
    }

    fn promoted_method_paths(
        index: &DeclarationIndex,
        owner: &str,
        method: &str,
    ) -> Vec<Vec<Ident>> {
        promoted_method_slot(index, owner, method)
            .map(|slot| {
                slot.aliases
                    .iter()
                    .map(|alias| alias.path.clone())
                    .collect()
            })
            .unwrap_or_default()
    }

    fn projections(index: &DeclarationIndex, owner: &str) -> Vec<ProjectionEntry> {
        let key = index
            .local_nominal_type(&ModuleScope::Root, ident(owner))
            .expect("missing owner");
        let aggregate = index.aggregate(&key).expect("missing aggregate");
        aggregate.projections.clone()
    }

    fn dependent_embeds(index: &DeclarationIndex, owner: &str) -> Vec<DependentEmbedTemplate> {
        let key = index
            .local_nominal_type(&ModuleScope::Root, ident(owner))
            .expect("missing owner");
        let aggregate = index.aggregate(&key).expect("missing aggregate");
        aggregate.dependent_embeds.clone()
    }

    #[test]
    fn bare_embed_builds_field_surface() {
        let index = checked_index(
            "struct Health { hp: int, max_hp: int } struct Enemy { embed health: Health }",
            &[],
        );

        assert_eq!(
            promoted_paths(&index, "Enemy", "hp"),
            vec![vec![ident("health"), ident("hp")]]
        );
        assert_eq!(
            promoted_paths(&index, "Enemy", "max_hp"),
            vec![vec![ident("health"), ident("max_hp")]]
        );
    }

    #[test]
    fn bare_embed_imports_transitive_field_surface() {
        let index = checked_index(
            "struct Health { hp: int } struct Actor { embed health: Health } struct Enemy { embed actor: Actor }",
            &[],
        );

        assert_eq!(
            promoted_paths(&index, "Enemy", "health"),
            vec![vec![ident("actor"), ident("health")]]
        );
        assert_eq!(
            promoted_paths(&index, "Enemy", "hp"),
            vec![vec![ident("actor"), ident("health"), ident("hp")]]
        );
    }

    #[test]
    fn bare_embed_imports_extern_fields() {
        let index = checked_index(
            "extern type Point { x: float; y: float; } struct Enemy { embed point: Point }",
            &[],
        );

        assert_eq!(
            promoted_paths(&index, "Enemy", "x"),
            vec![vec![ident("point"), ident("x")]]
        );
        assert_eq!(
            promoted_paths(&index, "Enemy", "y"),
            vec![vec![ident("point"), ident("y")]]
        );
    }

    #[test]
    fn bare_embed_imports_method_surface() {
        let index = checked_index(
            "struct Health { fn damage(self) {} fn to_string(self) -> string { \"hp\" } } struct Enemy { embed health: Health }",
            &[],
        );

        assert_eq!(
            promoted_method_paths(&index, "Enemy", "damage"),
            vec![vec![ident("health")]]
        );
        assert!(promoted_method_paths(&index, "Enemy", "to_string").is_empty());
    }

    #[test]
    fn bare_embed_imports_transitive_method_surface() {
        let index = checked_index(
            "struct Health { fn damage(self) {} } struct Actor { embed health: Health } struct Enemy { embed actor: Actor }",
            &[],
        );

        assert_eq!(
            promoted_method_paths(&index, "Enemy", "damage"),
            vec![vec![ident("actor"), ident("health")]]
        );
    }

    #[test]
    fn bare_embed_imports_extern_methods() {
        let index = checked_index(
            "extern type Host { fn damage(self, amount: int); } struct Enemy { embed host: Host }",
            &[],
        );

        assert_eq!(
            promoted_method_paths(&index, "Enemy", "damage"),
            vec![vec![ident("host")]]
        );
    }

    #[test]
    fn as_embed_builds_projection_entry() {
        let index = checked_index(
            "struct Entity { id: int } struct Enemy { @as embed entity: Entity }",
            &[],
        );
        let projections = projections(&index, "Enemy");

        assert_eq!(projections.len(), 1);
        assert_eq!(projections[0].field_path, vec![ident("entity")]);
        assert_eq!(
            projections[0].target_ty,
            Type::nominal(NominalKind::Struct, ident("Entity"), vec![], vec![], None)
        );
    }

    #[test]
    fn as_embed_allows_distinct_projection_targets() {
        let index = checked_index(
            "struct Entity { id: int } struct Body { mass: int } struct Enemy { @as embed entity: Entity, @as embed body: Body }",
            &[],
        );

        assert_eq!(projections(&index, "Enemy").len(), 2);
    }

    #[test]
    fn generic_embed_target_records_dependent_template_only() {
        let index = checked_index("struct Box<T> { embed value: T }", &[]);
        let templates = dependent_embeds(&index, "Box");

        assert!(promoted_paths(&index, "Box", "value").is_empty());
        assert!(promoted_method_paths(&index, "Box", "value").is_empty());
        assert_eq!(templates.len(), 1);
        assert_eq!(templates[0].field_path, vec![ident("value")]);
        assert!(matches!(templates[0].target_ty, Type::Var(_)));
        assert!(templates[0].selector.is_none());
        assert_eq!(templates[0].exposure, Exposure::Implicit);
    }

    #[test]
    fn generic_embed_selector_records_dependent_template_only() {
        let index = checked_index("struct Box<T> { embed value: T { x as y } }", &[]);
        let templates = dependent_embeds(&index, "Box");

        assert!(promoted_paths(&index, "Box", "y").is_empty());
        assert_eq!(templates.len(), 1);
        assert_eq!(templates[0].field_path, vec![ident("value")]);
        assert!(templates[0].selector.is_some());
        assert_eq!(templates[0].exposure, Exposure::Explicit);
    }

    #[test]
    fn concrete_embed_of_generic_target_uses_dependent_target_surface() {
        let index = checked_index(
            "struct Health { hp: int }
            struct Box<T> { embed value: T }
            struct Enemy { embed health: Box<Health> }",
            &[],
        );

        assert_eq!(
            promoted_paths(&index, "Enemy", "hp"),
            vec![vec![ident("health"), ident("value"), ident("hp")]]
        );
    }

    #[test]
    fn selector_builds_explicit_field_surface() {
        let index = checked_index(
            "struct Health { hp: int, max_hp: int } struct Enemy { embed health: Health { hp as health_hp } }",
            &[],
        );

        assert_eq!(
            promoted_paths(&index, "Enemy", "health_hp"),
            vec![vec![ident("health"), ident("hp")]]
        );
        assert!(promoted_paths(&index, "Enemy", "max_hp").is_empty());
    }

    #[test]
    fn selector_can_import_target_promoted_field() {
        let index = checked_index(
            "struct Health { hp: int } struct Actor { embed health: Health } struct Enemy { embed actor: Actor { hp as enemy_hp } }",
            &[],
        );

        assert_eq!(
            promoted_paths(&index, "Enemy", "enemy_hp"),
            vec![vec![ident("actor"), ident("health"), ident("hp")]]
        );
    }

    #[test]
    fn selector_builds_explicit_method_surface() {
        let index = checked_index(
            "struct Health { fn damage(self) {} } struct Enemy { embed health: Health { fn damage as hit } }",
            &[],
        );

        assert_eq!(
            promoted_method_paths(&index, "Enemy", "hit"),
            vec![vec![ident("health")]]
        );
        assert!(promoted_method_paths(&index, "Enemy", "damage").is_empty());
    }

    #[test]
    fn selector_can_import_target_promoted_method() {
        let index = checked_index(
            "struct Health { fn damage(self) {} } struct Actor { embed health: Health } struct Enemy { embed actor: Actor { fn damage as hit } }",
            &[],
        );

        assert_eq!(
            promoted_method_paths(&index, "Enemy", "hit"),
            vec![vec![ident("actor"), ident("health")]]
        );
    }

    #[test]
    fn same_length_implicit_fields_remain_ambiguous() {
        let index = checked_index(
            "struct A { x: int } struct B { x: int } struct Enemy { embed a: A, embed b: B }",
            &[],
        );
        let slot = promoted_slot(&index, "Enemy", "x").expect("missing promoted field");

        assert!(slot.ambiguous);
        assert_eq!(slot.aliases.len(), 2);
    }

    #[test]
    fn same_length_implicit_methods_remain_ambiguous() {
        let index = checked_index(
            "struct A { fn tick(self) {} } struct B { fn tick(self) {} } struct Enemy { embed a: A, embed b: B }",
            &[],
        );
        let slot = promoted_method_slot(&index, "Enemy", "tick").expect("missing promoted method");

        assert!(slot.ambiguous);
        assert_eq!(slot.aliases.len(), 2);
    }
}
