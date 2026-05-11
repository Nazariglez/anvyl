use super::{
    CheckedType, Exposure, ExternUseTarget, MemberAccessKind, MemberPathFact, MemberPathKind,
    TypeChecker, TypeError, ValueDecl, check_expr_checked, check_index_access,
    check_tuple_index_access, member,
};
use crate::{
    ast::{ExprId, ExprKind, ExprNode, Ident, Type},
    externs::catalog::ExternFieldRef,
    span::Span,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum PlaceAccess {
    Mutable,
    Settable,
    Immutable,
    Const,
    Captured,
    ReadonlySelf,
    NotPlace,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) enum PlaceIdentity {
    Unknown,
    Single(PlacePath),
    Alternatives {
        group: AliasAltGroupId,
        alternatives: Vec<PlaceIdentity>,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) struct AliasAltGroupId(u32);

#[derive(Clone)]
pub(super) struct AliasTarget {
    pub(super) access: PlaceAccess,
    pub(super) identity: PlaceIdentity,
    pub(super) facts: PlaceUseFacts,
    pub(super) accepts_extern_any: bool,
}

impl Default for AliasTarget {
    fn default() -> Self {
        Self {
            access: PlaceAccess::NotPlace,
            identity: PlaceIdentity::unknown(),
            facts: PlaceUseFacts::default(),
            accepts_extern_any: false,
        }
    }
}

impl AliasTarget {
    pub(super) fn merged(group: Option<AliasAltGroupId>, targets: Vec<Self>) -> Self {
        debug_assert!(!targets.is_empty());
        if targets.len() <= 1 {
            return targets.into_iter().next().unwrap_or_default();
        }
        let Some(group) = group else {
            debug_assert!(false);
            return Self::default();
        };
        let access = merged_access(targets.iter().map(|target| target.access));
        let facts = PlaceUseFacts::merged(targets.iter().map(|target| &target.facts));
        let accepts_extern_any = targets.iter().any(|target| target.accepts_extern_any);
        let alternatives = targets.into_iter().map(|target| target.identity).collect();
        Self {
            access,
            identity: PlaceIdentity::alternatives(group, alternatives),
            facts,
            accepts_extern_any,
        }
    }
}

impl AliasAltGroupId {
    pub(super) fn new(id: u32) -> Self {
        Self(id)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) struct PlacePath {
    root: Ident,
    segments: Vec<PlacePathSegment>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) enum PlacePathSegment {
    Field(Ident),
    Tuple(usize),
    Variant(Ident),
    Index,
}

impl PlaceIdentity {
    pub(super) fn unknown() -> Self {
        Self::Unknown
    }

    pub(super) fn root(name: Ident) -> Self {
        Self::Single(PlacePath::root(name))
    }

    pub(super) fn field(self, field: Ident) -> Self {
        self.project(&|path| path.field(field))
    }

    pub(super) fn fields(mut self, fields: &[Ident]) -> Self {
        for field in fields {
            self = self.field(*field);
        }
        self
    }

    pub(super) fn tuple(self, index: usize) -> Self {
        self.project(&|path| path.tuple(index))
    }

    pub(super) fn variant(self, variant: Ident) -> Self {
        self.project(&|path| path.variant(variant))
    }

    pub(super) fn index(self) -> Self {
        self.project(&PlacePath::index)
    }

    pub(super) fn alternatives(group: AliasAltGroupId, alternatives: Vec<Self>) -> Self {
        debug_assert!(!alternatives.is_empty());
        if alternatives.is_empty() {
            return Self::Unknown;
        }
        Self::Alternatives {
            group,
            alternatives,
        }
    }

    pub(super) fn conflicts_with(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Unknown, _) | (_, Self::Unknown) => true,
            (Self::Single(left), Self::Single(right)) => left.conflicts_with(right),
            (Self::Single(_), Self::Alternatives { alternatives, .. }) => {
                alternatives.iter().any(|alt| self.conflicts_with(alt))
            }
            (Self::Alternatives { alternatives, .. }, Self::Single(_)) => {
                alternatives.iter().any(|alt| alt.conflicts_with(other))
            }
            (
                Self::Alternatives {
                    group: left_group,
                    alternatives: left,
                },
                Self::Alternatives {
                    group: right_group,
                    alternatives: right,
                },
            ) => alternatives_conflict(*left_group, left, *right_group, right),
        }
    }

    fn project(self, project_path: &impl Fn(PlacePath) -> PlacePath) -> Self {
        match self {
            Self::Unknown => Self::Unknown,
            Self::Single(path) => Self::Single(project_path(path)),
            Self::Alternatives {
                group,
                alternatives,
            } => Self::alternatives(
                group,
                alternatives
                    .into_iter()
                    .map(|identity| identity.project(project_path))
                    .collect(),
            ),
        }
    }
}

fn alternatives_conflict(
    left_group: AliasAltGroupId,
    left: &[PlaceIdentity],
    right_group: AliasAltGroupId,
    right: &[PlaceIdentity],
) -> bool {
    if left_group != right_group {
        return left
            .iter()
            .any(|left| right.iter().any(|right| left.conflicts_with(right)));
    }
    if left.len() != right.len() {
        return true;
    }
    left.iter()
        .zip(right)
        .any(|(left, right)| left.conflicts_with(right))
}

impl PlacePath {
    pub(super) fn root(root: Ident) -> Self {
        Self {
            root,
            segments: vec![],
        }
    }

    pub(super) fn field(mut self, field: Ident) -> Self {
        self.segments.push(PlacePathSegment::Field(field));
        self
    }

    pub(super) fn tuple(mut self, index: usize) -> Self {
        self.segments.push(PlacePathSegment::Tuple(index));
        self
    }

    pub(super) fn variant(mut self, variant: Ident) -> Self {
        self.segments.push(PlacePathSegment::Variant(variant));
        self
    }

    pub(super) fn index(mut self) -> Self {
        self.segments.push(PlacePathSegment::Index);
        self
    }

    pub(super) fn conflicts_with(&self, other: &Self) -> bool {
        self.root == other.root
            && (self.segments.starts_with(&other.segments)
                || other.segments.starts_with(&self.segments))
    }
}

impl PlaceAccess {
    pub(super) fn can_assign(self) -> bool {
        matches!(self, Self::Mutable | Self::Settable)
    }

    pub(super) fn can_mut_borrow(self) -> bool {
        matches!(self, Self::Mutable)
    }

    pub(super) fn assign_error(
        self,
        name: Ident,
        span: Option<crate::span::SourceSpan>,
    ) -> Option<TypeError> {
        match self {
            Self::Mutable | Self::Settable => None,
            Self::Const => Some(TypeError::ConstAssignment { name, span }),
            Self::Captured => Some(TypeError::CannotMutateCapturedVariable { name, span }),
            Self::ReadonlySelf => Some(TypeError::ReadonlyMethodMutation { span }),
            Self::Immutable | Self::NotPlace => Some(TypeError::ImmutableAssignment { name, span }),
        }
    }

    pub(super) fn mut_borrow_error(
        self,
        name: Ident,
        span: Option<crate::span::SourceSpan>,
    ) -> Option<TypeError> {
        match self {
            Self::Mutable => None,
            Self::Settable => Some(TypeError::RequiresMutablePlace { name, span }),
            Self::Const => Some(TypeError::ConstAssignment { name, span }),
            Self::Captured => Some(TypeError::CannotMutateCapturedVariable { name, span }),
            Self::ReadonlySelf => Some(TypeError::ReadonlyMethodMutation { span }),
            Self::Immutable | Self::NotPlace => Some(TypeError::ImmutableAssignment { name, span }),
        }
    }
}

fn merged_access(accesses: impl IntoIterator<Item = PlaceAccess>) -> PlaceAccess {
    let accesses: Vec<_> = accesses.into_iter().collect();
    debug_assert!(!accesses.is_empty());
    if accesses.is_empty() {
        return PlaceAccess::NotPlace;
    }
    if accesses
        .iter()
        .all(|access| *access == PlaceAccess::Mutable)
    {
        return PlaceAccess::Mutable;
    }
    if accesses.iter().all(|access| access.can_assign()) {
        return PlaceAccess::Settable;
    }
    for restricted in [
        PlaceAccess::Captured,
        PlaceAccess::ReadonlySelf,
        PlaceAccess::Const,
        PlaceAccess::NotPlace,
    ] {
        if accesses.contains(&restricted) {
            return restricted;
        }
    }
    PlaceAccess::Immutable
}

#[derive(Debug, Clone, Default)]
pub(super) struct PlaceUseFacts {
    prefix_reads: Vec<ExternFieldRef>,
    targets: Vec<ExternFieldRef>,
}

impl PlaceUseFacts {
    pub(super) fn for_extern_field(receiver: &Self, field_ref: ExternFieldRef) -> Self {
        let mut facts = Self::default();
        let prefix_reads = receiver
            .prefix_reads
            .iter()
            .chain(&receiver.targets)
            .copied();
        extend_unique(&mut facts.prefix_reads, prefix_reads);
        push_unique(&mut facts.targets, field_ref);
        facts
    }

    pub(super) fn merged<'a>(items: impl IntoIterator<Item = &'a Self>) -> Self {
        let mut facts = Self::default();
        for item in items {
            extend_unique(&mut facts.prefix_reads, item.prefix_reads.iter().copied());
            extend_unique(&mut facts.targets, item.targets.iter().copied());
        }
        facts
    }
}

fn extend_unique(items: &mut Vec<ExternFieldRef>, refs: impl IntoIterator<Item = ExternFieldRef>) {
    for item in refs {
        push_unique(items, item);
    }
}

fn push_unique(items: &mut Vec<ExternFieldRef>, item: ExternFieldRef) {
    if !items.contains(&item) {
        items.push(item);
    }
}

#[derive(Clone)]
pub(super) struct PlaceValue {
    pub(super) checked: CheckedType,
    pub(super) access: PlaceAccess,
    pub(super) facts: PlaceUseFacts,
    pub(super) identity: PlaceIdentity,
}

pub(super) struct CheckedPlace {
    pub(super) value: PlaceValue,
    accepts_extern_any: bool,
}

impl PlaceValue {
    pub(super) fn new(checked: CheckedType, access: PlaceAccess, facts: PlaceUseFacts) -> Self {
        Self {
            checked,
            access,
            facts,
            identity: PlaceIdentity::unknown(),
        }
    }

    pub(super) fn not_place(checked: CheckedType) -> Self {
        Self::new(checked, PlaceAccess::NotPlace, PlaceUseFacts::default())
    }
}

pub(super) enum FieldValueResult {
    Value(PlaceValue, bool),
    StaticOnValue(Type),
    NonAggregate(Type),
    Error,
}

pub(super) fn field_value(
    expr: Option<&ExprNode>,
    receiver: &PlaceValue,
    field_id: ExprId,
    name: Ident,
    span: Span,
    tc: &mut TypeChecker,
) -> FieldValueResult {
    match member::resolve_field(&receiver.checked.ty, name, receiver.access, tc) {
        member::FieldResolution::Direct(field) => {
            tc.check_access_policy(
                &field.policy,
                MemberAccessKind::Field,
                name,
                &receiver.checked.ty,
                &field.origin,
                span,
            );
            let mut value = PlaceValue::new(
                field_checked(expr, field.ty, false, tc),
                projected_field_access(receiver.access),
                receiver.facts.clone(),
            );
            value.identity = receiver.identity.clone().field(name);
            FieldValueResult::Value(value, false)
        }
        member::FieldResolution::Promoted(promoted) => {
            promoted_field_value(expr, receiver, field_id, span, promoted, tc)
        }
        member::FieldResolution::AmbiguousPromoted {
            ty,
            name,
            candidates,
        } => {
            tc.push_error(TypeError::AmbiguousPromotedField {
                ty,
                member: name,
                candidates,
                span: tc.error_span(span),
            });
            FieldValueResult::Error
        }
        member::FieldResolution::Extern(field) => extern_field_value(
            expr,
            receiver,
            receiver.identity.clone().field(name),
            field,
            tc,
        ),
        member::FieldResolution::Missing { ty } => {
            tc.push_error(TypeError::UnknownMember {
                ty,
                member: name,
                kind: MemberAccessKind::Field,
                span: tc.error_span(span),
            });
            FieldValueResult::Error
        }
        member::FieldResolution::StaticOnValue { ty } => FieldValueResult::StaticOnValue(ty),
        member::FieldResolution::NonAggregate { ty } => FieldValueResult::NonAggregate(ty),
    }
}

fn promoted_field_value(
    expr: Option<&ExprNode>,
    receiver: &PlaceValue,
    field_id: ExprId,
    span: Span,
    promoted: member::PromotedFieldAccess,
    tc: &mut TypeChecker,
) -> FieldValueResult {
    tc.record_member_path(MemberPathFact {
        expr_id: field_id,
        kind: MemberPathKind::Field,
        path: promoted.path.clone(),
        origin_owner: promoted.origin_owner.clone(),
        origin_member: promoted.origin_field,
    });
    if promoted.exposure == Exposure::Implicit {
        let receiver_path_len = promoted.path.len().saturating_sub(1);
        tc.check_stored_field_path_access(
            &receiver.checked.ty,
            &promoted.path[..receiver_path_len],
            span,
        );
    }

    match promoted.target {
        member::PromotedFieldTarget::Aggregate(field) => {
            tc.check_access_policy(
                &field.policy,
                MemberAccessKind::Field,
                promoted.origin_field,
                &promoted.origin_owner,
                &field.origin,
                span,
            );
            let mut value = PlaceValue::new(
                field_checked(expr, field.ty, false, tc),
                projected_field_access(receiver.access),
                receiver.facts.clone(),
            );
            value.identity = receiver.identity.clone().fields(&promoted.path);
            FieldValueResult::Value(value, false)
        }
        member::PromotedFieldTarget::Extern(field) => extern_field_value(
            expr,
            receiver,
            receiver.identity.clone().fields(&promoted.path),
            field,
            tc,
        ),
    }
}

fn extern_field_value(
    expr: Option<&ExprNode>,
    receiver: &PlaceValue,
    identity: PlaceIdentity,
    field: member::ExternFieldAccess,
    tc: &mut TypeChecker,
) -> FieldValueResult {
    let mut value = PlaceValue::new(
        field_checked(expr, field.ty, field.contains_any, tc),
        field.access,
        PlaceUseFacts::for_extern_field(&receiver.facts, field.field_ref),
    );
    value.identity = identity;
    FieldValueResult::Value(value, field.contains_any)
}

fn field_checked(
    expr: Option<&ExprNode>,
    ty: Type,
    contains_extern_any: bool,
    tc: &mut TypeChecker,
) -> CheckedType {
    let mut checked = match expr {
        Some(expr) => super::checked_from_type(expr, ty, tc),
        None => super::checked_type(ty, tc),
    };
    checked.contains_extern_any = contains_extern_any;
    checked
}

pub(super) fn check_alias_scrutinee(expr: &ExprNode, tc: &mut TypeChecker) -> CheckedPlace {
    let place = check_place(expr, tc);
    if !place.value.access.can_assign() {
        tc.push_error(TypeError::VarPatternRequiresMutablePlace {
            span: tc.error_span(expr.span),
        });
    }
    record_value_read(expr.node.id, &place.value, tc);
    place
}

pub(super) fn check_place(expr: &ExprNode, tc: &mut TypeChecker) -> CheckedPlace {
    if let ExprKind::Ident(name) = &expr.node.kind {
        match tc.lookup_local_value_checked(*name, expr.span) {
            Ok(Some(value)) => {
                let checked =
                    super::checked_from_handle(expr, tc.local_handle(value.info.type_id), tc);
                let access = tc.local_value_access(*name, &value);
                let mut place = CheckedPlace::new(checked, access.access);
                place.value.facts = access.facts;
                place.value.identity = access.identity;
                place.accepts_extern_any = access.accepts_extern_any;
                return place;
            }
            Err(()) => {
                return CheckedPlace::new(
                    super::checked_from_type(expr, Type::Infer, tc),
                    PlaceAccess::NotPlace,
                );
            }
            Ok(None) => {}
        }
    }

    if let ExprKind::Ident(name) = &expr.node.kind
        && let Some((_, value_name, ValueDecl::Const(sig))) = tc.lookup_named_value(*name)
    {
        tc.warn_named_const_deprecated(value_name, expr.span);
        let checked = super::checked_from_type(expr, sig.ty.clone(), tc);
        return CheckedPlace::new(checked, PlaceAccess::Const);
    }

    if let ExprKind::Index(index) = &expr.node.kind {
        let target = check_place(&index.node.target, tc);
        let indexed = check_index_access(index, &target.value.checked, tc);
        let mut checked = super::checked_from_type(expr, indexed.write_ty, tc);
        checked.contains_extern_any = indexed.contains_extern_any;
        let access = projected_field_access(target.value.access);
        let mut place = CheckedPlace::new(checked, access);
        place.value.facts = target.value.facts;
        place.value.identity = target.value.identity.index();
        place.accepts_extern_any = target.accepts_extern_any;
        return place;
    }

    if let ExprKind::TupleIndex(index) = &expr.node.kind {
        let target = check_place(&index.node.target, tc);
        let checked = check_tuple_index_access(expr, index, &target.value.checked, tc);
        let access = projected_field_access(target.value.access);
        let mut place = CheckedPlace::new(checked, access);
        place.value.facts = target.value.facts;
        place.value.identity = target.value.identity.tuple(index.node.index as usize);
        place.accepts_extern_any = target.accepts_extern_any;
        return place;
    }

    if let ExprKind::Field(field) = &expr.node.kind {
        let receiver = check_place(&field.node.target, tc);
        match field_value(
            Some(expr),
            &receiver.value,
            expr.node.id,
            field.node.field,
            field.span,
            tc,
        ) {
            FieldValueResult::Value(value, accepts_extern_any) => {
                return CheckedPlace {
                    value,
                    accepts_extern_any,
                };
            }
            FieldValueResult::Error => {
                let checked = super::checked_from_type(expr, Type::Infer, tc);
                return CheckedPlace::new(checked, PlaceAccess::NotPlace);
            }
            FieldValueResult::StaticOnValue(_) | FieldValueResult::NonAggregate(_) => {}
        }
    }

    let checked = check_expr_checked(expr, tc);
    CheckedPlace::new(checked, PlaceAccess::NotPlace)
}

pub(super) fn record_write(expr_id: ExprId, place: &CheckedPlace, tc: &mut TypeChecker) {
    record_value_write(expr_id, &place.value, tc);
}

pub(super) fn record_compound_write(expr_id: ExprId, place: &CheckedPlace, tc: &mut TypeChecker) {
    record_prefix_reads(expr_id, &place.value.facts, tc);
    record_target_reads(expr_id, &place.value.facts, tc);
    record_target_writes(expr_id, &place.value.facts, tc);
}

pub(super) fn record_value_read(expr_id: ExprId, value: &PlaceValue, tc: &mut TypeChecker) {
    record_facts_read(expr_id, &value.facts, tc);
}

pub(super) fn record_value_write(expr_id: ExprId, value: &PlaceValue, tc: &mut TypeChecker) {
    record_facts_write(expr_id, &value.facts, tc);
}

pub(super) fn record_facts_read(expr_id: ExprId, facts: &PlaceUseFacts, tc: &mut TypeChecker) {
    record_prefix_reads(expr_id, facts, tc);
    record_target_reads(expr_id, facts, tc);
}

pub(super) fn record_facts_write(expr_id: ExprId, facts: &PlaceUseFacts, tc: &mut TypeChecker) {
    record_prefix_reads(expr_id, facts, tc);
    record_target_writes(expr_id, facts, tc);
}

fn record_prefix_reads(expr_id: ExprId, facts: &PlaceUseFacts, tc: &mut TypeChecker) {
    for field_ref in &facts.prefix_reads {
        tc.record_extern_use(expr_id, ExternUseTarget::FieldRead(*field_ref));
    }
}

fn record_target_reads(expr_id: ExprId, facts: &PlaceUseFacts, tc: &mut TypeChecker) {
    for field_ref in &facts.targets {
        tc.record_extern_use(expr_id, ExternUseTarget::FieldRead(*field_ref));
    }
}

fn record_target_writes(expr_id: ExprId, facts: &PlaceUseFacts, tc: &mut TypeChecker) {
    for field_ref in &facts.targets {
        tc.record_extern_use(expr_id, ExternUseTarget::FieldWrite(*field_ref));
    }
}

impl CheckedPlace {
    pub(super) fn accepts_extern_any(&self) -> bool {
        self.accepts_extern_any
    }

    pub(super) fn checked(&self) -> &CheckedType {
        &self.value.checked
    }

    pub(super) fn into_checked(self) -> CheckedType {
        self.value.checked
    }

    fn new(checked: CheckedType, access: PlaceAccess) -> Self {
        Self {
            value: PlaceValue::new(checked, access, PlaceUseFacts::default()),
            accepts_extern_any: false,
        }
    }
}

pub(super) fn projected_field_access(receiver_access: PlaceAccess) -> PlaceAccess {
    match receiver_access {
        PlaceAccess::Mutable => PlaceAccess::Mutable,
        PlaceAccess::Const => PlaceAccess::Const,
        PlaceAccess::Captured => PlaceAccess::Captured,
        PlaceAccess::ReadonlySelf => PlaceAccess::ReadonlySelf,
        PlaceAccess::NotPlace => PlaceAccess::NotPlace,
        PlaceAccess::Settable | PlaceAccess::Immutable => PlaceAccess::Immutable,
    }
}

pub(super) fn extern_field_access(receiver_access: PlaceAccess, computed: bool) -> PlaceAccess {
    if receiver_access == PlaceAccess::Mutable && computed {
        PlaceAccess::Settable
    } else {
        projected_field_access(receiver_access)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn ident(name: &str) -> Ident {
        Ident::new(name)
    }

    #[test]
    fn place_path_conflicts_on_same_or_prefix_path() {
        let root = PlacePath::root(ident("x"));
        let field = PlacePath::root(ident("x")).field(ident("a"));
        let nested = PlacePath::root(ident("x"))
            .field(ident("a"))
            .field(ident("b"));

        assert!(root.conflicts_with(&field));
        assert!(field.conflicts_with(&root));
        assert!(field.conflicts_with(&nested));
        assert!(root.conflicts_with(&PlacePath::root(ident("x")).tuple(0)));
    }

    #[test]
    fn place_path_does_not_conflict_on_distinct_fields_or_roots() {
        let a = PlacePath::root(ident("x")).field(ident("a"));
        let b = PlacePath::root(ident("x")).field(ident("b"));
        let other = PlacePath::root(ident("y")).field(ident("a"));
        let first = PlacePath::root(ident("x")).tuple(0);
        let second = PlacePath::root(ident("x")).tuple(1);

        assert!(!a.conflicts_with(&b));
        assert!(!a.conflicts_with(&other));
        assert!(!first.conflicts_with(&second));
    }

    fn group(id: u32) -> AliasAltGroupId {
        AliasAltGroupId(id)
    }

    fn tuple(root: &str, index: usize) -> PlaceIdentity {
        PlaceIdentity::root(ident(root)).tuple(index)
    }

    fn alternatives(group_id: u32, identities: Vec<PlaceIdentity>) -> PlaceIdentity {
        PlaceIdentity::alternatives(group(group_id), identities)
    }

    #[test]
    fn place_identity_conflicts_on_same_or_prefix_path() {
        let root = PlaceIdentity::root(ident("x"));
        let field = PlaceIdentity::root(ident("x")).field(ident("a"));
        let other = PlaceIdentity::root(ident("x")).field(ident("b"));

        assert!(root.conflicts_with(&field));
        assert!(field.conflicts_with(&root));
        assert!(!field.conflicts_with(&other));
    }

    #[test]
    fn same_group_swapped_paths_do_not_conflict_when_zipped() {
        let left = alternatives(1, vec![tuple("x", 0), tuple("x", 1)]);
        let right = alternatives(1, vec![tuple("x", 1), tuple("x", 0)]);

        assert!(!left.conflicts_with(&right));
    }

    #[test]
    fn same_group_same_path_conflicts_when_zipped() {
        let left = alternatives(1, vec![tuple("x", 0), PlaceIdentity::root(ident("y"))]);
        let right = alternatives(1, vec![tuple("x", 0), PlaceIdentity::root(ident("z"))]);

        assert!(left.conflicts_with(&right));
    }

    #[test]
    fn different_groups_use_cross_product() {
        let left = alternatives(1, vec![tuple("x", 0), PlaceIdentity::root(ident("y"))]);
        let right = alternatives(2, vec![PlaceIdentity::root(ident("z")), tuple("x", 0)]);

        assert!(left.conflicts_with(&right));
    }

    #[test]
    fn single_conflicts_with_any_alternative() {
        let single = tuple("x", 0);
        let choices = alternatives(
            1,
            vec![
                PlaceIdentity::root(ident("y")),
                PlaceIdentity::root(ident("x")),
            ],
        );

        assert!(single.conflicts_with(&choices));
        assert!(choices.conflicts_with(&single));
    }

    #[test]
    fn unknown_conflicts_with_mutable_identity() {
        let unknown = PlaceIdentity::unknown();
        let field = PlaceIdentity::root(ident("x")).field(ident("a"));

        assert!(unknown.conflicts_with(&field));
        assert!(field.conflicts_with(&unknown));
        assert!(unknown.conflicts_with(&PlaceIdentity::unknown()));
    }

    #[test]
    fn recursive_alternatives_preserve_inner_group_alignment() {
        let inner_left = alternatives(1, vec![tuple("x", 0), tuple("x", 1)]);
        let inner_right = alternatives(1, vec![tuple("x", 1), tuple("x", 0)]);
        let left = alternatives(2, vec![inner_left, PlaceIdentity::root(ident("y"))]);
        let right = alternatives(2, vec![inner_right, PlaceIdentity::root(ident("z"))]);

        assert!(!left.conflicts_with(&right));
    }

    #[test]
    fn projection_preserves_alternative_groups() {
        let left = alternatives(
            1,
            vec![
                PlaceIdentity::root(ident("x")),
                PlaceIdentity::root(ident("y")),
            ],
        )
        .field(ident("a"));
        let right = alternatives(
            1,
            vec![
                PlaceIdentity::root(ident("x")),
                PlaceIdentity::root(ident("z")),
            ],
        )
        .field(ident("a"));

        assert!(left.conflicts_with(&right));
    }
}
