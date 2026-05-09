use super::{
    CheckedType, ExternUseTarget, MemberAccessKind, TypeChecker, TypeError, ValueDecl,
    check_expr_checked, check_index_access, check_tuple_index_access, decls::nominal_type,
};
use crate::{
    ast::{ExprId, ExprKind, ExprNode, Ident, Type},
    externs::catalog::{ExternField, ExternFieldRef, ExternTypeId},
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

    pub(super) fn assign_error(self, name: Ident, span: crate::span::Span) -> Option<TypeError> {
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
        span: crate::span::Span,
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

#[derive(Debug, Clone, Default)]
pub(super) struct PlaceUseFacts {
    extern_field: Option<ExternFieldRef>,
    prefix_reads: Vec<ExternFieldRef>,
}

impl PlaceUseFacts {
    pub(super) fn for_extern_field(receiver: Option<&Self>, field_ref: ExternFieldRef) -> Self {
        let mut facts = receiver.cloned().unwrap_or_default();
        if let Some(prefix) = facts.extern_field.replace(field_ref) {
            facts.prefix_reads.push(prefix);
        }
        facts
    }
}

#[derive(Clone)]
pub(super) struct PlaceValue {
    pub(super) checked: CheckedType,
    pub(super) access: PlaceAccess,
    pub(super) facts: Option<PlaceUseFacts>,
    pub(super) path: Option<PlacePath>,
}

pub(super) struct CheckedPlace {
    pub(super) value: PlaceValue,
    accepts_extern_any: bool,
}

impl PlaceValue {
    pub(super) fn new(
        checked: CheckedType,
        access: PlaceAccess,
        facts: Option<PlaceUseFacts>,
    ) -> Self {
        Self {
            checked,
            access,
            facts,
            path: None,
        }
    }

    pub(super) fn not_place(checked: CheckedType) -> Self {
        Self::new(checked, PlaceAccess::NotPlace, None)
    }
}

pub(super) struct ExternFieldPlace<'a> {
    pub(super) field_ref: ExternFieldRef,
    pub(super) decl: &'a ExternField,
    pub(super) access: PlaceAccess,
}

pub(super) fn check_alias_scrutinee(expr: &ExprNode, tc: &mut TypeChecker) -> CheckedPlace {
    let place = check_place(expr, tc);
    if !place.value.access.can_assign() {
        tc.push_error(TypeError::VarPatternRequiresMutablePlace { span: expr.span });
    }
    record_value_read(expr.node.id, &place.value, tc);
    place
}

pub(super) fn check_place(expr: &ExprNode, tc: &mut TypeChecker) -> CheckedPlace {
    if let ExprKind::Ident(name) = &expr.node.kind {
        match tc.lookup_local_value_checked(*name, expr.span) {
            Ok(Some((info, depth))) => {
                tc.record_capture(*name, depth);
                let checked = super::checked_from_handle(expr, tc.local_handle(info.type_id), tc);
                let access = if tc.is_captured_local(depth) {
                    PlaceAccess::Captured
                } else {
                    info.alias
                        .as_ref()
                        .map_or_else(|| info.kind.place_access(), |alias| alias.access)
                };
                let mut place = CheckedPlace::new(checked, access);
                place.value.path =
                    Some(PlacePath::root(*name)).filter(|_| place.value.access.can_mut_borrow());
                if let Some(alias) = info.alias {
                    place.value.facts = alias.facts;
                    place.value.path = alias.path;
                    place.accepts_extern_any = alias.accepts_extern_any;
                }
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
        place.value.path = target.value.path.map(PlacePath::index);
        place.accepts_extern_any = target.accepts_extern_any;
        return place;
    }

    if let ExprKind::TupleIndex(index) = &expr.node.kind {
        let target = check_place(&index.node.target, tc);
        let checked = check_tuple_index_access(expr, index, &target.value.checked, tc);
        let access = projected_field_access(target.value.access);
        let mut place = CheckedPlace::new(checked, access);
        place.value.facts = target.value.facts;
        place.value.path = target
            .value
            .path
            .map(|path| path.tuple(index.node.index as usize));
        place.accepts_extern_any = target.accepts_extern_any;
        return place;
    }

    if let ExprKind::Field(field) = &expr.node.kind {
        let receiver = check_place(&field.node.target, tc);
        if let Some(owner) = tc.extern_type_id(&receiver.value.checked.ty) {
            let owner_ty = nominal_type(&tc.extern_type(owner).nominal);
            let Some(extern_field) =
                resolve_extern_field(owner, field.node.field, receiver.value.access, tc)
            else {
                tc.push_error(TypeError::UnknownMember {
                    ty: owner_ty,
                    member: field.node.field,
                    kind: MemberAccessKind::Field,
                    span: field.span,
                });
                let checked = super::checked_from_type(expr, Type::Infer, tc);
                return CheckedPlace::new(checked, PlaceAccess::NotPlace);
            };

            let field_ref = extern_field.field_ref;
            let access = extern_field.access;
            let ty = extern_field.decl.ty.ty.clone();
            let contains_any = extern_field.decl.ty.contains_any();
            let mut checked = super::checked_from_type(expr, ty, tc);
            checked.contains_extern_any = contains_any;
            let mut place = CheckedPlace::new(checked, access);
            place.value.facts = Some(PlaceUseFacts::for_extern_field(
                receiver.value.facts.as_ref(),
                field_ref,
            ));
            place.accepts_extern_any = contains_any;
            return place;
        }

        if let Some(key) = tc.decls.key_for_type(&receiver.value.checked.ty) {
            if let Some(ty) = tc
                .decls
                .aggregate_field_type(&receiver.value.checked.ty, field.node.field)
            {
                tc.check_field_access_policy(
                    &receiver.value.checked.ty,
                    field.node.field,
                    field.span,
                );
                let checked = super::checked_from_type(expr, ty, tc);
                let access = projected_field_access(receiver.value.access);
                let mut place = CheckedPlace::new(checked, access);
                place.value.facts = receiver.value.facts;
                place.value.path = receiver.value.path.map(|path| path.field(field.node.field));
                return place;
            }

            tc.push_error(TypeError::UnknownMember {
                ty: nominal_type(&key),
                member: field.node.field,
                kind: MemberAccessKind::Field,
                span: field.span,
            });
            let checked = super::checked_from_type(expr, Type::Infer, tc);
            return CheckedPlace::new(checked, PlaceAccess::NotPlace);
        }
    }

    let checked = check_expr_checked(expr, tc);
    CheckedPlace::new(checked, PlaceAccess::NotPlace)
}

pub(super) fn resolve_extern_field(
    owner: ExternTypeId,
    name: Ident,
    receiver_access: PlaceAccess,
    tc: &TypeChecker,
) -> Option<ExternFieldPlace<'_>> {
    let (field, decl) = tc.extern_field(owner, name)?;
    Some(ExternFieldPlace {
        field_ref: field,
        decl,
        access: extern_field_access(receiver_access, decl.computed),
    })
}

pub(super) fn record_write(expr_id: ExprId, place: &CheckedPlace, tc: &mut TypeChecker) {
    record_value_write(expr_id, &place.value, tc);
}

pub(super) fn record_compound_write(expr_id: ExprId, place: &CheckedPlace, tc: &mut TypeChecker) {
    if let Some(facts) = &place.value.facts {
        record_facts_read(expr_id, facts, tc);
        if let Some(field_ref) = facts.extern_field {
            tc.record_extern_use(expr_id, ExternUseTarget::FieldWrite(field_ref));
        }
    }
}

pub(super) fn record_value_read(expr_id: ExprId, value: &PlaceValue, tc: &mut TypeChecker) {
    if let Some(facts) = &value.facts {
        record_facts_read(expr_id, facts, tc);
    }
}

pub(super) fn record_value_write(expr_id: ExprId, value: &PlaceValue, tc: &mut TypeChecker) {
    if let Some(facts) = &value.facts {
        record_facts_write(expr_id, facts, tc);
    }
}

pub(super) fn record_facts_read(expr_id: ExprId, facts: &PlaceUseFacts, tc: &mut TypeChecker) {
    record_prefix_reads(expr_id, facts, tc);
    if let Some(field_ref) = facts.extern_field {
        tc.record_extern_use(expr_id, ExternUseTarget::FieldRead(field_ref));
    }
}

pub(super) fn record_facts_write(expr_id: ExprId, facts: &PlaceUseFacts, tc: &mut TypeChecker) {
    record_prefix_reads(expr_id, facts, tc);
    if let Some(field_ref) = facts.extern_field {
        tc.record_extern_use(expr_id, ExternUseTarget::FieldWrite(field_ref));
    }
}

fn record_prefix_reads(expr_id: ExprId, facts: &PlaceUseFacts, tc: &mut TypeChecker) {
    for field_ref in &facts.prefix_reads {
        tc.record_extern_use(expr_id, ExternUseTarget::FieldRead(*field_ref));
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
        let facts = (access != PlaceAccess::NotPlace).then(PlaceUseFacts::default);
        Self {
            value: PlaceValue::new(checked, access, facts),
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
    match receiver_access {
        PlaceAccess::Mutable if computed => PlaceAccess::Settable,
        PlaceAccess::Mutable => PlaceAccess::Mutable,
        PlaceAccess::Const => PlaceAccess::Const,
        PlaceAccess::Captured => PlaceAccess::Captured,
        PlaceAccess::ReadonlySelf => PlaceAccess::ReadonlySelf,
        PlaceAccess::NotPlace => PlaceAccess::NotPlace,
        PlaceAccess::Settable | PlaceAccess::Immutable => PlaceAccess::Immutable,
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
}
