use anvyx_externs::FieldAccess;

use super::{
    CheckedType, ExternUseTarget, MemberAccessKind, TypeChecker, TypeError, check_expr_checked,
    decls::nominal_type,
};
use crate::{
    ast::{ExprId, ExprKind, ExprNode, Ident, Type},
    externs::catalog::{ExternField, ExternFieldRef, ExternTypeId},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum PlaceAccess {
    Mutable,
    Immutable,
    NotPlace,
}

pub(super) struct CheckedPlace {
    pub(super) ty: Type,
    pub(super) handle: super::infer::TypeHandle,
    pub(super) access: PlaceAccess,
    pub(super) contains_extern_any: bool,
    extern_field: Option<ExternFieldRef>,
    prefix_reads: Vec<ExternFieldRef>,
    accepts_extern_any: bool,
}

pub(super) struct ExternFieldPlace<'a> {
    pub(super) field_ref: ExternFieldRef,
    pub(super) decl: &'a ExternField,
    pub(super) access: PlaceAccess,
}

pub(super) fn check_place(expr: &ExprNode, tc: &mut TypeChecker) -> CheckedPlace {
    if let ExprKind::Ident(name) = &expr.node.kind
        && let Some(info) = tc.lookup(*name).cloned()
    {
        let checked = super::checked_from_handle(expr, tc.local_handle(info.type_id), tc);
        let access = if info.mutable {
            PlaceAccess::Mutable
        } else {
            PlaceAccess::Immutable
        };
        return CheckedPlace::new(checked, access);
    }

    if let ExprKind::Field(field) = &expr.node.kind {
        let receiver = check_place(&field.node.target, tc);
        if let Some(owner) = tc.extern_type_id(&receiver.ty) {
            let owner_ty = nominal_type(&tc.extern_type(owner).nominal);
            let Some(extern_field) =
                resolve_extern_field(owner, field.node.field, receiver.access, tc)
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
            let contains_any = extern_field.decl.ty.contains_any;
            let mut checked = super::checked_from_type(expr, ty, tc);
            checked.contains_extern_any = contains_any;
            let mut place = CheckedPlace::new(checked, access);
            place.extern_field = Some(field_ref);
            place.accepts_extern_any = contains_any;
            place.prefix_reads = receiver.prefix_reads;
            if let Some(prefix) = receiver.extern_field {
                place.prefix_reads.push(prefix);
            }
            return place;
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
        access: extern_field_access(receiver_access, decl.access),
    })
}

pub(super) fn record_read(expr_id: ExprId, place: &CheckedPlace, tc: &mut TypeChecker) {
    record_prefix_reads(expr_id, place, tc);
    if let Some(field_ref) = place.extern_field {
        tc.record_extern_use(expr_id, ExternUseTarget::FieldRead(field_ref));
    }
}

pub(super) fn record_write(expr_id: ExprId, place: &CheckedPlace, tc: &mut TypeChecker) {
    record_prefix_reads(expr_id, place, tc);
    if let Some(field_ref) = place.extern_field {
        tc.record_extern_use(expr_id, ExternUseTarget::FieldWrite(field_ref));
    }
}

pub(super) fn record_compound_write(expr_id: ExprId, place: &CheckedPlace, tc: &mut TypeChecker) {
    record_read(expr_id, place, tc);
    if let Some(field_ref) = place.extern_field {
        tc.record_extern_use(expr_id, ExternUseTarget::FieldWrite(field_ref));
    }
}

fn record_prefix_reads(expr_id: ExprId, place: &CheckedPlace, tc: &mut TypeChecker) {
    for field_ref in &place.prefix_reads {
        tc.record_extern_use(expr_id, ExternUseTarget::FieldRead(*field_ref));
    }
}

impl CheckedPlace {
    pub(super) fn accepts_extern_any(&self) -> bool {
        self.accepts_extern_any
    }

    fn new(checked: CheckedType, access: PlaceAccess) -> Self {
        Self {
            ty: checked.ty,
            handle: checked.handle,
            access,
            contains_extern_any: checked.contains_extern_any,
            extern_field: None,
            prefix_reads: vec![],
            accepts_extern_any: false,
        }
    }
}

fn extern_field_access(receiver_access: PlaceAccess, field_access: FieldAccess) -> PlaceAccess {
    match (receiver_access, field_access) {
        (PlaceAccess::Mutable, FieldAccess::ReadWrite { computed: false }) => PlaceAccess::Mutable,
        _ => PlaceAccess::Immutable,
    }
}
