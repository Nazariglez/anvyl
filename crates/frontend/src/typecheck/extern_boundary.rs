use anvyx_externs::ParamFlow;

use super::{
    ArityError, CheckedType, ExternUseTarget, NominalKey, TypeChecker, TypeError, check_arg_count,
    check_expected_value_expr, check_expr_checked, check_expr_checked_with_hint, check_place,
    checked_from_type, field_check,
    infer::TypeHandle,
    literal::check_unknown_nominal_fields,
    nominal_type, place,
    projection::{ExpectedPlaceProjection, constrain_expected_return, expected_place_projection},
    solve_and_checked_from_handle,
    type_ops::{type_closure_facts, type_contains_dyn_value},
};
use crate::{
    ast::{ExprId, ExprKind, ExprNode, Ident, StructLiteralNode, Type},
    externs::catalog::{
        ExternTypeId, ResolvedExternParam, ResolvedExternSignature, ResolvedExternTy,
    },
    span::Span,
};

impl TypeChecker {
    pub(super) fn reject_extern_any_escape(&mut self, checked: &CheckedType, span: Span) {
        self.reject_extern_any_escape_fact(checked.contains_extern_any, span);
    }

    pub(super) fn reject_extern_any_escape_fact(&mut self, contains_extern_any: bool, span: Span) {
        if contains_extern_any {
            self.push_error(TypeError::ExternAnyEscape {
                span: self.error_span(span),
            });
        }
    }

    pub(super) fn reject_user_any_type(&mut self, ty: &Type, span: Span) -> bool {
        if !type_closure_facts(ty).contains_any {
            return false;
        }
        self.push_error(TypeError::AnyOutsideExternBoundary {
            span: self.error_span(span),
        });
        true
    }

    pub(super) fn reject_dyn_format(&mut self, ty: &Type, span: Span) -> bool {
        if !type_contains_dyn_value(ty, &self.decls, &mut std::collections::HashSet::new()) {
            return false;
        }
        self.push_error(TypeError::CompileError {
            message: "dynamic values cannot be formatted".to_string(),
            span: self.error_span(span),
        });
        true
    }
}

pub(super) fn check_call(
    signature: &ResolvedExternSignature,
    args: &[ExprNode],
    call_span: Span,
    expected: Option<TypeHandle>,
    tc: &mut TypeChecker,
) -> bool {
    if !check_arg_count(args, signature.params.len(), call_span, tc) {
        return false;
    }

    let mut ok = true;
    for (arg, param) in args.iter().zip(&signature.params) {
        ok &= check_arg(arg, param, tc);
    }

    let ret = tc.type_handle(&signature.ret.ty);
    ok &= !constrain_expected_return(call_span, ret, expected, tc).failed();

    ok
}

pub(super) fn check_arg(arg: &ExprNode, param: &ResolvedExternParam, tc: &mut TypeChecker) -> bool {
    let ok = match param.flow {
        ParamFlow::Value => check_arg_expr(arg, param, tc),
        ParamFlow::Borrow => check_arg_borrow(arg, param, tc),
        ParamFlow::MutBorrow => check_arg_place(arg, param, tc),
    };
    tc.record_argument_escape(arg, param.escape);
    ok
}

fn check_arg_borrow(arg: &ExprNode, param: &ResolvedExternParam, tc: &mut TypeChecker) -> bool {
    let checked = check_place(arg, tc);
    check_projected_place_arg(
        arg,
        param,
        &checked.value,
        place::record_immutable_borrow,
        tc,
    )
}

fn check_projected_place_arg(
    arg: &ExprNode,
    param: &ResolvedExternParam,
    value: &place::PlaceValue,
    record: fn(ExprId, &place::PlaceValue, &mut TypeChecker),
    tc: &mut TypeChecker,
) -> bool {
    match expected_place_projection(tc, arg, value, &param.ty.ty) {
        ExpectedPlaceProjection::Projected(projected) => {
            record(arg.node.id, &projected, tc);
            check_checked_value(arg, &projected.checked, &param.ty, tc)
        }
        ExpectedPlaceProjection::Failed => false,
        ExpectedPlaceProjection::SourceAccepted | ExpectedPlaceProjection::NotNeeded => {
            record(arg.node.id, value, tc);
            check_checked_value(arg, &value.checked, &param.ty, tc)
        }
    }
}

fn check_arg_expr(arg: &ExprNode, param: &ResolvedExternParam, tc: &mut TypeChecker) -> bool {
    let error_count = tc.errors.len();
    let expected = tc.type_handle(&param.ty.ty);
    let checked = check_expected_value_expr(arg, expected, tc);
    check_boundary_value(arg, &checked, &param.ty, tc) && tc.errors.len() == error_count
}

fn check_arg_place(arg: &ExprNode, param: &ResolvedExternParam, tc: &mut TypeChecker) -> bool {
    let checked = check_place(arg, tc);
    if !checked.value.access.can_mut_borrow() {
        let name = place_error_name(arg, param);
        if let Some(error) = checked
            .value
            .access
            .mut_borrow_error(name, tc.error_span(arg.span))
        {
            tc.push_error(error);
        }
        check_checked_value(arg, &checked.value.checked, &param.ty, tc);
        return false;
    }

    check_projected_place_arg(arg, param, &checked.value, place::record_mut_borrow, tc)
}

pub(super) fn check_checked_value(
    expr: &ExprNode,
    checked: &CheckedType,
    boundary: &ResolvedExternTy,
    tc: &mut TypeChecker,
) -> bool {
    let boundary_ok = check_boundary_value(expr, checked, boundary, tc);
    let expected = tc.type_handle(&boundary.ty);
    tc.expect_assignable_expr(expr.span, expr.node.id, checked.handle.clone(), expected);
    boundary_ok && !tc.solve_constraints()
}

fn check_boundary_value(
    expr: &ExprNode,
    checked: &CheckedType,
    boundary: &ResolvedExternTy,
    tc: &mut TypeChecker,
) -> bool {
    let allows_any = boundary.contains_any();
    let any_ok = allows_any || !checked.contains_extern_any;
    if !any_ok {
        tc.reject_extern_any_escape(checked, expr.span);
    }
    any_ok && !tc.solve_constraints()
}

pub(super) fn type_fits_boundary(
    found: &Type,
    boundary: &ResolvedExternTy,
    span: Span,
    tc: &TypeChecker,
) -> bool {
    tc.solver
        .type_assignable(tc.error_span(span), found, &boundary.ty)
}

fn place_error_name(arg: &ExprNode, param: &ResolvedExternParam) -> Ident {
    match &arg.node.kind {
        ExprKind::Ident(name) => *name,
        _ => param.name.unwrap_or_else(|| Ident::new("_")),
    }
}

pub(super) fn check_extern_lit(
    expr: &ExprNode,
    lit: &StructLiteralNode,
    key: &NominalKey,
    expected: Option<&TypeHandle>,
    tc: &mut TypeChecker,
) -> CheckedType {
    let Some(owner) = tc.externs.type_by_nominal(key) else {
        tc.push_error(TypeError::InvalidStructLiteral {
            name: key.name,
            kind: "extern".to_string(),
            span: tc.error_span(lit.span),
        });
        check_unknown_nominal_fields(&lit.node.fields, tc);
        return checked_from_type(expr, Type::Infer, tc);
    };

    if !lit.node.generic_args.is_empty() {
        tc.push_error(TypeError::GenericArity(ArityError::TypeArgs {
            expected: 0,
            found: lit.node.generic_args.len(),
        }));
        check_unknown_nominal_fields(&lit.node.fields, tc);
        return checked_from_type(expr, Type::Infer, tc);
    }

    let extern_type = tc.extern_type(owner);
    if extern_type.constructor_fields().is_none() {
        tc.push_error(TypeError::InvalidStructLiteral {
            name: key.name,
            kind: "extern".to_string(),
            span: tc.error_span(lit.span),
        });
        check_unknown_nominal_fields(&lit.node.fields, tc);
        return checked_from_type(expr, Type::Infer, tc);
    }

    let expected_ty = expected.map(|handle| tc.handle_type(handle));
    if let Some(expected_ty) = expected_ty.as_ref()
        && tc.decls.key_for_type(expected_ty).as_ref() == Some(key)
    {
        let expected = tc.type_handle(expected_ty);
        let actual = tc.type_handle(&nominal_type(key));
        tc.expect_equal(lit.span, actual, expected);
    }

    let fields_failed =
        check_extern_literal_fields(expr.node.id, &lit.node.fields, owner, lit.span, tc);
    if fields_failed {
        return checked_from_type(expr, Type::Infer, tc);
    }

    tc.record_extern_use(expr.node.id, ExternUseTarget::Init(owner));
    let ty = nominal_type(key);
    let handle = tc.type_handle(&ty);
    solve_and_checked_from_handle(expr, handle, tc)
}

fn check_extern_literal_fields(
    aggregate: ExprId,
    fields: &[(Ident, ExprNode)],
    owner: ExternTypeId,
    span: Span,
    tc: &mut TypeChecker,
) -> bool {
    let owner_ty = nominal_type(&tc.extern_type(owner).nominal);
    let schema = field_check::extern_field_schema(tc.extern_type(owner));
    let required = required_extern_literal_fields(owner, tc);
    let presence = presence_extern_literal_fields(owner, tc);
    let field_owner = field_check::FieldOwner::Nominal(owner_ty);
    let shape = field_check::check_named(
        fields,
        &schema,
        &field_owner,
        field_check::MissingFields::RequireOnly(&required),
        Some(span),
        |expr| expr.span,
        tc,
    );
    let mut failed = shape.failed;

    for index in &shape.invalid_indices {
        check_expr_checked(&fields[*index].1, tc);
    }

    for checked_field in shape.fields {
        let (name, value) = &fields[checked_field.index];
        let Some((field_ref, field)) = tc.extern_field(owner, *name) else {
            continue;
        };
        let writable = field.writable;
        let field_ty = field.ty.clone();
        let is_init_field = required.contains(name) || presence.contains(name);
        match (is_init_field, writable) {
            (true, _) => {}
            (false, true) => {
                tc.record_extern_use(aggregate, ExternUseTarget::FieldWrite(field_ref));
            }
            (false, false) => {
                tc.push_error(TypeError::ImmutableAssignment {
                    name: *name,
                    span: tc.error_span(value.span),
                });
                failed = true;
            }
        }
        let hint = tc.type_handle(&field_ty.ty);
        let checked = check_expr_checked_with_hint(value, Some(hint), tc);
        tc.record_aggregate_elem_escape(aggregate, value);
        failed |= !check_checked_value(value, &checked, &field_ty, tc);
    }
    failed
}

fn required_extern_literal_fields(owner: ExternTypeId, tc: &TypeChecker) -> Vec<Ident> {
    tc.extern_type(owner)
        .required_init_fields()
        .map(|fields| fields.map(|(_, field)| field.name).collect())
        .unwrap_or_default()
}

fn presence_extern_literal_fields(owner: ExternTypeId, tc: &TypeChecker) -> Vec<Ident> {
    tc.extern_type(owner)
        .presence_init_fields()
        .map(|fields| fields.map(|(_, field)| field.name).collect())
        .unwrap_or_default()
}
