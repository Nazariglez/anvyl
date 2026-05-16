use std::collections::HashMap;

use anvyx_externs::ParamFlow;

use super::{
    ArityError, CheckedType, ExternUseTarget, MemberAccessKind, NominalKey, TypeChecker, TypeError,
    check_arg_count, check_expr_checked, check_expr_checked_with_hint, check_place,
    check_value_expr_checked_with_hint, checked_from_type,
    infer::TypeHandle,
    literal::check_unknown_nominal_fields,
    nominal_type, solve_and_checked_from_handle,
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

    pub(super) fn reject_dyn_implicit_format(&mut self, ty: &Type, span: Span) -> bool {
        if !type_contains_dyn_value(ty, &self.decls, &mut std::collections::HashSet::new()) {
            return false;
        }
        self.push_error(TypeError::CompileError {
            message: "dynamic values cannot be implicitly formatted".to_string(),
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

    if let Some(expected) = expected {
        let ret = tc.type_handle(&signature.ret.ty);
        tc.expect_assignable(call_span, ret, expected);
        ok &= !tc.solve_constraints();
    }

    ok
}

pub(super) fn check_arg(arg: &ExprNode, param: &ResolvedExternParam, tc: &mut TypeChecker) -> bool {
    let ok = match param.flow {
        ParamFlow::Value => check_arg_expr(arg, param, tc),
        ParamFlow::Borrow => check_arg_borrow(arg, param, tc),
        ParamFlow::MutBorrow => check_arg_place(arg, param, tc),
    };
    tc.check_argument_escape(arg, param.escape);
    ok
}

fn check_arg_borrow(arg: &ExprNode, param: &ResolvedExternParam, tc: &mut TypeChecker) -> bool {
    let checked = check_place(arg, tc);
    super::place::record_immutable_borrow(arg.node.id, &checked.value, tc);
    let checked = checked.into_checked();
    check_checked_value(arg, &checked, &param.ty, tc)
}

fn check_arg_expr(arg: &ExprNode, param: &ResolvedExternParam, tc: &mut TypeChecker) -> bool {
    let expected = tc.type_handle(&param.ty.ty);
    let checked = check_value_expr_checked_with_hint(arg, Some(expected), tc);
    check_checked_value(arg, &checked, &param.ty, tc)
}

fn check_arg_place(arg: &ExprNode, param: &ResolvedExternParam, tc: &mut TypeChecker) -> bool {
    let checked = check_place(arg, tc);
    let is_mutable = checked.value.access.can_mut_borrow();
    if !is_mutable {
        let name = place_error_name(arg, param);
        if let Some(error) = checked
            .value
            .access
            .mut_borrow_error(name, tc.error_span(arg.span))
        {
            tc.push_error(error);
        }
    }
    if is_mutable {
        super::place::record_mut_borrow(arg.node.id, &checked.value, tc);
    }
    let checked = checked.into_checked();
    let value_ok = check_checked_value(arg, &checked, &param.ty, tc);
    is_mutable && value_ok
}

pub(super) fn check_checked_value(
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

    let expected = tc.type_handle(&boundary.ty);
    tc.expect_assignable_expr(expr.span, expr.node.id, checked.handle.clone(), expected);
    let failed = tc.solve_constraints();
    any_ok && !failed
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

    let Some(init) = tc.externs.init(owner).cloned() else {
        tc.push_error(TypeError::InvalidStructLiteral {
            name: key.name,
            kind: "extern".to_string(),
            span: tc.error_span(lit.span),
        });
        check_unknown_nominal_fields(&lit.node.fields, tc);
        return checked_from_type(expr, Type::Infer, tc);
    };

    let expected_ty = expected.map(|handle| tc.handle_type(handle));
    if let Some(expected_ty) = expected_ty.as_ref()
        && tc.decls.key_for_type(expected_ty).as_ref() == Some(key)
    {
        let expected = tc.type_handle(expected_ty);
        let actual = tc.type_handle(&nominal_type(key));
        tc.expect_equal(lit.span, actual, expected);
    }

    let fields_failed = check_extern_literal_fields(
        expr.node.id,
        &lit.node.fields,
        owner,
        &init.field_init,
        lit.span,
        tc,
    );
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
    explicit_init: &[Ident],
    span: Span,
    tc: &mut TypeChecker,
) -> bool {
    let owner_ty = nominal_type(&tc.extern_type(owner).nominal);
    let mut seen = HashMap::new();
    let mut failed = false;
    for (name, value) in fields {
        let duplicate = seen.insert(*name, value.span).is_some();
        if duplicate {
            tc.push_error(TypeError::DuplicateField {
                name: *name,
                span: tc.error_span(value.span),
            });
            failed = true;
        }

        let Some((_, field)) = tc.extern_field(owner, *name) else {
            tc.push_error(TypeError::UnknownMember {
                ty: owner_ty.clone(),
                member: *name,
                kind: MemberAccessKind::Field,
                span: tc.error_span(value.span),
            });
            check_expr_checked(value, tc);
            failed = true;
            continue;
        };

        let field_ty = field.ty.clone();
        let allowed = if explicit_init.is_empty() {
            !field.computed
        } else {
            explicit_init.contains(name)
        };
        if !allowed {
            tc.push_error(TypeError::ImmutableAssignment {
                name: *name,
                span: tc.error_span(value.span),
            });
            failed = true;
        }
        let hint = tc.type_handle(&field_ty.ty);
        let checked = check_expr_checked_with_hint(value, Some(hint), tc);
        if !duplicate && allowed {
            tc.record_aggregate_elem_flow(aggregate, value);
        }
        failed |= !check_checked_value(value, &checked, &field_ty, tc);
    }

    for name in required_extern_literal_fields(owner, explicit_init, tc) {
        if !seen.contains_key(&name) {
            tc.push_error(TypeError::MissingField {
                name,
                span: tc.error_span(span),
            });
            failed = true;
        }
    }
    failed
}

fn required_extern_literal_fields(
    owner: ExternTypeId,
    explicit_init: &[Ident],
    tc: &TypeChecker,
) -> Vec<Ident> {
    if !explicit_init.is_empty() {
        return explicit_init.to_vec();
    }
    tc.extern_type(owner)
        .fields
        .iter()
        .filter(|field| !field.computed)
        .map(|field| field.name)
        .collect()
}
