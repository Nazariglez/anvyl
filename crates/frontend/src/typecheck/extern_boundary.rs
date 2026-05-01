use anvyx_externs::ParamFlow;

use super::{
    CheckedType, PlaceAccess, TypeChecker, TypeError, check_expr, check_expr_checked_with_hint,
    check_place, infer::TypeHandle,
};
use crate::{
    ast::{ExprNode, Ident, Type},
    externs::catalog::{ResolvedExternParam, ResolvedExternSignature, ResolvedExternTy},
    span::Span,
};

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
    match param.flow {
        ParamFlow::Value | ParamFlow::Borrow => check_arg_expr(arg, param, tc),
        ParamFlow::MutBorrow => check_arg_place(arg, param, tc),
    }
}

pub(super) fn check_arg_expr(
    arg: &ExprNode,
    param: &ResolvedExternParam,
    tc: &mut TypeChecker,
) -> bool {
    let expected = tc.type_handle(&param.ty.ty);
    let checked = check_expr_checked_with_hint(arg, Some(expected), tc);
    check_checked_value(arg, &checked, &param.ty, tc)
}

pub(super) fn check_arg_place(
    arg: &ExprNode,
    param: &ResolvedExternParam,
    tc: &mut TypeChecker,
) -> bool {
    let checked = check_place(arg, tc);
    let is_mutable = checked.access == PlaceAccess::Mutable;
    if !is_mutable {
        tc.push_error(TypeError::ImmutableAssignment {
            name: place_error_name(arg, param),
            span: arg.span,
        });
    }
    super::place::record_write(arg.node.id, &checked, tc);
    let checked = CheckedType {
        ty: checked.ty,
        handle: checked.handle,
        contains_extern_any: checked.contains_extern_any,
    };
    let value_ok = check_checked_value(arg, &checked, &param.ty, tc);
    is_mutable && value_ok
}

pub(super) fn check_checked_value(
    expr: &ExprNode,
    checked: &CheckedType,
    boundary: &ResolvedExternTy,
    tc: &mut TypeChecker,
) -> bool {
    let allows_any = boundary.contains_any;
    let any_ok = allows_any || !checked.contains_extern_any;
    if !any_ok {
        tc.reject_extern_any_escape(checked, expr.span);
    }

    let expected = tc.type_handle(&boundary.ty);
    tc.expect_assignable(expr.span, checked.handle.clone(), expected);
    let failed = tc.solve_constraints();
    any_ok && !failed
}

pub(super) fn type_fits_boundary(found: &Type, boundary: &ResolvedExternTy) -> bool {
    type_fits_boundary_ty(found, &boundary.ty, boundary.contains_any)
}

fn check_arg_count(
    args: &[ExprNode],
    expected: usize,
    call_span: Span,
    tc: &mut TypeChecker,
) -> bool {
    if args.len() == expected {
        return true;
    }

    tc.push_error(TypeError::WrongArgCount {
        expected,
        found: args.len(),
        span: call_span,
    });
    for arg in args {
        check_expr(arg, tc);
    }
    false
}

fn place_error_name(arg: &ExprNode, param: &ResolvedExternParam) -> Ident {
    match &arg.node.kind {
        crate::ast::ExprKind::Ident(name) => *name,
        _ => param.name.unwrap_or_else(|| Ident::new("_")),
    }
}

fn type_fits_boundary_ty(found: &Type, expected: &Type, boundary_contains_any: bool) -> bool {
    found == expected
        || matches!(found, Type::Infer)
        || matches!(expected, Type::Infer)
        || boundary_contains_any && (matches!(found, Type::Any) || matches!(expected, Type::Any))
        || expected.option_inner().is_some_and(|inner| {
            !found.is_option() && type_fits_boundary_ty(found, inner, boundary_contains_any)
        })
}
