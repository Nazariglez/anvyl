use anvyx_externs::ParamFlow;

use super::{
    CheckedType, TypeChecker, check_arg_count, check_expr_checked_with_hint, check_place,
    infer::TypeHandle,
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
    let is_mutable = checked.value.access.can_mut_borrow();
    if !is_mutable {
        let name = place_error_name(arg, param);
        if let Some(error) = checked.value.access.mut_borrow_error(name, arg.span) {
            tc.push_error(error);
        }
    }
    if is_mutable {
        super::place::record_write(arg.node.id, &checked, tc);
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
    tc.expect_assignable(expr.span, checked.handle.clone(), expected);
    let failed = tc.solve_constraints();
    any_ok && !failed
}

pub(super) fn type_fits_boundary(
    found: &Type,
    boundary: &ResolvedExternTy,
    span: Span,
    tc: &TypeChecker,
) -> bool {
    tc.solver.type_assignable(span, found, &boundary.ty)
}

fn place_error_name(arg: &ExprNode, param: &ResolvedExternParam) -> Ident {
    match &arg.node.kind {
        crate::ast::ExprKind::Ident(name) => *name,
        _ => param.name.unwrap_or_else(|| Ident::new("_")),
    }
}
