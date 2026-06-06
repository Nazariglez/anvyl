use super::{CheckedType, ExternUseTarget, TypeChecker, TypeError, checked_type, extern_boundary};
use crate::{
    ast::{BinaryOp, ExprId, ExprNode, Type, UnaryOp},
    externs::catalog::{ExternOperatorRef, ResolvedExternTy},
    span::Span,
};

pub(super) fn check_unary(
    expr_id: ExprId,
    op: UnaryOp,
    operand: &CheckedType,
    tc: &mut TypeChecker,
) -> Option<CheckedType> {
    let op = unary_op(op)?;
    let owner = tc.extern_type_id(&operand.ty)?;
    let (operator, decl) = tc.externs.unary_operator(owner, op)?;
    debug_assert!(decl.signature.params.is_empty());

    let ret = decl.signature.ret.clone();
    tc.record_extern_use(expr_id, ExternUseTarget::UnaryOperator(operator));
    Some(
        CheckedType::new(ret.ty.clone(), tc.type_handle(&ret.ty))
            .with_extern_any(ret.contains_any()),
    )
}

pub(super) fn check_binary(
    expr_id: ExprId,
    op: BinaryOp,
    left_expr: &ExprNode,
    left: &CheckedType,
    right_expr: &ExprNode,
    right: &CheckedType,
    span: Span,
    tc: &mut TypeChecker,
) -> Option<CheckedType> {
    let op = binary_op(op)?;
    let mut candidates = vec![];
    let mut rejected = None;

    if let Some(owner) = tc.extern_type_id(&left.ty)
        && let Some((operator, decl)) = tc.externs.binary_operator(owner, op, false)
    {
        debug_assert_eq!(decl.signature.params.len(), 1);
        if let Some(candidate) = binary_candidate(
            operator,
            right_expr,
            right,
            &decl.signature.params[0].ty,
            &decl.signature.ret,
            span,
            tc,
            &mut rejected,
        ) {
            candidates.push(candidate);
        }
    }

    if let Some(owner) = tc.extern_type_id(&right.ty)
        && let Some((operator, decl)) = tc.externs.binary_operator(owner, op, true)
    {
        debug_assert_eq!(decl.signature.params.len(), 1);
        if let Some(candidate) = binary_candidate(
            operator,
            left_expr,
            left,
            &decl.signature.params[0].ty,
            &decl.signature.ret,
            span,
            tc,
            &mut rejected,
        ) {
            candidates.push(candidate);
        }
    }

    select_binary_candidate(expr_id, op, &candidates, rejected, right, span, tc)
}

struct BinaryCandidate<'a> {
    operator: ExternOperatorRef,
    ret: ResolvedExternTy,
    other_expr: &'a ExprNode,
    other: &'a CheckedType,
    param: ResolvedExternTy,
}

fn binary_candidate<'a>(
    operator: ExternOperatorRef,
    other_expr: &'a ExprNode,
    other: &'a CheckedType,
    param: &ResolvedExternTy,
    ret: &ResolvedExternTy,
    span: Span,
    tc: &TypeChecker,
    rejected: &mut Option<Type>,
) -> Option<BinaryCandidate<'a>> {
    if !extern_boundary::type_fits_boundary(&other.ty, param, span, tc) {
        rejected.get_or_insert_with(|| other.ty.clone());
        return None;
    }
    Some(BinaryCandidate {
        operator,
        ret: ret.clone(),
        other_expr,
        other,
        param: param.clone(),
    })
}

fn select_binary_candidate(
    expr_id: ExprId,
    op: anvyx_externs::BinaryOp,
    candidates: &[BinaryCandidate<'_>],
    rejected: Option<Type>,
    fallback_operand: &CheckedType,
    span: Span,
    tc: &mut TypeChecker,
) -> Option<CheckedType> {
    match candidates {
        [candidate] => Some(apply_binary_candidate(expr_id, candidate, tc)),
        [] => rejected.map(|operand_type| invalid_operand(op, operand_type, span, tc)),
        _ => Some(invalid_operand(op, fallback_operand.ty.clone(), span, tc)),
    }
}

fn apply_binary_candidate(
    expr_id: ExprId,
    candidate: &BinaryCandidate<'_>,
    tc: &mut TypeChecker,
) -> CheckedType {
    if extern_boundary::check_checked_value(
        candidate.other_expr,
        candidate.other,
        &candidate.param,
        tc,
    ) {
        tc.record_extern_use(expr_id, ExternUseTarget::BinaryOperator(candidate.operator));
    }

    CheckedType::new(candidate.ret.ty.clone(), tc.type_handle(&candidate.ret.ty))
        .with_extern_any(candidate.ret.contains_any())
}

fn invalid_operand(
    op: anvyx_externs::BinaryOp,
    operand_type: Type,
    span: Span,
    tc: &mut TypeChecker,
) -> CheckedType {
    tc.push_error(TypeError::InvalidOperand {
        op: op.to_string(),
        operand_type,
        span: tc.error_span(span),
    });
    checked_type(Type::Infer, tc)
}

fn binary_op(op: BinaryOp) -> Option<anvyx_externs::BinaryOp> {
    match op {
        BinaryOp::Add => Some(anvyx_externs::BinaryOp::Add),
        BinaryOp::Sub => Some(anvyx_externs::BinaryOp::Sub),
        BinaryOp::Mul => Some(anvyx_externs::BinaryOp::Mul),
        BinaryOp::Div => Some(anvyx_externs::BinaryOp::Div),
        BinaryOp::Rem => Some(anvyx_externs::BinaryOp::Rem),
        BinaryOp::Eq => Some(anvyx_externs::BinaryOp::Eq),
        BinaryOp::NotEq => Some(anvyx_externs::BinaryOp::NotEq),
        BinaryOp::LessThan => Some(anvyx_externs::BinaryOp::LessThan),
        BinaryOp::GreaterThan => Some(anvyx_externs::BinaryOp::GreaterThan),
        BinaryOp::LessThanEq => Some(anvyx_externs::BinaryOp::LessThanEq),
        BinaryOp::GreaterThanEq => Some(anvyx_externs::BinaryOp::GreaterThanEq),
        BinaryOp::And
        | BinaryOp::Or
        | BinaryOp::Xor
        | BinaryOp::BitAnd
        | BinaryOp::BitOr
        | BinaryOp::Shl
        | BinaryOp::Shr
        | BinaryOp::Coalesce => None,
    }
}

fn unary_op(op: UnaryOp) -> Option<anvyx_externs::UnaryOp> {
    match op {
        UnaryOp::Neg => Some(anvyx_externs::UnaryOp::Neg),
        UnaryOp::Not | UnaryOp::BitNot => None,
    }
}
