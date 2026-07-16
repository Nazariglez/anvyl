use super::{
    CastFromConversion, CheckedType, ExpectedProjectionFact, ProjectionPath, RawProjectionFact,
    TypeChecker, TypeError, checked_from_type, checked_type, convert,
    convert::{ExpectedDynPlan, ExplicitCast, RawEnumCastRejection},
    flags,
    infer::{Solver, TypeHandle},
    place,
    type_ops::type_depends_on_generics,
};
use crate::{
    ast::{ExprNode, Ident, Type},
    span::Span,
};

pub(super) enum ExpectedProjectionDecision {
    SourceAccepted,
    NotNeeded,
    Project(ProjectionPath),
    RawProject(RawProjectionPlan),
    Failed,
}

pub(super) enum ExpectedReturnConstraint {
    Unconstrained,
    Deferred,
    Committed { failed: bool },
}

impl ExpectedReturnConstraint {
    pub(super) fn failed(self) -> bool {
        matches!(self, Self::Committed { failed: true })
    }
}

#[derive(Clone)]
pub(super) struct RawProjectionPlan {
    source_ty: Type,
    target_ty: Type,
}

pub(super) struct RawBinaryProjectionPlan {
    left: Option<RawProjectionPlan>,
    right: Option<RawProjectionPlan>,
    result_ty: Type,
}

#[derive(Clone)]
pub(super) enum SourceAcceptance {
    Assignable,
    Dyn { plan: ExpectedDynPlan },
    CastFrom(CastFromConversion),
    ExplicitCast { conversion: ExplicitCast },
}

pub(super) enum ExpectedFit {
    SourceAccepted(SourceAcceptance),
    Deferred,
    Project {
        projection: ProjectionPath,
        acceptance: SourceAcceptance,
    },
    RawProject(RawProjectionPlan),
    Ambiguous(Vec<ProjectionPath>),
    MissingProjection {
        paths: Vec<Vec<Ident>>,
    },
    Mismatch,
    ExplicitCastRejected(RawEnumCastRejection),
}

#[derive(Clone, Copy)]
pub(super) enum ExpectedProjectionMode {
    Assignable,
    CastAcceptingParam,
    ExplicitCast,
}

pub(super) fn classify_expected_fit(
    tc: &mut TypeChecker,
    span: Span,
    source: &Type,
    target: &Type,
    mode: ExpectedProjectionMode,
) -> ExpectedFit {
    let source_rejection = match source_acceptance_without_effects(tc, span, source, target, mode) {
        Ok(Some(acceptance)) => return ExpectedFit::SourceAccepted(acceptance),
        Ok(None) => None,
        Err(rejection) => Some(rejection),
    };
    if !matches!(mode, ExpectedProjectionMode::ExplicitCast)
        && let Some(plan) = raw_projection_plan(tc, span, source, target)
    {
        return ExpectedFit::RawProject(plan);
    }
    if projection_probe_deferred(source, target) {
        return ExpectedFit::Deferred;
    }

    let matches = projection_matches(tc, span, source, target, mode);
    match matches.as_slice() {
        [entry] => ExpectedFit::Project {
            projection: entry.projection.clone(),
            acceptance: entry.acceptance.clone(),
        },
        [_, ..] => {
            ExpectedFit::Ambiguous(matches.into_iter().map(|entry| entry.projection).collect())
        }
        [] => {
            let paths = tc.decls.bare_embed_paths_to_type(source, target);
            if paths.is_empty() {
                source_rejection.map_or(ExpectedFit::Mismatch, ExpectedFit::ExplicitCastRejected)
            } else {
                ExpectedFit::MissingProjection { paths }
            }
        }
    }
}

pub(super) fn expected_projection(
    tc: &mut TypeChecker,
    span: Span,
    source: &Type,
    target: &Type,
    mode: ExpectedProjectionMode,
) -> ExpectedProjectionDecision {
    let fit = classify_expected_fit(tc, span, source, target, mode);
    expected_projection_decision(tc, span, source, target, fit)
}

pub(super) fn expected_projection_decision(
    tc: &mut TypeChecker,
    span: Span,
    source: &Type,
    target: &Type,
    fit: ExpectedFit,
) -> ExpectedProjectionDecision {
    match fit {
        ExpectedFit::SourceAccepted(_) => ExpectedProjectionDecision::SourceAccepted,
        ExpectedFit::Deferred | ExpectedFit::Mismatch => ExpectedProjectionDecision::NotNeeded,
        ExpectedFit::ExplicitCastRejected(_) => unreachable!("explicit cast handled by caller"),
        ExpectedFit::Project { projection, .. } => ExpectedProjectionDecision::Project(projection),
        ExpectedFit::RawProject(plan) => ExpectedProjectionDecision::RawProject(plan),
        ExpectedFit::Ambiguous(paths) => {
            tc.push_error(TypeError::AmbiguousProjection {
                source: source.clone(),
                target: target.clone(),
                paths: paths.into_iter().map(|entry| entry.field_path).collect(),
                span: tc.error_span(span),
            });
            ExpectedProjectionDecision::Failed
        }
        ExpectedFit::MissingProjection { paths } => {
            tc.push_error(TypeError::MissingProjection {
                source: source.clone(),
                target: target.clone(),
                paths,
                span: tc.error_span(span),
            });
            ExpectedProjectionDecision::Failed
        }
    }
}

pub(super) fn constrain_expected_return(
    span: Span,
    ret: TypeHandle,
    expected: Option<TypeHandle>,
    tc: &mut TypeChecker,
) -> ExpectedReturnConstraint {
    let Some(expected) = expected else {
        return ExpectedReturnConstraint::Unconstrained;
    };

    if tc.expected_returns_deferred() {
        return constrain_deferred_expected_return(span, ret, expected, tc);
    }
    if expected_return_should_commit(span, &ret, &expected, tc) {
        commit_expected_return(span, ret, expected, tc)
    } else {
        ExpectedReturnConstraint::Deferred
    }
}

fn constrain_deferred_expected_return(
    span: Span,
    ret: TypeHandle,
    expected: TypeHandle,
    tc: &mut TypeChecker,
) -> ExpectedReturnConstraint {
    if return_assignable_without_errors(tc, span, &ret, &expected) {
        commit_expected_return(span, ret, expected, tc)
    } else {
        ExpectedReturnConstraint::Deferred
    }
}

fn expected_return_should_commit(
    span: Span,
    ret: &TypeHandle,
    expected: &TypeHandle,
    tc: &mut TypeChecker,
) -> bool {
    let source = tc.handle_type(ret);
    let target = tc.handle_type(expected);
    if type_depends_on_generics(&source) || type_depends_on_generics(&target) {
        return true;
    }

    matches!(
        classify_expected_fit(
            tc,
            span,
            &source,
            &target,
            ExpectedProjectionMode::Assignable,
        ),
        ExpectedFit::SourceAccepted(_) | ExpectedFit::Deferred | ExpectedFit::Mismatch
    )
}

fn commit_expected_return(
    span: Span,
    ret: TypeHandle,
    expected: TypeHandle,
    tc: &mut TypeChecker,
) -> ExpectedReturnConstraint {
    tc.expect_assignable(span, ret, expected);
    ExpectedReturnConstraint::Committed {
        failed: tc.solve_constraints(),
    }
}

fn return_assignable_without_errors(
    tc: &TypeChecker,
    span: Span,
    ret: &TypeHandle,
    expected: &TypeHandle,
) -> bool {
    if tc.handle_is_poison(ret) || tc.handle_is_poison(expected) {
        return true;
    }
    let mut solver = tc.solver.clone();
    solver.add_handle_assignable(tc.error_span(span), ret.clone(), expected.clone());
    solver.solve_pending().is_empty()
}

struct ProjectionMatch {
    projection: ProjectionPath,
    acceptance: SourceAcceptance,
}

fn projection_matches(
    tc: &mut TypeChecker,
    span: Span,
    source: &Type,
    target: &Type,
    mode: ExpectedProjectionMode,
) -> Vec<ProjectionMatch> {
    tc.decls
        .projection_paths_from(source)
        .into_iter()
        .filter_map(|projection| {
            let acceptance =
                source_acceptance_without_effects(tc, span, &projection.target_ty, target, mode)
                    .ok()??;
            Some(ProjectionMatch {
                projection,
                acceptance,
            })
        })
        .collect()
}

pub(super) fn raw_binary_projection_plan(
    tc: &mut TypeChecker,
    op: crate::ast::BinaryOp,
    left_expr: &ExprNode,
    left: &Type,
    right_expr: &ExprNode,
    right: &Type,
) -> Option<RawBinaryProjectionPlan> {
    tc.ensure_pending_enum_values_for_type(left, left_expr.span);
    tc.ensure_pending_enum_values_for_type(right, right_expr.span);
    let left_flag = flags::is_type(tc, left_expr.span, left);
    let right_flag = flags::is_type(tc, right_expr.span, right);
    if left_flag && right_flag {
        return None;
    }
    let left_backing = raw_backing(tc, left).or_else(|| left_flag.then_some(Type::Int));
    let right_backing = raw_backing(tc, right).or_else(|| right_flag.then_some(Type::Int));
    let (left_target, right_target, backing) = match (left_backing, right_backing) {
        (Some(left_backing), Some(right_backing)) => {
            if left != right || matches!(op, crate::ast::BinaryOp::Eq | crate::ast::BinaryOp::NotEq)
            {
                return None;
            }
            (
                Some(left_backing.clone()),
                Some(right_backing),
                left_backing,
            )
        }
        (Some(backing), None) if backing == *right => (Some(backing.clone()), None, backing),
        (None, Some(backing)) if backing == *left => (None, Some(backing.clone()), backing),
        _ => return None,
    };
    let scalar = backing.scalar_kind()?;
    let result = if op == crate::ast::BinaryOp::Add && scalar == crate::ast::ScalarKind::String {
        crate::ast::ScalarKind::String
    } else {
        op.scalar_result(scalar, scalar)?
    };
    Some(RawBinaryProjectionPlan {
        left: left_target.map(|target_ty| RawProjectionPlan {
            source_ty: left.clone(),
            target_ty,
        }),
        right: right_target.map(|target_ty| RawProjectionPlan {
            source_ty: right.clone(),
            target_ty,
        }),
        result_ty: super::type_from_scalar(result),
    })
}

pub(super) fn apply_raw_binary_projection(
    tc: &mut TypeChecker,
    left_expr: &ExprNode,
    left: &CheckedType,
    right_expr: &ExprNode,
    right: &CheckedType,
    plan: RawBinaryProjectionPlan,
    project_left: bool,
) -> CheckedType {
    if project_left && let Some(projection) = plan.left {
        apply_raw_projection(tc, left_expr, left, projection);
    }
    if let Some(projection) = plan.right {
        apply_raw_projection(tc, right_expr, right, projection);
    }
    checked_type(plan.result_ty)
}

pub(super) fn raw_unary_projection_plan(
    tc: &mut TypeChecker,
    op: crate::ast::UnaryOp,
    operand_expr: &ExprNode,
    operand: &Type,
) -> Option<(RawProjectionPlan, Type)> {
    tc.ensure_pending_enum_values_for_type(operand, operand_expr.span);
    let backing = raw_backing(tc, operand)?;
    let result = op.scalar_result(backing.scalar_kind()?)?;
    Some((
        RawProjectionPlan {
            source_ty: operand.clone(),
            target_ty: backing,
        },
        super::type_from_scalar(result),
    ))
}

fn raw_backing(tc: &TypeChecker, ty: &Type) -> Option<Type> {
    Some(
        tc.decls
            .enum_schema_for_type(ty)?
            .body
            .kind
            .raw()?
            .backing
            .ty(),
    )
}

fn raw_projection_plan(
    tc: &mut TypeChecker,
    span: Span,
    source: &Type,
    target: &Type,
) -> Option<RawProjectionPlan> {
    tc.ensure_pending_enum_values_for_type(source, span);
    let kind = &tc.decls.enum_schema_for_type(source)?.body.kind;
    let backing = kind
        .raw()
        .map(|raw| raw.backing.ty())
        .or_else(|| kind.flag().map(|_| Type::Int))?;
    (backing == *target).then(|| RawProjectionPlan {
        source_ty: source.clone(),
        target_ty: target.clone(),
    })
}

pub(super) fn apply_raw_projection(
    tc: &mut TypeChecker,
    expr: &ExprNode,
    source_checked: &CheckedType,
    plan: RawProjectionPlan,
) -> CheckedType {
    let source = tc.expr_place(expr.node.id).unwrap_or_else(|| {
        let mut checked = checked_type(plan.source_ty.clone());
        checked.contains_extern_any = source_checked.contains_extern_any;
        place::PlaceValue::not_place(checked)
    });
    apply_raw_projection_from_place(tc, expr, &source, plan)
}

fn apply_raw_projection_from_place(
    tc: &mut TypeChecker,
    expr: &ExprNode,
    source: &place::PlaceValue,
    plan: RawProjectionPlan,
) -> CheckedType {
    let mut checked = checked_from_type(expr, plan.target_ty.clone(), tc);
    checked.contains_extern_any = source.checked.contains_extern_any;
    place::record_value_read(expr.node.id, source, tc);
    tc.record_raw_projection(RawProjectionFact {
        expr_id: expr.node.id,
        source_expr: expr.node.id,
        source_ty: plan.source_ty,
        target_ty: plan.target_ty,
    });
    checked
}

pub(super) fn apply_value_projection(
    tc: &mut TypeChecker,
    expr: &ExprNode,
    source_checked: &CheckedType,
    source_ty: &Type,
    projection: ProjectionPath,
) -> CheckedType {
    let path = projection.field_path;
    let target_ty = projection.target_ty;
    tc.check_stored_field_path_access(source_ty, &path, expr.span);

    let mut checked = checked_from_type(expr, target_ty.clone(), tc);
    checked.contains_extern_any = source_checked.contains_extern_any;

    let projected = match tc.expr_place(expr.node.id) {
        Some(source) => place::projected_value(&source, checked.clone(), &path),
        None => {
            let mut source = checked_type(source_ty.clone());
            source.contains_extern_any = source_checked.contains_extern_any;
            let source = place::PlaceValue::not_place(source);
            place::projected_value(&source, checked.clone(), &path)
        }
    };
    place::record_value_read(expr.node.id, &projected, tc);
    record_expected_projection(tc, expr, path, target_ty);
    checked
}

pub(super) enum ExpectedPlaceProjection {
    SourceAccepted,
    Projected(Box<place::PlaceValue>),
    RawProjected(CheckedType),
    RawValueRequired(RawProjectionPlan),
    NotNeeded,
    Failed,
}

pub(super) fn expected_place_projection(
    tc: &mut TypeChecker,
    expr: &ExprNode,
    source: &place::PlaceValue,
    target: &Type,
    allow_raw_value: bool,
) -> ExpectedPlaceProjection {
    let source_ty = source.checked.ty.clone();
    match expected_projection(
        tc,
        expr.span,
        &source_ty,
        target,
        ExpectedProjectionMode::Assignable,
    ) {
        ExpectedProjectionDecision::SourceAccepted => ExpectedPlaceProjection::SourceAccepted,
        ExpectedProjectionDecision::Project(projection) => ExpectedPlaceProjection::Projected(
            Box::new(apply_place_projection(tc, expr, source, projection)),
        ),
        ExpectedProjectionDecision::RawProject(plan) if allow_raw_value => {
            ExpectedPlaceProjection::RawProjected(apply_raw_projection_from_place(
                tc, expr, source, plan,
            ))
        }
        ExpectedProjectionDecision::RawProject(plan) => {
            ExpectedPlaceProjection::RawValueRequired(plan)
        }
        ExpectedProjectionDecision::NotNeeded => ExpectedPlaceProjection::NotNeeded,
        ExpectedProjectionDecision::Failed => ExpectedPlaceProjection::Failed,
    }
}

pub(super) fn reject_raw_place_projection(
    tc: &mut TypeChecker,
    expr: &ExprNode,
    plan: RawProjectionPlan,
) {
    tc.push_error(TypeError::RawProjectionRequiresValue {
        source: plan.source_ty,
        target: plan.target_ty,
        span: tc.error_span(expr.span),
    });
}

pub(super) fn apply_place_projection(
    tc: &mut TypeChecker,
    expr: &ExprNode,
    source: &place::PlaceValue,
    projection: ProjectionPath,
) -> place::PlaceValue {
    let source_ty = source.checked.ty.clone();
    let path = projection.field_path;
    let target_ty = projection.target_ty;
    tc.check_stored_field_path_access(&source_ty, &path, expr.span);

    let mut checked = checked_from_type(expr, target_ty.clone(), tc);
    checked.contains_extern_any = source.checked.contains_extern_any;
    let projected = place::projected_value(source, checked, &path);
    record_expected_projection(tc, expr, path, target_ty);
    projected
}

fn assignable_without_errors(tc: &TypeChecker, span: Span, source: &Type, target: &Type) -> bool {
    let mut solver = tc.solver.clone();
    solver.add_handle_assignable(
        tc.error_span(span),
        Solver::concrete_type(source),
        Solver::concrete_type(target),
    );
    solver.solve_pending().is_empty()
}

fn record_expected_projection(
    tc: &mut TypeChecker,
    expr: &ExprNode,
    path: Vec<Ident>,
    target_ty: Type,
) {
    tc.record_expected_projection(ExpectedProjectionFact {
        expr_id: expr.node.id,
        path,
        target_ty,
    });
}

pub(super) fn satisfies_without_effects(
    tc: &mut TypeChecker,
    span: Span,
    source: &Type,
    target: &Type,
    mode: ExpectedProjectionMode,
) -> bool {
    source_acceptance_without_effects(tc, span, source, target, mode)
        .is_ok_and(|acceptance| acceptance.is_some())
}

fn source_acceptance_without_effects(
    tc: &mut TypeChecker,
    span: Span,
    source: &Type,
    target: &Type,
    mode: ExpectedProjectionMode,
) -> Result<Option<SourceAcceptance>, RawEnumCastRejection> {
    let acceptance = match mode {
        ExpectedProjectionMode::Assignable => {
            assignable_acceptance_without_effects(tc, span, source, target)
        }
        ExpectedProjectionMode::CastAcceptingParam => {
            assignable_acceptance_without_effects(tc, span, source, target)
                .or_else(|| cast_from_acceptance_without_effects(tc, source, target))
        }
        ExpectedProjectionMode::ExplicitCast => {
            return tc
                .explicit_cast_plan_without_effects(span, source, target)
                .map(|conversion| {
                    conversion.map(|conversion| SourceAcceptance::ExplicitCast { conversion })
                });
        }
    };
    Ok(acceptance)
}

fn assignable_acceptance_without_effects(
    tc: &mut TypeChecker,
    span: Span,
    source: &Type,
    target: &Type,
) -> Option<SourceAcceptance> {
    if assignable_without_errors(tc, span, source, target) {
        Some(SourceAcceptance::Assignable)
    } else {
        dyn_assignable_plan_without_effects(tc, span, source, target)
            .map(|plan| SourceAcceptance::Dyn { plan })
    }
}

fn cast_from_acceptance_without_effects(
    tc: &mut TypeChecker,
    source: &Type,
    target: &Type,
) -> Option<SourceAcceptance> {
    tc.probe_compatibility_without_effects(|tc| tc.cast_from_conversion(source, target))
        .map(SourceAcceptance::CastFrom)
}

fn dyn_assignable_plan_without_effects(
    tc: &mut TypeChecker,
    span: Span,
    source: &Type,
    target: &Type,
) -> Option<ExpectedDynPlan> {
    convert::expected_dyn_plan_without_effects(tc, span, source, target)
}

fn projection_probe_deferred(source: &Type, target: &Type) -> bool {
    let pending = |ty: &Type| matches!(ty, Type::Infer | Type::InferReturn | Type::Var(_));
    pending(source) || pending(target)
}
