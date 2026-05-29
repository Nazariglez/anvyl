use super::{
    CastFromConversion, CheckedType, ExpectedProjectionFact, ProjectionPath, TypeChecker,
    TypeError, checked_from_type, checked_type, convert,
    convert::{ExpectedDynPlan, ExplicitCast},
    infer::TypeHandle,
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
    Ambiguous(Vec<ProjectionPath>),
    MissingProjection {
        paths: Vec<Vec<Ident>>,
    },
    Mismatch,
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
    if let Some(acceptance) = source_acceptance_without_effects(tc, span, source, target, mode) {
        return ExpectedFit::SourceAccepted(acceptance);
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
                ExpectedFit::Mismatch
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
        ExpectedFit::Project { projection, .. } => ExpectedProjectionDecision::Project(projection),
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
                source_acceptance_without_effects(tc, span, &projection.target_ty, target, mode)?;
            Some(ProjectionMatch {
                projection,
                acceptance,
            })
        })
        .collect()
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
            let mut source = checked_type(source_ty.clone(), tc);
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
    NotNeeded,
    Failed,
}

pub(super) fn expected_place_projection(
    tc: &mut TypeChecker,
    expr: &ExprNode,
    source: &place::PlaceValue,
    target: &Type,
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
        ExpectedProjectionDecision::NotNeeded => ExpectedPlaceProjection::NotNeeded,
        ExpectedProjectionDecision::Failed => ExpectedPlaceProjection::Failed,
    }
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
        solver.concrete_type(source),
        solver.concrete_type(target),
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
    source_acceptance_without_effects(tc, span, source, target, mode).is_some()
}

fn source_acceptance_without_effects(
    tc: &mut TypeChecker,
    span: Span,
    source: &Type,
    target: &Type,
    mode: ExpectedProjectionMode,
) -> Option<SourceAcceptance> {
    match mode {
        ExpectedProjectionMode::Assignable => {
            assignable_acceptance_without_effects(tc, span, source, target)
        }
        ExpectedProjectionMode::CastAcceptingParam => {
            assignable_acceptance_without_effects(tc, span, source, target)
                .or_else(|| cast_from_acceptance_without_effects(tc, source, target))
        }
        ExpectedProjectionMode::ExplicitCast => tc
            .explicit_cast_plan_without_effects(source, target)
            .map(|conversion| SourceAcceptance::ExplicitCast { conversion }),
    }
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
