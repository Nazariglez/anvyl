use super::{
    CheckedType, ExpectedProjectionFact, ProjectionPath, TypeChecker, TypeError, checked_from_type,
    checked_type, contracts, place,
};
use crate::{
    ast::{ContractRef, ExprNode, Ident, Type},
    span::Span,
};

pub(super) enum ExpectedProjectionDecision {
    SourceAccepted,
    NotNeeded,
    Project(ProjectionPath),
    Failed,
}

#[derive(Clone, Copy)]
pub(super) enum ExpectedProjectionMode {
    Assignable,
    CastAcceptingParam,
    ExplicitCast,
}

pub(super) fn expected_projection(
    tc: &mut TypeChecker,
    span: Span,
    source: &Type,
    target: &Type,
    mode: ExpectedProjectionMode,
) -> ExpectedProjectionDecision {
    if satisfies_without_effects(tc, span, source, target, mode) {
        return ExpectedProjectionDecision::SourceAccepted;
    }
    if projection_probe_deferred(source, target) {
        return ExpectedProjectionDecision::NotNeeded;
    }

    let matches = projection_matches(tc, span, source, target, mode);
    match matches.as_slice() {
        [entry] => ExpectedProjectionDecision::Project(entry.clone()),
        [_, ..] => {
            tc.push_error(TypeError::AmbiguousProjection {
                source: source.clone(),
                target: target.clone(),
                paths: matches.into_iter().map(|entry| entry.field_path).collect(),
                span: tc.error_span(span),
            });
            ExpectedProjectionDecision::Failed
        }
        [] => projection_failure(tc, span, source, target),
    }
}

fn projection_matches(
    tc: &mut TypeChecker,
    span: Span,
    source: &Type,
    target: &Type,
    mode: ExpectedProjectionMode,
) -> Vec<ProjectionPath> {
    tc.decls
        .projection_paths_from(source)
        .into_iter()
        .filter(|path| satisfies_without_effects(tc, span, &path.target_ty, target, mode))
        .collect()
}

pub(super) fn unique_projection_without_effects(
    tc: &mut TypeChecker,
    span: Span,
    source: &Type,
    target: &Type,
    mode: ExpectedProjectionMode,
) -> Option<ProjectionPath> {
    if satisfies_without_effects(tc, span, source, target, mode)
        || projection_probe_deferred(source, target)
    {
        return None;
    }

    let matches = projection_matches(tc, span, source, target, mode);
    match matches.as_slice() {
        [entry] => Some(entry.clone()),
        [] | [_, ..] => None,
    }
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

pub(super) fn assignable_without_errors(
    tc: &TypeChecker,
    span: Span,
    source: &Type,
    target: &Type,
) -> bool {
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
    match mode {
        ExpectedProjectionMode::Assignable => {
            dyn_assignable_without_effects(tc, span, source, target)
                || assignable_without_errors(tc, span, source, target)
        }
        ExpectedProjectionMode::CastAcceptingParam => {
            dyn_assignable_without_effects(tc, span, source, target)
                || assignable_without_errors(tc, span, source, target)
                || cast_from_without_effects(tc, source, target)
        }
        ExpectedProjectionMode::ExplicitCast => tc.explicit_cast_without_effects(source, target),
    }
}

fn cast_from_without_effects(tc: &mut TypeChecker, source: &Type, target: &Type) -> bool {
    let used_imports = tc.used_imports.clone();
    let ok = tc.cast_from_conversion_escape(source, target).is_some();
    tc.used_imports = used_imports;
    ok
}

fn dyn_assignable_without_effects(
    tc: &mut TypeChecker,
    span: Span,
    source: &Type,
    target: &Type,
) -> bool {
    match target {
        Type::Dyn(contract) => to_dyn_without_effects(tc, span, source, contract),
        target => {
            let Some(inner) = tc.decls.semantic_option_inner(target).cloned() else {
                return false;
            };
            let Type::Dyn(contract) = &inner else {
                return false;
            };
            if source == &inner {
                return true;
            }
            if tc.decls.semantic_option_inner(source).is_some() {
                return false;
            }
            to_dyn_without_effects(tc, span, source, contract)
        }
    }
}

fn to_dyn_without_effects(
    tc: &mut TypeChecker,
    span: Span,
    source: &Type,
    contract: &ContractRef,
) -> bool {
    match (source, contract) {
        (Type::Infer | Type::Var(_), _) => false,
        _ if tc.decls.semantic_option_inner(source).is_some() => false,
        (_, ContractRef::Hole(_)) => true,
        (Type::Dyn(ContractRef::Hole(_)), _) => true,
        (Type::Dyn(source), target) => {
            contracts::contract_ref_subset(&tc.decls, &tc.current_module, source, target)
        }
        _ => contract_satisfied_without_effects(tc, span, source, contract),
    }
}

fn contract_satisfied_without_effects(
    tc: &mut TypeChecker,
    span: Span,
    source: &Type,
    contract: &ContractRef,
) -> bool {
    let error_count = tc.errors.len();
    let warning_count = tc.warnings.len();
    let lint_count = tc.lint_events.len();
    let used_imports = tc.used_imports.clone();
    let promoted_surfaces = tc.promoted_surfaces.clone();
    let ok = contracts::match_contract(tc, source, contract, span).is_ok()
        && tc.errors.len() == error_count;
    tc.errors.truncate(error_count);
    tc.warnings.truncate(warning_count);
    tc.lint_events.truncate(lint_count);
    tc.used_imports = used_imports;
    tc.promoted_surfaces = promoted_surfaces;
    ok
}

fn projection_probe_deferred(source: &Type, target: &Type) -> bool {
    let pending = |ty: &Type| matches!(ty, Type::Infer | Type::InferReturn | Type::Var(_));
    pending(source) || pending(target)
}

fn projection_failure(
    tc: &mut TypeChecker,
    span: Span,
    source: &Type,
    target: &Type,
) -> ExpectedProjectionDecision {
    let paths = tc.decls.bare_embed_paths_to_type(source, target);
    if !paths.is_empty() {
        tc.push_error(TypeError::MissingProjection {
            source: source.clone(),
            target: target.clone(),
            paths,
            span: tc.error_span(span),
        });
        return ExpectedProjectionDecision::Failed;
    }

    ExpectedProjectionDecision::NotNeeded
}
