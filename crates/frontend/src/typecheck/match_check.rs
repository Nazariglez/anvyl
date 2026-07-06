use super::{
    PatternBindMode, PatternContext, PlaceAccess, TypeChecker, TypeError, downcast,
    pattern::{self, PatternOutcome, PatternPlace},
};
use crate::{
    ast::{ExprId, Ident, Match, MatchArmHead, MatchArmNode, Type},
    span::Span,
};

pub(super) fn check_arm_head(
    head: &MatchArmHead,
    scrutinee: PatternPlace,
    mode: PatternBindMode,
    scrutinee_id: ExprId,
    tc: &mut TypeChecker,
) -> PatternOutcome {
    match head {
        MatchArmHead::Pattern(pattern) => pattern::check_place_at(
            pattern,
            scrutinee,
            mode,
            scrutinee_id,
            PatternContext::Match,
            tc,
        ),
        MatchArmHead::DynDowncast(_) | MatchArmHead::DynFallback(_) => PatternOutcome::error(),
    }
}

pub(super) fn validate_dynamic_arms(arms: &[MatchArmNode], tc: &mut TypeChecker) -> bool {
    let mut valid = true;
    let Some(last) = arms.last() else {
        return false;
    };
    if !matches!(last.node.head, MatchArmHead::DynFallback(_)) {
        tc.push_error(TypeError::CompileError {
            message: "dynamic match requires a final fallback arm".to_string(),
            span: tc.error_span(last.span),
        });
        valid = false;
    }
    for arm in &arms[..arms.len() - 1] {
        if matches!(arm.node.head, MatchArmHead::DynFallback(_)) {
            tc.push_error(TypeError::CompileError {
                message: "dynamic match fallback arm must be last".to_string(),
                span: tc.error_span(arm.span),
            });
            valid = false;
        }
    }
    valid
}

pub(super) fn check_dynamic_source(
    node: &Match,
    tc: &mut TypeChecker,
) -> downcast::CheckedDowncastSource {
    let policy = if node.access.is_ref() {
        downcast::DowncastSourcePolicy::MutablePlace
    } else {
        downcast::DowncastSourcePolicy::Value
    };
    downcast::check_source(
        &node.scrutinee,
        &policy,
        downcast::DowncastSourceContext::DynamicMatch,
        tc,
    )
}

pub(super) fn with_dynamic_arm<R>(
    arm: &MatchArmNode,
    source: &downcast::CheckedDowncastSource,
    scrutinee_id: ExprId,
    targets: &mut Vec<Type>,
    tc: &mut TypeChecker,
    check_body: impl FnOnce(&mut TypeChecker) -> R,
) -> R {
    tc.push_scope();
    match &arm.node.head {
        MatchArmHead::DynDowncast(dyn_arm) => {
            let target = downcast::check_target_ref(tc, &dyn_arm.node.target, dyn_arm.span);
            if let Some(target) = &target {
                let duplicate = targets.contains(target);
                if duplicate {
                    push_duplicate_target(target, dyn_arm.span, tc);
                } else {
                    targets.push(target.clone());
                }
                if let Some(contract) = source.valid_contract().filter(|_| !duplicate) {
                    downcast::record_fact(
                        &downcast::DowncastSite {
                            id: dyn_arm.node.id,
                            source_id: scrutinee_id,
                            span: tc.source_span(dyn_arm.span),
                        },
                        contract,
                        target.clone(),
                        source.alias.is_some(),
                        tc,
                    );
                }
            }
            match (dyn_arm.node.binding, target, source.valid) {
                (Some(name), Some(target), true) => {
                    define_downcast_binding(name, &target, source, tc);
                }
                (Some(name), _, _) => define_recovery_binding(name, tc),
                (None, _, _) => {}
            }
        }
        MatchArmHead::DynFallback(Some(name)) => define_fallback_binding(*name, source, tc),
        MatchArmHead::DynFallback(None) | MatchArmHead::Pattern(_) => {}
    }
    let body = check_body(tc);
    tc.pop_scope();
    body
}

pub(super) fn with_dynamic_arm_recovery<R>(
    arm: &MatchArmNode,
    tc: &mut TypeChecker,
    check_body: impl FnOnce(&mut TypeChecker) -> R,
) -> R {
    tc.push_scope();
    if let Some(name) = head_binding(&arm.node.head) {
        define_recovery_binding(name, tc);
    }
    let body = check_body(tc);
    tc.pop_scope();
    body
}

fn define_downcast_binding(
    name: Ident,
    target: &Type,
    source: &downcast::CheckedDowncastSource,
    tc: &mut TypeChecker,
) {
    let handle = TypeChecker::type_handle(target);
    let Some(alias) = source.alias.as_ref() else {
        tc.define_pattern_binding_from_handle(name, &handle, false, None);
        return;
    };
    tc.define_ref_alias_binding_from_handle(
        name,
        &handle,
        alias.target(PlaceAccess::Mutable),
        PatternContext::Match,
        None,
    );
}

fn define_fallback_binding(
    name: Ident,
    source: &downcast::CheckedDowncastSource,
    tc: &mut TypeChecker,
) {
    if !source.valid {
        define_recovery_binding(name, tc);
    } else if let Some(alias) = source.alias.as_ref() {
        tc.define_ref_alias_binding_from_handle(
            name,
            &source.handle,
            alias.target(alias.access),
            PatternContext::Match,
            None,
        );
    } else {
        tc.define_pattern_binding_from_handle(name, &source.handle, false, None);
    }
}

fn define_recovery_binding(name: Ident, tc: &mut TypeChecker) {
    let handle = TypeChecker::type_handle(&Type::Infer);
    tc.define_pattern_binding_from_handle(name, &handle, false, None);
}

fn push_duplicate_target(target: &Type, span: Span, tc: &mut TypeChecker) {
    tc.push_error(TypeError::CompileError {
        message: format!("duplicate dynamic match target '{target}'"),
        span: tc.error_span(span),
    });
}

fn head_binding(head: &MatchArmHead) -> Option<Ident> {
    match head {
        MatchArmHead::DynDowncast(arm) => arm.node.binding,
        MatchArmHead::DynFallback(binding) => *binding,
        MatchArmHead::Pattern(_) => None,
    }
}
