use super::{
    PatternBindMode, PatternContext, PlaceAccess, TypeChecker, TypeError, downcast,
    pattern::{self, PatternCheckResult, PatternOutcome, PatternPlace},
    semantic_use::{CheckedDynMatchArm, CheckedDynMatchBinding, CheckedDynMatchFallback},
};
use crate::{
    ast::{ExprId, Ident, Match, MatchArmHead, MatchArmNode, Type},
    span::Span,
};

pub(super) fn check_arm_head_detailed(
    head: &MatchArmHead,
    scrutinee: PatternPlace,
    mode: PatternBindMode,
    scrutinee_id: ExprId,
    tc: &mut TypeChecker,
) -> PatternCheckResult {
    match head {
        MatchArmHead::Pattern(pattern) => pattern::check_place_at_detailed(
            pattern,
            scrutinee,
            mode,
            scrutinee_id,
            PatternContext::Match,
            tc,
        ),
        MatchArmHead::DynDowncast(_) | MatchArmHead::DynFallback(_) => {
            PatternCheckResult::empty(PatternOutcome::error())
        }
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
    if let Some(local) = tc.direct_local_id(&node.scrutinee) {
        tc.record_local_use(
            node.scrutinee.node.id,
            local,
            if node.access.is_ref() {
                super::LocalUseMode::MutBorrow
            } else {
                super::LocalUseMode::Read
            },
        );
    }
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
    plans: &mut Vec<CheckedDynMatchArm>,
    fallback: &mut Option<CheckedDynMatchFallback>,
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
            let binding = match (dyn_arm.node.binding, target, source.valid) {
                (Some(name), Some(target), true) => {
                    Some(define_downcast_binding(name, &target, source, tc))
                }
                (Some(name), _, _) => Some(define_recovery_binding(name, tc)),
                (None, _, _) => None,
            };
            plans.push(CheckedDynMatchArm {
                downcast: dyn_arm.node.id,
                binding,
            });
        }
        MatchArmHead::DynFallback(binding) => {
            let binding = binding.map(|name| define_fallback_binding(name, source, tc));
            *fallback = Some(CheckedDynMatchFallback { binding });
        }
        MatchArmHead::Pattern(_) => {}
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
) -> CheckedDynMatchBinding {
    let handle = TypeChecker::type_handle(target);
    let Some(alias) = source.alias.as_ref() else {
        return dyn_binding(tc.define_pattern_binding_from_handle(name, &handle, false, None));
    };
    dyn_binding(tc.define_ref_alias_binding_from_handle(
        name,
        &handle,
        alias.target(PlaceAccess::Mutable),
        PatternContext::Match,
        None,
    ))
}

fn define_fallback_binding(
    name: Ident,
    source: &downcast::CheckedDowncastSource,
    tc: &mut TypeChecker,
) -> CheckedDynMatchBinding {
    if !source.valid {
        define_recovery_binding(name, tc)
    } else if let Some(alias) = source.alias.as_ref() {
        dyn_binding(tc.define_ref_alias_binding_from_handle(
            name,
            &source.handle,
            alias.target(alias.access),
            PatternContext::Match,
            None,
        ))
    } else {
        dyn_binding(tc.define_pattern_binding_from_handle(name, &source.handle, false, None))
    }
}

fn define_recovery_binding(name: Ident, tc: &mut TypeChecker) -> CheckedDynMatchBinding {
    let handle = TypeChecker::type_handle(&Type::Infer);
    dyn_binding(tc.define_pattern_binding_from_handle(name, &handle, false, None))
}

fn dyn_binding(local: super::DefinedLocal) -> CheckedDynMatchBinding {
    CheckedDynMatchBinding {
        local: local.type_id,
        binding_id: local.binding_id,
    }
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
