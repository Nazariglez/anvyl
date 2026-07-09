use super::{
    CheckedType, TypeChecker, TypeError, TypeHandle, check_value_expr_checked_with_hint,
    checked_from_type, contracts, convert, dyn_infer,
    place::{
        self, CheckedPlace, PlaceAccess, PlaceIdentity, PlaceUseFacts, check_place,
        record_mut_borrow,
    },
    type_ops::{type_closure_facts, type_depends_on_generics},
};
use crate::{
    ast::{ContractRef, ExprId, ExprNode, FailableCastNode, Type},
    span::{SourceSpan, Span},
};

pub(super) struct DowncastSite {
    pub(super) id: ExprId,
    pub(super) source_id: ExprId,
    pub(super) span: SourceSpan,
}

pub(super) enum DowncastSourcePolicy {
    Value,
    MutablePlace,
}

pub(super) enum DowncastSourceContext {
    ExactDowncast,
    DynamicMatch,
}

pub(super) struct CheckedDowncast {
    pub(super) target: Option<Type>,
    pub(super) source: CheckedDowncastSource,
}

pub(super) struct CheckedDowncastSource {
    pub(super) handle: TypeHandle,
    pub(super) contract: Option<ContractRef>,
    pub(super) valid: bool,
    pub(super) alias: Option<DowncastAliasSource>,
}

pub(super) struct DowncastAliasSource {
    pub(super) access: PlaceAccess,
    pub(super) identity: PlaceIdentity,
    pub(super) facts: PlaceUseFacts,
    pub(super) accepts_extern_any: bool,
}

impl CheckedDowncastSource {
    pub(super) fn valid_contract(&self) -> Option<&ContractRef> {
        self.contract.as_ref().filter(|_| self.valid)
    }

    pub(super) fn valid_alias(&self) -> Option<&DowncastAliasSource> {
        self.alias.as_ref().filter(|_| self.valid)
    }

    fn mutable(&self) -> bool {
        self.alias.is_some()
    }
}

impl DowncastAliasSource {
    pub(super) fn target(&self, access: PlaceAccess) -> place::AliasTarget {
        place::AliasTarget {
            access,
            identity: self.identity.clone(),
            facts: self.facts.clone(),
            accepts_extern_any: self.accepts_extern_any,
        }
    }
}

pub(super) fn check_conditional_place(
    downcast: &FailableCastNode,
    target: Type,
    place: CheckedPlace,
    site: Option<&DowncastSite>,
    tc: &mut TypeChecker,
) -> CheckedDowncast {
    let target = runtime_target(tc, target, downcast.span);
    let handle = place.checked().handle.clone();
    let (contract, mut valid) = source_contract(
        &place.checked().ty,
        downcast.node.expr.span,
        DowncastSourceContext::ExactDowncast,
        tc,
    );
    if !place.value.access.can_mut_borrow() {
        tc.push_error(TypeError::RefPatternRequiresMutablePlace {
            span: tc.error_span(downcast.node.expr.span),
        });
        valid = false;
    }
    record_mut_borrow(downcast.node.expr.node.id, &place.value, tc);
    let accepts_extern_any = place.accepts_extern_any();
    let source = CheckedDowncastSource {
        handle,
        contract,
        valid,
        alias: Some(DowncastAliasSource {
            access: place.value.access,
            identity: place.value.identity,
            facts: place.value.facts,
            accepts_extern_any,
        }),
    };
    if let (Some(site), Some(target), Some(contract)) = (site, &target, source.valid_contract()) {
        record_fact(site, contract, target.clone(), source.mutable(), tc);
    }
    CheckedDowncast { target, source }
}

pub(super) fn check_expr_with_source(
    expr: &ExprNode,
    downcast: &FailableCastNode,
    target: Type,
    expected: Option<&TypeHandle>,
    checked_source: CheckedType,
    tc: &mut TypeChecker,
) -> CheckedType {
    let site = DowncastSite {
        id: expr.node.id,
        source_id: downcast.node.expr.node.id,
        span: tc.source_span(expr.span),
    };
    let target = runtime_target(tc, target, downcast.span);
    let source = checked_value_source(checked_source, downcast.node.expr.span, tc);
    if let (Some(target), Some(contract)) = (&target, source.valid_contract()) {
        record_fact(&site, contract, target.clone(), source.mutable(), tc);
    }
    let Some(target) = target else {
        return checked_from_type(expr, Type::Infer, tc);
    };
    let expected_ty = expected.map(|handle| tc.handle_type(handle));
    let ty = convert::expected_optional_payload_dyn(
        tc,
        expr.span,
        expr.node.id,
        &target,
        expected_ty.as_ref(),
    )
    .unwrap_or_else(|| tc.core_option_or_infer(target, expr.span));
    checked_from_type(expr, ty, tc)
}

fn checked_value_source(
    checked: CheckedType,
    span: Span,
    tc: &mut TypeChecker,
) -> CheckedDowncastSource {
    let (contract, valid) =
        source_contract(&checked.ty, span, DowncastSourceContext::ExactDowncast, tc);
    CheckedDowncastSource {
        handle: checked.handle,
        contract,
        valid,
        alias: None,
    }
}

pub(super) fn check_target_ref(tc: &mut TypeChecker, target: &Type, span: Span) -> Option<Type> {
    let target = tc.resolve_type_for_tc_at(target, span);
    runtime_target(tc, target, span)
}

pub(super) fn check_source(
    source: &ExprNode,
    policy: &DowncastSourcePolicy,
    context: DowncastSourceContext,
    tc: &mut TypeChecker,
) -> CheckedDowncastSource {
    match policy {
        DowncastSourcePolicy::Value => {
            let checked = check_value_expr_checked_with_hint(source, None, tc);
            let (contract, valid) = source_contract(&checked.ty, source.span, context, tc);
            CheckedDowncastSource {
                handle: checked.handle,
                contract,
                valid,
                alias: None,
            }
        }
        DowncastSourcePolicy::MutablePlace => {
            let place = check_place(source, tc);
            let handle = place.checked().handle.clone();
            let (contract, mut valid) =
                source_contract(&place.checked().ty, source.span, context, tc);
            if !place.value.access.can_mut_borrow() {
                tc.push_error(TypeError::RefPatternRequiresMutablePlace {
                    span: tc.error_span(source.span),
                });
                valid = false;
            }
            record_mut_borrow(source.node.id, &place.value, tc);
            let accepts_extern_any = place.accepts_extern_any();
            CheckedDowncastSource {
                handle,
                contract,
                valid,
                alias: Some(DowncastAliasSource {
                    access: place.value.access,
                    identity: place.value.identity,
                    facts: place.value.facts,
                    accepts_extern_any,
                }),
            }
        }
    }
}

fn source_contract(
    ty: &Type,
    span: Span,
    context: DowncastSourceContext,
    tc: &mut TypeChecker,
) -> (Option<ContractRef>, bool) {
    match ty {
        Type::Dyn(contract) => (Some(contract.clone()), true),
        Type::Infer => (None, false),
        _ => {
            tc.push_error(TypeError::CompileError {
                message: context.non_dyn_source_message().to_string(),
                span: tc.error_span(span),
            });
            (None, false)
        }
    }
}

impl DowncastSourceContext {
    fn non_dyn_source_message(self) -> &'static str {
        match self {
            Self::ExactDowncast => "exact downcast source must be a dynamic value",
            Self::DynamicMatch => "dynamic match source must be a dynamic value",
        }
    }
}

pub(super) fn record_fact(
    site: &DowncastSite,
    source_contract: &ContractRef,
    target: Type,
    mutable: bool,
    tc: &mut TypeChecker,
) {
    let expr_site = tc.current_expr_site(site.id);
    let source_site = tc.current_expr_site(site.source_id);
    if let Some(source) =
        contracts::contract_set_key_for_ref(&tc.decls, &tc.current_module, source_contract)
    {
        tc.record_resolved_dyn_downcast(
            expr_site,
            &source_site,
            source,
            target,
            mutable,
            site.span,
        );
    } else if let Some(hole) = dyn_infer::hole_id(source_contract) {
        tc.dyn_infer.add_downcast(
            tc.current_module.clone(),
            expr_site,
            source_site,
            hole,
            target,
            mutable,
            site.span,
        );
    }
}

fn runtime_target(tc: &mut TypeChecker, target: Type, span: Span) -> Option<Type> {
    match &target {
        Type::Dyn(_) => {
            tc.push_error(TypeError::CompileError {
                message: "downcast tests the stored concrete type; use a wider dynamic type at the conversion site instead of downcasting to another contract".to_string(),
                span: tc.error_span(span),
            });
            return None;
        }
        Type::Infer => return None,
        _ => {}
    }
    if runtime_target_has_unresolved(&target) {
        tc.push_error(TypeError::CompileError {
            message: "exact downcast target must be a fully concrete runtime-identifiable type"
                .to_string(),
            span: tc.error_span(span),
        });
        return None;
    }
    if tc.decls.key_for_type(&target).is_some() {
        Some(target)
    } else {
        tc.push_error(TypeError::CompileError {
            message: "exact downcast target must be a concrete nominal type".to_string(),
            span: tc.error_span(span),
        });
        None
    }
}

pub(super) fn runtime_target_valid(tc: &TypeChecker, target: &Type) -> bool {
    !matches!(target, Type::Dyn(_) | Type::Infer)
        && !runtime_target_has_unresolved(target)
        && tc.decls.key_for_type(target).is_some()
}

fn runtime_target_has_unresolved(target: &Type) -> bool {
    let facts = type_closure_facts(target);
    facts.first_unresolved.is_some()
        || facts.infer.contains_type
        || facts.infer.contains_return
        || facts.contains_unresolved_const
        || type_depends_on_generics(target)
}
