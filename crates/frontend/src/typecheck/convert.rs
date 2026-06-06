use super::{
    CastConversionMatch, CastFromConversion, CheckedType, DynContainerConversionKind, EnumRepr,
    TypeChecker, TypeError, TypeHandle, check_value_expr_checked_with_hint, checked_from_type,
    contracts::{self, ContractMatchError, RequirementError},
    projection::{
        ExpectedFit, ExpectedProjectionMode, SourceAcceptance, apply_value_projection,
        classify_expected_fit,
    },
};
use crate::{
    ast::{CastNode, ContractRef, DynContractHoleId, ExprId, ExprNode, Ident, Type, TypeVisitor},
    span::Span,
};

#[derive(Clone)]
pub(super) enum ExplicitCast {
    Identity,
    Builtin,
    RawEnum,
    CastFrom(CastFromConversion),
}

impl TypeChecker {
    pub(super) fn expect_assignable(&mut self, span: Span, from: TypeHandle, to: TypeHandle) {
        expect_assignable(self, span, None, from, to);
    }

    pub(super) fn expect_assignable_expr(
        &mut self,
        span: Span,
        expr_id: ExprId,
        from: TypeHandle,
        to: TypeHandle,
    ) {
        expect_assignable(self, span, Some(expr_id), from, to);
    }

    pub(super) fn expect_equal(&mut self, span: Span, left: TypeHandle, right: TypeHandle) {
        if self.handle_is_poison(&left) || self.handle_is_poison(&right) {
            return;
        }
        self.solver
            .add_handle_equal(self.error_span(span), left, right);
    }

    fn explicit_cast_conversion(&mut self, source: &Type, target: &Type) -> Option<ExplicitCast> {
        if source == target {
            return Some(ExplicitCast::Identity);
        }
        if builtin_numeric_cast(source, target) {
            return Some(ExplicitCast::Builtin);
        }
        match classify_raw_enum_cast(&self.decls, source, target) {
            Some(RawEnumCast::Accept) => return Some(ExplicitCast::RawEnum),
            Some(RawEnumCast::Reject) => return None,
            None => {}
        }
        self.cast_from_conversion(source, target)
            .map(ExplicitCast::CastFrom)
    }

    pub(super) fn explicit_cast_plan_without_effects(
        &mut self,
        source: &Type,
        target: &Type,
    ) -> Option<ExplicitCast> {
        self.probe_compatibility_without_effects(|tc| tc.explicit_cast_conversion(source, target))
    }

    pub(super) fn cast_from_conversion(
        &mut self,
        source: &Type,
        target: &Type,
    ) -> Option<CastFromConversion> {
        match self
            .decls
            .find_cast_conversion(source, target, |ext| self.extend_visible(ext))
        {
            Some(CastConversionMatch::Match(conversion)) => Some(conversion),
            Some(CastConversionMatch::Ambiguous) | None => None,
        }
    }

    pub(super) fn cast_from_ambiguous(&mut self, source: &Type, target: &Type) -> bool {
        matches!(
            self.decls
                .find_cast_conversion(source, target, |ext| self.extend_visible(ext)),
            Some(CastConversionMatch::Ambiguous)
        )
    }
}

fn expect_assignable(
    tc: &mut TypeChecker,
    span: Span,
    expr_id: Option<ExprId>,
    from: TypeHandle,
    to: TypeHandle,
) {
    if tc.handle_is_poison(&from) || tc.handle_is_poison(&to) {
        return;
    }
    let from_ty = tc.handle_type(&from);
    let to_ty = tc.handle_type(&to);
    if try_expected_dyn(tc, span, expr_id, &from_ty, &to_ty) {
        return;
    }
    if let Some(kind) = dyn_container_conversion_kind(&from_ty, &to_ty) {
        tc.push_error(TypeError::DynContainerConversion {
            kind,
            span: tc.error_span(span),
        });
        return;
    }
    tc.solver
        .add_handle_assignable(tc.error_span(span), from, to);
}

fn dyn_container_conversion_kind(from: &Type, to: &Type) -> Option<DynContainerConversionKind> {
    if from == to {
        return None;
    }
    match (from, to) {
        (Type::List { elem: from }, Type::List { elem: to }) => dyn_collection_kind(from, to),
        (Type::Array { elem: from, .. }, Type::Array { elem: to, .. }) => {
            dyn_collection_kind(from, to).map(|kind| match kind {
                DynContainerConversionKind::Collection => DynContainerConversionKind::FixedArray,
                other => other,
            })
        }
        (
            Type::Slice { elem: from } | Type::Array { elem: from, .. } | Type::List { elem: from },
            Type::Slice { elem: to },
        ) if dyn_elem_mismatch(from, to) => Some(DynContainerConversionKind::Slice),
        (
            Type::Map {
                key: from_key,
                value: from_value,
            },
            Type::Map {
                key: to_key,
                value: to_value,
            },
        ) if from_key == to_key && dyn_elem_mismatch(from_value, to_value) => {
            Some(DynContainerConversionKind::MapValue)
        }
        _ => None,
    }
}

fn dyn_collection_kind(from: &Type, to: &Type) -> Option<DynContainerConversionKind> {
    if from == to {
        return None;
    }
    match (from, to) {
        (Type::Dyn(_), Type::Dyn(_)) => Some(DynContainerConversionKind::DynamicWeakening),
        (_, Type::Dyn(_)) | (Type::Dyn(_), _) => Some(DynContainerConversionKind::Collection),
        _ => None,
    }
}

fn dyn_elem_mismatch(from: &Type, to: &Type) -> bool {
    from != to && (contains_dyn(from) || contains_dyn(to))
}

fn contains_dyn(ty: &Type) -> bool {
    DynTypeVisitor.visit_type(ty)
}

struct DynTypeVisitor;

impl TypeVisitor for DynTypeVisitor {
    fn visit_type_leaf(&mut self, ty: &Type) -> bool {
        matches!(ty, Type::Dyn(_))
    }
}

pub(super) fn expected_optional_payload_dyn(
    tc: &mut TypeChecker,
    span: Span,
    expr_id: ExprId,
    payload: &Type,
    expected: Option<&Type>,
) -> Option<Type> {
    let expected = expected?;
    optional_dyn_payload(tc, expected)?;
    try_expected_dyn(tc, span, Some(expr_id), payload, expected).then(|| expected.clone())
}

fn optional_dyn_payload(tc: &TypeChecker, ty: &Type) -> Option<(Type, ContractRef)> {
    let inner = tc.decls.semantic_option_inner(ty)?.clone();
    let Type::Dyn(contract) = &inner else {
        return None;
    };
    Some((inner.clone(), contract.clone()))
}

enum ExpectedDynFit {
    NoDyn,
    Accepted(ExpectedDynPlan),
    Rejected(ExpectedDynRejection),
}

#[derive(Clone)]
pub(super) enum ExpectedDynPlan {
    Noop,
    ConcreteToHole {
        hole: DynContractHoleId,
        source: Type,
    },
    DynToHole {
        hole: DynContractHoleId,
        source: ContractRef,
    },
    DynHoleToTarget {
        hole: DynContractHoleId,
        target: ContractRef,
    },
    DynWeakening {
        source: ContractRef,
        target: ContractRef,
    },
    ConcreteToDyn {
        matched: contracts::ContractMatch,
    },
}

enum ExpectedDynRejection {
    IndependentHoles,
    DynStrengthening {
        source: ContractRef,
        target: ContractRef,
    },
    ContractUnsatisfied {
        source: Type,
        contract: ContractRef,
        error: ContractMatchError,
    },
}

pub(super) fn expected_dyn_plan_without_effects(
    tc: &mut TypeChecker,
    span: Span,
    from: &Type,
    to: &Type,
) -> Option<ExpectedDynPlan> {
    match tc.probe_compatibility_without_effects(|tc| expected_dyn_fit(tc, span, from, to)) {
        ExpectedDynFit::Accepted(plan) => Some(plan),
        ExpectedDynFit::NoDyn | ExpectedDynFit::Rejected(_) => None,
    }
}

fn try_expected_dyn(
    tc: &mut TypeChecker,
    span: Span,
    expr_id: Option<ExprId>,
    from: &Type,
    to: &Type,
) -> bool {
    if let Some(plan) = expected_dyn_plan_without_effects(tc, span, from, to) {
        apply_expected_dyn_plan(tc, span, expr_id, plan);
        return true;
    }

    match expected_dyn_fit(tc, span, from, to) {
        ExpectedDynFit::NoDyn => false,
        ExpectedDynFit::Accepted(_) => unreachable!(),
        ExpectedDynFit::Rejected(rejection) => {
            push_expected_dyn_rejection(tc, span, rejection);
            true
        }
    }
}

fn expected_dyn_fit(tc: &mut TypeChecker, span: Span, from: &Type, to: &Type) -> ExpectedDynFit {
    match to {
        Type::Dyn(contract) => to_dyn_fit(tc, span, from, contract),
        to => {
            let Some((inner, contract)) = optional_dyn_payload(tc, to) else {
                return ExpectedDynFit::NoDyn;
            };
            if from == &inner {
                return ExpectedDynFit::Accepted(ExpectedDynPlan::Noop);
            }
            if tc.decls.semantic_option_inner(from).is_some() {
                return ExpectedDynFit::NoDyn;
            }
            to_dyn_fit(tc, span, from, &contract)
        }
    }
}

fn to_dyn_fit(
    tc: &mut TypeChecker,
    span: Span,
    from: &Type,
    contract: &ContractRef,
) -> ExpectedDynFit {
    match (from, contract) {
        (_, ContractRef::Hole(hole)) => to_dyn_hole_fit(tc, from, *hole),
        (Type::Dyn(ContractRef::Hole(hole)), target) => {
            ExpectedDynFit::Accepted(ExpectedDynPlan::DynHoleToTarget {
                hole: *hole,
                target: target.clone(),
            })
        }
        (Type::Dyn(source), target) => dyn_weakening_fit(tc, source, target),
        (_, target) => concrete_to_dyn_fit(tc, span, from, target),
    }
}

fn to_dyn_hole_fit(tc: &TypeChecker, from: &Type, target: DynContractHoleId) -> ExpectedDynFit {
    match from {
        Type::Dyn(ContractRef::Hole(source)) if *source != target => {
            ExpectedDynFit::Rejected(ExpectedDynRejection::IndependentHoles)
        }
        Type::Dyn(ContractRef::Hole(_)) => ExpectedDynFit::Accepted(ExpectedDynPlan::Noop),
        Type::Dyn(source) => ExpectedDynFit::Accepted(ExpectedDynPlan::DynToHole {
            hole: target,
            source: source.clone(),
        }),
        Type::Infer | Type::Var(_) => ExpectedDynFit::NoDyn,
        _ if tc.decls.semantic_option_inner(from).is_some() => ExpectedDynFit::NoDyn,
        _ => ExpectedDynFit::Accepted(ExpectedDynPlan::ConcreteToHole {
            hole: target,
            source: from.clone(),
        }),
    }
}

fn dyn_weakening_fit(
    tc: &TypeChecker,
    source: &ContractRef,
    target: &ContractRef,
) -> ExpectedDynFit {
    if contracts::contract_ref_subset(&tc.decls, &tc.current_module, source, target) {
        ExpectedDynFit::Accepted(ExpectedDynPlan::DynWeakening {
            source: source.clone(),
            target: target.clone(),
        })
    } else {
        ExpectedDynFit::Rejected(ExpectedDynRejection::DynStrengthening {
            source: source.clone(),
            target: target.clone(),
        })
    }
}

fn concrete_to_dyn_fit(
    tc: &mut TypeChecker,
    span: Span,
    from: &Type,
    contract: &ContractRef,
) -> ExpectedDynFit {
    if matches!(from, Type::Infer | Type::Var(_) | Type::Dyn(_))
        || tc.decls.semantic_option_inner(from).is_some()
    {
        return ExpectedDynFit::NoDyn;
    }
    match contracts::match_contract(tc, from, contract, span) {
        Ok(matched) => ExpectedDynFit::Accepted(ExpectedDynPlan::ConcreteToDyn { matched }),
        Err(error) => ExpectedDynFit::Rejected(ExpectedDynRejection::ContractUnsatisfied {
            source: from.clone(),
            contract: contract.clone(),
            error,
        }),
    }
}

pub(super) fn apply_expected_dyn_plan(
    tc: &mut TypeChecker,
    span: Span,
    expr_id: Option<ExprId>,
    plan: ExpectedDynPlan,
) {
    match plan {
        ExpectedDynPlan::Noop => {}
        ExpectedDynPlan::ConcreteToHole { hole, source } => {
            tc.dyn_infer.add_conversion(
                tc.current_module.clone(),
                expr_id.map(|id| tc.current_expr_site(id)),
                source,
                hole,
                span,
                tc.source_span(span),
            );
        }
        ExpectedDynPlan::DynToHole { hole, source } => {
            tc.dyn_infer.add_dyn_source(
                tc.current_module.clone(),
                expr_id.map(|id| tc.current_expr_site(id)),
                source,
                hole,
                span,
                tc.source_span(span),
            );
        }
        ExpectedDynPlan::DynHoleToTarget { hole, target } => {
            tc.dyn_infer.add_hole_target(
                tc.current_module.clone(),
                expr_id.map(|id| tc.current_expr_site(id)),
                hole,
                target,
                span,
                tc.source_span(span),
            );
        }
        ExpectedDynPlan::DynWeakening { source, target } => {
            if let Some(expr_id) = expr_id
                && source != target
                && let (Some(source), Some(target)) = (
                    contracts::contract_set_key_for_ref(&tc.decls, &tc.current_module, &source),
                    contracts::contract_set_key_for_ref(&tc.decls, &tc.current_module, &target),
                )
            {
                let site = tc.current_expr_site(expr_id);
                tc.record_dyn_weakening_at(site, source, target, tc.source_span(span));
            }
        }
        ExpectedDynPlan::ConcreteToDyn { matched } => {
            contracts::apply_match_access(tc, &matched, span);
            let witness = contracts::plan_witness(tc, &matched, span);
            if let Some(expr_id) = expr_id {
                let site = tc.current_expr_site(expr_id);
                tc.record_dyn_conversion_at(site, witness, tc.source_span(span));
            }
        }
    }
}

fn push_expected_dyn_rejection(tc: &mut TypeChecker, span: Span, rejection: ExpectedDynRejection) {
    match rejection {
        ExpectedDynRejection::IndependentHoles => {
            tc.push_error(TypeError::CompileError {
                message: "cannot infer dynamic contract across independent holes".to_string(),
                span: tc.error_span(span),
            });
        }
        ExpectedDynRejection::DynStrengthening { source, target } => {
            tc.push_error(TypeError::CompileError {
                message: format!(
                    "dynamic value '{source}' cannot be used as '{target}'; implicit dynamic strengthening is not allowed"
                ),
                span: tc.error_span(span),
            });
        }
        ExpectedDynRejection::ContractUnsatisfied {
            source,
            contract,
            error,
        } => push_match_error(tc, &source, &contract, error, span),
    }
}

pub(super) fn push_match_error(
    tc: &mut TypeChecker,
    from: &Type,
    contract: &ContractRef,
    error: ContractMatchError,
    span: Span,
) {
    let detail = match error {
        ContractMatchError::UnknownContract => "unknown contract".to_string(),
        ContractMatchError::ConflictingRequirement(name) => {
            format!("conflicting contract requirement '{name}'")
        }
        ContractMatchError::Unsatisfied(err) => {
            requirement_error_detail(err.requirement.name, &err.reason)
        }
    };
    tc.push_error(TypeError::ContractUnsatisfied {
        ty: from.clone(),
        contract: contract.to_string(),
        detail,
        span: tc.error_span(span),
    });
}

fn requirement_error_detail(name: Ident, error: &RequirementError) -> String {
    let reason = match error {
        RequirementError::Missing => return format!("missing method '{name}'"),
        RequirementError::Ambiguous => "is ambiguous",
        RequirementError::UnboundGeneric(_) => "has unbound generic parameters",
        RequirementError::GenericMethod => "is generic",
        RequirementError::Receiver { .. } => "has an incompatible receiver",
        RequirementError::Arity { .. } => "has an incompatible parameter count",
        RequirementError::Param { index, .. } => {
            return format!(
                "method '{name}' has an incompatible parameter at position {}",
                index + 1
            );
        }
        RequirementError::Return { .. } => "has an incompatible return type",
    };
    format!("method '{name}' {reason}")
}

enum RawEnumCast {
    Accept,
    Reject,
}

fn classify_raw_enum_cast(
    decls: &super::DeclarationIndex,
    source: &Type,
    target: &Type,
) -> Option<RawEnumCast> {
    if matches!(target, Type::Int | Type::String)
        && let Some(key) = decls.key_for_type(source)
        && decls
            .enum_repr_for_key(&key)
            .is_some_and(|repr| repr != EnumRepr::Adt)
    {
        return Some(if decls.raw_enum_raw_type(&key).as_ref() == Some(target) {
            RawEnumCast::Accept
        } else {
            RawEnumCast::Reject
        });
    }
    None
}

fn push_raw_enum_cast_error(tc: &mut TypeChecker, span: Span, from: &Type, to: &Type) -> bool {
    let from_key = tc.decls.key_for_type(from);
    if let Some(key) = from_key
        && matches!(to, Type::Int | Type::String)
    {
        match tc.decls.enum_repr_for_key(&key) {
            Some(EnumRepr::RawInt | EnumRepr::RawString) => {
                if let Some(raw_ty) = tc.decls.raw_enum_raw_type(&key) {
                    tc.push_error(TypeError::RawEnumWrongRawCast {
                        enum_ty: from.clone(),
                        expected: raw_ty,
                        found: to.clone(),
                        span: tc.error_span(span),
                    });
                    return true;
                }
            }
            Some(EnumRepr::Adt) => {
                tc.push_error(TypeError::NonRawEnumRawCast {
                    enum_ty: from.clone(),
                    raw_ty: to.clone(),
                    span: tc.error_span(span),
                });
                return true;
            }
            None => {}
        }
    }
    false
}

fn builtin_numeric_cast(source: &Type, target: &Type) -> bool {
    matches!(
        (source, target),
        (Type::Int, Type::Float) | (Type::Float, Type::Int)
    )
}

pub(super) fn check_cast_expr(
    expr: &ExprNode,
    cast: &CastNode,
    tc: &mut TypeChecker,
) -> CheckedType {
    let target = tc.resolve_type_for_tc_at(&cast.node.target, cast.span);
    let checked = check_value_expr_checked_with_hint(&cast.node.expr, None, tc);
    let from = checked.ty.clone();
    match classify_expected_fit(
        tc,
        cast.node.expr.span,
        &from,
        &target,
        ExpectedProjectionMode::ExplicitCast,
    ) {
        ExpectedFit::SourceAccepted(SourceAcceptance::ExplicitCast { conversion }) => {
            apply_explicit_cast_effects(expr, cast, conversion, &from, &target, tc);
            cast_expr_checked(expr, target, checked.contains_extern_any, tc)
        }
        ExpectedFit::Project {
            projection,
            acceptance: SourceAcceptance::ExplicitCast { conversion },
        } => {
            let projected =
                apply_value_projection(tc, &cast.node.expr, &checked, &from, projection);
            apply_explicit_cast_effects(expr, cast, conversion, &projected.ty, &target, tc);
            cast_expr_checked(expr, target, projected.contains_extern_any, tc)
        }
        ExpectedFit::Deferred if matches!(from, Type::Infer) || matches!(target, Type::Infer) => {
            cast_expr_checked(expr, target, checked.contains_extern_any, tc)
        }
        fit @ (ExpectedFit::Ambiguous(_) | ExpectedFit::MissingProjection { .. }) => {
            match super::projection::expected_projection_decision(
                tc,
                cast.node.expr.span,
                &from,
                &target,
                fit,
            ) {
                super::projection::ExpectedProjectionDecision::Failed => {}
                super::projection::ExpectedProjectionDecision::SourceAccepted
                | super::projection::ExpectedProjectionDecision::NotNeeded
                | super::projection::ExpectedProjectionDecision::Project(_) => unreachable!(),
            }
            cast_expr_checked(expr, Type::Infer, checked.contains_extern_any, tc)
        }
        ExpectedFit::Deferred | ExpectedFit::Mismatch => {
            if !push_raw_enum_cast_error(tc, cast.span, &from, &target) {
                if tc.cast_from_ambiguous(&from, &target) {
                    tc.push_error(TypeError::AmbiguousCast {
                        from,
                        to: target,
                        span: tc.error_span(cast.span),
                    });
                } else {
                    tc.push_error(TypeError::InvalidCast {
                        from,
                        to: target,
                        span: tc.error_span(cast.span),
                    });
                }
            }
            cast_expr_checked(expr, Type::Infer, checked.contains_extern_any, tc)
        }
        ExpectedFit::SourceAccepted(_) | ExpectedFit::Project { .. } => unreachable!(),
    }
}

fn apply_explicit_cast_effects(
    expr: &ExprNode,
    cast: &CastNode,
    conversion: ExplicitCast,
    source: &Type,
    target: &Type,
    tc: &mut TypeChecker,
) {
    match conversion {
        ExplicitCast::Identity => tc
            .closure
            .copy_expr_flow(cast.node.expr.node.id, expr.node.id),
        ExplicitCast::CastFrom(conversion) => {
            super::body::check_cast_from_conversion_body(&conversion, source, target, tc);
            tc.mark_activation_imports_used(&conversion.origin);
            tc.check_argument_escape(&cast.node.expr, conversion.escape);
        }
        ExplicitCast::Builtin | ExplicitCast::RawEnum => {}
    }
}

fn cast_expr_checked(
    expr: &ExprNode,
    ty: Type,
    contains_extern_any: bool,
    tc: &mut TypeChecker,
) -> CheckedType {
    let mut casted = checked_from_type(expr, ty, tc);
    casted.contains_extern_any = contains_extern_any;
    casted
}
