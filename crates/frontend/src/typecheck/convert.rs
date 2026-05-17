use super::{
    CastConversionMatch, CheckedType, DynContainerConversionKind, DynConversionFact,
    DynWeakeningFact, EscapeMode, TypeChecker, TypeError, TypeHandle,
    check_value_expr_checked_with_hint, checked_from_type,
    contracts::{self, ContractMatchError, RequirementError},
    type_ops::TypeVisitor,
};
use crate::{
    ast::{CastNode, ContractRef, ExprId, ExprNode, Ident, Type},
    span::Span,
};

#[derive(Clone, Copy)]
enum ExplicitCast {
    Identity,
    Builtin,
    CastFrom { escape: EscapeMode },
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
        self.cast_from_conversion_escape(source, target)
            .map(|escape| ExplicitCast::CastFrom { escape })
    }

    pub(super) fn cast_from_conversion_escape(
        &mut self,
        source: &Type,
        target: &Type,
    ) -> Option<EscapeMode> {
        match self
            .decls
            .find_cast_conversion(source, target, |ext| self.extend_visible(ext))
        {
            Some(CastConversionMatch::Match { escape, origin }) => {
                self.mark_activation_imports_used(&origin);
                Some(escape)
            }
            Some(CastConversionMatch::Ambiguous) | None => None,
        }
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

fn try_expected_dyn(
    tc: &mut TypeChecker,
    span: Span,
    expr_id: Option<ExprId>,
    from: &Type,
    to: &Type,
) -> bool {
    match to {
        Type::Dyn(contract) => try_to_dyn(tc, span, expr_id, from, contract),
        to => {
            let Some(inner) = tc.decls.semantic_option_inner(to).cloned() else {
                return false;
            };
            let Type::Dyn(contract) = &inner else {
                return false;
            };
            if from == &inner {
                return true;
            }
            if tc.decls.semantic_option_inner(from).is_some() {
                return false;
            }
            try_to_dyn(tc, span, expr_id, from, contract)
        }
    }
}

fn try_to_dyn(
    tc: &mut TypeChecker,
    span: Span,
    expr_id: Option<ExprId>,
    from: &Type,
    contract: &ContractRef,
) -> bool {
    match (from, contract) {
        (_, ContractRef::Hole(hole)) => try_to_dyn_hole(tc, span, expr_id, from, *hole),
        (Type::Dyn(ContractRef::Hole(hole)), target) => {
            tc.dyn_infer.add_hole_target(
                tc.current_module.clone(),
                expr_id,
                *hole,
                target.clone(),
                span,
            );
            true
        }
        (Type::Dyn(source), target) => try_dyn_weakening(tc, span, expr_id, source, target),
        (_, target) => try_concrete_to_dyn(tc, span, expr_id, from, target),
    }
}

fn try_to_dyn_hole(
    tc: &mut TypeChecker,
    span: Span,
    expr_id: Option<ExprId>,
    from: &Type,
    hole: crate::ast::DynContractHoleId,
) -> bool {
    match from {
        Type::Dyn(ContractRef::Hole(source)) if *source != hole => {
            tc.push_error(TypeError::CompileError {
                message: "cannot infer dynamic contract across independent holes".to_string(),
                span: tc.error_span(span),
            });
            true
        }
        Type::Dyn(ContractRef::Hole(_)) => true,
        Type::Dyn(source) => {
            tc.dyn_infer.add_dyn_source(
                tc.current_module.clone(),
                expr_id,
                source.clone(),
                hole,
                span,
            );
            true
        }
        Type::Infer | Type::Var(_) => false,
        _ if tc.decls.semantic_option_inner(from).is_some() => false,
        _ => {
            tc.dyn_infer.add_conversion(
                tc.current_module.clone(),
                expr_id,
                from.clone(),
                hole,
                span,
            );
            true
        }
    }
}

fn try_dyn_weakening(
    tc: &mut TypeChecker,
    span: Span,
    expr_id: Option<ExprId>,
    source: &ContractRef,
    target: &ContractRef,
) -> bool {
    if !contracts::contract_ref_subset(&tc.decls, &tc.current_module, source, target) {
        tc.push_error(TypeError::CompileError {
            message: format!(
                "dynamic value '{source}' cannot be used as '{target}'; implicit dynamic strengthening is not allowed"
            ),
            span: tc.error_span(span),
        });
        return true;
    }
    if let Some(expr_id) = expr_id
        && source != target
        && let (Some(source), Some(target)) = (
            contracts::contract_set_key_for_ref(&tc.decls, &tc.current_module, source),
            contracts::contract_set_key_for_ref(&tc.decls, &tc.current_module, target),
        )
    {
        tc.record_dyn_weakening(DynWeakeningFact {
            expr_id,
            source,
            target,
            span: tc.source_span(span),
        });
    }
    true
}

fn try_concrete_to_dyn(
    tc: &mut TypeChecker,
    span: Span,
    expr_id: Option<ExprId>,
    from: &Type,
    contract: &ContractRef,
) -> bool {
    if matches!(from, Type::Infer | Type::Var(_) | Type::Dyn(_))
        || tc.decls.semantic_option_inner(from).is_some()
    {
        return false;
    }
    match contracts::match_contract(tc, from, contract, span) {
        Ok(matched) => {
            let witness = contracts::plan_witness(tc, &matched, span);
            if let Some(expr_id) = expr_id {
                tc.record_dyn_conversion(DynConversionFact {
                    expr_id,
                    witness,
                    span: tc.source_span(span),
                });
            }
            true
        }
        Err(error) => {
            push_match_error(tc, from, contract, error, span);
            true
        }
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
    let from = checked.ty;
    let conversion = tc.explicit_cast_conversion(&from, &target);
    match conversion {
        Some(ExplicitCast::Identity) => tc
            .closure
            .copy_expr_flow(cast.node.expr.node.id, expr.node.id),
        Some(ExplicitCast::CastFrom { escape }) => {
            tc.check_argument_escape(&cast.node.expr, escape);
        }
        Some(ExplicitCast::Builtin) | None => {}
    }
    let ty = if conversion.is_some() || matches!(from, Type::Infer) || matches!(target, Type::Infer)
    {
        target
    } else {
        tc.push_error(TypeError::InvalidCast {
            from,
            to: target,
            span: tc.error_span(cast.span),
        });
        Type::Infer
    };
    let mut casted = checked_from_type(expr, ty, tc);
    casted.contains_extern_any = checked.contains_extern_any;
    casted
}
