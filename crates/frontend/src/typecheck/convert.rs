use super::{
    DynConversionFact, TypeChecker, TypeError, TypeHandle,
    contracts::{self, ContractMatchError, RequirementError},
};
use crate::{
    ast::{ContractRef, ExprId, Ident, Type},
    span::Span,
};

pub(super) fn expect_assignable(
    tc: &mut TypeChecker,
    span: Span,
    expr_id: Option<ExprId>,
    from: TypeHandle,
    to: TypeHandle,
) {
    let from_ty = tc.handle_type(&from);
    let to_ty = tc.handle_type(&to);
    if try_expected_dyn(tc, span, expr_id, &from_ty, &to_ty) {
        return;
    }
    tc.solver
        .add_handle_assignable(tc.error_span(span), from, to);
}

fn try_expected_dyn(
    tc: &mut TypeChecker,
    span: Span,
    expr_id: Option<ExprId>,
    from: &Type,
    to: &Type,
) -> bool {
    match to {
        Type::Dyn(contract) => try_concrete_to_dyn(tc, span, expr_id, from, contract),
        to => {
            let Some(inner) = tc.decls.core_option_inner(to).cloned() else {
                return false;
            };
            let Type::Dyn(contract) = &inner else {
                return false;
            };
            if from == &inner {
                return true;
            }
            if tc.decls.core_option_inner(from).is_some() {
                return false;
            }
            try_concrete_to_dyn(tc, span, expr_id, from, contract)
        }
    }
}

fn try_concrete_to_dyn(
    tc: &mut TypeChecker,
    span: Span,
    expr_id: Option<ExprId>,
    from: &Type,
    contract: &ContractRef,
) -> bool {
    if matches!(from, Type::Infer | Type::Var(_) | Type::Dyn(_))
        || tc.decls.core_option_inner(from).is_some()
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

fn push_match_error(
    tc: &mut TypeChecker,
    from: &Type,
    contract: &ContractRef,
    error: ContractMatchError,
    span: Span,
) {
    let detail = match error {
        ContractMatchError::UnknownContract => "unknown contract".to_string(),
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
