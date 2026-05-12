use std::collections::HashMap;

use anvyx_externs::{ParamFlow, ReceiverMode};

use super::{
    ContractKey, ContractRequirementSchema, DeclarationIndex, Exposure, FuncParam,
    MemberAccessKind, MethodMode, MethodReceiver, TypeChecker, TypeError,
    member::{self, PromotedMethodTarget},
    semantic_use::{ContractWitnessKey, WitnessId, WitnessSlot, WitnessSlotTarget},
    type_refs::TypeRefResolver,
};
use crate::{
    ast::{ContractRef, Ident, Type},
    span::Span,
};

#[derive(Clone)]
pub(super) struct ContractMatch {
    pub(super) concrete_ty: Type,
    pub(super) contract: ContractKey,
    pub(super) slots: Vec<ContractSlot>,
}

#[derive(Clone)]
pub(super) struct ContractSlot {
    pub(super) requirement: ContractRequirementSchema,
    pub(super) target: ContractSlotTarget,
}

#[derive(Clone)]
pub(super) enum ContractSlotTarget {
    Direct(Box<member::MethodAccess>),
    Extend(Box<member::ExtendMethodAccess>),
    Extern(Box<member::ExternMethodAccess>),
    Promoted(Box<member::PromotedMethodAccess>),
}

#[derive(Clone)]
pub(super) enum ContractMatchError {
    UnknownContract,
    Unsatisfied(Box<ContractRequirementError>),
}

#[derive(Clone)]
pub(super) struct ContractRequirementError {
    pub(super) requirement: ContractRequirementSchema,
    pub(super) reason: RequirementError,
}

#[derive(Debug, Clone, PartialEq)]
pub(super) enum RequirementError {
    Missing,
    Ambiguous,
    UnboundGeneric(Vec<Ident>),
    GenericMethod,
    Receiver {
        found: MethodReceiver,
    },
    Arity {
        expected: usize,
        found: usize,
    },
    Param {
        index: usize,
        expected: FuncParam,
        found: FuncParam,
    },
    Return {
        expected: Type,
        found: Type,
    },
}

struct CandidateSig {
    receiver: MethodReceiver,
    params: Vec<FuncParam>,
    ret: Type,
    generic_method: bool,
}

pub(super) fn match_contract(
    tc: &mut TypeChecker,
    concrete_ty: &Type,
    contract: &ContractRef,
    span: Span,
) -> Result<ContractMatch, ContractMatchError> {
    let resolver = TypeRefResolver::with_local_types(&tc.decls, &tc.local_type_scopes);
    let contract_key = resolver
        .resolve_contract_ref(&tc.current_module, contract)
        .map_err(|_| ContractMatchError::UnknownContract)?;
    let schema = tc
        .decls
        .contract(&contract_key)
        .cloned()
        .expect("resolved contract key missing schema");

    let mut slots = Vec::with_capacity(schema.requirements.len());
    for requirement in &schema.requirements {
        let target = match_requirement(tc, concrete_ty, requirement, span)?;
        slots.push(ContractSlot {
            requirement: requirement.clone(),
            target,
        });
    }

    Ok(ContractMatch {
        concrete_ty: concrete_ty.clone(),
        contract: contract_key,
        slots,
    })
}

pub(super) enum DynamicMethodError {
    UnknownContract,
    Missing { contract: ContractKey },
}

pub(super) fn resolve_dynamic_method(
    tc: &TypeChecker,
    contract: &ContractRef,
    name: Ident,
) -> Result<(ContractKey, ContractRequirementSchema), DynamicMethodError> {
    let resolver = TypeRefResolver::with_local_types(&tc.decls, &tc.local_type_scopes);
    let contract_key = resolver
        .resolve_contract_ref(&tc.current_module, contract)
        .map_err(|_| DynamicMethodError::UnknownContract)?;
    let schema = tc
        .decls
        .contract(&contract_key)
        .expect("resolved contract key missing schema");
    let Some(requirement) = schema.requirements.iter().find(|req| req.name == name) else {
        return Err(DynamicMethodError::Missing {
            contract: contract_key,
        });
    };
    Ok((contract_key, requirement.clone()))
}

pub(super) fn plan_witness(tc: &mut TypeChecker, matched: &ContractMatch, span: Span) -> WitnessId {
    let key = ContractWitnessKey {
        concrete_ty: matched.concrete_ty.clone(),
        contract: matched.contract.clone(),
        slots: matched.slots.iter().map(witness_slot).collect(),
    };
    tc.record_contract_witness(key, span)
}

fn match_requirement(
    tc: &mut TypeChecker,
    concrete_ty: &Type,
    requirement: &ContractRequirementSchema,
    span: Span,
) -> Result<ContractSlotTarget, ContractMatchError> {
    let target =
        resolve_requirement_target(tc, concrete_ty, requirement.name).map_err(|reason| {
            ContractMatchError::Unsatisfied(Box::new(ContractRequirementError {
                requirement: requirement.clone(),
                reason,
            }))
        })?;

    let candidate = target.signature();
    if let Some(reason) = signature_error(requirement, &candidate) {
        return Err(ContractMatchError::Unsatisfied(Box::new(
            ContractRequirementError {
                requirement: requirement.clone(),
                reason,
            },
        )));
    }
    check_slot_access(tc, concrete_ty, &target, span);
    Ok(target)
}

fn resolve_requirement_target(
    tc: &mut TypeChecker,
    concrete_ty: &Type,
    name: Ident,
) -> Result<ContractSlotTarget, RequirementError> {
    match member::resolve_method(concrete_ty, name, tc) {
        member::MethodResolution::Direct(method) => Ok(ContractSlotTarget::Direct(method)),
        member::MethodResolution::Extend(method) => Ok(ContractSlotTarget::Extend(method)),
        member::MethodResolution::Extern(method) => Ok(ContractSlotTarget::Extern(method)),
        member::MethodResolution::Promoted(method) => Ok(ContractSlotTarget::Promoted(method)),
        member::MethodResolution::AmbiguousPromoted { .. } => Err(RequirementError::Ambiguous),
        member::MethodResolution::ExtendError(member::ExtendMethodError::Ambiguous { .. }) => {
            Err(RequirementError::Ambiguous)
        }
        member::MethodResolution::ExtendError(member::ExtendMethodError::Unbound(names)) => {
            Err(RequirementError::UnboundGeneric(names))
        }
        member::MethodResolution::StaticOnValue { .. }
        | member::MethodResolution::Missing { .. }
        | member::MethodResolution::NonAggregate { .. } => Err(RequirementError::Missing),
    }
}

impl ContractSlotTarget {
    fn signature(&self) -> CandidateSig {
        match self {
            Self::Direct(method) => CandidateSig::from_callable(method.mode, &method.callee),
            Self::Extend(method) => CandidateSig::from_callable(method.mode, &method.callee),
            Self::Extern(method) => CandidateSig::from_extern(method),
            Self::Promoted(method) => match &method.target {
                PromotedMethodTarget::Aggregate(target) => {
                    CandidateSig::from_callable(target.mode, &target.callee)
                }
                PromotedMethodTarget::Extern(target) => CandidateSig::from_extern(target),
            },
        }
    }
}

impl CandidateSig {
    fn from_callable(mode: MethodMode, callee: &super::CallableRef) -> Self {
        Self {
            receiver: mode
                .receiver()
                .expect("contract matcher only receives instance methods"),
            params: callee.def.sig.params.clone(),
            ret: callee.def.sig.ret.clone(),
            generic_method: !callee.def.sig.generics.is_empty(),
        }
    }

    fn from_extern(method: &member::ExternMethodAccess) -> Self {
        Self {
            receiver: extern_receiver(method.receiver),
            params: method
                .signature
                .params
                .iter()
                .map(|param| {
                    FuncParam::new(
                        param.ty.ty.clone(),
                        matches!(param.flow, ParamFlow::MutBorrow),
                        false,
                    )
                })
                .collect(),
            ret: method.signature.ret.ty.clone(),
            generic_method: false,
        }
    }
}

fn signature_error(
    requirement: &ContractRequirementSchema,
    candidate: &CandidateSig,
) -> Option<RequirementError> {
    if candidate.generic_method {
        return Some(RequirementError::GenericMethod);
    }
    let required_receiver = requirement
        .receiver
        .expect("contract requirements are finalized with receivers");
    if !receiver_compatible(required_receiver, candidate.receiver) {
        return Some(RequirementError::Receiver {
            found: candidate.receiver,
        });
    }
    if requirement.params.len() != candidate.params.len() {
        return Some(RequirementError::Arity {
            expected: requirement.params.len(),
            found: candidate.params.len(),
        });
    }
    for (index, (expected, found)) in requirement.params.iter().zip(&candidate.params).enumerate() {
        if expected != found {
            return Some(RequirementError::Param {
                index,
                expected: expected.clone(),
                found: found.clone(),
            });
        }
    }
    if requirement.ret != candidate.ret {
        return Some(RequirementError::Return {
            expected: requirement.ret.clone(),
            found: candidate.ret.clone(),
        });
    }
    None
}

fn check_slot_access(
    tc: &mut TypeChecker,
    concrete_ty: &Type,
    target: &ContractSlotTarget,
    span: Span,
) {
    match target {
        ContractSlotTarget::Direct(method) => tc.check_access_policy(
            &method.policy,
            MemberAccessKind::Method,
            method.callee.def.id.name,
            concrete_ty,
            &method.origin,
            span,
        ),
        ContractSlotTarget::Extend(method) => tc.check_access_policy(
            &method.method.policy,
            MemberAccessKind::Method,
            method.callee.def.id.name,
            concrete_ty,
            &method.extend.origin,
            span,
        ),
        ContractSlotTarget::Promoted(method) => {
            if method.exposure == Exposure::Implicit {
                tc.check_stored_field_path_access(concrete_ty, &method.path, span);
            }
            if let PromotedMethodTarget::Aggregate(target) = &method.target {
                tc.check_access_policy(
                    &target.policy,
                    MemberAccessKind::Method,
                    method.origin_method,
                    &method.origin_owner,
                    &target.origin,
                    span,
                );
            }
        }
        ContractSlotTarget::Extern(_) => {}
    }
}

fn witness_slot(slot: &ContractSlot) -> WitnessSlot {
    WitnessSlot {
        name: slot.requirement.name,
        required_receiver: slot
            .requirement
            .receiver
            .expect("contract requirements are finalized with receivers"),
        target: witness_target(&slot.target),
    }
}

fn witness_target(target: &ContractSlotTarget) -> WitnessSlotTarget {
    match target {
        ContractSlotTarget::Direct(method) => direct_witness_target(method),
        ContractSlotTarget::Extend(method) => WitnessSlotTarget::Extend {
            extend: method.extend.id.clone(),
            callable: method.callee.def.id.clone(),
            owner_args: method.callee.owner_args.clone(),
            receiver_mode: method.mode,
        },
        ContractSlotTarget::Extern(method) => extern_witness_target(method),
        ContractSlotTarget::Promoted(method) => WitnessSlotTarget::Promoted {
            path: method.path.clone(),
            origin_owner: method.origin_owner.clone(),
            origin_method: method.origin_method,
            target: Box::new(promoted_target(&method.target)),
        },
    }
}

fn promoted_target(target: &PromotedMethodTarget) -> WitnessSlotTarget {
    match target {
        PromotedMethodTarget::Aggregate(method) => direct_witness_target(method),
        PromotedMethodTarget::Extern(method) => extern_witness_target(method),
    }
}

fn direct_witness_target(method: &member::MethodAccess) -> WitnessSlotTarget {
    WitnessSlotTarget::Direct {
        callable: method.callee.def.id.clone(),
        owner_args: method.callee.owner_args.clone(),
        receiver_mode: method.mode,
    }
}

fn extern_witness_target(method: &member::ExternMethodAccess) -> WitnessSlotTarget {
    WitnessSlotTarget::Extern {
        method: method.method_ref,
        receiver: method.receiver,
    }
}

fn receiver_compatible(required: MethodReceiver, found: MethodReceiver) -> bool {
    match required {
        MethodReceiver::Value => matches!(found, MethodReceiver::Value),
        MethodReceiver::Var => matches!(found, MethodReceiver::Value | MethodReceiver::Var),
    }
}

fn extern_receiver(receiver: ReceiverMode) -> MethodReceiver {
    match receiver {
        ReceiverMode::Mutable => MethodReceiver::Var,
        ReceiverMode::Value | ReceiverMode::Shared => MethodReceiver::Value,
    }
}

pub(crate) fn finalize_contracts(decls: &mut DeclarationIndex, errors: &mut Vec<TypeError>) {
    for contract in decls.contracts_mut() {
        let mut by_name: HashMap<Ident, ContractRequirementSchema> = HashMap::new();
        for req in std::mem::take(&mut contract.requirements) {
            if req.receiver.is_none() {
                errors.push(TypeError::CompileError {
                    message:
                        "contract method requirements must include a `self` or `var self` receiver"
                            .to_string(),
                    span: Some(req.span),
                });
                continue;
            }
            if !req.generics_empty {
                errors.push(TypeError::CompileError {
                    message: "contract method requirements cannot be generic".to_string(),
                    span: Some(req.span),
                });
                continue;
            }
            if contains_infer_return(&req.ret) {
                errors.push(TypeError::CompileError {
                    message: "contract method requirements cannot use inferred return types"
                        .to_string(),
                    span: Some(req.span),
                });
                continue;
            }
            match by_name.get(&req.name) {
                Some(prev) if same_requirement(prev, &req) => {}
                Some(_) => errors.push(TypeError::CompileError {
                    message: format!(
                        "conflicting contract requirement '{}' in contract '{}'",
                        req.name, contract.key.name
                    ),
                    span: Some(req.span),
                }),
                None => {
                    by_name.insert(req.name, req);
                }
            }
        }
        contract.requirements = by_name.into_values().collect();
        contract
            .requirements
            .sort_by(|left, right| left.name.as_str().cmp(right.name.as_str()));
        if contract.requirements.is_empty() {
            let noun = match contract.visibility {
                crate::ast::Visibility::Public => "public contract",
                crate::ast::Visibility::Private => "contract",
            };
            errors.push(TypeError::CompileError {
                message: format!("{noun} '{}' cannot be empty", contract.key.name),
                span: Some(contract.span),
            });
        }
    }
}

fn same_requirement(left: &ContractRequirementSchema, right: &ContractRequirementSchema) -> bool {
    left.receiver == right.receiver
        && left.params == right.params
        && left.required_params == right.required_params
        && left.ret == right.ret
}

fn contains_infer_return(ty: &Type) -> bool {
    match ty {
        Type::InferReturn => true,
        Type::Func { params, ret } => {
            params.iter().any(|param| contains_infer_return(&param.ty))
                || contains_infer_return(ret)
        }
        Type::Tuple(elems) => elems.iter().any(contains_infer_return),
        Type::Nominal(nominal) => nominal.type_args.iter().any(contains_infer_return),
        Type::List { elem } | Type::Slice { elem } | Type::Array { elem, .. } => {
            contains_infer_return(elem)
        }
        Type::Map { key, value } => contains_infer_return(key) || contains_infer_return(value),
        Type::Infer
        | Type::Any
        | Type::Int
        | Type::Float
        | Type::Bool
        | Type::String
        | Type::Void
        | Type::Dyn(_)
        | Type::Var(_)
        | Type::UnresolvedName(_)
        | Type::UnresolvedNominal { .. } => false,
    }
}
