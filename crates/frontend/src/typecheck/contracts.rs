use std::collections::HashMap;

use anvyx_externs::{ParamFlow, ReceiverMode};

use super::{
    ContractKey, ContractRequirementKey, ContractRequirementSchema, ContractSetKey, DeclError,
    DeclarationIndex, DeprecatedUseKind, Exposure, FuncParam, MemberAccessKind, MethodMode,
    MethodReceiver, ParamTypeSpans, TypeChecker, TypeError,
    annotation::deprecated_lint,
    member::{self, PromotedMethodTarget},
    semantic_use::{ContractWitnessKey, WitnessId, WitnessSlot, WitnessSlotTarget},
    type_refs::{TypeRefResolver, type_ref_error},
};
use crate::{
    ast::{AnonymousContract, ContractRef, Ident, ReturnSpec, Type, TypeVisitor},
    lint::LintEvent,
    span::{SourceSpan, Span},
};

#[derive(Clone)]
pub(super) struct ContractMatch {
    pub(super) concrete_ty: Type,
    pub(super) contract: ContractSetKey,
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
    ConflictingRequirement(Ident),
    Unsatisfied(Box<ContractRequirementError>),
}

#[derive(Clone)]
pub(super) enum ContractSetError {
    UnknownContract,
    ConflictingRequirement(Ident),
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
        expected: Box<FuncParam>,
        found: Box<FuncParam>,
    },
    Return {
        expected: Box<Type>,
        found: Box<Type>,
    },
}

#[derive(Clone)]
struct ContractSet {
    key: ContractSetKey,
    requirements: Vec<ContractRequirementSchema>,
}

struct CandidateSig {
    receiver: MethodReceiver,
    params: Vec<FuncParam>,
    ret: ReturnSpec,
    generic_method: bool,
}

fn contract_set_for_ref(
    decls: &DeclarationIndex,
    module: &super::ModuleScope,
    contract: &ContractRef,
) -> Result<ContractSet, ContractSetError> {
    let requirements = requirements_for_ref(decls, module, contract)?;
    Ok(ContractSet {
        key: contract_set_key(&requirements),
        requirements,
    })
}

pub(super) fn requirements_for_ref(
    decls: &DeclarationIndex,
    module: &super::ModuleScope,
    contract: &ContractRef,
) -> Result<Vec<ContractRequirementSchema>, ContractSetError> {
    let mut requirements = vec![];
    collect_contract_ref_requirements(decls, module, contract, &mut requirements)?;
    requirements.sort_by(|left, right| left.name.as_str().cmp(right.name.as_str()));
    Ok(requirements)
}

fn collect_contract_ref_requirements(
    decls: &DeclarationIndex,
    module: &super::ModuleScope,
    contract: &ContractRef,
    requirements: &mut Vec<ContractRequirementSchema>,
) -> Result<(), ContractSetError> {
    match contract {
        ContractRef::Named {
            qualifier,
            name,
            origin,
        } => {
            let resolver = TypeRefResolver::module_only(decls);
            let key = resolver
                .resolve_contract_name_with_import(module, *qualifier, *name, origin.as_ref())
                .map(|(key, _)| key)
                .map_err(|_| ContractSetError::UnknownContract)?;
            let schema = decls
                .contract(&key)
                .ok_or(ContractSetError::UnknownContract)?;
            merge_canonical_requirements(decls, requirements, &schema.requirements)
                .map_err(|req| ContractSetError::ConflictingRequirement(req.name))
        }
        ContractRef::Anonymous(surface) => {
            let anonymous = anonymous_requirements(surface);
            merge_canonical_requirements(decls, requirements, &anonymous)
                .map_err(|req| ContractSetError::ConflictingRequirement(req.name))
        }
        ContractRef::Intersection(contracts) => {
            for contract in contracts {
                collect_contract_ref_requirements(decls, module, contract, requirements)?;
            }
            Ok(())
        }
        ContractRef::Infer | ContractRef::Hole(_) => Err(ContractSetError::UnknownContract),
    }
}

fn anonymous_requirements(surface: &AnonymousContract) -> Vec<ContractRequirementSchema> {
    surface
        .requirements
        .iter()
        .map(|req| ContractRequirementSchema {
            name: req.name,
            receiver: Some(req.receiver),
            params: req
                .params
                .iter()
                .map(|param| FuncParam::new(param.ty.clone(), param.mutable, false, param.escape))
                .collect(),
            param_spans: ParamTypeSpans::default(),
            required_params: req.params.len(),
            ret: req.ret.clone(),
            generics_empty: true,
            span: None,
        })
        .collect()
}

pub(super) fn contract_set_key(requirements: &[ContractRequirementSchema]) -> ContractSetKey {
    let mut requirements = requirements
        .iter()
        .map(ContractRequirementKey::from_schema)
        .collect::<Vec<_>>();
    requirements.sort_by(|left, right| left.name.as_str().cmp(right.name.as_str()));
    ContractSetKey { requirements }
}

pub(super) fn merge_effective_requirements(
    target: &mut Vec<ContractRequirementSchema>,
    source: &[ContractRequirementSchema],
) -> Result<(), Box<ContractRequirementSchema>> {
    merge_requirements(target, source, same_requirement_signature)
}

fn merge_canonical_requirements(
    decls: &DeclarationIndex,
    target: &mut Vec<ContractRequirementSchema>,
    source: &[ContractRequirementSchema],
) -> Result<(), Box<ContractRequirementSchema>> {
    merge_requirements(target, source, |left, right| {
        let left = contract_set_key(std::slice::from_ref(left));
        let right = contract_set_key(std::slice::from_ref(right));
        contract_set_projection(decls, &left, &right).is_some()
    })
}

fn merge_requirements(
    target: &mut Vec<ContractRequirementSchema>,
    source: &[ContractRequirementSchema],
    same: impl Fn(&ContractRequirementSchema, &ContractRequirementSchema) -> bool,
) -> Result<(), Box<ContractRequirementSchema>> {
    for requirement in source {
        match target
            .iter()
            .find(|candidate| candidate.name == requirement.name)
        {
            Some(candidate) if !same(candidate, requirement) => {
                return Err(Box::new(requirement.clone()));
            }
            Some(_) => {}
            None => target.push(requirement.clone()),
        }
    }
    Ok(())
}

pub(crate) fn contract_set_key_for_ref(
    decls: &DeclarationIndex,
    module: &super::ModuleScope,
    contract: &ContractRef,
) -> Option<ContractSetKey> {
    contract_set_for_ref(decls, module, contract)
        .ok()
        .map(|set| set.key)
}

pub(crate) fn contract_set_projection(
    decls: &DeclarationIndex,
    source: &ContractSetKey,
    target: &ContractSetKey,
) -> Option<Vec<usize>> {
    super::contract_surface::canonical_projection(decls, source, target)
}

pub(crate) fn contract_ref_subset(
    decls: &DeclarationIndex,
    module: &super::ModuleScope,
    source: &ContractRef,
    target: &ContractRef,
) -> bool {
    let (Ok(source), Ok(target)) = (
        contract_set_for_ref(decls, module, source),
        contract_set_for_ref(decls, module, target),
    ) else {
        return false;
    };
    contract_set_projection(decls, &source.key, &target.key).is_some()
}

pub(super) fn match_contract(
    tc: &mut TypeChecker,
    concrete_ty: &Type,
    contract: &ContractRef,
    span: Span,
) -> Result<ContractMatch, ContractMatchError> {
    let set =
        contract_set_for_ref(&tc.decls, &tc.current_module, contract).map_err(|err| match err {
            ContractSetError::UnknownContract => ContractMatchError::UnknownContract,
            ContractSetError::ConflictingRequirement(name) => {
                ContractMatchError::ConflictingRequirement(name)
            }
        })?;

    let mut slots = Vec::with_capacity(set.requirements.len());
    for requirement in &set.requirements {
        let target = match_requirement(tc, concrete_ty, requirement, span)?;
        slots.push(ContractSlot {
            requirement: requirement.clone(),
            target,
        });
    }

    Ok(ContractMatch {
        concrete_ty: concrete_ty.clone(),
        contract: set.key,
        slots,
    })
}

pub(super) enum DynamicMethodError {
    UnknownContract,
    ConflictingRequirement(Ident),
    Missing { contract: Ident },
}

pub(super) fn resolve_dynamic_method(
    tc: &TypeChecker,
    contract: &ContractRef,
    name: Ident,
) -> Result<(ContractSetKey, ContractRequirementSchema), DynamicMethodError> {
    let set =
        contract_set_for_ref(&tc.decls, &tc.current_module, contract).map_err(|err| match err {
            ContractSetError::UnknownContract => DynamicMethodError::UnknownContract,
            ContractSetError::ConflictingRequirement(name) => {
                DynamicMethodError::ConflictingRequirement(name)
            }
        })?;
    let Some(requirement) = set.requirements.iter().find(|req| req.name == name) else {
        return Err(DynamicMethodError::Missing {
            contract: Ident::new(contract.to_string()),
        });
    };
    Ok((set.key, requirement.clone()))
}

pub(super) fn apply_match_access(tc: &mut TypeChecker, matched: &ContractMatch, span: Span) {
    for slot in &matched.slots {
        check_slot_access(tc, &matched.concrete_ty, &slot.target, span);
    }
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
                        param.escape,
                    )
                })
                .collect(),
            ret: ReturnSpec::value(method.signature.ret.ty.clone()),
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
        let compatible = expected.ty == found.ty
            && expected.mutable == found.mutable
            && expected.escape == found.escape;
        if !compatible {
            return Some(RequirementError::Param {
                index,
                expected: Box::new(expected.clone()),
                found: Box::new(found.clone()),
            });
        }
    }
    if requirement.ret != candidate.ret {
        return Some(RequirementError::Return {
            expected: Box::new(requirement.ret.ty().clone()),
            found: Box::new(candidate.ret.ty().clone()),
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
        MethodReceiver::Ref => matches!(found, MethodReceiver::Value | MethodReceiver::Ref),
    }
}

fn extern_receiver(receiver: ReceiverMode) -> MethodReceiver {
    match receiver {
        ReceiverMode::Mutable => MethodReceiver::Ref,
        ReceiverMode::Value | ReceiverMode::Shared => MethodReceiver::Value,
    }
}

pub(crate) fn finalize_contracts(
    decls: &mut DeclarationIndex,
    errors: &mut Vec<TypeError>,
    lint_events: &mut Vec<LintEvent>,
) {
    for contract in decls.contracts_mut() {
        let mut by_name: HashMap<Ident, ContractRequirementSchema> = HashMap::new();
        for req in std::mem::take(&mut contract.direct_requirements) {
            if req.receiver.is_none() {
                errors.push(TypeError::CompileError {
                    message:
                        "contract method requirements must include a `self` or `ref self` receiver"
                            .to_string(),
                    span: req.span,
                });
                continue;
            }
            if !req.generics_empty {
                errors.push(TypeError::CompileError {
                    message: "contract method requirements cannot be generic".to_string(),
                    span: req.span,
                });
                continue;
            }
            if contains_infer_return(&req.ret.ty()) {
                errors.push(TypeError::CompileError {
                    message: "contract method requirements cannot use inferred return types"
                        .to_string(),
                    span: req.span,
                });
                continue;
            }
            match by_name.get(&req.name) {
                Some(prev) if same_requirement_signature(prev, &req) => {
                    errors.push(TypeError::Decl(DeclError::DuplicateContractRequirement {
                        contract: contract.key.clone(),
                        name: req.name,
                        span: req.span,
                    }));
                }
                Some(_) => errors.push(conflicting_requirement_error(
                    &contract.key,
                    req.name,
                    req.span,
                )),
                None => {
                    by_name.insert(req.name, req);
                }
            }
        }
        contract.direct_requirements = by_name.into_values().collect();
        contract
            .direct_requirements
            .sort_by(|left, right| left.name.as_str().cmp(right.name.as_str()));
    }

    let keys = decls
        .contracts()
        .map(|contract| contract.key.clone())
        .collect::<Vec<_>>();
    for key in keys {
        let mut stack = vec![];
        finalize_effective_contract(decls, &key, &mut stack, errors, lint_events);
    }
}

fn finalize_effective_contract(
    decls: &mut DeclarationIndex,
    key: &ContractKey,
    stack: &mut Vec<ContractKey>,
    errors: &mut Vec<TypeError>,
    lint_events: &mut Vec<LintEvent>,
) -> Vec<ContractRequirementSchema> {
    if let Some(schema) = decls.contract(key)
        && schema.contract_set.is_some()
    {
        return schema.requirements.clone();
    }
    if stack.contains(key) {
        let span = decls.contract(key).map(|contract| contract.span);
        errors.push(TypeError::CompileError {
            message: format!("contract inclusion cycle involving '{}'", key.name),
            span,
        });
        return vec![];
    }

    let Some(schema) = decls.contract(key).cloned() else {
        return vec![];
    };
    stack.push(key.clone());

    let mut effective = schema.direct_requirements.clone();
    for (include, include_span) in schema.includes {
        let included_keys = resolve_included_contracts(
            decls,
            &schema.key,
            &include,
            include_span,
            errors,
            lint_events,
        );
        for included_key in included_keys {
            let included =
                finalize_effective_contract(decls, &included_key, stack, errors, lint_events);
            if let Err(conflict) = merge_effective_requirements(&mut effective, &included) {
                errors.push(conflicting_requirement_error(
                    &schema.key,
                    conflict.name,
                    Some(include_span),
                ));
            }
        }
    }

    stack.pop();
    effective.sort_by(|left, right| left.name.as_str().cmp(right.name.as_str()));
    let set = contract_set_key(&effective);
    if effective.is_empty() {
        let noun = match schema.visibility {
            crate::ast::Visibility::Public => "public contract",
            crate::ast::Visibility::Private => "contract",
        };
        errors.push(TypeError::CompileError {
            message: format!("{noun} '{}' cannot be empty", schema.key.name),
            span: Some(schema.span),
        });
    }
    let result = effective.clone();
    if let Some(contract) = decls.contracts_mut().find(|contract| contract.key == *key) {
        contract.requirements = effective;
        contract.contract_set = Some(set);
    }
    result
}

fn resolve_included_contracts(
    decls: &mut DeclarationIndex,
    owner: &ContractKey,
    include: &ContractRef,
    span: SourceSpan,
    errors: &mut Vec<TypeError>,
    lint_events: &mut Vec<LintEvent>,
) -> Vec<ContractKey> {
    match include {
        ContractRef::Named {
            qualifier,
            name,
            origin,
        } => {
            let resolved = {
                let resolver = TypeRefResolver::module_only(decls);
                resolver.resolve_contract_name_with_import(
                    &owner.module,
                    *qualifier,
                    *name,
                    origin.as_ref(),
                )
            };
            match resolved {
                Ok((key, import)) => {
                    decls.mark_import_used(import);
                    if let Some(schema) = decls.contract(&key)
                        && schema.policy.has_deprecated()
                    {
                        let event = deprecated_lint(
                            DeprecatedUseKind::Contract,
                            key.name,
                            schema.policy.deprecated_reason(),
                            span,
                        );
                        if !lint_events.contains(&event) {
                            lint_events.push(event);
                        }
                    }
                    vec![key]
                }
                Err(error) => {
                    decls.mark_import_used(error.import().cloned());
                    errors.push(type_ref_error(error, Some(span)));
                    vec![]
                }
            }
        }
        ContractRef::Intersection(contracts) => contracts
            .iter()
            .flat_map(|contract| {
                resolve_included_contracts(decls, owner, contract, span, errors, lint_events)
            })
            .collect(),
        ContractRef::Anonymous(_) => {
            errors.push(TypeError::CompileError {
                message: "anonymous contracts cannot be included in a contract".to_string(),
                span: Some(span),
            });
            vec![]
        }
        ContractRef::Infer | ContractRef::Hole(_) => {
            errors.push(TypeError::CompileError {
                message: "inferred contracts cannot be included in a contract".to_string(),
                span: Some(span),
            });
            vec![]
        }
    }
}

fn conflicting_requirement_error(
    contract: &ContractKey,
    name: Ident,
    span: Option<SourceSpan>,
) -> TypeError {
    TypeError::CompileError {
        message: format!(
            "conflicting contract requirement '{}' in contract '{}'",
            name, contract.name
        ),
        span,
    }
}

fn same_requirement_signature(
    left: &ContractRequirementSchema,
    right: &ContractRequirementSchema,
) -> bool {
    left.receiver == right.receiver
        && left.params == right.params
        && left.required_params == right.required_params
        && left.ret == right.ret
}

fn contains_infer_return(ty: &Type) -> bool {
    struct Visitor;

    impl TypeVisitor for Visitor {
        fn visit_type_leaf(&mut self, ty: &Type) -> bool {
            matches!(ty, Type::InferReturn)
        }
    }

    let mut visitor = Visitor;
    visitor.visit_type(ty)
}
