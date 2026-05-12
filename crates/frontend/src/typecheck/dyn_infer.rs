use std::collections::HashMap;

use super::{
    DynCallFact, DynConversionFact, DynWeakeningFact, FuncParam, MethodReceiver, TypeChecker,
    TypeError, contracts,
    convert::push_match_error,
    decls::{ContractRequirementSchema, ModuleScope},
    type_ops::{TypeFolder, TypeVisitor},
};
use crate::{
    ast::{
        AnonymousContract, AnonymousContractParam, AnonymousContractRequirement, ContractRef,
        DynContractHoleId, ExprId, Ident, Type,
    },
    span::{SourceSpan, Span},
};

#[derive(Default)]
pub(super) struct DynInference {
    next_id: u32,
    holes: HashMap<DynContractHoleId, DynHole>,
    conversions: Vec<PendingConversion>,
    dyn_sources: Vec<PendingDynSource>,
    hole_targets: Vec<PendingHoleTarget>,
    calls: Vec<PendingCall>,
    solutions: HashMap<DynContractHoleId, ContractRef>,
}

struct DynHole {
    module: ModuleScope,
    span: SourceSpan,
    exported: bool,
    requirements: Vec<ContractRequirementSchema>,
    expected: Vec<ContractRef>,
}

struct PendingConversion {
    module: ModuleScope,
    expr_id: Option<ExprId>,
    concrete_ty: Type,
    hole: DynContractHoleId,
    span: Span,
}

struct PendingDynSource {
    module: ModuleScope,
    expr_id: Option<ExprId>,
    source: ContractRef,
    hole: DynContractHoleId,
    span: Span,
}

struct PendingHoleTarget {
    module: ModuleScope,
    expr_id: Option<ExprId>,
    hole: DynContractHoleId,
    target: ContractRef,
    span: Span,
}

struct PendingCall {
    module: ModuleScope,
    call_id: ExprId,
    receiver_id: ExprId,
    hole: DynContractHoleId,
    method: Ident,
    arg_count: usize,
    requires_mutable: bool,
    span: SourceSpan,
}

impl DynInference {
    pub(super) fn has_raw_hole(ty: &Type) -> bool {
        let mut visitor = RawHoleVisitor;
        visitor.visit_type(ty)
    }

    pub(super) fn assign_holes(
        &mut self,
        module: &ModuleScope,
        ty: &Type,
        span: SourceSpan,
        exported: bool,
    ) -> Type {
        HoleAssigner {
            infer: self,
            module,
            span,
            exported,
        }
        .fold_type(ty)
    }

    pub(super) fn add_conversion(
        &mut self,
        module: ModuleScope,
        expr_id: Option<ExprId>,
        concrete_ty: Type,
        hole: DynContractHoleId,
        span: Span,
    ) {
        self.conversions.push(PendingConversion {
            module,
            expr_id,
            concrete_ty,
            hole,
            span,
        });
    }

    pub(super) fn add_dyn_source(
        &mut self,
        module: ModuleScope,
        expr_id: Option<ExprId>,
        source: ContractRef,
        hole: DynContractHoleId,
        span: Span,
    ) {
        self.dyn_sources.push(PendingDynSource {
            module,
            expr_id,
            source,
            hole,
            span,
        });
    }

    pub(super) fn add_hole_target(
        &mut self,
        module: ModuleScope,
        expr_id: Option<ExprId>,
        hole: DynContractHoleId,
        target: ContractRef,
        span: Span,
    ) {
        if let Some(hole) = self.holes.get_mut(&hole)
            && !hole.expected.contains(&target)
        {
            hole.expected.push(target.clone());
        }
        self.hole_targets.push(PendingHoleTarget {
            module,
            expr_id,
            hole,
            target,
            span,
        });
    }

    pub(super) fn collect_method(
        &mut self,
        hole: DynContractHoleId,
        method: Ident,
        receiver: MethodReceiver,
        params: Vec<FuncParam>,
        ret: Type,
        span: SourceSpan,
    ) -> Result<(), String> {
        let requirement = ContractRequirementSchema {
            name: method,
            receiver: Some(receiver),
            required_params: params.len(),
            params,
            ret,
            generics_empty: true,
            span: Some(span),
        };
        let Some(hole) = self.holes.get_mut(&hole) else {
            return Err("unknown inferred dynamic contract hole".to_string());
        };
        if let Err(conflict) = contracts::merge_effective_requirements(
            &mut hole.requirements,
            std::slice::from_ref(&requirement),
        ) {
            return Err(format!(
                "conflicting inferred dynamic requirement '{}'",
                conflict.name
            ));
        }
        Ok(())
    }

    pub(super) fn add_call(
        &mut self,
        module: ModuleScope,
        call_id: ExprId,
        receiver_id: ExprId,
        hole: DynContractHoleId,
        method: Ident,
        arg_count: usize,
        requires_mutable: bool,
        span: SourceSpan,
    ) {
        self.calls.push(PendingCall {
            module,
            call_id,
            receiver_id,
            hole,
            method,
            arg_count,
            requires_mutable,
            span,
        });
    }

    pub(super) fn solve(&mut self, tc: &mut TypeChecker) {
        let mut ids = self.holes.keys().copied().collect::<Vec<_>>();
        ids.sort_by_key(|id| id.0);
        for id in ids {
            self.solve_hole(id, tc);
        }
        self.finish_conversions(tc);
        self.finish_dyn_sources(tc);
        self.finish_hole_targets(tc);
        self.finish_calls(tc);
        self.rewrite_solver_types(tc);
    }

    fn solve_hole(&mut self, id: DynContractHoleId, tc: &mut TypeChecker) {
        let Some(hole) = self.holes.get(&id) else {
            return;
        };
        if self.solutions.contains_key(&id) {
            return;
        }

        let mut requirements = vec![];
        for contract in &hole.expected {
            match contracts::requirements_for_ref(&tc.decls, &hole.module, contract) {
                Ok(expected) => {
                    if let Err(conflict) =
                        contracts::merge_effective_requirements(&mut requirements, &expected)
                    {
                        tc.push_error(TypeError::CompileError {
                            message: format!(
                                "conflicting inferred dynamic requirement '{}'",
                                conflict.name
                            ),
                            span: Some(hole.span),
                        });
                    }
                }
                Err(contracts::ContractSetError::UnknownContract) => {
                    tc.push_error(TypeError::CompileError {
                        message: "unknown expected dynamic contract for inferred hole".to_string(),
                        span: Some(hole.span),
                    });
                }
                Err(contracts::ContractSetError::ConflictingRequirement(name)) => {
                    tc.push_error(TypeError::CompileError {
                        message: format!("conflicting inferred dynamic requirement '{name}'"),
                        span: Some(hole.span),
                    });
                }
            }
        }
        if let Err(conflict) =
            contracts::merge_effective_requirements(&mut requirements, &hole.requirements)
        {
            tc.push_error(TypeError::CompileError {
                message: format!(
                    "conflicting inferred dynamic requirement '{}'",
                    conflict.name
                ),
                span: Some(hole.span),
            });
        }

        if requirements.is_empty() {
            tc.push_error(TypeError::CompileError {
                message: "cannot infer empty dynamic contract".to_string(),
                span: Some(hole.span),
            });
            return;
        }

        if hole.exported {
            tc.push_warning(super::TypeWarning::CompileMessage {
                message: "exported function uses inferred dynamic contract; write `dyn Updatable` or an explicit `dyn { ... }`".to_string(),
                span: hole.span,
            });
        }

        self.solutions
            .insert(id, anonymous_contract_ref(&requirements));
    }

    fn finish_conversions(&mut self, tc: &mut TypeChecker) {
        let conversions = std::mem::take(&mut self.conversions);
        for pending in conversions {
            let Some(contract) = self.solution(pending.hole) else {
                continue;
            };
            match tc.with_current_module(&pending.module, |tc| {
                contracts::match_contract(tc, &pending.concrete_ty, &contract, pending.span)
            }) {
                Ok(matched) => {
                    let witness = contracts::plan_witness(tc, &matched, pending.span);
                    if let Some(expr_id) = pending.expr_id {
                        tc.record_dyn_conversion(DynConversionFact {
                            expr_id,
                            witness,
                            span: tc.source_span(pending.span),
                        });
                    }
                }
                Err(error) => {
                    push_match_error(tc, &pending.concrete_ty, &contract, error, pending.span);
                }
            }
        }
    }

    fn finish_dyn_sources(&mut self, tc: &mut TypeChecker) {
        let sources = std::mem::take(&mut self.dyn_sources);
        for pending in sources {
            let Some(target) = self.solution(pending.hole) else {
                continue;
            };
            self.finish_dyn_flow(
                tc,
                pending.expr_id,
                &pending.source,
                &target,
                &pending.module,
                pending.span,
                true,
            );
        }
    }

    fn finish_hole_targets(&mut self, tc: &mut TypeChecker) {
        let targets = std::mem::take(&mut self.hole_targets);
        for pending in targets {
            let Some(source) = self.solution(pending.hole) else {
                continue;
            };
            self.finish_dyn_flow(
                tc,
                pending.expr_id,
                &source,
                &pending.target,
                &pending.module,
                pending.span,
                false,
            );
        }
    }

    fn finish_calls(&mut self, tc: &mut TypeChecker) {
        let calls = std::mem::take(&mut self.calls);
        for pending in calls {
            let Some(contract) = self.solution(pending.hole) else {
                continue;
            };
            let Some(contract) =
                contracts::contract_set_key_for_ref(&tc.decls, &pending.module, &contract)
            else {
                continue;
            };
            tc.record_dyn_call(DynCallFact {
                call_id: pending.call_id,
                receiver_id: pending.receiver_id,
                contract,
                method: pending.method,
                arg_count: pending.arg_count,
                requires_mutable: pending.requires_mutable,
                span: pending.span,
            });
        }
    }

    fn rewrite_solver_types(&self, tc: &mut TypeChecker) {
        let solutions = self.solutions.clone();
        tc.solver
            .rewrite_contract_refs(&mut |contract| match contract {
                ContractRef::Hole(id) => solutions
                    .get(id)
                    .cloned()
                    .unwrap_or_else(|| contract.clone()),
                other => other.clone(),
            });
    }

    fn solution(&self, id: DynContractHoleId) -> Option<ContractRef> {
        self.solutions.get(&id).cloned()
    }

    fn finish_dyn_flow(
        &self,
        tc: &mut TypeChecker,
        expr_id: Option<ExprId>,
        source: &ContractRef,
        target: &ContractRef,
        module: &ModuleScope,
        span: Span,
        inferred_target: bool,
    ) {
        if !contracts::contract_ref_subset(&tc.decls, module, source, target) {
            let target = if inferred_target {
                format!("inferred dynamic contract '{target}'")
            } else {
                format!("'{target}'")
            };
            tc.push_error(TypeError::CompileError {
                message: format!(
                    "dynamic value '{source}' cannot be used as {target}; implicit dynamic strengthening is not allowed"
                ),
                span: tc.error_span(span),
            });
            return;
        }
        self.record_weakening(tc, expr_id, source, target, module, span);
    }

    fn record_weakening(
        &self,
        tc: &mut TypeChecker,
        expr_id: Option<ExprId>,
        source: &ContractRef,
        target: &ContractRef,
        module: &ModuleScope,
        span: Span,
    ) {
        if source == target {
            return;
        }
        let Some(expr_id) = expr_id else {
            return;
        };
        let (Some(source), Some(target)) = (
            contracts::contract_set_key_for_ref(&tc.decls, module, source),
            contracts::contract_set_key_for_ref(&tc.decls, module, target),
        ) else {
            return;
        };
        tc.record_dyn_weakening(DynWeakeningFact {
            expr_id,
            source,
            target,
            span: tc.source_span(span),
        });
    }
}

pub(super) fn hole_id(contract: &ContractRef) -> Option<DynContractHoleId> {
    match contract {
        ContractRef::Hole(id) => Some(*id),
        _ => None,
    }
}

fn anonymous_contract_ref(requirements: &[ContractRequirementSchema]) -> ContractRef {
    ContractRef::Anonymous(AnonymousContract {
        requirements: requirements
            .iter()
            .map(|req| AnonymousContractRequirement {
                receiver: req
                    .receiver
                    .expect("inferred contract requirements always have receivers"),
                name: req.name,
                params: req
                    .params
                    .iter()
                    .enumerate()
                    .map(|(index, param)| AnonymousContractParam {
                        mutable: param.mutable,
                        name: Ident::new(format!("arg{index}")),
                        ty: param.ty.clone(),
                    })
                    .collect(),
                ret: req.ret.clone(),
            })
            .collect(),
    })
}

struct RawHoleVisitor;

impl TypeVisitor for RawHoleVisitor {
    fn visit_contract_ref_leaf(&mut self, contract: &ContractRef) -> bool {
        matches!(contract, ContractRef::Infer)
    }
}

struct HoleAssigner<'a> {
    infer: &'a mut DynInference,
    module: &'a ModuleScope,
    span: SourceSpan,
    exported: bool,
}

impl TypeFolder for HoleAssigner<'_> {
    fn fold_contract_ref_leaf(&mut self, contract: ContractRef) -> ContractRef {
        match contract {
            ContractRef::Infer => {
                let id = DynContractHoleId(self.infer.next_id);
                self.infer.next_id += 1;
                self.infer.holes.insert(
                    id,
                    DynHole {
                        module: self.module.clone(),
                        span: self.span,
                        exported: self.exported,
                        requirements: vec![],
                        expected: vec![],
                    },
                );
                ContractRef::Hole(id)
            }
            contract => contract,
        }
    }
}
