use std::collections::HashMap;

use super::{
    FuncParam, MethodReceiver, ParamTypeSpans, SemanticExprSite, TypeChecker, TypeError, contracts,
    convert::push_match_error,
    decls::{ContractRequirementSchema, ModuleScope},
};
use crate::{
    ast::{
        AnonymousContract, AnonymousContractParam, AnonymousContractRequirement, ContractRef,
        DynContractHoleId, Ident, ReturnSpec, Type, TypeFolder, TypeVisitor,
    },
    lint::{LintEvent, LintId},
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
    downcasts: Vec<PendingDowncast>,
    solutions: HashMap<DynContractHoleId, ContractRef>,
}

#[derive(Clone, Default)]
pub(crate) struct DynInferenceFacts {
    next_id: u32,
    holes: HashMap<DynContractHoleId, DynHole>,
    conversions: Vec<PendingConversion>,
    dyn_sources: Vec<PendingDynSource>,
    hole_targets: Vec<PendingHoleTarget>,
    calls: Vec<PendingCall>,
    downcasts: Vec<PendingDowncast>,
    solutions: HashMap<DynContractHoleId, ContractRef>,
}

#[derive(Clone)]
pub(super) struct DynInferenceSnapshot {
    next_id: u32,
    holes: HashMap<DynContractHoleId, DynHole>,
    conversions_len: usize,
    dyn_sources_len: usize,
    hole_targets_len: usize,
    calls_len: usize,
    downcasts_len: usize,
    solutions: HashMap<DynContractHoleId, ContractRef>,
}

#[derive(Clone, PartialEq, Eq)]
struct DynHole {
    module: ModuleScope,
    span: SourceSpan,
    exported: bool,
    requirements: Vec<ContractRequirementSchema>,
    expected: Vec<ContractRef>,
}

#[derive(Clone, PartialEq, Eq)]
struct PendingConversion {
    module: ModuleScope,
    site: Option<SemanticExprSite>,
    concrete_ty: Type,
    hole: DynContractHoleId,
    span: Span,
    source_span: SourceSpan,
}

#[derive(Clone, PartialEq, Eq)]
struct PendingDynSource {
    module: ModuleScope,
    site: Option<SemanticExprSite>,
    source: ContractRef,
    hole: DynContractHoleId,
    span: Span,
    source_span: SourceSpan,
}

#[derive(Clone, PartialEq, Eq)]
struct PendingHoleTarget {
    module: ModuleScope,
    site: Option<SemanticExprSite>,
    hole: DynContractHoleId,
    target: ContractRef,
    span: Span,
    source_span: SourceSpan,
}

#[derive(Clone, PartialEq, Eq)]
struct PendingCall {
    module: ModuleScope,
    site: SemanticExprSite,
    receiver_site: SemanticExprSite,
    hole: DynContractHoleId,
    method: Ident,
    arg_count: usize,
    requires_mutable: bool,
    span: SourceSpan,
}

#[derive(Clone, PartialEq, Eq)]
struct PendingDowncast {
    module: ModuleScope,
    site: SemanticExprSite,
    source_site: SemanticExprSite,
    hole: DynContractHoleId,
    target: Type,
    mutable: bool,
    span: SourceSpan,
}

impl DynInference {
    pub(super) fn specialization_snapshot(&self) -> DynInferenceSnapshot {
        DynInferenceSnapshot {
            next_id: self.next_id,
            holes: self.holes.clone(),
            conversions_len: self.conversions.len(),
            dyn_sources_len: self.dyn_sources.len(),
            hole_targets_len: self.hole_targets.len(),
            calls_len: self.calls.len(),
            downcasts_len: self.downcasts.len(),
            solutions: self.solutions.clone(),
        }
    }

    pub(super) fn specialization_delta_since(
        &self,
        old: &DynInferenceSnapshot,
    ) -> DynInferenceFacts {
        let solutions = self
            .solutions
            .iter()
            .filter(|(id, contract)| old.solutions.get(id) != Some(contract))
            .map(|(id, contract)| (*id, contract.clone()))
            .collect();
        DynInferenceFacts {
            next_id: self.next_id.max(old.next_id),
            holes: self
                .holes
                .iter()
                .filter(|(id, hole)| old.holes.get(id) != Some(hole))
                .map(|(id, hole)| (*id, hole.clone()))
                .collect(),
            conversions: self.conversions[old.conversions_len..].to_vec(),
            dyn_sources: self.dyn_sources[old.dyn_sources_len..].to_vec(),
            hole_targets: self.hole_targets[old.hole_targets_len..].to_vec(),
            calls: self.calls[old.calls_len..].to_vec(),
            downcasts: self.downcasts[old.downcasts_len..].to_vec(),
            solutions,
        }
    }

    pub(super) fn restore_specialization(
        &mut self,
        facts: DynInferenceFacts,
    ) -> Result<(), String> {
        self.next_id = self.next_id.max(facts.next_id);
        for (id, hole) in facts.holes {
            self.restore_hole(id, hole)?;
        }
        push_unique(&mut self.conversions, facts.conversions);
        push_unique(&mut self.dyn_sources, facts.dyn_sources);
        push_unique(&mut self.hole_targets, facts.hole_targets);
        push_unique(&mut self.calls, facts.calls);
        push_unique(&mut self.downcasts, facts.downcasts);
        for (id, solution) in facts.solutions {
            if let Some(existing) = self.solutions.get(&id) {
                if existing != &solution {
                    return Err(format!(
                        "conflicting inferred dynamic solution for hole {}",
                        id.0
                    ));
                }
                continue;
            }
            self.solutions.insert(id, solution);
        }
        Ok(())
    }

    fn restore_hole(&mut self, id: DynContractHoleId, hole: DynHole) -> Result<(), String> {
        let Some(current) = self.holes.get_mut(&id) else {
            self.holes.insert(id, hole);
            return Ok(());
        };

        if let Err(conflict) =
            contracts::merge_effective_requirements(&mut current.requirements, &hole.requirements)
        {
            return Err(format!(
                "conflicting inferred dynamic requirement '{}'",
                conflict.name
            ));
        }
        for expected in hole.expected {
            if !current.expected.contains(&expected) {
                current.expected.push(expected);
            }
        }
        Ok(())
    }

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
        site: Option<SemanticExprSite>,
        concrete_ty: Type,
        hole: DynContractHoleId,
        span: Span,
        source_span: SourceSpan,
    ) {
        self.conversions.push(PendingConversion {
            module,
            site,
            concrete_ty,
            hole,
            span,
            source_span,
        });
    }

    pub(super) fn add_dyn_source(
        &mut self,
        module: ModuleScope,
        site: Option<SemanticExprSite>,
        source: ContractRef,
        hole: DynContractHoleId,
        span: Span,
        source_span: SourceSpan,
    ) {
        self.dyn_sources.push(PendingDynSource {
            module,
            site,
            source,
            hole,
            span,
            source_span,
        });
    }

    pub(super) fn add_hole_target(
        &mut self,
        module: ModuleScope,
        site: Option<SemanticExprSite>,
        hole: DynContractHoleId,
        target: ContractRef,
        span: Span,
        source_span: SourceSpan,
    ) {
        if let Some(hole) = self.holes.get_mut(&hole)
            && !hole.expected.contains(&target)
        {
            hole.expected.push(target.clone());
        }
        self.hole_targets.push(PendingHoleTarget {
            module,
            site,
            hole,
            target,
            span,
            source_span,
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
            param_spans: ParamTypeSpans::default(),
            required_params: params.len(),
            params,
            ret: ReturnSpec::value(ret),
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
        site: SemanticExprSite,
        receiver_site: SemanticExprSite,
        hole: DynContractHoleId,
        method: Ident,
        arg_count: usize,
        requires_mutable: bool,
        span: SourceSpan,
    ) {
        self.calls.push(PendingCall {
            module,
            site,
            receiver_site,
            hole,
            method,
            arg_count,
            requires_mutable,
            span,
        });
    }

    pub(super) fn add_downcast(
        &mut self,
        module: ModuleScope,
        site: SemanticExprSite,
        source_site: SemanticExprSite,
        hole: DynContractHoleId,
        target: Type,
        mutable: bool,
        span: SourceSpan,
    ) {
        self.downcasts.push(PendingDowncast {
            module,
            site,
            source_site,
            hole,
            target,
            mutable,
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
        self.finish_downcasts(tc);
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
            tc.push_lint_event(LintEvent {
                id: LintId::PublicInferredDynContract,
                span: hole.span,
                message: "exported function uses inferred dynamic contract".to_string(),
                label: "inferred dynamic contract in exported API".to_string(),
                notes: vec![],
                help: Some("declare a named contract and use `dyn Name`".to_string()),
                tags: vec![],
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
                    if let Some(site) = pending.site {
                        tc.record_dyn_conversion_at(site, witness, pending.source_span);
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
                pending.site,
                &pending.source,
                &target,
                &pending.module,
                pending.span,
                pending.source_span,
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
                pending.site,
                &source,
                &pending.target,
                &pending.module,
                pending.span,
                pending.source_span,
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
            tc.record_resolved_dyn_call(
                pending.site,
                pending.receiver_site,
                contract,
                pending.method,
                pending.arg_count,
                pending.requires_mutable,
                pending.span,
            );
        }
    }

    fn finish_downcasts(&mut self, tc: &mut TypeChecker) {
        let downcasts = std::mem::take(&mut self.downcasts);
        for pending in downcasts {
            let Some(source) = self.solution(pending.hole) else {
                continue;
            };
            let Some(source) =
                contracts::contract_set_key_for_ref(&tc.decls, &pending.module, &source)
            else {
                continue;
            };
            tc.record_resolved_dyn_downcast(
                pending.site,
                pending.source_site,
                source,
                pending.target,
                pending.mutable,
                pending.span,
            );
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
        site: Option<SemanticExprSite>,
        source: &ContractRef,
        target: &ContractRef,
        module: &ModuleScope,
        span: Span,
        source_span: SourceSpan,
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
                span: tc.module_error_span(module, span),
            });
            return;
        }
        self.record_weakening(tc, site, source, target, module, source_span);
    }

    fn record_weakening(
        &self,
        tc: &mut TypeChecker,
        site: Option<SemanticExprSite>,
        source: &ContractRef,
        target: &ContractRef,
        module: &ModuleScope,
        span: SourceSpan,
    ) {
        if source == target {
            return;
        }
        let Some(site) = site else {
            return;
        };
        let (Some(source), Some(target)) = (
            contracts::contract_set_key_for_ref(&tc.decls, module, source),
            contracts::contract_set_key_for_ref(&tc.decls, module, target),
        ) else {
            return;
        };
        tc.record_dyn_weakening_at(site, source, target, span);
    }
}

fn push_unique<T: PartialEq>(target: &mut Vec<T>, facts: Vec<T>) {
    for fact in facts {
        if !target.contains(&fact) {
            target.push(fact);
        }
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
                        escape: param.escape,
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        ast::ExprId,
        source::{SourceKind, SourceTable},
        typecheck::BodyInstanceKey,
    };

    fn test_span() -> SourceSpan {
        let mut sources = SourceTable::default();
        SourceSpan::empty(sources.add(SourceKind::Virtual, "test", None, ""), 0)
    }

    fn seed_infer() -> DynInference {
        let mut infer = DynInference::default();
        infer.holes.insert(
            DynContractHoleId(0),
            DynHole {
                module: ModuleScope::Root,
                span: test_span(),
                exported: false,
                requirements: vec![],
                expected: vec![],
            },
        );
        infer.next_id = 1;
        infer
    }

    #[test]
    fn specialization_restore_dedupes_pending_calls_and_downcasts() {
        let mut checked = seed_infer();
        let snapshot = checked.specialization_snapshot();
        checked.add_call(
            ModuleScope::Root,
            SemanticExprSite {
                body: BodyInstanceKey::Module(ModuleScope::Root),
                expr: ExprId(10),
            },
            SemanticExprSite {
                body: BodyInstanceKey::Module(ModuleScope::Root),
                expr: ExprId(11),
            },
            DynContractHoleId(0),
            Ident::new("draw"),
            0,
            false,
            test_span(),
        );
        checked.add_downcast(
            ModuleScope::Root,
            SemanticExprSite {
                body: BodyInstanceKey::Module(ModuleScope::Root),
                expr: ExprId(12),
            },
            SemanticExprSite {
                body: BodyInstanceKey::Module(ModuleScope::Root),
                expr: ExprId(13),
            },
            DynContractHoleId(0),
            Type::UnresolvedName(Ident::new("Enemy")),
            false,
            test_span(),
        );
        let facts = checked.specialization_delta_since(&snapshot);

        let mut restored = seed_infer();
        restored.restore_specialization(facts.clone()).unwrap();
        restored.restore_specialization(facts).unwrap();

        assert_eq!(restored.calls.len(), 1);
        assert_eq!(restored.downcasts.len(), 1);
        assert_eq!(restored.calls[0].method, Ident::new("draw"));
        assert_eq!(restored.downcasts[0].site.expr, ExprId(12));
    }
}
