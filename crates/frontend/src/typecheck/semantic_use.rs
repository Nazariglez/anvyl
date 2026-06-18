use std::{
    collections::{HashMap, hash_map::Entry},
    hash::Hash,
};

use super::{
    ContractSetKey, GenericArgs, MethodMode, MethodReceiver, ModuleScope, Type,
    decls::{CallableId, ExtendId, GlobalKey, NominalKey, nominal_key_for_type},
    infer::{SemanticLocalId, TypeHandle},
    type_ops::type_has_unfinished_facts,
};
use crate::{
    ast::{ConstValue, ContractRef, ExprId, Ident, ReturnSpec, TypeVisitor},
    externs::catalog::{
        ExternFieldRef, ExternFunctionId, ExternMethodRef, ExternOperatorRef, ExternStaticRef,
        ExternTypeId,
    },
    span::SourceSpan,
};

pub(crate) type SemanticExprTypeMap = HashMap<ExprId, SemanticExprType>;
pub(crate) type ConstValueMap = HashMap<ExprId, ConstValue>;
pub(crate) type CallMap = HashMap<ExprId, CallTarget>;
pub(crate) type FunctionValueMap = HashMap<ExprId, FunctionValueFact>;
pub(crate) type FunctionValueCallMap = HashMap<ExprId, FunctionValueCallFact>;
pub(crate) type DefaultArgMap = HashMap<ExprId, Vec<DefaultArgFact>>;
pub(crate) type DefaultFieldMap = HashMap<ExprId, Vec<DefaultFieldFact>>;
pub(crate) type ExternUseMap = HashMap<ExprId, Vec<ExternUseTarget>>;
pub(crate) type MemberPathMap = HashMap<ExprId, MemberPathFact>;
pub(crate) type ExpectedProjectionMap = HashMap<ExprId, ExpectedProjectionFact>;
pub(crate) type ContractWitnessMap = HashMap<WitnessId, ContractWitnessFact>;
pub(crate) type DynConversionMap = HashMap<ExprId, DynConversionFact>;
pub(crate) type DynWeakeningMap = HashMap<ExprId, DynWeakeningFact>;
pub(crate) type DynCallMap = HashMap<ExprId, DynCallFact>;
pub(crate) type DynDowncastMap = HashMap<ExprId, DynDowncastFact>;
pub(crate) type GlobalAccessMap = HashMap<ExprId, GlobalAccessFact>;
pub(crate) type StringifyMap = HashMap<ExprId, StringifyFact>;
pub(crate) type LambdaEscapeMap = HashMap<ExprId, LambdaEscapeFact>;
pub(crate) type LambdaCaptureMap = HashMap<(ExprId, BindingId), LambdaCaptureFact>;
pub(crate) type CaptureCellRequirementMap = HashMap<BindingId, CaptureCellRequirementFact>;
pub(crate) type ForStepRuntimeCheckMap = HashMap<ExprId, SourceSpan>;

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub(crate) struct LocalFacts {
    pub(crate) defs: HashMap<SemanticLocalId, LocalDefFact>,
    pub(crate) binding_defs: HashMap<SourceSpan, SemanticLocalId>,
    pub(crate) param_defs: HashMap<usize, SemanticLocalId>,
    pub(crate) uses: HashMap<ExprId, LocalUseFact>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct LocalDefFact {
    pub(crate) id: SemanticLocalId,
    pub(crate) binding_id: Option<BindingId>,
    pub(crate) name: Ident,
    pub(crate) span: Option<SourceSpan>,
    pub(crate) ty: Type,
    pub(crate) mutable: bool,
    pub(crate) kind: LocalDefKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum LocalDefKind {
    Binding,
    Parameter,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct LocalUseFact {
    pub(crate) expr_id: ExprId,
    pub(crate) local: SemanticLocalId,
    pub(crate) binding_id: Option<BindingId>,
    pub(crate) mode: LocalUseMode,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum LocalUseMode {
    Read,
    Assign,
    CompoundAssign,
    Borrow,
    MutBorrow,
    VarArgument,
}

impl LocalFacts {
    fn validate(&self) {
        for (id, fact) in &self.defs {
            debug_assert_eq!(*id, fact.id);
        }
        for (span, id) in &self.binding_defs {
            let Some(fact) = self.defs.get(id) else {
                debug_assert!(false, "binding local fact missing definition");
                continue;
            };
            debug_assert_eq!(fact.kind, LocalDefKind::Binding);
            debug_assert_eq!(fact.span, Some(*span));
            debug_assert!(fact.binding_id.is_some());
        }
        for id in self.param_defs.values() {
            let Some(fact) = self.defs.get(id) else {
                debug_assert!(false, "parameter local fact missing definition");
                continue;
            };
            debug_assert_eq!(fact.kind, LocalDefKind::Parameter);
            debug_assert!(fact.binding_id.is_some());
        }
        for (expr_id, fact) in &self.uses {
            debug_assert_eq!(*expr_id, fact.expr_id);
            debug_assert!(self.defs.contains_key(&fact.local) || fact.binding_id.is_some());
        }
    }

    fn validate_finished(&self) {
        self.validate();
        for fact in self.defs.values() {
            debug_assert!(!type_has_unfinished_facts(&fact.ty));
        }
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub(crate) struct SemanticDeclarations {
    pub(crate) modules: Vec<SemanticModuleFact>,
    pub(crate) functions: Vec<SemanticFunctionInstanceFact>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct SemanticModuleFact {
    pub(crate) module: ModuleScope,
    pub(crate) source: crate::source::SourceId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct SemanticFunctionInstanceFact {
    pub(crate) id: CallableId,
    pub(crate) args: GenericArgs,
    pub(crate) body: BodyInstanceKey,
    pub(crate) module: ModuleScope,
    pub(crate) name: Ident,
    pub(crate) span: SourceSpan,
    pub(crate) body_span: SourceSpan,
    pub(crate) params: Vec<SemanticParamSigFact>,
    pub(crate) ret: ReturnSpec,
    pub(crate) is_stringify_override: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct SemanticParamSigFact {
    pub(crate) name: Ident,
    pub(crate) span: SourceSpan,
    pub(crate) ty: Type,
    pub(crate) mutable: bool,
    pub(crate) escape: crate::ast::EscapeMode,
}

impl SemanticDeclarations {
    pub(crate) fn validate(&self) {
        let mut modules = std::collections::HashSet::new();
        for fact in &self.modules {
            debug_assert!(modules.insert(fact.module.clone()));
        }

        let mut functions = std::collections::HashSet::new();
        for fact in &self.functions {
            match &fact.body {
                BodyInstanceKey::Callable(key) => {
                    debug_assert_eq!(key.target, fact.id);
                    debug_assert_eq!(key.args, fact.args);
                }
                BodyInstanceKey::Module(_)
                | BodyInstanceKey::Lambda(_)
                | BodyInstanceKey::Global(_)
                | BodyInstanceKey::CastFrom(_) => {
                    debug_assert!(false, "semantic function fact has non-callable body");
                }
            }
            debug_assert_eq!(fact.module, fact.id.module);
            debug_assert_eq!(fact.name, fact.id.name);
            debug_assert!(functions.insert((fact.id.clone(), fact.args.clone())));
            debug_assert!(fact.span.start() <= fact.span.end());
            debug_assert!(fact.body_span.start() <= fact.body_span.end());
            for param in &fact.params {
                debug_assert!(param.span.start() <= param.span.end());
                debug_assert!(!type_has_unfinished_facts(&param.ty));
            }
            debug_assert!(!type_has_unfinished_facts(&fact.ret.ty));
            if fact.is_stringify_override {
                debug_assert_eq!(fact.name, Ident::new("to_string"));
                debug_assert_eq!(fact.params.len(), 1);
                debug_assert_eq!(fact.ret.ty, Type::String);
                debug_assert!(fact.id.kind.has_receiver_param());
            }
        }
    }

    pub(crate) fn validate_bodies(&self, facts: &SemanticFactMaps) {
        for body in facts.bodies.values() {
            for defaults in body.default_args.values() {
                for default in defaults {
                    if let Some(function) = self.functions.iter().find(|function| {
                        function.id == default.callee.target && function.args == default.callee.args
                    }) {
                        let param_index = default.param_index
                            + usize::from(function.id.kind.has_receiver_param());
                        debug_assert!(param_index < function.params.len());
                        debug_assert_eq!(function.params[param_index].ty, default.ty);
                    } else if default.callee.args.is_empty() {
                        debug_assert!(false, "default arg fact targets missing function instance");
                    }
                }
            }
        }
        for function in &self.functions {
            let Some(body) = facts.body(&function.body) else {
                debug_assert!(
                    function.params.is_empty() && function.ret.ty == Type::Void,
                    "semantic function fact missing body facts"
                );
                continue;
            };
            debug_assert_eq!(body.locals.param_defs.len(), function.params.len());
            for (index, param) in function.params.iter().enumerate() {
                let Some(local) = body.locals.param_defs.get(&index) else {
                    debug_assert!(false, "semantic function param missing local fact");
                    continue;
                };
                let Some(def) = body.locals.defs.get(local) else {
                    debug_assert!(false, "semantic function param missing local definition");
                    continue;
                };
                debug_assert_eq!(def.kind, LocalDefKind::Parameter);
                debug_assert_eq!(def.name, param.name);
                if !type_has_unfinished_facts(&def.ty)
                    && !type_has_unfinished_facts(&param.ty)
                    && !type_contains_dyn_hole(&def.ty)
                    && !type_contains_dyn_hole(&param.ty)
                {
                    debug_assert_eq!(def.ty, param.ty);
                }
                debug_assert_eq!(def.mutable, param.mutable);
            }
        }
    }
}

fn type_contains_dyn_hole(ty: &Type) -> bool {
    struct DynHoleVisitor;

    impl TypeVisitor for DynHoleVisitor {
        fn visit_contract_ref_leaf(&mut self, contract: &ContractRef) -> bool {
            matches!(contract, ContractRef::Infer | ContractRef::Hole(_))
        }
    }

    let mut visitor = DynHoleVisitor;
    visitor.visit_type(ty)
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct CallableInstanceKey {
    pub(crate) target: CallableId,
    pub(crate) args: GenericArgs,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct CastFromInstanceKey {
    pub(crate) extend: ExtendId,
    pub(crate) index: usize,
    pub(crate) args: GenericArgs,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) enum BodyInstanceKey {
    Module(ModuleScope),
    Callable(CallableInstanceKey),
    Lambda(LambdaBodyKey),
    Global(GlobalKey),
    CastFrom(CastFromInstanceKey),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct LambdaBodyKey {
    pub(crate) expr: ExprId,
    pub(crate) specialization: GenericArgs,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct SemanticExprSite {
    pub(crate) body: BodyInstanceKey,
    pub(crate) expr: ExprId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct DefaultArgFact {
    pub(crate) call: ExprId,
    pub(crate) callee: CallableInstanceKey,
    pub(crate) param_index: usize,
    pub(crate) default: DefaultExprSite,
    pub(crate) ty: Type,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct DefaultFieldFact {
    pub(crate) aggregate: ExprId,
    pub(crate) owner: Type,
    pub(crate) owner_key: NominalKey,
    pub(crate) field: Ident,
    pub(crate) slot: usize,
    pub(crate) default: DefaultExprSite,
    pub(crate) ty: Type,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct DefaultExprSite {
    pub(crate) expr: ExprId,
    pub(crate) source: crate::source::SourceId,
}

pub(super) fn map_delta<K, V>(old: &HashMap<K, V>, current: &HashMap<K, V>) -> HashMap<K, V>
where
    K: Copy + Eq + Hash,
    V: Clone + PartialEq,
{
    current
        .iter()
        .filter_map(|(id, item)| match old.get(id) {
            Some(old_item) if old_item == item => None,
            _ => Some((*id, item.clone())),
        })
        .collect()
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct SemanticExprType {
    pub(crate) span: Option<SourceSpan>,
    pub(super) handle: TypeHandle,
    pub(crate) ty: Option<Type>,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub(crate) struct SemanticBodyFacts {
    pub(crate) expr_types: SemanticExprTypeMap,
    pub(crate) const_values: ConstValueMap,
    pub(crate) calls: CallMap,
    pub(crate) function_values: FunctionValueMap,
    pub(crate) function_value_calls: FunctionValueCallMap,
    pub(crate) default_args: DefaultArgMap,
    pub(crate) default_fields: DefaultFieldMap,
    pub(crate) extern_uses: ExternUseMap,
    pub(crate) member_paths: MemberPathMap,
    pub(crate) expected_projections: ExpectedProjectionMap,
    pub(crate) dyn_conversions: DynConversionMap,
    pub(crate) dyn_weakenings: DynWeakeningMap,
    pub(crate) dyn_calls: DynCallMap,
    pub(crate) dyn_downcasts: DynDowncastMap,
    pub(crate) global_accesses: GlobalAccessMap,
    pub(crate) stringifies: StringifyMap,
    pub(crate) for_step_runtime_checks: ForStepRuntimeCheckMap,
    pub(crate) locals: LocalFacts,
}

impl SemanticBodyFacts {
    fn merge_from(&mut self, facts: Self) {
        self.expr_types.extend(facts.expr_types);
        self.const_values.extend(facts.const_values);
        self.calls.extend(facts.calls);
        self.function_values.extend(facts.function_values);
        self.function_value_calls.extend(facts.function_value_calls);
        self.default_args.extend(facts.default_args);
        self.default_fields.extend(facts.default_fields);
        self.extern_uses.extend(facts.extern_uses);
        self.member_paths.extend(facts.member_paths);
        self.expected_projections.extend(facts.expected_projections);
        self.dyn_conversions.extend(facts.dyn_conversions);
        self.dyn_weakenings.extend(facts.dyn_weakenings);
        self.dyn_calls.extend(facts.dyn_calls);
        self.dyn_downcasts.extend(facts.dyn_downcasts);
        self.global_accesses.extend(facts.global_accesses);
        self.stringifies.extend(facts.stringifies);
        self.for_step_runtime_checks
            .extend(facts.for_step_runtime_checks);
    }

    pub(crate) fn validate(&self) {
        for fact in self.expr_types.values() {
            debug_assert!(fact.span.is_some());
        }
        for expr_id in self.const_values.keys() {
            debug_assert!(self.expr_types.contains_key(expr_id));
        }
        for (expr_id, fact) in &self.function_values {
            debug_assert_eq!(*expr_id, fact.expr);
        }
        for (expr_id, fact) in &self.function_value_calls {
            debug_assert_eq!(*expr_id, fact.expr);
        }
        for (expr_id, facts) in &self.default_args {
            let call = self.calls.get(expr_id);
            for fact in facts {
                debug_assert_eq!(*expr_id, fact.call);
                if let Some(call) = call {
                    debug_assert_eq!(fact.callee.target, call.id);
                    debug_assert_eq!(fact.callee.args, call.args);
                } else {
                    debug_assert!(false, "default arg fact missing call target");
                }
                debug_assert!(!type_has_unfinished_facts(&fact.ty));
            }
        }
        for (expr_id, facts) in &self.default_fields {
            debug_assert!(self.expr_types.contains_key(expr_id));
            for fact in facts {
                debug_assert_eq!(*expr_id, fact.aggregate);
                debug_assert_eq!(
                    nominal_key_for_type(&fact.owner),
                    Some(fact.owner_key.clone())
                );
                debug_assert!(!type_has_unfinished_facts(&fact.owner));
                debug_assert!(!type_has_unfinished_facts(&fact.ty));
            }
        }
        for (expr_id, fact) in &self.member_paths {
            debug_assert_eq!(*expr_id, fact.expr_id);
        }
        for (expr_id, fact) in &self.expected_projections {
            debug_assert_eq!(*expr_id, fact.expr_id);
        }
        for (expr_id, fact) in &self.dyn_conversions {
            debug_assert_eq!(*expr_id, fact.expr_id);
        }
        for (expr_id, fact) in &self.dyn_weakenings {
            debug_assert_eq!(*expr_id, fact.expr_id);
        }
        for (expr_id, fact) in &self.dyn_calls {
            debug_assert_eq!(*expr_id, fact.call_id);
        }
        for (expr_id, fact) in &self.dyn_downcasts {
            debug_assert_eq!(*expr_id, fact.expr_id);
        }
        for (expr_id, fact) in &self.global_accesses {
            debug_assert_eq!(*expr_id, fact.expr_id);
        }
        for (expr_id, fact) in &self.stringifies {
            debug_assert!(self.expr_types.contains_key(expr_id));
            debug_assert!(self.expr_types.contains_key(&fact.arg));
        }
        self.locals.validate();
    }

    pub(crate) fn validate_finished(&self) {
        self.validate();
        for fact in self.expr_types.values() {
            debug_assert!(fact.ty.is_some());
        }
        self.locals.validate_finished();
        for fact in self.function_values.values() {
            debug_assert!(matches!(fact.ty, Type::Func { .. }));
            debug_assert!(!type_has_unfinished_facts(&fact.ty));
            if let FunctionValueKind::Lambda { lambda_expr } = &fact.kind {
                debug_assert_eq!(*lambda_expr, fact.expr);
            }
        }
        for fact in self.function_value_calls.values() {
            debug_assert!(!self.calls.contains_key(&fact.expr));
            let Type::Func { params, ret } = &fact.sig else {
                debug_assert!(false, "function-value call has non-function signature");
                continue;
            };
            debug_assert!(!type_has_unfinished_facts(&fact.sig));
            debug_assert!(!type_has_unfinished_facts(&ret.ty));
            debug_assert_eq!(params.len(), fact.args.len());
            for (arg, param) in fact.args.iter().zip(params) {
                debug_assert_eq!(arg.param_ty, param.ty);
                debug_assert_eq!(arg.mutable, param.mutable);
                debug_assert_eq!(arg.escape, param.escape);
                debug_assert!(self.expr_types.contains_key(&arg.expr));
            }
            let Some(callee_fact) = self.function_values.get(&fact.callee) else {
                debug_assert!(
                    false,
                    "function-value call missing callee function-value fact"
                );
                continue;
            };
            debug_assert_eq!(callee_fact.ty, fact.sig);
        }
        for fact in self.stringifies.values() {
            debug_assert!(!type_has_unfinished_facts(&fact.source_ty));
            let Some(arg) = self.expr_types.get(&fact.arg) else {
                debug_assert!(false, "stringify argument missing expression type in body");
                continue;
            };
            let Some(arg_ty) = arg.ty.as_ref() else {
                debug_assert!(false, "stringify argument expression type not finalized");
                continue;
            };
            debug_assert_eq!(arg_ty, &fact.source_ty);
        }
        #[cfg(debug_assertions)]
        for (expr_id, projection) in &self.expected_projections {
            let Some(expr) = self.expr_types.get(expr_id) else {
                debug_assert!(false, "expected projection missing expression type in body");
                continue;
            };
            let Some(ty) = expr.ty.as_ref() else {
                debug_assert!(false, "expected projection expression type not finalized");
                continue;
            };
            debug_assert_eq!(ty, &projection.target_ty);
        }
    }
}

#[derive(Clone, Default)]
pub(crate) struct SemanticFactMaps {
    pub(crate) bodies: HashMap<BodyInstanceKey, SemanticBodyFacts>,
    pub(crate) contract_witnesses: ContractWitnessMap,
    witness_keys: HashMap<ContractWitnessKey, WitnessId>,
    next_witness_id: u32,
}

impl SemanticFactMaps {
    pub(crate) fn body(&self, key: &BodyInstanceKey) -> Option<&SemanticBodyFacts> {
        self.bodies.get(key)
    }

    pub(crate) fn flattened_body_facts(&self) -> SemanticBodyFacts {
        let mut flat = SemanticBodyFacts::default();
        for body in self.bodies.values() {
            flat.merge_from(body.clone());
        }
        flat
    }

    fn body_mut(&mut self, key: BodyInstanceKey) -> &mut SemanticBodyFacts {
        self.bodies.entry(key).or_default()
    }

    pub(crate) fn merge_body(&mut self, key: BodyInstanceKey, body: SemanticBodyFacts) {
        match self.bodies.get_mut(&key) {
            Some(existing) => {
                assert_eq!(
                    existing, &body,
                    "conflicting semantic body facts for {key:?}"
                );
            }
            None => {
                self.bodies.insert(key, body);
            }
        }
    }

    pub(crate) fn merge_witnesses(&mut self, witnesses: ContractWitnessMap) {
        for fact in witnesses.into_values() {
            if let Some(existing) = self.contract_witnesses.get(&fact.id) {
                assert_eq!(existing, &fact, "conflicting contract witness id");
                continue;
            }
            if let Some(existing_id) = self.witness_keys.get(&fact.key) {
                assert_eq!(*existing_id, fact.id, "conflicting contract witness key");
                continue;
            }
            self.next_witness_id = self.next_witness_id.max(fact.id.0 + 1);
            self.witness_keys.insert(fact.key.clone(), fact.id);
            self.contract_witnesses.insert(fact.id, fact);
        }
    }

    pub(super) fn record_local_def(&mut self, body: BodyInstanceKey, fact: LocalDefFact) {
        let local = fact.id;
        self.body_mut(body).locals.defs.insert(local, fact);
    }

    pub(super) fn record_binding_def(
        &mut self,
        body: BodyInstanceKey,
        span: SourceSpan,
        local: SemanticLocalId,
    ) {
        self.body_mut(body).locals.binding_defs.insert(span, local);
    }

    pub(super) fn record_param_def(
        &mut self,
        body: BodyInstanceKey,
        index: usize,
        local: SemanticLocalId,
    ) {
        self.body_mut(body).locals.param_defs.insert(index, local);
    }

    pub(super) fn record_local_use(&mut self, body: BodyInstanceKey, fact: LocalUseFact) {
        match self.body_mut(body).locals.uses.entry(fact.expr_id) {
            Entry::Occupied(existing) => {
                debug_assert_eq!(existing.get(), &fact, "conflicting semantic local use fact");
            }
            Entry::Vacant(slot) => {
                slot.insert(fact);
            }
        }
    }

    pub(crate) fn finish_local_def(
        &mut self,
        body: &BodyInstanceKey,
        local: SemanticLocalId,
        ty: Type,
    ) {
        let fact = self
            .bodies
            .get_mut(body)
            .and_then(|body| body.locals.defs.get_mut(&local))
            .expect("semantic local definition missing during finish");
        fact.ty = ty;
    }

    pub(super) fn record_expr_type(
        &mut self,
        site: SemanticExprSite,
        span: Option<SourceSpan>,
        handle: TypeHandle,
    ) {
        self.body_mut(site.body).expr_types.insert(
            site.expr,
            SemanticExprType {
                span,
                handle,
                ty: None,
            },
        );
    }

    pub(crate) fn finish_expr_type(&mut self, body: &BodyInstanceKey, expr: ExprId, ty: Type) {
        let fact = self
            .bodies
            .get_mut(body)
            .and_then(|body| body.expr_types.get_mut(&expr))
            .expect("semantic expression type missing during finish");
        fact.ty = Some(ty);
    }

    pub(crate) fn record_const_value(&mut self, site: SemanticExprSite, value: ConstValue) {
        self.body_mut(site.body)
            .const_values
            .insert(site.expr, value);
    }

    pub(crate) fn record_call(&mut self, site: SemanticExprSite, target: CallTarget) {
        self.body_mut(site.body).calls.insert(site.expr, target);
    }

    pub(crate) fn record_function_value(
        &mut self,
        site: SemanticExprSite,
        fact: FunctionValueFact,
    ) {
        self.body_mut(site.body)
            .function_values
            .insert(site.expr, fact);
    }

    pub(crate) fn record_function_value_call(
        &mut self,
        site: SemanticExprSite,
        fact: FunctionValueCallFact,
    ) {
        self.body_mut(site.body)
            .function_value_calls
            .insert(site.expr, fact);
    }

    pub(crate) fn record_default_arg(&mut self, body: BodyInstanceKey, fact: DefaultArgFact) {
        self.body_mut(body)
            .default_args
            .entry(fact.call)
            .or_default()
            .push(fact);
    }

    pub(crate) fn record_default_field(&mut self, body: BodyInstanceKey, fact: DefaultFieldFact) {
        self.body_mut(body)
            .default_fields
            .entry(fact.aggregate)
            .or_default()
            .push(fact);
    }

    pub(crate) fn record_extern_use(&mut self, site: SemanticExprSite, target: ExternUseTarget) {
        let targets = self
            .body_mut(site.body)
            .extern_uses
            .entry(site.expr)
            .or_default();
        if !targets.contains(&target) {
            targets.push(target);
        }
    }

    pub(crate) fn record_member_path(&mut self, body: BodyInstanceKey, fact: MemberPathFact) {
        self.body_mut(body).member_paths.insert(fact.expr_id, fact);
    }

    pub(crate) fn record_expected_projection(
        &mut self,
        body: BodyInstanceKey,
        fact: ExpectedProjectionFact,
    ) {
        self.body_mut(body)
            .expected_projections
            .insert(fact.expr_id, fact);
    }

    pub(crate) fn record_contract_witness(
        &mut self,
        key: ContractWitnessKey,
        span: SourceSpan,
    ) -> WitnessId {
        if let Some(id) = self.witness_keys.get(&key) {
            return *id;
        }
        let id = WitnessId(self.next_witness_id);
        self.next_witness_id += 1;
        let fact = ContractWitnessFact {
            id,
            key: key.clone(),
            span,
        };
        self.witness_keys.insert(key, id);
        self.contract_witnesses.insert(id, fact);
        id
    }

    pub(crate) fn record_dyn_conversion(&mut self, body: BodyInstanceKey, fact: DynConversionFact) {
        self.body_mut(body)
            .dyn_conversions
            .insert(fact.expr_id, fact);
    }

    pub(crate) fn record_dyn_weakening(&mut self, body: BodyInstanceKey, fact: DynWeakeningFact) {
        self.body_mut(body)
            .dyn_weakenings
            .insert(fact.expr_id, fact);
    }

    pub(crate) fn record_dyn_call(&mut self, body: BodyInstanceKey, fact: DynCallFact) {
        self.body_mut(body).dyn_calls.insert(fact.call_id, fact);
    }

    pub(crate) fn record_dyn_downcast(&mut self, body: BodyInstanceKey, fact: DynDowncastFact) {
        self.body_mut(body).dyn_downcasts.insert(fact.expr_id, fact);
    }

    pub(crate) fn record_global_access(&mut self, body: BodyInstanceKey, fact: GlobalAccessFact) {
        self.body_mut(body)
            .global_accesses
            .insert(fact.expr_id, fact);
    }

    pub(crate) fn record_stringify(&mut self, site: SemanticExprSite, arg: ExprId) {
        self.body_mut(site.body).stringifies.insert(
            site.expr,
            StringifyFact {
                arg,
                source_ty: Type::Infer,
            },
        );
    }

    pub(crate) fn finish_stringifies(&mut self) {
        let records = self
            .bodies
            .iter()
            .flat_map(|(body_key, body)| {
                body.stringifies
                    .iter()
                    .map(|(call, fact)| (body_key.clone(), *call, fact.arg))
            })
            .collect::<Vec<_>>();
        for (body_key, call, arg) in records {
            let source_ty = self
                .bodies
                .get(&body_key)
                .and_then(|body| body.expr_types.get(&arg))
                .and_then(|fact| fact.ty.clone())
                .expect("stringify argument type missing during finish");
            let fact = self
                .bodies
                .get_mut(&body_key)
                .and_then(|body| body.stringifies.get_mut(&call))
                .expect("stringify fact missing during finish");
            fact.source_ty = source_ty;
        }
    }

    pub(crate) fn record_for_step_runtime_check(
        &mut self,
        site: SemanticExprSite,
        span: SourceSpan,
    ) {
        self.body_mut(site.body)
            .for_step_runtime_checks
            .insert(site.expr, span);
    }

    pub(crate) fn validate_finished(&self) {
        for (witness_id, fact) in &self.contract_witnesses {
            debug_assert_eq!(*witness_id, fact.id);
        }
        #[cfg(debug_assertions)]
        let binding_defs = self
            .bodies
            .values()
            .flat_map(|body| {
                body.locals
                    .defs
                    .iter()
                    .map(|(local, fact)| (*local, fact.binding_id))
            })
            .collect::<HashMap<_, _>>();
        for body in self.bodies.values() {
            body.validate_finished();
            #[cfg(debug_assertions)]
            {
                for fact in body.locals.uses.values() {
                    debug_assert_eq!(
                        binding_defs.get(&fact.local).copied().flatten(),
                        fact.binding_id
                    );
                }
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub(crate) struct BindingId(pub(crate) u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum LambdaEscapeKind {
    NonEscaping,
    Escaping,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct LambdaEscapeFact {
    pub(crate) expr_id: ExprId,
    pub(crate) escape: LambdaEscapeKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum CaptureStorageOrigin {
    Owned,
    BorrowedParam,
    VarSelf,
    DynView,
    PatternAlias,
    MutableDowncastAlias,
    ForVarAlias,
    Const,
    ReadonlySelf,
}

impl CaptureStorageOrigin {
    pub(crate) fn is_borrowed_runtime(self) -> bool {
        matches!(
            self,
            Self::BorrowedParam
                | Self::VarSelf
                | Self::DynView
                | Self::PatternAlias
                | Self::MutableDowncastAlias
                | Self::ForVarAlias
        )
    }

    pub(crate) fn requires_runtime_capture(self) -> bool {
        !matches!(self, Self::Const)
    }

    pub(crate) fn is_air_local(self) -> bool {
        matches!(
            self,
            Self::Owned
                | Self::BorrowedParam
                | Self::ReadonlySelf
                | Self::VarSelf
                | Self::PatternAlias
                | Self::ForVarAlias
        )
    }

    pub(crate) fn capture_storage(self, source_mutable: bool, escaping: bool) -> CaptureStorage {
        if self.is_borrowed_runtime() {
            return if escaping {
                CaptureStorage::BorrowedEscaping
            } else {
                CaptureStorage::BorrowedScoped
            };
        }

        match self {
            Self::Const => CaptureStorage::NoRuntime,
            Self::Owned if source_mutable && escaping => CaptureStorage::OwnedMutableUpvalue,
            Self::Owned if source_mutable => CaptureStorage::OwnedMutableScoped,
            Self::Owned | Self::ReadonlySelf => CaptureStorage::OwnedReadonly,
            Self::BorrowedParam
            | Self::VarSelf
            | Self::DynView
            | Self::PatternAlias
            | Self::MutableDowncastAlias
            | Self::ForVarAlias => unreachable!("borrowed capture origin returned early"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum CaptureAccess {
    Read,
    Mutable,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum CaptureStorage {
    NoRuntime,
    OwnedReadonly,
    OwnedMutableScoped,
    OwnedMutableUpvalue,
    BorrowedScoped,
    BorrowedEscaping,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct LambdaCaptureFact {
    pub(crate) lambda_id: ExprId,
    pub(crate) binding_id: BindingId,
    pub(crate) name: Ident,
    pub(crate) ty: Type,
    pub(crate) origin: CaptureStorageOrigin,
    pub(crate) source_mutable: bool,
    pub(crate) access: CaptureAccess,
    pub(crate) storage: CaptureStorage,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct CaptureCellRequirementFact {
    pub(crate) binding_id: BindingId,
    pub(crate) name: Ident,
    pub(crate) ty: Type,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct CallTarget {
    pub(crate) id: CallableId,
    pub(crate) args: GenericArgs,
    pub(crate) form: CallForm,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct FunctionValueFact {
    pub(crate) expr: ExprId,
    pub(crate) ty: Type,
    pub(crate) kind: FunctionValueKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum FunctionValueKind {
    Named(CallableInstanceKey),
    Lambda { lambda_expr: ExprId },
    LocalOrPlace,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct FunctionValueCallFact {
    pub(crate) expr: ExprId,
    pub(crate) callee: ExprId,
    pub(crate) sig: Type,
    pub(crate) args: Vec<FunctionValueArgFact>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct FunctionValueArgFact {
    pub(crate) expr: ExprId,
    pub(crate) param_ty: Type,
    pub(crate) mutable: bool,
    pub(crate) escape: crate::ast::EscapeMode,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum CallForm {
    Normal,
    QualifiedExtend { receiver: ExprId },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum GlobalAccessMode {
    Read,
    RootAssign,
    ProjectedAssign,
    CompoundAssign,
    ImmutableBorrow,
    MutableBorrow,
    VarArgument,
    MutReceiver,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum GlobalInitEffect {
    InitializeFirst,
    StoreWithoutInit,
}

impl GlobalAccessMode {
    pub(crate) fn init_effect(self) -> GlobalInitEffect {
        match self {
            Self::RootAssign => GlobalInitEffect::StoreWithoutInit,
            Self::Read
            | Self::ProjectedAssign
            | Self::CompoundAssign
            | Self::ImmutableBorrow
            | Self::MutableBorrow
            | Self::VarArgument
            | Self::MutReceiver => GlobalInitEffect::InitializeFirst,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct GlobalAccessFact {
    pub(crate) expr_id: ExprId,
    pub(crate) root_expr_id: ExprId,
    pub(crate) key: GlobalKey,
    pub(crate) mode: GlobalAccessMode,
    pub(crate) init_effect: GlobalInitEffect,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct StringifyFact {
    pub(crate) arg: ExprId,
    pub(crate) source_ty: Type,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum MemberPathKind {
    Field,
    MethodReceiver,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct MemberPathFact {
    pub(crate) expr_id: ExprId,
    pub(crate) kind: MemberPathKind,
    pub(crate) path: Vec<Ident>,
    pub(crate) origin_owner: Type,
    pub(crate) origin_member: Ident,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct ExpectedProjectionFact {
    pub(crate) expr_id: ExprId,
    pub(crate) path: Vec<Ident>,
    pub(crate) target_ty: Type,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct WitnessId(pub(crate) u32);

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct ContractWitnessFact {
    pub(crate) id: WitnessId,
    pub(crate) key: ContractWitnessKey,
    pub(crate) span: SourceSpan,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct ContractWitnessKey {
    pub(crate) concrete_ty: Type,
    pub(crate) contract: ContractSetKey,
    pub(crate) slots: Vec<WitnessSlot>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct WitnessSlot {
    pub(crate) name: Ident,
    pub(crate) required_receiver: MethodReceiver,
    pub(crate) target: WitnessSlotTarget,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) enum WitnessSlotTarget {
    Direct {
        callable: CallableId,
        owner_args: GenericArgs,
        receiver_mode: MethodMode,
    },
    Extend {
        extend: ExtendId,
        callable: CallableId,
        owner_args: GenericArgs,
        receiver_mode: MethodMode,
    },
    Extern {
        method: ExternMethodRef,
        receiver: anvyx_externs::ReceiverMode,
    },
    Promoted {
        path: Vec<Ident>,
        origin_owner: Type,
        origin_method: Ident,
        target: Box<WitnessSlotTarget>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct DynConversionFact {
    pub(crate) expr_id: ExprId,
    pub(crate) witness: WitnessId,
    pub(crate) span: SourceSpan,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct DynWeakeningFact {
    pub(crate) expr_id: ExprId,
    pub(crate) source: ContractSetKey,
    pub(crate) target: ContractSetKey,
    pub(crate) span: SourceSpan,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct DynCallFact {
    pub(crate) call_id: ExprId,
    pub(crate) receiver_id: ExprId,
    pub(crate) contract: ContractSetKey,
    pub(crate) method: Ident,
    pub(crate) arg_count: usize,
    pub(crate) requires_mutable: bool,
    pub(crate) span: SourceSpan,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct DynDowncastFact {
    pub(crate) expr_id: ExprId,
    pub(crate) source_id: ExprId,
    pub(crate) source: ContractSetKey,
    pub(crate) target: Type,
    pub(crate) mutable: bool,
    pub(crate) span: SourceSpan,
}

impl CallTarget {
    pub(crate) fn new(id: CallableId, args: GenericArgs) -> Self {
        Self {
            id,
            args,
            form: CallForm::Normal,
        }
    }

    pub(crate) fn qualified_extend(id: CallableId, args: GenericArgs, receiver: ExprId) -> Self {
        Self {
            id,
            args,
            form: CallForm::QualifiedExtend { receiver },
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum ExternUseTarget {
    Function(ExternFunctionId),
    FieldRead(ExternFieldRef),
    FieldWrite(ExternFieldRef),
    Method(ExternMethodRef),
    Static(ExternStaticRef),
    Init(ExternTypeId),
    UnaryOperator(ExternOperatorRef),
    BinaryOperator(ExternOperatorRef),
}
