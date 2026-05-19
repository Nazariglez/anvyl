use std::{collections::HashMap, hash::Hash};

use super::{
    ContractSetKey, GenericArgs, MethodMode, MethodReceiver, ModuleScope, Type,
    decls::{CallableId, ExtendId, GlobalKey},
    infer::TypeHandle,
};
use crate::{
    ast::{ExprId, Ident},
    externs::catalog::{
        ExternFieldRef, ExternFunctionId, ExternMethodRef, ExternOperatorRef, ExternStaticRef,
        ExternTypeId,
    },
    span::SourceSpan,
};

pub(crate) type SemanticExprTypeMap = HashMap<ExprId, SemanticExprType>;
pub(crate) type CallMap = HashMap<ExprId, CallTarget>;
pub(crate) type ExternUseMap = HashMap<ExprId, Vec<ExternUseTarget>>;
pub(crate) type MemberPathMap = HashMap<ExprId, MemberPathFact>;
pub(crate) type ExpectedProjectionMap = HashMap<ExprId, ExpectedProjectionFact>;
pub(crate) type ContractWitnessMap = HashMap<WitnessId, ContractWitnessFact>;
pub(crate) type DynConversionMap = HashMap<ExprId, DynConversionFact>;
pub(crate) type DynWeakeningMap = HashMap<ExprId, DynWeakeningFact>;
pub(crate) type DynCallMap = HashMap<ExprId, DynCallFact>;
pub(crate) type DynDowncastMap = HashMap<ExprId, DynDowncastFact>;
pub(crate) type GlobalAccessMap = HashMap<ExprId, GlobalAccessFact>;
pub(crate) type LambdaEscapeMap = HashMap<ExprId, LambdaEscapeFact>;
pub(crate) type LambdaCaptureMap = HashMap<(ExprId, BindingId), LambdaCaptureFact>;
pub(crate) type BindingPromotionMap = HashMap<BindingId, BindingPromotionFact>;
pub(crate) type ForStepRuntimeCheckMap = HashMap<ExprId, SourceSpan>;

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
    CastFrom(CastFromInstanceKey),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct SemanticExprSite {
    pub(crate) body: BodyInstanceKey,
    pub(crate) expr: ExprId,
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
    pub(crate) calls: CallMap,
    pub(crate) extern_uses: ExternUseMap,
    pub(crate) member_paths: MemberPathMap,
    pub(crate) expected_projections: ExpectedProjectionMap,
    pub(crate) dyn_conversions: DynConversionMap,
    pub(crate) dyn_weakenings: DynWeakeningMap,
    pub(crate) dyn_calls: DynCallMap,
    pub(crate) dyn_downcasts: DynDowncastMap,
    pub(crate) global_accesses: GlobalAccessMap,
    pub(crate) for_step_runtime_checks: ForStepRuntimeCheckMap,
}

impl SemanticBodyFacts {
    fn merge_from(&mut self, facts: Self) {
        self.expr_types.extend(facts.expr_types);
        self.calls.extend(facts.calls);
        self.extern_uses.extend(facts.extern_uses);
        self.member_paths.extend(facts.member_paths);
        self.expected_projections.extend(facts.expected_projections);
        self.dyn_conversions.extend(facts.dyn_conversions);
        self.dyn_weakenings.extend(facts.dyn_weakenings);
        self.dyn_calls.extend(facts.dyn_calls);
        self.dyn_downcasts.extend(facts.dyn_downcasts);
        self.global_accesses.extend(facts.global_accesses);
        self.for_step_runtime_checks
            .extend(facts.for_step_runtime_checks);
    }

    pub(crate) fn validate(&self) {
        for fact in self.expr_types.values() {
            debug_assert!(fact.span.is_some());
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
    }

    pub(crate) fn validate_finished(&self) {
        self.validate();
        for fact in self.expr_types.values() {
            debug_assert!(fact.ty.is_some());
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

    pub(crate) fn record_call(&mut self, site: SemanticExprSite, target: CallTarget) {
        self.body_mut(site.body).calls.insert(site.expr, target);
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
        for body in self.bodies.values() {
            body.validate_finished();
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
pub(crate) struct BindingPromotionFact {
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct GlobalAccessFact {
    pub(crate) expr_id: ExprId,
    pub(crate) key: GlobalKey,
    pub(crate) mode: GlobalAccessMode,
    pub(crate) init_effect: GlobalInitEffect,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
