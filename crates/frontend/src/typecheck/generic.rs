use std::collections::HashMap;

use super::{
    ArgumentProjectionMap, CallMap, CallableRef, CallableTemplate, ContractWitnessMap, DynCallMap,
    DynConversionMap, DynDowncastMap, DynWeakeningMap, ExternUseMap, ForStepRuntimeCheckMap,
    GenericTypeContext, GlobalAccessMap, MemberPathMap, TypeChecker, TypecheckFacts,
    const_term::ConstTerm,
    decls::CallableId,
    dyn_infer::DynInferenceFacts,
    infer::{GenericSolverSeeds, Solver},
    semantic_use::map_delta,
    type_ops::TypeFolder,
};
use crate::{
    ast::{
        ArrayLen, ConstArg, ConstParam, ConstParamId, ConstValue, ContractRef, ExprId, GenericArg,
        Ident, Type, TypeParam, TypeVarId,
    },
    span::Span,
};

pub(crate) type TypeSubst = HashMap<TypeVarId, Type>;
pub(crate) type ConstSubst = HashMap<ConstParamId, ConstTerm>;

struct CheckedSubstituter<'a, 'tc> {
    tc: &'tc mut TypeChecker,
    span: Span,
    types: &'a TypeSubst,
    consts: &'a ConstSubst,
}

impl TypeFolder for CheckedSubstituter<'_, '_> {
    fn fold_var(&mut self, id: TypeVarId) -> Type {
        self.types.get(&id).cloned().unwrap_or(Type::Var(id))
    }

    fn fold_const_arg(&mut self, arg: &ConstArg) -> ConstArg {
        match arg {
            ConstArg::Param(id) => self
                .consts
                .get(id)
                .and_then(ConstTerm::to_arg_no_infer)
                .unwrap_or_else(|| arg.clone()),
            ConstArg::Value(_) | ConstArg::Name(_) => arg.clone(),
        }
    }

    fn fold_array_len(&mut self, len: ArrayLen) -> ArrayLen {
        match len {
            ArrayLen::Param(id) => match self.consts.get(&id).cloned() {
                Some(term) => self
                    .tc
                    .array_len_from_term(term, self.span)
                    .unwrap_or(ArrayLen::Infer),
                None => ArrayLen::Param(id),
            },
            other => self
                .tc
                .array_len_from_term(ConstTerm::from_array_len(other), self.span)
                .unwrap_or(ArrayLen::Infer),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum ArityError {
    TypeArgs { expected: usize, found: usize },
}

#[derive(Debug, Clone, Default, PartialEq)]
pub(crate) struct GenericParams {
    pub(crate) type_params: Vec<TypeParam>,
    pub(crate) const_params: Vec<ConstParam>,
}

impl GenericParams {
    pub(crate) fn is_empty(&self) -> bool {
        self.type_params.is_empty() && self.const_params.is_empty()
    }

    pub(crate) fn type_param_bounds(&self, id: TypeVarId) -> Option<&[ContractRef]> {
        self.type_params
            .iter()
            .find(|param| param.id == id)
            .map(|param| param.bounds.as_slice())
    }

    pub(crate) fn substitutions(&self, args: &GenericArgs) -> (TypeSubst, ConstSubst) {
        let types = self
            .type_params
            .iter()
            .zip(&args.type_args)
            .map(|(param, ty)| (param.id, ty.clone()))
            .collect();
        let consts = self
            .const_params
            .iter()
            .zip(&args.const_args)
            .map(|(param, term)| (param.id, term.clone()))
            .collect();
        (types, consts)
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub(crate) struct GenericArgs {
    pub(crate) type_args: Vec<Type>,
    pub(crate) const_args: Vec<ConstTerm>,
}

impl GenericArgs {
    pub(crate) fn is_empty(&self) -> bool {
        self.type_args.is_empty() && self.const_args.is_empty()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct SpecializationKey {
    pub(crate) target: CallableId,
    pub(crate) args: GenericArgs,
}

#[derive(Clone, Default)]
pub(super) struct GenericOwnerFrame {
    pub(super) params: GenericParams,
    pub(super) args: GenericArgs,
    pub(super) generics: GenericTypeContext,
}

pub(crate) type SpecializedBodyTypes = HashMap<ExprId, (Span, Type)>;

#[derive(Clone, Default)]
pub(crate) struct SpecializedBodyFacts {
    pub(crate) types: SpecializedBodyTypes,
    pub(crate) calls: CallMap,
    pub(crate) extern_uses: ExternUseMap,
    pub(crate) member_paths: MemberPathMap,
    pub(crate) argument_projections: ArgumentProjectionMap,
    pub(crate) contract_witnesses: ContractWitnessMap,
    pub(crate) dyn_conversions: DynConversionMap,
    pub(crate) dyn_weakenings: DynWeakeningMap,
    pub(crate) dyn_calls: DynCallMap,
    pub(crate) dyn_downcasts: DynDowncastMap,
    pub(crate) global_accesses: GlobalAccessMap,
    pub(crate) for_step_runtime_checks: ForStepRuntimeCheckMap,
    pub(crate) closure: TypecheckFacts,
}

#[derive(Clone)]
pub(crate) struct SpecializedBody {
    pub(crate) facts: SpecializedBodyFacts,
    pub(crate) dyn_infer: DynInferenceFacts,
    pub(crate) inferred_ret: Option<Type>,
}

#[derive(Clone)]
pub(crate) enum SpecializationState {
    InProgress,
    Done(Box<SpecializedBody>),
}

pub(super) fn combined_callable_params(callee: &CallableRef) -> GenericParams {
    let mut params = callee.def.sig.owner_generics.clone();
    params
        .type_params
        .extend(callee.def.sig.generics.type_params.clone());
    params
        .const_params
        .extend(callee.def.sig.generics.const_params.clone());
    params
}

fn const_param_bindings(params: &GenericParams, args: &GenericArgs) -> Vec<(Ident, ConstValue)> {
    params
        .const_params
        .iter()
        .zip(&args.const_args)
        .filter_map(|(param, term)| match term {
            ConstTerm::Value(value) => Some((param.name, value.clone())),
            ConstTerm::Name(_)
            | ConstTerm::Param(_)
            | ConstTerm::ArrayInfer
            | ConstTerm::Infer(_) => None,
        })
        .collect()
}

pub(super) fn callable_const_bindings(
    owner_params: &GenericParams,
    owner_args: &GenericArgs,
    callable_params: &GenericParams,
    callable_args: &GenericArgs,
) -> Vec<(Ident, ConstValue)> {
    let mut bindings = const_param_bindings(owner_params, owner_args);
    bindings.extend(const_param_bindings(callable_params, callable_args));
    bindings
}

pub(super) fn specialized_body_facts(
    old: &SpecializedBodyFacts,
    current: &SpecializedBodyFacts,
) -> SpecializedBodyFacts {
    SpecializedBodyFacts {
        types: map_delta(&old.types, &current.types),
        calls: map_delta(&old.calls, &current.calls),
        extern_uses: map_delta(&old.extern_uses, &current.extern_uses),
        member_paths: map_delta(&old.member_paths, &current.member_paths),
        argument_projections: map_delta(&old.argument_projections, &current.argument_projections),
        contract_witnesses: map_delta(&old.contract_witnesses, &current.contract_witnesses),
        dyn_conversions: map_delta(&old.dyn_conversions, &current.dyn_conversions),
        dyn_weakenings: map_delta(&old.dyn_weakenings, &current.dyn_weakenings),
        dyn_calls: map_delta(&old.dyn_calls, &current.dyn_calls),
        dyn_downcasts: map_delta(&old.dyn_downcasts, &current.dyn_downcasts),
        global_accesses: map_delta(&old.global_accesses, &current.global_accesses),
        for_step_runtime_checks: map_delta(
            &old.for_step_runtime_checks,
            &current.for_step_runtime_checks,
        ),
        closure: current.closure.delta_since(&old.closure),
    }
}

pub(super) fn specialization_key(id: CallableId, args: &GenericArgs) -> SpecializationKey {
    SpecializationKey {
        target: id,
        args: args.clone(),
    }
}

pub(super) fn check_with_specialization(
    key: SpecializationKey,
    type_subst: TypeSubst,
    const_subst: ConstSubst,
    owner_frame: GenericOwnerFrame,
    tc: &mut TypeChecker,
    check_body: impl FnOnce(&mut TypeChecker) -> Option<Type>,
) -> Option<Type> {
    tc.solve_constraints();
    let old_facts = tc.specialization_facts();
    let old_dyn_infer = tc.dyn_infer.specialization_snapshot();
    tc.store_specialization(key.clone(), SpecializationState::InProgress);
    tc.push_type_subst(type_subst);
    tc.push_const_subst(const_subst);
    tc.push_generic_context(owner_frame.generics.clone());
    tc.push_generic_owner_frame(owner_frame);
    let inferred_ret = check_body(tc);
    tc.solve_constraints();
    tc.pop_generic_owner_frame();
    tc.pop_generic_context();
    tc.pop_const_subst();
    tc.pop_type_subst();
    tc.store_specialization(
        key,
        SpecializationState::Done(Box::new(SpecializedBody {
            facts: specialized_body_facts(&old_facts, &tc.specialization_facts()),
            dyn_infer: tc.dyn_infer.specialization_delta_since(&old_dyn_infer),
            inferred_ret: inferred_ret.clone(),
        })),
    );
    inferred_ret
}

impl TypeChecker {
    pub(super) fn substitute_checked(
        &mut self,
        ty: &Type,
        types: &TypeSubst,
        consts: &ConstSubst,
        span: Span,
    ) -> Type {
        CheckedSubstituter {
            tc: self,
            span,
            types,
            consts,
        }
        .fold_type(ty)
    }

    pub(super) fn push_generic_context(&mut self, generics: GenericTypeContext) {
        self.generic_contexts.push(generics);
    }

    pub(super) fn pop_generic_context(&mut self) {
        self.generic_contexts.pop();
    }

    pub(super) fn push_generic_owner_frame(&mut self, frame: GenericOwnerFrame) {
        self.generic_owner_frames.push(frame);
    }

    pub(super) fn pop_generic_owner_frame(&mut self) {
        self.generic_owner_frames.pop();
    }

    pub(super) fn visible_generic_owner(&self) -> GenericOwnerFrame {
        self.generic_owner_frames
            .last()
            .cloned()
            .unwrap_or_default()
    }

    pub(super) fn substituted_type_param(&self, name: Ident) -> Option<Type> {
        let id = self
            .generic_contexts
            .iter()
            .rev()
            .find_map(|ctx| ctx.type_param(name))?;
        self.type_substs
            .iter()
            .rev()
            .find_map(|subst| subst.get(&id).cloned())
            .or(Some(Type::Var(id)))
    }

    pub(super) fn store_callable_template(&mut self, id: CallableId, template: CallableTemplate) {
        self.callable_templates.insert(id, template);
    }

    pub(super) fn callable_template(&self, id: &CallableId) -> Option<&CallableTemplate> {
        self.callable_templates.get(id)
    }

    pub(super) fn specialization(&self, key: &SpecializationKey) -> Option<&SpecializationState> {
        self.specializations.get(key)
    }

    pub(super) fn store_specialization(
        &mut self,
        key: SpecializationKey,
        state: SpecializationState,
    ) {
        self.specializations.insert(key, state);
    }

    pub(super) fn closure_fact_snapshot(&self) -> TypecheckFacts {
        self.closure
            .fact_snapshot(|id| self.solver.local_type_to_type(id))
    }

    pub(super) fn specialization_facts(&self) -> SpecializedBodyFacts {
        SpecializedBodyFacts {
            types: self.expr_types(),
            calls: self.calls.clone(),
            extern_uses: self.extern_uses.clone(),
            member_paths: self.member_paths.clone(),
            argument_projections: self.argument_projections.clone(),
            contract_witnesses: self.contract_witnesses.clone(),
            dyn_conversions: self.dyn_conversions.clone(),
            dyn_weakenings: self.dyn_weakenings.clone(),
            dyn_calls: self.dyn_calls.clone(),
            dyn_downcasts: self.dyn_downcasts.clone(),
            global_accesses: self.global_accesses.clone(),
            for_step_runtime_checks: self.for_step_runtime_checks.clone(),
            closure: self.closure_fact_snapshot(),
        }
    }

    pub(super) fn restore_specialization(&mut self, facts: SpecializedBodyFacts) {
        for (id, (span, ty)) in facts.types {
            self.set_type(id, ty, span);
        }
        self.calls.extend(facts.calls);
        self.extern_uses.extend(facts.extern_uses);
        for fact in facts.member_paths.into_values() {
            self.record_member_path(fact);
        }
        for fact in facts.argument_projections.into_values() {
            self.record_argument_projection(fact);
        }
        for fact in facts.contract_witnesses.into_values() {
            self.next_witness_id = self.next_witness_id.max(fact.id.0 + 1);
            self.witness_keys.insert(fact.key.clone(), fact.id);
            self.contract_witnesses.insert(fact.id, fact);
        }
        self.dyn_conversions.extend(facts.dyn_conversions);
        self.dyn_weakenings.extend(facts.dyn_weakenings);
        self.dyn_calls.extend(facts.dyn_calls);
        self.dyn_downcasts.extend(facts.dyn_downcasts);
        self.global_accesses.extend(facts.global_accesses);
        self.for_step_runtime_checks
            .extend(facts.for_step_runtime_checks);
        self.closure.extend_facts(facts.closure);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Specificity {
    MoreSpecific,
    LessSpecific,
    Equal,
    Incomparable,
}

#[derive(Default)]
struct Cover<'a> {
    types: HashMap<TypeVarId, &'a Type>,
    consts: HashMap<ConstParamId, ConstTerm>,
}

pub(crate) fn compare_specificity(a: &Type, b: &Type) -> Specificity {
    let a_subset_b = covers(b, a);
    let b_subset_a = covers(a, b);
    match (a_subset_b, b_subset_a) {
        (true, false) => Specificity::MoreSpecific,
        (false, true) => Specificity::LessSpecific,
        (true, true) => Specificity::Equal,
        (false, false) => Specificity::Incomparable,
    }
}

fn covers(general: &Type, specific: &Type) -> bool {
    covers_type(general, specific, &mut Cover::default())
}

fn covers_type<'a>(general: &Type, specific: &'a Type, cover: &mut Cover<'a>) -> bool {
    match (general, specific) {
        (Type::Var(id), ty) => match cover.types.get(id) {
            Some(bound) => *bound == ty,
            None => cover.types.insert(*id, ty).is_none(),
        },
        (Type::Array { elem, len }, Type::Array { elem: se, len: sl }) => {
            covers_type(elem, se, cover) && covers_len(*len, *sl, cover)
        }
        (
            Type::Func { params, ret },
            Type::Func {
                params: specific_params,
                ret: specific_ret,
            },
        ) => {
            params.len() == specific_params.len()
                && params
                    .iter()
                    .zip(specific_params)
                    .all(|(a, b)| covers_type(&a.ty, &b.ty, cover))
                && ret.access == specific_ret.access
                && covers_type(&ret.ty, &specific_ret.ty, cover)
        }
        (Type::Tuple(a), Type::Tuple(b)) => covers_types(a, b, cover),
        (Type::Nominal(a), Type::Nominal(b)) => {
            a.kind == b.kind
                && a.name == b.name
                && a.origin == b.origin
                && covers_types(&a.type_args, &b.type_args, cover)
                && covers_const_args(&a.const_args, &b.const_args, cover)
        }
        (
            Type::UnresolvedNominal {
                qualifier,
                name,
                generic_args,
            },
            Type::UnresolvedNominal {
                qualifier: specific_qualifier,
                name: specific_name,
                generic_args: specific_args,
            },
        ) => {
            qualifier == specific_qualifier
                && name == specific_name
                && covers_generic_args(generic_args, specific_args, cover)
        }
        (
            Type::List { elem },
            Type::List {
                elem: specific_elem,
            },
        )
        | (
            Type::Slice { elem },
            Type::Slice {
                elem: specific_elem,
            },
        ) => covers_type(elem, specific_elem, cover),
        (
            Type::Map { key, value },
            Type::Map {
                key: specific_key,
                value: specific_value,
            },
        ) => covers_type(key, specific_key, cover) && covers_type(value, specific_value, cover),
        _ => general == specific,
    }
}

fn covers_types<'a>(general: &[Type], specific: &'a [Type], cover: &mut Cover<'a>) -> bool {
    general.len() == specific.len()
        && general
            .iter()
            .zip(specific)
            .all(|(general, specific)| covers_type(general, specific, cover))
}

fn covers_generic_args<'a>(
    general: &[GenericArg],
    specific: &'a [GenericArg],
    cover: &mut Cover<'a>,
) -> bool {
    general.len() == specific.len()
        && general
            .iter()
            .zip(specific)
            .all(|(general, specific)| match (general, specific) {
                (GenericArg::Type(general), GenericArg::Type(specific)) => {
                    covers_type(general, specific, cover)
                }
                (GenericArg::Const(general), GenericArg::Const(specific)) => {
                    covers_const_arg(general, specific, cover)
                }
                _ => false,
            })
}

fn covers_len(general: ArrayLen, specific: ArrayLen, cover: &mut Cover<'_>) -> bool {
    match general {
        ArrayLen::Param(id) => cover_const(id, const_term_len(specific), cover),
        _ => const_term_len(general) == const_term_len(specific),
    }
}

fn covers_const_args<'a>(
    general: &[ConstArg],
    specific: &'a [ConstArg],
    cover: &mut Cover<'a>,
) -> bool {
    general.len() == specific.len()
        && general
            .iter()
            .zip(specific)
            .all(|(general, specific)| covers_const_arg(general, specific, cover))
}

fn covers_const_arg(general: &ConstArg, specific: &ConstArg, cover: &mut Cover<'_>) -> bool {
    match general {
        ConstArg::Param(id) => cover_const(*id, const_term_arg(specific), cover),
        _ => const_term_arg(general) == const_term_arg(specific),
    }
}

fn cover_const(id: ConstParamId, term: ConstTerm, cover: &mut Cover<'_>) -> bool {
    match cover.consts.get(&id) {
        Some(bound) => *bound == term,
        None => cover.consts.insert(id, term).is_none(),
    }
}

fn const_term_len(len: ArrayLen) -> ConstTerm {
    ConstTerm::from_array_len(len)
}

fn const_term_arg(arg: &ConstArg) -> ConstTerm {
    ConstTerm::from_arg(arg)
}

struct Substituter<'a> {
    types: &'a TypeSubst,
    consts: &'a ConstSubst,
}

impl TypeFolder for Substituter<'_> {
    fn fold_var(&mut self, id: TypeVarId) -> Type {
        self.types.get(&id).cloned().unwrap_or(Type::Var(id))
    }

    fn fold_const_arg(&mut self, arg: &ConstArg) -> ConstArg {
        match arg {
            ConstArg::Param(id) => self
                .consts
                .get(id)
                .and_then(ConstTerm::to_arg_no_infer)
                .unwrap_or_else(|| arg.clone()),
            ConstArg::Value(_) | ConstArg::Name(_) => arg.clone(),
        }
    }

    fn fold_array_len(&mut self, len: ArrayLen) -> ArrayLen {
        match len {
            ArrayLen::Param(id) => self
                .consts
                .get(&id)
                .and_then(ConstTerm::to_array_len_no_infer)
                .unwrap_or(ArrayLen::Param(id)),
            other => other,
        }
    }
}

pub(crate) fn substitute(ty: &Type, ts: &TypeSubst, cs: &ConstSubst) -> Type {
    Substituter {
        types: ts,
        consts: cs,
    }
    .fold_type(ty)
}

pub(crate) fn same_extend_target(
    a: &Type,
    a_generics: &GenericParams,
    b: &Type,
    b_generics: &GenericParams,
) -> bool {
    let Some(a_template) = try_generic_template_type(a, a_generics) else {
        return false;
    };
    let Some(b_template) = try_generic_template_type(b, b_generics) else {
        return false;
    };
    match_generic_template_args(a_generics, &a_template, &b_template).is_some()
        && match_generic_template_args(b_generics, &b_template, &a_template).is_some()
}

struct GenericTemplate {
    generics: GenericTypeContext,
}

impl TypeFolder for GenericTemplate {
    fn fold_unresolved_name(&mut self, name: Ident) -> Type {
        self.generics
            .type_param(name)
            .map_or(Type::UnresolvedName(name), Type::Var)
    }

    fn fold_unresolved_nominal(
        &mut self,
        qualifier: Option<Ident>,
        name: Ident,
        generic_args: &[GenericArg],
    ) -> Type {
        if qualifier.is_none()
            && generic_args.is_empty()
            && let Some(id) = self.generics.type_param(name)
        {
            return Type::Var(id);
        }
        self.fold_unresolved_nominal_default(qualifier, name, generic_args)
    }

    fn fold_const_arg(&mut self, arg: &ConstArg) -> ConstArg {
        match arg {
            ConstArg::Name(name) => self
                .generics
                .const_param(*name)
                .map_or_else(|| arg.clone(), ConstArg::Param),
            ConstArg::Value(_) | ConstArg::Param(_) => arg.clone(),
        }
    }

    fn fold_array_len(&mut self, len: ArrayLen) -> ArrayLen {
        match len {
            ArrayLen::Named(name) => self
                .generics
                .const_param(name)
                .map_or(ArrayLen::Named(name), ArrayLen::Param),
            other => other,
        }
    }
}

type GenericTemplateMatch = Result<GenericArgs, Vec<Ident>>;

pub(crate) fn match_generic_template_args(
    generics: &GenericParams,
    template: &Type,
    concrete: &Type,
) -> Option<GenericTemplateMatch> {
    match_cast_conversion(generics, template, concrete, template, concrete)
}

pub(crate) fn match_cast_conversion(
    generics: &GenericParams,
    source_template: &Type,
    source: &Type,
    target_template: &Type,
    target: &Type,
) -> Option<GenericTemplateMatch> {
    let span = None;
    if generics.is_empty() {
        return (source_template == source && target_template == target)
            .then(|| Ok(GenericArgs::default()));
    }

    let mut solver = Solver::default();
    let seeds = GenericSolverSeeds::default();
    let vars = solver.generic_solver_vars(generics, &seeds, span);
    let source_template = solver.instantiate_generic_type(source_template, &vars);
    let target_template = solver.instantiate_generic_type(target_template, &vars);
    let source = solver.concrete_type(source);
    let target = solver.concrete_type(target);
    solver.add_handle_equal(span, source_template, source);
    solver.add_handle_equal(span, target_template, target);
    if !solver.solve_pending().is_empty() {
        return None;
    }

    Some(solver.finalize_generic_args(generics, &vars))
}

fn try_generic_template_type(ty: &Type, generics: &GenericParams) -> Option<Type> {
    let generics =
        GenericTypeContext::try_from_params(&generics.type_params, &generics.const_params).ok()?;
    Some(GenericTemplate { generics }.fold_type(ty))
}

pub(crate) fn generic_template_type(ty: &Type, generics: &GenericParams) -> Type {
    GenericTemplate {
        generics: GenericTypeContext::try_from_params(
            &generics.type_params,
            &generics.const_params,
        )
        .expect("generic_template_type requires validated generic params"),
    }
    .fold_type(ty)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{ConstValue, EscapeMode, FuncParam, Ident, NominalKind};

    fn ident(name: &str) -> Ident {
        Ident::new(name)
    }

    fn tv(id: u32) -> TypeVarId {
        TypeVarId(id)
    }

    fn cp(id: u32) -> ConstParamId {
        ConstParamId(id)
    }

    fn array_ty(elem: Type, len: ArrayLen) -> Type {
        Type::Array {
            elem: Box::new(elem),
            len,
        }
    }

    fn carg(n: i64) -> ConstArg {
        ConstArg::Value(ConstValue::Int(n))
    }

    fn cterm(n: usize) -> ConstTerm {
        ConstTerm::from_usize(n)
    }

    fn nominal(
        kind: NominalKind,
        name: &str,
        type_args: Vec<Type>,
        const_args: Vec<ConstArg>,
    ) -> Type {
        Type::nominal(kind, Ident::new(name), type_args, const_args, None)
    }

    fn struct_ty(name: &str, type_args: Vec<Type>) -> Type {
        nominal(NominalKind::Struct, name, type_args, vec![])
    }

    fn struct_const(name: &str, type_args: Vec<Type>, const_args: Vec<ConstArg>) -> Type {
        nominal(NominalKind::Struct, name, type_args, const_args)
    }

    #[test]
    fn substitute_type_var() {
        let ts = HashMap::from([(tv(0), Type::Int)]);
        let result = substitute(&Type::Var(tv(0)), &ts, &HashMap::new());
        assert_eq!(result, Type::Int);
    }

    #[test]
    fn substitute_type_var_unbound() {
        let result = substitute(&Type::Var(tv(99)), &HashMap::new(), &HashMap::new());
        assert_eq!(result, Type::Var(tv(99)));
    }

    #[test]
    fn array_const_param() {
        let cs = HashMap::from([(cp(0), cterm(4))]);
        let ty = array_ty(Type::Int, ArrayLen::Param(cp(0)));
        let result = substitute(&ty, &HashMap::new(), &cs);
        assert_eq!(result, array_ty(Type::Int, ArrayLen::Fixed(4)));
    }

    #[test]
    fn type_and_const() {
        let ts = HashMap::from([(tv(0), Type::String)]);
        let cs = HashMap::from([(cp(1), cterm(3))]);
        let ty = array_ty(Type::Var(tv(0)), ArrayLen::Param(cp(1)));
        let result = substitute(&ty, &ts, &cs);
        assert_eq!(result, array_ty(Type::String, ArrayLen::Fixed(3)));
    }

    #[test]
    fn substitute_nominal_const_param_to_bool() {
        let cs = HashMap::from([(cp(0), ConstTerm::Value(ConstValue::Bool(true)))]);
        let ty = struct_const("Flag", vec![], vec![ConstArg::Param(cp(0))]);
        let result = substitute(&ty, &HashMap::new(), &cs);
        assert_eq!(
            result,
            struct_const(
                "Flag",
                vec![],
                vec![ConstArg::Value(ConstValue::Bool(true))],
            ),
        );
    }

    #[test]
    fn substitute_array_const_param_to_name() {
        let name = Ident::new("N");
        let cs = HashMap::from([(cp(0), ConstTerm::Name(name))]);
        let ty = array_ty(Type::Int, ArrayLen::Param(cp(0)));
        let result = substitute(&ty, &HashMap::new(), &cs);
        assert_eq!(result, array_ty(Type::Int, ArrayLen::Named(name)));
    }

    #[test]
    fn substitute_array_const_param_rejects_non_int_term_without_infer() {
        let cs = HashMap::from([(cp(0), ConstTerm::Value(ConstValue::Bool(true)))]);
        let ty = array_ty(Type::Int, ArrayLen::Param(cp(0)));
        let result = substitute(&ty, &HashMap::new(), &cs);
        assert_eq!(result, array_ty(Type::Int, ArrayLen::Param(cp(0))));
    }

    #[test]
    fn substitute_func_type() {
        let ts = HashMap::from([(tv(0), Type::Int), (tv(1), Type::Bool)]);
        let ty = Type::Func {
            params: vec![FuncParam::new(
                Type::Var(tv(0)),
                false,
                false,
                EscapeMode::NonEscaping,
            )],
            ret: Box::new(crate::ast::ReturnSpec::value(Type::Var(tv(1)))),
        };
        let result = substitute(&ty, &ts, &HashMap::new());
        assert_eq!(
            result,
            Type::Func {
                params: vec![FuncParam::new(
                    Type::Int,
                    false,
                    false,
                    EscapeMode::NonEscaping
                )],
                ret: Box::new(crate::ast::ReturnSpec::value(Type::Bool)),
            }
        );
    }

    #[test]
    fn substitute_nested_struct() {
        let ts = HashMap::from([(tv(0), Type::Int)]);
        let ty = struct_ty(
            "Wrapper",
            vec![Type::Tuple(vec![Type::Var(tv(0)), Type::String])],
        );
        let result = substitute(&ty, &ts, &HashMap::new());
        assert_eq!(
            result,
            struct_ty("Wrapper", vec![Type::Tuple(vec![Type::Int, Type::String])])
        );
    }

    #[test]
    fn substitute_unresolved_nominal_args() {
        let ts = HashMap::from([(tv(0), Type::Int)]);
        let cs = HashMap::from([(cp(1), cterm(4))]);
        let ty = Type::UnresolvedNominal {
            qualifier: None,
            name: Ident::new("Box"),
            generic_args: vec![
                GenericArg::Type(Type::Var(tv(0))),
                GenericArg::Const(ConstArg::Param(cp(1))),
            ],
        };
        let result = substitute(&ty, &ts, &cs);

        assert_eq!(
            result,
            Type::UnresolvedNominal {
                qualifier: None,
                name: Ident::new("Box"),
                generic_args: vec![GenericArg::Type(Type::Int), GenericArg::Const(carg(4)),],
            }
        );
    }

    #[test]
    fn primitives_unchanged() {
        for ty in [Type::Int, Type::Float, Type::Bool, Type::String, Type::Void] {
            assert_eq!(substitute(&ty, &HashMap::new(), &HashMap::new()), ty);
        }
    }

    #[test]
    fn spec_exact() {
        assert_eq!(
            compare_specificity(&Type::Int, &Type::Var(tv(0))),
            Specificity::MoreSpecific
        );
        assert_eq!(
            compare_specificity(&Type::Var(tv(0)), &Type::Int),
            Specificity::LessSpecific
        );
    }

    #[test]
    fn spec_equal() {
        assert_eq!(
            compare_specificity(&Type::Var(tv(0)), &Type::Var(tv(1))),
            Specificity::Equal
        );
        assert_eq!(
            compare_specificity(&Type::Int, &Type::Int),
            Specificity::Equal
        );
    }

    #[test]
    fn spec_const() {
        let exact = array_ty(Type::Var(tv(0)), ArrayLen::Fixed(3));
        let generic = array_ty(Type::Var(tv(0)), ArrayLen::Param(cp(0)));
        assert_eq!(
            compare_specificity(&exact, &generic),
            Specificity::MoreSpecific
        );
    }

    #[test]
    fn spec_nested() {
        let nested = array_ty(
            array_ty(Type::Var(tv(0)), ArrayLen::Param(cp(0))),
            ArrayLen::Param(cp(1)),
        );
        let flat = array_ty(Type::Var(tv(2)), ArrayLen::Param(cp(1)));
        assert_eq!(
            compare_specificity(&nested, &flat),
            Specificity::MoreSpecific
        );
    }

    #[test]
    fn spec_repeat() {
        let repeat = Type::Tuple(vec![Type::Var(tv(0)), Type::Var(tv(0))]);
        let loose = Type::Tuple(vec![Type::Var(tv(1)), Type::Var(tv(2))]);
        assert_eq!(
            compare_specificity(&repeat, &loose),
            Specificity::MoreSpecific
        );
    }

    #[test]
    fn spec_bool_const_arg_exact() {
        let a = struct_const(
            "Flag",
            vec![],
            vec![ConstArg::Value(ConstValue::Bool(true))],
        );
        let b = struct_const(
            "Flag",
            vec![],
            vec![ConstArg::Value(ConstValue::Bool(true))],
        );
        assert_eq!(compare_specificity(&a, &b), Specificity::Equal);
    }

    #[test]
    fn spec_repeated_const_across_nominal_and_array_requires_equal_terms() {
        let repeated = Type::Tuple(vec![
            struct_const("Buf", vec![], vec![ConstArg::Param(cp(0))]),
            array_ty(Type::Int, ArrayLen::Param(cp(0))),
        ]);
        let same = Type::Tuple(vec![
            struct_const("Buf", vec![], vec![carg(3)]),
            array_ty(Type::Int, ArrayLen::Fixed(3)),
        ]);
        let different = Type::Tuple(vec![
            struct_const("Buf", vec![], vec![ConstArg::Value(ConstValue::Bool(true))]),
            array_ty(Type::Int, ArrayLen::Fixed(1)),
        ]);
        assert_eq!(
            compare_specificity(&same, &repeated),
            Specificity::MoreSpecific
        );
        assert_eq!(
            compare_specificity(&different, &repeated),
            Specificity::Incomparable
        );
    }

    #[test]
    fn spec_negative_const_arg_not_array_len() {
        let repeated = Type::Tuple(vec![
            struct_const("Buf", vec![], vec![ConstArg::Param(cp(0))]),
            array_ty(Type::Int, ArrayLen::Param(cp(0))),
        ]);
        let negative = Type::Tuple(vec![
            struct_const("Buf", vec![], vec![carg(-1)]),
            array_ty(Type::Int, ArrayLen::Fixed(0)),
        ]);
        assert_eq!(
            compare_specificity(&negative, &repeated),
            Specificity::Incomparable
        );
    }

    #[test]
    fn spec_ambig() {
        let cap = struct_const("FixedBuf", vec![Type::Var(tv(0))], vec![carg(5)]);
        let ints = struct_const("FixedBuf", vec![Type::Int], vec![ConstArg::Param(cp(0))]);
        assert_eq!(compare_specificity(&cap, &ints), Specificity::Incomparable);
    }
    #[test]
    fn generic_template_nominal_args() {
        let generics = GenericParams {
            type_params: vec![TypeParam {
                name: ident("T"),
                id: TypeVarId(0),
                bounds: vec![],
            }],
            const_params: vec![],
        };
        let ty = Type::UnresolvedNominal {
            qualifier: None,
            name: ident("Foo"),
            generic_args: vec![GenericArg::Type(Type::UnresolvedName(ident("T")))],
        };
        let result = generic_template_type(&ty, &generics);

        assert_eq!(
            result,
            Type::UnresolvedNominal {
                qualifier: None,
                name: ident("Foo"),
                generic_args: vec![GenericArg::Type(Type::Var(TypeVarId(0)))],
            }
        );
    }
}
