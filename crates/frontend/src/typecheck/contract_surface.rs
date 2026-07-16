use std::collections::HashMap;

use super::{
    ContractSetKey, MethodReceiver, ModuleScope,
    contracts::contract_set_key_for_ref,
    decls::DeclarationIndex,
    dyn_infer::DynInference,
    semantic_use::{SemanticDeclarations, SemanticFactMaps},
};
use crate::ast::{
    AnonymousContract, AnonymousContractParam, AnonymousContractRequirement, ArrayLen, ConstArg,
    ConstExpr, ConstValue, ContractRef, DynContractHoleId, EscapeMode, FuncParam, Ident,
    ReturnKind, ReturnSpec, Type, TypeFolder, TypeVisitor,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct ContractSurfaceId(pub(crate) u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct ContractSlotId(pub(crate) u32);

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum ContractWeakening {
    Identity,
    Projection {
        source: ContractSurfaceId,
        target: ContractSurfaceId,
        target_to_source: Vec<ContractSlotId>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct ContractSurfaceSchema {
    pub(crate) id: ContractSurfaceId,
    pub(crate) display_name: String,
    pub(crate) slots: Vec<ContractSlotSchema>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct ContractSlotSchema {
    pub(crate) id: ContractSlotId,
    pub(crate) name: Ident,
    pub(crate) receiver: MethodReceiver,
    pub(crate) params: Vec<ContractParamSchema>,
    pub(crate) ret: ContractReturnSchema,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct ContractParamSchema {
    pub(crate) ty: ContractTypeSchema,
    pub(crate) mutable: bool,
    pub(crate) cast_accept: bool,
    pub(crate) escape: EscapeMode,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) enum ContractReturnSchema {
    Value(ContractTypeSchema),
    Place(ContractTypeSchema),
    Iter,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) enum ContractTypeSchema {
    Any,
    Int,
    Float,
    Bool,
    String,
    Char,
    Void,
    Func {
        params: Vec<ContractParamSchema>,
        ret: Box<ContractReturnSchema>,
    },
    Dyn(ContractSurfaceId),
    Tuple(Vec<ContractTypeSchema>),
    Nominal {
        id: crate::semantic_id::NominalId,
        type_args: Vec<ContractTypeSchema>,
        const_args: Vec<ConstArg>,
    },
    List(Box<ContractTypeSchema>),
    Array {
        elem: Box<ContractTypeSchema>,
        len: ArrayLen,
    },
    Map {
        key: Box<ContractTypeSchema>,
        value: Box<ContractTypeSchema>,
    },
    Slice(Box<ContractTypeSchema>),
    Optional(Box<ContractTypeSchema>),
}

#[derive(Debug, Clone, Default)]
pub(crate) struct ContractSurfaceSchemas {
    surfaces: Vec<ContractSurfaceSchema>,
    by_contract_set: HashMap<ContractSetKey, ContractSurfaceId>,
    by_hole: HashMap<DynContractHoleId, ContractSurfaceId>,
}

impl ContractSurfaceSchemas {
    pub(super) fn build(
        decls: &DeclarationIndex,
        dyn_infer: &DynInference,
        facts: &SemanticFactMaps,
        declarations: &SemanticDeclarations,
    ) -> Self {
        SurfaceBuilder::new(decls, dyn_infer).build(facts, declarations)
    }

    pub(crate) fn iter(&self) -> impl Iterator<Item = &ContractSurfaceSchema> {
        self.surfaces.iter()
    }

    pub(crate) fn surface(&self, id: ContractSurfaceId) -> Option<&ContractSurfaceSchema> {
        self.surfaces.get(id.0 as usize)
    }

    pub(crate) fn id_for_set(&self, key: &ContractSetKey) -> Option<ContractSurfaceId> {
        self.by_contract_set.get(key).copied()
    }

    pub(crate) fn representative_ref(&self, id: ContractSurfaceId) -> Option<ContractRef> {
        let key = self
            .by_contract_set
            .iter()
            .find_map(|(key, found)| (*found == id).then_some(key))?;
        Some(ContractRef::Anonymous(AnonymousContract {
            requirements: key
                .requirements
                .iter()
                .map(|requirement| AnonymousContractRequirement {
                    receiver: requirement.receiver.expect("contract slot has a receiver"),
                    name: requirement.name,
                    params: requirement
                        .params
                        .iter()
                        .enumerate()
                        .map(|(index, param)| AnonymousContractParam {
                            mutable: param.mutable,
                            escape: param.escape,
                            name: Ident::new(format!("p{index}")),
                            ty: param.ty.clone(),
                        })
                        .collect(),
                    ret: requirement.ret.clone(),
                })
                .collect(),
        }))
    }

    pub(crate) fn resolve_weakening(
        &self,
        source: &ContractSetKey,
        target: &ContractSetKey,
        target_to_source: &[usize],
    ) -> Option<ContractWeakening> {
        let source = self.id_for_set(source)?;
        let target = self.id_for_set(target)?;
        if source == target {
            return Some(ContractWeakening::Identity);
        }
        let source_schema = self.surface(source)?;
        let target_schema = self.surface(target)?;
        if target_schema.slots.len() != target_to_source.len() {
            return None;
        }
        let target_to_source = target_to_source
            .iter()
            .enumerate()
            .map(|(target_slot, &source_slot)| {
                let source = source_schema.slots.get(source_slot)?;
                let target = &target_schema.slots[target_slot];
                same_slot_signature(source, target).then_some(source.id)
            })
            .collect::<Option<Vec<_>>>()?;
        Some(ContractWeakening::Projection {
            source,
            target,
            target_to_source,
        })
    }

    pub(crate) fn call_target(
        &self,
        contract: &ContractSetKey,
        method: Ident,
    ) -> Option<(ContractSurfaceId, ContractSlotId)> {
        let surface = self.id_for_set(contract)?;
        let slot = self
            .surface(surface)?
            .slots
            .iter()
            .find(|slot| slot.name == method)?
            .id;
        Some((surface, slot))
    }

    pub(crate) fn id_for_ref(
        &self,
        decls: &DeclarationIndex,
        module: &ModuleScope,
        contract: &ContractRef,
    ) -> Option<ContractSurfaceId> {
        match contract {
            ContractRef::Hole(id) => self.by_hole.get(id).copied(),
            ContractRef::Infer => None,
            _ => contract_set_key_for_ref(decls, module, contract)
                .and_then(|key| self.id_for_set(&key)),
        }
    }

    pub(crate) fn validate(&self, decls: &DeclarationIndex) {
        for (index, surface) in self.iter().enumerate() {
            debug_assert_eq!(surface.id, ContractSurfaceId(index as u32));
            debug_assert_eq!(self.surface(surface.id), Some(surface));
            debug_assert!(!surface.display_name.is_empty());
            for (slot, schema) in surface.slots.iter().enumerate() {
                debug_assert_eq!(schema.id, ContractSlotId(slot as u32));
                debug_assert!(!schema.name.as_str().is_empty());
                debug_assert!(matches!(
                    schema.receiver,
                    MethodReceiver::Value | MethodReceiver::Ref
                ));
                for param in &schema.params {
                    param.ty.validate(self);
                    let _ = (param.mutable, param.cast_accept, param.escape);
                }
                schema.ret.validate(self);
            }
        }
        for (key, id) in &self.by_contract_set {
            debug_assert_eq!(self.id_for_set(key), Some(*id));
            for slot in &self.surface(*id).expect("surface exists").slots {
                debug_assert_eq!(self.call_target(key, slot.name), Some((*id, slot.id)));
            }
            debug_assert!(self.surface(*id).is_some());
        }
        for (hole, id) in &self.by_hole {
            debug_assert_eq!(
                self.id_for_ref(decls, &ModuleScope::Root, &ContractRef::Hole(*hole)),
                Some(*id)
            );
            debug_assert!(self.surface(*id).is_some());
        }
    }
}

impl ContractReturnSchema {
    fn validate(&self, surfaces: &ContractSurfaceSchemas) {
        match self {
            Self::Value(ty) | Self::Place(ty) => ty.validate(surfaces),
            Self::Iter => {}
        }
    }
}

impl ContractTypeSchema {
    fn validate(&self, surfaces: &ContractSurfaceSchemas) {
        match self {
            Self::Func { params, ret } => {
                for param in params {
                    param.ty.validate(surfaces);
                }
                ret.validate(surfaces);
            }
            Self::Dyn(id) => debug_assert!(surfaces.surface(*id).is_some()),
            Self::Tuple(types) => {
                for ty in types {
                    ty.validate(surfaces);
                }
            }
            Self::Nominal { type_args, .. } => {
                for ty in type_args {
                    ty.validate(surfaces);
                }
            }
            Self::List(elem)
            | Self::Slice(elem)
            | Self::Optional(elem)
            | Self::Array { elem, .. } => elem.validate(surfaces),
            Self::Map { key, value } => {
                key.validate(surfaces);
                value.validate(surfaces);
            }
            Self::Any
            | Self::Int
            | Self::Float
            | Self::Bool
            | Self::String
            | Self::Char
            | Self::Void => {}
        }
    }
}

fn same_slot_signature(left: &ContractSlotSchema, right: &ContractSlotSchema) -> bool {
    left.name == right.name
        && left.receiver == right.receiver
        && left.params == right.params
        && left.ret == right.ret
}
pub(crate) fn canonical_projection(
    decls: &DeclarationIndex,
    source: &ContractSetKey,
    target: &ContractSetKey,
) -> Option<Vec<usize>> {
    let dyn_infer = DynInference::default();
    let mut builder = SurfaceBuilder::new(decls, &dyn_infer);
    builder.intern(source.clone(), None);
    builder.intern(target.clone(), None);
    builder.close_nested_surfaces();
    let surfaces = builder.finish();
    let source = surfaces.surface(surfaces.id_for_set(source)?)?;
    let target = surfaces.surface(surfaces.id_for_set(target)?)?;
    target
        .slots
        .iter()
        .map(|target| {
            source
                .slots
                .iter()
                .position(|source| same_slot_signature(source, target))
        })
        .collect()
}

fn collect_type_refs(ty: &Type) -> Vec<ContractRef> {
    #[derive(Default)]
    struct Collector {
        refs: Vec<ContractRef>,
    }

    impl TypeVisitor for Collector {
        fn visit_type_leaf(&mut self, ty: &Type) -> bool {
            let Type::Dyn(contract) = ty else {
                return false;
            };
            if !matches!(contract, ContractRef::Infer) {
                self.refs.push(contract.clone());
            }
            false
        }
    }

    let mut collector = Collector::default();
    collector.visit_type(ty);
    collector.refs
}

struct RawSurface {
    key: ContractSetKey,
    displays: Vec<String>,
}

struct SurfaceBuilder<'a> {
    decls: &'a DeclarationIndex,
    dyn_infer: &'a DynInference,
    raw: Vec<RawSurface>,
    raw_index: HashMap<ContractSetKey, usize>,
    raw_holes: HashMap<DynContractHoleId, usize>,
}

impl<'a> SurfaceBuilder<'a> {
    fn new(decls: &'a DeclarationIndex, dyn_infer: &'a DynInference) -> Self {
        Self {
            decls,
            dyn_infer,
            raw: vec![],
            raw_index: HashMap::new(),
            raw_holes: HashMap::new(),
        }
    }

    fn build(
        mut self,
        facts: &SemanticFactMaps,
        declarations: &SemanticDeclarations,
    ) -> ContractSurfaceSchemas {
        let mut contracts = self.decls.contracts().collect::<Vec<_>>();
        contracts.sort_by(|left, right| {
            left.key
                .module
                .to_string()
                .cmp(&right.key.module.to_string())
                .then_with(|| left.key.name.as_str().cmp(right.key.name.as_str()))
        });
        for contract in contracts {
            if let Some(key) = &contract.contract_set {
                self.intern(key.clone(), Some(contract.key.name.to_string()));
            }
        }

        let mut solutions = self.dyn_infer.solutions().collect::<Vec<_>>();
        solutions.sort_by_key(|(id, _)| id.0);
        for (id, _) in solutions {
            self.intern_ref(
                &ModuleScope::Root,
                &ContractRef::Hole(id),
                Some("dyn _".to_string()),
            );
        }

        self.collect_decl_roots();
        self.collect_fact_roots(facts, declarations);
        self.close_nested_surfaces();
        self.finish()
    }

    fn collect_decl_roots(&mut self) {
        let mut decls = self.decls.clone();
        let errors = decls.fold_canonical_type_uses(|site, ty| {
            self.intern_type(&site.module, &ty);
            ty
        });
        debug_assert!(errors.is_empty());
    }

    fn collect_fact_roots(
        &mut self,
        facts: &SemanticFactMaps,
        declarations: &SemanticDeclarations,
    ) {
        for witness in facts.contract_witnesses.values() {
            self.intern(witness.key.contract.clone(), None);
        }
        for body in facts.bodies.values() {
            for weakening in body.pending_dyn_weakenings.values() {
                self.intern(weakening.source.clone(), None);
                self.intern(weakening.target.clone(), None);
            }
            for call in body.pending_dyn_calls.values() {
                self.intern(call.contract.clone(), None);
            }
            for fact in body.expr_types.values() {
                if let Some(ty) = &fact.ty {
                    self.intern_type(&ModuleScope::Root, ty);
                }
            }
            for local in body.locals.defs.values() {
                self.intern_type(&ModuleScope::Root, &local.ty);
            }
        }
        for function in &declarations.functions {
            for ty in &function.args.type_args {
                self.intern_type(&function.module, ty);
            }
            for param in &function.params {
                self.intern_type(&function.module, &param.ty);
            }
            self.intern_type(&function.module, &function.ret.ty());
        }
    }

    fn close_nested_surfaces(&mut self) {
        let mut index = 0;
        while index < self.raw.len() {
            let key = self.raw[index].key.clone();
            for requirement in key.requirements {
                for param in requirement.params {
                    self.intern_type(&ModuleScope::Root, &param.ty);
                }
                self.intern_type(&ModuleScope::Root, &requirement.ret.ty());
            }
            index += 1;
        }
    }

    fn intern_type(&mut self, module: &ModuleScope, ty: &Type) {
        for contract in collect_type_refs(ty) {
            self.intern_ref(module, &contract, Some(contract.to_string()));
        }
    }

    fn intern_ref(
        &mut self,
        module: &ModuleScope,
        contract: &ContractRef,
        display: Option<String>,
    ) -> usize {
        let key = self
            .key_for_ref(module, contract)
            .expect("finished dynamic surface must resolve");
        let index = self.intern(key, display);
        if let ContractRef::Hole(id) = contract {
            self.raw_holes.insert(*id, index);
        }
        index
    }

    fn key_for_ref(&self, module: &ModuleScope, contract: &ContractRef) -> Option<ContractSetKey> {
        let contract = match contract {
            ContractRef::Hole(id) => self.dyn_infer.solution(*id)?,
            ContractRef::Infer => return None,
            contract => contract.clone(),
        };
        contract_set_key_for_ref(self.decls, module, &contract)
    }

    fn intern(&mut self, key: ContractSetKey, display: Option<String>) -> usize {
        if let Some(&index) = self.raw_index.get(&key) {
            if let Some(display) = display
                && !self.raw[index].displays.contains(&display)
            {
                self.raw[index].displays.push(display);
            }
            return index;
        }
        let index = self.raw.len();
        let displays = display.into_iter().collect();
        self.raw.push(RawSurface {
            key: key.clone(),
            displays,
        });
        self.raw_index.insert(key, index);
        index
    }

    fn finish(self) -> ContractSurfaceSchemas {
        if self.raw.is_empty() {
            return ContractSurfaceSchemas::default();
        }

        let colors = self.refine();
        let signatures = self.normalized_signatures(&colors);
        let mut groups = group_signatures(signatures, None);
        groups.sort_by(|left, right| left.order.cmp(&right.order));

        let mut by_contract_set = HashMap::new();
        let mut raw_ids = vec![ContractSurfaceId(0); self.raw.len()];
        for (surface, group) in groups.iter().enumerate() {
            let id = ContractSurfaceId(surface as u32);
            for &member in &group.members {
                raw_ids[member] = id;
                by_contract_set.insert(self.raw[member].key.clone(), id);
            }
        }
        let by_hole = self
            .raw_holes
            .iter()
            .map(|(hole, raw)| (*hole, raw_ids[*raw]))
            .collect();

        let surfaces = groups
            .into_iter()
            .enumerate()
            .map(|(surface, group)| {
                let id = ContractSurfaceId(surface as u32);
                let representative = group
                    .members
                    .iter()
                    .copied()
                    .min_by_key(|&index| raw_label(&self.raw[index]))
                    .expect("surface group is non-empty");
                let raw = &self.raw[representative];
                let slots = raw
                    .key
                    .requirements
                    .iter()
                    .enumerate()
                    .map(|(slot, requirement)| {
                        debug_assert_eq!(requirement.required_params, requirement.params.len());
                        ContractSlotSchema {
                            id: ContractSlotId(slot as u32),
                            name: requirement.name,
                            receiver: requirement
                                .receiver
                                .expect("finalized contract slot has a receiver"),
                            params: requirement
                                .params
                                .iter()
                                .map(|param| self.param_schema(param, &raw_ids))
                                .collect(),
                            ret: self.return_schema(&requirement.ret, &raw_ids),
                        }
                    })
                    .collect();
                ContractSurfaceSchema {
                    id,
                    display_name: raw_label(raw),
                    slots,
                }
            })
            .collect();

        ContractSurfaceSchemas {
            surfaces,
            by_contract_set,
            by_hole,
        }
    }

    fn param_schema(
        &self,
        param: &FuncParam,
        raw_ids: &[ContractSurfaceId],
    ) -> ContractParamSchema {
        ContractParamSchema {
            ty: self.type_schema(&param.ty, raw_ids),
            mutable: param.mutable,
            cast_accept: param.cast_accept,
            escape: param.escape,
        }
    }

    fn return_schema(
        &self,
        ret: &ReturnSpec,
        raw_ids: &[ContractSurfaceId],
    ) -> ContractReturnSchema {
        match &ret.kind {
            ReturnKind::Value(ty) => ContractReturnSchema::Value(self.type_schema(ty, raw_ids)),
            ReturnKind::Place(ty) => ContractReturnSchema::Place(self.type_schema(ty, raw_ids)),
            ReturnKind::Iter => ContractReturnSchema::Iter,
            ReturnKind::Infer => panic!("unfinished return in canonical contract surface"),
        }
    }

    fn type_schema(&self, ty: &Type, raw_ids: &[ContractSurfaceId]) -> ContractTypeSchema {
        match ty {
            Type::Any => ContractTypeSchema::Any,
            Type::Int => ContractTypeSchema::Int,
            Type::Float => ContractTypeSchema::Float,
            Type::Bool => ContractTypeSchema::Bool,
            Type::String => ContractTypeSchema::String,
            Type::Char => ContractTypeSchema::Char,
            Type::Void => ContractTypeSchema::Void,
            Type::Func { params, ret } => ContractTypeSchema::Func {
                params: params
                    .iter()
                    .map(|param| self.param_schema(param, raw_ids))
                    .collect(),
                ret: Box::new(self.return_schema(ret, raw_ids)),
            },
            Type::Dyn(contract) => {
                let key = self
                    .key_for_ref(&ModuleScope::Root, contract)
                    .expect("finished nested dynamic surface must resolve");
                ContractTypeSchema::Dyn(raw_ids[self.raw_index[&key]])
            }
            Type::Tuple(types) => ContractTypeSchema::Tuple(
                types
                    .iter()
                    .map(|ty| self.type_schema(ty, raw_ids))
                    .collect(),
            ),
            Type::Nominal(nominal) => ContractTypeSchema::Nominal {
                id: nominal.id.clone(),
                type_args: nominal
                    .type_args
                    .iter()
                    .map(|ty| self.type_schema(ty, raw_ids))
                    .collect(),
                const_args: nominal.const_args.clone(),
            },
            Type::List { elem } => {
                ContractTypeSchema::List(Box::new(self.type_schema(elem, raw_ids)))
            }
            Type::Array { elem, len } => ContractTypeSchema::Array {
                elem: Box::new(self.type_schema(elem, raw_ids)),
                len: len.clone(),
            },
            Type::Map { key, value } => ContractTypeSchema::Map {
                key: Box::new(self.type_schema(key, raw_ids)),
                value: Box::new(self.type_schema(value, raw_ids)),
            },
            Type::Slice { elem } => {
                ContractTypeSchema::Slice(Box::new(self.type_schema(elem, raw_ids)))
            }
            Type::Optional { inner } => {
                ContractTypeSchema::Optional(Box::new(self.type_schema(inner, raw_ids)))
            }
            Type::Infer
            | Type::InferReturn
            | Type::Var(_)
            | Type::UnresolvedName(_)
            | Type::UnresolvedNominal { .. } => {
                panic!("unfinished type in canonical contract surface: {ty}")
            }
        }
    }

    fn refine(&self) -> Vec<usize> {
        let mut colors = vec![0; self.raw.len()];
        loop {
            let signatures = self.normalized_signatures(&colors);
            let mut groups = group_signatures(signatures, Some(&colors));
            groups.sort_by(|left, right| left.order.cmp(&right.order));
            let mut next = vec![0; self.raw.len()];
            for (color, group) in groups.into_iter().enumerate() {
                for member in group.members {
                    next[member] = color;
                }
            }
            if same_partition(&colors, &next) {
                return next;
            }
            colors = next;
        }
    }

    fn normalized_signatures(&self, colors: &[usize]) -> Vec<ContractSetKey> {
        self.raw
            .iter()
            .map(|raw| {
                let mut folder = DynColorFolder {
                    builder: self,
                    colors,
                };
                ContractSetKey {
                    requirements: raw
                        .key
                        .requirements
                        .iter()
                        .map(|requirement| {
                            let mut requirement = requirement.clone();
                            for param in &mut requirement.params {
                                param.ty = folder.fold_type(&param.ty);
                            }
                            requirement.ret = folder.fold_return_spec(&requirement.ret);
                            requirement
                        })
                        .collect(),
                }
            })
            .collect()
    }
}

struct DynColorFolder<'a> {
    builder: &'a SurfaceBuilder<'a>,
    colors: &'a [usize],
}

impl TypeFolder for DynColorFolder<'_> {
    fn fold_contract_ref(&mut self, contract: &ContractRef) -> ContractRef {
        let key = self
            .builder
            .key_for_ref(&ModuleScope::Root, contract)
            .expect("finished nested dynamic surface must resolve");
        let index = self.builder.raw_index[&key];
        ContractRef::Hole(DynContractHoleId(self.colors[index] as u32))
    }
}

struct SignatureGroup {
    prior: usize,
    signature: ContractSetKey,
    members: Vec<usize>,
    order: Vec<u8>,
}

fn group_signatures(
    signatures: Vec<ContractSetKey>,
    colors: Option<&[usize]>,
) -> Vec<SignatureGroup> {
    let mut groups: Vec<SignatureGroup> = vec![];
    for (index, signature) in signatures.into_iter().enumerate() {
        let prior = colors.map_or(0, |colors| colors[index]);
        if let Some(group) = groups
            .iter_mut()
            .find(|group| group.prior == prior && group.signature == signature)
        {
            group.members.push(index);
            continue;
        }
        let mut order = Vec::new();
        push_u64(&mut order, prior as u64);
        encode_contract_set(&mut order, &signature);
        groups.push(SignatureGroup {
            prior,
            signature,
            members: vec![index],
            order,
        });
    }
    groups
}

fn same_partition(left: &[usize], right: &[usize]) -> bool {
    (0..left.len()).all(|i| (0..left.len()).all(|j| (left[i] == left[j]) == (right[i] == right[j])))
}

fn raw_label(raw: &RawSurface) -> String {
    raw.displays.iter().min().cloned().unwrap_or_else(|| {
        let names = raw
            .key
            .requirements
            .iter()
            .map(|requirement| requirement.name.as_str())
            .collect::<Vec<_>>()
            .join(", ");
        format!("dyn {{{names}}}")
    })
}

fn encode_contract_set(out: &mut Vec<u8>, set: &ContractSetKey) {
    push_len(out, set.requirements.len());
    for requirement in &set.requirements {
        push_str(out, requirement.name.as_str());
        out.push(match requirement.receiver {
            None => 0,
            Some(MethodReceiver::Value) => 1,
            Some(MethodReceiver::Ref) => 2,
        });
        push_len(out, requirement.params.len());
        for param in &requirement.params {
            encode_param(out, param);
        }
        push_len(out, requirement.required_params);
        encode_return(out, &requirement.ret);
    }
}

fn encode_param(out: &mut Vec<u8>, param: &FuncParam) {
    encode_type(out, &param.ty);
    out.extend([u8::from(param.mutable), u8::from(param.cast_accept)]);
    out.push(match param.escape {
        EscapeMode::NonEscaping => 0,
        EscapeMode::Escaping => 1,
    });
}

fn encode_return(out: &mut Vec<u8>, ret: &ReturnSpec) {
    match &ret.kind {
        ReturnKind::Value(ty) => {
            out.push(0);
            encode_type(out, ty);
        }
        ReturnKind::Place(ty) => {
            out.push(1);
            encode_type(out, ty);
        }
        ReturnKind::Infer => out.push(2),
        ReturnKind::Iter => out.push(3),
    }
}

fn encode_type(out: &mut Vec<u8>, ty: &Type) {
    match ty {
        Type::Infer => out.push(0),
        Type::InferReturn => out.push(1),
        Type::Any => out.push(2),
        Type::Int => out.push(3),
        Type::Float => out.push(4),
        Type::Bool => out.push(5),
        Type::String => out.push(6),
        Type::Char => out.push(7),
        Type::Void => out.push(8),
        Type::Func { params, ret } => {
            out.push(9);
            push_len(out, params.len());
            for param in params {
                encode_param(out, param);
            }
            encode_return(out, ret);
        }
        Type::Dyn(ContractRef::Hole(id)) => {
            out.push(10);
            push_u64(out, u64::from(id.0));
        }
        Type::Dyn(_) => panic!("dynamic surface was not normalized before ordering"),
        Type::Var(id) => {
            out.push(11);
            push_u64(out, u64::from(id.0));
        }
        Type::UnresolvedName(name) => {
            out.push(12);
            push_str(out, name.as_str());
        }
        Type::UnresolvedNominal {
            qualifier,
            name,
            generic_args,
        } => {
            out.push(13);
            encode_ident_option(out, *qualifier);
            push_str(out, name.as_str());
            push_len(out, generic_args.len());
            for arg in generic_args {
                match arg {
                    crate::ast::GenericArg::Type(ty) => {
                        out.push(0);
                        encode_type(out, ty);
                    }
                    crate::ast::GenericArg::Const(arg) => {
                        out.push(1);
                        encode_const_arg(out, arg);
                    }
                }
            }
        }
        Type::Tuple(types) => {
            out.push(14);
            push_len(out, types.len());
            for ty in types {
                encode_type(out, ty);
            }
        }
        Type::Nominal(nominal) => {
            out.push(15);
            nominal.id.encode(out);
            push_len(out, nominal.type_args.len());
            for ty in &nominal.type_args {
                encode_type(out, ty);
            }
            push_len(out, nominal.const_args.len());
            for arg in &nominal.const_args {
                encode_const_arg(out, arg);
            }
        }
        Type::List { elem } => {
            out.push(16);
            encode_type(out, elem);
        }
        Type::Array { elem, len } => {
            out.push(17);
            encode_type(out, elem);
            encode_array_len(out, len.clone());
        }
        Type::Map { key, value } => {
            out.push(18);
            encode_type(out, key);
            encode_type(out, value);
        }
        Type::Slice { elem } => {
            out.push(19);
            encode_type(out, elem);
        }
        Type::Optional { inner } => {
            out.push(20);
            encode_type(out, inner);
        }
    }
}

fn encode_const_arg(out: &mut Vec<u8>, arg: &ConstArg) {
    match arg {
        ConstArg::Value(value) => {
            out.push(0);
            match value.value() {
                ConstValue::Int(value) => {
                    out.push(0);
                    out.extend(value.to_le_bytes());
                }
                ConstValue::Float(value) => {
                    out.push(1);
                    out.extend(value.to_bits().to_le_bytes());
                }
                ConstValue::Bool(value) => out.extend([2, u8::from(*value)]),
                ConstValue::String(value) => {
                    out.push(3);
                    push_str(out, value);
                }
                ConstValue::Char(value) => {
                    out.push(4);
                    push_u64(out, u64::from(*value as u32));
                }
            }
        }
        ConstArg::Name(name) => {
            out.push(1);
            push_str(out, name.as_str());
        }
        ConstArg::Param(id) => {
            out.push(2);
            push_u64(out, u64::from(id.0));
        }
    }
}

fn encode_array_len(out: &mut Vec<u8>, len: ArrayLen) {
    match len {
        ArrayLen::Fixed(len) => {
            out.push(0);
            push_len(out, *len.value());
        }
        ArrayLen::Infer => out.push(1),
        ArrayLen::Named(name) => {
            out.push(2);
            push_str(out, name.as_str());
        }
        ArrayLen::Param(id) => {
            out.push(3);
            push_u64(out, u64::from(id.0));
        }
        ArrayLen::Expr(expr) => {
            out.push(4);
            encode_const_expr(out, &expr);
        }
    }
}

fn encode_const_expr(out: &mut Vec<u8>, expr: &ConstExpr) {
    match expr {
        ConstExpr::Value(value) => {
            out.push(0);
            encode_const_arg(out, &ConstArg::value(value.clone()));
        }
        ConstExpr::Param(id) => {
            out.push(1);
            push_u64(out, u64::from(id.0));
        }
        ConstExpr::Unary(op, expr) => {
            out.push(2);
            push_str(out, &op.to_string());
            encode_const_expr(out, expr);
        }
        ConstExpr::Binary(op, left, right) => {
            out.push(3);
            push_str(out, &op.to_string());
            encode_const_expr(out, left);
            encode_const_expr(out, right);
        }
    }
}

fn encode_ident_option(out: &mut Vec<u8>, ident: Option<Ident>) {
    match ident {
        Some(ident) => {
            out.push(1);
            push_str(out, ident.as_str());
        }
        None => out.push(0),
    }
}

fn push_str(out: &mut Vec<u8>, value: &str) {
    push_len(out, value.len());
    out.extend(value.as_bytes());
}

fn push_len(out: &mut Vec<u8>, value: usize) {
    push_u64(out, value as u64);
}

fn push_u64(out: &mut Vec<u8>, value: u64) {
    out.extend(value.to_le_bytes());
}
