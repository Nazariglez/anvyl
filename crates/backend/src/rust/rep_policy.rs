use std::collections::{BTreeMap, BTreeSet};

use anvyx_frontend::air::{
    self, AggregateKind, ContractSurfaceId, ContractWitnessId, ContractWitnessTarget, ParamMode,
    Program as AirProgram, TypeData, TypeId, TypePassClass, TypePassClasses, ValueUse,
    VariantShape,
};

use super::{
    rir::{
        RirCellDecl, RirCellStorage, RirCollectionStorageKind, RirDataRef, RirEnum, RirEnumId,
        RirField, RirFunctionId, RirLambda, RirLambdaCapture, RirLambdaEnvField,
        RirLambdaEnvFieldKind, RirLambdaEnvLayout, RirLambdaId, RirLambdaSigId, RirLambdaStorage,
        RirParamAbi, RirParamEscape, RirParamSemantic, RirProgram, RirStruct, RirStructId,
        RirTuple, RirTupleId, RirType, RirTypeId,
    },
    target,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RustValueRep {
    InlineCopy,
    InlineArray,
    InlineStruct,
    InlineEnum,
    RawIntEnum,
    RawStringEnum,
    HeapHandle,
    CowString,
    CowList,
    CowMap,
    Opaque,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RustBorrowView {
    Ref,
    Str,
    Slice,
    TargetGap,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RustMaterialization {
    Copy,
    Share,
    CloneHandle,
    CloneLambda,
    BorrowGuard,
    Gap,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RustPhysicalRecipe {
    Copy,
    ManagedShare,
    CloneHandle,
    CloneLambda,
    CloneValue,
    ReconstructAggregate,
    BorrowView,
    Move,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RustRecipePosition {
    Value,
    StoredPayload(LambdaStorageFamily),
    MapKey,
    Global,
    HeapEdge,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RustRecipeGap {
    UnsupportedType(TypeId),
    DynamicCarrierPending(TypeId),
    FunctionCapabilityPending(TypeId),
    UnsupportedStorage {
        ty: TypeId,
        family: LambdaStorageFamily,
    },
    UnsupportedMapKey(TypeId),
    UnsupportedGlobal(TypeId),
    UnsupportedHeapEdge(TypeId),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RustDynamicLayoutPlan {
    pub carriers: Vec<RustDynamicCarrierPlan>,
    pub declaration_order: Vec<ContractSurfaceId>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RustDynamicCarrierPlan {
    pub surface: ContractSurfaceId,
    pub layout: Result<RustApproxLayout, RustLayoutGap>,
    pub lifecycle: Result<RustLifecyclePlan, RustLifecycleGap>,
    pub dependencies: Vec<ContractSurfaceId>,
    pub variants: Vec<RustDynamicCarrierVariant>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RustDynamicCarrierVariant {
    pub witness: ContractWitnessId,
    pub concrete_ty: TypeId,
    pub payload: Result<RustPhysicalRecipe, RustRecipeGap>,
    pub storage: RustPayloadStorage,
    pub recursive: bool,
    pub payload_layout: Result<RustApproxLayout, RustLayoutGap>,
    pub lifecycle: Result<RustLifecyclePlan, RustLifecycleGap>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RustPayloadStorage {
    Inline,
    Boxed,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct RustApproxLayout {
    pub size: u64,
    pub align: u64,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RustLayoutGap {
    UnsupportedType(TypeId),
    ProviderInlineLayoutUnknown(air::ExternTypeId),
    FunctionLayoutUnknown(TypeId),
    ArithmeticOverflow,
    RecursiveInline(TypeId),
    RecursiveCarrier(ContractSurfaceId),
    MissingWeakeningWitness {
        source: ContractSurfaceId,
        target: ContractSurfaceId,
        concrete_ty: TypeId,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct RustLifecyclePlan {
    pub heap: RustHeapLifecycle,
    pub context: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct RustHeapLifecycle {
    pub owns_edges: bool,
    pub trace: bool,
    pub drop: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RustLifecycleGap {
    UnsupportedType(TypeId),
    FunctionMetadataUnknown(TypeId),
    ProviderInlineMetadataUnknown(air::ExternTypeId),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RustMaterialGap {
    UnsupportedType,
    UnsupportedRooting,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct RustMaterialPlan {
    pub materialization: RustMaterialization,
    pub gap: Option<RustMaterialGap>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RustMaterialSource {
    Value,
    ExactGlobalRoot,
    StoredPayload,
    DataRefMutPlace,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LambdaStorageFamily {
    StructField,
    TupleField,
    EnumPayload,
    DynamicPayload,
    OptionalPayload,
    FixedArrayElement,
    SliceView,
    ListElement,
    MapKey,
    MapValue,
    DataRefProjection,
    GlobalRoot,
    GlobalProjection,
    UnknownOrigin,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LambdaStorageGap {
    StorageImplementation,
    ProvenanceOrigin,
    Lifetime,
    Trace,
    GlobalRooting,
    MapKeyEqualityHash,
    UnsupportedType,
}

impl From<LambdaStorageGap> for RustMaterialGap {
    fn from(gap: LambdaStorageGap) -> Self {
        match gap {
            LambdaStorageGap::GlobalRooting => RustMaterialGap::UnsupportedRooting,
            LambdaStorageGap::StorageImplementation
            | LambdaStorageGap::ProvenanceOrigin
            | LambdaStorageGap::Lifetime
            | LambdaStorageGap::Trace
            | LambdaStorageGap::MapKeyEqualityHash
            | LambdaStorageGap::UnsupportedType => RustMaterialGap::UnsupportedType,
        }
    }
}

impl LambdaStorageFamily {
    fn lambda_gap(self) -> LambdaStorageGap {
        match self {
            LambdaStorageFamily::MapKey => LambdaStorageGap::MapKeyEqualityHash,
            LambdaStorageFamily::GlobalProjection => LambdaStorageGap::GlobalRooting,
            LambdaStorageFamily::UnknownOrigin => LambdaStorageGap::ProvenanceOrigin,
            LambdaStorageFamily::SliceView => LambdaStorageGap::Lifetime,
            LambdaStorageFamily::GlobalRoot
            | LambdaStorageFamily::StructField
            | LambdaStorageFamily::TupleField
            | LambdaStorageFamily::EnumPayload
            | LambdaStorageFamily::DynamicPayload
            | LambdaStorageFamily::OptionalPayload
            | LambdaStorageFamily::FixedArrayElement
            | LambdaStorageFamily::ListElement
            | LambdaStorageFamily::MapValue
            | LambdaStorageFamily::DataRefProjection => LambdaStorageGap::StorageImplementation,
        }
    }

    fn allows_dynamic_owned(self) -> bool {
        matches!(
            self,
            Self::StructField
                | Self::TupleField
                | Self::EnumPayload
                | Self::OptionalPayload
                | Self::FixedArrayElement
                | Self::ListElement
                | Self::MapValue
                | Self::DataRefProjection
                | Self::GlobalRoot
                | Self::DynamicPayload
        )
    }

    fn allows_function_payload(self) -> bool {
        matches!(
            self,
            Self::StructField
                | Self::TupleField
                | Self::DynamicPayload
                | Self::OptionalPayload
                | Self::FixedArrayElement
                | Self::ListElement
                | Self::MapValue
                | Self::DataRefProjection
                | Self::GlobalRoot
        )
    }
}

fn nested_storage_family(
    outer: LambdaStorageFamily,
    nested: LambdaStorageFamily,
) -> LambdaStorageFamily {
    match (outer, nested) {
        (
            LambdaStorageFamily::MapKey | LambdaStorageFamily::MapValue,
            LambdaStorageFamily::MapKey,
        ) => LambdaStorageFamily::MapKey,
        (
            LambdaStorageFamily::GlobalRoot
            | LambdaStorageFamily::GlobalProjection
            | LambdaStorageFamily::MapKey
            | LambdaStorageFamily::MapValue,
            _,
        ) => outer,
        _ => nested,
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RustMaterialIntent {
    Read,
    Store,
    MutPlacePayload,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub(super) struct LambdaSigStorageShape {
    pub(super) heap_env: bool,
    pub(super) lifetime: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum LambdaCaptureLayoutEdge {
    InlineValue,
    HeapEnvField,
    SharedBorrow,
    MutBorrow,
    StackCell,
    HeapCell,
    ScopedPlaceCell,
    Unsupported,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) enum LambdaTraceAction {
    Noop,
    HeapEnv,
    HeapCellCaptures(Vec<usize>),
}

pub(super) struct LambdaSigLayout<'a> {
    pub variants: Vec<LambdaVariantLayout<'a>>,
    pub captures_self: bool,
    pub mut_self: bool,
}

impl LambdaSigLayout<'_> {
    pub(super) fn self_arg(&self) -> &'static str {
        match (self.captures_self, self.mut_self) {
            (_, true) => "&mut self",
            (true, false) => "&self",
            (false, false) => "self",
        }
    }
}

pub(super) struct LambdaVariantLayout<'a> {
    pub id: RirLambdaId,
    pub function: RirFunctionId,
    pub storage: RirLambdaStorage,
    pub captures: &'a [RirLambdaCapture],
    pub trace_action: LambdaTraceAction,
}

fn materialization_is_owned_payload(materialization: RustMaterialization) -> bool {
    matches!(
        materialization,
        RustMaterialization::Copy | RustMaterialization::Share | RustMaterialization::CloneHandle
    )
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RustCarrierDiscriminant {
    U32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct RustTargetProfile {
    pub pointer_size: u8,
    pub pointer_align: u8,
    pub carrier_discriminant: RustCarrierDiscriminant,
}

impl RustTargetProfile {
    pub const AOT_64: Self = Self {
        pointer_size: 8,
        pointer_align: 8,
        carrier_discriminant: RustCarrierDiscriminant::U32,
    };
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RustRepresentationPlanError {
    InvalidPointerLayout { size: u8, align: u8 },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum RustLayoutNode {
    Type(TypeId),
    Carrier(ContractSurfaceId),
}

#[derive(Debug, Clone, Copy)]
pub struct RustRepresentationPlan<'a> {
    program: &'a AirProgram,
    classes: &'a TypePassClasses,
    target: RustTargetProfile,
}

impl<'a> RustRepresentationPlan<'a> {
    pub fn new(program: &'a AirProgram, classes: &'a TypePassClasses) -> Self {
        Self {
            program,
            classes,
            target: RustTargetProfile::AOT_64,
        }
    }

    pub fn for_target(
        program: &'a AirProgram,
        classes: &'a TypePassClasses,
        target: RustTargetProfile,
    ) -> Result<Self, RustRepresentationPlanError> {
        let valid_pointer = target.pointer_size > 0
            && target.pointer_align.is_power_of_two()
            && target.pointer_align <= target.pointer_size
            && target.pointer_size.is_multiple_of(target.pointer_align);
        if !valid_pointer {
            return Err(RustRepresentationPlanError::InvalidPointerLayout {
                size: target.pointer_size,
                align: target.pointer_align,
            });
        }
        Ok(Self {
            program,
            classes,
            target,
        })
    }

    pub fn target(self) -> RustTargetProfile {
        self.target
    }

    pub fn dynamic_layout_plan(self) -> Result<RustDynamicLayoutPlan, RustLayoutGap> {
        self.verify_weakening_witnesses()?;
        let graph = self.layout_graph();
        let components = strongly_connected_components(&graph);
        let weakening_classes = self.weakening_classes();
        let mut storage = BTreeMap::new();
        let mut recursive_variants = BTreeSet::new();

        for witness in &self.program.contract_witnesses {
            let carrier = RustLayoutNode::Carrier(witness.key.surface);
            let payload = RustLayoutNode::Type(witness.key.concrete_ty);
            let recursive = components.get(&carrier) == components.get(&payload)
                && component_is_cyclic(&graph, carrier, &components);
            let key = (witness.key.surface, witness.key.concrete_ty);
            if recursive {
                recursive_variants.insert(key);
            }
            storage.insert(
                key,
                if recursive {
                    RustPayloadStorage::Boxed
                } else {
                    RustPayloadStorage::Inline
                },
            );
        }
        propagate_weakening_storage(&mut storage, &weakening_classes);

        let initial = self.compute_layouts(&storage);
        for witness in &self.program.contract_witnesses {
            let key = (witness.key.surface, witness.key.concrete_ty);
            if storage[&key] == RustPayloadStorage::Inline
                && initial.payloads.get(&key).is_some_and(|layout| {
                    layout.as_ref().is_ok_and(|layout| {
                        layout.size > self.inline_payload_limit()
                            || layout.align > u64::from(self.target.pointer_align)
                    })
                })
            {
                storage.insert(key, RustPayloadStorage::Boxed);
            }
        }
        propagate_weakening_storage(&mut storage, &weakening_classes);

        let layouts = self.compute_layouts(&storage);
        let lifecycles = self.compute_lifecycles(&storage);
        let mut carriers = self.dynamic_carrier_closure();
        for carrier in &mut carriers {
            carrier.layout = layouts.carriers[&carrier.surface];
            carrier.dependencies = self.carrier_dependencies(carrier.surface, &storage);
            for variant in &mut carrier.variants {
                let key = (carrier.surface, variant.concrete_ty);
                variant.storage = storage[&key];
                variant.recursive = recursive_variants.contains(&key);
                variant.payload_layout = layouts.payloads[&key];
                variant.lifecycle = lifecycles.variants[&key];
            }
            carrier.lifecycle = lifecycles.carriers[&carrier.surface];
        }
        let declaration_order = carrier_declaration_order(&carriers);
        Ok(RustDynamicLayoutPlan {
            carriers,
            declaration_order,
        })
    }

    fn dynamic_carrier_closure(self) -> Vec<RustDynamicCarrierPlan> {
        let reachable = self.reachable_dynamic_surfaces();
        let mut by_surface = reachable
            .iter()
            .map(|surface| (surface.index(), vec![]))
            .collect::<BTreeMap<_, _>>();
        for (index, witness) in self.program.contract_witnesses.iter().enumerate() {
            let variant = RustDynamicCarrierVariant {
                witness: ContractWitnessId::from_index(index),
                concrete_ty: witness.key.concrete_ty,
                payload: self.recipe_for(
                    witness.key.concrete_ty,
                    ValueUse::Store,
                    RustRecipePosition::StoredPayload(LambdaStorageFamily::DynamicPayload),
                ),
                storage: RustPayloadStorage::Inline,
                recursive: false,
                payload_layout: Err(RustLayoutGap::UnsupportedType(witness.key.concrete_ty)),
                lifecycle: Err(RustLifecycleGap::UnsupportedType(witness.key.concrete_ty)),
            };
            if let Some(variants) = by_surface.get_mut(&witness.key.surface.index()) {
                variants.push(variant);
            }
        }
        let mut carriers = by_surface
            .into_iter()
            .map(|(surface, mut variants)| {
                variants.sort_by_cached_key(|variant| {
                    (
                        self.type_sort_key(variant.concrete_ty),
                        self.witness_sort_key(variant.witness),
                    )
                });
                RustDynamicCarrierPlan {
                    surface: ContractSurfaceId::from_index(surface),
                    layout: Err(RustLayoutGap::UnsupportedType(TypeId::from_index(0))),
                    lifecycle: Ok(RustLifecyclePlan::default()),
                    dependencies: vec![],
                    variants,
                }
            })
            .collect::<Vec<_>>();
        carriers.sort_by_cached_key(|carrier| self.carrier_sort_key(carrier.surface));
        carriers
    }

    fn carrier_sort_key(self, surface: ContractSurfaceId) -> String {
        let mut variants = self
            .program
            .contract_witnesses
            .iter()
            .enumerate()
            .filter(|(_, witness)| witness.key.surface == surface)
            .map(|(index, witness)| {
                format!(
                    "{}:{}",
                    self.type_sort_key(witness.key.concrete_ty),
                    self.witness_sort_key(ContractWitnessId::from_index(index))
                )
            })
            .collect::<Vec<_>>();
        variants.sort();
        format!("{}|{}", self.surface_sort_key(surface), variants.join(";"))
    }

    fn witness_sort_key(self, witness: ContractWitnessId) -> String {
        let witness = self.program.contract_witness(witness);
        let mut key = StructuralKeyCx::new(self);
        witness
            .key
            .slots
            .iter()
            .map(|slot| {
                format!(
                    "{}:{:?}:{}",
                    slot.slot.index(),
                    slot.receiver,
                    key.witness_target(&slot.target)
                )
            })
            .collect::<Vec<_>>()
            .join(";")
    }

    fn surface_sort_key(self, surface: ContractSurfaceId) -> String {
        StructuralKeyCx::new(self).surface(surface)
    }

    fn type_sort_key(self, ty: TypeId) -> String {
        StructuralKeyCx::new(self).ty(ty)
    }

    fn reachable_dynamic_surfaces(self) -> BTreeSet<ContractSurfaceId> {
        let mut reachable = self
            .program
            .type_arena
            .iter()
            .filter_map(|ty| match ty {
                TypeData::Dyn(surface) => Some(*surface),
                _ => None,
            })
            .collect::<BTreeSet<_>>();
        loop {
            let mut next = reachable.clone();
            for witness in self
                .program
                .contract_witnesses
                .iter()
                .filter(|witness| reachable.contains(&witness.key.surface))
            {
                self.collect_type_carriers(
                    witness.key.concrete_ty,
                    &mut BTreeSet::new(),
                    &mut next,
                );
            }
            if next == reachable {
                return reachable;
            }
            reachable = next;
        }
    }

    fn verify_weakening_witnesses(self) -> Result<(), RustLayoutGap> {
        for weakening in &self.program.contract_weakenings {
            for source in self
                .program
                .contract_witnesses
                .iter()
                .filter(|witness| witness.key.surface == weakening.source)
            {
                let projected = self.program.contract_witnesses.iter().any(|target| {
                    target.key.surface == weakening.target
                        && target.key.concrete_ty == source.key.concrete_ty
                });
                if !projected {
                    return Err(RustLayoutGap::MissingWeakeningWitness {
                        source: weakening.source,
                        target: weakening.target,
                        concrete_ty: source.key.concrete_ty,
                    });
                }
            }
        }
        Ok(())
    }

    fn layout_graph(self) -> BTreeMap<RustLayoutNode, BTreeSet<RustLayoutNode>> {
        let mut graph = BTreeMap::new();
        for index in 0..self.program.type_arena.len() {
            let ty = TypeId::from_index(index);
            graph.insert(
                RustLayoutNode::Type(ty),
                self.inline_type_dependencies(ty)
                    .into_iter()
                    .map(RustLayoutNode::Type)
                    .chain(match self.program.type_arena.data(ty) {
                        TypeData::Dyn(surface) => Some(RustLayoutNode::Carrier(*surface)),
                        _ => None,
                    })
                    .collect(),
            );
        }
        for index in 0..self.program.contract_surfaces.len() {
            let surface = ContractSurfaceId::from_index(index);
            graph.insert(
                RustLayoutNode::Carrier(surface),
                self.program
                    .contract_witnesses
                    .iter()
                    .filter(|witness| witness.key.surface == surface)
                    .map(|witness| RustLayoutNode::Type(witness.key.concrete_ty))
                    .collect(),
            );
        }
        graph
    }

    fn inline_type_dependencies(self, ty: TypeId) -> Vec<TypeId> {
        match self.program.type_arena.data(ty) {
            TypeData::Optional(inner) | TypeData::Array { elem: inner, .. } => vec![*inner],
            TypeData::Tuple(fields) => fields.clone(),
            TypeData::Aggregate(id) => self
                .program
                .aggregate(*id)
                .fields
                .iter()
                .map(|field| field.ty)
                .collect(),
            TypeData::Enum(id) => self
                .program
                .enum_decl(*id)
                .variants
                .iter()
                .flat_map(Self::variant_field_tys)
                .collect(),
            TypeData::Extern(id) if self.program.extern_type(*id).rep == air::ExternRep::Inline => {
                let decl = self.program.extern_type(*id);
                decl.fields
                    .iter()
                    .filter(|field| !field.computed)
                    .map(|field| field.ty)
                    .chain(decl.variants.iter().flat_map(Self::variant_field_tys))
                    .collect()
            }
            TypeData::Int
            | TypeData::Float
            | TypeData::Bool
            | TypeData::Char
            | TypeData::String
            | TypeData::Void
            | TypeData::Any
            | TypeData::List(_)
            | TypeData::Map { .. }
            | TypeData::Slice(_)
            | TypeData::DataRef(_)
            | TypeData::Extern(_)
            | TypeData::Function(_)
            | TypeData::Dyn(_) => vec![],
        }
    }

    fn weakening_classes(self) -> BTreeMap<ContractSurfaceId, ContractSurfaceId> {
        let mut classes = (0..self.program.contract_surfaces.len())
            .map(|index| {
                let surface = ContractSurfaceId::from_index(index);
                (surface, surface)
            })
            .collect::<BTreeMap<_, _>>();
        loop {
            let mut changed = false;
            for weakening in &self.program.contract_weakenings {
                let root = classes[&weakening.source].min(classes[&weakening.target]);
                let source_root = classes[&weakening.source];
                let target_root = classes[&weakening.target];
                for class in classes.values_mut() {
                    if *class == source_root || *class == target_root {
                        changed |= *class != root;
                        *class = root;
                    }
                }
            }
            if !changed {
                return classes;
            }
        }
    }

    fn compute_layouts(
        self,
        storage: &BTreeMap<(ContractSurfaceId, TypeId), RustPayloadStorage>,
    ) -> ComputedDynamicLayouts {
        let mut cx = ApproxLayoutCx {
            plan: self,
            storage,
            carrier_layouts: BTreeMap::new(),
            visiting_carriers: BTreeSet::new(),
            visiting_types: BTreeSet::new(),
        };
        for index in 0..self.program.contract_surfaces.len() {
            let _ = cx.carrier_layout(ContractSurfaceId::from_index(index));
        }
        let carriers = cx.carrier_layouts.clone();
        let mut payloads = BTreeMap::new();
        for witness in &self.program.contract_witnesses {
            let key = (witness.key.surface, witness.key.concrete_ty);
            let layout = match storage[&key] {
                RustPayloadStorage::Boxed => Ok(self.pointer_layout()),
                RustPayloadStorage::Inline => cx.type_layout(witness.key.concrete_ty),
            };
            payloads.insert(key, layout);
        }
        ComputedDynamicLayouts { carriers, payloads }
    }

    fn compute_lifecycles(
        self,
        storage: &BTreeMap<(ContractSurfaceId, TypeId), RustPayloadStorage>,
    ) -> ComputedDynamicLifecycles {
        let mut carriers = (0..self.program.contract_surfaces.len())
            .map(|index| {
                (
                    ContractSurfaceId::from_index(index),
                    Ok(RustLifecyclePlan::default()),
                )
            })
            .collect::<BTreeMap<_, _>>();
        let mut variants = BTreeMap::new();

        loop {
            let mut changed = false;
            variants.clear();
            let mut next = carriers.clone();
            for (index, _) in self.program.contract_surfaces.iter().enumerate() {
                let surface = ContractSurfaceId::from_index(index);
                let mut carrier = Ok(RustLifecyclePlan::default());
                for witness in self
                    .program
                    .contract_witnesses
                    .iter()
                    .filter(|witness| witness.key.surface == surface)
                {
                    let key = (surface, witness.key.concrete_ty);
                    let mut lifecycle = self.type_lifecycle(witness.key.concrete_ty, &carriers);
                    if storage[&key] == RustPayloadStorage::Boxed {
                        lifecycle = lifecycle.map(|mut lifecycle| {
                            lifecycle.heap.drop = true;
                            lifecycle
                        });
                    }
                    variants.insert(key, lifecycle);
                    carrier = merge_lifecycles(carrier, lifecycle);
                }
                changed |= next[&surface] != carrier;
                next.insert(surface, carrier);
            }
            carriers = next;
            if !changed {
                break;
            }
        }
        ComputedDynamicLifecycles { carriers, variants }
    }

    fn type_lifecycle(
        self,
        ty: TypeId,
        carriers: &BTreeMap<ContractSurfaceId, Result<RustLifecyclePlan, RustLifecycleGap>>,
    ) -> Result<RustLifecyclePlan, RustLifecycleGap> {
        fn visit(
            plan: RustRepresentationPlan<'_>,
            ty: TypeId,
            carriers: &BTreeMap<ContractSurfaceId, Result<RustLifecyclePlan, RustLifecycleGap>>,
            visiting: &mut BTreeSet<TypeId>,
        ) -> Result<RustLifecyclePlan, RustLifecycleGap> {
            if !visiting.insert(ty) {
                return Ok(RustLifecyclePlan::default());
            }
            let lifecycle = match plan.program.type_arena.data(ty) {
                TypeData::Int
                | TypeData::Float
                | TypeData::Bool
                | TypeData::Char
                | TypeData::Void
                | TypeData::String => Ok(RustLifecyclePlan::default()),
                TypeData::List(_) | TypeData::Map { .. } | TypeData::DataRef(_) => {
                    Ok(tracked_lifecycle())
                }
                TypeData::Slice(_) => Ok(RustLifecyclePlan {
                    heap: RustHeapLifecycle {
                        owns_edges: true,
                        trace: true,
                        drop: false,
                    },
                    context: true,
                }),
                TypeData::Function(_) => Err(RustLifecycleGap::FunctionMetadataUnknown(ty)),
                TypeData::Optional(inner) | TypeData::Array { elem: inner, .. } => {
                    visit(plan, *inner, carriers, visiting)
                }
                TypeData::Tuple(fields) => {
                    fields
                        .iter()
                        .try_fold(RustLifecyclePlan::default(), |lifecycle, field| {
                            visit(plan, *field, carriers, visiting)
                                .map(|field| merge_lifecycle(lifecycle, field))
                        })
                }
                TypeData::Aggregate(id) => plan.program.aggregate(*id).fields.iter().try_fold(
                    RustLifecyclePlan::default(),
                    |lifecycle, field| {
                        visit(plan, field.ty, carriers, visiting)
                            .map(|field| merge_lifecycle(lifecycle, field))
                    },
                ),
                TypeData::Enum(id) => plan
                    .program
                    .enum_decl(*id)
                    .variants
                    .iter()
                    .flat_map(RustRepresentationPlan::variant_field_tys)
                    .try_fold(RustLifecyclePlan::default(), |lifecycle, field| {
                        visit(plan, field, carriers, visiting)
                            .map(|field| merge_lifecycle(lifecycle, field))
                    }),
                TypeData::Dyn(surface) => carriers[surface],
                TypeData::Extern(id)
                    if plan.program.extern_type(*id).rep == air::ExternRep::Shared =>
                {
                    Ok(tracked_lifecycle())
                }
                TypeData::Extern(id) => match plan.program.extern_type(*id).owns_heap_edges {
                    Some(false) => Ok(RustLifecyclePlan::default()),
                    Some(true) => Ok(tracked_lifecycle()),
                    None => Err(RustLifecycleGap::ProviderInlineMetadataUnknown(*id)),
                },
                TypeData::Any => Err(RustLifecycleGap::UnsupportedType(ty)),
            };
            visiting.remove(&ty);
            lifecycle
        }
        visit(self, ty, carriers, &mut BTreeSet::new())
    }

    fn carrier_dependencies(
        self,
        surface: ContractSurfaceId,
        storage: &BTreeMap<(ContractSurfaceId, TypeId), RustPayloadStorage>,
    ) -> Vec<ContractSurfaceId> {
        let mut dependencies = BTreeSet::new();
        for witness in self
            .program
            .contract_witnesses
            .iter()
            .filter(|witness| witness.key.surface == surface)
        {
            if storage[&(surface, witness.key.concrete_ty)] == RustPayloadStorage::Inline {
                self.collect_type_carriers(
                    witness.key.concrete_ty,
                    &mut BTreeSet::new(),
                    &mut dependencies,
                );
            }
        }
        dependencies.remove(&surface);
        let mut dependencies = dependencies.into_iter().collect::<Vec<_>>();
        dependencies.sort_by_cached_key(|dependency| self.carrier_sort_key(*dependency));
        dependencies
    }

    fn collect_type_carriers(
        self,
        ty: TypeId,
        visited: &mut BTreeSet<TypeId>,
        carriers: &mut BTreeSet<ContractSurfaceId>,
    ) {
        if !visited.insert(ty) {
            return;
        }
        if let TypeData::Dyn(surface) = self.program.type_arena.data(ty) {
            carriers.insert(*surface);
            return;
        }
        for dependency in self.inline_type_dependencies(ty) {
            self.collect_type_carriers(dependency, visited, carriers);
        }
    }

    fn pointer_layout(self) -> RustApproxLayout {
        RustApproxLayout {
            size: u64::from(self.target.pointer_size),
            align: u64::from(self.target.pointer_align),
        }
    }

    fn inline_payload_limit(self) -> u64 {
        u64::from(self.target.pointer_size) * 3
    }

    fn discriminant_layout(self) -> RustApproxLayout {
        match self.target.carrier_discriminant {
            RustCarrierDiscriminant::U32 => RustApproxLayout { size: 4, align: 4 },
        }
    }

    pub fn recipe_for(
        self,
        ty: TypeId,
        use_: ValueUse,
        position: RustRecipePosition,
    ) -> Result<RustPhysicalRecipe, RustRecipeGap> {
        self.check_recipe_position(ty, position)?;
        if matches!(self.program.type_arena.data(ty), TypeData::Dyn(_)) {
            return Err(RustRecipeGap::DynamicCarrierPending(ty));
        }
        if let TypeData::Extern(id) = self.program.type_arena.data(ty) {
            let decl = self.program.extern_type(*id);
            if decl.rep == air::ExternRep::Inline {
                return match decl.materialization {
                    Some(anvyx_runtime::ExternMaterialization::Copy) => {
                        Ok(RustPhysicalRecipe::Copy)
                    }
                    Some(anvyx_runtime::ExternMaterialization::Clone) => {
                        Ok(RustPhysicalRecipe::CloneValue)
                    }
                    None => Err(RustRecipeGap::UnsupportedType(ty)),
                };
            }
        }
        if use_ == ValueUse::Consume {
            return Ok(RustPhysicalRecipe::Move);
        }
        if matches!(self.program.type_arena.data(ty), TypeData::Function(_)) {
            return if use_ == ValueUse::CallValue {
                Ok(RustPhysicalRecipe::Copy)
            } else {
                Err(RustRecipeGap::FunctionCapabilityPending(ty))
            };
        }
        if matches!(
            self.program.type_arena.data(ty),
            TypeData::Aggregate(_)
                | TypeData::Tuple(_)
                | TypeData::Enum(_)
                | TypeData::Array { .. }
        ) && !self.copyable(ty)
        {
            return Ok(RustPhysicalRecipe::ReconstructAggregate);
        }
        match self.materialization(ty) {
            RustMaterialization::Copy => Ok(RustPhysicalRecipe::Copy),
            RustMaterialization::BorrowGuard => Ok(RustPhysicalRecipe::BorrowView),
            RustMaterialization::Share => Ok(RustPhysicalRecipe::ManagedShare),
            RustMaterialization::CloneHandle => Ok(RustPhysicalRecipe::CloneHandle),
            RustMaterialization::CloneLambda => Ok(RustPhysicalRecipe::CloneLambda),
            RustMaterialization::Gap => Err(RustRecipeGap::UnsupportedType(ty)),
        }
    }

    fn check_recipe_position(
        self,
        ty: TypeId,
        position: RustRecipePosition,
    ) -> Result<(), RustRecipeGap> {
        match position {
            RustRecipePosition::Value => Ok(()),
            RustRecipePosition::StoredPayload(family) => self
                .storage_supported(ty, family)
                .map_err(|_| RustRecipeGap::UnsupportedStorage { ty, family }),
            RustRecipePosition::MapKey if self.map_key_supported(ty) => Ok(()),
            RustRecipePosition::MapKey => Err(RustRecipeGap::UnsupportedMapKey(ty)),
            RustRecipePosition::Global
                if !matches!(
                    self.exact_root_global_materialization(ty),
                    RustMaterialization::Gap
                ) =>
            {
                Ok(())
            }
            RustRecipePosition::Global => Err(RustRecipeGap::UnsupportedGlobal(ty)),
            RustRecipePosition::HeapEdge if self.stored_payload_supported(ty) => Ok(()),
            RustRecipePosition::HeapEdge => Err(RustRecipeGap::UnsupportedHeapEdge(ty)),
        }
    }

    pub fn copyable(self, ty: TypeId) -> bool {
        if let TypeData::Extern(id) = self.program.type_arena.data(ty) {
            let decl = self.program.extern_type(*id);
            if decl.rep == air::ExternRep::Inline {
                return matches!(
                    decl.materialization,
                    Some(anvyx_runtime::ExternMaterialization::Copy)
                );
            }
        }
        if let TypeData::Optional(inner) = self.program.type_arena.data(ty) {
            return self.copyable(*inner);
        }
        if matches!(self.program.type_arena.data(ty), TypeData::DataRef(_)) {
            return false;
        }
        if matches!(self.program.type_arena.data(ty), TypeData::Function(_)) {
            return true;
        }
        self.classes.get(ty).is_some_and(|class| {
            matches!(
                class,
                TypePassClass::Immediate | TypePassClass::SmallCopyInline(_)
            )
        })
    }

    pub fn value_place_shareable(self, ty: TypeId) -> bool {
        self.copyable(ty) || self.shareable_value(ty)
    }

    pub fn value_from_ref_supported(self, ty: TypeId) -> bool {
        !matches!(
            self.materialization_for(ty, RustMaterialSource::Value, RustMaterialIntent::Read),
            RustMaterialization::BorrowGuard | RustMaterialization::Gap
        )
    }

    pub fn scoped_function_value_gap(
        self,
        ty: TypeId,
        root: air::PlaceRoot,
    ) -> Option<LambdaStorageGap> {
        if matches!(self.program.type_arena.data(ty), TypeData::Function(_))
            && matches!(root, air::PlaceRoot::ScopedBorrow(_))
        {
            Some(LambdaStorageGap::Lifetime)
        } else {
            None
        }
    }

    pub fn materialization_for(
        self,
        ty: TypeId,
        source: RustMaterialSource,
        intent: RustMaterialIntent,
    ) -> RustMaterialization {
        self.materialization_plan_for(ty, source, intent)
            .materialization
    }

    pub fn materialization_plan_for(
        self,
        ty: TypeId,
        source: RustMaterialSource,
        intent: RustMaterialIntent,
    ) -> RustMaterialPlan {
        let materialization = match (source, intent) {
            (RustMaterialSource::Value, RustMaterialIntent::Read) => self.materialization(ty),
            (
                RustMaterialSource::ExactGlobalRoot,
                RustMaterialIntent::Read | RustMaterialIntent::Store,
            ) => self.exact_root_global_materialization(ty),
            (RustMaterialSource::StoredPayload, RustMaterialIntent::Store) => {
                if self.stored_payload_supported(ty) {
                    self.materialization(ty)
                } else {
                    RustMaterialization::Gap
                }
            }
            (RustMaterialSource::DataRefMutPlace, RustMaterialIntent::MutPlacePayload) => {
                match self.program.type_arena.data(ty) {
                    TypeData::Int | TypeData::Float | TypeData::Bool | TypeData::Char => {
                        RustMaterialization::Copy
                    }
                    TypeData::DataRef(_) => RustMaterialization::CloneHandle,
                    TypeData::Dyn(_) => RustMaterialization::Share,
                    TypeData::Aggregate(_) | TypeData::Tuple(_) => self.materialization(ty),
                    _ => RustMaterialization::Gap,
                }
            }
            _ => RustMaterialization::Gap,
        };
        RustMaterialPlan {
            materialization,
            gap: matches!(materialization, RustMaterialization::Gap)
                .then(|| self.material_gap(ty, source)),
        }
    }

    fn material_gap(self, ty: TypeId, source: RustMaterialSource) -> RustMaterialGap {
        match source {
            RustMaterialSource::ExactGlobalRoot => self.global_material_gap(ty),
            RustMaterialSource::StoredPayload => self
                .storage_supported(ty, LambdaStorageFamily::UnknownOrigin)
                .err()
                .map_or(RustMaterialGap::UnsupportedType, RustMaterialGap::from),
            _ => RustMaterialGap::UnsupportedType,
        }
    }

    fn global_material_gap(self, ty: TypeId) -> RustMaterialGap {
        if self.contains_function_payload(ty) {
            return self
                .storage_supported(ty, LambdaStorageFamily::GlobalRoot)
                .err()
                .map_or(RustMaterialGap::UnsupportedType, RustMaterialGap::from);
        }
        match self.program.type_arena.data(ty) {
            TypeData::Void | TypeData::Any | TypeData::Function(_) => {
                RustMaterialGap::UnsupportedType
            }
            TypeData::Optional(inner) | TypeData::Array { elem: inner, .. } => {
                self.global_material_gap(*inner)
            }
            TypeData::Tuple(elems) => self.first_global_material_gap(elems.iter().copied()),
            TypeData::Aggregate(id) => self.first_global_material_gap(
                self.program
                    .aggregate(*id)
                    .fields
                    .iter()
                    .map(|field| field.ty),
            ),
            TypeData::Enum(id) => self.first_global_material_gap(
                self.program
                    .enum_decl(*id)
                    .variants
                    .iter()
                    .flat_map(Self::variant_field_tys),
            ),
            TypeData::Int
            | TypeData::Float
            | TypeData::Bool
            | TypeData::Char
            | TypeData::String
            | TypeData::List(_)
            | TypeData::Map { .. }
            | TypeData::Slice(_)
            | TypeData::DataRef(_)
            | TypeData::Extern(_)
            | TypeData::Dyn(_) => RustMaterialGap::UnsupportedRooting,
        }
    }

    fn first_global_material_gap(
        self,
        fields: impl IntoIterator<Item = TypeId>,
    ) -> RustMaterialGap {
        fields
            .into_iter()
            .find_map(|field| {
                matches!(
                    self.materialization_for(
                        field,
                        RustMaterialSource::ExactGlobalRoot,
                        RustMaterialIntent::Read,
                    ),
                    RustMaterialization::Gap
                )
                .then(|| self.global_material_gap(field))
            })
            .unwrap_or(RustMaterialGap::UnsupportedRooting)
    }

    fn materialization(self, ty: TypeId) -> RustMaterialization {
        if matches!(self.program.type_arena.data(ty), TypeData::Void) {
            return RustMaterialization::Gap;
        }
        if self.copyable(ty) {
            return RustMaterialization::Copy;
        }
        match self.program.type_arena.data(ty) {
            TypeData::String | TypeData::List(_) | TypeData::Map { .. } | TypeData::Dyn(_) => {
                RustMaterialization::Share
            }
            TypeData::DataRef(_) | TypeData::Extern(_) => RustMaterialization::CloneHandle,
            TypeData::Slice(_) => RustMaterialization::BorrowGuard,
            TypeData::Optional(inner) => self.composite_materialization([*inner]),
            TypeData::Array { elem, .. } => self
                .composite_storage_materialization([*elem], LambdaStorageFamily::FixedArrayElement),
            TypeData::Tuple(elems) => self.composite_storage_materialization(
                elems.iter().copied(),
                LambdaStorageFamily::TupleField,
            ),
            TypeData::Aggregate(id) => {
                let decl = self.program.aggregate(*id);
                let family = match decl.kind {
                    AggregateKind::Struct => LambdaStorageFamily::StructField,
                    AggregateKind::DataRef => LambdaStorageFamily::DataRefProjection,
                };
                self.composite_storage_materialization(
                    decl.fields.iter().map(|field| field.ty),
                    family,
                )
            }
            TypeData::Enum(id) => self.composite_materialization(
                self.program
                    .enum_decl(*id)
                    .variants
                    .iter()
                    .flat_map(Self::variant_field_tys),
            ),
            TypeData::Int
            | TypeData::Float
            | TypeData::Bool
            | TypeData::Char
            | TypeData::Void
            | TypeData::Any
            | TypeData::Function(_) => RustMaterialization::Gap,
        }
    }

    fn exact_root_global_materialization(self, ty: TypeId) -> RustMaterialization {
        match self.program.type_arena.data(ty) {
            TypeData::Function(_)
                if self
                    .storage_supported(ty, LambdaStorageFamily::GlobalRoot)
                    .is_ok() =>
            {
                self.materialization(ty)
            }
            TypeData::Void
            | TypeData::Any
            | TypeData::Slice(_)
            | TypeData::Extern(_)
            | TypeData::Function(_) => RustMaterialization::Gap,
            TypeData::List(elem) if !self.exact_root_global_field_supported(*elem) => {
                RustMaterialization::Gap
            }
            TypeData::Map { key, value, .. }
                if !self.map_key_supported(*key)
                    || !self.exact_root_global_field_supported(*value) =>
            {
                RustMaterialization::Gap
            }
            TypeData::Optional(inner) | TypeData::Array { elem: inner, .. } => {
                self.composite_global_materialization([*inner])
            }
            TypeData::Tuple(elems) => self.composite_global_materialization(elems.iter().copied()),
            TypeData::Aggregate(id) => self.composite_global_materialization(
                self.program
                    .aggregate(*id)
                    .fields
                    .iter()
                    .map(|field| field.ty),
            ),
            TypeData::Enum(id) => self.composite_global_materialization(
                self.program
                    .enum_decl(*id)
                    .variants
                    .iter()
                    .flat_map(Self::variant_field_tys),
            ),
            TypeData::Int
            | TypeData::Float
            | TypeData::Bool
            | TypeData::Char
            | TypeData::String
            | TypeData::DataRef(_)
            | TypeData::List(_)
            | TypeData::Map { .. }
            | TypeData::Dyn(_) => self.materialization(ty),
        }
    }

    fn composite_materialization(
        self,
        fields: impl IntoIterator<Item = TypeId>,
    ) -> RustMaterialization {
        self.composite_storage_materialization(fields, LambdaStorageFamily::UnknownOrigin)
    }

    fn composite_storage_materialization(
        self,
        fields: impl IntoIterator<Item = TypeId>,
        family: LambdaStorageFamily,
    ) -> RustMaterialization {
        if fields.into_iter().all(|field| {
            self.storage_supported(field, family).is_ok()
                && !matches!(self.materialization(field), RustMaterialization::Gap)
        }) {
            RustMaterialization::Share
        } else {
            RustMaterialization::Gap
        }
    }

    fn composite_global_materialization(
        self,
        fields: impl IntoIterator<Item = TypeId>,
    ) -> RustMaterialization {
        if fields
            .into_iter()
            .all(|field| self.exact_root_global_field_supported(field))
        {
            RustMaterialization::Share
        } else {
            RustMaterialization::Gap
        }
    }

    fn exact_root_global_field_supported(self, ty: TypeId) -> bool {
        materialization_is_owned_payload(self.materialization_for(
            ty,
            RustMaterialSource::ExactGlobalRoot,
            RustMaterialIntent::Read,
        ))
    }

    pub fn contains_function_payload(self, ty: TypeId) -> bool {
        self.contains_function_payload_inner(ty, &mut BTreeSet::new())
    }

    fn contains_function_payload_inner(self, ty: TypeId, active: &mut BTreeSet<TypeId>) -> bool {
        if !active.insert(ty) {
            return false;
        }
        let contains = match self.program.type_arena.data(ty) {
            TypeData::Function(_) => true,
            TypeData::List(elem)
            | TypeData::Slice(elem)
            | TypeData::Optional(elem)
            | TypeData::Array { elem, .. } => self.contains_function_payload_inner(*elem, active),
            TypeData::Map { key, value, .. } => {
                self.contains_function_payload_inner(*key, active)
                    || self.contains_function_payload_inner(*value, active)
            }
            TypeData::Tuple(elems) => elems
                .iter()
                .any(|elem| self.contains_function_payload_inner(*elem, active)),
            TypeData::Aggregate(id) | TypeData::DataRef(id) => self
                .program
                .aggregate(*id)
                .fields
                .iter()
                .any(|field| self.contains_function_payload_inner(field.ty, active)),
            TypeData::Enum(id) => self.program.enum_decl(*id).variants.iter().any(|variant| {
                Self::variant_field_tys(variant)
                    .any(|ty| self.contains_function_payload_inner(ty, active))
            }),
            TypeData::Extern(id) => {
                let decl = self.program.extern_type(*id);
                decl.fields
                    .iter()
                    .any(|field| self.contains_function_payload_inner(field.ty, active))
                    || decl.variants.iter().any(|variant| {
                        Self::variant_field_tys(variant)
                            .any(|ty| self.contains_function_payload_inner(ty, active))
                    })
            }
            TypeData::Int
            | TypeData::Float
            | TypeData::Bool
            | TypeData::Char
            | TypeData::Void
            | TypeData::Any
            | TypeData::String
            | TypeData::Dyn(_) => false,
        };
        active.remove(&ty);
        contains
    }

    pub fn storage_supported(
        self,
        ty: TypeId,
        family: LambdaStorageFamily,
    ) -> Result<(), LambdaStorageGap> {
        self.storage_supported_inner(ty, family, false, &mut BTreeSet::new())
    }

    fn stored_payload_supported(self, ty: TypeId) -> bool {
        self.storage_supported(ty, LambdaStorageFamily::UnknownOrigin)
            .is_ok()
    }

    fn storage_supported_inner(
        self,
        ty: TypeId,
        family: LambdaStorageFamily,
        cycle_broken: bool,
        active: &mut BTreeSet<TypeId>,
    ) -> Result<(), LambdaStorageGap> {
        if !active.insert(ty) {
            return cycle_broken
                .then_some(())
                .ok_or(LambdaStorageGap::UnsupportedType);
        }
        let result = match self.program.type_arena.data(ty) {
            TypeData::Int
            | TypeData::Float
            | TypeData::Bool
            | TypeData::Char
            | TypeData::String
            | TypeData::DataRef(_) => Ok(()),
            TypeData::List(elem) => self.storage_supported_inner(
                *elem,
                nested_storage_family(family, LambdaStorageFamily::ListElement),
                true,
                active,
            ),
            TypeData::Map { key, value, .. } => self
                .storage_supported_inner(
                    *key,
                    nested_storage_family(family, LambdaStorageFamily::MapKey),
                    true,
                    active,
                )
                .and_then(|()| {
                    self.map_key_supported(*key)
                        .then_some(())
                        .ok_or(LambdaStorageGap::MapKeyEqualityHash)
                })
                .and_then(|()| {
                    self.storage_supported_inner(
                        *value,
                        nested_storage_family(family, LambdaStorageFamily::MapValue),
                        true,
                        active,
                    )
                }),
            TypeData::Optional(inner) => self.storage_supported_inner(
                *inner,
                nested_storage_family(family, LambdaStorageFamily::OptionalPayload),
                cycle_broken,
                active,
            ),
            TypeData::Array { elem, .. } => self.storage_supported_inner(
                *elem,
                nested_storage_family(family, LambdaStorageFamily::FixedArrayElement),
                cycle_broken,
                active,
            ),
            TypeData::Tuple(elems) => elems.iter().try_for_each(|elem| {
                self.storage_supported_inner(
                    *elem,
                    nested_storage_family(family, LambdaStorageFamily::TupleField),
                    cycle_broken,
                    active,
                )
            }),
            TypeData::Aggregate(id) => {
                self.program
                    .aggregate(*id)
                    .fields
                    .iter()
                    .try_for_each(|field| {
                        let field_family = match self.program.aggregate(*id).kind {
                            AggregateKind::Struct => LambdaStorageFamily::StructField,
                            AggregateKind::DataRef => LambdaStorageFamily::DataRefProjection,
                        };
                        self.storage_supported_inner(
                            field.ty,
                            nested_storage_family(family, field_family),
                            cycle_broken,
                            active,
                        )
                    })
            }
            TypeData::Enum(id) => {
                self.program
                    .enum_decl(*id)
                    .variants
                    .iter()
                    .try_for_each(|variant| {
                        Self::variant_field_tys(variant).try_for_each(|ty| {
                            self.storage_supported_inner(
                                ty,
                                nested_storage_family(family, LambdaStorageFamily::EnumPayload),
                                cycle_broken,
                                active,
                            )
                        })
                    })
            }
            TypeData::Function(_) if family.allows_function_payload() => Ok(()),
            TypeData::Function(_) => Err(family.lambda_gap()),
            TypeData::Slice(_) => Err(LambdaStorageGap::Lifetime),
            TypeData::Extern(id) if self.program.extern_type(*id).rep == air::ExternRep::Shared => {
                Ok(())
            }
            TypeData::Extern(id) => {
                let decl = self.program.extern_type(*id);
                decl.fields
                    .iter()
                    .map(|field| field.ty)
                    .chain(decl.variants.iter().flat_map(Self::variant_field_tys))
                    .try_for_each(|ty| {
                        self.storage_supported_inner(
                            ty,
                            nested_storage_family(family, LambdaStorageFamily::StructField),
                            cycle_broken,
                            active,
                        )
                    })
            }
            TypeData::Dyn(_) if family.allows_dynamic_owned() => Ok(()),
            TypeData::Void | TypeData::Any | TypeData::Dyn(_) => {
                Err(LambdaStorageGap::UnsupportedType)
            }
        };
        active.remove(&ty);
        result
    }

    pub fn list_supported(self, ty: TypeId) -> bool {
        matches!(self.program.type_arena.data(ty), TypeData::List(_))
            && self
                .storage_supported(ty, LambdaStorageFamily::UnknownOrigin)
                .is_ok()
    }

    fn shareable_value(self, ty: TypeId) -> bool {
        match self.program.type_arena.data(ty) {
            TypeData::String
            | TypeData::Dyn(_)
            | TypeData::DataRef(_)
            | TypeData::List(_)
            | TypeData::Map { .. }
            | TypeData::Slice(_) => true,
            TypeData::Optional(inner) => self.embedded_air_shareable_value(*inner),
            TypeData::Tuple(elems) => elems
                .iter()
                .all(|elem| self.embedded_air_shareable_value(*elem)),
            TypeData::Aggregate(id) => self
                .program
                .aggregate(*id)
                .fields
                .iter()
                .all(|field| self.embedded_air_shareable_value(field.ty)),
            TypeData::Enum(id) => self.enum_shareable(*id),
            TypeData::Extern(id) if self.program.extern_type(*id).rep == air::ExternRep::Shared => {
                true
            }
            _ => false,
        }
    }

    fn embedded_air_shareable_value(self, ty: TypeId) -> bool {
        !matches!(self.program.type_arena.data(ty), TypeData::Slice(_))
            && self.value_place_shareable(ty)
    }

    fn variant_field_tys(variant: &air::VariantDecl) -> Box<dyn Iterator<Item = TypeId> + '_> {
        match &variant.shape {
            VariantShape::Unit => Box::new(std::iter::empty()),
            VariantShape::Tuple(fields) => Box::new(fields.iter().copied()),
            VariantShape::Struct(fields) => Box::new(fields.iter().map(|field| field.ty)),
        }
    }

    fn enum_shareable(self, id: air::EnumId) -> bool {
        self.program.enum_decl(id).variants.iter().all(|variant| {
            let fields: &[TypeId] = match &variant.shape {
                VariantShape::Unit => &[],
                VariantShape::Tuple(fields) => fields,
                VariantShape::Struct(fields) => {
                    return fields
                        .iter()
                        .all(|field| self.embedded_air_shareable_value(field.ty));
                }
            };
            fields
                .iter()
                .all(|field| self.embedded_air_shareable_value(*field))
        })
    }

    pub fn map_supported(self, ty: TypeId) -> bool {
        let TypeData::Map { key, value, .. } = self.program.type_arena.data(ty) else {
            return false;
        };
        self.map_key_supported(*key) && self.map_value_supported(*value)
    }

    pub fn map_key_supported(self, ty: TypeId) -> bool {
        self.map_key_supported_inner(ty, &mut BTreeSet::new())
    }

    fn map_key_supported_inner(self, ty: TypeId, active: &mut BTreeSet<TypeId>) -> bool {
        if !active.insert(ty) {
            return false;
        }
        let supported = match self.program.type_arena.data(ty) {
            TypeData::Int | TypeData::Bool | TypeData::String | TypeData::Char => true,
            TypeData::Tuple(elems) => elems
                .iter()
                .all(|elem| self.map_key_supported_inner(*elem, active)),
            TypeData::Aggregate(id) => {
                let aggregate = self.program.aggregate(*id);
                aggregate.kind == AggregateKind::Struct
                    && aggregate
                        .fields
                        .iter()
                        .all(|field| self.map_key_supported_inner(field.ty, active))
            }
            TypeData::Enum(id) => self.program.enum_decl(*id).variants.iter().all(|variant| {
                Self::variant_field_tys(variant).all(|ty| self.map_key_supported_inner(ty, active))
            }),
            TypeData::Float
            | TypeData::List(_)
            | TypeData::Map { .. }
            | TypeData::Slice(_)
            | TypeData::Optional(_)
            | TypeData::Array { .. }
            | TypeData::DataRef(_)
            | TypeData::Extern(_)
            | TypeData::Function(_)
            | TypeData::Dyn(_)
            | TypeData::Any
            | TypeData::Void => false,
        };
        active.remove(&ty);
        supported
    }

    pub fn map_value_supported(self, ty: TypeId) -> bool {
        self.storage_supported(ty, LambdaStorageFamily::MapValue)
            .is_ok()
    }

    pub fn supports_param_mode(self, ty: TypeId, mode: ParamMode) -> bool {
        match mode {
            ParamMode::Value => match self.program.type_arena.data(ty) {
                TypeData::Optional(inner) => self.supports_param_mode(*inner, mode),
                TypeData::Tuple(elems) => elems
                    .iter()
                    .all(|elem| self.supports_param_mode(*elem, mode)),
                TypeData::Int
                | TypeData::Float
                | TypeData::Bool
                | TypeData::Char
                | TypeData::Void
                | TypeData::String
                | TypeData::Aggregate(_)
                | TypeData::DataRef(_)
                | TypeData::Enum(_)
                | TypeData::Extern(_)
                | TypeData::Array { .. }
                | TypeData::List(_)
                | TypeData::Map { .. }
                | TypeData::Slice(_)
                | TypeData::Function(_) => true,
                TypeData::Any | TypeData::Dyn(_) => false,
            },
            ParamMode::SharedBorrow => match self.program.type_arena.data(ty) {
                TypeData::Optional(inner) => self.supports_param_mode(*inner, mode),
                TypeData::Tuple(_)
                | TypeData::String
                | TypeData::Aggregate(_)
                | TypeData::DataRef(_)
                | TypeData::Enum(_)
                | TypeData::Extern(_)
                | TypeData::Array { .. }
                | TypeData::List(_)
                | TypeData::Map { .. }
                | TypeData::Slice(_) => true,
                TypeData::Int
                | TypeData::Float
                | TypeData::Bool
                | TypeData::Char
                | TypeData::Void
                | TypeData::Any
                | TypeData::Function(_)
                | TypeData::Dyn(_) => false,
            },
            ParamMode::MutBorrow => match self.program.type_arena.data(ty) {
                TypeData::Optional(inner) => self.supports_param_mode(*inner, mode),
                TypeData::Tuple(_)
                | TypeData::Int
                | TypeData::Float
                | TypeData::Bool
                | TypeData::Char
                | TypeData::String
                | TypeData::Aggregate(_)
                | TypeData::DataRef(_)
                | TypeData::Enum(_)
                | TypeData::Extern(_)
                | TypeData::Array { .. }
                | TypeData::List(_)
                | TypeData::Map { .. }
                | TypeData::Slice(_) => true,
                TypeData::Void | TypeData::Any | TypeData::Function(_) | TypeData::Dyn(_) => false,
            },
        }
    }
}

struct StructuralKeyCx<'a> {
    plan: RustRepresentationPlan<'a>,
    types: BTreeSet<TypeId>,
    type_stack: Vec<TypeId>,
    surfaces: BTreeSet<ContractSurfaceId>,
    surface_stack: Vec<ContractSurfaceId>,
}

impl<'a> StructuralKeyCx<'a> {
    fn new(plan: RustRepresentationPlan<'a>) -> Self {
        Self {
            plan,
            types: BTreeSet::new(),
            type_stack: vec![],
            surfaces: BTreeSet::new(),
            surface_stack: vec![],
        }
    }

    fn witness_target(&mut self, target: &ContractWitnessTarget) -> String {
        match target {
            ContractWitnessTarget::Function { function } => {
                format!("function:{}", self.function(*function))
            }
            ContractWitnessTarget::IteratorFunction { function } => {
                format!("iterator:{}", self.function(*function))
            }
            ContractWitnessTarget::Extern { function } => {
                format!("extern:{}", self.extern_function(*function))
            }
            ContractWitnessTarget::Promoted { fields, target } => format!(
                "promoted:{}:{}",
                fields
                    .iter()
                    .map(|field| field.index().to_string())
                    .collect::<Vec<_>>()
                    .join("."),
                self.witness_target(target)
            ),
        }
    }

    fn function(&mut self, id: air::FunctionId) -> String {
        let function = self.plan.program.function(id);
        let kind = match function.kind {
            air::FunctionKind::Normal => "normal".to_string(),
            air::FunctionKind::Method => "method".to_string(),
            air::FunctionKind::ExtendMethod => "extend".to_string(),
            air::FunctionKind::Lambda(id) => format!("lambda:{}", id.index()),
            air::FunctionKind::Helper => "helper".to_string(),
            air::FunctionKind::GlobalInit(id) => format!("global-init:{}", id.index()),
        };
        let owner = function
            .owner
            .as_ref()
            .map_or("", |owner| owner.name.as_str());
        let specialization =
            function
                .specialization
                .as_ref()
                .map_or_else(String::new, |specialization| {
                    format!(
                        "<{};{}>",
                        specialization
                            .type_args
                            .iter()
                            .map(|ty| self.ty(*ty))
                            .collect::<Vec<_>>()
                            .join(","),
                        specialization
                            .const_args
                            .iter()
                            .map(Self::const_value)
                            .collect::<Vec<_>>()
                            .join(",")
                    )
                });
        let params = function
            .signature
            .params
            .iter()
            .map(|param| {
                format!(
                    "{:?}:{:?}:{:?}:{}",
                    param.role,
                    param.mode,
                    param.escape,
                    self.ty(param.ty)
                )
            })
            .collect::<Vec<_>>()
            .join(",");
        let ret = match function.signature.return_mode {
            air::ReturnMode::Value(ty) => format!("value:{}", self.ty(ty)),
            air::ReturnMode::Place(ty) => format!("place:{}", self.ty(ty)),
        };
        format!(
            "{}::{owner}::{}:{kind}{specialization}({params})->{ret}",
            self.module(function.module),
            function.name.as_str()
        )
    }

    fn extern_function(&mut self, id: air::ExternId) -> String {
        let function = self.plan.program.extern_decl(id);
        let member = match &function.member {
            air::ExternMember::FreeFunction => "free".to_string(),
            air::ExternMember::FieldGetter {
                owner,
                receiver,
                computed,
            } => format!(
                "field-get:{}:{:?}:{}:{}",
                self.extern_type(*owner),
                receiver.mode,
                self.ty(receiver.ty),
                computed
            ),
            air::ExternMember::FieldSetter { owner, receiver } => format!(
                "field-set:{}:{:?}:{}",
                self.extern_type(*owner),
                receiver.mode,
                self.ty(receiver.ty)
            ),
            air::ExternMember::Method { owner, receiver } => format!(
                "method:{}:{:?}:{}",
                self.extern_type(*owner),
                receiver.mode,
                self.ty(receiver.ty)
            ),
            air::ExternMember::StaticMethod { owner } => {
                format!("static:{}", self.extern_type(*owner))
            }
            air::ExternMember::Init { owner } => format!("init:{}", self.extern_type(*owner)),
            air::ExternMember::UnaryOperator {
                owner,
                receiver,
                op,
            } => format!(
                "unary:{}:{op:?}:{:?}:{}",
                self.extern_type(*owner),
                receiver.mode,
                self.ty(receiver.ty)
            ),
            air::ExternMember::BinaryOperator {
                owner,
                receiver,
                op,
                self_on_right,
            } => format!(
                "binary:{}:{op:?}:{self_on_right}:{:?}:{}",
                self.extern_type(*owner),
                receiver.mode,
                self.ty(receiver.ty)
            ),
        };
        let params = function
            .params
            .iter()
            .map(|param| format!("{:?}:{:?}:{}", param.mode, param.escape, self.ty(param.ty)))
            .collect::<Vec<_>>()
            .join(",");
        format!(
            "{}::{}:{member}({params})->{}",
            self.module(function.module),
            function.name.as_str(),
            self.ty(function.return_type)
        )
    }

    fn extern_type(&mut self, id: air::ExternTypeId) -> String {
        let ty = self.plan.program.extern_type(id);
        self.named(ty.module, ty.name.as_str(), &ty.type_args, &ty.const_args)
    }

    fn module(&self, id: air::ModuleId) -> String {
        self.plan
            .program
            .module(id)
            .path
            .iter()
            .map(anvyx_frontend::ast::Ident::as_str)
            .collect::<Vec<_>>()
            .join("::")
    }

    fn const_value(value: &air::ConstValue) -> String {
        match value {
            air::ConstValue::Int(value) => format!("int:{value}"),
            air::ConstValue::Float(value) => format!("float:{:016x}", value.to_bits()),
            air::ConstValue::Bool(value) => format!("bool:{value}"),
            air::ConstValue::String(value) => format!("string:{value:?}"),
            air::ConstValue::Char(value) => format!("char:{}", *value as u32),
            air::ConstValue::Nil => "nil".to_string(),
        }
    }

    fn surface(&mut self, id: ContractSurfaceId) -> String {
        let surface = self.plan.program.contract_surface(id);
        if !self.surfaces.insert(id) {
            let depth = self
                .surface_stack
                .iter()
                .position(|active| *active == id)
                .expect("active surface must be on structural-key stack");
            return format!("recursive-surface:{depth}");
        }
        self.surface_stack.push(id);
        let slots = surface
            .slots
            .iter()
            .map(|slot| {
                let params = slot
                    .params
                    .iter()
                    .map(|param| {
                        format!(
                            "{:?}:{}:{:?}:{}",
                            param.mode,
                            self.ty(param.ty),
                            param.escape,
                            param.cast_accept
                        )
                    })
                    .collect::<Vec<_>>()
                    .join(",");
                let ret = match slot.ret {
                    air::ContractReturnDecl::Value(ty) => format!("value:{}", self.ty(ty)),
                    air::ContractReturnDecl::Place(ty) => format!("place:{}", self.ty(ty)),
                    air::ContractReturnDecl::Iter => "iter".into(),
                };
                format!(
                    "{}:{:?}({})->{}",
                    slot.name.as_str(),
                    slot.receiver,
                    params,
                    ret
                )
            })
            .collect::<Vec<_>>()
            .join(";");
        self.surfaces.remove(&id);
        self.surface_stack.pop();
        slots
    }

    fn ty(&mut self, ty: TypeId) -> String {
        if !self.types.insert(ty) {
            let depth = self
                .type_stack
                .iter()
                .position(|active| *active == ty)
                .expect("active type must be on structural-key stack");
            return format!("recursive-type:{depth}");
        }
        self.type_stack.push(ty);
        let key = match self.plan.program.type_arena.data(ty) {
            TypeData::Int => "int".into(),
            TypeData::Float => "float".into(),
            TypeData::Bool => "bool".into(),
            TypeData::Char => "char".into(),
            TypeData::String => "string".into(),
            TypeData::Void => "void".into(),
            TypeData::Any => "any".into(),
            TypeData::Optional(inner) => format!("optional<{}>", self.ty(*inner)),
            TypeData::Array { elem, len } => format!("array<{},{}>", self.ty(*elem), len),
            TypeData::List(elem) => format!("list<{}>", self.ty(*elem)),
            TypeData::Slice(elem) => format!("slice<{}>", self.ty(*elem)),
            TypeData::Map { key, value, .. } => {
                format!("map<{},{}>", self.ty(*key), self.ty(*value))
            }
            TypeData::Tuple(fields) => format!(
                "tuple<{}>",
                fields
                    .iter()
                    .map(|field| self.ty(*field))
                    .collect::<Vec<_>>()
                    .join(",")
            ),
            TypeData::Aggregate(id) => {
                let decl = self.plan.program.aggregate(*id);
                format!(
                    "aggregate:{}",
                    self.named(
                        decl.module,
                        decl.name.as_str(),
                        &decl.type_args,
                        &decl.const_args
                    )
                )
            }
            TypeData::Enum(id) => {
                let decl = self.plan.program.enum_decl(*id);
                format!(
                    "enum:{}",
                    self.named(
                        decl.module,
                        decl.name.as_str(),
                        &decl.type_args,
                        &decl.const_args
                    )
                )
            }
            TypeData::Extern(id) => {
                let decl = self.plan.program.extern_type(*id);
                format!(
                    "extern:{}",
                    self.named(
                        decl.module,
                        decl.name.as_str(),
                        &decl.type_args,
                        &decl.const_args
                    )
                )
            }
            TypeData::Function(sig) => {
                let params = sig
                    .params
                    .iter()
                    .map(|param| {
                        format!("{:?}:{}:{:?}", param.mode, self.ty(param.ty), param.escape)
                    })
                    .collect::<Vec<_>>()
                    .join(",");
                let ret = match sig.ret {
                    air::ReturnMode::Value(ty) => format!("value:{}", self.ty(ty)),
                    air::ReturnMode::Place(ty) => format!("place:{}", self.ty(ty)),
                };
                format!("fn({params})->{ret}")
            }
            TypeData::DataRef(id) => {
                let decl = self.plan.program.aggregate(*id);
                format!(
                    "dataref:{}",
                    self.named(
                        decl.module,
                        decl.name.as_str(),
                        &decl.type_args,
                        &decl.const_args
                    )
                )
            }
            TypeData::Dyn(surface) => format!("dyn:{}", self.surface(*surface)),
        };
        self.types.remove(&ty);
        self.type_stack.pop();
        key
    }

    fn named(
        &mut self,
        module: air::ModuleId,
        name: &str,
        type_args: &[TypeId],
        const_args: &[String],
    ) -> String {
        format!(
            "{}::{name}<{};{}>",
            self.plan
                .program
                .module(module)
                .path
                .iter()
                .map(anvyx_frontend::ast::Ident::as_str)
                .collect::<Vec<_>>()
                .join("::"),
            type_args
                .iter()
                .map(|arg| self.ty(*arg))
                .collect::<Vec<_>>()
                .join(","),
            const_args.join(",")
        )
    }
}

#[derive(Debug, Clone, Default)]
struct ComputedDynamicLayouts {
    carriers: BTreeMap<ContractSurfaceId, Result<RustApproxLayout, RustLayoutGap>>,
    payloads: BTreeMap<(ContractSurfaceId, TypeId), Result<RustApproxLayout, RustLayoutGap>>,
}

struct ComputedDynamicLifecycles {
    carriers: BTreeMap<ContractSurfaceId, Result<RustLifecyclePlan, RustLifecycleGap>>,
    variants: BTreeMap<(ContractSurfaceId, TypeId), Result<RustLifecyclePlan, RustLifecycleGap>>,
}

struct ApproxLayoutCx<'a> {
    plan: RustRepresentationPlan<'a>,
    storage: &'a BTreeMap<(ContractSurfaceId, TypeId), RustPayloadStorage>,
    carrier_layouts: BTreeMap<ContractSurfaceId, Result<RustApproxLayout, RustLayoutGap>>,
    visiting_carriers: BTreeSet<ContractSurfaceId>,
    visiting_types: BTreeSet<TypeId>,
}

impl ApproxLayoutCx<'_> {
    fn carrier_layout(
        &mut self,
        surface: ContractSurfaceId,
    ) -> Result<RustApproxLayout, RustLayoutGap> {
        if let Some(layout) = self.carrier_layouts.get(&surface) {
            return *layout;
        }
        if !self.visiting_carriers.insert(surface) {
            return Err(RustLayoutGap::RecursiveCarrier(surface));
        }
        let payloads = self
            .plan
            .program
            .contract_witnesses
            .iter()
            .filter(|witness| witness.key.surface == surface)
            .map(|witness| witness.key.concrete_ty)
            .collect::<Vec<_>>();
        let layout = (|| {
            let mut payload = RustApproxLayout { size: 0, align: 1 };
            for ty in payloads {
                let key = (surface, ty);
                let layout = match self.storage[&key] {
                    RustPayloadStorage::Boxed => Ok(self.plan.pointer_layout()),
                    RustPayloadStorage::Inline => self.type_layout(ty),
                }?;
                payload.size = payload.size.max(layout.size);
                payload.align = payload.align.max(layout.align);
            }
            enum_layout(self.plan.discriminant_layout(), payload)
        })();
        self.visiting_carriers.remove(&surface);
        self.carrier_layouts.insert(surface, layout);
        layout
    }

    fn type_layout(&mut self, ty: TypeId) -> Result<RustApproxLayout, RustLayoutGap> {
        if !self.visiting_types.insert(ty) {
            return Err(RustLayoutGap::RecursiveInline(ty));
        }
        let layout = (|| match self.plan.program.type_arena.data(ty) {
            TypeData::Int | TypeData::Float => Ok(RustApproxLayout { size: 8, align: 8 }),
            TypeData::Bool => Ok(RustApproxLayout { size: 1, align: 1 }),
            TypeData::Char => Ok(RustApproxLayout { size: 4, align: 4 }),
            TypeData::Void => Ok(RustApproxLayout { size: 0, align: 1 }),
            TypeData::String => repeat_layout(self.plan.pointer_layout(), 3),
            TypeData::List(_) | TypeData::Map { .. } | TypeData::DataRef(_) => {
                Ok(self.plan.pointer_layout())
            }
            TypeData::Slice(_) => repeat_layout(self.plan.pointer_layout(), 2),
            TypeData::Function(_) => Err(RustLayoutGap::FunctionLayoutUnknown(ty)),
            TypeData::Optional(inner) => {
                let payload = self.type_layout(*inner)?;
                enum_layout(RustApproxLayout { size: 1, align: 1 }, payload)
            }
            TypeData::Array { elem, len } => {
                let elem = self.type_layout(*elem)?;
                repeat_layout(
                    elem,
                    u64::try_from(*len).map_err(|_| RustLayoutGap::ArithmeticOverflow)?,
                )
            }
            TypeData::Tuple(fields) => self.fields_layout(fields.iter().copied()),
            TypeData::Aggregate(id) => self.fields_layout(
                self.plan
                    .program
                    .aggregate(*id)
                    .fields
                    .iter()
                    .map(|field| field.ty),
            ),
            TypeData::Enum(id) => {
                let mut payload = RustApproxLayout { size: 0, align: 1 };
                for variant in &self.plan.program.enum_decl(*id).variants {
                    let layout =
                        self.fields_layout(RustRepresentationPlan::variant_field_tys(variant))?;
                    payload.size = payload.size.max(layout.size);
                    payload.align = payload.align.max(layout.align);
                }
                enum_layout(self.plan.discriminant_layout(), payload)
            }
            TypeData::Dyn(surface) => self.carrier_layout(*surface),
            TypeData::Extern(id)
                if self.plan.program.extern_type(*id).rep == air::ExternRep::Shared =>
            {
                Ok(self.plan.pointer_layout())
            }
            TypeData::Extern(id) => self
                .plan
                .program
                .extern_type(*id)
                .layout
                .map(|layout| RustApproxLayout {
                    size: layout.size,
                    align: layout.align,
                })
                .ok_or(RustLayoutGap::ProviderInlineLayoutUnknown(*id)),
            TypeData::Any => Err(RustLayoutGap::UnsupportedType(ty)),
        })();
        self.visiting_types.remove(&ty);
        layout
    }

    fn fields_layout(
        &mut self,
        fields: impl IntoIterator<Item = TypeId>,
    ) -> Result<RustApproxLayout, RustLayoutGap> {
        let mut layout = RustApproxLayout { size: 0, align: 1 };
        for field in fields {
            let field = self.type_layout(field)?;
            layout.size = align_up(layout.size, field.align)?
                .checked_add(field.size)
                .ok_or(RustLayoutGap::ArithmeticOverflow)?;
            layout.align = layout.align.max(field.align);
        }
        layout.size = align_up(layout.size, layout.align)?;
        Ok(layout)
    }
}

fn tracked_lifecycle() -> RustLifecyclePlan {
    RustLifecyclePlan {
        heap: RustHeapLifecycle {
            owns_edges: true,
            trace: true,
            drop: true,
        },
        context: true,
    }
}

fn merge_lifecycle(left: RustLifecyclePlan, right: RustLifecyclePlan) -> RustLifecyclePlan {
    RustLifecyclePlan {
        heap: RustHeapLifecycle {
            owns_edges: left.heap.owns_edges || right.heap.owns_edges,
            trace: left.heap.trace || right.heap.trace,
            drop: left.heap.drop || right.heap.drop,
        },
        context: left.context || right.context,
    }
}

fn merge_lifecycles(
    left: Result<RustLifecyclePlan, RustLifecycleGap>,
    right: Result<RustLifecyclePlan, RustLifecycleGap>,
) -> Result<RustLifecyclePlan, RustLifecycleGap> {
    match (left, right) {
        (Ok(left), Ok(right)) => Ok(merge_lifecycle(left, right)),
        (Err(gap), _) | (_, Err(gap)) => Err(gap),
    }
}

fn repeat_layout(layout: RustApproxLayout, count: u64) -> Result<RustApproxLayout, RustLayoutGap> {
    Ok(RustApproxLayout {
        size: layout
            .size
            .checked_mul(count)
            .ok_or(RustLayoutGap::ArithmeticOverflow)?,
        align: layout.align,
    })
}

fn enum_layout(
    discriminant: RustApproxLayout,
    payload: RustApproxLayout,
) -> Result<RustApproxLayout, RustLayoutGap> {
    let align = discriminant.align.max(payload.align);
    let payload_offset = align_up(discriminant.size, payload.align)?;
    let size = payload_offset
        .checked_add(payload.size)
        .ok_or(RustLayoutGap::ArithmeticOverflow)?;
    Ok(RustApproxLayout {
        size: align_up(size, align)?,
        align,
    })
}

fn align_up(value: u64, align: u64) -> Result<u64, RustLayoutGap> {
    debug_assert!(align.is_power_of_two());
    let mask = align - 1;
    value
        .checked_add(mask)
        .map(|value| value & !mask)
        .ok_or(RustLayoutGap::ArithmeticOverflow)
}

fn strongly_connected_components(
    graph: &BTreeMap<RustLayoutNode, BTreeSet<RustLayoutNode>>,
) -> BTreeMap<RustLayoutNode, usize> {
    fn finish(
        node: RustLayoutNode,
        graph: &BTreeMap<RustLayoutNode, BTreeSet<RustLayoutNode>>,
        seen: &mut BTreeSet<RustLayoutNode>,
        order: &mut Vec<RustLayoutNode>,
    ) {
        if !seen.insert(node) {
            return;
        }
        if let Some(edges) = graph.get(&node) {
            for &next in edges {
                finish(next, graph, seen, order);
            }
        }
        order.push(node);
    }

    fn assign(
        node: RustLayoutNode,
        component: usize,
        reverse: &BTreeMap<RustLayoutNode, BTreeSet<RustLayoutNode>>,
        components: &mut BTreeMap<RustLayoutNode, usize>,
    ) {
        if components.insert(node, component).is_some() {
            return;
        }
        if let Some(edges) = reverse.get(&node) {
            for &next in edges {
                if !components.contains_key(&next) {
                    assign(next, component, reverse, components);
                }
            }
        }
    }

    let mut reverse = BTreeMap::<_, BTreeSet<_>>::new();
    for (&node, edges) in graph {
        reverse.entry(node).or_default();
        for &next in edges {
            reverse.entry(next).or_default().insert(node);
        }
    }
    let mut seen = BTreeSet::new();
    let mut order = vec![];
    for &node in reverse.keys() {
        finish(node, graph, &mut seen, &mut order);
    }
    let mut components = BTreeMap::new();
    let mut component = 0;
    for node in order.into_iter().rev() {
        if components.contains_key(&node) {
            continue;
        }
        assign(node, component, &reverse, &mut components);
        component += 1;
    }
    components
}

fn component_is_cyclic(
    graph: &BTreeMap<RustLayoutNode, BTreeSet<RustLayoutNode>>,
    node: RustLayoutNode,
    components: &BTreeMap<RustLayoutNode, usize>,
) -> bool {
    graph[&node].contains(&node)
        || components
            .values()
            .filter(|component| **component == components[&node])
            .count()
            > 1
}

fn propagate_weakening_storage(
    storage: &mut BTreeMap<(ContractSurfaceId, TypeId), RustPayloadStorage>,
    classes: &BTreeMap<ContractSurfaceId, ContractSurfaceId>,
) {
    let boxed = storage
        .iter()
        .filter(|(_, storage)| **storage == RustPayloadStorage::Boxed)
        .map(|((surface, ty), _)| (classes[surface], *ty))
        .collect::<BTreeSet<_>>();
    for ((surface, ty), storage) in storage {
        if boxed.contains(&(classes[surface], *ty)) {
            *storage = RustPayloadStorage::Boxed;
        }
    }
}

fn carrier_declaration_order(carriers: &[RustDynamicCarrierPlan]) -> Vec<ContractSurfaceId> {
    fn visit(
        surface: ContractSurfaceId,
        carriers: &[RustDynamicCarrierPlan],
        visiting: &mut BTreeSet<ContractSurfaceId>,
        visited: &mut BTreeSet<ContractSurfaceId>,
        order: &mut Vec<ContractSurfaceId>,
    ) {
        if visited.contains(&surface) || !visiting.insert(surface) {
            return;
        }
        if let Some(carrier) = carriers.iter().find(|carrier| carrier.surface == surface) {
            for dependency in &carrier.dependencies {
                visit(*dependency, carriers, visiting, visited, order);
            }
        }
        visiting.remove(&surface);
        visited.insert(surface);
        order.push(surface);
    }

    let mut order = vec![];
    let mut visiting = BTreeSet::new();
    let mut visited = BTreeSet::new();
    for carrier in carriers {
        visit(
            carrier.surface,
            carriers,
            &mut visiting,
            &mut visited,
            &mut order,
        );
    }
    order
}

#[derive(Debug, Clone, Copy)]
pub struct RirRustRepPolicy<'a> {
    program: &'a RirProgram,
}

impl<'a> RirRustRepPolicy<'a> {
    pub fn new(program: &'a RirProgram) -> Self {
        Self { program }
    }

    pub fn value_rep(self, ty: RirTypeId) -> RustValueRep {
        match self.ty(ty) {
            RirType::Int | RirType::Float | RirType::Bool | RirType::Char | RirType::Void => {
                RustValueRep::InlineCopy
            }
            RirType::String => RustValueRep::CowString,
            RirType::Array { .. } => RustValueRep::InlineArray,
            RirType::List(_) => RustValueRep::CowList,
            RirType::Map { .. } => RustValueRep::CowMap,
            RirType::Slice(_) => RustValueRep::Opaque,
            RirType::Lambda(sig) if self.lambda_sig_copyable(sig) => RustValueRep::InlineCopy,
            RirType::Option(_) | RirType::Lambda(_) => RustValueRep::InlineEnum,
            RirType::Struct(_) | RirType::Tuple(_) => RustValueRep::InlineStruct,
            RirType::DataRef(_) => RustValueRep::HeapHandle,
            RirType::Enum(id) => self.enum_rep(id),
        }
    }

    pub fn cow_value(self, ty: RirTypeId) -> bool {
        matches!(
            self.value_rep(ty),
            RustValueRep::CowString | RustValueRep::CowList | RustValueRep::CowMap
        )
    }

    pub fn param_abi(self, semantic: RirParamSemantic) -> RirParamAbi {
        match semantic {
            RirParamSemantic::Value => RirParamAbi::Value,
            RirParamSemantic::SharedBorrow => RirParamAbi::SharedBorrow,
            RirParamSemantic::MutBorrow => RirParamAbi::MutBorrow,
            RirParamSemantic::MutPlace => RirParamAbi::MutPlace,
            RirParamSemantic::DynBorrow => RirParamAbi::DynBorrow,
            RirParamSemantic::ScopedLambda => RirParamAbi::ScopedLambda,
            RirParamSemantic::EscapingLambda => RirParamAbi::EscapingLambda,
            RirParamSemantic::AnvCallback => RirParamAbi::AnvCallback,
            RirParamSemantic::StackCell => RirParamAbi::StackCell,
            RirParamSemantic::HeapCell => RirParamAbi::HeapCell,
            RirParamSemantic::ScopedPlaceCell => RirParamAbi::ScopedPlaceCell,
        }
    }

    pub fn supports_param(self, ty: RirTypeId, semantic: RirParamSemantic) -> bool {
        self.ty_opt(ty)
            .is_some_and(|ty_data| self.supports_type_semantic(ty_data, semantic))
    }

    pub fn call_arg_abi(self, ty: RirTypeId, semantic: RirParamSemantic) -> Option<RirParamAbi> {
        self.supports_param(ty, semantic)
            .then(|| self.param_abi(semantic))
    }

    pub fn shareable_value(self, ty: RirTypeId) -> bool {
        self.copyable(ty)
            || match self.ty(ty) {
                RirType::String
                | RirType::DataRef(_)
                | RirType::List(_)
                | RirType::Map { .. }
                | RirType::Slice(_) => true,
                RirType::Option(inner) | RirType::Array { elem: inner, .. } => {
                    self.embedded_shareable_value(inner)
                }
                RirType::Lambda(sig) => self.lambda_sig_cloneable(sig),
                RirType::Struct(id) => self.program.structs[id.index()]
                    .fields
                    .iter()
                    .all(|field| self.embedded_shareable_value(field.ty)),
                RirType::Tuple(id) => self.program.tuples[id.index()]
                    .fields
                    .iter()
                    .all(|field| self.embedded_shareable_value(field.ty)),
                RirType::Enum(id) if self.program.dyn_carrier_for_enum(id).is_some() => true,
                RirType::Enum(id) => {
                    self.program.enums[id.index()]
                        .variants
                        .iter()
                        .all(|variant| {
                            variant
                                .fields
                                .iter()
                                .all(|field| self.embedded_shareable_value(field.ty))
                        })
                }
                _ => false,
            }
    }

    fn embedded_shareable_value(self, ty: RirTypeId) -> bool {
        !matches!(self.ty(ty), RirType::Slice(_)) && self.shareable_value(ty)
    }

    pub fn value_from_ref_supported(self, ty: RirTypeId) -> bool {
        !matches!(
            self.materialization_for(ty, RustMaterialSource::Value, RustMaterialIntent::Read),
            RustMaterialization::BorrowGuard | RustMaterialization::Gap
        )
    }

    pub fn materialization_for(
        self,
        ty: RirTypeId,
        source: RustMaterialSource,
        intent: RustMaterialIntent,
    ) -> RustMaterialization {
        self.materialization_plan_for(ty, source, intent)
            .materialization
    }

    pub fn materialization_plan_for(
        self,
        ty: RirTypeId,
        source: RustMaterialSource,
        intent: RustMaterialIntent,
    ) -> RustMaterialPlan {
        let materialization = match (source, intent) {
            (RustMaterialSource::Value, RustMaterialIntent::Read) => self.materialization(ty),
            (
                RustMaterialSource::ExactGlobalRoot,
                RustMaterialIntent::Read | RustMaterialIntent::Store,
            ) => self.exact_root_global_materialization(ty),
            (RustMaterialSource::StoredPayload, RustMaterialIntent::Store) => {
                if self.stored_payload_supported(ty) {
                    self.materialization(ty)
                } else {
                    RustMaterialization::Gap
                }
            }
            (RustMaterialSource::DataRefMutPlace, RustMaterialIntent::MutPlacePayload) => {
                match self.ty_opt(ty) {
                    Some(RirType::Int | RirType::Float | RirType::Bool | RirType::Char) => {
                        RustMaterialization::Copy
                    }
                    Some(RirType::DataRef(_)) => RustMaterialization::CloneHandle,
                    Some(RirType::Enum(id)) if self.program.dyn_carrier_for_enum(id).is_some() => {
                        RustMaterialization::Share
                    }
                    Some(RirType::Struct(_) | RirType::Tuple(_)) => self.materialization(ty),
                    _ => RustMaterialization::Gap,
                }
            }
            _ => RustMaterialization::Gap,
        };
        RustMaterialPlan {
            materialization,
            gap: matches!(materialization, RustMaterialization::Gap)
                .then(|| self.material_gap(ty, source)),
        }
    }

    fn material_gap(self, ty: RirTypeId, source: RustMaterialSource) -> RustMaterialGap {
        match source {
            RustMaterialSource::ExactGlobalRoot => self.global_material_gap(ty),
            RustMaterialSource::StoredPayload => self
                .storage_supported(ty, LambdaStorageFamily::UnknownOrigin)
                .err()
                .map_or(RustMaterialGap::UnsupportedType, RustMaterialGap::from),
            _ => RustMaterialGap::UnsupportedType,
        }
    }

    fn global_material_gap(self, ty: RirTypeId) -> RustMaterialGap {
        if self.contains_function_payload(ty) {
            return self
                .storage_supported(ty, LambdaStorageFamily::GlobalRoot)
                .err()
                .map_or(RustMaterialGap::UnsupportedType, RustMaterialGap::from);
        }
        match self.ty_opt(ty) {
            Some(RirType::Void | RirType::Lambda(_)) | None => RustMaterialGap::UnsupportedType,
            Some(RirType::Option(inner) | RirType::Array { elem: inner, .. }) => {
                self.global_material_gap(inner)
            }
            Some(RirType::Struct(id)) => self
                .program
                .structs
                .get(id.index())
                .map_or(RustMaterialGap::UnsupportedType, |strukt| {
                    self.first_global_material_gap(strukt.fields.iter().map(|field| field.ty))
                }),
            Some(RirType::Tuple(id)) => self
                .program
                .tuples
                .get(id.index())
                .map_or(RustMaterialGap::UnsupportedType, |tuple| {
                    self.first_global_material_gap(tuple.fields.iter().map(|field| field.ty))
                }),
            Some(RirType::Enum(id)) => {
                self.program
                    .enums
                    .get(id.index())
                    .map_or(RustMaterialGap::UnsupportedType, |enm| {
                        self.first_global_material_gap(
                            enm.variants
                                .iter()
                                .flat_map(|variant| variant.fields.iter().map(|field| field.ty)),
                        )
                    })
            }
            Some(
                RirType::Int
                | RirType::Float
                | RirType::Bool
                | RirType::Char
                | RirType::String
                | RirType::DataRef(_)
                | RirType::List(_)
                | RirType::Map { .. }
                | RirType::Slice(_),
            ) => RustMaterialGap::UnsupportedRooting,
        }
    }

    fn first_global_material_gap(
        self,
        fields: impl IntoIterator<Item = RirTypeId>,
    ) -> RustMaterialGap {
        fields
            .into_iter()
            .find_map(|field| {
                matches!(
                    self.materialization_for(
                        field,
                        RustMaterialSource::ExactGlobalRoot,
                        RustMaterialIntent::Read,
                    ),
                    RustMaterialization::Gap
                )
                .then(|| self.global_material_gap(field))
            })
            .unwrap_or(RustMaterialGap::UnsupportedRooting)
    }

    fn materialization(self, ty: RirTypeId) -> RustMaterialization {
        if matches!(self.ty(ty), RirType::Void) {
            return RustMaterialization::Gap;
        }
        if self.copyable(ty) {
            return RustMaterialization::Copy;
        }
        match self.ty(ty) {
            RirType::String | RirType::List(_) | RirType::Map { .. } => RustMaterialization::Share,
            RirType::DataRef(_) => RustMaterialization::CloneHandle,
            RirType::Lambda(sig) if self.lambda_sig_cloneable(sig) => {
                RustMaterialization::CloneLambda
            }
            RirType::Option(inner) => self.composite_materialization([inner]),
            RirType::Array { elem, .. } => self
                .composite_storage_materialization([elem], LambdaStorageFamily::FixedArrayElement),
            RirType::Struct(id) if self.program.structs[id.index()].native_path.is_some() => {
                RustMaterialization::CloneHandle
            }
            RirType::Struct(id) => self.composite_storage_materialization(
                self.program.structs[id.index()]
                    .fields
                    .iter()
                    .map(|field| field.ty),
                LambdaStorageFamily::StructField,
            ),
            RirType::Tuple(id) => self.composite_storage_materialization(
                self.program.tuples[id.index()]
                    .fields
                    .iter()
                    .map(|field| field.ty),
                LambdaStorageFamily::TupleField,
            ),
            RirType::Enum(id) if self.program.dyn_carrier_for_enum(id).is_some() => {
                RustMaterialization::Share
            }
            RirType::Enum(id) if self.program.enums[id.index()].native_path.is_some() => {
                RustMaterialization::CloneHandle
            }
            RirType::Enum(id) => self.composite_materialization(
                self.program.enums[id.index()]
                    .variants
                    .iter()
                    .flat_map(|variant| variant.fields.iter().map(|field| field.ty)),
            ),
            RirType::Slice(_) => RustMaterialization::BorrowGuard,
            RirType::Int
            | RirType::Float
            | RirType::Bool
            | RirType::Char
            | RirType::Void
            | RirType::Lambda(_) => RustMaterialization::Gap,
        }
    }

    fn exact_root_global_materialization(self, ty: RirTypeId) -> RustMaterialization {
        let Some(data) = self.ty_opt(ty) else {
            return RustMaterialization::Gap;
        };
        if matches!(data, RirType::Void | RirType::Slice(_)) {
            return RustMaterialization::Gap;
        }
        if self.copyable(ty) {
            return RustMaterialization::Copy;
        }
        match data {
            RirType::String | RirType::DataRef(_) | RirType::Lambda(_) => self.materialization(ty),
            RirType::List(elem) if self.exact_root_global_field_supported(elem) => {
                self.materialization(ty)
            }
            RirType::Map { key, value }
                if self.map_key_supported(key) && self.exact_root_global_field_supported(value) =>
            {
                self.materialization(ty)
            }
            RirType::Option(inner) | RirType::Array { elem: inner, .. } => {
                self.composite_global_materialization([inner])
            }
            RirType::Struct(id) => {
                let Some(strukt) = self.program.structs.get(id.index()) else {
                    return RustMaterialization::Gap;
                };
                self.composite_global_materialization(strukt.fields.iter().map(|field| field.ty))
            }
            RirType::Tuple(id) => {
                let Some(tuple) = self.program.tuples.get(id.index()) else {
                    return RustMaterialization::Gap;
                };
                self.composite_global_materialization(tuple.fields.iter().map(|field| field.ty))
            }
            RirType::Enum(id) => {
                let Some(enm) = self.program.enums.get(id.index()) else {
                    return RustMaterialization::Gap;
                };
                self.composite_global_materialization(
                    enm.variants
                        .iter()
                        .flat_map(|variant| variant.fields.iter().map(|field| field.ty)),
                )
            }
            RirType::Int
            | RirType::Float
            | RirType::Bool
            | RirType::Char
            | RirType::Void
            | RirType::List(_)
            | RirType::Map { .. }
            | RirType::Slice(_) => RustMaterialization::Gap,
        }
    }

    fn composite_global_materialization(
        self,
        fields: impl IntoIterator<Item = RirTypeId>,
    ) -> RustMaterialization {
        if fields
            .into_iter()
            .all(|field| self.exact_root_global_field_supported(field))
        {
            RustMaterialization::Share
        } else {
            RustMaterialization::Gap
        }
    }

    fn exact_root_global_field_supported(self, ty: RirTypeId) -> bool {
        materialization_is_owned_payload(self.materialization_for(
            ty,
            RustMaterialSource::ExactGlobalRoot,
            RustMaterialIntent::Read,
        ))
    }

    fn map_key_supported(self, ty: RirTypeId) -> bool {
        self.map_key_supported_inner(ty, &mut BTreeSet::new())
    }

    fn map_key_supported_inner(self, ty: RirTypeId, active: &mut BTreeSet<RirTypeId>) -> bool {
        if !active.insert(ty) {
            return false;
        }
        let supported = match self.ty_opt(ty) {
            Some(RirType::Int | RirType::Bool | RirType::String | RirType::Char) => true,
            Some(RirType::Struct(id)) => {
                self.program.structs.get(id.index()).is_some_and(|strukt| {
                    strukt.native_path.is_none()
                        && self.record_key_supported(&strukt.fields, active)
                })
            }
            Some(RirType::Tuple(id)) => self
                .program
                .tuples
                .get(id.index())
                .is_some_and(|tuple| self.record_key_supported(&tuple.fields, active)),
            Some(RirType::Enum(id)) => self
                .program
                .enums
                .get(id.index())
                .is_some_and(|enm| self.enum_key_supported(enm, active)),
            Some(
                RirType::Float
                | RirType::List(_)
                | RirType::Map { .. }
                | RirType::Slice(_)
                | RirType::Option(_)
                | RirType::Array { .. }
                | RirType::DataRef(_)
                | RirType::Lambda(_)
                | RirType::Void,
            )
            | None => false,
        };
        active.remove(&ty);
        supported
    }

    fn record_key_supported(self, fields: &[RirField], active: &mut BTreeSet<RirTypeId>) -> bool {
        fields
            .iter()
            .all(|field| self.map_key_supported_inner(field.ty, active))
    }

    fn enum_key_supported(self, enm: &RirEnum, active: &mut BTreeSet<RirTypeId>) -> bool {
        enm.variants
            .iter()
            .all(|variant| self.record_key_supported(&variant.fields, active))
    }

    fn contains_dynamic(self, ty: RirTypeId, active: &mut BTreeSet<RirTypeId>) -> bool {
        if !active.insert(ty) {
            return false;
        }
        let contains = match self.ty_opt(ty) {
            Some(RirType::Enum(id)) if self.program.dyn_carrier_for_enum(id).is_some() => true,
            Some(
                RirType::List(inner)
                | RirType::Slice(inner)
                | RirType::Option(inner)
                | RirType::Array { elem: inner, .. },
            ) => self.contains_dynamic(inner, active),
            Some(RirType::Map { key, value }) => {
                self.contains_dynamic(key, active) || self.contains_dynamic(value, active)
            }
            Some(RirType::Struct(id)) => self.program.structs[id.index()]
                .fields
                .iter()
                .any(|field| self.contains_dynamic(field.ty, active)),
            Some(RirType::Tuple(id)) => self.program.tuples[id.index()]
                .fields
                .iter()
                .any(|field| self.contains_dynamic(field.ty, active)),
            Some(RirType::Enum(id)) => {
                self.program.enums[id.index()]
                    .variants
                    .iter()
                    .any(|variant| {
                        variant
                            .fields
                            .iter()
                            .any(|field| self.contains_dynamic(field.ty, active))
                    })
            }
            _ => false,
        };
        active.remove(&ty);
        contains
    }

    pub fn type_contains_dynamic(self, ty: RirTypeId) -> bool {
        self.contains_dynamic(ty, &mut BTreeSet::new())
    }

    pub fn record_derives(self, fields: &[RirField]) -> Vec<&'static str> {
        let mut derives = vec!["Clone"];
        let contains_dynamic = fields
            .iter()
            .any(|field| self.type_contains_dynamic(field.ty));
        if !contains_dynamic && self.record_key_supported(fields, &mut BTreeSet::new()) {
            derives.extend(["PartialEq", "Eq", "Hash"]);
        }
        derives
    }

    pub fn enum_derives(self, enm: &RirEnum) -> Vec<&'static str> {
        if self.program.dyn_carrier_for_enum(enm.id).is_some() {
            return vec![];
        }
        let mut derives = vec!["Clone"];
        if enm.repr == super::rir::RirEnumRepr::RawInt && !enm.variants.is_empty() {
            derives.push("Copy");
        }
        let contains_dynamic = enm.variants.iter().any(|variant| {
            variant
                .fields
                .iter()
                .any(|field| self.type_contains_dynamic(field.ty))
        });
        let key_supported = !contains_dynamic && self.enum_key_supported(enm, &mut BTreeSet::new());
        if enm.is_unit_only() || key_supported {
            derives.extend(["PartialEq", "Eq"]);
        }
        if key_supported {
            derives.push("Hash");
        }
        derives
    }

    fn composite_materialization(
        self,
        fields: impl IntoIterator<Item = RirTypeId>,
    ) -> RustMaterialization {
        self.composite_storage_materialization(fields, LambdaStorageFamily::UnknownOrigin)
    }

    fn composite_storage_materialization(
        self,
        fields: impl IntoIterator<Item = RirTypeId>,
        family: LambdaStorageFamily,
    ) -> RustMaterialization {
        if fields.into_iter().all(|field| {
            self.storage_supported(field, family).is_ok()
                && !matches!(self.materialization(field), RustMaterialization::Gap)
        }) {
            RustMaterialization::Share
        } else {
            RustMaterialization::Gap
        }
    }

    pub fn contains_function_payload(self, ty: RirTypeId) -> bool {
        self.contains_function_payload_inner(ty, &mut BTreeSet::new())
    }

    fn contains_function_payload_inner(
        self,
        ty: RirTypeId,
        active: &mut BTreeSet<RirTypeId>,
    ) -> bool {
        if !active.insert(ty) {
            return false;
        }
        let contains = match self.ty_opt(ty) {
            Some(RirType::Lambda(_)) => true,
            Some(
                RirType::List(elem)
                | RirType::Slice(elem)
                | RirType::Option(elem)
                | RirType::Array { elem, .. },
            ) => self.contains_function_payload_inner(elem, active),
            Some(RirType::Map { key, value }) => {
                self.contains_function_payload_inner(key, active)
                    || self.contains_function_payload_inner(value, active)
            }
            Some(RirType::Struct(id)) => {
                self.program.structs.get(id.index()).is_some_and(|strukt| {
                    strukt
                        .fields
                        .iter()
                        .any(|field| self.contains_function_payload_inner(field.ty, active))
                })
            }
            Some(RirType::DataRef(id)) => {
                self.program
                    .datarefs
                    .get(id.index())
                    .is_some_and(|dataref| {
                        dataref
                            .fields
                            .iter()
                            .any(|field| self.contains_function_payload_inner(field.ty, active))
                    })
            }
            Some(RirType::Tuple(id)) => self.program.tuples.get(id.index()).is_some_and(|tuple| {
                tuple
                    .fields
                    .iter()
                    .any(|field| self.contains_function_payload_inner(field.ty, active))
            }),
            Some(RirType::Enum(id)) => self.program.enums.get(id.index()).is_some_and(|enm| {
                enm.variants.iter().any(|variant| {
                    variant
                        .fields
                        .iter()
                        .any(|field| self.contains_function_payload_inner(field.ty, active))
                })
            }),
            Some(
                RirType::Int
                | RirType::Float
                | RirType::Bool
                | RirType::Char
                | RirType::Void
                | RirType::String,
            )
            | None => false,
        };
        active.remove(&ty);
        contains
    }

    pub fn storage_supported(
        self,
        ty: RirTypeId,
        family: LambdaStorageFamily,
    ) -> Result<(), LambdaStorageGap> {
        self.storage_supported_inner(ty, family, false, &mut BTreeSet::new())
    }

    fn stored_payload_supported(self, ty: RirTypeId) -> bool {
        self.storage_supported(ty, LambdaStorageFamily::UnknownOrigin)
            .is_ok()
    }

    fn storage_supported_inner(
        self,
        ty: RirTypeId,
        family: LambdaStorageFamily,
        cycle_broken: bool,
        active: &mut BTreeSet<RirTypeId>,
    ) -> Result<(), LambdaStorageGap> {
        if !active.insert(ty) {
            return cycle_broken
                .then_some(())
                .ok_or(LambdaStorageGap::UnsupportedType);
        }
        let result = match self.ty_opt(ty) {
            Some(
                RirType::Int
                | RirType::Float
                | RirType::Bool
                | RirType::Char
                | RirType::String
                | RirType::DataRef(_),
            ) => Ok(()),
            Some(RirType::List(elem)) => self.storage_supported_inner(
                elem,
                nested_storage_family(family, LambdaStorageFamily::ListElement),
                true,
                active,
            ),
            Some(RirType::Map { key, value }) => self
                .storage_supported_inner(
                    key,
                    nested_storage_family(family, LambdaStorageFamily::MapKey),
                    true,
                    active,
                )
                .and_then(|()| {
                    self.map_key_supported(key)
                        .then_some(())
                        .ok_or(LambdaStorageGap::MapKeyEqualityHash)
                })
                .and_then(|()| {
                    self.storage_supported_inner(
                        value,
                        nested_storage_family(family, LambdaStorageFamily::MapValue),
                        true,
                        active,
                    )
                }),
            Some(RirType::Option(inner)) => self.storage_supported_inner(
                inner,
                nested_storage_family(family, LambdaStorageFamily::OptionalPayload),
                cycle_broken,
                active,
            ),
            Some(RirType::Array { elem, .. }) => self.storage_supported_inner(
                elem,
                nested_storage_family(family, LambdaStorageFamily::FixedArrayElement),
                cycle_broken,
                active,
            ),
            Some(RirType::Struct(id)) => self
                .program
                .structs
                .get(id.index())
                .ok_or(LambdaStorageGap::UnsupportedType)
                .and_then(|strukt| {
                    strukt.fields.iter().try_for_each(|field| {
                        self.storage_supported_inner(
                            field.ty,
                            nested_storage_family(family, LambdaStorageFamily::StructField),
                            cycle_broken,
                            active,
                        )
                    })
                }),
            Some(RirType::Tuple(id)) => self
                .program
                .tuples
                .get(id.index())
                .ok_or(LambdaStorageGap::UnsupportedType)
                .and_then(|tuple| {
                    tuple.fields.iter().try_for_each(|field| {
                        self.storage_supported_inner(
                            field.ty,
                            nested_storage_family(family, LambdaStorageFamily::TupleField),
                            cycle_broken,
                            active,
                        )
                    })
                }),
            Some(RirType::Enum(id)) if self.program.dyn_carrier_for_enum(id).is_some() => Ok(()),
            Some(RirType::Enum(id)) => self
                .program
                .enums
                .get(id.index())
                .ok_or(LambdaStorageGap::UnsupportedType)
                .and_then(|enm| {
                    enm.variants.iter().try_for_each(|variant| {
                        variant.fields.iter().try_for_each(|field| {
                            self.storage_supported_inner(
                                field.ty,
                                nested_storage_family(family, LambdaStorageFamily::EnumPayload),
                                cycle_broken,
                                active,
                            )
                        })
                    })
                }),
            Some(RirType::Lambda(_)) if family.allows_function_payload() => Ok(()),
            Some(RirType::Lambda(_)) => Err(family.lambda_gap()),
            Some(RirType::Slice(_)) => Err(LambdaStorageGap::Lifetime),
            Some(RirType::Void) | None => Err(LambdaStorageGap::UnsupportedType),
        };
        active.remove(&ty);
        result
    }

    pub fn borrow_view(self, ty: RirTypeId) -> RustBorrowView {
        match self.ty(ty) {
            RirType::String => RustBorrowView::Str,
            RirType::Void => RustBorrowView::TargetGap,
            _ => RustBorrowView::Ref,
        }
    }

    pub fn param_ty(self, ty: RirTypeId, abi: RirParamAbi) -> String {
        self.param_ty_with_lifetime(ty, abi, None)
    }

    pub fn callable_param_ty(
        self,
        ty: RirTypeId,
        abi: RirParamAbi,
        escape: RirParamEscape,
    ) -> String {
        if abi == RirParamAbi::Value && escape == RirParamEscape::Escaping {
            self.escaping_value_ty(ty)
        } else {
            self.param_ty(ty, abi)
        }
    }

    pub fn capture_field_ty(self, ty: RirTypeId, abi: RirParamAbi) -> String {
        self.param_ty_with_lifetime(ty, abi, Some("'env"))
    }

    fn param_ty_with_lifetime(
        self,
        ty: RirTypeId,
        abi: RirParamAbi,
        lifetime: Option<&str>,
    ) -> String {
        let reference_lifetime = lifetime.map_or(String::new(), |lifetime| format!("{lifetime} "));
        match abi {
            RirParamAbi::Value => self.rust_ty(ty),
            RirParamAbi::SharedBorrow => match self.borrow_view(ty) {
                RustBorrowView::Str => format!("&{reference_lifetime}str"),
                _ => format!(
                    "&{reference_lifetime}{}",
                    self.rust_ty_with_env_lifetime(ty, lifetime)
                ),
            },
            RirParamAbi::MutBorrow => format!(
                "&{reference_lifetime}mut {}",
                self.rust_ty_with_env_lifetime(ty, lifetime)
            ),
            RirParamAbi::MutPlace => {
                let payload = self.rust_ty(ty);
                format!("{}<'_, 'cx, {payload}>", target::mut_place_ty())
            }
            RirParamAbi::DynBorrow => {
                let Some(RirType::Enum(id)) = self.program.types.get(ty.index()) else {
                    unreachable!("verified dynamic borrow carrier type")
                };
                let carrier = self
                    .program
                    .dyn_carrier_for_enum(*id)
                    .expect("verified dynamic borrow carrier");
                format!("{}<'_, 'cx>", self.program.dyn_borrow_symbol(carrier.id))
            }
            RirParamAbi::ScopedLambda => self.scoped_lambda_ty(ty),
            RirParamAbi::EscapingLambda => self.escaping_lambda_ty(ty),
            RirParamAbi::AnvCallback => self.anv_callback_ty(ty),
            RirParamAbi::StackCell => {
                let payload = self.rust_ty(ty);
                format!(
                    "&{reference_lifetime}{}",
                    target::stack_lambda_cell_ty(&payload)
                )
            }
            RirParamAbi::HeapCell => {
                let payload = self.rust_ty(ty);
                target::handle_ty(&target::lambda_cell_ty(&payload))
            }
            RirParamAbi::ScopedPlaceCell => {
                let payload = self.rust_ty(ty);
                let source_lifetime = lifetime.unwrap_or("'_");
                format!(
                    "&{reference_lifetime}{}",
                    target::scoped_mut_place_cell_ty(source_lifetime, &payload)
                )
            }
        }
    }

    pub(super) fn lambda_sig_layout(self, id: RirLambdaSigId) -> LambdaSigLayout<'a> {
        let variants = self
            .program
            .lambdas_for_sig(id)
            .map(|lambda| LambdaVariantLayout {
                id: lambda.id,
                function: lambda.function,
                storage: lambda.storage,
                captures: lambda.captures.as_slice(),
                trace_action: Self::lambda_trace_action(lambda),
            })
            .collect::<Vec<_>>();
        let captures_self = variants.iter().any(|variant| !variant.captures.is_empty());
        let mut_self = variants.iter().any(|variant| {
            if matches!(variant.storage, RirLambdaStorage::HeapEnv { .. }) {
                return false;
            }
            variant
                .captures
                .iter()
                .any(|capture| capture.semantic == RirParamSemantic::MutBorrow)
        });
        LambdaSigLayout {
            variants,
            captures_self,
            mut_self,
        }
    }

    pub fn scoped_lambda_sig_args_ret(self, sig: RirLambdaSigId) -> (String, String) {
        let sig = &self.program.lambda_sigs[sig.index()];
        let args = match sig.params.as_slice() {
            [] => "()".to_string(),
            [param] => format!("({},)", self.rust_ty(param.ty)),
            params => format!(
                "({})",
                params
                    .iter()
                    .map(|param| self.rust_ty(param.ty))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
        };
        (args, self.rust_ty(sig.ret))
    }

    fn scoped_lambda_ty(self, ty: RirTypeId) -> String {
        let sig = self.expect_lambda_sig(ty);
        let (args, ret) = self.scoped_lambda_sig_args_ret(sig);
        target::scoped_lambda_ty(&args, &ret)
    }

    fn escaping_lambda_ty(self, ty: RirTypeId) -> String {
        let sig = self.expect_lambda_sig(ty);
        let (args, ret) = self.scoped_lambda_sig_args_ret(sig);
        target::escaping_lambda_ty(&args, &ret)
    }

    fn anv_callback_ty(self, ty: RirTypeId) -> String {
        let sig = self.expect_lambda_sig(ty);
        let (args, ret) = self.scoped_lambda_sig_args_ret(sig);
        target::anv_callback_ty(&args, &ret)
    }

    fn expect_lambda_sig(self, ty: RirTypeId) -> RirLambdaSigId {
        let RirType::Lambda(sig) = self.ty(ty) else {
            unreachable!("verified lambda ABI type")
        };
        sig
    }

    pub fn dataref_storage_ty(self, dataref: &RirDataRef) -> String {
        let storage = dataref.storage_symbol();
        if self.dataref_cx_dependent(dataref) {
            format!("{storage}<'cx>")
        } else {
            storage
        }
    }

    pub fn lambda_sig_ty(self, id: RirLambdaSigId) -> String {
        format!(
            "{}{}",
            self.lambda_sig_symbol(id),
            self.lambda_sig_ty_generics(id)
        )
    }

    pub fn lambda_sig_impl_generics(self, id: RirLambdaSigId) -> &'static str {
        match (
            self.lambda_sig_needs_lifetime(id),
            self.lambda_sig_needs_ctx_lifetime(id),
        ) {
            (true, true) => "<'env, 'cx>",
            (true, false) => "<'env>",
            (false, true) => "<'cx>",
            (false, false) => "",
        }
    }

    pub fn lambda_sig_assoc_path(self, id: RirLambdaSigId) -> String {
        format!(
            "{}{}",
            self.lambda_sig_symbol(id),
            self.lambda_sig_assoc_generics(id)
        )
    }

    fn lambda_sig_ty_generics(self, id: RirLambdaSigId) -> &'static str {
        match (
            self.lambda_sig_needs_lifetime(id),
            self.lambda_sig_needs_ctx_lifetime(id),
        ) {
            (true, true) => "<'_, 'cx>",
            (true, false) => "<'_>",
            (false, true) => "<'cx>",
            (false, false) => "",
        }
    }

    fn lambda_sig_assoc_generics(self, id: RirLambdaSigId) -> &'static str {
        match (
            self.lambda_sig_needs_lifetime(id),
            self.lambda_sig_needs_ctx_lifetime(id),
        ) {
            (true, true) => "::<'_, 'cx>",
            (true, false) => "::<'_>",
            (false, true) => "::<'cx>",
            (false, false) => "",
        }
    }

    pub fn lambda_sig_symbol(self, id: RirLambdaSigId) -> String {
        format!("LambdaSig{}", id.index())
    }

    fn rust_ty_with_env_lifetime(self, ty: RirTypeId, lifetime: Option<&str>) -> String {
        match (self.ty(ty), lifetime) {
            (RirType::Lambda(sig), Some(lifetime)) => {
                format!(
                    "{}{}",
                    self.lambda_sig_symbol(sig),
                    self.lambda_sig_ty_generics_with_lifetime(sig, lifetime)
                )
            }
            _ => self.rust_ty(ty),
        }
    }

    fn lambda_sig_ty_generics_with_lifetime(self, id: RirLambdaSigId, lifetime: &str) -> String {
        match (
            self.lambda_sig_needs_lifetime(id),
            self.lambda_sig_needs_ctx_lifetime(id),
        ) {
            (true, true) => format!("<{lifetime}, 'cx>"),
            (true, false) => format!("<{lifetime}>"),
            (false, true) => "<'cx>".into(),
            (false, false) => String::new(),
        }
    }

    pub(super) fn lambda_sig_storage_shape(self, id: RirLambdaSigId) -> LambdaSigStorageShape {
        let mut shape = LambdaSigStorageShape::default();
        for lambda in self.program.lambdas_for_sig(id) {
            shape.heap_env |= matches!(lambda.storage, RirLambdaStorage::HeapEnv { .. });
            shape.lifetime |= lambda.captures.iter().any(|capture| {
                matches!(
                    Self::lambda_capture_layout_edge(lambda, capture),
                    LambdaCaptureLayoutEdge::SharedBorrow
                        | LambdaCaptureLayoutEdge::MutBorrow
                        | LambdaCaptureLayoutEdge::StackCell
                        | LambdaCaptureLayoutEdge::ScopedPlaceCell
                )
            });
        }
        shape
    }

    pub fn lambda_sig_needs_lifetime(self, id: RirLambdaSigId) -> bool {
        self.lambda_sig_storage_shape(id).lifetime
    }

    pub fn lambda_sig_needs_ctx_lifetime(self, id: RirLambdaSigId) -> bool {
        self.lambda_sig_needs_ctx_lifetime_inner(id, &mut BTreeSet::new())
    }

    fn lambda_sig_needs_ctx_lifetime_inner(
        self,
        id: RirLambdaSigId,
        active: &mut BTreeSet<RirLambdaSigId>,
    ) -> bool {
        if !active.insert(id) {
            return false;
        }
        let needs = self.lambda_sig_storage_shape(id).heap_env
            || self.program.lambdas_for_sig(id).any(|lambda| {
                lambda.captures.iter().any(|capture| {
                    let edge = Self::lambda_capture_layout_edge(lambda, capture);
                    self.value_type_cx_dependent(capture.ty, active)
                        || edge == LambdaCaptureLayoutEdge::HeapCell
                        || edge == LambdaCaptureLayoutEdge::ScopedPlaceCell
                })
            });
        active.remove(&id);
        needs
    }

    pub fn lambda_sig_has_heap_env(self, id: RirLambdaSigId) -> bool {
        self.lambda_sig_storage_shape(id).heap_env
    }

    pub(super) fn lambda_sig_has_cell_and_mut_borrow(self, id: RirLambdaSigId) -> bool {
        let mut has_cell = false;
        let mut has_mut_borrow = false;
        for lambda in self.program.lambdas_for_sig(id) {
            has_cell |= lambda.captures.iter().any(|capture| {
                matches!(
                    Self::lambda_capture_layout_edge(lambda, capture),
                    LambdaCaptureLayoutEdge::StackCell
                        | LambdaCaptureLayoutEdge::HeapCell
                        | LambdaCaptureLayoutEdge::ScopedPlaceCell
                )
            });
            has_mut_borrow |= lambda
                .captures
                .iter()
                .any(|capture| capture.abi == RirParamAbi::MutBorrow);
        }
        has_cell && has_mut_borrow
    }

    pub(super) fn lambda_env_field_storage_supported(self, field: &RirLambdaEnvField) -> bool {
        match field.kind {
            RirLambdaEnvFieldKind::Value => self.value_from_ref_supported(field.ty),
            RirLambdaEnvFieldKind::HeapCell { .. } => true,
        }
    }

    pub(super) fn lambda_capture_layout_edge(
        lambda: &RirLambda,
        capture: &RirLambdaCapture,
    ) -> LambdaCaptureLayoutEdge {
        match capture.abi {
            RirParamAbi::Value if matches!(lambda.storage, RirLambdaStorage::HeapEnv { .. }) => {
                LambdaCaptureLayoutEdge::HeapEnvField
            }
            RirParamAbi::Value => LambdaCaptureLayoutEdge::InlineValue,
            RirParamAbi::SharedBorrow => LambdaCaptureLayoutEdge::SharedBorrow,
            RirParamAbi::MutBorrow => LambdaCaptureLayoutEdge::MutBorrow,
            RirParamAbi::StackCell => LambdaCaptureLayoutEdge::StackCell,
            RirParamAbi::HeapCell => LambdaCaptureLayoutEdge::HeapCell,
            RirParamAbi::ScopedPlaceCell => LambdaCaptureLayoutEdge::ScopedPlaceCell,
            RirParamAbi::MutPlace
            | RirParamAbi::DynBorrow
            | RirParamAbi::ScopedLambda
            | RirParamAbi::EscapingLambda
            | RirParamAbi::AnvCallback => LambdaCaptureLayoutEdge::Unsupported,
        }
    }

    pub(super) fn inline_lambda_value_sig(
        self,
        lambda: &RirLambda,
        capture: &RirLambdaCapture,
    ) -> Option<RirLambdaSigId> {
        if Self::lambda_capture_layout_edge(lambda, capture) != LambdaCaptureLayoutEdge::InlineValue
        {
            return None;
        }
        match self.ty(capture.ty) {
            RirType::Lambda(sig) => Some(sig),
            _ => None,
        }
    }

    pub(super) fn lambda_has_recursive_inline_value_capture(self, lambda: &RirLambda) -> bool {
        lambda.captures.iter().any(|capture| {
            self.inline_lambda_value_sig(lambda, capture)
                .is_some_and(|sig| self.lambda_sig_reaches_inline_lambda_value(sig, lambda.sig))
        })
    }

    pub(super) fn lambda_sig_reaches_inline_lambda_value(
        self,
        from: RirLambdaSigId,
        target: RirLambdaSigId,
    ) -> bool {
        self.lambda_sig_reaches_inline_lambda_value_inner(from, target, &mut BTreeSet::new())
    }

    fn lambda_sig_reaches_inline_lambda_value_inner(
        self,
        from: RirLambdaSigId,
        target: RirLambdaSigId,
        visited: &mut BTreeSet<RirLambdaSigId>,
    ) -> bool {
        if from == target {
            return true;
        }
        if !visited.insert(from) {
            return false;
        }
        let reaches = self.program.lambdas_for_sig(from).any(|lambda| {
            lambda
                .captures
                .iter()
                .filter_map(|capture| self.inline_lambda_value_sig(lambda, capture))
                .any(|sig| self.lambda_sig_reaches_inline_lambda_value_inner(sig, target, visited))
        });
        visited.remove(&from);
        reaches
    }

    pub fn lambda_sig_copyable(self, id: RirLambdaSigId) -> bool {
        !self.lambda_sig_has_heap_env(id)
            && self.program.lambdas_for_sig(id).all(|lambda| {
                lambda
                    .captures
                    .iter()
                    .all(|capture| capture.abi != RirParamAbi::HeapCell)
            })
            && self.lambda_sig_copyable_inner(id, &mut BTreeSet::new())
    }

    pub fn lambda_sig_cloneable(self, id: RirLambdaSigId) -> bool {
        self.lambda_sig_has_heap_env(id)
            || self.program.lambdas_for_sig(id).all(|lambda| {
                lambda.captures.iter().all(|capture| match capture.abi {
                    RirParamAbi::Value => self.copyable(capture.ty),
                    RirParamAbi::SharedBorrow
                    | RirParamAbi::StackCell
                    | RirParamAbi::HeapCell
                    | RirParamAbi::ScopedPlaceCell => true,
                    RirParamAbi::MutBorrow
                    | RirParamAbi::MutPlace
                    | RirParamAbi::DynBorrow
                    | RirParamAbi::ScopedLambda
                    | RirParamAbi::EscapingLambda
                    | RirParamAbi::AnvCallback => false,
                })
            })
    }

    fn lambda_sig_copyable_inner(
        self,
        id: RirLambdaSigId,
        active: &mut BTreeSet<RirLambdaSigId>,
    ) -> bool {
        if !active.insert(id) {
            return false;
        }
        let copyable = self.program.lambdas_for_sig(id).all(|lambda| {
            lambda.captures.iter().all(|capture| match capture.abi {
                RirParamAbi::Value => self.copyable_inner(capture.ty, active),
                RirParamAbi::SharedBorrow
                | RirParamAbi::StackCell
                | RirParamAbi::HeapCell
                | RirParamAbi::ScopedPlaceCell => true,
                RirParamAbi::MutBorrow
                | RirParamAbi::MutPlace
                | RirParamAbi::DynBorrow
                | RirParamAbi::ScopedLambda
                | RirParamAbi::EscapingLambda
                | RirParamAbi::AnvCallback => false,
            })
        });
        active.remove(&id);
        copyable
    }

    pub fn fields_cx_dependent(self, fields: &[RirField]) -> bool {
        fields.iter().any(|field| self.type_cx_dependent(field.ty))
    }

    pub fn struct_cx_dependent(self, strukt: &RirStruct) -> bool {
        self.fields_cx_dependent(&strukt.fields)
    }

    pub fn tuple_cx_dependent(self, tuple: &RirTuple) -> bool {
        self.fields_cx_dependent(&tuple.fields)
    }

    pub fn enum_cx_dependent(self, enm: &RirEnum) -> bool {
        enm.variants
            .iter()
            .any(|variant| self.fields_cx_dependent(&variant.fields))
    }

    pub fn callable_ret_ty(self, ty: RirTypeId) -> String {
        self.escaping_value_ty(ty)
    }

    fn escaping_value_ty(self, ty: RirTypeId) -> String {
        match self.ty(ty) {
            RirType::Lambda(sig) if self.lambda_sig_needs_lifetime(sig) => {
                self.lambda_sig_storage_ty(sig)
            }
            _ => self.rust_ty(ty),
        }
    }

    pub fn rust_ty(self, ty: RirTypeId) -> String {
        self.rust_ty_inner(ty, false)
    }

    pub fn rust_storage_ty(self, ty: RirTypeId) -> String {
        self.rust_ty_inner(ty, true)
    }

    fn rust_ty_inner(self, ty: RirTypeId, storage: bool) -> String {
        match self.ty(ty) {
            RirType::Int => "i64".into(),
            RirType::Float => "f64".into(),
            RirType::Bool => "bool".into(),
            RirType::Char => "char".into(),
            RirType::String => target::anv_string_ty(),
            RirType::Void => "()".into(),
            RirType::Struct(id) => Self::named_ty(
                self.program.structs[id.index()].symbol.as_str(),
                self.type_cx_dependent(ty),
            ),
            RirType::DataRef(id) => {
                format!("{}<'cx>", self.program.datarefs[id.index()].symbol.as_str())
            }
            RirType::Enum(id) => Self::named_ty(
                self.program.enums[id.index()].symbol.as_str(),
                self.type_cx_dependent(ty),
            ),
            RirType::Tuple(id) => Self::named_ty(
                self.program.tuples[id.index()].symbol.as_str(),
                self.type_cx_dependent(ty),
            ),
            RirType::Array { elem, len } => {
                format!("[{}; {len}]", self.rust_ty_inner(elem, storage))
            }
            RirType::List(elem) => target::anv_list_ty(&self.rust_ty_inner(elem, storage)),
            RirType::Map { key, value } => target::anv_map_ty(
                &self.rust_ty_inner(key, storage),
                &self.rust_ty_inner(value, storage),
            ),
            RirType::Option(inner) => format!("Option<{}>", self.rust_ty_inner(inner, storage)),
            RirType::Slice(elem) => target::anv_slice_ty(&self.rust_ty_inner(elem, storage)),
            RirType::Lambda(id) if storage => self.lambda_sig_storage_ty(id),
            RirType::Lambda(id) => self.lambda_sig_ty(id),
        }
    }

    pub fn lambda_sig_storage_ty(self, id: RirLambdaSigId) -> String {
        let generics = match (
            self.lambda_sig_needs_lifetime(id),
            self.lambda_sig_needs_ctx_lifetime(id),
        ) {
            (true, true) => "<'cx, 'cx>",
            (true, false) | (false, true) => "<'cx>",
            (false, false) => "",
        };
        format!("{}{generics}", self.lambda_sig_symbol(id))
    }

    pub fn dataref_cx_dependent(self, dataref: &RirDataRef) -> bool {
        dataref
            .fields
            .iter()
            .any(|field| self.type_cx_dependent(field.ty))
    }

    pub fn dataref_storage_tracked(self, dataref: &RirDataRef) -> bool {
        dataref.cycle_capable
            || dataref
                .fields
                .iter()
                .any(|field| self.type_owns_heap_edges(field.ty))
    }

    pub fn cell_storage_ty(self, cell: &RirCellDecl) -> String {
        let payload = self.rust_ty(cell.payload_ty);
        target::lambda_cell_ty(&payload)
    }

    pub fn cell_storage_tracked(self, cell: &RirCellDecl) -> bool {
        self.type_owns_heap_edges(cell.payload_ty)
    }

    pub fn lambda_env_storage_ty(self, env: &RirLambdaEnvLayout) -> String {
        Self::named_ty(env.symbol.as_str(), self.lambda_env_cx_dependent(env))
    }

    pub fn lambda_env_cx_dependent(self, env: &RirLambdaEnvLayout) -> bool {
        env.fields.iter().any(|field| match field.kind {
            RirLambdaEnvFieldKind::Value => self.type_cx_dependent(field.ty),
            RirLambdaEnvFieldKind::HeapCell { .. } => true,
        })
    }

    pub fn lambda_env_field_ty(self, field: &RirLambdaEnvField) -> String {
        match field.kind {
            RirLambdaEnvFieldKind::Value => self.escaping_value_ty(field.ty),
            RirLambdaEnvFieldKind::HeapCell { .. } => {
                let payload = self.escaping_value_ty(field.ty);
                target::handle_ty(&target::lambda_cell_ty(&payload))
            }
        }
    }

    pub fn lambda_env_storage_tracked(self, env: &RirLambdaEnvLayout) -> bool {
        env.fields.iter().any(|field| match field.kind {
            RirLambdaEnvFieldKind::Value => self.type_owns_heap_edges(field.ty),
            RirLambdaEnvFieldKind::HeapCell { .. } => true,
        })
    }

    pub fn list_storage_tracked(self, elem: RirTypeId) -> bool {
        self.type_owns_heap_edges(elem)
    }

    pub fn map_storage_tracked(self, key: RirTypeId, value: RirTypeId) -> bool {
        self.type_owns_heap_edges(key) || self.type_owns_heap_edges(value)
    }

    pub fn type_owns_heap_edges(self, ty: RirTypeId) -> bool {
        self.type_has_heap_shape(ty, Self::lambda_sig_owns_heap_edges)
    }

    fn lambda_sig_owns_heap_edges(self, id: RirLambdaSigId) -> bool {
        self.lambda_sig_has_heap_env(id)
            || self.program.lambdas_for_sig(id).any(|lambda| {
                lambda
                    .captures
                    .iter()
                    .any(|capture| capture.abi == RirParamAbi::HeapCell)
            })
    }

    pub(super) fn lambda_trace_action(lambda: &RirLambda) -> LambdaTraceAction {
        if matches!(lambda.storage, RirLambdaStorage::HeapEnv { .. }) {
            return LambdaTraceAction::HeapEnv;
        }
        let cells = lambda
            .captures
            .iter()
            .enumerate()
            .filter_map(|(index, capture)| (capture.abi == RirParamAbi::HeapCell).then_some(index))
            .collect::<Vec<_>>();
        if cells.is_empty() {
            LambdaTraceAction::Noop
        } else {
            LambdaTraceAction::HeapCellCaptures(cells)
        }
    }

    pub fn type_cx_dependent(self, ty: RirTypeId) -> bool {
        self.type_cx_dependent_inner(ty, true, &mut BTreeSet::new(), &mut BTreeSet::new())
    }

    fn value_type_cx_dependent(
        self,
        ty: RirTypeId,
        active_sigs: &mut BTreeSet<RirLambdaSigId>,
    ) -> bool {
        self.type_cx_dependent_inner(ty, false, &mut BTreeSet::new(), active_sigs)
    }

    fn type_cx_dependent_inner(
        self,
        ty: RirTypeId,
        storage: bool,
        active_tys: &mut BTreeSet<RirTypeId>,
        active_sigs: &mut BTreeSet<RirLambdaSigId>,
    ) -> bool {
        if !active_tys.insert(ty) {
            return false;
        }
        let has_shape = match self.ty(ty) {
            RirType::DataRef(_) | RirType::List(_) | RirType::Slice(_) | RirType::Map { .. } => {
                true
            }
            RirType::Option(inner) | RirType::Array { elem: inner, .. } => {
                self.type_cx_dependent_inner(inner, storage, active_tys, active_sigs)
            }
            RirType::Lambda(sig) => {
                (storage && self.lambda_sig_needs_lifetime(sig))
                    || self.lambda_sig_needs_ctx_lifetime_inner(sig, active_sigs)
            }
            RirType::Struct(id) if self.program.structs[id.index()].native_ref => true,
            RirType::Struct(id) => self.program.structs[id.index()].fields.iter().any(|field| {
                self.type_cx_dependent_inner(field.ty, storage, active_tys, active_sigs)
            }),
            RirType::Tuple(id) => self.program.tuples[id.index()].fields.iter().any(|field| {
                self.type_cx_dependent_inner(field.ty, storage, active_tys, active_sigs)
            }),
            RirType::Enum(id) => self.program.enums[id.index()]
                .variants
                .iter()
                .any(|variant| {
                    variant.fields.iter().any(|field| {
                        self.type_cx_dependent_inner(field.ty, storage, active_tys, active_sigs)
                    })
                }),
            RirType::Int
            | RirType::Float
            | RirType::Bool
            | RirType::Char
            | RirType::String
            | RirType::Void => false,
        };
        active_tys.remove(&ty);
        has_shape
    }

    fn type_has_heap_shape(
        self,
        ty: RirTypeId,
        lambda_has_shape: fn(Self, RirLambdaSigId) -> bool,
    ) -> bool {
        self.type_has_heap_shape_inner(ty, lambda_has_shape, &mut BTreeSet::new())
    }

    fn type_has_heap_shape_inner(
        self,
        ty: RirTypeId,
        lambda_has_shape: fn(Self, RirLambdaSigId) -> bool,
        active: &mut BTreeSet<RirTypeId>,
    ) -> bool {
        if !active.insert(ty) {
            return false;
        }
        let has_shape = match self.ty(ty) {
            RirType::DataRef(_) | RirType::List(_) | RirType::Slice(_) | RirType::Map { .. } => {
                true
            }
            RirType::Option(inner) | RirType::Array { elem: inner, .. } => {
                self.type_has_heap_shape_inner(inner, lambda_has_shape, active)
            }
            RirType::Lambda(sig) => lambda_has_shape(self, sig),
            RirType::Struct(id) if self.program.structs[id.index()].native_ref => true,
            RirType::Struct(id) => self.program.structs[id.index()]
                .fields
                .iter()
                .any(|field| self.type_has_heap_shape_inner(field.ty, lambda_has_shape, active)),
            RirType::Tuple(id) => self.program.tuples[id.index()]
                .fields
                .iter()
                .any(|field| self.type_has_heap_shape_inner(field.ty, lambda_has_shape, active)),
            RirType::Enum(id) => self.program.enums[id.index()]
                .variants
                .iter()
                .any(|variant| {
                    variant.fields.iter().any(|field| {
                        self.type_has_heap_shape_inner(field.ty, lambda_has_shape, active)
                    })
                }),
            RirType::Int
            | RirType::Float
            | RirType::Bool
            | RirType::Char
            | RirType::String
            | RirType::Void => false,
        };
        active.remove(&ty);
        has_shape
    }

    fn named_ty(symbol: &str, cx_dependent: bool) -> String {
        if cx_dependent {
            format!("{symbol}<'cx>")
        } else {
            symbol.into()
        }
    }

    pub fn copyable(self, ty: RirTypeId) -> bool {
        self.copyable_inner(ty, &mut BTreeSet::new())
    }

    fn copyable_inner(self, ty: RirTypeId, active: &mut BTreeSet<RirLambdaSigId>) -> bool {
        match self.ty(ty) {
            RirType::Int | RirType::Float | RirType::Bool | RirType::Char | RirType::Void => true,
            RirType::String
            | RirType::DataRef(_)
            | RirType::List(_)
            | RirType::Map { .. }
            | RirType::Slice(_) => false,
            RirType::Lambda(id) => {
                !self.lambda_sig_has_heap_env(id) && self.lambda_sig_copyable_inner(id, active)
            }
            RirType::Option(inner) => self.copyable_inner(inner, active),
            RirType::Array { elem, .. } => self.copyable_inner(elem, active),
            RirType::Struct(id) => self.program.structs[id.index()].copyable,
            RirType::Enum(id) => self.program.enums[id.index()].copyable,
            RirType::Tuple(id) => self.program.tuples[id.index()].copyable,
        }
    }

    fn supports_type_semantic(self, ty: RirType, semantic: RirParamSemantic) -> bool {
        match semantic {
            RirParamSemantic::Value => match ty {
                RirType::Option(inner) => self.supports_param(inner, semantic),
                RirType::Int
                | RirType::Float
                | RirType::Bool
                | RirType::Char
                | RirType::Void
                | RirType::String
                | RirType::Struct(_)
                | RirType::DataRef(_)
                | RirType::Enum(_)
                | RirType::Array { .. }
                | RirType::List(_)
                | RirType::Map { .. }
                | RirType::Slice(_)
                | RirType::Lambda(_) => true,
                RirType::Tuple(id) => self.program.tuples[id.index()]
                    .fields
                    .iter()
                    .all(|field| self.supports_param(field.ty, semantic)),
            },
            RirParamSemantic::SharedBorrow => match ty {
                RirType::Option(inner) => self.supports_param(inner, semantic),
                RirType::String
                | RirType::Struct(_)
                | RirType::Tuple(_)
                | RirType::DataRef(_)
                | RirType::Enum(_)
                | RirType::Array { .. }
                | RirType::List(_)
                | RirType::Map { .. }
                | RirType::Slice(_)
                | RirType::Lambda(_) => true,
                RirType::Int | RirType::Float | RirType::Bool | RirType::Char | RirType::Void => {
                    false
                }
            },
            RirParamSemantic::MutBorrow => match ty {
                RirType::Option(inner) => self.supports_param(inner, semantic),
                RirType::Int
                | RirType::Float
                | RirType::Bool
                | RirType::Char
                | RirType::String
                | RirType::Struct(_)
                | RirType::Tuple(_)
                | RirType::DataRef(_)
                | RirType::Enum(_)
                | RirType::Array { .. }
                | RirType::List(_)
                | RirType::Map { .. } => true,
                RirType::Void | RirType::Slice(_) | RirType::Lambda(_) => false,
            },
            RirParamSemantic::ScopedLambda
            | RirParamSemantic::EscapingLambda
            | RirParamSemantic::AnvCallback => matches!(ty, RirType::Lambda(_)),
            RirParamSemantic::DynBorrow => match ty {
                RirType::Enum(id) => self.program.dyn_carrier_for_enum(id).is_some(),
                _ => false,
            },
            RirParamSemantic::MutPlace
            | RirParamSemantic::StackCell
            | RirParamSemantic::HeapCell
            | RirParamSemantic::ScopedPlaceCell => !matches!(ty, RirType::Void),
        }
    }

    fn enum_rep(self, id: RirEnumId) -> RustValueRep {
        let enum_ = &self.program.enums[id.index()];
        match enum_.raw_type {
            Some(ty) => match self.ty(ty) {
                RirType::String => RustValueRep::RawStringEnum,
                _ => RustValueRep::RawIntEnum,
            },
            None => RustValueRep::InlineEnum,
        }
    }

    fn ty(self, ty: RirTypeId) -> RirType {
        self.program.types[ty.index()]
    }

    fn ty_opt(self, ty: RirTypeId) -> Option<RirType> {
        self.program.types.get(ty.index()).copied()
    }
}

#[derive(Debug, Default, Clone)]
pub struct RustTracePlan {
    structs: BTreeSet<RirStructId>,
    enums: BTreeSet<RirEnumId>,
    tuples: BTreeSet<RirTupleId>,
    lambda_sigs: BTreeSet<RirLambdaSigId>,
    visited: BTreeSet<RirTypeId>,
}

impl RustTracePlan {
    pub fn build(program: &RirProgram) -> Self {
        let mut plan = Self::default();
        let policy = RirRustRepPolicy::new(program);
        for (index, ty) in program.types.iter().enumerate() {
            let id = RirTypeId::from_index(index);
            if matches!(
                ty,
                RirType::Struct(_) | RirType::Tuple(_) | RirType::Enum(_)
            ) && policy.type_owns_heap_edges(id)
            {
                plan.mark_type(program, id);
            }
        }
        for dataref in &program.datarefs {
            if policy.dataref_storage_tracked(dataref) {
                for field in &dataref.fields {
                    plan.mark_type(program, field.ty);
                }
            }
        }
        for cell in &program.cells {
            if cell.storage == RirCellStorage::Heap && policy.cell_storage_tracked(cell) {
                plan.mark_type(program, cell.payload_ty);
            }
        }
        for env in &program.lambda_envs {
            if policy.lambda_env_storage_tracked(env) {
                for field in &env.fields {
                    plan.mark_type(program, field.ty);
                }
            }
        }
        for storage in &program.collection_storages {
            match storage.kind {
                RirCollectionStorageKind::List { elem_ty }
                    if policy.list_storage_tracked(elem_ty) =>
                {
                    plan.mark_type(program, elem_ty);
                }
                RirCollectionStorageKind::Map { key_ty, value_ty }
                    if policy.map_storage_tracked(key_ty, value_ty) =>
                {
                    plan.mark_type(program, key_ty);
                    plan.mark_type(program, value_ty);
                }
                _ => {}
            }
        }
        for global in &program.globals {
            if policy.type_owns_heap_edges(global.ty) {
                plan.mark_type(program, global.ty);
            }
        }
        for sig in program.retained_callback_sigs() {
            plan.mark_lambda_sig(program, sig);
        }
        plan
    }

    pub fn needs_struct_trace(&self, id: RirStructId) -> bool {
        self.structs.contains(&id)
    }

    pub fn needs_enum_trace(&self, id: RirEnumId) -> bool {
        self.enums.contains(&id)
    }

    pub fn needs_tuple_trace(&self, id: RirTupleId) -> bool {
        self.tuples.contains(&id)
    }

    pub fn needs_lambda_sig_trace(&self, id: RirLambdaSigId) -> bool {
        self.lambda_sigs.contains(&id)
    }

    fn mark_lambda_sig(&mut self, program: &RirProgram, id: RirLambdaSigId) {
        if RirRustRepPolicy::new(program).lambda_sig_owns_heap_edges(id) {
            self.lambda_sigs.insert(id);
        }
    }

    fn mark_type(&mut self, program: &RirProgram, ty: RirTypeId) {
        if !self.visited.insert(ty) {
            return;
        }
        match program.types[ty.index()] {
            RirType::Option(inner) => self.mark_type(program, inner),
            RirType::Struct(id) => {
                let strukt = &program.structs[id.index()];
                if strukt.native_path.is_some() {
                    return;
                }
                self.structs.insert(id);
                for field in &strukt.fields {
                    self.mark_type(program, field.ty);
                }
            }
            RirType::Tuple(id) => {
                self.tuples.insert(id);
                for field in &program.tuples[id.index()].fields {
                    self.mark_type(program, field.ty);
                }
            }
            RirType::Enum(id) => {
                self.enums.insert(id);
                for variant in &program.enums[id.index()].variants {
                    for field in &variant.fields {
                        self.mark_type(program, field.ty);
                    }
                }
            }
            RirType::Array { elem, .. } | RirType::List(elem) | RirType::Slice(elem) => {
                self.mark_type(program, elem);
            }
            RirType::Map { key, value } => {
                self.mark_type(program, key);
                self.mark_type(program, value);
            }
            RirType::Lambda(id) => self.mark_lambda_sig(program, id),
            RirType::Int
            | RirType::Float
            | RirType::Bool
            | RirType::String
            | RirType::Char
            | RirType::Void
            | RirType::DataRef(_) => {}
        }
    }
}

#[cfg(test)]
mod tests {
    use anvyx_frontend::{
        air::{self, FieldDecl, FunctionId, ParamMode, Program, TypeData, TypePassClasses},
        ast::Ident,
    };

    use super::{
        LambdaStorageFamily, LambdaStorageGap, RirRustRepPolicy, RustBorrowView,
        RustCarrierDiscriminant, RustMaterialGap, RustMaterialIntent, RustMaterialSource,
        RustMaterialization, RustPayloadStorage, RustPhysicalRecipe, RustRecipeGap,
        RustRecipePosition, RustRepresentationPlan, RustRepresentationPlanError, RustTargetProfile,
        RustTracePlan, RustValueRep,
    };
    use crate::rust::rir::{
        RirCellId, RirDataRef, RirDataRefId, RirEnum, RirEnumId, RirEnumRepr, RirExtern,
        RirExternId, RirExternKind, RirExternParam, RirField, RirFieldId, RirFunctionId, RirLambda,
        RirLambdaCapture, RirLambdaCaptureKind, RirLambdaEnvField, RirLambdaEnvFieldKind,
        RirLambdaEnvId, RirLambdaEscape, RirLambdaId, RirLambdaParam, RirLambdaSig, RirLambdaSigId,
        RirLambdaSource, RirLambdaStorage, RirNativeExtern, RirParamAbi, RirParamEscape,
        RirParamSemantic, RirProgram, RirStruct, RirStructId, RirSymbol, RirTuple, RirTupleId,
        RirType, RirTypeId, RirVariant, RirVariantId, RirVariantKind,
    };

    fn surface(program: &mut Program, name: &str) -> air::ContractSurfaceId {
        program.alloc_contract_surface(air::ContractSurfaceDecl {
            display_name: name.into(),
            slots: vec![],
        })
    }

    fn witness(program: &mut Program, surface: air::ContractSurfaceId, concrete_ty: air::TypeId) {
        program.alloc_contract_witness(air::ContractWitnessDecl {
            key: air::ContractWitnessKey {
                concrete_ty,
                surface,
                slots: vec![],
            },
        });
    }

    fn payload_storage(
        program: &Program,
        surface: air::ContractSurfaceId,
        concrete_ty: air::TypeId,
    ) -> RustPayloadStorage {
        let classes = TypePassClasses::analyze(program);
        let layout = RustRepresentationPlan::new(program, &classes)
            .dynamic_layout_plan()
            .unwrap();
        layout
            .carriers
            .iter()
            .find(|carrier| carrier.surface == surface)
            .unwrap()
            .variants
            .iter()
            .find(|variant| variant.concrete_ty == concrete_ty)
            .unwrap()
            .storage
    }

    #[test]
    fn representation_plan_owns_target_layout() {
        let program = Program::default();
        let classes = TypePassClasses::analyze(&program);
        let plan = RustRepresentationPlan::new(&program, &classes);
        assert_eq!(plan.target(), RustTargetProfile::AOT_64);
        assert_eq!(
            plan.target().carrier_discriminant,
            RustCarrierDiscriminant::U32
        );

        let invalid = RustTargetProfile {
            pointer_size: 8,
            pointer_align: 3,
            carrier_discriminant: RustCarrierDiscriminant::U32,
        };
        assert_eq!(
            RustRepresentationPlan::for_target(&program, &classes, invalid).unwrap_err(),
            RustRepresentationPlanError::InvalidPointerLayout { size: 8, align: 3 }
        );
        let invalid_stride = RustTargetProfile {
            pointer_size: 6,
            pointer_align: 4,
            carrier_discriminant: RustCarrierDiscriminant::U32,
        };
        assert_eq!(
            RustRepresentationPlan::for_target(&program, &classes, invalid_stride).unwrap_err(),
            RustRepresentationPlanError::InvalidPointerLayout { size: 6, align: 4 }
        );
    }

    #[test]
    fn representation_plan_maps_air_use_to_physical_recipe() {
        let mut program = Program::default();
        let int = program.alloc_type(TypeData::Int);
        let string = program.alloc_type(TypeData::String);
        let tuple = program.alloc_type(TypeData::Tuple(vec![int, string]));
        let function = program.alloc_type(TypeData::Function(air::SignatureType::new(
            vec![],
            air::ReturnMode::Value(int),
        )));
        let dyn_ty = program.alloc_type(TypeData::Dyn(air::ContractSurfaceId::from_index(0)));
        let classes = TypePassClasses::analyze(&program);
        let plan = RustRepresentationPlan::new(&program, &classes);

        assert_eq!(
            plan.recipe_for(int, air::ValueUse::Read, RustRecipePosition::Value),
            Ok(RustPhysicalRecipe::Copy)
        );
        assert_eq!(
            plan.recipe_for(string, air::ValueUse::Read, RustRecipePosition::Value),
            Ok(RustPhysicalRecipe::ManagedShare)
        );
        assert_eq!(
            plan.recipe_for(tuple, air::ValueUse::Read, RustRecipePosition::Value),
            Ok(RustPhysicalRecipe::ReconstructAggregate)
        );
        assert_eq!(
            plan.recipe_for(tuple, air::ValueUse::Consume, RustRecipePosition::Value),
            Ok(RustPhysicalRecipe::Move)
        );
        assert_eq!(
            plan.recipe_for(function, air::ValueUse::Read, RustRecipePosition::Value),
            Err(RustRecipeGap::FunctionCapabilityPending(function))
        );
        assert_eq!(
            plan.recipe_for(dyn_ty, air::ValueUse::Read, RustRecipePosition::Value),
            Err(RustRecipeGap::DynamicCarrierPending(dyn_ty))
        );
    }

    #[test]
    fn representation_plan_closes_dynamic_carriers_deterministically() {
        let mut program = Program::default();
        let first = program.alloc_type(TypeData::Int);
        let second = program.alloc_type(TypeData::String);
        let surface = program.alloc_contract_surface(air::ContractSurfaceDecl {
            display_name: "Renderable".into(),
            slots: vec![],
        });
        program.alloc_type(TypeData::Dyn(surface));
        program.alloc_contract_witness(air::ContractWitnessDecl {
            key: air::ContractWitnessKey {
                concrete_ty: second,
                surface,
                slots: vec![],
            },
        });
        program.alloc_contract_witness(air::ContractWitnessDecl {
            key: air::ContractWitnessKey {
                concrete_ty: first,
                surface,
                slots: vec![],
            },
        });
        let classes = TypePassClasses::analyze(&program);
        let layout = RustRepresentationPlan::new(&program, &classes)
            .dynamic_layout_plan()
            .unwrap();

        assert_eq!(layout.carriers.len(), 1);
        assert_eq!(layout.carriers[0].surface, surface);
        assert_eq!(layout.carriers[0].variants.len(), 2);
        assert_eq!(layout.carriers[0].variants[0].concrete_ty, first);
        assert_eq!(layout.carriers[0].variants[1].concrete_ty, second);
        assert_eq!(layout.declaration_order, vec![surface]);
    }

    #[test]
    fn dynamic_layout_preserves_lexical_witness_variants() {
        let mut program = Program::default();
        let int = program.alloc_type(TypeData::Int);
        let surface = surface(&mut program, "Lexical");
        program.alloc_type(TypeData::Dyn(surface));
        witness(&mut program, surface, int);
        witness(&mut program, surface, int);
        let classes = TypePassClasses::analyze(&program);
        let layout = RustRepresentationPlan::new(&program, &classes)
            .dynamic_layout_plan()
            .unwrap();

        assert_eq!(layout.carriers[0].variants.len(), 2);
        assert_ne!(
            layout.carriers[0].variants[0].witness,
            layout.carriers[0].variants[1].witness
        );
        assert_eq!(
            layout.carriers[0].variants[0].storage,
            layout.carriers[0].variants[1].storage
        );
    }

    #[test]
    fn dynamic_layout_is_stable_across_declaration_order() {
        fn build(reverse: bool) -> Program {
            let mut program = Program::default();
            let (int, string) = if reverse {
                let string = program.alloc_type(TypeData::String);
                let int = program.alloc_type(TypeData::Int);
                (int, string)
            } else {
                let int = program.alloc_type(TypeData::Int);
                let string = program.alloc_type(TypeData::String);
                (int, string)
            };
            let (alpha, beta) = if reverse {
                let beta = surface(&mut program, "Beta");
                let alpha = surface(&mut program, "Alpha");
                (alpha, beta)
            } else {
                let alpha = surface(&mut program, "Alpha");
                let beta = surface(&mut program, "Beta");
                (alpha, beta)
            };
            program.alloc_type(TypeData::Dyn(alpha));
            program.alloc_type(TypeData::Dyn(beta));
            witness(&mut program, beta, string);
            witness(&mut program, alpha, int);
            program
        }

        fn normalized(program: &Program) -> (Vec<(String, Vec<String>)>, Vec<String>) {
            let classes = TypePassClasses::analyze(program);
            let plan = RustRepresentationPlan::new(program, &classes);
            let layout = plan.dynamic_layout_plan().unwrap();
            let carriers = layout
                .carriers
                .iter()
                .map(|carrier| {
                    (
                        plan.surface_sort_key(carrier.surface),
                        carrier
                            .variants
                            .iter()
                            .map(|variant| plan.type_sort_key(variant.concrete_ty))
                            .collect(),
                    )
                })
                .collect();
            let order = layout
                .declaration_order
                .iter()
                .map(|surface| plan.surface_sort_key(*surface))
                .collect();
            (carriers, order)
        }

        assert_eq!(normalized(&build(false)), normalized(&build(true)));
    }

    #[test]
    fn dynamic_layout_structural_keys_distinguish_collision_cases() {
        let mut program = Program::default();
        let int = program.alloc_type(TypeData::Int);
        let string = program.alloc_type(TypeData::String);
        let module = program.alloc_module(air::Module::default());
        let first = program.alloc_aggregate(air::AggregateDecl {
            name: Ident::new("Ref"),
            module,
            kind: air::AggregateKind::DataRef,
            type_args: vec![int],
            const_args: vec![],
            fields: vec![],
            cycle_capable: false,
            stringify_override: None,
        });
        let second = program.alloc_aggregate(air::AggregateDecl {
            name: Ident::new("Ref"),
            module,
            kind: air::AggregateKind::DataRef,
            type_args: vec![string],
            const_args: vec![],
            fields: vec![],
            cycle_capable: false,
            stringify_override: None,
        });
        let first = program.alloc_type(TypeData::DataRef(first));
        let second = program.alloc_type(TypeData::DataRef(second));
        let value_fn = program.alloc_type(TypeData::Function(air::SignatureType::new(
            vec![],
            air::ReturnMode::Value(int),
        )));
        let place_fn = program.alloc_type(TypeData::Function(air::SignatureType::new(
            vec![],
            air::ReturnMode::Place(int),
        )));
        let surface_a = program.alloc_contract_surface(air::ContractSurfaceDecl {
            display_name: "Same".into(),
            slots: vec![air::ContractSlotDecl {
                id: air::ContractSlotId::from_index(0),
                name: Ident::new("slot"),
                receiver: air::ContractReceiver::Ref,
                params: vec![air::ContractParamDecl {
                    ty: int,
                    mode: ParamMode::Value,
                    cast_accept: false,
                    escape: air::ParamEscape::NonEscaping,
                }],
                ret: air::ContractReturnDecl::Value(int),
            }],
        });
        let surface_b = program.alloc_contract_surface(air::ContractSurfaceDecl {
            display_name: "Same".into(),
            slots: vec![air::ContractSlotDecl {
                id: air::ContractSlotId::from_index(0),
                name: Ident::new("slot"),
                receiver: air::ContractReceiver::Ref,
                params: vec![air::ContractParamDecl {
                    ty: int,
                    mode: ParamMode::Value,
                    cast_accept: true,
                    escape: air::ParamEscape::NonEscaping,
                }],
                ret: air::ContractReturnDecl::Place(int),
            }],
        });
        let dyn_a = program.alloc_type(TypeData::Dyn(surface_a));
        let dyn_b = program.alloc_type(TypeData::Dyn(surface_b));
        let recursive = surface(&mut program, "Recursive");
        let recursive_dyn = program.alloc_type(TypeData::Dyn(recursive));
        let recursive_param = program.alloc_type(TypeData::Optional(recursive_dyn));
        program.contract_surface_mut(recursive).slots = vec![air::ContractSlotDecl {
            id: air::ContractSlotId::from_index(0),
            name: Ident::new("recurse"),
            receiver: air::ContractReceiver::Ref,
            params: vec![air::ContractParamDecl {
                ty: recursive_param,
                mode: ParamMode::Value,
                cast_accept: false,
                escape: air::ParamEscape::NonEscaping,
            }],
            ret: air::ContractReturnDecl::Iter,
        }];
        let root_a = surface(&mut program, "Root");
        let node_a = surface(&mut program, "Node");
        let root_b = surface(&mut program, "Root");
        let node_b = surface(&mut program, "Node");
        let root_a_ty = program.alloc_type(TypeData::Dyn(root_a));
        let node_a_ty = program.alloc_type(TypeData::Dyn(node_a));
        let root_b_ty = program.alloc_type(TypeData::Dyn(root_b));
        let node_b_ty = program.alloc_type(TypeData::Dyn(node_b));
        let recursive_slot = |ty| air::ContractSlotDecl {
            id: air::ContractSlotId::from_index(0),
            name: Ident::new("next"),
            receiver: air::ContractReceiver::Ref,
            params: vec![air::ContractParamDecl {
                ty,
                mode: ParamMode::Value,
                cast_accept: false,
                escape: air::ParamEscape::NonEscaping,
            }],
            ret: air::ContractReturnDecl::Iter,
        };
        program.contract_surface_mut(root_a).slots = vec![recursive_slot(node_a_ty)];
        program.contract_surface_mut(node_a).slots = vec![recursive_slot(root_a_ty)];
        program.contract_surface_mut(root_b).slots = vec![recursive_slot(node_b_ty)];
        program.contract_surface_mut(node_b).slots = vec![recursive_slot(node_b_ty)];

        let classes = TypePassClasses::analyze(&program);
        let plan = RustRepresentationPlan::new(&program, &classes);

        assert_ne!(plan.type_sort_key(first), plan.type_sort_key(second));
        assert_ne!(plan.type_sort_key(value_fn), plan.type_sort_key(place_fn));
        assert_ne!(plan.type_sort_key(dyn_a), plan.type_sort_key(dyn_b));
        assert!(plan.type_sort_key(recursive_dyn).contains("recursive"));
        assert_ne!(plan.type_sort_key(root_a_ty), plan.type_sort_key(root_b_ty));
    }

    #[test]
    fn dynamic_layout_boxes_recursive_wrappers_and_mutual_cycles() {
        let mut direct = Program::default();
        let a = surface(&mut direct, "A");
        let dyn_a = direct.alloc_type(TypeData::Dyn(a));
        let optional = direct.alloc_type(TypeData::Optional(dyn_a));
        let wrapped = direct.alloc_type(TypeData::Array {
            elem: optional,
            len: 1,
        });
        witness(&mut direct, a, wrapped);
        assert_eq!(
            payload_storage(&direct, a, wrapped),
            RustPayloadStorage::Boxed
        );

        let mut mutual = Program::default();
        let a = surface(&mut mutual, "A");
        let b = surface(&mut mutual, "B");
        let dyn_a = mutual.alloc_type(TypeData::Dyn(a));
        let dyn_b = mutual.alloc_type(TypeData::Dyn(b));
        let payload_a = mutual.alloc_type(TypeData::Tuple(vec![dyn_b]));
        let payload_b = mutual.alloc_type(TypeData::Tuple(vec![dyn_a]));
        witness(&mut mutual, a, payload_a);
        witness(&mut mutual, b, payload_b);
        assert_eq!(
            payload_storage(&mutual, a, payload_a),
            RustPayloadStorage::Boxed
        );
        assert_eq!(
            payload_storage(&mutual, b, payload_b),
            RustPayloadStorage::Boxed
        );
    }

    #[test]
    fn dynamic_layout_boxes_large_payloads_but_not_collection_indirection() {
        let mut program = Program::default();
        let surface = surface(&mut program, "Sized");
        let int = program.alloc_type(TypeData::Int);
        let large = program.alloc_type(TypeData::Array { elem: int, len: 17 });
        let dyn_ty = program.alloc_type(TypeData::Dyn(surface));
        let indirect = program.alloc_type(TypeData::List(dyn_ty));
        witness(&mut program, surface, large);
        witness(&mut program, surface, indirect);

        assert_eq!(
            payload_storage(&program, surface, large),
            RustPayloadStorage::Boxed
        );
        assert_eq!(
            payload_storage(&program, surface, indirect),
            RustPayloadStorage::Inline
        );
    }

    #[test]
    fn dynamic_layout_uses_three_pointer_words_and_alignment() {
        let mut program = Program::default();
        let surface = surface(&mut program, "Threshold");
        program.alloc_type(TypeData::Dyn(surface));
        let int = program.alloc_type(TypeData::Int);
        let at_limit = program.alloc_type(TypeData::Array { elem: int, len: 3 });
        let over_limit = program.alloc_type(TypeData::Array { elem: int, len: 4 });
        witness(&mut program, surface, int);
        witness(&mut program, surface, at_limit);
        witness(&mut program, surface, over_limit);
        assert_eq!(
            payload_storage(&program, surface, at_limit),
            RustPayloadStorage::Inline
        );
        assert_eq!(
            payload_storage(&program, surface, over_limit),
            RustPayloadStorage::Boxed
        );

        let classes = TypePassClasses::analyze(&program);
        let target = RustTargetProfile {
            pointer_size: 4,
            pointer_align: 4,
            carrier_discriminant: RustCarrierDiscriminant::U32,
        };
        let layout = RustRepresentationPlan::for_target(&program, &classes, target)
            .unwrap()
            .dynamic_layout_plan()
            .unwrap();
        let int_variant = layout.carriers[0]
            .variants
            .iter()
            .find(|variant| variant.concrete_ty == int)
            .unwrap();
        assert_eq!(int_variant.storage, RustPayloadStorage::Boxed);
    }

    #[test]
    fn dynamic_lifecycle_tracks_boxing_and_payload_edges() {
        let mut program = Program::default();
        let surface = surface(&mut program, "Lifecycle");
        program.alloc_type(TypeData::Dyn(surface));
        let int = program.alloc_type(TypeData::Int);
        let boxed = program.alloc_type(TypeData::Array { elem: int, len: 17 });
        let list = program.alloc_type(TypeData::List(int));
        witness(&mut program, surface, int);
        witness(&mut program, surface, boxed);
        witness(&mut program, surface, list);
        let classes = TypePassClasses::analyze(&program);
        let layout = RustRepresentationPlan::new(&program, &classes)
            .dynamic_layout_plan()
            .unwrap();
        let carrier = &layout.carriers[0];
        let lifecycle = |ty| {
            carrier
                .variants
                .iter()
                .find(|variant| variant.concrete_ty == ty)
                .unwrap()
                .lifecycle
                .unwrap()
        };

        assert_eq!(lifecycle(int), super::RustLifecyclePlan::default());
        assert!(!lifecycle(boxed).heap.owns_edges);
        assert!(!lifecycle(boxed).heap.trace);
        assert!(lifecycle(boxed).heap.drop);
        assert!(lifecycle(list).context);
        assert_eq!(carrier.lifecycle, Ok(super::tracked_lifecycle()));
    }

    #[test]
    fn dynamic_lifecycle_preserves_slice_string_and_function_gaps() {
        let mut program = Program::default();
        let surface = surface(&mut program, "LifecycleGaps");
        program.alloc_type(TypeData::Dyn(surface));
        let int = program.alloc_type(TypeData::Int);
        let string = program.alloc_type(TypeData::String);
        let slice = program.alloc_type(TypeData::Slice(int));
        let function = program.alloc_type(TypeData::Function(air::SignatureType::new(
            vec![],
            air::ReturnMode::Value(int),
        )));
        witness(&mut program, surface, string);
        witness(&mut program, surface, slice);
        witness(&mut program, surface, function);
        let classes = TypePassClasses::analyze(&program);
        let layout = RustRepresentationPlan::new(&program, &classes)
            .dynamic_layout_plan()
            .unwrap();
        let variants = &layout.carriers[0].variants;
        let lifecycle = |ty| {
            variants
                .iter()
                .find(|variant| variant.concrete_ty == ty)
                .unwrap()
                .lifecycle
        };

        assert_eq!(lifecycle(string), Ok(super::RustLifecyclePlan::default()));
        assert_eq!(
            lifecycle(slice).unwrap(),
            super::RustLifecyclePlan {
                heap: super::RustHeapLifecycle {
                    owns_edges: true,
                    trace: true,
                    drop: false,
                },
                context: true,
            }
        );
        assert_eq!(
            lifecycle(function),
            Err(super::RustLifecycleGap::FunctionMetadataUnknown(function))
        );
        assert_eq!(
            variants
                .iter()
                .find(|variant| variant.concrete_ty == function)
                .unwrap()
                .payload_layout,
            Err(super::RustLayoutGap::FunctionLayoutUnknown(function))
        );
    }

    #[test]
    fn dynamic_layout_propagates_boxing_across_weakening_classes() {
        for recursive_target in [false, true] {
            let mut program = Program::default();
            let source = surface(&mut program, "Source");
            let target = surface(&mut program, "Target");
            let recursive_surface = if recursive_target { target } else { source };
            program.alloc_type(TypeData::Dyn(source));
            program.alloc_type(TypeData::Dyn(target));
            let dyn_ty = program.alloc_type(TypeData::Dyn(recursive_surface));
            let concrete = program.alloc_type(TypeData::Optional(dyn_ty));
            witness(&mut program, source, concrete);
            witness(&mut program, target, concrete);
            program.alloc_contract_weakening(air::ContractWeakeningDecl {
                source,
                target,
                target_to_source: vec![],
            });

            assert_eq!(
                payload_storage(&program, source, concrete),
                RustPayloadStorage::Boxed
            );
            assert_eq!(
                payload_storage(&program, target, concrete),
                RustPayloadStorage::Boxed
            );
        }
    }

    #[test]
    fn dynamic_layout_orders_inline_dependencies_and_reports_overflow() {
        let mut program = Program::default();
        let dependency = surface(&mut program, "Dependency");
        let dependent = surface(&mut program, "Dependent");
        program.alloc_type(TypeData::Dyn(dependent));
        let int = program.alloc_type(TypeData::Int);
        let dyn_dependency = program.alloc_type(TypeData::Dyn(dependency));
        let nested = program.alloc_type(TypeData::Optional(dyn_dependency));
        let overflow = program.alloc_type(TypeData::Array {
            elem: int,
            len: usize::MAX,
        });
        witness(&mut program, dependency, int);
        witness(&mut program, dependent, nested);
        witness(&mut program, dependent, overflow);
        let classes = TypePassClasses::analyze(&program);
        let layout = RustRepresentationPlan::new(&program, &classes)
            .dynamic_layout_plan()
            .unwrap();

        assert_eq!(layout.declaration_order, vec![dependency, dependent]);
        let dependent = layout
            .carriers
            .iter()
            .find(|carrier| carrier.surface == dependent)
            .unwrap();
        assert_eq!(dependent.dependencies, vec![dependency]);
        assert!(dependent.variants.iter().any(|variant| {
            variant.concrete_ty == overflow
                && variant.payload_layout == Err(super::RustLayoutGap::ArithmeticOverflow)
        }));
    }

    #[test]
    fn policy_classifies_current_storage_reps() {
        let mut program = RirProgram::default();
        let int = RirTypeId::from_index(program.types.len());
        program.types.push(RirType::Int);
        let string = RirTypeId::from_index(program.types.len());
        program.types.push(RirType::String);
        let list = RirTypeId::from_index(program.types.len());
        program.types.push(RirType::List(int));
        let array = RirTypeId::from_index(program.types.len());
        program.types.push(RirType::Array { elem: int, len: 2 });
        let policy = RirRustRepPolicy::new(&program);

        assert_eq!(policy.value_rep(int), RustValueRep::InlineCopy);
        assert_eq!(policy.value_rep(string), RustValueRep::CowString);
        assert_eq!(policy.value_rep(list), RustValueRep::CowList);
        assert_eq!(policy.value_rep(array), RustValueRep::InlineArray);
    }

    #[test]
    fn air_policy_preserves_current_profile_param_modes() {
        let mut program = Program::default();
        let int = program.alloc_type(TypeData::Int);
        let string = program.alloc_type(TypeData::String);
        let list = program.alloc_type(TypeData::List(int));
        let function = program.alloc_type(TypeData::Function(air::SignatureType::new(
            vec![],
            air::ReturnMode::Value(int),
        )));
        let classes = TypePassClasses::analyze(&program);
        let policy = RustRepresentationPlan::new(&program, &classes);

        assert!(policy.supports_param_mode(int, ParamMode::Value));
        assert!(policy.supports_param_mode(string, ParamMode::SharedBorrow));
        assert!(policy.supports_param_mode(string, ParamMode::Value));
        assert!(policy.supports_param_mode(list, ParamMode::Value));
        assert!(policy.supports_param_mode(list, ParamMode::MutBorrow));
        assert!(policy.copyable(function));
        assert!(policy.value_from_ref_supported(function));
    }

    #[test]
    fn air_policy_splits_map_key_and_value_support() {
        let mut program = Program::default();
        let int = program.alloc_type(TypeData::Int);
        let string = program.alloc_type(TypeData::String);
        let float = program.alloc_type(TypeData::Float);
        let tuple = program.alloc_type(TypeData::Tuple(vec![int, string]));
        let optional = program.alloc_type(TypeData::Optional(int));
        let module = program.alloc_module(air::Module::default());
        let aggregate = program.alloc_aggregate(air::AggregateDecl {
            name: Ident::new("Key"),
            module,
            kind: air::AggregateKind::Struct,
            type_args: vec![],
            const_args: vec![],
            fields: vec![FieldDecl {
                name: Ident::new("pos"),
                ty: tuple,
            }],
            cycle_capable: false,
            stringify_override: None,
        });
        let aggregate = program.alloc_type(TypeData::Aggregate(aggregate));
        let enm = program.alloc_enum(air::EnumDecl {
            name: Ident::new("KeyEnum"),
            module,
            type_args: vec![],
            const_args: vec![],
            core: None,
            repr: air::EnumRepr::Adt,
            raw_type: None,
            variants: vec![air::VariantDecl {
                name: Ident::new("Payload"),
                shape: air::VariantShape::Tuple(vec![aggregate]),
                raw_value: None,
            }],
        });
        let enm = program.alloc_type(TypeData::Enum(enm));
        let float_key = program.alloc_type(TypeData::Map {
            key: float,
            value: int,
            order: air::MapOrder::Insertion,
        });
        let float_value = program.alloc_type(TypeData::Map {
            key: int,
            value: float,
            order: air::MapOrder::Insertion,
        });
        let tuple_key = program.alloc_type(TypeData::Map {
            key: tuple,
            value: int,
            order: air::MapOrder::Insertion,
        });
        let aggregate_key = program.alloc_type(TypeData::Map {
            key: aggregate,
            value: int,
            order: air::MapOrder::Insertion,
        });
        let enum_key = program.alloc_type(TypeData::Map {
            key: enm,
            value: int,
            order: air::MapOrder::Insertion,
        });
        let optional_key = program.alloc_type(TypeData::Map {
            key: optional,
            value: int,
            order: air::MapOrder::Insertion,
        });
        let classes = TypePassClasses::analyze(&program);
        let policy = RustRepresentationPlan::new(&program, &classes);

        assert!(!policy.map_supported(float_key));
        assert!(!policy.map_supported(optional_key));
        assert!(policy.map_supported(float_value));
        assert!(policy.map_supported(tuple_key));
        assert!(policy.map_supported(aggregate_key));
        assert!(policy.map_supported(enum_key));
    }

    #[test]
    fn air_policy_names_stored_payload_boundaries() {
        let mut program = Program::default();
        let int = program.alloc_type(TypeData::Int);
        let string = program.alloc_type(TypeData::String);
        let list = program.alloc_type(TypeData::List(int));
        let slice = program.alloc_type(TypeData::Slice(int));
        let function = program.alloc_type(TypeData::Function(air::SignatureType::new(
            vec![],
            air::ReturnMode::Value(int),
        )));
        let module = program.alloc_module(air::Module::default());
        let aggregate = program.alloc_aggregate(air::AggregateDecl {
            name: Ident::new("Payload"),
            module,
            kind: air::AggregateKind::Struct,
            type_args: vec![],
            const_args: vec![],
            fields: vec![FieldDecl {
                name: Ident::new("xs"),
                ty: list,
            }],
            cycle_capable: false,
            stringify_override: None,
        });
        let aggregate = program.alloc_type(TypeData::Aggregate(aggregate));
        let slice_tuple = program.alloc_type(TypeData::Tuple(vec![slice]));
        let function_tuple = program.alloc_type(TypeData::Tuple(vec![function]));
        let classes = TypePassClasses::analyze(&program);
        let policy = RustRepresentationPlan::new(&program, &classes);

        assert!(policy.stored_payload_supported(string));
        assert!(policy.stored_payload_supported(list));
        assert!(policy.stored_payload_supported(aggregate));
        assert!(!policy.stored_payload_supported(slice));
        assert!(!policy.stored_payload_supported(function));
        assert!(!policy.stored_payload_supported(slice_tuple));
        assert!(policy.stored_payload_supported(function_tuple));
        assert_eq!(
            policy.storage_supported(function, LambdaStorageFamily::StructField),
            Ok(())
        );
        assert_eq!(
            policy.storage_supported(function_tuple, LambdaStorageFamily::MapKey),
            Err(LambdaStorageGap::MapKeyEqualityHash)
        );
        assert_eq!(
            policy.storage_supported(function_tuple, LambdaStorageFamily::MapValue),
            Ok(())
        );
        assert_eq!(
            policy.storage_supported(function_tuple, LambdaStorageFamily::GlobalRoot),
            Ok(())
        );
    }

    #[test]
    fn policy_keeps_string_borrow_as_str_view() {
        let mut program = RirProgram::default();
        let string = RirTypeId::from_index(program.types.len());
        program.types.push(RirType::String);
        let policy = RirRustRepPolicy::new(&program);

        assert_eq!(policy.borrow_view(string), RustBorrowView::Str);
        assert!(policy.cow_value(string));
    }

    #[test]
    fn policy_renders_scoped_and_escaping_lambda_params_separately() {
        let mut program = RirProgram::default();
        let int = RirTypeId::from_index(program.types.len());
        program.types.push(RirType::Int);
        let sig = RirLambdaSigId::from_index(0);
        program.lambda_sigs.push(RirLambdaSig {
            id: sig,
            params: vec![RirLambdaParam {
                ty: int,
                semantic: RirParamSemantic::Value,
                abi: RirParamAbi::Value,
                escape: RirParamEscape::NonEscaping,
            }],
            ret: int,
        });
        let lambda = RirTypeId::from_index(program.types.len());
        program.types.push(RirType::Lambda(sig));
        let policy = RirRustRepPolicy::new(&program);

        assert_eq!(
            policy.param_ty_with_lifetime(lambda, RirParamAbi::ScopedLambda, None),
            "anvyx_runtime::ScopedLambda<'_, 'cx, (i64,), i64>"
        );
        assert_eq!(
            policy.param_ty_with_lifetime(lambda, RirParamAbi::EscapingLambda, None),
            "anvyx_runtime::EscapingLambda<(i64,), i64>"
        );
    }

    #[test]
    fn policy_shares_structs_with_shareable_fields() {
        let mut program = RirProgram::default();
        let string = RirTypeId::from_index(program.types.len());
        program.types.push(RirType::String);
        let label = RirTypeId::from_index(program.types.len());
        program
            .types
            .push(RirType::Struct(RirStructId::from_index(0)));
        program.structs.push(RirStruct {
            id: RirStructId::from_index(0),
            air_id: None,
            symbol: RirSymbol::new("Label"),
            display: RirSymbol::new("Label"),
            native_path: None,
            native_ref: false,
            native_key: None,
            copyable: false,
            fields: vec![RirField {
                id: RirFieldId::from_index(0),
                symbol: RirSymbol::new("text"),
                ty: string,
            }],
        });
        let policy = RirRustRepPolicy::new(&program);

        assert!(!policy.copyable(label));
        assert!(policy.shareable_value(label));
    }

    #[test]
    fn rir_policy_supports_keyable_aggregates_and_owns_derives() {
        fn ty(index: usize) -> RirTypeId {
            RirTypeId::from_index(index)
        }

        fn field(id: usize, symbol: &str, ty: RirTypeId) -> RirField {
            RirField {
                id: RirFieldId::from_index(id),
                symbol: RirSymbol::new(symbol),
                ty,
            }
        }

        fn strukt(id: usize, symbol: &str, fields: Vec<RirField>) -> RirStruct {
            RirStruct {
                id: RirStructId::from_index(id),
                air_id: None,
                symbol: RirSymbol::new(symbol),
                display: RirSymbol::new(symbol),
                native_path: None,
                native_ref: false,
                native_key: None,
                copyable: false,
                fields,
            }
        }

        let key_fields = vec![field(0, "x", ty(0)), field(1, "name", ty(1))];
        let float_fields = vec![field(0, "x", ty(2))];
        let program = RirProgram {
            types: vec![
                RirType::Int,
                RirType::String,
                RirType::Float,
                RirType::Struct(RirStructId::from_index(0)),
                RirType::Struct(RirStructId::from_index(1)),
                RirType::Tuple(RirTupleId::from_index(0)),
                RirType::Enum(RirEnumId::from_index(0)),
                RirType::Option(ty(0)),
            ],
            structs: vec![
                strukt(0, "Key", key_fields.clone()),
                strukt(1, "FloatKey", float_fields.clone()),
            ],
            tuples: vec![RirTuple {
                id: RirTupleId::from_index(0),
                symbol: RirSymbol::new("Tuple"),
                display: RirSymbol::new("Tuple"),
                copyable: false,
                fields: key_fields.clone(),
            }],
            enums: vec![RirEnum {
                id: RirEnumId::from_index(0),
                air_id: None,
                native_path: None,
                native_key: None,
                core: None,
                repr: RirEnumRepr::Adt,
                raw_type: None,
                symbol: RirSymbol::new("KeyEnum"),
                display: RirSymbol::new("KeyEnum"),
                copyable: false,
                variants: vec![RirVariant {
                    id: RirVariantId::from_index(0),
                    symbol: RirSymbol::new("Payload"),
                    display: RirSymbol::new("Payload"),
                    kind: RirVariantKind::Tuple,
                    raw_value: None,
                    fields: key_fields.clone(),
                }],
            }],
            ..RirProgram::default()
        };
        let policy = RirRustRepPolicy::new(&program);

        assert!(policy.map_key_supported(ty(3)));
        assert!(policy.map_key_supported(ty(5)));
        assert!(policy.map_key_supported(ty(6)));
        assert!(!policy.map_key_supported(ty(4)));
        assert!(!policy.map_key_supported(ty(7)));
        assert_eq!(
            policy.record_derives(&key_fields),
            vec!["Clone", "PartialEq", "Eq", "Hash"]
        );
        assert_eq!(policy.record_derives(&float_fields), vec!["Clone"]);
        assert_eq!(
            policy.enum_derives(&program.enums[0]),
            vec!["Clone", "PartialEq", "Eq", "Hash"]
        );
    }

    #[test]
    fn policy_treats_datarefs_as_shared_handles_not_copy_payloads() {
        let mut program = RirProgram::default();
        let int = RirTypeId::from_index(program.types.len());
        program.types.push(RirType::Int);
        let node = RirTypeId::from_index(program.types.len());
        program
            .types
            .push(RirType::DataRef(RirDataRefId::from_index(0)));
        program.datarefs.push(RirDataRef {
            id: RirDataRefId::from_index(0),
            air_id: air::AggregateId::from_index(0),
            native_key: None,
            symbol: RirSymbol::new("Node"),
            display: RirSymbol::new("Node"),
            cycle_capable: true,
            fields: vec![RirField {
                id: RirFieldId::from_index(0),
                symbol: RirSymbol::new("value"),
                ty: int,
            }],
        });
        let maybe_node = RirTypeId::from_index(program.types.len());
        program.types.push(RirType::Option(node));
        let policy = RirRustRepPolicy::new(&program);

        assert_eq!(policy.value_rep(node), RustValueRep::HeapHandle);
        assert!(!policy.copyable(node));
        assert!(policy.shareable_value(node));
        assert!(policy.supports_param(node, RirParamSemantic::Value));
        assert!(policy.supports_param(node, RirParamSemantic::SharedBorrow));
        assert!(policy.supports_param(node, RirParamSemantic::MutBorrow));
        assert_eq!(policy.borrow_view(node), RustBorrowView::Ref);
        assert_eq!(policy.rust_ty(node), "Node<'cx>");
        assert!(!policy.cow_value(node));
        assert!(!policy.copyable(maybe_node));
        assert!(policy.shareable_value(maybe_node));
    }

    #[test]
    fn policy_classifies_materialization() {
        let mut program = RirProgram::default();
        let int = RirTypeId::from_index(program.types.len());
        program.types.push(RirType::Int);
        let string = RirTypeId::from_index(program.types.len());
        program.types.push(RirType::String);
        let list = RirTypeId::from_index(program.types.len());
        program.types.push(RirType::List(int));
        let map = RirTypeId::from_index(program.types.len());
        program.types.push(RirType::Map {
            key: int,
            value: string,
        });
        let node = RirTypeId::from_index(program.types.len());
        program
            .types
            .push(RirType::DataRef(RirDataRefId::from_index(0)));
        program.datarefs.push(RirDataRef {
            id: RirDataRefId::from_index(0),
            air_id: air::AggregateId::from_index(0),
            native_key: None,
            symbol: RirSymbol::new("Node"),
            display: RirSymbol::new("Node"),
            cycle_capable: true,
            fields: vec![RirField {
                id: RirFieldId::from_index(0),
                symbol: RirSymbol::new("value"),
                ty: int,
            }],
        });
        let tuple = RirTypeId::from_index(program.types.len());
        program
            .types
            .push(RirType::Tuple(RirTupleId::from_index(0)));
        program.tuples.push(RirTuple {
            id: RirTupleId::from_index(0),
            symbol: RirSymbol::new("Tuple0"),
            display: RirSymbol::new("Tuple0"),
            fields: vec![
                RirField {
                    id: RirFieldId::from_index(0),
                    symbol: RirSymbol::new("_0"),
                    ty: string,
                },
                RirField {
                    id: RirFieldId::from_index(1),
                    symbol: RirSymbol::new("_1"),
                    ty: node,
                },
            ],
            copyable: false,
        });
        let maybe_node = RirTypeId::from_index(program.types.len());
        program.types.push(RirType::Option(node));
        let slice = RirTypeId::from_index(program.types.len());
        program.types.push(RirType::Slice(int));
        let lambda_sig = RirLambdaSigId::from_index(0);
        let lambda = RirTypeId::from_index(program.types.len());
        program.types.push(RirType::Lambda(lambda_sig));
        program.lambda_sigs.push(RirLambdaSig {
            id: lambda_sig,
            params: vec![],
            ret: int,
        });
        program.lambdas.push(RirLambda {
            id: RirLambdaId::from_index(0),
            source: RirLambdaSource::Function(FunctionId::from_index(0)),
            function: RirFunctionId::from_index(0),
            sig: lambda_sig,
            escape: RirLambdaEscape::NonEscaping,
            storage: RirLambdaStorage::ZeroEnv,
            captures: vec![],
        });
        let slice_tuple = RirTypeId::from_index(program.types.len());
        program
            .types
            .push(RirType::Tuple(RirTupleId::from_index(1)));
        program.tuples.push(RirTuple {
            id: RirTupleId::from_index(1),
            symbol: RirSymbol::new("Tuple1"),
            display: RirSymbol::new("Tuple1"),
            fields: vec![RirField {
                id: RirFieldId::from_index(0),
                symbol: RirSymbol::new("_0"),
                ty: slice,
            }],
            copyable: false,
        });
        let lambda_tuple = RirTypeId::from_index(program.types.len());
        program
            .types
            .push(RirType::Tuple(RirTupleId::from_index(2)));
        program.tuples.push(RirTuple {
            id: RirTupleId::from_index(2),
            symbol: RirSymbol::new("Tuple2"),
            display: RirSymbol::new("Tuple2"),
            fields: vec![RirField {
                id: RirFieldId::from_index(0),
                symbol: RirSymbol::new("_0"),
                ty: lambda,
            }],
            copyable: false,
        });
        let lambda_enum = RirTypeId::from_index(program.types.len());
        program.types.push(RirType::Enum(RirEnumId::from_index(0)));
        program.enums.push(RirEnum {
            id: RirEnumId::from_index(0),
            air_id: None,
            native_path: None,
            native_key: None,
            core: None,
            repr: RirEnumRepr::Adt,
            raw_type: None,
            symbol: RirSymbol::new("LambdaEnum"),
            display: RirSymbol::new("LambdaEnum"),
            copyable: false,
            variants: vec![RirVariant {
                id: RirVariantId::from_index(0),
                symbol: RirSymbol::new("Payload"),
                display: RirSymbol::new("Payload"),
                kind: RirVariantKind::Tuple,
                raw_value: None,
                fields: vec![RirField {
                    id: RirFieldId::from_index(0),
                    symbol: RirSymbol::new("_0"),
                    ty: lambda,
                }],
            }],
        });
        let lambda_key_map = RirTypeId::from_index(program.types.len());
        program.types.push(RirType::Map {
            key: lambda_tuple,
            value: int,
        });
        let lambda_value_map = RirTypeId::from_index(program.types.len());
        program.types.push(RirType::Map {
            key: int,
            value: lambda_tuple,
        });
        let nested_lambda_key_map_value = RirTypeId::from_index(program.types.len());
        program.types.push(RirType::Map {
            key: int,
            value: lambda_key_map,
        });
        let policy = RirRustRepPolicy::new(&program);

        assert_eq!(policy.materialization(int), RustMaterialization::Copy);
        assert_eq!(policy.materialization(string), RustMaterialization::Share);
        assert_eq!(policy.materialization(list), RustMaterialization::Share);
        assert_eq!(policy.materialization(map), RustMaterialization::Share);
        assert_eq!(
            policy.materialization(node),
            RustMaterialization::CloneHandle
        );
        assert_eq!(policy.materialization(tuple), RustMaterialization::Share);
        assert_eq!(
            policy.materialization(maybe_node),
            RustMaterialization::Share
        );
        assert_eq!(
            policy.materialization(slice),
            RustMaterialization::BorrowGuard
        );
        assert_eq!(policy.materialization(lambda), RustMaterialization::Copy);
        assert_eq!(
            policy.materialization(slice_tuple),
            RustMaterialization::Gap
        );
        assert_eq!(
            policy.materialization(lambda_tuple),
            RustMaterialization::Share
        );
        assert!(policy.stored_payload_supported(list));
        assert!(policy.stored_payload_supported(map));
        assert!(policy.stored_payload_supported(node));
        assert!(!policy.stored_payload_supported(slice));
        assert!(!policy.stored_payload_supported(lambda));
        assert_eq!(
            policy.storage_supported(lambda, LambdaStorageFamily::StructField),
            Ok(())
        );
        assert_eq!(
            policy.storage_supported(lambda_tuple, LambdaStorageFamily::MapKey),
            Err(LambdaStorageGap::MapKeyEqualityHash)
        );
        assert_eq!(
            policy.storage_supported(lambda_enum, LambdaStorageFamily::MapKey),
            Err(LambdaStorageGap::MapKeyEqualityHash)
        );
        assert_eq!(
            policy.storage_supported(lambda_tuple, LambdaStorageFamily::MapValue),
            Ok(())
        );
        assert_eq!(
            policy.storage_supported(nested_lambda_key_map_value, LambdaStorageFamily::MapValue),
            Err(LambdaStorageGap::MapKeyEqualityHash)
        );
        assert_eq!(
            policy
                .materialization_plan_for(
                    lambda_tuple,
                    RustMaterialSource::ExactGlobalRoot,
                    RustMaterialIntent::Read,
                )
                .gap,
            None
        );
        assert_eq!(
            policy
                .materialization_plan_for(
                    lambda_key_map,
                    RustMaterialSource::ExactGlobalRoot,
                    RustMaterialIntent::Read,
                )
                .gap,
            Some(RustMaterialGap::UnsupportedType)
        );
        assert_eq!(
            policy
                .materialization_plan_for(
                    lambda_value_map,
                    RustMaterialSource::ExactGlobalRoot,
                    RustMaterialIntent::Read,
                )
                .gap,
            None
        );
    }

    #[test]
    fn policy_rejects_non_cloneable_lambda_materialization() {
        let mut program = RirProgram::default();
        let int = RirTypeId::from_index(program.types.len());
        program.types.push(RirType::Int);
        let lambda_sig = RirLambdaSigId::from_index(0);
        let lambda = RirTypeId::from_index(program.types.len());
        program.types.push(RirType::Lambda(lambda_sig));
        program.lambda_sigs.push(RirLambdaSig {
            id: lambda_sig,
            params: vec![],
            ret: int,
        });
        program.lambdas.push(RirLambda {
            id: RirLambdaId::from_index(0),
            source: RirLambdaSource::Function(FunctionId::from_index(0)),
            function: RirFunctionId::from_index(0),
            sig: lambda_sig,
            escape: RirLambdaEscape::NonEscaping,
            storage: RirLambdaStorage::ScopedCaptures,
            captures: vec![RirLambdaCapture {
                ty: int,
                semantic: RirParamSemantic::MutBorrow,
                abi: RirParamAbi::MutBorrow,
                kind: RirLambdaCaptureKind::Param,
            }],
        });
        let policy = RirRustRepPolicy::new(&program);

        assert_eq!(policy.materialization(lambda), RustMaterialization::Gap);
        assert!(!policy.value_from_ref_supported(lambda));
    }

    #[test]
    fn policy_centralizes_lambda_storage_and_layout_edges() {
        let mut program = RirProgram::default();
        let int = RirTypeId::from_index(0);
        let lambda_a_ty = RirTypeId::from_index(1);
        let lambda_b_ty = RirTypeId::from_index(2);
        let sig_a = RirLambdaSigId::from_index(0);
        let sig_b = RirLambdaSigId::from_index(1);
        program.types.push(RirType::Int);
        program.types.push(RirType::Lambda(sig_a));
        program.types.push(RirType::Lambda(sig_b));
        program.lambda_sigs.push(RirLambdaSig {
            id: sig_a,
            params: vec![],
            ret: int,
        });
        program.lambda_sigs.push(RirLambdaSig {
            id: sig_b,
            params: vec![],
            ret: int,
        });
        program.lambdas.push(RirLambda {
            id: RirLambdaId::from_index(0),
            source: RirLambdaSource::Function(FunctionId::from_index(0)),
            function: RirFunctionId::from_index(0),
            sig: sig_a,
            escape: RirLambdaEscape::NonEscaping,
            storage: RirLambdaStorage::ScopedCaptures,
            captures: vec![RirLambdaCapture {
                ty: lambda_b_ty,
                semantic: RirParamSemantic::SharedBorrow,
                abi: RirParamAbi::SharedBorrow,
                kind: RirLambdaCaptureKind::Param,
            }],
        });
        program.lambdas.push(RirLambda {
            id: RirLambdaId::from_index(1),
            source: RirLambdaSource::Function(FunctionId::from_index(1)),
            function: RirFunctionId::from_index(1),
            sig: sig_a,
            escape: RirLambdaEscape::Escaping,
            storage: RirLambdaStorage::HeapEnv {
                env: RirLambdaEnvId::from_index(0),
            },
            captures: vec![],
        });
        program.lambdas.push(RirLambda {
            id: RirLambdaId::from_index(2),
            source: RirLambdaSource::Function(FunctionId::from_index(2)),
            function: RirFunctionId::from_index(2),
            sig: sig_b,
            escape: RirLambdaEscape::NonEscaping,
            storage: RirLambdaStorage::ScopedCaptures,
            captures: vec![RirLambdaCapture {
                ty: lambda_a_ty,
                semantic: RirParamSemantic::Value,
                abi: RirParamAbi::Value,
                kind: RirLambdaCaptureKind::Param,
            }],
        });
        let policy = RirRustRepPolicy::new(&program);

        let shape = policy.lambda_sig_storage_shape(sig_a);
        assert!(shape.heap_env);
        assert!(shape.lifetime);
        assert_eq!(policy.callable_ret_ty(lambda_a_ty), "LambdaSig0<'cx, 'cx>");
        assert_eq!(
            policy.callable_param_ty(lambda_a_ty, RirParamAbi::Value, RirParamEscape::NonEscaping),
            "LambdaSig0<'_, 'cx>"
        );
        assert_eq!(
            policy.callable_param_ty(lambda_a_ty, RirParamAbi::Value, RirParamEscape::Escaping),
            "LambdaSig0<'cx, 'cx>"
        );
        let lambda_field = RirLambdaEnvField {
            ty: lambda_a_ty,
            symbol: RirSymbol::new("f"),
            kind: RirLambdaEnvFieldKind::Value,
        };
        let lambda_cell_field = RirLambdaEnvField {
            ty: lambda_a_ty,
            symbol: RirSymbol::new("cell"),
            kind: RirLambdaEnvFieldKind::HeapCell {
                cell: RirCellId::from_index(0),
            },
        };
        assert_eq!(
            policy.lambda_env_field_ty(&lambda_field),
            "LambdaSig0<'cx, 'cx>"
        );
        assert!(policy.lambda_env_field_storage_supported(&lambda_field));
        assert!(policy.lambda_env_field_storage_supported(&lambda_cell_field));
        assert_eq!(
            policy.inline_lambda_value_sig(&program.lambdas[0], &program.lambdas[0].captures[0]),
            None
        );
        assert_eq!(
            policy.inline_lambda_value_sig(&program.lambdas[2], &program.lambdas[2].captures[0]),
            Some(sig_a)
        );
        let same_sig_shared = RirLambda {
            id: RirLambdaId::from_index(3),
            source: RirLambdaSource::Function(FunctionId::from_index(3)),
            function: RirFunctionId::from_index(3),
            sig: sig_a,
            escape: RirLambdaEscape::NonEscaping,
            storage: RirLambdaStorage::ScopedCaptures,
            captures: vec![RirLambdaCapture {
                ty: lambda_a_ty,
                semantic: RirParamSemantic::SharedBorrow,
                abi: RirParamAbi::SharedBorrow,
                kind: RirLambdaCaptureKind::Param,
            }],
        };
        let same_sig_value = RirLambda {
            id: RirLambdaId::from_index(4),
            source: RirLambdaSource::Function(FunctionId::from_index(4)),
            function: RirFunctionId::from_index(4),
            sig: sig_a,
            escape: RirLambdaEscape::NonEscaping,
            storage: RirLambdaStorage::ScopedCaptures,
            captures: vec![RirLambdaCapture {
                ty: lambda_a_ty,
                semantic: RirParamSemantic::Value,
                abi: RirParamAbi::Value,
                kind: RirLambdaCaptureKind::Param,
            }],
        };
        let same_sig_heap_env = RirLambda {
            id: RirLambdaId::from_index(5),
            source: RirLambdaSource::Function(FunctionId::from_index(5)),
            function: RirFunctionId::from_index(5),
            sig: sig_a,
            escape: RirLambdaEscape::Escaping,
            storage: RirLambdaStorage::HeapEnv {
                env: RirLambdaEnvId::from_index(1),
            },
            captures: vec![RirLambdaCapture {
                ty: lambda_a_ty,
                semantic: RirParamSemantic::Value,
                abi: RirParamAbi::Value,
                kind: RirLambdaCaptureKind::Param,
            }],
        };
        assert!(!policy.lambda_has_recursive_inline_value_capture(&same_sig_shared));
        assert!(policy.lambda_has_recursive_inline_value_capture(&same_sig_value));
        assert!(!policy.lambda_has_recursive_inline_value_capture(&same_sig_heap_env));
        assert!(policy.lambda_sig_reaches_inline_lambda_value(sig_b, sig_a));
        assert!(!policy.lambda_sig_reaches_inline_lambda_value(sig_a, sig_b));
    }

    #[test]
    fn air_policy_rejects_scoped_function_value_by_value() {
        let mut program = Program::default();
        let void = program.alloc_type(TypeData::Void);
        let lambda = program.alloc_type(TypeData::Function(air::SignatureType::new(
            vec![],
            air::ReturnMode::Value(void),
        )));
        let classes = TypePassClasses::analyze(&program);
        let policy = RustRepresentationPlan::new(&program, &classes);

        assert_eq!(
            policy.scoped_function_value_gap(
                lambda,
                air::PlaceRoot::ScopedBorrow(air::ScopedBorrowId::from_index(0)),
            ),
            Some(LambdaStorageGap::Lifetime)
        );
        assert_eq!(
            policy.scoped_function_value_gap(
                lambda,
                air::PlaceRoot::Local(air::LocalId::from_index(0)),
            ),
            None
        );
    }

    #[test]
    fn air_policy_shares_aggregates_with_shareable_fields() {
        let mut program = Program::default();
        let string = program.alloc_type(TypeData::String);
        let module = program.alloc_module(air::Module::default());
        let aggregate = program.alloc_aggregate(air::AggregateDecl {
            name: Ident::new("Label"),
            module,
            kind: air::AggregateKind::Struct,
            type_args: vec![],
            const_args: vec![],
            fields: vec![FieldDecl {
                name: Ident::new("text"),
                ty: string,
            }],
            cycle_capable: false,
            stringify_override: None,
        });
        let label = program.alloc_type(TypeData::Aggregate(aggregate));
        let classes = TypePassClasses::analyze(&program);
        let policy = RustRepresentationPlan::new(&program, &classes);

        assert!(!policy.copyable(label));
        assert!(policy.value_place_shareable(label));
    }

    #[test]
    fn air_policy_allows_collection_broken_payload_cycles() {
        let mut program = Program::default();
        let module = program.alloc_module(air::Module::default());
        let a = program.alloc_aggregate(air::AggregateDecl {
            name: Ident::new("A"),
            module,
            kind: air::AggregateKind::Struct,
            type_args: vec![],
            const_args: vec![],
            fields: vec![],
            cycle_capable: false,
            stringify_override: None,
        });
        let b = program.alloc_aggregate(air::AggregateDecl {
            name: Ident::new("B"),
            module,
            kind: air::AggregateKind::Struct,
            type_args: vec![],
            const_args: vec![],
            fields: vec![],
            cycle_capable: false,
            stringify_override: None,
        });
        let a_ty = program.alloc_type(TypeData::Aggregate(a));
        let b_ty = program.alloc_type(TypeData::Aggregate(b));
        let list_b = program.alloc_type(TypeData::List(b_ty));
        let maybe_a = program.alloc_type(TypeData::Optional(a_ty));
        program.aggregate_mut(a).fields.push(FieldDecl {
            name: Ident::new("bs"),
            ty: list_b,
        });
        program.aggregate_mut(b).fields.push(FieldDecl {
            name: Ident::new("a"),
            ty: maybe_a,
        });
        let classes = TypePassClasses::analyze(&program);
        let policy = RustRepresentationPlan::new(&program, &classes);

        assert!(policy.stored_payload_supported(a_ty));
        assert_eq!(policy.materialization(a_ty), RustMaterialization::Share);
    }

    #[test]
    fn rir_policy_allows_collection_broken_payload_cycles() {
        let mut program = RirProgram {
            types: vec![
                RirType::Int,
                RirType::Struct(RirStructId::from_index(0)),
                RirType::Struct(RirStructId::from_index(1)),
                RirType::List(RirTypeId::from_index(2)),
                RirType::Option(RirTypeId::from_index(1)),
            ],
            ..RirProgram::default()
        };
        program.structs.push(RirStruct {
            id: RirStructId::from_index(0),
            air_id: None,
            symbol: RirSymbol::new("A"),
            display: RirSymbol::new("A"),
            native_path: None,
            native_ref: false,
            native_key: None,
            copyable: false,
            fields: vec![RirField {
                id: RirFieldId::from_index(0),
                symbol: RirSymbol::new("bs"),
                ty: RirTypeId::from_index(3),
            }],
        });
        program.structs.push(RirStruct {
            id: RirStructId::from_index(1),
            air_id: None,
            symbol: RirSymbol::new("B"),
            display: RirSymbol::new("B"),
            native_path: None,
            native_ref: false,
            native_key: None,
            copyable: false,
            fields: vec![RirField {
                id: RirFieldId::from_index(0),
                symbol: RirSymbol::new("a"),
                ty: RirTypeId::from_index(4),
            }],
        });
        let policy = RirRustRepPolicy::new(&program);
        let a = RirTypeId::from_index(1);

        assert!(policy.stored_payload_supported(a));
        assert!(policy.type_owns_heap_edges(a));
        assert!(policy.type_cx_dependent(a));
        assert_eq!(policy.materialization(a), RustMaterialization::Share);
        assert!(RustTracePlan::build(&program).needs_struct_trace(RirStructId::from_index(0)));
    }

    #[test]
    fn retained_callback_sigs_seed_trace_plan() {
        let int = RirTypeId::from_index(0);
        let lambda_ty = RirTypeId::from_index(1);
        let sig = RirLambdaSigId::from_index(0);
        let callback = anvyx_runtime::ExternCallbackSignature {
            params: vec![],
            ret: Box::new(anvyx_runtime::ExternTypeExpr::Int),
            policy: anvyx_runtime::CallbackPolicy {
                escape: anvyx_runtime::CallbackEscape::Escaping,
                thread: anvyx_runtime::CallbackThread::SameThread,
            },
        };
        let program = RirProgram {
            types: vec![RirType::Int, RirType::Lambda(sig)],
            lambda_sigs: vec![RirLambdaSig {
                id: sig,
                params: vec![],
                ret: int,
            }],
            lambdas: vec![RirLambda {
                id: RirLambdaId::from_index(0),
                source: RirLambdaSource::Function(FunctionId::from_index(0)),
                function: RirFunctionId::from_index(0),
                sig,
                escape: RirLambdaEscape::Escaping,
                storage: RirLambdaStorage::ScopedCaptures,
                captures: vec![RirLambdaCapture {
                    ty: int,
                    semantic: RirParamSemantic::HeapCell,
                    abi: RirParamAbi::HeapCell,
                    kind: RirLambdaCaptureKind::HeapCell {
                        cell: RirCellId::from_index(0),
                    },
                }],
            }],
            externs: vec![RirExtern {
                id: RirExternId::from_index(0),
                air_id: None,
                symbol: RirSymbol::new("retain"),
                kind: RirExternKind::Native(RirNativeExtern {
                    path: vec!["host".to_string(), "retain".to_string()],
                    abi: anvyx_runtime::RustExternAbi {
                        params: vec![anvyx_runtime::RustParamAbi::AnvCallback(callback.clone())],
                        ret: anvyx_runtime::RustReturnAbi::Void,
                        fallible: false,
                        support: anvyx_runtime::RustAbiSupport::NeedsWrapperConversion,
                        ctx: anvyx_runtime::RustWrapperCtx::HiddenRuntime,
                    },
                    callback_receiver: None,
                }),
                params: vec![RirExternParam {
                    ty: lambda_ty,
                    semantic: RirParamSemantic::AnvCallback,
                    abi: RirParamAbi::AnvCallback,
                    escape: RirParamEscape::Escaping,
                }],
                ret: int,
                abi: air::ExternAbi {
                    params: vec![anvyx_runtime::ExternTypeExpr::Callback(callback)],
                    ret: anvyx_runtime::ExternTypeExpr::Void,
                },
            }],
            ..RirProgram::default()
        };

        assert!(RustTracePlan::build(&program).needs_lambda_sig_trace(sig));
    }

    #[test]
    fn air_policy_treats_datarefs_as_shared_handles_not_copy_payloads() {
        let mut program = Program::default();
        let int = program.alloc_type(TypeData::Int);
        let module = program.alloc_module(air::Module::default());
        let aggregate = program.alloc_aggregate(air::AggregateDecl {
            name: Ident::new("Node"),
            module,
            kind: air::AggregateKind::DataRef,
            type_args: vec![],
            const_args: vec![],
            fields: vec![FieldDecl {
                name: Ident::new("value"),
                ty: int,
            }],
            cycle_capable: true,
            stringify_override: None,
        });
        let node = program.alloc_type(TypeData::DataRef(aggregate));
        let maybe_node = program.alloc_type(TypeData::Optional(node));
        let classes = TypePassClasses::analyze(&program);
        let policy = RustRepresentationPlan::new(&program, &classes);

        assert!(!policy.copyable(node));
        assert!(policy.value_place_shareable(node));
        assert!(policy.supports_param_mode(node, ParamMode::Value));
        assert!(policy.supports_param_mode(node, ParamMode::SharedBorrow));
        assert!(policy.supports_param_mode(node, ParamMode::MutBorrow));
        assert!(!policy.copyable(maybe_node));
        assert!(policy.value_place_shareable(maybe_node));
    }
}
