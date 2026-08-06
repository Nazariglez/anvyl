use std::collections::{BTreeMap, BTreeSet, HashMap};

use anvyx_frontend::air::{
    self, AggregateKind, ContractSurfaceId, ContractWitnessId, ContractWitnessTarget,
    Program as AirProgram, TypeData, TypeId, TypePassClass, TypePassClasses, VariantShape,
};

use super::{
    rir::{
        RirCellDecl, RirCellStorage, RirCollectionStorageKind, RirCopyEvidence, RirDataRef,
        RirDynCarrierId, RirDynVariantId, RirEnum, RirEnumId, RirField, RirFunctionId, RirLambda,
        RirLambdaCapture, RirLambdaEnvField, RirLambdaEnvFieldKind, RirLambdaEnvLayout,
        RirLambdaId, RirLambdaSigId, RirLambdaStorage, RirMaterializer, RirMaterializerAction,
        RirMaterializerId, RirParamEscape, RirPassMode, RirProgram, RirStruct, RirStructId,
        RirSupportEvidence, RirTuple, RirTupleId, RirType, RirTypeId,
    },
    target,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RustBorrowView {
    Ref,
    Str,
    TargetGap,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum RustRecipePosition {
    Value,
    StoredPayload(LambdaStorageFamily),
    MapKey,
    Global,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RustRecipeGap {
    UnsupportedType(TypeId),
    FunctionCapabilityPending(TypeId),
    UnsupportedStorage {
        ty: TypeId,
        family: LambdaStorageFamily,
    },
    UnsupportedMapKey(TypeId),
    UnsupportedGlobal(TypeId),
}

#[derive(Debug, Clone)]
enum RustMaterializerNode {
    Reserved,
    Filling,
    Ready {
        action: RirMaterializerAction,
        copy: Option<RirCopyEvidence>,
        support: Option<RirSupportEvidence>,
    },
}

#[derive(Debug, Clone, Default)]
pub struct RustMaterializerGraph {
    nodes: Vec<RustMaterializerNode>,
    types: Vec<TypeId>,
    positions: Vec<RustRecipePosition>,
    entries: BTreeMap<(TypeId, RustRecipePosition), RirMaterializerId>,
    filled: bool,
    native_types: HashMap<
        (
            String,
            anvyx_runtime::ProviderId,
            anvyx_runtime::ExternTypeKey,
        ),
        anvyx_runtime::RustTypeBinding,
    >,
    callables: HashMap<TypeId, RirMaterializerAction>,
}

impl RustMaterializerGraph {
    pub fn with_native_support(supports: &[anvyx_runtime::RustProviderSupport]) -> Self {
        let mut graph = Self::default();
        for support in supports {
            for module in &support.modules {
                for ty in &module.types {
                    graph.native_types.insert(
                        (
                            support.package.clone(),
                            support.provider.clone(),
                            ty.key.clone(),
                        ),
                        ty.clone(),
                    );
                }
            }
        }
        graph
    }

    pub fn native_type(
        &self,
        binding: &air::ExternTypeBindingDecl,
    ) -> Option<&anvyx_runtime::RustTypeBinding> {
        self.native_types.get(&(
            binding.package.as_str().to_string(),
            binding.provider.clone(),
            binding.key.clone(),
        ))
    }

    pub fn set_callable_materializers(
        &mut self,
        callables: impl IntoIterator<Item = (TypeId, RirMaterializerAction)>,
    ) {
        self.nodes.clear();
        self.types.clear();
        self.positions.clear();
        self.entries.clear();
        self.filled = false;
        self.callables.clear();
        self.callables.extend(callables);
    }

    pub fn reserve(
        &mut self,
        plan: RustRepresentationPlan<'_>,
        ty: TypeId,
        position: RustRecipePosition,
        carriers: &HashMap<ContractSurfaceId, RirDynCarrierId>,
        variants: &HashMap<ContractWitnessId, RirDynVariantId>,
    ) -> Result<RirMaterializerId, RustRecipeGap> {
        let start = self.nodes.len();
        match self.visit(plan, ty, position, carriers, variants, false) {
            Ok(id) => Ok(id),
            Err(gap) => {
                self.nodes.truncate(start);
                self.types.truncate(start);
                self.positions.truncate(start);
                self.entries.retain(|_, id| id.index() < start);
                Err(gap)
            }
        }
    }

    pub fn fill(
        &mut self,
        plan: RustRepresentationPlan<'_>,
        carriers: &HashMap<ContractSurfaceId, RirDynCarrierId>,
        variants: &HashMap<ContractWitnessId, RirDynVariantId>,
    ) -> Result<(), RustRecipeGap> {
        for index in 0..self.nodes.len() {
            let ty = self.types[index];
            let position = self.positions[index];
            self.visit(plan, ty, position, carriers, variants, true)?;
        }
        self.filled = true;
        Ok(())
    }

    pub fn get(&self, ty: TypeId, position: RustRecipePosition) -> Option<RirMaterializerId> {
        debug_assert!(self.filled);
        self.entries.get(&(ty, position)).copied()
    }

    pub fn freeze(&mut self, type_map: &HashMap<TypeId, RirTypeId>) -> Vec<RirMaterializer> {
        let nodes = std::mem::take(&mut self.nodes);
        let types = std::mem::take(&mut self.types);
        let positions = std::mem::take(&mut self.positions);
        nodes
            .into_iter()
            .zip(types)
            .zip(positions)
            .enumerate()
            .map(|(index, ((node, ty), position))| {
                let RustMaterializerNode::Ready {
                    action,
                    copy,
                    support,
                } = node
                else {
                    panic!("materializer table frozen before fill");
                };
                RirMaterializer {
                    id: RirMaterializerId::from_index(index),
                    ty: type_map[&ty],
                    position,
                    action,
                    copy,
                    support,
                }
            })
            .collect()
    }

    fn check_position(
        plan: RustRepresentationPlan<'_>,
        ty: TypeId,
        position: RustRecipePosition,
    ) -> Result<(), RustRecipeGap> {
        let supported = match position {
            RustRecipePosition::Value => true,
            RustRecipePosition::StoredPayload(family) => match plan.program.type_arena.data(ty) {
                TypeData::Function(_) => family.allows_function_payload(),
                TypeData::Dyn(_) => family.allows_dynamic_owned(),
                TypeData::Void | TypeData::Any | TypeData::Slice(_) => false,
                _ => true,
            },
            RustRecipePosition::MapKey => match plan.program.type_arena.data(ty) {
                TypeData::Int
                | TypeData::Flag(_)
                | TypeData::Bool
                | TypeData::Char
                | TypeData::String
                | TypeData::Tuple(_)
                | TypeData::Enum(_) => true,
                TypeData::Aggregate(id) => {
                    plan.program.aggregate(*id).kind == AggregateKind::Struct
                }
                _ => false,
            },
            RustRecipePosition::Global => match plan.program.type_arena.data(ty) {
                TypeData::Void | TypeData::Any | TypeData::Slice(_) => false,
                TypeData::Extern(id) => {
                    let ext = plan.program.extern_type(*id);
                    ext.rep == air::ExternRep::Shared || ext.owns_heap_edges == Some(false)
                }
                TypeData::Function(_) => LambdaStorageFamily::GlobalRoot.allows_function_payload(),
                _ => true,
            },
        };
        if supported {
            return Ok(());
        }
        Err(match position {
            RustRecipePosition::StoredPayload(family) => {
                RustRecipeGap::UnsupportedStorage { ty, family }
            }
            RustRecipePosition::MapKey => RustRecipeGap::UnsupportedMapKey(ty),
            RustRecipePosition::Global => RustRecipeGap::UnsupportedGlobal(ty),
            RustRecipePosition::Value => RustRecipeGap::UnsupportedType(ty),
        })
    }

    fn visit(
        &mut self,
        plan: RustRepresentationPlan<'_>,
        ty: TypeId,
        position: RustRecipePosition,
        carriers: &HashMap<ContractSurfaceId, RirDynCarrierId>,
        variants: &HashMap<ContractWitnessId, RirDynVariantId>,
        fill: bool,
    ) -> Result<RirMaterializerId, RustRecipeGap> {
        Self::check_position(plan, ty, position)?;
        let key = (ty, position);
        let id = match self.entries.get(&key).copied() {
            Some(id) => match (&self.nodes[id.index()], fill) {
                (RustMaterializerNode::Ready { .. }, _)
                | (RustMaterializerNode::Filling, true)
                | (RustMaterializerNode::Reserved, false) => return Ok(id),
                (RustMaterializerNode::Filling, false) => unreachable!(),
                (RustMaterializerNode::Reserved, true) => {
                    self.nodes[id.index()] = RustMaterializerNode::Filling;
                    id
                }
            },
            None => {
                if fill || self.filled {
                    return Err(RustRecipeGap::UnsupportedType(ty));
                }
                let id = RirMaterializerId::from_index(self.nodes.len());
                self.entries.insert(key, id);
                self.nodes.push(RustMaterializerNode::Reserved);
                self.types.push(ty);
                self.positions.push(position);
                id
            }
        };

        let (copyable, copy) = self.copy_evidence(plan, ty, position, carriers, variants, fill)?;
        let support = self.support_evidence(plan, ty, position, carriers, variants, fill)?;
        let action = if let TypeData::Extern(extern_id) = plan.program.type_arena.data(ty) {
            let decl = plan.program.extern_type(*extern_id);
            let native = decl.binding.as_ref().and_then(|binding| {
                self.native_types.get(&(
                    binding.package.as_str().to_string(),
                    binding.provider.clone(),
                    binding.key.clone(),
                ))
            });
            match (decl.rep, decl.materialization, native) {
                (
                    air::ExternRep::Inline,
                    Some(mode @ anvyx_runtime::ExternMaterialization::Copy),
                    Some(native),
                ) if native.validated_materializer(mode).is_some() => {
                    fill.then_some(RirMaterializerAction::Copy)
                }
                (
                    air::ExternRep::Inline,
                    Some(mode @ anvyx_runtime::ExternMaterialization::Materialize),
                    Some(native),
                ) => {
                    let binding = native
                        .validated_materializer(mode)
                        .ok_or(RustRecipeGap::UnsupportedType(ty))?;
                    fill.then(|| RirMaterializerAction::ProviderMaterialize {
                        binding: binding.clone(),
                    })
                }
                (air::ExternRep::Shared, None, Some(native)) if native.materializer.is_none() => {
                    fill.then_some(RirMaterializerAction::IdentityShare)
                }
                _ => return Err(RustRecipeGap::UnsupportedType(ty)),
            }
        } else if matches!(
            plan.program.type_arena.data(ty),
            TypeData::Void | TypeData::Any | TypeData::Slice(_)
        ) {
            return Err(RustRecipeGap::UnsupportedType(ty));
        } else if matches!(plan.program.type_arena.data(ty), TypeData::Function(_)) {
            match self.callables.get(&ty) {
                Some(
                    action @ (RirMaterializerAction::Copy | RirMaterializerAction::CallableShare),
                ) => fill.then(|| action.clone()),
                Some(_) => unreachable!("callable materializer action"),
                None => return Err(RustRecipeGap::FunctionCapabilityPending(ty)),
            }
        } else if let TypeData::Dyn(surface) = plan.program.type_arena.data(ty) {
            let carrier = carriers
                .get(surface)
                .copied()
                .ok_or(RustRecipeGap::UnsupportedType(ty))?;
            let mut actions = vec![];
            for (index, witness) in plan.program.contract_witnesses.iter().enumerate() {
                if witness.key.surface != *surface {
                    continue;
                }
                let witness = ContractWitnessId::from_index(index);
                let payload = self.visit(
                    plan,
                    plan.program.contract_witnesses[witness.index()]
                        .key
                        .concrete_ty,
                    child_recipe_position(position, LambdaStorageFamily::DynamicPayload),
                    carriers,
                    variants,
                    fill,
                )?;
                if fill {
                    actions.push((
                        variants
                            .get(&witness)
                            .copied()
                            .ok_or(RustRecipeGap::UnsupportedType(ty))?,
                        payload,
                    ));
                }
            }
            actions.sort_by_key(|(variant, _)| variant.index());
            fill.then_some(RirMaterializerAction::DynamicMaterialize {
                carrier,
                variants: actions.into_iter().map(|(_, payload)| payload).collect(),
            })
        } else if copyable {
            fill.then_some(RirMaterializerAction::Copy)
        } else {
            match plan.program.type_arena.data(ty) {
                TypeData::Int
                | TypeData::Flag(_)
                | TypeData::Float
                | TypeData::Bool
                | TypeData::Char => unreachable!("immediate values are reusable-copyable"),
                TypeData::String | TypeData::List(_) | TypeData::Map { .. } => {
                    fill.then_some(RirMaterializerAction::ManagedShare)
                }
                TypeData::DataRef(_) => fill.then_some(RirMaterializerAction::IdentityShare),
                TypeData::Optional(inner) => {
                    let payload = self.visit(
                        plan,
                        *inner,
                        child_recipe_position(position, LambdaStorageFamily::OptionalPayload),
                        carriers,
                        variants,
                        fill,
                    )?;
                    fill.then_some(RirMaterializerAction::Optional { payload })
                }
                TypeData::Array { elem, .. } => {
                    let elem = self.visit(
                        plan,
                        *elem,
                        child_recipe_position(position, LambdaStorageFamily::FixedArrayElement),
                        carriers,
                        variants,
                        fill,
                    )?;
                    fill.then_some(RirMaterializerAction::Array { elem })
                }
                TypeData::Tuple(fields) => {
                    let fields = self.field_actions(
                        plan,
                        fields.iter().copied(),
                        position,
                        LambdaStorageFamily::TupleField,
                        carriers,
                        variants,
                        fill,
                    )?;
                    fill.then_some(RirMaterializerAction::Tuple { fields })
                }
                TypeData::Aggregate(aggregate) => {
                    let decl = plan.program.aggregate(*aggregate);
                    let family = match decl.kind {
                        AggregateKind::Struct => LambdaStorageFamily::StructField,
                        AggregateKind::DataRef => LambdaStorageFamily::DataRefProjection,
                    };
                    let fields = self.field_actions(
                        plan,
                        decl.fields.iter().map(|field| field.ty),
                        position,
                        family,
                        carriers,
                        variants,
                        fill,
                    )?;
                    fill.then_some(RirMaterializerAction::Struct { fields })
                }
                TypeData::Enum(enm) => {
                    let actions = plan
                        .program
                        .enum_decl(*enm)
                        .variants
                        .iter()
                        .map(|variant| {
                            self.field_actions(
                                plan,
                                RustRepresentationPlan::variant_field_tys(variant),
                                position,
                                LambdaStorageFamily::EnumPayload,
                                carriers,
                                variants,
                                fill,
                            )
                        })
                        .collect::<Result<Vec<_>, _>>()?;
                    fill.then_some(RirMaterializerAction::Enum { variants: actions })
                }
                TypeData::Dyn(_) => unreachable!("dynamic materializers handled above"),
                TypeData::Void
                | TypeData::Any
                | TypeData::Slice(_)
                | TypeData::Extern(_)
                | TypeData::Function(_) => return Err(RustRecipeGap::UnsupportedType(ty)),
            }
        };
        if fill {
            self.nodes[id.index()] = RustMaterializerNode::Ready {
                action: action.expect("filled action"),
                copy,
                support,
            };
        }
        Ok(id)
    }

    fn support_evidence(
        &mut self,
        plan: RustRepresentationPlan<'_>,
        ty: TypeId,
        position: RustRecipePosition,
        carriers: &HashMap<ContractSurfaceId, RirDynCarrierId>,
        variants: &HashMap<ContractWitnessId, RirDynVariantId>,
        fill: bool,
    ) -> Result<Option<RirSupportEvidence>, RustRecipeGap> {
        let evidence = match plan.program.type_arena.data(ty) {
            TypeData::List(elem) => {
                let elem = self.visit(
                    plan,
                    *elem,
                    child_recipe_position(position, LambdaStorageFamily::ListElement),
                    carriers,
                    variants,
                    fill,
                )?;
                Some(RirSupportEvidence::List { elem })
            }
            TypeData::Map { key, value, .. } => {
                let key_materializer = self.visit(
                    plan,
                    *key,
                    child_recipe_position(position, LambdaStorageFamily::MapKey),
                    carriers,
                    variants,
                    fill,
                )?;
                let key_contract = self.visit(
                    plan,
                    *key,
                    RustRecipePosition::MapKey,
                    carriers,
                    variants,
                    fill,
                )?;
                let value = self.visit(
                    plan,
                    *value,
                    child_recipe_position(position, LambdaStorageFamily::MapValue),
                    carriers,
                    variants,
                    fill,
                )?;
                Some(RirSupportEvidence::Map {
                    key: key_materializer,
                    key_contract,
                    value,
                })
            }
            TypeData::Extern(id) if plan.program.extern_type(*id).rep == air::ExternRep::Inline => {
                let decl = plan.program.extern_type(*id);
                if decl.variants.is_empty() {
                    Some(RirSupportEvidence::ProviderStruct {
                        fields: self.field_actions(
                            plan,
                            decl.fields
                                .iter()
                                .filter(|field| !field.computed)
                                .map(|field| field.ty),
                            position,
                            LambdaStorageFamily::StructField,
                            carriers,
                            variants,
                            fill,
                        )?,
                    })
                } else {
                    Some(RirSupportEvidence::ProviderEnum {
                        variants: decl
                            .variants
                            .iter()
                            .map(|variant| {
                                self.field_actions(
                                    plan,
                                    RustRepresentationPlan::variant_field_tys(variant),
                                    position,
                                    LambdaStorageFamily::EnumPayload,
                                    carriers,
                                    variants,
                                    fill,
                                )
                            })
                            .collect::<Result<_, _>>()?,
                    })
                }
            }
            _ => None,
        };
        Ok(fill.then_some(evidence).flatten())
    }

    fn node_copyable(&self, id: RirMaterializerId) -> bool {
        match &self.nodes[id.index()] {
            RustMaterializerNode::Ready { copy, .. } => copy.is_some(),
            RustMaterializerNode::Filling | RustMaterializerNode::Reserved => false,
        }
    }

    fn copy_evidence(
        &mut self,
        plan: RustRepresentationPlan<'_>,
        ty: TypeId,
        position: RustRecipePosition,
        carriers: &HashMap<ContractSurfaceId, RirDynCarrierId>,
        variants: &HashMap<ContractWitnessId, RirDynVariantId>,
        fill: bool,
    ) -> Result<(bool, Option<RirCopyEvidence>), RustRecipeGap> {
        let structural = plan.classes.get(ty).is_some_and(|class| {
            matches!(
                class,
                TypePassClass::Immediate
                    | TypePassClass::SmallCopyInline(_)
                    | TypePassClass::LargeInline(_)
            )
        });
        let evidence = match plan.program.type_arena.data(ty) {
            TypeData::Int
            | TypeData::Flag(_)
            | TypeData::Float
            | TypeData::Bool
            | TypeData::Char => Some(RirCopyEvidence::Leaf),
            TypeData::Extern(id)
                if matches!(
                    plan.program.extern_type(*id).materialization,
                    Some(anvyx_runtime::ExternMaterialization::Copy)
                ) =>
            {
                Some(RirCopyEvidence::Leaf)
            }
            TypeData::Function(_)
                if matches!(self.callables.get(&ty), Some(RirMaterializerAction::Copy)) =>
            {
                Some(RirCopyEvidence::Leaf)
            }
            TypeData::Optional(inner) if structural => {
                let payload = self.visit(
                    plan,
                    *inner,
                    child_recipe_position(position, LambdaStorageFamily::OptionalPayload),
                    carriers,
                    variants,
                    fill,
                )?;
                self.node_copyable(payload)
                    .then_some(RirCopyEvidence::Optional { payload })
            }
            TypeData::Array { elem, .. } if structural => {
                let elem = self.visit(
                    plan,
                    *elem,
                    child_recipe_position(position, LambdaStorageFamily::FixedArrayElement),
                    carriers,
                    variants,
                    fill,
                )?;
                self.node_copyable(elem)
                    .then_some(RirCopyEvidence::Array { elem })
            }
            TypeData::Tuple(fields) if structural => {
                let fields = self.field_actions(
                    plan,
                    fields.iter().copied(),
                    position,
                    LambdaStorageFamily::TupleField,
                    carriers,
                    variants,
                    fill,
                )?;
                fields
                    .iter()
                    .all(|id| self.node_copyable(*id))
                    .then_some(RirCopyEvidence::Tuple { fields })
            }
            TypeData::Aggregate(aggregate) if structural => {
                let decl = plan.program.aggregate(*aggregate);
                match (decl.kind, decl.cycle_capable) {
                    (AggregateKind::DataRef, _) | (_, true) => None,
                    (AggregateKind::Struct, false) => {
                        let family = LambdaStorageFamily::StructField;
                        let fields = self.field_actions(
                            plan,
                            decl.fields.iter().map(|field| field.ty),
                            position,
                            family,
                            carriers,
                            variants,
                            fill,
                        )?;
                        fields
                            .iter()
                            .all(|id| self.node_copyable(*id))
                            .then_some(RirCopyEvidence::Struct { family, fields })
                    }
                }
            }
            TypeData::Enum(enm) if structural => {
                let entries = plan
                    .program
                    .enum_decl(*enm)
                    .variants
                    .iter()
                    .map(|variant| {
                        self.field_actions(
                            plan,
                            RustRepresentationPlan::variant_field_tys(variant),
                            position,
                            LambdaStorageFamily::EnumPayload,
                            carriers,
                            variants,
                            fill,
                        )
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                entries
                    .iter()
                    .flatten()
                    .all(|id| self.node_copyable(*id))
                    .then_some(RirCopyEvidence::Enum { variants: entries })
            }
            TypeData::Void
            | TypeData::Any
            | TypeData::String
            | TypeData::Function(_)
            | TypeData::Slice(_)
            | TypeData::List(_)
            | TypeData::Map { .. }
            | TypeData::DataRef(_)
            | TypeData::Dyn(_)
            | TypeData::Extern(_)
            | TypeData::Optional(_)
            | TypeData::Array { .. }
            | TypeData::Tuple(_)
            | TypeData::Aggregate(_)
            | TypeData::Enum(_) => None,
        };
        Ok((evidence.is_some(), fill.then_some(evidence).flatten()))
    }

    fn field_actions(
        &mut self,
        plan: RustRepresentationPlan<'_>,
        fields: impl IntoIterator<Item = TypeId>,
        position: RustRecipePosition,
        family: LambdaStorageFamily,
        carriers: &HashMap<ContractSurfaceId, RirDynCarrierId>,
        variants: &HashMap<ContractWitnessId, RirDynVariantId>,
        fill: bool,
    ) -> Result<Vec<RirMaterializerId>, RustRecipeGap> {
        let position = child_recipe_position(position, family);
        fields
            .into_iter()
            .map(|field| self.visit(plan, field, position, carriers, variants, fill))
            .collect()
    }
}

pub(super) fn child_recipe_position(
    parent: RustRecipePosition,
    child: LambdaStorageFamily,
) -> RustRecipePosition {
    match parent {
        RustRecipePosition::Value => RustRecipePosition::StoredPayload(child),
        RustRecipePosition::StoredPayload(outer) => {
            RustRecipePosition::StoredPayload(nested_storage_family(outer, child))
        }
        RustRecipePosition::MapKey => RustRecipePosition::MapKey,
        RustRecipePosition::Global => RustRecipePosition::Global,
    }
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
    pub storage: RustPayloadStorage,
    pub recursive: bool,
    pub inline_payload_layout: Result<RustApproxLayout, RustLayoutGap>,
    pub box_reason: Option<RustDynamicBoxReason>,
    pub lifecycle: Result<RustLifecyclePlan, RustLifecycleGap>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RustDynamicBoxReason {
    Function,
    FunctionField,
    Recursive,
    Threshold,
    WeakeningClass(ContractSurfaceId),
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

pub(crate) mod layout {
    use super::RustApproxLayout;

    pub(crate) fn valid(layout: RustApproxLayout) -> bool {
        layout.align != 0 && layout.align.is_power_of_two()
    }

    pub(crate) fn repeat(layout: RustApproxLayout, count: u64) -> Option<RustApproxLayout> {
        if !valid(layout) {
            return None;
        }
        Some(RustApproxLayout {
            size: layout.size.checked_mul(count)?,
            align: layout.align,
        })
    }

    pub(crate) fn fields(
        fields: impl IntoIterator<Item = RustApproxLayout>,
    ) -> Option<RustApproxLayout> {
        let mut layout = RustApproxLayout { size: 0, align: 1 };
        for field in fields {
            if !valid(field) {
                return None;
            }
            layout.size = align_up(layout.size, field.align)?.checked_add(field.size)?;
            layout.align = layout.align.max(field.align);
        }
        Some(RustApproxLayout {
            size: align_up(layout.size, layout.align)?,
            align: layout.align,
        })
    }

    pub(crate) fn enum_layout(
        discriminant: RustApproxLayout,
        payload: RustApproxLayout,
    ) -> Option<RustApproxLayout> {
        if !valid(discriminant) || !valid(payload) {
            return None;
        }
        let align = discriminant.align.max(payload.align);
        let payload_offset = align_up(discriminant.size, payload.align)?;
        let size = payload_offset.checked_add(payload.size)?;
        Some(RustApproxLayout {
            size: align_up(size, align)?,
            align,
        })
    }

    fn align_up(value: u64, align: u64) -> Option<u64> {
        if !align.is_power_of_two() {
            return None;
        }
        let mask = align - 1;
        value.checked_add(mask).map(|value| value & !mask)
    }
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum LambdaStorageFamily {
    StructField,
    TupleField,
    EnumPayload,
    DynamicPayload,
    OptionalPayload,
    FixedArrayElement,
    ListElement,
    MapKey,
    MapValue,
    DataRefProjection,
    GlobalRoot,
    UnknownOrigin,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LambdaStorageGap {
    Lifetime,
}

impl LambdaStorageFamily {
    pub(super) fn allows_dynamic_owned(self) -> bool {
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

    pub(super) fn allows_function_payload(self) -> bool {
        matches!(
            self,
            Self::StructField
                | Self::TupleField
                | Self::EnumPayload
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
            | LambdaStorageFamily::DataRefProjection
            | LambdaStorageFamily::MapKey
            | LambdaStorageFamily::MapValue,
            _,
        ) => outer,
        _ => nested,
    }
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RustCarrierDiscriminant {
    U32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum RustPrimitiveLayout {
    Integer,
    Bool,
    Char,
    Unit,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct RustTargetProfile {
    pub pointer_size: u8,
    pub pointer_align: u8,
    pub carrier_discriminant: RustCarrierDiscriminant,
}

impl RustTargetProfile {
    pub(crate) const AOT_64: Self = Self {
        pointer_size: 8,
        pointer_align: 8,
        carrier_discriminant: RustCarrierDiscriminant::U32,
    };

    pub(crate) fn pointer_layout(self) -> RustApproxLayout {
        RustApproxLayout {
            size: u64::from(self.pointer_size),
            align: u64::from(self.pointer_align),
        }
    }

    pub(crate) fn inline_payload_limit(self) -> u64 {
        u64::from(self.pointer_size) * 3
    }

    pub(crate) fn pointer_align(self) -> u64 {
        u64::from(self.pointer_align)
    }

    pub(crate) fn carrier_discriminant_layout(self) -> RustApproxLayout {
        match self.carrier_discriminant {
            RustCarrierDiscriminant::U32 => RustApproxLayout { size: 4, align: 4 },
        }
    }

    pub(crate) fn primitive_layout(self, primitive: RustPrimitiveLayout) -> RustApproxLayout {
        match primitive {
            RustPrimitiveLayout::Integer => self.pointer_layout(),
            RustPrimitiveLayout::Bool => RustApproxLayout { size: 1, align: 1 },
            RustPrimitiveLayout::Char => RustApproxLayout { size: 4, align: 4 },
            RustPrimitiveLayout::Unit => RustApproxLayout { size: 0, align: 1 },
        }
    }
}

pub(crate) fn target_profile() -> RustTargetProfile {
    RustTargetProfile::AOT_64
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
            target: target_profile(),
        }
    }

    pub fn dynamic_layout_plan(self) -> Result<RustDynamicLayoutPlan, RustLayoutGap> {
        self.verify_weakening_witnesses()?;
        let graph = self.layout_graph();
        let components = strongly_connected_components(&graph);
        let weakening_classes = self.weakening_classes();
        let mut direct_reasons = BTreeMap::new();
        let mut recursive_variants = BTreeSet::new();

        for witness in &self.program.contract_witnesses {
            let carrier = RustLayoutNode::Carrier(witness.key.surface);
            let payload = RustLayoutNode::Type(witness.key.concrete_ty);
            let mut contained_carriers = BTreeSet::new();
            self.collect_type_carriers(
                witness.key.concrete_ty,
                &mut BTreeSet::new(),
                &mut contained_carriers,
            );
            let recursive = contained_carriers.contains(&witness.key.surface)
                || (components.get(&carrier) == components.get(&payload)
                    && component_is_cyclic(&graph, carrier, &components));
            let key = (witness.key.surface, witness.key.concrete_ty);
            if recursive {
                recursive_variants.insert(key);
            }
            let reason = if matches!(
                self.program.type_arena.data(witness.key.concrete_ty),
                TypeData::Function(_)
            ) {
                Some(RustDynamicBoxReason::Function)
            } else if recursive {
                Some(RustDynamicBoxReason::Recursive)
            } else {
                None
            };
            direct_reasons.insert(key, reason);
        }
        let mut storage = direct_reasons
            .iter()
            .map(|(&key, reason)| {
                (
                    key,
                    if reason.is_some() {
                        RustPayloadStorage::Boxed
                    } else {
                        RustPayloadStorage::Inline
                    },
                )
            })
            .collect::<BTreeMap<_, _>>();
        let mut inline_layouts = self.compute_layouts(&storage);
        loop {
            let mut changed = false;
            for witness in &self.program.contract_witnesses {
                let key = (witness.key.surface, witness.key.concrete_ty);
                if direct_reasons[&key].is_none()
                    && self.contains_function_payload(witness.key.concrete_ty)
                    && matches!(
                        inline_layouts.payloads[&key],
                        Err(RustLayoutGap::FunctionLayoutUnknown(_))
                    )
                {
                    direct_reasons.insert(key, Some(RustDynamicBoxReason::FunctionField));
                    storage.insert(key, RustPayloadStorage::Boxed);
                    changed = true;
                }
            }
            if !changed {
                break;
            }
            inline_layouts = self.compute_layouts(&storage);
        }
        for witness in &self.program.contract_witnesses {
            let key = (witness.key.surface, witness.key.concrete_ty);
            if direct_reasons[&key].is_none()
                && inline_layouts.payloads.get(&key).is_some_and(|layout| {
                    layout.as_ref().is_ok_and(|layout| {
                        layout.size > self.inline_payload_limit()
                            || layout.align > self.target.pointer_align()
                    })
                })
            {
                direct_reasons.insert(key, Some(RustDynamicBoxReason::Threshold));
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
                variant.inline_payload_layout = match direct_reasons[&key] {
                    Some(RustDynamicBoxReason::Function | RustDynamicBoxReason::FunctionField) => {
                        Err(RustLayoutGap::FunctionLayoutUnknown(variant.concrete_ty))
                    }
                    Some(RustDynamicBoxReason::Recursive) => {
                        Err(RustLayoutGap::RecursiveInline(variant.concrete_ty))
                    }
                    _ => inline_layouts.payloads[&key],
                };
                variant.box_reason = direct_reasons[&key].or_else(|| {
                    (storage[&key] == RustPayloadStorage::Boxed).then_some(
                        RustDynamicBoxReason::WeakeningClass(weakening_classes[&carrier.surface]),
                    )
                });
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
                storage: RustPayloadStorage::Inline,
                recursive: false,
                inline_payload_layout: Err(RustLayoutGap::UnsupportedType(witness.key.concrete_ty)),
                box_reason: None,
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
            | TypeData::Flag(_)
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
                | TypeData::Flag(_)
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
        self.target.pointer_layout()
    }

    fn inline_payload_limit(self) -> u64 {
        self.target.inline_payload_limit()
    }

    fn discriminant_layout(self) -> RustApproxLayout {
        self.target.carrier_discriminant_layout()
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
            | TypeData::Flag(_)
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

    fn variant_field_tys(variant: &air::VariantDecl) -> Box<dyn Iterator<Item = TypeId> + '_> {
        match &variant.shape {
            VariantShape::Unit => Box::new(std::iter::empty()),
            VariantShape::Tuple(fields) => Box::new(fields.iter().copied()),
            VariantShape::Struct(fields) => Box::new(fields.iter().map(|field| field.ty)),
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
            air::ConstValue::Flag { flag, bits } => format!("flag:{}:{bits}", flag.index()),
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
            TypeData::Flag(id) => {
                let decl = self.plan.program.flag_decl(*id);
                format!(
                    "flag:{}",
                    self.named(decl.module, decl.name.as_str(), &[], &[])
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
            let mut payload = target_profile().primitive_layout(RustPrimitiveLayout::Unit);
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
            TypeData::Int | TypeData::Flag(_) | TypeData::Float => {
                Ok(target_profile().primitive_layout(RustPrimitiveLayout::Integer))
            }
            TypeData::Bool => Ok(target_profile().primitive_layout(RustPrimitiveLayout::Bool)),
            TypeData::Char => Ok(target_profile().primitive_layout(RustPrimitiveLayout::Char)),
            TypeData::Void => Ok(target_profile().primitive_layout(RustPrimitiveLayout::Unit)),
            TypeData::String => repeat_layout(self.plan.pointer_layout(), 3),
            TypeData::List(_) | TypeData::Map { .. } | TypeData::DataRef(_) => {
                Ok(self.plan.pointer_layout())
            }
            TypeData::Slice(_) => repeat_layout(self.plan.pointer_layout(), 2),
            TypeData::Function(_) => Err(RustLayoutGap::FunctionLayoutUnknown(ty)),
            TypeData::Optional(inner) => {
                let payload = self.type_layout(*inner)?;
                enum_layout(
                    target_profile().primitive_layout(RustPrimitiveLayout::Bool),
                    payload,
                )
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
                let mut payload = target_profile().primitive_layout(RustPrimitiveLayout::Unit);
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
        let mut layouts = vec![];
        for field in fields {
            layouts.push(self.type_layout(field)?);
        }
        layout::fields(layouts).ok_or(RustLayoutGap::ArithmeticOverflow)
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
    layout::repeat(layout, count).ok_or(RustLayoutGap::ArithmeticOverflow)
}

fn enum_layout(
    discriminant: RustApproxLayout,
    payload: RustApproxLayout,
) -> Result<RustApproxLayout, RustLayoutGap> {
    layout::enum_layout(discriminant, payload).ok_or(RustLayoutGap::ArithmeticOverflow)
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

    pub fn supports_param(self, ty: RirTypeId, mode: RirPassMode) -> bool {
        self.ty_opt(ty)
            .is_some_and(|ty_data| self.supports_type_pass_mode(ty_data, mode))
    }

    pub fn capture_shareable_with(
        self,
        ty: RirTypeId,
        copyable: impl Fn(RirTypeId) -> bool + Copy,
    ) -> bool {
        copyable(ty)
            || match self.ty(ty) {
                RirType::String | RirType::DataRef(_) | RirType::List(_) | RirType::Map { .. } => {
                    true
                }
                RirType::Option(inner) | RirType::Array { elem: inner, .. } => {
                    self.embedded_capture_shareable_with(inner, copyable)
                }
                RirType::Lambda(sig) => self.lambda_sig_cloneable_with(sig, copyable),
                RirType::Struct(id) => self.program.structs[id.index()]
                    .fields
                    .iter()
                    .all(|field| self.embedded_capture_shareable_with(field.ty, copyable)),
                RirType::Tuple(id) => self.program.tuples[id.index()]
                    .fields
                    .iter()
                    .all(|field| self.embedded_capture_shareable_with(field.ty, copyable)),
                RirType::Enum(id) if self.program.dyn_carrier_for_enum(id).is_some() => true,
                RirType::Enum(id) => {
                    self.program.enums[id.index()]
                        .variants
                        .iter()
                        .all(|variant| {
                            variant.fields.iter().all(|field| {
                                self.embedded_capture_shareable_with(field.ty, copyable)
                            })
                        })
                }
                RirType::Slice(_)
                | RirType::Float
                | RirType::Bool
                | RirType::Char
                | RirType::Int
                | RirType::Flag(_)
                | RirType::Void => false,
            }
    }

    fn embedded_capture_shareable_with(
        self,
        ty: RirTypeId,
        copyable: impl Fn(RirTypeId) -> bool + Copy,
    ) -> bool {
        !matches!(self.ty(ty), RirType::Slice(_)) && self.capture_shareable_with(ty, copyable)
    }

    pub fn value_from_ref_supported(self, ty: RirTypeId) -> bool {
        self.program
            .value_materializers
            .get(ty.index())
            .is_some_and(Option::is_some)
    }

    fn declaration_ty(self, expected: RirType) -> RirTypeId {
        self.program
            .types
            .iter()
            .position(|ty| *ty == expected)
            .map(RirTypeId::from_index)
            .expect("verified declaration type")
    }

    fn has_materializer(self, ty: RirTypeId, position: RustRecipePosition) -> bool {
        self.program
            .materializers
            .iter()
            .any(|entry| entry.ty == ty && entry.position == position)
    }

    pub fn record_derives(self, ty: RirTypeId) -> Vec<&'static str> {
        let mut derives = if self.copyable(ty) {
            vec!["Clone", "Copy"]
        } else {
            vec![]
        };
        if self.has_materializer(ty, RustRecipePosition::MapKey) {
            derives.extend(["PartialEq", "Eq", "Hash"]);
        }
        derives
    }

    pub fn flag_derives() -> Vec<&'static str> {
        vec!["Clone", "Copy", "PartialEq", "Eq", "Hash"]
    }

    pub fn enum_derives(self, enm: &RirEnum) -> Vec<&'static str> {
        if self.program.dyn_carrier_for_enum(enm.id).is_some() {
            return vec![];
        }
        let ty = self.declaration_ty(RirType::Enum(enm.id));
        let mut derives = if self.copyable(ty) {
            vec!["Clone", "Copy"]
        } else {
            vec![]
        };
        let key_supported = self.has_materializer(ty, RustRecipePosition::MapKey);
        if enm.is_unit_only() || key_supported {
            derives.extend(["PartialEq", "Eq"]);
        }
        if key_supported {
            derives.push("Hash");
        }
        derives
    }

    pub fn struct_ty(self, id: RirStructId) -> RirTypeId {
        self.declaration_ty(RirType::Struct(id))
    }

    pub fn tuple_ty(self, id: RirTupleId) -> RirTypeId {
        self.declaration_ty(RirType::Tuple(id))
    }

    pub fn borrow_view(self, ty: RirTypeId) -> RustBorrowView {
        match self.ty(ty) {
            RirType::String => RustBorrowView::Str,
            RirType::Void => RustBorrowView::TargetGap,
            _ => RustBorrowView::Ref,
        }
    }

    pub fn param_ty(self, ty: RirTypeId, mode: RirPassMode) -> String {
        self.param_ty_with_lifetime(ty, mode, None)
    }

    pub fn callable_param_ty(
        self,
        ty: RirTypeId,
        mode: RirPassMode,
        escape: RirParamEscape,
    ) -> String {
        if mode == RirPassMode::Value && escape == RirParamEscape::Escaping {
            self.escaping_value_ty(ty)
        } else {
            self.param_ty(ty, mode)
        }
    }

    pub fn capture_field_ty(self, ty: RirTypeId, mode: RirPassMode) -> String {
        self.param_ty_with_lifetime(ty, mode, Some("'env"))
    }

    fn param_ty_with_lifetime(
        self,
        ty: RirTypeId,
        mode: RirPassMode,
        lifetime: Option<&str>,
    ) -> String {
        let reference_lifetime = lifetime.map_or(String::new(), |lifetime| format!("{lifetime} "));
        match mode {
            RirPassMode::Value => self.rust_ty(ty),
            RirPassMode::SharedBorrow => match self.borrow_view(ty) {
                RustBorrowView::Str => format!("&{reference_lifetime}str"),
                _ => format!(
                    "&{reference_lifetime}{}",
                    self.rust_ty_with_env_lifetime(ty, lifetime)
                ),
            },
            RirPassMode::MutBorrow => format!(
                "&{reference_lifetime}mut {}",
                self.rust_ty_with_env_lifetime(ty, lifetime)
            ),
            RirPassMode::MutPlace => {
                let payload = self.rust_ty(ty);
                format!("{}<'_, 'cx, {payload}>", target::mut_place_ty())
            }
            RirPassMode::DynBorrow => {
                let Some(RirType::Enum(id)) = self.program.types.get(ty.index()) else {
                    unreachable!("verified dynamic borrow carrier type")
                };
                let carrier = self
                    .program
                    .dyn_carrier_for_enum(*id)
                    .expect("verified dynamic borrow carrier");
                format!("{}<'_, 'cx>", self.program.dyn_borrow_symbol(carrier.id))
            }
            RirPassMode::ScopedLambda => self.scoped_lambda_ty(ty),
            RirPassMode::EscapingLambda => self.escaping_lambda_ty(ty),
            RirPassMode::AnvCallback => self.anv_callback_ty(ty),
            RirPassMode::StackCell => {
                let payload = self.rust_ty(ty);
                format!(
                    "&{reference_lifetime}{}",
                    target::stack_lambda_cell_ty(&payload)
                )
            }
            RirPassMode::HeapCell => {
                let payload = self.rust_ty(ty);
                target::handle_ty(&target::lambda_cell_ty(&payload))
            }
            RirPassMode::ScopedPlaceCell => {
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
                .any(|capture| capture.mode == RirPassMode::MutBorrow)
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
            Self::lambda_sig_symbol(id),
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
            Self::lambda_sig_symbol(id),
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

    pub fn lambda_sig_symbol(id: RirLambdaSigId) -> String {
        format!("LambdaSig{}", id.index())
    }

    fn rust_ty_with_env_lifetime(self, ty: RirTypeId, lifetime: Option<&str>) -> String {
        match (self.ty(ty), lifetime) {
            (RirType::Lambda(sig), Some(lifetime)) => {
                format!(
                    "{}{}",
                    Self::lambda_sig_symbol(sig),
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
                .any(|capture| capture.mode == RirPassMode::MutBorrow);
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
        match capture.mode {
            RirPassMode::Value if matches!(lambda.storage, RirLambdaStorage::HeapEnv { .. }) => {
                LambdaCaptureLayoutEdge::HeapEnvField
            }
            RirPassMode::Value => LambdaCaptureLayoutEdge::InlineValue,
            RirPassMode::SharedBorrow => LambdaCaptureLayoutEdge::SharedBorrow,
            RirPassMode::MutBorrow => LambdaCaptureLayoutEdge::MutBorrow,
            RirPassMode::StackCell => LambdaCaptureLayoutEdge::StackCell,
            RirPassMode::HeapCell => LambdaCaptureLayoutEdge::HeapCell,
            RirPassMode::ScopedPlaceCell => LambdaCaptureLayoutEdge::ScopedPlaceCell,
            RirPassMode::MutPlace
            | RirPassMode::DynBorrow
            | RirPassMode::ScopedLambda
            | RirPassMode::EscapingLambda
            | RirPassMode::AnvCallback => LambdaCaptureLayoutEdge::Unsupported,
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
        self.lambda_sig_copyable_with(id, |ty| self.copyable(ty))
    }

    pub fn lambda_sig_copyable_with(
        self,
        id: RirLambdaSigId,
        copyable: impl Fn(RirTypeId) -> bool,
    ) -> bool {
        !self.lambda_sig_has_heap_env(id)
            && self.program.lambdas_for_sig(id).all(|lambda| {
                lambda.captures.iter().all(|capture| match capture.mode {
                    RirPassMode::Value => copyable(capture.ty),
                    RirPassMode::SharedBorrow
                    | RirPassMode::StackCell
                    | RirPassMode::ScopedPlaceCell => true,
                    RirPassMode::MutBorrow
                    | RirPassMode::MutPlace
                    | RirPassMode::DynBorrow
                    | RirPassMode::ScopedLambda
                    | RirPassMode::EscapingLambda
                    | RirPassMode::AnvCallback
                    | RirPassMode::HeapCell => false,
                })
            })
    }

    pub fn lambda_sig_cloneable(self, id: RirLambdaSigId) -> bool {
        self.lambda_sig_cloneable_with(id, |ty| self.copyable(ty))
    }

    pub fn lambda_sig_derives(self, id: RirLambdaSigId) -> &'static [&'static str] {
        let ty = self.declaration_ty(RirType::Lambda(id));
        let materializer = self.program.value_materializers[ty.index()]
            .and_then(|id| self.program.materializers.get(id.index()))
            .expect("verified lambda materializer");
        if materializer.is_copy() {
            &["Clone", "Copy"]
        } else if matches!(materializer.action, RirMaterializerAction::CallableShare) {
            &["Clone"]
        } else {
            &[]
        }
    }

    pub fn lambda_sig_cloneable_with(
        self,
        id: RirLambdaSigId,
        copyable: impl Fn(RirTypeId) -> bool,
    ) -> bool {
        self.lambda_sig_has_heap_env(id)
            || self.program.lambdas_for_sig(id).all(|lambda| {
                lambda.captures.iter().all(|capture| match capture.mode {
                    RirPassMode::Value => copyable(capture.ty),
                    RirPassMode::SharedBorrow
                    | RirPassMode::StackCell
                    | RirPassMode::HeapCell
                    | RirPassMode::ScopedPlaceCell => true,
                    RirPassMode::MutBorrow
                    | RirPassMode::MutPlace
                    | RirPassMode::DynBorrow
                    | RirPassMode::ScopedLambda
                    | RirPassMode::EscapingLambda
                    | RirPassMode::AnvCallback => false,
                })
            })
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
            RirType::Flag(id) => Self::named_ty(
                self.program.flags[id.index()].symbol.as_str(),
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
        format!("{}{generics}", Self::lambda_sig_symbol(id))
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
                    .any(|capture| capture.mode == RirPassMode::HeapCell)
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
            .filter_map(|(index, capture)| (capture.mode == RirPassMode::HeapCell).then_some(index))
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
            | RirType::Flag(_)
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
            | RirType::Flag(_)
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
        matches!(self.ty(ty), RirType::Void)
            || self
                .program
                .value_materializers
                .get(ty.index())
                .copied()
                .flatten()
                .and_then(|id| self.program.materializers.get(id.index()))
                .is_some_and(RirMaterializer::is_copy)
    }

    fn supports_type_pass_mode(self, ty: RirType, mode: RirPassMode) -> bool {
        match mode {
            RirPassMode::Value => match ty {
                RirType::Option(inner) => self.supports_param(inner, mode),
                RirType::Int
                | RirType::Flag(_)
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
                    .all(|field| self.supports_param(field.ty, mode)),
            },
            RirPassMode::SharedBorrow => match ty {
                RirType::Option(inner) => self.supports_param(inner, mode),
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
                RirType::Int
                | RirType::Flag(_)
                | RirType::Float
                | RirType::Bool
                | RirType::Char
                | RirType::Void => false,
            },
            RirPassMode::MutBorrow => match ty {
                RirType::Option(inner) => self.supports_param(inner, mode),
                RirType::Int
                | RirType::Flag(_)
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
            RirPassMode::ScopedLambda | RirPassMode::EscapingLambda | RirPassMode::AnvCallback => {
                matches!(ty, RirType::Lambda(_))
            }
            RirPassMode::DynBorrow => match ty {
                RirType::Enum(id) => self.program.dyn_carrier_for_enum(id).is_some(),
                _ => false,
            },
            RirPassMode::MutPlace
            | RirPassMode::StackCell
            | RirPassMode::HeapCell
            | RirPassMode::ScopedPlaceCell => !matches!(ty, RirType::Void),
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
    pub fn build(program: &RirProgram, retained_callback_sigs: &[RirLambdaSigId]) -> Self {
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
            match &storage.kind {
                RirCollectionStorageKind::List { elem_ty, .. }
                    if policy.list_storage_tracked(*elem_ty) =>
                {
                    plan.mark_type(program, *elem_ty);
                }
                RirCollectionStorageKind::Map {
                    key_ty, value_ty, ..
                } if policy.map_storage_tracked(*key_ty, *value_ty) => {
                    plan.mark_type(program, *key_ty);
                    plan.mark_type(program, *value_ty);
                }
                _ => {}
            }
        }
        for global in &program.globals {
            if policy.type_owns_heap_edges(global.ty) {
                plan.mark_type(program, global.ty);
            }
        }
        for &sig in retained_callback_sigs {
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
                if strukt.native.is_some() {
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
            | RirType::Flag(_)
            | RirType::Float
            | RirType::Bool
            | RirType::String
            | RirType::Char
            | RirType::Void
            | RirType::DataRef(_) => {}
        }
    }
}
