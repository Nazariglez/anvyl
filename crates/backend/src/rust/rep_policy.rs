use std::collections::BTreeSet;

use anvyx_frontend::air::{
    self, ParamMode, Program as AirProgram, TypeData, TypeId, TypePassClass, TypePassClasses,
    VariantShape,
};

use super::{
    rir::{
        RirCellDecl, RirCellStorage, RirCollectionStorageKind, RirDataRef, RirEnum, RirEnumId,
        RirField, RirLambdaEnvField, RirLambdaEnvFieldKind, RirLambdaEnvLayout, RirLambdaSigId,
        RirLambdaStorage, RirParamAbi, RirParamSemantic, RirProgram, RirStruct, RirStructId,
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
pub enum RustMaterialIntent {
    Read,
    Store,
    MutPlacePayload,
}

fn materialization_is_owned_payload(materialization: RustMaterialization) -> bool {
    matches!(
        materialization,
        RustMaterialization::Copy | RustMaterialization::Share | RustMaterialization::CloneHandle
    )
}

#[derive(Debug, Clone, Copy)]
pub struct AirRustRepPolicy<'a> {
    program: &'a AirProgram,
    classes: &'a TypePassClasses,
}

impl<'a> AirRustRepPolicy<'a> {
    pub fn new(program: &'a AirProgram, classes: &'a TypePassClasses) -> Self {
        Self { program, classes }
    }

    pub fn copyable(self, ty: TypeId) -> bool {
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
                    TypeData::Int | TypeData::Float | TypeData::Bool => RustMaterialization::Copy,
                    TypeData::DataRef(_) => RustMaterialization::CloneHandle,
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
            _ => RustMaterialGap::UnsupportedType,
        }
    }

    fn global_material_gap(self, ty: TypeId) -> RustMaterialGap {
        match self.program.type_arena.data(ty) {
            TypeData::Void | TypeData::Any | TypeData::Function(_) | TypeData::Dyn(_) => {
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
            | TypeData::String
            | TypeData::List(_)
            | TypeData::Map { .. }
            | TypeData::Slice(_)
            | TypeData::DataRef(_)
            | TypeData::Extern(_) => RustMaterialGap::UnsupportedRooting,
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
            TypeData::String | TypeData::List(_) | TypeData::Map { .. } => {
                RustMaterialization::Share
            }
            TypeData::DataRef(_) => RustMaterialization::CloneHandle,
            TypeData::Slice(_) => RustMaterialization::BorrowGuard,
            TypeData::Optional(inner) | TypeData::Array { elem: inner, .. } => {
                self.composite_materialization([*inner])
            }
            TypeData::Tuple(elems) => self.composite_materialization(elems.iter().copied()),
            TypeData::Aggregate(id) => self.composite_materialization(
                self.program
                    .aggregate(*id)
                    .fields
                    .iter()
                    .map(|field| field.ty),
            ),
            TypeData::Enum(id) => self.composite_materialization(
                self.program
                    .enum_decl(*id)
                    .variants
                    .iter()
                    .flat_map(Self::variant_field_tys),
            ),
            TypeData::Extern(id) => self.composite_materialization(
                self.program
                    .extern_type(*id)
                    .fields
                    .iter()
                    .map(|field| field.ty),
            ),
            TypeData::Int
            | TypeData::Float
            | TypeData::Bool
            | TypeData::Void
            | TypeData::Any
            | TypeData::Function(_)
            | TypeData::Dyn(_) => RustMaterialization::Gap,
        }
    }

    fn exact_root_global_materialization(self, ty: TypeId) -> RustMaterialization {
        match self.program.type_arena.data(ty) {
            TypeData::Void
            | TypeData::Any
            | TypeData::Slice(_)
            | TypeData::Extern(_)
            | TypeData::Function(_)
            | TypeData::Dyn(_) => RustMaterialization::Gap,
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
            | TypeData::String
            | TypeData::DataRef(_)
            | TypeData::List(_)
            | TypeData::Map { .. } => self.materialization(ty),
        }
    }

    fn composite_materialization(
        self,
        fields: impl IntoIterator<Item = TypeId>,
    ) -> RustMaterialization {
        if fields
            .into_iter()
            .all(|field| self.stored_payload_supported(field))
        {
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

    fn stored_payload_supported(self, ty: TypeId) -> bool {
        self.stored_payload_supported_inner(ty, false, &mut BTreeSet::new())
    }

    fn stored_payload_supported_inner(
        self,
        ty: TypeId,
        cycle_broken: bool,
        active: &mut BTreeSet<TypeId>,
    ) -> bool {
        if !active.insert(ty) {
            return cycle_broken;
        }
        let supported =
            match self.program.type_arena.data(ty) {
                TypeData::Int
                | TypeData::Float
                | TypeData::Bool
                | TypeData::String
                | TypeData::DataRef(_) => true,
                TypeData::List(elem) => self.stored_payload_supported_inner(*elem, true, active),
                TypeData::Map { key, value, .. } => {
                    self.map_key_supported(*key)
                        && self.stored_payload_supported_inner(*value, true, active)
                }
                TypeData::Optional(inner) | TypeData::Array { elem: inner, .. } => {
                    self.stored_payload_supported_inner(*inner, cycle_broken, active)
                }
                TypeData::Tuple(elems) => elems
                    .iter()
                    .all(|elem| self.stored_payload_supported_inner(*elem, cycle_broken, active)),
                TypeData::Aggregate(id) => self.program.aggregate(*id).fields.iter().all(|field| {
                    self.stored_payload_supported_inner(field.ty, cycle_broken, active)
                }),
                TypeData::Enum(id) => self.program.enum_decl(*id).variants.iter().all(|variant| {
                    Self::variant_field_tys(variant)
                        .all(|ty| self.stored_payload_supported_inner(ty, cycle_broken, active))
                }),
                TypeData::Void
                | TypeData::Any
                | TypeData::Slice(_)
                | TypeData::Extern(_)
                | TypeData::Function(_)
                | TypeData::Dyn(_) => false,
            };
        active.remove(&ty);
        supported
    }

    pub fn list_supported(self, ty: TypeId) -> bool {
        let TypeData::List(elem) = self.program.type_arena.data(ty) else {
            return false;
        };
        self.stored_payload_supported(*elem)
    }

    fn shareable_value(self, ty: TypeId) -> bool {
        match self.program.type_arena.data(ty) {
            TypeData::String
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
        matches!(
            self.program.type_arena.data(ty),
            TypeData::Int | TypeData::Bool | TypeData::String
        )
    }

    pub fn map_value_supported(self, ty: TypeId) -> bool {
        self.stored_payload_supported(ty)
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

#[derive(Debug, Clone, Copy)]
pub struct RustRepPolicy<'a> {
    program: &'a RirProgram,
}

impl<'a> RustRepPolicy<'a> {
    pub fn new(program: &'a RirProgram) -> Self {
        Self { program }
    }

    pub fn value_rep(self, ty: RirTypeId) -> RustValueRep {
        match self.ty(ty) {
            RirType::Int | RirType::Float | RirType::Bool | RirType::Void => {
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
            RirParamSemantic::ScopedLambda => RirParamAbi::ScopedLambda,
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
                    Some(RirType::Int | RirType::Float | RirType::Bool) => {
                        RustMaterialization::Copy
                    }
                    Some(RirType::DataRef(_)) => RustMaterialization::CloneHandle,
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
            _ => RustMaterialGap::UnsupportedType,
        }
    }

    fn global_material_gap(self, ty: RirTypeId) -> RustMaterialGap {
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
            RirType::Option(inner) | RirType::Array { elem: inner, .. } => {
                self.composite_materialization([inner])
            }
            RirType::Struct(id) => self.composite_materialization(
                self.program.structs[id.index()]
                    .fields
                    .iter()
                    .map(|field| field.ty),
            ),
            RirType::Tuple(id) => self.composite_materialization(
                self.program.tuples[id.index()]
                    .fields
                    .iter()
                    .map(|field| field.ty),
            ),
            RirType::Enum(id) => self.composite_materialization(
                self.program.enums[id.index()]
                    .variants
                    .iter()
                    .flat_map(|variant| variant.fields.iter().map(|field| field.ty)),
            ),
            RirType::Slice(_) => RustMaterialization::BorrowGuard,
            RirType::Int | RirType::Float | RirType::Bool | RirType::Void | RirType::Lambda(_) => {
                RustMaterialization::Gap
            }
        }
    }

    fn exact_root_global_materialization(self, ty: RirTypeId) -> RustMaterialization {
        let Some(data) = self.ty_opt(ty) else {
            return RustMaterialization::Gap;
        };
        if matches!(data, RirType::Void | RirType::Slice(_) | RirType::Lambda(_)) {
            return RustMaterialization::Gap;
        }
        if self.copyable(ty) {
            return RustMaterialization::Copy;
        }
        match data {
            RirType::String | RirType::DataRef(_) => self.materialization(ty),
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
            | RirType::Void
            | RirType::List(_)
            | RirType::Map { .. }
            | RirType::Slice(_)
            | RirType::Lambda(_) => RustMaterialization::Gap,
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
        matches!(
            self.ty_opt(ty),
            Some(RirType::Int | RirType::Bool | RirType::String)
        )
    }

    fn composite_materialization(
        self,
        fields: impl IntoIterator<Item = RirTypeId>,
    ) -> RustMaterialization {
        if fields
            .into_iter()
            .all(|field| self.stored_payload_supported(field))
        {
            RustMaterialization::Share
        } else {
            RustMaterialization::Gap
        }
    }

    fn stored_payload_supported(self, ty: RirTypeId) -> bool {
        self.stored_payload_supported_inner(ty, false, &mut BTreeSet::new())
    }

    fn stored_payload_supported_inner(
        self,
        ty: RirTypeId,
        cycle_broken: bool,
        active: &mut BTreeSet<RirTypeId>,
    ) -> bool {
        if !active.insert(ty) {
            return cycle_broken;
        }
        let supported = match self.ty_opt(ty) {
            Some(
                RirType::Int
                | RirType::Float
                | RirType::Bool
                | RirType::String
                | RirType::DataRef(_),
            ) => true,
            Some(RirType::List(elem)) => self.stored_payload_supported_inner(elem, true, active),
            Some(RirType::Map { key, value }) => {
                self.map_key_supported(key)
                    && self.stored_payload_supported_inner(value, true, active)
            }
            Some(RirType::Option(inner) | RirType::Array { elem: inner, .. }) => {
                self.stored_payload_supported_inner(inner, cycle_broken, active)
            }
            Some(RirType::Struct(id)) => {
                self.program.structs.get(id.index()).is_some_and(|strukt| {
                    strukt.fields.iter().all(|field| {
                        self.stored_payload_supported_inner(field.ty, cycle_broken, active)
                    })
                })
            }
            Some(RirType::Tuple(id)) => self.program.tuples.get(id.index()).is_some_and(|tuple| {
                tuple.fields.iter().all(|field| {
                    self.stored_payload_supported_inner(field.ty, cycle_broken, active)
                })
            }),
            Some(RirType::Enum(id)) => self.program.enums.get(id.index()).is_some_and(|enm| {
                enm.variants.iter().all(|variant| {
                    variant.fields.iter().all(|field| {
                        self.stored_payload_supported_inner(field.ty, cycle_broken, active)
                    })
                })
            }),
            Some(RirType::Void | RirType::Slice(_) | RirType::Lambda(_)) | None => false,
        };
        active.remove(&ty);
        supported
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
                _ => format!("&{reference_lifetime}{}", self.rust_ty(ty)),
            },
            RirParamAbi::MutBorrow => format!("&{reference_lifetime}mut {}", self.rust_ty(ty)),
            RirParamAbi::MutPlace => {
                let payload = self.rust_ty(ty);
                format!("{}<'_, 'cx, {payload}>", target::mut_place_ty())
            }
            RirParamAbi::ScopedLambda => self.scoped_lambda_ty(ty),
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
        let RirType::Lambda(sig) = self.ty(ty) else {
            return target::scoped_lambda_ty("()", "()");
        };
        let (args, ret) = self.scoped_lambda_sig_args_ret(sig);
        target::scoped_lambda_ty(&args, &ret)
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

    pub fn lambda_sig_needs_lifetime(self, id: RirLambdaSigId) -> bool {
        self.program.lambdas_for_sig(id).any(|lambda| {
            lambda.captures.iter().any(|capture| {
                matches!(
                    capture.abi,
                    RirParamAbi::SharedBorrow
                        | RirParamAbi::MutBorrow
                        | RirParamAbi::StackCell
                        | RirParamAbi::ScopedPlaceCell
                )
            })
        })
    }

    pub fn lambda_sig_needs_ctx_lifetime(self, id: RirLambdaSigId) -> bool {
        self.lambda_sig_has_heap_env(id)
            || self.program.lambdas_for_sig(id).any(|lambda| {
                lambda.captures.iter().any(|capture| {
                    self.type_cx_dependent(capture.ty)
                        || matches!(
                            capture.abi,
                            RirParamAbi::HeapCell | RirParamAbi::ScopedPlaceCell
                        )
                })
            })
    }

    pub fn lambda_sig_has_heap_env(self, id: RirLambdaSigId) -> bool {
        self.program
            .lambdas_for_sig(id)
            .any(|lambda| matches!(lambda.storage, RirLambdaStorage::HeapEnv { .. }))
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
                    RirParamAbi::MutBorrow | RirParamAbi::MutPlace | RirParamAbi::ScopedLambda => {
                        false
                    }
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
                RirParamAbi::MutBorrow | RirParamAbi::MutPlace | RirParamAbi::ScopedLambda => false,
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

    pub fn rust_ty(self, ty: RirTypeId) -> String {
        match self.ty(ty) {
            RirType::Int => "i64".into(),
            RirType::Float => "f64".into(),
            RirType::Bool => "bool".into(),
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
            RirType::Array { elem, len } => format!("[{}; {len}]", self.rust_ty(elem)),
            RirType::List(elem) => target::anv_list_ty(self.rust_ty(elem)),
            RirType::Map { key, value } => {
                target::anv_map_ty(self.rust_ty(key), self.rust_ty(value))
            }
            RirType::Option(inner) => format!("Option<{}>", self.rust_ty(inner)),
            RirType::Slice(elem) => target::anv_slice_ty(self.rust_ty(elem)),
            RirType::Lambda(id) => self.lambda_sig_ty(id),
        }
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
            RirLambdaEnvFieldKind::Value => self.rust_ty(field.ty),
            RirLambdaEnvFieldKind::HeapCell { .. } => {
                let payload = self.rust_ty(field.ty);
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

    pub fn type_cx_dependent(self, ty: RirTypeId) -> bool {
        self.type_has_heap_shape(ty, Self::lambda_sig_needs_ctx_lifetime)
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
            RirType::Int | RirType::Float | RirType::Bool | RirType::String | RirType::Void => {
                false
            }
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
            RirType::Int | RirType::Float | RirType::Bool | RirType::Void => true,
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
                | RirType::Slice(_) => true,
                RirType::Int
                | RirType::Float
                | RirType::Bool
                | RirType::Void
                | RirType::Lambda(_) => false,
            },
            RirParamSemantic::MutBorrow => match ty {
                RirType::Option(inner) => self.supports_param(inner, semantic),
                RirType::Int
                | RirType::Float
                | RirType::Bool
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
            RirParamSemantic::ScopedLambda => matches!(ty, RirType::Lambda(_)),
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
        let policy = RustRepPolicy::new(program);
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
            RirType::Lambda(id) => {
                self.lambda_sigs.insert(id);
            }
            RirType::Int
            | RirType::Float
            | RirType::Bool
            | RirType::String
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
        AirRustRepPolicy, RustBorrowView, RustMaterialization, RustRepPolicy, RustTracePlan,
        RustValueRep,
    };
    use crate::rust::rir::{
        RirDataRef, RirDataRefId, RirField, RirFieldId, RirLambda, RirLambdaEscape, RirLambdaId,
        RirLambdaSig, RirLambdaSigId, RirLambdaSource, RirLambdaStorage, RirParamAbi,
        RirParamSemantic, RirProgram, RirStruct, RirStructId, RirSymbol, RirTuple, RirTupleId,
        RirType, RirTypeId,
    };

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
        let policy = RustRepPolicy::new(&program);

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
        let policy = AirRustRepPolicy::new(&program, &classes);

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
        let float = program.alloc_type(TypeData::Float);
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
        let classes = TypePassClasses::analyze(&program);
        let policy = AirRustRepPolicy::new(&program, &classes);

        assert!(!policy.map_supported(float_key));
        assert!(policy.map_supported(float_value));
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
        let policy = AirRustRepPolicy::new(&program, &classes);

        assert!(policy.stored_payload_supported(string));
        assert!(policy.stored_payload_supported(list));
        assert!(policy.stored_payload_supported(aggregate));
        assert!(!policy.stored_payload_supported(slice));
        assert!(!policy.stored_payload_supported(function));
        assert!(!policy.stored_payload_supported(slice_tuple));
        assert!(!policy.stored_payload_supported(function_tuple));
    }

    #[test]
    fn policy_keeps_string_borrow_as_str_view() {
        let mut program = RirProgram::default();
        let string = RirTypeId::from_index(program.types.len());
        program.types.push(RirType::String);
        let policy = RustRepPolicy::new(&program);

        assert_eq!(policy.borrow_view(string), RustBorrowView::Str);
        assert!(policy.cow_value(string));
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
            native_key: None,
            copyable: false,
            fields: vec![RirField {
                id: RirFieldId::from_index(0),
                symbol: RirSymbol::new("text"),
                ty: string,
            }],
        });
        let policy = RustRepPolicy::new(&program);

        assert!(!policy.copyable(label));
        assert!(policy.shareable_value(label));
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
        let policy = RustRepPolicy::new(&program);

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
            function: crate::rust::rir::RirFunctionId::from_index(0),
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
        let policy = RustRepPolicy::new(&program);

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
            RustMaterialization::Gap
        );
        assert!(policy.stored_payload_supported(list));
        assert!(policy.stored_payload_supported(map));
        assert!(policy.stored_payload_supported(node));
        assert!(!policy.stored_payload_supported(slice));
        assert!(!policy.stored_payload_supported(lambda));
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
            function: crate::rust::rir::RirFunctionId::from_index(0),
            sig: lambda_sig,
            escape: RirLambdaEscape::NonEscaping,
            storage: RirLambdaStorage::ScopedCaptures,
            captures: vec![crate::rust::rir::RirLambdaCapture {
                ty: int,
                semantic: RirParamSemantic::MutBorrow,
                abi: RirParamAbi::MutBorrow,
                kind: crate::rust::rir::RirLambdaCaptureKind::Param,
            }],
        });
        let policy = RustRepPolicy::new(&program);

        assert_eq!(policy.materialization(lambda), RustMaterialization::Gap);
        assert!(!policy.value_from_ref_supported(lambda));
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
        let policy = AirRustRepPolicy::new(&program, &classes);

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
        let policy = AirRustRepPolicy::new(&program, &classes);

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
            native_key: None,
            copyable: false,
            fields: vec![RirField {
                id: RirFieldId::from_index(0),
                symbol: RirSymbol::new("a"),
                ty: RirTypeId::from_index(4),
            }],
        });
        let policy = RustRepPolicy::new(&program);
        let a = RirTypeId::from_index(1);

        assert!(policy.stored_payload_supported(a));
        assert!(policy.type_owns_heap_edges(a));
        assert!(policy.type_cx_dependent(a));
        assert_eq!(policy.materialization(a), RustMaterialization::Share);
        assert!(RustTracePlan::build(&program).needs_struct_trace(RirStructId::from_index(0)));
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
        let policy = AirRustRepPolicy::new(&program, &classes);

        assert!(!policy.copyable(node));
        assert!(policy.value_place_shareable(node));
        assert!(policy.supports_param_mode(node, ParamMode::Value));
        assert!(policy.supports_param_mode(node, ParamMode::SharedBorrow));
        assert!(policy.supports_param_mode(node, ParamMode::MutBorrow));
        assert!(!policy.copyable(maybe_node));
        assert!(policy.value_place_shareable(maybe_node));
    }
}
