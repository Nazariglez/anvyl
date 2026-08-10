use std::{collections::HashSet, error::Error, fmt};

use anvyx_externs::{
    ExternMaterialization, ExternTypeKey, RustCallContext, RustMaterializerBinding, RustPath,
};
use anvyx_frontend::{
    air,
    ast::{BinaryOp, FormatKind, FormatSign, FormatSpec, ScalarKind, UnaryOp},
};

use super::{
    native_call,
    rep_policy::{
        LambdaStorageFamily, RirRustRepPolicy, RustApproxLayout, RustPrimitiveLayout,
        RustRecipePosition, child_recipe_position, layout, target_profile,
    },
};

macro_rules! rir_id {
    ($name:ident) => {
        #[repr(transparent)]
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
        pub struct $name(pub u32);

        impl $name {
            pub const fn from_index(index: usize) -> Self {
                Self(index as u32)
            }

            pub const fn index(self) -> usize {
                self.0 as usize
            }
        }
    };
}

rir_id!(RirFunctionId);
rir_id!(RirExternId);
rir_id!(RirGlobalId);
rir_id!(RirTypeId);
rir_id!(RirConstId);
rir_id!(RirStringLiteralId);
rir_id!(RirLocalId);
rir_id!(RirLoopId);
rir_id!(RirStructId);
rir_id!(RirDataRefId);
rir_id!(RirDataRefPlaceId);
rir_id!(RirEnumId);
rir_id!(RirFlagId);
rir_id!(RirFlagMemberId);
rir_id!(RirTupleId);
rir_id!(RirVariantId);
rir_id!(RirFieldId);
rir_id!(RirStringifyHelperId);
rir_id!(RirStringifyReqId);
rir_id!(RirLambdaSigId);
rir_id!(RirLambdaId);
rir_id!(RirLambdaEnvId);
rir_id!(RirCollectionStorageId);
rir_id!(RirCellId);
rir_id!(RirScopedPlaceCellId);
rir_id!(RirDynCarrierId);
rir_id!(RirDynDispatchId);
rir_id!(RirDynWeakeningId);
rir_id!(RirDynVariantSetId);
rir_id!(RirMaterializerId);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct RirDynVariantId {
    carrier: RirDynCarrierId,
    index: u32,
}

impl RirDynVariantId {
    pub(super) const fn new(carrier: RirDynCarrierId, index: usize) -> Self {
        Self {
            carrier,
            index: index as u32,
        }
    }

    pub const fn carrier(self) -> RirDynCarrierId {
        self.carrier
    }

    pub const fn index(self) -> usize {
        self.index as usize
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RirSymbol(pub String);

impl RirSymbol {
    pub fn new(text: impl Into<String>) -> Self {
        Self(text.into())
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct RirProgram {
    pub globals: Vec<RirGlobal>,
    pub functions: Vec<RirFunction>,
    pub externs: Vec<RirExtern>,
    pub types: Vec<RirType>,
    pub structs: Vec<RirStruct>,
    pub datarefs: Vec<RirDataRef>,
    pub dataref_places: Vec<RirDataRefPlace>,
    pub enums: Vec<RirEnum>,
    pub flags: Vec<RirFlag>,
    pub tuples: Vec<RirTuple>,
    pub lambda_sigs: Vec<RirLambdaSig>,
    pub lambdas: Vec<RirLambda>,
    pub lambda_envs: Vec<RirLambdaEnvLayout>,
    pub collection_storages: Vec<RirCollectionStorage>,
    pub cells: Vec<RirCellDecl>,
    pub scoped_place_cells: Vec<RirScopedPlaceCellDecl>,
    pub stringify_reqs: Vec<RirStringifyReq>,
    pub stringify_helpers: Vec<RirStringifyHelper>,
    pub dyn_carriers: Vec<RirDynCarrier>,
    pub dyn_dispatches: Vec<RirDynDispatch>,
    pub dyn_weakenings: Vec<RirDynWeakening>,
    pub dyn_variant_sets: Vec<RirDynVariantSet>,
    pub materializers: Vec<RirMaterializer>,
    pub value_materializers: Vec<Option<RirMaterializerId>>,
    pub consts: Vec<RirConst>,
    pub string_literals: Vec<RirStringLiteral>,
    pub entry: Option<RirFunctionId>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RirMaterializer {
    pub id: RirMaterializerId,
    pub ty: RirTypeId,
    pub position: RustRecipePosition,
    pub action: RirMaterializerAction,
    pub copy: Option<RirCopyEvidence>,
    pub support: Option<RirSupportEvidence>,
}

impl RirMaterializer {
    pub(super) fn is_copy(&self) -> bool {
        self.copy.is_some()
    }

    pub(super) fn stages_collection(&self, program: &RirProgram) -> bool {
        self.position == RustRecipePosition::StoredPayload(LambdaStorageFamily::DataRefProjection)
            && matches!(self.action, RirMaterializerAction::ManagedShare)
            && matches!(
                program.types[self.ty.index()],
                RirType::List(_) | RirType::Map { .. }
            )
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RirCopyEvidence {
    Leaf,
    Struct {
        family: LambdaStorageFamily,
        fields: Vec<RirMaterializerId>,
    },
    Tuple {
        fields: Vec<RirMaterializerId>,
    },
    Array {
        elem: RirMaterializerId,
    },
    Enum {
        variants: Vec<Vec<RirMaterializerId>>,
    },
    Optional {
        payload: RirMaterializerId,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RirSupportEvidence {
    List {
        elem: RirMaterializerId,
    },
    Map {
        key: RirMaterializerId,
        key_contract: RirMaterializerId,
        value: RirMaterializerId,
    },
    ProviderStruct {
        fields: Vec<RirMaterializerId>,
    },
    ProviderEnum {
        variants: Vec<Vec<RirMaterializerId>>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RirMaterializerAction {
    Copy,
    ManagedShare,
    IdentityShare,
    CallableShare,
    ProviderMaterialize {
        binding: RustMaterializerBinding,
    },
    Struct {
        fields: Vec<RirMaterializerId>,
    },
    Tuple {
        fields: Vec<RirMaterializerId>,
    },
    Array {
        elem: RirMaterializerId,
    },
    Enum {
        variants: Vec<Vec<RirMaterializerId>>,
    },
    Optional {
        payload: RirMaterializerId,
    },
    DynamicMaterialize {
        carrier: RirDynCarrierId,
        variants: Vec<RirMaterializerId>,
    },
}

impl RirMaterializerAction {
    pub(super) fn requires_helper(&self) -> bool {
        matches!(
            self,
            Self::Struct { .. }
                | Self::Tuple { .. }
                | Self::Array { .. }
                | Self::Enum { .. }
                | Self::Optional { .. }
                | Self::DynamicMaterialize { .. }
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RirDynCarrier {
    pub id: RirDynCarrierId,
    pub storage_ty: RirTypeId,
    pub variants: Vec<RirDynVariant>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RirDynVariant {
    pub id: RirDynVariantId,
    pub concrete_ty: RirTypeId,
    pub storage: RirDynStorage,
    pub inline_layout: RirDynInlineLayout,
    pub box_reason: Option<RirDynBoxReason>,
    pub payload: RirMaterializerId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct RirDynLayout {
    pub size: u64,
    pub align: u64,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RirDynInlineLayout {
    Known(RirDynLayout),
    Function,
    Recursive,
    Provider,
    Unsupported,
    Overflow,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RirDynBoxReason {
    Function,
    FunctionField,
    Recursive,
    Threshold,
    WeakeningClass(RirDynCarrierId),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RirDynStorage {
    Inline,
    Boxed,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RirResolvedCallTarget {
    Function(RirFunctionId),
    Extern(RirExternId),
    Promoted {
        projections: Vec<RirPlaceStep>,
        target: Box<RirResolvedCallTarget>,
    },
}

impl RirResolvedCallTarget {
    pub(super) fn base(&self) -> &Self {
        match self {
            Self::Promoted { target, .. } => target.base(),
            _ => self,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RirDynDispatchArm {
    pub receiver: RirPassMode,
    pub target: RirResolvedCallTarget,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RirDynDispatch {
    pub id: RirDynDispatchId,
    pub carrier: RirDynCarrierId,
    pub params: Vec<RirDynDispatchParam>,
    pub result_ty: RirTypeId,
    pub arms: Vec<RirDynDispatchArm>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct RirDynDispatchParam {
    pub ty: RirTypeId,
    pub mode: RirPassMode,
    pub escape: RirParamEscape,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct RirDynWeakenArm {
    pub target: RirDynVariantId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RirDynWeakening {
    pub id: RirDynWeakeningId,
    pub source: RirDynCarrierId,
    pub target: RirDynCarrierId,
    pub arms: Vec<RirDynWeakenArm>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RirDynVariantSet {
    pub id: RirDynVariantSetId,
    pub carrier: RirDynCarrierId,
    pub target: RirTypeId,
    pub variants: Vec<RirDynVariantId>,
}

impl RirProgram {
    pub fn string_literal(&self, id: RirStringLiteralId) -> &RirStringLiteral {
        &self.string_literals[id.index()]
    }

    pub fn stringify_req(&self, ty: RirTypeId) -> Option<&RirStringifyReq> {
        self.stringify_reqs.iter().find(|req| req.ty == ty)
    }

    pub fn dyn_borrow_symbol(&self, carrier: RirDynCarrierId) -> String {
        let carrier = &self.dyn_carriers[carrier.index()];
        let RirType::Enum(id) = self.types[carrier.storage_ty.index()] else {
            unreachable!("verified dynamic carrier storage")
        };
        format!("__AnvDynBorrow_{}", self.enums[id.index()].symbol.as_str())
    }

    pub fn lambdas_for_sig(&self, sig: RirLambdaSigId) -> impl Iterator<Item = &RirLambda> {
        self.lambdas.iter().filter(move |lambda| lambda.sig == sig)
    }

    pub(super) fn callback_sigs(
        &self,
    ) -> (
        Vec<RirLambdaSigId>,
        Vec<RirLambdaSigId>,
        Vec<RirLambdaSigId>,
    ) {
        let mut retained = vec![];
        let mut provider = vec![];
        let mut heap = vec![];
        for ext in &self.externs {
            for param in &ext.params {
                let RirType::Lambda(sig) = self.types[param.ty.index()] else {
                    continue;
                };
                let roles = match param.mode {
                    RirPassMode::EscapingLambda => (&mut retained, Some(&mut provider)),
                    RirPassMode::AnvCallback => (&mut retained, Some(&mut heap)),
                    _ => continue,
                };
                if !roles.0.contains(&sig) {
                    roles.0.push(sig);
                }
                if let Some(role) = roles.1
                    && !role.contains(&sig)
                {
                    role.push(sig);
                }
            }
        }
        (retained, provider, heap)
    }

    pub(super) fn has_retained_callbacks(&self) -> bool {
        self.externs.iter().any(|ext| {
            ext.params.iter().any(|param| {
                matches!(
                    param.mode,
                    RirPassMode::EscapingLambda | RirPassMode::AnvCallback
                )
            })
        })
    }

    pub(super) fn dyn_carrier_for_enum(&self, enm: RirEnumId) -> Option<&RirDynCarrier> {
        self.dyn_carriers
            .iter()
            .find(|carrier| self.types.get(carrier.storage_ty.index()) == Some(&RirType::Enum(enm)))
    }

    pub fn collection_storage_for(&self, value_ty: RirTypeId) -> Option<&RirCollectionStorage> {
        self.collection_storages
            .iter()
            .find(|storage| storage.value_ty == value_ty)
    }

    pub fn place_ty(&self, function: &RirFunction, place: &RirPlace) -> Option<RirTypeId> {
        if let Some(step) = place.projections.last() {
            return Some(step.target_ty);
        }
        match place.root {
            RirPlaceRoot::Local(local) => function.locals.get(local.index()).map(|local| local.ty),
            RirPlaceRoot::Global(global) => {
                self.globals.get(global.index()).map(|global| global.ty)
            }
        }
    }

    pub fn mut_place_root_ty(
        &self,
        function: &RirFunction,
        access: &RirMutPlaceAccess,
    ) -> Option<RirTypeId> {
        match access {
            RirMutPlaceAccess::Handle(handle) => match handle {
                RirMutPlaceHandle::Local { local } | RirMutPlaceHandle::Param { local } => {
                    function.locals.get(local.index()).map(|local| local.ty)
                }
                RirMutPlaceHandle::StackCell { cell } | RirMutPlaceHandle::HeapCell { cell } => {
                    let cell = match cell {
                        RirCellRef::Owner(cell) | RirCellRef::Capture { cell, .. } => cell,
                    };
                    self.cells.get(cell.index()).map(|cell| cell.payload_ty)
                }
                RirMutPlaceHandle::ScopedPlaceCell { cell } => {
                    let cell = match cell {
                        RirScopedPlaceCellRef::Owner(cell)
                        | RirScopedPlaceCellRef::Capture { cell, .. } => cell,
                    };
                    self.scoped_place_cells
                        .get(cell.index())
                        .map(|cell| cell.payload_ty)
                }
                RirMutPlaceHandle::Global { global } => {
                    self.globals.get(global.index()).map(|global| global.ty)
                }
            },
            RirMutPlaceAccess::DataRef { place, .. } => self
                .dataref_places
                .get(place.index())?
                .storage
                .last()
                .map(|step| step.target_ty),
        }
    }

    pub fn mut_place_ty(&self, function: &RirFunction, arg: &RirMutPlaceArg) -> Option<RirTypeId> {
        Some(arg.final_ty(self.mut_place_root_ty(function, &arg.access)?))
    }

    pub fn collection_ty(
        &self,
        function: &RirFunction,
        collection: &RirCollectionAccess,
    ) -> Option<RirTypeId> {
        match collection {
            RirCollectionAccess::Direct(place) => self.place_ty(function, place),
            RirCollectionAccess::MutPlace(place) => self.mut_place_ty(function, place),
        }
    }

    pub fn verified_place_ty(&self, function: &RirFunction, place: &RirPlace) -> RirTypeId {
        self.place_ty(function, place).expect("verified place root")
    }

    pub fn verified_mut_place_root_ty(
        &self,
        function: &RirFunction,
        access: &RirMutPlaceAccess,
    ) -> RirTypeId {
        self.mut_place_root_ty(function, access)
            .expect("verified mutable-place root")
    }

    pub fn verified_mut_place_ty(&self, function: &RirFunction, arg: &RirMutPlaceArg) -> RirTypeId {
        self.mut_place_ty(function, arg)
            .expect("verified mutable-place root")
    }

    pub fn verified_collection_ty(
        &self,
        function: &RirFunction,
        collection: &RirCollectionAccess,
    ) -> RirTypeId {
        self.collection_ty(function, collection)
            .expect("verified collection root")
    }

    pub fn sequence_elem_materializer(&self, value_ty: RirTypeId) -> Option<RirMaterializerId> {
        match self.collection_storage_for(value_ty)?.kind {
            RirCollectionStorageKind::Array {
                elem_materializer, ..
            }
            | RirCollectionStorageKind::Slice {
                elem_materializer, ..
            }
            | RirCollectionStorageKind::List {
                elem_materializer, ..
            } => Some(elem_materializer),
            RirCollectionStorageKind::Map { .. } => None,
        }
    }

    pub fn map_materializers(
        &self,
        value_ty: RirTypeId,
    ) -> Option<(RirMaterializerId, RirMaterializerId)> {
        let RirCollectionStorageKind::Map {
            key_materializer,
            value_materializer,
            ..
        } = self.collection_storage_for(value_ty)?.kind
        else {
            return None;
        };
        Some((key_materializer, value_materializer))
    }

    pub fn collection_replace_ty(&self, ty: RirTypeId) -> bool {
        matches!(
            self.types[ty.index()],
            RirType::List(_) | RirType::Map { .. }
        )
    }

    pub fn unit_only_enum(&self, ty: RirTypeId) -> bool {
        let Some(RirType::Enum(enum_id)) = self.types.get(ty.index()) else {
            return false;
        };
        self.enums
            .get(enum_id.index())
            .is_some_and(RirEnum::is_unit_only)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RirGlobal {
    pub id: RirGlobalId,
    pub name: RirSymbol,
    pub slot_symbol: RirSymbol,
    pub ty: RirTypeId,
    pub mutable: bool,
    pub init: RirFunctionId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RirNativeType {
    pub key: ExternTypeKey,
    pub path: RustPath,
    pub materializer: Option<RustMaterializerBinding>,
}

impl RirNativeType {
    fn validated_materializer(
        &self,
        mode: ExternMaterialization,
    ) -> Option<&RustMaterializerBinding> {
        let materializer = self.materializer.as_ref()?;
        if materializer.mode != mode || materializer.rust_type != self.path {
            return None;
        }
        let native_type = self.path.segments.last()?;
        let mut expected = self.path.segments[..self.path.segments.len() - 1].to_vec();
        expected.push(anvyx_externs::native_materializer_module(native_type));
        expected.push(anvyx_externs::INLINE_MATERIALIZER_SYMBOL.to_string());
        (materializer.path.crate_name == self.path.crate_name
            && materializer.path.segments == expected)
            .then_some(materializer)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RirStruct {
    pub id: RirStructId,
    pub role: RirStructRole,
    pub symbol: RirSymbol,
    pub display: RirSymbol,
    pub native: Option<RirNativeType>,
    pub native_layout: Option<RirDynLayout>,
    pub native_ref: bool,
    pub fields: Vec<RirField>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RirStructRole {
    Source,
    Extern,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RirField {
    pub id: RirFieldId,
    pub symbol: RirSymbol,
    pub ty: RirTypeId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RirTuple {
    pub id: RirTupleId,
    pub symbol: RirSymbol,
    pub display: RirSymbol,
    pub fields: Vec<RirField>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RirDataRef {
    pub id: RirDataRefId,
    pub native_key: Option<ExternTypeKey>,
    pub symbol: RirSymbol,
    pub display: RirSymbol,
    pub cycle_capable: bool,
    pub fields: Vec<RirField>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RirDataRefPlace {
    pub dataref: RirDataRefId,
    pub storage: Vec<RirPlaceStep>,
    pub materializer: RirMaterializerId,
}

impl RirDataRef {
    pub fn storage_symbol(&self) -> String {
        format!("{}Storage", self.symbol.as_str())
    }

    pub fn heap_type_symbol(&self) -> String {
        format!("{}HeapType", self.symbol.as_str())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RirEnum {
    pub id: RirEnumId,
    pub role: RirEnumRole,
    pub native: Option<RirNativeType>,
    pub native_layout: Option<RirDynLayout>,
    pub core: Option<RirCoreEnumKind>,
    pub repr: RirEnumRepr,
    pub raw_type: Option<RirTypeId>,
    pub symbol: RirSymbol,
    pub display: RirSymbol,
    pub variants: Vec<RirVariant>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RirEnumRole {
    DynamicCarrier,
    Source,
    Extern,
}

impl RirEnum {
    pub fn is_unit_only(&self) -> bool {
        self.variants
            .iter()
            .all(|variant| matches!(variant.kind, RirVariantKind::Unit))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RirFlag {
    pub id: RirFlagId,
    pub symbol: RirSymbol,
    pub display: RirSymbol,
    pub known_bits: i64,
    pub members: Vec<RirFlagMember>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RirFlagMember {
    pub id: RirFlagMemberId,
    pub symbol: RirSymbol,
    pub display: RirSymbol,
    pub value: i64,
    pub atomic: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RirEnumRepr {
    Adt,
    RawInt,
    RawString,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum RirRawEnumValue {
    Int(i64),
    String(RirStringLiteralId),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RirCoreEnumKind {
    Option,
    Result,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RirVariant {
    pub id: RirVariantId,
    pub symbol: RirSymbol,
    pub display: RirSymbol,
    pub kind: RirVariantKind,
    pub raw_value: Option<RirRawEnumValue>,
    pub fields: Vec<RirField>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RirVariantKind {
    Unit,
    Tuple,
    Struct,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RirLambdaSig {
    pub id: RirLambdaSigId,
    pub params: Vec<RirLambdaParam>,
    pub ret: RirTypeId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct RirLambdaParam {
    pub ty: RirTypeId,
    pub mode: RirPassMode,
    pub escape: RirParamEscape,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RirParamEscape {
    NonEscaping,
    Escaping,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RirLambda {
    pub id: RirLambdaId,
    pub function: RirFunctionId,
    pub sig: RirLambdaSigId,
    pub escape: RirLambdaEscape,
    pub storage: RirLambdaStorage,
    pub captures: Vec<RirLambdaCapture>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct RirLambdaCapture {
    pub ty: RirTypeId,
    pub mode: RirPassMode,
    pub kind: RirLambdaCaptureKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RirLambdaCaptureKind {
    Param,
    StackCell { cell: RirCellId },
    HeapCell { cell: RirCellId },
    ScopedPlaceCell { cell: RirScopedPlaceCellId },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RirLambdaEscape {
    NonEscaping,
    Escaping,
}

impl RirLambdaEscape {
    fn from_param_escape(escape: RirParamEscape) -> Self {
        match escape {
            RirParamEscape::NonEscaping => Self::NonEscaping,
            RirParamEscape::Escaping => Self::Escaping,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RirLambdaStorage {
    ZeroEnv,
    ScopedCaptures,
    HeapEnv { env: RirLambdaEnvId },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RirLambdaEnvLayout {
    pub id: RirLambdaEnvId,
    pub lambda: RirLambdaId,
    pub symbol: RirSymbol,
    pub fields: Vec<RirLambdaEnvField>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RirCollectionStorage {
    pub id: RirCollectionStorageId,
    pub value_ty: RirTypeId,
    pub kind: RirCollectionStorageKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RirCollectionStorageKind {
    Array {
        elem_ty: RirTypeId,
        elem_materializer: RirMaterializerId,
    },
    Slice {
        elem_ty: RirTypeId,
        elem_materializer: RirMaterializerId,
    },
    List {
        elem_ty: RirTypeId,
        elem_materializer: RirMaterializerId,
        symbol: RirSymbol,
    },
    Map {
        key_ty: RirTypeId,
        value_ty: RirTypeId,
        key_materializer: RirMaterializerId,
        value_materializer: RirMaterializerId,
        symbol: RirSymbol,
    },
}

impl RirCollectionStorage {
    pub fn heap_symbol(&self) -> Option<&RirSymbol> {
        match &self.kind {
            RirCollectionStorageKind::List { symbol, .. }
            | RirCollectionStorageKind::Map { symbol, .. } => Some(symbol),
            RirCollectionStorageKind::Array { .. } | RirCollectionStorageKind::Slice { .. } => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RirLambdaEnvField {
    pub ty: RirTypeId,
    pub symbol: RirSymbol,
    pub kind: RirLambdaEnvFieldKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RirLambdaEnvFieldKind {
    Value,
    HeapCell { cell: RirCellId },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RirCellStorage {
    StackScoped,
    Heap,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RirCellDecl {
    pub id: RirCellId,
    pub owner: RirFunctionId,
    pub source_local: RirLocalId,
    pub payload_ty: RirTypeId,
    pub storage: RirCellStorage,
    pub lifetime: RirCellLifetime,
    pub symbol: RirSymbol,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RirCellLifetime {
    Function,
    Loop { loop_id: RirLoopId },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RirCellRef {
    Owner(RirCellId),
    Capture { cell: RirCellId, local: RirLocalId },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RirScopedPlaceCellDecl {
    pub id: RirScopedPlaceCellId,
    pub owner: RirFunctionId,
    pub source: RirScopedPlaceSource,
    pub payload_ty: RirTypeId,
    pub symbol: RirSymbol,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RirScopedPlaceSource {
    SourceMutParam { place: RirMutPlaceArg },
    RefSelf { place: RirMutPlaceArg },
    PatternAlias { place: RirMutPlaceArg },
    ForRefAlias { place: RirMutPlaceArg },
}

impl RirScopedPlaceSource {
    pub fn place(&self) -> &RirMutPlaceArg {
        match self {
            Self::SourceMutParam { place }
            | Self::RefSelf { place }
            | Self::PatternAlias { place }
            | Self::ForRefAlias { place } => place,
        }
    }

    pub fn root_local(&self) -> Option<RirLocalId> {
        self.place().root_local()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RirScopedPlaceCellRef {
    Owner(RirScopedPlaceCellId),
    Capture {
        cell: RirScopedPlaceCellId,
        local: RirLocalId,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RirStringifyReq {
    pub id: RirStringifyReqId,
    pub ty: RirTypeId,
    pub kind: RirStringifyReqKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RirStringifyReqKind {
    Helper(RirStringifyHelperId),
    Override {
        function: RirFunctionId,
        mode: RirPassMode,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RirStringifyHelper {
    pub id: RirStringifyHelperId,
    pub ty: RirTypeId,
    pub symbol: RirSymbol,
    pub kind: RirStringifyHelperKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RirStringifyHelperKind {
    Struct(RirStructId),
    Enum {
        enm: RirEnumId,
        variants: Vec<RirEnumStringifyVariant>,
    },
    Flag {
        flag: RirFlagId,
        empty: RirStringLiteralId,
        members: Vec<RirStringLiteralId>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RirEnumStringifyVariant {
    pub label: RirStringLiteralId,
    pub field_labels: Vec<RirStringLiteralId>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct RirFunction {
    pub id: RirFunctionId,
    pub symbol: RirSymbol,
    pub params: Vec<RirLocalId>,
    pub ret: RirReturn,
    pub locals: Vec<RirLocal>,
    pub body: RirStructuredBlock,
}

impl RirFunction {
    fn parameter(&self, local: RirLocalId) -> Option<&RirLocal> {
        self.locals
            .get(local.index())
            .filter(|local| matches!(local.binding, RirLocalBinding::Parameter { .. }))
    }

    fn parameter_data(
        &self,
        local: RirLocalId,
    ) -> Option<(RirTypeId, RirPassMode, RirParamEscape)> {
        let local = self.parameter(local)?;
        let RirLocalBinding::Parameter { mode, escape } = local.binding else {
            unreachable!("parameter local binding")
        };
        Some((local.ty, mode, escape))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RirPassMode {
    Value,
    SharedBorrow,
    MutBorrow,
    MutPlace,
    DynBorrow,
    ScopedLambda,
    EscapingLambda,
    AnvCallback,
    StackCell,
    HeapCell,
    ScopedPlaceCell,
}

impl RirPassMode {
    pub(super) fn is_readonly_receiver(self) -> bool {
        matches!(self, Self::Value | Self::SharedBorrow)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RirLocalBinding {
    Value,
    Parameter {
        mode: RirPassMode,
        escape: RirParamEscape,
    },
    DirectPayload,
    ScopedPlacePayload,
}

impl RirLocalBinding {
    pub(super) fn parameter(self) -> Option<(RirPassMode, RirParamEscape)> {
        let Self::Parameter { mode, escape } = self else {
            return None;
        };
        Some((mode, escape))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct RirReturn {
    pub ty: RirTypeId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RirLocal {
    pub id: RirLocalId,
    pub ty: RirTypeId,
    pub mutable: bool,
    pub symbol: RirSymbol,
    pub binding: RirLocalBinding,
}

#[derive(Debug, Clone, PartialEq)]
pub enum RirStmt {
    Init {
        local: RirLocalId,
        value: RirRValue,
    },
    GlobalEnsure {
        global: RirGlobalId,
    },
    GlobalSetRoot {
        global: RirGlobalId,
        value: RirRValue,
    },
    GlobalUpdateRoot {
        global: RirGlobalId,
        value: RirRValue,
    },
    MutPlaceSet {
        place: RirMutPlaceArg,
        value: RirRValue,
    },
    Assign {
        dst: RirPlace,
        value: RirRValue,
    },
    CellInit {
        cell: RirCellRef,
        value: RirRValue,
    },
    ScopedPlaceCellInit {
        cell: RirScopedPlaceCellId,
    },
    CellSet {
        cell: RirCellRef,
        value: RirRValue,
    },
    ScopedPlaceCellSet {
        cell: RirScopedPlaceCellRef,
        value: RirRValue,
    },
    DataRefSet {
        object: RirOperand,
        place: RirDataRefPlaceId,
        suffix: Vec<RirPlaceStep>,
        value: RirRValue,
    },
    SequenceSlotSet {
        collection: RirCollectionAccess,
        step: RirPlaceStep,
        value: RirOperand,
    },
    MapValueSet {
        map: RirCollectionAccess,
        index: RirLocalId,
        value: RirOperand,
    },
    Eval(RirRValue),
    If(RirIf),
    Loop(RirLoop),
    RangeFor(RirRangeFor),
    CollectionFor(RirCollectionFor),
    CollectionLoanScope(RirCollectionLoanScope),
    CollectionSlotScope(RirStructuredBlock),
    PatternMatch(RirPatternMatch),
    DynMatch(RirDynMatch),
    OptionMatch(RirOptionMatch),
    MapEntryMatch(RirMapEntryMatch),
}

impl RirStmt {
    pub fn for_each_child(&self, f: &mut impl FnMut(RirChild<'_>)) {
        match self {
            Self::Init { value, .. }
            | Self::GlobalSetRoot { value, .. }
            | Self::GlobalUpdateRoot { value, .. }
            | Self::CellInit { value, .. }
            | Self::CellSet { value, .. }
            | Self::ScopedPlaceCellSet { value, .. } => {
                value.for_each_child(f);
            }
            Self::Eval(value) => value.for_each_child(f),
            Self::MutPlaceSet { place, value } => {
                f(RirChild::MutPlace { place });
                value.for_each_child(f);
            }
            Self::Assign { dst, value } => {
                f(RirChild::Place { place: dst });
                value.for_each_child(f);
            }
            Self::DataRefSet {
                object,
                suffix,
                value,
                ..
            } => {
                f(RirChild::Operand { operand: object });
                for step in suffix {
                    if let Some(local) = step.index_local() {
                        f(RirChild::LocalRead(local));
                    }
                }
                value.for_each_child(f);
            }
            Self::SequenceSlotSet {
                collection,
                step,
                value,
            } => {
                f(RirChild::Collection { collection });
                f(RirChild::LocalRead(
                    step.index_local().expect("sequence slot step"),
                ));
                f(RirChild::Operand { operand: value });
            }
            Self::MapValueSet { map, index, value } => {
                f(RirChild::Collection { collection: map });
                f(RirChild::LocalRead(*index));
                f(RirChild::Operand { operand: value });
            }
            Self::If(branch) => {
                f(RirChild::Operand {
                    operand: &branch.cond,
                });
                f(RirChild::Block(&branch.then_block));
                if let Some(block) = &branch.else_block {
                    f(RirChild::Block(block));
                }
            }
            Self::Loop(loop_) => f(RirChild::Block(&loop_.body)),
            Self::RangeFor(range) => {
                f(RirChild::Operand {
                    operand: &range.start,
                });
                f(RirChild::Operand {
                    operand: &range.end,
                });
                for operand in range.ordinal_plan.operands() {
                    f(RirChild::Operand { operand });
                }
                f(RirChild::Block(&range.body));
            }
            Self::CollectionFor(for_) => {
                f(RirChild::LocalRead(for_.len));
                for operand in for_.ordinal_plan.operands() {
                    f(RirChild::Operand { operand });
                }
                f(RirChild::Block(&for_.body));
            }
            Self::CollectionLoanScope(scope) => {
                f(RirChild::Collection {
                    collection: &scope.root,
                });
                f(RirChild::Block(&scope.body));
            }
            Self::CollectionSlotScope(block) => f(RirChild::Block(block)),
            Self::PatternMatch(match_) => {
                f(RirChild::Place {
                    place: &match_.subject,
                });
                for arm in &match_.arms {
                    f(RirChild::Block(&arm.block));
                }
            }
            Self::DynMatch(match_) => {
                match &match_.source {
                    RirDynMatchSource::Owned(value) => owned_value(f, value),
                    RirDynMatchSource::MutPlace(place) => f(RirChild::MutPlace { place }),
                    RirDynMatchSource::Borrowed(borrow) => match &borrow.source {
                        RirDynBorrowSource::Concrete { place, .. }
                        | RirDynBorrowSource::Owned { place, .. } => {
                            f(RirChild::MutPlace { place });
                        }
                        RirDynBorrowSource::Borrowed { local, .. }
                        | RirDynBorrowSource::Reborrowed { local, .. } => {
                            f(RirChild::LocalRead(*local));
                        }
                    },
                }
                for arm in &match_.arms {
                    f(RirChild::Block(&arm.block));
                }
                f(RirChild::Block(&match_.fallback));
            }
            Self::OptionMatch(match_) => {
                match &match_.subject {
                    RirOptionSubject::Place(place) => f(RirChild::Place { place }),
                    RirOptionSubject::MutPlace(place) => f(RirChild::MutPlace { place }),
                }
                f(RirChild::Block(&match_.some_block));
                f(RirChild::Block(&match_.none_block));
            }
            Self::MapEntryMatch(match_) => {
                f(RirChild::MutPlace { place: &match_.map });
                f(RirChild::Operand {
                    operand: &match_.key,
                });
                f(RirChild::Block(&match_.some_block));
                f(RirChild::Block(&match_.none_block));
            }
            Self::GlobalEnsure { .. } | Self::ScopedPlaceCellInit { .. } => {}
        }
    }
}

pub(super) fn stmt_child_blocks_any(
    stmt: &RirStmt,
    mut block_matches: impl FnMut(&RirStructuredBlock) -> bool,
) -> bool {
    let mut found = false;
    stmt.for_each_child(&mut |child| {
        if let RirChild::Block(block) = child {
            found |= block_matches(block);
        }
    });
    found
}

#[derive(Debug, Clone, PartialEq)]
pub struct RirCollectionLoanScope {
    pub root: RirCollectionAccess,
    pub root_kind: RirCollectionRootKind,
    pub mode: RirCollectionLoanMode,
    pub body: RirStructuredBlock,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RirCollectionRootKind {
    List,
    FixedArray,
    Slice,
    Map,
}

impl RirCollectionRootKind {
    pub(super) fn tracks_shape_loan(self) -> bool {
        matches!(self, Self::List | Self::Map)
    }

    fn accepts_mode(self, mode: RirCollectionLoanMode) -> bool {
        matches!(
            (self, mode),
            (
                Self::List | Self::FixedArray | Self::Slice,
                RirCollectionLoanMode::ReadonlySequence
                    | RirCollectionLoanMode::MutableSequenceElement,
            ) | (
                Self::Map,
                RirCollectionLoanMode::ReadonlyMap | RirCollectionLoanMode::MutableMapValue,
            )
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RirCollectionLoanMode {
    ReadonlySequence,
    MutableSequenceElement,
    ReadonlyMap,
    MutableMapValue,
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct RirStructuredBlock {
    pub stmts: Vec<RirStmt>,
    pub term: RirTerm,
}

#[derive(Debug, Clone, PartialEq)]
pub struct RirIf {
    pub cond: RirOperand,
    pub then_block: RirStructuredBlock,
    pub else_block: Option<RirStructuredBlock>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct RirLoop {
    pub id: RirLoopId,
    pub body: RirStructuredBlock,
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct RirOrdinalPlan {
    pub adapters: Vec<RirOrdinalAdapter>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum RirOrdinalAdapter {
    Rev,
    Skip { count: RirOperand },
    Take { count: RirOperand },
    StepBy { step: RirOperand },
}

#[derive(Debug, Clone, PartialEq)]
pub struct RirRangeFor {
    pub id: RirLoopId,
    pub start: RirOperand,
    pub end: RirOperand,
    pub ordinal_plan: RirOrdinalPlan,
    pub inclusive: bool,
    pub ordinal: Option<RirLocalId>,
    pub item: RirLocalId,
    pub body: RirStructuredBlock,
}

#[derive(Debug, Clone, PartialEq)]
pub struct RirCollectionFor {
    pub id: RirLoopId,
    pub len: RirLocalId,
    pub ordinal_plan: RirOrdinalPlan,
    pub index: RirLocalId,
    pub ordinal: Option<RirLocalId>,
    pub body: RirStructuredBlock,
}

impl RirOrdinalPlan {
    pub fn operands(&self) -> impl Iterator<Item = &RirOperand> {
        self.adapters.iter().filter_map(|adapter| match adapter {
            RirOrdinalAdapter::Rev => None,
            RirOrdinalAdapter::Skip { count }
            | RirOrdinalAdapter::Take { count }
            | RirOrdinalAdapter::StepBy { step: count } => Some(count),
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct RirDynMatch {
    pub carrier: RirDynCarrierId,
    pub source: RirDynMatchSource,
    pub arms: Vec<RirDynMatchArm>,
    pub mappings: Vec<RirDynMatchMapping>,
    pub fallback_binding: RirDynMatchFallbackBinding,
    pub fallback: RirStructuredBlock,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RirDynMatchMapping {
    pub carrier: RirDynCarrierId,
    pub variants: Vec<RirDynVariantSetId>,
    pub targets: Vec<Option<usize>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum RirDynMatchSource {
    Owned(RirOwnedValue),
    MutPlace(RirMutPlaceArg),
    Borrowed(RirDynBorrow),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RirDynMatchBinding {
    Discard,
    Take(RirLocalId),
    Materialize {
        local: RirLocalId,
        materializer: RirMaterializerId,
    },
    Alias(RirLocalId),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RirDynMatchFallbackBinding {
    Discard,
    Take(RirLocalId),
    Preserve(RirLocalId),
    Alias(RirLocalId),
}

impl RirDynMatchFallbackBinding {
    pub fn local(self) -> Option<RirLocalId> {
        match self {
            Self::Discard => None,
            Self::Take(local) | Self::Preserve(local) | Self::Alias(local) => Some(local),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct RirDynMatchArm {
    pub binding: RirDynMatchBinding,
    pub block: RirStructuredBlock,
}

#[derive(Debug, Clone, PartialEq)]
pub struct RirOptionMatch {
    pub subject: RirOptionSubject,
    pub payload: Option<RirOptionPayloadBinding>,
    pub some_block: RirStructuredBlock,
    pub none_block: RirStructuredBlock,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RirOptionPayloadBinding {
    Owned {
        local: RirLocalId,
        materializer: RirMaterializerId,
    },
    Ref {
        local: RirLocalId,
        escapes: bool,
    },
}

impl RirOptionPayloadBinding {
    pub fn local(self) -> RirLocalId {
        match self {
            Self::Owned { local, .. } | Self::Ref { local, .. } => local,
        }
    }

    pub fn is_ref(self) -> bool {
        matches!(self, Self::Ref { .. })
    }

    pub fn escapes(self) -> bool {
        matches!(self, Self::Ref { escapes: true, .. })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct RirMapEntryMatch {
    pub map: RirMutPlaceArg,
    pub key: RirOperand,
    pub payload: Option<RirLocalId>,
    pub payload_escapes: bool,
    pub some_block: RirStructuredBlock,
    pub none_block: RirStructuredBlock,
}

#[derive(Debug, Clone, PartialEq)]
pub struct RirPatternMatch {
    pub subject: RirPlace,
    pub arms: Vec<RirPatternArm>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct RirPatternArm {
    pub alternatives: Vec<RirPatternAlternative>,
    pub block: RirStructuredBlock,
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct RirPatternAlternative {
    pub tests: Vec<RirPatternTest>,
    pub bindings: Vec<RirPatternBinding>,
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct RirPatternPath {
    pub steps: Vec<RirPatternPathStep>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RirPatternPathStep {
    Place(RirPlaceStep),
    OptionalSome {
        source_ty: RirTypeId,
        target_ty: RirTypeId,
    },
    EnumTupleField {
        source_ty: RirTypeId,
        target_ty: RirTypeId,
        enum_id: RirEnumId,
        variant: RirVariantId,
        field: u16,
    },
    EnumStructField {
        source_ty: RirTypeId,
        target_ty: RirTypeId,
        enum_id: RirEnumId,
        variant: RirVariantId,
        field: u16,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RirPatternTest {
    Any {
        branches: Vec<Vec<RirPatternTest>>,
    },
    Literal {
        path: RirPatternPath,
        value: RirConstId,
    },
    Nil {
        path: RirPatternPath,
    },
    OptionalSome {
        path: RirPatternPath,
    },
    EnumVariant {
        path: RirPatternPath,
        enum_id: RirEnumId,
        variant: RirVariantId,
    },
    FlagValue {
        path: RirPatternPath,
        flag: RirFlagId,
        bits: i64,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RirPatternBinding {
    pub local: RirLocalId,
    pub path: RirPatternPath,
    pub ty: RirTypeId,
    pub mode: RirPatternBindingMode,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RirPatternBindingMode {
    Owned { materializer: RirMaterializerId },
    Alias,
}

impl RirPatternBindingMode {
    pub fn is_alias(self) -> bool {
        matches!(self, Self::Alias)
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
struct RirPatternBindingSignature {
    local: RirLocalId,
    ty: RirTypeId,
    mode: RirPatternBindingMode,
}

impl RirPatternBindingSignature {
    fn sort_key(self) -> (usize, usize, u8, usize) {
        let (mode, materializer) = match self.mode {
            RirPatternBindingMode::Owned { materializer } => (0, materializer.index()),
            RirPatternBindingMode::Alias => (1, 0),
        };
        (self.local.index(), self.ty.index(), mode, materializer)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum RirOptionSubject {
    Place(RirPlace),
    MutPlace(RirMutPlaceArg),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RirIterCountCheck {
    SkipNonNegative,
    TakeNonNegative,
    StepByPositive,
}

#[derive(Debug, Clone, PartialEq)]
pub enum RirDynReceiver {
    Owned(RirOwnedValue),
    MutPlace(RirMutPlaceArg),
    Borrowed(RirDynBorrow),
}

#[derive(Debug, Clone, Copy)]
pub enum RirChild<'a> {
    Operand { operand: &'a RirOperand },
    Place { place: &'a RirPlace },
    MutPlace { place: &'a RirMutPlaceArg },
    Collection { collection: &'a RirCollectionAccess },
    CallArg(&'a RirCallArg),
    CaptureArg(&'a RirLambdaCaptureArg),
    LocalRead(RirLocalId),
    Block(&'a RirStructuredBlock),
}

fn operand<'a>(f: &mut impl FnMut(RirChild<'a>), operand: &'a RirOperand) {
    f(RirChild::Operand { operand });
}

fn place<'a>(f: &mut impl FnMut(RirChild<'a>), place: &'a RirPlace) {
    f(RirChild::Place { place });
}

fn dyn_borrow<'a>(f: &mut impl FnMut(RirChild<'a>), borrow: &'a RirDynBorrow) {
    match &borrow.source {
        RirDynBorrowSource::Concrete { place, .. } | RirDynBorrowSource::Owned { place, .. } => {
            f(RirChild::MutPlace { place });
        }
        RirDynBorrowSource::Borrowed { local, .. }
        | RirDynBorrowSource::Reborrowed { local, .. } => f(RirChild::LocalRead(*local)),
    }
}

fn owned_operand<'a>(f: &mut impl FnMut(RirChild<'a>), owned: &'a RirOwnedOperand) {
    match owned {
        RirOwnedOperand::Value(value) => operand(f, value),
        RirOwnedOperand::Access(place) => f(RirChild::MutPlace { place }),
        RirOwnedOperand::DynBorrow(borrow) => dyn_borrow(f, borrow),
    }
}

fn owned_value<'a>(f: &mut impl FnMut(RirChild<'a>), owned: &'a RirOwnedValue) {
    owned_operand(f, &owned.value);
}

fn collection<'a>(f: &mut impl FnMut(RirChild<'a>), collection: &'a RirCollectionAccess) {
    f(RirChild::Collection { collection });
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RirFlagStaticOp {
    Empty,
    All,
}

#[derive(Debug, Clone, PartialEq)]
pub struct RirOwnedValue {
    pub value: RirOwnedOperand,
    pub source: RirOwnedSource,
}

#[derive(Debug, Clone, PartialEq)]
pub enum RirOwnedOperand {
    Value(RirOperand),
    Access(RirMutPlaceArg),
    DynBorrow(RirDynBorrow),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RirOwnedSource {
    Direct,
    Reuse(RirMaterializerId),
    Transfer { local: RirLocalId },
}

#[derive(Debug, Clone, PartialEq)]
pub enum RirRValue {
    Use(RirOperand),
    TakeStaged(RirOperand),
    Materialize(RirOwnedValue),
    DynPack {
        variant: RirDynVariantId,
        value: RirOwnedValue,
        ty: RirTypeId,
    },
    DynWeaken {
        weakening: RirDynWeakeningId,
        value: RirOwnedValue,
        ty: RirTypeId,
    },
    DynDowncast {
        variants: RirDynVariantSetId,
        value: RirOwnedValue,
        ty: RirTypeId,
    },
    DynCall {
        dispatch: RirDynDispatchId,
        exact_variant: Option<RirDynVariantId>,
        receiver: RirDynReceiver,
        args: Vec<RirCallArg>,
        ty: RirTypeId,
    },
    FunctionValue {
        value: RirOwnedValue,
        escape: Option<RirLambdaEscape>,
        ty: RirTypeId,
    },
    Struct {
        ty: RirTypeId,
        fields: Vec<RirOwnedValue>,
    },
    Tuple {
        ty: RirTypeId,
        fields: Vec<RirOwnedValue>,
    },
    DataRefAlloc {
        ty: RirTypeId,
        fields: Vec<RirOwnedValue>,
    },
    Array {
        ty: RirTypeId,
        elems: Vec<RirOwnedValue>,
    },
    List {
        ty: RirTypeId,
        elems: Vec<RirOwnedValue>,
    },
    Map {
        ty: RirTypeId,
        entries: Vec<(RirOwnedValue, RirOwnedValue)>,
    },
    EnumVariant {
        ty: RirTypeId,
        variant: RirVariantId,
        fields: Vec<RirOwnedValue>,
    },
    Unary {
        op: UnaryOp,
        value: RirOperand,
        ty: RirTypeId,
    },
    Binary {
        op: BinaryOp,
        lhs: RirOperand,
        rhs: RirOperand,
        ty: RirTypeId,
    },
    SharedRefEq {
        lhs: RirOperand,
        rhs: RirOperand,
        negated: bool,
    },
    Cast {
        value: RirOperand,
        target: RirTypeId,
    },
    RawProject {
        value: RirOperand,
        target: RirTypeId,
    },
    RawTryConstruct {
        value: RirOperand,
        target: RirTypeId,
        ty: RirTypeId,
    },
    FlagStatic {
        op: RirFlagStaticOp,
        ty: RirTypeId,
    },
    OptionalSome {
        value: RirOwnedValue,
        ty: RirTypeId,
    },
    Call {
        callee: RirCallTarget,
        args: Vec<RirCallArg>,
        ty: RirTypeId,
    },
    Stringify {
        value: RirOperand,
        source_ty: RirTypeId,
    },
    StringConcat {
        parts: Vec<RirOperand>,
    },
    Format {
        value: RirOperand,
        source_ty: RirTypeId,
        spec: FormatSpec,
    },
    Len {
        source: RirPlace,
    },
    CollectionLen {
        source: RirCollectionAccess,
    },
    SequenceSlotAt {
        collection: RirCollectionAccess,
        step: RirPlaceStep,
    },
    ListPush {
        list: RirCollectionAccess,
        value: RirOwnedValue,
    },
    SliceView {
        source: RirCollectionAccess,
        start: RirLocalId,
        end: RirLocalId,
        inclusive: bool,
        mutable: bool,
        ty: RirTypeId,
    },
    RangeListCopy {
        source: RirPlace,
        start: RirLocalId,
        end: RirLocalId,
        inclusive: bool,
        ty: RirTypeId,
    },
    MapGet {
        map: RirCollectionAccess,
        key: RirOperand,
        ty: RirTypeId,
    },
    MapInsert {
        map: RirCollectionAccess,
        key: RirOwnedValue,
        value: RirOwnedValue,
        kind: RirMapWriteKind,
    },
    MapRemove {
        map: RirCollectionAccess,
        key: RirOperand,
        ty: RirTypeId,
    },
    CheckedIterCount {
        count: RirOperand,
        check: RirIterCountCheck,
    },
    MapEntryAt {
        map: RirCollectionAccess,
        index: RirLocalId,
        ty: RirTypeId,
    },
    MapKeyAt {
        map: RirCollectionAccess,
        index: RirLocalId,
        ty: RirTypeId,
    },
    MapValueAt {
        map: RirCollectionAccess,
        index: RirLocalId,
        ty: RirTypeId,
    },
    Lambda {
        lambda: RirLambdaId,
        captures: Vec<RirLambdaCaptureArg>,
        ty: RirTypeId,
    },
}

impl RirRValue {
    pub fn for_each_owned_value(&self, f: &mut impl FnMut(&RirOwnedValue)) {
        match self {
            Self::Materialize(owned)
            | Self::DynPack { value: owned, .. }
            | Self::DynWeaken { value: owned, .. }
            | Self::DynDowncast { value: owned, .. }
            | Self::FunctionValue { value: owned, .. }
            | Self::OptionalSome { value: owned, .. }
            | Self::ListPush { value: owned, .. } => f(owned),
            Self::Struct { fields, .. }
            | Self::Tuple { fields, .. }
            | Self::DataRefAlloc { fields, .. }
            | Self::Array { elems: fields, .. }
            | Self::List { elems: fields, .. }
            | Self::EnumVariant { fields, .. } => fields.iter().for_each(f),
            Self::Map { entries, .. } => {
                for (key, value) in entries {
                    f(key);
                    f(value);
                }
            }
            Self::MapInsert { key, value, .. } => {
                f(key);
                f(value);
            }
            Self::Call { args, .. } => {
                for arg in args {
                    arg.for_each_owned_value(f);
                }
            }
            Self::DynCall { receiver, args, .. } => {
                if let RirDynReceiver::Owned(owned) = receiver {
                    f(owned);
                }
                for arg in args {
                    arg.for_each_owned_value(f);
                }
            }
            Self::Lambda { captures, .. } => {
                for capture in captures {
                    if let RirLambdaCaptureArg::Owned { value } = capture {
                        f(value);
                    }
                }
            }
            _ => {}
        }
    }

    pub fn for_each_child(&self, f: &mut impl FnMut(RirChild<'_>)) {
        match self {
            Self::Materialize(owned)
            | Self::DynPack { value: owned, .. }
            | Self::DynWeaken { value: owned, .. }
            | Self::DynDowncast { value: owned, .. }
            | Self::FunctionValue { value: owned, .. } => owned_value(f, owned),
            Self::DynCall { receiver, args, .. } => {
                match receiver {
                    RirDynReceiver::Owned(value) => owned_value(f, value),
                    RirDynReceiver::MutPlace(value) => f(RirChild::MutPlace { place: value }),
                    RirDynReceiver::Borrowed(borrow) => match &borrow.source {
                        RirDynBorrowSource::Concrete { place, .. }
                        | RirDynBorrowSource::Owned { place, .. } => {
                            f(RirChild::MutPlace { place });
                        }
                        RirDynBorrowSource::Borrowed { local, .. }
                        | RirDynBorrowSource::Reborrowed { local, .. } => {
                            f(RirChild::LocalRead(*local));
                        }
                    },
                }
                for arg in args {
                    f(RirChild::CallArg(arg));
                }
            }
            Self::Struct { fields, .. }
            | Self::Tuple { fields, .. }
            | Self::DataRefAlloc { fields, .. }
            | Self::Array { elems: fields, .. }
            | Self::List { elems: fields, .. }
            | Self::EnumVariant { fields, .. } => {
                for field in fields {
                    owned_value(f, field);
                }
            }
            Self::FlagStatic { .. } => {}
            Self::Map { entries, .. } => {
                for (key, value) in entries {
                    owned_value(f, key);
                    owned_value(f, value);
                }
            }
            Self::Use(value)
            | Self::TakeStaged(value)
            | Self::Unary { value, .. }
            | Self::Cast { value, .. }
            | Self::RawProject { value, .. }
            | Self::RawTryConstruct { value, .. }
            | Self::Stringify { value, .. }
            | Self::Format { value, .. } => operand(f, value),
            Self::OptionalSome { value, .. } => owned_value(f, value),
            Self::Binary { lhs, rhs, .. } | Self::SharedRefEq { lhs, rhs, .. } => {
                operand(f, lhs);
                operand(f, rhs);
            }
            Self::Call { callee, args, .. } => {
                if let RirCallTarget::LambdaValue { callee, .. } = callee {
                    operand(f, callee);
                }
                for arg in args {
                    f(RirChild::CallArg(arg));
                }
            }
            Self::StringConcat { parts } => {
                for part in parts {
                    operand(f, part);
                }
            }
            Self::Len { source } => place(f, source),
            Self::CollectionLen { source } => collection(f, source),
            Self::SequenceSlotAt {
                collection: source,
                step,
            } => {
                collection(f, source);
                f(RirChild::LocalRead(
                    step.index_local().expect("sequence slot step"),
                ));
            }
            Self::MapEntryAt {
                map: source, index, ..
            }
            | Self::MapKeyAt {
                map: source, index, ..
            }
            | Self::MapValueAt {
                map: source, index, ..
            } => {
                collection(f, source);
                f(RirChild::LocalRead(*index));
            }
            Self::ListPush { list, value } => {
                collection(f, list);
                owned_value(f, value);
            }
            Self::SliceView {
                source, start, end, ..
            } => {
                collection(f, source);
                f(RirChild::LocalRead(*start));
                f(RirChild::LocalRead(*end));
            }
            Self::RangeListCopy {
                source, start, end, ..
            } => {
                place(f, source);
                f(RirChild::LocalRead(*start));
                f(RirChild::LocalRead(*end));
            }
            Self::MapGet { map, key, .. } | Self::MapRemove { map, key, .. } => {
                collection(f, map);
                operand(f, key);
            }
            Self::MapInsert {
                map, key, value, ..
            } => {
                collection(f, map);
                owned_value(f, key);
                owned_value(f, value);
            }
            Self::CheckedIterCount { count, .. } => operand(f, count),
            Self::Lambda { captures, .. } => {
                for capture in captures {
                    f(RirChild::CaptureArg(capture));
                }
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum RirLambdaCaptureArg {
    Owned { value: RirOwnedValue },
    Shared { place: RirPlace },
    StackCell { cell: RirCellRef },
    HeapCell { cell: RirCellRef },
    ScopedPlaceCell { cell: RirScopedPlaceCellRef },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RirCallTarget {
    Function(RirFunctionId),
    Extern(RirExternId),
    LambdaValue {
        callee: RirOperand,
        sig: RirLambdaSigId,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RirDynBorrow {
    pub source: RirDynBorrowSource,
    pub target: RirDynCarrierId,
    pub weakening: Option<RirDynWeakeningId>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RirDynBorrowSource {
    Concrete {
        place: RirMutPlaceArg,
        variant: RirDynVariantId,
    },
    Owned {
        place: RirMutPlaceArg,
        carrier: RirDynCarrierId,
    },
    Borrowed {
        local: RirLocalId,
        carrier: RirDynCarrierId,
    },
    Reborrowed {
        local: RirLocalId,
        carrier: RirDynCarrierId,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum RirCallArg {
    Value(RirOwnedValue),
    InitFieldProvided(RirOwnedValue),
    InitFieldOmitted,
    SharedBorrow(RirPlace),
    SharedStringConst(RirStringLiteralId),
    MutBorrow(RirPlace),
    MutPlace(RirMutPlaceArg),
    DynBorrow(RirDynBorrow),
    ScopedLambda {
        callee: RirOwnedValue,
        sig: RirLambdaSigId,
    },
    EscapingLambda {
        callee: RirOwnedValue,
        sig: RirLambdaSigId,
    },
    AnvCallback {
        callee: RirOwnedValue,
        sig: RirLambdaSigId,
    },
}

impl RirCallArg {
    pub fn for_each_owned_value(&self, f: &mut impl FnMut(&RirOwnedValue)) {
        match self {
            Self::Value(value)
            | Self::InitFieldProvided(value)
            | Self::ScopedLambda { callee: value, .. }
            | Self::EscapingLambda { callee: value, .. }
            | Self::AnvCallback { callee: value, .. } => f(value),
            Self::InitFieldOmitted
            | Self::SharedBorrow(_)
            | Self::SharedStringConst(_)
            | Self::MutBorrow(_)
            | Self::MutPlace(_)
            | Self::DynBorrow(_) => {}
        }
    }

    pub fn mode(&self) -> RirPassMode {
        match self {
            Self::Value(_) | Self::InitFieldProvided(_) | Self::InitFieldOmitted => {
                RirPassMode::Value
            }
            Self::SharedBorrow(_) | Self::SharedStringConst(_) => RirPassMode::SharedBorrow,
            Self::MutBorrow(_) => RirPassMode::MutBorrow,
            Self::MutPlace(_) => RirPassMode::MutPlace,
            Self::DynBorrow(_) => RirPassMode::DynBorrow,
            Self::ScopedLambda { .. } => RirPassMode::ScopedLambda,
            Self::EscapingLambda { .. } => RirPassMode::EscapingLambda,
            Self::AnvCallback { .. } => RirPassMode::AnvCallback,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RirMutPlaceArg {
    pub access: RirMutPlaceAccess,
    pub projections: Vec<RirPlaceStep>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RirMutPlaceAccess {
    Handle(RirMutPlaceHandle),
    DataRef {
        object: RirOperand,
        place: RirDataRefPlaceId,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RirMutPlaceHandle {
    Local { local: RirLocalId },
    Param { local: RirLocalId },
    StackCell { cell: RirCellRef },
    HeapCell { cell: RirCellRef },
    ScopedPlaceCell { cell: RirScopedPlaceCellRef },
    Global { global: RirGlobalId },
}

impl RirMutPlaceHandle {
    pub fn local(&self) -> Option<RirLocalId> {
        match self {
            Self::Local { local, .. } | Self::Param { local, .. } => Some(*local),
            Self::StackCell { .. }
            | Self::HeapCell { .. }
            | Self::ScopedPlaceCell { .. }
            | Self::Global { .. } => None,
        }
    }
}

impl RirMutPlaceArg {
    pub fn from_handle(handle: RirMutPlaceHandle, projections: Vec<RirPlaceStep>) -> Self {
        Self {
            access: RirMutPlaceAccess::Handle(handle),
            projections,
        }
    }

    pub fn final_ty(&self, root_ty: RirTypeId) -> RirTypeId {
        self.projections
            .last()
            .map_or(root_ty, |step| step.target_ty)
    }

    pub fn root_local(&self) -> Option<RirLocalId> {
        match &self.access {
            RirMutPlaceAccess::Handle(handle) => handle.local(),
            RirMutPlaceAccess::DataRef { .. } => None,
        }
    }

    pub fn uses_local(&self, local: RirLocalId) -> bool {
        self.root_local() == Some(local)
            || self
                .projections
                .iter()
                .any(|projection| projection.index_local() == Some(local))
            || matches!(&self.access, RirMutPlaceAccess::DataRef { object: RirOperand::Place(place), .. } if place.uses_local(local))
    }

    pub fn dataref(
        object: RirOperand,
        place: RirDataRefPlaceId,
        projections: Vec<RirPlaceStep>,
    ) -> Self {
        Self {
            access: RirMutPlaceAccess::DataRef { object, place },
            projections,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RirPlaceRoot {
    Local(RirLocalId),
    Global(RirGlobalId),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RirPlace {
    pub root: RirPlaceRoot,
    pub projections: Vec<RirPlaceStep>,
}

impl RirPlace {
    pub fn local(local: RirLocalId, projections: Vec<RirPlaceStep>) -> Self {
        Self {
            root: RirPlaceRoot::Local(local),
            projections,
        }
    }

    pub fn global(global: RirGlobalId, projections: Vec<RirPlaceStep>) -> Self {
        Self {
            root: RirPlaceRoot::Global(global),
            projections,
        }
    }

    pub fn uses_local(&self, local: RirLocalId) -> bool {
        self.root == RirPlaceRoot::Local(local)
            || self
                .projections
                .iter()
                .any(|projection| projection.index_local() == Some(local))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RirCollectionAccess {
    Direct(RirPlace),
    MutPlace(RirMutPlaceArg),
}

impl RirCollectionAccess {
    pub fn uses_local(&self, local: RirLocalId) -> bool {
        match self {
            Self::Direct(place) => place.uses_local(local),
            Self::MutPlace(place) => place.uses_local(local),
        }
    }

    fn is_direct_global(&self, global: RirGlobalId) -> bool {
        matches!(
            self,
            Self::Direct(place) if place.root == RirPlaceRoot::Global(global)
        )
    }

    fn replaced_by_place(&self, dst: &RirPlace) -> bool {
        let Self::Direct(root) = self else {
            return false;
        };
        dst.root == root.root
            && dst.projections.len() <= root.projections.len()
            && dst
                .projections
                .iter()
                .zip(&root.projections)
                .all(|(dst, root)| dst == root)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct RirPlaceStep {
    pub source_ty: RirTypeId,
    pub target_ty: RirTypeId,
    pub kind: RirPlaceStepKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum RirPlaceStepKind {
    StructField(RirFieldId),
    DataRefField(RirFieldId),
    ExternField(RirFieldId),
    TupleField(RirFieldId),
    ArrayIndex {
        index: RirLocalId,
        len: u64,
        elem_materializer: RirMaterializerId,
    },
    ListIndex {
        index: RirLocalId,
        elem_materializer: RirMaterializerId,
    },
    SliceIndex {
        index: RirLocalId,
        elem_materializer: RirMaterializerId,
    },
}

impl RirPlaceStep {
    pub fn index_local(self) -> Option<RirLocalId> {
        match self.kind {
            RirPlaceStepKind::ArrayIndex { index, .. }
            | RirPlaceStepKind::ListIndex { index, .. }
            | RirPlaceStepKind::SliceIndex { index, .. } => Some(index),
            RirPlaceStepKind::StructField(_)
            | RirPlaceStepKind::DataRefField(_)
            | RirPlaceStepKind::ExternField(_)
            | RirPlaceStepKind::TupleField(_) => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RirOperand {
    Place(RirPlace),
    Const(RirConstId),
}

#[derive(Debug, Clone, PartialEq, Default)]
pub enum RirTerm {
    #[default]
    None,
    Return(Option<RirOperand>),
    ReturnOwned(RirOwnedValue),
    Break(RirLoopId),
    Continue(RirLoopId),
    Unreachable,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RirType {
    Int,
    Float,
    Bool,
    String,
    Char,
    Void,
    Struct(RirStructId),
    DataRef(RirDataRefId),
    Enum(RirEnumId),
    Flag(RirFlagId),
    Tuple(RirTupleId),
    Array { elem: RirTypeId, len: u64 },
    List(RirTypeId),
    Map { key: RirTypeId, value: RirTypeId },
    Option(RirTypeId),
    Slice(RirTypeId),
    Lambda(RirLambdaSigId),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RirMapWriteKind {
    IndexedAssignment,
    StructuralInsert,
}

#[derive(Debug, Clone, PartialEq)]
pub struct RirConst {
    pub id: RirConstId,
    pub ty: RirTypeId,
    pub value: RirConstValue,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RirStringLiteral {
    pub id: RirStringLiteralId,
    pub text: String,
    pub needs_owned: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub enum RirConstValue {
    Int(i64),
    Flag { flag: RirFlagId, bits: i64 },
    Float(f64),
    Bool(bool),
    String(RirStringLiteralId),
    Char(char),
    Nil,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RirNativeParam {
    Value,
    SharedNamed,
    OwnedNamed,
    Borrow,
    MutBorrow,
    MutPlace,
    ScopedLambda,
    EscapingLambda,
    AnvCallback,
    InitField(Box<RirNativeParam>),
    Option(Box<RirNativeParam>),
    Result(Box<RirNativeParam>, Box<RirNativeParam>),
    Array(Box<RirNativeParam>),
    Slice(Box<RirNativeParam>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RirNativeReturn {
    Void,
    Value,
    SharedNamed,
    OwnedNamed {
        adopt: bool,
    },
    Option {
        payload_ty: RirTypeId,
        payload: Box<RirNativeReturn>,
    },
    Result {
        ok_ty: RirTypeId,
        ok: Box<RirNativeReturn>,
        err_ty: RirTypeId,
        err: Box<RirNativeReturn>,
    },
    Array {
        elem_ty: RirTypeId,
        elem: Box<RirNativeReturn>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct RirExtern {
    pub id: RirExternId,
    pub path: RustPath,
    pub params: Vec<RirExternParam>,
    pub ret: RirTypeId,
    pub ret_plan: RirNativeReturn,
    pub callback_receiver: Option<usize>,
    pub ctx: RustCallContext,
    pub fallible: bool,
    pub suspends_runtime_entry: bool,
}

impl RirExtern {
    pub(super) fn arg_action(
        &self,
        index: usize,
        arg: &RirCallArg,
    ) -> native_call::NativeArgAction {
        let action = self.params[index].action;
        match (action, arg) {
            (native_call::NativeArgAction::SnapshotString, RirCallArg::SharedStringConst(_))
            | (native_call::NativeArgAction::RejectLiveBoundary, RirCallArg::InitFieldOmitted) => {
                native_call::NativeArgAction::Direct
            }
            _ => action,
        }
    }

    pub(super) fn rejects_reentry_arg(&self, index: usize, arg: &RirCallArg) -> bool {
        self.arg_action(index, arg) == native_call::NativeArgAction::RejectLiveBoundary
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RirExternParam {
    pub ty: RirTypeId,
    pub mode: RirPassMode,
    pub escape: RirParamEscape,
    pub plan: RirNativeParam,
    pub(super) action: native_call::NativeArgAction,
}

pub(super) fn verify(program: &RirProgram) -> Result<(), Vec<RirVerifyError>> {
    let mut cx = VerifyCx::new(program);
    cx.check();
    if !super::exact_witness::is_canonical(program) {
        cx.errors.push(RirVerifyError {
            site: RirVerifySite::Program,
            kind: RirVerifyErrorKind::InvalidExactWitness,
        });
    }
    if cx.errors.is_empty() {
        Ok(())
    } else {
        Err(cx.errors)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RirVerifyError {
    pub site: RirVerifySite,
    pub kind: RirVerifyErrorKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RirVerifySite {
    Program,
    Global(RirGlobalId),
    Type(RirTypeId),
    Const(RirConstId),
    StringLiteral(RirStringLiteralId),
    Extern(RirExternId),
    Function(RirFunctionId),
    Cell(RirCellId),
    ScopedPlaceCell(RirScopedPlaceCellId),
    Param(RirFunctionId, usize),
    Local(RirFunctionId, RirLocalId),
    Statement(RirFunctionId, usize),
    RValue(RirFunctionId, usize),
    CallArg(RirFunctionId, usize, usize),
    Terminator(RirFunctionId),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum StorageProjectionMode {
    Ordinary,
    MutPlace,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum MutPlaceUse {
    ReadCopy,
    OwnedRead,
    SliceViewRead,
    Write,
    CallArg,
    ScopedPlaceSource,
    CollectionMutation,
    MapRead,
    IndexedMapAssignment,
}

impl MutPlaceUse {
    fn allow_dataref(self) -> bool {
        matches!(
            self,
            Self::ReadCopy
                | Self::OwnedRead
                | Self::SliceViewRead
                | Self::CallArg
                | Self::CollectionMutation
                | Self::MapRead
                | Self::IndexedMapAssignment
        )
    }

    fn allow_cell_collection_projection(self) -> bool {
        matches!(self, Self::ReadCopy | Self::OwnedRead | Self::SliceViewRead)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RirVerifyErrorKind {
    BadId,
    TypeMismatch {
        expected: RirTypeId,
        found: RirTypeId,
    },
    ConstTypeMismatch,
    VoidConst,
    DuplicateSymbol,
    DuplicateStringLiteral,
    StringLiteralOwnershipMismatch,
    DuplicateStringifyReq,
    InvalidStringifyHelperReferenceCount,
    ReturnValueRequired,
    UnexpectedReturnValue,
    ImmutableAssign,
    ParamLocalMissing,
    ParamLocalNotInitialized,
    InitParamLocal,
    UninitializedLocal(RirLocalId),
    UninitializedGlobal(RirGlobalId),
    UninitializedCell(RirCellId),
    InitCellTwice(RirCellId),
    UnsupportedAbi,
    UnsupportedRValueType,
    InvalidNumericCast,
    InvalidRawProject,
    InvalidRawTryConstruct,
    InvalidFlagStatic,
    InvalidDynPlan,
    InvalidDynCarrier,
    InvalidDynVariant,
    InvalidDynStorage,
    InvalidDynDispatch,
    InvalidExactWitness,
    UnsupportedLambdaCapture,
    UnsupportedLambdaCell,
    ImmutableCellSourceLocal(RirLocalId),
    CellSourcePayloadBinding(RirLocalId),
    DuplicateCell {
        owner: RirFunctionId,
        source_local: RirLocalId,
        first: RirCellId,
        second: RirCellId,
    },
    DuplicateScopedPlaceCell {
        owner: RirFunctionId,
        source_local: RirLocalId,
        first: RirScopedPlaceCellId,
        second: RirScopedPlaceCellId,
    },
    RawEnumMissingRawType,
    RawEnumWrongRawType,
    RawEnumMissingValue,
    RawEnumWrongValue,
    RawEnumPayload,
    RawEnumDuplicateValue,
    FlagInvalidKnownBits,
    FlagInvalidValue,
    FlagUnknownBits,
    FlagPatternTypeMismatch,
    FlagPatternUnknownBits,
    FlagAtomicMismatch,
    FlagDuplicateValue,
    NonCopyValueRequired,
    FieldCount {
        expected: usize,
        found: usize,
    },
    CallArgCount {
        expected: usize,
        found: usize,
    },
    CallArgMode,
    CallArgEscape,
    LambdaEscapeProofMismatch,
    PatternAlternativeRequired,
    PatternBindingMismatch,
    PatternPayloadWithoutVariantTest,
    OptionPayloadEscapeRequiresPayload,
    OptionPayloadEscapeNoneMustDiverge,
    OptionPayloadRefLocalMismatch,
    OptionPayloadRefDiscriminantMustBeMutable,
    PayloadBindingWithoutProducer,
    InitPayloadRefLocal,
    BreakOutsideLoop(RirLoopId),
    ContinueOutsideLoop(RirLoopId),
}

impl fmt::Display for RirVerifyError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}: {:?}", self.site, self.kind)
    }
}

impl Error for RirVerifyError {}

#[derive(Clone, PartialEq, Eq)]
enum RirFunctionValueState {
    Unknown,
    NonFunction,
    Lambda(Option<RirLambdaEscape>),
    Fields(Vec<RirFunctionValueState>),
    Variants(Vec<Vec<RirFunctionValueState>>),
}

impl RirFunctionValueState {
    fn join(left: Self, right: Self) -> Self {
        match (left, right) {
            (Self::Lambda(left), Self::Lambda(right)) if left == right => Self::Lambda(left),
            (Self::NonFunction, Self::NonFunction) => Self::NonFunction,
            (Self::Fields(left), Self::Fields(right)) if left.len() == right.len() => Self::Fields(
                left.into_iter()
                    .zip(right)
                    .map(|(left, right)| Self::join(left, right))
                    .collect(),
            ),
            (Self::Variants(left), Self::Variants(right)) if left.len() == right.len() => {
                if left
                    .iter()
                    .zip(&right)
                    .any(|(left, right)| left.len() != right.len())
                {
                    Self::Unknown
                } else {
                    Self::Variants(
                        left.into_iter()
                            .zip(right)
                            .map(|(left, right)| {
                                left.into_iter()
                                    .zip(right)
                                    .map(|(left, right)| Self::join(left, right))
                                    .collect()
                            })
                            .collect(),
                    )
                }
            }
            _ => Self::Unknown,
        }
    }

    fn escape(&self) -> Option<RirLambdaEscape> {
        match self {
            Self::Lambda(escape) => *escape,
            Self::Unknown | Self::NonFunction | Self::Fields(_) | Self::Variants(_) => None,
        }
    }

    fn project_field(&self, field: RirFieldId) -> Self {
        let Self::Fields(fields) = self else {
            return Self::Unknown;
        };
        fields.get(field.index()).cloned().unwrap_or(Self::Unknown)
    }

    fn project(&self, projection: RirPlaceStep) -> Self {
        match projection.kind {
            RirPlaceStepKind::StructField(field)
            | RirPlaceStepKind::DataRefField(field)
            | RirPlaceStepKind::ExternField(field)
            | RirPlaceStepKind::TupleField(field) => self.project_field(field),
            RirPlaceStepKind::ArrayIndex { .. }
            | RirPlaceStepKind::ListIndex { .. }
            | RirPlaceStepKind::SliceIndex { .. } => match self {
                Self::Fields(fields) => fields
                    .iter()
                    .cloned()
                    .reduce(Self::join)
                    .unwrap_or(Self::Unknown),
                _ => Self::Unknown,
            },
        }
    }

    fn variant_field(&self, variant: RirVariantId, field: u16) -> Self {
        let Self::Variants(variants) = self else {
            return Self::Unknown;
        };
        variants
            .get(variant.index())
            .and_then(|fields| fields.get(field as usize))
            .cloned()
            .unwrap_or(Self::Unknown)
    }

    fn projection_mut(&mut self, projections: &[RirPlaceStep]) -> Option<&mut Self> {
        let Some((first, rest)) = projections.split_first() else {
            return Some(self);
        };
        let Self::Fields(fields) = self else {
            return None;
        };
        let index = match first.kind {
            RirPlaceStepKind::StructField(field)
            | RirPlaceStepKind::DataRefField(field)
            | RirPlaceStepKind::ExternField(field)
            | RirPlaceStepKind::TupleField(field) => field.index(),
            RirPlaceStepKind::ArrayIndex { .. }
            | RirPlaceStepKind::ListIndex { .. }
            | RirPlaceStepKind::SliceIndex { .. } => return None,
        };
        fields.get_mut(index)?.projection_mut(rest)
    }

    fn assign_projection(&mut self, projections: &[RirPlaceStep], value: Self) -> bool {
        let Some(slot) = self.projection_mut(projections) else {
            return false;
        };
        *slot = value;
        true
    }

    fn push_projection(&mut self, projections: &[RirPlaceStep], value: Self) -> bool {
        let Some(Self::Fields(fields)) = self.projection_mut(projections) else {
            return false;
        };
        fields.push(value);
        true
    }
}

#[derive(Clone)]
struct RirBlockState {
    definite: Vec<bool>,
    possible: Vec<bool>,
    lambda_escapes: Vec<Option<RirLambdaEscape>>,
    lambda_values: Vec<RirFunctionValueState>,
    loop_lambda_scopes: Vec<Option<usize>>,
    cell_definite: Vec<bool>,
    cell_possible: Vec<bool>,
    globals: Vec<bool>,
    global_values: Vec<RirFunctionValueState>,
}

impl RirBlockState {
    fn merge_with(&mut self, next: Self) {
        self.definite = self
            .definite
            .iter()
            .zip(next.definite)
            .map(|(lhs, rhs)| *lhs && rhs)
            .collect();
        self.possible = self
            .possible
            .iter()
            .zip(next.possible)
            .map(|(lhs, rhs)| *lhs || rhs)
            .collect();
        self.lambda_escapes = self
            .lambda_escapes
            .iter()
            .zip(next.lambda_escapes)
            .map(|(lhs, rhs)| if *lhs == rhs { rhs } else { None })
            .collect();
        self.lambda_values = self
            .lambda_values
            .drain(..)
            .zip(next.lambda_values)
            .map(|(lhs, rhs)| RirFunctionValueState::join(lhs, rhs))
            .collect();
        self.loop_lambda_scopes = self
            .loop_lambda_scopes
            .iter()
            .zip(next.loop_lambda_scopes)
            .map(|(lhs, rhs)| if *lhs == rhs { rhs } else { None })
            .collect();
        self.cell_definite = self
            .cell_definite
            .iter()
            .zip(next.cell_definite)
            .map(|(lhs, rhs)| *lhs && rhs)
            .collect();
        self.cell_possible = self
            .cell_possible
            .iter()
            .zip(next.cell_possible)
            .map(|(lhs, rhs)| *lhs || rhs)
            .collect();
        self.globals = self
            .globals
            .iter()
            .zip(next.globals)
            .map(|(lhs, rhs)| *lhs && rhs)
            .collect();
        self.global_values = self
            .global_values
            .drain(..)
            .zip(next.global_values)
            .map(|(lhs, rhs)| RirFunctionValueState::join(lhs, rhs))
            .collect();
    }
}

#[derive(Clone)]
struct RirBlockEntryState {
    definite: Vec<bool>,
    possible: Vec<bool>,
    lambda_escapes: Vec<Option<RirLambdaEscape>>,
    lambda_values: Vec<RirFunctionValueState>,
    loop_lambda_scopes: Vec<Option<usize>>,
    global_initialized: Vec<bool>,
    global_values: Vec<RirFunctionValueState>,
}

impl RirBlockEntryState {
    fn merge_with(&mut self, next: Self) {
        self.definite = self
            .definite
            .iter()
            .zip(next.definite)
            .map(|(left, right)| *left && right)
            .collect();
        self.possible = self
            .possible
            .iter()
            .zip(next.possible)
            .map(|(left, right)| *left || right)
            .collect();
        self.lambda_escapes = self
            .lambda_escapes
            .iter()
            .zip(next.lambda_escapes)
            .map(|(left, right)| if *left == right { right } else { None })
            .collect();
        self.lambda_values = self
            .lambda_values
            .drain(..)
            .zip(next.lambda_values)
            .map(|(left, right)| RirFunctionValueState::join(left, right))
            .collect();
        self.loop_lambda_scopes = self
            .loop_lambda_scopes
            .iter()
            .zip(next.loop_lambda_scopes)
            .map(|(left, right)| if *left == right { right } else { None })
            .collect();
        self.global_initialized = self
            .global_initialized
            .iter()
            .zip(next.global_initialized)
            .map(|(left, right)| *left && right)
            .collect();
        self.global_values = self
            .global_values
            .drain(..)
            .zip(next.global_values)
            .map(|(left, right)| RirFunctionValueState::join(left, right))
            .collect();
    }
}

fn final_dyn_storage(program: &RirProgram) -> Option<Vec<Vec<RirDynStorage>>> {
    let classes = dyn_weakening_classes(program)?;
    let mut storage = program
        .dyn_carriers
        .iter()
        .map(|carrier| vec![RirDynStorage::Inline; carrier.variants.len()])
        .collect::<Vec<_>>();
    let mut direct_reasons = storage
        .iter()
        .map(|variants| vec![None; variants.len()])
        .collect::<Vec<Vec<Option<RirDynBoxReason>>>>();
    let mut scratch = DynStorageScratch::new(program);
    for (carrier_index, carrier) in program.dyn_carriers.iter().enumerate() {
        for (variant_index, variant) in carrier.variants.iter().enumerate() {
            scratch.clear();
            let recursive =
                type_reaches_carrier(program, variant.concrete_ty, carrier.id, &mut scratch);
            let reason = if matches!(
                program.types.get(variant.concrete_ty.index()),
                Some(RirType::Lambda(_))
            ) {
                Some(RirDynBoxReason::Function)
            } else if recursive {
                Some(RirDynBoxReason::Recursive)
            } else {
                None
            };
            if reason.is_some() {
                storage[carrier_index][variant_index] = RirDynStorage::Boxed;
            }
            direct_reasons[carrier_index][variant_index] = reason;
        }
    }
    loop {
        let mut layouts = DynStorageLayoutCx::new(program, &storage);
        scratch.boxed.clear();
        for (carrier_index, carrier) in program.dyn_carriers.iter().enumerate() {
            for (variant_index, variant) in carrier.variants.iter().enumerate() {
                if direct_reasons[carrier_index][variant_index].is_none()
                    && matches!(
                        layouts.type_layout(variant.concrete_ty),
                        Err(FinalDynLayoutFailure::Function)
                    )
                {
                    scratch.boxed.push((carrier_index, variant_index));
                }
            }
        }
        if scratch.boxed.is_empty() {
            break;
        }
        for &(carrier, variant) in &scratch.boxed {
            direct_reasons[carrier][variant] = Some(RirDynBoxReason::FunctionField);
            storage[carrier][variant] = RirDynStorage::Boxed;
        }
    }
    let thresholds = {
        let mut layouts = DynStorageLayoutCx::new(program, &storage);
        scratch.boxed.clear();
        for (carrier_index, carrier) in program.dyn_carriers.iter().enumerate() {
            for (variant_index, variant) in carrier.variants.iter().enumerate() {
                let direct_reason = direct_reasons[carrier_index][variant_index];
                if let Some(reason) = direct_reason {
                    let evidence = match reason {
                        RirDynBoxReason::Function => RirDynInlineLayout::Function,
                        RirDynBoxReason::FunctionField
                            if matches!(
                                layouts.type_layout(variant.concrete_ty),
                                Err(FinalDynLayoutFailure::Function)
                            ) =>
                        {
                            RirDynInlineLayout::Function
                        }
                        RirDynBoxReason::Recursive => RirDynInlineLayout::Recursive,
                        RirDynBoxReason::FunctionField
                        | RirDynBoxReason::Threshold
                        | RirDynBoxReason::WeakeningClass(_) => return None,
                    };
                    if variant.inline_layout != evidence {
                        return None;
                    }
                    continue;
                }
                let Ok(layout) = layouts.type_layout(variant.concrete_ty) else {
                    return None;
                };
                let RirDynInlineLayout::Known(found) = variant.inline_layout else {
                    return None;
                };
                if !layout::valid(RustApproxLayout {
                    size: found.size,
                    align: found.align,
                }) || found.size != layout.size
                    || found.align != layout.align
                {
                    return None;
                }
                if layout.size > target_profile().inline_payload_limit()
                    || layout.align > target_profile().pointer_align()
                {
                    scratch.boxed.push((carrier_index, variant_index));
                }
            }
        }
        &scratch.boxed
    };
    for &(carrier, variant) in thresholds {
        direct_reasons[carrier][variant] = Some(RirDynBoxReason::Threshold);
        storage[carrier][variant] = RirDynStorage::Boxed;
    }
    while propagate_dyn_storage(program, &classes, &mut storage)? {}
    for (carrier_index, carrier) in program.dyn_carriers.iter().enumerate() {
        for (variant_index, variant) in carrier.variants.iter().enumerate() {
            let expected = direct_reasons[carrier_index][variant_index].or_else(|| {
                (storage.get(carrier_index)?.get(variant_index) == Some(&RirDynStorage::Boxed))
                    .then_some(RirDynBoxReason::WeakeningClass(classes[&carrier.id]))
            });
            if variant.box_reason != expected {
                return None;
            }
        }
    }
    Some(storage)
}

struct DynStorageScratch {
    visiting_carriers: Vec<bool>,
    visiting_types: Vec<bool>,
    boxed: Vec<(usize, usize)>,
}

impl DynStorageScratch {
    fn new(program: &RirProgram) -> Self {
        Self {
            visiting_carriers: vec![false; program.dyn_carriers.len()],
            visiting_types: vec![false; program.types.len()],
            boxed: vec![],
        }
    }

    fn clear(&mut self) {
        self.visiting_carriers.fill(false);
        self.visiting_types.fill(false);
    }
}

fn type_reaches_carrier(
    program: &RirProgram,
    ty: RirTypeId,
    target: RirDynCarrierId,
    scratch: &mut DynStorageScratch,
) -> bool {
    let Some(visiting) = scratch.visiting_types.get_mut(ty.index()) else {
        return false;
    };
    if *visiting {
        return false;
    }
    *visiting = true;
    let reaches = match program.types.get(ty.index()) {
        Some(RirType::Enum(id)) => {
            if let Some(carrier) = program.dyn_carriers.iter().find(|carrier| {
                matches!(program.types.get(carrier.storage_ty.index()), Some(RirType::Enum(candidate)) if candidate == id)
            }) {
                carrier_reaches(program, carrier.id, target, scratch)
            } else {
                program.enums.get(id.index()).is_some_and(|enm| {
                    enm.variants.iter().flat_map(|variant| &variant.fields).any(|field| {
                        type_reaches_carrier(program, field.ty, target, scratch)
                    })
                })
            }
        }
        Some(RirType::Struct(id)) => program.structs.get(id.index()).is_some_and(|decl| {
            decl.fields.iter().any(|field| type_reaches_carrier(program, field.ty, target, scratch))
        }),
        Some(RirType::Tuple(id)) => program.tuples.get(id.index()).is_some_and(|decl| {
            decl.fields.iter().any(|field| type_reaches_carrier(program, field.ty, target, scratch))
        }),
        Some(RirType::Option(inner) | RirType::Array { elem: inner, .. }) => {
            type_reaches_carrier(program, *inner, target, scratch)
        }
        _ => false,
    };
    scratch.visiting_types[ty.index()] = false;
    reaches
}

fn carrier_reaches(
    program: &RirProgram,
    current: RirDynCarrierId,
    target: RirDynCarrierId,
    scratch: &mut DynStorageScratch,
) -> bool {
    if current == target {
        return true;
    }
    let Some(visiting) = scratch.visiting_carriers.get_mut(current.index()) else {
        return false;
    };
    if *visiting {
        return false;
    }
    *visiting = true;
    let reaches = program
        .dyn_carriers
        .get(current.index())
        .is_some_and(|carrier| {
            carrier
                .variants
                .iter()
                .any(|variant| type_reaches_carrier(program, variant.concrete_ty, target, scratch))
        });
    scratch.visiting_carriers[current.index()] = false;
    reaches
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum FinalDynLayoutFailure {
    Function,
    FunctionCarrier,
    Provider,
    Recursive,
    Overflow,
    Unsupported,
    Invalid,
}

type FinalDynLayout = Result<RustApproxLayout, FinalDynLayoutFailure>;

struct DynStorageLayoutCx<'a> {
    program: &'a RirProgram,
    storage: &'a [Vec<RirDynStorage>],
    carriers: Vec<Option<FinalDynLayout>>,
    visiting_carriers: Vec<bool>,
    visiting_types: Vec<bool>,
}

impl<'a> DynStorageLayoutCx<'a> {
    fn new(program: &'a RirProgram, storage: &'a [Vec<RirDynStorage>]) -> Self {
        Self {
            program,
            storage,
            carriers: vec![None; program.dyn_carriers.len()],
            visiting_carriers: vec![false; program.dyn_carriers.len()],
            visiting_types: vec![false; program.types.len()],
        }
    }

    fn carrier_layout(&mut self, carrier: RirDynCarrierId) -> FinalDynLayout {
        if let Some(layout) = self.carriers.get(carrier.index()).copied().flatten() {
            return layout;
        }
        let Some(visiting) = self.visiting_carriers.get_mut(carrier.index()) else {
            return Err(FinalDynLayoutFailure::Invalid);
        };
        if *visiting {
            return Err(FinalDynLayoutFailure::Recursive);
        }
        *visiting = true;
        let layout = (|| {
            let decl = self
                .program
                .dyn_carriers
                .get(carrier.index())
                .ok_or(FinalDynLayoutFailure::Invalid)?;
            let mut payload = target_profile().primitive_layout(RustPrimitiveLayout::Unit);
            for (index, variant) in decl.variants.iter().enumerate() {
                let layout = match self
                    .storage
                    .get(carrier.index())
                    .and_then(|variants| variants.get(index))
                    .ok_or(FinalDynLayoutFailure::Invalid)?
                {
                    RirDynStorage::Inline => match self.type_layout(variant.concrete_ty) {
                        Err(
                            FinalDynLayoutFailure::Function
                            | FinalDynLayoutFailure::FunctionCarrier,
                        ) => return Err(FinalDynLayoutFailure::FunctionCarrier),
                        layout => layout?,
                    },
                    RirDynStorage::Boxed => target_profile().pointer_layout(),
                };
                payload.size = payload.size.max(layout.size);
                payload.align = payload.align.max(layout.align);
            }
            layout::enum_layout(target_profile().carrier_discriminant_layout(), payload)
                .ok_or(FinalDynLayoutFailure::Overflow)
        })();
        self.visiting_carriers[carrier.index()] = false;
        if let Some(slot) = self.carriers.get_mut(carrier.index()) {
            *slot = Some(layout);
        }
        layout
    }

    fn type_layout(&mut self, ty: RirTypeId) -> FinalDynLayout {
        let Some(visiting) = self.visiting_types.get_mut(ty.index()) else {
            return Err(FinalDynLayoutFailure::Invalid);
        };
        if *visiting {
            return Err(FinalDynLayoutFailure::Recursive);
        }
        *visiting = true;
        let layout = (|| {
            match self
            .program
            .types
            .get(ty.index())
            .ok_or(FinalDynLayoutFailure::Invalid)?
        {
            RirType::Int | RirType::Float | RirType::Flag(_) => {
                Ok(target_profile().primitive_layout(RustPrimitiveLayout::Integer))
            }
            RirType::Bool => Ok(target_profile().primitive_layout(RustPrimitiveLayout::Bool)),
            RirType::Char => Ok(target_profile().primitive_layout(RustPrimitiveLayout::Char)),
            RirType::Void => Ok(target_profile().primitive_layout(RustPrimitiveLayout::Unit)),
            RirType::String => layout::repeat(
                target_profile().pointer_layout(),
                3,
            )
            .ok_or(FinalDynLayoutFailure::Overflow),
            RirType::List(_) | RirType::Map { .. } => {
                Ok(target_profile().pointer_layout())
            }
            RirType::DataRef(id) => self
                .program
                .datarefs
                .get(id.index())
                .map(|_| target_profile().pointer_layout())
                .ok_or(FinalDynLayoutFailure::Unsupported),
            RirType::Slice(_) => layout::repeat(
                target_profile().pointer_layout(),
                2,
            )
            .ok_or(FinalDynLayoutFailure::Overflow),
            RirType::Lambda(_) => Err(FinalDynLayoutFailure::Function),
            RirType::Option(inner) => layout::enum_layout(
                target_profile().primitive_layout(RustPrimitiveLayout::Bool),
                self.type_layout(*inner)?,
            )
            .ok_or(FinalDynLayoutFailure::Overflow),
            RirType::Array { elem, len } => layout::repeat(
                self.type_layout(*elem)?,
                *len,
            )
            .ok_or(FinalDynLayoutFailure::Overflow),
            RirType::Struct(id) => {
                let decl = self
                    .program
                    .structs
                    .get(id.index())
                    .ok_or(FinalDynLayoutFailure::Invalid)?;
                if decl.native_ref {
                    Ok(target_profile().pointer_layout())
                } else if decl.native.is_some() {
                    decl.native_layout
                        .map(layout_from_evidence)
                        .ok_or(FinalDynLayoutFailure::Provider)?
                } else {
                    self.fields_layout(&decl.fields)
                }
            }
            RirType::Tuple(id) => self.fields_layout(
                &self
                    .program
                    .tuples
                    .get(id.index())
                    .ok_or(FinalDynLayoutFailure::Invalid)?
                    .fields,
            ),
            RirType::Enum(id) => {
                if let Some(carrier) = self.program.dyn_carriers.iter().find(|carrier| {
                    matches!(self.program.types.get(carrier.storage_ty.index()), Some(RirType::Enum(candidate)) if candidate == id)
                }) {
                    self.carrier_layout(carrier.id)
                } else {
                    let decl = self
                        .program
                        .enums
                        .get(id.index())
                        .ok_or(FinalDynLayoutFailure::Invalid)?;
                    if decl.native.is_some() {
                        decl.native_layout
                            .map(layout_from_evidence)
                            .ok_or(FinalDynLayoutFailure::Provider)?
                    } else {
                        let mut payload = target_profile().primitive_layout(RustPrimitiveLayout::Unit);
                        for variant in &decl.variants {
                            let layout = self.fields_layout(&variant.fields)?;
                            payload.size = payload.size.max(layout.size);
                            payload.align = payload.align.max(layout.align);
                        }
                        layout::enum_layout(
                            target_profile().carrier_discriminant_layout(),
                            payload,
                        )
                        .ok_or(FinalDynLayoutFailure::Overflow)
                    }
                }
            }
        }
        })();
        self.visiting_types[ty.index()] = false;
        layout
    }

    fn fields_layout(&mut self, fields: &[RirField]) -> FinalDynLayout {
        let mut layouts = Vec::with_capacity(fields.len());
        for field in fields {
            layouts.push(self.type_layout(field.ty)?);
        }
        layout::fields(layouts).ok_or(FinalDynLayoutFailure::Overflow)
    }
}

fn layout_from_evidence(layout: RirDynLayout) -> FinalDynLayout {
    let layout = RustApproxLayout {
        size: layout.size,
        align: layout.align,
    };
    layout::valid(layout)
        .then_some(layout)
        .ok_or(FinalDynLayoutFailure::Invalid)
}

fn dyn_weakening_classes(
    program: &RirProgram,
) -> Option<std::collections::BTreeMap<RirDynCarrierId, RirDynCarrierId>> {
    let mut classes = program
        .dyn_carriers
        .iter()
        .map(|carrier| (carrier.id, carrier.id))
        .collect::<std::collections::BTreeMap<_, _>>();
    loop {
        let mut changed = false;
        for weakening in &program.dyn_weakenings {
            let root = (*classes.get(&weakening.source)?).min(*classes.get(&weakening.target)?);
            let source = *classes.get(&weakening.source)?;
            let target = *classes.get(&weakening.target)?;
            for class in classes.values_mut() {
                if *class == source || *class == target {
                    changed |= *class != root;
                    *class = root;
                }
            }
        }
        if !changed {
            return Some(classes);
        }
    }
}

fn propagate_dyn_storage(
    program: &RirProgram,
    classes: &std::collections::BTreeMap<RirDynCarrierId, RirDynCarrierId>,
    storage: &mut [Vec<RirDynStorage>],
) -> Option<bool> {
    let mut changed = false;
    for (carrier_index, carrier) in program.dyn_carriers.iter().enumerate() {
        for (variant_index, variant) in carrier.variants.iter().enumerate() {
            let boxed =
                program
                    .dyn_carriers
                    .iter()
                    .enumerate()
                    .any(|(other_index, other)| {
                        classes.get(&other.id) == classes.get(&carrier.id)
                            && other.variants.iter().enumerate().any(
                                |(other_variant, other_decl)| {
                                    other_decl.concrete_ty == variant.concrete_ty
                                        && storage
                                            .get(other_index)
                                            .and_then(|variants| variants.get(other_variant))
                                            == Some(&RirDynStorage::Boxed)
                                },
                            )
                    });
            if boxed {
                let slot = storage.get_mut(carrier_index)?.get_mut(variant_index)?;
                changed |= *slot != RirDynStorage::Boxed;
                *slot = RirDynStorage::Boxed;
            }
        }
    }
    Some(changed)
}

struct VerifyCx<'a> {
    program: &'a RirProgram,
    errors: Vec<RirVerifyError>,
    initialized: Vec<bool>,
    possibly_initialized: Vec<bool>,
    payload_bound: Vec<bool>,
    lambda_escapes: Vec<Option<RirLambdaEscape>>,
    lambda_values: Vec<RirFunctionValueState>,
    loop_lambda_scopes: Vec<Option<usize>>,
    local_decl_scopes: Vec<Option<usize>>,
    initialized_cells: Vec<bool>,
    possibly_initialized_cells: Vec<bool>,
    global_initialized: Vec<bool>,
    global_values: Vec<RirFunctionValueState>,
    loops: Vec<RirLoopId>,
    scope_depth: usize,
    collection_loans: Vec<ActiveRirCollectionLoan>,
    active_dyn_reborrows: Vec<(RirLocalId, RirDynCarrierId)>,
}

#[derive(Debug, Clone)]
struct ActiveRirCollectionLoan {
    root: RirCollectionAccess,
    mode: RirCollectionLoanMode,
}

fn cell_ref_id(cell: RirCellRef) -> RirCellId {
    match cell {
        RirCellRef::Owner(cell) | RirCellRef::Capture { cell, .. } => cell,
    }
}

fn type_has_value_materializer(
    program: &RirProgram,
    ty: RirTypeId,
    visiting: &mut HashSet<RirTypeId>,
) -> bool {
    if !visiting.insert(ty) {
        return false;
    }
    let supported = match program.types.get(ty.index()) {
        Some(RirType::Void | RirType::Slice(_)) | None => false,
        Some(RirType::Option(inner)) => type_has_value_materializer(program, *inner, visiting),
        Some(RirType::Enum(id)) => program.enums[id.index()]
            .variants
            .iter()
            .flat_map(|variant| &variant.fields)
            .all(|field| type_has_value_materializer(program, field.ty, visiting)),
        Some(_) => true,
    };
    visiting.remove(&ty);
    supported
}

fn native_extern_signature_ok(
    program: &RirProgram,
    ext: &RirExtern,
    retained_callbacks: bool,
) -> bool {
    !ext.path.crate_name.is_empty()
        && !ext.path.segments.is_empty()
        && ext.path.segments.iter().all(|segment| !segment.is_empty())
        && ext.params.iter().all(|param| {
            let Some(ty) = program.types.get(param.ty.index()) else {
                return false;
            };
            let (mode, escape) = native_call::classify_param(&param.plan);
            let native_ref = native_ty_is_resource_ref(program, param.ty);
            param.mode == mode
                && param.escape == escape
                && param.action
                    == native_call::classify_arg_action(
                        &param.plan,
                        mode,
                        native_ref,
                        ext.suspends_runtime_entry,
                        ty,
                    )
                && rir_native_param_valid(program, param.ty, &param.plan)
        })
        && rir_native_return_valid(program, ext.ret, &ext.ret_plan)
        && ext.suspends_runtime_entry == retained_callbacks
        && native_context_ok(ext)
        && native_callback_receiver_ok(program, ext)
        && native_hidden_ctx_borrows_ok(program, ext)
}

fn rir_native_param_valid(program: &RirProgram, ty: RirTypeId, plan: &RirNativeParam) -> bool {
    match plan {
        RirNativeParam::Value => rir_native_value_valid(program, ty),
        RirNativeParam::SharedNamed => native_ty_is_resource_ref(program, ty),
        RirNativeParam::OwnedNamed => rir_native_inline_named(program, ty),
        RirNativeParam::Borrow | RirNativeParam::MutBorrow | RirNativeParam::MutPlace => {
            rir_native_borrow_valid(program, ty)
        }
        RirNativeParam::ScopedLambda
        | RirNativeParam::EscapingLambda
        | RirNativeParam::AnvCallback => {
            matches!(program.types.get(ty.index()), Some(RirType::Lambda(_)))
        }
        RirNativeParam::InitField(inner) => rir_native_param_valid(program, ty, inner),
        RirNativeParam::Option(inner) => {
            matches!(program.types.get(ty.index()), Some(RirType::Option(payload)) if rir_native_param_valid(program, *payload, inner))
        }
        RirNativeParam::Slice(inner) => {
            matches!(program.types.get(ty.index()), Some(RirType::Slice(elem)) if rir_native_param_valid(program, *elem, inner))
        }
        RirNativeParam::Array(inner) => {
            matches!(program.types.get(ty.index()), Some(RirType::Array { elem, .. }) if rir_native_param_valid(program, *elem, inner))
        }
        RirNativeParam::Result(ok, err) => rir_native_result_valid(program, ty, ok, err),
    }
}

fn rir_native_value_valid(program: &RirProgram, ty: RirTypeId) -> bool {
    rir_native_value_shape_valid(program, ty, false, &mut HashSet::new())
}

fn rir_native_value_shape_valid(
    program: &RirProgram,
    ty: RirTypeId,
    nested: bool,
    visiting: &mut HashSet<RirTypeId>,
) -> bool {
    if !visiting.insert(ty) {
        return false;
    }
    let valid = match program.types.get(ty.index()) {
        Some(RirType::Int | RirType::Float | RirType::Bool | RirType::String | RirType::Char) => {
            true
        }
        Some(RirType::Enum(id))
            if program
                .enums
                .get(id.index())
                .is_some_and(|enm| enm.core == Some(RirCoreEnumKind::Result)) =>
        {
            nested && rir_native_value_result_valid(program, ty, visiting)
        }
        Some(RirType::Struct(_) | RirType::Enum(_)) => rir_native_inline_named(program, ty),
        Some(RirType::Tuple(tuple)) => program.tuples.get(tuple.index()).is_some_and(|tuple| {
            tuple
                .fields
                .iter()
                .all(|field| rir_native_value_shape_valid(program, field.ty, true, visiting))
        }),
        Some(RirType::Array { elem, .. } | RirType::List(elem)) => {
            rir_native_value_shape_valid(program, *elem, true, visiting)
        }
        Some(RirType::Map { key, value }) => {
            rir_native_value_shape_valid(program, *key, true, visiting)
                && rir_native_value_shape_valid(program, *value, true, visiting)
        }
        Some(RirType::Option(inner)) => {
            nested && rir_native_value_shape_valid(program, *inner, true, visiting)
        }
        Some(
            RirType::Void
            | RirType::DataRef(_)
            | RirType::Flag(_)
            | RirType::Slice(_)
            | RirType::Lambda(_),
        )
        | None => false,
    };
    visiting.remove(&ty);
    valid
}

fn rir_native_value_result_valid(
    program: &RirProgram,
    ty: RirTypeId,
    visiting: &mut HashSet<RirTypeId>,
) -> bool {
    rir_result_variants_valid(program, ty, |variant, name| {
        rir_native_value_result_variant_valid(program, variant, name, visiting)
    })
}

fn rir_result_variants_valid(
    program: &RirProgram,
    ty: RirTypeId,
    mut valid: impl FnMut(&RirVariant, &str) -> bool,
) -> bool {
    let Some(RirType::Enum(id)) = program.types.get(ty.index()) else {
        return false;
    };
    let Some(enm) = program.enums.get(id.index()) else {
        return false;
    };
    let [ok, err] = enm.variants.as_slice() else {
        return false;
    };
    enm.core == Some(RirCoreEnumKind::Result) && valid(ok, "Ok") && valid(err, "Err")
}

fn rir_native_value_result_variant_valid(
    program: &RirProgram,
    variant: &RirVariant,
    name: &str,
    visiting: &mut HashSet<RirTypeId>,
) -> bool {
    variant.symbol.as_str() == name
        && variant.kind == RirVariantKind::Tuple
        && matches!(variant.fields.as_slice(), [field]
            if rir_native_value_shape_valid(program, field.ty, true, visiting))
}

fn rir_native_inline_named(program: &RirProgram, ty: RirTypeId) -> bool {
    match program.types.get(ty.index()) {
        Some(RirType::Struct(id)) => program.structs.get(id.index()).is_some_and(|strukt| {
            strukt.role == RirStructRole::Extern && strukt.native.is_some() && !strukt.native_ref
        }),
        Some(RirType::Enum(id)) => program
            .enums
            .get(id.index())
            .is_some_and(|enm| enm.role == RirEnumRole::Extern && enm.native.is_some()),
        _ => false,
    }
}

fn rir_native_named(program: &RirProgram, ty: RirTypeId) -> bool {
    match program.types.get(ty.index()) {
        Some(RirType::Struct(id)) => program
            .structs
            .get(id.index())
            .is_some_and(|strukt| strukt.role == RirStructRole::Extern && strukt.native.is_some()),
        Some(RirType::Enum(id)) => program
            .enums
            .get(id.index())
            .is_some_and(|enm| enm.role == RirEnumRole::Extern && enm.native.is_some()),
        _ => false,
    }
}

fn rir_native_borrow_valid(program: &RirProgram, ty: RirTypeId) -> bool {
    !matches!(
        program.types.get(ty.index()),
        Some(RirType::Void | RirType::Lambda(_)) | None
    )
}

fn rir_native_result_valid(
    program: &RirProgram,
    ty: RirTypeId,
    ok: &RirNativeParam,
    err: &RirNativeParam,
) -> bool {
    rir_result_variants_valid(program, ty, |variant, name| {
        rir_native_param_variant_valid(program, variant, name, if name == "Ok" { ok } else { err })
    })
}

fn rir_native_param_variant_valid(
    program: &RirProgram,
    variant: &RirVariant,
    name: &str,
    plan: &RirNativeParam,
) -> bool {
    variant.symbol.as_str() == name
        && variant.kind == RirVariantKind::Tuple
        && matches!(variant.fields.as_slice(), [field] if rir_native_param_valid(program, field.ty, plan))
}

fn rir_native_return_valid(program: &RirProgram, ty: RirTypeId, plan: &RirNativeReturn) -> bool {
    match plan {
        RirNativeReturn::Void => matches!(program.types.get(ty.index()), Some(RirType::Void)),
        RirNativeReturn::Value => rir_native_value_valid(program, ty),
        RirNativeReturn::SharedNamed => native_ty_is_resource_ref(program, ty),
        RirNativeReturn::OwnedNamed { adopt } => {
            rir_native_named(program, ty) && *adopt == native_ty_is_resource_ref(program, ty)
        }
        RirNativeReturn::Option {
            payload_ty,
            payload,
        } => {
            matches!(program.types.get(ty.index()), Some(RirType::Option(expected)) if expected == payload_ty)
                && rir_native_return_valid(program, *payload_ty, payload)
        }
        RirNativeReturn::Result {
            ok_ty,
            ok,
            err_ty,
            err,
        } => {
            let Some(RirType::Enum(id)) = program.types.get(ty.index()) else {
                return false;
            };
            let Some(enm) = program.enums.get(id.index()) else {
                return false;
            };
            let [ok_variant, err_variant] = enm.variants.as_slice() else {
                return false;
            };
            enm.core == Some(RirCoreEnumKind::Result)
                && rir_native_return_variant_valid(program, ok_variant, "Ok", *ok_ty, ok)
                && rir_native_return_variant_valid(program, err_variant, "Err", *err_ty, err)
        }
        RirNativeReturn::Array { elem_ty, elem } => {
            matches!(program.types.get(ty.index()), Some(RirType::Array { elem: expected, .. }) if expected == elem_ty)
                && rir_native_return_valid(program, *elem_ty, elem)
        }
    }
}

fn rir_native_return_variant_valid(
    program: &RirProgram,
    variant: &RirVariant,
    name: &str,
    ty: RirTypeId,
    plan: &RirNativeReturn,
) -> bool {
    variant.symbol.as_str() == name
        && variant.kind == RirVariantKind::Tuple
        && matches!(variant.fields.as_slice(), [field] if field.ty == ty)
        && rir_native_return_valid(program, ty, plan)
}

fn native_context_ok(ext: &RirExtern) -> bool {
    let (callbacks, scoped_callbacks, hidden_runtime) = ext
        .params
        .iter()
        .map(|param| native_param_facts(&param.plan))
        .fold((false, false, false), |facts, param| {
            (facts.0 || param.0, facts.1 || param.1, facts.2 || param.2)
        });
    (!scoped_callbacks || ext.ctx == RustCallContext::None)
        && (callbacks || ext.ctx != RustCallContext::None || !hidden_runtime)
}

fn native_param_facts(plan: &RirNativeParam) -> (bool, bool, bool) {
    match plan {
        RirNativeParam::ScopedLambda | RirNativeParam::EscapingLambda => (true, true, false),
        RirNativeParam::AnvCallback => (true, false, false),
        RirNativeParam::MutPlace => (false, false, true),
        RirNativeParam::InitField(inner)
        | RirNativeParam::Option(inner)
        | RirNativeParam::Array(inner)
        | RirNativeParam::Slice(inner) => native_param_facts(inner),
        RirNativeParam::Result(ok, err) => {
            let ok = native_param_facts(ok);
            let err = native_param_facts(err);
            (ok.0 || err.0, ok.1 || err.1, ok.2 || err.2)
        }
        RirNativeParam::Value
        | RirNativeParam::SharedNamed
        | RirNativeParam::OwnedNamed
        | RirNativeParam::Borrow
        | RirNativeParam::MutBorrow => (false, false, false),
    }
}

fn native_callback_receiver_ok(program: &RirProgram, ext: &RirExtern) -> bool {
    if !ext
        .params
        .iter()
        .any(|param| native_param_facts(&param.plan).0)
    {
        return true;
    }
    let Some(receiver) = ext.callback_receiver else {
        return true;
    };
    ext.params.get(receiver).is_some_and(|param| {
        matches!(
            param.plan,
            RirNativeParam::Borrow | RirNativeParam::MutBorrow
        ) && native_ty_is_resource_ref(program, param.ty)
    })
}

fn native_hidden_ctx_borrows_ok(program: &RirProgram, ext: &RirExtern) -> bool {
    ext.ctx != RustCallContext::HiddenRuntime
        || ext.params.iter().all(|param| {
            !matches!(
                param.plan,
                RirNativeParam::Borrow | RirNativeParam::MutBorrow
            ) || !native_ty_is_resource_ref(program, param.ty)
        })
}

pub(super) fn native_ty_is_resource_ref(program: &RirProgram, ty: RirTypeId) -> bool {
    let Some(RirType::Struct(id)) = program.types.get(ty.index()) else {
        return false;
    };
    program.structs.get(id.index()).is_some_and(|strukt| {
        strukt.role == RirStructRole::Extern && strukt.native.is_some() && strukt.native_ref
    })
}

impl VerifyCx<'_> {
    fn new(program: &RirProgram) -> VerifyCx<'_> {
        VerifyCx {
            program,
            errors: vec![],
            initialized: vec![],
            possibly_initialized: vec![],
            payload_bound: vec![],
            lambda_escapes: vec![],
            lambda_values: vec![],
            loop_lambda_scopes: vec![],
            local_decl_scopes: vec![],
            initialized_cells: vec![],
            possibly_initialized_cells: vec![],
            global_initialized: vec![],
            global_values: vec![],
            loops: vec![],
            scope_depth: 0,
            collection_loans: vec![],
            active_dyn_reborrows: vec![],
        }
    }

    fn check(&mut self) {
        if let Some(entry) = self.program.entry {
            self.check_function_id(RirVerifySite::Program, entry);
        }
        for (index, ty) in self.program.types.iter().enumerate() {
            let site = RirVerifySite::Type(RirTypeId::from_index(index));
            self.check_type_id(site, RirTypeId::from_index(index));
            match ty {
                RirType::Struct(id) => self.check_struct_id(site, *id),
                RirType::DataRef(id) => self.check_dataref_id(site, *id),
                RirType::Enum(id) => self.check_enum_id(site, *id),
                RirType::Flag(id) => self.check_flag_id(site, *id),
                RirType::Tuple(id) => self.check_tuple_id(site, *id),
                RirType::Array { elem, .. } | RirType::List(elem) | RirType::Slice(elem) => {
                    self.check_type_id(site, *elem);
                }
                RirType::Map { key, value } => {
                    self.check_type_id(site, *key);
                    self.check_type_id(site, *value);
                }
                RirType::Option(inner) => {
                    self.check_type_id(site, *inner);
                }
                RirType::Lambda(sig) => self.check_lambda_sig_id(site, *sig),
                _ => {}
            }
        }
        self.check_materializers();
        self.check_dyn_carriers();
        self.check_globals();
        self.check_lambda_sigs();
        self.check_lambdas();
        self.check_lambda_envs();
        self.check_collection_storages();
        self.check_cells();
        self.check_scoped_place_cells();
        self.check_cell_symbol_uniqueness();
        self.check_structs();
        self.check_datarefs();
        self.check_dataref_places();
        self.check_enums();
        self.check_flags();
        self.check_tuples();
        self.check_stringify_helpers();
        let mut string_texts = HashSet::new();
        let owned_string_literals = super::analysis::owned_string_literals(self.program);
        for (index, literal) in self.program.string_literals.iter().enumerate() {
            let id = RirStringLiteralId::from_index(index);
            if literal.id != id {
                self.push(RirVerifySite::StringLiteral(id), RirVerifyErrorKind::BadId);
            }
            if !string_texts.insert(literal.text.as_str()) {
                self.push(
                    RirVerifySite::StringLiteral(id),
                    RirVerifyErrorKind::DuplicateStringLiteral,
                );
            }
            if literal.needs_owned != owned_string_literals.contains(&id) {
                self.push(
                    RirVerifySite::StringLiteral(id),
                    RirVerifyErrorKind::StringLiteralOwnershipMismatch,
                );
            }
        }
        for (index, konst) in self.program.consts.iter().enumerate() {
            let id = RirConstId::from_index(index);
            if konst.id != id {
                self.push(RirVerifySite::Const(id), RirVerifyErrorKind::BadId);
            }
            self.check_const(id, konst);
        }
        let retained_callbacks = self.program.has_retained_callbacks();
        for (index, ext) in self.program.externs.iter().enumerate() {
            let id = RirExternId::from_index(index);
            if ext.id != id {
                self.push(RirVerifySite::Extern(id), RirVerifyErrorKind::BadId);
            }
            self.check_extern(id, ext, retained_callbacks);
        }
        for (index, function) in self.program.functions.iter().enumerate() {
            let id = RirFunctionId::from_index(index);
            if function.id != id {
                self.push(RirVerifySite::Function(id), RirVerifyErrorKind::BadId);
            }
            self.check_function(id, function);
        }
    }

    fn check_dyn_carriers(&mut self) {
        let site = RirVerifySite::Program;
        let expected_storage = final_dyn_storage(self.program);
        if expected_storage.is_none() {
            self.push(site, RirVerifyErrorKind::InvalidDynStorage);
        }
        let mut backings = std::collections::BTreeSet::new();
        for (index, carrier) in self.program.dyn_carriers.iter().enumerate() {
            if carrier.id != RirDynCarrierId::from_index(index) {
                self.push(site, RirVerifyErrorKind::InvalidDynCarrier);
            }
            self.check_type_id(site, carrier.storage_ty);
            let backing = match self.program.types.get(carrier.storage_ty.index()) {
                Some(RirType::Enum(id)) => self.program.enums.get(id.index()),
                _ => None,
            };
            let Some(backing) = backing else {
                self.push(site, RirVerifyErrorKind::InvalidDynStorage);
                continue;
            };
            if backing.role != RirEnumRole::DynamicCarrier
                || backing.native.is_some()
                || backing.core.is_some()
                || backing.repr != RirEnumRepr::Adt
                || !backings.insert(backing.id)
                || backing.variants.len() != carrier.variants.len()
            {
                self.push(site, RirVerifyErrorKind::InvalidDynStorage);
            }
            for (variant_index, variant) in carrier.variants.iter().enumerate() {
                if variant.id != RirDynVariantId::new(carrier.id, variant_index) {
                    self.push(site, RirVerifyErrorKind::InvalidDynVariant);
                }
                self.check_type_id(site, variant.concrete_ty);
                let payload_valid = backing.variants.get(variant_index).is_some_and(|backing| {
                    backing.kind == RirVariantKind::Tuple
                        && backing.fields.len() == 1
                        && backing.fields[0].ty == variant.concrete_ty
                }) && self
                    .program
                    .materializers
                    .get(variant.payload.index())
                    .is_some_and(|payload| {
                        payload.ty == variant.concrete_ty
                            && payload.position
                                == RustRecipePosition::StoredPayload(
                                    LambdaStorageFamily::DynamicPayload,
                                )
                    });
                if !payload_valid
                    || expected_storage
                        .as_ref()
                        .and_then(|carriers| carriers.get(index))
                        .and_then(|variants| variants.get(variant_index))
                        != Some(&variant.storage)
                {
                    self.push(site, RirVerifyErrorKind::InvalidDynStorage);
                }
            }
        }
        for (index, dispatch) in self.program.dyn_dispatches.iter().enumerate() {
            if dispatch.id != RirDynDispatchId::from_index(index) {
                self.push(site, RirVerifyErrorKind::InvalidDynDispatch);
            }
            let Some(carrier) = self.program.dyn_carriers.get(dispatch.carrier.index()) else {
                self.push(site, RirVerifyErrorKind::InvalidDynCarrier);
                continue;
            };
            self.check_type_id(site, dispatch.result_ty);
            for param in &dispatch.params {
                self.check_type_id(site, param.ty);
            }
            if dispatch.arms.len() != carrier.variants.len()
                || dispatch.arms.iter().enumerate().any(|(index, arm)| {
                    !self.dyn_dispatch_target_valid(
                        carrier.variants[index].concrete_ty,
                        arm,
                        &dispatch.params,
                        dispatch.result_ty,
                    )
                })
                || !dispatch.arms.first().is_some_and(|first| {
                    dispatch.arms.iter().all(|arm| {
                        arm.receiver.is_readonly_receiver() == first.receiver.is_readonly_receiver()
                    })
                })
            {
                self.push(site, RirVerifyErrorKind::InvalidDynDispatch);
            }
        }
        for (index, weakening) in self.program.dyn_weakenings.iter().enumerate() {
            if weakening.id != RirDynWeakeningId::from_index(index) {
                self.push(site, RirVerifyErrorKind::InvalidDynStorage);
            }
            let (Some(source), Some(target)) = (
                self.program.dyn_carriers.get(weakening.source.index()),
                self.program.dyn_carriers.get(weakening.target.index()),
            ) else {
                self.push(site, RirVerifyErrorKind::InvalidDynCarrier);
                continue;
            };
            if weakening.source == weakening.target
                || weakening.arms.len() != source.variants.len()
                || weakening.arms.iter().enumerate().any(|(index, arm)| {
                    arm.target.carrier() != target.id
                        || target
                            .variants
                            .get(arm.target.index())
                            .is_none_or(|target_variant| {
                                let source_variant = &source.variants[index];
                                target_variant.concrete_ty != source_variant.concrete_ty
                                    || target_variant.storage != source_variant.storage
                            })
                })
            {
                self.push(site, RirVerifyErrorKind::InvalidDynStorage);
            }
        }
        for (index, set) in self.program.dyn_variant_sets.iter().enumerate() {
            if set.id != RirDynVariantSetId::from_index(index) {
                self.push(site, RirVerifyErrorKind::InvalidDynVariant);
            }
            self.check_type_id(site, set.target);
            let Some(carrier) = self.program.dyn_carriers.get(set.carrier.index()) else {
                self.push(site, RirVerifyErrorKind::InvalidDynCarrier);
                continue;
            };
            let expected = carrier
                .variants
                .iter()
                .enumerate()
                .filter_map(|(index, variant)| {
                    (variant.concrete_ty == set.target)
                        .then_some(RirDynVariantId::new(carrier.id, index))
                })
                .collect::<Vec<_>>();
            if set.variants != expected {
                self.push(site, RirVerifyErrorKind::InvalidDynVariant);
            }
        }
    }

    fn check_materializers(&mut self) {
        let site = RirVerifySite::Program;
        if self.program.value_materializers.len() != self.program.types.len() {
            self.push(site, RirVerifyErrorKind::BadId);
        }
        for (index, materializer) in self.program.value_materializers.iter().enumerate() {
            let ty = RirTypeId::from_index(index);
            let required = match self.program.types.get(index) {
                Some(RirType::Lambda(sig)) => {
                    let policy = RirRustRepPolicy::new(self.program);
                    policy.lambda_sig_copyable(*sig) || policy.lambda_sig_cloneable(*sig)
                }
                Some(_) => type_has_value_materializer(self.program, ty, &mut HashSet::new()),
                None => false,
            };
            if materializer.is_some() != required
                || materializer.is_some_and(|materializer| {
                    !self.materializer_matches(ty, materializer, RustRecipePosition::Value)
                })
            {
                self.push(site, RirVerifyErrorKind::BadId);
            }
        }
        let mut declarations = std::collections::BTreeSet::new();
        for (index, materializer) in self.program.materializers.iter().enumerate() {
            if materializer.id.index() != index
                || materializer.ty.index() >= self.program.types.len()
                || !declarations.insert((materializer.ty, materializer.position))
                || !self.materializer_action_valid(materializer)
            {
                self.push(site, RirVerifyErrorKind::BadId);
            }
        }
        if !self.materializer_cycles_valid() {
            self.push(site, RirVerifyErrorKind::BadId);
        }
    }

    fn materializer_cycles_valid(&self) -> bool {
        fn visit_all<'a>(
            program: &RirProgram,
            ids: impl IntoIterator<Item = &'a RirMaterializerId>,
            states: &mut [u8],
        ) -> bool {
            ids.into_iter().all(|id| visit(program, *id, states))
        }

        fn visit(program: &RirProgram, id: RirMaterializerId, states: &mut [u8]) -> bool {
            let Some(state) = states.get_mut(id.index()) else {
                return false;
            };
            match *state {
                1 => return false,
                2 => return true,
                _ => *state = 1,
            }
            let Some(materializer) = program.materializers.get(id.index()) else {
                return false;
            };
            let copy_valid = match &materializer.copy {
                Some(
                    RirCopyEvidence::Struct { fields, .. } | RirCopyEvidence::Tuple { fields },
                ) => visit_all(program, fields, states),
                Some(RirCopyEvidence::Array { elem }) => visit(program, *elem, states),
                Some(RirCopyEvidence::Enum { variants }) => {
                    visit_all(program, variants.iter().flatten(), states)
                }
                Some(RirCopyEvidence::Optional { payload }) => visit(program, *payload, states),
                Some(RirCopyEvidence::Leaf) | None => true,
            };
            let support_valid = match &materializer.support {
                Some(RirSupportEvidence::ProviderStruct { fields }) => {
                    visit_all(program, fields, states)
                }
                Some(RirSupportEvidence::ProviderEnum { variants }) => {
                    visit_all(program, variants.iter().flatten(), states)
                }
                Some(RirSupportEvidence::List { .. } | RirSupportEvidence::Map { .. }) | None => {
                    true
                }
            };
            let action_valid =
                match &materializer.action {
                    RirMaterializerAction::Struct { fields }
                    | RirMaterializerAction::Tuple { fields } => visit_all(program, fields, states),
                    RirMaterializerAction::Array { elem } => visit(program, *elem, states),
                    RirMaterializerAction::Enum { variants } => {
                        visit_all(program, variants.iter().flatten(), states)
                    }
                    RirMaterializerAction::Optional { payload } => visit(program, *payload, states),
                    RirMaterializerAction::DynamicMaterialize { carrier, variants } => {
                        let Some(carrier) = program.dyn_carriers.get(carrier.index()) else {
                            return false;
                        };
                        visit_all(
                            program,
                            carrier.variants.iter().zip(variants).filter_map(
                                |(variant, payload)| {
                                    (variant.storage == RirDynStorage::Inline).then_some(payload)
                                },
                            ),
                            states,
                        )
                    }
                    RirMaterializerAction::Copy
                    | RirMaterializerAction::ManagedShare
                    | RirMaterializerAction::IdentityShare
                    | RirMaterializerAction::CallableShare
                    | RirMaterializerAction::ProviderMaterialize { .. } => true,
                };
            if !copy_valid || !support_valid || !action_valid {
                return false;
            }
            states[id.index()] = 2;
            true
        }

        let mut states = vec![0; self.program.materializers.len()];
        (0..states.len()).all(|index| {
            visit(
                self.program,
                RirMaterializerId::from_index(index),
                &mut states,
            )
        })
    }

    fn support_evidence_valid(&self, materializer: &RirMaterializer) -> bool {
        let child = |id: RirMaterializerId, ty: RirTypeId, family: LambdaStorageFamily| {
            self.materializer_matches(ty, id, child_recipe_position(materializer.position, family))
        };
        let ty = self.program.types.get(materializer.ty.index());
        match (&materializer.support, ty) {
            (Some(RirSupportEvidence::List { elem }), Some(RirType::List(ty))) => {
                child(*elem, *ty, LambdaStorageFamily::ListElement)
            }
            (
                Some(RirSupportEvidence::Map {
                    key,
                    key_contract,
                    value,
                }),
                Some(RirType::Map {
                    key: key_ty,
                    value: value_ty,
                }),
            ) => {
                child(*key, *key_ty, LambdaStorageFamily::MapKey)
                    && self.materializer_matches(*key_ty, *key_contract, RustRecipePosition::MapKey)
                    && child(*value, *value_ty, LambdaStorageFamily::MapValue)
            }
            (Some(RirSupportEvidence::ProviderStruct { fields }), Some(RirType::Struct(id))) => {
                self.program.structs.get(id.index()).is_some_and(|strukt| {
                    strukt.native.is_some()
                        && !strukt.native_ref
                        && fields.len() == strukt.fields.len()
                        && fields.iter().zip(&strukt.fields).all(|(entry, field)| {
                            child(*entry, field.ty, LambdaStorageFamily::StructField)
                        })
                })
            }
            (Some(RirSupportEvidence::ProviderEnum { variants }), Some(RirType::Enum(id))) => {
                self.program.enums.get(id.index()).is_some_and(|enm| {
                    enm.native.is_some()
                        && variants.len() == enm.variants.len()
                        && variants
                            .iter()
                            .zip(&enm.variants)
                            .all(|(entries, variant)| {
                                entries.len() == variant.fields.len()
                                    && entries.iter().zip(&variant.fields).all(|(entry, field)| {
                                        child(*entry, field.ty, LambdaStorageFamily::EnumPayload)
                                    })
                            })
                })
            }
            (None, Some(RirType::Struct(id))) => {
                self.program.structs[id.index()].native.is_none()
                    || self.program.structs[id.index()].native_ref
            }
            (None, Some(RirType::Enum(id))) => self.program.enums[id.index()].native.is_none(),
            (None, Some(RirType::List(_) | RirType::Map { .. })) | (Some(_), _) => false,
            (None, _) => true,
        }
    }

    fn copy_evidence_valid(&self, materializer: &RirMaterializer) -> bool {
        let child = |id: RirMaterializerId, ty: RirTypeId, family: LambdaStorageFamily| {
            self.program
                .materializers
                .get(id.index())
                .is_some_and(|entry| {
                    entry.ty == ty
                        && entry.position == child_recipe_position(materializer.position, family)
                        && entry.is_copy()
                })
        };
        let ty = self.program.types.get(materializer.ty.index());
        match (&materializer.copy, ty) {
            (None, _)
            | (
                Some(RirCopyEvidence::Leaf),
                Some(
                    RirType::Int
                    | RirType::Flag(_)
                    | RirType::Float
                    | RirType::Bool
                    | RirType::Char,
                ),
            ) => true,
            (Some(RirCopyEvidence::Leaf), Some(RirType::Lambda(sig))) => {
                let policy = RirRustRepPolicy::new(self.program);
                !policy.lambda_sig_has_heap_env(*sig) && policy.lambda_sig_copyable(*sig)
            }
            (Some(RirCopyEvidence::Leaf), Some(RirType::Struct(id))) => {
                self.program.structs.get(id.index()).is_some_and(|strukt| {
                    !strukt.native_ref
                        && strukt.native.as_ref().is_some_and(|native| {
                            native
                                .validated_materializer(ExternMaterialization::Copy)
                                .is_some()
                        })
                })
            }
            (Some(RirCopyEvidence::Leaf), Some(RirType::Enum(id))) => self
                .program
                .enums
                .get(id.index())
                .and_then(|enm| enm.native.as_ref())
                .is_some_and(|native| {
                    native
                        .validated_materializer(ExternMaterialization::Copy)
                        .is_some()
                }),
            (Some(RirCopyEvidence::Struct { family, fields }), Some(RirType::Struct(id))) => {
                self.program.structs.get(id.index()).is_some_and(|strukt| {
                    strukt.native.is_none()
                        && fields.len() == strukt.fields.len()
                        && fields
                            .iter()
                            .zip(&strukt.fields)
                            .all(|(entry, field)| child(*entry, field.ty, *family))
                })
            }
            (Some(RirCopyEvidence::Tuple { fields }), Some(RirType::Tuple(id))) => {
                self.program.tuples.get(id.index()).is_some_and(|tuple| {
                    fields.len() == tuple.fields.len()
                        && fields.iter().zip(&tuple.fields).all(|(entry, field)| {
                            child(*entry, field.ty, LambdaStorageFamily::TupleField)
                        })
                })
            }
            (Some(RirCopyEvidence::Array { elem }), Some(RirType::Array { elem: ty, .. })) => {
                child(*elem, *ty, LambdaStorageFamily::FixedArrayElement)
            }
            (Some(RirCopyEvidence::Optional { payload }), Some(RirType::Option(ty))) => {
                child(*payload, *ty, LambdaStorageFamily::OptionalPayload)
            }
            (Some(RirCopyEvidence::Enum { variants }), Some(RirType::Enum(id))) => {
                self.program.enums.get(id.index()).is_some_and(|enm| {
                    self.program.dyn_carrier_for_enum(*id).is_none()
                        && variants.len() == enm.variants.len()
                        && variants
                            .iter()
                            .zip(&enm.variants)
                            .all(|(entries, variant)| {
                                entries.len() == variant.fields.len()
                                    && entries.iter().zip(&variant.fields).all(|(entry, field)| {
                                        child(*entry, field.ty, LambdaStorageFamily::EnumPayload)
                                    })
                            })
                })
            }
            (Some(_), _) => false,
        }
    }

    fn materializer_action_valid(&self, materializer: &RirMaterializer) -> bool {
        let child = |id: RirMaterializerId, ty: RirTypeId, family: LambdaStorageFamily| {
            self.materializer_matches(ty, id, child_recipe_position(materializer.position, family))
        };
        let ty = self.program.types.get(materializer.ty.index());
        let position_valid = match materializer.position {
            RustRecipePosition::Value => true,
            RustRecipePosition::StoredPayload(family) => match ty {
                Some(RirType::Lambda(_)) => family.allows_function_payload(),
                Some(RirType::Enum(id)) if self.program.dyn_carrier_for_enum(*id).is_some() => {
                    family.allows_dynamic_owned()
                }
                Some(RirType::Void | RirType::Slice(_)) | None => false,
                Some(_) => true,
            },
            RustRecipePosition::MapKey => match ty {
                Some(
                    RirType::Int
                    | RirType::Bool
                    | RirType::String
                    | RirType::Char
                    | RirType::Flag(_)
                    | RirType::Tuple(_),
                ) => true,
                Some(RirType::Struct(id)) => self.program.structs[id.index()].native.is_none(),
                Some(RirType::Enum(id)) => self.program.dyn_carrier_for_enum(*id).is_none(),
                _ => false,
            },
            RustRecipePosition::Global => {
                !matches!(ty, Some(RirType::Void | RirType::Slice(_)) | None)
            }
        };
        if !position_valid
            || materializer.is_copy() != matches!(materializer.action, RirMaterializerAction::Copy)
            || !self.copy_evidence_valid(materializer)
            || !self.support_evidence_valid(materializer)
        {
            return false;
        }
        let ty = self.program.types.get(materializer.ty.index());
        match &materializer.action {
            RirMaterializerAction::Copy => ty.is_some(),
            RirMaterializerAction::ManagedShare => {
                matches!(
                    ty,
                    Some(RirType::String | RirType::List(_) | RirType::Map { .. })
                )
            }
            RirMaterializerAction::IdentityShare => match ty {
                Some(RirType::DataRef(_)) => true,
                Some(RirType::Struct(id)) => self
                    .program
                    .structs
                    .get(id.index())
                    .is_some_and(|strukt| strukt.native_ref),
                _ => false,
            },
            RirMaterializerAction::CallableShare => matches!(
                ty,
                Some(RirType::Lambda(sig))
                    if RirRustRepPolicy::new(self.program).lambda_sig_cloneable(*sig)
            ),
            RirMaterializerAction::ProviderMaterialize { binding } => {
                let native = match ty {
                    Some(RirType::Struct(id)) => self
                        .program
                        .structs
                        .get(id.index())
                        .filter(|strukt| !strukt.native_ref)
                        .and_then(|strukt| strukt.native.as_ref()),
                    Some(RirType::Enum(id)) => self
                        .program
                        .enums
                        .get(id.index())
                        .and_then(|enm| enm.native.as_ref()),
                    _ => None,
                };
                native.is_some_and(|native| {
                    native.validated_materializer(ExternMaterialization::Materialize)
                        == Some(binding)
                })
            }
            RirMaterializerAction::Struct { fields } => {
                let Some(RirType::Struct(id)) = ty else {
                    return false;
                };
                let Some(strukt) = self.program.structs.get(id.index()) else {
                    return false;
                };
                if strukt.native.is_some() {
                    return false;
                }
                let expected = &strukt.fields;
                fields.len() == expected.len()
                    && fields.iter().zip(expected).all(|(materializer, field)| {
                        child(*materializer, field.ty, LambdaStorageFamily::StructField)
                    })
            }
            RirMaterializerAction::Tuple { fields } => {
                let Some(RirType::Tuple(id)) = ty else {
                    return false;
                };
                let Some(tuple) = self.program.tuples.get(id.index()) else {
                    return false;
                };
                let expected = &tuple.fields;
                fields.len() == expected.len()
                    && fields.iter().zip(expected).all(|(materializer, field)| {
                        child(*materializer, field.ty, LambdaStorageFamily::TupleField)
                    })
            }
            RirMaterializerAction::Array { elem } => {
                matches!(ty, Some(RirType::Array { elem: expected, .. }) if child(
                    *elem,
                    *expected,
                    LambdaStorageFamily::FixedArrayElement,
                ))
            }
            RirMaterializerAction::Optional { payload } => {
                matches!(ty, Some(RirType::Option(expected)) if child(
                    *payload,
                    *expected,
                    LambdaStorageFamily::OptionalPayload,
                ))
            }
            RirMaterializerAction::Enum { variants } => {
                let Some(RirType::Enum(id)) = ty else {
                    return false;
                };
                let Some(enm) = self.program.enums.get(id.index()) else {
                    return false;
                };
                if enm.native.is_some() || self.program.dyn_carrier_for_enum(*id).is_some() {
                    return false;
                }
                let expected = &enm.variants;
                variants.len() == expected.len()
                    && variants
                        .iter()
                        .zip(expected)
                        .all(|(materializers, variant)| {
                            materializers.len() == variant.fields.len()
                                && materializers.iter().zip(&variant.fields).all(
                                    |(materializer, field)| {
                                        child(
                                            *materializer,
                                            field.ty,
                                            LambdaStorageFamily::EnumPayload,
                                        )
                                    },
                                )
                        })
            }
            RirMaterializerAction::DynamicMaterialize { carrier, variants } => {
                let Some(carrier) = self.program.dyn_carriers.get(carrier.index()) else {
                    return false;
                };
                materializer.ty == carrier.storage_ty
                    && variants.len() == carrier.variants.len()
                    && variants
                        .iter()
                        .zip(&carrier.variants)
                        .all(|(payload, variant)| {
                            child(
                                *payload,
                                variant.concrete_ty,
                                LambdaStorageFamily::DynamicPayload,
                            )
                        })
            }
        }
    }

    fn canonical_value_materializer(&self, ty: RirTypeId, materializer: RirMaterializerId) -> bool {
        self.program
            .value_materializers
            .get(ty.index())
            .copied()
            .flatten()
            == Some(materializer)
            && self.materializer_matches(ty, materializer, RustRecipePosition::Value)
    }

    fn materializer_matches(
        &self,
        ty: RirTypeId,
        materializer: RirMaterializerId,
        position: RustRecipePosition,
    ) -> bool {
        self.program
            .materializers
            .get(materializer.index())
            .is_some_and(|decl| decl.ty == ty && decl.position == position)
    }

    fn check_globals(&mut self) {
        let mut slots = HashSet::new();
        for (index, global) in self.program.globals.iter().enumerate() {
            let id = RirGlobalId::from_index(index);
            let site = RirVerifySite::Global(id);
            if global.id != id {
                self.push(site, RirVerifyErrorKind::BadId);
            }
            self.check_type_id(site, global.ty);
            if !self.global_payload_supported(global.ty) {
                self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
            }
            self.check_function_id(site, global.init);
            if let Some(function) = self.program.functions.get(global.init.index()) {
                if function.ret.ty != global.ty {
                    self.push(
                        site,
                        RirVerifyErrorKind::TypeMismatch {
                            expected: global.ty,
                            found: function.ret.ty,
                        },
                    );
                }
                if !function.params.is_empty() {
                    self.push(
                        site,
                        RirVerifyErrorKind::CallArgCount {
                            expected: 0,
                            found: function.params.len(),
                        },
                    );
                }
            }
            if global.name.as_str().is_empty() || global.slot_symbol.as_str().is_empty() {
                self.push(site, RirVerifyErrorKind::BadId);
            }
            if !slots.insert(global.slot_symbol.as_str()) {
                self.push(site, RirVerifyErrorKind::DuplicateSymbol);
            }
        }
    }

    fn global_payload_supported(&self, ty: RirTypeId) -> bool {
        self.program.materializers.iter().any(|materializer| {
            materializer.ty == ty && materializer.position == RustRecipePosition::Global
        })
    }

    fn stored_payload_supported(&self, ty: RirTypeId) -> bool {
        self.program.materializers.iter().any(|materializer| {
            materializer.ty == ty
                && materializer.position
                    == RustRecipePosition::StoredPayload(LambdaStorageFamily::UnknownOrigin)
        })
    }

    fn value_from_ref_supported(&self, ty: RirTypeId) -> bool {
        let Some(materializer) = self
            .program
            .value_materializers
            .get(ty.index())
            .copied()
            .flatten()
            .and_then(|id| self.program.materializers.get(id.index()))
        else {
            return false;
        };
        materializer.ty == ty
            && materializer.position == RustRecipePosition::Value
            && self.materializer_action_valid(materializer)
    }

    fn check_stored_payload_family(
        &mut self,
        site: RirVerifySite,
        ty: RirTypeId,
        family: LambdaStorageFamily,
    ) {
        self.check_type_id(site, ty);
        if self.ty(ty).is_none()
            || (family == LambdaStorageFamily::EnumPayload
                && matches!(self.ty(ty), Some(RirType::Slice(_))))
        {
            return;
        }
        let position = RustRecipePosition::StoredPayload(family);
        let supported = self
            .program
            .materializers
            .iter()
            .any(|materializer| materializer.ty == ty && materializer.position == position);
        if !supported {
            self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
        }
    }

    fn check_lambda_sigs(&mut self) {
        for (index, sig) in self.program.lambda_sigs.iter().enumerate() {
            let id = RirLambdaSigId::from_index(index);
            let site = RirVerifySite::Program;
            if sig.id != id {
                self.push(site, RirVerifyErrorKind::BadId);
            }
            for param in &sig.params {
                self.check_type_id(site, param.ty);
                self.check_pass_mode(site, param.ty, param.mode);
                if matches!(
                    param.mode,
                    RirPassMode::ScopedLambda
                        | RirPassMode::EscapingLambda
                        | RirPassMode::AnvCallback
                        | RirPassMode::StackCell
                        | RirPassMode::HeapCell
                        | RirPassMode::ScopedPlaceCell
                ) {
                    self.push(site, RirVerifyErrorKind::UnsupportedAbi);
                }
            }
            self.check_type_id(site, sig.ret);
            self.check_lambda_sig_capture_kinds(site, id);
        }
    }

    fn check_lambda_sig_capture_kinds(&mut self, site: RirVerifySite, sig: RirLambdaSigId) {
        let policy = RirRustRepPolicy::new(self.program);
        if policy.lambda_sig_has_cell_and_mut_borrow(sig) {
            self.push(site, RirVerifyErrorKind::UnsupportedLambdaCapture);
        }
    }

    fn check_lambdas(&mut self) {
        let policy = RirRustRepPolicy::new(self.program);
        let mut env_owners = vec![None; self.program.lambda_envs.len()];
        for (index, lambda) in self.program.lambdas.iter().enumerate() {
            let site = RirVerifySite::Program;
            let id = RirLambdaId::from_index(index);
            if lambda.id != id {
                self.push(site, RirVerifyErrorKind::BadId);
            }
            self.check_function_id(site, lambda.function);
            self.check_lambda_sig_id(site, lambda.sig);
            self.check_lambda_storage(site, lambda);
            if policy.lambda_has_recursive_inline_value_capture(lambda) {
                self.push(site, RirVerifyErrorKind::UnsupportedLambdaCapture);
            }
            if let RirLambdaStorage::HeapEnv { env } = lambda.storage
                && let Some(owner) = env_owners.get_mut(env.index())
                && owner.replace(lambda.id).is_some()
            {
                self.push(site, RirVerifyErrorKind::UnsupportedLambdaCapture);
            }
            self.check_lambda_function_signature(site, lambda);
        }
    }

    fn check_lambda_envs(&mut self) {
        let policy = RirRustRepPolicy::new(self.program);
        for (index, env) in self.program.lambda_envs.iter().enumerate() {
            let site = RirVerifySite::Program;
            let id = RirLambdaEnvId::from_index(index);
            if env.id != id {
                self.push(site, RirVerifyErrorKind::BadId);
            }
            self.check_lambda_id(site, env.lambda);
            if env.symbol.as_str().is_empty() {
                self.push(site, RirVerifyErrorKind::BadId);
            }
            let Some(lambda) = self.program.lambdas.get(env.lambda.index()) else {
                continue;
            };
            if !matches!(lambda.storage, RirLambdaStorage::HeapEnv { env: storage } if storage == id)
            {
                self.push(site, RirVerifyErrorKind::UnsupportedLambdaCapture);
            }
            if env.fields.len() != lambda.captures.len() {
                self.push(site, RirVerifyErrorKind::UnsupportedLambdaCapture);
            }
            for (index, field) in env.fields.iter().enumerate() {
                self.check_type_id(site, field.ty);
                if self.ty(field.ty).is_some() && !policy.lambda_env_field_storage_supported(field)
                {
                    self.push(site, RirVerifyErrorKind::UnsupportedLambdaCapture);
                }
                if field.symbol.as_str().is_empty() {
                    self.push(site, RirVerifyErrorKind::BadId);
                }
                match lambda.captures.get(index) {
                    Some(capture) if capture.ty != field.ty => self.push(
                        site,
                        RirVerifyErrorKind::TypeMismatch {
                            expected: capture.ty,
                            found: field.ty,
                        },
                    ),
                    Some(capture) => match (field.kind, capture.kind) {
                        (RirLambdaEnvFieldKind::Value, RirLambdaCaptureKind::Param) => {}
                        (
                            RirLambdaEnvFieldKind::HeapCell { cell: field_cell },
                            RirLambdaCaptureKind::HeapCell { cell: capture_cell },
                        ) if field_cell == capture_cell => {}
                        _ => self.push(site, RirVerifyErrorKind::UnsupportedLambdaCapture),
                    },
                    None => {}
                }
            }
        }
    }

    fn check_lambda_storage(&mut self, site: RirVerifySite, lambda: &RirLambda) {
        match lambda.storage {
            RirLambdaStorage::ZeroEnv if !lambda.captures.is_empty() => {
                self.push(site, RirVerifyErrorKind::UnsupportedLambdaCapture);
            }
            RirLambdaStorage::ScopedCaptures if lambda.captures.is_empty() => {
                self.push(site, RirVerifyErrorKind::UnsupportedLambdaCapture);
            }
            RirLambdaStorage::ScopedCaptures if lambda.escape == RirLambdaEscape::Escaping => {
                self.push(site, RirVerifyErrorKind::UnsupportedLambdaCapture);
            }
            RirLambdaStorage::HeapEnv { env } if lambda.captures.is_empty() => {
                self.check_lambda_env_id(site, env);
                self.push(site, RirVerifyErrorKind::UnsupportedLambdaCapture);
            }
            RirLambdaStorage::HeapEnv { env } if lambda.escape != RirLambdaEscape::Escaping => {
                self.check_lambda_env_id(site, env);
                self.push(site, RirVerifyErrorKind::UnsupportedLambdaCapture);
            }
            RirLambdaStorage::HeapEnv { env } => {
                self.check_lambda_env_id(site, env);
                if self
                    .program
                    .lambda_envs
                    .get(env.index())
                    .is_some_and(|layout| layout.lambda != lambda.id)
                {
                    self.push(site, RirVerifyErrorKind::UnsupportedLambdaCapture);
                }
            }
            RirLambdaStorage::ZeroEnv | RirLambdaStorage::ScopedCaptures => {}
        }
        let heap_env = matches!(lambda.storage, RirLambdaStorage::HeapEnv { .. });
        for capture in &lambda.captures {
            self.check_type_id(site, capture.ty);
            self.check_pass_mode(site, capture.ty, capture.mode);
            match capture.kind {
                RirLambdaCaptureKind::Param => {
                    if heap_env {
                        if capture.mode != RirPassMode::Value
                            || (self.ty(capture.ty).is_some()
                                && !self.value_from_ref_supported(capture.ty))
                        {
                            self.push(site, RirVerifyErrorKind::UnsupportedLambdaCapture);
                        }
                    } else {
                        if matches!(
                            capture.mode,
                            RirPassMode::MutBorrow
                                | RirPassMode::ScopedLambda
                                | RirPassMode::EscapingLambda
                                | RirPassMode::AnvCallback
                                | RirPassMode::StackCell
                                | RirPassMode::HeapCell
                                | RirPassMode::ScopedPlaceCell
                        ) {
                            self.push(site, RirVerifyErrorKind::UnsupportedLambdaCapture);
                        }
                        if capture.mode == RirPassMode::Value && !self.copyable_type(capture.ty) {
                            self.push(site, RirVerifyErrorKind::NonCopyValueRequired);
                        }
                    }
                }
                RirLambdaCaptureKind::StackCell { cell } => {
                    if lambda.escape != RirLambdaEscape::NonEscaping {
                        self.push(site, RirVerifyErrorKind::UnsupportedLambdaCapture);
                    }
                    self.check_lambda_cell_capture_decl(
                        site,
                        capture,
                        cell,
                        RirCellStorage::StackScoped,
                    );
                }
                RirLambdaCaptureKind::HeapCell { cell } => {
                    if lambda.escape == RirLambdaEscape::Escaping && !heap_env {
                        self.push(site, RirVerifyErrorKind::UnsupportedLambdaCapture);
                    }
                    self.check_lambda_cell_capture_decl(site, capture, cell, RirCellStorage::Heap);
                }
                RirLambdaCaptureKind::ScopedPlaceCell { cell } => {
                    if capture.mode != RirPassMode::ScopedPlaceCell {
                        self.push(site, RirVerifyErrorKind::UnsupportedLambdaCapture);
                    }
                    if lambda.escape != RirLambdaEscape::NonEscaping {
                        self.push(site, RirVerifyErrorKind::UnsupportedLambdaCapture);
                    }
                    match self.check_scoped_place_cell_ref(site, RirScopedPlaceCellRef::Owner(cell))
                    {
                        Some(decl) if decl.payload_ty != capture.ty => self.push(
                            site,
                            RirVerifyErrorKind::TypeMismatch {
                                expected: decl.payload_ty,
                                found: capture.ty,
                            },
                        ),
                        Some(_) | None => {}
                    }
                }
            }
        }
    }

    fn check_lambda_cell_capture_decl(
        &mut self,
        site: RirVerifySite,
        capture: &RirLambdaCapture,
        cell: RirCellId,
        storage: RirCellStorage,
    ) {
        let mode = Self::cell_capture_mode(storage);
        if capture.mode != mode {
            self.push(site, RirVerifyErrorKind::UnsupportedLambdaCapture);
        }
        if let Some(decl) = self.check_cell_ref(site, RirCellRef::Owner(cell), storage)
            && decl.payload_ty != capture.ty
        {
            self.push(
                site,
                RirVerifyErrorKind::TypeMismatch {
                    expected: decl.payload_ty,
                    found: capture.ty,
                },
            );
        }
    }

    fn cell_capture_mode(storage: RirCellStorage) -> RirPassMode {
        match storage {
            RirCellStorage::StackScoped => RirPassMode::StackCell,
            RirCellStorage::Heap => RirPassMode::HeapCell,
        }
    }

    fn check_collection_storages(&mut self) {
        let mut seen = HashSet::new();
        for (index, storage) in self.program.collection_storages.iter().enumerate() {
            if storage.id.index() != index {
                self.push(RirVerifySite::Program, RirVerifyErrorKind::BadId);
            }
            if !seen.insert(storage.value_ty) {
                self.push(RirVerifySite::Program, RirVerifyErrorKind::BadId);
            }
            self.check_type_id(RirVerifySite::Program, storage.value_ty);
            let heap_backed = match &storage.kind {
                RirCollectionStorageKind::Array {
                    elem_ty,
                    elem_materializer,
                } => {
                    self.check_type_id(RirVerifySite::Program, *elem_ty);
                    let valid = matches!(
                        self.ty(storage.value_ty),
                        Some(RirType::Array { elem, .. }) if elem == *elem_ty
                    ) && self.materializer_matches(
                        *elem_ty,
                        *elem_materializer,
                        RustRecipePosition::StoredPayload(LambdaStorageFamily::FixedArrayElement),
                    );
                    if !valid {
                        self.push(RirVerifySite::Program, RirVerifyErrorKind::BadId);
                    }
                    false
                }
                RirCollectionStorageKind::Slice {
                    elem_ty,
                    elem_materializer,
                } => {
                    self.check_type_id(RirVerifySite::Program, *elem_ty);
                    let valid = matches!(
                        self.ty(storage.value_ty),
                        Some(RirType::Slice(elem)) if elem == *elem_ty
                    ) && self.materializer_matches(
                        *elem_ty,
                        *elem_materializer,
                        RustRecipePosition::Value,
                    );
                    if !valid {
                        self.push(RirVerifySite::Program, RirVerifyErrorKind::BadId);
                    }
                    false
                }
                RirCollectionStorageKind::List {
                    elem_ty,
                    elem_materializer,
                    symbol,
                } => {
                    self.check_type_id(RirVerifySite::Program, *elem_ty);
                    let valid = !symbol.as_str().is_empty()
                        && matches!(
                            self.ty(storage.value_ty),
                            Some(RirType::List(elem)) if elem == *elem_ty
                        )
                        && self.materializer_matches(
                            *elem_ty,
                            *elem_materializer,
                            RustRecipePosition::StoredPayload(LambdaStorageFamily::ListElement),
                        );
                    if !valid {
                        self.push(RirVerifySite::Program, RirVerifyErrorKind::BadId);
                    }
                    true
                }
                RirCollectionStorageKind::Map {
                    key_ty,
                    value_ty,
                    key_materializer,
                    value_materializer,
                    symbol,
                } => {
                    self.check_type_id(RirVerifySite::Program, *key_ty);
                    self.check_type_id(RirVerifySite::Program, *value_ty);
                    let valid = !symbol.as_str().is_empty()
                        && matches!(
                            self.ty(storage.value_ty),
                            Some(RirType::Map { key, value })
                                if key == *key_ty && value == *value_ty
                        )
                        && self.materializer_matches(
                            *key_ty,
                            *key_materializer,
                            RustRecipePosition::MapKey,
                        )
                        && self.materializer_matches(
                            *value_ty,
                            *value_materializer,
                            RustRecipePosition::StoredPayload(LambdaStorageFamily::MapValue),
                        );
                    if !valid {
                        self.push(RirVerifySite::Program, RirVerifyErrorKind::BadId);
                    }
                    true
                }
            };
            if heap_backed
                && self.ty(storage.value_ty).is_some()
                && !self.stored_payload_supported(storage.value_ty)
            {
                self.push(
                    RirVerifySite::Program,
                    RirVerifyErrorKind::UnsupportedRValueType,
                );
            }
        }
        for (index, ty) in self.program.types.iter().enumerate() {
            if matches!(
                ty,
                RirType::Array { .. } | RirType::Slice(_) | RirType::List(_) | RirType::Map { .. }
            ) && !seen.contains(&RirTypeId::from_index(index))
            {
                self.push(RirVerifySite::Program, RirVerifyErrorKind::BadId);
            }
        }
    }

    fn check_cells(&mut self) {
        for (index, cell) in self.program.cells.iter().enumerate() {
            let id = RirCellId::from_index(index);
            let site = RirVerifySite::Cell(id);
            if cell.id != id {
                self.push(site, RirVerifyErrorKind::BadId);
            }
            self.check_cell_ref(site, RirCellRef::Owner(cell.id), cell.storage);
            self.check_cell_decl(site, cell);
        }
        self.check_cell_uniqueness();
    }

    fn check_cell_decl(&mut self, site: RirVerifySite, cell: &RirCellDecl) {
        self.check_function_id(site, cell.owner);
        self.check_type_id(site, cell.payload_ty);
        if cell.symbol.as_str().is_empty() {
            self.push(site, RirVerifyErrorKind::BadId);
        }
        let Some(owner) = self.program.functions.get(cell.owner.index()) else {
            return;
        };
        if owner.locals.iter().any(|local| local.symbol == cell.symbol) {
            self.push(site, RirVerifyErrorKind::DuplicateSymbol);
        }
        match owner.locals.get(cell.source_local.index()) {
            Some(local) if local.id != cell.source_local => {
                self.push(site, RirVerifyErrorKind::BadId);
            }
            Some(local) if local.ty != cell.payload_ty => {
                self.push(
                    site,
                    RirVerifyErrorKind::TypeMismatch {
                        expected: cell.payload_ty,
                        found: local.ty,
                    },
                );
            }
            Some(local) if !local.mutable => {
                self.push(
                    site,
                    RirVerifyErrorKind::ImmutableCellSourceLocal(cell.source_local),
                );
            }
            Some(local) if !matches!(local.binding, RirLocalBinding::Value) => {
                self.push(
                    site,
                    RirVerifyErrorKind::CellSourcePayloadBinding(cell.source_local),
                );
            }
            Some(_) => {}
            None => self.push(site, RirVerifyErrorKind::BadId),
        }
    }

    fn check_cell_uniqueness(&mut self) {
        for (index, cell) in self.program.cells.iter().enumerate() {
            let id = RirCellId::from_index(index);
            for (other_index, other) in self.program.cells[..index].iter().enumerate() {
                if cell.owner == other.owner && cell.source_local == other.source_local {
                    self.push(
                        RirVerifySite::Cell(id),
                        RirVerifyErrorKind::DuplicateCell {
                            owner: cell.owner,
                            source_local: cell.source_local,
                            first: RirCellId::from_index(other_index),
                            second: id,
                        },
                    );
                }
                if cell.owner == other.owner && cell.symbol == other.symbol {
                    self.push(RirVerifySite::Cell(id), RirVerifyErrorKind::DuplicateSymbol);
                }
            }
        }
    }

    fn check_scoped_place_cells(&mut self) {
        for (index, cell) in self.program.scoped_place_cells.iter().enumerate() {
            let id = RirScopedPlaceCellId::from_index(index);
            let site = RirVerifySite::ScopedPlaceCell(id);
            if cell.id != id {
                self.push(site, RirVerifyErrorKind::BadId);
            }
            self.check_scoped_place_cell(site, cell);
        }
        self.check_scoped_place_cell_uniqueness();
    }

    fn check_scoped_place_cell(&mut self, site: RirVerifySite, cell: &RirScopedPlaceCellDecl) {
        self.check_scoped_place_cell_ref(site, RirScopedPlaceCellRef::Owner(cell.id));
        self.check_function_id(site, cell.owner);
        self.check_type_id(site, cell.payload_ty);
        if cell.symbol.as_str().is_empty() {
            self.push(site, RirVerifyErrorKind::BadId);
        }
        let Some(owner) = self.program.functions.get(cell.owner.index()) else {
            return;
        };
        if owner.locals.iter().any(|local| local.symbol == cell.symbol) {
            self.push(site, RirVerifyErrorKind::DuplicateSymbol);
        }
        let source_ty = self.check_mut_place_arg(
            site,
            cell.owner,
            owner,
            cell.source.place(),
            MutPlaceUse::ScopedPlaceSource,
        );
        if let Some(found) = source_ty
            && found != cell.payload_ty
        {
            self.push(
                site,
                RirVerifyErrorKind::TypeMismatch {
                    expected: cell.payload_ty,
                    found,
                },
            );
        }
        match &cell.source {
            RirScopedPlaceSource::SourceMutParam { place }
            | RirScopedPlaceSource::RefSelf { place } => {
                let Some(source_local) = place.root_local() else {
                    self.push(site, RirVerifyErrorKind::BadId);
                    return;
                };
                if !place.projections.is_empty() {
                    self.push(site, RirVerifyErrorKind::UnsupportedLambdaCapture);
                }
                match owner.locals.get(source_local.index()) {
                    Some(local) if local.id != source_local => {
                        self.push(site, RirVerifyErrorKind::BadId);
                    }
                    Some(_) if !Self::function_local_is_mut_place_param(owner, source_local) => {
                        self.push(site, RirVerifyErrorKind::CallArgMode);
                    }
                    Some(_) => {}
                    None => self.push(site, RirVerifyErrorKind::BadId),
                }
            }
            RirScopedPlaceSource::PatternAlias { place } => {
                let valid = match place.access {
                    RirMutPlaceAccess::Handle(RirMutPlaceHandle::Param { local }) => {
                        Self::function_local_is_mut_place_param(owner, local)
                    }
                    RirMutPlaceAccess::Handle(RirMutPlaceHandle::Local { local }) => {
                        owner.locals.get(local.index()).is_some_and(|local| {
                            matches!(
                                local.binding,
                                RirLocalBinding::DirectPayload
                                    | RirLocalBinding::ScopedPlacePayload
                            )
                        })
                    }
                    _ => false,
                };
                if !valid {
                    self.push(site, RirVerifyErrorKind::UnsupportedLambdaCapture);
                }
            }
            RirScopedPlaceSource::ForRefAlias { place } => {
                if !matches!(
                    &place.access,
                    RirMutPlaceAccess::Handle(RirMutPlaceHandle::Local { .. })
                ) {
                    self.push(site, RirVerifyErrorKind::UnsupportedLambdaCapture);
                }
            }
        }
    }

    fn check_scoped_place_cell_uniqueness(&mut self) {
        for (index, cell) in self.program.scoped_place_cells.iter().enumerate() {
            let id = RirScopedPlaceCellId::from_index(index);
            for (other_index, other) in self.program.scoped_place_cells[..index].iter().enumerate()
            {
                if cell.owner == other.owner && cell.source.place() == other.source.place() {
                    self.push(
                        RirVerifySite::ScopedPlaceCell(id),
                        RirVerifyErrorKind::DuplicateScopedPlaceCell {
                            owner: cell.owner,
                            source_local: cell
                                .source
                                .root_local()
                                .unwrap_or(RirLocalId::from_index(usize::MAX)),
                            first: RirScopedPlaceCellId::from_index(other_index),
                            second: id,
                        },
                    );
                }
                if cell.owner == other.owner && cell.symbol == other.symbol {
                    self.push(
                        RirVerifySite::ScopedPlaceCell(id),
                        RirVerifyErrorKind::DuplicateSymbol,
                    );
                }
            }
        }
    }

    fn check_cell_symbol_uniqueness(&mut self) {
        for scoped in &self.program.scoped_place_cells {
            for cell in &self.program.cells {
                if scoped.owner == cell.owner && scoped.symbol == cell.symbol {
                    self.push(
                        RirVerifySite::ScopedPlaceCell(scoped.id),
                        RirVerifyErrorKind::DuplicateSymbol,
                    );
                }
            }
        }
    }

    fn check_lambda_function_signature(&mut self, site: RirVerifySite, lambda: &RirLambda) {
        let Some(function) = self.program.functions.get(lambda.function.index()) else {
            return;
        };
        let Some(sig) = self.program.lambda_sigs.get(lambda.sig.index()) else {
            return;
        };
        let hidden_params = lambda.captures.len();
        let expected_params = hidden_params + sig.params.len();
        if function.params.len() != expected_params {
            self.push(
                site,
                RirVerifyErrorKind::CallArgCount {
                    expected: expected_params,
                    found: function.params.len(),
                },
            );
            return;
        }
        for (param, capture) in function.params.iter().zip(&lambda.captures) {
            let Some((ty, mode, escape)) = function.parameter_data(*param) else {
                self.push(site, RirVerifyErrorKind::ParamLocalMissing);
                continue;
            };
            if ty != capture.ty {
                self.push(
                    site,
                    RirVerifyErrorKind::TypeMismatch {
                        expected: capture.ty,
                        found: ty,
                    },
                );
            }
            if mode != capture.mode {
                self.push(site, RirVerifyErrorKind::CallArgMode);
            }
            if escape != RirParamEscape::NonEscaping {
                self.push(site, RirVerifyErrorKind::CallArgEscape);
            }
        }
        for (param, sig_param) in function.params.iter().skip(hidden_params).zip(&sig.params) {
            let Some((ty, mode, escape)) = function.parameter_data(*param) else {
                self.push(site, RirVerifyErrorKind::ParamLocalMissing);
                continue;
            };
            if ty != sig_param.ty {
                self.push(
                    site,
                    RirVerifyErrorKind::TypeMismatch {
                        expected: sig_param.ty,
                        found: ty,
                    },
                );
            }
            if mode != sig_param.mode {
                self.push(site, RirVerifyErrorKind::CallArgMode);
            }
            if escape != sig_param.escape {
                self.push(site, RirVerifyErrorKind::CallArgEscape);
            }
        }
        if function.ret.ty != sig.ret {
            self.push(
                site,
                RirVerifyErrorKind::TypeMismatch {
                    expected: sig.ret,
                    found: function.ret.ty,
                },
            );
        }
    }

    fn check_structs(&mut self) {
        let mut symbols = Vec::new();
        for (index, strukt) in self.program.structs.iter().enumerate() {
            let id = RirStructId::from_index(index);
            let site = RirVerifySite::Type(RirTypeId::from_index(index));
            if strukt.id != id {
                self.push(site, RirVerifyErrorKind::BadId);
            }
            if strukt.symbol.as_str().is_empty()
                || symbols.iter().any(|symbol| symbol == &strukt.symbol)
            {
                self.push(site, RirVerifyErrorKind::DuplicateSymbol);
            }
            symbols.push(strukt.symbol.clone());
            let mut field_symbols = Vec::new();
            for (field_index, field) in strukt.fields.iter().enumerate() {
                if field.id != RirFieldId::from_index(field_index) {
                    self.push(site, RirVerifyErrorKind::BadId);
                }
                if field.symbol.as_str().is_empty()
                    || field_symbols.iter().any(|symbol| symbol == &field.symbol)
                {
                    self.push(site, RirVerifyErrorKind::DuplicateSymbol);
                }
                field_symbols.push(field.symbol.clone());
                self.check_stored_payload_family(site, field.ty, LambdaStorageFamily::StructField);
            }
        }
    }

    fn check_datarefs(&mut self) {
        let mut symbols = Vec::new();
        for (index, dataref) in self.program.datarefs.iter().enumerate() {
            let id = RirDataRefId::from_index(index);
            let site = RirVerifySite::Type(RirTypeId::from_index(index));
            if dataref.id != id {
                self.push(site, RirVerifyErrorKind::BadId);
            }
            let storage_symbol = dataref.storage_symbol();
            let heap_type_symbol = dataref.heap_type_symbol();
            for symbol in [
                dataref.symbol.as_str(),
                storage_symbol.as_str(),
                heap_type_symbol.as_str(),
            ] {
                if symbol.is_empty() || symbols.iter().any(|seen| seen == symbol) {
                    self.push(site, RirVerifyErrorKind::DuplicateSymbol);
                }
                symbols.push(symbol.to_owned());
            }
            if dataref.display.as_str().is_empty() {
                self.push(site, RirVerifyErrorKind::DuplicateSymbol);
            }
            let mut field_symbols = Vec::new();
            for (field_index, field) in dataref.fields.iter().enumerate() {
                if field.id != RirFieldId::from_index(field_index) {
                    self.push(site, RirVerifyErrorKind::BadId);
                }
                if field.symbol.as_str().is_empty()
                    || field_symbols.iter().any(|symbol| symbol == &field.symbol)
                {
                    self.push(site, RirVerifyErrorKind::DuplicateSymbol);
                }
                field_symbols.push(field.symbol.clone());
                self.check_stored_payload_family(
                    site,
                    field.ty,
                    LambdaStorageFamily::DataRefProjection,
                );
            }
        }
    }

    fn check_enums(&mut self) {
        let mut symbols = Vec::new();
        for (index, enm) in self.program.enums.iter().enumerate() {
            let id = RirEnumId::from_index(index);
            let site = RirVerifySite::Type(RirTypeId::from_index(index));
            if enm.id != id {
                self.push(site, RirVerifyErrorKind::BadId);
            }
            if enm.symbol.as_str().is_empty() || symbols.iter().any(|symbol| symbol == &enm.symbol)
            {
                self.push(site, RirVerifyErrorKind::DuplicateSymbol);
            }
            symbols.push(enm.symbol.clone());
            self.check_raw_enum(site, enm);
            let mut variant_symbols = Vec::new();
            for (variant_index, variant) in enm.variants.iter().enumerate() {
                if variant.id != RirVariantId::from_index(variant_index) {
                    self.push(site, RirVerifyErrorKind::BadId);
                }
                if variant.symbol.as_str().is_empty()
                    || variant_symbols
                        .iter()
                        .any(|symbol| symbol == &variant.symbol)
                {
                    self.push(site, RirVerifyErrorKind::DuplicateSymbol);
                }
                variant_symbols.push(variant.symbol.clone());
                if variant.kind == RirVariantKind::Unit && !variant.fields.is_empty() {
                    self.push(
                        site,
                        RirVerifyErrorKind::FieldCount {
                            expected: 0,
                            found: variant.fields.len(),
                        },
                    );
                }
                let mut field_symbols = Vec::new();
                for (field_index, field) in variant.fields.iter().enumerate() {
                    if field.id != RirFieldId::from_index(field_index) {
                        self.push(site, RirVerifyErrorKind::BadId);
                    }
                    if field.symbol.as_str().is_empty()
                        || field_symbols.iter().any(|symbol| symbol == &field.symbol)
                    {
                        self.push(site, RirVerifyErrorKind::DuplicateSymbol);
                    }
                    field_symbols.push(field.symbol.clone());
                    if self.program.dyn_carrier_for_enum(enm.id).is_some() {
                        self.check_stored_payload_family(
                            site,
                            field.ty,
                            LambdaStorageFamily::DynamicPayload,
                        );
                    } else {
                        self.check_stored_payload_family(
                            site,
                            field.ty,
                            LambdaStorageFamily::EnumPayload,
                        );
                    }
                }
            }
        }
    }

    fn check_flags(&mut self) {
        let mut symbols = Vec::new();
        for (index, flag) in self.program.flags.iter().enumerate() {
            let id = RirFlagId::from_index(index);
            let site = self
                .type_id(RirType::Flag(id))
                .map_or(RirVerifySite::Program, RirVerifySite::Type);
            if flag.id != id {
                self.push(site, RirVerifyErrorKind::BadId);
            }
            if flag.symbol.as_str().is_empty()
                || symbols.iter().any(|symbol| symbol == &flag.symbol)
                || flag.display.as_str().is_empty()
            {
                self.push(site, RirVerifyErrorKind::DuplicateSymbol);
            }
            symbols.push(flag.symbol.clone());
            if flag.known_bits < 0 {
                self.push(site, RirVerifyErrorKind::FlagInvalidKnownBits);
            }

            let mut member_symbols = Vec::new();
            let mut values = HashSet::new();
            let mut known_bits = 0;
            for (member_index, member) in flag.members.iter().enumerate() {
                if member.id.index() != member_index {
                    self.push(site, RirVerifyErrorKind::BadId);
                }
                if member.symbol.as_str().is_empty()
                    || member.display.as_str().is_empty()
                    || member_symbols.iter().any(|symbol| symbol == &member.symbol)
                {
                    self.push(site, RirVerifyErrorKind::DuplicateSymbol);
                }
                member_symbols.push(member.symbol.clone());
                if member.value < 0 {
                    self.push(site, RirVerifyErrorKind::FlagInvalidValue);
                    continue;
                }
                let atomic = member.value > 0 && member.value & (member.value - 1) == 0;
                if member.atomic != atomic {
                    self.push(site, RirVerifyErrorKind::FlagAtomicMismatch);
                }
                if !atomic && member.value & !known_bits != 0 {
                    self.push(site, RirVerifyErrorKind::FlagUnknownBits);
                }
                if !values.insert(member.value) {
                    self.push(site, RirVerifyErrorKind::FlagDuplicateValue);
                }
                if atomic {
                    known_bits |= member.value;
                }
            }
            if known_bits != flag.known_bits {
                self.push(site, RirVerifyErrorKind::FlagInvalidKnownBits);
            }
        }
    }

    fn check_tuples(&mut self) {
        let mut symbols = Vec::new();
        for (index, tuple) in self.program.tuples.iter().enumerate() {
            let id = RirTupleId::from_index(index);
            let site = RirVerifySite::Type(RirTypeId::from_index(index));
            if tuple.id != id {
                self.push(site, RirVerifyErrorKind::BadId);
            }
            if tuple.symbol.as_str().is_empty()
                || symbols.iter().any(|symbol| symbol == &tuple.symbol)
            {
                self.push(site, RirVerifyErrorKind::DuplicateSymbol);
            }
            symbols.push(tuple.symbol.clone());
            if tuple.display.as_str().is_empty() {
                self.push(site, RirVerifyErrorKind::DuplicateSymbol);
            }
            let mut field_symbols = Vec::new();
            for (field_index, field) in tuple.fields.iter().enumerate() {
                if field.id != RirFieldId::from_index(field_index) {
                    self.push(site, RirVerifyErrorKind::BadId);
                }
                if field.symbol.as_str().is_empty()
                    || field_symbols.iter().any(|symbol| symbol == &field.symbol)
                {
                    self.push(site, RirVerifyErrorKind::DuplicateSymbol);
                }
                field_symbols.push(field.symbol.clone());
                self.check_stored_payload_family(site, field.ty, LambdaStorageFamily::TupleField);
            }
        }
    }

    fn check_construct_fields(
        &mut self,
        site: RirVerifySite,
        function: &RirFunction,
        ty: RirTypeId,
        expected: &[RirField],
        found: &[RirOwnedValue],
        family: LambdaStorageFamily,
    ) -> RirTypeId {
        if found.len() != expected.len() {
            self.push(
                site,
                RirVerifyErrorKind::FieldCount {
                    expected: expected.len(),
                    found: found.len(),
                },
            );
        }
        for (field, owned) in expected.iter().zip(found) {
            self.check_owned_value_ty(
                site,
                function,
                owned,
                field.ty,
                RustRecipePosition::StoredPayload(family),
            );
        }
        ty
    }

    fn check_raw_enum(&mut self, site: RirVerifySite, enm: &RirEnum) {
        match enm.repr {
            RirEnumRepr::Adt => {
                if enm.raw_type.is_some()
                    || enm
                        .variants
                        .iter()
                        .any(|variant| variant.raw_value.is_some())
                {
                    self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                }
            }
            RirEnumRepr::RawInt | RirEnumRepr::RawString => {
                let Some(raw_type) = enm.raw_type else {
                    self.push(site, RirVerifyErrorKind::RawEnumMissingRawType);
                    return;
                };
                self.check_type_id(site, raw_type);
                if Some(raw_type) != self.raw_enum_primitive_type(enm.repr) {
                    self.push(site, RirVerifyErrorKind::RawEnumWrongRawType);
                }
                let mut raw_values = HashSet::new();
                for variant in &enm.variants {
                    if variant.kind != RirVariantKind::Unit || !variant.fields.is_empty() {
                        self.push(site, RirVerifyErrorKind::RawEnumPayload);
                    }
                    match (enm.repr, variant.raw_value.as_ref()) {
                        (RirEnumRepr::RawInt, Some(raw @ RirRawEnumValue::Int(_))) => {
                            if !raw_values.insert(raw) {
                                self.push(site, RirVerifyErrorKind::RawEnumDuplicateValue);
                            }
                        }
                        (RirEnumRepr::RawString, Some(raw @ RirRawEnumValue::String(id))) => {
                            self.check_string_literal_id(site, *id);
                            if !raw_values.insert(raw) {
                                self.push(site, RirVerifyErrorKind::RawEnumDuplicateValue);
                            }
                        }
                        (_, Some(_)) => self.push(site, RirVerifyErrorKind::RawEnumWrongValue),
                        (_, None) => self.push(site, RirVerifyErrorKind::RawEnumMissingValue),
                    }
                }
            }
        }
    }

    fn raw_enum_primitive_type(&self, repr: RirEnumRepr) -> Option<RirTypeId> {
        match repr {
            RirEnumRepr::Adt => None,
            RirEnumRepr::RawInt => self.type_id(RirType::Int),
            RirEnumRepr::RawString => self.type_id(RirType::String),
        }
    }

    fn check_stringify_helpers(&mut self) {
        let mut symbols = Vec::new();
        let mut tys = Vec::new();
        let mut req_tys = HashSet::new();
        let mut helper_refs = vec![0; self.program.stringify_helpers.len()];
        for (index, req) in self.program.stringify_reqs.iter().enumerate() {
            let site = RirVerifySite::Type(req.ty);
            if req.id.index() != index {
                self.push(site, RirVerifyErrorKind::BadId);
            }
            self.check_type_id(site, req.ty);
            if !req_tys.insert(req.ty) {
                self.push(site, RirVerifyErrorKind::DuplicateStringifyReq);
            }
            match (req.kind, self.ty(req.ty)) {
                (
                    RirStringifyReqKind::Helper(helper),
                    Some(RirType::Struct(_) | RirType::Enum(_) | RirType::Flag(_)),
                ) => {
                    self.check_stringify_helper_id(site, helper);
                    if let Some(count) = helper_refs.get_mut(helper.index()) {
                        *count += 1;
                    }
                    if self
                        .program
                        .stringify_helpers
                        .get(helper.index())
                        .is_none_or(|helper| helper.ty != req.ty)
                    {
                        self.push(
                            site,
                            RirVerifyErrorKind::TypeMismatch {
                                expected: req.ty,
                                found: self
                                    .program
                                    .stringify_helpers
                                    .get(helper.index())
                                    .map_or(req.ty, |helper| helper.ty),
                            },
                        );
                    }
                }
                (RirStringifyReqKind::Override { function, mode }, Some(RirType::Struct(_))) => {
                    self.check_function_id(site, function);
                    let Some(function) = self.program.functions.get(function.index()) else {
                        continue;
                    };
                    if function.ret.ty != self.type_id(RirType::String).unwrap_or(req.ty) {
                        self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                    }
                    if function.params.len() != 1 {
                        self.push(
                            site,
                            RirVerifyErrorKind::CallArgCount {
                                expected: 1,
                                found: function.params.len(),
                            },
                        );
                        continue;
                    }
                    let param = &function.params[0];
                    let Some((ty, param_mode, _)) = function.parameter_data(*param) else {
                        self.push(site, RirVerifyErrorKind::ParamLocalMissing);
                        continue;
                    };
                    if ty != req.ty || param_mode != mode {
                        self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                    }
                    if !matches!(mode, RirPassMode::Value | RirPassMode::SharedBorrow) {
                        self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                    }
                    if mode == RirPassMode::Value && !self.copyable_type(req.ty) {
                        self.push(site, RirVerifyErrorKind::NonCopyValueRequired);
                    }
                }
                _ => self.push(site, RirVerifyErrorKind::UnsupportedRValueType),
            }
        }
        for (index, helper) in self.program.stringify_helpers.iter().enumerate() {
            let site = RirVerifySite::Type(helper.ty);
            if helper_refs[index] != 1 {
                self.push(
                    site,
                    RirVerifyErrorKind::InvalidStringifyHelperReferenceCount,
                );
            }
            if helper.id != RirStringifyHelperId::from_index(index) {
                self.push(site, RirVerifyErrorKind::BadId);
            }
            self.check_type_id(site, helper.ty);
            if helper.symbol.as_str().is_empty()
                || symbols.iter().any(|symbol| symbol == &helper.symbol)
            {
                self.push(site, RirVerifyErrorKind::DuplicateSymbol);
            }
            symbols.push(helper.symbol.clone());
            if tys.contains(&helper.ty) {
                self.push(site, RirVerifyErrorKind::DuplicateSymbol);
            }
            tys.push(helper.ty);
            let valid = match (&helper.kind, self.ty(helper.ty)) {
                (RirStringifyHelperKind::Struct(kind_id), Some(RirType::Struct(struct_id)))
                    if *kind_id == struct_id =>
                {
                    self.program
                        .structs
                        .get(struct_id.index())
                        .is_some_and(|strukt| {
                            strukt
                                .fields
                                .iter()
                                .all(|field| self.stringify_field_supported(field.ty))
                        })
                }
                (
                    RirStringifyHelperKind::Enum { enm, variants },
                    Some(RirType::Enum(type_enum)),
                ) if *enm == type_enum => self.program.enums.get(enm.index()).is_some_and(|enm| {
                    variants.len() == enm.variants.len()
                        && variants.iter().zip(&enm.variants).all(|(plan, variant)| {
                            self.program
                                .string_literals
                                .get(plan.label.index())
                                .is_some()
                                && plan.field_labels.len()
                                    == if variant.kind == RirVariantKind::Struct {
                                        variant.fields.len()
                                    } else {
                                        0
                                    }
                                && plan.field_labels.iter().all(|id| {
                                    self.program.string_literals.get(id.index()).is_some()
                                })
                                && variant
                                    .fields
                                    .iter()
                                    .all(|field| self.stringify_field_supported(field.ty))
                        })
                }),
                (
                    RirStringifyHelperKind::Flag {
                        flag,
                        empty,
                        members,
                    },
                    Some(RirType::Flag(type_flag)),
                ) if *flag == type_flag => {
                    self.program.flags.get(flag.index()).is_some_and(|flag| {
                        members.len() == flag.members.len()
                            && self.program.string_literals.get(empty.index()).is_some()
                            && members
                                .iter()
                                .all(|id| self.program.string_literals.get(id.index()).is_some())
                    })
                }
                _ => false,
            };
            if !valid {
                self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
            }
        }
    }

    fn check_const(&mut self, id: RirConstId, konst: &RirConst) {
        self.check_type_id(RirVerifySite::Const(id), konst.ty);
        if matches!(self.ty(konst.ty), Some(RirType::Void)) {
            self.push(RirVerifySite::Const(id), RirVerifyErrorKind::VoidConst);
        }
        let flag_ok = match (self.ty(konst.ty), &konst.value) {
            (Some(RirType::Flag(ty_flag)), RirConstValue::Flag { flag, bits })
                if ty_flag == *flag =>
            {
                self.program
                    .flags
                    .get(flag.index())
                    .is_some_and(|decl| *bits >= 0 && *bits & !decl.known_bits == 0)
            }
            _ => false,
        };
        let ok = flag_ok
            || matches!(
                (self.ty(konst.ty), &konst.value),
                (Some(RirType::Int), RirConstValue::Int(_))
                    | (Some(RirType::Float), RirConstValue::Float(_))
                    | (Some(RirType::Bool), RirConstValue::Bool(_))
                    | (Some(RirType::String), RirConstValue::String(_))
                    | (Some(RirType::Char), RirConstValue::Char(_))
                    | (Some(RirType::Option(_)), RirConstValue::Nil)
            );
        if !ok {
            self.push(
                RirVerifySite::Const(id),
                RirVerifyErrorKind::ConstTypeMismatch,
            );
        }
        if let RirConstValue::String(literal) = konst.value {
            self.check_string_literal_id(RirVerifySite::Const(id), literal);
        }
    }

    fn check_extern(&mut self, id: RirExternId, ext: &RirExtern, retained_callbacks: bool) {
        self.check_type_id(RirVerifySite::Extern(id), ext.ret);
        self.check_extern_signature(id, ext, retained_callbacks);
        for param in &ext.params {
            self.check_type_id(RirVerifySite::Extern(id), param.ty);
            self.check_pass_mode(RirVerifySite::Extern(id), param.ty, param.mode);
        }
    }

    fn check_extern_signature(
        &mut self,
        id: RirExternId,
        ext: &RirExtern,
        retained_callbacks: bool,
    ) {
        if !native_extern_signature_ok(self.program, ext, retained_callbacks) {
            self.push(
                RirVerifySite::Extern(id),
                RirVerifyErrorKind::UnsupportedRValueType,
            );
        }
    }

    fn check_function(&mut self, id: RirFunctionId, function: &RirFunction) {
        self.check_type_id(RirVerifySite::Function(id), function.ret.ty);
        let mut seen_params = vec![false; function.locals.len()];
        for (index, param) in function.params.iter().enumerate() {
            let site = RirVerifySite::Param(id, index);
            self.check_local_id(site, function, *param);
            let Some(local) = function.locals.get(param.index()) else {
                self.push(site, RirVerifyErrorKind::ParamLocalMissing);
                continue;
            };
            if std::mem::replace(&mut seen_params[param.index()], true) {
                self.push(site, RirVerifyErrorKind::BadId);
            }
            let RirLocalBinding::Parameter { mode, escape } = local.binding else {
                self.push(site, RirVerifyErrorKind::ParamLocalNotInitialized);
                continue;
            };
            self.check_type_id(site, local.ty);
            self.check_pass_mode(site, local.ty, mode);
            if mode == RirPassMode::DynBorrow && escape != RirParamEscape::NonEscaping {
                self.push(site, RirVerifyErrorKind::CallArgEscape);
            }
            for storage in [RirCellStorage::StackScoped, RirCellStorage::Heap] {
                if mode == Self::cell_capture_mode(storage)
                    && !self.function_param_is_cell_capture(id, index, *param, storage)
                {
                    self.push(site, RirVerifyErrorKind::UnsupportedAbi);
                }
            }
            if matches!(
                mode,
                RirPassMode::ScopedLambda | RirPassMode::EscapingLambda | RirPassMode::AnvCallback
            ) {
                self.push(site, RirVerifyErrorKind::UnsupportedAbi);
            }
            if mode == RirPassMode::ScopedPlaceCell
                && !self.function_param_is_scoped_place_cell_capture(id, index, *param)
            {
                self.push(site, RirVerifyErrorKind::UnsupportedAbi);
            }
        }
        for local in &function.locals {
            if matches!(local.binding, RirLocalBinding::Parameter { .. })
                && !seen_params[local.id.index()]
            {
                self.push(
                    RirVerifySite::Local(id, local.id),
                    RirVerifyErrorKind::ParamLocalMissing,
                );
            }
        }
        for (index, local) in function.locals.iter().enumerate() {
            let local_id = RirLocalId::from_index(index);
            if local.id != local_id {
                self.push(
                    RirVerifySite::Local(id, local_id),
                    RirVerifyErrorKind::BadId,
                );
            }
            self.check_type_id(RirVerifySite::Local(id, local_id), local.ty);
        }
        let previous_initialized = std::mem::take(&mut self.initialized);
        let previous_possible = std::mem::take(&mut self.possibly_initialized);
        let previous_payload_bound = std::mem::take(&mut self.payload_bound);
        let previous_lambda_escapes = std::mem::take(&mut self.lambda_escapes);
        let previous_lambda_values = std::mem::take(&mut self.lambda_values);
        let previous_loop_lambda_scopes = std::mem::take(&mut self.loop_lambda_scopes);
        let previous_local_decl_scopes = std::mem::take(&mut self.local_decl_scopes);
        let previous_initialized_cells = std::mem::take(&mut self.initialized_cells);
        let previous_possible_cells = std::mem::take(&mut self.possibly_initialized_cells);
        let previous_global_initialized = std::mem::take(&mut self.global_initialized);
        let previous_global_values = std::mem::take(&mut self.global_values);
        let previous_scope_depth = self.scope_depth;
        self.initialized = function
            .locals
            .iter()
            .map(|local| matches!(local.binding, RirLocalBinding::Parameter { .. }))
            .collect();
        self.possibly_initialized.clone_from(&self.initialized);
        self.payload_bound = vec![false; function.locals.len()];
        self.lambda_escapes = vec![None; function.locals.len()];
        self.lambda_values = function
            .locals
            .iter()
            .map(|local| self.type_function_value_state(local.ty))
            .collect();
        self.loop_lambda_scopes = vec![None; function.locals.len()];
        self.local_decl_scopes = function
            .locals
            .iter()
            .map(|local| matches!(local.binding, RirLocalBinding::Parameter { .. }).then_some(0))
            .collect();
        self.initialized_cells = vec![false; self.program.cells.len()];
        self.possibly_initialized_cells = vec![false; self.program.cells.len()];
        self.global_initialized = vec![false; self.program.globals.len()];
        self.global_values = self
            .program
            .globals
            .iter()
            .map(|global| self.type_function_value_state(global.ty))
            .collect();
        self.scope_depth = 0;
        for lambda in self
            .program
            .lambdas
            .iter()
            .filter(|lambda| lambda.function == id)
        {
            for capture in &lambda.captures {
                match capture.kind {
                    RirLambdaCaptureKind::StackCell { cell }
                    | RirLambdaCaptureKind::HeapCell { cell } => self.mark_cell_initialized(cell),
                    RirLambdaCaptureKind::Param | RirLambdaCaptureKind::ScopedPlaceCell { .. } => {}
                }
            }
        }
        for local in &function.locals {
            let Some((_, escape)) = local.binding.parameter() else {
                continue;
            };
            let value = if matches!(self.ty(local.ty), Some(RirType::Lambda(_))) {
                let escape = Some(RirLambdaEscape::from_param_escape(escape));
                self.lambda_escapes[local.id.index()] = escape;
                RirFunctionValueState::Lambda(escape)
            } else {
                self.source_call_return_state(local.ty)
            };
            self.lambda_values[local.id.index()] = value;
        }
        for (stmt_index, stmt) in function.body.stmts.iter().enumerate() {
            self.check_stmt(id, function, stmt_index, stmt);
        }
        self.check_term(id, function, &function.body.term);
        for local in &function.locals {
            if matches!(
                local.binding,
                RirLocalBinding::DirectPayload | RirLocalBinding::ScopedPlacePayload
            ) && !self
                .payload_bound
                .get(local.id.index())
                .copied()
                .unwrap_or(false)
            {
                self.push(
                    RirVerifySite::Local(id, local.id),
                    RirVerifyErrorKind::PayloadBindingWithoutProducer,
                );
            }
        }
        if self.structured_block_falls_through(&function.body)
            && !matches!(self.ty(function.ret.ty), Some(RirType::Void))
        {
            self.push(
                RirVerifySite::Terminator(id),
                RirVerifyErrorKind::ReturnValueRequired,
            );
        }
        self.initialized = previous_initialized;
        self.possibly_initialized = previous_possible;
        self.payload_bound = previous_payload_bound;
        self.lambda_escapes = previous_lambda_escapes;
        self.lambda_values = previous_lambda_values;
        self.loop_lambda_scopes = previous_loop_lambda_scopes;
        self.local_decl_scopes = previous_local_decl_scopes;
        self.initialized_cells = previous_initialized_cells;
        self.possibly_initialized_cells = previous_possible_cells;
        self.global_initialized = previous_global_initialized;
        self.global_values = previous_global_values;
        self.scope_depth = previous_scope_depth;
    }

    fn check_dyn_match(
        &mut self,
        function_id: RirFunctionId,
        function: &RirFunction,
        site: RirVerifySite,
        match_: &RirDynMatch,
    ) {
        let Some(carrier) = self.program.dyn_carriers.get(match_.carrier.index()) else {
            self.push(site, RirVerifyErrorKind::InvalidDynCarrier);
            return;
        };
        let (mutable, borrowed) = match &match_.source {
            RirDynMatchSource::Owned(value) => {
                if self.check_owned_value(site, function, value, RustRecipePosition::Value)
                    != Some(carrier.storage_ty)
                {
                    self.push(site, RirVerifyErrorKind::InvalidDynStorage);
                }
                (false, false)
            }
            RirDynMatchSource::MutPlace(source) => {
                let ty = self.check_mut_place_arg(
                    site,
                    function_id,
                    function,
                    source,
                    MutPlaceUse::CallArg,
                );
                if ty != Some(carrier.storage_ty) {
                    self.push(site, RirVerifyErrorKind::InvalidDynStorage);
                }
                (true, false)
            }
            RirDynMatchSource::Borrowed(borrow) => {
                self.check_dyn_borrow(site, function_id, function, borrow);
                if borrow.target != match_.carrier {
                    self.push(site, RirVerifyErrorKind::InvalidDynCarrier);
                }
                let aliases = match_
                    .arms
                    .iter()
                    .any(|arm| matches!(arm.binding, RirDynMatchBinding::Alias(_)));
                (aliases, true)
            }
        };
        let expected_mappings = std::iter::once(carrier.id)
            .chain(
                self.program
                    .dyn_weakenings
                    .iter()
                    .filter(|weakening| weakening.target == carrier.id)
                    .map(|weakening| weakening.source),
            )
            .collect::<std::collections::BTreeSet<_>>();
        let mappings_valid = match_.mappings.first().is_some_and(|mapping| {
            mapping.carrier == carrier.id
                && match_.mappings.len() == expected_mappings.len()
                && match_.mappings.iter().all(|mapping| {
                    mapping.variants.len() == match_.arms.len()
                        && self
                            .program
                            .dyn_carriers
                            .get(mapping.carrier.index())
                            .is_some_and(|mapped| {
                                mapping.targets.len() == mapped.variants.len()
                                    && mapping.targets.iter().enumerate().all(|(index, target)| {
                                        let variant = RirDynVariantId::new(mapped.id, index);
                                        let expected = mapping.variants.iter().position(|set| {
                                            self.program
                                                .dyn_variant_sets
                                                .get(set.index())
                                                .is_some_and(|set| set.variants.contains(&variant))
                                        });
                                        *target == expected
                                    })
                            })
                })
                && match_
                    .mappings
                    .iter()
                    .map(|mapping| mapping.carrier)
                    .collect::<std::collections::BTreeSet<_>>()
                    == expected_mappings
        });
        if !mappings_valid {
            self.push(site, RirVerifyErrorKind::InvalidDynVariant);
        }
        let direct_mapping = match_.mappings.first();
        let entry = self.block_entry_state();
        let mut states = vec![];
        let mut seen = vec![];
        for (arm_index, arm) in match_.arms.iter().enumerate() {
            let Some(set) = direct_mapping
                .and_then(|mapping| mapping.variants.get(arm_index))
                .and_then(|id| self.program.dyn_variant_sets.get(id.index()))
            else {
                self.push(site, RirVerifyErrorKind::InvalidDynVariant);
                continue;
            };
            let target = set.target;
            self.check_type_id(site, target);
            if set.carrier != carrier.id
                || set.variants.iter().any(|variant| seen.contains(variant))
                || match_.mappings.iter().any(|mapping| {
                    mapping
                        .variants
                        .get(arm_index)
                        .and_then(|id| self.program.dyn_variant_sets.get(id.index()))
                        .is_none_or(|mapped| {
                            mapped.carrier != mapping.carrier || mapped.target != target
                        })
                })
            {
                self.push(site, RirVerifyErrorKind::InvalidDynVariant);
            }
            seen.extend(&set.variants);
            let binding = match arm.binding {
                RirDynMatchBinding::Discard => None,
                RirDynMatchBinding::Take(local) if !mutable && !borrowed => {
                    self.check_dyn_match_local(site, function, local, target, false, false);
                    Some(local)
                }
                RirDynMatchBinding::Materialize {
                    local,
                    materializer,
                } if !mutable && borrowed => {
                    self.check_dyn_match_local(site, function, local, target, false, false);
                    if !self.canonical_value_materializer(target, materializer) {
                        self.push(site, RirVerifyErrorKind::BadId);
                    }
                    Some(local)
                }
                RirDynMatchBinding::Alias(local) if mutable => {
                    self.check_dyn_match_local(site, function, local, target, true, false);
                    if let Some(slot) = self.payload_bound.get_mut(local.index()) {
                        *slot = true;
                    }
                    Some(local)
                }
                RirDynMatchBinding::Take(local)
                | RirDynMatchBinding::Materialize { local, .. }
                | RirDynMatchBinding::Alias(local) => {
                    self.push(site, RirVerifyErrorKind::InvalidDynPlan);
                    Some(local)
                }
            };
            let mut arm_entry = entry.clone();
            Self::init_entry_locals(&mut arm_entry, binding);
            if let Some(binding) = binding {
                let value = self.source_call_return_state(target);
                arm_entry.lambda_escapes[binding.index()] = value.escape();
                arm_entry.lambda_values[binding.index()] = value;
            }
            let mut state =
                self.check_structured_block(function_id, function, &arm.block, arm_entry, None);
            if let (Some(state), Some(binding)) = (&mut state, binding) {
                state.definite[binding.index()] = false;
                state.possible[binding.index()] = false;
            }
            states.push(state);
        }
        let fallback_binding = match_.fallback_binding;
        let binding = fallback_binding.local();
        match fallback_binding {
            RirDynMatchFallbackBinding::Discard => {}
            RirDynMatchFallbackBinding::Take(local) if !mutable && !borrowed => {
                self.check_dyn_match_local(site, function, local, carrier.storage_ty, false, false);
            }
            RirDynMatchFallbackBinding::Preserve(local) if borrowed => {
                self.check_dyn_match_local(site, function, local, carrier.storage_ty, false, false);
            }
            RirDynMatchFallbackBinding::Alias(local) if mutable && !borrowed => {
                self.check_dyn_match_local(site, function, local, carrier.storage_ty, true, false);
                if let Some(slot) = self.payload_bound.get_mut(local.index()) {
                    *slot = true;
                }
            }
            RirDynMatchFallbackBinding::Take(local)
            | RirDynMatchFallbackBinding::Preserve(local)
            | RirDynMatchFallbackBinding::Alias(local) => {
                self.push(site, RirVerifyErrorKind::InvalidDynPlan);
                self.check_dyn_match_local(site, function, local, carrier.storage_ty, false, false);
            }
        }
        let mut fallback_entry = entry;
        Self::init_entry_locals(&mut fallback_entry, binding);
        let reborrow = matches!(fallback_binding, RirDynMatchFallbackBinding::Preserve(_));
        if let RirDynMatchFallbackBinding::Preserve(local) = fallback_binding {
            self.active_dyn_reborrows.push((local, match_.carrier));
        }
        let mut fallback = self.check_structured_block(
            function_id,
            function,
            &match_.fallback,
            fallback_entry,
            None,
        );
        if reborrow {
            self.active_dyn_reborrows.pop();
        }
        if let (Some(state), Some(binding)) = (&mut fallback, binding) {
            state.definite[binding.index()] = false;
            state.possible[binding.index()] = false;
        }
        states.push(fallback);
        self.merge_structured_states(states);
    }

    fn check_dyn_match_local(
        &mut self,
        site: RirVerifySite,
        function: &RirFunction,
        local: RirLocalId,
        ty: RirTypeId,
        borrows_payload: bool,
        invalid_mode: bool,
    ) {
        self.check_local_id(site, function, local);
        if invalid_mode
            || !function.locals.get(local.index()).is_some_and(|local| {
                local.ty == ty
                    && matches!(local.binding, RirLocalBinding::ScopedPlacePayload)
                        == borrows_payload
                    && (!borrows_payload || local.mutable)
            })
        {
            self.push(site, RirVerifyErrorKind::OptionPayloadRefLocalMismatch);
        }
    }

    fn check_pattern_match(
        &mut self,
        function_id: RirFunctionId,
        function: &RirFunction,
        site: RirVerifySite,
        match_: &RirPatternMatch,
    ) {
        self.check_place(site, function, &match_.subject);
        if Self::pattern_match_has_alias_binding(match_) {
            let RirPlaceRoot::Local(local) = match_.subject.root else {
                self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                return;
            };
            if !function
                .locals
                .get(local.index())
                .is_some_and(|local| local.mutable)
            {
                self.push(site, RirVerifyErrorKind::ImmutableAssign);
            }
        }
        let Some(subject_ty) = self.program.place_ty(function, &match_.subject) else {
            return;
        };
        let subject_value = self.place_function_value_state(function, &match_.subject);
        let entry = self.block_entry_state();
        let mut states = vec![];
        for arm in &match_.arms {
            if arm.alternatives.is_empty() {
                self.push(site, RirVerifyErrorKind::PatternAlternativeRequired);
            }
            let mut arm_entries = vec![];
            let mut expected_bindings: Option<Vec<RirPatternBindingSignature>> = None;
            for alternative in &arm.alternatives {
                let mut alternative_entry = entry.clone();
                let bindings = self.check_pattern_alternative(
                    site,
                    function,
                    subject_ty,
                    &subject_value,
                    alternative,
                    &entry,
                    &mut alternative_entry,
                );
                match &expected_bindings {
                    Some(expected) if expected != &bindings => {
                        self.push(site, RirVerifyErrorKind::PatternBindingMismatch);
                    }
                    Some(_) => {}
                    None => expected_bindings = Some(bindings),
                }
                arm_entries.push(alternative_entry);
            }
            let mut arm_entry = arm_entries.pop().unwrap_or_else(|| entry.clone());
            for alternative in arm_entries {
                arm_entry.merge_with(alternative);
            }
            let mut state =
                self.check_structured_block(function_id, function, &arm.block, arm_entry, None);
            Self::clear_pattern_binding_locals(&mut state, arm);
            states.push(state);
        }
        self.merge_structured_states(states);
    }

    fn clear_pattern_binding_locals(state: &mut Option<RirBlockState>, arm: &RirPatternArm) {
        let Some(state) = state else {
            return;
        };
        for alternative in &arm.alternatives {
            for binding in &alternative.bindings {
                if binding.local.index() < state.definite.len() {
                    state.definite[binding.local.index()] = false;
                    state.possible[binding.local.index()] = false;
                    state.lambda_escapes[binding.local.index()] = None;
                    state.lambda_values[binding.local.index()] = RirFunctionValueState::Unknown;
                    state.loop_lambda_scopes[binding.local.index()] = None;
                }
            }
        }
    }

    fn pattern_binding_function_value_state(
        subject: &RirFunctionValueState,
        path: &RirPatternPath,
    ) -> RirFunctionValueState {
        let mut value = subject.clone();
        for step in &path.steps {
            value = match step {
                RirPatternPathStep::Place(step) => match step.kind {
                    RirPlaceStepKind::StructField(field)
                    | RirPlaceStepKind::DataRefField(field)
                    | RirPlaceStepKind::ExternField(field)
                    | RirPlaceStepKind::TupleField(field) => value.project_field(field),
                    RirPlaceStepKind::ArrayIndex { .. }
                    | RirPlaceStepKind::ListIndex { .. }
                    | RirPlaceStepKind::SliceIndex { .. } => RirFunctionValueState::Unknown,
                },
                RirPatternPathStep::OptionalSome { .. } => {
                    value.project_field(RirFieldId::from_index(0))
                }
                RirPatternPathStep::EnumTupleField { variant, field, .. }
                | RirPatternPathStep::EnumStructField { variant, field, .. } => {
                    value.variant_field(*variant, *field)
                }
            };
        }
        value
    }

    fn check_pattern_alternative(
        &mut self,
        site: RirVerifySite,
        function: &RirFunction,
        subject_ty: RirTypeId,
        subject_value: &RirFunctionValueState,
        alternative: &RirPatternAlternative,
        entry: &RirBlockEntryState,
        arm_entry: &mut RirBlockEntryState,
    ) -> Vec<RirPatternBindingSignature> {
        let guards = Self::pattern_variant_guards(alternative);
        self.check_pattern_variant_guards(site, &guards);
        let mut optional_guards = Vec::new();
        for test in &alternative.tests {
            self.check_pattern_test(site, subject_ty, &guards, &optional_guards, test);
            if let RirPatternTest::OptionalSome { path } = test {
                optional_guards.push(path.steps.clone());
            }
        }
        let mut bindings = Vec::new();
        for binding in &alternative.bindings {
            if bindings
                .iter()
                .any(|signature: &RirPatternBindingSignature| signature.local == binding.local)
            {
                self.push(site, RirVerifyErrorKind::PatternBindingMismatch);
            }
            self.check_pattern_binding(
                site,
                function,
                subject_ty,
                &guards,
                &optional_guards,
                binding,
                entry,
            );
            if let Some(slot) = arm_entry.definite.get_mut(binding.local.index()) {
                *slot = true;
            }
            if let Some(slot) = arm_entry.possible.get_mut(binding.local.index()) {
                *slot = true;
            }
            let value = Self::pattern_binding_function_value_state(subject_value, &binding.path);
            if let Some(slot) = arm_entry.lambda_escapes.get_mut(binding.local.index()) {
                *slot = value.escape();
            }
            if let Some(slot) = arm_entry.lambda_values.get_mut(binding.local.index()) {
                *slot = value;
            }
            if let Some(slot) = arm_entry.loop_lambda_scopes.get_mut(binding.local.index()) {
                *slot = None;
            }
            bindings.push(RirPatternBindingSignature {
                local: binding.local,
                ty: binding.ty,
                mode: binding.mode,
            });
        }
        bindings.sort_by_key(|signature| signature.sort_key());
        bindings
    }

    fn check_pattern_variant_guards(
        &mut self,
        site: RirVerifySite,
        guards: &[(Vec<RirPatternPathStep>, RirEnumId, RirVariantId)],
    ) {
        for (index, (path, enum_id, variant)) in guards.iter().enumerate() {
            if guards[..index]
                .iter()
                .any(|(seen_path, seen_enum, seen_variant)| {
                    seen_path == path && (seen_enum != enum_id || seen_variant != variant)
                })
            {
                self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
            }
        }
    }

    fn pattern_disjunction_test_supported(test: &RirPatternTest) -> bool {
        match test {
            RirPatternTest::Any { branches } => branches
                .iter()
                .flatten()
                .all(Self::pattern_disjunction_test_supported),
            RirPatternTest::Literal { path, .. } | RirPatternTest::FlagValue { path, .. } => {
                path.steps.iter().all(|step| {
                    matches!(
                        step,
                        RirPatternPathStep::Place(RirPlaceStep {
                            kind: RirPlaceStepKind::StructField(_)
                                | RirPlaceStepKind::TupleField(_),
                            ..
                        })
                    )
                })
            }
            RirPatternTest::Nil { .. }
            | RirPatternTest::OptionalSome { .. }
            | RirPatternTest::EnumVariant { .. } => false,
        }
    }

    fn check_pattern_test(
        &mut self,
        site: RirVerifySite,
        subject_ty: RirTypeId,
        guards: &[(Vec<RirPatternPathStep>, RirEnumId, RirVariantId)],
        optional_guards: &[Vec<RirPatternPathStep>],
        test: &RirPatternTest,
    ) {
        match test {
            RirPatternTest::Any { branches } => {
                if branches.is_empty()
                    || branches
                        .iter()
                        .flatten()
                        .any(|test| !Self::pattern_disjunction_test_supported(test))
                {
                    self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                    return;
                }
                for tests in branches {
                    let alternative = RirPatternAlternative {
                        tests: tests.clone(),
                        bindings: vec![],
                    };
                    let guards = Self::pattern_variant_guards(&alternative);
                    self.check_pattern_variant_guards(site, &guards);
                    let mut optional_guards = vec![];
                    for test in tests {
                        self.check_pattern_test(site, subject_ty, &guards, &optional_guards, test);
                        if let RirPatternTest::OptionalSome { path } = test {
                            optional_guards.push(path.steps.clone());
                        }
                    }
                }
            }
            RirPatternTest::Literal { path, value } => {
                self.check_const_id(site, *value);
                let Some(path_ty) =
                    self.pattern_path_ty(site, subject_ty, guards, optional_guards, path)
                else {
                    return;
                };
                if let Some(konst) = self.program.consts.get(value.index())
                    && konst.ty != path_ty
                {
                    self.push(
                        site,
                        RirVerifyErrorKind::TypeMismatch {
                            expected: path_ty,
                            found: konst.ty,
                        },
                    );
                }
            }
            RirPatternTest::Nil { path } | RirPatternTest::OptionalSome { path } => {
                if let Some(path_ty) =
                    self.pattern_path_ty(site, subject_ty, guards, optional_guards, path)
                    && !matches!(self.ty(path_ty), Some(RirType::Option(_)))
                {
                    self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                }
            }
            RirPatternTest::FlagValue { path, flag, bits } => {
                let Some(flag_decl) = self.program.flags.get(flag.index()) else {
                    self.push(site, RirVerifyErrorKind::BadId);
                    return;
                };
                let Some(path_ty) =
                    self.pattern_path_ty(site, subject_ty, guards, optional_guards, path)
                else {
                    return;
                };
                if !matches!(self.ty(path_ty), Some(RirType::Flag(found)) if found == *flag) {
                    self.push(site, RirVerifyErrorKind::FlagPatternTypeMismatch);
                }
                if *bits < 0 || *bits & !flag_decl.known_bits != 0 {
                    self.push(site, RirVerifyErrorKind::FlagPatternUnknownBits);
                }
            }
            RirPatternTest::EnumVariant {
                path,
                enum_id,
                variant,
            } => {
                let Some(path_ty) =
                    self.pattern_path_ty(site, subject_ty, guards, optional_guards, path)
                else {
                    return;
                };
                if !self.variant_belongs_to_enum(*enum_id, *variant) {
                    self.push(site, RirVerifyErrorKind::BadId);
                }
                if !matches!(self.ty(path_ty), Some(RirType::Enum(found)) if found == *enum_id) {
                    self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                }
            }
        }
    }

    fn check_pattern_binding(
        &mut self,
        site: RirVerifySite,
        function: &RirFunction,
        subject_ty: RirTypeId,
        guards: &[(Vec<RirPatternPathStep>, RirEnumId, RirVariantId)],
        optional_guards: &[Vec<RirPatternPathStep>],
        binding: &RirPatternBinding,
        entry: &RirBlockEntryState,
    ) {
        self.check_local_id(site, function, binding.local);
        if matches!(
            function
                .locals
                .get(binding.local.index())
                .map(|local| local.binding),
            Some(RirLocalBinding::Parameter { .. })
        ) || entry
            .possible
            .get(binding.local.index())
            .copied()
            .unwrap_or(false)
        {
            self.push(site, RirVerifyErrorKind::InitParamLocal);
        }
        let Some(path_ty) =
            self.pattern_path_ty(site, subject_ty, guards, optional_guards, &binding.path)
        else {
            return;
        };
        if path_ty != binding.ty {
            self.push(
                site,
                RirVerifyErrorKind::TypeMismatch {
                    expected: path_ty,
                    found: binding.ty,
                },
            );
        }
        if let Some(local) = function.locals.get(binding.local.index()) {
            if local.ty != binding.ty {
                self.push(
                    site,
                    RirVerifyErrorKind::TypeMismatch {
                        expected: local.ty,
                        found: binding.ty,
                    },
                );
            }
            match binding.mode {
                RirPatternBindingMode::Alias
                    if !local.mutable
                        || !matches!(local.binding, RirLocalBinding::DirectPayload) =>
                {
                    self.push(site, RirVerifyErrorKind::OptionPayloadRefLocalMismatch);
                }
                RirPatternBindingMode::Owned { .. } => {
                    if !matches!(local.binding, RirLocalBinding::Value) {
                        self.push(site, RirVerifyErrorKind::OptionPayloadRefLocalMismatch);
                    }
                }
                RirPatternBindingMode::Alias => {}
            }
        }
        if let RirPatternBindingMode::Owned { materializer } = binding.mode
            && !self.canonical_value_materializer(binding.ty, materializer)
        {
            self.push(site, RirVerifyErrorKind::BadId);
        }
        if binding.mode.is_alias() {
            if !Self::pattern_alias_path_supported(&binding.path) {
                self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
            }
            if let Some(slot) = self.payload_bound.get_mut(binding.local.index()) {
                *slot = true;
            }
        }
    }

    fn pattern_match_has_alias_binding(match_: &RirPatternMatch) -> bool {
        match_.arms.iter().any(|arm| {
            arm.alternatives.iter().any(|alternative| {
                alternative
                    .bindings
                    .iter()
                    .any(|binding| binding.mode.is_alias())
            })
        })
    }

    fn pattern_alias_path_supported(path: &RirPatternPath) -> bool {
        matches!(
            path.steps.as_slice(),
            [RirPatternPathStep::EnumTupleField { .. }
                | RirPatternPathStep::EnumStructField { .. }]
        )
    }

    fn pattern_path_ty(
        &mut self,
        site: RirVerifySite,
        subject_ty: RirTypeId,
        guards: &[(Vec<RirPatternPathStep>, RirEnumId, RirVariantId)],
        optional_guards: &[Vec<RirPatternPathStep>],
        path: &RirPatternPath,
    ) -> Option<RirTypeId> {
        let mut ty = subject_ty;
        let mut prefix = Vec::new();
        for step in &path.steps {
            ty = self.pattern_path_step_ty(site, ty, guards, optional_guards, &prefix, step)?;
            prefix.push(step.clone());
        }
        Some(ty)
    }

    fn pattern_path_step_ty(
        &mut self,
        site: RirVerifySite,
        source_ty: RirTypeId,
        guards: &[(Vec<RirPatternPathStep>, RirEnumId, RirVariantId)],
        optional_guards: &[Vec<RirPatternPathStep>],
        prefix: &[RirPatternPathStep],
        step: &RirPatternPathStep,
    ) -> Option<RirTypeId> {
        let target = match step {
            RirPatternPathStep::Place(step) if step.source_ty == source_ty => {
                match (self.ty(source_ty), step.kind) {
                    (
                        Some(RirType::Struct(strukt)),
                        RirPlaceStepKind::StructField(field) | RirPlaceStepKind::ExternField(field),
                    ) => self
                        .program
                        .structs
                        .get(strukt.index())
                        .and_then(|strukt| strukt.fields.get(field.index()))
                        .map(|field| field.ty),
                    (Some(RirType::DataRef(dataref)), RirPlaceStepKind::DataRefField(field)) => {
                        self.program
                            .datarefs
                            .get(dataref.index())
                            .and_then(|dataref| dataref.fields.get(field.index()))
                            .map(|field| field.ty)
                    }
                    (Some(RirType::Tuple(tuple)), RirPlaceStepKind::TupleField(field)) => self
                        .program
                        .tuples
                        .get(tuple.index())
                        .and_then(|tuple| tuple.fields.get(field.index()))
                        .map(|field| field.ty),
                    _ => None,
                }
                .filter(|target| *target == step.target_ty)
            }
            RirPatternPathStep::Place(_) => None,
            RirPatternPathStep::OptionalSome {
                source_ty: expected_source,
                target_ty,
            } => {
                if *expected_source != source_ty
                    || !optional_guards.iter().any(|guard| guard == prefix)
                {
                    if !optional_guards.iter().any(|guard| guard == prefix) {
                        self.push(site, RirVerifyErrorKind::PatternPayloadWithoutVariantTest);
                    }
                    None
                } else {
                    match self.ty(source_ty) {
                        Some(RirType::Option(inner)) if inner == *target_ty => Some(inner),
                        _ => None,
                    }
                }
            }
            RirPatternPathStep::EnumTupleField {
                source_ty: expected_source,
                target_ty,
                enum_id,
                variant,
                field,
            }
            | RirPatternPathStep::EnumStructField {
                source_ty: expected_source,
                target_ty,
                enum_id,
                variant,
                field,
            } => {
                if *expected_source != source_ty
                    || !guards.iter().any(|(path, guard_enum, guard_variant)| {
                        path == prefix && *guard_enum == *enum_id && *guard_variant == *variant
                    })
                {
                    if !guards.iter().any(|(path, guard_enum, guard_variant)| {
                        path == prefix && *guard_enum == *enum_id && *guard_variant == *variant
                    }) {
                        self.push(site, RirVerifyErrorKind::PatternPayloadWithoutVariantTest);
                    }
                    None
                } else {
                    let kind = match step {
                        RirPatternPathStep::EnumTupleField { .. } => RirVariantKind::Tuple,
                        RirPatternPathStep::EnumStructField { .. } => RirVariantKind::Struct,
                        _ => unreachable!(),
                    };
                    self.enum_payload_field_ty(source_ty, *enum_id, *variant, kind, *field)
                        .filter(|found| *found == *target_ty)
                }
            }
        };
        target.or_else(|| {
            self.push(site, RirVerifyErrorKind::BadId);
            None
        })
    }

    fn enum_payload_field_ty(
        &self,
        source_ty: RirTypeId,
        enum_id: RirEnumId,
        variant: RirVariantId,
        expected_kind: RirVariantKind,
        field: u16,
    ) -> Option<RirTypeId> {
        match self.ty(source_ty) {
            Some(RirType::Enum(found)) if found == enum_id => {}
            _ => return None,
        }
        let variant = self
            .program
            .enums
            .get(enum_id.index())?
            .variants
            .get(variant.index())?;
        if variant.kind != expected_kind {
            return None;
        }
        variant.fields.get(field as usize).map(|field| field.ty)
    }

    fn pattern_variant_guards(
        alternative: &RirPatternAlternative,
    ) -> Vec<(Vec<RirPatternPathStep>, RirEnumId, RirVariantId)> {
        alternative
            .tests
            .iter()
            .filter_map(|test| match test {
                RirPatternTest::EnumVariant {
                    path,
                    enum_id,
                    variant,
                } => Some((path.steps.clone(), *enum_id, *variant)),
                _ => None,
            })
            .collect()
    }

    fn variant_belongs_to_enum(&self, enum_id: RirEnumId, variant: RirVariantId) -> bool {
        self.program
            .enums
            .get(enum_id.index())
            .is_some_and(|enm| variant.index() < enm.variants.len())
    }

    fn check_stmt(
        &mut self,
        function_id: RirFunctionId,
        function: &RirFunction,
        index: usize,
        stmt: &RirStmt,
    ) {
        let site = RirVerifySite::Statement(function_id, index);
        match stmt {
            RirStmt::Init { local, value } => {
                self.check_local_id(site, function, *local);
                match function
                    .locals
                    .get(local.index())
                    .map(|local| local.binding)
                {
                    Some(RirLocalBinding::Parameter { .. }) => {
                        self.push(site, RirVerifyErrorKind::InitParamLocal);
                    }
                    Some(RirLocalBinding::DirectPayload | RirLocalBinding::ScopedPlacePayload) => {
                        self.push(site, RirVerifyErrorKind::InitPayloadRefLocal);
                    }
                    Some(RirLocalBinding::Value)
                        if self
                            .possibly_initialized
                            .get(local.index())
                            .copied()
                            .unwrap_or(false) =>
                    {
                        self.push(site, RirVerifyErrorKind::InitParamLocal);
                    }
                    Some(RirLocalBinding::Value) | None => {}
                }
                if let Some(expected) = function.locals.get(local.index()).map(|local| local.ty) {
                    self.check_rvalue(function_id, function, index, value, Some(expected));
                }
                self.check_hidden_stack_loop_lambda_rvalue(site, function, value);
                let escape = self.rvalue_lambda_escape(function, value);
                self.set_local_lambda_escape(function, *local, escape);
                self.set_local_lambda_value(
                    *local,
                    self.rvalue_function_value_state(function, value),
                );
                self.set_local_decl_scope(*local);
                let loop_scope = self.rvalue_loop_lambda_scope(function, value);
                self.set_local_loop_lambda_scope(function, *local, loop_scope);
                if let Some(initialized) = self.initialized.get_mut(local.index()) {
                    *initialized = true;
                }
                if let Some(possible) = self.possibly_initialized.get_mut(local.index()) {
                    *possible = true;
                }
            }
            RirStmt::GlobalEnsure { global } => {
                if self.check_global_id(site, *global).is_some()
                    && !self
                        .global_initialized
                        .get(global.index())
                        .copied()
                        .unwrap_or(false)
                {
                    if let Some(slot) = self.global_initialized.get_mut(global.index()) {
                        *slot = true;
                    }
                    let value = self.global_initializer_function_value_state(*global);
                    if let Some(slot) = self.global_values.get_mut(global.index()) {
                        *slot = value;
                    }
                }
            }
            RirStmt::GlobalSetRoot { global, value }
            | RirStmt::GlobalUpdateRoot { global, value } => {
                let Some(global_decl) = self.check_global_id(site, *global).cloned() else {
                    self.check_rvalue(function_id, function, index, value, None);
                    self.check_stack_loop_lambda_rvalue(site, function, value);
                    return;
                };
                let requires_init = matches!(stmt, RirStmt::GlobalUpdateRoot { .. });
                if requires_init
                    && !self
                        .global_initialized
                        .get(global.index())
                        .copied()
                        .unwrap_or(false)
                {
                    self.push(site, RirVerifyErrorKind::UninitializedGlobal(*global));
                }
                if !global_decl.mutable {
                    self.push(site, RirVerifyErrorKind::ImmutableAssign);
                }
                if !self.global_payload_supported(global_decl.ty) {
                    self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                }
                if self.global_replaces_active_collection_root(*global) {
                    self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                }
                self.check_rvalue(function_id, function, index, value, Some(global_decl.ty));
                self.check_stack_loop_lambda_rvalue(site, function, value);
                let value_state = self.rvalue_function_value_state(function, value);
                if let Some(slot) = self.global_initialized.get_mut(global.index()) {
                    *slot = true;
                }
                if let Some(slot) = self.global_values.get_mut(global.index()) {
                    *slot = value_state;
                }
            }
            RirStmt::MutPlaceSet { place, value } => {
                if matches!(place.access, RirMutPlaceAccess::DataRef { .. }) {
                    self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                }
                if self.mut_place_set_replaces_active_collection_root(place) {
                    self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                }
                let ty = self.check_mut_place_arg(
                    site,
                    function_id,
                    function,
                    place,
                    MutPlaceUse::Write,
                );
                self.check_rvalue(function_id, function, index, value, ty);
                self.check_stack_loop_lambda_rvalue(site, function, value);
                let value_state = self.rvalue_function_value_state(function, value);
                self.set_mut_place_function_value(function, place, value_state);
            }
            RirStmt::Assign { dst, value } => {
                self.check_place(site, function, dst);
                let Some(dst_local) = self.local_root(site, dst) else {
                    self.check_rvalue(
                        function_id,
                        function,
                        index,
                        value,
                        self.program.place_ty(function, dst),
                    );
                    self.check_stack_loop_lambda_rvalue(site, function, value);
                    return;
                };
                self.check_mutable_local_root(site, function, Some(dst_local));
                if self.assignment_replaces_active_collection_root(dst) {
                    self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                }
                if !Self::assignment_projection_supported(&dst.projections) {
                    self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                }
                self.check_rvalue(
                    function_id,
                    function,
                    index,
                    value,
                    self.program.place_ty(function, dst),
                );
                self.check_hidden_stack_loop_lambda_rvalue(site, function, value);
                let value_state = self.rvalue_function_value_state(function, value);
                let loop_scope = self.rvalue_loop_lambda_scope(function, value);
                if dst.projections.is_empty() {
                    self.set_place_function_value(function, dst, value_state);
                    self.check_loop_lambda_assignment_scope(site, dst_local, loop_scope);
                    self.set_local_loop_lambda_scope(function, dst_local, loop_scope);
                } else {
                    self.set_place_function_value(function, dst, value_state);
                    if loop_scope.is_some() {
                        self.push(site, RirVerifyErrorKind::UnsupportedLambdaCell);
                    }
                }
            }
            RirStmt::CellInit { cell, value } => {
                if matches!(cell, RirCellRef::Capture { .. }) {
                    self.push(site, RirVerifyErrorKind::UnsupportedLambdaCapture);
                    return;
                }
                if let Some(decl) = self.check_function_cell_ref(site, function_id, *cell) {
                    match decl.lifetime {
                        RirCellLifetime::Function if !self.loops.is_empty() => {
                            self.push(site, RirVerifyErrorKind::UnsupportedLambdaCell);
                        }
                        RirCellLifetime::Loop { loop_id }
                            if self.loops.last().copied() != Some(loop_id) =>
                        {
                            self.push(site, RirVerifyErrorKind::UnsupportedLambdaCell);
                        }
                        RirCellLifetime::Function | RirCellLifetime::Loop { .. } => {}
                    }
                    if self.cell_possibly_initialized(decl.id) {
                        self.push(site, RirVerifyErrorKind::InitCellTwice(decl.id));
                    }
                    self.check_rvalue(function_id, function, index, value, Some(decl.payload_ty));
                    self.check_stack_loop_lambda_rvalue(site, function, value);
                    self.mark_cell_initialized(decl.id);
                }
            }
            RirStmt::ScopedPlaceCellInit { cell } => {
                self.check_function_scoped_place_cell_ref(
                    site,
                    function_id,
                    RirScopedPlaceCellRef::Owner(*cell),
                );
            }
            RirStmt::CellSet { cell, value } => {
                if !matches!(value, RirRValue::Use(_) | RirRValue::TakeStaged(_)) {
                    self.push(site, RirVerifyErrorKind::UnsupportedLambdaCell);
                }
                if let Some(decl) = self.check_function_cell_ref(site, function_id, *cell) {
                    if decl.storage == RirCellStorage::Heap
                        && matches!(value, RirRValue::Use(RirOperand::Place(place)) if Self::place_is_mut_place_param_root(function, place))
                    {
                        self.push(site, RirVerifyErrorKind::UnsupportedLambdaCell);
                    }
                    if !self.cell_initialized(decl.id) {
                        self.push(site, RirVerifyErrorKind::UninitializedCell(decl.id));
                    }
                    self.check_rvalue(function_id, function, index, value, Some(decl.payload_ty));
                    self.check_stack_loop_lambda_rvalue(site, function, value);
                }
            }
            RirStmt::ScopedPlaceCellSet { cell, value } => {
                if !matches!(value, RirRValue::Use(_) | RirRValue::TakeStaged(_)) {
                    self.push(site, RirVerifyErrorKind::UnsupportedLambdaCell);
                }
                if let Some(decl) =
                    self.check_function_scoped_place_cell_ref(site, function_id, *cell)
                {
                    self.check_rvalue(function_id, function, index, value, Some(decl.payload_ty));
                    self.check_stack_loop_lambda_rvalue(site, function, value);
                }
            }
            RirStmt::DataRefSet {
                object,
                place,
                suffix,
                value,
            } => {
                if let Some(expected) = self.check_dataref_access(
                    site,
                    function,
                    object,
                    *place,
                    suffix,
                    StorageProjectionMode::Ordinary,
                ) {
                    self.check_rvalue(function_id, function, index, value, Some(expected));
                    self.check_stack_loop_lambda_rvalue(site, function, value);
                }
            }
            RirStmt::SequenceSlotSet {
                collection,
                step,
                value,
            } => {
                if let Some(collection_ty) = self.program.collection_ty(function, collection) {
                    self.check_place_step(site, function, collection_ty, step, true);
                }
                let value_ty = self.check_sequence_slot(
                    site,
                    function_id,
                    function,
                    collection,
                    step.index_local().expect("sequence slot step"),
                    None,
                    true,
                );
                if let Some(value_ty) = value_ty {
                    self.check_value_operand_ty(site, function, value, value_ty);
                    self.check_stack_loop_lambda_operand(site, function, value);
                }
                self.clear_collection_function_value(function, collection);
            }
            RirStmt::MapValueSet { map, index, value } => {
                if !self.collection_loan_root_mutable(function, map) {
                    self.push(site, RirVerifyErrorKind::ImmutableAssign);
                }
                if let Some((_, value_ty)) =
                    self.check_map_slot(site, function_id, function, map, *index, true)
                {
                    self.check_value_operand_ty(site, function, value, value_ty);
                    self.check_stack_loop_lambda_operand(site, function, value);
                }
                self.clear_collection_function_value(function, map);
            }
            RirStmt::Eval(value) => {
                self.check_rvalue(function_id, function, index, value, None);
                self.check_hidden_stack_loop_lambda_rvalue(site, function, value);
                self.clear_rvalue_function_writes(function, value);
            }
            RirStmt::If(branch) => {
                if let Some(bool_ty) = self.type_id(RirType::Bool) {
                    let found = self.value_operand_ty(site, function, &branch.cond);
                    if found != Some(bool_ty) {
                        self.push(
                            site,
                            RirVerifyErrorKind::TypeMismatch {
                                expected: bool_ty,
                                found: found.unwrap_or(bool_ty),
                            },
                        );
                    }
                }
                let entry = self.block_entry_state();
                let then_state = self.check_structured_block(
                    function_id,
                    function,
                    &branch.then_block,
                    entry.clone(),
                    None,
                );
                let else_state = match &branch.else_block {
                    Some(else_block) => self.check_structured_block(
                        function_id,
                        function,
                        else_block,
                        entry.clone(),
                        None,
                    ),
                    None => Some(self.current_block_state()),
                };
                self.merge_structured_states([then_state, else_state]);
            }
            RirStmt::Loop(loop_) => {
                self.loops.push(loop_.id);
                self.check_structured_block(
                    function_id,
                    function,
                    &loop_.body,
                    self.block_entry_state(),
                    None,
                );
                self.loops.pop();
            }
            RirStmt::RangeFor(range) => {
                if let Some(int) = self.type_id(RirType::Int) {
                    self.check_int_operands(
                        site,
                        function,
                        std::iter::once(&range.start)
                            .chain(std::iter::once(&range.end))
                            .chain(range.ordinal_plan.operands()),
                        int,
                    );
                    self.check_int_locals(
                        site,
                        function,
                        range.ordinal.into_iter().chain([range.item]),
                        int,
                    );
                }
                self.loops.push(range.id);
                let mut entry = self.block_entry_state();
                Self::init_entry_locals(&mut entry, range.ordinal.into_iter().chain([range.item]));
                self.check_structured_block(function_id, function, &range.body, entry, None);
                self.loops.pop();
            }
            RirStmt::CollectionFor(for_) => {
                if let Some(int) = self.type_id(RirType::Int) {
                    self.check_int_operands(site, function, for_.ordinal_plan.operands(), int);
                    self.check_int_locals(
                        site,
                        function,
                        [Some(for_.len), Some(for_.index), for_.ordinal]
                            .into_iter()
                            .flatten(),
                        int,
                    );
                    if function.locals.get(for_.len.index()).is_some()
                        && !self
                            .initialized
                            .get(for_.len.index())
                            .copied()
                            .unwrap_or(false)
                    {
                        self.push(site, RirVerifyErrorKind::UninitializedLocal(for_.len));
                    }
                }
                self.loops.push(for_.id);
                let mut entry = self.block_entry_state();
                Self::init_entry_locals(
                    &mut entry,
                    [Some(for_.index), for_.ordinal].into_iter().flatten(),
                );
                self.check_structured_block(function_id, function, &for_.body, entry, None);
                self.loops.pop();
            }
            RirStmt::CollectionLoanScope(scope) => {
                self.check_collection_loan_scope(function_id, function, site, scope);
            }
            RirStmt::CollectionSlotScope(block) => {
                let state = self.check_structured_block(
                    function_id,
                    function,
                    block,
                    self.block_entry_state(),
                    None,
                );
                self.merge_structured_states([state]);
            }
            RirStmt::PatternMatch(match_) => {
                self.check_pattern_match(function_id, function, site, match_);
            }
            RirStmt::DynMatch(match_) => {
                self.check_dyn_match(function_id, function, site, match_);
            }
            RirStmt::MapEntryMatch(match_) => {
                let map_ty = self.check_mut_place_arg(
                    site,
                    function_id,
                    function,
                    &match_.map,
                    MutPlaceUse::IndexedMapAssignment,
                );
                let (key_ty, value_ty) = match map_ty.and_then(|ty| self.ty(ty)) {
                    Some(RirType::Map { key, value }) => (Some(key), Some(value)),
                    _ => {
                        self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                        (None, None)
                    }
                };
                if let Some(key_ty) = key_ty {
                    self.check_value_operand_ty(site, function, &match_.key, key_ty);
                } else if let Some(map_ty) = map_ty {
                    self.check_value_operand_ty(site, function, &match_.key, map_ty);
                }
                let entry = self.block_entry_state();
                let mut some_entry = entry.clone();
                if match_.payload_escapes && match_.payload.is_none() {
                    self.push(site, RirVerifyErrorKind::OptionPayloadEscapeRequiresPayload);
                }
                if let Some(payload) = match_.payload {
                    if let Some(local) = function.locals.get(payload.index()) {
                        if function.locals.get(payload.index()).is_some_and(|local| {
                            matches!(local.binding, RirLocalBinding::Parameter { .. })
                        }) || entry
                            .possible
                            .get(payload.index())
                            .copied()
                            .unwrap_or(false)
                        {
                            self.push(site, RirVerifyErrorKind::InitParamLocal);
                        }
                        if !local.mutable
                            || !matches!(local.binding, RirLocalBinding::ScopedPlacePayload)
                        {
                            self.push(site, RirVerifyErrorKind::OptionPayloadRefLocalMismatch);
                        }
                        if let Some(slot) = self.payload_bound.get_mut(payload.index()) {
                            *slot = true;
                        }
                        if let Some(value_ty) = value_ty
                            && local.ty != value_ty
                        {
                            self.push(
                                site,
                                RirVerifyErrorKind::TypeMismatch {
                                    expected: value_ty,
                                    found: local.ty,
                                },
                            );
                        }
                        if let Some(slot) = some_entry.definite.get_mut(payload.index()) {
                            *slot = true;
                        }
                        if let Some(slot) = some_entry.possible.get_mut(payload.index()) {
                            *slot = true;
                        }
                        let payload_value = value_ty
                            .map_or(RirFunctionValueState::Unknown, |value_ty| {
                                self.source_call_return_state(value_ty)
                            });
                        if let Some(slot) = some_entry.lambda_escapes.get_mut(payload.index()) {
                            *slot = payload_value.escape();
                        }
                        if let Some(slot) = some_entry.lambda_values.get_mut(payload.index()) {
                            *slot = payload_value;
                        }
                        if let Some(slot) = some_entry.loop_lambda_scopes.get_mut(payload.index()) {
                            *slot = None;
                        }
                    } else {
                        self.push(site, RirVerifyErrorKind::BadId);
                    }
                }
                let escaping_payload = match_.payload_escapes.then_some(match_.payload).flatten();
                let mut some_state = self.check_structured_block(
                    function_id,
                    function,
                    &match_.some_block,
                    some_entry,
                    escaping_payload,
                );
                if !match_.payload_escapes
                    && let (Some(payload), Some(state)) = (match_.payload, &mut some_state)
                {
                    if let Some(slot) = state.definite.get_mut(payload.index()) {
                        *slot = false;
                    }
                    if let Some(slot) = state.possible.get_mut(payload.index()) {
                        *slot = false;
                    }
                }
                let none_state = self.check_structured_block(
                    function_id,
                    function,
                    &match_.none_block,
                    entry,
                    None,
                );
                if match_.payload_escapes && none_state.is_some() {
                    self.push(site, RirVerifyErrorKind::OptionPayloadEscapeNoneMustDiverge);
                }
                self.merge_structured_states([some_state, none_state]);
            }
            RirStmt::OptionMatch(match_) => {
                let borrows_payload = match_.payload.is_some_and(RirOptionPayloadBinding::is_ref);
                let payload_escapes = match_.payload.is_some_and(RirOptionPayloadBinding::escapes);
                let payload_local = match_.payload.map(RirOptionPayloadBinding::local);
                let subject_ty = match &match_.subject {
                    RirOptionSubject::Place(place) => {
                        self.check_place(site, function, place);
                        self.program.place_ty(function, place)
                    }
                    RirOptionSubject::MutPlace(place) => {
                        let ty = self.check_mut_place_arg(
                            site,
                            function_id,
                            function,
                            place,
                            MutPlaceUse::CallArg,
                        );
                        if !borrows_payload {
                            self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                        }
                        ty
                    }
                };
                let discr_local = match &match_.subject {
                    RirOptionSubject::Place(place) => self.local_root(site, place),
                    RirOptionSubject::MutPlace(_) => None,
                };
                let inner = match subject_ty.and_then(|ty| self.ty(ty)) {
                    Some(RirType::Option(inner)) => Some(inner),
                    _ => {
                        self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                        None
                    }
                };
                if let (Some(RirOptionPayloadBinding::Owned { materializer, .. }), Some(inner)) =
                    (match_.payload, inner)
                    && !self.canonical_value_materializer(inner, materializer)
                {
                    self.push(site, RirVerifyErrorKind::BadId);
                }
                let entry = self.block_entry_state();
                let mut some_entry = entry.clone();
                if let Some(payload) = payload_local {
                    if let Some(local) = function.locals.get(payload.index()) {
                        if function.locals.get(payload.index()).is_some_and(|local| {
                            matches!(local.binding, RirLocalBinding::Parameter { .. })
                        }) {
                            self.push(site, RirVerifyErrorKind::InitParamLocal);
                        }
                        if entry
                            .possible
                            .get(payload.index())
                            .copied()
                            .unwrap_or(false)
                        {
                            self.push(site, RirVerifyErrorKind::InitParamLocal);
                        }
                        if local.mutable != borrows_payload {
                            self.push(site, RirVerifyErrorKind::ImmutableAssign);
                        }
                        let expected_binding = match match_.subject {
                            RirOptionSubject::Place(_) if borrows_payload => {
                                RirLocalBinding::DirectPayload
                            }
                            RirOptionSubject::MutPlace(_) if borrows_payload => {
                                RirLocalBinding::ScopedPlacePayload
                            }
                            _ => RirLocalBinding::Value,
                        };
                        if local.binding != expected_binding {
                            self.push(site, RirVerifyErrorKind::OptionPayloadRefLocalMismatch);
                        }
                        if borrows_payload
                            && let Some(slot) = self.payload_bound.get_mut(payload.index())
                        {
                            *slot = true;
                        }
                        if let Some(inner) = inner
                            && local.ty != inner
                        {
                            self.push(
                                site,
                                RirVerifyErrorKind::TypeMismatch {
                                    expected: inner,
                                    found: local.ty,
                                },
                            );
                        }
                        if let Some(slot) = some_entry.definite.get_mut(payload.index()) {
                            *slot = true;
                        }
                        if let Some(slot) = some_entry.possible.get_mut(payload.index()) {
                            *slot = true;
                        }
                        let payload_value = self.option_payload_function_value_state(
                            function,
                            &match_.subject,
                            inner,
                        );
                        if let Some(slot) = some_entry.lambda_escapes.get_mut(payload.index()) {
                            *slot = payload_value.escape();
                        }
                        if let Some(slot) = some_entry.lambda_values.get_mut(payload.index()) {
                            *slot = payload_value;
                        }
                        if let Some(slot) = some_entry.loop_lambda_scopes.get_mut(payload.index()) {
                            *slot = None;
                        }
                    } else {
                        self.push(site, RirVerifyErrorKind::BadId);
                    }
                }
                if borrows_payload
                    && discr_local.is_some_and(|local| Self::local_root_immutable(function, local))
                {
                    self.push(
                        site,
                        RirVerifyErrorKind::OptionPayloadRefDiscriminantMustBeMutable,
                    );
                }
                let escaping_payload = payload_escapes.then_some(payload_local).flatten();
                let mut some_state = self.check_structured_block(
                    function_id,
                    function,
                    &match_.some_block,
                    some_entry,
                    escaping_payload,
                );
                if borrows_payload
                    && !payload_escapes
                    && let (Some(payload), Some(state)) = (payload_local, &mut some_state)
                {
                    if let Some(slot) = state.definite.get_mut(payload.index()) {
                        *slot = false;
                    }
                    if let Some(slot) = state.possible.get_mut(payload.index()) {
                        *slot = false;
                    }
                }
                let none_state = self.check_structured_block(
                    function_id,
                    function,
                    &match_.none_block,
                    entry,
                    None,
                );
                if payload_escapes && none_state.is_some() {
                    self.push(site, RirVerifyErrorKind::OptionPayloadEscapeNoneMustDiverge);
                }
                self.merge_structured_states([some_state, none_state]);
            }
        }
    }

    fn set_local_lambda_escape(
        &mut self,
        function: &RirFunction,
        local: RirLocalId,
        escape: Option<RirLambdaEscape>,
    ) {
        let is_lambda = function
            .locals
            .get(local.index())
            .is_some_and(|local| matches!(self.ty(local.ty), Some(RirType::Lambda(_))));
        if let Some(slot) = self.lambda_escapes.get_mut(local.index()) {
            *slot = is_lambda.then_some(escape).flatten();
        }
    }

    fn set_local_lambda_value(&mut self, local: RirLocalId, value: RirFunctionValueState) {
        if let Some(slot) = self.lambda_values.get_mut(local.index()) {
            *slot = value;
        }
    }

    fn clear_local_lambda_value(&mut self, local: RirLocalId) {
        if let Some(slot) = self.lambda_escapes.get_mut(local.index()) {
            *slot = None;
        }
        if let Some(slot) = self.lambda_values.get_mut(local.index()) {
            *slot = RirFunctionValueState::Unknown;
        }
    }

    fn set_place_function_value(
        &mut self,
        function: &RirFunction,
        place: &RirPlace,
        value: RirFunctionValueState,
    ) {
        match place.root {
            RirPlaceRoot::Local(local) if place.projections.is_empty() => {
                self.set_local_lambda_escape(function, local, value.escape());
                self.set_local_lambda_value(local, value);
            }
            RirPlaceRoot::Local(local) if local.index() < self.lambda_values.len() => {
                if !self.lambda_values[local.index()].assign_projection(&place.projections, value) {
                    self.clear_local_lambda_value(local);
                }
            }
            RirPlaceRoot::Global(global) if place.projections.is_empty() => {
                if let Some(slot) = self.global_values.get_mut(global.index()) {
                    *slot = value;
                }
            }
            RirPlaceRoot::Global(global) if global.index() < self.global_values.len() => {
                if !self.global_values[global.index()].assign_projection(&place.projections, value)
                {
                    self.global_values[global.index()] = RirFunctionValueState::Unknown;
                }
            }
            RirPlaceRoot::Local(_) | RirPlaceRoot::Global(_) => {}
        }
    }

    fn set_mut_place_function_value(
        &mut self,
        function: &RirFunction,
        place: &RirMutPlaceArg,
        value: RirFunctionValueState,
    ) {
        match &place.access {
            RirMutPlaceAccess::Handle(
                RirMutPlaceHandle::Local { local } | RirMutPlaceHandle::Param { local },
            ) if place.projections.is_empty() => {
                self.set_local_lambda_escape(function, *local, value.escape());
                self.set_local_lambda_value(*local, value);
            }
            RirMutPlaceAccess::Handle(
                RirMutPlaceHandle::Local { local } | RirMutPlaceHandle::Param { local },
            ) if local.index() < self.lambda_values.len() => {
                if !self.lambda_values[local.index()].assign_projection(&place.projections, value) {
                    self.clear_local_lambda_value(*local);
                }
            }
            RirMutPlaceAccess::Handle(RirMutPlaceHandle::Global { global })
                if place.projections.is_empty() =>
            {
                if let Some(slot) = self.global_values.get_mut(global.index()) {
                    *slot = value;
                }
            }
            RirMutPlaceAccess::Handle(RirMutPlaceHandle::Global { global })
                if global.index() < self.global_values.len() =>
            {
                if !self.global_values[global.index()].assign_projection(&place.projections, value)
                {
                    self.global_values[global.index()] = RirFunctionValueState::Unknown;
                }
            }
            RirMutPlaceAccess::Handle(
                RirMutPlaceHandle::Local { .. }
                | RirMutPlaceHandle::Param { .. }
                | RirMutPlaceHandle::Global { .. }
                | RirMutPlaceHandle::StackCell { .. }
                | RirMutPlaceHandle::HeapCell { .. }
                | RirMutPlaceHandle::ScopedPlaceCell { .. },
            )
            | RirMutPlaceAccess::DataRef { .. } => {}
        }
    }

    fn push_place_function_value(&mut self, place: &RirPlace, value: RirFunctionValueState) {
        match place.root {
            RirPlaceRoot::Local(local) if local.index() < self.lambda_values.len() => {
                if !self.lambda_values[local.index()].push_projection(&place.projections, value) {
                    self.clear_local_lambda_value(local);
                }
            }
            RirPlaceRoot::Global(global) if global.index() < self.global_values.len() => {
                if !self.global_values[global.index()].push_projection(&place.projections, value) {
                    self.global_values[global.index()] = RirFunctionValueState::Unknown;
                }
            }
            RirPlaceRoot::Local(_) | RirPlaceRoot::Global(_) => {}
        }
    }

    fn push_mut_place_function_value(
        &mut self,
        place: &RirMutPlaceArg,
        value: RirFunctionValueState,
    ) {
        match &place.access {
            RirMutPlaceAccess::Handle(
                RirMutPlaceHandle::Local { local } | RirMutPlaceHandle::Param { local },
            ) if local.index() < self.lambda_values.len() => {
                if !self.lambda_values[local.index()].push_projection(&place.projections, value) {
                    self.clear_local_lambda_value(*local);
                }
            }
            RirMutPlaceAccess::Handle(RirMutPlaceHandle::Global { global })
                if global.index() < self.global_values.len() =>
            {
                if !self.global_values[global.index()].push_projection(&place.projections, value) {
                    self.global_values[global.index()] = RirFunctionValueState::Unknown;
                }
            }
            RirMutPlaceAccess::Handle(
                RirMutPlaceHandle::Local { .. }
                | RirMutPlaceHandle::Param { .. }
                | RirMutPlaceHandle::Global { .. }
                | RirMutPlaceHandle::StackCell { .. }
                | RirMutPlaceHandle::HeapCell { .. }
                | RirMutPlaceHandle::ScopedPlaceCell { .. },
            )
            | RirMutPlaceAccess::DataRef { .. } => {}
        }
    }

    fn clear_place_function_value(&mut self, function: &RirFunction, place: &RirPlace) {
        self.set_place_function_value(function, place, RirFunctionValueState::Unknown);
    }

    fn clear_mut_place_function_value(&mut self, function: &RirFunction, place: &RirMutPlaceArg) {
        self.set_mut_place_function_value(function, place, RirFunctionValueState::Unknown);
    }

    fn clear_collection_function_value(
        &mut self,
        function: &RirFunction,
        collection: &RirCollectionAccess,
    ) {
        match collection {
            RirCollectionAccess::Direct(place) => self.clear_place_function_value(function, place),
            RirCollectionAccess::MutPlace(place) => {
                self.clear_mut_place_function_value(function, place);
            }
        }
    }

    fn push_collection_function_value(
        &mut self,
        collection: &RirCollectionAccess,
        value: RirFunctionValueState,
    ) {
        match collection {
            RirCollectionAccess::Direct(place) => self.push_place_function_value(place, value),
            RirCollectionAccess::MutPlace(place) => {
                self.push_mut_place_function_value(place, value);
            }
        }
    }

    fn clear_rvalue_function_writes(&mut self, function: &RirFunction, value: &RirRValue) {
        match value {
            RirRValue::ListPush { list, value } => {
                let value = self.owned_function_value_state(function, value);
                self.push_collection_function_value(list, value);
            }
            RirRValue::MapInsert { map, .. } | RirRValue::MapRemove { map, .. } => {
                self.clear_collection_function_value(function, map);
            }
            _ => {}
        }
    }

    fn set_local_decl_scope(&mut self, local: RirLocalId) {
        if let Some(slot) = self.local_decl_scopes.get_mut(local.index())
            && slot.is_none()
        {
            *slot = Some(self.scope_depth);
        }
    }

    fn set_local_loop_lambda_scope(
        &mut self,
        function: &RirFunction,
        local: RirLocalId,
        scope: Option<usize>,
    ) {
        let is_lambda = function
            .locals
            .get(local.index())
            .is_some_and(|local| matches!(self.ty(local.ty), Some(RirType::Lambda(_))));
        if let Some(slot) = self.loop_lambda_scopes.get_mut(local.index()) {
            *slot = is_lambda.then_some(scope).flatten();
        }
    }

    fn check_loop_lambda_assignment_scope(
        &mut self,
        site: RirVerifySite,
        local: RirLocalId,
        scope: Option<usize>,
    ) {
        if scope.is_some() && self.local_decl_scopes.get(local.index()).copied().flatten() != scope
        {
            self.push(site, RirVerifyErrorKind::UnsupportedLambdaCell);
        }
    }

    fn rvalue_loop_lambda_scope(&self, function: &RirFunction, value: &RirRValue) -> Option<usize> {
        match value {
            RirRValue::Lambda { captures, .. } => captures
                .iter()
                .any(|capture| self.lambda_capture_arg_uses_loop_cell(capture))
                .then_some(self.scope_depth),
            RirRValue::Use(operand)
            | RirRValue::TakeStaged(operand)
            | RirRValue::Materialize(RirOwnedValue {
                value: RirOwnedOperand::Value(operand),
                ..
            })
            | RirRValue::FunctionValue {
                value:
                    RirOwnedValue {
                        value: RirOwnedOperand::Value(operand),
                        ..
                    },
                ..
            } => self.operand_loop_lambda_scope(function, operand),
            _ => None,
        }
    }

    fn check_stack_loop_lambda_rvalue(
        &mut self,
        site: RirVerifySite,
        function: &RirFunction,
        value: &RirRValue,
    ) {
        if self.rvalue_contains_stack_loop_lambda(function, value) {
            self.push(site, RirVerifyErrorKind::UnsupportedLambdaCell);
        }
    }

    fn check_hidden_stack_loop_lambda_rvalue(
        &mut self,
        site: RirVerifySite,
        function: &RirFunction,
        value: &RirRValue,
    ) {
        if self.rvalue_hides_stack_loop_lambda(function, value) {
            self.push(site, RirVerifyErrorKind::UnsupportedLambdaCell);
        }
    }

    fn check_stack_loop_lambda_operand(
        &mut self,
        site: RirVerifySite,
        function: &RirFunction,
        operand: &RirOperand,
    ) {
        if self.operand_contains_stack_loop_lambda(function, operand) {
            self.push(site, RirVerifyErrorKind::UnsupportedLambdaCell);
        }
    }

    fn rvalue_hides_stack_loop_lambda(&self, function: &RirFunction, value: &RirRValue) -> bool {
        if matches!(
            value,
            RirRValue::Materialize(_) | RirRValue::FunctionValue { .. }
        ) {
            return false;
        }
        let mut hidden = false;
        value.for_each_owned_value(&mut |owned| {
            hidden |= self.owned_contains_stack_loop_lambda(function, owned);
        });
        hidden
    }

    fn rvalue_contains_stack_loop_lambda(&self, function: &RirFunction, value: &RirRValue) -> bool {
        match value {
            RirRValue::Use(operand) | RirRValue::TakeStaged(operand) => {
                self.operand_contains_stack_loop_lambda(function, operand)
            }
            RirRValue::Lambda { captures, .. } => captures
                .iter()
                .any(|capture| self.lambda_capture_arg_uses_loop_cell(capture)),
            value => self.rvalue_hides_stack_loop_lambda(function, value),
        }
    }

    fn owned_contains_stack_loop_lambda(
        &self,
        function: &RirFunction,
        owned: &RirOwnedValue,
    ) -> bool {
        match &owned.value {
            RirOwnedOperand::Value(value) => {
                self.operand_contains_stack_loop_lambda(function, value)
            }
            RirOwnedOperand::Access(_) | RirOwnedOperand::DynBorrow(_) => false,
        }
    }

    fn operand_contains_stack_loop_lambda(
        &self,
        function: &RirFunction,
        operand: &RirOperand,
    ) -> bool {
        self.operand_loop_lambda_scope(function, operand).is_some()
    }

    fn operand_loop_lambda_scope(
        &self,
        function: &RirFunction,
        operand: &RirOperand,
    ) -> Option<usize> {
        let RirOperand::Place(place) = operand else {
            return None;
        };
        let RirPlaceRoot::Local(local) = place.root else {
            return None;
        };
        let ty = self.program.place_ty(function, place);
        if !place.projections.is_empty()
            || !matches!(ty.and_then(|ty| self.ty(ty)), Some(RirType::Lambda(_)))
            || function
                .locals
                .get(local.index())
                .is_none_or(|local_decl| Some(local_decl.ty) != ty)
        {
            return None;
        }
        self.loop_lambda_scopes
            .get(local.index())
            .copied()
            .flatten()
    }

    fn lambda_capture_arg_uses_loop_cell(&self, capture: &RirLambdaCaptureArg) -> bool {
        let RirLambdaCaptureArg::StackCell { cell } = capture else {
            return false;
        };
        self.program
            .cells
            .get(cell_ref_id(*cell).index())
            .is_some_and(|cell| matches!(cell.lifetime, RirCellLifetime::Loop { .. }))
    }

    fn type_function_value_state(&self, ty: RirTypeId) -> RirFunctionValueState {
        if matches!(self.ty(ty), Some(RirType::Lambda(_))) {
            RirFunctionValueState::Lambda(None)
        } else {
            RirFunctionValueState::NonFunction
        }
    }

    fn source_call_return_state(&self, ty: RirTypeId) -> RirFunctionValueState {
        self.source_call_return_state_inner(ty, &mut HashSet::new())
    }

    fn source_call_return_state_inner(
        &self,
        ty: RirTypeId,
        active: &mut HashSet<RirTypeId>,
    ) -> RirFunctionValueState {
        if !active.insert(ty) {
            return RirFunctionValueState::Unknown;
        }
        let state =
            match self.ty(ty) {
                Some(RirType::Lambda(_)) => {
                    RirFunctionValueState::Lambda(Some(RirLambdaEscape::Escaping))
                }
                Some(RirType::Tuple(tuple)) => self.program.tuples.get(tuple.index()).map_or(
                    RirFunctionValueState::Unknown,
                    |tuple| {
                        RirFunctionValueState::Fields(
                            tuple
                                .fields
                                .iter()
                                .map(|field| self.source_call_return_state_inner(field.ty, active))
                                .collect(),
                        )
                    },
                ),
                Some(RirType::Struct(strukt)) => self.program.structs.get(strukt.index()).map_or(
                    RirFunctionValueState::Unknown,
                    |strukt| {
                        RirFunctionValueState::Fields(
                            strukt
                                .fields
                                .iter()
                                .map(|field| self.source_call_return_state_inner(field.ty, active))
                                .collect(),
                        )
                    },
                ),
                Some(RirType::Enum(enm)) => self.program.enums.get(enm.index()).map_or(
                    RirFunctionValueState::Unknown,
                    |enm| {
                        RirFunctionValueState::Variants(
                            enm.variants
                                .iter()
                                .map(|variant| {
                                    variant
                                        .fields
                                        .iter()
                                        .map(|field| {
                                            self.source_call_return_state_inner(field.ty, active)
                                        })
                                        .collect()
                                })
                                .collect(),
                        )
                    },
                ),
                Some(RirType::Option(inner)) => RirFunctionValueState::Fields(vec![
                    self.source_call_return_state_inner(inner, active),
                ]),
                Some(RirType::Array { elem, .. } | RirType::List(elem)) => {
                    RirFunctionValueState::Fields(vec![
                        self.source_call_return_state_inner(elem, active),
                    ])
                }
                Some(RirType::Map { value, .. }) => RirFunctionValueState::Fields(vec![
                    self.source_call_return_state_inner(value, active),
                ]),
                _ => RirFunctionValueState::NonFunction,
            };
        active.remove(&ty);
        state
    }

    fn global_initializer_function_value_state(
        &mut self,
        global: RirGlobalId,
    ) -> RirFunctionValueState {
        self.global_initializer_function_value_state_inner(global, false)
    }

    fn immutable_global_initializer_function_value_state(
        &mut self,
        global: RirGlobalId,
    ) -> RirFunctionValueState {
        self.global_initializer_function_value_state_inner(global, true)
    }

    fn global_initializer_function_value_state_inner(
        &mut self,
        global: RirGlobalId,
        require_immutable: bool,
    ) -> RirFunctionValueState {
        let Some(global_decl) = self.program.globals.get(global.index()) else {
            return RirFunctionValueState::Unknown;
        };
        if require_immutable && global_decl.mutable {
            return RirFunctionValueState::Unknown;
        }
        let Some(function) = self.program.functions.get(global_decl.init.index()) else {
            return RirFunctionValueState::Unknown;
        };
        let lambda_values = function
            .locals
            .iter()
            .map(|local| self.type_function_value_state(local.ty))
            .collect();
        let outer_lambda_values = std::mem::replace(&mut self.lambda_values, lambda_values);
        let mut result = RirFunctionValueState::Unknown;
        for stmt in &function.body.stmts {
            match stmt {
                RirStmt::Init { local, value } => {
                    let value = self.rvalue_function_value_state(function, value);
                    self.set_local_lambda_value(*local, value);
                }
                RirStmt::Assign { dst, value } => {
                    if let RirPlaceRoot::Local(local) = dst.root {
                        if dst.projections.is_empty() {
                            let value = self.rvalue_function_value_state(function, value);
                            self.set_local_lambda_value(local, value);
                        } else {
                            self.clear_local_lambda_value(local);
                        }
                    }
                }
                RirStmt::GlobalSetRoot { global: dst, value }
                | RirStmt::GlobalUpdateRoot { global: dst, value }
                    if *dst == global =>
                {
                    result = self.rvalue_function_value_state(function, value);
                    break;
                }
                _ => {}
            }
        }
        if matches!(result, RirFunctionValueState::Unknown) {
            let value = match &function.body.term {
                RirTerm::Return(Some(value))
                | RirTerm::ReturnOwned(RirOwnedValue {
                    value: RirOwnedOperand::Value(value),
                    ..
                }) => Some(value),
                _ => None,
            };
            if let Some(value) = value {
                result = self.operand_function_value_state(function, value);
            }
        }
        self.lambda_values = outer_lambda_values;
        result
    }

    fn global_initializer_operand_escape(
        &mut self,
        operand: &RirOperand,
    ) -> Option<RirLambdaEscape> {
        let RirOperand::Place(place) = operand else {
            return None;
        };
        let RirPlaceRoot::Global(global) = place.root else {
            return None;
        };
        self.global_initializer_projection_escape(global, &place.projections)
    }

    fn global_initializer_access_escape(
        &mut self,
        place: &RirMutPlaceArg,
    ) -> Option<RirLambdaEscape> {
        let RirMutPlaceAccess::Handle(RirMutPlaceHandle::Global { global }) = place.access else {
            return None;
        };
        self.global_initializer_projection_escape(global, &place.projections)
    }

    fn global_initializer_projection_escape(
        &mut self,
        global: RirGlobalId,
        projections: &[RirPlaceStep],
    ) -> Option<RirLambdaEscape> {
        if self
            .global_initialized
            .get(global.index())
            .copied()
            .unwrap_or(false)
        {
            return None;
        }
        let mut value = self.immutable_global_initializer_function_value_state(global);
        for projection in projections {
            value = value.project(*projection);
        }
        value.escape()
    }

    fn rvalue_function_value_state(
        &self,
        function: &RirFunction,
        value: &RirRValue,
    ) -> RirFunctionValueState {
        match value {
            RirRValue::Use(operand) | RirRValue::TakeStaged(operand) => {
                self.operand_function_value_state(function, operand)
            }
            RirRValue::Materialize(owned) => self.owned_function_value_state(function, owned),
            RirRValue::FunctionValue { escape, .. } => RirFunctionValueState::Lambda(*escape),
            RirRValue::Lambda { lambda, .. } => RirFunctionValueState::Lambda(
                self.program
                    .lambdas
                    .get(lambda.index())
                    .map(|decl| decl.escape),
            ),
            RirRValue::OptionalSome { value, .. } => RirFunctionValueState::Fields(vec![
                self.owned_function_value_state(function, value),
            ]),
            RirRValue::MapGet { ty, .. } | RirRValue::MapRemove { ty, .. } => match self.ty(*ty) {
                Some(RirType::Option(inner)) => {
                    RirFunctionValueState::Fields(vec![self.source_call_return_state(inner)])
                }
                _ => RirFunctionValueState::Unknown,
            },
            RirRValue::SequenceSlotAt { step, .. } => self.source_call_return_state(step.target_ty),
            RirRValue::MapEntryAt { ty, .. }
            | RirRValue::MapKeyAt { ty, .. }
            | RirRValue::MapValueAt { ty, .. } => self.source_call_return_state(*ty),
            RirRValue::Struct { fields, .. }
            | RirRValue::Tuple { fields, .. }
            | RirRValue::DataRefAlloc { fields, .. } => RirFunctionValueState::Fields(
                fields
                    .iter()
                    .map(|field| self.owned_function_value_state(function, field))
                    .collect(),
            ),
            RirRValue::EnumVariant {
                ty,
                variant,
                fields,
            } => {
                let Some(RirType::Enum(enm)) = self.ty(*ty) else {
                    return RirFunctionValueState::Unknown;
                };
                let Some(enm) = self.program.enums.get(enm.index()) else {
                    return RirFunctionValueState::Unknown;
                };
                let mut variants = enm
                    .variants
                    .iter()
                    .map(|variant| {
                        variant
                            .fields
                            .iter()
                            .map(|field| self.source_call_return_state(field.ty))
                            .collect()
                    })
                    .collect::<Vec<_>>();
                variants[variant.index()] = fields
                    .iter()
                    .map(|field| self.owned_function_value_state(function, field))
                    .collect();
                RirFunctionValueState::Variants(variants)
            }
            RirRValue::Array { elems, .. } | RirRValue::List { elems, .. } => {
                RirFunctionValueState::Fields(
                    elems
                        .iter()
                        .map(|elem| self.owned_function_value_state(function, elem))
                        .collect(),
                )
            }
            RirRValue::Map { entries, .. } => RirFunctionValueState::Fields(
                entries
                    .iter()
                    .map(|(_, value)| self.owned_function_value_state(function, value))
                    .collect(),
            ),
            RirRValue::Call {
                callee: RirCallTarget::Function(callee),
                ty,
                ..
            } if !self
                .program
                .globals
                .iter()
                .any(|global| global.init == *callee) =>
            {
                self.source_call_return_state(*ty)
            }
            _ => RirFunctionValueState::Unknown,
        }
    }

    fn owned_function_value_state(
        &self,
        function: &RirFunction,
        owned: &RirOwnedValue,
    ) -> RirFunctionValueState {
        match &owned.value {
            RirOwnedOperand::Value(value) => self.operand_function_value_state(function, value),
            RirOwnedOperand::Access(place) => self.mut_place_function_value_state(function, place),
            RirOwnedOperand::DynBorrow(borrow) => self
                .program
                .dyn_carriers
                .get(borrow.target.index())
                .map_or(RirFunctionValueState::Unknown, |carrier| {
                    self.source_call_return_state(carrier.storage_ty)
                }),
        }
    }

    fn mut_place_function_value_state(
        &self,
        function: &RirFunction,
        place: &RirMutPlaceArg,
    ) -> RirFunctionValueState {
        let mut value = match &place.access {
            RirMutPlaceAccess::Handle(
                RirMutPlaceHandle::Local { local } | RirMutPlaceHandle::Param { local },
            ) => self
                .lambda_values
                .get(local.index())
                .cloned()
                .unwrap_or(RirFunctionValueState::Unknown),
            RirMutPlaceAccess::Handle(RirMutPlaceHandle::Global { global }) => self
                .global_values
                .get(global.index())
                .cloned()
                .unwrap_or(RirFunctionValueState::Unknown),
            RirMutPlaceAccess::Handle(_) => self
                .program
                .mut_place_root_ty(function, &place.access)
                .map_or(RirFunctionValueState::Unknown, |ty| {
                    self.source_call_return_state(ty)
                }),
            RirMutPlaceAccess::DataRef { .. } => {
                return self
                    .program
                    .mut_place_ty(function, place)
                    .map_or(RirFunctionValueState::Unknown, |ty| {
                        self.source_call_return_state(ty)
                    });
            }
        };
        for projection in &place.projections {
            value = value.project(*projection);
        }
        value
    }

    fn operand_function_value_state(
        &self,
        function: &RirFunction,
        operand: &RirOperand,
    ) -> RirFunctionValueState {
        match operand {
            RirOperand::Place(place) => self.place_function_value_state(function, place),
            RirOperand::Const(id) => self
                .program
                .consts
                .get(id.index())
                .map_or(RirFunctionValueState::Unknown, |konst| {
                    self.type_function_value_state(konst.ty)
                }),
        }
    }

    fn option_payload_function_value_state(
        &self,
        function: &RirFunction,
        subject: &RirOptionSubject,
        inner: Option<RirTypeId>,
    ) -> RirFunctionValueState {
        match subject {
            RirOptionSubject::Place(place) => {
                match self.place_function_value_state(function, place) {
                    RirFunctionValueState::Fields(fields) if fields.len() == 1 => fields[0].clone(),
                    _ => RirFunctionValueState::Unknown,
                }
            }
            RirOptionSubject::MutPlace(_) => inner
                .map_or(RirFunctionValueState::Unknown, |inner| {
                    self.source_call_return_state(inner)
                }),
        }
    }

    fn place_function_value_state(
        &self,
        _function: &RirFunction,
        place: &RirPlace,
    ) -> RirFunctionValueState {
        let mut value = match place.root {
            RirPlaceRoot::Local(local) => self
                .lambda_values
                .get(local.index())
                .cloned()
                .unwrap_or(RirFunctionValueState::Unknown),
            RirPlaceRoot::Global(global) => self
                .global_values
                .get(global.index())
                .cloned()
                .unwrap_or(RirFunctionValueState::Unknown),
        };
        for projection in &place.projections {
            value = value.project(*projection);
        }
        value
    }

    fn check_lambda_escape_proof(
        &mut self,
        site: RirVerifySite,
        function: &RirFunction,
        owned: &RirOwnedValue,
        claimed: Option<RirLambdaEscape>,
    ) {
        let actual = self
            .owned_function_value_state(function, owned)
            .escape()
            .or_else(|| match &owned.value {
                RirOwnedOperand::Value(operand) => self.global_initializer_operand_escape(operand),
                RirOwnedOperand::Access(place) => self.global_initializer_access_escape(place),
                RirOwnedOperand::DynBorrow(_) => None,
            });
        if let Some(claimed) = claimed
            && actual != Some(claimed)
        {
            self.push(site, RirVerifyErrorKind::LambdaEscapeProofMismatch);
        }
    }

    fn rvalue_lambda_escape(
        &self,
        function: &RirFunction,
        value: &RirRValue,
    ) -> Option<RirLambdaEscape> {
        match value {
            RirRValue::Lambda { lambda, .. } => self
                .program
                .lambdas
                .get(lambda.index())
                .map(|decl| decl.escape),
            RirRValue::FunctionValue { escape, .. } => *escape,
            RirRValue::Use(operand)
            | RirRValue::TakeStaged(operand)
            | RirRValue::Materialize(RirOwnedValue {
                value: RirOwnedOperand::Value(operand),
                ..
            }) => self.operand_lambda_escape(function, operand),
            _ => None,
        }
    }

    fn owned_value_lambda_escape(
        &self,
        function: &RirFunction,
        value: &RirOwnedValue,
    ) -> Option<RirLambdaEscape> {
        let RirOwnedOperand::Value(operand) = &value.value else {
            return None;
        };
        self.operand_lambda_escape(function, operand)
    }

    fn operand_lambda_escape(
        &self,
        function: &RirFunction,
        operand: &RirOperand,
    ) -> Option<RirLambdaEscape> {
        let RirOperand::Place(place) = operand else {
            return None;
        };
        let RirPlaceRoot::Local(local) = place.root else {
            return None;
        };
        let ty = self.program.place_ty(function, place);
        if !place.projections.is_empty()
            || !matches!(ty.and_then(|ty| self.ty(ty)), Some(RirType::Lambda(_)))
            || function
                .locals
                .get(local.index())
                .is_none_or(|local| Some(local.ty) != ty)
        {
            return None;
        }
        self.lambda_escapes.get(local.index()).copied().flatten()
    }

    fn check_collection_loan_scope(
        &mut self,
        function_id: RirFunctionId,
        function: &RirFunction,
        site: RirVerifySite,
        scope: &RirCollectionLoanScope,
    ) {
        let root_ty = self.check_collection_access(
            site,
            function_id,
            function,
            &scope.root,
            MutPlaceUse::CallArg,
        );
        match &scope.root {
            RirCollectionAccess::Direct(root) => {
                self.check_collection_loan_root(site, function, root);
            }
            RirCollectionAccess::MutPlace(arg) => {
                if matches!(&arg.access, RirMutPlaceAccess::Handle(_))
                    && let Some(root_ty) = self.program.mut_place_root_ty(function, &arg.access)
                {
                    self.check_collection_loan_steps(site, root_ty, &arg.projections);
                }
            }
        }
        let root_ty = root_ty.and_then(|ty| self.ty(ty));
        let compatible_root = matches!(
            (scope.root_kind, root_ty),
            (RirCollectionRootKind::List, Some(RirType::List(_)))
                | (
                    RirCollectionRootKind::FixedArray,
                    Some(RirType::Array { .. })
                )
                | (RirCollectionRootKind::Slice, Some(RirType::Slice(_)))
                | (RirCollectionRootKind::Map, Some(RirType::Map { .. }))
        );
        let compatible_mode = scope.root_kind.accepts_mode(scope.mode);
        if !compatible_root || !compatible_mode {
            self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
        }
        if matches!(
            scope.mode,
            RirCollectionLoanMode::MutableSequenceElement | RirCollectionLoanMode::MutableMapValue
        ) {
            if !self.collection_loan_root_mutable(function, &scope.root) {
                self.push(site, RirVerifyErrorKind::ImmutableAssign);
            }
            let steps = match &scope.root {
                RirCollectionAccess::Direct(place) => &place.projections,
                RirCollectionAccess::MutPlace(place) => &place.projections,
            };
            if steps
                .iter()
                .any(|step| matches!(step.kind, RirPlaceStepKind::ExternField(_)))
            {
                self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
            }
        }
        self.collection_loans.push(ActiveRirCollectionLoan {
            root: scope.root.clone(),
            mode: scope.mode,
        });
        let state = self.check_structured_block(
            function_id,
            function,
            &scope.body,
            self.block_entry_state(),
            None,
        );
        self.collection_loans.pop();
        self.merge_structured_states([state]);
    }

    fn collection_loan_root_mutable(
        &self,
        function: &RirFunction,
        root: &RirCollectionAccess,
    ) -> bool {
        match root {
            RirCollectionAccess::Direct(root) => match root.root {
                RirPlaceRoot::Local(_) => Self::place_is_mutable_root(function, root),
                RirPlaceRoot::Global(global) => self
                    .program
                    .globals
                    .get(global.index())
                    .is_some_and(|global| global.mutable),
            },
            RirCollectionAccess::MutPlace(_) => true,
        }
    }

    fn check_collection_access(
        &mut self,
        site: RirVerifySite,
        function_id: RirFunctionId,
        function: &RirFunction,
        root: &RirCollectionAccess,
        use_: MutPlaceUse,
    ) -> Option<RirTypeId> {
        match root {
            RirCollectionAccess::Direct(root) => {
                self.check_place(site, function, root);
                self.program.place_ty(function, root)
            }
            RirCollectionAccess::MutPlace(root) => {
                self.check_mut_place_arg(site, function_id, function, root, use_)
            }
        }
    }

    fn check_map_access(
        &mut self,
        site: RirVerifySite,
        function_id: RirFunctionId,
        function: &RirFunction,
        map: &RirCollectionAccess,
        use_: MutPlaceUse,
    ) -> Option<(RirTypeId, RirTypeId)> {
        let ty = self.check_collection_access(site, function_id, function, map, use_)?;
        let Some(RirType::Map { key, value }) = self.ty(ty) else {
            self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
            return None;
        };
        Some((key, value))
    }

    fn check_map_slot(
        &mut self,
        site: RirVerifySite,
        function_id: RirFunctionId,
        function: &RirFunction,
        map: &RirCollectionAccess,
        index: RirLocalId,
        write: bool,
    ) -> Option<(RirTypeId, RirTypeId)> {
        let use_ = if write {
            MutPlaceUse::IndexedMapAssignment
        } else {
            MutPlaceUse::MapRead
        };
        let item = self.check_map_access(site, function_id, function, map, use_);
        self.check_sequence_index_local(site, function, index);
        if self.slot_mode_conflicts(
            map,
            write,
            |mode| {
                matches!(
                    mode,
                    RirCollectionLoanMode::ReadonlyMap | RirCollectionLoanMode::MutableMapValue
                )
            },
            |mode| matches!(mode, RirCollectionLoanMode::MutableMapValue),
        ) {
            self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
        }
        item
    }

    fn slot_mode_conflicts(
        &self,
        root: &RirCollectionAccess,
        write: bool,
        read_ok: impl Fn(RirCollectionLoanMode) -> bool,
        write_ok: impl Fn(RirCollectionLoanMode) -> bool,
    ) -> bool {
        let mut saw_loan = false;
        let mut saw_writable = false;
        for loan in self
            .collection_loans
            .iter()
            .rev()
            .filter(|loan| loan.root == *root)
        {
            if !write {
                return !read_ok(loan.mode);
            }
            saw_loan = true;
            saw_writable |= write_ok(loan.mode);
        }
        saw_loan && !saw_writable
    }

    fn check_sequence_slot(
        &mut self,
        site: RirVerifySite,
        function_id: RirFunctionId,
        function: &RirFunction,
        collection: &RirCollectionAccess,
        index: RirLocalId,
        expected_ty: Option<RirTypeId>,
        write: bool,
    ) -> Option<RirTypeId> {
        let collection_ty = self.check_collection_access(
            site,
            function_id,
            function,
            collection,
            MutPlaceUse::CallArg,
        );
        self.check_sequence_index_local(site, function, index);
        let collection_ty = collection_ty?;
        let ty = self.ty(collection_ty);
        if matches!(collection, RirCollectionAccess::MutPlace(_))
            && !matches!(
                ty,
                Some(RirType::Array { .. } | RirType::List(_) | RirType::Slice(_))
            )
        {
            self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
        }
        let elem_ty = self.sequence_elem(collection_ty);
        if elem_ty.is_none() {
            self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
        }
        if !write
            && matches!(ty, Some(RirType::Array { .. }))
            && self
                .program
                .collection_storage_for(collection_ty)
                .is_none_or(|storage| {
                    !matches!(
                        storage.kind,
                        RirCollectionStorageKind::Array { elem_ty: stored, .. }
                            if Some(stored) == elem_ty
                    )
                })
        {
            self.push(site, RirVerifyErrorKind::BadId);
        }
        if let (Some(expected), Some(elem_ty)) = (expected_ty, elem_ty)
            && expected != elem_ty
        {
            self.push(
                site,
                RirVerifyErrorKind::TypeMismatch {
                    expected: elem_ty,
                    found: expected,
                },
            );
        }
        if self.slot_mode_conflicts(
            collection,
            write,
            |mode| {
                matches!(
                    mode,
                    RirCollectionLoanMode::ReadonlySequence
                        | RirCollectionLoanMode::MutableSequenceElement
                )
            },
            |mode| matches!(mode, RirCollectionLoanMode::MutableSequenceElement),
        ) {
            self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
        }
        elem_ty
    }

    fn check_collection_loan_root(
        &mut self,
        site: RirVerifySite,
        function: &RirFunction,
        root: &RirPlace,
    ) {
        let root_ty = match root.root {
            RirPlaceRoot::Local(local) => {
                self.local_root(site, root);
                if Self::function_local_is_mut_place_param(function, local) {
                    self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                }
                function.locals.get(local.index()).map(|local| local.ty)
            }
            RirPlaceRoot::Global(global) => {
                let Some(root_ty) = self.check_global_id(site, global).map(|global| global.ty)
                else {
                    return;
                };
                if !self
                    .program
                    .place_ty(function, root)
                    .is_some_and(|ty| self.global_payload_supported(ty))
                {
                    self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                }
                if !self
                    .global_initialized
                    .get(global.index())
                    .copied()
                    .unwrap_or(false)
                {
                    self.push(site, RirVerifyErrorKind::UninitializedGlobal(global));
                }
                Some(root_ty)
            }
        };
        if let Some(root_ty) = root_ty {
            self.check_collection_loan_steps(site, root_ty, &root.projections);
        }
    }

    fn check_collection_loan_steps(
        &mut self,
        site: RirVerifySite,
        root_ty: RirTypeId,
        steps: &[RirPlaceStep],
    ) {
        let mut ty = root_ty;
        for step in steps {
            let supported = matches!(
                (self.ty(ty), step.kind),
                (
                    Some(RirType::Struct(_)),
                    RirPlaceStepKind::StructField(_) | RirPlaceStepKind::ExternField(_),
                ) | (Some(RirType::Tuple(_)), RirPlaceStepKind::TupleField(_))
                    | (
                        Some(RirType::Array { .. }),
                        RirPlaceStepKind::ArrayIndex { .. }
                    )
            );
            if !supported || step.source_ty != ty {
                self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                return;
            }
            ty = step.target_ty;
        }
    }

    fn local_root(&mut self, site: RirVerifySite, place: &RirPlace) -> Option<RirLocalId> {
        match place.root {
            RirPlaceRoot::Local(local) => Some(local),
            RirPlaceRoot::Global(_) => {
                self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                None
            }
        }
    }

    fn local_root_mutable(function: &RirFunction, local: RirLocalId) -> bool {
        function
            .locals
            .get(local.index())
            .is_some_and(|local| local.mutable)
    }

    fn local_root_immutable(function: &RirFunction, local: RirLocalId) -> bool {
        function
            .locals
            .get(local.index())
            .is_some_and(|local| !local.mutable)
    }

    fn check_mutable_local_root(
        &mut self,
        site: RirVerifySite,
        function: &RirFunction,
        local: Option<RirLocalId>,
    ) {
        if local.is_some_and(|local| Self::local_root_immutable(function, local)) {
            self.push(site, RirVerifyErrorKind::ImmutableAssign);
        }
    }

    fn place_is_mutable_root(function: &RirFunction, place: &RirPlace) -> bool {
        match place.root {
            RirPlaceRoot::Local(local) => {
                Self::local_root_mutable(function, local)
                    || Self::place_is_mut_place_param_root(function, place)
            }
            RirPlaceRoot::Global(_) => false,
        }
    }

    fn global_replaces_active_collection_root(&self, global: RirGlobalId) -> bool {
        self.collection_loans
            .iter()
            .any(|loan| loan.root.is_direct_global(global))
    }

    fn mut_place_set_replaces_active_collection_root(&self, place: &RirMutPlaceArg) -> bool {
        let RirMutPlaceAccess::Handle(RirMutPlaceHandle::Global { global }) = &place.access else {
            return false;
        };
        let dst = RirPlace::global(*global, place.projections.clone());
        self.assignment_replaces_active_collection_root(&dst)
    }

    fn assignment_replaces_active_collection_root(&self, dst: &RirPlace) -> bool {
        self.collection_loans
            .iter()
            .any(|loan| loan.root.replaced_by_place(dst))
    }

    fn check_int_operands<'b>(
        &mut self,
        site: RirVerifySite,
        function: &RirFunction,
        operands: impl IntoIterator<Item = &'b RirOperand>,
        int: RirTypeId,
    ) {
        for operand in operands {
            self.check_value_operand_ty(site, function, operand, int);
        }
    }

    fn check_int_locals(
        &mut self,
        site: RirVerifySite,
        function: &RirFunction,
        locals: impl IntoIterator<Item = RirLocalId>,
        int: RirTypeId,
    ) {
        for local in locals {
            let Some(local_decl) = function.locals.get(local.index()) else {
                self.push(site, RirVerifyErrorKind::BadId);
                continue;
            };
            if local_decl.ty != int {
                self.push(
                    site,
                    RirVerifyErrorKind::TypeMismatch {
                        expected: int,
                        found: local_decl.ty,
                    },
                );
            }
        }
    }

    fn init_entry_locals(
        entry: &mut RirBlockEntryState,
        locals: impl IntoIterator<Item = RirLocalId>,
    ) {
        for local in locals {
            if local.index() < entry.definite.len() {
                entry.definite[local.index()] = true;
                entry.possible[local.index()] = true;
            }
        }
    }

    fn block_entry_state(&self) -> RirBlockEntryState {
        RirBlockEntryState {
            definite: self.initialized.clone(),
            possible: self.possibly_initialized.clone(),
            lambda_escapes: self.lambda_escapes.clone(),
            lambda_values: self.lambda_values.clone(),
            loop_lambda_scopes: self.loop_lambda_scopes.clone(),
            global_initialized: self.global_initialized.clone(),
            global_values: self.global_values.clone(),
        }
    }

    fn current_block_state(&self) -> RirBlockState {
        RirBlockState {
            definite: self.initialized.clone(),
            possible: self.possibly_initialized.clone(),
            lambda_escapes: self.lambda_escapes.clone(),
            lambda_values: self.lambda_values.clone(),
            loop_lambda_scopes: self.loop_lambda_scopes.clone(),
            cell_definite: self.initialized_cells.clone(),
            cell_possible: self.possibly_initialized_cells.clone(),
            globals: self.global_initialized.clone(),
            global_values: self.global_values.clone(),
        }
    }

    fn check_structured_block(
        &mut self,
        function_id: RirFunctionId,
        function: &RirFunction,
        body: &RirStructuredBlock,
        entry: RirBlockEntryState,
        preserved_payload_ref: Option<RirLocalId>,
    ) -> Option<RirBlockState> {
        let outer_definite = std::mem::replace(&mut self.initialized, entry.definite);
        let outer_possible = std::mem::replace(&mut self.possibly_initialized, entry.possible);
        let outer_lambda_escapes =
            std::mem::replace(&mut self.lambda_escapes, entry.lambda_escapes);
        let outer_lambda_values = std::mem::replace(&mut self.lambda_values, entry.lambda_values);
        let outer_loop_lambda_scopes =
            std::mem::replace(&mut self.loop_lambda_scopes, entry.loop_lambda_scopes);
        let outer_globals =
            std::mem::replace(&mut self.global_initialized, entry.global_initialized);
        let outer_global_values = std::mem::replace(&mut self.global_values, entry.global_values);
        let outer_cell_definite = self.initialized_cells.clone();
        let outer_cell_possible = self.possibly_initialized_cells.clone();
        let outer_scope_depth = self.scope_depth;
        self.scope_depth += 1;
        for (index, stmt) in body.stmts.iter().enumerate() {
            self.check_stmt(function_id, function, index, stmt);
        }
        self.check_term(function_id, function, &body.term);
        let falls_through = self.structured_block_falls_through(body);
        let result = falls_through.then(|| {
            let mut definite = self.initialized.clone();
            let mut possible = self.possibly_initialized.clone();
            let lambda_escapes = self.lambda_escapes.clone();
            let lambda_values = self.lambda_values.clone();
            let loop_lambda_scopes = self.loop_lambda_scopes.clone();
            let cell_definite = self.initialized_cells.clone();
            let cell_possible = self.possibly_initialized_cells.clone();
            let globals = self.global_initialized.clone();
            let global_values = self.global_values.clone();
            for local in &function.locals {
                let payload_bound = self
                    .payload_bound
                    .get(local.id.index())
                    .copied()
                    .unwrap_or(false);
                if matches!(
                    local.binding,
                    RirLocalBinding::DirectPayload | RirLocalBinding::ScopedPlacePayload
                ) && Some(local.id) != preserved_payload_ref
                    && !payload_bound
                {
                    if let Some(slot) = definite.get_mut(local.id.index()) {
                        *slot = false;
                    }
                    if let Some(slot) = possible.get_mut(local.id.index()) {
                        *slot = false;
                    }
                }
            }
            RirBlockState {
                definite,
                possible,
                lambda_escapes,
                lambda_values,
                loop_lambda_scopes,
                cell_definite,
                cell_possible,
                globals,
                global_values,
            }
        });
        self.initialized = outer_definite;
        self.possibly_initialized = outer_possible;
        self.lambda_escapes = outer_lambda_escapes;
        self.lambda_values = outer_lambda_values;
        self.loop_lambda_scopes = outer_loop_lambda_scopes;
        self.global_initialized = outer_globals;
        self.global_values = outer_global_values;
        self.initialized_cells = outer_cell_definite;
        self.possibly_initialized_cells = outer_cell_possible;
        self.scope_depth = outer_scope_depth;
        result
    }

    fn structured_block_falls_through(&self, block: &RirStructuredBlock) -> bool {
        for stmt in &block.stmts {
            if !self.stmt_falls_through(stmt) {
                return false;
            }
        }
        matches!(block.term, RirTerm::None)
    }

    fn stmt_falls_through(&self, stmt: &RirStmt) -> bool {
        match stmt {
            RirStmt::If(branch) => {
                branch
                    .else_block
                    .as_ref()
                    .is_none_or(|block| self.structured_block_falls_through(block))
                    || self.structured_block_falls_through(&branch.then_block)
            }
            RirStmt::PatternMatch(match_) => match_
                .arms
                .iter()
                .any(|arm| self.structured_block_falls_through(&arm.block)),
            RirStmt::DynMatch(match_) => {
                match_
                    .arms
                    .iter()
                    .any(|arm| self.structured_block_falls_through(&arm.block))
                    || self.structured_block_falls_through(&match_.fallback)
            }
            RirStmt::OptionMatch(match_) => {
                self.structured_block_falls_through(&match_.some_block)
                    || self.structured_block_falls_through(&match_.none_block)
            }
            RirStmt::MapEntryMatch(match_) => {
                self.structured_block_falls_through(&match_.some_block)
                    || self.structured_block_falls_through(&match_.none_block)
            }
            RirStmt::CollectionSlotScope(block) => self.structured_block_falls_through(block),
            RirStmt::Loop(_)
            | RirStmt::RangeFor(_)
            | RirStmt::CollectionFor(_)
            | RirStmt::CollectionLoanScope(_)
            | RirStmt::Init { .. }
            | RirStmt::GlobalEnsure { .. }
            | RirStmt::GlobalSetRoot { .. }
            | RirStmt::GlobalUpdateRoot { .. }
            | RirStmt::MutPlaceSet { .. }
            | RirStmt::Assign { .. }
            | RirStmt::CellInit { .. }
            | RirStmt::ScopedPlaceCellInit { .. }
            | RirStmt::CellSet { .. }
            | RirStmt::ScopedPlaceCellSet { .. }
            | RirStmt::DataRefSet { .. }
            | RirStmt::SequenceSlotSet { .. }
            | RirStmt::MapValueSet { .. }
            | RirStmt::Eval(_) => true,
        }
    }

    fn merge_structured_states(&mut self, states: impl IntoIterator<Item = Option<RirBlockState>>) {
        let mut states = states.into_iter().flatten();
        let Some(mut merged) = states.next() else {
            return;
        };
        for state in states {
            merged.merge_with(state);
        }
        self.initialized = merged.definite;
        self.possibly_initialized = merged.possible;
        self.lambda_escapes = merged.lambda_escapes;
        self.lambda_values = merged.lambda_values;
        self.loop_lambda_scopes = merged.loop_lambda_scopes;
        self.initialized_cells = merged.cell_definite;
        self.possibly_initialized_cells = merged.cell_possible;
        self.global_initialized = merged.globals;
        self.global_values = merged.global_values;
    }

    fn check_lambda_capture_args(
        &mut self,
        site: RirVerifySite,
        function_id: RirFunctionId,
        function: &RirFunction,
        lambda: &RirLambda,
        captures: &[RirLambdaCaptureArg],
    ) {
        if captures.len() != lambda.captures.len() {
            self.push(
                site,
                RirVerifyErrorKind::CallArgCount {
                    expected: lambda.captures.len(),
                    found: captures.len(),
                },
            );
            return;
        }
        for (arg, decl) in captures.iter().zip(&lambda.captures) {
            match (arg, decl.mode) {
                (RirLambdaCaptureArg::Owned { value }, RirPassMode::Value) => {
                    let found =
                        self.check_owned_value(site, function, value, RustRecipePosition::Value);
                    if found != Some(decl.ty) {
                        self.push(
                            site,
                            RirVerifyErrorKind::TypeMismatch {
                                expected: decl.ty,
                                found: found.unwrap_or(decl.ty),
                            },
                        );
                    }
                    if matches!(lambda.storage, RirLambdaStorage::HeapEnv { .. })
                        && matches!(self.ty(decl.ty), Some(RirType::Lambda(_)))
                        && self.owned_value_lambda_escape(function, value)
                            != Some(RirLambdaEscape::Escaping)
                    {
                        self.push(site, RirVerifyErrorKind::CallArgEscape);
                    }
                }
                (RirLambdaCaptureArg::Shared { place }, RirPassMode::SharedBorrow) => {
                    self.check_lambda_capture_place(site, function, place, decl.ty);
                }
                (RirLambdaCaptureArg::StackCell { cell }, RirPassMode::StackCell) => {
                    let Some(expected) =
                        Self::lambda_capture_kind_cell(decl.kind, RirCellStorage::StackScoped)
                    else {
                        self.push(site, RirVerifyErrorKind::UnsupportedLambdaCapture);
                        continue;
                    };
                    self.check_lambda_cell_capture_arg(
                        site,
                        function_id,
                        *cell,
                        expected,
                        decl.ty,
                        RirCellStorage::StackScoped,
                    );
                }
                (RirLambdaCaptureArg::HeapCell { cell }, RirPassMode::HeapCell) => {
                    let Some(expected) =
                        Self::lambda_capture_kind_cell(decl.kind, RirCellStorage::Heap)
                    else {
                        self.push(site, RirVerifyErrorKind::UnsupportedLambdaCapture);
                        continue;
                    };
                    self.check_lambda_cell_capture_arg(
                        site,
                        function_id,
                        *cell,
                        expected,
                        decl.ty,
                        RirCellStorage::Heap,
                    );
                }
                (RirLambdaCaptureArg::ScopedPlaceCell { cell }, RirPassMode::ScopedPlaceCell) => {
                    let RirLambdaCaptureKind::ScopedPlaceCell { cell: expected } = decl.kind else {
                        self.push(site, RirVerifyErrorKind::UnsupportedLambdaCapture);
                        continue;
                    };
                    let Some(cell_decl) =
                        self.check_function_scoped_place_cell_ref(site, function_id, *cell)
                    else {
                        continue;
                    };
                    if cell_decl.id != expected {
                        self.push(site, RirVerifyErrorKind::UnsupportedLambdaCapture);
                    }
                    if cell_decl.payload_ty != decl.ty {
                        self.push(
                            site,
                            RirVerifyErrorKind::TypeMismatch {
                                expected: cell_decl.payload_ty,
                                found: decl.ty,
                            },
                        );
                    }
                }
                _ => self.push(site, RirVerifyErrorKind::CallArgMode),
            }
        }
    }

    fn lambda_capture_kind_cell(
        kind: RirLambdaCaptureKind,
        storage: RirCellStorage,
    ) -> Option<RirCellId> {
        match (kind, storage) {
            (RirLambdaCaptureKind::StackCell { cell }, RirCellStorage::StackScoped)
            | (RirLambdaCaptureKind::HeapCell { cell }, RirCellStorage::Heap) => Some(cell),
            _ => None,
        }
    }

    fn check_lambda_cell_capture_arg(
        &mut self,
        site: RirVerifySite,
        function_id: RirFunctionId,
        cell: RirCellRef,
        expected: RirCellId,
        ty: RirTypeId,
        storage: RirCellStorage,
    ) {
        let Some(cell_decl) = self.check_function_cell_ref(site, function_id, cell) else {
            return;
        };
        if cell_decl.id != expected || cell_decl.storage != storage {
            self.push(site, RirVerifyErrorKind::UnsupportedLambdaCapture);
        }
        if cell_decl.payload_ty != ty {
            self.push(
                site,
                RirVerifyErrorKind::TypeMismatch {
                    expected: cell_decl.payload_ty,
                    found: ty,
                },
            );
        }
        if !self.cell_initialized(cell_decl.id) {
            self.push(site, RirVerifyErrorKind::UninitializedCell(cell_decl.id));
        }
    }

    fn check_lambda_capture_place(
        &mut self,
        site: RirVerifySite,
        function: &RirFunction,
        place: &RirPlace,
        expected: RirTypeId,
    ) {
        self.check_place(site, function, place);
        if !place.projections.is_empty() {
            self.push(site, RirVerifyErrorKind::UnsupportedLambdaCapture);
        }
        if let Some(found) = self.program.place_ty(function, place)
            && found != expected
        {
            self.push(site, RirVerifyErrorKind::TypeMismatch { expected, found });
        }
    }

    fn check_owned_value_ty(
        &mut self,
        site: RirVerifySite,
        function: &RirFunction,
        owned: &RirOwnedValue,
        expected: RirTypeId,
        position: RustRecipePosition,
    ) {
        if let Some(found) = self.check_owned_value(site, function, owned, position)
            && found != expected
        {
            self.push(site, RirVerifyErrorKind::TypeMismatch { expected, found });
        }
    }

    fn check_owned_value(
        &mut self,
        site: RirVerifySite,
        function: &RirFunction,
        owned: &RirOwnedValue,
        position: RustRecipePosition,
    ) -> Option<RirTypeId> {
        match owned.source {
            RirOwnedSource::Direct => match &owned.value {
                RirOwnedOperand::Value(RirOperand::Const(id)) => self
                    .program
                    .consts
                    .get(id.index())
                    .map(|constant| constant.ty),
                RirOwnedOperand::Value(_)
                | RirOwnedOperand::Access(_)
                | RirOwnedOperand::DynBorrow(_) => {
                    self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                    None
                }
            },
            RirOwnedSource::Reuse(materializer) => {
                let found = match &owned.value {
                    RirOwnedOperand::Value(value) => self.value_operand_ty(site, function, value),
                    RirOwnedOperand::Access(access) => self.check_mut_place_arg(
                        site,
                        function.id,
                        function,
                        access,
                        MutPlaceUse::OwnedRead,
                    ),
                    RirOwnedOperand::DynBorrow(borrow) => {
                        let direct_reborrow = matches!(
                            &borrow.source,
                            RirDynBorrowSource::Reborrowed { carrier, .. }
                                if *carrier == borrow.target
                        ) && borrow.weakening.is_none();
                        if !direct_reborrow {
                            self.push(site, RirVerifyErrorKind::InvalidDynPlan);
                        }
                        self.check_dyn_borrow(site, function.id, function, borrow);
                        self.program
                            .dyn_carriers
                            .get(borrow.target.index())
                            .map(|carrier| carrier.storage_ty)
                    }
                };
                let valid =
                    found.is_some_and(|ty| self.materializer_matches(ty, materializer, position));
                if !valid {
                    self.push(site, RirVerifyErrorKind::BadId);
                }
                found
            }
            RirOwnedSource::Transfer { local } => match &owned.value {
                RirOwnedOperand::Value(value) => {
                    self.check_moved_value(site, function, value, local)
                }
                RirOwnedOperand::Access(_) | RirOwnedOperand::DynBorrow(_) => {
                    self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                    None
                }
            },
        }
    }

    fn check_rvalue(
        &mut self,
        function_id: RirFunctionId,
        function: &RirFunction,
        index: usize,
        value: &RirRValue,
        expected: Option<RirTypeId>,
    ) {
        let site = RirVerifySite::RValue(function_id, index);
        let found = match value {
            RirRValue::Use(operand) => self.value_operand_ty(site, function, operand),
            RirRValue::TakeStaged(operand) => {
                let found = self.value_operand_ty(site, function, operand);
                let RirOperand::Place(place) = operand else {
                    self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                    return;
                };
                let RirPlaceRoot::Local(local) = place.root else {
                    self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                    return;
                };
                let valid = place.projections.is_empty()
                    && function.locals.get(local.index()).is_some_and(|decl| {
                        !decl.mutable && Some(decl.ty) == self.program.place_ty(function, place)
                    })
                    && function
                        .locals
                        .get(local.index())
                        .is_some_and(|local| matches!(local.binding, RirLocalBinding::Value));
                if !valid {
                    self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                } else if let Some(initialized) = self.initialized.get_mut(local.index()) {
                    *initialized = false;
                }
                found
            }
            RirRValue::Materialize(owned) => {
                self.check_owned_value(site, function, owned, RustRecipePosition::Value)
            }
            RirRValue::DynPack { variant, value, ty } => {
                self.check_type_id(site, *ty);
                let Some(carrier) = self.program.dyn_carriers.get(variant.carrier().index()) else {
                    self.push(site, RirVerifyErrorKind::InvalidDynCarrier);
                    return;
                };
                let Some(decl) = carrier.variants.get(variant.index()) else {
                    self.push(site, RirVerifyErrorKind::InvalidDynVariant);
                    return;
                };
                let found = self.check_owned_value(
                    site,
                    function,
                    value,
                    RustRecipePosition::StoredPayload(LambdaStorageFamily::DynamicPayload),
                );
                let payload_matches = match value.source {
                    RirOwnedSource::Reuse(materializer) => materializer == decl.payload,
                    RirOwnedSource::Direct | RirOwnedSource::Transfer { .. } => true,
                };
                if carrier.storage_ty != *ty || !payload_matches || found != Some(decl.concrete_ty)
                {
                    self.push(site, RirVerifyErrorKind::InvalidDynVariant);
                }
                Some(*ty)
            }
            RirRValue::DynWeaken {
                weakening,
                value,
                ty,
            } => {
                self.check_type_id(site, *ty);
                let Some(decl) = self.program.dyn_weakenings.get(weakening.index()) else {
                    self.push(site, RirVerifyErrorKind::InvalidDynStorage);
                    return;
                };
                let (Some(source), Some(target)) = (
                    self.program.dyn_carriers.get(decl.source.index()),
                    self.program.dyn_carriers.get(decl.target.index()),
                ) else {
                    self.push(site, RirVerifyErrorKind::InvalidDynCarrier);
                    return;
                };
                if target.storage_ty != *ty
                    || self.check_owned_value(site, function, value, RustRecipePosition::Value)
                        != Some(source.storage_ty)
                {
                    self.push(site, RirVerifyErrorKind::InvalidDynStorage);
                }
                Some(*ty)
            }
            RirRValue::DynDowncast {
                variants,
                value,
                ty,
            } => {
                self.check_type_id(site, *ty);
                let Some(set) = self.program.dyn_variant_sets.get(variants.index()) else {
                    self.push(site, RirVerifyErrorKind::InvalidDynVariant);
                    return;
                };
                let Some(carrier) = self.program.dyn_carriers.get(set.carrier.index()) else {
                    self.push(site, RirVerifyErrorKind::InvalidDynCarrier);
                    return;
                };
                if self.check_owned_value(site, function, value, RustRecipePosition::Value)
                    != Some(carrier.storage_ty)
                    || self.ty(*ty) != Some(RirType::Option(set.target))
                {
                    self.push(site, RirVerifyErrorKind::InvalidDynStorage);
                }
                Some(*ty)
            }
            RirRValue::DynCall {
                dispatch,
                exact_variant,
                receiver,
                args,
                ty,
            } => {
                self.check_type_id(site, *ty);
                let Some(dispatch) = self.program.dyn_dispatches.get(dispatch.index()) else {
                    self.push(site, RirVerifyErrorKind::InvalidDynDispatch);
                    return;
                };
                let Some(carrier) = self.program.dyn_carriers.get(dispatch.carrier.index()) else {
                    self.push(site, RirVerifyErrorKind::InvalidDynCarrier);
                    return;
                };
                let receiver_ty = match receiver {
                    RirDynReceiver::Owned(value) => {
                        self.check_owned_value(site, function, value, RustRecipePosition::Value)
                    }
                    RirDynReceiver::Borrowed(borrow) => {
                        self.check_dyn_borrow(site, function_id, function, borrow);
                        self.program
                            .dyn_carriers
                            .get(borrow.target.index())
                            .map(|carrier| carrier.storage_ty)
                    }
                    RirDynReceiver::MutPlace(place) => self.check_mut_place_arg(
                        site,
                        function_id,
                        function,
                        place,
                        MutPlaceUse::ReadCopy,
                    ),
                };
                if receiver_ty != Some(carrier.storage_ty)
                    || dispatch.result_ty != *ty
                    || exact_variant.is_some_and(|variant| variant.carrier() != carrier.id)
                {
                    self.push(site, RirVerifyErrorKind::InvalidDynStorage);
                }
                for (arg_index, arg) in args.iter().enumerate() {
                    self.check_untyped_dyn_arg(function_id, function, index, arg_index, arg);
                }
                if dispatch.arms.iter().any(|arm| {
                    !self.dyn_dispatch_args_valid(function, args, &dispatch.params)
                        || !Self::dyn_receiver_mode_valid(receiver, arm.receiver)
                        || matches!(arm.target.base(), RirResolvedCallTarget::Extern(id)
                        if args.iter().enumerate().any(|(index, arg)| {
                            self.program.externs[id.index()].rejects_reentry_arg(index + 1, arg)
                        }))
                }) {
                    self.push(site, RirVerifyErrorKind::InvalidDynDispatch);
                }
                Some(*ty)
            }
            RirRValue::FunctionValue { value, escape, ty } => {
                self.check_type_id(site, *ty);
                self.check_lambda_escape_proof(site, function, value, *escape);
                match (
                    self.check_owned_value(site, function, value, RustRecipePosition::Value),
                    self.ty(*ty),
                ) {
                    (Some(found), Some(RirType::Lambda(_))) if found == *ty => Some(*ty),
                    (Some(found), Some(RirType::Lambda(_))) => {
                        self.push(
                            site,
                            RirVerifyErrorKind::TypeMismatch {
                                expected: *ty,
                                found,
                            },
                        );
                        Some(*ty)
                    }
                    _ => {
                        self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                        Some(*ty)
                    }
                }
            }
            RirRValue::Struct { ty, fields } => {
                self.check_type_id(site, *ty);
                let Some(RirType::Struct(struct_id)) = self.ty(*ty) else {
                    self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                    return;
                };
                let Some(strukt) = self.program.structs.get(struct_id.index()).cloned() else {
                    self.push(site, RirVerifyErrorKind::BadId);
                    return;
                };
                Some(self.check_construct_fields(
                    site,
                    function,
                    *ty,
                    &strukt.fields,
                    fields,
                    LambdaStorageFamily::StructField,
                ))
            }
            RirRValue::Tuple { ty, fields } => {
                self.check_type_id(site, *ty);
                let Some(RirType::Tuple(tuple_id)) = self.ty(*ty) else {
                    self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                    return;
                };
                let Some(tuple) = self.program.tuples.get(tuple_id.index()).cloned() else {
                    self.push(site, RirVerifyErrorKind::BadId);
                    return;
                };
                Some(self.check_construct_fields(
                    site,
                    function,
                    *ty,
                    &tuple.fields,
                    fields,
                    LambdaStorageFamily::TupleField,
                ))
            }
            RirRValue::DataRefAlloc { ty, fields } => {
                self.check_type_id(site, *ty);
                let Some(RirType::DataRef(dataref_id)) = self.ty(*ty) else {
                    self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                    return;
                };
                let Some(dataref) = self.program.datarefs.get(dataref_id.index()).cloned() else {
                    self.push(site, RirVerifyErrorKind::BadId);
                    return;
                };
                Some(self.check_construct_fields(
                    site,
                    function,
                    *ty,
                    &dataref.fields,
                    fields,
                    LambdaStorageFamily::DataRefProjection,
                ))
            }
            RirRValue::Array { ty, elems } => {
                self.check_type_id(site, *ty);
                let Some(RirType::Array { elem, len }) = self.ty(*ty) else {
                    self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                    return;
                };
                if elems.len() != len as usize {
                    self.push(
                        site,
                        RirVerifyErrorKind::FieldCount {
                            expected: len as usize,
                            found: elems.len(),
                        },
                    );
                }
                for elem_value in elems {
                    self.check_owned_value_ty(
                        site,
                        function,
                        elem_value,
                        elem,
                        RustRecipePosition::StoredPayload(LambdaStorageFamily::FixedArrayElement),
                    );
                }
                Some(*ty)
            }
            RirRValue::List { ty, elems } => {
                self.check_type_id(site, *ty);
                let Some(RirType::List(elem)) = self.ty(*ty) else {
                    self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                    return;
                };
                for elem_value in elems {
                    self.check_owned_value_ty(
                        site,
                        function,
                        elem_value,
                        elem,
                        RustRecipePosition::StoredPayload(LambdaStorageFamily::ListElement),
                    );
                }
                Some(*ty)
            }
            RirRValue::Map { ty, entries } => {
                self.check_type_id(site, *ty);
                let Some(RirType::Map { key, value }) = self.ty(*ty) else {
                    self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                    return;
                };
                for (entry_key, entry_value) in entries {
                    self.check_owned_value_ty(
                        site,
                        function,
                        entry_key,
                        key,
                        RustRecipePosition::MapKey,
                    );
                    self.check_owned_value_ty(
                        site,
                        function,
                        entry_value,
                        value,
                        RustRecipePosition::StoredPayload(LambdaStorageFamily::MapValue),
                    );
                }
                Some(*ty)
            }
            RirRValue::EnumVariant {
                ty,
                variant,
                fields,
            } => {
                self.check_type_id(site, *ty);
                let Some(RirType::Enum(enum_id)) = self.ty(*ty) else {
                    self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                    return;
                };
                let Some(enm) = self.program.enums.get(enum_id.index()) else {
                    self.push(site, RirVerifyErrorKind::BadId);
                    return;
                };
                let Some(variant) = enm.variants.get(variant.index()).cloned() else {
                    self.push(site, RirVerifyErrorKind::BadId);
                    return;
                };
                if fields.len() != variant.fields.len() {
                    self.push(
                        site,
                        RirVerifyErrorKind::FieldCount {
                            expected: variant.fields.len(),
                            found: fields.len(),
                        },
                    );
                }
                for (field, operand) in variant.fields.iter().zip(fields) {
                    self.check_owned_value_ty(
                        site,
                        function,
                        operand,
                        field.ty,
                        RustRecipePosition::StoredPayload(LambdaStorageFamily::EnumPayload),
                    );
                }
                Some(*ty)
            }
            RirRValue::Unary { op, value, ty } => {
                let value_ty = self.operand_ty(site, function, value);
                self.check_type_id(site, *ty);
                if !self.unary_ok(*op, value_ty, *ty) {
                    self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                }
                Some(*ty)
            }
            RirRValue::SharedRefEq { lhs, rhs, .. } => {
                let lhs_ty = self.operand_ty(site, function, lhs);
                let rhs_ty = self.operand_ty(site, function, rhs);
                if lhs_ty != rhs_ty
                    || !matches!(lhs_ty.and_then(|ty| self.ty(ty)), Some(RirType::DataRef(_)))
                {
                    self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                }
                self.type_id(RirType::Bool)
            }
            RirRValue::Binary { op, lhs, rhs, ty } => {
                let lhs_ty = self.operand_ty(site, function, lhs);
                let rhs_ty = self.operand_ty(site, function, rhs);
                self.check_type_id(site, *ty);
                if op.has_lazy_rhs() || !self.binary_ok(*op, lhs_ty, rhs_ty, *ty) {
                    self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                }
                Some(*ty)
            }
            RirRValue::Cast { value, target } => {
                let value_ty = self.operand_ty(site, function, value);
                self.check_type_id(site, *target);
                if !self.cast_ok(value_ty, *target) {
                    self.push(site, RirVerifyErrorKind::InvalidNumericCast);
                }
                Some(*target)
            }
            RirRValue::RawProject { value, target } => {
                let value_ty = self.operand_ty(site, function, value);
                self.check_type_id(site, *target);
                if !self.raw_project_ok(value_ty, *target) {
                    self.push(site, RirVerifyErrorKind::InvalidRawProject);
                }
                Some(*target)
            }
            RirRValue::RawTryConstruct { value, target, ty } => {
                let value_ty = self.operand_ty(site, function, value);
                self.check_type_id(site, *target);
                self.check_type_id(site, *ty);
                if !self.raw_try_construct_ok(value_ty, *target, *ty) {
                    self.push(site, RirVerifyErrorKind::InvalidRawTryConstruct);
                }
                Some(*ty)
            }
            RirRValue::FlagStatic { ty, .. } => {
                if !matches!(self.ty(*ty), Some(RirType::Flag(_))) {
                    self.push(site, RirVerifyErrorKind::InvalidFlagStatic);
                }
                Some(*ty)
            }
            RirRValue::OptionalSome { value, ty } => {
                let value_ty = self.check_owned_value(
                    site,
                    function,
                    value,
                    RustRecipePosition::StoredPayload(LambdaStorageFamily::OptionalPayload),
                );
                self.check_type_id(site, *ty);
                match (self.ty(*ty), value_ty) {
                    (Some(RirType::Option(inner)), Some(value_ty)) if inner == value_ty => {}
                    _ => self.push(site, RirVerifyErrorKind::UnsupportedRValueType),
                }
                Some(*ty)
            }
            RirRValue::Stringify { value, source_ty } => {
                let value_ty = self.operand_ty(site, function, value);
                self.check_type_id(site, *source_ty);
                if value_ty != Some(*source_ty) || !self.stringify_ok(value, *source_ty) {
                    self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                }
                self.type_id(RirType::String)
            }
            RirRValue::StringConcat { parts } => {
                for part in parts {
                    let ty = self.operand_ty(site, function, part);
                    if ty.is_none_or(|ty| self.ty(ty) != Some(RirType::String)) {
                        self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                    }
                }
                self.type_id(RirType::String)
            }
            RirRValue::Format {
                value,
                source_ty,
                spec,
            } => {
                let value_ty = self.operand_ty(site, function, value);
                self.check_type_id(site, *source_ty);
                if value_ty != Some(*source_ty) || !self.format_ok(*source_ty, *spec) {
                    self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                }
                self.type_id(RirType::String)
            }
            RirRValue::Call { callee, args, ty } => {
                self.check_type_id(site, *ty);
                self.check_call(function_id, function, index, callee.clone(), args, *ty);
                Some(*ty)
            }
            RirRValue::Len { source } => {
                self.check_place(site, function, source);
                if !matches!(
                    self.program
                        .place_ty(function, source)
                        .and_then(|ty| self.ty(ty)),
                    Some(
                        RirType::String
                            | RirType::Array { .. }
                            | RirType::List(_)
                            | RirType::Map { .. }
                            | RirType::Slice(_)
                    )
                ) {
                    self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                }
                self.type_id(RirType::Int)
            }
            RirRValue::CollectionLen { source } => {
                let source_ty = self.check_collection_access(
                    site,
                    function_id,
                    function,
                    source,
                    MutPlaceUse::CallArg,
                );
                if !matches!(
                    source_ty.and_then(|ty| self.ty(ty)),
                    Some(
                        RirType::Array { .. }
                            | RirType::List(_)
                            | RirType::Map { .. }
                            | RirType::Slice(_)
                    )
                ) {
                    self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                }
                self.type_id(RirType::Int)
            }
            RirRValue::SequenceSlotAt { collection, step } => {
                self.check_type_id(site, step.target_ty);
                if let Some(collection_ty) = self.program.collection_ty(function, collection) {
                    self.check_place_step(site, function, collection_ty, step, true);
                }
                self.check_sequence_slot(
                    site,
                    function_id,
                    function,
                    collection,
                    step.index_local().expect("sequence slot step"),
                    Some(step.target_ty),
                    false,
                );
                Some(step.target_ty)
            }
            RirRValue::ListPush { list, value } => {
                let list_ty = self.check_collection_access(
                    site,
                    function_id,
                    function,
                    list,
                    MutPlaceUse::CollectionMutation,
                );
                if !self.collection_loan_root_mutable(function, list) {
                    self.push(site, RirVerifyErrorKind::ImmutableAssign);
                }
                let Some(RirType::List(elem)) = list_ty.and_then(|ty| self.ty(ty)) else {
                    self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                    return;
                };
                self.check_owned_value_ty(
                    site,
                    function,
                    value,
                    elem,
                    RustRecipePosition::StoredPayload(LambdaStorageFamily::ListElement),
                );
                self.type_id(RirType::Void)
            }
            RirRValue::SliceView {
                source,
                start,
                end,
                mutable,
                ty,
                ..
            } => {
                self.check_range_locals(site, function, *start, *end);
                if matches!(
                    source,
                    RirCollectionAccess::Direct(RirPlace {
                        root: RirPlaceRoot::Global(_),
                        ..
                    })
                ) {
                    self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                }
                let source_ty = self.check_collection_access(
                    site,
                    function_id,
                    function,
                    source,
                    if *mutable {
                        MutPlaceUse::CollectionMutation
                    } else {
                        MutPlaceUse::SliceViewRead
                    },
                );
                let Some(RirType::Slice(elem)) = self.ty(*ty) else {
                    self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                    return;
                };
                if *mutable && !self.collection_loan_root_mutable(function, source) {
                    self.push(site, RirVerifyErrorKind::ImmutableAssign);
                }
                if source_ty.and_then(|ty| self.sequence_elem(ty)) != Some(elem) {
                    self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                }
                Some(*ty)
            }
            RirRValue::RangeListCopy {
                source,
                start,
                end,
                ty,
                ..
            } => {
                self.check_slice_range(site, function, source, *start, *end);
                let Some(RirType::List(elem)) = self.ty(*ty) else {
                    self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                    return;
                };
                if self
                    .program
                    .place_ty(function, source)
                    .and_then(|ty| self.sequence_elem(ty))
                    != Some(elem)
                {
                    self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                } else if !RirRustRepPolicy::new(self.program).value_from_ref_supported(elem) {
                    self.push(site, RirVerifyErrorKind::NonCopyValueRequired);
                }
                Some(*ty)
            }
            RirRValue::MapGet { map, key, ty } => {
                self.check_type_id(site, *ty);
                if matches!(map, RirCollectionAccess::MutPlace(_)) {
                    self.check_short_region_operand(site, function, key);
                }
                let Some((key_ty, value)) =
                    self.check_map_access(site, function_id, function, map, MutPlaceUse::MapRead)
                else {
                    return;
                };
                let Some(RirType::Option(option_value)) = self.ty(*ty) else {
                    self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                    return;
                };
                if option_value != value {
                    self.push(
                        site,
                        RirVerifyErrorKind::TypeMismatch {
                            expected: value,
                            found: option_value,
                        },
                    );
                }
                self.check_value_operand_ty(site, function, key, key_ty);
                Some(*ty)
            }
            RirRValue::MapRemove { map, key, ty } => {
                self.check_type_id(site, *ty);
                if !self.collection_loan_root_mutable(function, map) {
                    self.push(site, RirVerifyErrorKind::ImmutableAssign);
                }
                let Some((key_ty, value)) = self.check_map_access(
                    site,
                    function_id,
                    function,
                    map,
                    MutPlaceUse::CollectionMutation,
                ) else {
                    return;
                };
                let Some(RirType::Option(option_value)) = self.ty(*ty) else {
                    self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                    return;
                };
                if option_value != value {
                    self.push(
                        site,
                        RirVerifyErrorKind::TypeMismatch {
                            expected: value,
                            found: option_value,
                        },
                    );
                }
                self.check_value_operand_ty(site, function, key, key_ty);
                Some(*ty)
            }
            RirRValue::MapEntryAt { map, index, ty } => {
                self.check_type_id(site, *ty);
                let Some((key, value)) =
                    self.check_map_slot(site, function_id, function, map, *index, false)
                else {
                    return;
                };
                let Some(RirType::Tuple(tuple_id)) = self.ty(*ty) else {
                    self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                    return;
                };
                let Some(tuple) = self.program.tuples.get(tuple_id.index()) else {
                    self.push(site, RirVerifyErrorKind::BadId);
                    return;
                };
                if tuple.fields.len() != 2
                    || tuple.fields[0].ty != key
                    || tuple.fields[1].ty != value
                {
                    self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                }
                Some(*ty)
            }
            RirRValue::MapKeyAt { map, index, ty } => {
                self.check_type_id(site, *ty);
                let Some((key, _)) =
                    self.check_map_slot(site, function_id, function, map, *index, false)
                else {
                    return;
                };
                if key != *ty {
                    self.push(
                        site,
                        RirVerifyErrorKind::TypeMismatch {
                            expected: key,
                            found: *ty,
                        },
                    );
                }
                Some(*ty)
            }
            RirRValue::MapValueAt { map, index, ty } => {
                self.check_type_id(site, *ty);
                let Some((_, value)) =
                    self.check_map_slot(site, function_id, function, map, *index, false)
                else {
                    return;
                };
                if value != *ty {
                    self.push(
                        site,
                        RirVerifyErrorKind::TypeMismatch {
                            expected: value,
                            found: *ty,
                        },
                    );
                }
                Some(*ty)
            }
            RirRValue::CheckedIterCount { count, .. } => {
                if let Some(int) = self.type_id(RirType::Int) {
                    self.check_value_operand_ty(site, function, count, int);
                }
                self.type_id(RirType::Int)
            }
            RirRValue::Lambda {
                lambda,
                captures,
                ty,
            } => {
                self.check_type_id(site, *ty);
                let Some(RirType::Lambda(sig)) = self.ty(*ty) else {
                    self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                    return;
                };
                match self.program.lambdas.get(lambda.index()) {
                    Some(decl) if decl.sig == sig => {
                        self.check_lambda_capture_args(site, function_id, function, decl, captures);
                    }
                    Some(_) => self.push(site, RirVerifyErrorKind::UnsupportedRValueType),
                    None => self.push(site, RirVerifyErrorKind::BadId),
                }
                Some(*ty)
            }
            RirRValue::MapInsert {
                map,
                key,
                value,
                kind,
            } => {
                let (key_ty, value_ty) = match (kind, map) {
                    (RirMapWriteKind::IndexedAssignment, RirCollectionAccess::MutPlace(map)) => {
                        let map_ty = self.check_mut_place_arg(
                            site,
                            function_id,
                            function,
                            map,
                            MutPlaceUse::IndexedMapAssignment,
                        );
                        let Some(RirType::Map { key, value }) = map_ty.and_then(|ty| self.ty(ty))
                        else {
                            self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                            return;
                        };
                        (key, value)
                    }
                    _ => {
                        if !self.collection_loan_root_mutable(function, map) {
                            self.push(site, RirVerifyErrorKind::ImmutableAssign);
                        }
                        let Some((key_ty, value_ty)) = self.check_map_access(
                            site,
                            function_id,
                            function,
                            map,
                            MutPlaceUse::CollectionMutation,
                        ) else {
                            return;
                        };
                        (key_ty, value_ty)
                    }
                };
                self.check_owned_value_ty(site, function, key, key_ty, RustRecipePosition::MapKey);
                self.check_owned_value_ty(
                    site,
                    function,
                    value,
                    value_ty,
                    RustRecipePosition::StoredPayload(LambdaStorageFamily::MapValue),
                );
                self.type_id(RirType::Void)
            }
        };
        if let (Some(expected), Some(found)) = (expected, found)
            && expected != found
        {
            self.push(site, RirVerifyErrorKind::TypeMismatch { expected, found });
        }
    }

    fn call_arg_lambda_escape(
        &self,
        function: &RirFunction,
        arg: &RirCallArg,
    ) -> Option<RirLambdaEscape> {
        match arg {
            RirCallArg::Value(operand)
            | RirCallArg::InitFieldProvided(operand)
            | RirCallArg::ScopedLambda {
                callee: operand, ..
            }
            | RirCallArg::EscapingLambda {
                callee: operand, ..
            }
            | RirCallArg::AnvCallback {
                callee: operand, ..
            } => self.owned_value_lambda_escape(function, operand),
            RirCallArg::SharedBorrow(place)
                if matches!(
                    self.program
                        .place_ty(function, place)
                        .and_then(|ty| self.program.types.get(ty.index())),
                    Some(RirType::Lambda(_))
                ) =>
            {
                self.operand_lambda_escape(function, &RirOperand::Place(place.clone()))
            }
            RirCallArg::SharedBorrow(_)
            | RirCallArg::MutBorrow(_)
            | RirCallArg::MutPlace(_)
            | RirCallArg::DynBorrow(_)
            | RirCallArg::InitFieldOmitted
            | RirCallArg::SharedStringConst(_) => None,
        }
    }

    fn dyn_dispatch_target_valid(
        &self,
        mut receiver_ty: RirTypeId,
        arm: &RirDynDispatchArm,
        params: &[RirDynDispatchParam],
        ret: RirTypeId,
    ) -> bool {
        let mut target = &arm.target;
        while let RirResolvedCallTarget::Promoted {
            projections,
            target: next,
        } = target
        {
            if projections.is_empty() {
                return false;
            }
            for projection in projections {
                let RirPlaceStepKind::StructField(field) = projection.kind else {
                    return false;
                };
                if projection.source_ty != receiver_ty {
                    return false;
                }
                let Some(RirType::Struct(id)) = self.program.types.get(receiver_ty.index()) else {
                    return false;
                };
                let Some(projected) = self
                    .program
                    .structs
                    .get(id.index())
                    .and_then(|decl| decl.fields.get(field.index()))
                    .filter(|projected| {
                        projected.id == field && projected.ty == projection.target_ty
                    })
                else {
                    return false;
                };
                receiver_ty = projected.ty;
            }
            target = next;
        }
        let (target_params, target_ret, native) = match target {
            RirResolvedCallTarget::Function(id) => {
                let Some(function) = self.program.functions.get(id.index()) else {
                    return false;
                };
                (
                    function
                        .params
                        .iter()
                        .filter_map(|param| function.parameter_data(*param))
                        .collect::<Vec<_>>(),
                    function.ret.ty,
                    None,
                )
            }
            RirResolvedCallTarget::Extern(id) => {
                let Some(function) = self.program.externs.get(id.index()) else {
                    return false;
                };
                (
                    function
                        .params
                        .iter()
                        .map(|param| (param.ty, param.mode, param.escape))
                        .collect::<Vec<_>>(),
                    function.ret,
                    Some(*id),
                )
            }
            RirResolvedCallTarget::Promoted { .. } => unreachable!(),
        };
        let Some((receiver, target_params)) = target_params.split_first() else {
            return false;
        };
        let native_valid = native.is_none_or(|id| {
            let resource = native_ty_is_resource_ref(self.program, receiver.0);
            self.program.externs[id.index()]
                .params
                .first()
                .is_some_and(|param| {
                    param.action != native_call::NativeArgAction::RejectLiveBoundary
                        && match arm.receiver {
                            RirPassMode::MutBorrow => resource,
                            RirPassMode::MutPlace => !resource,
                            _ => true,
                        }
                })
        });
        receiver.0 == receiver_ty
            && receiver.1 == arm.receiver
            && target_ret == ret
            && target_params.len() == params.len()
            && target_params
                .iter()
                .zip(params)
                .all(|((ty, mode, escape), param)| {
                    *ty == param.ty
                        && *mode == param.mode
                        && *escape == param.escape
                        && RirRustRepPolicy::new(self.program).supports_param(*ty, *mode)
                })
            && native_valid
    }

    fn dyn_dispatch_args_valid(
        &self,
        caller: &RirFunction,
        args: &[RirCallArg],
        params: &[RirDynDispatchParam],
    ) -> bool {
        args.len() == params.len() && params.iter().zip(args).all(|(param, arg)| {
            let callback_sig = match arg {
                RirCallArg::ScopedLambda { sig, .. }
                | RirCallArg::EscapingLambda { sig, .. }
                | RirCallArg::AnvCallback { sig, .. } => matches!(self.program.types.get(param.ty.index()), Some(RirType::Lambda(expected)) if *expected == *sig),
                _ => true,
            };
            arg.mode() == param.mode
                && self.dyn_arg_ty(caller, arg) == Some(param.ty)
                && callback_sig
                && (param.escape != RirParamEscape::Escaping
                    || !matches!(self.program.types.get(param.ty.index()), Some(RirType::Lambda(_)))
                    || self.call_arg_lambda_escape(caller, arg) == Some(RirLambdaEscape::Escaping))
        })
    }

    fn dyn_receiver_mode_valid(receiver: &RirDynReceiver, mode: RirPassMode) -> bool {
        match receiver {
            RirDynReceiver::Owned(_) => {
                matches!(mode, RirPassMode::Value | RirPassMode::SharedBorrow)
            }
            RirDynReceiver::Borrowed(_) => matches!(
                mode,
                RirPassMode::Value
                    | RirPassMode::SharedBorrow
                    | RirPassMode::MutBorrow
                    | RirPassMode::MutPlace
            ),
            RirDynReceiver::MutPlace(_) => {
                matches!(mode, RirPassMode::MutBorrow | RirPassMode::MutPlace)
            }
        }
    }

    fn dyn_arg_ty(&self, function: &RirFunction, arg: &RirCallArg) -> Option<RirTypeId> {
        let operand_ty = |operand: &RirOperand| match operand {
            RirOperand::Const(id) => self.program.consts.get(id.index()).map(|konst| konst.ty),
            RirOperand::Place(place) => self.program.place_ty(function, place),
        };
        let owned_ty = |owned: &RirOwnedValue| match &owned.value {
            RirOwnedOperand::Value(operand) => operand_ty(operand),
            RirOwnedOperand::Access(place) => self.program.mut_place_ty(function, place),
            RirOwnedOperand::DynBorrow(borrow) => self
                .program
                .dyn_carriers
                .get(borrow.target.index())
                .map(|carrier| carrier.storage_ty),
        };
        match arg {
            RirCallArg::Value(operand) | RirCallArg::InitFieldProvided(operand) => {
                owned_ty(operand)
            }
            RirCallArg::InitFieldOmitted => None,
            RirCallArg::SharedBorrow(place) | RirCallArg::MutBorrow(place) => {
                self.program.place_ty(function, place)
            }
            RirCallArg::SharedStringConst(_) => self.string_ty(),
            RirCallArg::MutPlace(place) => self.program.mut_place_ty(function, place),
            RirCallArg::DynBorrow(borrow) => self
                .program
                .dyn_carriers
                .get(borrow.target.index())
                .map(|carrier| carrier.storage_ty),
            RirCallArg::ScopedLambda { callee, .. }
            | RirCallArg::EscapingLambda { callee, .. }
            | RirCallArg::AnvCallback { callee, .. } => owned_ty(callee),
        }
    }

    fn check_moved_value(
        &mut self,
        site: RirVerifySite,
        function: &RirFunction,
        value: &RirOperand,
        local: RirLocalId,
    ) -> Option<RirTypeId> {
        let ty = self.operand_ty(site, function, value);
        let RirOperand::Place(place) = value else {
            self.push(site, RirVerifyErrorKind::CallArgMode);
            return ty;
        };
        let valid = matches!(place.root, RirPlaceRoot::Local(root) if root == local)
            && place.projections.is_empty();
        if !valid {
            self.push(site, RirVerifyErrorKind::CallArgMode);
        }
        ty
    }

    fn check_dyn_borrow(
        &mut self,
        site: RirVerifySite,
        function_id: RirFunctionId,
        function: &RirFunction,
        borrow: &RirDynBorrow,
    ) {
        let Some(target) = self.program.dyn_carriers.get(borrow.target.index()) else {
            self.push(site, RirVerifyErrorKind::InvalidDynCarrier);
            return;
        };
        let (source, _) = match &borrow.source {
            RirDynBorrowSource::Concrete { place, variant } => {
                let ty = self.check_mut_place_arg(
                    site,
                    function_id,
                    function,
                    place,
                    MutPlaceUse::CallArg,
                );
                let source = self.program.dyn_carriers.get(variant.carrier().index());
                if source.is_none_or(|source| {
                    source
                        .variants
                        .get(variant.index())
                        .is_none_or(|decl| Some(decl.concrete_ty) != ty)
                }) {
                    self.push(site, RirVerifyErrorKind::InvalidDynVariant);
                }
                (source.map(|source| source.id), ty)
            }
            RirDynBorrowSource::Owned { place, carrier } => {
                let ty = self.check_mut_place_arg(
                    site,
                    function_id,
                    function,
                    place,
                    MutPlaceUse::CallArg,
                );
                let source = self.program.dyn_carriers.get(carrier.index());
                if source.is_none_or(|source| Some(source.storage_ty) != ty) {
                    self.push(site, RirVerifyErrorKind::InvalidDynStorage);
                }
                (Some(*carrier), ty)
            }
            RirDynBorrowSource::Borrowed { local, carrier } => {
                self.check_local_id(site, function, *local);
                let source = self.program.dyn_carriers.get(carrier.index());
                if !source.is_some_and(|source| matches!(function.parameter_data(*local), Some((ty, RirPassMode::DynBorrow, RirParamEscape::NonEscaping)) if ty == source.storage_ty)) { self.push(site, RirVerifyErrorKind::InvalidDynStorage); }
                (Some(*carrier), source.map(|source| source.storage_ty))
            }
            RirDynBorrowSource::Reborrowed { local, carrier } => {
                self.check_local_id(site, function, *local);
                let source = self.program.dyn_carriers.get(carrier.index());
                if !source.is_some_and(|source| {
                    self.initialized.get(local.index()).copied() == Some(true)
                        && self.active_dyn_reborrows.contains(&(*local, *carrier))
                        && function
                            .locals
                            .get(local.index())
                            .is_some_and(|local| local.ty == source.storage_ty)
                }) {
                    self.push(site, RirVerifyErrorKind::InvalidDynStorage);
                }
                (Some(*carrier), source.map(|source| source.storage_ty))
            }
        };
        let valid = match borrow.weakening {
            Some(id) => self
                .program
                .dyn_weakenings
                .get(id.index())
                .is_some_and(|weakening| {
                    Some(weakening.source) == source && weakening.target == target.id
                }),
            None => source == Some(target.id),
        };
        if !valid {
            self.push(site, RirVerifyErrorKind::InvalidDynStorage);
        }
    }

    fn check_untyped_dyn_arg(
        &mut self,
        function_id: RirFunctionId,
        function: &RirFunction,
        stmt: usize,
        index: usize,
        arg: &RirCallArg,
    ) {
        let site = RirVerifySite::CallArg(function_id, stmt, index);
        match arg {
            RirCallArg::Value(value) | RirCallArg::InitFieldProvided(value) => {
                self.check_owned_value(site, function, value, RustRecipePosition::Value);
            }
            RirCallArg::InitFieldOmitted => {
                self.push(site, RirVerifyErrorKind::CallArgMode);
            }
            RirCallArg::SharedBorrow(place) => {
                self.check_place(site, function, place);
            }
            RirCallArg::MutBorrow(place) => {
                self.check_place(site, function, place);
                let RirPlaceRoot::Local(local) = place.root else {
                    self.push(site, RirVerifyErrorKind::CallArgMode);
                    return;
                };
                let direct_local = function.locals.get(local.index()).is_some_and(|local| {
                    local.mutable
                        && !matches!(
                            local.binding,
                            RirLocalBinding::DirectPayload | RirLocalBinding::ScopedPlacePayload
                        )
                });
                if Self::function_local_is_mut_place_param(function, local)
                    || !place.projections.is_empty()
                    || !direct_local
                {
                    self.push(site, RirVerifyErrorKind::CallArgMode);
                }
            }
            RirCallArg::SharedStringConst(id) => self.check_string_literal_id(site, *id),
            RirCallArg::MutPlace(place) => {
                self.check_mut_place_arg(site, function_id, function, place, MutPlaceUse::CallArg);
            }
            RirCallArg::DynBorrow(borrow) => {
                self.check_dyn_borrow(site, function_id, function, borrow);
            }
            RirCallArg::ScopedLambda { callee, sig }
            | RirCallArg::EscapingLambda { callee, sig }
            | RirCallArg::AnvCallback { callee, sig } => {
                self.check_lambda_sig_id(site, *sig);
                self.check_owned_value(site, function, callee, RustRecipePosition::Value);
            }
        }
    }

    fn check_call(
        &mut self,
        function_id: RirFunctionId,
        function: &RirFunction,
        stmt: usize,
        callee: RirCallTarget,
        args: &[RirCallArg],
        ret: RirTypeId,
    ) {
        let native_call = matches!(callee, RirCallTarget::Extern(_));
        let (expected, callee_ret, init_fields, native_ext) = match callee {
            RirCallTarget::Function(id) => {
                self.check_function_id(RirVerifySite::RValue(function_id, stmt), id);
                match self.program.functions.get(id.index()) {
                    Some(function) => (
                        function
                            .params
                            .iter()
                            .filter_map(|param| function.parameter_data(*param))
                            .collect::<Vec<_>>(),
                        function.ret.ty,
                        vec![false; function.params.len()],
                        None,
                    ),
                    None => return,
                }
            }
            RirCallTarget::Extern(id) => {
                self.check_extern_id(RirVerifySite::RValue(function_id, stmt), id);
                match self.program.externs.get(id.index()) {
                    Some(ext) => {
                        let init_fields = ext
                            .params
                            .iter()
                            .map(|param| matches!(param.plan, RirNativeParam::InitField(_)))
                            .collect();
                        (
                            ext.params
                                .iter()
                                .map(|param| (param.ty, param.mode, param.escape))
                                .collect::<Vec<_>>(),
                            ext.ret,
                            init_fields,
                            Some(id),
                        )
                    }
                    None => return,
                }
            }
            RirCallTarget::LambdaValue { callee, sig } => {
                self.check_lambda_sig_id(RirVerifySite::RValue(function_id, stmt), sig);
                let Some(sig_decl) = self.program.lambda_sigs.get(sig.index()) else {
                    return;
                };
                match self.operand_ty(RirVerifySite::RValue(function_id, stmt), function, &callee) {
                    Some(ty) if self.ty(ty) == Some(RirType::Lambda(sig)) => {}
                    Some(found) => {
                        let site = RirVerifySite::RValue(function_id, stmt);
                        match self.type_id(RirType::Lambda(sig)) {
                            Some(expected) => self
                                .push(site, RirVerifyErrorKind::TypeMismatch { expected, found }),
                            None => self.push(site, RirVerifyErrorKind::UnsupportedRValueType),
                        }
                    }
                    None => return,
                }
                (
                    sig_decl
                        .params
                        .iter()
                        .map(|param| (param.ty, param.mode, param.escape))
                        .collect::<Vec<_>>(),
                    sig_decl.ret,
                    vec![false; sig_decl.params.len()],
                    None,
                )
            }
        };
        if expected.len() != args.len() {
            self.push(
                RirVerifySite::RValue(function_id, stmt),
                RirVerifyErrorKind::CallArgCount {
                    expected: expected.len(),
                    found: args.len(),
                },
            );
            return;
        }
        if ret != callee_ret {
            self.push(
                RirVerifySite::RValue(function_id, stmt),
                RirVerifyErrorKind::TypeMismatch {
                    expected: callee_ret,
                    found: ret,
                },
            );
        }
        for (index, (arg, (ty, mode, escape))) in args.iter().zip(expected).enumerate() {
            let site = RirVerifySite::CallArg(function_id, stmt, index);
            let found_init_field = matches!(
                arg,
                RirCallArg::InitFieldProvided(_) | RirCallArg::InitFieldOmitted
            );
            if found_init_field != init_fields[index] {
                self.push(site, RirVerifyErrorKind::CallArgMode);
            }
            if arg.mode() != mode
                || matches!(
                    arg,
                    RirCallArg::ScopedLambda { .. }
                        | RirCallArg::EscapingLambda { .. }
                        | RirCallArg::AnvCallback { .. }
                ) && !native_call
            {
                self.push(site, RirVerifyErrorKind::CallArgMode);
            }
            if native_ext
                .is_some_and(|id| self.program.externs[id.index()].rejects_reentry_arg(index, arg))
            {
                self.push(site, RirVerifyErrorKind::CallArgMode);
            }
            if !RirRustRepPolicy::new(self.program).supports_param(ty, arg.mode()) {
                self.push(site, RirVerifyErrorKind::UnsupportedAbi);
            }
            if escape == RirParamEscape::Escaping
                && matches!(self.ty(ty), Some(RirType::Lambda(_)))
                && self.call_arg_lambda_escape(function, arg) != Some(RirLambdaEscape::Escaping)
            {
                self.push(site, RirVerifyErrorKind::CallArgEscape);
            }
            let found = match arg {
                RirCallArg::Value(operand) | RirCallArg::InitFieldProvided(operand) => {
                    self.check_owned_value(site, function, operand, RustRecipePosition::Value)
                }
                RirCallArg::InitFieldOmitted => None,
                RirCallArg::SharedBorrow(place) => {
                    self.check_place(site, function, place);
                    match place.root {
                        RirPlaceRoot::Local(local) => {
                            if Self::function_local_is_mut_place_param(function, local) {
                                self.push(site, RirVerifyErrorKind::CallArgMode);
                            }
                        }
                        RirPlaceRoot::Global(global) => {
                            if !self
                                .program
                                .place_ty(function, place)
                                .is_some_and(|ty| self.global_payload_supported(ty))
                            {
                                self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                            }
                            if !self
                                .global_initialized
                                .get(global.index())
                                .copied()
                                .unwrap_or(false)
                            {
                                self.push(site, RirVerifyErrorKind::UninitializedGlobal(global));
                            }
                        }
                    }
                    self.program.place_ty(function, place)
                }
                RirCallArg::MutBorrow(place) => {
                    let RirPlaceRoot::Local(local) = place.root else {
                        self.check_place(site, function, place);
                        self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                        return;
                    };
                    self.check_place(site, function, place);
                    let direct_local = function.locals.get(local.index()).is_some_and(|local| {
                        local.mutable
                            && !matches!(
                                local.binding,
                                RirLocalBinding::DirectPayload
                                    | RirLocalBinding::ScopedPlacePayload
                            )
                    });
                    if Self::function_local_is_mut_place_param(function, local)
                        || !place.projections.is_empty()
                        || !direct_local
                    {
                        self.push(site, RirVerifyErrorKind::CallArgMode);
                    }
                    self.program.place_ty(function, place)
                }
                RirCallArg::MutPlace(arg) => {
                    self.check_mut_place_arg(site, function_id, function, arg, MutPlaceUse::CallArg)
                }
                RirCallArg::DynBorrow(borrow) => {
                    if escape != RirParamEscape::NonEscaping {
                        self.push(site, RirVerifyErrorKind::CallArgEscape);
                    }
                    self.check_dyn_borrow(site, function_id, function, borrow);
                    self.program
                        .dyn_carriers
                        .get(borrow.target.index())
                        .map(|carrier| carrier.storage_ty)
                }
                RirCallArg::ScopedLambda { callee, sig }
                | RirCallArg::EscapingLambda { callee, sig }
                | RirCallArg::AnvCallback { callee, sig } => {
                    self.check_lambda_sig_id(site, *sig);
                    let found =
                        self.check_owned_value(site, function, callee, RustRecipePosition::Value);
                    match found.and_then(|ty| match self.ty(ty) {
                        Some(RirType::Lambda(found_sig)) => Some((ty, found_sig)),
                        _ => None,
                    }) {
                        Some((ty, found_sig)) if found_sig == *sig => Some(ty),
                        Some((ty, _)) => {
                            match self.type_id(RirType::Lambda(*sig)) {
                                Some(expected) => self.push(
                                    site,
                                    RirVerifyErrorKind::TypeMismatch {
                                        expected,
                                        found: ty,
                                    },
                                ),
                                None => self.push(site, RirVerifyErrorKind::UnsupportedRValueType),
                            }
                            Some(ty)
                        }
                        None => found,
                    }
                }
                RirCallArg::SharedStringConst(id) => {
                    self.check_string_literal_id(site, *id);
                    self.string_ty()
                }
            };
            if let Some(found) = found
                && found != ty
            {
                self.push(
                    site,
                    RirVerifyErrorKind::TypeMismatch {
                        expected: ty,
                        found,
                    },
                );
            }
        }
    }

    fn check_mut_place_arg(
        &mut self,
        site: RirVerifySite,
        function_id: RirFunctionId,
        function: &RirFunction,
        arg: &RirMutPlaceArg,
        use_: MutPlaceUse,
    ) -> Option<RirTypeId> {
        let Some(root_ty) = self.program.mut_place_root_ty(function, &arg.access) else {
            self.push(site, RirVerifyErrorKind::BadId);
            return None;
        };
        let final_ty = arg.final_ty(root_ty);
        self.check_type_id(site, final_ty);
        match &arg.access {
            RirMutPlaceAccess::Handle(handle) => {
                self.check_mut_place_handle(
                    site,
                    function_id,
                    function,
                    handle,
                    arg,
                    root_ty,
                    use_,
                );
            }
            RirMutPlaceAccess::DataRef { object, place } => {
                if !use_.allow_dataref() {
                    self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                }
                let mode = if use_ == MutPlaceUse::CallArg {
                    StorageProjectionMode::MutPlace
                } else {
                    StorageProjectionMode::Ordinary
                };
                self.check_dataref_access(site, function, object, *place, &arg.projections, mode);
            }
        }
        Some(final_ty)
    }

    fn check_mut_place_handle(
        &mut self,
        site: RirVerifySite,
        function_id: RirFunctionId,
        function: &RirFunction,
        handle: &RirMutPlaceHandle,
        arg: &RirMutPlaceArg,
        root_ty: RirTypeId,
        use_: MutPlaceUse,
    ) {
        match handle {
            RirMutPlaceHandle::Local { local } => {
                if use_ == MutPlaceUse::ScopedPlaceSource {
                    self.check_scoped_place_source_projection(site, function, root_ty, arg);
                } else {
                    self.check_place(
                        site,
                        function,
                        &RirPlace::local(*local, arg.projections.clone()),
                    );
                    if Self::function_local_is_mut_place_param(function, *local) {
                        self.push(site, RirVerifyErrorKind::CallArgMode);
                    }
                }
                match function.locals.get(local.index()) {
                    Some(local) => {
                        if use_ == MutPlaceUse::OwnedRead
                            && !matches!(
                                local.binding,
                                RirLocalBinding::DirectPayload
                                    | RirLocalBinding::ScopedPlacePayload
                            )
                        {
                            self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                        } else if !matches!(
                            use_,
                            MutPlaceUse::OwnedRead | MutPlaceUse::SliceViewRead
                        ) && !local.mutable
                            && !matches!(
                                local.binding,
                                RirLocalBinding::DirectPayload
                                    | RirLocalBinding::ScopedPlacePayload
                            )
                        {
                            self.push(site, RirVerifyErrorKind::ImmutableAssign);
                        }
                    }
                    None => self.push(site, RirVerifyErrorKind::BadId),
                }
                if !Self::projected_mut_place_arg_supported(root_ty, &arg.projections, true) {
                    self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                }
            }
            RirMutPlaceHandle::Param { local } => {
                self.check_local_id(site, function, *local);
                if use_ != MutPlaceUse::ScopedPlaceSource
                    && self.function_local_is_scoped_place_source(function_id, *local)
                {
                    self.push(site, RirVerifyErrorKind::CallArgMode);
                }
                match function.parameter_data(*local) {
                    Some((_, RirPassMode::MutPlace, _)) => {}
                    Some(_) => self.push(site, RirVerifyErrorKind::CallArgMode),
                    None => self.push(site, RirVerifyErrorKind::ParamLocalMissing),
                }
                if !arg.projections.is_empty() && use_ == MutPlaceUse::ScopedPlaceSource {
                    self.check_scoped_place_source_projection(site, function, root_ty, arg);
                } else if !arg.projections.is_empty() {
                    self.check_place(
                        site,
                        function,
                        &RirPlace::local(*local, arg.projections.clone()),
                    );
                    if !Self::projected_mut_place_arg_supported(root_ty, &arg.projections, true) {
                        self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                    }
                }
            }
            RirMutPlaceHandle::StackCell { cell } => {
                self.check_cell_mut_place_arg(
                    site,
                    function_id,
                    *cell,
                    RirCellStorage::StackScoped,
                );
                self.check_projected_cell_mut_place_root(
                    site,
                    function,
                    root_ty,
                    &arg.projections,
                    use_,
                );
            }
            RirMutPlaceHandle::HeapCell { cell } => {
                self.check_cell_mut_place_arg(site, function_id, *cell, RirCellStorage::Heap);
                self.check_projected_cell_mut_place_root(
                    site,
                    function,
                    root_ty,
                    &arg.projections,
                    use_,
                );
            }
            RirMutPlaceHandle::ScopedPlaceCell { cell } => {
                self.check_function_scoped_place_cell_ref(site, function_id, *cell);
                self.check_projected_cell_mut_place_root(
                    site,
                    function,
                    root_ty,
                    &arg.projections,
                    use_,
                );
            }
            RirMutPlaceHandle::Global { global } => {
                let Some(decl) = self.check_global_id(site, *global).cloned() else {
                    return;
                };
                if !matches!(use_, MutPlaceUse::OwnedRead | MutPlaceUse::SliceViewRead)
                    && !self
                        .global_initialized
                        .get(global.index())
                        .copied()
                        .unwrap_or(false)
                {
                    self.push(site, RirVerifyErrorKind::UninitializedGlobal(*global));
                }
                if !decl.mutable
                    && !matches!(use_, MutPlaceUse::OwnedRead | MutPlaceUse::SliceViewRead)
                {
                    self.push(site, RirVerifyErrorKind::ImmutableAssign);
                }
                self.check_projection_chain(site, function, root_ty, &arg.projections, true);
                if !Self::projected_mut_place_arg_supported(root_ty, &arg.projections, true) {
                    self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                }
            }
        }
    }

    fn check_projected_cell_mut_place_root(
        &mut self,
        site: RirVerifySite,
        function: &RirFunction,
        root_ty: RirTypeId,
        projections: &[RirPlaceStep],
        use_: MutPlaceUse,
    ) {
        self.check_projection_chain(
            site,
            function,
            root_ty,
            projections,
            use_.allow_cell_collection_projection(),
        );
        if !Self::projected_mut_place_arg_supported(
            root_ty,
            projections,
            use_.allow_cell_collection_projection(),
        ) {
            self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
        }
    }

    fn check_projection_chain(
        &mut self,
        site: RirVerifySite,
        function: &RirFunction,
        root_ty: RirTypeId,
        projections: &[RirPlaceStep],
        allow_collections: bool,
    ) -> Option<RirTypeId> {
        let mut ty = root_ty;
        for step in projections {
            if !self.check_place_step(site, function, ty, step, allow_collections) {
                return None;
            }
            ty = step.target_ty;
        }
        Some(ty)
    }

    fn check_place_step(
        &mut self,
        site: RirVerifySite,
        function: &RirFunction,
        current: RirTypeId,
        step: &RirPlaceStep,
        allow_collections: bool,
    ) -> bool {
        if matches!(
            step.kind,
            RirPlaceStepKind::StructField(_)
                | RirPlaceStepKind::ExternField(_)
                | RirPlaceStepKind::DataRefField(_)
                | RirPlaceStepKind::TupleField(_)
        ) {
            return self.check_field_place_step(site, current, step);
        }
        if !self.check_place_step_source(site, current, step) {
            return false;
        }
        let target = match step.kind {
            RirPlaceStepKind::ArrayIndex {
                index,
                len,
                elem_materializer,
            } => {
                self.check_sequence_index_local(site, function, index);
                match self.ty(current) {
                    Some(RirType::Array {
                        elem,
                        len: expected,
                    }) if len == expected
                        && self.program.sequence_elem_materializer(current)
                            == Some(elem_materializer) =>
                    {
                        Some(elem)
                    }
                    _ => None,
                }
            }
            RirPlaceStepKind::ListIndex {
                index,
                elem_materializer,
            } => {
                self.check_sequence_index_local(site, function, index);
                if allow_collections {
                    match self.ty(current) {
                        Some(RirType::List(elem))
                            if self.program.sequence_elem_materializer(current)
                                == Some(elem_materializer) =>
                        {
                            Some(elem)
                        }
                        _ => None,
                    }
                } else {
                    None
                }
            }
            RirPlaceStepKind::SliceIndex {
                index,
                elem_materializer,
            } => {
                self.check_sequence_index_local(site, function, index);
                if allow_collections {
                    match self.ty(current) {
                        Some(RirType::Slice(elem))
                            if self.program.sequence_elem_materializer(current)
                                == Some(elem_materializer) =>
                        {
                            Some(elem)
                        }
                        _ => None,
                    }
                } else {
                    None
                }
            }
            RirPlaceStepKind::StructField(_)
            | RirPlaceStepKind::ExternField(_)
            | RirPlaceStepKind::DataRefField(_)
            | RirPlaceStepKind::TupleField(_) => unreachable!("checked field step"),
        };
        self.check_place_step_target(site, step, target)
    }

    fn check_field_place_step(
        &mut self,
        site: RirVerifySite,
        current: RirTypeId,
        step: &RirPlaceStep,
    ) -> bool {
        if !self.check_place_step_source(site, current, step) {
            return false;
        }
        let target = match step.kind {
            RirPlaceStepKind::StructField(field) => {
                let Some(RirType::Struct(id)) = self.ty(current) else {
                    self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                    return false;
                };
                let Some(strukt) = self.program.structs.get(id.index()) else {
                    self.push(site, RirVerifyErrorKind::BadId);
                    return false;
                };
                if strukt.role != RirStructRole::Source {
                    self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                    return false;
                }
                strukt.fields.get(field.index()).map(|field| field.ty)
            }
            RirPlaceStepKind::ExternField(field) => {
                let Some(RirType::Struct(id)) = self.ty(current) else {
                    self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                    return false;
                };
                let Some(strukt) = self.program.structs.get(id.index()) else {
                    self.push(site, RirVerifyErrorKind::BadId);
                    return false;
                };
                if strukt.native.is_none() {
                    self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                    return false;
                }
                strukt.fields.get(field.index()).map(|field| field.ty)
            }
            RirPlaceStepKind::DataRefField(field) => {
                let Some(RirType::DataRef(id)) = self.ty(current) else {
                    self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                    return false;
                };
                self.program
                    .datarefs
                    .get(id.index())
                    .and_then(|dataref| dataref.fields.get(field.index()))
                    .map(|field| field.ty)
            }
            RirPlaceStepKind::TupleField(field) => {
                let Some(RirType::Tuple(id)) = self.ty(current) else {
                    self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                    return false;
                };
                self.program
                    .tuples
                    .get(id.index())
                    .and_then(|tuple| tuple.fields.get(field.index()))
                    .map(|field| field.ty)
            }
            _ => unreachable!("expected field step"),
        };
        self.check_place_step_target(site, step, target)
    }

    fn check_place_step_source(
        &mut self,
        site: RirVerifySite,
        current: RirTypeId,
        step: &RirPlaceStep,
    ) -> bool {
        if step.source_ty == current {
            return true;
        }
        self.push(
            site,
            RirVerifyErrorKind::TypeMismatch {
                expected: current,
                found: step.source_ty,
            },
        );
        false
    }

    fn check_place_step_target(
        &mut self,
        site: RirVerifySite,
        step: &RirPlaceStep,
        target: Option<RirTypeId>,
    ) -> bool {
        let Some(target) = target else {
            self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
            return false;
        };
        if target != step.target_ty {
            self.push(
                site,
                RirVerifyErrorKind::TypeMismatch {
                    expected: target,
                    found: step.target_ty,
                },
            );
            return false;
        }
        true
    }

    fn check_sequence_index_local(
        &mut self,
        site: RirVerifySite,
        function: &RirFunction,
        local: RirLocalId,
    ) -> Option<()> {
        let index_local = self.check_projection_index_local(site, function, local)?;
        if self.ty(index_local.ty) != Some(RirType::Int) {
            self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
        }
        Some(())
    }

    fn check_projection_index_local(
        &mut self,
        site: RirVerifySite,
        function: &RirFunction,
        local: RirLocalId,
    ) -> Option<RirLocal> {
        let Some(index_local) = function.locals.get(local.index()).cloned() else {
            self.push(site, RirVerifyErrorKind::BadId);
            return None;
        };
        if Self::function_local_is_hidden_cell_param(function, local)
            || self.function_local_is_scoped_place_source(function.id, local)
        {
            self.push(site, RirVerifyErrorKind::UnsupportedLambdaCapture);
        }
        if Self::function_local_is_mut_place_param(function, local) {
            self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
        }
        if !self
            .initialized
            .get(local.index())
            .copied()
            .unwrap_or(false)
        {
            self.push(site, RirVerifyErrorKind::UninitializedLocal(local));
        }
        Some(index_local)
    }

    fn check_scoped_place_source_projection(
        &mut self,
        site: RirVerifySite,
        function: &RirFunction,
        root_ty: RirTypeId,
        arg: &RirMutPlaceArg,
    ) {
        self.check_projection_chain(site, function, root_ty, &arg.projections, true);
        if !Self::projected_mut_place_arg_supported(root_ty, &arg.projections, true) {
            self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
        }
    }

    fn check_cell_mut_place_arg(
        &mut self,
        site: RirVerifySite,
        function_id: RirFunctionId,
        cell: RirCellRef,
        storage: RirCellStorage,
    ) {
        if let Some(decl) = self.check_function_cell_ref(site, function_id, cell) {
            if decl.storage != storage {
                self.push(site, RirVerifyErrorKind::CallArgMode);
            }
            if !self.cell_initialized(decl.id) {
                self.push(site, RirVerifyErrorKind::UninitializedCell(decl.id));
            }
        }
    }

    fn check_return_value(
        &mut self,
        site: RirVerifySite,
        function: &RirFunction,
        operand: &RirOperand,
        found: Option<RirTypeId>,
    ) {
        self.check_stack_loop_lambda_operand(site, function, operand);
        if let Some(found) = found {
            if found != function.ret.ty {
                self.push(
                    site,
                    RirVerifyErrorKind::TypeMismatch {
                        expected: function.ret.ty,
                        found,
                    },
                );
            }
            if matches!(self.ty(found), Some(RirType::Slice(_))) {
                self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
            }
            if matches!(self.ty(function.ret.ty), Some(RirType::Lambda(_)))
                && self.operand_lambda_escape(function, operand) != Some(RirLambdaEscape::Escaping)
            {
                self.push(site, RirVerifyErrorKind::CallArgEscape);
            }
        }
    }

    fn check_term(&mut self, function_id: RirFunctionId, function: &RirFunction, term: &RirTerm) {
        let site = RirVerifySite::Terminator(function_id);
        match term {
            RirTerm::Return(None) if !matches!(self.ty(function.ret.ty), Some(RirType::Void)) => {
                self.push(site, RirVerifyErrorKind::ReturnValueRequired);
            }
            RirTerm::Return(Some(_)) | RirTerm::ReturnOwned(_)
                if matches!(self.ty(function.ret.ty), Some(RirType::Void)) =>
            {
                self.push(site, RirVerifyErrorKind::UnexpectedReturnValue);
            }
            RirTerm::Return(Some(operand)) => {
                let found = self.value_operand_ty(site, function, operand);
                self.check_return_value(site, function, operand, found);
            }
            RirTerm::ReturnOwned(owned) => {
                let found =
                    self.check_owned_value(site, function, owned, RustRecipePosition::Value);
                match &owned.value {
                    RirOwnedOperand::Value(value) => {
                        self.check_return_value(site, function, value, found);
                    }
                    RirOwnedOperand::Access(_) | RirOwnedOperand::DynBorrow(_) => {
                        if let Some(found) = found {
                            if found != function.ret.ty {
                                self.push(
                                    site,
                                    RirVerifyErrorKind::TypeMismatch {
                                        expected: function.ret.ty,
                                        found,
                                    },
                                );
                            }
                            if matches!(
                                self.ty(found),
                                Some(RirType::Slice(_) | RirType::Lambda(_))
                            ) {
                                self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                            }
                        }
                    }
                }
            }
            RirTerm::Break(id) => {
                if !self.loops.contains(id) {
                    self.push(site, RirVerifyErrorKind::BreakOutsideLoop(*id));
                }
            }
            RirTerm::Continue(id) => {
                if !self.loops.contains(id) {
                    self.push(site, RirVerifyErrorKind::ContinueOutsideLoop(*id));
                }
            }
            RirTerm::None | RirTerm::Return(None) | RirTerm::Unreachable => {}
        }
    }

    fn check_pass_mode(&mut self, site: RirVerifySite, ty: RirTypeId, mode: RirPassMode) {
        if self.ty(ty).is_none() || !RirRustRepPolicy::new(self.program).supports_param(ty, mode) {
            self.push(site, RirVerifyErrorKind::UnsupportedAbi);
        }
    }

    fn unary_ok(&self, op: UnaryOp, value: Option<RirTypeId>, ret: RirTypeId) -> bool {
        match (value.and_then(|ty| self.scalar(ty)), self.scalar(ret)) {
            (Some(value), Some(ret)) => op.scalar_result(value) == Some(ret),
            _ => {
                op == UnaryOp::BitNot
                    && value == Some(ret)
                    && matches!(self.ty(ret), Some(RirType::Flag(_)))
            }
        }
    }

    fn binary_ok(
        &self,
        op: BinaryOp,
        lhs: Option<RirTypeId>,
        rhs: Option<RirTypeId>,
        ret: RirTypeId,
    ) -> bool {
        match (
            lhs.and_then(|ty| self.scalar(ty)),
            rhs.and_then(|ty| self.scalar(ty)),
            self.scalar(ret),
        ) {
            (Some(lhs), Some(rhs), Some(ret)) => op.scalar_result(lhs, rhs) == Some(ret),
            _ if lhs == rhs
                && lhs.is_some_and(|ty| matches!(self.ty(ty), Some(RirType::Flag(_)))) =>
            {
                match op {
                    BinaryOp::BitAnd | BinaryOp::BitOr | BinaryOp::Xor => lhs == Some(ret),
                    BinaryOp::Eq | BinaryOp::NotEq => self.type_id(RirType::Bool) == Some(ret),
                    _ => false,
                }
            }
            _ if matches!(op, BinaryOp::Eq | BinaryOp::NotEq)
                && self.type_id(RirType::Bool) == Some(ret) =>
            {
                match (lhs, rhs) {
                    (Some(lhs), Some(rhs)) if lhs == rhs => self.program.unit_only_enum(lhs),
                    _ => false,
                }
            }
            _ => false,
        }
    }

    fn scalar(&self, ty: RirTypeId) -> Option<ScalarKind> {
        match self.ty(ty) {
            Some(RirType::Int) => Some(ScalarKind::Int),
            Some(RirType::Float) => Some(ScalarKind::Float),
            Some(RirType::Bool) => Some(ScalarKind::Bool),
            Some(RirType::String) => Some(ScalarKind::String),
            Some(RirType::Char) => Some(ScalarKind::Char),
            _ => None,
        }
    }

    fn cast_ok(&self, value: Option<RirTypeId>, ret: RirTypeId) -> bool {
        matches!(
            (value.and_then(|ty| self.ty(ty)), self.ty(ret)),
            (Some(RirType::Int), Some(RirType::Float)) | (Some(RirType::Float), Some(RirType::Int))
        )
    }

    fn raw_project_ok(&self, value: Option<RirTypeId>, ret: RirTypeId) -> bool {
        match value.and_then(|ty| self.ty(ty)) {
            Some(RirType::Flag(_)) => matches!(self.ty(ret), Some(RirType::Int)),
            Some(RirType::Enum(enum_id)) => {
                let Some(enm) = self.program.enums.get(enum_id.index()) else {
                    return false;
                };
                let backing_matches = matches!(
                    (enm.repr, self.ty(ret)),
                    (RirEnumRepr::RawInt, Some(RirType::Int))
                        | (RirEnumRepr::RawString, Some(RirType::String))
                );
                backing_matches && enm.raw_type == Some(ret)
            }
            _ => false,
        }
    }

    fn raw_try_construct_ok(
        &self,
        value: Option<RirTypeId>,
        target: RirTypeId,
        result: RirTypeId,
    ) -> bool {
        let backing_matches = match self.ty(target) {
            Some(RirType::Flag(_)) => {
                matches!(value.and_then(|ty| self.ty(ty)), Some(RirType::Int))
            }
            Some(RirType::Enum(enum_id)) => {
                let Some(enm) = self.program.enums.get(enum_id.index()) else {
                    return false;
                };
                matches!(
                    (enm.repr, value.and_then(|ty| self.ty(ty))),
                    (RirEnumRepr::RawInt, Some(RirType::Int))
                        | (RirEnumRepr::RawString, Some(RirType::String))
                ) && enm.raw_type == value
            }
            _ => false,
        };
        backing_matches
            && matches!(self.ty(result), Some(RirType::Option(inner)) if inner == target)
    }

    fn stringify_field_supported(&self, ty: RirTypeId) -> bool {
        self.scalar(ty).is_some()
            || matches!(
                self.ty(ty),
                Some(RirType::Struct(_) | RirType::Enum(_) | RirType::Flag(_))
            ) && self.program.stringify_req(ty).is_some()
    }

    fn stringify_ok(&self, value: &RirOperand, source_ty: RirTypeId) -> bool {
        if self.scalar(source_ty).is_some() {
            return true;
        }
        match self.program.stringify_req(source_ty).map(|req| req.kind) {
            Some(RirStringifyReqKind::Helper(_)) => matches!(
                self.ty(source_ty),
                Some(RirType::Struct(_) | RirType::Enum(_) | RirType::Flag(_))
            ),
            Some(RirStringifyReqKind::Override { .. }) => {
                matches!(self.ty(source_ty), Some(RirType::Struct(_)))
                    && matches!(value, RirOperand::Place(_))
            }
            None => false,
        }
    }

    fn format_ok(&self, source_ty: RirTypeId, spec: FormatSpec) -> bool {
        if self.scalar(source_ty).is_none() {
            return false;
        }
        let ty = self
            .ty(source_ty)
            .expect("scalar type id has RIR type data");
        match spec.kind {
            FormatKind::Hex | FormatKind::HexUpper | FormatKind::Binary if ty != RirType::Int => {
                return false;
            }
            FormatKind::Exp | FormatKind::ExpUpper if ty != RirType::Float => {
                return false;
            }
            _ => {}
        }
        if spec.precision.is_some() && !matches!(ty, RirType::Float | RirType::String) {
            return false;
        }
        if spec.sign == FormatSign::Always && !matches!(ty, RirType::Int | RirType::Float) {
            return false;
        }
        true
    }

    fn operand_ty(
        &mut self,
        site: RirVerifySite,
        function: &RirFunction,
        operand: &RirOperand,
    ) -> Option<RirTypeId> {
        match operand {
            RirOperand::Place(place) => {
                self.check_place(site, function, place);
                self.program.place_ty(function, place)
            }
            RirOperand::Const(id) => {
                self.check_const_id(site, *id);
                self.program.consts.get(id.index()).map(|konst| konst.ty)
            }
        }
    }

    fn value_operand_ty(
        &mut self,
        site: RirVerifySite,
        function: &RirFunction,
        operand: &RirOperand,
    ) -> Option<RirTypeId> {
        let ty = self.operand_ty(site, function, operand);
        self.check_value_operand_materializable(site, operand, ty);
        ty
    }

    fn check_value_operand_materializable(
        &mut self,
        site: RirVerifySite,
        operand: &RirOperand,
        ty: Option<RirTypeId>,
    ) {
        if matches!(operand, RirOperand::Place(_))
            && ty.is_some_and(|ty| !self.value_from_ref_supported(ty))
        {
            self.push(site, RirVerifyErrorKind::NonCopyValueRequired);
        }
    }

    fn check_short_region_operand(
        &mut self,
        site: RirVerifySite,
        function: &RirFunction,
        operand: &RirOperand,
    ) {
        if let RirOperand::Place(place) = operand {
            let RirPlaceRoot::Local(local) = place.root else {
                self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                return;
            };
            if Self::function_local_is_hidden_cell_param(function, local)
                || self.function_local_is_scoped_place_source(function.id, local)
            {
                self.push(site, RirVerifyErrorKind::UnsupportedLambdaCapture);
            }
            if Self::function_local_is_mut_place_param(function, local) {
                self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
            }
        }
    }

    fn check_value_operand_ty(
        &mut self,
        site: RirVerifySite,
        function: &RirFunction,
        operand: &RirOperand,
        expected: RirTypeId,
    ) {
        let found = self.value_operand_ty(site, function, operand);
        if found != Some(expected) {
            self.push(
                site,
                RirVerifyErrorKind::TypeMismatch {
                    expected,
                    found: found.unwrap_or(expected),
                },
            );
        }
    }

    fn check_slice_range(
        &mut self,
        site: RirVerifySite,
        function: &RirFunction,
        source: &RirPlace,
        start: RirLocalId,
        end: RirLocalId,
    ) {
        self.check_place(site, function, source);
        self.check_range_locals(site, function, start, end);
    }

    fn check_range_locals(
        &mut self,
        site: RirVerifySite,
        function: &RirFunction,
        start: RirLocalId,
        end: RirLocalId,
    ) {
        for local in [start, end] {
            let Some(data) = function.locals.get(local.index()) else {
                self.push(site, RirVerifyErrorKind::BadId);
                continue;
            };
            if self.ty(data.ty) != Some(RirType::Int) {
                self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
            }
            if !self
                .initialized
                .get(local.index())
                .copied()
                .unwrap_or(false)
            {
                self.push(site, RirVerifyErrorKind::UninitializedLocal(local));
            }
        }
    }

    fn check_dataref_access(
        &mut self,
        site: RirVerifySite,
        function: &RirFunction,
        object: &RirOperand,
        place: RirDataRefPlaceId,
        suffix: &[RirPlaceStep],
        mode: StorageProjectionMode,
    ) -> Option<RirTypeId> {
        let Some((dataref, storage_ty, has_extern)) = self
            .program
            .dataref_places
            .get(place.index())
            .and_then(|descriptor| {
                descriptor.storage.last().map(|step| {
                    (
                        descriptor.dataref,
                        step.target_ty,
                        descriptor
                            .storage
                            .iter()
                            .any(|step| matches!(step.kind, RirPlaceStepKind::ExternField(_))),
                    )
                })
            })
        else {
            self.push(site, RirVerifyErrorKind::BadId);
            return None;
        };
        let object_ty = self.value_operand_ty(site, function, object);
        self.check_short_region_operand(site, function, object);
        if object_ty.and_then(|ty| self.ty(ty)) != Some(RirType::DataRef(dataref)) {
            self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
        }
        if mode == StorageProjectionMode::MutPlace && has_extern {
            self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
        }
        if suffix.first().is_some_and(|step| {
            matches!(
                step.kind,
                RirPlaceStepKind::StructField(_)
                    | RirPlaceStepKind::ExternField(_)
                    | RirPlaceStepKind::TupleField(_)
            )
        }) || suffix
            .iter()
            .any(|step| matches!(step.kind, RirPlaceStepKind::DataRefField(_)))
        {
            self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
        }
        let found = self.check_projection_chain(site, function, storage_ty, suffix, true)?;
        if mode == StorageProjectionMode::MutPlace
            && !suffix.is_empty()
            && !Self::projected_mut_place_arg_supported(storage_ty, suffix, true)
        {
            self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
        }
        Some(found)
    }

    fn check_dataref_places(&mut self) {
        for index in 0..self.program.dataref_places.len() {
            let site = RirVerifySite::Program;
            let (dataref, materializer, storage_len) = {
                let descriptor = &self.program.dataref_places[index];
                (
                    descriptor.dataref,
                    descriptor.materializer,
                    descriptor.storage.len(),
                )
            };
            if self.program.dataref_places[..index].iter().any(|other| {
                other.dataref == dataref
                    && other.materializer == materializer
                    && other.storage == self.program.dataref_places[index].storage
            }) {
                self.push(site, RirVerifyErrorKind::BadId);
            }
            self.check_dataref_id(site, dataref);
            let Some(mut ty) = self.type_id(RirType::DataRef(dataref)) else {
                self.push(site, RirVerifyErrorKind::BadId);
                continue;
            };
            let Some(first) = self.program.dataref_places[index].storage.first().copied() else {
                self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                continue;
            };
            if !matches!(first.kind, RirPlaceStepKind::DataRefField(_)) {
                self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                continue;
            }
            if !self.check_field_place_step(site, ty, &first) {
                continue;
            }
            ty = first.target_ty;
            let mut valid = true;
            for storage_index in 1..storage_len {
                let step = self.program.dataref_places[index].storage[storage_index];
                if !matches!(
                    step.kind,
                    RirPlaceStepKind::StructField(_)
                        | RirPlaceStepKind::ExternField(_)
                        | RirPlaceStepKind::TupleField(_)
                ) {
                    self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                    valid = false;
                    break;
                }
                if !self.check_field_place_step(site, ty, &step) {
                    valid = false;
                    break;
                }
                ty = step.target_ty;
            }
            if !valid {
                continue;
            }
            let exact_materializer = self
                .program
                .materializers
                .get(materializer.index())
                .is_some_and(|entry| {
                    entry.id == materializer
                        && entry.ty == ty
                        && entry.position
                            == RustRecipePosition::StoredPayload(
                                LambdaStorageFamily::DataRefProjection,
                            )
                });
            if !exact_materializer
                || matches!(self.ty(ty), None | Some(RirType::Void | RirType::Slice(_)))
            {
                self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
            }
        }
    }

    fn check_place(&mut self, site: RirVerifySite, function: &RirFunction, place: &RirPlace) {
        let current = match place.root {
            RirPlaceRoot::Local(local) => {
                self.check_local_id(site, function, local);
                if self
                    .active_dyn_reborrows
                    .iter()
                    .any(|(reborrowed, _)| *reborrowed == local)
                {
                    self.push(site, RirVerifyErrorKind::InvalidDynPlan);
                }
                let current = match function.locals.get(local.index()) {
                    Some(local) => local.ty,
                    None => return,
                };
                if Self::function_local_is_hidden_cell_param(function, local) {
                    self.push(site, RirVerifyErrorKind::UnsupportedLambdaCapture);
                    return;
                }
                if self.function_local_is_scoped_place_source(function.id, local) {
                    self.push(site, RirVerifyErrorKind::UnsupportedLambdaCapture);
                    return;
                }
                if Self::function_local_is_mut_place_param(function, local)
                    && !place.projections.is_empty()
                    && !Self::projected_mut_place_arg_supported(current, &place.projections, true)
                {
                    self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                    return;
                }
                if !self
                    .initialized
                    .get(local.index())
                    .copied()
                    .unwrap_or(false)
                {
                    self.push(site, RirVerifyErrorKind::UninitializedLocal(local));
                }
                current
            }
            RirPlaceRoot::Global(global) => {
                let Some(global_ty) = self.check_global_id(site, global).map(|global| global.ty)
                else {
                    return;
                };
                global_ty
            }
        };
        self.check_projection_chain(site, function, current, &place.projections, true);
    }

    fn ty(&self, id: RirTypeId) -> Option<RirType> {
        self.program.types.get(id.index()).copied()
    }

    fn sequence_elem(&self, id: RirTypeId) -> Option<RirTypeId> {
        match self.ty(id)? {
            RirType::Array { elem, .. } | RirType::List(elem) | RirType::Slice(elem) => Some(elem),
            _ => None,
        }
    }

    fn type_id(&self, target: RirType) -> Option<RirTypeId> {
        self.program
            .types
            .iter()
            .position(|ty| *ty == target)
            .map(RirTypeId::from_index)
    }

    fn check_type_id(&mut self, site: RirVerifySite, id: RirTypeId) {
        if id.index() >= self.program.types.len() {
            self.push(site, RirVerifyErrorKind::BadId);
        }
    }

    fn check_struct_id(&mut self, site: RirVerifySite, id: RirStructId) {
        if id.index() >= self.program.structs.len() {
            self.push(site, RirVerifyErrorKind::BadId);
        }
    }

    fn check_dataref_id(&mut self, site: RirVerifySite, id: RirDataRefId) {
        if id.index() >= self.program.datarefs.len() {
            self.push(site, RirVerifyErrorKind::BadId);
        }
    }

    fn check_enum_id(&mut self, site: RirVerifySite, id: RirEnumId) {
        if id.index() >= self.program.enums.len() {
            self.push(site, RirVerifyErrorKind::BadId);
        }
    }

    fn check_flag_id(&mut self, site: RirVerifySite, id: RirFlagId) {
        if id.index() >= self.program.flags.len() {
            self.push(site, RirVerifyErrorKind::BadId);
        }
    }

    fn check_tuple_id(&mut self, site: RirVerifySite, id: RirTupleId) {
        if id.index() >= self.program.tuples.len() {
            self.push(site, RirVerifyErrorKind::BadId);
        }
    }

    fn check_stringify_helper_id(&mut self, site: RirVerifySite, id: RirStringifyHelperId) {
        if id.index() >= self.program.stringify_helpers.len() {
            self.push(site, RirVerifyErrorKind::BadId);
        }
    }

    fn check_cell_id(&mut self, site: RirVerifySite, id: RirCellId) -> Option<RirCellDecl> {
        match self.program.cells.get(id.index()).cloned() {
            Some(cell) => Some(cell),
            None => {
                self.push(site, RirVerifyErrorKind::BadId);
                None
            }
        }
    }

    fn check_cell_ref(
        &mut self,
        site: RirVerifySite,
        cell_ref: RirCellRef,
        storage: RirCellStorage,
    ) -> Option<RirCellDecl> {
        let cell = match cell_ref {
            RirCellRef::Owner(cell) | RirCellRef::Capture { cell, .. } => cell,
        };
        let decl = self.check_cell_id(site, cell)?;
        if decl.storage != storage {
            self.push(site, RirVerifyErrorKind::UnsupportedLambdaCapture);
            return None;
        }
        Some(decl)
    }

    fn check_function_cell_ref(
        &mut self,
        site: RirVerifySite,
        function: RirFunctionId,
        cell_ref: RirCellRef,
    ) -> Option<RirCellDecl> {
        let cell = match cell_ref {
            RirCellRef::Owner(cell) | RirCellRef::Capture { cell, .. } => cell,
        };
        let decl = self.check_cell_id(site, cell)?;
        match cell_ref {
            RirCellRef::Owner(_) if decl.owner == function => Some(decl),
            RirCellRef::Owner(_) => {
                self.push(site, RirVerifyErrorKind::BadId);
                None
            }
            RirCellRef::Capture { local, .. } => {
                if self.function_has_cell_capture(function, decl.id, local) {
                    Some(decl)
                } else {
                    self.push(site, RirVerifyErrorKind::UnsupportedLambdaCapture);
                    None
                }
            }
        }
    }

    fn check_scoped_place_cell_ref(
        &mut self,
        site: RirVerifySite,
        cell_ref: RirScopedPlaceCellRef,
    ) -> Option<RirScopedPlaceCellDecl> {
        let cell = match cell_ref {
            RirScopedPlaceCellRef::Owner(cell) | RirScopedPlaceCellRef::Capture { cell, .. } => {
                cell
            }
        };
        let decl = self.program.scoped_place_cells.get(cell.index()).cloned();
        if decl.is_none() {
            self.push(site, RirVerifyErrorKind::BadId);
        }
        decl
    }

    fn check_function_scoped_place_cell_ref(
        &mut self,
        site: RirVerifySite,
        function: RirFunctionId,
        cell_ref: RirScopedPlaceCellRef,
    ) -> Option<RirScopedPlaceCellDecl> {
        let decl = self.check_scoped_place_cell_ref(site, cell_ref)?;
        match cell_ref {
            RirScopedPlaceCellRef::Owner(_) if decl.owner == function => Some(decl),
            RirScopedPlaceCellRef::Owner(_) => {
                self.push(site, RirVerifyErrorKind::BadId);
                None
            }
            RirScopedPlaceCellRef::Capture { local, .. } => {
                if self.function_has_scoped_place_cell_capture(function, decl.id, local) {
                    Some(decl)
                } else {
                    self.push(site, RirVerifyErrorKind::UnsupportedLambdaCapture);
                    None
                }
            }
        }
    }

    fn mark_cell_initialized(&mut self, cell: RirCellId) {
        if let Some(slot) = self.initialized_cells.get_mut(cell.index()) {
            *slot = true;
        }
        if let Some(slot) = self.possibly_initialized_cells.get_mut(cell.index()) {
            *slot = true;
        }
    }

    fn cell_initialized(&self, cell: RirCellId) -> bool {
        self.initialized_cells
            .get(cell.index())
            .copied()
            .unwrap_or(false)
    }

    fn cell_possibly_initialized(&self, cell: RirCellId) -> bool {
        self.possibly_initialized_cells
            .get(cell.index())
            .copied()
            .unwrap_or(false)
    }

    fn function_local_is_hidden_cell_param(function: &RirFunction, local: RirLocalId) -> bool {
        matches!(
            Self::function_local_pass_mode(function, local),
            Some(RirPassMode::StackCell | RirPassMode::HeapCell | RirPassMode::ScopedPlaceCell)
        )
    }

    fn function_local_is_scoped_place_source(
        &self,
        function: RirFunctionId,
        local: RirLocalId,
    ) -> bool {
        self.program.scoped_place_cells.iter().any(|cell| {
            cell.owner == function
                && match &cell.source {
                    RirScopedPlaceSource::SourceMutParam { .. }
                    | RirScopedPlaceSource::RefSelf { .. } => {
                        cell.source.root_local() == Some(local)
                    }
                    RirScopedPlaceSource::PatternAlias { place } => matches!(
                        place.access,
                        RirMutPlaceAccess::Handle(RirMutPlaceHandle::Param { local: found })
                            if found == local
                    ),
                    RirScopedPlaceSource::ForRefAlias { .. } => false,
                }
        })
    }

    fn function_local_is_mut_place_param(function: &RirFunction, local: RirLocalId) -> bool {
        Self::function_local_pass_mode(function, local) == Some(RirPassMode::MutPlace)
    }

    fn place_is_mut_place_param_root(function: &RirFunction, place: &RirPlace) -> bool {
        let RirPlaceRoot::Local(local) = place.root else {
            return false;
        };
        place.projections.is_empty() && Self::function_local_is_mut_place_param(function, local)
    }

    fn assignment_projection_supported(projections: &[RirPlaceStep]) -> bool {
        projections
            .iter()
            .all(|step| !matches!(step.kind, RirPlaceStepKind::DataRefField(_)))
    }

    fn projected_mut_place_arg_supported(
        root_ty: RirTypeId,
        projections: &[RirPlaceStep],
        allow_collections: bool,
    ) -> bool {
        let mut ty = root_ty;
        let mut slice_dynamic = false;
        for step in projections {
            if step.source_ty != ty {
                return false;
            }
            let supported = match step.kind {
                RirPlaceStepKind::StructField(_)
                | RirPlaceStepKind::TupleField(_)
                | RirPlaceStepKind::ArrayIndex { .. } => true,
                RirPlaceStepKind::ExternField(_) | RirPlaceStepKind::DataRefField(_) => false,
                RirPlaceStepKind::ListIndex { .. } => allow_collections && !slice_dynamic,
                RirPlaceStepKind::SliceIndex { .. } => allow_collections,
            };
            if !supported {
                return false;
            }
            slice_dynamic |= matches!(step.kind, RirPlaceStepKind::SliceIndex { .. });
            ty = step.target_ty;
        }
        true
    }

    fn function_local_pass_mode(function: &RirFunction, local: RirLocalId) -> Option<RirPassMode> {
        function
            .locals
            .get(local.index())?
            .binding
            .parameter()
            .map(|(mode, _)| mode)
    }

    fn function_has_cell_capture(
        &self,
        function: RirFunctionId,
        cell: RirCellId,
        local: RirLocalId,
    ) -> bool {
        let Some(function) = self.program.functions.get(function.index()) else {
            return false;
        };
        let Some((param_index, _)) = function
            .params
            .iter()
            .enumerate()
            .find(|(_, param)| **param == local)
        else {
            return false;
        };
        self.function_param_maps_cell(function.id, param_index, cell)
    }

    fn function_param_is_cell_capture(
        &self,
        function: RirFunctionId,
        param_index: usize,
        param: RirLocalId,
        storage: RirCellStorage,
    ) -> bool {
        let Some((ty, param_mode, escape)) =
            self.program.functions[function.index()].parameter_data(param)
        else {
            return false;
        };
        let mode = Self::cell_capture_mode(storage);
        param_mode == mode
            && escape == RirParamEscape::NonEscaping
            && self.function_lambdas_match(function, |lambda| {
                matches!(lambda.captures.get(param_index), Some(capture)
                    if capture.ty == ty
                        && capture.mode == mode
                        && Self::lambda_capture_kind_cell(capture.kind, storage).is_some())
            })
    }

    fn function_param_maps_cell(
        &self,
        function: RirFunctionId,
        param_index: usize,
        cell: RirCellId,
    ) -> bool {
        let Some(decl) = self.program.cells.get(cell.index()) else {
            return false;
        };
        self.function_lambdas_match(function, |lambda| {
            matches!(lambda.captures.get(param_index), Some(capture)
                if Self::lambda_capture_kind_cell(capture.kind, decl.storage) == Some(cell))
        })
    }

    fn function_lambdas_match(
        &self,
        function: RirFunctionId,
        mut matches_lambda: impl FnMut(&RirLambda) -> bool,
    ) -> bool {
        let mut found = false;
        self.program
            .lambdas
            .iter()
            .filter(|lambda| lambda.function == function)
            .all(|lambda| {
                found = true;
                matches_lambda(lambda)
            })
            && found
    }

    fn function_has_scoped_place_cell_capture(
        &self,
        function: RirFunctionId,
        cell: RirScopedPlaceCellId,
        local: RirLocalId,
    ) -> bool {
        let Some(function) = self.program.functions.get(function.index()) else {
            return false;
        };
        let Some((param_index, _)) = function
            .params
            .iter()
            .enumerate()
            .find(|(_, param)| **param == local)
        else {
            return false;
        };
        self.function_param_maps_scoped_place_cell(function.id, param_index, cell)
    }

    fn function_param_is_scoped_place_cell_capture(
        &self,
        function: RirFunctionId,
        param_index: usize,
        param: RirLocalId,
    ) -> bool {
        let Some((param_ty, mode, escape)) =
            self.program.functions[function.index()].parameter_data(param)
        else {
            return false;
        };
        mode == RirPassMode::ScopedPlaceCell
            && escape == RirParamEscape::NonEscaping
            && self
                .program
                .lambdas
                .iter()
                .filter(|lambda| lambda.function == function)
                .all(|lambda| {
                    matches!(
                        lambda.captures.get(param_index),
                        Some(RirLambdaCapture {
                            ty,
                            mode: RirPassMode::ScopedPlaceCell,
                            kind: RirLambdaCaptureKind::ScopedPlaceCell { .. },
                        }) if *ty == param_ty
                    )
                })
            && self
                .program
                .lambdas
                .iter()
                .any(|lambda| lambda.function == function)
    }

    fn function_param_maps_scoped_place_cell(
        &self,
        function: RirFunctionId,
        param_index: usize,
        cell: RirScopedPlaceCellId,
    ) -> bool {
        self.program
            .lambdas
            .iter()
            .filter(|lambda| lambda.function == function)
            .all(|lambda| {
                matches!(
                    lambda.captures.get(param_index),
                    Some(RirLambdaCapture {
                        kind: RirLambdaCaptureKind::ScopedPlaceCell { cell: found },
                        ..
                    }) if *found == cell
                )
            })
            && self
                .program
                .lambdas
                .iter()
                .any(|lambda| lambda.function == function)
    }

    fn copyable_type(&self, ty: RirTypeId) -> bool {
        RirRustRepPolicy::new(self.program).copyable(ty)
    }

    fn string_ty(&self) -> Option<RirTypeId> {
        self.program
            .types
            .iter()
            .position(|ty| *ty == RirType::String)
            .map(RirTypeId::from_index)
    }

    fn check_string_literal_id(&mut self, site: RirVerifySite, id: RirStringLiteralId) {
        if id.index() >= self.program.string_literals.len() {
            self.push(site, RirVerifyErrorKind::BadId);
        }
    }

    fn check_const_id(&mut self, site: RirVerifySite, id: RirConstId) {
        if id.index() >= self.program.consts.len() {
            self.push(site, RirVerifyErrorKind::BadId);
        }
    }

    fn check_extern_id(&mut self, site: RirVerifySite, id: RirExternId) {
        if id.index() >= self.program.externs.len() {
            self.push(site, RirVerifyErrorKind::BadId);
        }
    }

    fn check_lambda_sig_id(&mut self, site: RirVerifySite, id: RirLambdaSigId) {
        if id.index() >= self.program.lambda_sigs.len() {
            self.push(site, RirVerifyErrorKind::BadId);
        }
    }

    fn check_lambda_id(&mut self, site: RirVerifySite, id: RirLambdaId) {
        if id.index() >= self.program.lambdas.len() {
            self.push(site, RirVerifyErrorKind::BadId);
        }
    }

    fn check_lambda_env_id(&mut self, site: RirVerifySite, id: RirLambdaEnvId) {
        if id.index() >= self.program.lambda_envs.len() {
            self.push(site, RirVerifyErrorKind::BadId);
        }
    }
    fn check_global_id(&mut self, site: RirVerifySite, id: RirGlobalId) -> Option<&RirGlobal> {
        match self.program.globals.get(id.index()) {
            Some(global) => Some(global),
            None => {
                self.push(site, RirVerifyErrorKind::BadId);
                None
            }
        }
    }

    fn check_function_id(&mut self, site: RirVerifySite, id: RirFunctionId) {
        if id.index() >= self.program.functions.len() {
            self.push(site, RirVerifyErrorKind::BadId);
        }
    }

    fn check_local_id(&mut self, site: RirVerifySite, function: &RirFunction, id: RirLocalId) {
        if id.index() >= function.locals.len() {
            self.push(site, RirVerifyErrorKind::BadId);
        }
    }

    fn push(&mut self, site: RirVerifySite, kind: RirVerifyErrorKind) {
        self.errors.push(RirVerifyError { site, kind });
    }
}

pub fn source_pass_mode(
    program: &air::Program,
    ty: air::TypeId,
    mode: air::ParamMode,
) -> RirPassMode {
    match mode {
        air::ParamMode::Value => RirPassMode::Value,
        air::ParamMode::SharedBorrow => RirPassMode::SharedBorrow,
        air::ParamMode::MutBorrow
            if matches!(program.type_arena.data(ty), air::TypeData::Dyn(_)) =>
        {
            RirPassMode::DynBorrow
        }
        air::ParamMode::MutBorrow => RirPassMode::MutPlace,
    }
}
