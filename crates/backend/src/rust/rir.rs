use std::{collections::HashSet, error::Error, fmt};

use anvyx_frontend::{
    air::{self, FunctionId, TypePassClasses},
    ast::{BinaryOp, ScalarKind, UnaryOp},
};
use anvyx_runtime::{
    CallbackEscape, CallbackThread, ExternCallbackSignature, ExternTypeExpr, ExternTypeKey,
    RustExternAbi, RustParamAbi,
};

use super::{
    native, native_call,
    place_access::{CollectionLoanBase, CollectionLoanProjection, collection_loan_step_supported},
    rep_policy::{
        LambdaStorageFamily, RirRustRepPolicy, RustMaterialIntent, RustMaterialSource,
        RustMaterialization, RustPayloadStorage, RustPhysicalRecipe, RustRepresentationPlan,
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
rir_id!(RirDynVariantId);

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
    pub ctx: RirCtxPlan,
    pub globals: Vec<RirGlobal>,
    pub functions: Vec<RirFunction>,
    pub externs: Vec<RirExtern>,
    pub types: Vec<RirType>,
    pub structs: Vec<RirStruct>,
    pub datarefs: Vec<RirDataRef>,
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
    pub dyn_origins: RirDynOrigins,
    pub dyn_carriers: Vec<RirDynCarrier>,
    pub dyn_weakenings: Vec<RirDynWeakening>,
    pub consts: Vec<RirConst>,
    pub string_literals: Vec<RirStringLiteral>,
    pub entry: Option<RirFunctionId>,
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct RirDynOrigins {
    pub surfaces: Vec<air::ContractSurfaceId>,
    pub witnesses: Vec<RirDynWitnessOrigin>,
    pub weakenings: Vec<RirDynWeakeningOrigin>,
    pub slots: Vec<RirDynSlotOrigin>,
    pub dispatches: Vec<RirDynDispatchOrigin>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct RirDynWitnessOrigin {
    pub air_witness: air::ContractWitnessId,
    pub surface: air::ContractSurfaceId,
    pub concrete_ty: RirTypeId,
    pub storage: RirDynStorage,
    pub payload: RirDynPayloadAction,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct RirDynWeakeningOrigin {
    pub air_weakening: air::ContractWeakeningId,
    pub source: air::ContractSurfaceId,
    pub target: air::ContractSurfaceId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct RirDynSlotOrigin {
    pub air_slot: air::ContractSlotId,
    pub surface: air::ContractSurfaceId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RirDynDispatchOrigin {
    pub air_witness: air::ContractWitnessId,
    pub air_slot: air::ContractSlotId,
    pub receiver: RirParamAbi,
    pub target: RirResolvedCallTarget,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RirDynCarrier {
    pub id: RirDynCarrierId,
    pub air_surface: air::ContractSurfaceId,
    pub storage_ty: RirTypeId,
    pub variants: Vec<RirDynVariant>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RirDynVariant {
    pub id: RirDynVariantId,
    pub air_witness: air::ContractWitnessId,
    pub concrete_ty: RirTypeId,
    pub storage: RirDynStorage,
    pub payload: RirDynPayloadAction,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RirDynStorage {
    Inline,
    Boxed,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RirDynPayloadAction {
    Copy,
    ManagedShare,
    CloneHandle,
    CloneLambda,
    CloneValue,
    ReconstructAggregate,
    CloneRecursive,
    BorrowView,
    Move,
}

impl From<RustPhysicalRecipe> for RirDynPayloadAction {
    fn from(recipe: RustPhysicalRecipe) -> Self {
        match recipe {
            RustPhysicalRecipe::Copy => Self::Copy,
            RustPhysicalRecipe::ManagedShare => Self::ManagedShare,
            RustPhysicalRecipe::CloneHandle => Self::CloneHandle,
            RustPhysicalRecipe::CloneLambda => Self::CloneLambda,
            RustPhysicalRecipe::CloneValue => Self::CloneValue,
            RustPhysicalRecipe::ReconstructAggregate => Self::ReconstructAggregate,
            RustPhysicalRecipe::BorrowView => Self::BorrowView,
            RustPhysicalRecipe::Move => Self::Move,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RirResolvedCallTarget {
    Function(RirFunctionId),
    Extern(RirExternId),
    Promoted {
        fields: Vec<RirFieldId>,
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
    pub variant: RirDynVariantId,
    pub receiver: RirParamAbi,
    pub target: RirResolvedCallTarget,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct RirDynWeakenArm {
    pub source: RirDynVariantId,
    pub target: RirDynVariantId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RirDynWeakening {
    pub air_id: air::ContractWeakeningId,
    pub source: RirDynCarrierId,
    pub target: RirDynCarrierId,
    pub arms: Vec<RirDynWeakenArm>,
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

    pub(super) fn retained_callback_sigs(&self) -> Vec<RirLambdaSigId> {
        self.callback_sigs_by_role(|semantic| {
            matches!(
                semantic,
                RirParamSemantic::EscapingLambda | RirParamSemantic::AnvCallback
            )
        })
    }

    pub(super) fn provider_callback_sigs(&self) -> Vec<RirLambdaSigId> {
        self.callback_sigs_by_role(|semantic| semantic == RirParamSemantic::EscapingLambda)
    }

    pub(super) fn heap_callback_sigs(&self) -> Vec<RirLambdaSigId> {
        self.callback_sigs_by_role(|semantic| semantic == RirParamSemantic::AnvCallback)
    }

    pub(super) fn has_retained_callbacks(&self) -> bool {
        !self.retained_callback_sigs().is_empty()
    }

    pub(super) fn native_call_plan(&self, ext: RirExternId) -> native_call::NativeCallPlan {
        let RirExternKind::Native(native) = &self.externs[ext.index()].kind;
        native.call_plan(self.has_retained_callbacks())
    }

    pub(super) fn dyn_carrier_for_enum(&self, enm: RirEnumId) -> Option<&RirDynCarrier> {
        self.dyn_carriers
            .iter()
            .find(|carrier| self.types.get(carrier.storage_ty.index()) == Some(&RirType::Enum(enm)))
    }

    fn callback_sigs_by_role(
        &self,
        role: impl Fn(RirParamSemantic) -> bool,
    ) -> Vec<RirLambdaSigId> {
        let mut sigs = vec![];
        for ext in &self.externs {
            for param in &ext.params {
                if !role(param.semantic) {
                    continue;
                }
                let RirType::Lambda(sig) = self.types[param.ty.index()] else {
                    continue;
                };
                if !sigs.contains(&sig) {
                    sigs.push(sig);
                }
            }
        }
        sigs
    }

    pub fn collection_storage_for(&self, value_ty: RirTypeId) -> Option<&RirCollectionStorage> {
        self.collection_storages
            .iter()
            .find(|storage| storage.value_ty == value_ty)
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
    pub air_id: air::GlobalId,
    pub module: air::ModuleId,
    pub name: RirSymbol,
    pub slot_symbol: RirSymbol,
    pub ty: RirTypeId,
    pub mutable: bool,
    pub init: RirFunctionId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RirStruct {
    pub id: RirStructId,
    pub air_id: Option<air::AggregateId>,
    pub symbol: RirSymbol,
    pub display: RirSymbol,
    pub native_path: Option<Vec<String>>,
    pub native_ref: bool,
    pub native_key: Option<ExternTypeKey>,
    pub copyable: bool,
    pub fields: Vec<RirField>,
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
    pub copyable: bool,
    pub fields: Vec<RirField>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RirDataRef {
    pub id: RirDataRefId,
    pub air_id: air::AggregateId,
    pub native_key: Option<ExternTypeKey>,
    pub symbol: RirSymbol,
    pub display: RirSymbol,
    pub cycle_capable: bool,
    pub fields: Vec<RirField>,
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
    pub air_id: Option<air::EnumId>,
    pub native_path: Option<Vec<String>>,
    pub native_key: Option<ExternTypeKey>,
    pub core: Option<RirCoreEnumKind>,
    pub repr: RirEnumRepr,
    pub raw_type: Option<RirTypeId>,
    pub symbol: RirSymbol,
    pub display: RirSymbol,
    pub copyable: bool,
    pub variants: Vec<RirVariant>,
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
    pub air_id: air::FlagId,
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
    pub semantic: RirParamSemantic,
    pub abi: RirParamAbi,
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
    pub source: RirLambdaSource,
    pub function: RirFunctionId,
    pub sig: RirLambdaSigId,
    pub escape: RirLambdaEscape,
    pub storage: RirLambdaStorage,
    pub captures: Vec<RirLambdaCapture>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct RirLambdaCapture {
    pub ty: RirTypeId,
    pub semantic: RirParamSemantic,
    pub abi: RirParamAbi,
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
pub enum RirLambdaSource {
    Function(FunctionId),
    Lambda(air::LambdaId),
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
    pub symbol: RirSymbol,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RirCollectionStorageKind {
    List {
        elem_ty: RirTypeId,
    },
    Map {
        key_ty: RirTypeId,
        value_ty: RirTypeId,
    },
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
        mode: RirParamSemantic,
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RirCtxPlan {
    pub statics_symbol: RirSymbol,
    pub globals_symbol: RirSymbol,
}

impl Default for RirCtxPlan {
    fn default() -> Self {
        Self {
            statics_symbol: RirSymbol::new("AnvStatics"),
            globals_symbol: RirSymbol::new("AnvGlobals"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct RirFunction {
    pub id: RirFunctionId,
    pub air_id: Option<FunctionId>,
    pub symbol: RirSymbol,
    pub params: Vec<RirParam>,
    pub ret: RirReturn,
    pub locals: Vec<RirLocal>,
    pub body: RirStructuredBlock,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct RirParam {
    pub local: RirLocalId,
    pub ty: RirTypeId,
    pub semantic: RirParamSemantic,
    pub abi: RirParamAbi,
    pub escape: RirParamEscape,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RirParamSemantic {
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RirParamAbi {
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

impl RirParamAbi {
    pub(super) fn is_readonly_receiver(self) -> bool {
        matches!(self, Self::Value | Self::SharedBorrow)
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
    pub initialized: bool,
    pub payload_ref: bool,
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
        dataref: RirDataRefId,
        projections: Vec<RirProjection>,
        value: RirOperand,
    },
    SequenceSlotSet {
        collection: RirCollectionAccess,
        index: RirLocalId,
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
                value.for_each_child(RirValueUse::Store, f);
            }
            Self::Eval(value) => value.for_each_child(RirValueUse::Read, f),
            Self::MutPlaceSet { place, value } => {
                f(RirChild::MutPlace {
                    place,
                    use_: RirPlaceUse::Mutate,
                });
                value.for_each_child(RirValueUse::Store, f);
            }
            Self::Assign { dst, value } => {
                f(RirChild::Place {
                    place: dst,
                    use_: RirPlaceUse::Mutate,
                });
                value.for_each_child(RirValueUse::Store, f);
            }
            Self::DataRefSet { object, value, .. } => {
                f(RirChild::Operand {
                    operand: object,
                    use_: RirValueUse::Read,
                });
                f(RirChild::Operand {
                    operand: value,
                    use_: RirValueUse::Store,
                });
            }
            Self::SequenceSlotSet {
                collection,
                index,
                value,
            }
            | Self::MapValueSet {
                map: collection,
                index,
                value,
            } => {
                f(RirChild::Collection {
                    collection,
                    use_: RirPlaceUse::Mutate,
                });
                f(RirChild::LocalRead(*index));
                f(RirChild::Operand {
                    operand: value,
                    use_: RirValueUse::Store,
                });
            }
            Self::If(branch) => {
                f(RirChild::Operand {
                    operand: &branch.cond,
                    use_: RirValueUse::Read,
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
                    use_: RirValueUse::Read,
                });
                f(RirChild::Operand {
                    operand: &range.end,
                    use_: RirValueUse::Read,
                });
                for operand in range.ordinal_plan.operands() {
                    f(RirChild::Operand {
                        operand,
                        use_: RirValueUse::Read,
                    });
                }
                f(RirChild::Block(&range.body));
            }
            Self::CollectionFor(for_) => {
                f(RirChild::LocalRead(for_.len));
                for operand in for_.ordinal_plan.operands() {
                    f(RirChild::Operand {
                        operand,
                        use_: RirValueUse::Read,
                    });
                }
                f(RirChild::Block(&for_.body));
            }
            Self::CollectionLoanScope(scope) => {
                let abi = match scope.mode {
                    RirCollectionLoanMode::ReadonlySequence
                    | RirCollectionLoanMode::ReadonlyMap => RirParamAbi::SharedBorrow,
                    RirCollectionLoanMode::MutableSequenceElement
                    | RirCollectionLoanMode::MutableMapValue => RirParamAbi::MutBorrow,
                };
                f(RirChild::Collection {
                    collection: &scope.root,
                    use_: RirPlaceUse::Borrow(abi),
                });
                f(RirChild::Block(&scope.body));
            }
            Self::CollectionSlotScope(block) => f(RirChild::Block(block)),
            Self::PatternMatch(match_) => {
                let aliases = match_.arms.iter().any(|arm| {
                    arm.alternatives.iter().any(|alternative| {
                        alternative
                            .bindings
                            .iter()
                            .any(|binding| binding.mode == RirPatternBindingMode::Alias)
                    })
                });
                f(RirChild::Place {
                    place: &match_.subject,
                    use_: if aliases {
                        RirPlaceUse::Mutate
                    } else {
                        RirPlaceUse::Read
                    },
                });
                for arm in &match_.arms {
                    f(RirChild::Block(&arm.block));
                }
            }
            Self::DynMatch(match_) => {
                match &match_.source {
                    RirDynMatchSource::Owned { value, air_use, .. } => operand(
                        f,
                        value,
                        if *air_use == air::DynOwnedUse::ConsumeTemporary {
                            RirValueUse::Consume
                        } else {
                            RirValueUse::Read
                        },
                    ),
                    RirDynMatchSource::MutPlace(place) => f(RirChild::MutPlace {
                        place,
                        use_: RirPlaceUse::Mutate,
                    }),
                    RirDynMatchSource::Borrowed(borrow) => match &borrow.source {
                        RirDynBorrowSource::Concrete { place, .. }
                        | RirDynBorrowSource::Owned { place, .. } => f(RirChild::MutPlace {
                            place,
                            use_: RirPlaceUse::Mutate,
                        }),
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
                    RirOptionSubject::Place(place) => f(RirChild::Place {
                        place,
                        use_: if match_.payload_ref {
                            RirPlaceUse::Borrow(RirParamAbi::SharedBorrow)
                        } else {
                            RirPlaceUse::Read
                        },
                    }),
                    RirOptionSubject::MutPlace(place) => f(RirChild::MutPlace {
                        place,
                        use_: if match_.payload_ref {
                            RirPlaceUse::Borrow(RirParamAbi::MutBorrow)
                        } else {
                            RirPlaceUse::Read
                        },
                    }),
                }
                f(RirChild::Block(&match_.some_block));
                f(RirChild::Block(&match_.none_block));
            }
            Self::MapEntryMatch(match_) => {
                f(RirChild::MutPlace {
                    place: &match_.map,
                    use_: RirPlaceUse::Mutate,
                });
                f(RirChild::Operand {
                    operand: &match_.key,
                    use_: RirValueUse::Read,
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

impl RirStructuredBlock {
    pub fn for_each_child(&self, f: &mut impl FnMut(RirChild<'_>)) {
        for stmt in &self.stmts {
            stmt.for_each_child(f);
        }
        if let RirTerm::Return(Some(value)) = &self.term {
            f(RirChild::Operand {
                operand: value,
                use_: RirValueUse::Consume,
            });
        }
        f(RirChild::Tail(&self.term));
    }
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
    pub fallback_binding: Option<RirLocalId>,
    pub fallback: RirStructuredBlock,
}

#[derive(Debug, Clone, PartialEq)]
pub enum RirDynMatchSource {
    Owned {
        value: RirOperand,
        air_use: air::DynOwnedUse,
        air_local: Option<air::LocalId>,
    },
    MutPlace(RirMutPlaceArg),
    Borrowed(RirDynBorrow),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RirDynMatchBinding {
    Discard,
    Owned(RirLocalId),
    Alias(RirLocalId),
}

#[derive(Debug, Clone, PartialEq)]
pub struct RirDynMatchArm {
    pub target: RirTypeId,
    pub variants: Vec<RirDynVariantId>,
    pub binding: RirDynMatchBinding,
    pub block: RirStructuredBlock,
}

#[derive(Debug, Clone, PartialEq)]
pub struct RirOptionMatch {
    pub subject: RirOptionSubject,
    pub payload: Option<RirLocalId>,
    pub payload_ref: bool,
    pub payload_escapes: bool,
    pub some_block: RirStructuredBlock,
    pub none_block: RirStructuredBlock,
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
    Field(RirFieldId),
    TupleField(u32),
    OptionalSome,
    EnumTupleField {
        enum_id: RirEnumId,
        variant: RirVariantId,
        field: u16,
    },
    EnumStructField {
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
    Owned,
    Alias,
}

#[derive(Clone, Copy, PartialEq, Eq)]
struct RirPatternBindingSignature {
    local: RirLocalId,
    ty: RirTypeId,
    mode: RirPatternBindingMode,
}

impl RirPatternBindingSignature {
    fn sort_key(self) -> (usize, usize, u8) {
        (
            self.local.index(),
            self.ty.index(),
            u8::from(self.mode == RirPatternBindingMode::Alias),
        )
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
    Owned { value: RirOperand, consume: bool },
    MutPlace(RirMutPlaceArg),
    Borrowed(RirDynBorrow),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RirValueUse {
    Read,
    Store,
    CallValue,
    Consume,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RirPlaceUse {
    Read,
    Mutate,
    Borrow(RirParamAbi),
}

#[derive(Debug, Clone, Copy)]
pub enum RirChild<'a> {
    Operand {
        operand: &'a RirOperand,
        use_: RirValueUse,
    },
    Place {
        place: &'a RirPlace,
        use_: RirPlaceUse,
    },
    MutPlace {
        place: &'a RirMutPlaceArg,
        use_: RirPlaceUse,
    },
    Collection {
        collection: &'a RirCollectionAccess,
        use_: RirPlaceUse,
    },
    CallArg(&'a RirCallArg),
    CaptureArg(&'a RirLambdaCaptureArg),
    LocalRead(RirLocalId),
    Block(&'a RirStructuredBlock),
    Tail(&'a RirTerm),
}

fn operand<'a>(f: &mut impl FnMut(RirChild<'a>), operand: &'a RirOperand, use_: RirValueUse) {
    f(RirChild::Operand { operand, use_ });
}

fn place<'a>(f: &mut impl FnMut(RirChild<'a>), place: &'a RirPlace, use_: RirPlaceUse) {
    f(RirChild::Place { place, use_ });
}

fn collection<'a>(
    f: &mut impl FnMut(RirChild<'a>),
    collection: &'a RirCollectionAccess,
    use_: RirPlaceUse,
) {
    f(RirChild::Collection { collection, use_ });
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RirFlagStaticOp {
    Empty,
    All,
}

#[derive(Debug, Clone, PartialEq)]
pub enum RirRValue {
    Use(RirOperand),
    MoveValue {
        value: RirOperand,
        air_local: air::LocalId,
        ty: RirTypeId,
    },
    DynCopy {
        carrier: RirDynCarrierId,
        value: RirOperand,
        ty: RirTypeId,
    },
    DynPack {
        carrier: RirDynCarrierId,
        variant: RirDynVariantId,
        air_witness: air::ContractWitnessId,
        air_use: air::DynOwnedUse,
        air_local: Option<air::LocalId>,
        value: RirOperand,
        action: RirDynPayloadAction,
        ty: RirTypeId,
    },
    DynWeaken {
        source: RirDynCarrierId,
        target: RirDynCarrierId,
        air_weakening: air::ContractWeakeningId,
        air_use: air::DynOwnedUse,
        air_local: Option<air::LocalId>,
        value: RirOperand,
        arms: Vec<RirDynWeakenArm>,
        ty: RirTypeId,
    },
    DynDowncast {
        carrier: RirDynCarrierId,
        air_surface: air::ContractSurfaceId,
        air_use: air::DynOwnedUse,
        air_local: Option<air::LocalId>,
        value: RirOperand,
        target: RirTypeId,
        variants: Vec<RirDynVariantId>,
        ty: RirTypeId,
    },
    DynCall {
        carrier: RirDynCarrierId,
        air_slot: air::ContractSlotId,
        exact_variant: Option<RirDynVariantId>,
        receiver: RirDynReceiver,
        args: Vec<RirCallArg>,
        arms: Vec<RirDynDispatchArm>,
        ty: RirTypeId,
    },
    FunctionValue {
        value: RirOperand,
        escape: Option<RirLambdaEscape>,
        ty: RirTypeId,
    },
    Struct {
        ty: RirTypeId,
        fields: Vec<RirOperand>,
    },
    Tuple {
        ty: RirTypeId,
        fields: Vec<RirOperand>,
    },
    DataRefAlloc {
        ty: RirTypeId,
        fields: Vec<RirOperand>,
    },
    DataRefGet {
        object: RirOperand,
        dataref: RirDataRefId,
        projections: Vec<RirProjection>,
        ty: RirTypeId,
    },
    CellGetCopy {
        cell: RirCellRef,
        ty: RirTypeId,
    },
    ScopedPlaceCellGet {
        cell: RirScopedPlaceCellRef,
        ty: RirTypeId,
    },
    MutPlaceGetCopy {
        place: RirMutPlaceArg,
        ty: RirTypeId,
    },
    Array {
        ty: RirTypeId,
        elems: Vec<RirOperand>,
    },
    List {
        ty: RirTypeId,
        elems: Vec<RirOperand>,
    },
    Map {
        ty: RirTypeId,
        entries: Vec<(RirOperand, RirOperand)>,
    },
    EnumVariant {
        ty: RirTypeId,
        variant: RirVariantId,
        fields: Vec<RirOperand>,
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
        value: RirOperand,
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
        spec: RirFormatSpec,
    },
    Len {
        source: RirPlace,
    },
    CollectionLen {
        source: RirCollectionAccess,
    },
    SequenceSlotAt {
        collection: RirCollectionAccess,
        index: RirLocalId,
        ty: RirTypeId,
    },
    ListPush {
        list: RirCollectionAccess,
        value: RirOperand,
    },
    SliceView {
        source: RirPlace,
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
        key: RirOperand,
        value: RirOperand,
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
    pub fn for_each_child(&self, use_: RirValueUse, f: &mut impl FnMut(RirChild<'_>)) {
        match self {
            Self::Use(value) | Self::FunctionValue { value, .. } => operand(f, value, use_),
            Self::MoveValue { value, .. } => operand(f, value, RirValueUse::Consume),
            Self::DynCopy { value, .. } => {
                let use_ = RirValueUse::Read;
                operand(f, value, use_);
            }
            Self::DynPack { value, action, .. } => operand(
                f,
                value,
                if *action == RirDynPayloadAction::Move {
                    RirValueUse::Consume
                } else {
                    RirValueUse::Store
                },
            ),
            Self::DynWeaken { value, air_use, .. } | Self::DynDowncast { value, air_use, .. } => {
                operand(
                    f,
                    value,
                    if *air_use == air::DynOwnedUse::ConsumeTemporary {
                        RirValueUse::Consume
                    } else {
                        RirValueUse::Read
                    },
                );
            }
            Self::DynCall { receiver, args, .. } => {
                match receiver {
                    RirDynReceiver::Owned { value, consume } => operand(
                        f,
                        value,
                        if *consume {
                            RirValueUse::Consume
                        } else {
                            RirValueUse::Read
                        },
                    ),
                    RirDynReceiver::MutPlace(value) => f(RirChild::MutPlace {
                        place: value,
                        use_: RirPlaceUse::Mutate,
                    }),
                    RirDynReceiver::Borrowed(borrow) => match &borrow.source {
                        RirDynBorrowSource::Concrete { place, .. }
                        | RirDynBorrowSource::Owned { place, .. } => f(RirChild::MutPlace {
                            place,
                            use_: RirPlaceUse::Mutate,
                        }),
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
                    operand(f, field, RirValueUse::Store);
                }
            }
            Self::DataRefGet {
                object,
                projections,
                ..
            } => {
                operand(f, object, RirValueUse::Read);
                for projection in projections {
                    if let RirProjection::Index(local) = projection {
                        f(RirChild::LocalRead(*local));
                    }
                }
            }
            Self::CellGetCopy { .. }
            | Self::ScopedPlaceCellGet { .. }
            | Self::FlagStatic { .. } => {}
            Self::MutPlaceGetCopy { place, .. } => f(RirChild::MutPlace {
                place,
                use_: RirPlaceUse::Read,
            }),
            Self::Map { entries, .. } => {
                for (key, value) in entries {
                    operand(f, key, RirValueUse::Store);
                    operand(f, value, RirValueUse::Store);
                }
            }
            Self::Unary { value, .. }
            | Self::Cast { value, .. }
            | Self::RawProject { value, .. }
            | Self::RawTryConstruct { value, .. }
            | Self::Stringify { value, .. }
            | Self::Format { value, .. } => operand(f, value, RirValueUse::Read),
            Self::OptionalSome { value, .. } => operand(f, value, RirValueUse::Store),
            Self::Binary { lhs, rhs, .. } | Self::SharedRefEq { lhs, rhs, .. } => {
                operand(f, lhs, RirValueUse::Read);
                operand(f, rhs, RirValueUse::Read);
            }
            Self::Call { callee, args, .. } => {
                if let RirCallTarget::LambdaValue { callee, .. } = callee {
                    operand(f, callee, RirValueUse::CallValue);
                }
                for arg in args {
                    f(RirChild::CallArg(arg));
                }
            }
            Self::StringConcat { parts } => {
                for part in parts {
                    operand(f, part, RirValueUse::Read);
                }
            }
            Self::Len { source } => place(f, source, RirPlaceUse::Read),
            Self::CollectionLen { source } => collection(f, source, RirPlaceUse::Read),
            Self::SequenceSlotAt {
                collection: source,
                index,
                ..
            }
            | Self::MapEntryAt {
                map: source, index, ..
            }
            | Self::MapKeyAt {
                map: source, index, ..
            }
            | Self::MapValueAt {
                map: source, index, ..
            } => {
                collection(f, source, RirPlaceUse::Read);
                f(RirChild::LocalRead(*index));
            }
            Self::ListPush { list, value } => {
                collection(f, list, RirPlaceUse::Mutate);
                operand(f, value, RirValueUse::Store);
            }
            Self::SliceView {
                source,
                start,
                end,
                mutable,
                ..
            } => {
                place(
                    f,
                    source,
                    RirPlaceUse::Borrow(if *mutable {
                        RirParamAbi::MutBorrow
                    } else {
                        RirParamAbi::SharedBorrow
                    }),
                );
                f(RirChild::LocalRead(*start));
                f(RirChild::LocalRead(*end));
            }
            Self::RangeListCopy {
                source, start, end, ..
            } => {
                place(f, source, RirPlaceUse::Read);
                f(RirChild::LocalRead(*start));
                f(RirChild::LocalRead(*end));
            }
            Self::MapGet { map, key, .. } | Self::MapRemove { map, key, .. } => {
                collection(
                    f,
                    map,
                    if matches!(self, Self::MapRemove { .. }) {
                        RirPlaceUse::Mutate
                    } else {
                        RirPlaceUse::Read
                    },
                );
                operand(f, key, RirValueUse::Read);
            }
            Self::MapInsert {
                map, key, value, ..
            } => {
                collection(f, map, RirPlaceUse::Mutate);
                operand(f, key, RirValueUse::Store);
                operand(f, value, RirValueUse::Store);
            }
            Self::CheckedIterCount { count, .. } => operand(f, count, RirValueUse::Read),
            Self::Lambda { captures, .. } => {
                for capture in captures {
                    f(RirChild::CaptureArg(capture));
                }
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct RirFormatSpec {
    pub fill: char,
    pub align: Option<RirFormatAlign>,
    pub sign: RirFormatSign,
    pub zero_pad: bool,
    pub width: Option<u32>,
    pub precision: Option<u32>,
    pub kind: RirFormatKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RirFormatAlign {
    Left,
    Right,
    Center,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum RirFormatSign {
    #[default]
    Default,
    Always,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum RirFormatKind {
    #[default]
    Default,
    Hex,
    HexUpper,
    Binary,
    Exp,
    ExpUpper,
}

#[derive(Debug, Clone, PartialEq)]
pub enum RirLambdaCaptureArg {
    Readonly { value: RirOperand },
    Scoped { place: RirPlace },
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
    pub air_weakening: Option<air::ContractWeakeningId>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RirDynBorrowSource {
    Concrete {
        place: RirMutPlaceArg,
        carrier: RirDynCarrierId,
        air_witness: air::ContractWitnessId,
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
    Value(RirOperand),
    MovedValue {
        value: RirOperand,
        air_local: air::LocalId,
    },
    InitFieldProvided(RirOperand),
    InitFieldOmitted,
    SharedBorrow(RirPlace),
    SharedStringConst(RirStringLiteralId),
    MutBorrow(RirPlace),
    MutPlace(RirMutPlaceArg),
    DynBorrow(RirDynBorrow),
    ScopedLambda {
        callee: RirOperand,
        sig: RirLambdaSigId,
    },
    EscapingLambda {
        callee: RirOperand,
        sig: RirLambdaSigId,
    },
    AnvCallback {
        callee: RirOperand,
        sig: RirLambdaSigId,
    },
}

impl RirCallArg {
    pub fn semantic(&self) -> RirParamSemantic {
        match self {
            Self::Value(_)
            | Self::MovedValue { .. }
            | Self::InitFieldProvided(_)
            | Self::InitFieldOmitted => RirParamSemantic::Value,
            Self::SharedBorrow(_) | Self::SharedStringConst(_) => RirParamSemantic::SharedBorrow,
            Self::MutBorrow(_) => RirParamSemantic::MutBorrow,
            Self::MutPlace(_) => RirParamSemantic::MutPlace,
            Self::DynBorrow(_) => RirParamSemantic::DynBorrow,
            Self::ScopedLambda { .. } => RirParamSemantic::ScopedLambda,
            Self::EscapingLambda { .. } => RirParamSemantic::EscapingLambda,
            Self::AnvCallback { .. } => RirParamSemantic::AnvCallback,
        }
    }

    pub fn adapted_to(&self, semantic: RirParamSemantic, program: &RirProgram) -> Option<Self> {
        if self.semantic() == semantic {
            return Some(self.clone());
        }
        match (semantic, self) {
            (RirParamSemantic::Value, Self::SharedBorrow(place)) => {
                Some(Self::Value(RirOperand::Place(place.clone())))
            }
            (RirParamSemantic::SharedBorrow, Self::Value(RirOperand::Place(place))) => {
                Some(Self::SharedBorrow(place.clone()))
            }
            (
                RirParamSemantic::ScopedLambda
                | RirParamSemantic::EscapingLambda
                | RirParamSemantic::AnvCallback,
                Self::SharedBorrow(place) | Self::Value(RirOperand::Place(place)),
            ) => {
                let Some(RirType::Lambda(sig)) = program.types.get(place.ty.index()) else {
                    return None;
                };
                let callee = RirOperand::Place(place.clone());
                Some(match semantic {
                    RirParamSemantic::ScopedLambda => Self::ScopedLambda { callee, sig: *sig },
                    RirParamSemantic::EscapingLambda => Self::EscapingLambda { callee, sig: *sig },
                    RirParamSemantic::AnvCallback => Self::AnvCallback { callee, sig: *sig },
                    _ => unreachable!(),
                })
            }
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RirMutPlaceArg {
    pub access: RirMutPlaceAccess,
    pub projections: Vec<RirProjection>,
    pub ty: RirTypeId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RirMutPlaceAccess {
    Handle(RirMutPlaceHandle),
    DataRef {
        object: RirOperand,
        dataref: RirDataRefId,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RirMutPlaceHandle {
    Local {
        local: RirLocalId,
        ty: RirTypeId,
    },
    Param {
        local: RirLocalId,
        ty: RirTypeId,
    },
    StackCell {
        cell: RirCellRef,
        ty: RirTypeId,
    },
    HeapCell {
        cell: RirCellRef,
        ty: RirTypeId,
    },
    ScopedPlaceCell {
        cell: RirScopedPlaceCellRef,
        ty: RirTypeId,
    },
    Global {
        global: RirGlobalId,
        ty: RirTypeId,
    },
}

impl RirMutPlaceHandle {
    pub fn ty(&self) -> RirTypeId {
        match self {
            Self::Local { ty, .. }
            | Self::Param { ty, .. }
            | Self::StackCell { ty, .. }
            | Self::HeapCell { ty, .. }
            | Self::ScopedPlaceCell { ty, .. }
            | Self::Global { ty, .. } => *ty,
        }
    }

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

impl RirMutPlaceAccess {
    pub fn ty(&self) -> Option<RirTypeId> {
        match self {
            Self::Handle(handle) => Some(handle.ty()),
            Self::DataRef { .. } => None,
        }
    }
}

impl RirMutPlaceArg {
    pub fn from_handle(
        handle: RirMutPlaceHandle,
        projections: Vec<RirProjection>,
        ty: RirTypeId,
    ) -> Self {
        Self {
            access: RirMutPlaceAccess::Handle(handle),
            projections,
            ty,
        }
    }

    pub fn projected(
        handle: RirMutPlaceHandle,
        projections: Vec<RirProjection>,
        ty: RirTypeId,
    ) -> Self {
        Self::from_handle(handle, projections, ty)
    }

    pub fn local(place: RirPlace) -> Self {
        let RirPlaceRoot::Local(local) = place.root else {
            unreachable!("expected a local RIR place")
        };
        Self {
            access: RirMutPlaceAccess::Handle(RirMutPlaceHandle::Local {
                local,
                ty: place.ty,
            }),
            projections: place.projections,
            ty: place.ty,
        }
    }

    pub fn param(local: RirLocalId, ty: RirTypeId) -> Self {
        Self {
            access: RirMutPlaceAccess::Handle(RirMutPlaceHandle::Param { local, ty }),
            projections: vec![],
            ty,
        }
    }

    pub fn stack_cell(cell: RirCellRef, ty: RirTypeId) -> Self {
        Self {
            access: RirMutPlaceAccess::Handle(RirMutPlaceHandle::StackCell { cell, ty }),
            projections: vec![],
            ty,
        }
    }

    pub fn heap_cell(cell: RirCellRef, ty: RirTypeId) -> Self {
        Self {
            access: RirMutPlaceAccess::Handle(RirMutPlaceHandle::HeapCell { cell, ty }),
            projections: vec![],
            ty,
        }
    }

    pub fn scoped_place_cell(cell: RirScopedPlaceCellRef, ty: RirTypeId) -> Self {
        Self {
            access: RirMutPlaceAccess::Handle(RirMutPlaceHandle::ScopedPlaceCell { cell, ty }),
            projections: vec![],
            ty,
        }
    }

    pub fn global(global: RirGlobalId, ty: RirTypeId) -> Self {
        Self {
            access: RirMutPlaceAccess::Handle(RirMutPlaceHandle::Global { global, ty }),
            projections: vec![],
            ty,
        }
    }

    pub fn root_local(&self) -> Option<RirLocalId> {
        match &self.access {
            RirMutPlaceAccess::Handle(handle) => handle.local(),
            RirMutPlaceAccess::DataRef { .. } => None,
        }
    }

    pub fn uses_local(&self, local: RirLocalId) -> bool {
        self.root_local() == Some(local)
            || self.projections.iter().any(
                |projection| matches!(projection, RirProjection::Index(index) if *index == local),
            )
            || matches!(&self.access, RirMutPlaceAccess::DataRef { object: RirOperand::Place(place), .. } if place.uses_local(local))
    }

    pub fn dataref(
        object: RirOperand,
        dataref: RirDataRefId,
        projections: Vec<RirProjection>,
        ty: RirTypeId,
    ) -> Self {
        Self {
            access: RirMutPlaceAccess::DataRef { object, dataref },
            projections,
            ty,
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
    pub projections: Vec<RirProjection>,
    pub ty: RirTypeId,
}

impl RirPlace {
    pub fn local(local: RirLocalId, projections: Vec<RirProjection>, ty: RirTypeId) -> Self {
        Self {
            root: RirPlaceRoot::Local(local),
            projections,
            ty,
        }
    }

    pub fn global(global: RirGlobalId, projections: Vec<RirProjection>, ty: RirTypeId) -> Self {
        Self {
            root: RirPlaceRoot::Global(global),
            projections,
            ty,
        }
    }

    pub fn uses_local(&self, local: RirLocalId) -> bool {
        self.root == RirPlaceRoot::Local(local)
            || self.projections.iter().any(
                |projection| matches!(projection, RirProjection::Index(index) if *index == local),
            )
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RirCollectionAccess {
    Direct(RirPlace),
    MutPlace(RirMutPlaceArg),
}

impl RirCollectionAccess {
    pub fn ty(&self) -> RirTypeId {
        match self {
            Self::Direct(place) => place.ty,
            Self::MutPlace(place) => place.ty,
        }
    }

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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RirProjection {
    Field(RirFieldId),
    TupleField(RirFieldId),
    Index(RirLocalId),
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

#[derive(Debug, Clone, PartialEq)]
pub struct RirExtern {
    pub id: RirExternId,
    pub air_id: Option<air::ExternId>,
    pub symbol: RirSymbol,
    pub kind: RirExternKind,
    pub params: Vec<RirExternParam>,
    pub ret: RirTypeId,
    pub abi: air::ExternAbi,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RirExternKind {
    Native(RirNativeExtern),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RirNativeExtern {
    pub path: Vec<String>,
    pub abi: RustExternAbi,
    pub callback_receiver: Option<usize>,
}

impl RirNativeExtern {
    pub(super) fn new(path: Vec<String>, abi: RustExternAbi) -> Self {
        Self {
            path,
            abi,
            callback_receiver: None,
        }
    }

    pub(super) fn with_callback_receiver(mut self, receiver: Option<usize>) -> Self {
        self.callback_receiver = receiver;
        self
    }

    pub(super) fn call_plan(&self, retained_callbacks: bool) -> native_call::NativeCallPlan {
        native_call::NativeCallPlan::for_abi(&self.abi, retained_callbacks)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct RirExternParam {
    pub ty: RirTypeId,
    pub semantic: RirParamSemantic,
    pub abi: RirParamAbi,
    pub escape: RirParamEscape,
}

#[derive(Debug, Clone, Copy)]
pub struct VerifiedRirProgram<'a> {
    program: &'a RirProgram,
}

impl<'a> VerifiedRirProgram<'a> {
    pub fn program(&self) -> &'a RirProgram {
        self.program
    }
}

pub fn verify(program: &RirProgram) -> Result<VerifiedRirProgram<'_>, Vec<RirVerifyError>> {
    let mut cx = VerifyCx {
        program,
        errors: vec![],
        initialized: vec![],
        possibly_initialized: vec![],
        payload_ref_owned: vec![],
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
    };
    cx.check();
    if !super::exact_witness::is_canonical(program) {
        cx.errors.push(RirVerifyError {
            site: RirVerifySite::Program,
            kind: RirVerifyErrorKind::InvalidExactWitness,
        });
    }
    if cx.errors.is_empty() {
        Ok(VerifiedRirProgram { program })
    } else {
        Err(cx.errors)
    }
}

pub fn verify_with_air<'a>(
    program: &'a RirProgram,
    air: &air::Program,
) -> Result<VerifiedRirProgram<'a>, Vec<RirVerifyError>> {
    let mut errors = match verify(program) {
        Ok(_) => vec![],
        Err(errors) => errors,
    };
    let invalid = || RirVerifyError {
        site: RirVerifySite::Program,
        kind: RirVerifyErrorKind::InvalidDynOrigin,
    };
    for surface in &program.dyn_origins.surfaces {
        if surface.index() >= air.contract_surfaces.len() {
            errors.push(invalid());
        }
    }
    for origin in &program.dyn_origins.witnesses {
        let valid = air
            .contract_witnesses
            .get(origin.air_witness.index())
            .is_some_and(|witness| {
                witness.key.surface == origin.surface
                    && air_rir_type_matches(
                        air,
                        program,
                        witness.key.concrete_ty,
                        origin.concrete_ty,
                        &mut std::collections::BTreeSet::new(),
                    )
            });
        if !valid {
            errors.push(invalid());
        }
    }
    for origin in &program.dyn_origins.weakenings {
        let valid = air
            .contract_weakenings
            .get(origin.air_weakening.index())
            .is_some_and(|weakening| {
                weakening.source == origin.source && weakening.target == origin.target
            });
        if !valid {
            errors.push(invalid());
        }
    }
    for origin in &program.dyn_origins.slots {
        let valid = air
            .contract_surfaces
            .get(origin.surface.index())
            .is_some_and(|surface| surface.slots.iter().any(|slot| slot.id == origin.air_slot));
        if !valid {
            errors.push(invalid());
        }
    }
    for enm in &program.enums {
        if let Some(air_id) = enm.air_id
            && !air_enum_metadata_matches(air, program, air_id, enm)
        {
            errors.push(RirVerifyError {
                site: RirVerifySite::Program,
                kind: RirVerifyErrorKind::InvalidAirMetadata,
            });
        }
    }
    for flag in &program.flags {
        if !air_flag_metadata_matches(air, flag) {
            errors.push(RirVerifyError {
                site: RirVerifySite::Program,
                kind: RirVerifyErrorKind::InvalidAirMetadata,
            });
        }
    }
    for helper in &program.stringify_helpers {
        if !air_stringify_metadata_matches(air, program, helper) {
            errors.push(RirVerifyError {
                site: RirVerifySite::Program,
                kind: RirVerifyErrorKind::InvalidAirMetadata,
            });
        }
    }
    for carrier in &program.dyn_carriers {
        let storage_matches = air.type_arena.iter().enumerate().any(|(index, ty)| {
            matches!(ty, air::TypeData::Dyn(surface) if *surface == carrier.air_surface)
                && RirTypeId::from_index(index) == carrier.storage_ty
                && air_rir_type_matches(
                    air,
                    program,
                    air::TypeId::from_index(index),
                    carrier.storage_ty,
                    &mut std::collections::BTreeSet::new(),
                )
        });
        if !storage_matches {
            errors.push(invalid());
        }
    }
    let classes = TypePassClasses::analyze(air);
    let physical = RustRepresentationPlan::new(air, &classes).dynamic_layout_plan();
    let physical_valid = physical.is_ok_and(|physical| {
        physical.carriers.len() == program.dyn_carriers.len()
            && physical.carriers.iter().all(|planned| {
                program
                    .dyn_carriers
                    .iter()
                    .find(|carrier| carrier.air_surface == planned.surface)
                    .is_some_and(|carrier| {
                        carrier.variants.len() == planned.variants.len()
                            && planned.variants.iter().all(|planned| {
                                carrier.variants.iter().any(|variant| {
                                    variant.air_witness == planned.witness
                                        && variant.concrete_ty.index()
                                            == planned.concrete_ty.index()
                                        && (variant.storage
                                            == match planned.storage {
                                                RustPayloadStorage::Inline => RirDynStorage::Inline,
                                                RustPayloadStorage::Boxed => RirDynStorage::Boxed,
                                            }
                                            || (matches!(
                                                air.type_arena.data(planned.concrete_ty),
                                                air::TypeData::Function(_)
                                            ) && variant.storage == RirDynStorage::Boxed))
                                        && if planned.recursive {
                                            variant.payload == RirDynPayloadAction::CloneRecursive
                                        } else {
                                            match planned.payload {
                                            Ok(payload) => variant.payload == payload.into(),
                                            Err(_)
                                                if matches!(
                                                    air.type_arena.data(planned.concrete_ty),
                                                    air::TypeData::Function(_)
                                                ) =>
                                            {
                                                match program.types.get(variant.concrete_ty.index()) {
                                                    Some(RirType::Lambda(sig)) => {
                                                        let policy = RirRustRepPolicy::new(program);
                                                        if policy.lambda_sig_copyable(*sig) {
                                                            variant.payload == RirDynPayloadAction::Copy
                                                        } else {
                                                            policy.lambda_sig_cloneable(*sig)
                                                                && variant.payload
                                                                    == RirDynPayloadAction::CloneLambda
                                                        }
                                                    }
                                                    _ => false,
                                                }
                                            }
                                            Err(_) => false,
                                            }
                                        }
                                })
                            })
                    })
            })
    });
    if !physical_valid {
        errors.push(invalid());
    }
    verify_dynamic_weakenings(program, air, &mut errors);
    verify_dynamic_dispatch_origins(program, air, &mut errors);
    verify_moved_value_origins(program, air, &mut errors);
    if errors.is_empty() {
        Ok(VerifiedRirProgram { program })
    } else {
        Err(errors)
    }
}

fn air_enum_metadata_matches(
    air: &air::Program,
    rir: &RirProgram,
    air_id: air::EnumId,
    enm: &RirEnum,
) -> bool {
    let Some(source) = air.enums.get(air_id.index()) else {
        return false;
    };
    let repr_matches = matches!(
        (source.repr, enm.repr),
        (air::EnumRepr::Adt, RirEnumRepr::Adt)
            | (air::EnumRepr::RawInt, RirEnumRepr::RawInt)
            | (air::EnumRepr::RawString, RirEnumRepr::RawString)
    );
    repr_matches
        && source.name.as_str() == enm.display.as_str()
        && source.variants.len() == enm.variants.len()
        && source
            .variants
            .iter()
            .zip(&enm.variants)
            .all(|(source, target)| {
                source.name.as_str() == target.display.as_str()
                    && match (&source.raw_value, &target.raw_value) {
                        (None, None) => true,
                        (Some(air::RawEnumValue::Int(a)), Some(RirRawEnumValue::Int(b))) => a == b,
                        (Some(air::RawEnumValue::String(a)), Some(RirRawEnumValue::String(b))) => {
                            rir.string_literals
                                .get(b.index())
                                .is_some_and(|literal| literal.text == *a)
                        }
                        _ => false,
                    }
            })
}

fn air_flag_metadata_matches(air: &air::Program, flag: &RirFlag) -> bool {
    let Some(source) = air.flags.get(flag.air_id.index()) else {
        return false;
    };
    source.name.as_str() == flag.display.as_str()
        && source.known_bits == flag.known_bits
        && source.members.len() == flag.members.len()
        && source
            .members
            .iter()
            .zip(&flag.members)
            .all(|(source, target)| {
                source.id.index() == target.id.index()
                    && source.name.as_str() == target.display.as_str()
                    && source.value == target.value
                    && source.atomic == target.atomic
            })
}

fn air_stringify_metadata_matches(
    air: &air::Program,
    rir: &RirProgram,
    helper: &RirStringifyHelper,
) -> bool {
    match &helper.kind {
        RirStringifyHelperKind::Struct(strukt) => {
            let Some(target) = rir.structs.get(strukt.index()) else {
                return false;
            };
            let Some(air_id) = target.air_id else {
                return false;
            };
            let Some(source) = air.aggregates.get(air_id.index()) else {
                return false;
            };
            source.kind == air::AggregateKind::Struct
                && source.name.as_str() == target.display.as_str()
                && source.fields.len() == target.fields.len()
        }
        RirStringifyHelperKind::Enum { enm, variants } => {
            let Some(target) = rir.enums.get(enm.index()) else {
                return false;
            };
            let Some(air_id) = target.air_id else {
                return false;
            };
            let Some(source) = air.enums.get(air_id.index()) else {
                return false;
            };
            variants.len() == source.variants.len()
                && variants
                    .iter()
                    .zip(&source.variants)
                    .all(|(plan, variant)| {
                        let label = format!("{}.{}", source.name.as_str(), variant.name.as_str());
                        rir.string_literals
                            .get(plan.label.index())
                            .is_some_and(|literal| literal.text == label)
                            && match &variant.shape {
                                air::VariantShape::Struct(fields) => {
                                    plan.field_labels.len() == fields.len()
                                        && plan.field_labels.iter().zip(fields).all(
                                            |(id, field)| {
                                                rir.string_literals.get(id.index()).is_some_and(
                                                    |literal| {
                                                        literal.text
                                                            == format!("{}: ", field.name.as_str())
                                                    },
                                                )
                                            },
                                        )
                                }
                                air::VariantShape::Unit | air::VariantShape::Tuple(_) => {
                                    plan.field_labels.is_empty()
                                }
                            }
                    })
        }
        RirStringifyHelperKind::Flag {
            flag,
            empty,
            members,
        } => {
            let Some(target) = rir.flags.get(flag.index()) else {
                return false;
            };
            let Some(source) = air.flags.get(target.air_id.index()) else {
                return false;
            };
            rir.string_literals
                .get(empty.index())
                .is_some_and(|literal| literal.text == format!("{}.empty()", source.name.as_str()))
                && members.len() == source.members.len()
                && members.iter().zip(&source.members).all(|(id, member)| {
                    rir.string_literals.get(id.index()).is_some_and(|literal| {
                        literal.text == format!("{}.{}", source.name.as_str(), member.name.as_str())
                    })
                })
        }
    }
}

fn air_rir_type_matches(
    air_program: &air::Program,
    rir: &RirProgram,
    air_ty: air::TypeId,
    rir_ty: RirTypeId,
    visiting: &mut std::collections::BTreeSet<(air::TypeId, RirTypeId)>,
) -> bool {
    if !visiting.insert((air_ty, rir_ty)) {
        return true;
    }
    let matches =
        match (
            air_program.type_arena.data(air_ty),
            rir.types.get(rir_ty.index()),
        ) {
            (air::TypeData::Int, Some(RirType::Int))
            | (air::TypeData::Float, Some(RirType::Float))
            | (air::TypeData::Bool, Some(RirType::Bool))
            | (air::TypeData::String, Some(RirType::String))
            | (air::TypeData::Char, Some(RirType::Char))
            | (air::TypeData::Void, Some(RirType::Void))
            | (air::TypeData::Dyn(_), Some(RirType::Enum(_))) => true,
            (air::TypeData::Optional(inner), Some(RirType::Option(rir_inner)))
            | (air::TypeData::List(inner), Some(RirType::List(rir_inner)))
            | (air::TypeData::Slice(inner), Some(RirType::Slice(rir_inner))) => {
                air_rir_type_matches(air_program, rir, *inner, *rir_inner, visiting)
            }
            (
                air::TypeData::Array { elem, len },
                Some(RirType::Array {
                    elem: rir_inner,
                    len: rir_len,
                }),
            ) => {
                u64::try_from(*len).ok() == Some(*rir_len)
                    && air_rir_type_matches(air_program, rir, *elem, *rir_inner, visiting)
            }
            (
                air::TypeData::Map { key, value, .. },
                Some(RirType::Map {
                    key: rir_key,
                    value: rir_value,
                }),
            ) => {
                air_rir_type_matches(air_program, rir, *key, *rir_key, visiting)
                    && air_rir_type_matches(air_program, rir, *value, *rir_value, visiting)
            }
            (air::TypeData::Aggregate(air_id), Some(RirType::Struct(rir_id))) => rir
                .structs
                .get(rir_id.index())
                .is_some_and(|decl| decl.air_id == Some(*air_id)),
            (air::TypeData::DataRef(air_id), Some(RirType::DataRef(rir_id))) => rir
                .datarefs
                .get(rir_id.index())
                .is_some_and(|decl| decl.air_id == *air_id),
            (air::TypeData::Enum(air_id), Some(RirType::Enum(rir_id))) => rir
                .enums
                .get(rir_id.index())
                .is_some_and(|decl| decl.air_id == Some(*air_id)),
            (air::TypeData::Flag(air_id), Some(RirType::Flag(rir_id))) => rir
                .flags
                .get(rir_id.index())
                .is_some_and(|decl| decl.air_id == *air_id),
            (air::TypeData::Tuple(air_fields), Some(RirType::Tuple(rir_id))) => {
                rir.tuples.get(rir_id.index()).is_some_and(|decl| {
                    decl.fields.len() == air_fields.len()
                        && air_fields.iter().zip(&decl.fields).all(|(air_ty, field)| {
                            air_rir_type_matches(air_program, rir, *air_ty, field.ty, visiting)
                        })
                })
            }
            (air::TypeData::Function(air_sig), Some(RirType::Lambda(rir_id))) => {
                rir.lambda_sigs.get(rir_id.index()).is_some_and(|rir_sig| {
                    air_sig.params.len() == rir_sig.params.len()
                        && air_sig.params.iter().zip(&rir_sig.params).all(
                            |(air_param, rir_param)| {
                                source_param_semantic(air_program, air_param.ty, air_param.mode)
                                    == rir_param.semantic
                                    && matches!(
                                        (air_param.escape, rir_param.escape),
                                        (air::ParamEscape::Escaping, RirParamEscape::Escaping)
                                            | (
                                                air::ParamEscape::NonEscaping,
                                                RirParamEscape::NonEscaping
                                            )
                                    )
                                    && air_rir_type_matches(
                                        air_program,
                                        rir,
                                        air_param.ty,
                                        rir_param.ty,
                                        visiting,
                                    )
                            },
                        )
                        && air_rir_type_matches(
                            air_program,
                            rir,
                            air_sig.ret.ty(),
                            rir_sig.ret,
                            visiting,
                        )
                })
            }
            (air::TypeData::Extern(air_id), Some(RirType::Struct(rir_id))) => {
                let air_decl = air_program.extern_type(*air_id);
                rir.structs.get(rir_id.index()).is_some_and(|rir_decl| {
                    rir_decl.native_key.is_some()
                        && rir_decl.native_key
                            == air_decl.binding.as_ref().map(|binding| binding.key.clone())
                })
            }
            (air::TypeData::Extern(air_id), Some(RirType::Enum(rir_id))) => {
                let air_decl = air_program.extern_type(*air_id);
                rir.enums.get(rir_id.index()).is_some_and(|rir_decl| {
                    rir_decl.native_key.is_some()
                        && rir_decl.native_key
                            == air_decl.binding.as_ref().map(|binding| binding.key.clone())
                        && extern_variants_match(
                            air_program,
                            rir,
                            &air_decl.variants,
                            &rir_decl.variants,
                            visiting,
                        )
                })
            }
            _ => false,
        };
    visiting.remove(&(air_ty, rir_ty));
    matches
}

fn extern_variants_match(
    air_program: &air::Program,
    rir_program: &RirProgram,
    air_variants: &[air::VariantDecl],
    rir_variants: &[RirVariant],
    visiting: &mut std::collections::BTreeSet<(air::TypeId, RirTypeId)>,
) -> bool {
    air_variants.len() == rir_variants.len()
        && air_variants.iter().zip(rir_variants).all(|(air, rir)| {
            let (kind, fields) = match &air.shape {
                air::VariantShape::Unit => (RirVariantKind::Unit, vec![]),
                air::VariantShape::Tuple(fields) => (RirVariantKind::Tuple, fields.clone()),
                air::VariantShape::Struct(fields) => (
                    RirVariantKind::Struct,
                    fields.iter().map(|field| field.ty).collect(),
                ),
            };
            rir.display.as_str() == air.name.as_str()
                && rir.kind == kind
                && fields.len() == rir.fields.len()
                && fields.iter().zip(&rir.fields).all(|(air, rir)| {
                    air_rir_type_matches(air_program, rir_program, *air, rir.ty, visiting)
                })
        })
}

fn verify_moved_value_origins(
    program: &RirProgram,
    air: &air::Program,
    errors: &mut Vec<RirVerifyError>,
) {
    fn visit(
        function: &RirFunction,
        air_program: &air::Program,
        air_function: Option<&air::Function>,
        block: &RirStructuredBlock,
        uses: &[usize],
        errors: &mut Vec<RirVerifyError>,
    ) {
        for stmt in &block.stmts {
            let value = match stmt {
                RirStmt::Init { value, .. }
                | RirStmt::GlobalSetRoot { value, .. }
                | RirStmt::GlobalUpdateRoot { value, .. }
                | RirStmt::MutPlaceSet { value, .. }
                | RirStmt::Assign { value, .. }
                | RirStmt::CellInit { value, .. }
                | RirStmt::CellSet { value, .. }
                | RirStmt::ScopedPlaceCellSet { value, .. }
                | RirStmt::Eval(value) => Some(value),
                _ => None,
            };
            let air_local = match value {
                Some(RirRValue::MoveValue { air_local, .. }) => Some(*air_local),
                Some(
                    RirRValue::DynPack {
                        air_use: air::DynOwnedUse::ConsumeTemporary,
                        air_local,
                        ..
                    }
                    | RirRValue::DynWeaken {
                        air_use: air::DynOwnedUse::ConsumeTemporary,
                        air_local,
                        ..
                    },
                ) => *air_local,
                Some(
                    RirRValue::DynPack {
                        air_use: air::DynOwnedUse::ReusableRead,
                        air_local: Some(_),
                        ..
                    }
                    | RirRValue::DynWeaken {
                        air_use: air::DynOwnedUse::ReusableRead,
                        air_local: Some(_),
                        ..
                    },
                ) => {
                    errors.push(RirVerifyError {
                        site: RirVerifySite::Function(function.id),
                        kind: RirVerifyErrorKind::InvalidDynOrigin,
                    });
                    None
                }
                _ => None,
            };
            if let Some(air_local) = air_local {
                let valid = air_function
                    .and_then(|function| function.locals.get(air_local.index()))
                    .is_some_and(|local| {
                        local.binding.is_none()
                            && local.kind == air::LocalKind::Temp
                            && uses.get(air_local.index()) == Some(&1)
                    });
                if !valid {
                    errors.push(RirVerifyError {
                        site: RirVerifySite::Function(function.id),
                        kind: RirVerifyErrorKind::InvalidDynOrigin,
                    });
                }
            }
        }
        block.for_each_child(&mut |child| match child {
            RirChild::CallArg(RirCallArg::MovedValue { air_local, .. }) => {
                let valid = air_function
                    .and_then(|function| function.locals.get(air_local.index()))
                    .is_some_and(|local| {
                        local.binding.is_none()
                            && local.kind == air::LocalKind::Temp
                            && matches!(
                                air_program.type_arena.data(local.ty),
                                air::TypeData::Dyn(_)
                            )
                            && uses.get(air_local.index()) == Some(&1)
                    });
                if !valid {
                    errors.push(RirVerifyError {
                        site: RirVerifySite::Function(function.id),
                        kind: RirVerifyErrorKind::InvalidDynOrigin,
                    });
                }
            }
            RirChild::Block(child) => {
                visit(function, air_program, air_function, child, uses, errors);
            }
            _ => {}
        });
    }

    fn count_uses(block: &RirStructuredBlock, uses: &mut [usize]) {
        block.for_each_child(&mut |child| {
            if let RirChild::Block(child) = child {
                count_uses(child, uses);
                return;
            }
            for (index, count) in uses.iter_mut().enumerate() {
                let local = RirLocalId::from_index(index);
                let used = match child {
                    RirChild::Operand {
                        operand: RirOperand::Place(place),
                        ..
                    }
                    | RirChild::Place { place, .. }
                    | RirChild::CaptureArg(
                        RirLambdaCaptureArg::Readonly {
                            value: RirOperand::Place(place),
                        }
                        | RirLambdaCaptureArg::Scoped { place },
                    ) => place.uses_local(local),
                    RirChild::MutPlace { place, .. } => place.uses_local(local),
                    RirChild::Collection { collection, .. } => collection.uses_local(local),
                    RirChild::LocalRead(found) => found == local,
                    RirChild::CallArg(arg) => call_arg_uses_local(arg, local),
                    RirChild::CaptureArg(
                        RirLambdaCaptureArg::StackCell {
                            cell: RirCellRef::Capture { local: found, .. },
                        }
                        | RirLambdaCaptureArg::HeapCell {
                            cell: RirCellRef::Capture { local: found, .. },
                        }
                        | RirLambdaCaptureArg::ScopedPlaceCell {
                            cell: RirScopedPlaceCellRef::Capture { local: found, .. },
                        },
                    ) => *found == local,
                    _ => false,
                };
                if used {
                    *count += 1;
                }
            }
        });
    }

    fn call_arg_uses_local(arg: &RirCallArg, local: RirLocalId) -> bool {
        match arg {
            RirCallArg::Value(operand)
            | RirCallArg::MovedValue { value: operand, .. }
            | RirCallArg::InitFieldProvided(operand)
            | RirCallArg::ScopedLambda {
                callee: operand, ..
            }
            | RirCallArg::EscapingLambda {
                callee: operand, ..
            }
            | RirCallArg::AnvCallback {
                callee: operand, ..
            } => {
                matches!(operand, RirOperand::Place(place) if place.uses_local(local))
            }
            RirCallArg::SharedBorrow(place) | RirCallArg::MutBorrow(place) => {
                place.uses_local(local)
            }
            RirCallArg::MutPlace(place) => place.uses_local(local),
            RirCallArg::DynBorrow(borrow) => match &borrow.source {
                RirDynBorrowSource::Concrete { place, .. }
                | RirDynBorrowSource::Owned { place, .. } => place.uses_local(local),
                RirDynBorrowSource::Borrowed { local: found, .. }
                | RirDynBorrowSource::Reborrowed { local: found, .. } => *found == local,
            },
            RirCallArg::InitFieldOmitted | RirCallArg::SharedStringConst(_) => false,
        }
    }

    for function in &program.functions {
        let air_function = function
            .air_id
            .and_then(|air_id| air.functions.get(air_id.index()));
        let mut uses = vec![0; function.locals.len()];
        count_uses(&function.body, &mut uses);
        visit(function, air, air_function, &function.body, &uses, errors);
    }
}

fn verify_dynamic_weakenings(
    program: &RirProgram,
    air: &air::Program,
    errors: &mut Vec<RirVerifyError>,
) {
    let mut found = std::collections::BTreeSet::new();
    for weakening in &program.dyn_weakenings {
        let valid = found.insert(weakening.air_id)
            && program
                .dyn_origins
                .weakenings
                .iter()
                .filter(|origin| origin.air_weakening == weakening.air_id)
                .count()
                == 1
            && air
                .contract_weakenings
                .get(weakening.air_id.index())
                .is_some_and(|origin| {
                    let Some(source) = program.dyn_carriers.get(weakening.source.index()) else {
                        return false;
                    };
                    let Some(target) = program.dyn_carriers.get(weakening.target.index()) else {
                        return false;
                    };
                    origin.source == source.air_surface
                        && origin.target == target.air_surface
                        && weakening.arms.len() == source.variants.len()
                        && weakening.arms.iter().enumerate().all(|(index, arm)| {
                            let Some(source_variant) = source.variants.get(arm.source.index())
                            else {
                                return false;
                            };
                            let Some(target_variant) = target.variants.get(arm.target.index())
                            else {
                                return false;
                            };
                            arm.source.index() == index
                                && source_variant.concrete_ty == target_variant.concrete_ty
                                && source_variant.storage == target_variant.storage
                                && weakening_arm_matches_air(
                                    air,
                                    source_variant.air_witness,
                                    target_variant.air_witness,
                                    origin,
                                )
                        })
                });
        if !valid {
            errors.push(RirVerifyError {
                site: RirVerifySite::Program,
                kind: RirVerifyErrorKind::InvalidDynOrigin,
            });
        }
    }
    let expected = program
        .dyn_origins
        .weakenings
        .iter()
        .map(|origin| origin.air_weakening)
        .collect::<std::collections::BTreeSet<_>>();
    if expected.len() != program.dyn_origins.weakenings.len() || found != expected {
        errors.push(RirVerifyError {
            site: RirVerifySite::Program,
            kind: RirVerifyErrorKind::InvalidDynOrigin,
        });
    }
}

fn verify_dynamic_dispatch_origins(
    program: &RirProgram,
    air: &air::Program,
    errors: &mut Vec<RirVerifyError>,
) {
    fn visit_block(
        program: &RirProgram,
        air: &air::Program,
        block: &RirStructuredBlock,
        errors: &mut Vec<RirVerifyError>,
    ) {
        for stmt in &block.stmts {
            let value = match stmt {
                RirStmt::Init { value, .. }
                | RirStmt::GlobalSetRoot { value, .. }
                | RirStmt::GlobalUpdateRoot { value, .. }
                | RirStmt::MutPlaceSet { value, .. }
                | RirStmt::Assign { value, .. }
                | RirStmt::CellInit { value, .. }
                | RirStmt::CellSet { value, .. }
                | RirStmt::ScopedPlaceCellSet { value, .. }
                | RirStmt::Eval(value) => Some(value),
                _ => None,
            };
            if let Some(RirRValue::DynWeaken {
                source,
                target,
                air_weakening,
                arms,
                ..
            }) = value
            {
                let valid = match (
                    program.dyn_carriers.get(source.index()),
                    program.dyn_carriers.get(target.index()),
                    air.contract_weakenings.get(air_weakening.index()),
                ) {
                    (Some(source), Some(target), Some(weakening)) => arms.iter().all(|arm| {
                        let source = source
                            .variants
                            .iter()
                            .find(|variant| variant.id == arm.source);
                        let target = target
                            .variants
                            .iter()
                            .find(|variant| variant.id == arm.target);
                        match (source, target) {
                            (Some(source), Some(target)) => weakening_arm_matches_air(
                                air,
                                source.air_witness,
                                target.air_witness,
                                weakening,
                            ),
                            _ => false,
                        }
                    }),
                    _ => false,
                };
                if !valid {
                    errors.push(RirVerifyError {
                        site: RirVerifySite::Program,
                        kind: RirVerifyErrorKind::InvalidDynOrigin,
                    });
                }
            }
            if let Some(RirRValue::DynCall {
                carrier,
                air_slot,
                arms,
                ..
            }) = value
            {
                let valid = program
                    .dyn_carriers
                    .get(carrier.index())
                    .is_some_and(|carrier| {
                        arms.iter().all(|arm| {
                            let Some(variant) = carrier
                                .variants
                                .iter()
                                .find(|variant| variant.id == arm.variant)
                            else {
                                return false;
                            };
                            let Some(witness) =
                                air.contract_witnesses.get(variant.air_witness.index())
                            else {
                                return false;
                            };
                            witness
                                .key
                                .slots
                                .iter()
                                .find(|slot| slot.slot == *air_slot)
                                .is_some_and(|slot| {
                                    dyn_target_matches_air(
                                        program,
                                        air,
                                        variant.concrete_ty,
                                        witness.key.concrete_ty,
                                        &arm.target,
                                        &slot.target,
                                    )
                                })
                        })
                    });
                if !valid {
                    errors.push(RirVerifyError {
                        site: RirVerifySite::Program,
                        kind: RirVerifyErrorKind::InvalidDynOrigin,
                    });
                }
            }
            stmt.for_each_child(&mut |child| {
                if let RirChild::Block(child) = child {
                    visit_block(program, air, child, errors);
                }
            });
        }
    }
    for function in &program.functions {
        visit_block(program, air, &function.body, errors);
    }
}

fn dyn_target_matches_air(
    program: &RirProgram,
    air_program: &air::Program,
    rir_ty: RirTypeId,
    air_ty: air::TypeId,
    rir: &RirResolvedCallTarget,
    air: &air::ContractWitnessTarget,
) -> bool {
    match (rir, air) {
        (
            RirResolvedCallTarget::Function(rir),
            air::ContractWitnessTarget::Function { function }
            | air::ContractWitnessTarget::IteratorFunction { function },
        ) => program
            .functions
            .get(rir.index())
            .is_some_and(|rir| rir.air_id == Some(*function)),
        (RirResolvedCallTarget::Extern(rir), air::ContractWitnessTarget::Extern { function }) => {
            program
                .externs
                .get(rir.index())
                .is_some_and(|rir| rir.air_id == Some(*function))
        }
        (
            RirResolvedCallTarget::Promoted {
                fields: rir_fields,
                target: rir_target,
            },
            air::ContractWitnessTarget::Promoted {
                fields: air_fields,
                target: air_target,
            },
        ) => {
            if rir_fields.len() != air_fields.len() {
                return false;
            }
            let mut rir_ty = rir_ty;
            let mut air_ty = air_ty;
            for (rir_field, air_field) in rir_fields.iter().zip(air_fields) {
                let (Some(RirType::Struct(rir_struct)), air::TypeData::Aggregate(air_aggregate)) = (
                    program.types.get(rir_ty.index()),
                    air_program.type_arena.data(air_ty),
                ) else {
                    return false;
                };
                let Some(rir_decl) = program.structs.get(rir_struct.index()) else {
                    return false;
                };
                if rir_decl.air_id != Some(*air_aggregate) || rir_field.index() != air_field.index()
                {
                    return false;
                }
                let (Some(rir_field), Some(air_field)) = (
                    rir_decl.fields.get(rir_field.index()),
                    air_program
                        .aggregate(*air_aggregate)
                        .fields
                        .get(air_field.index()),
                ) else {
                    return false;
                };
                rir_ty = rir_field.ty;
                air_ty = air_field.ty;
            }
            dyn_target_matches_air(program, air_program, rir_ty, air_ty, rir_target, air_target)
        }
        _ => false,
    }
}

fn weakening_arm_matches_air(
    air: &air::Program,
    source: air::ContractWitnessId,
    target: air::ContractWitnessId,
    weakening: &air::ContractWeakeningDecl,
) -> bool {
    let (Some(source), Some(target)) = (
        air.contract_witnesses.get(source.index()),
        air.contract_witnesses.get(target.index()),
    ) else {
        return false;
    };
    source.key.surface == weakening.source
        && target.key.surface == weakening.target
        && source.key.concrete_ty == target.key.concrete_ty
        && target.key.slots.len() == weakening.target_to_source.len()
        && target
            .key
            .slots
            .iter()
            .zip(&weakening.target_to_source)
            .all(|(target, source_slot)| {
                source
                    .key
                    .slots
                    .iter()
                    .find(|source| source.slot == *source_slot)
                    .is_some_and(|source| source.target == target.target)
            })
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RirVerifyError {
    pub site: RirVerifySite,
    pub kind: RirVerifyErrorKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RirVerifySite {
    Program,
    Context,
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
            Self::ReadCopy | Self::CallArg | Self::MapRead | Self::IndexedMapAssignment
        )
    }

    fn allow_cell_collection_projection(self) -> bool {
        matches!(self, Self::ReadCopy)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum RirPlaceError {
    BadId,
    Unsupported,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) struct RirProjectionPath {
    ty: RirTypeId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) struct RirDataRefStoragePath {
    ty: RirTypeId,
    steps: Vec<RirDataRefStorageStep>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) struct RirDataRefStorageStep {
    pub(super) projection: RirProjection,
    pub(super) symbol: RirSymbol,
    pub(super) ty: RirTypeId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct RirProjectionStep {
    ty: RirTypeId,
    kind: RirProjectionKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum RirProjectionKind {
    Field,
    TupleField,
    SequenceIndex { local: RirLocalId },
}

pub(super) struct RirPlaceModel<'a> {
    program: &'a RirProgram,
}

impl RirProjectionPath {
    pub(super) fn ty(&self) -> RirTypeId {
        self.ty
    }
}

impl RirDataRefStoragePath {
    pub(super) fn ty(&self) -> RirTypeId {
        self.ty
    }

    pub(super) fn steps(&self) -> &[RirDataRefStorageStep] {
        &self.steps
    }
}

impl<'a> RirPlaceModel<'a> {
    pub(super) fn new(program: &'a RirProgram) -> Self {
        Self { program }
    }

    pub(super) fn dataref_storage_path(
        &self,
        dataref_id: RirDataRefId,
        projections: &[RirProjection],
    ) -> Result<RirDataRefStoragePath, RirPlaceError> {
        let Some((first, rest)) = projections.split_first() else {
            return Err(RirPlaceError::Unsupported);
        };
        let RirProjection::Field(field_id) = first else {
            return Err(RirPlaceError::Unsupported);
        };
        let field = self
            .program
            .datarefs
            .get(dataref_id.index())
            .and_then(|dataref| dataref.fields.get(field_id.index()))
            .ok_or(RirPlaceError::BadId)?;
        let mut ty = field.ty;
        let mut steps = vec![RirDataRefStorageStep {
            projection: *first,
            symbol: field.symbol.clone(),
            ty,
        }];
        for (index, projection) in rest.iter().enumerate() {
            if matches!(self.ty(ty), Some(RirType::DataRef(_))) {
                return Err(RirPlaceError::Unsupported);
            }
            let step = self.dataref_storage_step(ty, *projection)?;
            ty = step.ty;
            steps.push(step);
            if index + 1 < rest.len() && matches!(self.ty(ty), Some(RirType::DataRef(_))) {
                return Err(RirPlaceError::Unsupported);
            }
        }
        Ok(RirDataRefStoragePath { ty, steps })
    }

    pub(super) fn projection_path(
        &self,
        root_ty: RirTypeId,
        projections: &[RirProjection],
    ) -> Option<RirProjectionPath> {
        let mut ty = root_ty;
        for projection in projections {
            ty = self.step(ty, *projection, true).ok()?.ty;
        }
        Some(RirProjectionPath { ty })
    }

    pub(super) fn collection_loan_projection_supported(
        &self,
        root_ty: RirTypeId,
        projections: &[RirProjection],
    ) -> bool {
        let mut ty = root_ty;
        for projection in projections {
            if !collection_loan_step_supported(
                rir_collection_loan_base(self.ty(ty)),
                rir_collection_loan_projection(*projection),
            ) {
                return false;
            }
            let Ok(step) = self.step(ty, *projection, true) else {
                return false;
            };
            ty = step.ty;
        }
        true
    }

    pub(super) fn projection_dynamic_facts(
        &self,
        root_ty: RirTypeId,
        projections: &[RirProjection],
    ) -> Option<bool> {
        let mut ty = root_ty;
        let mut fallible = false;
        for projection in projections {
            if matches!(
                (self.ty(ty), projection),
                (
                    Some(RirType::Array { .. } | RirType::List(_) | RirType::Slice(_)),
                    RirProjection::Index(_),
                )
            ) {
                fallible = true;
            }
            let Ok(step) = self.step(ty, *projection, true) else {
                return None;
            };
            ty = step.ty;
        }
        Some(fallible)
    }

    fn mut_place_supported(
        &self,
        root_ty: RirTypeId,
        projections: &[RirProjection],
        allow_collections: bool,
    ) -> bool {
        let mut ty = root_ty;
        let mut slice_dynamic = false;
        for projection in projections {
            let Ok(step) = self.step(ty, *projection, true) else {
                return false;
            };
            let collection = allow_collections
                && !slice_dynamic
                && matches!(
                    (self.ty(ty), projection),
                    (Some(RirType::List(_)), RirProjection::Index(_))
                );
            let slice = allow_collections
                && matches!(
                    (self.ty(ty), projection),
                    (Some(RirType::Slice(_)), RirProjection::Index(_))
                );
            let static_value = matches!(
                (self.ty(ty), projection),
                (
                    Some(RirType::Struct(_) | RirType::Tuple(_) | RirType::Array { .. }),
                    RirProjection::Field(_)
                        | RirProjection::TupleField(_)
                        | RirProjection::Index(_),
                )
            );
            if !(static_value || collection || slice) {
                return false;
            }
            slice_dynamic |= slice;
            ty = step.ty;
        }
        true
    }

    fn dataref_mut_place_payload_supported(&self, ty: RirTypeId) -> bool {
        !matches!(
            RirRustRepPolicy::new(self.program).materialization_for(
                ty,
                RustMaterialSource::DataRefMutPlace,
                RustMaterialIntent::MutPlacePayload,
            ),
            RustMaterialization::Gap
        )
    }

    fn dataref_storage_step(
        &self,
        ty: RirTypeId,
        projection: RirProjection,
    ) -> Result<RirDataRefStorageStep, RirPlaceError> {
        match (self.ty(ty), projection) {
            (Some(RirType::Struct(id)), RirProjection::Field(field)) => {
                let field = self
                    .program
                    .structs
                    .get(id.index())
                    .and_then(|strukt| strukt.fields.get(field.index()))
                    .ok_or(RirPlaceError::BadId)?;
                Ok(RirDataRefStorageStep {
                    projection,
                    symbol: field.symbol.clone(),
                    ty: field.ty,
                })
            }
            (Some(RirType::Tuple(id)), RirProjection::TupleField(field)) => {
                let field = self
                    .program
                    .tuples
                    .get(id.index())
                    .and_then(|tuple| tuple.fields.get(field.index()))
                    .ok_or(RirPlaceError::BadId)?;
                Ok(RirDataRefStorageStep {
                    projection,
                    symbol: field.symbol.clone(),
                    ty: field.ty,
                })
            }
            _ => Err(RirPlaceError::Unsupported),
        }
    }

    fn step(
        &self,
        ty: RirTypeId,
        projection: RirProjection,
        allow_collections: bool,
    ) -> Result<RirProjectionStep, RirPlaceError> {
        let (next, kind) = match (self.ty(ty), projection) {
            (Some(RirType::Struct(id)), RirProjection::Field(field)) => {
                let field = self
                    .program
                    .structs
                    .get(id.index())
                    .and_then(|strukt| strukt.fields.get(field.index()))
                    .ok_or(RirPlaceError::BadId)?;
                (field.ty, RirProjectionKind::Field)
            }
            (Some(RirType::Tuple(id)), RirProjection::TupleField(field)) => {
                let field = self
                    .program
                    .tuples
                    .get(id.index())
                    .and_then(|tuple| tuple.fields.get(field.index()))
                    .ok_or(RirPlaceError::BadId)?;
                (field.ty, RirProjectionKind::TupleField)
            }
            (
                Some(
                    RirType::Array { elem: next, .. } | RirType::List(next) | RirType::Slice(next),
                ),
                RirProjection::Index(local),
            ) => {
                if !allow_collections && !matches!(self.ty(ty), Some(RirType::Array { .. })) {
                    return Err(RirPlaceError::Unsupported);
                }
                (next, RirProjectionKind::SequenceIndex { local })
            }
            _ => return Err(RirPlaceError::Unsupported),
        };
        Ok(RirProjectionStep { ty: next, kind })
    }

    fn ty(&self, id: RirTypeId) -> Option<RirType> {
        self.program.types.get(id.index()).copied()
    }
}

fn rir_collection_loan_base(ty: Option<RirType>) -> CollectionLoanBase {
    match ty {
        Some(RirType::Struct(_)) => CollectionLoanBase::Aggregate,
        Some(RirType::Tuple(_)) => CollectionLoanBase::Tuple,
        Some(RirType::Array { .. }) => CollectionLoanBase::Array,
        _ => CollectionLoanBase::Other,
    }
}

fn rir_collection_loan_projection(projection: RirProjection) -> CollectionLoanProjection {
    match projection {
        RirProjection::Field(_) => CollectionLoanProjection::Field,
        RirProjection::TupleField(_) => CollectionLoanProjection::TupleField,
        RirProjection::Index(_) => CollectionLoanProjection::Index,
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
    InvalidDynOrigin,
    InvalidAirMetadata,
    InvalidDynCarrier,
    InvalidDynVariant,
    InvalidDynStorage,
    InvalidDynDispatch,
    InvalidExactWitness,
    UnsupportedLambdaCapture,
    UnsupportedLambdaCell,
    ImmutableCellSourceLocal(RirLocalId),
    CellSourceLocalPayloadRef(RirLocalId),
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
    DuplicateMatchArm,
    MatchNotExhaustive,
    PatternAlternativeRequired,
    PatternBindingMismatch,
    PatternPayloadWithoutVariantTest,
    OptionPayloadEscapeRequiresPayload,
    OptionPayloadEscapeRequiresRef,
    OptionPayloadEscapeNoneMustDiverge,
    OptionPayloadRefLocalMismatch,
    OptionPayloadRefDiscriminantMustBeMutable,
    OptionPayloadRefWithoutOwner,
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
            _ => Self::Unknown,
        }
    }

    fn escape(&self) -> Option<RirLambdaEscape> {
        match self {
            Self::Lambda(escape) => *escape,
            Self::Unknown | Self::NonFunction | Self::Fields(_) => None,
        }
    }

    fn project(&self, projection: RirProjection) -> Self {
        let Self::Fields(fields) = self else {
            return Self::Unknown;
        };
        match projection {
            RirProjection::Field(field) | RirProjection::TupleField(field) => {
                fields.get(field.index()).cloned().unwrap_or(Self::Unknown)
            }
            RirProjection::Index(_) => fields
                .iter()
                .cloned()
                .reduce(Self::join)
                .unwrap_or(Self::Unknown),
        }
    }

    fn projection_mut(&mut self, projections: &[RirProjection]) -> Option<&mut Self> {
        let Some((first, rest)) = projections.split_first() else {
            return Some(self);
        };
        let Self::Fields(fields) = self else {
            return None;
        };
        let index = match first {
            RirProjection::Field(field) | RirProjection::TupleField(field) => field.index(),
            RirProjection::Index(_) => return None,
        };
        fields.get_mut(index)?.projection_mut(rest)
    }

    fn assign_projection(&mut self, projections: &[RirProjection], value: Self) -> bool {
        let Some(slot) = self.projection_mut(projections) else {
            return false;
        };
        *slot = value;
        true
    }

    fn push_projection(&mut self, projections: &[RirProjection], value: Self) -> bool {
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

struct VerifyCx<'a> {
    program: &'a RirProgram,
    errors: Vec<RirVerifyError>,
    initialized: Vec<bool>,
    possibly_initialized: Vec<bool>,
    payload_ref_owned: Vec<bool>,
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

fn native_extern_signature_ok(
    program: &RirProgram,
    native: &RirNativeExtern,
    ext: &RirExtern,
    void: Option<RirTypeId>,
) -> bool {
    !native.path.is_empty()
        && native.path.iter().all(|segment| !segment.is_empty())
        && rust_extern_abi_supported_with_receiver(&native.abi, native.callback_receiver)
        && native.abi.params.len() == ext.params.len()
        && ext.abi.params.len() == ext.params.len()
        && native::rust_abi_matches_air(&native.abi.params, &native.abi.ret, &ext.abi)
        && program
            .native_call_plan(ext.id)
            .matches_signature(&ext.params)
        && native_callback_receiver_ok(program, native, ext)
        && native_hidden_ctx_borrows_ok(program, &native.abi, ext)
        && ext
            .abi
            .params
            .iter()
            .zip(&ext.params)
            .all(|(abi, param)| extern_param_abi_ok(program, abi, *param))
        && extern_return_abi_ok(program, &ext.abi.ret, ext.ret, void)
}

fn native_callback_receiver_ok(
    program: &RirProgram,
    native: &RirNativeExtern,
    ext: &RirExtern,
) -> bool {
    if !native.abi.has_callback_wrapper() {
        return true;
    }
    let Some(receiver) = native.callback_receiver else {
        return true;
    };
    native
        .abi
        .params
        .get(receiver)
        .is_some_and(|param| matches!(param, RustParamAbi::Borrow(_) | RustParamAbi::MutBorrow(_)))
        && ext
            .params
            .get(receiver)
            .is_some_and(|param| native_ty_is_resource_ref(program, param.ty))
}

fn native_hidden_ctx_borrows_ok(
    program: &RirProgram,
    abi: &RustExternAbi,
    ext: &RirExtern,
) -> bool {
    !native_call::NativeHiddenCtxPlan::from_abi(abi.ctx).borrows_runtime()
        || abi.params.iter().zip(&ext.params).all(|(abi, param)| {
            !matches!(abi, RustParamAbi::Borrow(_) | RustParamAbi::MutBorrow(_))
                || !native_ty_is_resource_ref(program, param.ty)
        })
}

pub(super) fn rust_extern_abi_supported_with_receiver(
    abi: &RustExternAbi,
    receiver: Option<usize>,
) -> bool {
    match abi.support {
        anvyx_runtime::RustAbiSupport::Direct => abi.backend_supported(),
        anvyx_runtime::RustAbiSupport::NeedsWrapperConversion => {
            abi.supported_callback_wrapper_with_receiver(receiver)
        }
        anvyx_runtime::RustAbiSupport::Unsupported => false,
    }
}

pub(super) fn native_return_adopts_resource(
    program: &RirProgram,
    ret: RirTypeId,
    abi: &anvyx_runtime::RustReturnAbi,
) -> bool {
    match abi {
        anvyx_runtime::RustReturnAbi::OwnedNamed(_) => native_ty_is_resource_ref(program, ret),
        anvyx_runtime::RustReturnAbi::Option(inner) => {
            let RirType::Option(inner_ty) = program.types[ret.index()] else {
                return false;
            };
            native_return_adopts_resource(program, inner_ty, inner)
        }
        anvyx_runtime::RustReturnAbi::Result(ok, err) => {
            let RirType::Enum(enum_id) = program.types[ret.index()] else {
                return false;
            };
            let [ok_variant, err_variant] = program.enums[enum_id.index()].variants.as_slice()
            else {
                return false;
            };
            native_return_adopts_resource(program, ok_variant.fields[0].ty, ok)
                || native_return_adopts_resource(program, err_variant.fields[0].ty, err)
        }
        anvyx_runtime::RustReturnAbi::Void | anvyx_runtime::RustReturnAbi::Value(_) => false,
    }
}

pub(super) fn native_dynamic_arg_facts(
    program: &RirProgram,
    ty: RirTypeId,
    semantic: RirParamSemantic,
) -> native_call::NativeArgFacts {
    native_call::NativeArgFacts::dynamic(
        semantic,
        matches!(program.types[ty.index()], RirType::String),
        native_ty_is_resource_ref(program, ty),
    )
}

pub(super) fn native_arg_facts(
    program: &RirProgram,
    ty: RirTypeId,
    arg: &RirCallArg,
) -> native_call::NativeArgFacts {
    native_call::NativeArgFacts::rir(
        arg,
        matches!(program.types[ty.index()], RirType::String),
        native_ty_is_resource_ref(program, ty),
    )
}

pub(super) fn native_ty_is_resource_ref(program: &RirProgram, ty: RirTypeId) -> bool {
    matches!(
        program.types[ty.index()],
        RirType::Struct(id) if program.structs[id.index()].native_ref
    )
}

fn extern_param_abi_ok(program: &RirProgram, abi: &ExternTypeExpr, param: RirExternParam) -> bool {
    param.escape == extern_param_escape(abi) && rir_type_matches_extern(program, param.ty, abi)
}

fn extern_param_escape(abi: &ExternTypeExpr) -> RirParamEscape {
    match abi {
        ExternTypeExpr::Callback(callback)
            if callback.policy.escape == CallbackEscape::Escaping =>
        {
            RirParamEscape::Escaping
        }
        _ => RirParamEscape::NonEscaping,
    }
}

fn rir_type_matches_callback(
    program: &RirProgram,
    ty: RirTypeId,
    callback: &ExternCallbackSignature,
) -> bool {
    if callback.policy.thread != CallbackThread::SameThread
        || !callback.callback_wrapper_signature_supported()
    {
        return false;
    }
    let Some(RirType::Lambda(sig)) = program.types.get(ty.index()) else {
        return false;
    };
    let sig = &program.lambda_sigs[sig.index()];
    sig.params.len() == callback.params.len()
        && sig
            .params
            .iter()
            .zip(&callback.params)
            .all(|(param, callback)| {
                param.escape == RirParamEscape::NonEscaping
                    && callback.escape == CallbackEscape::NonEscaping
                    && rir_type_matches_extern(program, param.ty, &callback.ty)
            })
        && rir_type_matches_extern(program, sig.ret, &callback.ret)
}

fn extern_return_abi_ok(
    program: &RirProgram,
    abi: &ExternTypeExpr,
    ret: RirTypeId,
    void: Option<RirTypeId>,
) -> bool {
    match abi {
        ExternTypeExpr::Void => Some(ret) == void,
        ExternTypeExpr::Slice(_) | ExternTypeExpr::Callback(_) => false,
        _ => rir_type_matches_extern(program, ret, abi),
    }
}

fn native_key_matches(
    key: Option<&ExternTypeKey>,
    module: Option<&anvyx_runtime::ModulePath>,
    name: &str,
) -> bool {
    key.is_some_and(|key| module.is_none_or(|module| key.module == *module) && key.name == name)
}

fn rir_type_matches_result(
    program: &RirProgram,
    id: RirEnumId,
    ok: &ExternTypeExpr,
    err: &ExternTypeExpr,
) -> bool {
    let enm = &program.enums[id.index()];
    if enm.core != Some(RirCoreEnumKind::Result) {
        return false;
    }
    let [ok_variant, err_variant] = enm.variants.as_slice() else {
        return false;
    };
    rir_result_variant_matches(program, ok_variant, "Ok", ok)
        && rir_result_variant_matches(program, err_variant, "Err", err)
}

fn rir_result_variant_matches(
    program: &RirProgram,
    variant: &RirVariant,
    name: &str,
    expected: &ExternTypeExpr,
) -> bool {
    let [field] = variant.fields.as_slice() else {
        return false;
    };
    variant.display.as_str() == name
        && variant.kind == RirVariantKind::Tuple
        && rir_type_matches_extern(program, field.ty, expected)
}

fn rir_type_matches_extern(program: &RirProgram, id: RirTypeId, expected: &ExternTypeExpr) -> bool {
    let Some(found) = program.types.get(id.index()) else {
        return false;
    };
    match (found, expected) {
        (RirType::Void, ExternTypeExpr::Void)
        | (RirType::Bool, ExternTypeExpr::Bool)
        | (RirType::Int, ExternTypeExpr::Int)
        | (RirType::Float, ExternTypeExpr::Float)
        | (RirType::String, ExternTypeExpr::String)
        | (RirType::Char, ExternTypeExpr::Char) => true,
        (RirType::Tuple(tuple), ExternTypeExpr::Unit) => {
            program.tuples[tuple.index()].fields.is_empty()
        }
        (RirType::Struct(struct_id), ExternTypeExpr::Named { module, name, args }) => {
            args.is_empty()
                && native_key_matches(
                    program.structs[struct_id.index()].native_key.as_ref(),
                    module.as_ref(),
                    name,
                )
        }
        (RirType::DataRef(dataref_id), ExternTypeExpr::Named { module, name, args }) => {
            args.is_empty()
                && native_key_matches(
                    program.datarefs[dataref_id.index()].native_key.as_ref(),
                    module.as_ref(),
                    name,
                )
        }
        (RirType::Enum(enum_id), ExternTypeExpr::Named { module, name, args }) => {
            args.is_empty()
                && native_key_matches(
                    program.enums[enum_id.index()].native_key.as_ref(),
                    module.as_ref(),
                    name,
                )
        }
        (RirType::Enum(enum_id), ExternTypeExpr::Result(ok, err)) => {
            rir_type_matches_result(program, *enum_id, ok, err)
        }
        (RirType::List(elem), ExternTypeExpr::List(expected))
        | (RirType::Option(elem), ExternTypeExpr::Option(expected))
        | (RirType::Slice(elem), ExternTypeExpr::Slice(expected)) => {
            rir_type_matches_extern(program, *elem, expected)
        }
        (
            RirType::Array { elem, len },
            ExternTypeExpr::Array {
                elem: expected,
                len: expected_len,
            },
        ) => len == expected_len && rir_type_matches_extern(program, *elem, expected),
        (RirType::Map { key, value }, ExternTypeExpr::Map(expected_key, expected_value)) => {
            rir_type_matches_extern(program, *key, expected_key)
                && rir_type_matches_extern(program, *value, expected_value)
        }
        (RirType::Lambda(_), ExternTypeExpr::Callback(callback)) => {
            rir_type_matches_callback(program, id, callback)
        }
        (RirType::Tuple(tuple), ExternTypeExpr::Tuple(expected)) => {
            let fields = &program.tuples[tuple.index()].fields;
            fields.len() == expected.len()
                && fields
                    .iter()
                    .zip(expected)
                    .all(|(field, expected)| rir_type_matches_extern(program, field.ty, expected))
        }
        _ => false,
    }
}

impl VerifyCx<'_> {
    fn check(&mut self) {
        if let Some(entry) = self.program.entry {
            self.check_function_id(RirVerifySite::Program, entry);
        }
        if self.program.ctx.statics_symbol.as_str().is_empty()
            || self.program.ctx.globals_symbol.as_str().is_empty()
        {
            self.push(RirVerifySite::Context, RirVerifyErrorKind::BadId);
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
                RirType::Array { elem, .. } => {
                    self.check_type_id(site, *elem);
                    self.check_lambda_container_type_family(
                        site,
                        *elem,
                        LambdaStorageFamily::FixedArrayElement,
                    );
                }
                RirType::List(elem) => {
                    self.check_type_id(site, *elem);
                    self.check_lambda_container_type_family(
                        site,
                        *elem,
                        LambdaStorageFamily::ListElement,
                    );
                }
                RirType::Slice(elem) => {
                    self.check_type_id(site, *elem);
                    self.check_lambda_container_type(site, *elem);
                }
                RirType::Map { key, value } => {
                    self.check_type_id(site, *key);
                    self.check_type_id(site, *value);
                    self.check_lambda_container_type_family(
                        site,
                        *key,
                        LambdaStorageFamily::MapKey,
                    );
                    self.check_lambda_container_type_family(
                        site,
                        *value,
                        LambdaStorageFamily::MapValue,
                    );
                }
                RirType::Option(inner) => {
                    self.check_type_id(site, *inner);
                    self.check_lambda_container_type_family(
                        site,
                        *inner,
                        LambdaStorageFamily::OptionalPayload,
                    );
                }
                RirType::Lambda(sig) => self.check_lambda_sig_id(site, *sig),
                _ => {}
            }
        }
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
        for (index, ext) in self.program.externs.iter().enumerate() {
            let id = RirExternId::from_index(index);
            if ext.id != id {
                self.push(RirVerifySite::Extern(id), RirVerifyErrorKind::BadId);
            }
            self.check_extern(id, ext);
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
        let mut surfaces = std::collections::BTreeSet::new();
        let mut witnesses = std::collections::BTreeSet::new();
        let mut backings = std::collections::BTreeSet::new();
        for (index, carrier) in self.program.dyn_carriers.iter().enumerate() {
            if carrier.id != RirDynCarrierId::from_index(index)
                || !self
                    .program
                    .dyn_origins
                    .surfaces
                    .contains(&carrier.air_surface)
                || !surfaces.insert(carrier.air_surface)
            {
                self.push(site, RirVerifyErrorKind::InvalidDynCarrier);
            }
            self.check_type_id(site, carrier.storage_ty);
            let backing = match self.program.types.get(carrier.storage_ty.index()) {
                Some(RirType::Enum(id))
                    if self.program.enums.get(id.index()).is_some_and(|decl| {
                        decl.air_id.is_none()
                            && decl.native_path.is_none()
                            && decl.native_key.is_none()
                            && decl.core.is_none()
                            && decl.repr == RirEnumRepr::Adt
                    }) =>
                {
                    Some(*id)
                }
                _ => {
                    self.push(site, RirVerifyErrorKind::InvalidDynStorage);
                    None
                }
            };
            if backing.is_some_and(|id| !backings.insert(id)) {
                self.push(site, RirVerifyErrorKind::InvalidDynStorage);
            }
            if backing
                .and_then(|id| self.program.enums.get(id.index()))
                .map_or(0, |decl| decl.variants.len())
                != carrier.variants.len()
            {
                self.push(site, RirVerifyErrorKind::InvalidDynStorage);
            }
            for (variant_index, variant) in carrier.variants.iter().enumerate() {
                if variant.id != RirDynVariantId::from_index(variant_index) {
                    self.push(site, RirVerifyErrorKind::InvalidDynVariant);
                }
                let origin = self
                    .program
                    .dyn_origins
                    .witnesses
                    .iter()
                    .find(|origin| origin.air_witness == variant.air_witness);
                if !witnesses.insert(variant.air_witness)
                    || !origin.is_some_and(|origin| {
                        origin.surface == carrier.air_surface
                            && origin.concrete_ty == variant.concrete_ty
                            && origin.storage == variant.storage
                            && origin.payload == variant.payload
                    })
                {
                    self.push(site, RirVerifyErrorKind::InvalidDynOrigin);
                }
                self.check_type_id(site, variant.concrete_ty);
                let payload_valid = backing
                    .and_then(|id| self.program.enums.get(id.index()))
                    .and_then(|decl| decl.variants.get(variant_index))
                    .is_some_and(|backing| {
                        backing.kind == RirVariantKind::Tuple
                            && backing.fields.len() == 1
                            && backing.fields[0].ty == variant.concrete_ty
                    });
                if !payload_valid {
                    self.push(site, RirVerifyErrorKind::InvalidDynStorage);
                }
            }
            let expected = self
                .program
                .dyn_origins
                .witnesses
                .iter()
                .filter(|origin| origin.surface == carrier.air_surface)
                .count();
            if carrier.variants.len() != expected {
                self.push(site, RirVerifyErrorKind::InvalidDynVariant);
            }
        }
        let dispatches_valid = self.program.dyn_carriers.iter().all(|carrier| {
            let slots = self
                .program
                .dyn_origins
                .slots
                .iter()
                .filter(|slot| slot.surface == carrier.air_surface)
                .collect::<Vec<_>>();
            carrier.variants.iter().all(|variant| {
                slots.iter().all(|slot| {
                    self.program
                        .dyn_origins
                        .dispatches
                        .iter()
                        .filter(|origin| {
                            origin.air_witness == variant.air_witness
                                && origin.air_slot == slot.air_slot
                        })
                        .count()
                        == 1
                })
            })
        });
        let expected_dispatches = self
            .program
            .dyn_carriers
            .iter()
            .map(|carrier| {
                carrier.variants.len()
                    * self
                        .program
                        .dyn_origins
                        .slots
                        .iter()
                        .filter(|slot| slot.surface == carrier.air_surface)
                        .count()
            })
            .sum::<usize>();
        if surfaces.len() != self.program.dyn_origins.surfaces.len()
            || witnesses.len() != self.program.dyn_origins.witnesses.len()
            || !dispatches_valid
            || expected_dispatches != self.program.dyn_origins.dispatches.len()
        {
            self.push(site, RirVerifyErrorKind::InvalidDynOrigin);
        }
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
        matches!(
            RirRustRepPolicy::new(self.program).materialization_for(
                ty,
                RustMaterialSource::ExactGlobalRoot,
                RustMaterialIntent::Read,
            ),
            RustMaterialization::Copy
                | RustMaterialization::Share
                | RustMaterialization::CloneHandle
                | RustMaterialization::CloneLambda
        )
    }

    fn stored_payload_supported(&self, ty: RirTypeId) -> bool {
        !matches!(
            RirRustRepPolicy::new(self.program).materialization_for(
                ty,
                RustMaterialSource::StoredPayload,
                RustMaterialIntent::Store,
            ),
            RustMaterialization::Gap
        )
    }

    fn value_from_ref_supported(&self, ty: RirTypeId) -> bool {
        RirRustRepPolicy::new(self.program).value_from_ref_supported(ty)
    }

    fn check_stored_payload(&mut self, site: RirVerifySite, ty: RirTypeId) {
        self.check_stored_payload_family(site, ty, LambdaStorageFamily::UnknownOrigin);
    }

    fn check_stored_payload_family(
        &mut self,
        site: RirVerifySite,
        ty: RirTypeId,
        family: LambdaStorageFamily,
    ) {
        self.check_type_id(site, ty);
        if self.ty(ty).is_none() {
            return;
        }
        let policy = RirRustRepPolicy::new(self.program);
        let supported = policy.storage_supported(ty, family).is_ok();
        if !supported {
            self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
        }
    }

    fn check_lambda_container_type(&mut self, site: RirVerifySite, ty: RirTypeId) {
        self.check_lambda_container_type_family(site, ty, LambdaStorageFamily::UnknownOrigin);
    }

    fn check_lambda_container_type_family(
        &mut self,
        site: RirVerifySite,
        ty: RirTypeId,
        family: LambdaStorageFamily,
    ) {
        let policy = RirRustRepPolicy::new(self.program);
        if policy.contains_function_payload(ty) && policy.storage_supported(ty, family).is_err() {
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
                self.check_abi(site, param.ty, param.semantic, param.abi);
                if matches!(
                    (param.semantic, param.abi),
                    (
                        RirParamSemantic::ScopedLambda
                            | RirParamSemantic::EscapingLambda
                            | RirParamSemantic::AnvCallback
                            | RirParamSemantic::StackCell
                            | RirParamSemantic::HeapCell
                            | RirParamSemantic::ScopedPlaceCell,
                        _
                    ) | (
                        _,
                        RirParamAbi::ScopedLambda
                            | RirParamAbi::EscapingLambda
                            | RirParamAbi::AnvCallback
                            | RirParamAbi::StackCell
                            | RirParamAbi::HeapCell
                            | RirParamAbi::ScopedPlaceCell,
                    )
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
            self.check_abi(site, capture.ty, capture.semantic, capture.abi);
            match capture.kind {
                RirLambdaCaptureKind::Param => {
                    if heap_env {
                        if capture.semantic != RirParamSemantic::Value
                            || capture.abi != RirParamAbi::Value
                            || (self.ty(capture.ty).is_some()
                                && !self.value_from_ref_supported(capture.ty))
                        {
                            self.push(site, RirVerifyErrorKind::UnsupportedLambdaCapture);
                        }
                    } else {
                        if matches!(
                            (capture.semantic, capture.abi),
                            (
                                RirParamSemantic::MutBorrow
                                    | RirParamSemantic::ScopedLambda
                                    | RirParamSemantic::EscapingLambda
                                    | RirParamSemantic::AnvCallback
                                    | RirParamSemantic::StackCell
                                    | RirParamSemantic::HeapCell
                                    | RirParamSemantic::ScopedPlaceCell,
                                _
                            ) | (
                                _,
                                RirParamAbi::MutBorrow
                                    | RirParamAbi::ScopedLambda
                                    | RirParamAbi::EscapingLambda
                                    | RirParamAbi::AnvCallback
                                    | RirParamAbi::StackCell
                                    | RirParamAbi::HeapCell
                                    | RirParamAbi::ScopedPlaceCell
                            )
                        ) {
                            self.push(site, RirVerifyErrorKind::UnsupportedLambdaCapture);
                        }
                        if capture.semantic == RirParamSemantic::Value
                            && !self.copyable_type(capture.ty)
                        {
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
                    if capture.semantic != RirParamSemantic::ScopedPlaceCell
                        || capture.abi != RirParamAbi::ScopedPlaceCell
                    {
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
        let (semantic, abi) = Self::cell_capture_modes(storage);
        if capture.semantic != semantic || capture.abi != abi {
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

    fn cell_capture_modes(storage: RirCellStorage) -> (RirParamSemantic, RirParamAbi) {
        match storage {
            RirCellStorage::StackScoped => (RirParamSemantic::StackCell, RirParamAbi::StackCell),
            RirCellStorage::Heap => (RirParamSemantic::HeapCell, RirParamAbi::HeapCell),
        }
    }

    fn check_collection_storages(&mut self) {
        let mut seen = HashSet::new();
        for (index, storage) in self.program.collection_storages.iter().enumerate() {
            if storage.id != RirCollectionStorageId::from_index(index) {
                self.push(RirVerifySite::Program, RirVerifyErrorKind::BadId);
            }
            if !seen.insert(storage.value_ty) {
                self.push(RirVerifySite::Program, RirVerifyErrorKind::BadId);
            }
            self.check_type_id(RirVerifySite::Program, storage.value_ty);
            match storage.kind {
                RirCollectionStorageKind::List { elem_ty } => {
                    self.check_type_id(RirVerifySite::Program, elem_ty);
                    match self.ty(storage.value_ty) {
                        Some(RirType::List(elem)) if elem == elem_ty => {}
                        _ => self.push(RirVerifySite::Program, RirVerifyErrorKind::BadId),
                    }
                }
                RirCollectionStorageKind::Map { key_ty, value_ty } => {
                    self.check_type_id(RirVerifySite::Program, key_ty);
                    self.check_type_id(RirVerifySite::Program, value_ty);
                    match self.ty(storage.value_ty) {
                        Some(RirType::Map { key, value }) if key == key_ty && value == value_ty => {
                        }
                        _ => self.push(RirVerifySite::Program, RirVerifyErrorKind::BadId),
                    }
                }
            }
            if self.ty(storage.value_ty).is_some()
                && !self.stored_payload_supported(storage.value_ty)
            {
                self.push(
                    RirVerifySite::Program,
                    RirVerifyErrorKind::UnsupportedRValueType,
                );
            }
        }
        for (index, ty) in self.program.types.iter().enumerate() {
            if matches!(ty, RirType::List(_) | RirType::Map { .. })
                && !seen.contains(&RirTypeId::from_index(index))
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
            Some(local) if local.payload_ref => {
                self.push(
                    site,
                    RirVerifyErrorKind::CellSourceLocalPayloadRef(cell.source_local),
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
        if cell.source.place().ty != cell.payload_ty {
            self.push(
                site,
                RirVerifyErrorKind::TypeMismatch {
                    expected: cell.payload_ty,
                    found: cell.source.place().ty,
                },
            );
        }
        self.check_mut_place_arg(
            site,
            cell.owner,
            owner,
            cell.source.place(),
            MutPlaceUse::ScopedPlaceSource,
        );
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
                if !matches!(
                    &place.access,
                    RirMutPlaceAccess::Handle(RirMutPlaceHandle::Param { .. })
                ) {
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
            if param.ty != capture.ty {
                self.push(
                    site,
                    RirVerifyErrorKind::TypeMismatch {
                        expected: capture.ty,
                        found: param.ty,
                    },
                );
            }
            if param.semantic != capture.semantic {
                self.push(site, RirVerifyErrorKind::CallArgMode);
            }
            if param.abi != capture.abi {
                self.push(site, RirVerifyErrorKind::UnsupportedAbi);
            }
            if param.escape != RirParamEscape::NonEscaping {
                self.push(site, RirVerifyErrorKind::CallArgEscape);
            }
        }
        for (param, sig_param) in function.params.iter().skip(hidden_params).zip(&sig.params) {
            if param.ty != sig_param.ty {
                self.push(
                    site,
                    RirVerifyErrorKind::TypeMismatch {
                        expected: sig_param.ty,
                        found: param.ty,
                    },
                );
            }
            if param.semantic != sig_param.semantic {
                self.push(site, RirVerifyErrorKind::CallArgMode);
            }
            if param.abi != sig_param.abi {
                self.push(site, RirVerifyErrorKind::UnsupportedAbi);
            }
            if param.escape != sig_param.escape {
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
                self.check_lambda_container_type_family(
                    site,
                    field.ty,
                    LambdaStorageFamily::StructField,
                );
                if strukt.copyable && !self.copyable_type(field.ty) {
                    self.push(site, RirVerifyErrorKind::NonCopyValueRequired);
                }
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
                self.check_lambda_container_type_family(
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
                        self.check_lambda_container_type_family(
                            site,
                            field.ty,
                            LambdaStorageFamily::DynamicPayload,
                        );
                    } else {
                        self.check_stored_payload(site, field.ty);
                        self.check_lambda_container_type(site, field.ty);
                    }
                    if enm.copyable && !self.copyable_type(field.ty) {
                        self.push(site, RirVerifyErrorKind::NonCopyValueRequired);
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
                if member.id != RirFlagMemberId::from_index(member_index) {
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
                self.check_lambda_container_type_family(
                    site,
                    field.ty,
                    LambdaStorageFamily::TupleField,
                );
                if tuple.copyable && !self.copyable_type(field.ty) {
                    self.push(site, RirVerifyErrorKind::NonCopyValueRequired);
                }
            }
        }
    }

    fn check_construct_fields(
        &mut self,
        site: RirVerifySite,
        function: &RirFunction,
        ty: RirTypeId,
        expected: &[RirField],
        found: &[RirOperand],
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
        for (field, operand) in expected.iter().zip(found) {
            self.check_value_operand_ty(site, function, operand, field.ty);
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
            if req.id != RirStringifyReqId::from_index(index) {
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
                    if param.ty != req.ty || param.semantic != mode {
                        self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                    }
                    if !matches!(
                        mode,
                        RirParamSemantic::Value | RirParamSemantic::SharedBorrow
                    ) {
                        self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                    }
                    if mode == RirParamSemantic::Value && !self.copyable_type(req.ty) {
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

    fn check_extern(&mut self, id: RirExternId, ext: &RirExtern) {
        self.check_type_id(RirVerifySite::Extern(id), ext.ret);
        self.check_extern_signature(id, ext);
        for param in &ext.params {
            self.check_type_id(RirVerifySite::Extern(id), param.ty);
            self.check_abi(
                RirVerifySite::Extern(id),
                param.ty,
                param.semantic,
                param.abi,
            );
        }
    }

    fn check_extern_signature(&mut self, id: RirExternId, ext: &RirExtern) {
        let site = RirVerifySite::Extern(id);
        let void = self.type_id(RirType::Void);
        let ok = match &ext.kind {
            RirExternKind::Native(native) => {
                native_extern_signature_ok(self.program, native, ext, void)
            }
        };
        if !ok {
            self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
        }
    }

    fn check_function(&mut self, id: RirFunctionId, function: &RirFunction) {
        self.check_type_id(RirVerifySite::Function(id), function.ret.ty);
        for (index, param) in function.params.iter().enumerate() {
            let site = RirVerifySite::Param(id, index);
            self.check_local_id(site, function, param.local);
            self.check_type_id(site, param.ty);
            self.check_abi(site, param.ty, param.semantic, param.abi);
            if (param.semantic == RirParamSemantic::DynBorrow
                || param.abi == RirParamAbi::DynBorrow)
                && param.escape != RirParamEscape::NonEscaping
            {
                self.push(site, RirVerifyErrorKind::CallArgEscape);
            }
            for storage in [RirCellStorage::StackScoped, RirCellStorage::Heap] {
                if Self::param_uses_cell_mode(*param, storage)
                    && !self.function_param_is_cell_capture(id, index, *param, storage)
                {
                    self.push(site, RirVerifyErrorKind::UnsupportedAbi);
                }
            }
            if matches!(
                param.semantic,
                RirParamSemantic::ScopedLambda
                    | RirParamSemantic::EscapingLambda
                    | RirParamSemantic::AnvCallback
            ) || matches!(
                param.abi,
                RirParamAbi::ScopedLambda | RirParamAbi::EscapingLambda | RirParamAbi::AnvCallback
            ) {
                self.push(site, RirVerifyErrorKind::UnsupportedAbi);
            }
            if (matches!(param.semantic, RirParamSemantic::ScopedPlaceCell)
                || matches!(param.abi, RirParamAbi::ScopedPlaceCell))
                && !self.function_param_is_scoped_place_cell_capture(id, index, *param)
            {
                self.push(site, RirVerifyErrorKind::UnsupportedAbi);
            }
            match function.locals.get(param.local.index()) {
                Some(local) if local.ty != param.ty => self.push(
                    site,
                    RirVerifyErrorKind::TypeMismatch {
                        expected: param.ty,
                        found: local.ty,
                    },
                ),
                Some(local) if !local.initialized => {
                    self.push(site, RirVerifyErrorKind::ParamLocalNotInitialized);
                }
                Some(_) => {}
                None => self.push(site, RirVerifyErrorKind::ParamLocalMissing),
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
        let previous_payload_ref_owned = std::mem::take(&mut self.payload_ref_owned);
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
            .map(|local| local.initialized)
            .collect();
        self.possibly_initialized.clone_from(&self.initialized);
        self.payload_ref_owned = vec![false; function.locals.len()];
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
            .map(|local| local.initialized.then_some(0))
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
        for param in &function.params {
            if let Some(initialized) = self.initialized.get_mut(param.local.index()) {
                *initialized = true;
            }
            if let Some(possible) = self.possibly_initialized.get_mut(param.local.index()) {
                *possible = true;
            }
            let value = if matches!(self.ty(param.ty), Some(RirType::Lambda(_))) {
                let escape = Some(RirLambdaEscape::from_param_escape(param.escape));
                if let Some(slot) = self.lambda_escapes.get_mut(param.local.index()) {
                    *slot = escape;
                }
                RirFunctionValueState::Lambda(escape)
            } else {
                self.source_call_return_state(param.ty)
            };
            if let Some(slot) = self.lambda_values.get_mut(param.local.index()) {
                *slot = value;
            }
        }
        for (stmt_index, stmt) in function.body.stmts.iter().enumerate() {
            self.check_stmt(id, function, stmt_index, stmt);
        }
        self.check_term(id, function, &function.body.term);
        for local in &function.locals {
            if local.payload_ref
                && !self
                    .payload_ref_owned
                    .get(local.id.index())
                    .copied()
                    .unwrap_or(false)
            {
                self.push(
                    RirVerifySite::Local(id, local.id),
                    RirVerifyErrorKind::OptionPayloadRefWithoutOwner,
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
        self.payload_ref_owned = previous_payload_ref_owned;
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
            RirDynMatchSource::Owned {
                value,
                air_use,
                air_local,
            } => {
                self.check_dyn_owned_use(site, value, *air_use, *air_local);
                if self.value_operand_ty(site, function, value) != Some(carrier.storage_ty) {
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
                if ty != carrier.storage_ty {
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
        let entry = self.block_entry_state();
        let mut states = vec![];
        let mut seen = vec![];
        for arm in &match_.arms {
            self.check_type_id(site, arm.target);
            let expected = carrier
                .variants
                .iter()
                .filter(|variant| variant.concrete_ty == arm.target)
                .map(|variant| variant.id)
                .collect::<Vec<_>>();
            if arm.variants != expected || arm.variants.iter().any(|variant| seen.contains(variant))
            {
                self.push(site, RirVerifyErrorKind::InvalidDynVariant);
            }
            seen.extend(&arm.variants);
            let binding = match arm.binding {
                RirDynMatchBinding::Discard => None,
                RirDynMatchBinding::Owned(local) => {
                    self.check_dyn_match_local(site, function, local, arm.target, false, mutable);
                    Some(local)
                }
                RirDynMatchBinding::Alias(local) => {
                    self.check_dyn_match_local(site, function, local, arm.target, true, !mutable);
                    if let Some(slot) = self.payload_ref_owned.get_mut(local.index()) {
                        *slot = true;
                    }
                    Some(local)
                }
            };
            let mut arm_entry = entry.clone();
            Self::init_entry_locals(&mut arm_entry, binding);
            let mut state =
                self.check_structured_block(function_id, function, &arm.block, arm_entry, None);
            if let (Some(state), Some(binding)) = (&mut state, binding) {
                state.definite[binding.index()] = false;
                state.possible[binding.index()] = false;
            }
            states.push(state);
        }
        let fallback_binding = match_.fallback_binding;
        if let Some(local) = fallback_binding {
            self.check_dyn_match_local(
                site,
                function,
                local,
                carrier.storage_ty,
                mutable && !borrowed,
                false,
            );
            if mutable
                && !borrowed
                && let Some(slot) = self.payload_ref_owned.get_mut(local.index())
            {
                *slot = true;
            }
        }
        let mut fallback_entry = entry;
        Self::init_entry_locals(&mut fallback_entry, fallback_binding);
        let reborrow = fallback_binding.filter(|_| borrowed);
        if let Some(local) = reborrow {
            self.active_dyn_reborrows.push((local, match_.carrier));
        }
        let mut fallback = self.check_structured_block(
            function_id,
            function,
            &match_.fallback,
            fallback_entry,
            None,
        );
        if reborrow.is_some() {
            self.active_dyn_reborrows.pop();
        }
        if let (Some(state), Some(binding)) = (&mut fallback, fallback_binding) {
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
        payload_ref: bool,
        invalid_mode: bool,
    ) {
        self.check_local_id(site, function, local);
        if invalid_mode
            || !function.locals.get(local.index()).is_some_and(|local| {
                local.ty == ty
                    && local.payload_ref == payload_ref
                    && (!payload_ref || local.mutable)
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
        let subject_ty = match_.subject.ty;
        let entry = self.block_entry_state();
        let mut states = vec![];
        for arm in &match_.arms {
            if arm.alternatives.is_empty() {
                self.push(site, RirVerifyErrorKind::PatternAlternativeRequired);
            }
            let mut arm_entry = entry.clone();
            let mut expected_bindings: Option<Vec<RirPatternBindingSignature>> = None;
            for alternative in &arm.alternatives {
                let bindings = self.check_pattern_alternative(
                    site,
                    function,
                    subject_ty,
                    alternative,
                    &entry,
                    &mut arm_entry,
                );
                match &expected_bindings {
                    Some(expected) if expected != &bindings => {
                        self.push(site, RirVerifyErrorKind::PatternBindingMismatch);
                    }
                    Some(_) => {}
                    None => expected_bindings = Some(bindings),
                }
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

    fn check_pattern_alternative(
        &mut self,
        site: RirVerifySite,
        function: &RirFunction,
        subject_ty: RirTypeId,
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
            let value = self.source_call_return_state(binding.ty);
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
                        RirPatternPathStep::Field(_) | RirPatternPathStep::TupleField(_)
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
        if function
            .params
            .iter()
            .any(|param| param.local == binding.local)
            || entry
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
                RirPatternBindingMode::Alias if !local.mutable || !local.payload_ref => {
                    self.push(site, RirVerifyErrorKind::OptionPayloadRefLocalMismatch);
                }
                RirPatternBindingMode::Owned if local.payload_ref => {
                    self.push(site, RirVerifyErrorKind::OptionPayloadRefLocalMismatch);
                }
                RirPatternBindingMode::Alias | RirPatternBindingMode::Owned => {}
            }
        }
        if matches!(binding.mode, RirPatternBindingMode::Alias) {
            if !Self::pattern_alias_path_supported(&binding.path) {
                self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
            }
            if let Some(slot) = self.payload_ref_owned.get_mut(binding.local.index()) {
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
                    .any(|binding| binding.mode == RirPatternBindingMode::Alias)
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
        match *step {
            RirPatternPathStep::Field(field) => match self.ty(source_ty) {
                Some(RirType::Struct(strukt)) => self
                    .program
                    .structs
                    .get(strukt.index())
                    .and_then(|strukt| strukt.fields.get(field.index()))
                    .map(|field| field.ty),
                _ => None,
            },
            RirPatternPathStep::TupleField(field) => match self.ty(source_ty) {
                Some(RirType::Tuple(tuple)) => self
                    .program
                    .tuples
                    .get(tuple.index())
                    .and_then(|tuple| tuple.fields.get(field as usize))
                    .map(|field| field.ty),
                _ => None,
            },
            RirPatternPathStep::OptionalSome => {
                if !optional_guards.iter().any(|guard| guard == prefix) {
                    self.push(site, RirVerifyErrorKind::PatternPayloadWithoutVariantTest);
                    return None;
                }
                match self.ty(source_ty) {
                    Some(RirType::Option(inner)) => Some(inner),
                    _ => None,
                }
            }
            RirPatternPathStep::EnumTupleField {
                enum_id,
                variant,
                field,
            } => {
                if !guards.iter().any(|(path, guard_enum, guard_variant)| {
                    path == prefix && *guard_enum == enum_id && *guard_variant == variant
                }) {
                    self.push(site, RirVerifyErrorKind::PatternPayloadWithoutVariantTest);
                    return None;
                }
                self.enum_payload_field_ty(
                    source_ty,
                    enum_id,
                    variant,
                    RirVariantKind::Tuple,
                    field,
                )
            }
            RirPatternPathStep::EnumStructField {
                enum_id,
                variant,
                field,
            } => {
                if !guards.iter().any(|(path, guard_enum, guard_variant)| {
                    path == prefix && *guard_enum == enum_id && *guard_variant == variant
                }) {
                    self.push(site, RirVerifyErrorKind::PatternPayloadWithoutVariantTest);
                    return None;
                }
                self.enum_payload_field_ty(
                    source_ty,
                    enum_id,
                    variant,
                    RirVariantKind::Struct,
                    field,
                )
            }
        }
        .or_else(|| {
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
                if function.params.iter().any(|param| param.local == *local) {
                    self.push(site, RirVerifyErrorKind::InitParamLocal);
                }
                if function
                    .locals
                    .get(local.index())
                    .is_some_and(|local| local.payload_ref)
                {
                    self.push(site, RirVerifyErrorKind::InitPayloadRefLocal);
                }
                if self
                    .possibly_initialized
                    .get(local.index())
                    .copied()
                    .unwrap_or(false)
                {
                    self.push(site, RirVerifyErrorKind::InitParamLocal);
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
                self.check_rvalue(function_id, function, index, value, Some(ty));
                self.check_stack_loop_lambda_rvalue(site, function, value);
                let value_state = self.rvalue_function_value_state(function, value);
                self.set_mut_place_function_value(function, place, value_state);
            }
            RirStmt::Assign { dst, value } => {
                self.check_place(site, function, dst);
                let Some(dst_local) = self.local_root(site, dst) else {
                    self.check_rvalue(function_id, function, index, value, Some(dst.ty));
                    self.check_stack_loop_lambda_rvalue(site, function, value);
                    return;
                };
                self.check_mutable_local_root(site, function, Some(dst_local));
                if self.assignment_replaces_active_collection_root(dst) {
                    self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                }
                if let Some(local) = function.locals.get(dst_local.index())
                    && !self.projected_mut_place_arg_supported(local.ty, &dst.projections, true)
                {
                    self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                }
                self.check_rvalue(function_id, function, index, value, Some(dst.ty));
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
                if !matches!(value, RirRValue::Use(_)) {
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
                if !matches!(value, RirRValue::Use(_)) {
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
                dataref,
                projections,
                value,
            } => {
                if let Some(ty) = self.check_dataref_access(
                    site,
                    function,
                    object,
                    *dataref,
                    projections,
                    StorageProjectionMode::Ordinary,
                ) {
                    self.check_value_operand_ty(site, function, value, ty);
                    self.check_stack_loop_lambda_operand(site, function, value);
                }
            }
            RirStmt::SequenceSlotSet {
                collection,
                index,
                value,
            } => {
                let value_ty = self.check_sequence_slot(
                    site,
                    function_id,
                    function,
                    collection,
                    *index,
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
                let (key_ty, value_ty) = match self.ty(map_ty) {
                    Some(RirType::Map { key, value }) => (Some(key), Some(value)),
                    _ => {
                        self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                        (None, None)
                    }
                };
                if let Some(key_ty) = key_ty {
                    self.check_value_operand_ty(site, function, &match_.key, key_ty);
                } else {
                    self.check_value_operand_ty(site, function, &match_.key, map_ty);
                }
                let entry = self.block_entry_state();
                let mut some_entry = entry.clone();
                if match_.payload_escapes && match_.payload.is_none() {
                    self.push(site, RirVerifyErrorKind::OptionPayloadEscapeRequiresPayload);
                }
                if let Some(payload) = match_.payload {
                    if let Some(local) = function.locals.get(payload.index()) {
                        if function.params.iter().any(|param| param.local == payload)
                            || entry
                                .possible
                                .get(payload.index())
                                .copied()
                                .unwrap_or(false)
                        {
                            self.push(site, RirVerifyErrorKind::InitParamLocal);
                        }
                        if !local.mutable || !local.payload_ref {
                            self.push(site, RirVerifyErrorKind::OptionPayloadRefLocalMismatch);
                        }
                        if let Some(slot) = self.payload_ref_owned.get_mut(payload.index()) {
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
                let subject_ty = match &match_.subject {
                    RirOptionSubject::Place(place) => {
                        self.check_place(site, function, place);
                        place.ty
                    }
                    RirOptionSubject::MutPlace(place) => {
                        let ty = self.check_mut_place_arg(
                            site,
                            function_id,
                            function,
                            place,
                            MutPlaceUse::CallArg,
                        );
                        if !match_.payload_ref {
                            self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                        }
                        ty
                    }
                };
                let discr_local = match &match_.subject {
                    RirOptionSubject::Place(place) => self.local_root(site, place),
                    RirOptionSubject::MutPlace(_) => None,
                };
                let inner = match self.ty(subject_ty) {
                    Some(RirType::Option(inner)) => Some(inner),
                    _ => {
                        self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                        None
                    }
                };
                let entry = self.block_entry_state();
                let mut some_entry = entry.clone();
                if (match_.payload_ref || match_.payload_escapes) && match_.payload.is_none() {
                    self.push(site, RirVerifyErrorKind::OptionPayloadEscapeRequiresPayload);
                }
                if match_.payload_escapes && !match_.payload_ref {
                    self.push(site, RirVerifyErrorKind::OptionPayloadEscapeRequiresRef);
                }
                if match_.payload.is_some()
                    && !match_.payload_ref
                    && inner.is_some_and(|inner| {
                        !RirRustRepPolicy::new(self.program).value_from_ref_supported(inner)
                    })
                {
                    self.push(site, RirVerifyErrorKind::NonCopyValueRequired);
                }
                if let Some(payload) = match_.payload {
                    if let Some(local) = function.locals.get(payload.index()) {
                        if function.params.iter().any(|param| param.local == payload) {
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
                        if local.mutable != match_.payload_ref {
                            self.push(site, RirVerifyErrorKind::ImmutableAssign);
                        }
                        if local.payload_ref != match_.payload_ref {
                            self.push(site, RirVerifyErrorKind::OptionPayloadRefLocalMismatch);
                        }
                        if match_.payload_ref
                            && let Some(slot) = self.payload_ref_owned.get_mut(payload.index())
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
                if match_.payload_ref
                    && discr_local.is_some_and(|local| Self::local_root_immutable(function, local))
                {
                    self.push(
                        site,
                        RirVerifyErrorKind::OptionPayloadRefDiscriminantMustBeMutable,
                    );
                }
                let escaping_payload = (match_.payload_ref && match_.payload_escapes)
                    .then_some(match_.payload)
                    .flatten();
                let mut some_state = self.check_structured_block(
                    function_id,
                    function,
                    &match_.some_block,
                    some_entry,
                    escaping_payload,
                );
                if match_.payload_ref
                    && !match_.payload_escapes
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
                RirMutPlaceHandle::Local { local, .. } | RirMutPlaceHandle::Param { local, .. },
            ) if place.projections.is_empty() => {
                self.set_local_lambda_escape(function, *local, value.escape());
                self.set_local_lambda_value(*local, value);
            }
            RirMutPlaceAccess::Handle(
                RirMutPlaceHandle::Local { local, .. } | RirMutPlaceHandle::Param { local, .. },
            ) if local.index() < self.lambda_values.len() => {
                if !self.lambda_values[local.index()].assign_projection(&place.projections, value) {
                    self.clear_local_lambda_value(*local);
                }
            }
            RirMutPlaceAccess::Handle(RirMutPlaceHandle::Global { global, .. })
                if place.projections.is_empty() =>
            {
                if let Some(slot) = self.global_values.get_mut(global.index()) {
                    *slot = value;
                }
            }
            RirMutPlaceAccess::Handle(RirMutPlaceHandle::Global { global, .. })
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
                RirMutPlaceHandle::Local { local, .. } | RirMutPlaceHandle::Param { local, .. },
            ) if local.index() < self.lambda_values.len() => {
                if !self.lambda_values[local.index()].push_projection(&place.projections, value) {
                    self.clear_local_lambda_value(*local);
                }
            }
            RirMutPlaceAccess::Handle(RirMutPlaceHandle::Global { global, .. })
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
                let value = self.operand_function_value_state(function, value);
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
            RirRValue::Use(operand) | RirRValue::FunctionValue { value: operand, .. } => {
                self.operand_loop_lambda_scope(function, operand)
            }
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
        match value {
            RirRValue::Struct { fields, .. }
            | RirRValue::Tuple { fields, .. }
            | RirRValue::DataRefAlloc { fields, .. }
            | RirRValue::EnumVariant { fields, .. }
            | RirRValue::Array { elems: fields, .. }
            | RirRValue::List { elems: fields, .. } => fields
                .iter()
                .any(|field| self.operand_contains_stack_loop_lambda(function, field)),
            RirRValue::Map { entries, .. } => entries.iter().any(|(key, value)| {
                self.operand_contains_stack_loop_lambda(function, key)
                    || self.operand_contains_stack_loop_lambda(function, value)
            }),
            RirRValue::OptionalSome { value, .. } | RirRValue::ListPush { value, .. } => {
                self.operand_contains_stack_loop_lambda(function, value)
            }
            RirRValue::MapInsert { key, value, .. } => {
                self.operand_contains_stack_loop_lambda(function, key)
                    || self.operand_contains_stack_loop_lambda(function, value)
            }
            _ => false,
        }
    }

    fn rvalue_contains_stack_loop_lambda(&self, function: &RirFunction, value: &RirRValue) -> bool {
        match value {
            RirRValue::Use(operand) | RirRValue::FunctionValue { value: operand, .. } => {
                self.operand_contains_stack_loop_lambda(function, operand)
            }
            RirRValue::Lambda { captures, .. } => captures
                .iter()
                .any(|capture| self.lambda_capture_arg_uses_loop_cell(capture)),
            value => self.rvalue_hides_stack_loop_lambda(function, value),
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
        if !place.projections.is_empty()
            || !matches!(self.ty(place.ty), Some(RirType::Lambda(_)))
            || function
                .locals
                .get(local.index())
                .is_none_or(|local_decl| local_decl.ty != place.ty)
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
                            .map(|field| self.source_call_return_state(field.ty))
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
                            .map(|field| self.source_call_return_state(field.ty))
                            .collect(),
                    )
                },
            ),
            Some(RirType::Option(inner)) => {
                RirFunctionValueState::Fields(vec![self.source_call_return_state(inner)])
            }
            Some(RirType::Array { elem, .. } | RirType::List(elem)) => {
                RirFunctionValueState::Fields(vec![self.source_call_return_state(elem)])
            }
            Some(RirType::Map { value, .. }) => {
                RirFunctionValueState::Fields(vec![self.source_call_return_state(value)])
            }
            _ => RirFunctionValueState::NonFunction,
        }
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
        if matches!(result, RirFunctionValueState::Unknown)
            && let RirTerm::Return(Some(value)) = &function.body.term
        {
            result = self.operand_function_value_state(function, value);
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
        if self
            .global_initialized
            .get(global.index())
            .copied()
            .unwrap_or(false)
        {
            return None;
        }
        let mut value = self.immutable_global_initializer_function_value_state(global);
        for projection in &place.projections {
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
            RirRValue::Use(operand) => self.operand_function_value_state(function, operand),
            RirRValue::FunctionValue { escape, .. } => RirFunctionValueState::Lambda(*escape),
            RirRValue::Lambda { lambda, .. } => RirFunctionValueState::Lambda(
                self.program
                    .lambdas
                    .get(lambda.index())
                    .map(|decl| decl.escape),
            ),
            RirRValue::OptionalSome { value, .. } => RirFunctionValueState::Fields(vec![
                self.operand_function_value_state(function, value),
            ]),
            RirRValue::MapGet { ty, .. } | RirRValue::MapRemove { ty, .. } => match self.ty(*ty) {
                Some(RirType::Option(inner)) => {
                    RirFunctionValueState::Fields(vec![self.source_call_return_state(inner)])
                }
                _ => RirFunctionValueState::Unknown,
            },
            RirRValue::SequenceSlotAt { ty, .. }
            | RirRValue::MapEntryAt { ty, .. }
            | RirRValue::MapKeyAt { ty, .. }
            | RirRValue::MapValueAt { ty, .. } => self.source_call_return_state(*ty),
            RirRValue::Struct { fields, .. }
            | RirRValue::Tuple { fields, .. }
            | RirRValue::DataRefAlloc { fields, .. }
            | RirRValue::EnumVariant { fields, .. } => RirFunctionValueState::Fields(
                fields
                    .iter()
                    .map(|field| self.operand_function_value_state(function, field))
                    .collect(),
            ),
            RirRValue::Array { elems, .. } | RirRValue::List { elems, .. } => {
                RirFunctionValueState::Fields(
                    elems
                        .iter()
                        .map(|elem| self.operand_function_value_state(function, elem))
                        .collect(),
                )
            }
            RirRValue::Map { entries, .. } => RirFunctionValueState::Fields(
                entries
                    .iter()
                    .map(|(_, value)| self.operand_function_value_state(function, value))
                    .collect(),
            ),
            RirRValue::DataRefGet {
                object,
                projections,
                ..
            } => {
                let mut value = self.operand_function_value_state(function, object);
                for projection in projections {
                    value = value.project(*projection);
                }
                value
            }
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
        operand: &RirOperand,
        claimed: Option<RirLambdaEscape>,
    ) {
        let actual = self
            .operand_function_value_state(function, operand)
            .escape()
            .or_else(|| self.global_initializer_operand_escape(operand));
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
            RirRValue::Use(operand) => self.operand_lambda_escape(function, operand),
            _ => None,
        }
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
        if !place.projections.is_empty()
            || !matches!(self.ty(place.ty), Some(RirType::Lambda(_)))
            || function
                .locals
                .get(local.index())
                .is_none_or(|local| local.ty != place.ty)
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
        self.check_collection_access(
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
            RirCollectionAccess::MutPlace(root)
                if matches!(root.access, RirMutPlaceAccess::DataRef { .. }) =>
            {
                self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
            }
            RirCollectionAccess::MutPlace(_) => {}
        }
        let root_ty = self.ty(scope.root.ty());
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
        ) && !self.collection_loan_root_mutable(function, &scope.root)
        {
            self.push(site, RirVerifyErrorKind::ImmutableAssign);
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
    ) {
        match root {
            RirCollectionAccess::Direct(root) => {
                self.check_place(site, function, root);
            }
            RirCollectionAccess::MutPlace(root) => {
                self.check_mut_place_arg(site, function_id, function, root, use_);
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
        self.check_collection_access(site, function_id, function, map, use_);
        let Some(RirType::Map { key, value }) = self.ty(map.ty()) else {
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
        self.check_collection_access(
            site,
            function_id,
            function,
            collection,
            MutPlaceUse::CallArg,
        );
        self.check_sequence_index_local(site, function, index);
        if matches!(collection, RirCollectionAccess::MutPlace(_))
            && !matches!(
                self.ty(collection.ty()),
                Some(RirType::Array { .. } | RirType::List(_) | RirType::Slice(_))
            )
        {
            self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
        }
        let elem_ty = self.sequence_elem(collection.ty());
        if elem_ty.is_none() {
            self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
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
                if !self.global_payload_supported(root.ty) {
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
        if let Some(root_ty) = root_ty
            && !RirPlaceModel::new(self.program)
                .collection_loan_projection_supported(root_ty, &root.projections)
        {
            self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
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
        let RirMutPlaceAccess::Handle(RirMutPlaceHandle::Global { global, .. }) = &place.access
        else {
            return false;
        };
        let dst = RirPlace::global(*global, place.projections.clone(), place.ty);
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
                let payload_ref_owned = self
                    .payload_ref_owned
                    .get(local.id.index())
                    .copied()
                    .unwrap_or(false);
                if local.payload_ref
                    && Some(local.id) != preserved_payload_ref
                    && !payload_ref_owned
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
            match (arg, decl.semantic) {
                (RirLambdaCaptureArg::Readonly { value }, RirParamSemantic::Value) => {
                    let RirOperand::Place(place) = value else {
                        self.push(site, RirVerifyErrorKind::UnsupportedLambdaCapture);
                        continue;
                    };
                    self.check_lambda_capture_place(site, function, place, decl.ty);
                    if matches!(lambda.storage, RirLambdaStorage::HeapEnv { .. })
                        && matches!(self.ty(decl.ty), Some(RirType::Lambda(_)))
                        && self.operand_lambda_escape(function, value)
                            != Some(RirLambdaEscape::Escaping)
                    {
                        self.push(site, RirVerifyErrorKind::CallArgEscape);
                    }
                    if !matches!(lambda.storage, RirLambdaStorage::HeapEnv { .. })
                        && !self.copyable_type(decl.ty)
                    {
                        self.push(site, RirVerifyErrorKind::NonCopyValueRequired);
                    }
                }
                (RirLambdaCaptureArg::Readonly { value }, RirParamSemantic::SharedBorrow) => {
                    let RirOperand::Place(place) = value else {
                        self.push(site, RirVerifyErrorKind::CallArgMode);
                        continue;
                    };
                    self.check_lambda_capture_place(site, function, place, decl.ty);
                }
                (RirLambdaCaptureArg::Scoped { place }, RirParamSemantic::MutBorrow) => {
                    let local = self.local_root(site, place);
                    self.check_lambda_capture_place(site, function, place, decl.ty);
                    self.check_mutable_local_root(site, function, local);
                }
                (RirLambdaCaptureArg::StackCell { cell }, RirParamSemantic::StackCell) => {
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
                (RirLambdaCaptureArg::HeapCell { cell }, RirParamSemantic::HeapCell) => {
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
                (
                    RirLambdaCaptureArg::ScopedPlaceCell { cell },
                    RirParamSemantic::ScopedPlaceCell,
                ) => {
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
        if place.ty != expected {
            self.push(
                site,
                RirVerifyErrorKind::TypeMismatch {
                    expected,
                    found: place.ty,
                },
            );
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
            RirRValue::MoveValue {
                value,
                air_local,
                ty,
            } => {
                self.check_type_id(site, *ty);
                let found = self.check_moved_value(site, function, value, *air_local);
                if found != Some(*ty) {
                    self.push(
                        site,
                        RirVerifyErrorKind::TypeMismatch {
                            expected: *ty,
                            found: found.unwrap_or(*ty),
                        },
                    );
                }
                Some(*ty)
            }
            RirRValue::DynCopy { carrier, value, ty } => {
                self.check_type_id(site, *ty);
                let valid = self
                    .program
                    .dyn_carriers
                    .get(carrier.index())
                    .is_some_and(|carrier| carrier.storage_ty == *ty)
                    && self.value_operand_ty(site, function, value) == Some(*ty);
                if !valid {
                    self.push(site, RirVerifyErrorKind::InvalidDynStorage);
                }
                Some(*ty)
            }
            RirRValue::DynPack {
                carrier,
                variant,
                air_witness,
                air_use,
                air_local,
                value,
                action,
                ty,
            } => {
                self.check_type_id(site, *ty);
                self.check_dyn_owned_use(site, value, *air_use, *air_local);
                let Some(carrier) = self.program.dyn_carriers.get(carrier.index()) else {
                    self.push(site, RirVerifyErrorKind::InvalidDynCarrier);
                    return;
                };
                if carrier.storage_ty != *ty {
                    self.push(site, RirVerifyErrorKind::InvalidDynStorage);
                }
                let Some(variant) = carrier.variants.iter().find(|decl| decl.id == *variant) else {
                    self.push(site, RirVerifyErrorKind::InvalidDynVariant);
                    return;
                };
                let expected_action = match air_use {
                    air::DynOwnedUse::ConsumeTemporary => RirDynPayloadAction::Move,
                    air::DynOwnedUse::ReusableRead => variant.payload,
                };
                if variant.air_witness != *air_witness
                    || *action != expected_action
                    || self.value_operand_ty(site, function, value) != Some(variant.concrete_ty)
                {
                    self.push(site, RirVerifyErrorKind::InvalidDynVariant);
                }
                Some(*ty)
            }
            RirRValue::DynWeaken {
                source,
                target,
                air_weakening,
                air_use,
                air_local,
                value,
                arms,
                ty,
            } => {
                self.check_type_id(site, *ty);
                self.check_dyn_owned_use(site, value, *air_use, *air_local);
                let weakening_origin = self
                    .program
                    .dyn_origins
                    .weakenings
                    .iter()
                    .find(|origin| origin.air_weakening == *air_weakening);
                let (Some(source), Some(target)) = (
                    self.program.dyn_carriers.get(source.index()),
                    self.program.dyn_carriers.get(target.index()),
                ) else {
                    self.push(site, RirVerifyErrorKind::InvalidDynCarrier);
                    return;
                };
                if !weakening_origin.is_some_and(|origin| {
                    origin.source == source.air_surface && origin.target == target.air_surface
                }) {
                    self.push(site, RirVerifyErrorKind::InvalidDynOrigin);
                }
                if target.storage_ty != *ty
                    || self.value_operand_ty(site, function, value) != Some(source.storage_ty)
                {
                    self.push(site, RirVerifyErrorKind::InvalidDynStorage);
                }
                let valid = arms.len() == source.variants.len()
                    && source.variants.iter().all(|source_variant| {
                        arms.iter()
                            .filter(|arm| arm.source == source_variant.id)
                            .count()
                            == 1
                            && arms
                                .iter()
                                .find(|arm| arm.source == source_variant.id)
                                .is_some_and(|arm| {
                                    target
                                        .variants
                                        .iter()
                                        .find(|target_variant| target_variant.id == arm.target)
                                        .is_some_and(|target_variant| {
                                            target_variant.concrete_ty == source_variant.concrete_ty
                                                && target_variant.storage == source_variant.storage
                                        })
                                })
                    });
                if !valid {
                    self.push(site, RirVerifyErrorKind::InvalidDynStorage);
                }
                Some(*ty)
            }
            RirRValue::DynDowncast {
                carrier,
                air_surface,
                air_use,
                air_local,
                value,
                target,
                variants,
                ty,
            } => {
                self.check_type_id(site, *target);
                self.check_type_id(site, *ty);
                self.check_dyn_owned_use(site, value, *air_use, *air_local);
                let Some(carrier) = self.program.dyn_carriers.get(carrier.index()) else {
                    self.push(site, RirVerifyErrorKind::InvalidDynCarrier);
                    return;
                };
                if carrier.air_surface != *air_surface
                    || !self.program.dyn_origins.surfaces.contains(air_surface)
                {
                    self.push(site, RirVerifyErrorKind::InvalidDynOrigin);
                }
                if self.value_operand_ty(site, function, value) != Some(carrier.storage_ty)
                    || self.ty(*ty) != Some(RirType::Option(*target))
                {
                    self.push(site, RirVerifyErrorKind::InvalidDynStorage);
                }
                let expected = carrier
                    .variants
                    .iter()
                    .filter(|variant| variant.concrete_ty == *target)
                    .map(|variant| variant.id)
                    .collect::<Vec<_>>();
                if *variants != expected {
                    self.push(site, RirVerifyErrorKind::InvalidDynVariant);
                }
                Some(*ty)
            }
            RirRValue::DynCall {
                carrier,
                air_slot,
                receiver,
                args,
                arms,
                ty,
                ..
            } => {
                self.check_type_id(site, *ty);
                let Some(carrier) = self.program.dyn_carriers.get(carrier.index()) else {
                    self.push(site, RirVerifyErrorKind::InvalidDynCarrier);
                    return;
                };
                if !self.program.dyn_origins.slots.iter().any(|origin| {
                    origin.air_slot == *air_slot && origin.surface == carrier.air_surface
                }) {
                    self.push(site, RirVerifyErrorKind::InvalidDynOrigin);
                }
                let receiver_ty = match receiver {
                    RirDynReceiver::Owned { value, .. } => {
                        self.value_operand_ty(site, function, value)
                    }
                    RirDynReceiver::Borrowed(borrow) => {
                        self.check_dyn_borrow(site, function_id, function, borrow);
                        self.program
                            .dyn_carriers
                            .get(borrow.target.index())
                            .map(|carrier| carrier.storage_ty)
                    }
                    RirDynReceiver::MutPlace(place) => Some(self.check_mut_place_arg(
                        site,
                        function_id,
                        function,
                        place,
                        MutPlaceUse::ReadCopy,
                    )),
                };
                if receiver_ty != Some(carrier.storage_ty) {
                    self.push(site, RirVerifyErrorKind::InvalidDynStorage);
                }
                for (arg_index, arg) in args.iter().enumerate() {
                    self.check_untyped_dyn_arg(function_id, function, index, arg_index, arg);
                }
                let uniform_receiver = arms.first().is_some_and(|first| {
                    arms.iter().all(|arm| {
                        arm.receiver.is_readonly_receiver() == first.receiver.is_readonly_receiver()
                    })
                });
                let valid = arms.len() == carrier.variants.len()
                    && uniform_receiver
                    && carrier.variants.iter().all(|variant| {
                        arms.iter().filter(|arm| arm.variant == variant.id).count() == 1
                    })
                    && arms
                        .iter()
                        .all(|arm| Self::dyn_receiver_abi_valid(receiver, arm.receiver))
                    && arms.iter().all(|arm| {
                        carrier
                            .variants
                            .iter()
                            .find(|variant| variant.id == arm.variant)
                            .is_some_and(|variant| {
                                let receiver_materialization_valid = !matches!(
                                    (receiver, arm.receiver),
                                    (
                                        RirDynReceiver::Owned { consume: false, .. },
                                        RirParamAbi::Value
                                    )
                                ) || !matches!(
                                    variant.payload,
                                    RirDynPayloadAction::Move | RirDynPayloadAction::BorrowView
                                );
                                receiver_materialization_valid
                                    && self.program.dyn_origins.dispatches.iter().any(|origin| {
                                        origin.air_witness == variant.air_witness
                                            && origin.air_slot == *air_slot
                                            && origin.receiver == arm.receiver
                                            && origin.target == arm.target
                                    })
                                    && self.dyn_dispatch_target_valid(
                                        function,
                                        receiver,
                                        variant.concrete_ty,
                                        arm,
                                        args,
                                        *ty,
                                    )
                            })
                    });
                if !valid {
                    self.push(site, RirVerifyErrorKind::InvalidDynDispatch);
                }
                Some(*ty)
            }
            RirRValue::FunctionValue { value, escape, ty } => {
                self.check_type_id(site, *ty);
                self.check_lambda_escape_proof(site, function, value, *escape);
                match (self.value_operand_ty(site, function, value), self.ty(*ty)) {
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
                Some(self.check_construct_fields(site, function, *ty, &strukt.fields, fields))
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
                Some(self.check_construct_fields(site, function, *ty, &tuple.fields, fields))
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
                Some(self.check_construct_fields(site, function, *ty, &dataref.fields, fields))
            }
            RirRValue::DataRefGet {
                object,
                dataref,
                projections,
                ty,
            } => {
                self.check_type_id(site, *ty);
                let found = self.check_dataref_access(
                    site,
                    function,
                    object,
                    *dataref,
                    projections,
                    StorageProjectionMode::Ordinary,
                );
                if let Some(found) = found
                    && found != *ty
                {
                    self.push(
                        site,
                        RirVerifyErrorKind::TypeMismatch {
                            expected: found,
                            found: *ty,
                        },
                    );
                }
                Some(*ty)
            }
            RirRValue::CellGetCopy { cell, ty } => {
                self.check_type_id(site, *ty);
                if let Some(decl) = self.check_function_cell_ref(site, function_id, *cell) {
                    if decl.payload_ty != *ty {
                        self.push(
                            site,
                            RirVerifyErrorKind::TypeMismatch {
                                expected: decl.payload_ty,
                                found: *ty,
                            },
                        );
                    }
                    if !self.cell_initialized(decl.id) {
                        self.push(site, RirVerifyErrorKind::UninitializedCell(decl.id));
                    }
                    if !self.copyable_type(decl.payload_ty)
                        && !RirRustRepPolicy::new(self.program).shareable_value(decl.payload_ty)
                    {
                        self.push(site, RirVerifyErrorKind::NonCopyValueRequired);
                    }
                }
                Some(*ty)
            }
            RirRValue::ScopedPlaceCellGet { cell, ty } => {
                self.check_type_id(site, *ty);
                if let Some(decl) =
                    self.check_function_scoped_place_cell_ref(site, function_id, *cell)
                {
                    if decl.payload_ty != *ty {
                        self.push(
                            site,
                            RirVerifyErrorKind::TypeMismatch {
                                expected: decl.payload_ty,
                                found: *ty,
                            },
                        );
                    }
                    if !RirRustRepPolicy::new(self.program).shareable_value(decl.payload_ty) {
                        self.push(site, RirVerifyErrorKind::NonCopyValueRequired);
                    }
                }
                Some(*ty)
            }
            RirRValue::MutPlaceGetCopy { place, ty } => {
                self.check_type_id(site, *ty);
                let found = self.check_mut_place_arg(
                    site,
                    function_id,
                    function,
                    place,
                    MutPlaceUse::ReadCopy,
                );
                if found != *ty {
                    self.push(
                        site,
                        RirVerifyErrorKind::TypeMismatch {
                            expected: found,
                            found: *ty,
                        },
                    );
                }
                if !self.copyable_type(*ty) {
                    self.push(site, RirVerifyErrorKind::NonCopyValueRequired);
                }
                Some(*ty)
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
                    self.check_value_operand_ty(site, function, elem_value, elem);
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
                    self.check_value_operand_ty(site, function, elem_value, elem);
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
                    self.check_value_operand_ty(site, function, entry_key, key);
                    self.check_value_operand_ty(site, function, entry_value, value);
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
                    self.check_value_operand_ty(site, function, operand, field.ty);
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
                if !self.binary_ok(*op, lhs_ty, rhs_ty, *ty) {
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
                let value_ty = self.value_operand_ty(site, function, value);
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
                    self.ty(source.ty),
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
                self.check_collection_access(
                    site,
                    function_id,
                    function,
                    source,
                    MutPlaceUse::CallArg,
                );
                if !matches!(
                    self.ty(source.ty()),
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
            RirRValue::SequenceSlotAt {
                collection,
                index,
                ty,
            } => {
                self.check_type_id(site, *ty);
                self.check_sequence_slot(
                    site,
                    function_id,
                    function,
                    collection,
                    *index,
                    Some(*ty),
                    false,
                );
                Some(*ty)
            }
            RirRValue::ListPush { list, value } => {
                self.check_collection_access(
                    site,
                    function_id,
                    function,
                    list,
                    MutPlaceUse::CollectionMutation,
                );
                if !self.collection_loan_root_mutable(function, list) {
                    self.push(site, RirVerifyErrorKind::ImmutableAssign);
                }
                let Some(RirType::List(elem)) = self.ty(list.ty()) else {
                    self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                    return;
                };
                self.check_value_operand_ty(site, function, value, elem);
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
                self.check_slice_range(site, function, source, *start, *end);
                let Some(RirType::Slice(elem)) = self.ty(*ty) else {
                    self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                    return;
                };
                let source_local = self.local_root(site, source);
                if *mutable {
                    self.check_mutable_local_root(site, function, source_local);
                }
                if self.sequence_elem(source.ty) != Some(elem) {
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
                if self.sequence_elem(source.ty) != Some(elem) {
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
                        let Some(RirType::Map { key, value }) = self.ty(map_ty) else {
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
                self.check_value_operand_ty(site, function, key, key_ty);
                self.check_value_operand_ty(site, function, value, value_ty);
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
            | RirCallArg::MovedValue { value: operand, .. }
            | RirCallArg::InitFieldProvided(operand)
            | RirCallArg::ScopedLambda {
                callee: operand, ..
            }
            | RirCallArg::EscapingLambda {
                callee: operand, ..
            }
            | RirCallArg::AnvCallback {
                callee: operand, ..
            } => self.operand_lambda_escape(function, operand),
            RirCallArg::SharedBorrow(place)
                if matches!(
                    self.program.types.get(place.ty.index()),
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
        caller: &RirFunction,
        receiver_value: &RirDynReceiver,
        mut receiver_ty: RirTypeId,
        arm: &RirDynDispatchArm,
        args: &[RirCallArg],
        ret: RirTypeId,
    ) -> bool {
        let mut target = &arm.target;
        while let RirResolvedCallTarget::Promoted {
            fields,
            target: next,
        } = target
        {
            if fields.is_empty() {
                return false;
            }
            for field in fields {
                let Some(RirType::Struct(id)) = self.program.types.get(receiver_ty.index()) else {
                    return false;
                };
                let Some(projected) = self
                    .program
                    .structs
                    .get(id.index())
                    .and_then(|decl| decl.fields.get(field.index()))
                    .filter(|projected| projected.id == *field)
                else {
                    return false;
                };
                receiver_ty = projected.ty;
            }
            target = next;
        }
        let (params, target_ret, native) = match target {
            RirResolvedCallTarget::Function(id) => {
                let Some(function) = self.program.functions.get(id.index()) else {
                    return false;
                };
                (
                    function
                        .params
                        .iter()
                        .map(|param| (param.ty, param.semantic, param.abi, param.escape))
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
                        .map(|param| (param.ty, param.semantic, param.abi, param.escape))
                        .collect::<Vec<_>>(),
                    function.ret,
                    Some(*id),
                )
            }
            RirResolvedCallTarget::Promoted { .. } => unreachable!(),
        };
        let Some((receiver, params)) = params.split_first() else {
            return false;
        };
        if params.len() != args.len() {
            return false;
        }
        let receiver_present = Self::dyn_receiver_abi_valid(receiver_value, arm.receiver);
        let source_receiver_valid = arm.receiver != RirParamAbi::MutBorrow || native.is_some();
        let native_valid = native.is_none_or(|id| {
            let plan = self.program.native_call_plan(id);
            let resource = native_ty_is_resource_ref(self.program, receiver.0);
            receiver_present
                && match arm.receiver {
                    RirParamAbi::MutBorrow => resource,
                    RirParamAbi::MutPlace => !resource,
                    _ => true,
                }
                && !plan.rejects_reentry_arg(
                    0,
                    native_dynamic_arg_facts(self.program, receiver.0, receiver.1),
                )
                && !args.iter().enumerate().any(|(index, arg)| {
                    self.program.native_call_plan(id).rejects_reentry_arg(
                        index + 1,
                        native_arg_facts(self.program, params[index].0, arg),
                    )
                })
        });
        let params_valid = params
            .iter()
            .zip(args)
            .all(|((ty, semantic, abi, escape), arg)| {
                let callback_sig = match arg {
                    RirCallArg::ScopedLambda { sig, .. }
                    | RirCallArg::EscapingLambda { sig, .. }
                    | RirCallArg::AnvCallback { sig, .. } => {
                        matches!(self.program.types.get(ty.index()), Some(RirType::Lambda(expected)) if expected == sig)
                    }
                    _ => true,
                };
                let adaptable = arg.adapted_to(*semantic, self.program).is_some_and(|_| {
                    !matches!(
                        (semantic, arg),
                        (RirParamSemantic::Value, RirCallArg::SharedBorrow(_))
                    ) || RirRustRepPolicy::new(self.program).value_from_ref_supported(*ty)
                });
                adaptable
                    && self.dyn_arg_ty(arg) == Some(*ty)
                    && callback_sig
                    && (*escape != RirParamEscape::Escaping
                        || !matches!(
                            self.program.types.get(ty.index()),
                            Some(RirType::Lambda(_))
                        )
                        || self.call_arg_lambda_escape(caller, arg)
                            == Some(RirLambdaEscape::Escaping))
                    && RirRustRepPolicy::new(self.program)
                        .call_arg_abi(*ty, *semantic)
                        .is_some_and(|found| found == *abi)
            });
        source_receiver_valid
            && native_valid
            && receiver.0 == receiver_ty
            && receiver.2 == arm.receiver
            && target_ret == ret
            && params_valid
    }

    fn dyn_receiver_abi_valid(receiver: &RirDynReceiver, abi: RirParamAbi) -> bool {
        match receiver {
            RirDynReceiver::Owned { value, .. } => {
                abi == RirParamAbi::Value
                    || (abi == RirParamAbi::SharedBorrow && matches!(value, RirOperand::Place(_)))
            }
            RirDynReceiver::Borrowed(_) => matches!(
                abi,
                RirParamAbi::Value
                    | RirParamAbi::SharedBorrow
                    | RirParamAbi::MutBorrow
                    | RirParamAbi::MutPlace
            ),
            RirDynReceiver::MutPlace(_) => {
                matches!(abi, RirParamAbi::MutBorrow | RirParamAbi::MutPlace)
            }
        }
    }

    fn dyn_arg_ty(&self, arg: &RirCallArg) -> Option<RirTypeId> {
        let operand_ty = |operand: &RirOperand| match operand {
            RirOperand::Const(id) => self.program.consts.get(id.index()).map(|konst| konst.ty),
            RirOperand::Place(place) => Some(place.ty),
        };
        match arg {
            RirCallArg::Value(operand)
            | RirCallArg::MovedValue { value: operand, .. }
            | RirCallArg::InitFieldProvided(operand) => operand_ty(operand),
            RirCallArg::InitFieldOmitted => None,
            RirCallArg::SharedBorrow(place) | RirCallArg::MutBorrow(place) => Some(place.ty),
            RirCallArg::SharedStringConst(_) => self.string_ty(),
            RirCallArg::MutPlace(place) => Some(place.ty),
            RirCallArg::DynBorrow(borrow) => self
                .program
                .dyn_carriers
                .get(borrow.target.index())
                .map(|carrier| carrier.storage_ty),
            RirCallArg::ScopedLambda { callee, .. }
            | RirCallArg::EscapingLambda { callee, .. }
            | RirCallArg::AnvCallback { callee, .. } => operand_ty(callee),
        }
    }

    fn check_dyn_owned_use(
        &mut self,
        site: RirVerifySite,
        value: &RirOperand,
        use_: air::DynOwnedUse,
        air_local: Option<air::LocalId>,
    ) {
        let valid = match (use_, air_local, value) {
            (air::DynOwnedUse::ConsumeTemporary, Some(air_local), RirOperand::Place(place)) => {
                matches!(place.root, RirPlaceRoot::Local(local) if local.index() == air_local.index())
                    && place.projections.is_empty()
            }
            (air::DynOwnedUse::ReusableRead, None, _) => true,
            _ => false,
        };
        if !valid {
            self.push(site, RirVerifyErrorKind::InvalidDynOrigin);
        }
    }

    fn check_moved_value(
        &mut self,
        site: RirVerifySite,
        function: &RirFunction,
        value: &RirOperand,
        air_local: air::LocalId,
    ) -> Option<RirTypeId> {
        let ty = self.value_operand_ty(site, function, value);
        let RirOperand::Place(place) = value else {
            self.push(site, RirVerifyErrorKind::CallArgMode);
            return ty;
        };
        let valid = matches!(place.root, RirPlaceRoot::Local(local) if local.index() == air_local.index())
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
    ) -> Option<RirTypeId> {
        let target_surface = self
            .program
            .dyn_carriers
            .get(borrow.target.index())?
            .air_surface;
        let (source_carrier, source_surface, source_ty) = match &borrow.source {
            RirDynBorrowSource::Concrete {
                place,
                carrier,
                air_witness,
            } => {
                let ty = self.check_mut_place_arg(
                    site,
                    function_id,
                    function,
                    place,
                    MutPlaceUse::CallArg,
                );
                let source = self.program.dyn_carriers.get(carrier.index());
                let valid = source.is_some_and(|source| {
                    source.variants.iter().any(|variant| {
                        variant.air_witness == *air_witness && variant.concrete_ty == ty
                    })
                });
                if !valid {
                    self.push(site, RirVerifyErrorKind::InvalidDynOrigin);
                }
                (
                    Some(*carrier),
                    source.map(|source| source.air_surface),
                    Some(ty),
                )
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
                if source.is_none_or(|source| source.storage_ty != ty) {
                    self.push(site, RirVerifyErrorKind::InvalidDynStorage);
                }
                (
                    Some(*carrier),
                    source.map(|source| source.air_surface),
                    Some(ty),
                )
            }
            RirDynBorrowSource::Borrowed { local, carrier } => {
                self.check_local_id(site, function, *local);
                let source = self.program.dyn_carriers.get(carrier.index());
                let valid = source.is_some_and(|source| {
                    function.params.iter().any(|param| {
                        param.local == *local
                            && param.semantic == RirParamSemantic::DynBorrow
                            && param.abi == RirParamAbi::DynBorrow
                            && param.ty == source.storage_ty
                            && param.escape == RirParamEscape::NonEscaping
                    })
                });
                if !valid {
                    self.push(site, RirVerifyErrorKind::InvalidDynOrigin);
                }
                (
                    Some(*carrier),
                    source.map(|source| source.air_surface),
                    source.map(|source| source.storage_ty),
                )
            }
            RirDynBorrowSource::Reborrowed { local, carrier } => {
                self.check_local_id(site, function, *local);
                let source = self.program.dyn_carriers.get(carrier.index());
                let valid =
                    source.is_some_and(|source| {
                        function.locals.get(local.index()).is_some_and(|local| {
                            local.ty == source.storage_ty && !local.payload_ref
                        }) && self.initialized.get(local.index()).copied() == Some(true)
                            && self.active_dyn_reborrows.contains(&(*local, *carrier))
                    });
                if !valid {
                    self.push(site, RirVerifyErrorKind::InvalidDynOrigin);
                }
                (
                    Some(*carrier),
                    source.map(|source| source.air_surface),
                    source.map(|source| source.storage_ty),
                )
            }
        };
        let projection_valid = match borrow.air_weakening {
            Some(id) => {
                self.program.dyn_origins.weakenings.iter().any(|origin| {
                    origin.air_weakening == id
                        && Some(origin.source) == source_surface
                        && origin.target == target_surface
                }) && self.program.dyn_weakenings.iter().any(|weakening| {
                    weakening.air_id == id
                        && Some(weakening.source) == source_carrier
                        && weakening.target == borrow.target
                })
            }
            None => source_surface == Some(target_surface) && source_carrier == Some(borrow.target),
        };
        if !projection_valid {
            self.push(site, RirVerifyErrorKind::InvalidDynOrigin);
        }
        source_ty
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
                self.value_operand_ty(site, function, value);
            }
            RirCallArg::MovedValue { value, air_local } => {
                self.check_moved_value(site, function, value, *air_local);
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
                let direct_local = function
                    .locals
                    .get(local.index())
                    .is_some_and(|local| local.mutable && !local.payload_ref);
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
                self.value_operand_ty(site, function, callee);
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
        let (expected, callee_ret, init_fields, native_plan) = match callee {
            RirCallTarget::Function(id) => {
                self.check_function_id(RirVerifySite::RValue(function_id, stmt), id);
                match self.program.functions.get(id.index()) {
                    Some(function) => (
                        function
                            .params
                            .iter()
                            .map(|param| (param.ty, param.semantic, param.abi, param.escape))
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
                        let init_fields = match &ext.kind {
                            RirExternKind::Native(native) => native
                                .abi
                                .params
                                .iter()
                                .map(|param| matches!(param, RustParamAbi::InitField(_)))
                                .collect(),
                        };
                        (
                            ext.params
                                .iter()
                                .map(|param| (param.ty, param.semantic, param.abi, param.escape))
                                .collect::<Vec<_>>(),
                            ext.ret,
                            init_fields,
                            Some(match &ext.kind {
                                RirExternKind::Native(_) => self.program.native_call_plan(id),
                            }),
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
                        .map(|param| (param.ty, param.semantic, param.abi, param.escape))
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
        for (index, (arg, (ty, mode, abi, escape))) in args.iter().zip(expected).enumerate() {
            let site = RirVerifySite::CallArg(function_id, stmt, index);
            let found_init_field = matches!(
                arg,
                RirCallArg::InitFieldProvided(_) | RirCallArg::InitFieldOmitted
            );
            if found_init_field != init_fields.get(index).copied().unwrap_or(false) {
                self.push(site, RirVerifyErrorKind::CallArgMode);
            }
            if arg.semantic() != mode
                || matches!(
                    arg,
                    RirCallArg::ScopedLambda { .. }
                        | RirCallArg::EscapingLambda { .. }
                        | RirCallArg::AnvCallback { .. }
                ) && !native_call
            {
                self.push(site, RirVerifyErrorKind::CallArgMode);
            }
            if native_plan.as_ref().is_some_and(|plan| {
                plan.rejects_reentry_arg(index, native_arg_facts(self.program, ty, arg))
            }) {
                self.push(site, RirVerifyErrorKind::CallArgMode);
            }
            if RirRustRepPolicy::new(self.program).call_arg_abi(ty, arg.semantic()) != Some(abi) {
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
                    self.value_operand_ty(site, function, operand)
                }
                RirCallArg::MovedValue { value, air_local } => {
                    self.check_moved_value(site, function, value, *air_local)
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
                            if !self.global_payload_supported(place.ty) {
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
                    Some(place.ty)
                }
                RirCallArg::MutBorrow(place) => {
                    let RirPlaceRoot::Local(local) = place.root else {
                        self.check_place(site, function, place);
                        self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                        return;
                    };
                    self.check_place(site, function, place);
                    let direct_local = function
                        .locals
                        .get(local.index())
                        .is_some_and(|local| local.mutable && !local.payload_ref);
                    if Self::function_local_is_mut_place_param(function, local)
                        || !place.projections.is_empty()
                        || !direct_local
                    {
                        self.push(site, RirVerifyErrorKind::CallArgMode);
                    }
                    Some(place.ty)
                }
                RirCallArg::MutPlace(arg) => Some(self.check_mut_place_arg(
                    site,
                    function_id,
                    function,
                    arg,
                    MutPlaceUse::CallArg,
                )),
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
                    let found = self.value_operand_ty(site, function, callee);
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
    ) -> RirTypeId {
        self.check_type_id(site, arg.ty);
        match &arg.access {
            RirMutPlaceAccess::Handle(handle) => {
                self.check_mut_place_handle(site, function_id, function, handle, arg, use_)
            }
            RirMutPlaceAccess::DataRef { object, dataref } => {
                if !use_.allow_dataref() {
                    self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                }
                let mode = if matches!(
                    use_,
                    MutPlaceUse::MapRead | MutPlaceUse::IndexedMapAssignment
                ) {
                    StorageProjectionMode::Ordinary
                } else {
                    StorageProjectionMode::MutPlace
                };
                let found = self.check_dataref_access(
                    site,
                    function,
                    object,
                    *dataref,
                    &arg.projections,
                    mode,
                );
                if let Some(found) = found
                    && found != arg.ty
                {
                    self.push(
                        site,
                        RirVerifyErrorKind::TypeMismatch {
                            expected: found,
                            found: arg.ty,
                        },
                    );
                }
                arg.ty
            }
        }
    }

    fn check_mut_place_handle(
        &mut self,
        site: RirVerifySite,
        function_id: RirFunctionId,
        function: &RirFunction,
        handle: &RirMutPlaceHandle,
        arg: &RirMutPlaceArg,
        use_: MutPlaceUse,
    ) -> RirTypeId {
        match handle {
            RirMutPlaceHandle::Local { local, ty } => {
                if use_ == MutPlaceUse::ScopedPlaceSource {
                    self.check_scoped_place_source_projection(site, function, *ty, arg);
                } else {
                    self.check_place(
                        site,
                        function,
                        &RirPlace::local(*local, arg.projections.clone(), arg.ty),
                    );
                    if Self::function_local_is_mut_place_param(function, *local) {
                        self.push(site, RirVerifyErrorKind::CallArgMode);
                    }
                }
                match function.locals.get(local.index()) {
                    Some(local) if local.mutable || local.payload_ref => {
                        if local.ty != *ty {
                            self.push(
                                site,
                                RirVerifyErrorKind::TypeMismatch {
                                    expected: local.ty,
                                    found: *ty,
                                },
                            );
                        }
                    }
                    Some(_) => self.push(site, RirVerifyErrorKind::ImmutableAssign),
                    None => self.push(site, RirVerifyErrorKind::BadId),
                }
                if !self.projected_mut_place_arg_supported(*ty, &arg.projections, true) {
                    self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                }
                arg.ty
            }
            RirMutPlaceHandle::Param { local, ty } => {
                self.check_local_id(site, function, *local);
                if use_ != MutPlaceUse::ScopedPlaceSource
                    && self.function_local_is_scoped_place_source(function_id, *local)
                {
                    self.push(site, RirVerifyErrorKind::CallArgMode);
                }
                match function.params.iter().find(|param| param.local == *local) {
                    Some(param)
                        if param.semantic == RirParamSemantic::MutPlace
                            && param.abi == RirParamAbi::MutPlace =>
                    {
                        if param.ty != *ty {
                            self.push(
                                site,
                                RirVerifyErrorKind::TypeMismatch {
                                    expected: param.ty,
                                    found: *ty,
                                },
                            );
                        }
                    }
                    Some(_) => self.push(site, RirVerifyErrorKind::CallArgMode),
                    None => self.push(site, RirVerifyErrorKind::ParamLocalMissing),
                }
                if arg.projections.is_empty() {
                    if arg.ty != *ty {
                        self.push(
                            site,
                            RirVerifyErrorKind::TypeMismatch {
                                expected: *ty,
                                found: arg.ty,
                            },
                        );
                    }
                } else if use_ == MutPlaceUse::ScopedPlaceSource {
                    self.check_scoped_place_source_projection(site, function, *ty, arg);
                } else {
                    self.check_place(
                        site,
                        function,
                        &RirPlace::local(*local, arg.projections.clone(), arg.ty),
                    );
                    if !self.projected_mut_place_arg_supported(*ty, &arg.projections, true) {
                        self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                    }
                }
                arg.ty
            }
            RirMutPlaceHandle::StackCell { cell, ty } => {
                self.check_cell_mut_place_arg(
                    site,
                    function_id,
                    *cell,
                    *ty,
                    RirCellStorage::StackScoped,
                );
                self.check_projected_cell_mut_place_root(
                    site,
                    function,
                    *ty,
                    &arg.projections,
                    arg.ty,
                    use_,
                );
                arg.ty
            }
            RirMutPlaceHandle::HeapCell { cell, ty } => {
                self.check_cell_mut_place_arg(site, function_id, *cell, *ty, RirCellStorage::Heap);
                self.check_projected_cell_mut_place_root(
                    site,
                    function,
                    *ty,
                    &arg.projections,
                    arg.ty,
                    use_,
                );
                arg.ty
            }
            RirMutPlaceHandle::ScopedPlaceCell { cell, ty } => {
                if let Some(decl) =
                    self.check_function_scoped_place_cell_ref(site, function_id, *cell)
                    && decl.payload_ty != *ty
                {
                    self.push(
                        site,
                        RirVerifyErrorKind::TypeMismatch {
                            expected: decl.payload_ty,
                            found: *ty,
                        },
                    );
                }
                self.check_projected_cell_mut_place_root(
                    site,
                    function,
                    *ty,
                    &arg.projections,
                    arg.ty,
                    use_,
                );
                arg.ty
            }
            RirMutPlaceHandle::Global { global, ty } => {
                let Some(decl) = self.check_global_id(site, *global).cloned() else {
                    return arg.ty;
                };
                if !self
                    .global_initialized
                    .get(global.index())
                    .copied()
                    .unwrap_or(false)
                {
                    self.push(site, RirVerifyErrorKind::UninitializedGlobal(*global));
                }
                if !decl.mutable {
                    self.push(site, RirVerifyErrorKind::ImmutableAssign);
                }
                if decl.ty != *ty {
                    self.push(
                        site,
                        RirVerifyErrorKind::TypeMismatch {
                            expected: decl.ty,
                            found: *ty,
                        },
                    );
                }
                let Some(found) =
                    self.check_projection_chain(site, function, *ty, &arg.projections, true)
                else {
                    return arg.ty;
                };
                if found != arg.ty {
                    self.push(
                        site,
                        RirVerifyErrorKind::TypeMismatch {
                            expected: found,
                            found: arg.ty,
                        },
                    );
                }
                if !self.projected_mut_place_arg_supported(*ty, &arg.projections, true) {
                    self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                }
                arg.ty
            }
        }
    }

    fn check_projected_cell_mut_place_root(
        &mut self,
        site: RirVerifySite,
        function: &RirFunction,
        root_ty: RirTypeId,
        projections: &[RirProjection],
        final_ty: RirTypeId,
        use_: MutPlaceUse,
    ) {
        let Some(expected) = self.check_projection_chain(
            site,
            function,
            root_ty,
            projections,
            use_.allow_cell_collection_projection(),
        ) else {
            return;
        };
        if expected != final_ty {
            self.push(
                site,
                RirVerifyErrorKind::TypeMismatch {
                    expected,
                    found: final_ty,
                },
            );
        }
    }

    fn check_projection_chain(
        &mut self,
        site: RirVerifySite,
        function: &RirFunction,
        root_ty: RirTypeId,
        projections: &[RirProjection],
        allow_collections: bool,
    ) -> Option<RirTypeId> {
        let model = RirPlaceModel::new(self.program);
        let mut ty = root_ty;
        for projection in projections {
            let step = match model.step(ty, *projection, allow_collections) {
                Ok(step) => step,
                Err(error) => {
                    self.push_place_error(site, error);
                    return None;
                }
            };
            match step.kind {
                RirProjectionKind::SequenceIndex { local } => {
                    self.check_sequence_index_local(site, function, local)?;
                }
                RirProjectionKind::Field | RirProjectionKind::TupleField => {}
            }
            ty = step.ty;
        }
        Some(ty)
    }

    fn push_place_error(&mut self, site: RirVerifySite, error: RirPlaceError) {
        match error {
            RirPlaceError::BadId => self.push(site, RirVerifyErrorKind::BadId),
            RirPlaceError::Unsupported => {
                self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
            }
        }
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
        let Some(found) =
            self.check_projection_chain(site, function, root_ty, &arg.projections, true)
        else {
            return;
        };
        if found != arg.ty {
            self.push(
                site,
                RirVerifyErrorKind::TypeMismatch {
                    expected: found,
                    found: arg.ty,
                },
            );
        }
        if !self.projected_mut_place_arg_supported(root_ty, &arg.projections, true) {
            self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
        }
    }

    fn check_cell_mut_place_arg(
        &mut self,
        site: RirVerifySite,
        function_id: RirFunctionId,
        cell: RirCellRef,
        ty: RirTypeId,
        storage: RirCellStorage,
    ) {
        self.check_type_id(site, ty);
        if let Some(decl) = self.check_function_cell_ref(site, function_id, cell) {
            if decl.storage != storage {
                self.push(site, RirVerifyErrorKind::CallArgMode);
            }
            if decl.payload_ty != ty {
                self.push(
                    site,
                    RirVerifyErrorKind::TypeMismatch {
                        expected: decl.payload_ty,
                        found: ty,
                    },
                );
            }
            if !self.cell_initialized(decl.id) {
                self.push(site, RirVerifyErrorKind::UninitializedCell(decl.id));
            }
        }
    }

    fn check_term(&mut self, function_id: RirFunctionId, function: &RirFunction, term: &RirTerm) {
        let site = RirVerifySite::Terminator(function_id);
        match term {
            RirTerm::Return(None) if !matches!(self.ty(function.ret.ty), Some(RirType::Void)) => {
                self.push(site, RirVerifyErrorKind::ReturnValueRequired);
            }
            RirTerm::Return(Some(_)) if matches!(self.ty(function.ret.ty), Some(RirType::Void)) => {
                self.push(site, RirVerifyErrorKind::UnexpectedReturnValue);
            }
            RirTerm::Return(Some(operand)) => {
                self.check_stack_loop_lambda_operand(site, function, operand);
                if let Some(found) = self.value_operand_ty(site, function, operand) {
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
                        && self.operand_lambda_escape(function, operand)
                            != Some(RirLambdaEscape::Escaping)
                    {
                        self.push(site, RirVerifyErrorKind::CallArgEscape);
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

    fn check_abi(
        &mut self,
        site: RirVerifySite,
        ty: RirTypeId,
        semantic: RirParamSemantic,
        abi: RirParamAbi,
    ) {
        if self.ty(ty).is_none() {
            self.push(site, RirVerifyErrorKind::UnsupportedAbi);
            return;
        }
        let policy = RirRustRepPolicy::new(self.program);
        let supported = policy.supports_param(ty, semantic) && abi == policy.param_abi(semantic);
        if !supported {
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

    fn format_ok(&self, source_ty: RirTypeId, spec: RirFormatSpec) -> bool {
        if self.scalar(source_ty).is_none() {
            return false;
        }
        let ty = self
            .ty(source_ty)
            .expect("scalar type id has RIR type data");
        match spec.kind {
            RirFormatKind::Hex | RirFormatKind::HexUpper | RirFormatKind::Binary
                if ty != RirType::Int =>
            {
                return false;
            }
            RirFormatKind::Exp | RirFormatKind::ExpUpper if ty != RirType::Float => {
                return false;
            }
            _ => {}
        }
        if spec.precision.is_some() && !matches!(ty, RirType::Float | RirType::String) {
            return false;
        }
        if spec.sign == RirFormatSign::Always && !matches!(ty, RirType::Int | RirType::Float) {
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
                Some(place.ty)
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
        self.check_value_operand_shareable(site, operand, ty);
        ty
    }

    fn check_value_operand_shareable(
        &mut self,
        site: RirVerifySite,
        operand: &RirOperand,
        ty: Option<RirTypeId>,
    ) {
        if matches!(operand, RirOperand::Place(_))
            && ty.is_some_and(|ty| !RirRustRepPolicy::new(self.program).shareable_value(ty))
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
        dataref_id: RirDataRefId,
        projections: &[RirProjection],
        mode: StorageProjectionMode,
    ) -> Option<RirTypeId> {
        let object_ty = self.value_operand_ty(site, function, object);
        self.check_short_region_operand(site, function, object);
        if object_ty.and_then(|ty| self.ty(ty)) != Some(RirType::DataRef(dataref_id)) {
            self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
        }
        self.check_dataref_id(site, dataref_id);
        let model = RirPlaceModel::new(self.program);
        let path = match model.dataref_storage_path(dataref_id, projections) {
            Ok(path) => path,
            Err(error) => {
                self.push_place_error(site, error);
                return None;
            }
        };
        if mode == StorageProjectionMode::MutPlace
            && !model.dataref_mut_place_payload_supported(path.ty())
        {
            self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
        }
        Some(path.ty())
    }

    fn check_place(&mut self, site: RirVerifySite, function: &RirFunction, place: &RirPlace) {
        self.check_type_id(site, place.ty);
        let current = match place.root {
            RirPlaceRoot::Local(local) => {
                self.check_local_id(site, function, local);
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
                    && !self.projected_mut_place_arg_supported(current, &place.projections, true)
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
        let Some(current) =
            self.check_projection_chain(site, function, current, &place.projections, true)
        else {
            return;
        };
        if current != place.ty {
            self.push(
                site,
                RirVerifyErrorKind::TypeMismatch {
                    expected: current,
                    found: place.ty,
                },
            );
        }
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
            Self::function_local_param_abi(function, local),
            Some(RirParamAbi::StackCell | RirParamAbi::HeapCell | RirParamAbi::ScopedPlaceCell)
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
                        RirMutPlaceAccess::Handle(RirMutPlaceHandle::Param { local: found, .. })
                            if found == local
                    ),
                    RirScopedPlaceSource::ForRefAlias { .. } => false,
                }
        })
    }

    fn function_local_is_mut_place_param(function: &RirFunction, local: RirLocalId) -> bool {
        Self::function_local_param_abi(function, local) == Some(RirParamAbi::MutPlace)
    }

    fn place_is_mut_place_param_root(function: &RirFunction, place: &RirPlace) -> bool {
        let RirPlaceRoot::Local(local) = place.root else {
            return false;
        };
        place.projections.is_empty() && Self::function_local_is_mut_place_param(function, local)
    }

    fn projected_mut_place_arg_supported(
        &self,
        root_ty: RirTypeId,
        projections: &[RirProjection],
        allow_collections: bool,
    ) -> bool {
        RirPlaceModel::new(self.program).mut_place_supported(
            root_ty,
            projections,
            allow_collections,
        )
    }

    fn function_local_param_abi(function: &RirFunction, local: RirLocalId) -> Option<RirParamAbi> {
        function
            .params
            .iter()
            .find(|param| param.local == local)
            .map(|param| param.abi)
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
            .find(|(_, param)| param.local == local)
        else {
            return false;
        };
        self.function_param_maps_cell(function.id, param_index, cell)
    }

    fn param_uses_cell_mode(param: RirParam, storage: RirCellStorage) -> bool {
        let (semantic, abi) = Self::cell_capture_modes(storage);
        param.semantic == semantic || param.abi == abi
    }

    fn function_param_is_cell_capture(
        &self,
        function: RirFunctionId,
        param_index: usize,
        param: RirParam,
        storage: RirCellStorage,
    ) -> bool {
        let (semantic, abi) = Self::cell_capture_modes(storage);
        param.semantic == semantic
            && param.abi == abi
            && param.escape == RirParamEscape::NonEscaping
            && self.function_lambdas_match(function, |lambda| {
                matches!(lambda.captures.get(param_index), Some(capture)
                    if capture.ty == param.ty
                        && capture.semantic == semantic
                        && capture.abi == abi
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
            .find(|(_, param)| param.local == local)
        else {
            return false;
        };
        self.function_param_maps_scoped_place_cell(function.id, param_index, cell)
    }

    fn function_param_is_scoped_place_cell_capture(
        &self,
        function: RirFunctionId,
        param_index: usize,
        param: RirParam,
    ) -> bool {
        param.semantic == RirParamSemantic::ScopedPlaceCell
            && param.abi == RirParamAbi::ScopedPlaceCell
            && param.escape == RirParamEscape::NonEscaping
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
                            semantic: RirParamSemantic::ScopedPlaceCell,
                            abi: RirParamAbi::ScopedPlaceCell,
                            kind: RirLambdaCaptureKind::ScopedPlaceCell { .. },
                        }) if *ty == param.ty
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
        match self.ty(ty) {
            Some(RirType::Struct(id)) if self.program.structs.get(id.index()).is_some() => {
                RirRustRepPolicy::new(self.program).copyable(ty)
                    && self.inherently_copyable_type(ty)
            }
            Some(RirType::Enum(id)) if self.program.enums.get(id.index()).is_some() => {
                RirRustRepPolicy::new(self.program).copyable(ty)
                    && self.inherently_copyable_type(ty)
            }
            Some(RirType::Tuple(id)) if self.program.tuples.get(id.index()).is_some() => {
                RirRustRepPolicy::new(self.program).copyable(ty)
                    && self.inherently_copyable_type(ty)
            }
            Some(RirType::Array { .. }) => self.inherently_copyable_type(ty),
            Some(_) => RirRustRepPolicy::new(self.program).copyable(ty),
            None => false,
        }
    }

    fn inherently_copyable_type(&self, ty: RirTypeId) -> bool {
        match self.ty(ty) {
            Some(
                RirType::Int
                | RirType::Float
                | RirType::Bool
                | RirType::Char
                | RirType::Void
                | RirType::Flag(_),
            ) => true,
            Some(RirType::Struct(id)) => {
                self.program.structs.get(id.index()).is_some_and(|strukt| {
                    strukt
                        .fields
                        .iter()
                        .all(|field| self.inherently_copyable_type(field.ty))
                })
            }
            Some(RirType::Enum(id)) => self.program.enums.get(id.index()).is_some_and(|enm| {
                enm.variants.iter().all(|variant| {
                    variant
                        .fields
                        .iter()
                        .all(|field| self.inherently_copyable_type(field.ty))
                })
            }),
            Some(RirType::Tuple(id)) => self.program.tuples.get(id.index()).is_some_and(|tuple| {
                tuple
                    .fields
                    .iter()
                    .all(|field| self.inherently_copyable_type(field.ty))
            }),
            Some(RirType::Array { elem, .. }) => self.inherently_copyable_type(elem),
            Some(RirType::Option(inner)) => self.inherently_copyable_type(inner),
            Some(
                RirType::DataRef(_)
                | RirType::List(_)
                | RirType::Map { .. }
                | RirType::Slice(_)
                | RirType::Lambda(_)
                | RirType::String,
            )
            | None => false,
        }
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

pub fn source_param_semantic(
    program: &air::Program,
    ty: air::TypeId,
    mode: air::ParamMode,
) -> RirParamSemantic {
    match mode {
        air::ParamMode::Value => RirParamSemantic::Value,
        air::ParamMode::SharedBorrow => RirParamSemantic::SharedBorrow,
        air::ParamMode::MutBorrow
            if matches!(program.type_arena.data(ty), air::TypeData::Dyn(_)) =>
        {
            RirParamSemantic::DynBorrow
        }
        air::ParamMode::MutBorrow => RirParamSemantic::MutPlace,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn ty(index: usize) -> RirTypeId {
        RirTypeId::from_index(index)
    }

    fn local(index: usize) -> RirLocalId {
        RirLocalId::from_index(index)
    }

    fn dynamic_carrier_program() -> RirProgram {
        RirProgram {
            types: vec![RirType::Int, RirType::Enum(RirEnumId::from_index(0))],
            enums: vec![RirEnum {
                id: RirEnumId::from_index(0),
                air_id: None,
                native_path: None,
                native_key: None,
                core: None,
                repr: RirEnumRepr::Adt,
                raw_type: None,
                symbol: RirSymbol::new("Dyn0"),
                display: RirSymbol::new("Dyn0"),
                copyable: false,
                variants: vec![RirVariant {
                    id: RirVariantId::from_index(0),
                    symbol: RirSymbol::new("V0"),
                    display: RirSymbol::new("V0"),
                    kind: RirVariantKind::Tuple,
                    raw_value: None,
                    fields: vec![RirField {
                        id: RirFieldId::from_index(0),
                        symbol: RirSymbol::new("0"),
                        ty: ty(0),
                    }],
                }],
            }],
            dyn_origins: RirDynOrigins {
                surfaces: vec![air::ContractSurfaceId::from_index(0)],
                witnesses: vec![RirDynWitnessOrigin {
                    air_witness: air::ContractWitnessId::from_index(0),
                    surface: air::ContractSurfaceId::from_index(0),
                    concrete_ty: ty(0),
                    storage: RirDynStorage::Inline,
                    payload: RirDynPayloadAction::Copy,
                }],
                weakenings: vec![],
                slots: vec![],
                dispatches: vec![],
            },
            dyn_carriers: vec![RirDynCarrier {
                id: RirDynCarrierId::from_index(0),
                air_surface: air::ContractSurfaceId::from_index(0),
                storage_ty: ty(1),
                variants: vec![RirDynVariant {
                    id: RirDynVariantId::from_index(0),
                    air_witness: air::ContractWitnessId::from_index(0),
                    concrete_ty: ty(0),
                    storage: RirDynStorage::Inline,
                    payload: RirDynPayloadAction::Copy,
                }],
            }],
            ..RirProgram::default()
        }
    }

    fn verify_borrow(
        program: &RirProgram,
        function: &RirFunction,
        borrow: &RirDynBorrow,
    ) -> Vec<RirVerifyError> {
        let mut cx = VerifyCx {
            program,
            errors: vec![],
            initialized: vec![true; function.locals.len()],
            possibly_initialized: vec![true; function.locals.len()],
            payload_ref_owned: vec![false; function.locals.len()],
            lambda_escapes: vec![None; function.locals.len()],
            lambda_values: vec![RirFunctionValueState::NonFunction; function.locals.len()],
            loop_lambda_scopes: vec![None; function.locals.len()],
            local_decl_scopes: vec![Some(0); function.locals.len()],
            initialized_cells: vec![],
            possibly_initialized_cells: vec![],
            global_initialized: vec![],
            global_values: vec![],
            loops: vec![],
            scope_depth: 0,
            collection_loans: vec![],
            active_dyn_reborrows: vec![],
        };
        cx.check_dyn_borrow(RirVerifySite::Program, function.id, function, borrow);
        cx.errors
    }

    fn borrowed_param_function(escape: RirParamEscape) -> RirFunction {
        RirFunction {
            id: RirFunctionId::from_index(0),
            air_id: None,
            symbol: RirSymbol::new("borrowed"),
            params: vec![RirParam {
                local: local(0),
                ty: ty(1),
                semantic: RirParamSemantic::DynBorrow,
                abi: RirParamAbi::DynBorrow,
                escape,
            }],
            ret: RirReturn { ty: ty(0) },
            locals: vec![RirLocal {
                id: local(0),
                ty: ty(1),
                mutable: true,
                symbol: RirSymbol::new("borrow"),
                initialized: true,
                payload_ref: false,
            }],
            body: RirStructuredBlock::default(),
        }
    }

    #[test]
    fn dynamic_borrow_verifier_rejects_rematch_weakening_and_escape() {
        let mut program = dynamic_carrier_program();
        let function = borrowed_param_function(RirParamEscape::NonEscaping);
        let mut borrow = RirDynBorrow {
            source: RirDynBorrowSource::Borrowed {
                local: local(0),
                carrier: RirDynCarrierId::from_index(0),
            },
            target: RirDynCarrierId::from_index(0),
            air_weakening: None,
        };
        let errors = verify_borrow(&program, &function, &borrow);
        assert!(errors.is_empty(), "{errors:?}");

        program.dyn_carriers.push(RirDynCarrier {
            id: RirDynCarrierId::from_index(1),
            air_surface: air::ContractSurfaceId::from_index(1),
            storage_ty: ty(1),
            variants: vec![],
        });
        borrow.target = RirDynCarrierId::from_index(1);
        assert!(
            verify_borrow(&program, &function, &borrow)
                .iter()
                .any(|error| error.kind == RirVerifyErrorKind::InvalidDynOrigin)
        );

        let weakening = air::ContractWeakeningId::from_index(0);
        program.dyn_origins.weakenings.push(RirDynWeakeningOrigin {
            air_weakening: weakening,
            source: air::ContractSurfaceId::from_index(0),
            target: air::ContractSurfaceId::from_index(1),
        });
        program.dyn_weakenings.push(RirDynWeakening {
            air_id: weakening,
            source: RirDynCarrierId::from_index(0),
            target: RirDynCarrierId::from_index(1),
            arms: vec![],
        });
        borrow.air_weakening = Some(weakening);
        assert!(verify_borrow(&program, &function, &borrow).is_empty());

        let escaping = borrowed_param_function(RirParamEscape::Escaping);
        assert!(
            verify_borrow(&program, &escaping, &borrow)
                .iter()
                .any(|error| error.kind == RirVerifyErrorKind::InvalidDynOrigin)
        );
    }

    #[test]
    fn dynamic_borrow_verifier_rejects_concrete_and_owned_mismatch() {
        let program = dynamic_carrier_program();
        let mut concrete_function = borrowed_param_function(RirParamEscape::NonEscaping);
        concrete_function.params.clear();
        concrete_function.locals[0].ty = ty(0);
        let concrete_place = RirMutPlaceArg::from_handle(
            RirMutPlaceHandle::Local {
                local: local(0),
                ty: ty(0),
            },
            vec![],
            ty(0),
        );
        let mut concrete = RirDynBorrow {
            source: RirDynBorrowSource::Concrete {
                place: concrete_place,
                carrier: RirDynCarrierId::from_index(0),
                air_witness: air::ContractWitnessId::from_index(0),
            },
            target: RirDynCarrierId::from_index(0),
            air_weakening: None,
        };
        assert!(verify_borrow(&program, &concrete_function, &concrete).is_empty());
        let RirDynBorrowSource::Concrete { air_witness, .. } = &mut concrete.source else {
            unreachable!()
        };
        *air_witness = air::ContractWitnessId::from_index(1);
        assert!(
            verify_borrow(&program, &concrete_function, &concrete)
                .iter()
                .any(|error| error.kind == RirVerifyErrorKind::InvalidDynOrigin)
        );

        let mut owned_function = borrowed_param_function(RirParamEscape::NonEscaping);
        owned_function.params.clear();
        let mut owned = RirDynBorrow {
            source: RirDynBorrowSource::Owned {
                place: RirMutPlaceArg::from_handle(
                    RirMutPlaceHandle::Local {
                        local: local(0),
                        ty: ty(1),
                    },
                    vec![],
                    ty(1),
                ),
                carrier: RirDynCarrierId::from_index(0),
            },
            target: RirDynCarrierId::from_index(0),
            air_weakening: None,
        };
        assert!(verify_borrow(&program, &owned_function, &owned).is_empty());
        let RirDynBorrowSource::Owned { place, .. } = &mut owned.source else {
            unreachable!()
        };
        place.ty = ty(0);
        assert!(
            verify_borrow(&program, &owned_function, &owned)
                .iter()
                .any(|error| error.kind == RirVerifyErrorKind::InvalidDynStorage)
        );
    }

    #[test]
    fn dynamic_origin_verifier_attests_against_air() {
        let mut air = air::Program::default();
        let int = air.alloc_type(air::TypeData::Int);
        let surface = air.alloc_contract_surface(air::ContractSurfaceDecl {
            display_name: "C".into(),
            slots: vec![],
        });
        air.alloc_type(air::TypeData::Dyn(surface));
        air.alloc_contract_witness(air::ContractWitnessDecl {
            key: air::ContractWitnessKey {
                concrete_ty: int,
                surface,
                slots: vec![],
            },
        });
        let mut program = dynamic_carrier_program();
        assert!(verify_with_air(&program, &air).is_ok());

        program.types[0] = RirType::Bool;
        assert!(
            verify_with_air(&program, &air)
                .unwrap_err()
                .iter()
                .any(|error| { error.kind == RirVerifyErrorKind::InvalidDynOrigin })
        );
        program.types[0] = RirType::Int;
        program.dyn_origins.witnesses[0].surface = air::ContractSurfaceId::from_index(1);
        assert!(
            verify_with_air(&program, &air)
                .unwrap_err()
                .iter()
                .any(|error| { error.kind == RirVerifyErrorKind::InvalidDynOrigin })
        );
    }

    #[test]
    fn dynamic_carrier_verifier_rejects_bad_physical_identity() {
        let mut program = dynamic_carrier_program();
        assert!(verify(&program).is_ok());

        program.dyn_carriers[0].variants[0].air_witness = air::ContractWitnessId::from_index(1);
        assert!(
            verify(&program)
                .unwrap_err()
                .iter()
                .any(|error| { error.kind == RirVerifyErrorKind::InvalidDynOrigin })
        );
        program.dyn_carriers[0].variants[0].air_witness = air::ContractWitnessId::from_index(0);
        program.dyn_carriers[0].storage_ty = ty(0);
        assert!(
            verify(&program)
                .unwrap_err()
                .iter()
                .any(|error| { error.kind == RirVerifyErrorKind::InvalidDynStorage })
        );
        program.dyn_carriers[0].storage_ty = ty(1);
        program.enums[0].variants[0].fields[0].ty = ty(1);
        assert!(
            verify(&program)
                .unwrap_err()
                .iter()
                .any(|error| { error.kind == RirVerifyErrorKind::InvalidDynStorage })
        );
    }

    #[test]
    fn dynamic_pack_verifier_rejects_plan_drift() {
        let mut program = dynamic_carrier_program();
        program.consts = vec![RirConst {
            id: RirConstId::from_index(0),
            ty: ty(0),
            value: RirConstValue::Int(1),
        }];
        program.functions = vec![RirFunction {
            id: RirFunctionId::from_index(0),
            air_id: None,
            symbol: RirSymbol::new("pack"),
            params: vec![],
            ret: RirReturn { ty: ty(1) },
            locals: vec![RirLocal {
                id: local(0),
                ty: ty(1),
                mutable: false,
                symbol: RirSymbol::new("packed"),
                initialized: false,
                payload_ref: false,
            }],
            body: RirStructuredBlock {
                stmts: vec![RirStmt::Init {
                    local: local(0),
                    value: RirRValue::DynPack {
                        carrier: RirDynCarrierId::from_index(0),
                        variant: RirDynVariantId::from_index(0),
                        air_witness: air::ContractWitnessId::from_index(0),
                        air_use: air::DynOwnedUse::ReusableRead,
                        air_local: None,
                        value: RirOperand::Const(RirConstId::from_index(0)),
                        action: RirDynPayloadAction::Copy,
                        ty: ty(1),
                    },
                }],
                term: RirTerm::Return(Some(RirOperand::Place(RirPlace::local(
                    local(0),
                    vec![],
                    ty(1),
                )))),
            },
        }];
        program.entry = Some(RirFunctionId::from_index(0));
        assert!(verify(&program).is_ok());

        let RirStmt::Init { value, .. } = &mut program.functions[0].body.stmts[0] else {
            unreachable!()
        };
        let RirRValue::DynPack { action, .. } = value else {
            unreachable!()
        };
        *action = RirDynPayloadAction::Move;
        assert!(
            verify(&program)
                .unwrap_err()
                .iter()
                .any(|error| { error.kind == RirVerifyErrorKind::InvalidDynVariant })
        );
    }

    #[test]
    fn dynamic_rvalue_traversal_preserves_roles() {
        let value = RirOperand::Const(RirConstId::from_index(0));
        let pack = RirRValue::DynPack {
            carrier: RirDynCarrierId::from_index(0),
            variant: RirDynVariantId::from_index(0),
            air_witness: air::ContractWitnessId::from_index(0),
            air_use: air::DynOwnedUse::ConsumeTemporary,
            air_local: Some(air::LocalId::from_index(0)),
            value,
            action: RirDynPayloadAction::Move,
            ty: ty(0),
        };
        let mut roles = vec![];
        pack.for_each_child(RirValueUse::Read, &mut |child| {
            if let RirChild::Operand { use_, .. } = child {
                roles.push(use_);
            }
        });
        assert_eq!(roles, vec![RirValueUse::Consume]);

        let call = RirRValue::DynCall {
            carrier: RirDynCarrierId::from_index(0),
            exact_variant: None,
            air_slot: air::ContractSlotId::from_index(0),
            receiver: RirDynReceiver::Owned {
                value: RirOperand::Const(RirConstId::from_index(0)),
                consume: false,
            },
            args: vec![RirCallArg::Value(RirOperand::Const(
                RirConstId::from_index(1),
            ))],
            arms: vec![],
            ty: ty(0),
        };
        let mut children = vec![];
        call.for_each_child(RirValueUse::Read, &mut |child| match child {
            RirChild::Operand { use_, .. } => children.push(("operand", Some(use_))),
            RirChild::CallArg(_) => children.push(("arg", None)),
            _ => {}
        });
        assert_eq!(
            children,
            vec![("operand", Some(RirValueUse::Read)), ("arg", None)]
        );
    }

    #[test]
    fn statement_traversal_preserves_match_and_tail_roles() {
        let eval = RirStmt::Eval(RirRValue::Use(RirOperand::Const(RirConstId::from_index(0))));
        let mut eval_role = None;
        eval.for_each_child(&mut |child| {
            if let RirChild::Operand { use_, .. } = child {
                eval_role = Some(use_);
            }
        });
        assert_eq!(eval_role, Some(RirValueUse::Read));

        let block = RirStructuredBlock {
            stmts: vec![],
            term: RirTerm::Return(Some(RirOperand::Const(RirConstId::from_index(0)))),
        };
        let mut return_role = None;
        block.for_each_child(&mut |child| {
            if let RirChild::Operand { use_, .. } = child {
                return_role = Some(use_);
            }
        });
        assert_eq!(return_role, Some(RirValueUse::Consume));

        let option = RirStmt::OptionMatch(RirOptionMatch {
            subject: RirOptionSubject::MutPlace(RirMutPlaceArg::local(RirPlace::local(
                local(0),
                vec![],
                ty(0),
            ))),
            payload: Some(local(1)),
            payload_ref: true,
            payload_escapes: false,
            some_block: RirStructuredBlock::default(),
            none_block: RirStructuredBlock::default(),
        });
        let mut option_role = None;
        option.for_each_child(&mut |child| {
            if let RirChild::MutPlace { use_, .. } = child {
                option_role = Some(use_);
            }
        });
        assert_eq!(
            option_role,
            Some(RirPlaceUse::Borrow(RirParamAbi::MutBorrow))
        );

        let slice = RirRValue::SliceView {
            source: RirPlace::local(local(0), vec![], ty(0)),
            start: local(1),
            end: local(2),
            inclusive: false,
            mutable: false,
            ty: ty(0),
        };
        let mut slice_role = None;
        slice.for_each_child(RirValueUse::Read, &mut |child| {
            if let RirChild::Place { use_, .. } = child {
                slice_role = Some(use_);
            }
        });
        assert_eq!(
            slice_role,
            Some(RirPlaceUse::Borrow(RirParamAbi::SharedBorrow))
        );
    }

    #[test]
    fn place_model_allows_nested_slice_mut_place_projection() {
        let program = RirProgram {
            types: vec![RirType::Int, RirType::Slice(ty(0)), RirType::Slice(ty(1))],
            ..RirProgram::default()
        };
        let projections = vec![
            RirProjection::Index(local(0)),
            RirProjection::Index(local(1)),
        ];

        assert!(RirPlaceModel::new(&program).mut_place_supported(ty(2), &projections, true));
    }

    #[test]
    fn place_model_rejects_dataref_storage_without_field_root() {
        let program = RirProgram {
            types: vec![RirType::Int, RirType::DataRef(RirDataRefId::from_index(0))],
            datarefs: vec![RirDataRef {
                id: RirDataRefId::from_index(0),
                air_id: air::AggregateId::from_index(0),
                native_key: None,
                symbol: RirSymbol::new("D"),
                display: RirSymbol::new("D"),
                cycle_capable: false,
                fields: vec![RirField {
                    id: RirFieldId::from_index(0),
                    symbol: RirSymbol::new("x"),
                    ty: ty(0),
                }],
            }],
            ..RirProgram::default()
        };

        assert_eq!(
            RirPlaceModel::new(&program).dataref_storage_path(
                RirDataRefId::from_index(0),
                &[RirProjection::Index(local(0))],
            ),
            Err(RirPlaceError::Unsupported),
        );
    }

    #[test]
    fn place_model_accepts_dataref_handle_mut_place_payloads() {
        let program = RirProgram {
            types: vec![
                RirType::DataRef(RirDataRefId::from_index(0)),
                RirType::Struct(RirStructId::from_index(0)),
            ],
            ..RirProgram::default()
        };
        let model = RirPlaceModel::new(&program);

        assert!(model.dataref_mut_place_payload_supported(ty(0)));
    }

    fn stack_loop_lambda_value() -> RirOperand {
        RirOperand::Place(RirPlace::local(local(1), vec![], ty(2)))
    }

    fn stack_loop_lambda_program(stmts: Vec<RirStmt>) -> RirProgram {
        let int = ty(0);
        let void = ty(1);
        let lambda_ty = ty(2);
        let list_ty = ty(3);
        let map_ty = ty(4);
        let lambda_sig = RirLambdaSigId::from_index(0);
        let lambda_id = RirLambdaId::from_index(0);
        let lambda_function = RirFunctionId::from_index(0);
        let main_function = RirFunctionId::from_index(1);
        let cell = RirCellId::from_index(0);
        let loop_id = RirLoopId::from_index(0);
        let x = local(0);
        let callback = local(1);
        let callbacks = local(2);
        let callback_map = local(3);
        let index = local(4);
        let cell_source = local(5);

        let mut body_stmts = vec![
            RirStmt::CellInit {
                cell: RirCellRef::Owner(cell),
                value: RirRValue::Use(RirOperand::Const(RirConstId::from_index(0))),
            },
            RirStmt::Init {
                local: callback,
                value: RirRValue::Lambda {
                    lambda: lambda_id,
                    captures: vec![RirLambdaCaptureArg::StackCell {
                        cell: RirCellRef::Owner(cell),
                    }],
                    ty: lambda_ty,
                },
            },
        ];
        body_stmts.extend(stmts);

        RirProgram {
            types: vec![
                RirType::Int,
                RirType::Void,
                RirType::Lambda(lambda_sig),
                RirType::List(lambda_ty),
                RirType::Map {
                    key: int,
                    value: lambda_ty,
                },
            ],
            lambda_sigs: vec![RirLambdaSig {
                id: lambda_sig,
                params: vec![],
                ret: void,
            }],
            collection_storages: vec![
                RirCollectionStorage {
                    id: RirCollectionStorageId::from_index(0),
                    value_ty: list_ty,
                    kind: RirCollectionStorageKind::List { elem_ty: lambda_ty },
                    symbol: RirSymbol::new("ListLambda"),
                },
                RirCollectionStorage {
                    id: RirCollectionStorageId::from_index(1),
                    value_ty: map_ty,
                    kind: RirCollectionStorageKind::Map {
                        key_ty: int,
                        value_ty: lambda_ty,
                    },
                    symbol: RirSymbol::new("MapLambda"),
                },
            ],
            cells: vec![
                RirCellDecl {
                    id: cell,
                    owner: main_function,
                    source_local: x,
                    payload_ty: int,
                    storage: RirCellStorage::StackScoped,
                    lifetime: RirCellLifetime::Loop { loop_id },
                    symbol: RirSymbol::new("cell"),
                },
                RirCellDecl {
                    id: RirCellId::from_index(1),
                    owner: main_function,
                    source_local: cell_source,
                    payload_ty: lambda_ty,
                    storage: RirCellStorage::StackScoped,
                    lifetime: RirCellLifetime::Loop { loop_id },
                    symbol: RirSymbol::new("storage_cell"),
                },
            ],
            lambdas: vec![RirLambda {
                id: lambda_id,
                source: RirLambdaSource::Function(FunctionId::from_index(0)),
                function: lambda_function,
                sig: lambda_sig,
                escape: RirLambdaEscape::NonEscaping,
                storage: RirLambdaStorage::ScopedCaptures,
                captures: vec![RirLambdaCapture {
                    ty: int,
                    semantic: RirParamSemantic::StackCell,
                    abi: RirParamAbi::StackCell,
                    kind: RirLambdaCaptureKind::StackCell { cell },
                }],
            }],
            functions: vec![
                RirFunction {
                    id: lambda_function,
                    air_id: None,
                    symbol: RirSymbol::new("lambda"),
                    params: vec![RirParam {
                        local: x,
                        ty: int,
                        semantic: RirParamSemantic::StackCell,
                        abi: RirParamAbi::StackCell,
                        escape: RirParamEscape::NonEscaping,
                    }],
                    ret: RirReturn { ty: void },
                    locals: vec![RirLocal {
                        id: x,
                        ty: int,
                        mutable: false,
                        symbol: RirSymbol::new("cell_param"),
                        initialized: true,
                        payload_ref: false,
                    }],
                    body: RirStructuredBlock {
                        stmts: vec![],
                        term: RirTerm::Return(None),
                    },
                },
                RirFunction {
                    id: main_function,
                    air_id: None,
                    symbol: RirSymbol::new("main"),
                    params: vec![],
                    ret: RirReturn { ty: void },
                    locals: vec![
                        RirLocal {
                            id: x,
                            ty: int,
                            mutable: true,
                            symbol: RirSymbol::new("x"),
                            initialized: false,
                            payload_ref: false,
                        },
                        RirLocal {
                            id: callback,
                            ty: lambda_ty,
                            mutable: false,
                            symbol: RirSymbol::new("callback"),
                            initialized: false,
                            payload_ref: false,
                        },
                        RirLocal {
                            id: callbacks,
                            ty: list_ty,
                            mutable: true,
                            symbol: RirSymbol::new("callbacks"),
                            initialized: false,
                            payload_ref: false,
                        },
                        RirLocal {
                            id: callback_map,
                            ty: map_ty,
                            mutable: true,
                            symbol: RirSymbol::new("callback_map"),
                            initialized: true,
                            payload_ref: false,
                        },
                        RirLocal {
                            id: index,
                            ty: int,
                            mutable: false,
                            symbol: RirSymbol::new("index"),
                            initialized: true,
                            payload_ref: false,
                        },
                        RirLocal {
                            id: cell_source,
                            ty: lambda_ty,
                            mutable: true,
                            symbol: RirSymbol::new("cell_source"),
                            initialized: false,
                            payload_ref: false,
                        },
                    ],
                    body: RirStructuredBlock {
                        stmts: vec![RirStmt::Loop(RirLoop {
                            id: loop_id,
                            body: RirStructuredBlock {
                                stmts: body_stmts,
                                term: RirTerm::Return(None),
                            },
                        })],
                        term: RirTerm::Return(None),
                    },
                },
            ],
            consts: vec![RirConst {
                id: RirConstId::from_index(0),
                ty: int,
                value: RirConstValue::Int(0),
            }],
            entry: Some(main_function),
            ..RirProgram::default()
        }
    }

    fn expect_unsupported_loop_lambda(program: &RirProgram) {
        let errors = verify(program).expect_err("stack loop-cell lambda must fail");
        assert!(
            errors
                .iter()
                .any(|error| error.kind == RirVerifyErrorKind::UnsupportedLambdaCell)
        );
    }

    #[test]
    fn rejects_stack_loop_lambda_in_list_literal() {
        expect_unsupported_loop_lambda(&stack_loop_lambda_program(vec![RirStmt::Init {
            local: local(2),
            value: RirRValue::List {
                ty: ty(3),
                elems: vec![stack_loop_lambda_value()],
            },
        }]));
    }

    #[test]
    fn rejects_stack_loop_lambda_in_cell_init() {
        expect_unsupported_loop_lambda(&stack_loop_lambda_program(vec![RirStmt::CellInit {
            cell: RirCellRef::Owner(RirCellId::from_index(1)),
            value: RirRValue::Use(stack_loop_lambda_value()),
        }]));
    }

    #[test]
    fn rejects_stack_loop_lambda_in_sequence_slot() {
        expect_unsupported_loop_lambda(&stack_loop_lambda_program(vec![
            RirStmt::Init {
                local: local(2),
                value: RirRValue::List {
                    ty: ty(3),
                    elems: vec![],
                },
            },
            RirStmt::SequenceSlotSet {
                collection: RirCollectionAccess::Direct(RirPlace::local(local(2), vec![], ty(3))),
                index: local(4),
                value: stack_loop_lambda_value(),
            },
        ]));
    }

    #[test]
    fn rejects_stack_loop_lambda_in_map_value() {
        expect_unsupported_loop_lambda(&stack_loop_lambda_program(vec![RirStmt::MapValueSet {
            map: RirCollectionAccess::Direct(RirPlace::local(local(3), vec![], ty(4))),
            index: local(4),
            value: stack_loop_lambda_value(),
        }]));
    }

    #[test]
    fn verifier_reports_bad_dynamic_local_before_later_projection_shape() {
        let array = ty(1);
        let mut program = RirProgram {
            types: vec![
                RirType::Int,
                RirType::Array {
                    elem: ty(0),
                    len: 1,
                },
            ],
            functions: vec![RirFunction {
                id: RirFunctionId::from_index(0),
                air_id: None,
                symbol: RirSymbol::new("f"),
                params: vec![],
                ret: RirReturn { ty: ty(0) },
                locals: vec![RirLocal {
                    id: local(0),
                    ty: array,
                    mutable: false,
                    symbol: RirSymbol::new("a"),
                    initialized: true,
                    payload_ref: false,
                }],
                body: RirStructuredBlock {
                    stmts: vec![],
                    term: RirTerm::Return(Some(RirOperand::Place(RirPlace::local(
                        local(0),
                        vec![
                            RirProjection::Index(local(99)),
                            RirProjection::Field(RirFieldId::from_index(0)),
                        ],
                        ty(0),
                    )))),
                },
            }],
            entry: Some(RirFunctionId::from_index(0)),
            ..RirProgram::default()
        };
        program.functions[0].id = RirFunctionId::from_index(0);

        let errors = verify(&program).expect_err("malformed RIR must fail");
        assert!(
            errors
                .iter()
                .any(|error| error.kind == RirVerifyErrorKind::BadId)
        );
    }
}
