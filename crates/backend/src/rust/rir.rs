use std::{error::Error, fmt};

use anvyx_frontend::{
    air::{self, FunctionId},
    ast::{BinaryOp, ScalarKind, UnaryOp},
};
use anvyx_runtime::{
    CallbackEscape, ExternCallbackSignature, ExternTypeExpr, ExternTypeKey, RustExternAbi,
    RustParamAbi, RustReturnAbi,
};

use super::{
    place_access::{CollectionLoanBase, CollectionLoanProjection, collection_loan_step_supported},
    rep_policy::{RustMaterialIntent, RustMaterialSource, RustMaterialization, RustRepPolicy},
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
rir_id!(RirLocalId);
rir_id!(RirLoopId);
rir_id!(RirStructId);
rir_id!(RirDataRefId);
rir_id!(RirEnumId);
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
    pub tuples: Vec<RirTuple>,
    pub lambda_sigs: Vec<RirLambdaSig>,
    pub lambdas: Vec<RirLambda>,
    pub lambda_envs: Vec<RirLambdaEnvLayout>,
    pub collection_storages: Vec<RirCollectionStorage>,
    pub cells: Vec<RirCellDecl>,
    pub scoped_place_cells: Vec<RirScopedPlaceCellDecl>,
    pub stringify_reqs: Vec<RirStringifyReq>,
    pub stringify_helpers: Vec<RirStringifyHelper>,
    pub consts: Vec<RirConst>,
    pub entry: Option<RirFunctionId>,
}

impl RirProgram {
    pub fn lambdas_for_sig(&self, sig: RirLambdaSigId) -> impl Iterator<Item = &RirLambda> {
        self.lambdas.iter().filter(move |lambda| lambda.sig == sig)
    }

    pub fn collection_storage_for(&self, value_ty: RirTypeId) -> Option<&RirCollectionStorage> {
        self.collection_storages
            .iter()
            .find(|storage| storage.value_ty == value_ty)
    }

    pub fn option_ty(&self, inner: RirTypeId) -> Option<RirTypeId> {
        self.types
            .iter()
            .position(|ty| matches!(ty, RirType::Option(found) if *found == inner))
            .map(RirTypeId::from_index)
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RirEnumRepr {
    Adt,
    RawInt,
    RawString,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum RirRawEnumValue {
    Int(i64),
    String(String),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RirCoreEnumKind {
    Option,
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
    pub symbol: RirSymbol,
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
    pub source_local: RirLocalId,
    pub payload_ty: RirTypeId,
    pub symbol: RirSymbol,
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
    Structural(RirStringifyHelperId),
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
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RirCtxPlan {
    pub types_symbol: RirSymbol,
    pub globals_symbol: RirSymbol,
}

impl Default for RirCtxPlan {
    fn default() -> Self {
        Self {
            types_symbol: RirSymbol::new("AnvTypes"),
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
    ScopedLambda,
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
    ScopedLambda,
    StackCell,
    HeapCell,
    ScopedPlaceCell,
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
    MapValueSet {
        map: RirPlace,
        index: RirLocalId,
        value: RirOperand,
    },
    Eval(RirRValue),
    If(RirIf),
    Loop(RirLoop),
    CollectionLoanScope(RirCollectionLoanScope),
    CollectionSlotScope(RirStructuredBlock),
    EnumMatch(RirEnumMatch),
    OptionMatch(RirOptionMatch),
}

pub(super) fn stmt_child_blocks_any(
    stmt: &RirStmt,
    mut block_matches: impl FnMut(&RirStructuredBlock) -> bool,
) -> bool {
    match stmt {
        RirStmt::If(branch) => {
            block_matches(&branch.then_block)
                || branch.else_block.as_ref().is_some_and(block_matches)
        }
        RirStmt::Loop(loop_) => block_matches(&loop_.body),
        RirStmt::CollectionLoanScope(scope) => block_matches(&scope.body),
        RirStmt::CollectionSlotScope(block) => block_matches(block),
        RirStmt::OptionMatch(match_) => {
            block_matches(&match_.some_block) || block_matches(&match_.none_block)
        }
        RirStmt::EnumMatch(match_) => {
            match_.arms.iter().any(|arm| block_matches(&arm.block))
                || match_.else_block.as_ref().is_some_and(block_matches)
        }
        RirStmt::Init { .. }
        | RirStmt::GlobalEnsure { .. }
        | RirStmt::GlobalSetRoot { .. }
        | RirStmt::GlobalUpdateRoot { .. }
        | RirStmt::MutPlaceSet { .. }
        | RirStmt::Assign { .. }
        | RirStmt::CellInit { .. }
        | RirStmt::CellSet { .. }
        | RirStmt::ScopedPlaceCellSet { .. }
        | RirStmt::Eval(_)
        | RirStmt::DataRefSet { .. }
        | RirStmt::MapValueSet { .. } => false,
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct RirCollectionLoanScope {
    pub root: RirPlace,
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

#[derive(Debug, Clone, PartialEq)]
pub struct RirEnumMatch {
    pub discr: RirPlace,
    pub arms: Vec<RirEnumMatchArm>,
    pub else_block: Option<RirStructuredBlock>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct RirEnumMatchArm {
    pub variant: RirVariantId,
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
pub enum RirOptionSubject {
    Place(RirPlace),
    MutPlace(RirMutPlaceArg),
}

#[derive(Debug, Clone, PartialEq)]
pub enum RirRValue {
    Use(RirOperand),
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
    ListPush {
        list: RirPlace,
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
        map: RirPlace,
        key: RirOperand,
        ty: RirTypeId,
    },
    MapInsert {
        map: RirPlace,
        key: RirOperand,
        value: RirOperand,
    },
    MapRemove {
        map: RirPlace,
        key: RirOperand,
        ty: RirTypeId,
    },
    MapEntryAt {
        map: RirPlace,
        index: RirLocalId,
        ty: RirTypeId,
    },
    MapValueAt {
        map: RirPlace,
        index: RirLocalId,
        ty: RirTypeId,
    },
    Lambda {
        lambda: RirLambdaId,
        captures: Vec<RirLambdaCaptureArg>,
        ty: RirTypeId,
    },
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

#[derive(Debug, Clone, PartialEq)]
pub enum RirCallArg {
    Value(RirOperand),
    SharedBorrow(RirPlace),
    SharedStringConst(RirConstId),
    MutBorrow(RirPlace),
    MutPlace(RirMutPlaceArg),
    ScopedLambda {
        callee: RirOperand,
        sig: RirLambdaSigId,
    },
}

impl RirCallArg {
    pub fn semantic(&self) -> RirParamSemantic {
        match self {
            Self::Value(_) => RirParamSemantic::Value,
            Self::SharedBorrow(_) | Self::SharedStringConst(_) => RirParamSemantic::SharedBorrow,
            Self::MutBorrow(_) => RirParamSemantic::MutBorrow,
            Self::MutPlace(_) => RirParamSemantic::MutPlace,
            Self::ScopedLambda { .. } => RirParamSemantic::ScopedLambda,
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
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RirProjection {
    Field(RirFieldId),
    TupleField(RirFieldId),
    Index(RirLocalId),
    MapIndex(RirLocalId),
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
    Void,
    Struct(RirStructId),
    DataRef(RirDataRefId),
    Enum(RirEnumId),
    Tuple(RirTupleId),
    Array { elem: RirTypeId, len: u64 },
    List(RirTypeId),
    Map { key: RirTypeId, value: RirTypeId },
    Option(RirTypeId),
    Slice(RirTypeId),
    Lambda(RirLambdaSigId),
}

#[derive(Debug, Clone, PartialEq)]
pub struct RirConst {
    pub id: RirConstId,
    pub ty: RirTypeId,
    pub value: RirConstValue,
}

#[derive(Debug, Clone, PartialEq)]
pub enum RirConstValue {
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),
    Nil,
}

#[derive(Debug, Clone, PartialEq)]
pub struct RirExtern {
    pub id: RirExternId,
    pub symbol: RirSymbol,
    pub kind: RirExternKind,
    pub params: Vec<RirExternParam>,
    pub ret: RirTypeId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RirExternKind {
    Native(RirNativeExtern),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RirNativeExtern {
    pub path: Vec<String>,
    pub abi: RustExternAbi,
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
        initialized_cells: vec![],
        possibly_initialized_cells: vec![],
        global_initialized: vec![],
        loops: vec![],
        collection_loans: vec![],
    };
    cx.check();
    if cx.errors.is_empty() {
        Ok(VerifiedRirProgram { program })
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
    Context,
    Global(RirGlobalId),
    Type(RirTypeId),
    Const(RirConstId),
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
    MapIndex { local: RirLocalId, key: RirTypeId },
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
    ) -> Option<(bool, bool)> {
        let mut ty = root_ty;
        let mut fallible = false;
        for projection in projections {
            match (self.ty(ty), projection) {
                (
                    Some(RirType::Array { .. } | RirType::List(_) | RirType::Slice(_)),
                    RirProjection::Index(_),
                )
                | (Some(RirType::Map { .. }), RirProjection::MapIndex(_)) => fallible = true,
                _ => {}
            }
            let Ok(step) = self.step(ty, *projection, true) else {
                return None;
            };
            ty = step.ty;
        }
        Some((fallible, fallible))
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
                        | (Some(RirType::Map { .. }), RirProjection::MapIndex(_))
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
            RustRepPolicy::new(self.program).materialization_for(
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
            (_, RirProjection::Index(_) | RirProjection::MapIndex(_)) => {
                Err(RirPlaceError::Unsupported)
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
            (Some(RirType::Map { key, value }), RirProjection::MapIndex(local)) => {
                if !allow_collections {
                    return Err(RirPlaceError::Unsupported);
                }
                let slot = self
                    .program
                    .option_ty(value)
                    .ok_or(RirPlaceError::Unsupported)?;
                (slot, RirProjectionKind::MapIndex { local, key })
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
        RirProjection::MapIndex(_) => CollectionLoanProjection::Other,
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
    DuplicateMatchArm,
    MatchNotExhaustive,
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

type RirBlockState = (
    Vec<bool>,
    Vec<bool>,
    Vec<Option<RirLambdaEscape>>,
    Vec<bool>,
    Vec<bool>,
    Vec<bool>,
);

struct VerifyCx<'a> {
    program: &'a RirProgram,
    errors: Vec<RirVerifyError>,
    initialized: Vec<bool>,
    possibly_initialized: Vec<bool>,
    payload_ref_owned: Vec<bool>,
    lambda_escapes: Vec<Option<RirLambdaEscape>>,
    initialized_cells: Vec<bool>,
    possibly_initialized_cells: Vec<bool>,
    global_initialized: Vec<bool>,
    loops: Vec<RirLoopId>,
    collection_loans: Vec<ActiveRirCollectionLoan>,
}

#[derive(Debug, Clone)]
struct ActiveRirCollectionLoan {
    root: RirPlace,
}

fn place_prefix(prefix: &RirPlace, place: &RirPlace) -> bool {
    prefix.root == place.root
        && prefix.projections.len() <= place.projections.len()
        && prefix
            .projections
            .iter()
            .zip(&place.projections)
            .all(|(prefix, projection)| prefix == projection)
}

fn native_extern_signature_ok(
    program: &RirProgram,
    native: &RirNativeExtern,
    ext: &RirExtern,
    void: Option<RirTypeId>,
) -> bool {
    !native.path.is_empty()
        && native.path.iter().all(|segment| !segment.is_empty())
        && rust_extern_abi_supported(&native.abi)
        && native.abi.params.len() == ext.params.len()
        && native
            .abi
            .params
            .iter()
            .zip(&ext.params)
            .all(|(abi, param)| native_param_abi_ok(program, abi, *param))
        && native_return_abi_ok(program, &native.abi.ret, ext.ret, void)
}

pub(super) fn rust_extern_abi_supported(abi: &RustExternAbi) -> bool {
    match abi.support {
        anvyx_runtime::RustAbiSupport::Direct => {
            abi.ctx == anvyx_runtime::RustWrapperCtx::HiddenRuntime
                && !abi.params.iter().any(|param| {
                    param.is_scoped_lambda()
                        || param.contains_collection_wrapper()
                        || param.direct_collection_abi()
                })
                && !abi.ret.contains_collection_wrapper()
                && !abi.ret.direct_collection_abi()
        }
        anvyx_runtime::RustAbiSupport::NeedsWrapperConversion => {
            scoped_lambda_wrapper_supported(abi) || abi.supported_collection_wrapper()
        }
        anvyx_runtime::RustAbiSupport::Unsupported => false,
    }
}

fn scoped_lambda_wrapper_supported(abi: &RustExternAbi) -> bool {
    abi.ctx == anvyx_runtime::RustWrapperCtx::None
        && abi.has_scoped_lambda()
        && !abi.params.iter().any(|param| {
            matches!(
                param,
                RustParamAbi::Borrow(_) | RustParamAbi::MutBorrow(_) | RustParamAbi::MutPlace(_)
            ) || param.contains_collection_wrapper()
                || param.direct_collection_abi()
        })
        && !abi.ret.contains_collection_wrapper()
        && !abi.ret.direct_collection_abi()
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) struct NativeParamAbi {
    pub semantic: RirParamSemantic,
    pub abi: RirParamAbi,
}

pub(super) fn rust_param_abi(abi: &RustParamAbi) -> Option<NativeParamAbi> {
    let (semantic, abi) = match abi {
        RustParamAbi::Value(_) => (RirParamSemantic::Value, RirParamAbi::Value),
        RustParamAbi::Borrow(_) => (RirParamSemantic::SharedBorrow, RirParamAbi::SharedBorrow),
        RustParamAbi::MutBorrow(_) => (RirParamSemantic::MutBorrow, RirParamAbi::MutBorrow),
        RustParamAbi::MutPlace(_) => (RirParamSemantic::MutPlace, RirParamAbi::MutPlace),
        RustParamAbi::ScopedLambda(_) => {
            (RirParamSemantic::ScopedLambda, RirParamAbi::ScopedLambda)
        }
        RustParamAbi::List(_) => (RirParamSemantic::Value, RirParamAbi::Value),
        RustParamAbi::Option(_) => return None,
    };
    Some(NativeParamAbi { semantic, abi })
}

fn native_param_abi_ok(program: &RirProgram, abi: &RustParamAbi, param: RirExternParam) -> bool {
    let Some(native) = rust_param_abi(abi) else {
        return false;
    };
    param.semantic == native.semantic
        && param.abi == native.abi
        && param.escape == native_param_escape(abi)
        && match abi {
            RustParamAbi::Value(ty)
            | RustParamAbi::Borrow(ty)
            | RustParamAbi::MutBorrow(ty)
            | RustParamAbi::MutPlace(ty) => {
                !extern_type_contains_collection(ty)
                    && rir_type_matches_extern(program, param.ty, ty)
            }
            RustParamAbi::ScopedLambda(callback) => {
                rir_type_matches_callback(program, param.ty, callback)
            }
            RustParamAbi::List(inner) => {
                abi.supported_collection_wrapper()
                    && rir_type_matches_list_wrapper_param(program, param.ty, inner)
            }
            RustParamAbi::Option(_) => false,
        }
}

fn native_param_escape(_abi: &RustParamAbi) -> RirParamEscape {
    RirParamEscape::NonEscaping
}

fn rir_type_matches_callback(
    program: &RirProgram,
    ty: RirTypeId,
    callback: &ExternCallbackSignature,
) -> bool {
    if !callback.scoped_lambda_supported() {
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

fn native_return_abi_ok(
    program: &RirProgram,
    abi: &RustReturnAbi,
    ret: RirTypeId,
    void: Option<RirTypeId>,
) -> bool {
    match abi {
        RustReturnAbi::Void => Some(ret) == void,
        RustReturnAbi::Value(ty) => {
            !extern_type_contains_collection(ty) && rir_type_matches_extern(program, ret, ty)
        }
        RustReturnAbi::Option(inner) => rir_type_matches_option(program, ret, inner),
        RustReturnAbi::List(inner) => {
            abi.supported_collection_wrapper()
                && rir_type_matches_list_wrapper_return(program, ret, inner)
        }
    }
}

fn rir_type_matches_list_wrapper_param(
    program: &RirProgram,
    id: RirTypeId,
    inner: &RustParamAbi,
) -> bool {
    let RustParamAbi::Value(expected) = inner else {
        return false;
    };
    rir_type_matches_list_wrapper(program, id, expected)
}

fn rir_type_matches_list_wrapper_return(
    program: &RirProgram,
    id: RirTypeId,
    inner: &RustReturnAbi,
) -> bool {
    let RustReturnAbi::Value(expected) = inner else {
        return false;
    };
    rir_type_matches_list_wrapper(program, id, expected)
}

fn rir_type_matches_list_wrapper(
    program: &RirProgram,
    id: RirTypeId,
    expected: &ExternTypeExpr,
) -> bool {
    let Some(RirType::List(elem)) = program.types.get(id.index()) else {
        return false;
    };
    rir_type_matches_extern(program, *elem, expected)
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
        | (RirType::String, ExternTypeExpr::String) => true,
        (RirType::Struct(struct_id), ExternTypeExpr::Named { module, name, args }) => {
            let strukt = &program.structs[struct_id.index()];
            args.is_empty()
                && strukt.native_key.as_ref().is_some_and(|key| {
                    module.as_ref().is_none_or(|module| key.module == *module) && key.name == *name
                })
        }
        (RirType::List(elem), ExternTypeExpr::List(expected))
        | (RirType::Option(elem), ExternTypeExpr::Option(expected)) => {
            rir_type_matches_extern(program, *elem, expected)
        }
        (RirType::Map { key, value }, ExternTypeExpr::Map(expected_key, expected_value)) => {
            rir_type_matches_extern(program, *key, expected_key)
                && rir_type_matches_extern(program, *value, expected_value)
        }
        _ => false,
    }
}

fn rir_type_matches_option(program: &RirProgram, id: RirTypeId, inner: &RustReturnAbi) -> bool {
    let Some(RirType::Option(payload)) = program.types.get(id.index()) else {
        return false;
    };
    match inner {
        RustReturnAbi::Value(ty) => {
            !extern_type_contains_collection(ty) && rir_type_matches_extern(program, *payload, ty)
        }
        RustReturnAbi::Void | RustReturnAbi::Option(_) | RustReturnAbi::List(_) => false,
    }
}

fn extern_type_contains_collection(ty: &ExternTypeExpr) -> bool {
    match ty {
        ExternTypeExpr::List(_) | ExternTypeExpr::Map(_, _) => true,
        ExternTypeExpr::Option(inner) => extern_type_contains_collection(inner),
        ExternTypeExpr::Callback(callback) => {
            callback
                .params
                .iter()
                .any(|param| extern_type_contains_collection(&param.ty))
                || extern_type_contains_collection(&callback.ret)
        }
        ExternTypeExpr::Void
        | ExternTypeExpr::Bool
        | ExternTypeExpr::Int
        | ExternTypeExpr::Float
        | ExternTypeExpr::String
        | ExternTypeExpr::Any
        | ExternTypeExpr::Named { .. } => false,
    }
}

impl VerifyCx<'_> {
    fn check(&mut self) {
        if let Some(entry) = self.program.entry {
            self.check_function_id(RirVerifySite::Program, entry);
        }
        if self.program.ctx.types_symbol.as_str().is_empty()
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
                RirType::Tuple(id) => self.check_tuple_id(site, *id),
                RirType::Array { elem, .. } | RirType::List(elem) | RirType::Slice(elem) => {
                    self.check_type_id(site, *elem);
                    self.check_lambda_container_type(site, *elem);
                }
                RirType::Map { key, value } => {
                    self.check_type_id(site, *key);
                    self.check_type_id(site, *value);
                    self.check_lambda_container_type(site, *key);
                    self.check_lambda_container_type(site, *value);
                }
                RirType::Option(inner) => {
                    self.check_type_id(site, *inner);
                    self.check_lambda_container_type(site, *inner);
                }
                RirType::Lambda(sig) => self.check_lambda_sig_id(site, *sig),
                _ => {}
            }
        }
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
        self.check_tuples();
        self.check_stringify_helpers();
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

    fn check_globals(&mut self) {
        let mut slots = std::collections::HashSet::new();
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
            RustRepPolicy::new(self.program).materialization_for(
                ty,
                RustMaterialSource::ExactGlobalRoot,
                RustMaterialIntent::Read,
            ),
            RustMaterialization::Copy
                | RustMaterialization::Share
                | RustMaterialization::CloneHandle
        )
    }

    fn stored_payload_supported(&self, ty: RirTypeId) -> bool {
        !matches!(
            RustRepPolicy::new(self.program).materialization_for(
                ty,
                RustMaterialSource::StoredPayload,
                RustMaterialIntent::Store,
            ),
            RustMaterialization::Gap
        )
    }

    fn value_from_ref_supported(&self, ty: RirTypeId) -> bool {
        RustRepPolicy::new(self.program).value_from_ref_supported(ty)
    }

    fn check_stored_payload(&mut self, site: RirVerifySite, ty: RirTypeId) {
        self.check_type_id(site, ty);
        if self.ty(ty).is_some() && !self.stored_payload_supported(ty) {
            self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
        }
    }

    fn check_lambda_container_type(&mut self, site: RirVerifySite, ty: RirTypeId) {
        if self.type_contains_lambda(ty) {
            self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
        }
    }

    fn type_contains_lambda(&self, ty: RirTypeId) -> bool {
        self.type_contains_lambda_inner(ty, &mut Vec::new())
    }

    fn type_contains_lambda_inner(&self, ty: RirTypeId, visited: &mut Vec<RirTypeId>) -> bool {
        if visited.contains(&ty) {
            return false;
        }
        visited.push(ty);
        match self.ty(ty) {
            Some(RirType::Lambda(_)) => true,
            Some(
                RirType::Array { elem, .. }
                | RirType::List(elem)
                | RirType::Slice(elem)
                | RirType::Option(elem),
            ) => self.type_contains_lambda_inner(elem, visited),
            Some(RirType::Map { key, value }) => {
                self.type_contains_lambda_inner(key, visited)
                    || self.type_contains_lambda_inner(value, visited)
            }
            Some(RirType::Struct(id)) => {
                self.program.structs.get(id.index()).is_some_and(|strukt| {
                    strukt
                        .fields
                        .iter()
                        .any(|field| self.type_contains_lambda_inner(field.ty, visited))
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
                            .any(|field| self.type_contains_lambda_inner(field.ty, visited))
                    })
            }
            Some(RirType::Enum(id)) => self.program.enums.get(id.index()).is_some_and(|enm| {
                enm.variants.iter().any(|variant| {
                    variant
                        .fields
                        .iter()
                        .any(|field| self.type_contains_lambda_inner(field.ty, visited))
                })
            }),
            Some(RirType::Tuple(id)) => self.program.tuples.get(id.index()).is_some_and(|tuple| {
                tuple
                    .fields
                    .iter()
                    .any(|field| self.type_contains_lambda_inner(field.ty, visited))
            }),
            Some(
                RirType::Int | RirType::Float | RirType::Bool | RirType::String | RirType::Void,
            )
            | None => false,
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
                            | RirParamSemantic::StackCell
                            | RirParamSemantic::HeapCell
                            | RirParamSemantic::ScopedPlaceCell,
                        _
                    ) | (
                        _,
                        RirParamAbi::ScopedLambda
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
        let mut has_cell = false;
        let mut has_mut_borrow = false;
        for lambda in self.program.lambdas_for_sig(sig) {
            has_cell |= lambda.captures.iter().any(|capture| {
                matches!(
                    capture.kind,
                    RirLambdaCaptureKind::StackCell { .. }
                        | RirLambdaCaptureKind::HeapCell { .. }
                        | RirLambdaCaptureKind::ScopedPlaceCell { .. }
                )
            });
            has_mut_borrow |= lambda
                .captures
                .iter()
                .any(|capture| capture.semantic == RirParamSemantic::MutBorrow);
        }
        let policy = RustRepPolicy::new(self.program);
        let mixed_heap_env_and_borrows =
            policy.lambda_sig_has_heap_env(sig) && policy.lambda_sig_needs_lifetime(sig);
        if (has_cell && has_mut_borrow) || mixed_heap_env_and_borrows {
            self.push(site, RirVerifyErrorKind::UnsupportedLambdaCapture);
        }
    }

    fn check_lambdas(&mut self) {
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
                if self.ty(field.ty).is_some()
                    && matches!(field.kind, RirLambdaEnvFieldKind::Value)
                    && !self.value_from_ref_supported(field.ty)
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
                                    | RirParamSemantic::StackCell
                                    | RirParamSemantic::HeapCell
                                    | RirParamSemantic::ScopedPlaceCell,
                                _
                            ) | (
                                _,
                                RirParamAbi::MutBorrow
                                    | RirParamAbi::ScopedLambda
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
        let mut seen = std::collections::HashSet::new();
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
            Some(_) if !Self::function_local_is_mut_place_param(owner, cell.source_local) => {
                self.push(site, RirVerifyErrorKind::CallArgMode);
            }
            Some(_) => {}
            None => self.push(site, RirVerifyErrorKind::BadId),
        }
    }

    fn check_scoped_place_cell_uniqueness(&mut self) {
        for (index, cell) in self.program.scoped_place_cells.iter().enumerate() {
            let id = RirScopedPlaceCellId::from_index(index);
            for (other_index, other) in self.program.scoped_place_cells[..index].iter().enumerate()
            {
                if cell.owner == other.owner && cell.source_local == other.source_local {
                    self.push(
                        RirVerifySite::ScopedPlaceCell(id),
                        RirVerifyErrorKind::DuplicateScopedPlaceCell {
                            owner: cell.owner,
                            source_local: cell.source_local,
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
                self.check_stored_payload(site, field.ty);
                self.check_lambda_container_type(site, field.ty);
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
                self.check_stored_payload(site, field.ty);
                self.check_lambda_container_type(site, field.ty);
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
                    self.check_stored_payload(site, field.ty);
                    self.check_lambda_container_type(site, field.ty);
                    if enm.copyable && !self.copyable_type(field.ty) {
                        self.push(site, RirVerifyErrorKind::NonCopyValueRequired);
                    }
                }
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
                self.check_stored_payload(site, field.ty);
                self.check_lambda_container_type(site, field.ty);
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
                let mut raw_values = std::collections::HashSet::new();
                for variant in &enm.variants {
                    if variant.kind != RirVariantKind::Unit || !variant.fields.is_empty() {
                        self.push(site, RirVerifyErrorKind::RawEnumPayload);
                    }
                    match (enm.repr, variant.raw_value.as_ref()) {
                        (RirEnumRepr::RawInt, Some(raw @ RirRawEnumValue::Int(_)))
                        | (RirEnumRepr::RawString, Some(raw @ RirRawEnumValue::String(_))) => {
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
        for (index, req) in self.program.stringify_reqs.iter().enumerate() {
            let site = RirVerifySite::Type(req.ty);
            if req.id != RirStringifyReqId::from_index(index) {
                self.push(site, RirVerifyErrorKind::BadId);
            }
            self.check_type_id(site, req.ty);
            match (req.kind, self.ty(req.ty)) {
                (RirStringifyReqKind::Structural(helper), Some(RirType::Struct(_))) => {
                    self.check_stringify_helper_id(site, helper);
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
            match self.ty(helper.ty) {
                Some(RirType::Struct(struct_id)) => {
                    let Some(strukt) = self.program.structs.get(struct_id.index()) else {
                        self.push(site, RirVerifyErrorKind::BadId);
                        continue;
                    };
                    for field in &strukt.fields {
                        match self.ty(field.ty) {
                            Some(
                                RirType::Int | RirType::Float | RirType::Bool | RirType::String,
                            ) => {}
                            Some(RirType::Struct(_))
                                if self.stringify_req(field.ty).is_some_and(|req| {
                                    matches!(req.kind, RirStringifyReqKind::Structural(helper)
                                        if self.program.stringify_helpers.get(helper.index()).is_some_and(|helper| helper.ty == field.ty))
                                }) => {}
                            Some(_) | None => {
                                self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                            }
                        }
                    }
                }
                _ => self.push(site, RirVerifyErrorKind::UnsupportedRValueType),
            }
        }
    }

    fn check_const(&mut self, id: RirConstId, konst: &RirConst) {
        self.check_type_id(RirVerifySite::Const(id), konst.ty);
        if matches!(self.ty(konst.ty), Some(RirType::Void)) {
            self.push(RirVerifySite::Const(id), RirVerifyErrorKind::VoidConst);
        }
        let ok = matches!(
            (self.ty(konst.ty), &konst.value),
            (Some(RirType::Int), RirConstValue::Int(_))
                | (Some(RirType::Float), RirConstValue::Float(_))
                | (Some(RirType::Bool), RirConstValue::Bool(_))
                | (Some(RirType::String), RirConstValue::String(_))
                | (Some(RirType::Option(_)), RirConstValue::Nil)
        );
        if !ok {
            self.push(
                RirVerifySite::Const(id),
                RirVerifyErrorKind::ConstTypeMismatch,
            );
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
            for storage in [RirCellStorage::StackScoped, RirCellStorage::Heap] {
                if Self::param_uses_cell_mode(*param, storage)
                    && !self.function_param_is_cell_capture(id, index, *param, storage)
                {
                    self.push(site, RirVerifyErrorKind::UnsupportedAbi);
                }
            }
            if matches!(param.semantic, RirParamSemantic::ScopedLambda)
                || matches!(param.abi, RirParamAbi::ScopedLambda)
            {
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
        let previous_initialized_cells = std::mem::take(&mut self.initialized_cells);
        let previous_possible_cells = std::mem::take(&mut self.possibly_initialized_cells);
        let previous_global_initialized = std::mem::take(&mut self.global_initialized);
        self.initialized = function
            .locals
            .iter()
            .map(|local| local.initialized)
            .collect();
        self.possibly_initialized.clone_from(&self.initialized);
        self.payload_ref_owned = vec![false; function.locals.len()];
        self.lambda_escapes = vec![None; function.locals.len()];
        self.initialized_cells = vec![false; self.program.cells.len()];
        self.possibly_initialized_cells = vec![false; self.program.cells.len()];
        self.global_initialized = vec![false; self.program.globals.len()];
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
            if matches!(self.ty(param.ty), Some(RirType::Lambda(_)))
                && let Some(escape) = self.lambda_escapes.get_mut(param.local.index())
            {
                *escape = Some(RirLambdaEscape::from_param_escape(param.escape));
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
        self.initialized_cells = previous_initialized_cells;
        self.possibly_initialized_cells = previous_possible_cells;
        self.global_initialized = previous_global_initialized;
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
                let escape = self.rvalue_lambda_escape(function, value);
                self.set_local_lambda_escape(function, *local, escape);
                if let Some(initialized) = self.initialized.get_mut(local.index()) {
                    *initialized = true;
                }
                if let Some(possible) = self.possibly_initialized.get_mut(local.index()) {
                    *possible = true;
                }
            }
            RirStmt::GlobalEnsure { global } => {
                if self.check_global_id(site, *global).is_some()
                    && let Some(slot) = self.global_initialized.get_mut(global.index())
                {
                    *slot = true;
                }
            }
            RirStmt::GlobalSetRoot { global, value }
            | RirStmt::GlobalUpdateRoot { global, value } => {
                let Some(global_decl) = self.check_global_id(site, *global).cloned() else {
                    self.check_rvalue(function_id, function, index, value, None);
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
                if let Some(slot) = self.global_initialized.get_mut(global.index()) {
                    *slot = true;
                }
            }
            RirStmt::MutPlaceSet { place, value } => {
                if matches!(place.access, RirMutPlaceAccess::DataRef { .. }) {
                    self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                }
                if self.mut_place_set_replaces_active_collection_root(place) {
                    self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                }
                let ty = self.check_mut_place_arg(site, function_id, function, place);
                self.check_rvalue(function_id, function, index, value, Some(ty));
            }
            RirStmt::Assign { dst, value } => {
                self.check_place(site, function, dst);
                let Some(dst_local) = self.local_root(site, dst) else {
                    self.check_rvalue(function_id, function, index, value, Some(dst.ty));
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
                if dst.projections.is_empty() {
                    let escape = self.rvalue_lambda_escape(function, value);
                    self.set_local_lambda_escape(function, dst_local, escape);
                }
            }
            RirStmt::CellInit { cell, value } => {
                if matches!(cell, RirCellRef::Capture { .. }) {
                    self.push(site, RirVerifyErrorKind::UnsupportedLambdaCapture);
                    return;
                }
                if let Some(decl) = self.check_function_cell_ref(site, function_id, *cell) {
                    if !self.loops.is_empty() {
                        self.push(site, RirVerifyErrorKind::UnsupportedLambdaCell);
                    }
                    if self.cell_possibly_initialized(decl.id) {
                        self.push(site, RirVerifyErrorKind::InitCellTwice(decl.id));
                    }
                    self.check_rvalue(function_id, function, index, value, Some(decl.payload_ty));
                    self.mark_cell_initialized(decl.id);
                }
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
                }
            }
            RirStmt::MapValueSet { map, index, value } => {
                self.check_place(site, function, map);
                let Some(map_local) = self.local_root(site, map) else {
                    self.check_local_id(site, function, *index);
                    self.check_value_operand_ty(site, function, value, map.ty);
                    return;
                };
                self.check_local_id(site, function, *index);
                let Some(RirType::Map {
                    value: value_ty, ..
                }) = self.ty(map.ty)
                else {
                    self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                    return;
                };
                self.check_mutable_local_root(site, function, Some(map_local));
                if function
                    .locals
                    .get(index.index())
                    .is_none_or(|local| !matches!(self.ty(local.ty), Some(RirType::Int)))
                {
                    self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                }
                self.check_value_operand_ty(site, function, value, value_ty);
            }
            RirStmt::Eval(value) => {
                self.check_rvalue(function_id, function, index, value, None);
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
                let entry_definite = self.initialized.clone();
                let entry_possible = self.possibly_initialized.clone();
                let entry_lambda_escapes = self.lambda_escapes.clone();
                let entry_globals = self.global_initialized.clone();
                let then_state = self.check_structured_block(
                    function_id,
                    function,
                    &branch.then_block,
                    entry_definite.clone(),
                    entry_possible.clone(),
                    entry_lambda_escapes.clone(),
                    entry_globals.clone(),
                    None,
                );
                let else_state = branch.else_block.as_ref().map_or(
                    Some((
                        entry_definite.clone(),
                        entry_possible.clone(),
                        entry_lambda_escapes.clone(),
                        self.initialized_cells.clone(),
                        self.possibly_initialized_cells.clone(),
                        entry_globals.clone(),
                    )),
                    |else_block| {
                        self.check_structured_block(
                            function_id,
                            function,
                            else_block,
                            entry_definite.clone(),
                            entry_possible.clone(),
                            entry_lambda_escapes.clone(),
                            entry_globals.clone(),
                            None,
                        )
                    },
                );
                self.merge_structured_states([then_state, else_state]);
            }
            RirStmt::Loop(loop_) => {
                self.loops.push(loop_.id);
                self.check_structured_block(
                    function_id,
                    function,
                    &loop_.body,
                    self.initialized.clone(),
                    self.possibly_initialized.clone(),
                    self.lambda_escapes.clone(),
                    self.global_initialized.clone(),
                    None,
                );
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
                    self.initialized.clone(),
                    self.possibly_initialized.clone(),
                    self.lambda_escapes.clone(),
                    self.global_initialized.clone(),
                    None,
                );
                self.merge_structured_states([state]);
            }
            RirStmt::EnumMatch(match_) => {
                self.check_place(site, function, &match_.discr);
                let enum_id = match self.ty(match_.discr.ty) {
                    Some(RirType::Enum(id)) => Some(id),
                    _ => {
                        self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                        None
                    }
                };
                let variant_count = enum_id
                    .and_then(|id| self.program.enums.get(id.index()))
                    .map(|enm| enm.variants.len());
                let mut seen = Vec::new();
                let entry_definite = self.initialized.clone();
                let entry_possible = self.possibly_initialized.clone();
                let entry_lambda_escapes = self.lambda_escapes.clone();
                let entry_globals = self.global_initialized.clone();
                let mut states = vec![];
                for arm in &match_.arms {
                    if variant_count.is_none_or(|len| arm.variant.index() >= len) {
                        self.push(site, RirVerifyErrorKind::BadId);
                    } else if seen.contains(&arm.variant) {
                        self.push(site, RirVerifyErrorKind::DuplicateMatchArm);
                    } else {
                        seen.push(arm.variant);
                    }
                    states.push(self.check_structured_block(
                        function_id,
                        function,
                        &arm.block,
                        entry_definite.clone(),
                        entry_possible.clone(),
                        entry_lambda_escapes.clone(),
                        entry_globals.clone(),
                        None,
                    ));
                }
                if let Some(else_block) = &match_.else_block {
                    states.push(self.check_structured_block(
                        function_id,
                        function,
                        else_block,
                        entry_definite.clone(),
                        entry_possible.clone(),
                        entry_lambda_escapes.clone(),
                        entry_globals.clone(),
                        None,
                    ));
                } else if variant_count.is_some_and(|len| seen.len() < len) {
                    self.push(site, RirVerifyErrorKind::MatchNotExhaustive);
                }
                self.merge_structured_states(states);
            }
            RirStmt::OptionMatch(match_) => {
                let subject_ty = match &match_.subject {
                    RirOptionSubject::Place(place) => {
                        self.check_place(site, function, place);
                        place.ty
                    }
                    RirOptionSubject::MutPlace(place) => {
                        let ty = self.check_mut_place_arg(site, function_id, function, place);
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
                let entry_definite = self.initialized.clone();
                let entry_possible = self.possibly_initialized.clone();
                let entry_lambda_escapes = self.lambda_escapes.clone();
                let entry_globals = self.global_initialized.clone();
                let mut some_definite = entry_definite.clone();
                let mut some_possible = entry_possible.clone();
                let mut some_lambda_escapes = entry_lambda_escapes.clone();
                if (match_.payload_ref || match_.payload_escapes) && match_.payload.is_none() {
                    self.push(site, RirVerifyErrorKind::OptionPayloadEscapeRequiresPayload);
                }
                if match_.payload_escapes && !match_.payload_ref {
                    self.push(site, RirVerifyErrorKind::OptionPayloadEscapeRequiresRef);
                }
                if match_.payload.is_some()
                    && !match_.payload_ref
                    && inner.is_some_and(|inner| {
                        !RustRepPolicy::new(self.program).value_from_ref_supported(inner)
                    })
                {
                    self.push(site, RirVerifyErrorKind::NonCopyValueRequired);
                }
                if let Some(payload) = match_.payload {
                    if let Some(local) = function.locals.get(payload.index()) {
                        if function.params.iter().any(|param| param.local == payload) {
                            self.push(site, RirVerifyErrorKind::InitParamLocal);
                        }
                        if entry_possible
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
                        if let Some(slot) = some_definite.get_mut(payload.index()) {
                            *slot = true;
                        }
                        if let Some(slot) = some_possible.get_mut(payload.index()) {
                            *slot = true;
                        }
                        if let Some(slot) = some_lambda_escapes.get_mut(payload.index()) {
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
                    some_definite,
                    some_possible,
                    some_lambda_escapes,
                    entry_globals.clone(),
                    escaping_payload,
                );
                if match_.payload_ref
                    && !match_.payload_escapes
                    && let (Some(payload), Some((definite, possible, _, _, _, _))) =
                        (match_.payload, &mut some_state)
                {
                    if let Some(slot) = definite.get_mut(payload.index()) {
                        *slot = false;
                    }
                    if let Some(slot) = possible.get_mut(payload.index()) {
                        *slot = false;
                    }
                }
                let none_state = self.check_structured_block(
                    function_id,
                    function,
                    &match_.none_block,
                    entry_definite,
                    entry_possible,
                    entry_lambda_escapes,
                    entry_globals,
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
        self.check_place(site, function, &scope.root);
        self.check_collection_loan_root(site, function, &scope.root);
        let root_ty = self.ty(scope.root.ty);
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
        });
        let state = self.check_structured_block(
            function_id,
            function,
            &scope.body,
            self.initialized.clone(),
            self.possibly_initialized.clone(),
            self.lambda_escapes.clone(),
            self.global_initialized.clone(),
            None,
        );
        self.collection_loans.pop();
        self.merge_structured_states([state]);
    }

    fn collection_loan_root_mutable(&self, function: &RirFunction, root: &RirPlace) -> bool {
        match root.root {
            RirPlaceRoot::Local(_) => Self::place_is_mutable_root(function, root),
            RirPlaceRoot::Global(global) => self
                .program
                .globals
                .get(global.index())
                .is_some_and(|global| global.mutable),
        }
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
                if !root.projections.is_empty()
                    && Self::function_local_is_mut_place_param(function, local)
                {
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

    fn check_collection_mutation_root(
        &mut self,
        site: RirVerifySite,
        function: &RirFunction,
        place: &RirPlace,
    ) {
        if self.assignment_replaces_active_collection_root(place) {
            self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
        }
        match place.root {
            RirPlaceRoot::Local(_) => {
                let local = self.local_root(site, place);
                self.check_place(site, function, place);
                self.check_mutable_local_root(site, function, local);
            }
            RirPlaceRoot::Global(global) => {
                let Some(decl) = self.check_global_id(site, global).cloned() else {
                    self.check_place(site, function, place);
                    return;
                };
                self.check_place(site, function, place);
                if !decl.mutable {
                    self.push(site, RirVerifyErrorKind::ImmutableAssign);
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
            .any(|loan| loan.root.root == RirPlaceRoot::Global(global))
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
            .any(|loan| place_prefix(dst, &loan.root))
    }

    fn check_structured_block(
        &mut self,
        function_id: RirFunctionId,
        function: &RirFunction,
        body: &RirStructuredBlock,
        definite: Vec<bool>,
        possible: Vec<bool>,
        lambda_escapes: Vec<Option<RirLambdaEscape>>,
        global_initialized: Vec<bool>,
        preserved_payload_ref: Option<RirLocalId>,
    ) -> Option<RirBlockState> {
        let outer_definite = std::mem::replace(&mut self.initialized, definite);
        let outer_possible = std::mem::replace(&mut self.possibly_initialized, possible);
        let outer_lambda_escapes = std::mem::replace(&mut self.lambda_escapes, lambda_escapes);
        let outer_globals = std::mem::replace(&mut self.global_initialized, global_initialized);
        let outer_cell_definite = self.initialized_cells.clone();
        let outer_cell_possible = self.possibly_initialized_cells.clone();
        for (index, stmt) in body.stmts.iter().enumerate() {
            self.check_stmt(function_id, function, index, stmt);
        }
        self.check_term(function_id, function, &body.term);
        let falls_through = self.structured_block_falls_through(body);
        let result = falls_through.then(|| {
            let mut definite = self.initialized.clone();
            let mut possible = self.possibly_initialized.clone();
            let lambda_escapes = self.lambda_escapes.clone();
            let cell_definite = self.initialized_cells.clone();
            let cell_possible = self.possibly_initialized_cells.clone();
            let globals = self.global_initialized.clone();
            for local in &function.locals {
                if local.payload_ref && Some(local.id) != preserved_payload_ref {
                    if let Some(slot) = definite.get_mut(local.id.index()) {
                        *slot = false;
                    }
                    if let Some(slot) = possible.get_mut(local.id.index()) {
                        *slot = false;
                    }
                }
            }
            (
                definite,
                possible,
                lambda_escapes,
                cell_definite,
                cell_possible,
                globals,
            )
        });
        self.initialized = outer_definite;
        self.possibly_initialized = outer_possible;
        self.lambda_escapes = outer_lambda_escapes;
        self.global_initialized = outer_globals;
        self.initialized_cells = outer_cell_definite;
        self.possibly_initialized_cells = outer_cell_possible;
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
            RirStmt::EnumMatch(match_) => {
                let arm_falls = match_
                    .arms
                    .iter()
                    .any(|arm| self.structured_block_falls_through(&arm.block));
                match &match_.else_block {
                    Some(block) => arm_falls || self.structured_block_falls_through(block),
                    None => arm_falls || !self.enum_match_is_exhaustive(match_),
                }
            }
            RirStmt::OptionMatch(match_) => {
                self.structured_block_falls_through(&match_.some_block)
                    || self.structured_block_falls_through(&match_.none_block)
            }
            RirStmt::CollectionSlotScope(block) => self.structured_block_falls_through(block),
            RirStmt::Loop(_)
            | RirStmt::CollectionLoanScope(_)
            | RirStmt::Init { .. }
            | RirStmt::GlobalEnsure { .. }
            | RirStmt::GlobalSetRoot { .. }
            | RirStmt::GlobalUpdateRoot { .. }
            | RirStmt::MutPlaceSet { .. }
            | RirStmt::Assign { .. }
            | RirStmt::CellInit { .. }
            | RirStmt::CellSet { .. }
            | RirStmt::ScopedPlaceCellSet { .. }
            | RirStmt::DataRefSet { .. }
            | RirStmt::MapValueSet { .. }
            | RirStmt::Eval(_) => true,
        }
    }

    fn enum_match_is_exhaustive(&self, match_: &RirEnumMatch) -> bool {
        let Some(RirType::Enum(enum_id)) = self.ty(match_.discr.ty) else {
            return false;
        };
        let Some(enm) = self.program.enums.get(enum_id.index()) else {
            return false;
        };
        let mut seen = Vec::new();
        for arm in &match_.arms {
            if arm.variant.index() >= enm.variants.len() || seen.contains(&arm.variant) {
                return false;
            }
            seen.push(arm.variant);
        }
        seen.len() == enm.variants.len()
    }

    fn merge_structured_states(&mut self, states: impl IntoIterator<Item = Option<RirBlockState>>) {
        let mut states = states.into_iter().flatten();
        let Some((
            mut definite,
            mut possible,
            mut lambda_escapes,
            mut cell_definite,
            mut cell_possible,
            mut globals,
        )) = states.next()
        else {
            return;
        };
        for (
            next_definite,
            next_possible,
            next_lambda_escapes,
            next_cell_definite,
            next_cell_possible,
            next_globals,
        ) in states
        {
            definite = definite
                .iter()
                .zip(&next_definite)
                .map(|(lhs, rhs)| *lhs && *rhs)
                .collect();
            possible = possible
                .iter()
                .zip(&next_possible)
                .map(|(lhs, rhs)| *lhs || *rhs)
                .collect();
            lambda_escapes = lambda_escapes
                .iter()
                .zip(&next_lambda_escapes)
                .map(|(lhs, rhs)| if lhs == rhs { *lhs } else { None })
                .collect();
            cell_definite = cell_definite
                .iter()
                .zip(&next_cell_definite)
                .map(|(lhs, rhs)| *lhs && *rhs)
                .collect();
            cell_possible = cell_possible
                .iter()
                .zip(&next_cell_possible)
                .map(|(lhs, rhs)| *lhs || *rhs)
                .collect();
            globals = globals
                .iter()
                .zip(&next_globals)
                .map(|(lhs, rhs)| *lhs && *rhs)
                .collect();
        }
        self.initialized = definite;
        self.possibly_initialized = possible;
        self.lambda_escapes = lambda_escapes;
        self.initialized_cells = cell_definite;
        self.possibly_initialized_cells = cell_possible;
        self.global_initialized = globals;
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
                    if matches!(self.ty(decl.ty), Some(RirType::Lambda(_)))
                        && matches!(lambda.storage, RirLambdaStorage::HeapEnv { .. })
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
                        && !RustRepPolicy::new(self.program).shareable_value(decl.payload_ty)
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
                    if !RustRepPolicy::new(self.program).shareable_value(decl.payload_ty) {
                        self.push(site, RirVerifyErrorKind::NonCopyValueRequired);
                    }
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
                    self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                }
                Some(*target)
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
            RirRValue::ListPush { list, value } => {
                self.check_collection_mutation_root(site, function, list);
                let Some(RirType::List(elem)) = self.ty(list.ty) else {
                    self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                    return;
                };
                if Self::place_is_mut_place_param_root(function, list) {
                    self.check_short_region_operand(site, function, value);
                }
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
                } else if !RustRepPolicy::new(self.program).shareable_value(elem) {
                    self.push(site, RirVerifyErrorKind::NonCopyValueRequired);
                }
                Some(*ty)
            }
            RirRValue::MapGet { map, key, ty } | RirRValue::MapRemove { map, key, ty } => {
                self.check_place(site, function, map);
                if matches!(value, RirRValue::MapRemove { .. }) {
                    self.check_collection_mutation_root(site, function, map);
                } else if matches!(map.root, RirPlaceRoot::Local(_)) {
                    self.local_root(site, map);
                }
                self.check_type_id(site, *ty);
                let Some(RirType::Map { key: key_ty, value }) = self.ty(map.ty) else {
                    self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
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
                if Self::place_is_mut_place_param_root(function, map) {
                    self.check_short_region_operand(site, function, key);
                }
                self.check_value_operand_ty(site, function, key, key_ty);
                Some(*ty)
            }
            RirRValue::MapEntryAt { map, index, ty } => {
                self.check_place(site, function, map);
                self.check_local_id(site, function, *index);
                self.check_type_id(site, *ty);
                let Some(RirType::Map { key, value }) = self.ty(map.ty) else {
                    self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
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
                if function
                    .locals
                    .get(index.index())
                    .is_none_or(|local| !matches!(self.ty(local.ty), Some(RirType::Int)))
                {
                    self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                }
                Some(*ty)
            }
            RirRValue::MapValueAt { map, index, ty } => {
                self.check_place(site, function, map);
                self.check_local_id(site, function, *index);
                self.check_type_id(site, *ty);
                let Some(RirType::Map { value, .. }) = self.ty(map.ty) else {
                    self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
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
                if function
                    .locals
                    .get(index.index())
                    .is_none_or(|local| !matches!(self.ty(local.ty), Some(RirType::Int)))
                {
                    self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                }
                Some(*ty)
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
            RirRValue::MapInsert { map, key, value } => {
                self.check_collection_mutation_root(site, function, map);
                let Some(RirType::Map {
                    key: key_ty,
                    value: value_ty,
                }) = self.ty(map.ty)
                else {
                    self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                    return;
                };
                if Self::place_is_mut_place_param_root(function, map) {
                    self.check_short_region_operand(site, function, key);
                    self.check_short_region_operand(site, function, value);
                }
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
            | RirCallArg::ScopedLambda {
                callee: operand, ..
            } => self.operand_lambda_escape(function, operand),
            RirCallArg::SharedBorrow(_)
            | RirCallArg::MutBorrow(_)
            | RirCallArg::MutPlace(_)
            | RirCallArg::SharedStringConst(_) => None,
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
        let (expected, callee_ret) = match callee {
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
                    ),
                    None => return,
                }
            }
            RirCallTarget::Extern(id) => {
                self.check_extern_id(RirVerifySite::RValue(function_id, stmt), id);
                match self.program.externs.get(id.index()) {
                    Some(ext) => (
                        ext.params
                            .iter()
                            .map(|param| (param.ty, param.semantic, param.abi, param.escape))
                            .collect::<Vec<_>>(),
                        ext.ret,
                    ),
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
            if arg.semantic() != mode
                || matches!(arg, RirCallArg::ScopedLambda { .. }) && !native_call
            {
                self.push(site, RirVerifyErrorKind::CallArgMode);
            }
            if RustRepPolicy::new(self.program).call_arg_abi(ty, arg.semantic()) != Some(abi) {
                self.push(site, RirVerifyErrorKind::UnsupportedAbi);
            }
            if escape == RirParamEscape::Escaping
                && matches!(self.ty(ty), Some(RirType::Lambda(_)))
                && self.call_arg_lambda_escape(function, arg) != Some(RirLambdaEscape::Escaping)
            {
                self.push(site, RirVerifyErrorKind::CallArgEscape);
            }
            let found = match arg {
                RirCallArg::Value(operand) => self.value_operand_ty(site, function, operand),
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
                RirCallArg::MutPlace(arg) => {
                    Some(self.check_mut_place_arg(site, function_id, function, arg))
                }
                RirCallArg::ScopedLambda { callee, sig } => {
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
                    self.check_const_id(site, *id);
                    self.program.consts.get(id.index()).map(|konst| konst.ty)
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
    ) -> RirTypeId {
        self.check_type_id(site, arg.ty);
        match &arg.access {
            RirMutPlaceAccess::Handle(handle) => {
                self.check_mut_place_handle(site, function_id, function, handle, arg)
            }
            RirMutPlaceAccess::DataRef { object, dataref } => {
                let found = self.check_dataref_access(
                    site,
                    function,
                    object,
                    *dataref,
                    &arg.projections,
                    StorageProjectionMode::MutPlace,
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
    ) -> RirTypeId {
        match handle {
            RirMutPlaceHandle::Local { local, ty } => {
                self.check_place(
                    site,
                    function,
                    &RirPlace::local(*local, arg.projections.clone(), arg.ty),
                );
                if Self::function_local_is_mut_place_param(function, *local) {
                    self.push(site, RirVerifyErrorKind::CallArgMode);
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
                if self.function_local_is_scoped_place_source(function_id, *local) {
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
    ) {
        let Some(expected) =
            self.check_projection_chain(site, function, root_ty, projections, false)
        else {
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
                RirProjectionKind::MapIndex { local, key } => {
                    self.check_map_index_local(site, function, local, key)?;
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

    fn check_map_index_local(
        &mut self,
        site: RirVerifySite,
        function: &RirFunction,
        local: RirLocalId,
        key: RirTypeId,
    ) -> Option<()> {
        let index_local = self.check_projection_index_local(site, function, local)?;
        if index_local.ty != key {
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
        let policy = RustRepPolicy::new(self.program);
        let supported = policy.supports_param(ty, semantic) && abi == policy.param_abi(semantic);
        if !supported {
            self.push(site, RirVerifyErrorKind::UnsupportedAbi);
        }
    }

    fn unary_ok(&self, op: UnaryOp, value: Option<RirTypeId>, ret: RirTypeId) -> bool {
        match (value.and_then(|ty| self.scalar(ty)), self.scalar(ret)) {
            (Some(value), Some(ret)) => op.scalar_result(value) == Some(ret),
            _ => false,
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
            _ => None,
        }
    }

    fn cast_ok(&self, value: Option<RirTypeId>, ret: RirTypeId) -> bool {
        if matches!(
            (value.and_then(|ty| self.ty(ty)), self.ty(ret)),
            (
                Some(RirType::Int | RirType::Float),
                Some(RirType::Int | RirType::Float)
            )
        ) {
            return true;
        }
        let Some(RirType::Enum(enum_id)) = value.and_then(|ty| self.ty(ty)) else {
            return false;
        };
        self.program
            .enums
            .get(enum_id.index())
            .is_some_and(|enm| enm.raw_type == Some(ret))
    }

    fn stringify_ok(&self, value: &RirOperand, source_ty: RirTypeId) -> bool {
        match self.ty(source_ty) {
            Some(RirType::Int | RirType::Float | RirType::Bool | RirType::String) => true,
            Some(RirType::Struct(_)) => {
                matches!(value, RirOperand::Place(_))
                    && matches!(
                        self.stringify_req(source_ty).map(|req| req.kind),
                        Some(
                            RirStringifyReqKind::Structural(_)
                                | RirStringifyReqKind::Override { .. }
                        )
                    )
            }
            Some(
                RirType::Void
                | RirType::Tuple(_)
                | RirType::DataRef(_)
                | RirType::Enum(_)
                | RirType::Array { .. }
                | RirType::List(_)
                | RirType::Map { .. }
                | RirType::Option(_)
                | RirType::Slice(_)
                | RirType::Lambda(_),
            )
            | None => false,
        }
    }

    fn format_ok(&self, source_ty: RirTypeId, spec: RirFormatSpec) -> bool {
        let Some(ty) = self.ty(source_ty) else {
            return false;
        };
        match ty {
            RirType::Int | RirType::Float | RirType::Bool | RirType::String => {}
            RirType::Void
            | RirType::Struct(_)
            | RirType::Tuple(_)
            | RirType::DataRef(_)
            | RirType::Enum(_)
            | RirType::Array { .. }
            | RirType::List(_)
            | RirType::Map { .. }
            | RirType::Option(_)
            | RirType::Slice(_)
            | RirType::Lambda(_) => {
                return false;
            }
        }
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
            && ty.is_some_and(|ty| !RustRepPolicy::new(self.program).shareable_value(ty))
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
        self.program
            .scoped_place_cells
            .iter()
            .any(|cell| cell.owner == function && cell.source_local == local)
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

    fn stringify_req(&self, ty: RirTypeId) -> Option<&RirStringifyReq> {
        self.program.stringify_reqs.iter().find(|req| req.ty == ty)
    }

    fn copyable_type(&self, ty: RirTypeId) -> bool {
        match self.ty(ty) {
            Some(RirType::Struct(id)) if self.program.structs.get(id.index()).is_some() => {
                RustRepPolicy::new(self.program).copyable(ty) && self.inherently_copyable_type(ty)
            }
            Some(RirType::Enum(id)) if self.program.enums.get(id.index()).is_some() => {
                RustRepPolicy::new(self.program).copyable(ty) && self.inherently_copyable_type(ty)
            }
            Some(RirType::Tuple(id)) if self.program.tuples.get(id.index()).is_some() => {
                RustRepPolicy::new(self.program).copyable(ty) && self.inherently_copyable_type(ty)
            }
            Some(RirType::Array { .. }) => self.inherently_copyable_type(ty),
            Some(_) => RustRepPolicy::new(self.program).copyable(ty),
            None => false,
        }
    }

    fn inherently_copyable_type(&self, ty: RirTypeId) -> bool {
        match self.ty(ty) {
            Some(RirType::Int | RirType::Float | RirType::Bool | RirType::Void) => true,
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

pub fn source_param_semantic(mode: air::ParamMode) -> RirParamSemantic {
    match mode {
        air::ParamMode::Value => RirParamSemantic::Value,
        air::ParamMode::SharedBorrow => RirParamSemantic::SharedBorrow,
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
