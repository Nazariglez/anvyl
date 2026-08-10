use anvyx_externs::{
    AbiPosition, AbiTypeError, BinaryOp as ExternBinaryOp, ExternBindingOp, ExternBindingTarget,
    ExternMemberSelector, ExternOperator, ExternTypeExpr, ExternTypeKey,
    ModulePath as ExternModulePath, UnaryOp as ExternUnaryOp,
};

pub use super::typing::PrimitiveKind;
use super::{
    AggregateKind, CaptureCellLifetime, CaptureLocalSource, ConstValue, ContractParamDecl,
    ContractReceiver, ContractReturnDecl, ContractSlotDecl, ContractWitnessKey,
    ContractWitnessSlotDecl, ContractWitnessTarget, EnumRepr, ExternMember, Function, FunctionKind,
    FunctionValueCapability, LambdaCaptureDecl, LambdaDecl, LambdaEscape, Local, LocalKind,
    Mutability, Param, ParamEscape, ParamMode, ParamRole, ParamType, Program, RawEnumValue,
    ReturnMode, ScopedBorrowDecl, ScopedBorrowSource, SignatureType, TypeData, VariantShape,
    body::{
        AggregateCtor, AirBlock, AirChild, AirCollectionLoan, AirCollectionLoanMode,
        AirCollectionRootKind, AirCollectionSlot, AirCollectionSlotKind, AirCollectionSlotScope,
        AirDynMatch, AirDynMatchFallbackBinding, AirDynMatchSource, AirDynMatchTargetBinding,
        AirIf, AirMapEntryMatch, AirOptionalMatch, AirPatternAlternative, AirPatternArm,
        AirPatternBinding, AirPatternBindingMode, AirPatternMatch, AirPatternPath,
        AirPatternPathStep, AirPatternTest, AirStmt, AirTail, CallArg, Callee, DynBorrow,
        DynBorrowSource, DynReceiver, GlobalInitEffect, LambdaCaptureArg, MapWriteKind, Operand,
        OwnedValue, Place, PlaceReadLocal, PlaceRoot, Projection, RValue, ValueSource, ValueUse,
    },
    ids::*,
    place_model,
    typing::{self, PrimitiveTypes},
};
use crate::{
    ast::{BinaryOp, UnaryOp},
    collection_effect::{
        CollectionStructuralEffect, MapStructuralEffect, SequenceStructuralEffect,
    },
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VerifyError {
    pub site: VerifySite,
    pub kind: VerifyErrorKind,
}

impl std::fmt::Display for VerifyError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}: {:?}", self.site, self.kind)
    }
}

impl std::error::Error for VerifyError {}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VerifySite {
    Program,
    Module(ModuleId),
    Type(TypeId),
    Const(ConstId),
    Aggregate(AggregateId),
    Enum(EnumId),
    Flag(FlagId),
    ExternType(ExternTypeId),
    Extern(ExternId),
    Lambda(LambdaId),
    DynBorrowParam(DynBorrowParamId),
    ScopedBorrow(ScopedBorrowId),
    CaptureCell(CaptureCellId),
    Global(GlobalId),
    ContractSurface(ContractSurfaceId),
    ContractSlot {
        surface: ContractSurfaceId,
        slot: ContractSlotId,
    },
    ContractWitness(ContractWitnessId),
    ContractWeakening(ContractWeakeningId),
    Function(FunctionId),
    Statement {
        function: FunctionId,
        block: BlockId,
        index: usize,
    },
    Terminator {
        function: FunctionId,
        block: BlockId,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VerifyErrorKind {
    BadReference(BadReference),
    BadFunction(BadFunction),
    BadPlace(BadPlace),
    BadCall(BadCall),
    BadType(BadType),
    BadConst(BadConst),
    BadModule(BadModule),
    BadEnum(BadEnum),
    BadFlag(BadFlag),
    BadRValue(BadRValue),
    BadStatement(BadStatement),
    BadExtern(BadExtern),
    BadContract(BadContract),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BadContract {
    EmptySurface,
    EmptyDisplayName,
    EmptySlotName,
    DuplicateSlotName,
    DuplicateSurface,
    SlotIdMismatch,
    WitnessSlotCountMismatch,
    WitnessSlotIdMismatch,
    DuplicateWitnessKey,
    WitnessReceiverMismatch,
    WitnessTargetSignatureMismatch,
    InvalidWitnessProjection,
    InvalidWitnessConcreteType,
    MissingProjectedWitness,
    DuplicateWeakening,
    WeakeningSlotCountMismatch,
    WeakeningNotProper,
    WeakeningSlotSignatureMismatch,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BadExtern {
    ReceiverTypeMismatch {
        expected: TypeId,
        found: TypeId,
    },
    InvalidInitField(FieldId),
    OperatorOperandMismatch,
    MemberParamCountMismatch {
        expected: usize,
        found: usize,
    },
    AbiParamCountMismatch {
        expected: usize,
        found: usize,
    },
    VariantAbiCountMismatch {
        expected: usize,
        found: usize,
    },
    VariantFieldAbiCountMismatch {
        expected: usize,
        found: usize,
    },
    ReceiverModeMismatch,
    EscapingParamMustBeValue(usize),
    EscapingParamMustBeFunction(usize),
    InvalidAbi {
        position: AbiPosition,
        reason: AbiTypeError,
    },
    BindingMismatch,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BadType {
    DuplicatePrimitive {
        kind: PrimitiveKind,
        first: TypeId,
        duplicate: TypeId,
    },
    MissingAggregate(AggregateId),
    DuplicateAggregate {
        aggregate: AggregateId,
        first: TypeId,
        duplicate: TypeId,
    },
    AggregateKindMismatch {
        aggregate: AggregateId,
        declared: AggregateKind,
        represented: AggregateKind,
    },
    MissingEnum(EnumId),
    DuplicateEnum {
        enm: EnumId,
        first: TypeId,
        duplicate: TypeId,
    },
    MissingFlag(FlagId),
    DuplicateFlag {
        flag: FlagId,
        first: TypeId,
        duplicate: TypeId,
    },
    Recursive(TypeId),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BadConst {
    TypeMismatch { expected: TypeId, found: TypeId },
    InvalidFlagValue,
    NilMustBeOptional(TypeId),
    MissingPrimitive(PrimitiveKind),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BadEnum {
    AdtHasRawType,
    RawMissingRawType,
    RawTypeMismatch(TypeId),
    AdtVariantHasRawValue(VariantId),
    RawVariantMissingValue(VariantId),
    RawVariantPayload(VariantId),
    RawIntVariantValueType(VariantId),
    RawStringVariantValueType(VariantId),
    DuplicateRawValue(VariantId),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BadFlag {
    KnownBitsNegative,
    MemberIdMismatch(FlagMemberId),
    NegativeValue(FlagMemberId),
    AtomicMismatch(FlagMemberId),
    DuplicateValue(FlagMemberId),
    UnknownCompositeBits(FlagMemberId),
    PatternTypeMismatch { flag: FlagId, found: TypeId },
    PatternUnknownBits { flag: FlagId, bits: i64 },
    KnownBitsMismatch,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BadModule {
    DuplicateItem(ModuleItem),
    ItemWrongModule {
        item: ModuleItem,
        expected: ModuleId,
        found: ModuleId,
    },
    MissingItem(ModuleItem),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ModuleItem {
    Function(FunctionId),
    Global(GlobalId),
    Aggregate(AggregateId),
    Enum(EnumId),
    Flag(FlagId),
    ExternType(ExternTypeId),
    Extern(ExternId),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BadRValue {
    MissingPrimitive(PrimitiveKind),
    UnaryTypeMismatch {
        value: TypeId,
        result: TypeId,
    },
    BinaryTypeMismatch {
        lhs: TypeId,
        rhs: TypeId,
        result: TypeId,
    },
    UnsupportedBinaryOp(BinaryOp),
    CastMustConvertIntAndFloat {
        value: TypeId,
        target: TypeId,
    },
    RawProjectMustMatchBacking {
        value: TypeId,
        target: TypeId,
    },
    RawTryConstructMustMatch {
        value: TypeId,
        target: TypeId,
        result: TypeId,
    },
    FlagStaticMustBeFlag(TypeId),
    StringConcatPartMustBeString(TypeId),
    StringifyOperandTypeMismatch {
        operand: TypeId,
        source: TypeId,
    },
    StringifyAnySource {
        source: TypeId,
    },
    StringifyVoidSource {
        source: TypeId,
    },
    FunctionValueTypeMismatch {
        expected: SignatureType,
        found: TypeId,
    },
    FunctionValueMustBeFunction(TypeId),
    FunctionValueEscapeMismatch {
        claimed: FunctionValueCapability,
        actual: FunctionValueCapability,
    },
    FunctionRefMustBeNamed(FunctionId),
    MakeLambdaOwnerMismatch {
        lambda: LambdaId,
        expected: FunctionId,
        found: FunctionId,
    },
    LambdaCaptureMismatch {
        index: usize,
    },
    ReadonlyCaptureMustBeImmutableOwned {
        index: usize,
    },
    AggregateCtorResultTypeMismatch {
        aggregate: AggregateId,
        expected: AggregateKind,
        found: TypeId,
    },
    AggregateCtorKindMismatch {
        aggregate: AggregateId,
        expected: AggregateKind,
        found: AggregateKind,
    },
    AggregateCtorFieldCountMismatch {
        aggregate: AggregateId,
        expected: usize,
        found: usize,
    },
    AggregateCtorFieldTypeMismatch {
        aggregate: AggregateId,
        field: usize,
        expected: TypeId,
        found: TypeId,
    },
    CollectionCtorResultTypeMismatch {
        ctor: AggregateCtor,
        found: TypeId,
    },
    CollectionCtorFieldCountMismatch {
        ctor: AggregateCtor,
        expected: usize,
        found: usize,
    },
    CollectionCtorFieldTypeMismatch {
        ctor: AggregateCtor,
        field: usize,
        expected: TypeId,
        found: TypeId,
    },
    EnumCtorResultTypeMismatch {
        enum_id: EnumId,
        found: TypeId,
    },
    EnumCtorFieldCountMismatch {
        enum_id: EnumId,
        variant: VariantId,
        expected: usize,
        found: usize,
    },
    EnumCtorFieldTypeMismatch {
        enum_id: EnumId,
        variant: VariantId,
        field: usize,
        expected: TypeId,
        found: TypeId,
    },
    OptionalSomeTypeMismatch {
        expected: TypeId,
        found: TypeId,
    },
    InvalidDynPack,
    InvalidDynWeaken,
    InvalidDynDowncast,
    InvalidDynCall,
    InvalidDynBorrow,
    InvalidMaterialize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BadStatement {
    InitParamLocal(LocalId),
    InitTypeMismatch { expected: TypeId, found: TypeId },
    AssignTypeMismatch { expected: TypeId, found: TypeId },
    AssignGlobalRoot(GlobalId),
    GlobalSetRootInitMustStoreWithoutInit,
    GlobalSetRootTypeMismatch { expected: TypeId, found: TypeId },
    GlobalUpdateRootTypeMismatch { expected: TypeId, found: TypeId },
    GlobalUpdateRootWithoutEnsure(GlobalId),
    ReadUninitializedLocal(LocalId),
    ReadUninitializedCaptureCell(CaptureCellId),
    AssignUninitializedLocal(LocalId),
    AssignUninitializedCaptureCell(CaptureCellId),
    InitImmutableLocalTwice(LocalId),
    InvalidDynMatch,
    InvalidPatternMatch,
    DynMatchRootUsed,
    DynMatchAliasEscapes(LocalId),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BadReference {
    InvalidEntry(FunctionId),
    InvalidFunction(FunctionId),
    InvalidLambda(LambdaId),
    InvalidLambdaCaptureSlot(LambdaCaptureSlotId),
    InvalidExtern(ExternId),
    InvalidExternType(ExternTypeId),
    InvalidAggregate(AggregateId),
    InvalidEnum(EnumId),
    InvalidFlag(FlagId),
    InvalidType(TypeId),
    InvalidConst(ConstId),
    InvalidLocal(LocalId),
    InvalidScopedBorrow(ScopedBorrowId),
    InvalidCaptureCell(CaptureCellId),
    InvalidGlobal(GlobalId),
    InvalidField {
        aggregate: AggregateId,
        field: FieldId,
    },
    InvalidVariant {
        enum_id: EnumId,
        variant: VariantId,
    },
    InvalidModule(ModuleId),
    InvalidContractSurface(ContractSurfaceId),
    InvalidContractSlot {
        surface: ContractSurfaceId,
        slot: ContractSlotId,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BadFunction {
    GlobalVoidType(GlobalId),
    GlobalInitKindMismatch {
        global: GlobalId,
        init: FunctionId,
    },
    GlobalInitFunctionMismatch {
        global: GlobalId,
        init: FunctionId,
    },
    GlobalInitSignatureMismatch {
        global: GlobalId,
        init: FunctionId,
    },
    GlobalInitModuleMismatch {
        global: GlobalId,
        expected: ModuleId,
        found: ModuleId,
    },
    ParamLocalOutOfRange {
        param: usize,
        total_locals: usize,
    },
    DuplicateParamLocal {
        first: LocalId,
        second: LocalId,
    },
    DuplicateLocalBinding {
        binding: BindingId,
        first: LocalId,
        second: LocalId,
    },
    LocalBindingInvalidKind {
        local: LocalId,
        kind: LocalKind,
    },
    IfCondMustBeBool(TypeId),
    SwitchDiscriminantMustBeEnum(TypeId),
    DuplicateSwitchArm(VariantId),
    SwitchArmVariantMismatch {
        expected_enum: EnumId,
        variant: VariantId,
    },
    EntryMustBeNamed(FunctionId),
    NonVoidFunctionMustReturnValue(TypeId),
    VoidFunctionMustReturnNone,
    ReturnedTypeMismatch {
        expected: TypeId,
        found: TypeId,
    },
    PlaceReturnMustReturnPlace,
    StringifyOverrideModuleMismatch {
        expected: ModuleId,
        found: ModuleId,
    },
    StringifyOverrideMissingReceiver,
    StringifyOverrideReceiverTypeMismatch {
        expected: AggregateId,
        found: TypeId,
    },
    StringifyOverrideReturnMustBeString(TypeId),
    StringifyOverrideMustBeNamed(FunctionId),
    LenSourceMustBeCountable(TypeId),
    ListElementTypeMismatch {
        expected: TypeId,
        found: TypeId,
    },
    MapKeyTypeMismatch {
        expected: TypeId,
        found: TypeId,
    },
    MapValueTypeMismatch {
        expected: TypeId,
        found: TypeId,
    },
    MapGetResultMustBeOptionalValue {
        expected_value: TypeId,
        found: TypeId,
    },
    MapEntrySourceMustBeMap(TypeId),
    MapEntryResultTypeMismatch {
        expected: TypeId,
        found: TypeId,
    },
    ListPopResultMustBeOptionalElement {
        expected_elem: TypeId,
        found: TypeId,
    },
    RangeListCopySourceMustBeSequence(TypeId),
    RangeListCopyResultMustBeList {
        expected_elem: TypeId,
        found: TypeId,
    },
    SliceViewSourceMustBeSequence(TypeId),
    SliceViewResultMustBeSlice {
        expected_elem: TypeId,
        found: TypeId,
    },
    SliceIndexMustBeInt {
        which: &'static str,
        found: TypeId,
    },
    CollectionLoanRootKindMismatch {
        root_kind: AirCollectionRootKind,
        found: TypeId,
    },
    CollectionLoanModeRootKindMismatch {
        root_kind: AirCollectionRootKind,
        mode: AirCollectionLoanMode,
    },
    CollectionLoanSlotKindMismatch {
        local: LocalId,
        mode: AirCollectionLoanMode,
        kind: AirCollectionSlotKind,
    },
    CollectionLoanSlotTypeMismatch {
        local: LocalId,
        expected: TypeId,
        found: TypeId,
    },
    CollectionLoanSlotMutabilityMismatch {
        local: LocalId,
        expected: bool,
        found: bool,
    },
    CollectionLoanSlotAlreadyInitialized(LocalId),
    CollectionLoanSlotMustBeFreshLocal {
        local: LocalId,
        kind: LocalKind,
    },
    CollectionLoanSlotOutOfScope(LocalId),
    CollectionLoanSlotEscapesBody(LocalId),
    PatternPathInvalid,
    PatternPayloadWithoutVariantTest,
    PatternAlternativeBindingMismatch(LocalId),
    CollectionLoanStructuralOpConflict {
        mode: AirCollectionLoanMode,
        op: &'static str,
    },
    CollectionLoanRootRebindConflict {
        mode: AirCollectionLoanMode,
    },
    IndexTypeUnavailable,
    ParamLocalMustBeArg {
        param: usize,
        local: LocalId,
    },
    ParamLocalTypeMismatch {
        param: usize,
        expected: TypeId,
        found: TypeId,
    },
    MutBorrowParamLocalMustBeMutable {
        param: usize,
        local: LocalId,
    },
    DynBorrowParamCount {
        local: LocalId,
        found: usize,
    },
    BreakOutsideLoop(AirLoopId),
    ContinueOutsideLoop(AirLoopId),
    MatchNotExhaustive(EnumId),
    OptionalPayloadLocalAlreadyInitialized(LocalId),
    OptionalPayloadLocalMustBeImmutable(LocalId),
    OptionalPayloadEscapeRequiresPayload,
    OptionalPayloadEscapeRequiresRef,
    OptionalPayloadEscapeNoneMustDiverge,
    LambdaBodyKindMismatch {
        lambda: LambdaId,
        body: FunctionId,
    },
    LambdaBodySignatureMismatch {
        lambda: LambdaId,
        body: FunctionId,
    },
    LambdaBodyModuleMismatch {
        lambda: LambdaId,
        expected: ModuleId,
        found: ModuleId,
    },
    EscapingLambdaScopedCapture {
        lambda: LambdaId,
    },
    EscapingLambdaCapturesNonEscapingFunction {
        lambda: LambdaId,
        local: LocalId,
    },
    DuplicateLambdaCapture {
        lambda: LambdaId,
        binding: BindingId,
        first: usize,
        second: usize,
    },
    DuplicateLambdaCaptureSource {
        lambda: LambdaId,
        first: usize,
        second: usize,
    },
    LambdaCaptureSourceMismatch {
        lambda: LambdaId,
        index: usize,
    },
    ReadonlyLambdaCaptureSourceMustBeImmutableOwned {
        lambda: LambdaId,
        index: usize,
        local: LocalId,
    },
    LambdaCaptureCellNotAccessible {
        lambda: LambdaId,
        owner: FunctionId,
        cell: CaptureCellId,
    },
    LambdaScopedBorrowNotAccessible {
        lambda: LambdaId,
        owner: FunctionId,
        borrow: ScopedBorrowId,
    },
    DuplicateCaptureCell {
        owner: FunctionId,
        binding: BindingId,
        first: CaptureCellId,
        second: CaptureCellId,
    },
    ScopedBorrowSourceLocalMismatch {
        borrow: ScopedBorrowId,
        owner: FunctionId,
        local: LocalId,
    },
    ScopedBorrowSourceLocalTypeMismatch {
        borrow: ScopedBorrowId,
        expected: TypeId,
        found: TypeId,
    },
    ScopedBorrowSourceLocalBindingMismatch {
        borrow: ScopedBorrowId,
        expected: BindingId,
        found: Option<BindingId>,
    },
    ScopedBorrowSourceLocalMustBeMutParam {
        borrow: ScopedBorrowId,
        local: LocalId,
    },
    ScopedBorrowSourceMustBeMutable {
        borrow: ScopedBorrowId,
    },
    DuplicateScopedBorrow {
        owner: FunctionId,
        binding: BindingId,
        first: ScopedBorrowId,
        second: ScopedBorrowId,
    },
    DuplicateScopedBorrowSource {
        owner: FunctionId,
        source: ScopedBorrowSource,
        first: ScopedBorrowId,
        second: ScopedBorrowId,
    },
    CaptureCellSourceLocalMismatch {
        cell: CaptureCellId,
        owner: FunctionId,
        local: LocalId,
    },
    CaptureCellSourceLocalTypeMismatch {
        cell: CaptureCellId,
        expected: TypeId,
        found: TypeId,
    },
    CaptureCellSourceLocalMustBeOwnedBinding {
        cell: CaptureCellId,
        local: LocalId,
        kind: LocalKind,
    },
    CaptureCellSourceLocalMustBeMutable {
        cell: CaptureCellId,
        local: LocalId,
    },
    CaptureCellSourceLocalBindingMismatch {
        cell: CaptureCellId,
        expected: BindingId,
        found: Option<BindingId>,
    },
    DuplicateCaptureCellSourceLocal {
        owner: FunctionId,
        local: LocalId,
        first: CaptureCellId,
        second: CaptureCellId,
    },
    CaptureCellLoopMissing {
        cell: CaptureCellId,
        loop_id: AirLoopId,
    },
    CaptureCellOutsideLoop {
        cell: CaptureCellId,
        loop_id: AirLoopId,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BadPlace {
    UnsupportedRoot(PlaceRoot),
    InvalidDynBorrowParam(DynBorrowParamId),
    NoRuntimeLambdaCaptureRoot(LambdaCaptureSlotId),
    FieldProjectionOnNonAggregate(TypeId),
    FieldProjectionKindMismatch {
        aggregate: AggregateId,
        expected: AggregateKind,
        found: AggregateKind,
    },
    TupleFieldOutOfRange {
        ty: TypeId,
        index: u32,
        len: usize,
    },
    TupleProjectionOnNonTuple(TypeId),
    IndexProjectionOnNonIndexable(TypeId),
    PlaceTypeMismatch {
        expected: TypeId,
        found: TypeId,
    },
    IndexLocalTypeMismatch {
        expected: TypeId,
        found: TypeId,
    },
    ImmutableRoot(PlaceRoot),
    UnsupportedCaptureCellProjection(CaptureCellId),
    UnsupportedScopedBorrowProjection(ScopedBorrowId),
    DynBorrowParamEscapes(DynBorrowParamId),
    PromotedBindingBypassesCell {
        binding: BindingId,
        cell: CaptureCellId,
        local: LocalId,
    },
    PromotedBindingBypassesScopedBorrow {
        binding: BindingId,
        borrow: ScopedBorrowId,
        local: LocalId,
    },
    CaptureCellNotAccessible {
        cell: CaptureCellId,
        function: FunctionId,
    },
    ScopedBorrowNotAccessible {
        borrow: ScopedBorrowId,
        function: FunctionId,
    },
    EscapingLambdaScopedBorrowRoot {
        lambda: LambdaId,
        root: ScopedBorrowId,
    },
    RawScopedBorrowCaptureBypass {
        lambda: LambdaId,
        root: ScopedBorrowId,
    },
    RawCaptureCellCaptureBypass {
        lambda: LambdaId,
        root: CaptureCellId,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BadCall {
    LambdaCalleeMustBeFunction,
    FunctionCalleeMustBeNamed(FunctionId),
    FunctionCalleeMustBeSourceCallable(FunctionId),
    ArityMismatch {
        expected: usize,
        found: usize,
    },
    ArgTypeMismatch {
        index: usize,
        expected: TypeId,
        found: TypeId,
    },
    ArgModeMismatch {
        index: usize,
        expected: ParamMode,
        found: ParamMode,
    },
    ArgEscapeMismatch {
        index: usize,
        expected: ParamEscape,
        found: ParamEscape,
    },
    ArgEscapeUnknown {
        index: usize,
        expected: ParamEscape,
    },
    ArgAliasConflict {
        first: usize,
        second: usize,
    },
    ReceiverArgAliasConflict {
        arg: usize,
    },
    UnexpectedInitFieldArg {
        index: usize,
    },
}

pub fn verify(program: &Program) -> Result<VerifiedProgram<'_>, Vec<VerifyError>> {
    let mut cx = VerifyCx::new(program);
    collect_errors(&mut cx);
    if cx.errors.is_empty() {
        Ok(VerifiedProgram { program })
    } else {
        Err(cx.errors)
    }
}

pub struct VerifiedProgram<'a> {
    program: &'a Program,
}

impl std::fmt::Debug for VerifiedProgram<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("VerifiedProgram").finish()
    }
}

impl VerifiedProgram<'_> {
    pub fn program(&self) -> &Program {
        self.program
    }
}

struct VerifyCx<'a> {
    program: &'a Program,
    primitives: PrimitiveTypes,
    type_states: Vec<TypeState>,
    transferable: Vec<std::collections::HashSet<LocalId>>,
    errors: Vec<VerifyError>,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum TypeState {
    Unseen,
    Visiting,
    Done,
}

impl<'a> VerifyCx<'a> {
    fn new(program: &'a Program) -> Self {
        let transferable = (0..program.functions.len())
            .map(|index| super::materialization::transferable_locals(program, index))
            .collect();
        Self {
            program,
            primitives: PrimitiveTypes::scan(program),
            type_states: vec![TypeState::Unseen; program.type_arena.len()],
            transferable,
            errors: Vec::new(),
        }
    }

    fn push(&mut self, site: VerifySite, kind: VerifyErrorKind) {
        self.errors.push(VerifyError { site, kind });
    }

    fn term_site(function_id: FunctionId, block_id: BlockId) -> VerifySite {
        VerifySite::Terminator {
            function: function_id,
            block: block_id,
        }
    }

    fn stmt_site(function_id: FunctionId, block_id: BlockId, index: usize) -> VerifySite {
        VerifySite::Statement {
            function: function_id,
            block: block_id,
            index,
        }
    }

    fn has_type(&self, id: TypeId) -> bool {
        id.index() < self.program.type_arena.len()
    }

    fn has_aggregate(&self, id: AggregateId) -> bool {
        id.index() < self.program.aggregates.len()
    }

    fn has_enum(&self, id: EnumId) -> bool {
        id.index() < self.program.enums.len()
    }

    fn has_flag(&self, id: FlagId) -> bool {
        id.index() < self.program.flags.len()
    }

    fn has_extern_type(&self, id: ExternTypeId) -> bool {
        id.index() < self.program.extern_types.len()
    }

    fn has_function(&self, id: FunctionId) -> bool {
        id.index() < self.program.functions.len()
    }

    fn has_extern(&self, id: ExternId) -> bool {
        id.index() < self.program.externs.len()
    }

    fn has_const(&self, id: ConstId) -> bool {
        id.index() < self.program.const_arena.len()
    }

    fn has_module(&self, id: ModuleId) -> bool {
        id.index() < self.program.modules.len()
    }

    fn has_scoped_borrow(&self, id: ScopedBorrowId) -> bool {
        id.index() < self.program.scoped_borrows.len()
    }

    fn has_capture_cell(&self, id: CaptureCellId) -> bool {
        id.index() < self.program.capture_cells.len()
    }

    fn has_global(&self, id: GlobalId) -> bool {
        id.index() < self.program.globals.len()
    }

    fn has_contract_surface(&self, id: ContractSurfaceId) -> bool {
        id.index() < self.program.contract_surfaces.len()
    }

    fn verify_module_ref(&mut self, site: VerifySite, module: ModuleId) {
        if !self.has_module(module) {
            self.push(
                site,
                VerifyErrorKind::BadReference(BadReference::InvalidModule(module)),
            );
        }
    }

    fn verify_type_ref(&mut self, site: VerifySite, ty: TypeId) {
        if !self.has_type(ty) {
            self.push(
                site,
                VerifyErrorKind::BadReference(BadReference::InvalidType(ty)),
            );
            return;
        }
        match self.type_states[ty.index()] {
            TypeState::Done => return,
            TypeState::Visiting => {
                self.push(site, VerifyErrorKind::BadType(BadType::Recursive(ty)));
                return;
            }
            TypeState::Unseen => {}
        }
        self.type_states[ty.index()] = TypeState::Visiting;
        verify_type(self, ty);
        self.type_states[ty.index()] = TypeState::Done;
    }

    fn variant_belongs_to_enum(&self, enum_id: EnumId, variant: VariantId) -> bool {
        self.program
            .enums
            .get(enum_id.index())
            .is_some_and(|enm| variant.index() < enm.variants.len())
    }

    fn type_data(&self, ty: TypeId) -> Option<&TypeData> {
        self.program.type_arena.get(ty)
    }
}

#[derive(Clone, PartialEq, Eq)]
enum FunctionValueState {
    Scalar(FunctionValueCapability),
    Fields(Vec<FunctionValueState>),
    Variants(Vec<Vec<FunctionValueState>>),
}

impl FunctionValueState {
    fn unknown() -> Self {
        Self::Scalar(FunctionValueCapability::Unknown)
    }

    fn non_function() -> Self {
        Self::Scalar(FunctionValueCapability::NonFunction)
    }

    fn function(capability: FunctionValueCapability) -> Self {
        Self::Scalar(capability)
    }

    fn join(left: Self, right: Self) -> Self {
        match (left, right) {
            (Self::Scalar(left), Self::Scalar(right)) => Self::Scalar(join_escape(left, right)),
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
                    Self::unknown()
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
            _ => Self::unknown(),
        }
    }

    fn capability(&self) -> FunctionValueCapability {
        match self {
            Self::Scalar(capability) => *capability,
            Self::Fields(_) | Self::Variants(_) => FunctionValueCapability::Unknown,
        }
    }

    fn project(&self, projection: &Projection) -> Self {
        let Self::Fields(fields) = self else {
            return Self::unknown();
        };
        match projection {
            Projection::Field(field) => fields
                .get(field.index())
                .cloned()
                .unwrap_or_else(Self::unknown),
            Projection::TupleField(field) => fields
                .get(*field as usize)
                .cloned()
                .unwrap_or_else(Self::unknown),
            Projection::Index(_) => fields
                .iter()
                .cloned()
                .reduce(Self::join)
                .unwrap_or_else(Self::unknown),
        }
    }

    fn variant_field(&self, variant: VariantId, field: u16) -> Self {
        let Self::Variants(variants) = self else {
            return Self::unknown();
        };
        variants
            .get(variant.index())
            .and_then(|fields| fields.get(field as usize))
            .cloned()
            .unwrap_or_else(Self::unknown)
    }

    fn projection_mut(&mut self, projections: &[Projection]) -> Option<&mut Self> {
        let Some((first, rest)) = projections.split_first() else {
            return Some(self);
        };
        let Self::Fields(fields) = self else {
            return None;
        };
        let index = match first {
            Projection::Field(field) => field.index(),
            Projection::TupleField(field) => *field as usize,
            Projection::Index(_) => return None,
        };
        fields.get_mut(index)?.projection_mut(rest)
    }

    fn assign_projection(&mut self, projections: &[Projection], value: Self) -> bool {
        let Some(slot) = self.projection_mut(projections) else {
            return false;
        };
        *slot = value;
        true
    }

    fn push_projection(&mut self, projections: &[Projection], value: Self) -> bool {
        let Some(Self::Fields(fields)) = self.projection_mut(projections) else {
            return false;
        };
        fields.push(value);
        true
    }
}

#[derive(Clone)]
struct LocalInit {
    definite: Vec<bool>,
    possible: Vec<bool>,
    local_values: Vec<FunctionValueState>,
    cell_definite: Vec<bool>,
    global_definite: Vec<bool>,
    global_values: Vec<FunctionValueState>,
}

impl LocalInit {
    fn new(program: &Program, function: &Function) -> Self {
        let mut state = Self {
            definite: vec![false; function.locals.len()],
            possible: vec![false; function.locals.len()],
            local_values: function
                .locals
                .iter()
                .map(|local| type_function_state(program, local.ty))
                .collect(),
            cell_definite: vec![false; program.capture_cells.len()],
            global_definite: vec![false; program.globals.len()],
            global_values: program
                .globals
                .iter()
                .map(|global| type_function_state(program, global.ty))
                .collect(),
        };
        for param in &function.signature.params {
            if param.local_id.index() < function.locals.len() {
                state.definite[param.local_id.index()] = true;
                state.possible[param.local_id.index()] = true;
                state.local_values[param.local_id.index()] = match program.type_arena.get(param.ty)
                {
                    Some(TypeData::Function(_)) => FunctionValueState::function(
                        FunctionValueCapability::from_param_escape(param.escape),
                    ),
                    _ => source_call_return_state(program, param.ty),
                };
            }
        }
        if let FunctionKind::Lambda(lambda) = function.kind
            && let Some(decl) = program.lambdas.get(lambda.index())
        {
            for capture in &decl.captures {
                if let LambdaCaptureDecl::CaptureCell { cell, .. } = capture
                    && cell.index() < state.cell_definite.len()
                {
                    state.cell_definite[cell.index()] = true;
                }
            }
        }
        state
    }

    fn is_definite(&self, local: LocalId) -> bool {
        self.definite.get(local.index()).copied().unwrap_or(false)
    }

    fn is_possible(&self, local: LocalId) -> bool {
        self.possible.get(local.index()).copied().unwrap_or(false)
    }

    fn init(&mut self, local: LocalId) {
        if local.index() < self.definite.len() {
            self.definite[local.index()] = true;
            self.possible[local.index()] = true;
        }
    }

    fn set_local_value(&mut self, local: LocalId, value: FunctionValueState) {
        if local.index() < self.local_values.len() {
            self.local_values[local.index()] = value;
        }
    }

    fn clear_local_value(&mut self, local: LocalId) {
        if local.index() < self.local_values.len() {
            self.local_values[local.index()] = FunctionValueState::unknown();
        }
    }

    fn set_place_value(&mut self, place: &Place, value: FunctionValueState) {
        match place.root {
            PlaceRoot::Local(local) if place.projection.is_empty() => {
                self.set_local_value(local, value);
            }
            PlaceRoot::Local(local) if local.index() < self.local_values.len() => {
                if !self.local_values[local.index()].assign_projection(&place.projection, value) {
                    self.clear_local_value(local);
                }
            }
            PlaceRoot::Global(global) if place.projection.is_empty() => {
                self.set_global_value(global, value);
            }
            PlaceRoot::Global(global) if global.index() < self.global_values.len() => {
                self.global_definite[global.index()] = true;
                if !self.global_values[global.index()].assign_projection(&place.projection, value) {
                    self.global_values[global.index()] = FunctionValueState::unknown();
                }
            }
            PlaceRoot::Local(_)
            | PlaceRoot::Global(_)
            | PlaceRoot::LambdaCapture(_)
            | PlaceRoot::ScopedBorrow(_)
            | PlaceRoot::DynBorrowParam(_)
            | PlaceRoot::CaptureCell(_) => {}
        }
    }

    fn push_place_value(&mut self, place: &Place, value: FunctionValueState) {
        match place.root {
            PlaceRoot::Local(local) if local.index() < self.local_values.len() => {
                if !self.local_values[local.index()].push_projection(&place.projection, value) {
                    self.clear_local_value(local);
                }
            }
            PlaceRoot::Global(global) if global.index() < self.global_values.len() => {
                self.global_definite[global.index()] = true;
                if !self.global_values[global.index()].push_projection(&place.projection, value) {
                    self.global_values[global.index()] = FunctionValueState::unknown();
                }
            }
            PlaceRoot::Local(_)
            | PlaceRoot::Global(_)
            | PlaceRoot::LambdaCapture(_)
            | PlaceRoot::ScopedBorrow(_)
            | PlaceRoot::DynBorrowParam(_)
            | PlaceRoot::CaptureCell(_) => {}
        }
    }

    fn clear_place_value(&mut self, place: &Place) {
        self.set_place_value(place, FunctionValueState::unknown());
    }

    fn escape(&self, local: LocalId) -> FunctionValueCapability {
        self.local_value(local).capability()
    }

    fn local_value(&self, local: LocalId) -> FunctionValueState {
        self.local_values
            .get(local.index())
            .cloned()
            .unwrap_or_else(FunctionValueState::unknown)
    }

    fn init_cell(&mut self, cell: CaptureCellId) {
        if cell.index() < self.cell_definite.len() {
            self.cell_definite[cell.index()] = true;
        }
    }

    fn cell_is_definite(&self, cell: CaptureCellId) -> bool {
        self.cell_definite
            .get(cell.index())
            .copied()
            .unwrap_or(false)
    }

    fn ensure_global(&mut self, global: GlobalId, value: FunctionValueState) {
        if global.index() < self.global_definite.len() {
            self.global_definite[global.index()] = true;
            self.global_values[global.index()] = value;
        }
    }

    fn set_global_value(&mut self, global: GlobalId, value: FunctionValueState) {
        if global.index() < self.global_values.len() {
            self.global_definite[global.index()] = true;
            self.global_values[global.index()] = value;
        }
    }

    fn global_is_definite(&self, global: GlobalId) -> bool {
        self.global_definite
            .get(global.index())
            .copied()
            .unwrap_or(false)
    }

    fn global_value(&self, global: GlobalId) -> FunctionValueState {
        self.global_values
            .get(global.index())
            .cloned()
            .unwrap_or_else(FunctionValueState::unknown)
    }

    fn clear(&mut self, local: LocalId) {
        if local.index() < self.definite.len() {
            self.definite[local.index()] = false;
            self.possible[local.index()] = false;
            self.local_values[local.index()] = FunctionValueState::unknown();
        }
    }

    fn join(states: impl IntoIterator<Item = Self>) -> Option<Self> {
        let mut states = states.into_iter();
        let mut joined = states.next()?;
        for state in states {
            for (left, right) in joined.definite.iter_mut().zip(state.definite) {
                *left &= right;
            }
            for (left, right) in joined.possible.iter_mut().zip(state.possible) {
                *left |= right;
            }
            for (left, right) in joined.local_values.iter_mut().zip(state.local_values) {
                *left = FunctionValueState::join(left.clone(), right);
            }
            for (left, right) in joined.cell_definite.iter_mut().zip(state.cell_definite) {
                *left &= right;
            }
            for (left, right) in joined.global_definite.iter_mut().zip(state.global_definite) {
                *left &= right;
            }
            for (left, right) in joined.global_values.iter_mut().zip(state.global_values) {
                *left = FunctionValueState::join(left.clone(), right);
            }
        }
        Some(joined)
    }
}

fn join_escape(
    left: FunctionValueCapability,
    right: FunctionValueCapability,
) -> FunctionValueCapability {
    match (left, right) {
        (FunctionValueCapability::Escaping, FunctionValueCapability::Escaping) => {
            FunctionValueCapability::Escaping
        }
        (FunctionValueCapability::NonFunction, FunctionValueCapability::NonFunction) => {
            FunctionValueCapability::NonFunction
        }
        (FunctionValueCapability::Unknown, _) | (_, FunctionValueCapability::Unknown) => {
            FunctionValueCapability::Unknown
        }
        (
            FunctionValueCapability::Escaping | FunctionValueCapability::NonEscaping,
            FunctionValueCapability::Escaping | FunctionValueCapability::NonEscaping,
        ) => FunctionValueCapability::NonEscaping,
        _ => FunctionValueCapability::Unknown,
    }
}

fn collect_errors(cx: &mut VerifyCx<'_>) {
    if let Some(entry) = cx.program.entry {
        match cx.program.functions.get(entry.index()) {
            Some(function)
                if matches!(
                    function.kind,
                    FunctionKind::Lambda(_) | FunctionKind::GlobalInit(_)
                ) =>
            {
                cx.push(
                    VerifySite::Program,
                    VerifyErrorKind::BadFunction(BadFunction::EntryMustBeNamed(entry)),
                );
            }
            Some(_) => {}
            None => cx.push(
                VerifySite::Program,
                VerifyErrorKind::BadReference(BadReference::InvalidEntry(entry)),
            ),
        }
    }

    for duplicate in cx.primitives.duplicates().to_vec() {
        cx.push(
            VerifySite::Type(duplicate.duplicate),
            VerifyErrorKind::BadType(BadType::DuplicatePrimitive {
                kind: duplicate.kind,
                first: duplicate.first,
                duplicate: duplicate.duplicate,
            }),
        );
    }

    collect_contract_errors(cx);
    for (id, _) in cx.program.type_arena.iter().enumerate() {
        let ty = TypeId::from_index(id);
        cx.verify_type_ref(VerifySite::Type(ty), ty);
    }
    verify_nominal_type_identity(cx);

    for (id, _) in cx.program.modules.iter().enumerate() {
        verify_module(cx, ModuleId::from_index(id));
    }
    for (id, _) in cx.program.const_arena.iter().enumerate() {
        verify_const(cx, ConstId::from_index(id));
    }
    for (id, _) in cx.program.aggregates.iter().enumerate() {
        verify_aggregate(cx, AggregateId::from_index(id));
    }
    for (id, _) in cx.program.enums.iter().enumerate() {
        verify_enum(cx, EnumId::from_index(id));
    }
    for (id, _) in cx.program.flags.iter().enumerate() {
        verify_flag(cx, FlagId::from_index(id));
    }
    for (id, _) in cx.program.extern_types.iter().enumerate() {
        verify_extern_type(cx, ExternTypeId::from_index(id));
    }
    for (id, _) in cx.program.externs.iter().enumerate() {
        verify_extern(cx, ExternId::from_index(id));
    }
    for (id, _) in cx.program.lambdas.iter().enumerate() {
        verify_lambda(cx, LambdaId::from_index(id));
    }
    for (id, _) in cx.program.dyn_borrow_params.iter().enumerate() {
        verify_dyn_borrow_param(cx, DynBorrowParamId::from_index(id));
    }
    for (id, _) in cx.program.scoped_borrows.iter().enumerate() {
        verify_scoped_borrow(cx, ScopedBorrowId::from_index(id));
    }
    verify_scoped_borrow_uniqueness(cx);
    for (id, _) in cx.program.capture_cells.iter().enumerate() {
        verify_capture_cell(cx, CaptureCellId::from_index(id));
    }
    verify_capture_cell_uniqueness(cx);
    for (id, _) in cx.program.globals.iter().enumerate() {
        verify_global(cx, GlobalId::from_index(id));
    }
    for (id, _) in cx.program.functions.iter().enumerate() {
        verify_function(cx, FunctionId::from_index(id));
    }
}

fn verify_nominal_type_identity(cx: &mut VerifyCx<'_>) {
    let mut aggregates = std::collections::HashMap::new();
    let mut enums = std::collections::HashMap::new();
    let mut flags = std::collections::HashMap::new();
    for (index, ty) in cx.program.type_arena.iter().enumerate() {
        let current = TypeId::from_index(index);
        match ty {
            TypeData::Aggregate(aggregate) | TypeData::DataRef(aggregate) => {
                let represented = match ty {
                    TypeData::Aggregate(_) => AggregateKind::Struct,
                    TypeData::DataRef(_) => AggregateKind::DataRef,
                    _ => unreachable!(),
                };
                if let Some(decl) = cx.program.aggregates.get(aggregate.index())
                    && decl.kind != represented
                {
                    cx.push(
                        VerifySite::Type(current),
                        VerifyErrorKind::BadType(BadType::AggregateKindMismatch {
                            aggregate: *aggregate,
                            declared: decl.kind,
                            represented,
                        }),
                    );
                }
                if let Some(first) = aggregates.insert(*aggregate, current) {
                    cx.push(
                        VerifySite::Type(current),
                        VerifyErrorKind::BadType(BadType::DuplicateAggregate {
                            aggregate: *aggregate,
                            first,
                            duplicate: current,
                        }),
                    );
                }
            }
            TypeData::Enum(enm) => {
                if let Some(first) = enums.insert(*enm, current) {
                    cx.push(
                        VerifySite::Type(current),
                        VerifyErrorKind::BadType(BadType::DuplicateEnum {
                            enm: *enm,
                            first,
                            duplicate: current,
                        }),
                    );
                }
            }
            TypeData::Flag(flag) => {
                if let Some(first) = flags.insert(*flag, current) {
                    cx.push(
                        VerifySite::Type(current),
                        VerifyErrorKind::BadType(BadType::DuplicateFlag {
                            flag: *flag,
                            first,
                            duplicate: current,
                        }),
                    );
                }
            }
            _ => {}
        }
    }
    for index in 0..cx.program.aggregates.len() {
        let aggregate = AggregateId::from_index(index);
        if !aggregates.contains_key(&aggregate) {
            cx.push(
                VerifySite::Program,
                VerifyErrorKind::BadType(BadType::MissingAggregate(aggregate)),
            );
        }
    }
    for index in 0..cx.program.enums.len() {
        let enm = EnumId::from_index(index);
        if !enums.contains_key(&enm) {
            cx.push(
                VerifySite::Program,
                VerifyErrorKind::BadType(BadType::MissingEnum(enm)),
            );
        }
    }
    for index in 0..cx.program.flags.len() {
        let flag = FlagId::from_index(index);
        if !flags.contains_key(&flag) {
            cx.push(
                VerifySite::Program,
                VerifyErrorKind::BadType(BadType::MissingFlag(flag)),
            );
        }
    }
}

fn collect_contract_errors(cx: &mut VerifyCx<'_>) {
    let mut surface_keys = std::collections::HashSet::new();
    for (id, surface) in cx.program.contract_surfaces.iter().enumerate() {
        let id = ContractSurfaceId::from_index(id);
        if !surface_keys.insert(&surface.slots) {
            cx.push(
                VerifySite::ContractSurface(id),
                VerifyErrorKind::BadContract(BadContract::DuplicateSurface),
            );
        }
        verify_contract_surface(cx, id);
    }
    let mut witness_keys = std::collections::HashSet::new();
    for (id, witness) in cx.program.contract_witnesses.iter().enumerate() {
        let id = ContractWitnessId::from_index(id);
        if !witness_keys.insert(&witness.key) {
            cx.push(
                VerifySite::ContractWitness(id),
                VerifyErrorKind::BadContract(BadContract::DuplicateWitnessKey),
            );
        }
        verify_contract_witness(cx, id);
    }
    let mut weakening_keys = std::collections::HashSet::new();
    for (id, weakening) in cx.program.contract_weakenings.iter().enumerate() {
        let id = ContractWeakeningId::from_index(id);
        if !weakening_keys.insert(weakening) {
            cx.push(
                VerifySite::ContractWeakening(id),
                VerifyErrorKind::BadContract(BadContract::DuplicateWeakening),
            );
        }
        verify_contract_weakening(cx, id);
    }
    verify_projected_contract_witnesses(cx);
}

fn verify_contract_surface(cx: &mut VerifyCx<'_>, id: ContractSurfaceId) {
    let surface = cx.program.contract_surface(id).clone();
    let site = VerifySite::ContractSurface(id);
    if surface.display_name.is_empty() {
        cx.push(
            site.clone(),
            VerifyErrorKind::BadContract(BadContract::EmptyDisplayName),
        );
    }
    if surface.slots.is_empty() {
        cx.push(
            site,
            VerifyErrorKind::BadContract(BadContract::EmptySurface),
        );
    }
    let mut slot_names = std::collections::HashSet::new();
    for (index, slot) in surface.slots.iter().enumerate() {
        let slot_site = VerifySite::ContractSlot {
            surface: id,
            slot: slot.id,
        };
        if slot.id.index() != index {
            cx.push(
                slot_site.clone(),
                VerifyErrorKind::BadContract(BadContract::SlotIdMismatch),
            );
        }
        if slot.name.as_str().is_empty() {
            cx.push(
                slot_site.clone(),
                VerifyErrorKind::BadContract(BadContract::EmptySlotName),
            );
        } else if !slot_names.insert(slot.name) {
            cx.push(
                slot_site.clone(),
                VerifyErrorKind::BadContract(BadContract::DuplicateSlotName),
            );
        }
        for param in &slot.params {
            cx.verify_type_ref(slot_site.clone(), param.ty);
        }
        match slot.ret {
            ContractReturnDecl::Value(ty) | ContractReturnDecl::Place(ty) => {
                cx.verify_type_ref(slot_site.clone(), ty);
            }
            ContractReturnDecl::Iter => {}
        }
    }
}

fn verify_contract_witness(cx: &mut VerifyCx<'_>, id: ContractWitnessId) {
    let witness = cx.program.contract_witness(id).clone();
    let site = VerifySite::ContractWitness(id);
    cx.verify_type_ref(site.clone(), witness.key.concrete_ty);
    if matches!(
        cx.program.type_arena.get(witness.key.concrete_ty),
        Some(TypeData::Dyn(_))
    ) {
        cx.push(
            site.clone(),
            VerifyErrorKind::BadContract(BadContract::InvalidWitnessConcreteType),
        );
    }
    let Some(surface) = cx
        .program
        .contract_surfaces
        .get(witness.key.surface.index())
        .cloned()
    else {
        cx.push(
            site,
            VerifyErrorKind::BadReference(BadReference::InvalidContractSurface(
                witness.key.surface,
            )),
        );
        return;
    };
    if witness.key.slots.len() != surface.slots.len() {
        cx.push(
            site.clone(),
            VerifyErrorKind::BadContract(BadContract::WitnessSlotCountMismatch),
        );
    }
    for (index, slot) in witness.key.slots.iter().enumerate() {
        let Some(required) = surface.slots.get(slot.slot.index()) else {
            cx.push(
                site.clone(),
                VerifyErrorKind::BadReference(BadReference::InvalidContractSlot {
                    surface: witness.key.surface,
                    slot: slot.slot,
                }),
            );
            continue;
        };
        if slot.slot.index() != index {
            cx.push(
                site.clone(),
                VerifyErrorKind::BadContract(BadContract::WitnessSlotIdMismatch),
            );
        }
        let receiver_compatible = contract_receiver_mode_matches(required.receiver, slot.receiver);
        if !receiver_compatible {
            cx.push(
                site.clone(),
                VerifyErrorKind::BadContract(BadContract::WitnessReceiverMismatch),
            );
        }
        verify_contract_witness_target(
            cx,
            site.clone(),
            witness.key.concrete_ty,
            required,
            slot,
            &slot.target,
            false,
        );
    }
}

fn verify_contract_function_target(
    cx: &mut VerifyCx<'_>,
    site: VerifySite,
    receiver_ty: TypeId,
    required: &ContractSlotDecl,
    witness: &ContractWitnessSlotDecl,
    function: FunctionId,
    iter: bool,
    check_name: bool,
) {
    let Some(function) = cx.program.functions.get(function.index()).cloned() else {
        cx.push(
            site,
            VerifyErrorKind::BadReference(BadReference::InvalidFunction(function)),
        );
        return;
    };
    let Some(receiver) = function
        .signature
        .params
        .first()
        .filter(|param| param.role == ParamRole::Receiver)
    else {
        cx.push(
            site,
            VerifyErrorKind::BadContract(BadContract::WitnessTargetSignatureMismatch),
        );
        return;
    };
    let params_match = contract_params_match(
        &required.params,
        function.signature.params[1..].iter().map(Param::param_type),
    );
    let return_matches = if iter {
        matches!(required.ret, ContractReturnDecl::Iter)
    } else {
        contract_return_matches(required.ret, function.signature.return_mode)
    };
    if (check_name && function.name != required.name)
        || receiver.ty != receiver_ty
        || !witness_target_mode_matches(witness.receiver, receiver.mode)
        || !params_match
        || !return_matches
    {
        cx.push(
            site,
            VerifyErrorKind::BadContract(BadContract::WitnessTargetSignatureMismatch),
        );
    }
}

fn verify_contract_witness_target(
    cx: &mut VerifyCx<'_>,
    site: VerifySite,
    receiver_ty: TypeId,
    required: &ContractSlotDecl,
    witness: &ContractWitnessSlotDecl,
    target: &ContractWitnessTarget,
    promoted: bool,
) {
    match target {
        ContractWitnessTarget::Function { function } => verify_contract_function_target(
            cx,
            site,
            receiver_ty,
            required,
            witness,
            *function,
            false,
            !promoted,
        ),
        ContractWitnessTarget::IteratorFunction { function } => verify_contract_function_target(
            cx,
            site,
            receiver_ty,
            required,
            witness,
            *function,
            true,
            !promoted,
        ),
        ContractWitnessTarget::Extern { function } => {
            let Some(function) = cx.program.externs.get(function.index()).cloned() else {
                cx.push(
                    site,
                    VerifyErrorKind::BadReference(BadReference::InvalidExtern(*function)),
                );
                return;
            };
            let ExternMember::Method { receiver, .. } = function.member else {
                cx.push(
                    site,
                    VerifyErrorKind::BadContract(BadContract::WitnessTargetSignatureMismatch),
                );
                return;
            };
            let params_match = contract_params_match(
                &required.params,
                function.params.iter().map(|param| ParamType {
                    ty: param.ty,
                    mode: param.mode,
                    escape: param.escape,
                }),
            );
            let return_matches = match required.ret {
                ContractReturnDecl::Value(ty) => function.return_type == ty,
                ContractReturnDecl::Place(_) | ContractReturnDecl::Iter => false,
            };
            if (!promoted && function.name != required.name)
                || receiver.ty != receiver_ty
                || !witness_target_mode_matches(witness.receiver, receiver.mode)
                || !params_match
                || !return_matches
            {
                cx.push(
                    site,
                    VerifyErrorKind::BadContract(BadContract::WitnessTargetSignatureMismatch),
                );
            }
        }
        ContractWitnessTarget::Promoted { fields, target } => {
            if fields.is_empty() {
                cx.push(
                    site,
                    VerifyErrorKind::BadContract(BadContract::InvalidWitnessProjection),
                );
                return;
            }
            let mut ty = receiver_ty;
            for field in fields {
                let aggregate = match cx.program.type_arena.get(ty) {
                    Some(TypeData::Aggregate(id) | TypeData::DataRef(id)) => {
                        cx.program.aggregates.get(id.index())
                    }
                    _ => None,
                };
                let Some(aggregate) = aggregate else {
                    cx.push(
                        site,
                        VerifyErrorKind::BadContract(BadContract::InvalidWitnessProjection),
                    );
                    return;
                };
                let Some(field) = aggregate.fields.get(field.index()) else {
                    cx.push(
                        site,
                        VerifyErrorKind::BadContract(BadContract::InvalidWitnessProjection),
                    );
                    return;
                };
                ty = field.ty;
            }
            verify_contract_witness_target(cx, site, ty, required, witness, target, true);
        }
    }
}

fn contract_receiver_mode_matches(receiver: ContractReceiver, mode: ParamMode) -> bool {
    match receiver {
        ContractReceiver::Value => matches!(mode, ParamMode::Value | ParamMode::SharedBorrow),
        ContractReceiver::Ref => true,
    }
}

fn witness_target_mode_matches(witness: ParamMode, target: ParamMode) -> bool {
    match witness {
        ParamMode::SharedBorrow => matches!(target, ParamMode::SharedBorrow | ParamMode::Value),
        ParamMode::Value => target == ParamMode::Value,
        ParamMode::MutBorrow => target == ParamMode::MutBorrow,
    }
}

fn contract_params_match(
    required: &[ContractParamDecl],
    actual: impl Iterator<Item = ParamType>,
) -> bool {
    let actual = actual.collect::<Vec<_>>();
    required.len() == actual.len()
        && required.iter().zip(actual).all(|(required, actual)| {
            let mode_matches = match required.mode {
                ParamMode::SharedBorrow => {
                    matches!(actual.mode, ParamMode::SharedBorrow | ParamMode::Value)
                }
                ParamMode::MutBorrow => actual.mode == ParamMode::MutBorrow,
                ParamMode::Value => actual.mode == ParamMode::Value,
            };
            required.ty == actual.ty && required.escape == actual.escape && mode_matches
        })
}

fn contract_return_matches(required: ContractReturnDecl, actual: ReturnMode) -> bool {
    match (required, actual) {
        (ContractReturnDecl::Value(expected), ReturnMode::Value(actual))
        | (ContractReturnDecl::Place(expected), ReturnMode::Place(actual)) => expected == actual,
        _ => false,
    }
}

fn verify_contract_weakening(cx: &mut VerifyCx<'_>, id: ContractWeakeningId) {
    let weakening = cx.program.contract_weakening(id).clone();
    let site = VerifySite::ContractWeakening(id);
    let Some(source) = cx.program.contract_surfaces.get(weakening.source.index()) else {
        cx.push(
            site.clone(),
            VerifyErrorKind::BadReference(BadReference::InvalidContractSurface(weakening.source)),
        );
        return;
    };
    let Some(target) = cx.program.contract_surfaces.get(weakening.target.index()) else {
        cx.push(
            site.clone(),
            VerifyErrorKind::BadReference(BadReference::InvalidContractSurface(weakening.target)),
        );
        return;
    };
    if weakening.target_to_source.len() != target.slots.len() {
        cx.push(
            site.clone(),
            VerifyErrorKind::BadContract(BadContract::WeakeningSlotCountMismatch),
        );
        return;
    }
    if weakening.source == weakening.target || target.slots.len() >= source.slots.len() {
        cx.push(
            site.clone(),
            VerifyErrorKind::BadContract(BadContract::WeakeningNotProper),
        );
        return;
    }
    for (target_slot, source_slot) in target.slots.iter().zip(&weakening.target_to_source) {
        let Some(source_slot) = source.slots.get(source_slot.index()) else {
            cx.push(
                site.clone(),
                VerifyErrorKind::BadReference(BadReference::InvalidContractSlot {
                    surface: weakening.source,
                    slot: *source_slot,
                }),
            );
            continue;
        };
        let same_signature = source_slot.name == target_slot.name
            && source_slot.receiver == target_slot.receiver
            && source_slot.params == target_slot.params
            && source_slot.ret == target_slot.ret;
        if !same_signature {
            cx.push(
                site.clone(),
                VerifyErrorKind::BadContract(BadContract::WeakeningSlotSignatureMismatch),
            );
        }
    }
}

fn verify_projected_contract_witnesses(cx: &mut VerifyCx<'_>) {
    for (weakening_index, weakening) in cx.program.contract_weakenings.iter().enumerate() {
        for witness in &cx.program.contract_witnesses {
            if witness.key.surface != weakening.source {
                continue;
            }
            let slots = weakening
                .target_to_source
                .iter()
                .enumerate()
                .filter_map(|(target_slot, source_slot)| {
                    let source = witness.key.slots.get(source_slot.index())?;
                    Some(ContractWitnessSlotDecl {
                        slot: ContractSlotId::from_index(target_slot),
                        receiver: source.receiver,
                        target: source.target.clone(),
                    })
                })
                .collect::<Vec<_>>();
            if slots.len() != weakening.target_to_source.len() {
                continue;
            }
            let key = ContractWitnessKey {
                concrete_ty: witness.key.concrete_ty,
                surface: weakening.target,
                slots,
            };
            if !cx
                .program
                .contract_witnesses
                .iter()
                .any(|candidate| candidate.key == key)
            {
                cx.push(
                    VerifySite::ContractWeakening(ContractWeakeningId::from_index(weakening_index)),
                    VerifyErrorKind::BadContract(BadContract::MissingProjectedWitness),
                );
            }
        }
    }
}

fn verify_module(cx: &mut VerifyCx<'_>, id: ModuleId) {
    let module = cx.program.module(id);
    let site = VerifySite::Module(id);
    verify_module_items(cx, &site, id, &module.functions, |cx, item| {
        cx.program
            .functions
            .get(item.index())
            .map(|decl| decl.module)
    });
    verify_module_items(cx, &site, id, &module.globals, |cx, item| {
        cx.program.globals.get(item.index()).map(|decl| decl.module)
    });
    verify_module_items(cx, &site, id, &module.aggregates, |cx, item| {
        cx.program
            .aggregates
            .get(item.index())
            .map(|decl| decl.module)
    });
    verify_module_items(cx, &site, id, &module.enums, |cx, item| {
        cx.program.enums.get(item.index()).map(|decl| decl.module)
    });
    verify_module_items(cx, &site, id, &module.flags, |cx, item| {
        cx.program.flags.get(item.index()).map(|decl| decl.module)
    });
    verify_module_items(cx, &site, id, &module.extern_types, |cx, item| {
        cx.program
            .extern_types
            .get(item.index())
            .map(|decl| decl.module)
    });
    verify_module_items(cx, &site, id, &module.externs, |cx, item| {
        cx.program.externs.get(item.index()).map(|decl| decl.module)
    });
}

fn verify_module_items<T>(
    cx: &mut VerifyCx<'_>,
    site: &VerifySite,
    module: ModuleId,
    items: &[T],
    owner: impl Fn(&VerifyCx<'_>, T) -> Option<ModuleId>,
) where
    T: Copy + Eq + std::hash::Hash + IntoModuleReference,
{
    let mut seen = std::collections::HashSet::new();
    for item in items {
        if !seen.insert(*item) {
            cx.push(
                site.clone(),
                VerifyErrorKind::BadModule(BadModule::DuplicateItem((*item).module_item())),
            );
        }
        match owner(cx, *item) {
            Some(found) if found == module => {}
            Some(found) => cx.push(
                site.clone(),
                VerifyErrorKind::BadModule(BadModule::ItemWrongModule {
                    item: (*item).module_item(),
                    expected: module,
                    found,
                }),
            ),
            None => cx.push(site.clone(), (*item).invalid_reference()),
        }
    }
}

trait IntoModuleReference {
    fn invalid_reference(self) -> VerifyErrorKind;
    fn module_item(self) -> ModuleItem;
}

macro_rules! impl_module_ref {
    ($id:ty, $bad:ident, $item:ident) => {
        impl IntoModuleReference for $id {
            fn invalid_reference(self) -> VerifyErrorKind {
                VerifyErrorKind::BadReference(BadReference::$bad(self))
            }

            fn module_item(self) -> ModuleItem {
                ModuleItem::$item(self)
            }
        }
    };
}

impl_module_ref!(FunctionId, InvalidFunction, Function);
impl_module_ref!(GlobalId, InvalidGlobal, Global);
impl_module_ref!(AggregateId, InvalidAggregate, Aggregate);
impl_module_ref!(EnumId, InvalidEnum, Enum);
impl_module_ref!(FlagId, InvalidFlag, Flag);
impl_module_ref!(ExternTypeId, InvalidExternType, ExternType);
impl_module_ref!(ExternId, InvalidExtern, Extern);

fn verify_lambda(cx: &mut VerifyCx<'_>, id: LambdaId) {
    let decl = &cx.program.lambdas[id.index()];
    let site = VerifySite::Lambda(id);
    cx.verify_module_ref(site.clone(), decl.module);
    cx.verify_type_ref(site.clone(), decl.signature.ret.ty());
    if !cx.has_function(decl.owner) {
        cx.push(
            site.clone(),
            VerifyErrorKind::BadReference(BadReference::InvalidFunction(decl.owner)),
        );
    }
    verify_signature_type(cx, site.clone(), &decl.signature);
    match cx.program.functions.get(decl.body.index()) {
        Some(function) if function.kind == FunctionKind::Lambda(id) => {
            if function.module != decl.module {
                cx.push(
                    site.clone(),
                    VerifyErrorKind::BadFunction(BadFunction::LambdaBodyModuleMismatch {
                        lambda: id,
                        expected: decl.module,
                        found: function.module,
                    }),
                );
            }
            if function_signature_type(function) != decl.signature {
                cx.push(
                    site.clone(),
                    VerifyErrorKind::BadFunction(BadFunction::LambdaBodySignatureMismatch {
                        lambda: id,
                        body: decl.body,
                    }),
                );
            }
        }
        Some(_) => cx.push(
            site.clone(),
            VerifyErrorKind::BadFunction(BadFunction::LambdaBodyKindMismatch {
                lambda: id,
                body: decl.body,
            }),
        ),
        None => cx.push(
            site.clone(),
            VerifyErrorKind::BadReference(BadReference::InvalidFunction(decl.body)),
        ),
    }
    let mut bindings = std::collections::HashMap::new();
    let mut sources = std::collections::HashMap::new();
    for (index, capture) in decl.captures.iter().enumerate() {
        let binding = capture.binding();
        if let Some(first) = bindings.insert(binding, index) {
            cx.push(
                site.clone(),
                VerifyErrorKind::BadFunction(BadFunction::DuplicateLambdaCapture {
                    lambda: id,
                    binding,
                    first,
                    second: index,
                }),
            );
        }
        if let Some(source) = lambda_capture_decl_source(capture)
            && let Some(first) = sources.insert(source, index)
        {
            cx.push(
                site.clone(),
                VerifyErrorKind::BadFunction(BadFunction::DuplicateLambdaCaptureSource {
                    lambda: id,
                    first,
                    second: index,
                }),
            );
        }
        if decl.escape == LambdaEscape::Escaping
            && matches!(
                capture,
                LambdaCaptureDecl::ScopedLocal { .. } | LambdaCaptureDecl::ScopedBorrow { .. }
            )
        {
            cx.push(
                site.clone(),
                VerifyErrorKind::BadFunction(BadFunction::EscapingLambdaScopedCapture {
                    lambda: id,
                }),
            );
        }
        verify_lambda_capture_decl(cx, site.clone(), id, index, decl, capture);
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
enum LambdaCaptureSourceKey {
    Local(CaptureLocalSource),
    ScopedBorrow(ScopedBorrowId),
    CaptureCell(CaptureCellId),
}

fn lambda_capture_decl_source(capture: &LambdaCaptureDecl) -> Option<LambdaCaptureSourceKey> {
    Some(match capture {
        LambdaCaptureDecl::ReadonlyLocal { source, .. }
        | LambdaCaptureDecl::ScopedLocal { source, .. } => LambdaCaptureSourceKey::Local(*source),
        LambdaCaptureDecl::ScopedBorrow { borrow, .. } => {
            LambdaCaptureSourceKey::ScopedBorrow(*borrow)
        }
        LambdaCaptureDecl::CaptureCell { cell, .. } => LambdaCaptureSourceKey::CaptureCell(*cell),
        LambdaCaptureDecl::NoRuntime { .. } => return None,
    })
}

fn verify_lambda_capture_decl(
    cx: &mut VerifyCx<'_>,
    site: VerifySite,
    lambda: LambdaId,
    index: usize,
    decl: &LambdaDecl,
    capture: &LambdaCaptureDecl,
) {
    match capture {
        LambdaCaptureDecl::NoRuntime { ty, .. } => cx.verify_type_ref(site, *ty),
        LambdaCaptureDecl::ReadonlyLocal {
            binding,
            source,
            ty,
        } => {
            cx.verify_type_ref(site.clone(), *ty);
            verify_capture_local_source(
                cx,
                site.clone(),
                lambda,
                index,
                decl.owner,
                *binding,
                *source,
                *ty,
                CaptureLocalAccess::Readonly,
            );
            if decl.escape == LambdaEscape::Escaping {
                verify_escaping_function_capture(cx, site, lambda, *source, *ty);
            }
        }
        LambdaCaptureDecl::ScopedLocal {
            binding,
            source,
            ty,
            mutability,
        } => {
            cx.verify_type_ref(site.clone(), *ty);
            verify_capture_local_source(
                cx,
                site,
                lambda,
                index,
                decl.owner,
                *binding,
                *source,
                *ty,
                CaptureLocalAccess::Scoped(*mutability),
            );
        }
        LambdaCaptureDecl::ScopedBorrow {
            binding,
            borrow,
            ty,
            mutability,
        } => {
            cx.verify_type_ref(site.clone(), *ty);
            match cx.program.scoped_borrows.get(borrow.index()) {
                Some(borrow_decl)
                    if borrow_decl.binding == *binding
                        && borrow_decl.ty == *ty
                        && borrow_decl.mutability == *mutability =>
                {
                    if !function_can_access_scoped_borrow(cx.program, decl.owner, *borrow) {
                        cx.push(
                            site,
                            VerifyErrorKind::BadFunction(
                                BadFunction::LambdaScopedBorrowNotAccessible {
                                    lambda,
                                    owner: decl.owner,
                                    borrow: *borrow,
                                },
                            ),
                        );
                    }
                }
                Some(_) => cx.push(
                    site,
                    VerifyErrorKind::BadFunction(BadFunction::LambdaCaptureSourceMismatch {
                        lambda,
                        index,
                    }),
                ),
                None => cx.push(
                    site,
                    VerifyErrorKind::BadReference(BadReference::InvalidScopedBorrow(*borrow)),
                ),
            }
        }
        LambdaCaptureDecl::CaptureCell { binding, cell, ty } => {
            cx.verify_type_ref(site.clone(), *ty);
            match cx.program.capture_cells.get(cell.index()) {
                Some(cell_decl) if cell_decl.binding == *binding && cell_decl.ty == *ty => {
                    if !function_can_access_capture_cell(cx.program, decl.owner, *cell) {
                        cx.push(
                            site,
                            VerifyErrorKind::BadFunction(
                                BadFunction::LambdaCaptureCellNotAccessible {
                                    lambda,
                                    owner: decl.owner,
                                    cell: *cell,
                                },
                            ),
                        );
                    }
                }
                Some(_) => cx.push(
                    site,
                    VerifyErrorKind::BadFunction(BadFunction::LambdaCaptureSourceMismatch {
                        lambda,
                        index,
                    }),
                ),
                None => cx.push(
                    site,
                    VerifyErrorKind::BadReference(BadReference::InvalidCaptureCell(*cell)),
                ),
            }
        }
    }
}

#[derive(Clone, Copy)]
enum CaptureLocalAccess {
    Readonly,
    Scoped(Mutability),
}

fn function_captures_local_source(
    program: &Program,
    function_id: FunctionId,
    binding: BindingId,
    source: CaptureLocalSource,
    ty: TypeId,
) -> bool {
    let Some(function) = program.functions.get(function_id.index()) else {
        return false;
    };
    let FunctionKind::Lambda(lambda) = function.kind else {
        return false;
    };
    program.lambdas[lambda.index()]
        .captures
        .iter()
        .any(|capture| lambda_capture_matches_local_source(capture, binding, source, ty))
}

fn lambda_capture_matches_local_source(
    capture: &LambdaCaptureDecl,
    binding: BindingId,
    source: CaptureLocalSource,
    ty: TypeId,
) -> bool {
    capture.binding() == binding
        && capture.ty() == ty
        && matches!(
            lambda_capture_decl_source(capture),
            Some(LambdaCaptureSourceKey::Local(found)) if found == source
        )
}

fn verify_capture_local_source(
    cx: &mut VerifyCx<'_>,
    site: VerifySite,
    lambda: LambdaId,
    index: usize,
    expected_owner: FunctionId,
    binding: BindingId,
    source: CaptureLocalSource,
    ty: TypeId,
    access: CaptureLocalAccess,
) {
    if source.owner != expected_owner
        && !function_captures_local_source(cx.program, expected_owner, binding, source, ty)
    {
        cx.push(
            site,
            VerifyErrorKind::BadFunction(BadFunction::LambdaCaptureSourceMismatch {
                lambda,
                index,
            }),
        );
        return;
    }
    match cx.program.functions.get(source.owner.index()) {
        Some(function) => match function.locals.get(source.local.index()) {
            Some(local) if local.binding == Some(binding) && local.ty == ty => {
                verify_capture_local_access(
                    cx,
                    site,
                    lambda,
                    index,
                    function,
                    source.local,
                    local,
                    access,
                );
            }
            Some(_) => cx.push(
                site,
                VerifyErrorKind::BadFunction(BadFunction::LambdaCaptureSourceMismatch {
                    lambda,
                    index,
                }),
            ),
            None => cx.push(
                site,
                VerifyErrorKind::BadReference(BadReference::InvalidLocal(source.local)),
            ),
        },
        None => cx.push(
            site,
            VerifyErrorKind::BadReference(BadReference::InvalidFunction(source.owner)),
        ),
    }
}

fn verify_capture_local_access(
    cx: &mut VerifyCx<'_>,
    site: VerifySite,
    lambda: LambdaId,
    index: usize,
    function: &Function,
    local_id: LocalId,
    local: &Local,
    access: CaptureLocalAccess,
) {
    match access {
        CaptureLocalAccess::Readonly => {
            if !readonly_local_source_is_valid(function, local_id, local) {
                cx.push(
                    site,
                    VerifyErrorKind::BadFunction(
                        BadFunction::ReadonlyLambdaCaptureSourceMustBeImmutableOwned {
                            lambda,
                            index,
                            local: local_id,
                        },
                    ),
                );
            }
        }
        CaptureLocalAccess::Scoped(mutability) => {
            if local.mutability != mutability {
                cx.push(
                    site,
                    VerifyErrorKind::BadFunction(BadFunction::LambdaCaptureSourceMismatch {
                        lambda,
                        index,
                    }),
                );
            }
        }
    }
}

fn readonly_local_source_is_valid(function: &Function, local_id: LocalId, local: &Local) -> bool {
    if local.mutability == Mutability::Mutable {
        return false;
    }
    match local.kind {
        LocalKind::User => true,
        LocalKind::Arg => function
            .signature
            .params
            .iter()
            .find(|param| param.local_id == local_id)
            .is_some_and(|param| param.mode == ParamMode::Value),
        LocalKind::Return | LocalKind::Temp | LocalKind::PatternBinding => false,
    }
}

fn verify_escaping_function_capture(
    cx: &mut VerifyCx<'_>,
    site: VerifySite,
    lambda: LambdaId,
    source: CaptureLocalSource,
    ty: TypeId,
) {
    if !matches!(cx.type_data(ty), Some(TypeData::Function(_))) {
        return;
    }
    let Some(function) = cx.program.functions.get(source.owner.index()) else {
        return;
    };
    let Some(param) = function
        .signature
        .params
        .iter()
        .find(|param| param.local_id == source.local)
    else {
        return;
    };
    if param.escape == ParamEscape::NonEscaping {
        cx.push(
            site,
            VerifyErrorKind::BadFunction(BadFunction::EscapingLambdaCapturesNonEscapingFunction {
                lambda,
                local: source.local,
            }),
        );
    }
}

fn verify_signature_type(cx: &mut VerifyCx<'_>, site: VerifySite, sig: &SignatureType) {
    for param in &sig.params {
        cx.verify_type_ref(site.clone(), param.ty);
    }
    cx.verify_type_ref(site, sig.ret.ty());
}

fn verify_dyn_borrow_param(cx: &mut VerifyCx<'_>, id: DynBorrowParamId) {
    let decl = cx.program.dyn_borrow_params[id.index()].clone();
    let site = VerifySite::DynBorrowParam(id);
    cx.verify_type_ref(site.clone(), decl.ty);
    let valid = cx
        .program
        .functions
        .get(decl.owner.index())
        .is_some_and(|function| {
            function.signature.params.iter().any(|param| {
                param.local_id == decl.source
                    && param.mode == ParamMode::MutBorrow
                    && param.ty == decl.ty
            })
        })
        && matches!(cx.program.type_arena.get(decl.ty), Some(TypeData::Dyn(surface)) if *surface == decl.surface);
    if !valid {
        cx.push(
            site,
            VerifyErrorKind::BadPlace(BadPlace::InvalidDynBorrowParam(id)),
        );
    }
}

fn verify_scoped_borrow(cx: &mut VerifyCx<'_>, id: ScopedBorrowId) {
    let decl = cx.program.scoped_borrows[id.index()].clone();
    let site = VerifySite::ScopedBorrow(id);
    if !cx.has_function(decl.owner) {
        cx.push(
            site.clone(),
            VerifyErrorKind::BadReference(BadReference::InvalidFunction(decl.owner)),
        );
    }
    match &decl.source {
        ScopedBorrowSource::SourceMutParam { local } => {
            verify_source_scoped_borrow(cx, site.clone(), id, &decl, *local, ParamRole::Normal);
        }
        ScopedBorrowSource::RefSelf { local } => {
            verify_source_scoped_borrow(cx, site.clone(), id, &decl, *local, ParamRole::Receiver);
        }
        ScopedBorrowSource::PatternAlias { source } => {
            verify_pattern_alias_scoped_borrow(cx, site.clone(), id, &decl, source);
        }
        ScopedBorrowSource::ForRefAlias { source } => {
            verify_for_ref_alias_scoped_borrow(cx, site.clone(), id, &decl, source);
        }
    }
    if decl.mutability != Mutability::Mutable {
        cx.push(
            site.clone(),
            VerifyErrorKind::BadFunction(BadFunction::ScopedBorrowSourceMustBeMutable {
                borrow: id,
            }),
        );
    }
    cx.verify_type_ref(site, decl.ty);
}

fn verify_source_scoped_borrow(
    cx: &mut VerifyCx<'_>,
    site: VerifySite,
    id: ScopedBorrowId,
    decl: &ScopedBorrowDecl,
    local: LocalId,
    role: ParamRole,
) {
    let Some(function) = cx.program.functions.get(decl.owner.index()) else {
        return;
    };
    match function.locals.get(local.index()) {
        Some(source) => {
            if source.ty != decl.ty {
                cx.push(
                    site.clone(),
                    VerifyErrorKind::BadFunction(
                        BadFunction::ScopedBorrowSourceLocalTypeMismatch {
                            borrow: id,
                            expected: decl.ty,
                            found: source.ty,
                        },
                    ),
                );
            }
            if source.binding != Some(decl.binding) {
                cx.push(
                    site.clone(),
                    VerifyErrorKind::BadFunction(
                        BadFunction::ScopedBorrowSourceLocalBindingMismatch {
                            borrow: id,
                            expected: decl.binding,
                            found: source.binding,
                        },
                    ),
                );
            }
            if !function_param_is_mut_borrow(function, local, role) {
                cx.push(
                    site,
                    VerifyErrorKind::BadFunction(
                        BadFunction::ScopedBorrowSourceLocalMustBeMutParam { borrow: id, local },
                    ),
                );
            }
        }
        None => cx.push(
            site,
            VerifyErrorKind::BadFunction(BadFunction::ScopedBorrowSourceLocalMismatch {
                borrow: id,
                owner: decl.owner,
                local,
            }),
        ),
    }
}

fn verify_pattern_alias_scoped_borrow(
    cx: &mut VerifyCx<'_>,
    site: VerifySite,
    id: ScopedBorrowId,
    decl: &ScopedBorrowDecl,
    source: &Place,
) {
    if source.ty != decl.ty {
        cx.push(
            site.clone(),
            VerifyErrorKind::BadFunction(BadFunction::ScopedBorrowSourceLocalTypeMismatch {
                borrow: id,
                expected: decl.ty,
                found: source.ty,
            }),
        );
    }
    verify_place(cx, decl.owner, BlockId::from_index(0), None, source);
    verify_mutable_place(cx, decl.owner, &site, source);
    let Some(function) = cx.program.functions.get(decl.owner.index()) else {
        return;
    };
    let Some(root_local) = source.root.local() else {
        cx.push(
            site.clone(),
            VerifyErrorKind::BadPlace(BadPlace::UnsupportedRoot(source.root)),
        );
        return;
    };
    let pattern_binding = function
        .locals
        .get(root_local.index())
        .is_some_and(|local| {
            local.kind == LocalKind::PatternBinding
                && local.mutability == Mutability::Mutable
                && local.binding == Some(decl.binding)
        });
    if !pattern_binding && !function_local_is_any_mut_borrow(function, root_local) {
        cx.push(
            site.clone(),
            VerifyErrorKind::BadFunction(BadFunction::ScopedBorrowSourceLocalMustBeMutParam {
                borrow: id,
                local: root_local,
            }),
        );
    }
    if !source
        .projection
        .iter()
        .all(|projection| matches!(projection, Projection::Field(_) | Projection::TupleField(_)))
    {
        cx.push(
            site,
            VerifyErrorKind::BadPlace(BadPlace::UnsupportedScopedBorrowProjection(id)),
        );
    }
}

fn verify_for_ref_alias_scoped_borrow(
    cx: &mut VerifyCx<'_>,
    site: VerifySite,
    id: ScopedBorrowId,
    decl: &ScopedBorrowDecl,
    source: &Place,
) {
    if source.ty != decl.ty {
        cx.push(
            site.clone(),
            VerifyErrorKind::BadFunction(BadFunction::ScopedBorrowSourceLocalTypeMismatch {
                borrow: id,
                expected: decl.ty,
                found: source.ty,
            }),
        );
    }
    verify_place(cx, decl.owner, BlockId::from_index(0), None, source);
    verify_mutable_place(cx, decl.owner, &site, source);
    let Some(function) = cx.program.functions.get(decl.owner.index()) else {
        return;
    };
    let Some(root_local) = source.root.local() else {
        cx.push(
            site.clone(),
            VerifyErrorKind::BadPlace(BadPlace::UnsupportedRoot(source.root)),
        );
        return;
    };
    if !source.projection.is_empty()
        || !function
            .locals
            .get(root_local.index())
            .is_some_and(|local| {
                local.kind == LocalKind::PatternBinding && local.binding == Some(decl.binding)
            })
        || !function_has_collection_slot_local(function, root_local)
    {
        cx.push(
            site,
            VerifyErrorKind::BadPlace(BadPlace::UnsupportedScopedBorrowProjection(id)),
        );
    }
}

fn function_has_collection_slot_local(function: &Function, local: LocalId) -> bool {
    let mut slots = std::collections::HashSet::new();
    collect_collection_loan_slot_locals(&function.body.block, &mut slots);
    slots.contains(&local)
}

fn function_param_is_mut_borrow(function: &Function, local: LocalId, role: ParamRole) -> bool {
    function.signature.params.iter().any(|param| {
        param.local_id == local && param.mode == ParamMode::MutBorrow && param.role == role
    })
}

fn function_local_is_any_mut_borrow(function: &Function, local: LocalId) -> bool {
    function
        .signature
        .params
        .iter()
        .any(|param| param.local_id == local && param.mode == ParamMode::MutBorrow)
}

fn verify_scoped_borrow_uniqueness(cx: &mut VerifyCx<'_>) {
    let mut bindings = std::collections::HashMap::new();
    for (index, decl) in cx.program.scoped_borrows.iter().enumerate() {
        let id = ScopedBorrowId::from_index(index);
        if let Some(first) = bindings.insert((decl.owner, decl.binding), id) {
            cx.push(
                VerifySite::ScopedBorrow(id),
                VerifyErrorKind::BadFunction(BadFunction::DuplicateScopedBorrow {
                    owner: decl.owner,
                    binding: decl.binding,
                    first,
                    second: id,
                }),
            );
        }
        if let Some(first) = cx.program.scoped_borrows[..index]
            .iter()
            .enumerate()
            .find_map(|(other_index, other)| {
                (other.owner == decl.owner && other.source == decl.source)
                    .then_some(ScopedBorrowId::from_index(other_index))
            })
        {
            cx.push(
                VerifySite::ScopedBorrow(id),
                VerifyErrorKind::BadFunction(BadFunction::DuplicateScopedBorrowSource {
                    owner: decl.owner,
                    source: decl.source.clone(),
                    first,
                    second: id,
                }),
            );
        }
    }
}

fn verify_capture_cell(cx: &mut VerifyCx<'_>, id: CaptureCellId) {
    let decl = &cx.program.capture_cells[id.index()];
    let site = VerifySite::CaptureCell(id);
    if !cx.has_function(decl.owner) {
        cx.push(
            site.clone(),
            VerifyErrorKind::BadReference(BadReference::InvalidFunction(decl.owner)),
        );
    }
    let local = decl.source_local;
    if let CaptureCellLifetime::Loop { loop_id } = decl.lifetime
        && let Some(block) = cx
            .program
            .functions
            .get(decl.owner.index())
            .map(|function| function.body.block.clone())
        && !verify_loop_capture_cell_block(cx, decl.owner, &block, id, loop_id, false)
    {
        cx.push(
            site.clone(),
            VerifyErrorKind::BadFunction(BadFunction::CaptureCellLoopMissing { cell: id, loop_id }),
        );
    }
    if let Some(function) = cx.program.functions.get(decl.owner.index()) {
        match function.locals.get(local.index()) {
            Some(source) => {
                if source.ty != decl.ty {
                    cx.push(
                        site.clone(),
                        VerifyErrorKind::BadFunction(
                            BadFunction::CaptureCellSourceLocalTypeMismatch {
                                cell: id,
                                expected: decl.ty,
                                found: source.ty,
                            },
                        ),
                    );
                }
                if source.binding != Some(decl.binding) {
                    cx.push(
                        site.clone(),
                        VerifyErrorKind::BadFunction(
                            BadFunction::CaptureCellSourceLocalBindingMismatch {
                                cell: id,
                                expected: decl.binding,
                                found: source.binding,
                            },
                        ),
                    );
                }
                if source.kind != LocalKind::User {
                    cx.push(
                        site.clone(),
                        VerifyErrorKind::BadFunction(
                            BadFunction::CaptureCellSourceLocalMustBeOwnedBinding {
                                cell: id,
                                local,
                                kind: source.kind,
                            },
                        ),
                    );
                }
                if source.mutability != Mutability::Mutable {
                    cx.push(
                        site.clone(),
                        VerifyErrorKind::BadFunction(
                            BadFunction::CaptureCellSourceLocalMustBeMutable { cell: id, local },
                        ),
                    );
                }
            }
            None => cx.push(
                site.clone(),
                VerifyErrorKind::BadFunction(BadFunction::CaptureCellSourceLocalMismatch {
                    cell: id,
                    owner: decl.owner,
                    local,
                }),
            ),
        }
    }
    cx.verify_type_ref(site, decl.ty);
}

fn verify_loop_capture_cell_block(
    cx: &mut VerifyCx<'_>,
    function_id: FunctionId,
    block: &AirBlock,
    cell: CaptureCellId,
    loop_id: AirLoopId,
    in_loop: bool,
) -> bool {
    let mut found_loop = false;
    for (index, stmt) in block.stmts.iter().enumerate() {
        if stmt_uses_capture_cell(stmt, cell) && !in_loop {
            cx.push(
                VerifyCx::stmt_site(function_id, BlockId::from_index(0), index),
                VerifyErrorKind::BadFunction(BadFunction::CaptureCellOutsideLoop { cell, loop_id }),
            );
        }
        let enters_target_loop = match stmt {
            AirStmt::Loop(loop_) => loop_.id == loop_id,
            AirStmt::RangeFor(range) => range.id == loop_id,
            AirStmt::CollectionFor(for_) => for_.id == loop_id,
            AirStmt::Init { .. }
            | AirStmt::Assign { .. }
            | AirStmt::Eval(_)
            | AirStmt::GlobalEnsure { .. }
            | AirStmt::GlobalSetRoot { .. }
            | AirStmt::GlobalUpdateRoot { .. }
            | AirStmt::If(_)
            | AirStmt::CollectionLoan(_)
            | AirStmt::CollectionSlotScope(_)
            | AirStmt::PatternMatch(_)
            | AirStmt::DynMatch(_)
            | AirStmt::OptionalMatch(_)
            | AirStmt::MapEntryMatch(_) => false,
        };
        found_loop |= enters_target_loop;
        stmt.for_each_child(&mut |child| {
            if let AirChild::Block(child) = child {
                found_loop |= verify_loop_capture_cell_block(
                    cx,
                    function_id,
                    child,
                    cell,
                    loop_id,
                    in_loop || enters_target_loop,
                );
            }
        });
    }
    if tail_uses_capture_cell(&block.tail, cell) && !in_loop {
        cx.push(
            VerifySite::Function(function_id),
            VerifyErrorKind::BadFunction(BadFunction::CaptureCellOutsideLoop { cell, loop_id }),
        );
    }
    found_loop
}

fn stmt_uses_capture_cell(stmt: &AirStmt, cell: CaptureCellId) -> bool {
    let mut found = false;
    stmt.for_each_child(&mut |child| found |= child_uses_capture_cell(child, cell));
    found
}

fn tail_return_operand(tail: &AirTail) -> Option<&Operand> {
    match tail {
        AirTail::Return(Some(operand)) => Some(operand),
        AirTail::ReturnOwned(owned) => Some(&owned.value),
        _ => None,
    }
}

fn tail_uses_capture_cell(tail: &AirTail, cell: CaptureCellId) -> bool {
    tail_return_operand(tail).is_some_and(|operand| operand_uses_capture_cell(operand, cell))
}

fn rvalue_uses_capture_cell(value: &RValue, cell: CaptureCellId) -> bool {
    let mut found = false;
    value.for_each_child(ValueUse::Read, &mut |child| {
        found |= child_uses_capture_cell(child, cell);
    });
    found
}

fn child_uses_capture_cell(child: AirChild<'_>, cell: CaptureCellId) -> bool {
    match child {
        AirChild::RValue { value, .. } => rvalue_uses_capture_cell(value, cell),
        AirChild::Operand { operand, .. } => operand_uses_capture_cell(operand, cell),
        AirChild::Place { place, .. } => place_uses_capture_cell(place, cell),
        AirChild::CallArg { arg, .. } => call_arg_uses_capture_cell(arg, cell),
        AirChild::LambdaCapture(capture) => lambda_capture_arg_uses_capture_cell(capture, cell),
        AirChild::DynBorrow(borrow) => place_uses_capture_cell(borrow.place(), cell),
        AirChild::LocalRead(_) | AirChild::Block(_) => false,
    }
}

fn call_arg_uses_capture_cell(arg: &CallArg, cell: CaptureCellId) -> bool {
    match arg {
        CallArg::Value(owned) | CallArg::InitFieldProvided(owned) => {
            operand_uses_capture_cell(&owned.value, cell)
        }
        CallArg::SharedBorrow(place) | CallArg::MutBorrow(place) => {
            place_uses_capture_cell(place, cell)
        }
        CallArg::DynBorrow(borrow) => place_uses_capture_cell(borrow.place(), cell),
        CallArg::InitFieldOmitted | CallArg::SharedStringConst(_) => false,
    }
}

fn lambda_capture_arg_uses_capture_cell(arg: &LambdaCaptureArg, cell: CaptureCellId) -> bool {
    match arg {
        LambdaCaptureArg::CaptureCell { cell: found } => *found == cell,
        LambdaCaptureArg::ReadonlyLocal { value } => operand_uses_capture_cell(&value.value, cell),
        LambdaCaptureArg::ScopedLocal { place } | LambdaCaptureArg::ScopedBorrow { place } => {
            place_uses_capture_cell(place, cell)
        }
        LambdaCaptureArg::NoRuntime => false,
    }
}

fn operand_uses_capture_cell(operand: &Operand, cell: CaptureCellId) -> bool {
    match operand {
        Operand::Place(place) => place_uses_capture_cell(place, cell),
        Operand::Const(_) => false,
    }
}

fn place_uses_capture_cell(place: &Place, cell: CaptureCellId) -> bool {
    place.root == PlaceRoot::CaptureCell(cell)
}

fn verify_capture_cell_uniqueness(cx: &mut VerifyCx<'_>) {
    let mut bindings = std::collections::HashMap::new();
    let mut locals = std::collections::HashMap::new();
    for (index, decl) in cx.program.capture_cells.iter().enumerate() {
        let id = CaptureCellId::from_index(index);
        if let Some(first) = bindings.insert((decl.owner, decl.binding), id) {
            cx.push(
                VerifySite::CaptureCell(id),
                VerifyErrorKind::BadFunction(BadFunction::DuplicateCaptureCell {
                    owner: decl.owner,
                    binding: decl.binding,
                    first,
                    second: id,
                }),
            );
        }
        if let Some(first) = locals.insert((decl.owner, decl.source_local), id) {
            cx.push(
                VerifySite::CaptureCell(id),
                VerifyErrorKind::BadFunction(BadFunction::DuplicateCaptureCellSourceLocal {
                    owner: decl.owner,
                    local: decl.source_local,
                    first,
                    second: id,
                }),
            );
        }
    }
}

fn verify_global(cx: &mut VerifyCx<'_>, id: GlobalId) {
    let decl = &cx.program.globals[id.index()];
    let site = VerifySite::Global(id);
    cx.verify_module_ref(site.clone(), decl.module);
    verify_decl_listed_once(cx, site.clone(), decl.module, id, |m| &m.globals);
    cx.verify_type_ref(site.clone(), decl.ty);
    if cx.primitives.void() == Some(decl.ty) {
        cx.push(
            site.clone(),
            VerifyErrorKind::BadFunction(BadFunction::GlobalVoidType(id)),
        );
    }
    match cx.program.functions.get(decl.init.index()) {
        Some(function) if function.kind == FunctionKind::GlobalInit(id) => {
            if function.module != decl.module {
                cx.push(
                    site.clone(),
                    VerifyErrorKind::BadFunction(BadFunction::GlobalInitModuleMismatch {
                        global: id,
                        expected: decl.module,
                        found: function.module,
                    }),
                );
            }
            if !function.signature.params.is_empty()
                || function.signature.return_mode != ReturnMode::Value(decl.ty)
            {
                cx.push(
                    site.clone(),
                    VerifyErrorKind::BadFunction(BadFunction::GlobalInitSignatureMismatch {
                        global: id,
                        init: decl.init,
                    }),
                );
            }
        }
        Some(_) => cx.push(
            site,
            VerifyErrorKind::BadFunction(BadFunction::GlobalInitKindMismatch {
                global: id,
                init: decl.init,
            }),
        ),
        None => cx.push(
            site,
            VerifyErrorKind::BadReference(BadReference::InvalidFunction(decl.init)),
        ),
    }
}

fn verify_const(cx: &mut VerifyCx<'_>, id: ConstId) {
    let konst = cx.program.const_data(id);
    let site = VerifySite::Const(id);
    if !cx.has_type(konst.ty) {
        cx.push(
            site,
            VerifyErrorKind::BadReference(BadReference::InvalidType(konst.ty)),
        );
        return;
    }
    let expected = match &konst.value {
        ConstValue::Int(_) => required_const_primitive(cx, site.clone(), PrimitiveKind::Int),
        ConstValue::Flag { flag, bits } => {
            let valid = cx
                .program
                .flags
                .get(flag.index())
                .is_some_and(|decl| *bits >= 0 && *bits & !decl.known_bits == 0);
            if !valid {
                cx.push(
                    site.clone(),
                    VerifyErrorKind::BadConst(BadConst::InvalidFlagValue),
                );
            }
            match cx.program.type_data(konst.ty) {
                TypeData::Flag(found) if found == flag => Some(konst.ty),
                _ => {
                    cx.push(
                        site.clone(),
                        VerifyErrorKind::BadConst(BadConst::InvalidFlagValue),
                    );
                    None
                }
            }
        }
        ConstValue::Float(_) => required_const_primitive(cx, site.clone(), PrimitiveKind::Float),
        ConstValue::Bool(_) => required_const_primitive(cx, site.clone(), PrimitiveKind::Bool),
        ConstValue::String(_) => required_const_primitive(cx, site.clone(), PrimitiveKind::String),
        ConstValue::Char(_) => required_const_primitive(cx, site.clone(), PrimitiveKind::Char),
        ConstValue::Nil => {
            if typing::optional_inner(cx.program, konst.ty).is_none() {
                cx.push(
                    site.clone(),
                    VerifyErrorKind::BadConst(BadConst::NilMustBeOptional(konst.ty)),
                );
            }
            None
        }
    };
    if let Some(expected) = expected
        && konst.ty != expected
    {
        cx.push(
            site,
            VerifyErrorKind::BadConst(BadConst::TypeMismatch {
                expected,
                found: konst.ty,
            }),
        );
    }
}

fn required_const_primitive(
    cx: &mut VerifyCx<'_>,
    site: VerifySite,
    kind: PrimitiveKind,
) -> Option<TypeId> {
    require_primitive(cx, site, kind, |kind| {
        VerifyErrorKind::BadConst(BadConst::MissingPrimitive(kind))
    })
}

fn required_rvalue_primitive(
    cx: &mut VerifyCx<'_>,
    site: VerifySite,
    kind: PrimitiveKind,
) -> Option<TypeId> {
    require_primitive(cx, site, kind, |kind| {
        VerifyErrorKind::BadRValue(BadRValue::MissingPrimitive(kind))
    })
}

fn require_primitive(
    cx: &mut VerifyCx<'_>,
    site: VerifySite,
    kind: PrimitiveKind,
    missing: impl FnOnce(PrimitiveKind) -> VerifyErrorKind,
) -> Option<TypeId> {
    let ty = cx.primitives.get(kind);
    if ty.is_none() {
        cx.push(site, missing(kind));
    }
    ty
}

fn verify_decl_listed_once<T>(
    cx: &mut VerifyCx<'_>,
    site: VerifySite,
    module: ModuleId,
    item: T,
    list: impl Fn(&super::Module) -> &[T],
) where
    T: Copy + Eq + IntoModuleReference,
{
    if !cx.has_module(module) {
        return;
    }
    let count = list(cx.program.module(module))
        .iter()
        .filter(|listed| **listed == item)
        .count();
    if count == 0 {
        cx.push(
            site,
            VerifyErrorKind::BadModule(BadModule::MissingItem(item.module_item())),
        );
    } else if count > 1 {
        cx.push(
            site,
            VerifyErrorKind::BadModule(BadModule::DuplicateItem(item.module_item())),
        );
    }
}

fn verify_aggregate(cx: &mut VerifyCx<'_>, id: AggregateId) {
    let agg = cx.program.aggregate(id);
    let site = VerifySite::Aggregate(id);
    cx.verify_module_ref(site.clone(), agg.module);
    verify_decl_listed_once(cx, site.clone(), agg.module, id, |m| &m.aggregates);
    for field in &agg.fields {
        cx.verify_type_ref(site.clone(), field.ty);
    }
    if let Some(function_id) = agg.stringify_override {
        if function_id.index() >= cx.program.functions.len() {
            cx.push(
                site,
                VerifyErrorKind::BadReference(BadReference::InvalidFunction(function_id)),
            );
            return;
        }
        let function = cx.program.function(function_id);
        if matches!(function.kind, FunctionKind::Lambda(_)) {
            cx.push(
                site.clone(),
                VerifyErrorKind::BadFunction(BadFunction::StringifyOverrideMustBeNamed(
                    function_id,
                )),
            );
        }
        if function.module != agg.module {
            cx.push(
                site.clone(),
                VerifyErrorKind::BadFunction(BadFunction::StringifyOverrideModuleMismatch {
                    expected: agg.module,
                    found: function.module,
                }),
            );
        }
        match function.signature.params.first() {
            Some(param) if param.role == ParamRole::Receiver => {
                let matches_owner = cx.has_type(param.ty)
                    && match (agg.kind, cx.program.type_data(param.ty)) {
                        (AggregateKind::Struct, TypeData::Aggregate(owner))
                        | (AggregateKind::DataRef, TypeData::DataRef(owner)) => *owner == id,
                        _ => false,
                    };
                if !matches_owner {
                    cx.push(
                        site.clone(),
                        VerifyErrorKind::BadFunction(
                            BadFunction::StringifyOverrideReceiverTypeMismatch {
                                expected: id,
                                found: param.ty,
                            },
                        ),
                    );
                }
            }
            _ => cx.push(
                site.clone(),
                VerifyErrorKind::BadFunction(BadFunction::StringifyOverrideMissingReceiver),
            ),
        }
        if !cx.has_type(function.signature.return_type()) {
            cx.push(
                site,
                VerifyErrorKind::BadReference(BadReference::InvalidType(
                    function.signature.return_type(),
                )),
            );
            return;
        }
        if !matches!(function.signature.return_mode, ReturnMode::Value(_))
            || !matches!(
                cx.program.type_data(function.signature.return_type()),
                TypeData::String
            )
        {
            cx.push(
                site,
                VerifyErrorKind::BadFunction(BadFunction::StringifyOverrideReturnMustBeString(
                    function.signature.return_type(),
                )),
            );
        }
    }
}

fn verify_enum(cx: &mut VerifyCx<'_>, id: EnumId) {
    let enm = cx.program.enum_decl(id);
    let site = VerifySite::Enum(id);
    cx.verify_module_ref(site.clone(), enm.module);
    verify_decl_listed_once(cx, site.clone(), enm.module, id, |m| &m.enums);
    match enm.repr {
        EnumRepr::Adt if enm.raw_type.is_some() => cx.push(
            site.clone(),
            VerifyErrorKind::BadEnum(BadEnum::AdtHasRawType),
        ),
        EnumRepr::Adt => {}
        EnumRepr::RawInt | EnumRepr::RawString => {
            let Some(raw_type) = enm.raw_type else {
                cx.push(
                    site.clone(),
                    VerifyErrorKind::BadEnum(BadEnum::RawMissingRawType),
                );
                return;
            };
            cx.verify_type_ref(site.clone(), raw_type);
            let expected = match enm.repr {
                EnumRepr::RawInt => TypeData::Int,
                EnumRepr::RawString => TypeData::String,
                EnumRepr::Adt => unreachable!(),
            };
            if cx.has_type(raw_type) && cx.program.type_data(raw_type) != &expected {
                cx.push(
                    site.clone(),
                    VerifyErrorKind::BadEnum(BadEnum::RawTypeMismatch(raw_type)),
                );
            }
        }
    }
    let mut raw_values = std::collections::HashSet::new();
    for (index, variant) in enm.variants.iter().enumerate() {
        let variant_id = VariantId::from_index(index);
        match (&enm.repr, &variant.raw_value) {
            (EnumRepr::Adt, None) => {}
            (EnumRepr::Adt, Some(_)) => cx.push(
                site.clone(),
                VerifyErrorKind::BadEnum(BadEnum::AdtVariantHasRawValue(variant_id)),
            ),
            (EnumRepr::RawInt, Some(RawEnumValue::Int(_)))
            | (EnumRepr::RawString, Some(RawEnumValue::String(_))) => {
                if !raw_values.insert(variant.raw_value.as_ref()) {
                    cx.push(
                        site.clone(),
                        VerifyErrorKind::BadEnum(BadEnum::DuplicateRawValue(variant_id)),
                    );
                }
                if !matches!(variant.shape, VariantShape::Unit) {
                    cx.push(
                        site.clone(),
                        VerifyErrorKind::BadEnum(BadEnum::RawVariantPayload(variant_id)),
                    );
                }
            }
            (EnumRepr::RawInt, Some(_)) => cx.push(
                site.clone(),
                VerifyErrorKind::BadEnum(BadEnum::RawIntVariantValueType(variant_id)),
            ),
            (EnumRepr::RawString, Some(_)) => cx.push(
                site.clone(),
                VerifyErrorKind::BadEnum(BadEnum::RawStringVariantValueType(variant_id)),
            ),
            (EnumRepr::RawInt | EnumRepr::RawString, None) => cx.push(
                site.clone(),
                VerifyErrorKind::BadEnum(BadEnum::RawVariantMissingValue(variant_id)),
            ),
        }

        match &variant.shape {
            VariantShape::Unit => {}
            VariantShape::Tuple(types) => {
                for ty in types {
                    cx.verify_type_ref(site.clone(), *ty);
                }
            }
            VariantShape::Struct(fields) => {
                for field in fields {
                    cx.verify_type_ref(site.clone(), field.ty);
                }
            }
        }
    }
}

fn extern_owner_type(cx: &VerifyCx<'_>, owner: ExternTypeId) -> Option<TypeId> {
    cx.program
        .type_arena
        .iter()
        .enumerate()
        .find_map(|(index, ty)| {
            matches!(ty, TypeData::Extern(id) if *id == owner).then_some(TypeId::from_index(index))
        })
}

fn verify_receiver(cx: &mut VerifyCx<'_>, site: VerifySite, owner_ty: TypeId, receiver_ty: TypeId) {
    cx.verify_type_ref(site.clone(), receiver_ty);
    if owner_ty != receiver_ty {
        cx.push(
            site,
            VerifyErrorKind::BadExtern(BadExtern::ReceiverTypeMismatch {
                expected: owner_ty,
                found: receiver_ty,
            }),
        );
    }
}

fn verify_extern_variants(cx: &mut VerifyCx<'_>, site: &VerifySite, ty: &super::ExternTypeDecl) {
    if ty.variants.len() != ty.variant_abis.len() {
        cx.push(
            site.clone(),
            VerifyErrorKind::BadExtern(BadExtern::VariantAbiCountMismatch {
                expected: ty.variants.len(),
                found: ty.variant_abis.len(),
            }),
        );
    }
    for (variant, abi) in ty.variants.iter().zip(&ty.variant_abis) {
        let field_count = match &variant.shape {
            VariantShape::Unit => 0,
            VariantShape::Tuple(types) => {
                for ty in types {
                    cx.verify_type_ref(site.clone(), *ty);
                }
                types.len()
            }
            VariantShape::Struct(fields) => {
                for field in fields {
                    cx.verify_type_ref(site.clone(), field.ty);
                }
                fields.len()
            }
        };
        if field_count != abi.fields.len() {
            cx.push(
                site.clone(),
                VerifyErrorKind::BadExtern(BadExtern::VariantFieldAbiCountMismatch {
                    expected: field_count,
                    found: abi.fields.len(),
                }),
            );
        }
        for field_abi in &abi.fields {
            verify_extern_abi(cx, site, field_abi, AbiPosition::Field);
        }
    }
}

fn verify_flag(cx: &mut VerifyCx<'_>, id: FlagId) {
    let flag = cx.program.flag_decl(id);
    let site = VerifySite::Flag(id);
    cx.verify_module_ref(site.clone(), flag.module);
    verify_decl_listed_once(cx, site.clone(), flag.module, id, |m| &m.flags);
    if flag.known_bits < 0 {
        cx.push(
            site.clone(),
            VerifyErrorKind::BadFlag(BadFlag::KnownBitsNegative),
        );
    }

    let mut seen = std::collections::HashSet::new();
    let mut known_bits = 0;
    for (index, member) in flag.members.iter().enumerate() {
        let expected_id = FlagMemberId::from_index(index);
        if member.id != expected_id {
            cx.push(
                site.clone(),
                VerifyErrorKind::BadFlag(BadFlag::MemberIdMismatch(member.id)),
            );
        }
        if member.value < 0 {
            cx.push(
                site.clone(),
                VerifyErrorKind::BadFlag(BadFlag::NegativeValue(member.id)),
            );
            continue;
        }
        let atomic = member.value > 0 && member.value & (member.value - 1) == 0;
        if member.atomic != atomic {
            cx.push(
                site.clone(),
                VerifyErrorKind::BadFlag(BadFlag::AtomicMismatch(member.id)),
            );
        }
        if !atomic && member.value & !known_bits != 0 {
            cx.push(
                site.clone(),
                VerifyErrorKind::BadFlag(BadFlag::UnknownCompositeBits(member.id)),
            );
        }
        if !seen.insert(member.value) {
            cx.push(
                site.clone(),
                VerifyErrorKind::BadFlag(BadFlag::DuplicateValue(member.id)),
            );
        }
        if atomic {
            known_bits |= member.value;
        }
    }
    if flag.known_bits != known_bits {
        cx.push(site, VerifyErrorKind::BadFlag(BadFlag::KnownBitsMismatch));
    }
}

fn verify_extern_type(cx: &mut VerifyCx<'_>, id: ExternTypeId) {
    let ty = cx.program.extern_type(id);
    let site = VerifySite::ExternType(id);
    cx.verify_module_ref(site.clone(), ty.module);
    verify_decl_listed_once(cx, site.clone(), ty.module, id, |m| &m.extern_types);
    let owner_ty = extern_owner_type(cx, id);
    let init_decl = cx
        .program
        .externs
        .iter()
        .find(|ext| matches!(ext.member, ExternMember::Init { owner } if owner == id));
    let mut init_arg_fields = std::collections::HashSet::new();
    let mut init_arg_params = std::collections::HashSet::new();
    for init_arg in &ty.init_args {
        let field = ty.fields.get(init_arg.field.index());
        let param_invalid = init_decl.is_some_and(|decl| {
            decl.params.get(init_arg.param).is_none_or(|param| {
                param.mode != ParamMode::Value || field.is_some_and(|field| param.ty != field.ty)
            })
        });
        let invalid = !ty.has_init
            || field.is_none()
            || !init_arg_fields.insert(init_arg.field)
            || !init_arg_params.insert(init_arg.param)
            || param_invalid;
        if invalid {
            cx.push(
                site.clone(),
                VerifyErrorKind::BadExtern(BadExtern::InvalidInitField(init_arg.field)),
            );
        }
    }
    for field in &ty.fields {
        cx.verify_type_ref(site.clone(), field.ty);
        verify_extern_abi(cx, &site, &field.abi, AbiPosition::Field);
        if let Some(owner_ty) = owner_ty {
            verify_receiver(cx, site.clone(), owner_ty, field.get_receiver.ty);
            verify_receiver(cx, site.clone(), owner_ty, field.set_receiver.ty);
        } else {
            cx.verify_type_ref(site.clone(), field.get_receiver.ty);
            cx.verify_type_ref(site.clone(), field.set_receiver.ty);
        }
        if field.get_receiver.mode != ParamMode::SharedBorrow {
            cx.push(
                site.clone(),
                VerifyErrorKind::BadExtern(BadExtern::ReceiverModeMismatch),
            );
        }
        if field.set_receiver.mode != ParamMode::MutBorrow {
            cx.push(
                site.clone(),
                VerifyErrorKind::BadExtern(BadExtern::ReceiverModeMismatch),
            );
        }
    }
    verify_extern_variants(cx, &site, ty);
    for method in &ty.methods {
        if let Some(owner_ty) = owner_ty {
            verify_receiver(cx, site.clone(), owner_ty, method.receiver.ty);
        } else {
            cx.verify_type_ref(site.clone(), method.receiver.ty);
        }
        for (index, param) in method.params.iter().enumerate() {
            verify_extern_param(cx, site.clone(), index, param);
        }
        verify_extern_abi_signature(
            cx,
            &site,
            &method.abi,
            std::iter::once(method.receiver.mode)
                .chain(method.params.iter().map(|param| param.mode)),
        );
        cx.verify_type_ref(site.clone(), method.return_type);
        verify_extern_abi(cx, &site, &method.abi.ret, AbiPosition::Return);
    }
    for static_ in &ty.statics {
        for (index, param) in static_.params.iter().enumerate() {
            verify_extern_param(cx, site.clone(), index, param);
        }
        verify_extern_abi_signature(
            cx,
            &site,
            &static_.abi,
            static_.params.iter().map(|param| param.mode),
        );
        cx.verify_type_ref(site.clone(), static_.return_type);
        verify_extern_abi(cx, &site, &static_.abi.ret, AbiPosition::Return);
    }
    for op in &ty.operators {
        if let Some(owner_ty) = owner_ty {
            verify_receiver(cx, site.clone(), owner_ty, op.receiver.ty);
        } else {
            cx.verify_type_ref(site.clone(), op.receiver.ty);
        }
        if op.receiver.mode != ParamMode::SharedBorrow {
            cx.push(
                site.clone(),
                VerifyErrorKind::BadExtern(BadExtern::ReceiverModeMismatch),
            );
        }
        match (&op.kind, &op.operand) {
            (super::ExternOp::Unary(_), Some(_)) | (super::ExternOp::Binary { .. }, None) => {
                cx.push(
                    site.clone(),
                    VerifyErrorKind::BadExtern(BadExtern::OperatorOperandMismatch),
                );
            }
            _ => {}
        }
        if let Some(operand) = &op.operand {
            verify_extern_param(cx, site.clone(), 0, operand);
        }
        verify_extern_abi_signature(
            cx,
            &site,
            &op.abi,
            std::iter::once(op.receiver.mode).chain(op.operand.iter().map(|param| param.mode)),
        );
        cx.verify_type_ref(site.clone(), op.return_type);
        verify_extern_abi(cx, &site, &op.abi.ret, AbiPosition::Return);
    }
}

fn verify_extern_param(
    cx: &mut VerifyCx<'_>,
    site: VerifySite,
    index: usize,
    param: &super::ExternParamDecl,
) {
    cx.verify_type_ref(site.clone(), param.ty);
    if param.escape != ParamEscape::Escaping {
        return;
    }
    if param.mode != ParamMode::Value {
        cx.push(
            site.clone(),
            VerifyErrorKind::BadExtern(BadExtern::EscapingParamMustBeValue(index)),
        );
    }
    if !matches!(cx.type_data(param.ty), Some(TypeData::Function(_))) {
        cx.push(
            site,
            VerifyErrorKind::BadExtern(BadExtern::EscapingParamMustBeFunction(index)),
        );
    }
}

fn verify_extern(cx: &mut VerifyCx<'_>, id: ExternId) {
    let ext = cx.program.extern_decl(id);
    let site = VerifySite::Extern(id);
    cx.verify_module_ref(site.clone(), ext.module);
    verify_decl_listed_once(cx, site.clone(), ext.module, id, |m| &m.externs);
    verify_extern_binding(cx, site.clone(), ext);
    verify_extern_member(cx, site.clone(), &ext.member, &ext.params);
    for (index, param) in ext.params.iter().enumerate() {
        verify_extern_param(cx, site.clone(), index, param);
    }
    verify_extern_abi_signature(
        cx,
        &site,
        &ext.abi,
        ext.call_params().map(|param| param.mode),
    );
    cx.verify_type_ref(site.clone(), ext.return_type);
    verify_extern_abi(cx, &site, &ext.abi.ret, AbiPosition::Return);
}

fn verify_extern_abi_signature(
    cx: &mut VerifyCx<'_>,
    site: &VerifySite,
    abi: &super::ExternAbi,
    modes: impl IntoIterator<Item = ParamMode>,
) {
    let modes = modes.into_iter().collect::<Vec<_>>();
    if abi.params.len() != modes.len() {
        cx.push(
            site.clone(),
            VerifyErrorKind::BadExtern(BadExtern::AbiParamCountMismatch {
                expected: modes.len(),
                found: abi.params.len(),
            }),
        );
    }
    for (param_abi, mode) in abi.params.iter().zip(modes) {
        verify_extern_abi(cx, site, param_abi, abi_position_for_param(mode));
    }
}

fn abi_position_for_param(mode: ParamMode) -> AbiPosition {
    match mode {
        ParamMode::Value => AbiPosition::ParamValue,
        ParamMode::SharedBorrow => AbiPosition::ParamBorrow,
        ParamMode::MutBorrow => AbiPosition::ParamMutBorrow,
    }
}

fn verify_extern_abi(
    cx: &mut VerifyCx<'_>,
    site: &VerifySite,
    abi: &ExternTypeExpr,
    position: AbiPosition,
) {
    if let Err(violations) = abi.classify_abi(position) {
        for violation in violations {
            cx.push(
                site.clone(),
                VerifyErrorKind::BadExtern(BadExtern::InvalidAbi {
                    position: violation.0,
                    reason: violation.1,
                }),
            );
        }
    }
}

fn verify_extern_binding(cx: &mut VerifyCx<'_>, site: VerifySite, ext: &super::ExternDecl) {
    let Some(binding) = &ext.binding else {
        return;
    };
    let valid = match (&ext.member, &binding.key.target, binding.key.operation) {
        (
            ExternMember::FreeFunction,
            ExternBindingTarget::Function(function),
            ExternBindingOp::Call,
        ) => {
            function.name == ext.name.to_string()
                && function.module == extern_module_path(cx, ext.module)
        }
        (
            ExternMember::FieldGetter { owner, .. },
            ExternBindingTarget::Member(member),
            ExternBindingOp::Get,
        )
        | (
            ExternMember::FieldSetter { owner, .. },
            ExternBindingTarget::Member(member),
            ExternBindingOp::Set,
        ) => {
            member.owner == extern_owner_key(cx, *owner)
                && member.selector == ExternMemberSelector::Field(ext.name.to_string())
        }
        (
            ExternMember::Method { owner, .. },
            ExternBindingTarget::Member(member),
            ExternBindingOp::Call,
        ) => {
            member.owner == extern_owner_key(cx, *owner)
                && member.selector == ExternMemberSelector::Method(ext.name.to_string())
        }
        (
            ExternMember::StaticMethod { owner },
            ExternBindingTarget::Member(member),
            ExternBindingOp::Call,
        ) => {
            member.owner == extern_owner_key(cx, *owner)
                && member.selector == ExternMemberSelector::Static(ext.name.to_string())
        }
        (
            ExternMember::Init { owner },
            ExternBindingTarget::Member(member),
            ExternBindingOp::Call,
        ) => {
            member.owner == extern_owner_key(cx, *owner)
                && member.selector == ExternMemberSelector::Init
        }
        (
            ExternMember::UnaryOperator { owner, op, .. },
            ExternBindingTarget::Member(member),
            ExternBindingOp::Call,
        ) => extern_unary_op(*op).is_some_and(|op| {
            member.owner == extern_owner_key(cx, *owner)
                && member.selector == ExternMemberSelector::Operator(ExternOperator::Unary(op))
        }),
        (
            ExternMember::BinaryOperator {
                owner,
                op,
                self_on_right,
                ..
            },
            ExternBindingTarget::Member(member),
            ExternBindingOp::Call,
        ) => extern_binary_op(*op).is_some_and(|op| {
            member.owner == extern_owner_key(cx, *owner)
                && member.selector
                    == ExternMemberSelector::Operator(ExternOperator::Binary {
                        op,
                        self_on_right: *self_on_right,
                    })
        }),
        _ => false,
    };
    if !valid {
        cx.push(site, VerifyErrorKind::BadExtern(BadExtern::BindingMismatch));
    }
}

fn extern_owner_key(cx: &VerifyCx<'_>, id: ExternTypeId) -> ExternTypeKey {
    if !cx.has_extern_type(id) {
        return ExternTypeKey {
            module: ExternModulePath { segments: vec![] },
            name: String::new(),
        };
    }
    let ty = cx.program.extern_type(id);
    ExternTypeKey {
        module: extern_module_path(cx, ty.module),
        name: ty.name.to_string(),
    }
}

fn extern_module_path(cx: &VerifyCx<'_>, id: ModuleId) -> ExternModulePath {
    let segments = cx
        .program
        .modules
        .get(id.index())
        .map(|module| module.path.iter().map(ToString::to_string).collect())
        .unwrap_or_default();
    ExternModulePath { segments }
}

fn extern_unary_op(op: UnaryOp) -> Option<ExternUnaryOp> {
    match op {
        UnaryOp::Neg => Some(ExternUnaryOp::Neg),
        UnaryOp::Not | UnaryOp::BitNot => None,
    }
}

fn extern_binary_op(op: BinaryOp) -> Option<ExternBinaryOp> {
    match op {
        BinaryOp::Add => Some(ExternBinaryOp::Add),
        BinaryOp::Sub => Some(ExternBinaryOp::Sub),
        BinaryOp::Mul => Some(ExternBinaryOp::Mul),
        BinaryOp::Div => Some(ExternBinaryOp::Div),
        BinaryOp::Rem => Some(ExternBinaryOp::Rem),
        BinaryOp::Eq => Some(ExternBinaryOp::Eq),
        BinaryOp::NotEq => Some(ExternBinaryOp::NotEq),
        BinaryOp::LessThan => Some(ExternBinaryOp::LessThan),
        BinaryOp::GreaterThan => Some(ExternBinaryOp::GreaterThan),
        BinaryOp::LessThanEq => Some(ExternBinaryOp::LessThanEq),
        BinaryOp::GreaterThanEq => Some(ExternBinaryOp::GreaterThanEq),
        BinaryOp::And
        | BinaryOp::Or
        | BinaryOp::Xor
        | BinaryOp::BitAnd
        | BinaryOp::BitOr
        | BinaryOp::Shl
        | BinaryOp::Shr
        | BinaryOp::Coalesce => None,
    }
}

fn verify_receiver_mode(
    cx: &mut VerifyCx<'_>,
    site: VerifySite,
    found: ParamMode,
    expected: ParamMode,
) {
    if found != expected {
        cx.push(
            site,
            VerifyErrorKind::BadExtern(BadExtern::ReceiverModeMismatch),
        );
    }
}

fn verify_member_param_count(
    cx: &mut VerifyCx<'_>,
    site: VerifySite,
    found: usize,
    expected: usize,
) {
    if found != expected {
        cx.push(
            site,
            VerifyErrorKind::BadExtern(BadExtern::MemberParamCountMismatch { expected, found }),
        );
    }
}

fn verify_extern_member(
    cx: &mut VerifyCx<'_>,
    site: VerifySite,
    member: &ExternMember,
    params: &[super::ExternParamDecl],
) {
    let expected_params = match member {
        ExternMember::FieldGetter { .. } | ExternMember::UnaryOperator { .. } => Some(0),
        ExternMember::FieldSetter { .. } | ExternMember::BinaryOperator { .. } => Some(1),
        ExternMember::FreeFunction
        | ExternMember::Init { .. }
        | ExternMember::Method { .. }
        | ExternMember::StaticMethod { .. } => None,
    };
    if let Some(expected) = expected_params {
        verify_member_param_count(cx, site.clone(), params.len(), expected);
    }
    let receiver_mode = match member {
        ExternMember::FieldGetter { receiver, .. }
        | ExternMember::UnaryOperator { receiver, .. }
        | ExternMember::BinaryOperator { receiver, .. } => {
            Some((receiver.mode, ParamMode::SharedBorrow))
        }
        ExternMember::FieldSetter { receiver, .. } => Some((receiver.mode, ParamMode::MutBorrow)),
        ExternMember::FreeFunction
        | ExternMember::Init { .. }
        | ExternMember::Method { .. }
        | ExternMember::StaticMethod { .. } => None,
    };
    if let Some((found, expected)) = receiver_mode {
        verify_receiver_mode(cx, site.clone(), found, expected);
    }
    match member {
        ExternMember::FreeFunction => {}
        ExternMember::FieldGetter {
            owner, receiver, ..
        }
        | ExternMember::FieldSetter { owner, receiver }
        | ExternMember::Method { owner, receiver }
        | ExternMember::UnaryOperator {
            owner, receiver, ..
        }
        | ExternMember::BinaryOperator {
            owner, receiver, ..
        } => {
            if !cx.has_extern_type(*owner) {
                cx.push(
                    site.clone(),
                    VerifyErrorKind::BadReference(BadReference::InvalidExternType(*owner)),
                );
                cx.verify_type_ref(site, receiver.ty);
                return;
            }
            if let Some(owner_ty) = extern_owner_type(cx, *owner) {
                verify_receiver(cx, site, owner_ty, receiver.ty);
            } else {
                cx.verify_type_ref(site, receiver.ty);
            }
        }
        ExternMember::StaticMethod { owner } | ExternMember::Init { owner } => {
            if !cx.has_extern_type(*owner) {
                cx.push(
                    site,
                    VerifyErrorKind::BadReference(BadReference::InvalidExternType(*owner)),
                );
            }
        }
    }
}

fn verify_function(cx: &mut VerifyCx<'_>, id: FunctionId) {
    let func = cx.program.function(id);
    let site = VerifySite::Function(id);
    cx.verify_module_ref(site.clone(), func.module);
    verify_decl_listed_once(cx, site.clone(), func.module, id, |m| &m.functions);
    match func.kind {
        FunctionKind::Lambda(lambda) => match cx.program.lambdas.get(lambda.index()) {
            Some(decl) if decl.body == id => {}
            Some(decl) => cx.push(
                site.clone(),
                VerifyErrorKind::BadFunction(BadFunction::LambdaBodyKindMismatch {
                    lambda,
                    body: decl.body,
                }),
            ),
            None => cx.push(
                site.clone(),
                VerifyErrorKind::BadReference(BadReference::InvalidLambda(lambda)),
            ),
        },
        FunctionKind::GlobalInit(global) => match cx.program.globals.get(global.index()) {
            Some(decl) if decl.init == id => {}
            Some(decl) => cx.push(
                site.clone(),
                VerifyErrorKind::BadFunction(BadFunction::GlobalInitFunctionMismatch {
                    global,
                    init: decl.init,
                }),
            ),
            None => cx.push(
                site.clone(),
                VerifyErrorKind::BadReference(BadReference::InvalidGlobal(global)),
            ),
        },
        FunctionKind::Normal
        | FunctionKind::Method
        | FunctionKind::ExtendMethod
        | FunctionKind::Helper => {}
    }

    let mut seen_locals = std::collections::HashSet::new();
    for (i, param) in func.signature.params.iter().enumerate() {
        cx.verify_type_ref(site.clone(), param.ty);
        let is_out_of_range = param.local_id.index() >= func.locals.len();
        if is_out_of_range {
            cx.push(
                site.clone(),
                VerifyErrorKind::BadFunction(BadFunction::ParamLocalOutOfRange {
                    param: i,
                    total_locals: func.locals.len(),
                }),
            );
        } else {
            let local = &func.locals[param.local_id.index()];
            if !seen_locals.insert(param.local_id) {
                let first_idx = func.signature.params[..i]
                    .iter()
                    .position(|p| p.local_id == param.local_id)
                    .unwrap();
                cx.push(
                    site.clone(),
                    VerifyErrorKind::BadFunction(BadFunction::DuplicateParamLocal {
                        first: func.signature.params[first_idx].local_id,
                        second: param.local_id,
                    }),
                );
            }
            if local.kind != LocalKind::Arg {
                cx.push(
                    site.clone(),
                    VerifyErrorKind::BadFunction(BadFunction::ParamLocalMustBeArg {
                        param: i,
                        local: param.local_id,
                    }),
                );
            }
            if local.ty != param.ty {
                cx.push(
                    site.clone(),
                    VerifyErrorKind::BadFunction(BadFunction::ParamLocalTypeMismatch {
                        param: i,
                        expected: param.ty,
                        found: local.ty,
                    }),
                );
            }
            if param.mode == ParamMode::MutBorrow && local.mutability != Mutability::Mutable {
                cx.push(
                    site.clone(),
                    VerifyErrorKind::BadFunction(BadFunction::MutBorrowParamLocalMustBeMutable {
                        param: i,
                        local: param.local_id,
                    }),
                );
            }
            if param.mode == ParamMode::MutBorrow
                && matches!(cx.type_data(param.ty), Some(TypeData::Dyn(_)))
            {
                let found = cx
                    .program
                    .dyn_borrow_params
                    .iter()
                    .filter(|decl| decl.owner == id && decl.source == param.local_id)
                    .count();
                if found != 1 {
                    cx.push(
                        site.clone(),
                        VerifyErrorKind::BadFunction(BadFunction::DynBorrowParamCount {
                            local: param.local_id,
                            found,
                        }),
                    );
                }
            }
        }
    }

    let mut bindings = std::collections::HashMap::new();
    for (index, local) in func.locals.iter().enumerate() {
        cx.verify_type_ref(site.clone(), local.ty);
        if let Some(binding) = local.binding {
            let id = LocalId::from_index(index);
            if !matches!(
                local.kind,
                LocalKind::Arg | LocalKind::User | LocalKind::PatternBinding
            ) {
                cx.push(
                    site.clone(),
                    VerifyErrorKind::BadFunction(BadFunction::LocalBindingInvalidKind {
                        local: id,
                        kind: local.kind,
                    }),
                );
            }
            if let Some(first) = bindings.insert(binding, id) {
                cx.push(
                    site.clone(),
                    VerifyErrorKind::BadFunction(BadFunction::DuplicateLocalBinding {
                        binding,
                        first,
                        second: id,
                    }),
                );
            }
        }
    }
    if let Some(specialization) = &func.specialization {
        for ty in &specialization.type_args {
            cx.verify_type_ref(site.clone(), *ty);
        }
    }

    cx.verify_type_ref(site, func.signature.return_type());

    let mut state = LocalInit::new(cx.program, func);
    let falls_through = verify_air_block(cx, id, &func.body.block, &mut state, &mut Vec::new());
    verify_collection_loan_contract(cx, id, &func.body.block);
    verify_dyn_borrow_root_contract(cx, id, &func.body.block);
    if falls_through.is_some()
        && !matches!(
            cx.type_data(func.signature.return_type()),
            Some(TypeData::Void)
        )
    {
        cx.push(
            VerifySite::Function(id),
            VerifyErrorKind::BadFunction(BadFunction::NonVoidFunctionMustReturnValue(
                func.signature.return_type(),
            )),
        );
    }
}

fn verify_air_block(
    cx: &mut VerifyCx<'_>,
    function_id: FunctionId,
    block: &AirBlock,
    state: &mut LocalInit,
    loops: &mut Vec<LoopCtx>,
) -> Option<LocalInit> {
    for (index, stmt) in block.stmts.iter().enumerate() {
        *state = verify_air_stmt(cx, function_id, index, stmt, state, loops)?;
    }
    verify_air_tail(cx, function_id, &block.tail, state, loops)
}

struct LoopCtx {
    id: AirLoopId,
    breaks: Vec<LocalInit>,
    backedges: Vec<LocalInit>,
}

fn clear_rvalue_write_state(
    program: &Program,
    function_id: FunctionId,
    value: &RValue,
    state: &mut LocalInit,
) {
    match value {
        RValue::ListPush { list, value } => {
            state.push_place_value(
                list,
                operand_function_state(program, function_id, &value.value, state),
            );
        }
        RValue::ListPop { list, .. }
        | RValue::MapInsert { map: list, .. }
        | RValue::MapRemove { map: list, .. } => state.clear_place_value(list),
        _ => {}
    }
}

fn clear_transferred_temporaries(value: &RValue, state: &mut LocalInit) {
    value.for_each_owned_value(&mut |owned| {
        if let ValueSource::TransferTemp { local } = owned.source {
            state.clear(local);
        }
    });
}

fn verify_air_stmt(
    cx: &mut VerifyCx<'_>,
    function_id: FunctionId,
    index: usize,
    stmt: &AirStmt,
    state: &LocalInit,
    loops: &mut Vec<LoopCtx>,
) -> Option<LocalInit> {
    let block_id = BlockId::from_index(0);
    match stmt {
        AirStmt::Init { local, value } => {
            verify_materialize_role(cx, function_id, block_id, index, value, true);
            verify_air_rvalue_reads(cx, function_id, index, value, state);
            verify_promoted_local_not_initialized(cx, function_id, block_id, index, *local);
            verify_init_stmt(cx, function_id, block_id, index, *local, value);
            verify_function_value_escape_proof(cx, function_id, index, value, state);
            let function = cx.program.function(function_id);
            if let Some(local_decl) = function.locals.get(local.index()) {
                if local_decl.mutability == Mutability::Immutable && state.is_possible(*local) {
                    cx.push(
                        VerifyCx::stmt_site(function_id, block_id, index),
                        VerifyErrorKind::BadStatement(BadStatement::InitImmutableLocalTwice(
                            *local,
                        )),
                    );
                }
                let mut next = state.clone();
                clear_transferred_temporaries(value, &mut next);
                next.init(*local);
                next.set_local_value(
                    *local,
                    rvalue_function_state(cx.program, function_id, &cx.primitives, value, state),
                );
                return Some(next);
            }
            Some(state.clone())
        }
        AirStmt::Assign { dst, value } => {
            verify_materialize_role(cx, function_id, block_id, index, value, true);
            verify_air_rvalue_reads(cx, function_id, index, value, state);
            verify_air_place_write(cx, function_id, index, dst, state);
            verify_assign_stmt(cx, function_id, block_id, index, dst, value);
            verify_function_value_escape_proof(cx, function_id, index, value, state);
            let mut next = state.clone();
            clear_transferred_temporaries(value, &mut next);
            if let PlaceRoot::CaptureCell(cell) = dst.root
                && dst.projection.is_empty()
            {
                next.init_cell(cell);
            }
            next.set_place_value(
                dst,
                rvalue_function_state(cx.program, function_id, &cx.primitives, value, state),
            );
            Some(next)
        }
        AirStmt::Eval(value) => {
            verify_materialize_role(cx, function_id, block_id, index, value, false);
            verify_air_rvalue_reads(cx, function_id, index, value, state);
            verify_rvalue(cx, function_id, block_id, Some(index), value);
            verify_function_value_escape_proof(cx, function_id, index, value, state);
            let mut next = state.clone();
            clear_rvalue_write_state(cx.program, function_id, value, &mut next);
            clear_transferred_temporaries(value, &mut next);
            Some(next)
        }
        AirStmt::GlobalEnsure { global } => {
            if !cx.has_global(*global) {
                cx.push(
                    VerifyCx::stmt_site(function_id, block_id, index),
                    VerifyErrorKind::BadReference(BadReference::InvalidGlobal(*global)),
                );
            }
            let mut next = state.clone();
            if !next.global_is_definite(*global) {
                let value = global_initializer_function_state(cx.program, *global);
                next.ensure_global(*global, value);
            }
            Some(next)
        }
        AirStmt::GlobalSetRoot {
            global,
            value,
            init,
        } => {
            verify_materialize_role(cx, function_id, block_id, index, value, true);
            verify_air_rvalue_reads(cx, function_id, index, value, state);
            verify_function_value_escape_proof(cx, function_id, index, value, state);
            verify_global_set_root_stmt(cx, function_id, block_id, index, *global, value, *init);
            let mut next = state.clone();
            clear_transferred_temporaries(value, &mut next);
            next.set_global_value(
                *global,
                rvalue_function_state(cx.program, function_id, &cx.primitives, value, state),
            );
            Some(next)
        }
        AirStmt::GlobalUpdateRoot { global, value } => {
            verify_materialize_role(cx, function_id, block_id, index, value, true);
            verify_air_rvalue_reads(cx, function_id, index, value, state);
            verify_function_value_escape_proof(cx, function_id, index, value, state);
            verify_global_update_root_stmt(cx, function_id, block_id, index, *global, value, state);
            let mut next = state.clone();
            clear_transferred_temporaries(value, &mut next);
            next.set_global_value(
                *global,
                rvalue_function_state(cx.program, function_id, &cx.primitives, value, state),
            );
            Some(next)
        }
        AirStmt::If(branch) => verify_air_if(cx, function_id, index, branch, state, loops),
        AirStmt::Loop(loop_) => {
            loops.push(LoopCtx {
                id: loop_.id,
                breaks: vec![],
                backedges: vec![],
            });
            let mut body_state = state.clone();
            let fallthrough =
                verify_air_block(cx, function_id, &loop_.body, &mut body_state, loops);
            let mut backedges = loops
                .last_mut()
                .unwrap()
                .backedges
                .drain(..)
                .collect::<Vec<_>>();
            backedges.extend(fallthrough);
            if let Some(mut backedge) = LocalInit::join(backedges) {
                clear_loop_iteration_locals(&mut backedge, &loop_.body);
                verify_air_block(cx, function_id, &loop_.body, &mut backedge, loops);
            }
            let loop_ctx = loops.pop().unwrap();
            LocalInit::join(loop_ctx.breaks)
        }
        AirStmt::RangeFor(range) => {
            verify_air_operand_read(cx, function_id, index, &range.start, state);
            verify_air_operand_read(cx, function_id, index, &range.end, state);
            for operand in range.ordinal_plan.operands() {
                verify_air_operand_read(cx, function_id, index, operand, state);
            }
            verify_loop_int_parts(
                cx,
                function_id,
                block_id,
                index,
                std::iter::once(&range.start)
                    .chain(std::iter::once(&range.end))
                    .chain(range.ordinal_plan.operands()),
                range.ordinal.into_iter().chain([range.item]),
            );
            verify_air_for_body(
                cx,
                function_id,
                range.id,
                &range.body,
                state,
                loops,
                range.ordinal.into_iter().chain([range.item]),
            )
        }
        AirStmt::CollectionFor(for_) => {
            verify_air_local_read(cx, function_id, index, for_.len, state);
            for operand in for_.ordinal_plan.operands() {
                verify_air_operand_read(cx, function_id, index, operand, state);
            }
            verify_loop_int_parts(
                cx,
                function_id,
                block_id,
                index,
                for_.ordinal_plan.operands(),
                [Some(for_.len), Some(for_.index), for_.ordinal]
                    .into_iter()
                    .flatten(),
            );
            verify_air_for_body(
                cx,
                function_id,
                for_.id,
                &for_.body,
                state,
                loops,
                [Some(for_.index), for_.ordinal].into_iter().flatten(),
            )
        }
        AirStmt::CollectionLoan(loan) => {
            verify_collection_loan(cx, function_id, block_id, index, loan, state, loops)
        }
        AirStmt::CollectionSlotScope(scope) => {
            verify_collection_slot_scope(cx, function_id, block_id, index, scope, state, loops)
        }
        AirStmt::PatternMatch(match_) => {
            verify_air_pattern_match(cx, function_id, index, match_, state, loops)
        }
        AirStmt::DynMatch(match_) => {
            verify_air_dyn_match(cx, function_id, index, match_, state, loops)
        }
        AirStmt::OptionalMatch(match_) => {
            verify_air_optional_match(cx, function_id, index, match_, state, loops)
        }
        AirStmt::MapEntryMatch(match_) => {
            verify_air_map_entry_match(cx, function_id, index, match_, state, loops)
        }
    }
}

fn verify_air_for_body(
    cx: &mut VerifyCx<'_>,
    function_id: FunctionId,
    id: AirLoopId,
    body: &AirBlock,
    state: &LocalInit,
    loops: &mut Vec<LoopCtx>,
    locals: impl IntoIterator<Item = LocalId>,
) -> Option<LocalInit> {
    loops.push(LoopCtx {
        id,
        breaks: vec![],
        backedges: vec![],
    });
    let locals = locals.into_iter().collect::<Vec<_>>();
    let mut body_state = state.clone();
    init_loop_locals(&mut body_state, &locals);
    let fallthrough = verify_air_block(cx, function_id, body, &mut body_state, loops);
    let mut backedges = loops
        .last_mut()
        .unwrap()
        .backedges
        .drain(..)
        .collect::<Vec<_>>();
    backedges.extend(fallthrough);
    if let Some(mut backedge) = LocalInit::join(backedges) {
        clear_loop_iteration_locals(&mut backedge, body);
        init_loop_locals(&mut backedge, &locals);
        verify_air_block(cx, function_id, body, &mut backedge, loops);
    }
    let loop_ctx = loops.pop().unwrap();
    LocalInit::join(std::iter::once(state.clone()).chain(loop_ctx.breaks))
}

fn init_loop_locals(state: &mut LocalInit, locals: &[LocalId]) {
    for local in locals {
        state.init(*local);
        state.set_local_value(*local, FunctionValueState::non_function());
    }
}

fn clear_loop_iteration_locals(state: &mut LocalInit, block: &AirBlock) {
    for stmt in &block.stmts {
        match stmt {
            AirStmt::Init { local, .. } => state.clear(*local),
            AirStmt::CollectionSlotScope(scope) => {
                for slot in &scope.slots {
                    state.clear(slot.local);
                }
            }
            AirStmt::PatternMatch(match_) => {
                for arm in &match_.arms {
                    for alternative in &arm.alternatives {
                        for binding in &alternative.bindings {
                            state.clear(binding.local);
                        }
                    }
                }
            }
            AirStmt::DynMatch(match_) => {
                for binding in match_
                    .arms
                    .iter()
                    .filter_map(|arm| arm.binding.local())
                    .chain(match_.fallback.binding.local())
                {
                    state.clear(binding);
                }
            }
            AirStmt::OptionalMatch(match_) => {
                if let Some(payload) = match_.payload {
                    state.clear(payload);
                }
            }
            AirStmt::MapEntryMatch(match_) => {
                if let Some(payload) = match_.payload {
                    state.clear(payload);
                }
            }
            AirStmt::Assign { .. }
            | AirStmt::Eval(_)
            | AirStmt::GlobalEnsure { .. }
            | AirStmt::GlobalSetRoot { .. }
            | AirStmt::GlobalUpdateRoot { .. }
            | AirStmt::If(_)
            | AirStmt::Loop(_)
            | AirStmt::RangeFor(_)
            | AirStmt::CollectionFor(_)
            | AirStmt::CollectionLoan(_) => {}
        }
        stmt.for_each_child(&mut |child| {
            if let AirChild::Block(block) = child {
                clear_loop_iteration_locals(state, block);
            }
        });
    }
}

fn verify_collection_loan(
    cx: &mut VerifyCx<'_>,
    function_id: FunctionId,
    block_id: BlockId,
    index: usize,
    loan: &AirCollectionLoan,
    state: &LocalInit,
    loops: &mut Vec<LoopCtx>,
) -> Option<LocalInit> {
    let site = VerifyCx::stmt_site(function_id, block_id, index);
    verify_air_place_read(cx, function_id, index, &loan.root, state);
    let root_ty = verify_place(cx, function_id, block_id, Some(index), &loan.root);
    verify_collection_loan_root(cx, &site, loan, root_ty);
    if matches!(
        loan.mode,
        AirCollectionLoanMode::MutableSequenceElement | AirCollectionLoanMode::MutableMapValue
    ) {
        verify_mutable_place(cx, function_id, &site, &loan.root);
    }
    let mut body_state = state.clone();
    verify_air_block(cx, function_id, &loan.body, &mut body_state, loops)
}

fn verify_collection_slot_scope(
    cx: &mut VerifyCx<'_>,
    function_id: FunctionId,
    block_id: BlockId,
    index: usize,
    scope: &AirCollectionSlotScope,
    state: &LocalInit,
    loops: &mut Vec<LoopCtx>,
) -> Option<LocalInit> {
    let site = VerifyCx::stmt_site(function_id, block_id, index);
    verify_air_place_read(cx, function_id, index, &scope.root, state);
    verify_air_local_read(cx, function_id, index, scope.index, state);
    verify_slice_index(cx, function_id, block_id, index, "index", scope.index);
    let root_ty = verify_place(cx, function_id, block_id, Some(index), &scope.root);
    for slot in &scope.slots {
        let mode = match slot.kind {
            AirCollectionSlotKind::SequenceElement => AirCollectionLoanMode::MutableSequenceElement,
            AirCollectionSlotKind::MapValue => AirCollectionLoanMode::MutableMapValue,
        };
        let expected_slot = collection_slot_scope_expected(cx.program, slot.kind, root_ty);
        verify_collection_loan_slot(cx, function_id, &site, mode, expected_slot, slot, state);
    }
    let mut body_state = state.clone();
    for slot in &scope.slots {
        body_state.init(slot.local);
    }
    let mut exits = verify_air_block(cx, function_id, &scope.body, &mut body_state, loops);
    if let Some(next) = &mut exits {
        for slot in &scope.slots {
            next.clear(slot.local);
        }
    }
    exits
}

fn collection_slot_scope_expected(
    program: &Program,
    kind: AirCollectionSlotKind,
    root_ty: Option<TypeId>,
) -> Option<(AirCollectionSlotKind, TypeId, bool)> {
    let root_ty = root_ty?;
    match kind {
        AirCollectionSlotKind::SequenceElement => collection_sequence_elem(program, root_ty)
            .map(|ty| (AirCollectionSlotKind::SequenceElement, ty, true)),
        AirCollectionSlotKind::MapValue => typing::map_kv(program, root_ty)
            .map(|(_, value)| (AirCollectionSlotKind::MapValue, value, true)),
    }
}

fn verify_collection_loan_root(
    cx: &mut VerifyCx<'_>,
    site: &VerifySite,
    loan: &AirCollectionLoan,
    root_ty: Option<TypeId>,
) {
    let Some(root_ty) = root_ty else {
        return;
    };
    if !collection_root_kind_matches(cx.program, loan.root_kind, root_ty) {
        cx.push(
            site.clone(),
            VerifyErrorKind::BadFunction(BadFunction::CollectionLoanRootKindMismatch {
                root_kind: loan.root_kind,
                found: root_ty,
            }),
        );
    }
    if !collection_mode_matches_root_kind(loan.mode, loan.root_kind) {
        cx.push(
            site.clone(),
            VerifyErrorKind::BadFunction(BadFunction::CollectionLoanModeRootKindMismatch {
                root_kind: loan.root_kind,
                mode: loan.mode,
            }),
        );
    }
}

fn verify_collection_loan_slot(
    cx: &mut VerifyCx<'_>,
    function_id: FunctionId,
    site: &VerifySite,
    mode: AirCollectionLoanMode,
    expected_slot: Option<(AirCollectionSlotKind, TypeId, bool)>,
    slot: &AirCollectionSlot,
    state: &LocalInit,
) {
    let Some(local) = cx
        .program
        .function(function_id)
        .locals
        .get(slot.local.index())
    else {
        cx.push(
            site.clone(),
            VerifyErrorKind::BadReference(BadReference::InvalidLocal(slot.local)),
        );
        return;
    };
    if state.is_possible(slot.local) {
        cx.push(
            site.clone(),
            VerifyErrorKind::BadFunction(BadFunction::CollectionLoanSlotAlreadyInitialized(
                slot.local,
            )),
        );
    }
    if matches!(local.kind, LocalKind::Arg) {
        cx.push(
            site.clone(),
            VerifyErrorKind::BadFunction(BadFunction::CollectionLoanSlotMustBeFreshLocal {
                local: slot.local,
                kind: local.kind,
            }),
        );
    }
    if let Some((expected_kind, expected_ty, expected_mutable)) = expected_slot {
        if slot.kind != expected_kind {
            cx.push(
                site.clone(),
                VerifyErrorKind::BadFunction(BadFunction::CollectionLoanSlotKindMismatch {
                    local: slot.local,
                    mode,
                    kind: slot.kind,
                }),
            );
        }
        if !same_type(cx, slot.ty, expected_ty) {
            cx.push(
                site.clone(),
                VerifyErrorKind::BadFunction(BadFunction::CollectionLoanSlotTypeMismatch {
                    local: slot.local,
                    expected: expected_ty,
                    found: slot.ty,
                }),
            );
        }
        if !same_type(cx, local.ty, expected_ty) {
            cx.push(
                site.clone(),
                VerifyErrorKind::BadFunction(BadFunction::CollectionLoanSlotTypeMismatch {
                    local: slot.local,
                    expected: expected_ty,
                    found: local.ty,
                }),
            );
        }
        let local_mutable = local.mutability == Mutability::Mutable;
        if slot.mutable != expected_mutable {
            cx.push(
                site.clone(),
                VerifyErrorKind::BadFunction(BadFunction::CollectionLoanSlotMutabilityMismatch {
                    local: slot.local,
                    expected: expected_mutable,
                    found: slot.mutable,
                }),
            );
        }
        if local_mutable != expected_mutable {
            cx.push(
                site.clone(),
                VerifyErrorKind::BadFunction(BadFunction::CollectionLoanSlotMutabilityMismatch {
                    local: slot.local,
                    expected: expected_mutable,
                    found: local_mutable,
                }),
            );
        }
    } else {
        cx.push(
            site.clone(),
            VerifyErrorKind::BadFunction(BadFunction::CollectionLoanSlotKindMismatch {
                local: slot.local,
                mode,
                kind: slot.kind,
            }),
        );
    }
}

fn collection_root_kind_matches(
    program: &Program,
    root_kind: AirCollectionRootKind,
    root_ty: TypeId,
) -> bool {
    matches!(
        (root_kind, program.type_arena.get(root_ty)),
        (AirCollectionRootKind::List, Some(TypeData::List(_)))
            | (
                AirCollectionRootKind::FixedArray,
                Some(TypeData::Array { .. })
            )
            | (AirCollectionRootKind::Slice, Some(TypeData::Slice(_)))
            | (AirCollectionRootKind::Map, Some(TypeData::Map { .. }))
    )
}

fn collection_mode_matches_root_kind(
    mode: AirCollectionLoanMode,
    root_kind: AirCollectionRootKind,
) -> bool {
    match mode {
        AirCollectionLoanMode::ReadonlySequence | AirCollectionLoanMode::MutableSequenceElement => {
            matches!(
                root_kind,
                AirCollectionRootKind::List
                    | AirCollectionRootKind::FixedArray
                    | AirCollectionRootKind::Slice
            )
        }
        AirCollectionLoanMode::ReadonlyMap | AirCollectionLoanMode::MutableMapValue => {
            root_kind == AirCollectionRootKind::Map
        }
    }
}

fn collection_sequence_elem(program: &Program, root_ty: TypeId) -> Option<TypeId> {
    typing::sequence_elem(program, root_ty)
}

fn verify_dyn_borrow_root_contract(
    cx: &mut VerifyCx<'_>,
    function_id: FunctionId,
    block: &AirBlock,
) {
    for stmt in &block.stmts {
        if let AirStmt::DynMatch(match_) = stmt {
            match &match_.source {
                AirDynMatchSource::Borrowed(_) => {}
                AirDynMatchSource::Mutable(place) => {
                    if !matches!(place.root, PlaceRoot::DynBorrowParam(_)) {
                        reject_dyn_borrow_place(cx, function_id, place);
                    }
                }
                AirDynMatchSource::Owned(value) => {
                    reject_dyn_borrow_operand(cx, function_id, &value.value);
                }
            }
            for arm in &match_.arms {
                verify_dyn_borrow_root_contract(cx, function_id, &arm.block);
            }
            verify_dyn_borrow_root_contract(cx, function_id, &match_.fallback.block);
            continue;
        }
        stmt.for_each_child(&mut |child| verify_dyn_borrow_child(cx, function_id, child));
    }
    if let Some(value) = tail_return_operand(&block.tail) {
        reject_dyn_borrow_operand(cx, function_id, value);
    }
}

fn verify_dyn_borrow_child(cx: &mut VerifyCx<'_>, function_id: FunctionId, child: AirChild<'_>) {
    match child {
        AirChild::Operand { operand, .. } => {
            reject_dyn_borrow_operand(cx, function_id, operand);
        }
        AirChild::Place { place, .. } => reject_dyn_borrow_place(cx, function_id, place),
        AirChild::CallArg { arg, .. } => match arg {
            CallArg::DynBorrow(_) | CallArg::InitFieldOmitted | CallArg::SharedStringConst(_) => {}
            CallArg::Value(owned) | CallArg::InitFieldProvided(owned) => {
                reject_dyn_borrow_operand(cx, function_id, &owned.value);
            }
            CallArg::SharedBorrow(place) | CallArg::MutBorrow(place) => {
                reject_dyn_borrow_place(cx, function_id, place);
            }
        },
        AirChild::LambdaCapture(capture) => match capture {
            LambdaCaptureArg::ReadonlyLocal { value } => {
                reject_dyn_borrow_operand(cx, function_id, &value.value);
            }
            LambdaCaptureArg::ScopedLocal { place } | LambdaCaptureArg::ScopedBorrow { place } => {
                reject_dyn_borrow_place(cx, function_id, place);
            }
            LambdaCaptureArg::NoRuntime | LambdaCaptureArg::CaptureCell { .. } => {}
        },
        AirChild::RValue { value, use_ } => {
            value.for_each_child(use_, &mut |child| {
                verify_dyn_borrow_child(cx, function_id, child);
            });
        }
        AirChild::Block(block) => verify_dyn_borrow_root_contract(cx, function_id, block),
        AirChild::LocalRead(local) => {
            if let Some(decl) = cx
                .program
                .dyn_borrow_params
                .iter()
                .find(|decl| decl.owner == function_id && decl.source == local)
            {
                reject_dyn_borrow_place(
                    cx,
                    function_id,
                    &Place {
                        root: PlaceRoot::Local(local),
                        projection: vec![],
                        ty: decl.ty,
                    },
                );
            }
        }
        AirChild::DynBorrow(_) => {}
    }
}

fn reject_dyn_borrow_operand(cx: &mut VerifyCx<'_>, function_id: FunctionId, operand: &Operand) {
    if let Operand::Place(place) = operand {
        reject_dyn_borrow_place(cx, function_id, place);
    }
}

fn is_dyn_borrow_backing_local(
    program: &Program,
    function_id: FunctionId,
    root: PlaceRoot,
) -> bool {
    let PlaceRoot::Local(local) = root else {
        return false;
    };
    program
        .dyn_borrow_params
        .iter()
        .any(|decl| decl.owner == function_id && decl.source == local)
}

fn reject_dyn_borrow_place(cx: &mut VerifyCx<'_>, function_id: FunctionId, place: &Place) {
    let borrow = match place.root {
        PlaceRoot::DynBorrowParam(id) => Some(id),
        PlaceRoot::Local(local) => cx
            .program
            .dyn_borrow_params
            .iter()
            .enumerate()
            .find(|(_, decl)| decl.owner == function_id && decl.source == local)
            .map(|(index, _)| DynBorrowParamId::from_index(index)),
        PlaceRoot::ScopedBorrow(_)
        | PlaceRoot::CaptureCell(_)
        | PlaceRoot::LambdaCapture(_)
        | PlaceRoot::Global(_) => None,
    };
    if let Some(id) = borrow {
        cx.push(
            VerifySite::Function(function_id),
            VerifyErrorKind::BadPlace(BadPlace::DynBorrowParamEscapes(id)),
        );
    }
}

fn verify_collection_loan_contract(
    cx: &mut VerifyCx<'_>,
    function_id: FunctionId,
    body: &AirBlock,
) {
    let mut slot_locals = std::collections::HashSet::new();
    collect_collection_loan_slot_locals(body, &mut slot_locals);
    let mut active_slots = std::collections::HashSet::new();
    let mut active_loans = vec![];
    verify_collection_loan_contract_block(
        cx,
        function_id,
        body,
        &slot_locals,
        &mut active_slots,
        &mut active_loans,
    );
}

fn collect_collection_loan_slot_locals(
    block: &AirBlock,
    slot_locals: &mut std::collections::HashSet<LocalId>,
) {
    for stmt in &block.stmts {
        match stmt {
            AirStmt::CollectionLoan(loan) => {
                collect_collection_loan_slot_locals(&loan.body, slot_locals);
            }
            AirStmt::CollectionSlotScope(scope) => {
                for slot in &scope.slots {
                    slot_locals.insert(slot.local);
                }
                collect_collection_loan_slot_locals(&scope.body, slot_locals);
            }
            AirStmt::If(branch) => {
                collect_collection_loan_slot_locals(&branch.then_block, slot_locals);
                if let Some(block) = &branch.else_block {
                    collect_collection_loan_slot_locals(block, slot_locals);
                }
            }
            AirStmt::Loop(loop_) => collect_collection_loan_slot_locals(&loop_.body, slot_locals),
            AirStmt::RangeFor(range) => {
                collect_collection_loan_slot_locals(&range.body, slot_locals);
            }
            AirStmt::CollectionFor(for_) => {
                collect_collection_loan_slot_locals(&for_.body, slot_locals);
            }
            AirStmt::PatternMatch(match_) => {
                for arm in &match_.arms {
                    collect_collection_loan_slot_locals(&arm.block, slot_locals);
                }
            }
            AirStmt::DynMatch(match_) => {
                for arm in &match_.arms {
                    collect_collection_loan_slot_locals(&arm.block, slot_locals);
                }
                collect_collection_loan_slot_locals(&match_.fallback.block, slot_locals);
            }
            AirStmt::OptionalMatch(match_) => {
                collect_collection_loan_slot_locals(&match_.some_block, slot_locals);
                collect_collection_loan_slot_locals(&match_.none_block, slot_locals);
            }
            AirStmt::MapEntryMatch(match_) => {
                collect_collection_loan_slot_locals(&match_.some_block, slot_locals);
                collect_collection_loan_slot_locals(&match_.none_block, slot_locals);
            }
            AirStmt::Init { .. }
            | AirStmt::Assign { .. }
            | AirStmt::Eval(_)
            | AirStmt::GlobalEnsure { .. }
            | AirStmt::GlobalSetRoot { .. }
            | AirStmt::GlobalUpdateRoot { .. } => {}
        }
    }
}

#[derive(Clone)]
struct CollectionLoanFrame {
    root: Place,
    mode: AirCollectionLoanMode,
}

fn verify_collection_loan_contract_block(
    cx: &mut VerifyCx<'_>,
    function_id: FunctionId,
    block: &AirBlock,
    slot_locals: &std::collections::HashSet<LocalId>,
    active_slots: &mut std::collections::HashSet<LocalId>,
    active_loans: &mut Vec<CollectionLoanFrame>,
) {
    for stmt in &block.stmts {
        verify_collection_loan_contract_stmt(
            cx,
            function_id,
            stmt,
            slot_locals,
            active_slots,
            active_loans,
        );
    }
    verify_collection_loan_contract_tail(cx, function_id, &block.tail, slot_locals, active_slots);
}

fn verify_collection_loan_contract_stmt(
    cx: &mut VerifyCx<'_>,
    function_id: FunctionId,
    stmt: &AirStmt,
    slot_locals: &std::collections::HashSet<LocalId>,
    active_slots: &mut std::collections::HashSet<LocalId>,
    active_loans: &mut Vec<CollectionLoanFrame>,
) {
    match stmt {
        AirStmt::Init { value, .. } | AirStmt::Eval(value) => {
            verify_collection_loan_contract_rvalue(
                cx,
                function_id,
                value,
                slot_locals,
                active_slots,
                active_loans,
            );
        }
        AirStmt::GlobalSetRoot { global, value, .. }
        | AirStmt::GlobalUpdateRoot { global, value } => {
            if let Some(decl) = cx.program.globals.get(global.index()) {
                verify_collection_loan_root_rebind(
                    cx,
                    function_id,
                    active_loans,
                    &Place {
                        root: PlaceRoot::Global(*global),
                        projection: vec![],
                        ty: decl.ty,
                    },
                );
            }
            verify_collection_loan_contract_rvalue(
                cx,
                function_id,
                value,
                slot_locals,
                active_slots,
                active_loans,
            );
        }
        AirStmt::GlobalEnsure { .. } => {}
        AirStmt::Assign { dst, value } => {
            verify_collection_loan_contract_place(
                cx,
                function_id,
                dst,
                slot_locals,
                active_slots,
                false,
            );
            verify_collection_loan_root_rebind(cx, function_id, active_loans, dst);
            verify_collection_loan_contract_rvalue(
                cx,
                function_id,
                value,
                slot_locals,
                active_slots,
                active_loans,
            );
        }
        AirStmt::If(branch) => {
            verify_collection_loan_contract_operand(
                cx,
                function_id,
                &branch.cond,
                slot_locals,
                active_slots,
                false,
            );
            verify_collection_loan_contract_block(
                cx,
                function_id,
                &branch.then_block,
                slot_locals,
                active_slots,
                active_loans,
            );
            if let Some(block) = &branch.else_block {
                verify_collection_loan_contract_block(
                    cx,
                    function_id,
                    block,
                    slot_locals,
                    active_slots,
                    active_loans,
                );
            }
        }
        AirStmt::Loop(loop_) => verify_collection_loan_contract_block(
            cx,
            function_id,
            &loop_.body,
            slot_locals,
            active_slots,
            active_loans,
        ),
        AirStmt::RangeFor(range) => {
            for operand in std::iter::once(&range.start)
                .chain(std::iter::once(&range.end))
                .chain(range.ordinal_plan.operands())
            {
                verify_collection_loan_contract_operand(
                    cx,
                    function_id,
                    operand,
                    slot_locals,
                    active_slots,
                    false,
                );
            }
            verify_collection_loan_contract_block(
                cx,
                function_id,
                &range.body,
                slot_locals,
                active_slots,
                active_loans,
            );
        }
        AirStmt::CollectionFor(for_) => {
            for operand in for_.ordinal_plan.operands() {
                verify_collection_loan_contract_operand(
                    cx,
                    function_id,
                    operand,
                    slot_locals,
                    active_slots,
                    false,
                );
            }
            verify_collection_loan_contract_block(
                cx,
                function_id,
                &for_.body,
                slot_locals,
                active_slots,
                active_loans,
            );
        }
        AirStmt::CollectionLoan(loan) => {
            verify_collection_loan_contract_place(
                cx,
                function_id,
                &loan.root,
                slot_locals,
                active_slots,
                false,
            );
            active_loans.push(CollectionLoanFrame {
                root: loan.root.clone(),
                mode: loan.mode,
            });
            verify_collection_loan_contract_block(
                cx,
                function_id,
                &loan.body,
                slot_locals,
                active_slots,
                active_loans,
            );
            active_loans.pop();
        }
        AirStmt::CollectionSlotScope(scope) => {
            verify_collection_loan_contract_place(
                cx,
                function_id,
                &scope.root,
                slot_locals,
                active_slots,
                false,
            );
            verify_collection_loan_contract_local(
                cx,
                function_id,
                scope.index,
                slot_locals,
                active_slots,
                false,
            );
            for slot in &scope.slots {
                if !active_loan_allows_slot_scope(
                    cx.program,
                    function_id,
                    active_loans,
                    &scope.root,
                    slot,
                ) {
                    cx.push(
                        VerifySite::Function(function_id),
                        VerifyErrorKind::BadFunction(BadFunction::CollectionLoanSlotOutOfScope(
                            slot.local,
                        )),
                    );
                }
                active_slots.insert(slot.local);
            }
            verify_collection_loan_contract_block(
                cx,
                function_id,
                &scope.body,
                slot_locals,
                active_slots,
                active_loans,
            );
            for slot in &scope.slots {
                active_slots.remove(&slot.local);
            }
        }
        AirStmt::PatternMatch(match_) => {
            verify_collection_loan_contract_place(
                cx,
                function_id,
                &match_.subject,
                slot_locals,
                active_slots,
                false,
            );
            for arm in &match_.arms {
                verify_collection_loan_contract_block(
                    cx,
                    function_id,
                    &arm.block,
                    slot_locals,
                    active_slots,
                    active_loans,
                );
            }
        }
        AirStmt::DynMatch(match_) => {
            match &match_.source {
                AirDynMatchSource::Owned(value) => verify_collection_loan_contract_operand(
                    cx,
                    function_id,
                    &value.value,
                    slot_locals,
                    active_slots,
                    false,
                ),
                AirDynMatchSource::Borrowed(borrow) => verify_collection_loan_contract_place(
                    cx,
                    function_id,
                    borrow.place(),
                    slot_locals,
                    active_slots,
                    false,
                ),
                AirDynMatchSource::Mutable(place) => verify_collection_loan_contract_place(
                    cx,
                    function_id,
                    place,
                    slot_locals,
                    active_slots,
                    false,
                ),
            }
            for arm in &match_.arms {
                verify_collection_loan_contract_block(
                    cx,
                    function_id,
                    &arm.block,
                    slot_locals,
                    active_slots,
                    active_loans,
                );
            }
            verify_collection_loan_contract_block(
                cx,
                function_id,
                &match_.fallback.block,
                slot_locals,
                active_slots,
                active_loans,
            );
        }
        AirStmt::OptionalMatch(match_) => {
            verify_collection_loan_contract_place(
                cx,
                function_id,
                &match_.discr,
                slot_locals,
                active_slots,
                false,
            );
            verify_collection_loan_contract_block(
                cx,
                function_id,
                &match_.some_block,
                slot_locals,
                active_slots,
                active_loans,
            );
            verify_collection_loan_contract_block(
                cx,
                function_id,
                &match_.none_block,
                slot_locals,
                active_slots,
                active_loans,
            );
        }
        AirStmt::MapEntryMatch(match_) => {
            verify_collection_loan_contract_place(
                cx,
                function_id,
                &match_.map,
                slot_locals,
                active_slots,
                false,
            );
            verify_collection_loan_contract_operand(
                cx,
                function_id,
                &match_.key,
                slot_locals,
                active_slots,
                false,
            );
            verify_collection_loan_contract_block(
                cx,
                function_id,
                &match_.some_block,
                slot_locals,
                active_slots,
                active_loans,
            );
            verify_collection_loan_contract_block(
                cx,
                function_id,
                &match_.none_block,
                slot_locals,
                active_slots,
                active_loans,
            );
        }
    }
}

fn verify_collection_loan_contract_tail(
    cx: &mut VerifyCx<'_>,
    function_id: FunctionId,
    tail: &AirTail,
    slot_locals: &std::collections::HashSet<LocalId>,
    active_slots: &std::collections::HashSet<LocalId>,
) {
    if let Some(value) = tail_return_operand(tail) {
        verify_collection_loan_contract_operand(
            cx,
            function_id,
            value,
            slot_locals,
            active_slots,
            true,
        );
    }
}

fn verify_collection_loan_contract_rvalue(
    cx: &mut VerifyCx<'_>,
    function_id: FunctionId,
    value: &RValue,
    slot_locals: &std::collections::HashSet<LocalId>,
    active_slots: &std::collections::HashSet<LocalId>,
    active_loans: &[CollectionLoanFrame],
) {
    match value {
        RValue::Materialize(owned)
        | RValue::DynPack { value: owned, .. }
        | RValue::DynWeaken { value: owned, .. }
        | RValue::DynDowncast { value: owned, .. }
        | RValue::FunctionValue { value: owned, .. } => verify_collection_loan_contract_operand(
            cx,
            function_id,
            &owned.value,
            slot_locals,
            active_slots,
            false,
        ),
        RValue::Use(op)
        | RValue::Unary { value: op, .. }
        | RValue::Cast { value: op, .. }
        | RValue::RawProject { value: op, .. }
        | RValue::RawTryConstruct { value: op, .. }
        | RValue::Stringify { value: op, .. }
        | RValue::Format { value: op, .. } => verify_collection_loan_contract_operand(
            cx,
            function_id,
            op,
            slot_locals,
            active_slots,
            false,
        ),
        RValue::DynCall { receiver, args, .. } => {
            match receiver {
                DynReceiver::Owned(op) => verify_collection_loan_contract_operand(
                    cx,
                    function_id,
                    &op.value,
                    slot_locals,
                    active_slots,
                    false,
                ),
                DynReceiver::MutableOwned(place) => verify_collection_loan_contract_place(
                    cx,
                    function_id,
                    place,
                    slot_locals,
                    active_slots,
                    false,
                ),
                DynReceiver::Borrowed(borrow) => verify_collection_loan_contract_place(
                    cx,
                    function_id,
                    borrow.place(),
                    slot_locals,
                    active_slots,
                    false,
                ),
            }
            for arg in args {
                verify_collection_loan_contract_call_arg(
                    cx,
                    function_id,
                    arg,
                    slot_locals,
                    active_slots,
                    false,
                );
            }
        }
        RValue::OptionalSome { value, .. } => verify_collection_loan_contract_operand(
            cx,
            function_id,
            &value.value,
            slot_locals,
            active_slots,
            false,
        ),
        RValue::Binary { lhs, rhs, .. } | RValue::SharedRefEq { lhs, rhs, .. } => {
            verify_collection_loan_contract_operand(
                cx,
                function_id,
                lhs,
                slot_locals,
                active_slots,
                false,
            );
            verify_collection_loan_contract_operand(
                cx,
                function_id,
                rhs,
                slot_locals,
                active_slots,
                false,
            );
        }
        RValue::Aggregate { fields, .. } => {
            for field in fields {
                verify_collection_loan_contract_operand(
                    cx,
                    function_id,
                    &field.value,
                    slot_locals,
                    active_slots,
                    false,
                );
            }
        }
        RValue::Call { args, .. } => {
            for arg in args {
                verify_collection_loan_contract_call_arg(
                    cx,
                    function_id,
                    arg,
                    slot_locals,
                    active_slots,
                    false,
                );
            }
        }
        RValue::StringConcat { parts } => {
            for part in parts {
                verify_collection_loan_contract_operand(
                    cx,
                    function_id,
                    part,
                    slot_locals,
                    active_slots,
                    false,
                );
            }
        }
        RValue::Len { source } => {
            verify_collection_loan_contract_place(
                cx,
                function_id,
                source,
                slot_locals,
                active_slots,
                false,
            );
        }
        RValue::ListPop { list, .. } => {
            verify_collection_loan_contract_place(
                cx,
                function_id,
                list,
                slot_locals,
                active_slots,
                false,
            );
            verify_collection_loan_structural_op(
                cx,
                function_id,
                active_loans,
                list,
                CollectionStructuralEffect::Sequence(SequenceStructuralEffect::InternalPop),
            );
        }
        RValue::SliceView {
            source, start, end, ..
        }
        | RValue::RangeListCopy {
            source, start, end, ..
        } => {
            verify_collection_loan_contract_place(
                cx,
                function_id,
                source,
                slot_locals,
                active_slots,
                false,
            );
            verify_collection_loan_contract_local(
                cx,
                function_id,
                *start,
                slot_locals,
                active_slots,
                false,
            );
            verify_collection_loan_contract_local(
                cx,
                function_id,
                *end,
                slot_locals,
                active_slots,
                false,
            );
        }
        RValue::ListPush { list, value } => {
            verify_collection_loan_contract_place(
                cx,
                function_id,
                list,
                slot_locals,
                active_slots,
                false,
            );
            verify_collection_loan_contract_operand(
                cx,
                function_id,
                &value.value,
                slot_locals,
                active_slots,
                false,
            );
            verify_collection_loan_structural_op(
                cx,
                function_id,
                active_loans,
                list,
                CollectionStructuralEffect::Sequence(SequenceStructuralEffect::Push),
            );
        }
        RValue::MapGet { map, key, .. } => {
            verify_collection_loan_contract_place(
                cx,
                function_id,
                map,
                slot_locals,
                active_slots,
                false,
            );
            verify_collection_loan_contract_operand(
                cx,
                function_id,
                key,
                slot_locals,
                active_slots,
                false,
            );
        }
        RValue::MapInsert {
            map,
            key,
            value,
            kind,
        } => {
            verify_collection_loan_contract_place(
                cx,
                function_id,
                map,
                slot_locals,
                active_slots,
                false,
            );
            verify_collection_loan_contract_operand(
                cx,
                function_id,
                &key.value,
                slot_locals,
                active_slots,
                false,
            );
            verify_collection_loan_contract_operand(
                cx,
                function_id,
                &value.value,
                slot_locals,
                active_slots,
                false,
            );
            if *kind == MapWriteKind::StructuralInsert {
                verify_collection_loan_structural_op(
                    cx,
                    function_id,
                    active_loans,
                    map,
                    CollectionStructuralEffect::Map(MapStructuralEffect::Insert),
                );
            }
        }
        RValue::MapRemove { map, key, .. } => {
            verify_collection_loan_contract_place(
                cx,
                function_id,
                map,
                slot_locals,
                active_slots,
                false,
            );
            verify_collection_loan_contract_operand(
                cx,
                function_id,
                key,
                slot_locals,
                active_slots,
                false,
            );
            verify_collection_loan_structural_op(
                cx,
                function_id,
                active_loans,
                map,
                CollectionStructuralEffect::Map(MapStructuralEffect::Remove),
            );
        }
        RValue::MapEntryAt { map, index, .. }
        | RValue::MapKeyAt { map, index, .. }
        | RValue::MapValueAt { map, index, .. } => {
            verify_collection_loan_contract_place(
                cx,
                function_id,
                map,
                slot_locals,
                active_slots,
                false,
            );
            verify_collection_loan_contract_local(
                cx,
                function_id,
                *index,
                slot_locals,
                active_slots,
                false,
            );
        }
        RValue::CheckedIterCount { count, .. } => verify_collection_loan_contract_operand(
            cx,
            function_id,
            count,
            slot_locals,
            active_slots,
            false,
        ),
        RValue::FunctionRef { .. } | RValue::FlagStatic { .. } => {}
        RValue::MakeLambda {
            lambda, captures, ..
        } => {
            let escapes = cx
                .program
                .lambdas
                .get(lambda.index())
                .is_some_and(|decl| decl.escape == LambdaEscape::Escaping);
            for capture in captures {
                match capture {
                    LambdaCaptureArg::NoRuntime | LambdaCaptureArg::CaptureCell { .. } => {}
                    LambdaCaptureArg::ReadonlyLocal { value } => {
                        verify_collection_loan_contract_operand(
                            cx,
                            function_id,
                            &value.value,
                            slot_locals,
                            active_slots,
                            escapes,
                        );
                    }
                    LambdaCaptureArg::ScopedLocal { place }
                    | LambdaCaptureArg::ScopedBorrow { place } => {
                        verify_collection_loan_contract_place(
                            cx,
                            function_id,
                            place,
                            slot_locals,
                            active_slots,
                            escapes,
                        );
                    }
                }
            }
        }
    }
}

fn verify_collection_loan_structural_op(
    cx: &mut VerifyCx<'_>,
    function_id: FunctionId,
    active_loans: &[CollectionLoanFrame],
    place: &Place,
    op: CollectionStructuralEffect,
) {
    for loan in active_loans {
        if is_conflicting_collection_structural_op(loan.mode, op)
            && same_collection_root(cx.program, function_id, &loan.root, place)
        {
            cx.push(
                VerifySite::Function(function_id),
                VerifyErrorKind::BadFunction(BadFunction::CollectionLoanStructuralOpConflict {
                    mode: loan.mode,
                    op: op.name(),
                }),
            );
        }
    }
}

fn is_conflicting_collection_structural_op(
    mode: AirCollectionLoanMode,
    op: CollectionStructuralEffect,
) -> bool {
    match mode {
        AirCollectionLoanMode::ReadonlySequence | AirCollectionLoanMode::MutableSequenceElement => {
            op.sequence()
        }
        AirCollectionLoanMode::ReadonlyMap | AirCollectionLoanMode::MutableMapValue => op.map(),
    }
}

fn same_collection_root(
    program: &Program,
    function_id: FunctionId,
    left: &Place,
    right: &Place,
) -> bool {
    if left.projection != right.projection {
        return false;
    }
    if left.root == right.root {
        return true;
    }
    let left_alias = matches!(
        left.root,
        PlaceRoot::ScopedBorrow(_) | PlaceRoot::LambdaCapture(_) | PlaceRoot::CaptureCell(_)
    );
    let right_alias = matches!(
        right.root,
        PlaceRoot::ScopedBorrow(_) | PlaceRoot::LambdaCapture(_) | PlaceRoot::CaptureCell(_)
    );
    let alias_root = left_alias || right_alias;
    alias_root && program.places_may_overlap(function_id, left, right)
}

fn active_loan_allows_slot_scope(
    program: &Program,
    function_id: FunctionId,
    active_loans: &[CollectionLoanFrame],
    root: &Place,
    slot: &AirCollectionSlot,
) -> bool {
    active_loans.iter().any(|loan| {
        same_collection_root(program, function_id, &loan.root, root)
            && matches!(
                (loan.mode, slot.kind),
                (
                    AirCollectionLoanMode::MutableSequenceElement,
                    AirCollectionSlotKind::SequenceElement,
                ) | (
                    AirCollectionLoanMode::MutableMapValue,
                    AirCollectionSlotKind::MapValue
                )
            )
    })
}

fn verify_collection_loan_root_rebind(
    cx: &mut VerifyCx<'_>,
    function_id: FunctionId,
    active_loans: &[CollectionLoanFrame],
    dst: &Place,
) {
    for loan in active_loans {
        if collection_root_rebind_conflict(cx.program, function_id, &loan.root, dst) {
            cx.push(
                VerifySite::Function(function_id),
                VerifyErrorKind::BadFunction(BadFunction::CollectionLoanRootRebindConflict {
                    mode: loan.mode,
                }),
            );
        }
    }
}

fn collection_root_rebind_conflict(
    program: &Program,
    function_id: FunctionId,
    root: &Place,
    dst: &Place,
) -> bool {
    same_collection_root(program, function_id, root, dst) || place_replaces_root(dst, root)
}

fn place_replaces_root(dst: &Place, root: &Place) -> bool {
    dst.root == root.root
        && dst.projection.len() <= root.projection.len()
        && dst
            .projection
            .iter()
            .zip(&root.projection)
            .all(|(left, right)| left == right)
}

fn verify_collection_loan_contract_call_arg(
    cx: &mut VerifyCx<'_>,
    function_id: FunctionId,
    arg: &CallArg,
    slot_locals: &std::collections::HashSet<LocalId>,
    active_slots: &std::collections::HashSet<LocalId>,
    escapes: bool,
) {
    match arg {
        CallArg::Value(owned) | CallArg::InitFieldProvided(owned) => {
            verify_collection_loan_contract_operand(
                cx,
                function_id,
                &owned.value,
                slot_locals,
                active_slots,
                escapes,
            );
        }
        CallArg::SharedBorrow(place) | CallArg::MutBorrow(place) => {
            verify_collection_loan_contract_place(
                cx,
                function_id,
                place,
                slot_locals,
                active_slots,
                escapes,
            );
        }
        CallArg::DynBorrow(borrow) => verify_collection_loan_contract_place(
            cx,
            function_id,
            borrow.place(),
            slot_locals,
            active_slots,
            escapes,
        ),
        CallArg::InitFieldOmitted | CallArg::SharedStringConst(_) => {}
    }
}

fn verify_collection_loan_contract_operand(
    cx: &mut VerifyCx<'_>,
    function_id: FunctionId,
    op: &Operand,
    slot_locals: &std::collections::HashSet<LocalId>,
    active_slots: &std::collections::HashSet<LocalId>,
    escapes: bool,
) {
    if let Operand::Place(place) = op {
        verify_collection_loan_contract_place(
            cx,
            function_id,
            place,
            slot_locals,
            active_slots,
            escapes,
        );
    }
}

fn verify_collection_loan_contract_place(
    cx: &mut VerifyCx<'_>,
    function_id: FunctionId,
    place: &Place,
    slot_locals: &std::collections::HashSet<LocalId>,
    active_slots: &std::collections::HashSet<LocalId>,
    escapes: bool,
) {
    verify_collection_loan_contract_place_reads(
        cx,
        function_id,
        place,
        slot_locals,
        active_slots,
        escapes,
    );
    if let Some(source) = scoped_borrow_contract_source(cx.program, function_id, place) {
        verify_collection_loan_contract_place_reads(
            cx,
            function_id,
            &source,
            slot_locals,
            active_slots,
            escapes,
        );
    }
}

fn verify_collection_loan_contract_place_reads(
    cx: &mut VerifyCx<'_>,
    function_id: FunctionId,
    place: &Place,
    slot_locals: &std::collections::HashSet<LocalId>,
    active_slots: &std::collections::HashSet<LocalId>,
    escapes: bool,
) {
    place.for_each_read_local(&mut |local| {
        let local = match local {
            PlaceReadLocal::Root(local) | PlaceReadLocal::Index(local) => local,
        };
        verify_collection_loan_contract_local(
            cx,
            function_id,
            local,
            slot_locals,
            active_slots,
            escapes,
        );
    });
}

fn scoped_borrow_contract_source(
    program: &Program,
    function_id: FunctionId,
    place: &Place,
) -> Option<Place> {
    let borrow = program.scoped_borrow_root(function_id, place.root)?;
    let decl = program.scoped_borrows.get(borrow.index())?;
    if decl.owner != function_id {
        return None;
    }
    let mut source = match &decl.source {
        ScopedBorrowSource::SourceMutParam { local } | ScopedBorrowSource::RefSelf { local } => {
            Place {
                root: PlaceRoot::Local(*local),
                projection: vec![],
                ty: decl.ty,
            }
        }
        ScopedBorrowSource::PatternAlias { source }
        | ScopedBorrowSource::ForRefAlias { source } => source.clone(),
    };
    source.projection.extend(place.projection.clone());
    source.ty = place.ty;
    Some(source)
}

fn verify_collection_loan_contract_local(
    cx: &mut VerifyCx<'_>,
    function_id: FunctionId,
    local: LocalId,
    slot_locals: &std::collections::HashSet<LocalId>,
    active_slots: &std::collections::HashSet<LocalId>,
    escapes: bool,
) {
    if !slot_locals.contains(&local) {
        return;
    }
    let error = if active_slots.contains(&local) {
        escapes.then_some(BadFunction::CollectionLoanSlotEscapesBody(local))
    } else {
        Some(BadFunction::CollectionLoanSlotOutOfScope(local))
    };
    if let Some(error) = error {
        cx.push(
            VerifySite::Function(function_id),
            VerifyErrorKind::BadFunction(error),
        );
    }
}

fn verify_air_if(
    cx: &mut VerifyCx<'_>,
    function_id: FunctionId,
    index: usize,
    branch: &AirIf,
    state: &LocalInit,
    loops: &mut Vec<LoopCtx>,
) -> Option<LocalInit> {
    let site = VerifyCx::stmt_site(function_id, BlockId::from_index(0), index);
    verify_air_operand_read(cx, function_id, index, &branch.cond, state);
    verify_operand(
        cx,
        function_id,
        BlockId::from_index(0),
        Some(index),
        &branch.cond,
    );
    if let Some(cond_ty) = typing::operand_ty(cx.program, &branch.cond)
        && !cx.primitives.is_bool(cond_ty)
    {
        cx.push(
            site,
            VerifyErrorKind::BadFunction(BadFunction::IfCondMustBeBool(cond_ty)),
        );
    }

    let mut then_state = state.clone();
    let then_fallthrough =
        verify_air_block(cx, function_id, &branch.then_block, &mut then_state, loops);
    let else_fallthrough = match &branch.else_block {
        Some(else_block) => {
            let mut else_state = state.clone();
            verify_air_block(cx, function_id, else_block, &mut else_state, loops)
        }
        None => Some(state.clone()),
    };
    LocalInit::join([then_fallthrough, else_fallthrough].into_iter().flatten())
}

fn verify_air_pattern_match(
    cx: &mut VerifyCx<'_>,
    function_id: FunctionId,
    index: usize,
    match_: &AirPatternMatch,
    state: &LocalInit,
    loops: &mut Vec<LoopCtx>,
) -> Option<LocalInit> {
    let site = VerifyCx::stmt_site(function_id, BlockId::from_index(0), index);
    verify_air_place_read(cx, function_id, index, &match_.subject, state);
    let subject_ty = verify_place(
        cx,
        function_id,
        BlockId::from_index(0),
        Some(index),
        &match_.subject,
    );
    let Some(subject_ty) = subject_ty else {
        return Some(state.clone());
    };
    let subject_value = place_function_state(cx.program, function_id, &match_.subject, state, true);

    let mut fallthrough = vec![];
    for arm in &match_.arms {
        if arm.alternatives.is_empty() {
            cx.push(
                site.clone(),
                VerifyErrorKind::BadStatement(BadStatement::InvalidPatternMatch),
            );
            continue;
        }
        for alternative in &arm.alternatives {
            verify_air_pattern_alternative(cx, &site, function_id, subject_ty, alternative);
        }
        verify_pattern_alternative_binding_sets(cx, &site, arm);
        let mut alternatives = vec![];
        for alternative in &arm.alternatives {
            let guards = enum_variant_guards(alternative);
            let optional_guards = optional_some_guards(alternative);
            let mut alternative_state = state.clone();
            for binding in &alternative.bindings {
                verify_pattern_binding(
                    cx,
                    &site,
                    function_id,
                    subject_ty,
                    &subject_value,
                    &guards,
                    &optional_guards,
                    binding,
                    &mut alternative_state,
                );
            }
            alternatives.push(alternative_state);
        }
        let mut arm_state = LocalInit::join(alternatives).unwrap_or_else(|| state.clone());
        if let Some(mut state) =
            verify_air_block(cx, function_id, &arm.block, &mut arm_state, loops)
        {
            clear_pattern_binding_locals(&mut state, arm);
            fallthrough.push(state);
        }
    }
    LocalInit::join(fallthrough)
}

fn clear_taken_place(state: &mut LocalInit, place: &Place) {
    if let PlaceRoot::Local(local) = place.root
        && place.projection.is_empty()
    {
        state.clear(local);
    } else {
        state.clear_place_value(place);
    }
}

fn verify_air_dyn_match(
    cx: &mut VerifyCx<'_>,
    function_id: FunctionId,
    index: usize,
    match_: &AirDynMatch,
    state: &LocalInit,
    loops: &mut Vec<LoopCtx>,
) -> Option<LocalInit> {
    let site = VerifyCx::stmt_site(function_id, BlockId::from_index(0), index);
    let (source_ty, source_place, takes, binding_mutability) = match &match_.source {
        AirDynMatchSource::Owned(owned) => {
            verify_air_operand_read(cx, function_id, index, &owned.value, state);
            verify_operand(
                cx,
                function_id,
                BlockId::from_index(0),
                Some(index),
                &owned.value,
            );
            (
                typing::operand_ty(cx.program, &owned.value),
                match &owned.value {
                    Operand::Place(place) => Some(place),
                    Operand::Const(_) => None,
                },
                matches!(owned.source, ValueSource::TransferTemp { .. }),
                Mutability::Immutable,
            )
        }
        AirDynMatchSource::Borrowed(borrow) => {
            verify_dyn_borrow(cx, function_id, BlockId::from_index(0), Some(index), borrow);
            (
                Some(borrow.ty),
                Some(borrow.place()),
                false,
                Mutability::Immutable,
            )
        }
        AirDynMatchSource::Mutable(place) => {
            verify_air_place_write(cx, function_id, index, place, state);
            verify_mutable_place(cx, function_id, &site, place);
            (
                verify_place(cx, function_id, BlockId::from_index(0), Some(index), place),
                Some(place),
                false,
                Mutability::Mutable,
            )
        }
    };
    let ownership_valid = match &match_.source {
        AirDynMatchSource::Owned(owned) => owned_source_valid(cx, function_id, owned, false),
        AirDynMatchSource::Borrowed(_) | AirDynMatchSource::Mutable(_) => true,
    };
    let source_matches = ownership_valid && source_ty.is_some_and(|ty| {
        matches!(cx.program.type_arena.get(ty), Some(TypeData::Dyn(surface)) if *surface == match_.surface)
    });
    let mut valid = source_matches;
    let mut targets = vec![];
    let mut fallthrough = vec![];

    for arm in &match_.arms {
        let binding = arm.binding.local();
        valid &= matches!(
            (&match_.source, arm.binding),
            (
                AirDynMatchSource::Mutable(_),
                AirDynMatchTargetBinding::Alias(_)
            ) | (_, AirDynMatchTargetBinding::Discard)
                | (
                    AirDynMatchSource::Owned(OwnedValue {
                        source: ValueSource::TransferTemp { .. },
                        ..
                    }),
                    AirDynMatchTargetBinding::Take(_)
                )
                | (
                    AirDynMatchSource::Owned(OwnedValue {
                        source: ValueSource::Reusable,
                        ..
                    }) | AirDynMatchSource::Borrowed(_),
                    AirDynMatchTargetBinding::Materialize(_)
                )
        );
        let target_is_nominal = matches!(
            cx.program.type_arena.get(arm.target),
            Some(
                TypeData::Aggregate(_)
                    | TypeData::DataRef(_)
                    | TypeData::Enum(_)
                    | TypeData::Extern(_)
            )
        );
        let duplicate = targets.iter().any(|target| {
            cx.program.type_arena.get(*target) == cx.program.type_arena.get(arm.target)
        });
        valid &= target_is_nominal && !duplicate;
        targets.push(arm.target);

        let mut arm_state = state.clone();
        if takes && let Some(place) = source_place {
            clear_taken_place(&mut arm_state, place);
        }
        valid &= verify_dyn_match_binding(
            cx,
            function_id,
            binding,
            arm.target,
            source_call_return_state(cx.program, arm.target),
            binding_mutability,
            state,
            &mut arm_state,
        );
        if binding_mutability == Mutability::Mutable
            && binding.is_some()
            && let Some(place) = source_place
            && dyn_match_block_uses_root(cx.program, function_id, &arm.block, place, binding)
        {
            cx.push(
                site.clone(),
                VerifyErrorKind::BadStatement(BadStatement::DynMatchRootUsed),
            );
        }
        if binding_mutability == Mutability::Mutable
            && let Some(binding) = binding
            && matches!(
                cx.program.function(function_id).signature.return_mode,
                ReturnMode::Place(_)
            )
            && dyn_match_binding_returned(cx.program, function_id, &arm.block, binding)
        {
            cx.push(
                site.clone(),
                VerifyErrorKind::BadStatement(BadStatement::DynMatchAliasEscapes(binding)),
            );
            valid = false;
        }
        let break_counts = loops
            .iter()
            .map(|loop_| (loop_.breaks.len(), loop_.backedges.len()))
            .collect::<Vec<_>>();
        if let Some(mut next) = verify_air_block(cx, function_id, &arm.block, &mut arm_state, loops)
        {
            if let Some(binding) = binding {
                next.clear(binding);
            }
            fallthrough.push(next);
        }
        clear_dyn_match_loop_breaks(loops, &break_counts, binding);
    }

    let fallback_binding = match_.fallback.binding.local();
    valid &= matches!(
        (&match_.source, match_.fallback.binding),
        (
            AirDynMatchSource::Mutable(_),
            AirDynMatchFallbackBinding::Alias(_)
        ) | (_, AirDynMatchFallbackBinding::Discard)
            | (
                AirDynMatchSource::Owned(_) | AirDynMatchSource::Borrowed(_),
                AirDynMatchFallbackBinding::Preserve(_)
            )
    );
    let mut fallback_state = state.clone();
    if takes && let Some(place) = source_place {
        clear_taken_place(&mut fallback_state, place);
    }
    if let Some(source_ty) = source_ty {
        valid &= verify_dyn_match_binding(
            cx,
            function_id,
            fallback_binding,
            source_ty,
            source_call_return_state(cx.program, source_ty),
            binding_mutability,
            state,
            &mut fallback_state,
        );
    } else {
        valid = false;
    }
    if binding_mutability == Mutability::Mutable
        && fallback_binding.is_some()
        && let Some(place) = source_place
        && dyn_match_block_uses_root(
            cx.program,
            function_id,
            &match_.fallback.block,
            place,
            fallback_binding,
        )
    {
        cx.push(
            site.clone(),
            VerifyErrorKind::BadStatement(BadStatement::DynMatchRootUsed),
        );
    }
    if binding_mutability == Mutability::Mutable
        && let Some(binding) = fallback_binding
        && matches!(
            cx.program.function(function_id).signature.return_mode,
            ReturnMode::Place(_)
        )
        && dyn_match_binding_returned(cx.program, function_id, &match_.fallback.block, binding)
    {
        cx.push(
            site.clone(),
            VerifyErrorKind::BadStatement(BadStatement::DynMatchAliasEscapes(binding)),
        );
        valid = false;
    }
    let break_counts = loops
        .iter()
        .map(|loop_| (loop_.breaks.len(), loop_.backedges.len()))
        .collect::<Vec<_>>();
    if let Some(mut next) = verify_air_block(
        cx,
        function_id,
        &match_.fallback.block,
        &mut fallback_state,
        loops,
    ) {
        if let Some(binding) = fallback_binding {
            next.clear(binding);
        }
        fallthrough.push(next);
    }
    clear_dyn_match_loop_breaks(loops, &break_counts, fallback_binding);
    if !valid {
        cx.push(
            site,
            VerifyErrorKind::BadStatement(BadStatement::InvalidDynMatch),
        );
    }
    LocalInit::join(fallthrough)
}

fn verify_dyn_match_binding(
    cx: &VerifyCx<'_>,
    function_id: FunctionId,
    binding: Option<LocalId>,
    ty: TypeId,
    value: FunctionValueState,
    mutability: Mutability,
    prior: &LocalInit,
    state: &mut LocalInit,
) -> bool {
    let Some(binding) = binding else {
        return true;
    };
    let Some(local) = cx.program.function(function_id).locals.get(binding.index()) else {
        return false;
    };
    let valid = local.kind == LocalKind::PatternBinding
        && local.ty == ty
        && local.mutability == mutability
        && !prior.is_possible(binding);
    if valid {
        state.init(binding);
        state.set_local_value(binding, value);
    }
    valid
}

fn clear_dyn_match_loop_breaks(
    loops: &mut [LoopCtx],
    prior_counts: &[(usize, usize)],
    binding: Option<LocalId>,
) {
    let Some(binding) = binding else {
        return;
    };
    for (loop_, (breaks, backedges)) in loops.iter_mut().zip(prior_counts) {
        for state in &mut loop_.breaks[*breaks..] {
            state.clear(binding);
        }
        for state in &mut loop_.backedges[*backedges..] {
            state.clear(binding);
        }
    }
}

fn dyn_match_binding_returned(
    program: &Program,
    function_id: FunctionId,
    block: &AirBlock,
    binding: LocalId,
) -> bool {
    if matches!(
        tail_return_operand(&block.tail),
        Some(Operand::Place(place))
            if dyn_match_place_has_binding_origin(program, function_id, place, binding)
    ) {
        return true;
    }
    let mut returned = false;
    block.for_each_child(&mut |child| {
        if let AirChild::Block(block) = child {
            returned |= dyn_match_binding_returned(program, function_id, block, binding);
        }
    });
    returned
}

fn dyn_match_place_has_binding_origin(
    program: &Program,
    function_id: FunctionId,
    place: &Place,
    binding: LocalId,
) -> bool {
    if place.root == PlaceRoot::Local(binding) {
        return true;
    }
    let PlaceRoot::ScopedBorrow(id) = place.root else {
        return false;
    };
    let Some(decl) = program.scoped_borrows.get(id.index()) else {
        return false;
    };
    if decl.owner != function_id {
        return false;
    }
    matches!(
        &decl.source,
        ScopedBorrowSource::PatternAlias { source }
            if source.root == PlaceRoot::Local(binding)
    )
}

fn dyn_match_block_uses_root(
    program: &Program,
    function_id: FunctionId,
    block: &AirBlock,
    root: &Place,
    binding: Option<LocalId>,
) -> bool {
    let mut used = block.stmts.iter().any(|stmt| {
        let global = match stmt {
            AirStmt::GlobalSetRoot { global, .. } | AirStmt::GlobalUpdateRoot { global, .. } => {
                Some(*global)
            }
            _ => None,
        };
        global.is_some_and(|global| root.root == PlaceRoot::Global(global))
    });
    block.for_each_child(&mut |child| {
        used |= dyn_match_child_uses_root(program, function_id, child, root, binding);
    });
    used
}

fn dyn_match_child_uses_root(
    program: &Program,
    function_id: FunctionId,
    child: AirChild<'_>,
    root: &Place,
    binding: Option<LocalId>,
) -> bool {
    let conflicts = |place: &Place| {
        let guarded = binding.is_some_and(|binding| place.root == PlaceRoot::Local(binding));
        !guarded && program.places_may_overlap(function_id, root, place)
    };
    match child {
        AirChild::RValue { value, use_ } => {
            let mut used = false;
            value.for_each_child(use_, &mut |child| {
                used |= dyn_match_child_uses_root(program, function_id, child, root, binding);
            });
            used
        }
        AirChild::Operand { operand, .. } => {
            matches!(operand, Operand::Place(place) if conflicts(place))
        }
        AirChild::Place { place, .. } => conflicts(place),
        AirChild::CallArg { arg, .. } => arg.place().is_some_and(conflicts),
        AirChild::DynBorrow(borrow) => conflicts(borrow.place()),
        AirChild::LambdaCapture(capture) => match capture {
            LambdaCaptureArg::ReadonlyLocal { value } => {
                matches!(&value.value, Operand::Place(place) if conflicts(place))
            }
            LambdaCaptureArg::ScopedLocal { place } | LambdaCaptureArg::ScopedBorrow { place } => {
                conflicts(place)
            }
            LambdaCaptureArg::CaptureCell { cell } => {
                program.capture_cell_root(function_id, root.root) == Some(*cell)
                    || program
                        .capture_cell_source_place(function_id, *cell)
                        .is_some_and(|place| conflicts(&place))
            }
            LambdaCaptureArg::NoRuntime => false,
        },
        AirChild::Block(block) => {
            dyn_match_block_uses_root(program, function_id, block, root, binding)
        }
        AirChild::LocalRead(local) => {
            matches!(root.root, PlaceRoot::Local(root) if root == local)
        }
    }
}

fn clear_pattern_binding_locals(state: &mut LocalInit, arm: &AirPatternArm) {
    for alternative in &arm.alternatives {
        for binding in &alternative.bindings {
            state.clear(binding.local);
        }
    }
}

fn verify_air_pattern_alternative(
    cx: &mut VerifyCx<'_>,
    site: &VerifySite,
    function_id: FunctionId,
    subject_ty: TypeId,
    alternative: &AirPatternAlternative,
) {
    let guards = enum_variant_guards(alternative);
    let mut optional_guards = Vec::new();
    for test in &alternative.tests {
        match test {
            AirPatternTest::Any { branches } => {
                if branches.is_empty()
                    || branches
                        .iter()
                        .flatten()
                        .any(|test| !pattern_disjunction_test_supported(test))
                {
                    cx.push(
                        site.clone(),
                        VerifyErrorKind::BadStatement(BadStatement::InvalidPatternMatch),
                    );
                    continue;
                }
                for tests in branches {
                    verify_air_pattern_alternative(
                        cx,
                        site,
                        function_id,
                        subject_ty,
                        &AirPatternAlternative {
                            tests: tests.clone(),
                            bindings: vec![],
                        },
                    );
                }
            }
            AirPatternTest::Literal { path, value } => {
                if !cx.has_const(*value) {
                    cx.push(
                        site.clone(),
                        VerifyErrorKind::BadReference(BadReference::InvalidConst(*value)),
                    );
                    continue;
                }
                let Some(path_ty) =
                    verify_pattern_path(cx, site, subject_ty, &guards, &optional_guards, path)
                else {
                    continue;
                };
                let const_ty = cx.program.const_data(*value).ty;
                if !same_type(cx, path_ty, const_ty) {
                    cx.push(
                        site.clone(),
                        VerifyErrorKind::BadConst(BadConst::TypeMismatch {
                            expected: path_ty,
                            found: const_ty,
                        }),
                    );
                }
            }
            AirPatternTest::Nil { path } | AirPatternTest::OptionalSome { path } => {
                if let Some(path_ty) =
                    verify_pattern_path(cx, site, subject_ty, &guards, &optional_guards, path)
                    && typing::optional_inner(cx.program, path_ty).is_none()
                {
                    cx.push(
                        site.clone(),
                        VerifyErrorKind::BadConst(BadConst::NilMustBeOptional(path_ty)),
                    );
                }
                if matches!(test, AirPatternTest::OptionalSome { .. }) {
                    optional_guards.push(path.clone());
                }
            }
            AirPatternTest::FlagValue { path, flag, bits } => {
                if !cx.has_flag(*flag) {
                    cx.push(
                        site.clone(),
                        VerifyErrorKind::BadReference(BadReference::InvalidFlag(*flag)),
                    );
                    continue;
                }
                if let Some(path_ty) =
                    verify_pattern_path(cx, site, subject_ty, &guards, &optional_guards, path)
                    && !matches!(cx.type_data(path_ty), Some(TypeData::Flag(found)) if found == flag)
                {
                    cx.push(
                        site.clone(),
                        VerifyErrorKind::BadFlag(BadFlag::PatternTypeMismatch {
                            flag: *flag,
                            found: path_ty,
                        }),
                    );
                }
                let known_bits = cx.program.flag_decl(*flag).known_bits;
                if *bits < 0 || *bits & !known_bits != 0 {
                    cx.push(
                        site.clone(),
                        VerifyErrorKind::BadFlag(BadFlag::PatternUnknownBits {
                            flag: *flag,
                            bits: *bits,
                        }),
                    );
                }
            }
            AirPatternTest::EnumVariant {
                path,
                enum_id,
                variant,
            } => {
                if let Some(path_ty) =
                    verify_pattern_path(cx, site, subject_ty, &guards, &optional_guards, path)
                {
                    match cx.type_data(path_ty) {
                        Some(TypeData::Enum(found)) if found == enum_id => {
                            if !cx.variant_belongs_to_enum(*enum_id, *variant) {
                                cx.push(
                                    site.clone(),
                                    VerifyErrorKind::BadReference(BadReference::InvalidVariant {
                                        enum_id: *enum_id,
                                        variant: *variant,
                                    }),
                                );
                            }
                        }
                        _ => cx.push(
                            site.clone(),
                            VerifyErrorKind::BadFunction(
                                BadFunction::SwitchDiscriminantMustBeEnum(path_ty),
                            ),
                        ),
                    }
                }
            }
        }
    }
    let _ = function_id;
}

fn pattern_disjunction_test_supported(test: &AirPatternTest) -> bool {
    match test {
        AirPatternTest::Any { branches } => branches
            .iter()
            .flatten()
            .all(pattern_disjunction_test_supported),
        AirPatternTest::Literal { path, .. } | AirPatternTest::FlagValue { path, .. } => {
            path.steps.iter().all(|step| {
                matches!(
                    step,
                    AirPatternPathStep::Field(_) | AirPatternPathStep::TupleField(_)
                )
            })
        }
        AirPatternTest::Nil { .. }
        | AirPatternTest::OptionalSome { .. }
        | AirPatternTest::EnumVariant { .. } => false,
    }
}

#[derive(Clone)]
struct AirPatternVariantGuard {
    path: AirPatternPath,
    enum_id: EnumId,
    variant: VariantId,
}

#[derive(Clone, Copy, PartialEq, Eq)]
struct AirPatternBindingSignature {
    local: LocalId,
    ty: TypeId,
    mode: AirPatternBindingMode,
}

impl AirPatternBindingSignature {
    fn sort_key(self) -> (usize, usize, u8) {
        (
            self.local.index(),
            self.ty.index(),
            u8::from(self.mode == AirPatternBindingMode::Alias),
        )
    }
}

fn enum_variant_guards(alternative: &AirPatternAlternative) -> Vec<AirPatternVariantGuard> {
    alternative
        .tests
        .iter()
        .filter_map(|test| match test {
            AirPatternTest::EnumVariant {
                path,
                enum_id,
                variant,
            } => Some(AirPatternVariantGuard {
                path: path.clone(),
                enum_id: *enum_id,
                variant: *variant,
            }),
            _ => None,
        })
        .collect()
}

fn optional_some_guards(alternative: &AirPatternAlternative) -> Vec<AirPatternPath> {
    alternative
        .tests
        .iter()
        .filter_map(|test| match test {
            AirPatternTest::OptionalSome { path } => Some(path.clone()),
            _ => None,
        })
        .collect()
}

fn verify_pattern_alternative_binding_sets(
    cx: &mut VerifyCx<'_>,
    site: &VerifySite,
    arm: &AirPatternArm,
) {
    let Some(first) = arm.alternatives.first() else {
        return;
    };
    let expected = pattern_binding_signatures(cx, site, first);
    for alternative in &arm.alternatives[1..] {
        let found = pattern_binding_signatures(cx, site, alternative);
        if expected != found {
            report_pattern_binding_mismatch(cx, site, &expected, &found);
        }
    }
}

fn pattern_binding_signatures(
    cx: &mut VerifyCx<'_>,
    site: &VerifySite,
    alternative: &AirPatternAlternative,
) -> Vec<AirPatternBindingSignature> {
    let mut signatures = vec![];
    for binding in &alternative.bindings {
        if signatures
            .iter()
            .any(|signature: &AirPatternBindingSignature| signature.local == binding.local)
        {
            cx.push(
                site.clone(),
                VerifyErrorKind::BadFunction(BadFunction::PatternAlternativeBindingMismatch(
                    binding.local,
                )),
            );
        }
        signatures.push(AirPatternBindingSignature {
            local: binding.local,
            ty: binding.ty,
            mode: binding.mode,
        });
    }
    signatures.sort_by_key(|signature| signature.sort_key());
    signatures
}

fn report_pattern_binding_mismatch(
    cx: &mut VerifyCx<'_>,
    site: &VerifySite,
    expected: &[AirPatternBindingSignature],
    found: &[AirPatternBindingSignature],
) {
    if let Some(signature) = expected
        .iter()
        .chain(found)
        .find(|signature| expected.contains(signature) != found.contains(signature))
    {
        cx.push(
            site.clone(),
            VerifyErrorKind::BadFunction(BadFunction::PatternAlternativeBindingMismatch(
                signature.local,
            )),
        );
    }
}

fn verify_pattern_binding(
    cx: &mut VerifyCx<'_>,
    site: &VerifySite,
    function_id: FunctionId,
    subject_ty: TypeId,
    subject_value: &FunctionValueState,
    guards: &[AirPatternVariantGuard],
    optional_guards: &[AirPatternPath],
    binding: &AirPatternBinding,
    state: &mut LocalInit,
) {
    let Some(local) = cx
        .program
        .function(function_id)
        .locals
        .get(binding.local.index())
    else {
        cx.push(
            site.clone(),
            VerifyErrorKind::BadReference(BadReference::InvalidLocal(binding.local)),
        );
        return;
    };
    let Some(path_ty) =
        verify_pattern_path(cx, site, subject_ty, guards, optional_guards, &binding.path)
    else {
        return;
    };
    if !same_type(cx, path_ty, binding.ty) || !same_type(cx, local.ty, binding.ty) {
        cx.push(
            site.clone(),
            VerifyErrorKind::BadStatement(BadStatement::InitTypeMismatch {
                expected: path_ty,
                found: binding.ty,
            }),
        );
    }
    state.init(binding.local);
    state.set_local_value(
        binding.local,
        pattern_binding_function_state(subject_value, &binding.path),
    );
}

fn pattern_binding_function_state(
    subject: &FunctionValueState,
    path: &AirPatternPath,
) -> FunctionValueState {
    let mut value = subject.clone();
    for step in &path.steps {
        value = match step {
            AirPatternPathStep::Field(field) => value.project(&Projection::Field(*field)),
            AirPatternPathStep::TupleField(field) => value.project(&Projection::TupleField(*field)),
            AirPatternPathStep::OptionalSome => value.project(&Projection::TupleField(0)),
            AirPatternPathStep::EnumTupleField { variant, field, .. }
            | AirPatternPathStep::EnumStructField { variant, field, .. } => {
                value.variant_field(*variant, *field)
            }
        };
    }
    value
}

fn verify_pattern_path(
    cx: &mut VerifyCx<'_>,
    site: &VerifySite,
    subject_ty: TypeId,
    guards: &[AirPatternVariantGuard],
    optional_guards: &[AirPatternPath],
    path: &AirPatternPath,
) -> Option<TypeId> {
    let mut ty = subject_ty;
    let mut prefix = AirPatternPath::default();
    for step in &path.steps {
        if let Some((enum_id, variant)) = pattern_step_enum_payload_guard(step)
            && !guards.iter().any(|guard| {
                guard.path == prefix && guard.enum_id == enum_id && guard.variant == variant
            })
        {
            cx.push(
                site.clone(),
                VerifyErrorKind::BadFunction(BadFunction::PatternPayloadWithoutVariantTest),
            );
            return None;
        }
        if matches!(step, AirPatternPathStep::OptionalSome)
            && !optional_guards.iter().any(|guard| guard == &prefix)
        {
            cx.push(
                site.clone(),
                VerifyErrorKind::BadFunction(BadFunction::PatternPayloadWithoutVariantTest),
            );
            return None;
        }
        let Some(next_ty) = pattern_path_step_ty(cx, ty, step) else {
            cx.push(
                site.clone(),
                VerifyErrorKind::BadFunction(BadFunction::PatternPathInvalid),
            );
            return None;
        };
        prefix.steps.push(step.clone());
        ty = next_ty;
    }
    Some(ty)
}

fn pattern_step_enum_payload_guard(step: &AirPatternPathStep) -> Option<(EnumId, VariantId)> {
    match step {
        AirPatternPathStep::EnumTupleField {
            enum_id, variant, ..
        }
        | AirPatternPathStep::EnumStructField {
            enum_id, variant, ..
        } => Some((*enum_id, *variant)),
        AirPatternPathStep::Field(_)
        | AirPatternPathStep::TupleField(_)
        | AirPatternPathStep::OptionalSome => None,
    }
}

fn pattern_path_step_ty(
    cx: &mut VerifyCx<'_>,
    source_ty: TypeId,
    step: &AirPatternPathStep,
) -> Option<TypeId> {
    match step {
        AirPatternPathStep::Field(field) => typing::field_by_id(cx.program, source_ty, *field),
        AirPatternPathStep::TupleField(index) => typing::tuple_field(cx.program, source_ty, *index),
        AirPatternPathStep::OptionalSome => typing::optional_inner(cx.program, source_ty),
        AirPatternPathStep::EnumTupleField {
            enum_id,
            variant,
            field,
        } => enum_payload_field_ty(
            cx,
            source_ty,
            *enum_id,
            *variant,
            VariantShapeKind::Tuple,
            *field,
        ),
        AirPatternPathStep::EnumStructField {
            enum_id,
            variant,
            field,
        } => enum_payload_field_ty(
            cx,
            source_ty,
            *enum_id,
            *variant,
            VariantShapeKind::Struct,
            *field,
        ),
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum VariantShapeKind {
    Tuple,
    Struct,
}

fn enum_payload_field_ty(
    cx: &mut VerifyCx<'_>,
    source_ty: TypeId,
    enum_id: EnumId,
    variant: VariantId,
    expected_kind: VariantShapeKind,
    field: u16,
) -> Option<TypeId> {
    match cx.type_data(source_ty) {
        Some(TypeData::Enum(found))
            if *found == enum_id && cx.variant_belongs_to_enum(enum_id, variant) => {}
        Some(_) | None => return None,
    }
    let variant = &cx.program.enum_decl(enum_id).variants[variant.index()];
    match (&variant.shape, expected_kind) {
        (VariantShape::Tuple(fields), VariantShapeKind::Tuple) => {
            fields.get(field as usize).copied()
        }
        (VariantShape::Struct(fields), VariantShapeKind::Struct) => {
            fields.get(field as usize).map(|field| field.ty)
        }
        _ => None,
    }
}

fn verify_air_optional_match(
    cx: &mut VerifyCx<'_>,
    function_id: FunctionId,
    index: usize,
    match_: &AirOptionalMatch,
    state: &LocalInit,
    loops: &mut Vec<LoopCtx>,
) -> Option<LocalInit> {
    let site = VerifyCx::stmt_site(function_id, BlockId::from_index(0), index);
    verify_air_place_read(cx, function_id, index, &match_.discr, state);
    let discr_ty = verify_place(
        cx,
        function_id,
        BlockId::from_index(0),
        Some(index),
        &match_.discr,
    );
    if match_.payload_ref {
        verify_mutable_place(cx, function_id, &site, &match_.discr);
    }
    let inner = match discr_ty {
        Some(ty) => match typing::optional_inner(cx.program, ty) {
            Some(inner) => Some(inner),
            None => {
                cx.push(
                    site.clone(),
                    VerifyErrorKind::BadFunction(BadFunction::SwitchDiscriminantMustBeEnum(ty)),
                );
                None
            }
        },
        None => None,
    };

    let mut some_state = state.clone();
    if let Some(payload) = match_.payload {
        let function = cx.program.function(function_id);
        match function.locals.get(payload.index()) {
            Some(local) => {
                if function
                    .signature
                    .params
                    .iter()
                    .any(|param| param.local_id == payload)
                {
                    cx.push(
                        site.clone(),
                        VerifyErrorKind::BadStatement(BadStatement::InitParamLocal(payload)),
                    );
                }
                if state.is_possible(payload) {
                    cx.push(
                        site.clone(),
                        VerifyErrorKind::BadFunction(
                            BadFunction::OptionalPayloadLocalAlreadyInitialized(payload),
                        ),
                    );
                }
                let expected_mutability = if match_.payload_ref {
                    Mutability::Mutable
                } else {
                    Mutability::Immutable
                };
                if local.mutability != expected_mutability {
                    cx.push(
                        site.clone(),
                        VerifyErrorKind::BadFunction(
                            BadFunction::OptionalPayloadLocalMustBeImmutable(payload),
                        ),
                    );
                }
                if let Some(inner) = inner
                    && !same_type(cx, local.ty, inner)
                {
                    cx.push(
                        site.clone(),
                        VerifyErrorKind::BadStatement(BadStatement::InitTypeMismatch {
                            expected: inner,
                            found: local.ty,
                        }),
                    );
                }
                some_state.init(payload);
                some_state.set_local_value(
                    payload,
                    optional_payload_function_state(cx.program, function_id, &match_.discr, state),
                );
            }
            None => cx.push(
                site.clone(),
                VerifyErrorKind::BadReference(BadReference::InvalidLocal(payload)),
            ),
        }
    }

    if (match_.payload_ref || match_.payload_escapes) && match_.payload.is_none() {
        cx.push(
            site.clone(),
            VerifyErrorKind::BadFunction(BadFunction::OptionalPayloadEscapeRequiresPayload),
        );
    }
    if match_.payload_escapes && !match_.payload_ref {
        cx.push(
            site.clone(),
            VerifyErrorKind::BadFunction(BadFunction::OptionalPayloadEscapeRequiresRef),
        );
    }

    let mut some_fallthrough =
        verify_air_block(cx, function_id, &match_.some_block, &mut some_state, loops);
    if match_.payload_ref
        && !match_.payload_escapes
        && let (Some(payload), Some(state)) = (match_.payload, &mut some_fallthrough)
    {
        state.clear(payload);
    }
    let mut none_state = state.clone();
    let none_fallthrough =
        verify_air_block(cx, function_id, &match_.none_block, &mut none_state, loops);
    if match_.payload_escapes && none_fallthrough.is_some() {
        cx.push(
            site,
            VerifyErrorKind::BadFunction(BadFunction::OptionalPayloadEscapeNoneMustDiverge),
        );
    }
    LocalInit::join([some_fallthrough, none_fallthrough].into_iter().flatten())
}

fn verify_air_map_entry_match(
    cx: &mut VerifyCx<'_>,
    function_id: FunctionId,
    index: usize,
    match_: &AirMapEntryMatch,
    state: &LocalInit,
    loops: &mut Vec<LoopCtx>,
) -> Option<LocalInit> {
    let site = VerifyCx::stmt_site(function_id, BlockId::from_index(0), index);
    verify_air_place_read(cx, function_id, index, &match_.map, state);
    verify_air_operand_read(cx, function_id, index, &match_.key, state);
    let map_ty = verify_place(
        cx,
        function_id,
        BlockId::from_index(0),
        Some(index),
        &match_.map,
    );
    verify_mutable_place(cx, function_id, &site, &match_.map);
    let kv = map_ty.and_then(|ty| typing::map_kv(cx.program, ty));
    if kv.is_none()
        && let Some(ty) = map_ty
    {
        cx.push(
            site.clone(),
            VerifyErrorKind::BadFunction(BadFunction::MapEntrySourceMustBeMap(ty)),
        );
    }
    if let Some((key_ty, _)) = kv
        && let Some(found) = typing::operand_ty(cx.program, &match_.key)
        && !same_type(cx, found, key_ty)
    {
        cx.push(
            site.clone(),
            VerifyErrorKind::BadStatement(BadStatement::InitTypeMismatch {
                expected: key_ty,
                found,
            }),
        );
    }

    let mut some_state = state.clone();
    if let Some(payload) = match_.payload {
        let function = cx.program.function(function_id);
        match function.locals.get(payload.index()) {
            Some(local) => {
                if function
                    .signature
                    .params
                    .iter()
                    .any(|param| param.local_id == payload)
                    || state.is_possible(payload)
                {
                    cx.push(
                        site.clone(),
                        VerifyErrorKind::BadStatement(BadStatement::InitParamLocal(payload)),
                    );
                }
                if local.mutability != Mutability::Mutable {
                    cx.push(
                        site.clone(),
                        VerifyErrorKind::BadFunction(
                            BadFunction::OptionalPayloadLocalMustBeImmutable(payload),
                        ),
                    );
                }
                if let Some((_, value_ty)) = kv
                    && !same_type(cx, local.ty, value_ty)
                {
                    cx.push(
                        site.clone(),
                        VerifyErrorKind::BadStatement(BadStatement::InitTypeMismatch {
                            expected: value_ty,
                            found: local.ty,
                        }),
                    );
                }
                some_state.init(payload);
                some_state.set_local_value(
                    payload,
                    kv.map_or_else(FunctionValueState::unknown, |(_, value_ty)| {
                        source_call_return_state(cx.program, value_ty)
                    }),
                );
            }
            None => cx.push(
                site.clone(),
                VerifyErrorKind::BadReference(BadReference::InvalidLocal(payload)),
            ),
        }
    }
    if match_.payload_escapes && match_.payload.is_none() {
        cx.push(
            site.clone(),
            VerifyErrorKind::BadFunction(BadFunction::OptionalPayloadEscapeRequiresPayload),
        );
    }

    let mut some_fallthrough =
        verify_air_block(cx, function_id, &match_.some_block, &mut some_state, loops);
    if !match_.payload_escapes
        && let (Some(payload), Some(state)) = (match_.payload, &mut some_fallthrough)
    {
        state.clear(payload);
    }
    let mut none_state = state.clone();
    let none_fallthrough =
        verify_air_block(cx, function_id, &match_.none_block, &mut none_state, loops);
    if let (Some(payload), Some(state)) = (match_.payload, &none_fallthrough)
        && state.is_possible(payload)
    {
        cx.push(
            site.clone(),
            VerifyErrorKind::BadFunction(BadFunction::OptionalPayloadLocalAlreadyInitialized(
                payload,
            )),
        );
    }
    if match_.payload_escapes && none_fallthrough.is_some() {
        cx.push(
            site,
            VerifyErrorKind::BadFunction(BadFunction::OptionalPayloadEscapeNoneMustDiverge),
        );
    }
    LocalInit::join([some_fallthrough, none_fallthrough].into_iter().flatten())
}

fn verify_air_tail(
    cx: &mut VerifyCx<'_>,
    function_id: FunctionId,
    tail: &AirTail,
    state: &LocalInit,
    loops: &mut [LoopCtx],
) -> Option<LocalInit> {
    let site = VerifyCx::term_site(function_id, BlockId::from_index(0));
    match tail {
        AirTail::None => Some(state.clone()),
        AirTail::Return(value) => {
            let function = cx.program.function(function_id);
            if let Some(value) = value {
                verify_air_operand_read(cx, function_id, 0, value, state);
                if matches!(function.signature.return_mode, ReturnMode::Value(_)) {
                    cx.push(
                        site.clone(),
                        VerifyErrorKind::BadRValue(BadRValue::InvalidMaterialize),
                    );
                }
            }
            verify_return(
                cx,
                function_id,
                BlockId::from_index(0),
                site,
                function.signature.return_mode,
                value.as_ref(),
            );
            None
        }
        AirTail::ReturnOwned(owned) => {
            verify_air_operand_read(cx, function_id, 0, &owned.value, state);
            let function = cx.program.function(function_id);
            if matches!(function.signature.return_mode, ReturnMode::Place(_))
                || !owned_source_valid(cx, function_id, owned, false)
            {
                cx.push(
                    site.clone(),
                    VerifyErrorKind::BadRValue(BadRValue::InvalidMaterialize),
                );
            }
            verify_return(
                cx,
                function_id,
                BlockId::from_index(0),
                site,
                function.signature.return_mode,
                Some(&owned.value),
            );
            None
        }
        AirTail::Break(id) => {
            if let Some(loop_ctx) = loops.iter_mut().rev().find(|loop_ctx| loop_ctx.id == *id) {
                loop_ctx.breaks.push(state.clone());
            } else {
                cx.push(
                    site,
                    VerifyErrorKind::BadFunction(BadFunction::BreakOutsideLoop(*id)),
                );
            }
            None
        }
        AirTail::Continue(id) => {
            if let Some(loop_ctx) = loops.iter_mut().rev().find(|loop_ctx| loop_ctx.id == *id) {
                loop_ctx.backedges.push(state.clone());
            } else {
                cx.push(
                    site,
                    VerifyErrorKind::BadFunction(BadFunction::ContinueOutsideLoop(*id)),
                );
            }
            None
        }
        AirTail::Unreachable => None,
    }
}

fn verify_air_rvalue_reads(
    cx: &mut VerifyCx<'_>,
    function_id: FunctionId,
    index: usize,
    value: &RValue,
    state: &LocalInit,
) {
    match value {
        RValue::Call { callee, args } => {
            verify_call_escape_args(cx, function_id, index, callee, args, state);
        }
        RValue::DynCall {
            surface,
            slot,
            args,
            ..
        } => verify_dyn_call_escape_args(cx, function_id, index, *surface, *slot, args, state),
        _ => {}
    }
    value.for_each_child(ValueUse::Read, &mut |child| match child {
        AirChild::Operand { operand, .. } => {
            verify_air_operand_read(cx, function_id, index, operand, state);
        }
        AirChild::Place { place, .. } => {
            verify_air_place_read(cx, function_id, index, place, state);
        }
        AirChild::CallArg { arg, .. } => match arg {
            CallArg::Value(owned) | CallArg::InitFieldProvided(owned) => {
                verify_air_operand_read(cx, function_id, index, &owned.value, state);
            }
            CallArg::SharedBorrow(place) | CallArg::MutBorrow(place) => {
                verify_air_place_read(cx, function_id, index, place, state);
            }
            CallArg::DynBorrow(borrow) => {
                verify_air_place_read(cx, function_id, index, borrow.place(), state);
            }
            CallArg::InitFieldOmitted | CallArg::SharedStringConst(_) => {}
        },
        AirChild::DynBorrow(borrow) => {
            verify_air_place_read(cx, function_id, index, borrow.place(), state);
        }
        AirChild::LocalRead(local) => {
            verify_air_local_read(cx, function_id, index, local, state);
        }
        AirChild::LambdaCapture(capture) => match capture {
            LambdaCaptureArg::ReadonlyLocal { value } => {
                verify_air_operand_read(cx, function_id, index, &value.value, state);
            }
            LambdaCaptureArg::ScopedLocal { place } | LambdaCaptureArg::ScopedBorrow { place } => {
                verify_air_place_read(cx, function_id, index, place, state);
            }
            LambdaCaptureArg::NoRuntime | LambdaCaptureArg::CaptureCell { .. } => {}
        },
        AirChild::RValue { .. } | AirChild::Block(_) => {}
    });
}

fn verify_air_operand_read(
    cx: &mut VerifyCx<'_>,
    function_id: FunctionId,
    index: usize,
    op: &Operand,
    state: &LocalInit,
) {
    if let Operand::Place(place) = op {
        verify_air_place_read(cx, function_id, index, place, state);
    }
}

fn verify_air_place_read(
    cx: &mut VerifyCx<'_>,
    function_id: FunctionId,
    index: usize,
    place: &Place,
    state: &LocalInit,
) {
    place.for_each_read_local(&mut |local| {
        let local = match local {
            PlaceReadLocal::Root(local) | PlaceReadLocal::Index(local) => local,
        };
        verify_air_local_read(cx, function_id, index, local, state);
    });
    if let PlaceRoot::CaptureCell(cell) = place.root {
        verify_air_cell_read(cx, function_id, index, cell, state);
    }
}

fn verify_air_place_write(
    cx: &mut VerifyCx<'_>,
    function_id: FunctionId,
    index: usize,
    place: &Place,
    state: &LocalInit,
) {
    for projection in &place.projection {
        if let Projection::Index(local) = projection {
            verify_air_local_read(cx, function_id, index, *local, state);
        }
    }
    match place.root {
        PlaceRoot::Local(local) if !state.is_definite(local) => cx.push(
            VerifyCx::stmt_site(function_id, BlockId::from_index(0), index),
            VerifyErrorKind::BadStatement(BadStatement::AssignUninitializedLocal(local)),
        ),
        PlaceRoot::CaptureCell(cell) if !place.projection.is_empty() => {
            verify_air_cell_write(cx, function_id, index, cell, state);
        }
        _ => {}
    }
}

fn verify_air_local_read(
    cx: &mut VerifyCx<'_>,
    function_id: FunctionId,
    index: usize,
    local: LocalId,
    state: &LocalInit,
) {
    let site = VerifyCx::stmt_site(function_id, BlockId::from_index(0), index);
    verify_promoted_local_not_used(cx, function_id, &site, local);
    if !state.is_definite(local) {
        cx.push(
            site,
            VerifyErrorKind::BadStatement(BadStatement::ReadUninitializedLocal(local)),
        );
    }
}

fn verify_air_cell_read(
    cx: &mut VerifyCx<'_>,
    function_id: FunctionId,
    index: usize,
    cell: CaptureCellId,
    state: &LocalInit,
) {
    if !state.cell_is_definite(cell) {
        cx.push(
            VerifyCx::stmt_site(function_id, BlockId::from_index(0), index),
            VerifyErrorKind::BadStatement(BadStatement::ReadUninitializedCaptureCell(cell)),
        );
    }
}

fn verify_air_cell_write(
    cx: &mut VerifyCx<'_>,
    function_id: FunctionId,
    index: usize,
    cell: CaptureCellId,
    state: &LocalInit,
) {
    if !state.cell_is_definite(cell) {
        cx.push(
            VerifyCx::stmt_site(function_id, BlockId::from_index(0), index),
            VerifyErrorKind::BadStatement(BadStatement::AssignUninitializedCaptureCell(cell)),
        );
    }
}

fn verify_promoted_local_not_initialized(
    cx: &mut VerifyCx<'_>,
    function_id: FunctionId,
    block_id: BlockId,
    index: usize,
    local: LocalId,
) {
    let site = VerifyCx::stmt_site(function_id, block_id, index);
    verify_promoted_local_not_used(cx, function_id, &site, local);
}

fn verify_promoted_local_not_used(
    cx: &mut VerifyCx<'_>,
    function_id: FunctionId,
    site: &VerifySite,
    local: LocalId,
) {
    for (cell_index, decl) in cx.program.capture_cells.iter().enumerate() {
        if decl.owner == function_id && decl.source_local == local {
            cx.push(
                site.clone(),
                VerifyErrorKind::BadPlace(BadPlace::PromotedBindingBypassesCell {
                    binding: decl.binding,
                    cell: CaptureCellId::from_index(cell_index),
                    local,
                }),
            );
        }
    }
    for (borrow_index, decl) in cx.program.scoped_borrows.iter().enumerate() {
        let source = match &decl.source {
            ScopedBorrowSource::SourceMutParam { local }
            | ScopedBorrowSource::RefSelf { local } => Some(*local),
            ScopedBorrowSource::PatternAlias { .. } | ScopedBorrowSource::ForRefAlias { .. } => {
                None
            }
        };
        if decl.owner == function_id && source == Some(local) {
            cx.push(
                site.clone(),
                VerifyErrorKind::BadPlace(BadPlace::PromotedBindingBypassesScopedBorrow {
                    binding: decl.binding,
                    borrow: ScopedBorrowId::from_index(borrow_index),
                    local,
                }),
            );
        }
    }
}

fn verify_loop_int_parts<'a>(
    cx: &mut VerifyCx<'_>,
    function_id: FunctionId,
    block_id: BlockId,
    index: usize,
    operands: impl IntoIterator<Item = &'a Operand>,
    locals: impl IntoIterator<Item = LocalId>,
) {
    let site = VerifyCx::stmt_site(function_id, block_id, index);
    let Some(int_ty) = cx.primitives.int() else {
        cx.push(
            site,
            VerifyErrorKind::BadRValue(BadRValue::MissingPrimitive(PrimitiveKind::Int)),
        );
        return;
    };
    for operand in operands {
        if let Some(found) = typing::operand_ty(cx.program, operand)
            && !same_type(cx, int_ty, found)
        {
            cx.push(
                site.clone(),
                VerifyErrorKind::BadStatement(BadStatement::InitTypeMismatch {
                    expected: int_ty,
                    found,
                }),
            );
        }
    }
    for local in locals {
        let Some(local_decl) = cx.program.function(function_id).locals.get(local.index()) else {
            cx.push(
                site.clone(),
                VerifyErrorKind::BadReference(BadReference::InvalidLocal(local)),
            );
            continue;
        };
        if !same_type(cx, int_ty, local_decl.ty) {
            cx.push(
                site.clone(),
                VerifyErrorKind::BadStatement(BadStatement::InitTypeMismatch {
                    expected: int_ty,
                    found: local_decl.ty,
                }),
            );
        }
    }
}

fn verify_init_stmt(
    cx: &mut VerifyCx<'_>,
    function_id: FunctionId,
    block_id: BlockId,
    index: usize,
    local: LocalId,
    value: &RValue,
) {
    let function = cx.program.function(function_id);
    let site = VerifyCx::stmt_site(function_id, block_id, index);
    if local.index() >= function.locals.len() {
        cx.push(
            site.clone(),
            VerifyErrorKind::BadReference(BadReference::InvalidLocal(local)),
        );
    } else {
        let target = &function.locals[local.index()];
        if function
            .signature
            .params
            .iter()
            .any(|param| param.local_id == local)
        {
            cx.push(
                site.clone(),
                VerifyErrorKind::BadStatement(BadStatement::InitParamLocal(local)),
            );
        }
        cx.verify_type_ref(site.clone(), target.ty);
        if let Some(value_ty) = typing::rvalue_ty(cx.program, &cx.primitives, value)
            && !same_type(cx, value_ty, target.ty)
        {
            cx.push(
                site.clone(),
                VerifyErrorKind::BadStatement(BadStatement::InitTypeMismatch {
                    expected: target.ty,
                    found: value_ty,
                }),
            );
        }
    }
    verify_rvalue(cx, function_id, block_id, Some(index), value);
}

fn verify_assign_stmt(
    cx: &mut VerifyCx<'_>,
    function_id: FunctionId,
    block_id: BlockId,
    index: usize,
    dst: &Place,
    value: &RValue,
) {
    let site = VerifyCx::stmt_site(function_id, block_id, index);
    let dst_ty = verify_place(cx, function_id, block_id, Some(index), dst);
    if let PlaceRoot::Global(global) = dst.root
        && dst.projection.is_empty()
    {
        cx.push(
            site.clone(),
            VerifyErrorKind::BadStatement(BadStatement::AssignGlobalRoot(global)),
        );
    }
    verify_mutable_place(cx, function_id, &site, dst);
    verify_rvalue(cx, function_id, block_id, Some(index), value);
    if let (Some(expected), Some(found)) =
        (dst_ty, typing::rvalue_ty(cx.program, &cx.primitives, value))
        && !same_type(cx, expected, found)
    {
        cx.push(
            site,
            VerifyErrorKind::BadStatement(BadStatement::AssignTypeMismatch { expected, found }),
        );
    }
}

fn verify_global_set_root_stmt(
    cx: &mut VerifyCx<'_>,
    function_id: FunctionId,
    block_id: BlockId,
    index: usize,
    global: GlobalId,
    value: &RValue,
    init: GlobalInitEffect,
) {
    let site = VerifyCx::stmt_site(function_id, block_id, index);
    if init != GlobalInitEffect::StoreWithoutInit {
        cx.push(
            site.clone(),
            VerifyErrorKind::BadStatement(BadStatement::GlobalSetRootInitMustStoreWithoutInit),
        );
    }
    verify_global_root_store_stmt(
        cx,
        function_id,
        block_id,
        index,
        site,
        global,
        value,
        |expected, found| BadStatement::GlobalSetRootTypeMismatch { expected, found },
    );
}

fn verify_global_update_root_stmt(
    cx: &mut VerifyCx<'_>,
    function_id: FunctionId,
    block_id: BlockId,
    index: usize,
    global: GlobalId,
    value: &RValue,
    state: &LocalInit,
) {
    let site = VerifyCx::stmt_site(function_id, block_id, index);
    if !state.global_is_definite(global) {
        cx.push(
            site.clone(),
            VerifyErrorKind::BadStatement(BadStatement::GlobalUpdateRootWithoutEnsure(global)),
        );
    }
    verify_global_root_store_stmt(
        cx,
        function_id,
        block_id,
        index,
        site,
        global,
        value,
        |expected, found| BadStatement::GlobalUpdateRootTypeMismatch { expected, found },
    );
}

fn verify_global_root_store_stmt(
    cx: &mut VerifyCx<'_>,
    function_id: FunctionId,
    block_id: BlockId,
    index: usize,
    site: VerifySite,
    global: GlobalId,
    value: &RValue,
    mismatch: impl Fn(TypeId, TypeId) -> BadStatement,
) {
    let Some(decl) = cx.program.globals.get(global.index()) else {
        cx.push(
            site.clone(),
            VerifyErrorKind::BadReference(BadReference::InvalidGlobal(global)),
        );
        verify_rvalue(cx, function_id, block_id, Some(index), value);
        return;
    };
    if decl.mutability == Mutability::Immutable {
        cx.push(
            site.clone(),
            VerifyErrorKind::BadPlace(BadPlace::ImmutableRoot(PlaceRoot::Global(global))),
        );
    }
    verify_rvalue(cx, function_id, block_id, Some(index), value);
    if let Some(found) = typing::rvalue_ty(cx.program, &cx.primitives, value)
        && !same_type(cx, decl.ty, found)
    {
        cx.push(
            site,
            VerifyErrorKind::BadStatement(mismatch(decl.ty, found)),
        );
    }
}

fn verify_return(
    cx: &mut VerifyCx<'_>,
    function_id: FunctionId,
    block_id: BlockId,
    site: VerifySite,
    ret: ReturnMode,
    value: Option<&Operand>,
) {
    let ret_ty = ret.ty();
    let ret_is_void = cx.primitives.void() == Some(ret_ty);
    match value {
        None => {
            if !ret_is_void {
                cx.push(
                    site,
                    VerifyErrorKind::BadFunction(BadFunction::NonVoidFunctionMustReturnValue(
                        ret_ty,
                    )),
                );
            }
        }
        Some(op) => {
            if ret_is_void {
                cx.push(
                    site.clone(),
                    VerifyErrorKind::BadFunction(BadFunction::VoidFunctionMustReturnNone),
                );
            }
            if matches!(ret, ReturnMode::Place(_)) && !matches!(op, Operand::Place(_)) {
                cx.push(
                    site.clone(),
                    VerifyErrorKind::BadFunction(BadFunction::PlaceReturnMustReturnPlace),
                );
            }
            verify_operand(cx, function_id, block_id, None, op);
            if let Some(op_ty) = typing::operand_ty(cx.program, op)
                && op_ty != ret_ty
            {
                cx.push(
                    site,
                    VerifyErrorKind::BadFunction(BadFunction::ReturnedTypeMismatch {
                        expected: ret_ty,
                        found: op_ty,
                    }),
                );
            }
        }
    }
}

fn verify_stringify(
    cx: &mut VerifyCx<'_>,
    function_id: FunctionId,
    block_id: BlockId,
    stmt_index: Option<usize>,
    site: VerifySite,
    value: &Operand,
    source_ty: TypeId,
) {
    required_rvalue_primitive(cx, site.clone(), PrimitiveKind::String);
    cx.verify_type_ref(site.clone(), source_ty);
    match cx.type_data(source_ty) {
        Some(TypeData::Any) => cx.push(
            site.clone(),
            VerifyErrorKind::BadRValue(BadRValue::StringifyAnySource { source: source_ty }),
        ),
        Some(TypeData::Void) => cx.push(
            site.clone(),
            VerifyErrorKind::BadRValue(BadRValue::StringifyVoidSource { source: source_ty }),
        ),
        _ => {}
    }
    verify_operand(cx, function_id, block_id, stmt_index, value);
    if let Some(operand) = typing::operand_ty(cx.program, value)
        && operand != source_ty
    {
        cx.push(
            site,
            VerifyErrorKind::BadRValue(BadRValue::StringifyOperandTypeMismatch {
                operand,
                source: source_ty,
            }),
        );
    }
}

fn verify_materialize_role(
    cx: &mut VerifyCx<'_>,
    function: FunctionId,
    block: BlockId,
    index: usize,
    value: &RValue,
    owning: bool,
) {
    let invalid = if owning {
        matches!(value, RValue::Use(_))
    } else {
        matches!(value, RValue::Materialize(_))
    };
    if invalid {
        cx.push(
            VerifyCx::stmt_site(function, block, index),
            VerifyErrorKind::BadRValue(BadRValue::InvalidMaterialize),
        );
    }
}

fn owned_source_valid(
    cx: &VerifyCx<'_>,
    function: FunctionId,
    owned: &OwnedValue<Operand>,
    allow_slice_transfer: bool,
) -> bool {
    let slice = matches!(
        &owned.value,
        Operand::Place(place)
            if matches!(cx.program.type_arena.get(place.ty), Some(TypeData::Slice(_)))
    );
    match owned.source {
        ValueSource::Reusable => !slice,
        ValueSource::TransferTemp { local } => {
            (!slice || allow_slice_transfer)
                && matches!(
                    &owned.value,
                    Operand::Place(place)
                        if place.root == PlaceRoot::Local(local) && place.projection.is_empty()
                )
                && cx.transferable[function.index()].contains(&local)
        }
    }
}

fn rvalue_owned_sources_valid(cx: &VerifyCx<'_>, function: FunctionId, value: &RValue) -> bool {
    let valid = |owned: &OwnedValue<Operand>, allow_slice| {
        owned_source_valid(cx, function, owned, allow_slice)
    };
    match value {
        RValue::Materialize(owned)
        | RValue::DynPack { value: owned, .. }
        | RValue::DynWeaken { value: owned, .. }
        | RValue::DynDowncast { value: owned, .. }
        | RValue::FunctionValue { value: owned, .. }
        | RValue::ListPush { value: owned, .. } => valid(owned, false),
        RValue::OptionalSome { value: owned, .. } => valid(owned, true),
        RValue::Aggregate { kind, fields, .. } => fields.iter().all(|owned| {
            valid(owned, matches!(kind, AggregateCtor::EnumVariant { .. }))
                && (!matches!(kind, AggregateCtor::ArrayFill)
                    || matches!(owned.source, ValueSource::Reusable))
        }),
        RValue::MapInsert { key, value, .. } => valid(key, false) && valid(value, false),
        RValue::Call { args, .. } => call_owned_sources_valid(args, &valid),
        RValue::DynCall { receiver, args, .. } => {
            (!matches!(receiver, DynReceiver::Owned(owned) if !valid(owned, false)))
                && call_owned_sources_valid(args, &valid)
        }
        RValue::MakeLambda { captures, .. } => captures.iter().all(|capture| {
            !matches!(capture, LambdaCaptureArg::ReadonlyLocal { value } if !valid(value, false))
        }),
        _ => true,
    }
}

fn call_owned_sources_valid(
    args: &[CallArg],
    valid: &impl Fn(&OwnedValue<Operand>, bool) -> bool,
) -> bool {
    args.iter().all(|arg| match arg {
        CallArg::Value(owned) | CallArg::InitFieldProvided(owned) => valid(owned, true),
        _ => true,
    })
}

fn verify_rvalue(
    cx: &mut VerifyCx<'_>,
    function_id: FunctionId,
    block_id: BlockId,
    stmt_index: Option<usize>,
    value: &RValue,
) {
    let stmt_idx = stmt_index.unwrap_or(0);
    let site = VerifyCx::stmt_site(function_id, block_id, stmt_idx);
    if !rvalue_owned_sources_valid(cx, function_id, value) {
        cx.push(
            site.clone(),
            VerifyErrorKind::BadRValue(BadRValue::InvalidMaterialize),
        );
    }

    match value {
        RValue::Use(op) => {
            verify_operand(cx, function_id, block_id, stmt_index, op);
        }
        RValue::Materialize(owned) => {
            verify_operand(cx, function_id, block_id, stmt_index, &owned.value);
        }
        RValue::DynPack { value, witness, ty } => {
            verify_operand(cx, function_id, block_id, stmt_index, &value.value);
            let valid = cx.program.contract_witnesses.get(witness.index()).is_some_and(|decl| {
                typing::operand_ty(cx.program, &value.value) == Some(decl.key.concrete_ty)
                    && matches!(cx.program.type_arena.get(*ty), Some(TypeData::Dyn(surface)) if *surface == decl.key.surface)
            });
            if !valid {
                cx.push(
                    site.clone(),
                    VerifyErrorKind::BadRValue(BadRValue::InvalidDynPack),
                );
            }
            cx.verify_type_ref(site, *ty);
        }
        RValue::DynWeaken {
            value,
            weakening,
            ty,
        } => {
            verify_operand(cx, function_id, block_id, stmt_index, &value.value);
            let source_ty = typing::operand_ty(cx.program, &value.value);
            let valid = cx.program.contract_weakenings.get(weakening.index()).is_some_and(|decl| {
                matches!(source_ty.and_then(|ty| cx.program.type_arena.get(ty)), Some(TypeData::Dyn(surface)) if *surface == decl.source)
                    && matches!(cx.program.type_arena.get(*ty), Some(TypeData::Dyn(surface)) if *surface == decl.target)
            });
            if !valid {
                cx.push(
                    site.clone(),
                    VerifyErrorKind::BadRValue(BadRValue::InvalidDynWeaken),
                );
            }
            cx.verify_type_ref(site, *ty);
        }
        RValue::DynDowncast {
            value,
            surface,
            target,
            ty,
        } => {
            verify_operand(cx, function_id, block_id, stmt_index, &value.value);
            let source_matches = matches!(
                typing::operand_ty(cx.program, &value.value)
                    .and_then(|ty| cx.program.type_arena.get(ty)),
                Some(TypeData::Dyn(found)) if found == surface
            );
            let target_is_nominal = matches!(
                cx.program.type_arena.get(*target),
                Some(
                    TypeData::Aggregate(_)
                        | TypeData::DataRef(_)
                        | TypeData::Enum(_)
                        | TypeData::Flag(_)
                        | TypeData::Extern(_)
                )
            );
            let option_matches = matches!(
                cx.program.type_arena.get(*ty),
                Some(TypeData::Optional(found)) if found == target
            );
            if !(source_matches && target_is_nominal && option_matches) {
                cx.push(
                    site.clone(),
                    VerifyErrorKind::BadRValue(BadRValue::InvalidDynDowncast),
                );
            }
            cx.verify_type_ref(site.clone(), *target);
            cx.verify_type_ref(site, *ty);
        }
        RValue::DynCall {
            receiver,
            surface,
            slot,
            args,
        } => verify_dyn_call(
            cx,
            function_id,
            block_id,
            stmt_index,
            receiver,
            *surface,
            *slot,
            args,
        ),
        RValue::FunctionValue { value, .. } => {
            verify_operand(cx, function_id, block_id, stmt_index, &value.value);
            verify_function_value_operand_type(cx, site, &value.value);
        }
        RValue::Stringify { value, source_ty } => {
            verify_stringify(
                cx,
                function_id,
                block_id,
                stmt_index,
                site,
                value,
                *source_ty,
            );
        }
        RValue::Unary {
            op: unary,
            value: op,
            ty,
        } => {
            verify_operand(cx, function_id, block_id, stmt_index, op);
            cx.verify_type_ref(site.clone(), *ty);
            if let Some(value_ty) = typing::operand_ty(cx.program, op) {
                let valid = match (cx.primitives.scalar(value_ty), cx.primitives.scalar(*ty)) {
                    (Some(value), Some(result)) => unary.scalar_result(value) == Some(result),
                    _ => {
                        unary == &UnaryOp::BitNot
                            && value_ty == *ty
                            && matches!(cx.type_data(value_ty), Some(TypeData::Flag(_)))
                    }
                };
                if !valid {
                    cx.push(
                        site,
                        VerifyErrorKind::BadRValue(BadRValue::UnaryTypeMismatch {
                            value: value_ty,
                            result: *ty,
                        }),
                    );
                }
            }
        }
        RValue::Binary { op, lhs, rhs, ty } => {
            verify_operand(cx, function_id, block_id, stmt_index, lhs);
            verify_operand(cx, function_id, block_id, stmt_index, rhs);
            cx.verify_type_ref(site.clone(), *ty);
            if op.has_lazy_rhs() {
                cx.push(
                    site,
                    VerifyErrorKind::BadRValue(BadRValue::UnsupportedBinaryOp(*op)),
                );
            } else if let (Some(lhs_ty), Some(rhs_ty)) = (
                typing::operand_ty(cx.program, lhs),
                typing::operand_ty(cx.program, rhs),
            ) {
                let valid = match (
                    cx.primitives.scalar(lhs_ty),
                    cx.primitives.scalar(rhs_ty),
                    cx.primitives.scalar(*ty),
                ) {
                    (Some(lhs), Some(rhs), Some(result)) => {
                        op.scalar_result(lhs, rhs) == Some(result)
                    }
                    _ if lhs_ty == rhs_ty
                        && matches!(cx.type_data(lhs_ty), Some(TypeData::Flag(_))) =>
                    {
                        match op {
                            BinaryOp::BitAnd | BinaryOp::BitOr | BinaryOp::Xor => *ty == lhs_ty,
                            BinaryOp::Eq | BinaryOp::NotEq => Some(*ty) == cx.primitives.bool(),
                            _ => false,
                        }
                    }
                    _ => {
                        matches!(op, BinaryOp::Eq | BinaryOp::NotEq)
                            && lhs_ty == rhs_ty
                            && Some(*ty) == cx.primitives.bool()
                            && cx.program.unit_only_enum(lhs_ty)
                    }
                };
                if !valid {
                    cx.push(
                        site,
                        VerifyErrorKind::BadRValue(BadRValue::BinaryTypeMismatch {
                            lhs: lhs_ty,
                            rhs: rhs_ty,
                            result: *ty,
                        }),
                    );
                }
            }
        }
        RValue::SharedRefEq { lhs, rhs, .. } => {
            verify_operand(cx, function_id, block_id, stmt_index, lhs);
            verify_operand(cx, function_id, block_id, stmt_index, rhs);
            required_rvalue_primitive(cx, site, PrimitiveKind::Bool);
        }
        RValue::OptionalSome { value, ty } => {
            verify_operand(cx, function_id, block_id, stmt_index, &value.value);
            cx.verify_type_ref(site.clone(), *ty);
            match (
                typing::optional_inner(cx.program, *ty),
                typing::operand_ty(cx.program, &value.value),
            ) {
                (Some(inner), Some(value_ty)) if inner == value_ty => {}
                (Some(inner), Some(value_ty)) => cx.push(
                    site,
                    VerifyErrorKind::BadRValue(BadRValue::OptionalSomeTypeMismatch {
                        expected: inner,
                        found: value_ty,
                    }),
                ),
                _ => cx.push(
                    site,
                    VerifyErrorKind::BadRValue(BadRValue::OptionalSomeTypeMismatch {
                        expected: *ty,
                        found: typing::operand_ty(cx.program, &value.value).unwrap_or(*ty),
                    }),
                ),
            }
        }
        RValue::Cast { value: op, target } => {
            verify_operand(cx, function_id, block_id, stmt_index, op);
            cx.verify_type_ref(site.clone(), *target);
            if let Some(value_ty) = typing::operand_ty(cx.program, op)
                && !typing::valid_cast(&cx.primitives, value_ty, *target)
            {
                cx.push(
                    site,
                    VerifyErrorKind::BadRValue(BadRValue::CastMustConvertIntAndFloat {
                        value: value_ty,
                        target: *target,
                    }),
                );
            }
        }
        RValue::RawProject { value: op, target } => {
            verify_operand(cx, function_id, block_id, stmt_index, op);
            cx.verify_type_ref(site.clone(), *target);
            if let Some(value_ty) = typing::operand_ty(cx.program, op)
                && !typing::valid_raw_project(cx.program, value_ty, *target)
            {
                cx.push(
                    site,
                    VerifyErrorKind::BadRValue(BadRValue::RawProjectMustMatchBacking {
                        value: value_ty,
                        target: *target,
                    }),
                );
            }
        }
        RValue::RawTryConstruct {
            value: op,
            target,
            ty,
        } => {
            verify_operand(cx, function_id, block_id, stmt_index, op);
            cx.verify_type_ref(site.clone(), *target);
            cx.verify_type_ref(site.clone(), *ty);
            if let Some(value_ty) = typing::operand_ty(cx.program, op)
                && !typing::valid_raw_try_construct(cx.program, value_ty, *target, *ty)
            {
                cx.push(
                    site,
                    VerifyErrorKind::BadRValue(BadRValue::RawTryConstructMustMatch {
                        value: value_ty,
                        target: *target,
                        result: *ty,
                    }),
                );
            }
        }
        RValue::FlagStatic { ty, .. } => {
            cx.verify_type_ref(site.clone(), *ty);
            if !matches!(cx.type_data(*ty), Some(TypeData::Flag(_))) {
                cx.push(
                    site,
                    VerifyErrorKind::BadRValue(BadRValue::FlagStaticMustBeFlag(*ty)),
                );
            }
        }
        RValue::Aggregate { kind, fields, ty } => {
            cx.verify_type_ref(site.clone(), *ty);
            match kind {
                AggregateCtor::Struct(id) => {
                    verify_aggregate_ctor(
                        cx,
                        site.clone(),
                        *id,
                        AggregateKind::Struct,
                        *ty,
                        fields,
                    );
                }
                AggregateCtor::DataRef(id) => verify_aggregate_ctor(
                    cx,
                    site.clone(),
                    *id,
                    AggregateKind::DataRef,
                    *ty,
                    fields,
                ),
                AggregateCtor::EnumVariant { enum_id, variant } => {
                    verify_enum_ctor(cx, site.clone(), *enum_id, *variant, *ty, fields);
                }
                AggregateCtor::Array | AggregateCtor::ArrayFill => {
                    verify_array_ctor(cx, site.clone(), *ty, fields);
                }
                AggregateCtor::List => verify_list_ctor(cx, site.clone(), *ty, fields),
                AggregateCtor::Map => verify_map_ctor(cx, site.clone(), *ty, fields),
                AggregateCtor::Tuple => verify_tuple_ctor(cx, site.clone(), *ty, fields),
            }
            for field in fields {
                verify_operand(cx, function_id, block_id, stmt_index, &field.value);
            }
        }
        RValue::Call { callee, args } => {
            verify_call(cx, function_id, block_id, stmt_index, callee, args);
        }
        RValue::StringConcat { parts } => {
            required_rvalue_primitive(cx, site.clone(), PrimitiveKind::String);
            for part in parts {
                verify_operand(cx, function_id, block_id, stmt_index, part);
                if let Some(ty) = typing::operand_ty(cx.program, part)
                    && !cx.primitives.is_string(ty)
                {
                    cx.push(
                        site.clone(),
                        VerifyErrorKind::BadRValue(BadRValue::StringConcatPartMustBeString(ty)),
                    );
                }
            }
        }
        RValue::Format { value, .. } => {
            verify_operand(cx, function_id, block_id, stmt_index, value);
            required_rvalue_primitive(cx, site, PrimitiveKind::String);
        }
        RValue::Len { source } => {
            required_rvalue_primitive(cx, site.clone(), PrimitiveKind::Int);
            if let Some(source_ty) = verify_place(cx, function_id, block_id, stmt_index, source)
                && !typing::is_countable(cx.program, &cx.primitives, source_ty)
            {
                cx.push(
                    site,
                    VerifyErrorKind::BadFunction(BadFunction::LenSourceMustBeCountable(source_ty)),
                );
            }
        }
        RValue::ListPush { list, value } => {
            required_rvalue_primitive(cx, site.clone(), PrimitiveKind::Void);
            verify_place(cx, function_id, block_id, stmt_index, list);
            verify_mutable_place(cx, function_id, &site, list);
            verify_operand(cx, function_id, block_id, stmt_index, &value.value);
            if let Some(expected_elem) = typing::list_elem(cx.program, list.ty)
                && let Some(value_ty) = typing::operand_ty(cx.program, &value.value)
                && value_ty != expected_elem
            {
                cx.push(
                    site,
                    VerifyErrorKind::BadFunction(BadFunction::ListElementTypeMismatch {
                        expected: expected_elem,
                        found: value_ty,
                    }),
                );
            }
        }
        RValue::ListPop { list, ty } => {
            verify_place(cx, function_id, block_id, stmt_index, list);
            cx.verify_type_ref(site.clone(), *ty);
            if let Some(expected_elem) = typing::list_elem(cx.program, list.ty) {
                let valid = typing::optional_inner(cx.program, *ty) == Some(expected_elem);
                if !valid {
                    cx.push(
                        site,
                        VerifyErrorKind::BadFunction(
                            BadFunction::ListPopResultMustBeOptionalElement {
                                expected_elem,
                                found: *ty,
                            },
                        ),
                    );
                }
            }
        }
        RValue::RangeListCopy {
            source,
            start,
            end,
            ty,
            ..
        } => {
            verify_place(cx, function_id, block_id, stmt_index, source);
            cx.verify_type_ref(site.clone(), *ty);
            match collection_sequence_elem(cx.program, source.ty) {
                Some(expected_elem)
                    if typing::list_elem(cx.program, *ty) == Some(expected_elem) => {}
                Some(expected_elem) => cx.push(
                    site.clone(),
                    VerifyErrorKind::BadFunction(BadFunction::RangeListCopyResultMustBeList {
                        expected_elem,
                        found: *ty,
                    }),
                ),
                None => cx.push(
                    site.clone(),
                    VerifyErrorKind::BadFunction(BadFunction::RangeListCopySourceMustBeSequence(
                        source.ty,
                    )),
                ),
            }
            verify_slice_index(cx, function_id, block_id, stmt_idx, "start", *start);
            verify_slice_index(cx, function_id, block_id, stmt_idx, "end", *end);
        }
        RValue::MapGet { map, key, ty } => {
            verify_place(cx, function_id, block_id, stmt_index, map);
            verify_operand(cx, function_id, block_id, stmt_index, key);
            cx.verify_type_ref(site.clone(), *ty);
            if let Some((expected_key, expected_value)) = typing::map_kv(cx.program, map.ty) {
                verify_map_key(cx, &site, key, expected_key);
                verify_optional_map_value(cx, &site, *ty, expected_value);
            }
        }
        RValue::MapRemove { map, key, ty } => {
            verify_place(cx, function_id, block_id, stmt_index, map);
            verify_mutable_place(cx, function_id, &site, map);
            verify_operand(cx, function_id, block_id, stmt_index, key);
            cx.verify_type_ref(site.clone(), *ty);
            if let Some((expected_key, expected_value)) = typing::map_kv(cx.program, map.ty) {
                verify_map_key(cx, &site, key, expected_key);
                verify_optional_map_value(cx, &site, *ty, expected_value);
            }
        }
        RValue::MapInsert {
            map,
            key,
            value,
            kind: _,
        } => {
            required_rvalue_primitive(cx, site.clone(), PrimitiveKind::Void);
            verify_place(cx, function_id, block_id, stmt_index, map);
            verify_mutable_place(cx, function_id, &site, map);
            verify_operand(cx, function_id, block_id, stmt_index, &key.value);
            verify_operand(cx, function_id, block_id, stmt_index, &value.value);
            if let Some((expected_key, expected_value)) = typing::map_kv(cx.program, map.ty) {
                verify_map_key(cx, &site, &key.value, expected_key);
                verify_map_value(cx, &site, &value.value, expected_value);
            }
        }
        RValue::MapEntryAt { map, index, ty } => {
            verify_place(cx, function_id, block_id, stmt_index, map);
            verify_slice_index(cx, function_id, block_id, stmt_idx, "index", *index);
            cx.verify_type_ref(site.clone(), *ty);
            match typing::map_kv(cx.program, map.ty) {
                Some((key_ty, value_ty)) if matches!(cx.program.type_data(*ty), TypeData::Tuple(fields) if fields.as_slice() == [key_ty, value_ty]) =>
                    {}
                Some((_, value_ty)) => cx.push(
                    site,
                    VerifyErrorKind::BadFunction(BadFunction::MapEntryResultTypeMismatch {
                        expected: value_ty,
                        found: *ty,
                    }),
                ),
                None => cx.push(
                    site,
                    VerifyErrorKind::BadFunction(BadFunction::MapEntrySourceMustBeMap(map.ty)),
                ),
            }
        }
        RValue::MapKeyAt { map, index, ty } => {
            verify_place(cx, function_id, block_id, stmt_index, map);
            verify_slice_index(cx, function_id, block_id, stmt_idx, "index", *index);
            cx.verify_type_ref(site.clone(), *ty);
            match typing::map_kv(cx.program, map.ty) {
                Some((key_ty, _)) if *ty == key_ty => {}
                Some((_, value_ty)) => cx.push(
                    site,
                    VerifyErrorKind::BadFunction(BadFunction::MapEntryResultTypeMismatch {
                        expected: value_ty,
                        found: *ty,
                    }),
                ),
                None => cx.push(
                    site,
                    VerifyErrorKind::BadFunction(BadFunction::MapEntrySourceMustBeMap(map.ty)),
                ),
            }
        }
        RValue::MapValueAt { map, index, ty } => {
            verify_place(cx, function_id, block_id, stmt_index, map);
            verify_slice_index(cx, function_id, block_id, stmt_idx, "index", *index);
            cx.verify_type_ref(site.clone(), *ty);
            match typing::map_kv(cx.program, map.ty) {
                Some((_, value_ty)) if *ty == value_ty => {}
                Some((_, value_ty)) => cx.push(
                    site,
                    VerifyErrorKind::BadFunction(BadFunction::MapEntryResultTypeMismatch {
                        expected: value_ty,
                        found: *ty,
                    }),
                ),
                None => cx.push(
                    site,
                    VerifyErrorKind::BadFunction(BadFunction::MapEntrySourceMustBeMap(map.ty)),
                ),
            }
        }
        RValue::SliceView {
            source,
            start,
            end,
            ty,
            inclusive: _,
        } => {
            verify_place(cx, function_id, block_id, stmt_index, source);
            verify_slice_index(cx, function_id, block_id, stmt_idx, "start", *start);
            verify_slice_index(cx, function_id, block_id, stmt_idx, "end", *end);
            cx.verify_type_ref(site.clone(), *ty);
            match collection_sequence_elem(cx.program, source.ty) {
                Some(expected_elem) if matches!(cx.program.type_data(*ty), TypeData::Slice(elem) if *elem == expected_elem) =>
                    {}
                Some(expected_elem) => cx.push(
                    site,
                    VerifyErrorKind::BadFunction(BadFunction::SliceViewResultMustBeSlice {
                        expected_elem,
                        found: *ty,
                    }),
                ),
                None => cx.push(
                    site,
                    VerifyErrorKind::BadFunction(BadFunction::SliceViewSourceMustBeSequence(
                        source.ty,
                    )),
                ),
            }
        }
        RValue::CheckedIterCount { count, .. } => {
            verify_operand(cx, function_id, block_id, stmt_index, count);
            required_rvalue_primitive(cx, site.clone(), PrimitiveKind::Int);
            if let Some(found) = typing::operand_ty(cx.program, count)
                && cx.primitives.int() != Some(found)
            {
                let expected = cx.primitives.int().unwrap_or(found);
                cx.push(
                    site.clone(),
                    VerifyErrorKind::BadStatement(BadStatement::InitTypeMismatch {
                        expected,
                        found,
                    }),
                );
            }
        }
        RValue::FunctionRef { function, ty } => {
            match cx.program.functions.get(function.index()) {
                Some(function_decl)
                    if matches!(
                        function_decl.kind,
                        FunctionKind::Lambda(_) | FunctionKind::GlobalInit(_)
                    ) =>
                {
                    cx.push(
                        site.clone(),
                        VerifyErrorKind::BadRValue(BadRValue::FunctionRefMustBeNamed(*function)),
                    );
                }
                Some(_) => {}
                None => cx.push(
                    site.clone(),
                    VerifyErrorKind::BadReference(BadReference::InvalidFunction(*function)),
                ),
            }
            verify_function_value_type(cx, site, *function, *ty);
        }
        RValue::MakeLambda {
            lambda,
            captures,
            ty,
        } => {
            if let Some(decl) = cx.program.lambdas.get(lambda.index()) {
                if decl.owner != function_id {
                    cx.push(
                        site.clone(),
                        VerifyErrorKind::BadRValue(BadRValue::MakeLambdaOwnerMismatch {
                            lambda: *lambda,
                            expected: decl.owner,
                            found: function_id,
                        }),
                    );
                }
                verify_lambda_value_type(cx, site.clone(), *lambda, *ty);
                verify_lambda_captures(cx, &site, function_id, *lambda, captures);
            } else {
                cx.push(
                    site.clone(),
                    VerifyErrorKind::BadReference(BadReference::InvalidLambda(*lambda)),
                );
            }
            for cap in captures {
                verify_lambda_capture(cx, function_id, block_id, stmt_index, cap);
            }
            cx.verify_type_ref(site, *ty);
        }
    }
}

fn verify_lambda_captures(
    cx: &mut VerifyCx<'_>,
    site: &VerifySite,
    function_id: FunctionId,
    lambda: LambdaId,
    captures: &[LambdaCaptureArg],
) {
    let Some(decl) = cx.program.lambdas.get(lambda.index()) else {
        return;
    };
    if captures.len() != decl.captures.len() {
        cx.push(
            site.clone(),
            VerifyErrorKind::BadRValue(BadRValue::LambdaCaptureMismatch {
                index: captures.len().min(decl.captures.len()),
            }),
        );
    }
    for (index, (decl_capture, capture)) in decl.captures.iter().zip(captures).enumerate() {
        if !lambda_capture_matches(cx.program, function_id, decl_capture, capture) {
            cx.push(
                site.clone(),
                VerifyErrorKind::BadRValue(BadRValue::LambdaCaptureMismatch { index }),
            );
        }
        if let LambdaCaptureArg::ReadonlyLocal { value } = capture
            && !readonly_capture_arg_is_valid(cx.program, function_id, decl_capture, &value.value)
        {
            cx.push(
                site.clone(),
                VerifyErrorKind::BadRValue(BadRValue::ReadonlyCaptureMustBeImmutableOwned {
                    index,
                }),
            );
        }
        if decl_capture.mutability() == Mutability::Mutable
            && let Some(place) = lambda_capture_arg_place(capture)
        {
            verify_mutable_place(cx, function_id, site, place);
        }
    }
}

fn readonly_capture_arg_is_valid(
    program: &Program,
    function_id: FunctionId,
    decl: &LambdaCaptureDecl,
    value: &Operand,
) -> bool {
    if readonly_capture_is_immutable_owned(program, function_id, value, None) {
        return true;
    }
    let LambdaCaptureDecl::ReadonlyLocal {
        binding,
        source,
        ty,
    } = decl
    else {
        return false;
    };
    operand_is_matching_lambda_capture(program, function_id, value, *binding, *source, *ty)
}

fn readonly_capture_is_immutable_owned(
    program: &Program,
    function_id: FunctionId,
    value: &Operand,
    expected_source: Option<CaptureLocalSource>,
) -> bool {
    let Operand::Place(place) = value else {
        return false;
    };
    if !place.projection.is_empty() {
        return false;
    }
    let PlaceRoot::Local(local) = place.root else {
        return false;
    };
    if expected_source.is_some_and(|source| source.owner != function_id || source.local != local) {
        return false;
    }
    let Some(function) = program.functions.get(function_id.index()) else {
        return false;
    };
    let Some(decl) = function.locals.get(local.index()) else {
        return false;
    };
    readonly_local_source_is_valid(function, local, decl)
}

fn place_is_exact_local(place: &Place, source: CaptureLocalSource, ty: TypeId) -> bool {
    place.root == PlaceRoot::Local(source.local) && place.projection.is_empty() && place.ty == ty
}

fn place_is_exact_scoped_borrow(
    program: &Program,
    function_id: FunctionId,
    place: &Place,
    borrow: ScopedBorrowId,
    ty: TypeId,
) -> bool {
    place.projection.is_empty()
        && place.ty == ty
        && program.scoped_borrow_root(function_id, place.root) == Some(borrow)
}

fn lambda_capture_arg_place(capture: &LambdaCaptureArg) -> Option<&Place> {
    match capture {
        LambdaCaptureArg::ScopedLocal { place } | LambdaCaptureArg::ScopedBorrow { place } => {
            Some(place)
        }
        LambdaCaptureArg::NoRuntime
        | LambdaCaptureArg::ReadonlyLocal { .. }
        | LambdaCaptureArg::CaptureCell { .. } => None,
    }
}

fn lambda_capture_matches(
    program: &Program,
    function_id: FunctionId,
    decl: &LambdaCaptureDecl,
    capture: &LambdaCaptureArg,
) -> bool {
    match (decl, capture) {
        (LambdaCaptureDecl::NoRuntime { .. }, LambdaCaptureArg::NoRuntime) => true,
        (
            LambdaCaptureDecl::ReadonlyLocal {
                binding,
                source,
                ty,
            },
            LambdaCaptureArg::ReadonlyLocal { value },
        ) => {
            typing::operand_ty(program, &value.value) == Some(*ty)
                && (source.owner == function_id
                    && readonly_capture_is_immutable_owned(
                        program,
                        function_id,
                        &value.value,
                        Some(*source),
                    )
                    || operand_is_matching_lambda_capture(
                        program,
                        function_id,
                        &value.value,
                        *binding,
                        *source,
                        *ty,
                    ))
        }
        (
            LambdaCaptureDecl::ScopedLocal {
                binding,
                source,
                ty,
                ..
            },
            LambdaCaptureArg::ScopedLocal { place },
        ) => {
            source.owner == function_id && place_is_exact_local(place, *source, *ty)
                || place_is_matching_lambda_capture(
                    program,
                    function_id,
                    place,
                    *binding,
                    *source,
                    *ty,
                )
        }
        (
            LambdaCaptureDecl::ScopedBorrow { borrow, ty, .. },
            LambdaCaptureArg::ScopedBorrow { place },
        ) => place_is_exact_scoped_borrow(program, function_id, place, *borrow, *ty),
        (
            LambdaCaptureDecl::CaptureCell { cell: expected, .. },
            LambdaCaptureArg::CaptureCell { cell },
        ) => cell == expected,
        _ => false,
    }
}

fn operand_is_matching_lambda_capture(
    program: &Program,
    function_id: FunctionId,
    operand: &Operand,
    binding: BindingId,
    source: CaptureLocalSource,
    ty: TypeId,
) -> bool {
    matches!(operand, Operand::Place(place)
        if place_is_matching_lambda_capture(program, function_id, place, binding, source, ty))
}

fn place_is_matching_lambda_capture(
    program: &Program,
    function_id: FunctionId,
    place: &Place,
    binding: BindingId,
    source: CaptureLocalSource,
    ty: TypeId,
) -> bool {
    if !place.projection.is_empty() || place.ty != ty {
        return false;
    }
    let PlaceRoot::LambdaCapture(slot) = place.root else {
        return false;
    };
    let Some(function) = program.functions.get(function_id.index()) else {
        return false;
    };
    let FunctionKind::Lambda(lambda) = function.kind else {
        return false;
    };
    let Some(capture) = program.lambdas[lambda.index()].captures.get(slot.index()) else {
        return false;
    };
    lambda_capture_matches_local_source(capture, binding, source, ty)
}

fn verify_function_value_type(
    cx: &mut VerifyCx<'_>,
    site: VerifySite,
    function: FunctionId,
    ty: TypeId,
) {
    let expected = cx
        .program
        .functions
        .get(function.index())
        .map(function_signature_type);
    verify_function_type(cx, site, expected.as_ref(), ty);
}

fn verify_lambda_value_type(cx: &mut VerifyCx<'_>, site: VerifySite, lambda: LambdaId, ty: TypeId) {
    let expected = cx
        .program
        .lambdas
        .get(lambda.index())
        .map(|decl| &decl.signature);
    verify_function_type(cx, site, expected, ty);
}

fn verify_function_value_operand_type(cx: &mut VerifyCx<'_>, site: VerifySite, value: &Operand) {
    let Some(ty) = typing::operand_ty(cx.program, value) else {
        return;
    };
    if !matches!(cx.type_data(ty), Some(TypeData::Function(_))) {
        cx.push(
            site,
            VerifyErrorKind::BadRValue(BadRValue::FunctionValueMustBeFunction(ty)),
        );
    }
}

fn verify_function_type(
    cx: &mut VerifyCx<'_>,
    site: VerifySite,
    expected: Option<&SignatureType>,
    ty: TypeId,
) {
    cx.verify_type_ref(site.clone(), ty);
    let Some(found) = cx.type_data(ty) else {
        return;
    };
    let TypeData::Function(found) = found else {
        cx.push(
            site,
            VerifyErrorKind::BadRValue(BadRValue::FunctionValueMustBeFunction(ty)),
        );
        return;
    };
    if let Some(expected) = expected
        && found != expected
    {
        cx.push(
            site,
            VerifyErrorKind::BadRValue(BadRValue::FunctionValueTypeMismatch {
                expected: expected.clone(),
                found: ty,
            }),
        );
    }
}

fn function_signature_type(function: &Function) -> SignatureType {
    SignatureType {
        params: function
            .signature
            .params
            .iter()
            .map(Param::param_type)
            .collect(),
        ret: function.signature.return_mode,
    }
}

fn verify_mutable_place(
    cx: &mut VerifyCx<'_>,
    function_id: FunctionId,
    site: &VerifySite,
    place: &Place,
) {
    if place_model::place_crosses_dataref(cx.program, function_id, place) {
        return;
    }
    if place_mutability(cx.program, function_id, place.root) == Some(Mutability::Immutable) {
        cx.push(
            site.clone(),
            VerifyErrorKind::BadPlace(BadPlace::ImmutableRoot(place.root)),
        );
    }
}

fn verify_map_key(cx: &mut VerifyCx<'_>, site: &VerifySite, key: &Operand, expected: TypeId) {
    if let Some(found) = typing::operand_ty(cx.program, key)
        && found != expected
    {
        cx.push(
            site.clone(),
            VerifyErrorKind::BadFunction(BadFunction::MapKeyTypeMismatch { expected, found }),
        );
    }
}

fn same_type(cx: &VerifyCx<'_>, lhs: TypeId, rhs: TypeId) -> bool {
    lhs == rhs
        || matches!(
            (cx.type_data(lhs), cx.type_data(rhs)),
            (Some(TypeData::Int), Some(TypeData::Int))
                | (Some(TypeData::Float), Some(TypeData::Float))
                | (Some(TypeData::Bool), Some(TypeData::Bool))
                | (Some(TypeData::String), Some(TypeData::String))
                | (Some(TypeData::Void), Some(TypeData::Void))
        )
        || match (
            typing::optional_inner(cx.program, lhs),
            typing::optional_inner(cx.program, rhs),
        ) {
            (Some(lhs), Some(rhs)) => same_type(cx, lhs, rhs),
            _ => false,
        }
}

fn verify_map_value(cx: &mut VerifyCx<'_>, site: &VerifySite, value: &Operand, expected: TypeId) {
    if let Some(found) = typing::operand_ty(cx.program, value)
        && found != expected
    {
        cx.push(
            site.clone(),
            VerifyErrorKind::BadFunction(BadFunction::MapValueTypeMismatch { expected, found }),
        );
    }
}

fn verify_optional_map_value(
    cx: &mut VerifyCx<'_>,
    site: &VerifySite,
    found: TypeId,
    expected_value: TypeId,
) {
    let valid = typing::optional_inner(cx.program, found)
        .is_some_and(|inner| same_type(cx, inner, expected_value));
    if !valid {
        cx.push(
            site.clone(),
            VerifyErrorKind::BadFunction(BadFunction::MapGetResultMustBeOptionalValue {
                expected_value,
                found,
            }),
        );
    }
}

fn verify_lambda_capture(
    cx: &mut VerifyCx<'_>,
    function_id: FunctionId,
    block_id: BlockId,
    stmt_index: Option<usize>,
    capture: &LambdaCaptureArg,
) {
    match capture {
        LambdaCaptureArg::NoRuntime => {}
        LambdaCaptureArg::ReadonlyLocal { value } => {
            verify_operand(cx, function_id, block_id, stmt_index, &value.value);
        }
        LambdaCaptureArg::ScopedLocal { place } | LambdaCaptureArg::ScopedBorrow { place } => {
            verify_place(cx, function_id, block_id, stmt_index, place);
        }
        LambdaCaptureArg::CaptureCell { cell } => {
            let site = VerifyCx::stmt_site(function_id, block_id, stmt_index.unwrap_or(0));
            if !cx.has_capture_cell(*cell) {
                cx.push(
                    site,
                    VerifyErrorKind::BadReference(BadReference::InvalidCaptureCell(*cell)),
                );
            } else if !function_can_access_capture_cell(cx.program, function_id, *cell) {
                cx.push(
                    site,
                    VerifyErrorKind::BadPlace(BadPlace::CaptureCellNotAccessible {
                        cell: *cell,
                        function: function_id,
                    }),
                );
            }
        }
    }
}

fn verify_operand(
    cx: &mut VerifyCx<'_>,
    function_id: FunctionId,
    block_id: BlockId,
    stmt_index: Option<usize>,
    op: &Operand,
) {
    let site = VerifyCx::stmt_site(function_id, block_id, stmt_index.unwrap_or(0));
    match op {
        Operand::Place(place) => {
            verify_place(cx, function_id, block_id, stmt_index, place);
        }
        Operand::Const(id) => {
            if !cx.has_const(*id) {
                cx.push(
                    site,
                    VerifyErrorKind::BadReference(BadReference::InvalidConst(*id)),
                );
            }
        }
    }
}

fn verify_place_root(
    cx: &mut VerifyCx<'_>,
    function_id: FunctionId,
    site: &VerifySite,
    root: PlaceRoot,
) -> Option<TypeId> {
    match root {
        PlaceRoot::Local(local) => match cx.program.function(function_id).locals.get(local.index())
        {
            Some(decl) => Some(decl.ty),
            None => {
                cx.push(
                    site.clone(),
                    VerifyErrorKind::BadReference(BadReference::InvalidLocal(local)),
                );
                None
            }
        },
        PlaceRoot::LambdaCapture(slot) => {
            let Some(lambda) = lambda_function(cx.program, function_id) else {
                cx.push(
                    site.clone(),
                    VerifyErrorKind::BadPlace(BadPlace::UnsupportedRoot(root)),
                );
                return None;
            };
            match cx.program.lambdas.get(lambda.index()).and_then(|decl| {
                decl.captures
                    .get(slot.index())
                    .map(|capture| (decl, capture))
            }) {
                Some((_, LambdaCaptureDecl::NoRuntime { .. })) => {
                    cx.push(
                        site.clone(),
                        VerifyErrorKind::BadPlace(BadPlace::NoRuntimeLambdaCaptureRoot(slot)),
                    );
                    None
                }
                Some((decl, capture)) => {
                    if decl.escape == LambdaEscape::Escaping
                        && matches!(
                            capture,
                            LambdaCaptureDecl::ScopedLocal { .. }
                                | LambdaCaptureDecl::ScopedBorrow { .. }
                        )
                    {
                        cx.push(
                            site.clone(),
                            VerifyErrorKind::BadFunction(
                                BadFunction::EscapingLambdaScopedCapture { lambda },
                            ),
                        );
                    }
                    Some(capture.ty())
                }
                None => {
                    cx.push(
                        site.clone(),
                        VerifyErrorKind::BadReference(BadReference::InvalidLambdaCaptureSlot(slot)),
                    );
                    None
                }
            }
        }
        PlaceRoot::ScopedBorrow(id) => {
            if !cx.has_scoped_borrow(id) {
                cx.push(
                    site.clone(),
                    VerifyErrorKind::BadReference(BadReference::InvalidScopedBorrow(id)),
                );
                return None;
            }
            if let Some(lambda) = lambda_function(cx.program, function_id)
                && lambda_captures_scoped_borrow(cx.program, lambda, id)
            {
                cx.push(
                    site.clone(),
                    VerifyErrorKind::BadPlace(BadPlace::RawScopedBorrowCaptureBypass {
                        lambda,
                        root: id,
                    }),
                );
                if cx
                    .program
                    .lambdas
                    .get(lambda.index())
                    .is_some_and(|decl| decl.escape == LambdaEscape::Escaping)
                {
                    cx.push(
                        site.clone(),
                        VerifyErrorKind::BadPlace(BadPlace::EscapingLambdaScopedBorrowRoot {
                            lambda,
                            root: id,
                        }),
                    );
                }
            }
            if !function_can_access_scoped_borrow(cx.program, function_id, id) {
                cx.push(
                    site.clone(),
                    VerifyErrorKind::BadPlace(BadPlace::ScopedBorrowNotAccessible {
                        borrow: id,
                        function: function_id,
                    }),
                );
            }
            Some(cx.program.scoped_borrows[id.index()].ty)
        }
        PlaceRoot::DynBorrowParam(id) => {
            let Some(decl) = cx.program.dyn_borrow_params.get(id.index()) else {
                cx.push(
                    site.clone(),
                    VerifyErrorKind::BadPlace(BadPlace::InvalidDynBorrowParam(id)),
                );
                return None;
            };
            let valid = decl.owner == function_id
                && cx
                    .program
                    .function(function_id)
                    .signature
                    .params
                    .iter()
                    .any(|param| {
                        param.local_id == decl.source
                            && param.mode == ParamMode::MutBorrow
                            && param.ty == decl.ty
                    })
                && matches!(cx.program.type_arena.get(decl.ty), Some(TypeData::Dyn(surface)) if *surface == decl.surface);
            if !valid {
                cx.push(
                    site.clone(),
                    VerifyErrorKind::BadPlace(BadPlace::InvalidDynBorrowParam(id)),
                );
                return None;
            }
            Some(decl.ty)
        }
        PlaceRoot::CaptureCell(id) => {
            if !cx.has_capture_cell(id) {
                cx.push(
                    site.clone(),
                    VerifyErrorKind::BadReference(BadReference::InvalidCaptureCell(id)),
                );
                return None;
            }
            if let Some(lambda) = lambda_function(cx.program, function_id)
                && lambda_captures_capture_cell(cx.program, lambda, id)
            {
                cx.push(
                    site.clone(),
                    VerifyErrorKind::BadPlace(BadPlace::RawCaptureCellCaptureBypass {
                        lambda,
                        root: id,
                    }),
                );
            }
            if !function_can_access_capture_cell(cx.program, function_id, id) {
                cx.push(
                    site.clone(),
                    VerifyErrorKind::BadPlace(BadPlace::CaptureCellNotAccessible {
                        cell: id,
                        function: function_id,
                    }),
                );
            }
            Some(cx.program.capture_cells[id.index()].ty)
        }
        PlaceRoot::Global(id) => {
            if cx.has_global(id) {
                Some(cx.program.globals[id.index()].ty)
            } else {
                cx.push(
                    site.clone(),
                    VerifyErrorKind::BadReference(BadReference::InvalidGlobal(id)),
                );
                None
            }
        }
    }
}

fn verify_promoted_binding_not_bypassed(
    cx: &mut VerifyCx<'_>,
    function_id: FunctionId,
    site: &VerifySite,
    root: PlaceRoot,
) {
    if let PlaceRoot::Local(local) = root {
        verify_promoted_local_not_used(cx, function_id, site, local);
    }
}

fn lambda_function(program: &Program, function_id: FunctionId) -> Option<LambdaId> {
    let FunctionKind::Lambda(lambda) = program.functions.get(function_id.index())?.kind else {
        return None;
    };
    Some(lambda)
}

fn function_can_access_capture_cell(
    program: &Program,
    function_id: FunctionId,
    cell: CaptureCellId,
) -> bool {
    if program
        .capture_cells
        .get(cell.index())
        .is_some_and(|decl| decl.owner == function_id)
    {
        return true;
    }
    let Some(FunctionKind::Lambda(lambda)) =
        program.functions.get(function_id.index()).map(|f| f.kind)
    else {
        return false;
    };
    lambda_captures_capture_cell(program, lambda, cell)
}

fn lambda_captures_capture_cell(program: &Program, lambda: LambdaId, cell: CaptureCellId) -> bool {
    program
        .lambdas
        .get(lambda.index())
        .is_some_and(|decl| decl.captures.iter().any(|capture| {
            matches!(capture, LambdaCaptureDecl::CaptureCell { cell: captured, .. } if *captured == cell)
        }))
}

fn function_can_access_scoped_borrow(
    program: &Program,
    function_id: FunctionId,
    borrow: ScopedBorrowId,
) -> bool {
    if program
        .scoped_borrows
        .get(borrow.index())
        .is_some_and(|decl| decl.owner == function_id)
    {
        return true;
    }
    let Some(FunctionKind::Lambda(lambda)) =
        program.functions.get(function_id.index()).map(|f| f.kind)
    else {
        return false;
    };
    lambda_captures_scoped_borrow(program, lambda, borrow)
}

fn lambda_captures_scoped_borrow(
    program: &Program,
    lambda: LambdaId,
    borrow: ScopedBorrowId,
) -> bool {
    program
        .lambdas
        .get(lambda.index())
        .is_some_and(|decl| decl.captures.iter().any(|capture| {
            matches!(capture, LambdaCaptureDecl::ScopedBorrow { borrow: captured, .. } if *captured == borrow)
        }))
}

fn place_mutability(
    program: &Program,
    function_id: FunctionId,
    root: PlaceRoot,
) -> Option<Mutability> {
    place_model::root_info(program, function_id, root).map(|root| root.mutability)
}

fn verify_place(
    cx: &mut VerifyCx<'_>,
    function_id: FunctionId,
    block_id: BlockId,
    stmt_index: Option<usize>,
    place: &Place,
) -> Option<TypeId> {
    let stmt_idx = stmt_index.unwrap_or(0);
    let site = VerifyCx::stmt_site(function_id, block_id, stmt_idx);
    let mut current_ty = verify_place_root(cx, function_id, &site, place.root)?;
    verify_promoted_binding_not_bypassed(cx, function_id, &site, place.root);
    if !cx.has_type(current_ty) {
        cx.push(
            site.clone(),
            VerifyErrorKind::BadReference(BadReference::InvalidType(current_ty)),
        );
        return None;
    }
    if !cx.has_type(place.ty) {
        cx.push(
            site.clone(),
            VerifyErrorKind::BadReference(BadReference::InvalidType(place.ty)),
        );
        return None;
    }
    for proj in &place.projection {
        if let Some(step) = place_model::project_step(cx.program, function_id, current_ty, proj) {
            current_ty = step.ty();
            continue;
        }
        let Some(data) = cx.type_data(current_ty) else {
            cx.push(
                site.clone(),
                VerifyErrorKind::BadReference(BadReference::InvalidType(current_ty)),
            );
            return None;
        };
        match proj {
            Projection::Field(field_id) => {
                if let Some(ty) = typing::field_by_id(cx.program, current_ty, *field_id) {
                    current_ty = ty;
                    continue;
                }
                match data {
                    TypeData::Aggregate(agg_id) | TypeData::DataRef(agg_id) => {
                        let Some(agg) = cx.program.aggregates.get(agg_id.index()) else {
                            cx.push(
                                site.clone(),
                                VerifyErrorKind::BadReference(BadReference::InvalidAggregate(
                                    *agg_id,
                                )),
                            );
                            return None;
                        };
                        let expected = match data {
                            TypeData::Aggregate(_) => AggregateKind::Struct,
                            TypeData::DataRef(_) => AggregateKind::DataRef,
                            _ => unreachable!(),
                        };
                        if agg.kind != expected {
                            cx.push(
                                site.clone(),
                                VerifyErrorKind::BadPlace(BadPlace::FieldProjectionKindMismatch {
                                    aggregate: *agg_id,
                                    expected,
                                    found: agg.kind,
                                }),
                            );
                            return None;
                        }
                        if agg.fields.get(field_id.index()).is_none() {
                            cx.push(
                                site.clone(),
                                VerifyErrorKind::BadReference(BadReference::InvalidField {
                                    aggregate: *agg_id,
                                    field: *field_id,
                                }),
                            );
                            return None;
                        }
                    }
                    _ => {
                        cx.push(
                            site.clone(),
                            VerifyErrorKind::BadPlace(BadPlace::FieldProjectionOnNonAggregate(
                                current_ty,
                            )),
                        );
                        return None;
                    }
                }
            }
            Projection::TupleField(index) => match data {
                TypeData::Tuple(elems) => {
                    let Some(ty) = typing::tuple_field(cx.program, current_ty, *index) else {
                        cx.push(
                            site.clone(),
                            VerifyErrorKind::BadPlace(BadPlace::TupleFieldOutOfRange {
                                ty: current_ty,
                                index: *index,
                                len: elems.len(),
                            }),
                        );
                        return None;
                    };
                    current_ty = ty;
                }
                _ => {
                    cx.push(
                        site.clone(),
                        VerifyErrorKind::BadPlace(BadPlace::TupleProjectionOnNonTuple(current_ty)),
                    );
                    return None;
                }
            },
            Projection::Index(local) => {
                let Some(index_local) = cx.program.function(function_id).locals.get(local.index())
                else {
                    cx.push(
                        site.clone(),
                        VerifyErrorKind::BadReference(BadReference::InvalidLocal(*local)),
                    );
                    return None;
                };
                let Some(elem) = typing::index_elem(cx.program, current_ty) else {
                    cx.push(
                        site.clone(),
                        VerifyErrorKind::BadPlace(BadPlace::IndexProjectionOnNonIndexable(
                            current_ty,
                        )),
                    );
                    return None;
                };
                let Some(int_ty) = cx.primitives.int() else {
                    cx.push(
                        site.clone(),
                        VerifyErrorKind::BadFunction(BadFunction::IndexTypeUnavailable),
                    );
                    return None;
                };
                if index_local.ty != int_ty {
                    cx.push(
                        site.clone(),
                        VerifyErrorKind::BadPlace(BadPlace::IndexLocalTypeMismatch {
                            expected: int_ty,
                            found: index_local.ty,
                        }),
                    );
                    return None;
                }
                current_ty = elem;
            }
        }
    }

    if current_ty != place.ty {
        cx.push(
            site,
            VerifyErrorKind::BadPlace(BadPlace::PlaceTypeMismatch {
                expected: current_ty,
                found: place.ty,
            }),
        );
        return None;
    }

    Some(current_ty)
}

fn verify_array_ctor(
    cx: &mut VerifyCx<'_>,
    site: VerifySite,
    ty: TypeId,
    fields: &[OwnedValue<Operand>],
) {
    let Some((elem, len)) = typing::array_elem_len(cx.program, ty) else {
        push_collection_result_mismatch(cx, site, AggregateCtor::Array, ty);
        return;
    };
    if fields.len() != len {
        cx.push(
            site.clone(),
            VerifyErrorKind::BadRValue(BadRValue::CollectionCtorFieldCountMismatch {
                ctor: AggregateCtor::Array,
                expected: len,
                found: fields.len(),
            }),
        );
    }
    verify_collection_fields(cx, &site, &AggregateCtor::Array, elem, fields);
}

fn verify_list_ctor(
    cx: &mut VerifyCx<'_>,
    site: VerifySite,
    ty: TypeId,
    fields: &[OwnedValue<Operand>],
) {
    let Some(elem) = typing::list_elem(cx.program, ty) else {
        push_collection_result_mismatch(cx, site, AggregateCtor::List, ty);
        return;
    };
    verify_collection_fields(cx, &site, &AggregateCtor::List, elem, fields);
}

fn verify_map_ctor(
    cx: &mut VerifyCx<'_>,
    site: VerifySite,
    ty: TypeId,
    fields: &[OwnedValue<Operand>],
) {
    let Some((key, value)) = typing::map_kv(cx.program, ty) else {
        push_collection_result_mismatch(cx, site, AggregateCtor::Map, ty);
        return;
    };
    if !fields.len().is_multiple_of(2) {
        cx.push(
            site.clone(),
            VerifyErrorKind::BadRValue(BadRValue::CollectionCtorFieldCountMismatch {
                ctor: AggregateCtor::Map,
                expected: fields.len() + 1,
                found: fields.len(),
            }),
        );
    }
    for (index, entry) in fields.chunks_exact(2).enumerate() {
        let base = index * 2;
        for (offset, expected) in [(0, key), (1, value)] {
            verify_collection_field(
                cx,
                &site,
                &AggregateCtor::Map,
                base + offset,
                &entry[offset].value,
                expected,
            );
        }
    }
}

fn verify_tuple_ctor(
    cx: &mut VerifyCx<'_>,
    site: VerifySite,
    ty: TypeId,
    fields: &[OwnedValue<Operand>],
) {
    let Some(TypeData::Tuple(elems)) = cx.type_data(ty).cloned() else {
        push_collection_result_mismatch(cx, site, AggregateCtor::Tuple, ty);
        return;
    };
    if fields.len() != elems.len() {
        cx.push(
            site.clone(),
            VerifyErrorKind::BadRValue(BadRValue::CollectionCtorFieldCountMismatch {
                ctor: AggregateCtor::Tuple,
                expected: elems.len(),
                found: fields.len(),
            }),
        );
    }
    for (field, (operand, expected)) in fields.iter().zip(elems).enumerate() {
        verify_collection_field(
            cx,
            &site,
            &AggregateCtor::Tuple,
            field,
            &operand.value,
            expected,
        );
    }
}

fn verify_collection_fields(
    cx: &mut VerifyCx<'_>,
    site: &VerifySite,
    ctor: &AggregateCtor,
    expected: TypeId,
    fields: &[OwnedValue<Operand>],
) {
    for (field, operand) in fields.iter().enumerate() {
        verify_collection_field(cx, site, ctor, field, &operand.value, expected);
    }
}

fn push_collection_result_mismatch(
    cx: &mut VerifyCx<'_>,
    site: VerifySite,
    ctor: AggregateCtor,
    found: TypeId,
) {
    cx.push(
        site,
        VerifyErrorKind::BadRValue(BadRValue::CollectionCtorResultTypeMismatch { ctor, found }),
    );
}

fn verify_collection_field(
    cx: &mut VerifyCx<'_>,
    site: &VerifySite,
    ctor: &AggregateCtor,
    field: usize,
    operand: &Operand,
    expected: TypeId,
) {
    if let Some(found) = typing::operand_ty(cx.program, operand)
        && found != expected
    {
        cx.push(
            site.clone(),
            VerifyErrorKind::BadRValue(BadRValue::CollectionCtorFieldTypeMismatch {
                ctor: ctor.clone(),
                field,
                expected,
                found,
            }),
        );
    }
}

fn verify_ordered_fields(
    cx: &mut VerifyCx<'_>,
    site: &VerifySite,
    fields: &[OwnedValue<Operand>],
    expected: &[TypeId],
    count_mismatch: impl FnOnce(usize, usize) -> VerifyErrorKind,
    mut type_mismatch: impl FnMut(usize, TypeId, TypeId) -> VerifyErrorKind,
) {
    if fields.len() != expected.len() {
        cx.push(site.clone(), count_mismatch(expected.len(), fields.len()));
    }
    for (index, (operand, expected)) in fields.iter().zip(expected).enumerate() {
        if let Some(found) = typing::operand_ty(cx.program, &operand.value)
            && found != *expected
        {
            cx.push(site.clone(), type_mismatch(index, *expected, found));
        }
    }
}

fn verify_aggregate_ctor(
    cx: &mut VerifyCx<'_>,
    site: VerifySite,
    aggregate_id: AggregateId,
    expected_kind: AggregateKind,
    ty: TypeId,
    fields: &[OwnedValue<Operand>],
) {
    let Some(aggregate) = cx.program.aggregates.get(aggregate_id.index()) else {
        cx.push(
            site,
            VerifyErrorKind::BadReference(BadReference::InvalidAggregate(aggregate_id)),
        );
        return;
    };

    let expected_ty = match expected_kind {
        AggregateKind::Struct => TypeData::Aggregate(aggregate_id),
        AggregateKind::DataRef => TypeData::DataRef(aggregate_id),
    };
    if cx.type_data(ty) != Some(&expected_ty) {
        cx.push(
            site.clone(),
            VerifyErrorKind::BadRValue(BadRValue::AggregateCtorResultTypeMismatch {
                aggregate: aggregate_id,
                expected: expected_kind,
                found: ty,
            }),
        );
    }
    if aggregate.kind != expected_kind {
        cx.push(
            site.clone(),
            VerifyErrorKind::BadRValue(BadRValue::AggregateCtorKindMismatch {
                aggregate: aggregate_id,
                expected: expected_kind,
                found: aggregate.kind,
            }),
        );
    }
    let expected_fields = aggregate
        .fields
        .iter()
        .map(|field| field.ty)
        .collect::<Vec<_>>();
    verify_ordered_fields(
        cx,
        &site,
        fields,
        &expected_fields,
        |expected, found| {
            VerifyErrorKind::BadRValue(BadRValue::AggregateCtorFieldCountMismatch {
                aggregate: aggregate_id,
                expected,
                found,
            })
        },
        |field, expected, found| {
            VerifyErrorKind::BadRValue(BadRValue::AggregateCtorFieldTypeMismatch {
                aggregate: aggregate_id,
                field,
                expected,
                found,
            })
        },
    );
}

fn verify_enum_ctor(
    cx: &mut VerifyCx<'_>,
    site: VerifySite,
    enum_id: EnumId,
    variant: VariantId,
    ty: TypeId,
    fields: &[OwnedValue<Operand>],
) {
    let Some(enm) = cx.program.enums.get(enum_id.index()) else {
        cx.push(
            site,
            VerifyErrorKind::BadReference(BadReference::InvalidEnum(enum_id)),
        );
        return;
    };
    let Some(variant_decl) = enm.variants.get(variant.index()) else {
        cx.push(
            site,
            VerifyErrorKind::BadReference(BadReference::InvalidVariant { enum_id, variant }),
        );
        return;
    };
    if cx.type_data(ty) != Some(&TypeData::Enum(enum_id)) {
        cx.push(
            site.clone(),
            VerifyErrorKind::BadRValue(BadRValue::EnumCtorResultTypeMismatch {
                enum_id,
                found: ty,
            }),
        );
    }
    let expected_len = typing::variant_field_count(&variant_decl.shape);
    let expected_fields = (0..expected_len)
        .map(|index| {
            typing::variant_field_ty(&variant_decl.shape, index)
                .expect("variant field count should match field type lookup")
        })
        .collect::<Vec<_>>();
    verify_ordered_fields(
        cx,
        &site,
        fields,
        &expected_fields,
        |expected, found| {
            VerifyErrorKind::BadRValue(BadRValue::EnumCtorFieldCountMismatch {
                enum_id,
                variant,
                expected,
                found,
            })
        },
        |field, expected, found| {
            VerifyErrorKind::BadRValue(BadRValue::EnumCtorFieldTypeMismatch {
                enum_id,
                variant,
                field,
                expected,
                found,
            })
        },
    );
}

fn verify_dyn_call(
    cx: &mut VerifyCx<'_>,
    function_id: FunctionId,
    block_id: BlockId,
    stmt_index: Option<usize>,
    receiver: &DynReceiver,
    surface: ContractSurfaceId,
    slot: ContractSlotId,
    args: &[CallArg],
) {
    let site = VerifyCx::stmt_site(function_id, block_id, stmt_index.unwrap_or(0));
    let Some(slot_decl) = cx
        .program
        .contract_surfaces
        .get(surface.index())
        .and_then(|surface| surface.slots.get(slot.index()))
        .cloned()
    else {
        cx.push(site, VerifyErrorKind::BadRValue(BadRValue::InvalidDynCall));
        return;
    };
    let receiver_valid = match receiver {
        DynReceiver::Owned(value) => {
            verify_operand(cx, function_id, block_id, stmt_index, &value.value);
            slot_decl.receiver != ContractReceiver::Ref
                && operand_dyn_surface(cx.program, &value.value) == Some(surface)
        }
        DynReceiver::MutableOwned(place) => {
            verify_place(cx, function_id, block_id, stmt_index, place);
            verify_mutable_place(cx, function_id, &site, place);
            slot_decl.receiver == ContractReceiver::Ref
                && place_dyn_surface(cx.program, place) == Some(surface)
        }
        DynReceiver::Borrowed(borrow) => {
            verify_dyn_borrow(cx, function_id, block_id, stmt_index, borrow)
                && borrow.surface == surface
        }
    };
    if !receiver_valid || args.len() != slot_decl.params.len() {
        cx.push(
            site.clone(),
            VerifyErrorKind::BadRValue(BadRValue::InvalidDynCall),
        );
    }
    for first in 0..args.len() {
        for second in first + 1..args.len() {
            if call_args_conflict(cx.program, function_id, &args[first], &args[second]) {
                cx.push(
                    site.clone(),
                    VerifyErrorKind::BadCall(BadCall::ArgAliasConflict { first, second }),
                );
            }
        }
    }
    let receiver_place = match receiver {
        DynReceiver::Owned(OwnedValue {
            value: Operand::Place(place),
            ..
        }) => Some((ParamMode::SharedBorrow, place)),
        DynReceiver::MutableOwned(place) => Some((ParamMode::MutBorrow, place)),
        DynReceiver::Borrowed(borrow) => Some((ParamMode::MutBorrow, borrow.place())),
        DynReceiver::Owned(OwnedValue {
            value: Operand::Const(_),
            ..
        }) => None,
    };
    if let Some((receiver_mode, receiver_place)) = receiver_place {
        for (index, arg) in args.iter().enumerate() {
            if arg.place().is_some_and(|place| {
                call_places_conflict(
                    cx.program,
                    function_id,
                    receiver_mode,
                    receiver_place,
                    arg.mode(),
                    place,
                )
            }) {
                cx.push(
                    site.clone(),
                    VerifyErrorKind::BadCall(BadCall::ReceiverArgAliasConflict { arg: index }),
                );
            }
        }
    }
    for (index, arg) in args.iter().enumerate() {
        verify_call_arg(cx, function_id, block_id, stmt_index, index, arg);
        let valid = slot_decl.params.get(index).is_some_and(|param| {
            typing::call_arg_ty(cx.program, arg) == Some(param.ty) && arg.mode() == param.mode
        });
        if !valid {
            cx.push(
                site.clone(),
                VerifyErrorKind::BadRValue(BadRValue::InvalidDynCall),
            );
        }
    }
}

fn operand_dyn_surface(program: &Program, value: &Operand) -> Option<ContractSurfaceId> {
    let ty = typing::operand_ty(program, value)?;
    match program.type_arena.get(ty) {
        Some(TypeData::Dyn(surface)) => Some(*surface),
        _ => None,
    }
}

fn place_dyn_surface(program: &Program, place: &Place) -> Option<ContractSurfaceId> {
    match program.type_arena.get(place.ty) {
        Some(TypeData::Dyn(surface)) => Some(*surface),
        _ => None,
    }
}

fn verify_dyn_borrow(
    cx: &mut VerifyCx<'_>,
    function_id: FunctionId,
    block_id: BlockId,
    stmt_index: Option<usize>,
    borrow: &DynBorrow,
) -> bool {
    let site = VerifyCx::stmt_site(function_id, block_id, stmt_index.unwrap_or(0));
    cx.verify_type_ref(site.clone(), borrow.ty);
    let target_valid = matches!(cx.program.type_arena.get(borrow.ty), Some(TypeData::Dyn(surface)) if *surface == borrow.surface);
    let (source_surface, source_valid) = match &borrow.source {
        DynBorrowSource::Concrete { place, witness } => {
            verify_place(cx, function_id, block_id, stmt_index, place);
            verify_mutable_place(cx, function_id, &site, place);
            let valid = cx
                .program
                .contract_witnesses
                .get(witness.index())
                .is_some_and(|decl| decl.key.concrete_ty == place.ty);
            let surface = cx
                .program
                .contract_witnesses
                .get(witness.index())
                .map(|decl| decl.key.surface);
            (surface, valid)
        }
        DynBorrowSource::Owned(place) => {
            verify_place(cx, function_id, block_id, stmt_index, place);
            verify_mutable_place(cx, function_id, &site, place);
            (
                place_dyn_surface(cx.program, place),
                !matches!(place.root, PlaceRoot::DynBorrowParam(_))
                    && !is_dyn_borrow_backing_local(cx.program, function_id, place.root),
            )
        }
        DynBorrowSource::Borrowed(place) => {
            verify_place(cx, function_id, block_id, stmt_index, place);
            let mode_matches = match place.root {
                PlaceRoot::DynBorrowParam(id) => cx
                    .program
                    .dyn_borrow_params
                    .get(id.index())
                    .is_some_and(|decl| decl.owner == function_id),
                _ => false,
            };
            (place_dyn_surface(cx.program, place), mode_matches)
        }
    };
    let projection_valid = match borrow.weakening {
        Some(id) => cx
            .program
            .contract_weakenings
            .get(id.index())
            .is_some_and(|decl| {
                Some(decl.source) == source_surface && decl.target == borrow.surface
            }),
        None => source_surface == Some(borrow.surface),
    };
    let valid = target_valid && source_valid && projection_valid;
    if !valid {
        cx.push(
            site,
            VerifyErrorKind::BadRValue(BadRValue::InvalidDynBorrow),
        );
    }
    valid
}

fn verify_call(
    cx: &mut VerifyCx<'_>,
    function_id: FunctionId,
    block_id: BlockId,
    stmt_index: Option<usize>,
    callee: &Callee,
    args: &[CallArg],
) {
    let site = VerifyCx::stmt_site(function_id, block_id, stmt_index.unwrap_or(0));

    if let Callee::Lambda(op) = callee {
        verify_operand(cx, function_id, block_id, stmt_index, op);
    }
    for (arg_index, arg) in args.iter().enumerate() {
        verify_call_arg(cx, function_id, block_id, stmt_index, arg_index, arg);
    }

    match callee {
        Callee::Function(id) => match cx.program.functions.get(id.index()) {
            Some(function) if matches!(function.kind, FunctionKind::Lambda(_)) => {
                cx.push(
                    site,
                    VerifyErrorKind::BadCall(BadCall::FunctionCalleeMustBeNamed(*id)),
                );
                return;
            }
            Some(function) if matches!(function.kind, FunctionKind::GlobalInit(_)) => {
                cx.push(
                    site,
                    VerifyErrorKind::BadCall(BadCall::FunctionCalleeMustBeSourceCallable(*id)),
                );
                return;
            }
            Some(_) => {}
            None => {
                cx.push(
                    site,
                    VerifyErrorKind::BadReference(BadReference::InvalidFunction(*id)),
                );
                return;
            }
        },
        Callee::Extern(id) if !cx.has_extern(*id) => {
            cx.push(
                site,
                VerifyErrorKind::BadReference(BadReference::InvalidExtern(*id)),
            );
            return;
        }
        Callee::Lambda(op) => {
            if let Some(ty) = typing::operand_ty(cx.program, op)
                && !matches!(cx.type_data(ty), Some(TypeData::Function(_)))
            {
                cx.push(
                    site,
                    VerifyErrorKind::BadCall(BadCall::LambdaCalleeMustBeFunction),
                );
                return;
            }
        }
        Callee::Extern(_) => {}
    }

    verify_call_args(cx, function_id, &site, callee, args);
}

fn verify_call_arg(
    cx: &mut VerifyCx<'_>,
    function_id: FunctionId,
    block_id: BlockId,
    stmt_index: Option<usize>,
    arg_index: usize,
    arg: &CallArg,
) {
    match arg {
        CallArg::Value(owned) | CallArg::InitFieldProvided(owned) => {
            verify_operand(cx, function_id, block_id, stmt_index, &owned.value);
        }
        CallArg::InitFieldOmitted => {}
        CallArg::SharedBorrow(place) => {
            verify_place(cx, function_id, block_id, stmt_index, place);
        }
        CallArg::MutBorrow(place) => {
            let site = VerifyCx::stmt_site(function_id, block_id, stmt_index.unwrap_or(0));
            verify_place(cx, function_id, block_id, stmt_index, place);
            verify_mutable_place(cx, function_id, &site, place);
        }
        CallArg::DynBorrow(borrow) => {
            verify_dyn_borrow(cx, function_id, block_id, stmt_index, borrow);
        }
        CallArg::SharedStringConst(id) => {
            let site = VerifyCx::stmt_site(function_id, block_id, stmt_index.unwrap_or(0));
            let Some(konst) = cx.program.const_arena.get_checked(*id) else {
                cx.push(
                    site,
                    VerifyErrorKind::BadReference(BadReference::InvalidConst(*id)),
                );
                return;
            };
            if !typing::const_is_string(cx.program, &cx.primitives, *id) {
                cx.push(
                    site,
                    VerifyErrorKind::BadCall(BadCall::ArgTypeMismatch {
                        index: arg_index,
                        expected: konst.ty,
                        found: konst.ty,
                    }),
                );
            }
        }
    }
}

fn verify_dyn_call_escape_args(
    cx: &mut VerifyCx<'_>,
    function_id: FunctionId,
    stmt_index: usize,
    surface: ContractSurfaceId,
    slot: ContractSlotId,
    args: &[CallArg],
    state: &LocalInit,
) {
    let Some(params) = cx
        .program
        .contract_surfaces
        .get(surface.index())
        .and_then(|surface| surface.slots.get(slot.index()))
        .map(|slot| &slot.params)
    else {
        return;
    };
    for (index, (arg, param)) in args.iter().zip(params).enumerate() {
        if param.escape != ParamEscape::Escaping
            || !matches!(cx.type_data(param.ty), Some(TypeData::Function(_)))
        {
            continue;
        }
        match arg_function_escape(cx.program, function_id, arg, state) {
            FunctionValueCapability::Escaping => {}
            FunctionValueCapability::NonEscaping => cx.push(
                VerifyCx::stmt_site(function_id, BlockId::from_index(0), stmt_index),
                VerifyErrorKind::BadCall(BadCall::ArgEscapeMismatch {
                    index,
                    expected: ParamEscape::Escaping,
                    found: ParamEscape::NonEscaping,
                }),
            ),
            FunctionValueCapability::Unknown | FunctionValueCapability::NonFunction => cx.push(
                VerifyCx::stmt_site(function_id, BlockId::from_index(0), stmt_index),
                VerifyErrorKind::BadCall(BadCall::ArgEscapeUnknown {
                    index,
                    expected: ParamEscape::Escaping,
                }),
            ),
        }
    }
}

fn verify_call_escape_args(
    cx: &mut VerifyCx<'_>,
    function_id: FunctionId,
    stmt_index: usize,
    callee: &Callee,
    args: &[CallArg],
    state: &LocalInit,
) {
    let Some(params) = typing::callee_params(cx.program, callee) else {
        return;
    };
    for (index, arg) in args.iter().enumerate() {
        let Some(expected_param) = params.get(cx.program, index) else {
            continue;
        };
        if expected_param.escape != ParamEscape::Escaping
            || !matches!(cx.type_data(expected_param.ty), Some(TypeData::Function(_)))
        {
            continue;
        }
        match arg_function_escape(cx.program, function_id, arg, state) {
            FunctionValueCapability::Escaping => {}
            FunctionValueCapability::NonEscaping => cx.push(
                VerifyCx::stmt_site(function_id, BlockId::from_index(0), stmt_index),
                VerifyErrorKind::BadCall(BadCall::ArgEscapeMismatch {
                    index,
                    expected: ParamEscape::Escaping,
                    found: ParamEscape::NonEscaping,
                }),
            ),
            FunctionValueCapability::Unknown | FunctionValueCapability::NonFunction => cx.push(
                VerifyCx::stmt_site(function_id, BlockId::from_index(0), stmt_index),
                VerifyErrorKind::BadCall(BadCall::ArgEscapeUnknown {
                    index,
                    expected: ParamEscape::Escaping,
                }),
            ),
        }
    }
}

fn arg_function_escape(
    program: &Program,
    function_id: FunctionId,
    arg: &CallArg,
    state: &LocalInit,
) -> FunctionValueCapability {
    match arg {
        CallArg::Value(owned) | CallArg::InitFieldProvided(owned) => {
            operand_function_escape(program, function_id, &owned.value, state)
        }
        CallArg::SharedBorrow(place) | CallArg::MutBorrow(place)
            if matches!(program.type_arena.data(place.ty), TypeData::Function(_)) =>
        {
            operand_function_escape(program, function_id, &Operand::Place(place.clone()), state)
        }
        CallArg::SharedBorrow(place) | CallArg::MutBorrow(place) => {
            type_function_capability(program, place.ty)
        }
        CallArg::InitFieldOmitted | CallArg::SharedStringConst(_) | CallArg::DynBorrow(_) => {
            FunctionValueCapability::NonFunction
        }
    }
}

fn operand_function_escape(
    program: &Program,
    function_id: FunctionId,
    operand: &Operand,
    state: &LocalInit,
) -> FunctionValueCapability {
    match operand {
        Operand::Const(id) => program
            .const_arena
            .get_checked(*id)
            .map_or(FunctionValueCapability::Unknown, |konst| {
                type_function_capability(program, konst.ty)
            }),
        Operand::Place(place) if place.projection.is_empty() => match place.root {
            PlaceRoot::Local(local) => state.escape(local),
            PlaceRoot::LambdaCapture(slot) => {
                lambda_capture_function_escape(program, function_id, slot)
            }
            PlaceRoot::ScopedBorrow(_)
            | PlaceRoot::DynBorrowParam(_)
            | PlaceRoot::CaptureCell(_)
            | PlaceRoot::Global(_) => type_function_capability(program, place.ty),
        },
        Operand::Place(place) => type_function_capability(program, place.ty),
    }
}

fn lambda_capture_function_escape(
    program: &Program,
    function_id: FunctionId,
    slot: LambdaCaptureSlotId,
) -> FunctionValueCapability {
    let Some(lambda) = lambda_function(program, function_id) else {
        return FunctionValueCapability::Unknown;
    };
    let Some(capture) = program
        .lambdas
        .get(lambda.index())
        .and_then(|decl| decl.captures.get(slot.index()))
    else {
        return FunctionValueCapability::Unknown;
    };
    if !matches!(
        program.type_arena.get(capture.ty()),
        Some(TypeData::Function(_))
    ) {
        return FunctionValueCapability::NonFunction;
    }
    let source = match capture {
        LambdaCaptureDecl::ReadonlyLocal { source, .. }
        | LambdaCaptureDecl::ScopedLocal { source, .. } => *source,
        LambdaCaptureDecl::NoRuntime { .. }
        | LambdaCaptureDecl::ScopedBorrow { .. }
        | LambdaCaptureDecl::CaptureCell { .. } => return FunctionValueCapability::Unknown,
    };
    program
        .functions
        .get(source.owner.index())
        .and_then(|function| {
            function
                .signature
                .params
                .iter()
                .find(|param| param.local_id == source.local)
        })
        .map_or(FunctionValueCapability::Unknown, |param| {
            FunctionValueCapability::from_param_escape(param.escape)
        })
}

fn global_initializer_function_state(program: &Program, global: GlobalId) -> FunctionValueState {
    let mut visiting = std::collections::HashSet::new();
    global_initializer_function_state_inner(program, global, false, &mut visiting)
}

fn immutable_global_initializer_function_state(
    program: &Program,
    global: GlobalId,
) -> FunctionValueState {
    let mut visiting = std::collections::HashSet::new();
    global_initializer_function_state_inner(program, global, true, &mut visiting)
}

fn global_initializer_function_state_inner(
    program: &Program,
    global: GlobalId,
    require_immutable: bool,
    visiting: &mut std::collections::HashSet<GlobalId>,
) -> FunctionValueState {
    let Some(global_decl) = program.globals.get(global.index()) else {
        return FunctionValueState::unknown();
    };
    if (require_immutable && global_decl.mutability != Mutability::Immutable)
        || !visiting.insert(global)
    {
        return FunctionValueState::unknown();
    }
    let Some(function) = program.functions.get(global_decl.init.index()) else {
        visiting.remove(&global);
        return FunctionValueState::unknown();
    };
    let primitives = PrimitiveTypes::scan(program);
    let mut state = LocalInit::new(program, function);
    for stmt in &function.body.block.stmts {
        match stmt {
            AirStmt::Init { local, value } => {
                let value = initializer_rvalue_function_state(
                    program,
                    global_decl.init,
                    &primitives,
                    value,
                    &state,
                );
                state.init(*local);
                state.set_local_value(*local, value);
            }
            AirStmt::Assign { dst, value } => {
                let value = initializer_rvalue_function_state(
                    program,
                    global_decl.init,
                    &primitives,
                    value,
                    &state,
                );
                state.set_place_value(dst, value);
            }
            AirStmt::GlobalSetRoot {
                global: dst, value, ..
            }
            | AirStmt::GlobalUpdateRoot { global: dst, value }
                if *dst == global =>
            {
                let result = initializer_rvalue_function_state(
                    program,
                    global_decl.init,
                    &primitives,
                    value,
                    &state,
                );
                visiting.remove(&global);
                return result;
            }
            AirStmt::Eval(value) => {
                clear_rvalue_write_state(program, global_decl.init, value, &mut state);
            }
            _ => {}
        }
    }
    let result = match &function.body.block.tail {
        AirTail::Return(Some(value)) => {
            initializer_operand_function_state(program, global_decl.init, value, &state)
        }
        AirTail::ReturnOwned(owned) => {
            initializer_operand_function_state(program, global_decl.init, &owned.value, &state)
        }
        AirTail::None
        | AirTail::Return(None)
        | AirTail::Break(_)
        | AirTail::Continue(_)
        | AirTail::Unreachable => FunctionValueState::unknown(),
    };
    visiting.remove(&global);
    result
}

fn initializer_rvalue_function_state(
    program: &Program,
    function_id: FunctionId,
    primitives: &PrimitiveTypes,
    value: &RValue,
    state: &LocalInit,
) -> FunctionValueState {
    rvalue_function_state_inner(program, function_id, primitives, value, state, false)
}

fn initializer_operand_function_state(
    program: &Program,
    function_id: FunctionId,
    operand: &Operand,
    state: &LocalInit,
) -> FunctionValueState {
    operand_function_state_inner(program, function_id, operand, state, false)
}

fn rvalue_function_state(
    program: &Program,
    function_id: FunctionId,
    primitives: &PrimitiveTypes,
    value: &RValue,
    state: &LocalInit,
) -> FunctionValueState {
    rvalue_function_state_inner(program, function_id, primitives, value, state, true)
}

fn rvalue_function_state_inner(
    program: &Program,
    function_id: FunctionId,
    primitives: &PrimitiveTypes,
    value: &RValue,
    state: &LocalInit,
    global_init: bool,
) -> FunctionValueState {
    match value {
        RValue::Use(operand) => {
            operand_function_state_inner(program, function_id, operand, state, global_init)
        }
        RValue::Materialize(owned) => {
            operand_function_state_inner(program, function_id, &owned.value, state, global_init)
        }
        RValue::FunctionValue { capability, .. } => FunctionValueState::function(*capability),
        RValue::FunctionRef { .. } => {
            FunctionValueState::function(FunctionValueCapability::Escaping)
        }
        RValue::MakeLambda { lambda, .. } => {
            FunctionValueState::function(program.lambdas.get(lambda.index()).map_or(
                FunctionValueCapability::Unknown,
                |decl| {
                    if decl.escape == LambdaEscape::Escaping {
                        FunctionValueCapability::Escaping
                    } else {
                        FunctionValueCapability::NonEscaping
                    }
                },
            ))
        }
        RValue::OptionalSome { value, .. } => {
            FunctionValueState::Fields(vec![operand_function_state_inner(
                program,
                function_id,
                &value.value,
                state,
                global_init,
            )])
        }
        RValue::MapGet { ty, .. } | RValue::MapRemove { ty, .. } | RValue::ListPop { ty, .. } => {
            typing::optional_inner(program, *ty).map_or_else(FunctionValueState::unknown, |inner| {
                FunctionValueState::Fields(vec![source_call_return_state(program, inner)])
            })
        }
        RValue::Aggregate { kind, fields, .. } => aggregate_function_state(
            program,
            kind,
            fields
                .iter()
                .map(|field| {
                    operand_function_state_inner(
                        program,
                        function_id,
                        &field.value,
                        state,
                        global_init,
                    )
                })
                .collect(),
        ),
        RValue::Call {
            callee: Callee::Function(function),
            ..
        } if program
            .functions
            .get(function.index())
            .is_some_and(|function| {
                matches!(
                    function.kind,
                    FunctionKind::Normal | FunctionKind::Method | FunctionKind::ExtendMethod
                )
            }) =>
        {
            typing::rvalue_ty(program, primitives, value)
                .map_or_else(FunctionValueState::unknown, |ty| {
                    source_call_return_state(program, ty)
                })
        }
        _ => typing::rvalue_ty(program, primitives, value)
            .map_or_else(FunctionValueState::unknown, |ty| {
                type_function_state(program, ty)
            }),
    }
}

fn aggregate_function_state(
    program: &Program,
    kind: &AggregateCtor,
    fields: Vec<FunctionValueState>,
) -> FunctionValueState {
    match kind {
        AggregateCtor::Struct(_)
        | AggregateCtor::Tuple
        | AggregateCtor::Array
        | AggregateCtor::ArrayFill
        | AggregateCtor::List
        | AggregateCtor::DataRef(_) => FunctionValueState::Fields(fields),
        AggregateCtor::EnumVariant { enum_id, variant } => {
            let mut variants = program
                .enum_decl(*enum_id)
                .variants
                .iter()
                .map(|variant| match &variant.shape {
                    VariantShape::Unit => vec![],
                    VariantShape::Tuple(fields) => fields
                        .iter()
                        .map(|field| source_call_return_state(program, *field))
                        .collect(),
                    VariantShape::Struct(fields) => fields
                        .iter()
                        .map(|field| source_call_return_state(program, field.ty))
                        .collect(),
                })
                .collect::<Vec<_>>();
            variants[variant.index()] = fields;
            FunctionValueState::Variants(variants)
        }
        AggregateCtor::Map => FunctionValueState::Fields(
            fields
                .into_iter()
                .enumerate()
                .filter_map(|(index, field)| (index % 2 == 1).then_some(field))
                .collect(),
        ),
    }
}

fn operand_function_state(
    program: &Program,
    function_id: FunctionId,
    operand: &Operand,
    state: &LocalInit,
) -> FunctionValueState {
    operand_function_state_inner(program, function_id, operand, state, true)
}

fn operand_function_state_inner(
    program: &Program,
    function_id: FunctionId,
    operand: &Operand,
    state: &LocalInit,
    global_init: bool,
) -> FunctionValueState {
    match operand {
        Operand::Const(id) => program
            .const_arena
            .get_checked(*id)
            .map_or_else(FunctionValueState::unknown, |konst| {
                type_function_state(program, konst.ty)
            }),
        Operand::Place(place) => {
            place_function_state(program, function_id, place, state, global_init)
        }
    }
}

fn optional_payload_function_state(
    program: &Program,
    function_id: FunctionId,
    place: &Place,
    state: &LocalInit,
) -> FunctionValueState {
    match place_function_state(program, function_id, place, state, true) {
        FunctionValueState::Fields(fields) if fields.len() == 1 => fields[0].clone(),
        _ => FunctionValueState::unknown(),
    }
}

fn place_function_state(
    program: &Program,
    function_id: FunctionId,
    place: &Place,
    state: &LocalInit,
    global_init: bool,
) -> FunctionValueState {
    let mut value = match place.root {
        PlaceRoot::Local(local) => state.local_value(local),
        PlaceRoot::Global(global) if state.global_is_definite(global) => state.global_value(global),
        PlaceRoot::Global(global) if global_init => {
            immutable_global_initializer_function_state(program, global)
        }
        PlaceRoot::Global(_)
        | PlaceRoot::LambdaCapture(_)
        | PlaceRoot::ScopedBorrow(_)
        | PlaceRoot::DynBorrowParam(_)
        | PlaceRoot::CaptureCell(_) => type_function_state(program, place.ty),
    };
    if let PlaceRoot::LambdaCapture(slot) = place.root
        && place.projection.is_empty()
    {
        value = FunctionValueState::function(lambda_capture_function_escape(
            program,
            function_id,
            slot,
        ));
    }
    for projection in &place.projection {
        value = value.project(projection);
    }
    value
}

fn verify_function_value_escape_proof(
    cx: &mut VerifyCx<'_>,
    function_id: FunctionId,
    index: usize,
    value: &RValue,
    state: &LocalInit,
) {
    let RValue::FunctionValue {
        value, capability, ..
    } = value
    else {
        return;
    };
    let actual = operand_function_state(cx.program, function_id, &value.value, state).capability();
    if function_value_escape_proof_conflicts(*capability, actual) {
        cx.push(
            VerifyCx::stmt_site(function_id, BlockId::from_index(0), index),
            VerifyErrorKind::BadRValue(BadRValue::FunctionValueEscapeMismatch {
                claimed: *capability,
                actual,
            }),
        );
    }
}

fn function_value_escape_proof_conflicts(
    claimed: FunctionValueCapability,
    actual: FunctionValueCapability,
) -> bool {
    matches!(claimed, FunctionValueCapability::NonFunction)
        || matches!(
            (claimed, actual),
            (
                FunctionValueCapability::Escaping,
                FunctionValueCapability::NonEscaping
                    | FunctionValueCapability::NonFunction
                    | FunctionValueCapability::Unknown
            ) | (
                FunctionValueCapability::NonEscaping,
                FunctionValueCapability::NonFunction
            )
        )
}

fn type_function_capability(program: &Program, ty: TypeId) -> FunctionValueCapability {
    if matches!(program.type_arena.get(ty), Some(TypeData::Function(_))) {
        FunctionValueCapability::Unknown
    } else {
        FunctionValueCapability::NonFunction
    }
}

fn type_function_state(program: &Program, ty: TypeId) -> FunctionValueState {
    match program.type_arena.get(ty) {
        Some(TypeData::Function(_)) => {
            FunctionValueState::function(FunctionValueCapability::Unknown)
        }
        _ => FunctionValueState::non_function(),
    }
}

fn source_call_return_state(program: &Program, ty: TypeId) -> FunctionValueState {
    match program.type_arena.get(ty) {
        Some(TypeData::Function(_)) => {
            FunctionValueState::function(FunctionValueCapability::Escaping)
        }
        Some(TypeData::Optional(inner)) => {
            FunctionValueState::Fields(vec![source_call_return_state(program, *inner)])
        }
        Some(TypeData::Tuple(fields)) => FunctionValueState::Fields(
            fields
                .iter()
                .map(|field| source_call_return_state(program, *field))
                .collect(),
        ),
        Some(TypeData::Aggregate(aggregate)) => program
            .aggregates
            .get(aggregate.index())
            .map_or_else(FunctionValueState::unknown, |decl| {
                FunctionValueState::Fields(
                    decl.fields
                        .iter()
                        .map(|field| source_call_return_state(program, field.ty))
                        .collect(),
                )
            }),
        Some(TypeData::Enum(enm)) => {
            program
                .enums
                .get(enm.index())
                .map_or_else(FunctionValueState::unknown, |enm| {
                    FunctionValueState::Variants(
                        enm.variants
                            .iter()
                            .map(|variant| match &variant.shape {
                                VariantShape::Unit => vec![],
                                VariantShape::Tuple(fields) => fields
                                    .iter()
                                    .map(|field| source_call_return_state(program, *field))
                                    .collect(),
                                VariantShape::Struct(fields) => fields
                                    .iter()
                                    .map(|field| source_call_return_state(program, field.ty))
                                    .collect(),
                            })
                            .collect(),
                    )
                })
        }
        Some(TypeData::Array { elem, .. } | TypeData::List(elem)) => {
            FunctionValueState::Fields(vec![source_call_return_state(program, *elem)])
        }
        Some(TypeData::Map { value, .. }) => {
            FunctionValueState::Fields(vec![source_call_return_state(program, *value)])
        }
        _ => FunctionValueState::non_function(),
    }
}

fn extern_presence_init_arg(program: &Program, callee: ExternId, index: usize) -> bool {
    let ExternMember::Init { owner } = program.externs[callee.index()].member else {
        return false;
    };
    program
        .extern_type(owner)
        .init_args
        .iter()
        .any(|arg| arg.param == index && arg.presence)
}

fn verify_call_args(
    cx: &mut VerifyCx<'_>,
    function_id: FunctionId,
    site: &VerifySite,
    callee: &Callee,
    args: &[CallArg],
) {
    let Some(params) = typing::callee_params(cx.program, callee) else {
        return;
    };
    let Some(expected) = params.len(cx.program) else {
        return;
    };
    if args.len() != expected {
        cx.push(
            site.clone(),
            VerifyErrorKind::BadCall(BadCall::ArityMismatch {
                expected,
                found: args.len(),
            }),
        );
    }
    for first in 0..args.len() {
        for second in first + 1..args.len() {
            if call_args_conflict(cx.program, function_id, &args[first], &args[second]) {
                cx.push(
                    site.clone(),
                    VerifyErrorKind::BadCall(BadCall::ArgAliasConflict { first, second }),
                );
            }
        }
    }
    for (i, arg) in args.iter().enumerate() {
        let Some(expected_param) = params.get(cx.program, i) else {
            continue;
        };
        let expected_init_field = matches!(callee, Callee::Extern(id)
            if extern_presence_init_arg(cx.program, *id, i));
        let found_init_field = matches!(
            arg,
            CallArg::InitFieldProvided(_) | CallArg::InitFieldOmitted
        );
        if found_init_field != expected_init_field {
            cx.push(
                site.clone(),
                VerifyErrorKind::BadCall(BadCall::UnexpectedInitFieldArg { index: i }),
            );
        }
        if arg.mode() != expected_param.mode {
            cx.push(
                site.clone(),
                VerifyErrorKind::BadCall(BadCall::ArgModeMismatch {
                    index: i,
                    expected: expected_param.mode,
                    found: arg.mode(),
                }),
            );
        }
        if let Some(found_ty) = typing::call_arg_ty(cx.program, arg)
            && found_ty != expected_param.ty
        {
            cx.push(
                site.clone(),
                VerifyErrorKind::BadCall(BadCall::ArgTypeMismatch {
                    index: i,
                    expected: expected_param.ty,
                    found: found_ty,
                }),
            );
        }
    }
}

fn call_args_conflict(
    program: &Program,
    function_id: FunctionId,
    left: &CallArg,
    right: &CallArg,
) -> bool {
    left.place()
        .zip(right.place())
        .is_some_and(|(left_place, right_place)| {
            call_places_conflict(
                program,
                function_id,
                left.mode(),
                left_place,
                right.mode(),
                right_place,
            )
        })
}

fn call_places_conflict(
    program: &Program,
    function_id: FunctionId,
    left_mode: ParamMode,
    left: &Place,
    right_mode: ParamMode,
    right: &Place,
) -> bool {
    let borrow_conflict = matches!(
        (left_mode, right_mode),
        (ParamMode::SharedBorrow, ParamMode::MutBorrow)
            | (
                ParamMode::MutBorrow,
                ParamMode::SharedBorrow | ParamMode::MutBorrow
            )
    );
    borrow_conflict && program.places_may_overlap(function_id, left, right)
}

fn verify_slice_index(
    cx: &mut VerifyCx<'_>,
    function_id: FunctionId,
    block_id: BlockId,
    stmt_idx: usize,
    which: &'static str,
    local: LocalId,
) {
    let site = VerifyCx::stmt_site(function_id, block_id, stmt_idx);
    let Some(local) = cx.program.function(function_id).locals.get(local.index()) else {
        cx.push(
            site,
            VerifyErrorKind::BadReference(BadReference::InvalidLocal(local)),
        );
        return;
    };
    if !cx.primitives.is_int(local.ty) {
        cx.push(
            site,
            VerifyErrorKind::BadFunction(BadFunction::SliceIndexMustBeInt {
                which,
                found: local.ty,
            }),
        );
    }
}

fn verify_type(cx: &mut VerifyCx<'_>, id: TypeId) {
    let Some(data) = cx.type_data(id).cloned() else {
        return;
    };
    let site = VerifySite::Type(id);
    match data {
        TypeData::Int
        | TypeData::Float
        | TypeData::Bool
        | TypeData::String
        | TypeData::Char
        | TypeData::Void
        | TypeData::Any => {}
        TypeData::Optional(inner) | TypeData::List(inner) | TypeData::Slice(inner) => {
            cx.verify_type_ref(site, inner);
        }
        TypeData::Tuple(items) => {
            for item in items {
                cx.verify_type_ref(site.clone(), item);
            }
        }
        TypeData::Array { elem, .. } => {
            cx.verify_type_ref(site, elem);
        }
        TypeData::Map { key, value, .. } => {
            cx.verify_type_ref(site.clone(), key);
            cx.verify_type_ref(site, value);
        }
        TypeData::Function(sig) => {
            for param in &sig.params {
                cx.verify_type_ref(site.clone(), param.ty);
            }
            cx.verify_type_ref(site, sig.ret.ty());
        }
        TypeData::Dyn(surface) => {
            if !cx.has_contract_surface(surface) {
                cx.push(
                    site,
                    VerifyErrorKind::BadReference(BadReference::InvalidContractSurface(surface)),
                );
            }
        }
        TypeData::Aggregate(agg_id) | TypeData::DataRef(agg_id) => {
            if !cx.has_aggregate(agg_id) {
                cx.push(
                    VerifySite::Type(id),
                    VerifyErrorKind::BadReference(BadReference::InvalidAggregate(agg_id)),
                );
            }
        }
        TypeData::Enum(enum_id) => {
            if !cx.has_enum(enum_id) {
                cx.push(
                    VerifySite::Type(id),
                    VerifyErrorKind::BadReference(BadReference::InvalidEnum(enum_id)),
                );
            }
        }
        TypeData::Flag(flag_id) => {
            if !cx.has_flag(flag_id) {
                cx.push(
                    VerifySite::Type(id),
                    VerifyErrorKind::BadReference(BadReference::InvalidFlag(flag_id)),
                );
            }
        }
        TypeData::Extern(ext_id) => {
            if !cx.has_extern_type(ext_id) {
                cx.push(
                    VerifySite::Type(id),
                    VerifyErrorKind::BadReference(BadReference::InvalidExternType(ext_id)),
                );
            }
        }
    }
}
