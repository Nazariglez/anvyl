use anvyx_externs::{
    BinaryOp as ExternBinaryOp, ExternBindingOp, ExternBindingTarget, ExternMemberSelector,
    ExternOperator, ExternTypeKey, ModulePath as ExternModulePath, UnaryOp as ExternUnaryOp,
};

pub use super::typing::PrimitiveKind;
use super::{
    AggregateKind, CaptureLocalSource, ConstValue, EnumRepr, ExternMember, ExternRep, Function,
    FunctionKind, LambdaCaptureDecl, LambdaDecl, LambdaEscape, Local, LocalKind, Mutability, Param,
    ParamEscape, ParamMode, ParamRole, Program, RawEnumValue, ReturnMode, SignatureType, TypeData,
    VariantShape,
    body::{
        AggregateCtor, AirBlock, AirEnumMatch, AirIf, AirOptionalMatch, AirStmt, AirTail, CallArg,
        Callee, LambdaCaptureArg, Operand, Place, PlaceReadLocal, PlaceRoot, Projection, RValue,
    },
    ids::*,
    typing::{self, PrimitiveTypes},
};
use crate::ast::{BinaryOp, UnaryOp};

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
    ExternType(ExternTypeId),
    Extern(ExternId),
    Lambda(LambdaId),
    ScopedBorrow(ScopedBorrowId),
    CaptureCell(CaptureCellId),
    Global(GlobalId),
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
    BadRValue(BadRValue),
    BadStatement(BadStatement),
    BadExtern(BadExtern),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BadExtern {
    ReceiverTypeMismatch { expected: TypeId, found: TypeId },
    InvalidInitField(FieldId),
    OperatorOperandMismatch,
    MemberParamCountMismatch { expected: usize, found: usize },
    ReceiverModeMismatch,
    BindingMismatch,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BadType {
    DuplicatePrimitive {
        kind: PrimitiveKind,
        first: TypeId,
        duplicate: TypeId,
    },
    EmptyDynContract,
    Recursive(TypeId),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BadConst {
    TypeMismatch { expected: TypeId, found: TypeId },
    NilMustBeOptional(TypeId),
    MissingPrimitive(PrimitiveKind),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BadEnum {
    AdtHasRawType,
    RawMissingRawType,
    RawTypeMismatch(TypeId),
    RawGeneric,
    AdtVariantHasRawValue(VariantId),
    RawVariantMissingValue(VariantId),
    RawVariantPayload(VariantId),
    RawIntVariantValueType(VariantId),
    RawStringVariantValueType(VariantId),
    DuplicateRawValue(VariantId),
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
    Aggregate(AggregateId),
    Enum(EnumId),
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
    ExternCtorResultTypeMismatch {
        extern_id: ExternTypeId,
        found: TypeId,
    },
    ExternCtorUnavailable {
        extern_id: ExternTypeId,
    },
    ExternCtorFieldCountMismatch {
        extern_id: ExternTypeId,
        expected: usize,
        found: usize,
    },
    ExternCtorFieldTypeMismatch {
        extern_id: ExternTypeId,
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
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BadStatement {
    InitParamLocal(LocalId),
    InitTypeMismatch { expected: TypeId, found: TypeId },
    AssignTypeMismatch { expected: TypeId, found: TypeId },
    ReadUninitializedLocal(LocalId),
    ReadUninitializedCaptureCell(CaptureCellId),
    AssignUninitializedLocal(LocalId),
    AssignUninitializedCaptureCell(CaptureCellId),
    InitImmutableLocalTwice(LocalId),
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
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BadFunction {
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
    ListSliceResultMustBeList {
        found: TypeId,
    },
    SliceIndexMustBeInt {
        which: &'static str,
        found: TypeId,
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
    DuplicateCaptureCell {
        owner: FunctionId,
        binding: BindingId,
        first: CaptureCellId,
        second: CaptureCellId,
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
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BadPlace {
    UnsupportedRoot(PlaceRoot),
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
    VariantFieldOutOfRange {
        ty: TypeId,
        index: u16,
        len: usize,
    },
    VariantProjectionOnNonEnum(TypeId),
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
    PromotedBindingBypassesCell {
        binding: BindingId,
        cell: CaptureCellId,
        local: LocalId,
    },
    CaptureCellNotAccessible {
        cell: CaptureCellId,
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

#[cfg(test)]
pub(crate) fn verify_structured_body(
    program: &Program,
    function_id: FunctionId,
    body: &super::AirBody,
) -> Result<(), Vec<VerifyError>> {
    let mut cx = VerifyCx::new(program);
    let mut state = LocalInit::new(program, program.function(function_id));
    verify_air_block(
        &mut cx,
        function_id,
        &body.block,
        &mut state,
        &mut Vec::new(),
    );
    if cx.errors.is_empty() {
        Ok(())
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
        Self {
            program,
            primitives: PrimitiveTypes::scan(program),
            type_states: vec![TypeState::Unseen; program.type_arena.len()],
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum FunctionValueCapability {
    NonFunction,
    Escaping,
    NonEscaping,
    Unknown,
}

impl FunctionValueCapability {
    fn from_param_escape(escape: ParamEscape) -> Self {
        match escape {
            ParamEscape::Escaping => Self::Escaping,
            ParamEscape::NonEscaping => Self::NonEscaping,
        }
    }
}

#[derive(Clone)]
struct LocalInit {
    definite: Vec<bool>,
    possible: Vec<bool>,
    local_escape: Vec<FunctionValueCapability>,
    cell_definite: Vec<bool>,
}

impl LocalInit {
    fn new(program: &Program, function: &Function) -> Self {
        let mut state = Self {
            definite: vec![false; function.locals.len()],
            possible: vec![false; function.locals.len()],
            local_escape: function
                .locals
                .iter()
                .map(|local| {
                    if matches!(
                        program.type_arena.get(local.ty),
                        Some(TypeData::Function(_))
                    ) {
                        FunctionValueCapability::Unknown
                    } else {
                        FunctionValueCapability::NonFunction
                    }
                })
                .collect(),
            cell_definite: vec![false; program.capture_cells.len()],
        };
        for param in &function.signature.params {
            if param.local_id.index() < function.locals.len() {
                state.definite[param.local_id.index()] = true;
                state.possible[param.local_id.index()] = true;
                if matches!(
                    program.type_arena.get(param.ty),
                    Some(TypeData::Function(_))
                ) {
                    state.local_escape[param.local_id.index()] =
                        FunctionValueCapability::from_param_escape(param.escape);
                }
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

    fn set_escape(&mut self, local: LocalId, escape: FunctionValueCapability) {
        if local.index() < self.local_escape.len() {
            self.local_escape[local.index()] = escape;
        }
    }

    fn escape(&self, local: LocalId) -> FunctionValueCapability {
        self.local_escape
            .get(local.index())
            .copied()
            .unwrap_or(FunctionValueCapability::Unknown)
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

    fn clear(&mut self, local: LocalId) {
        if local.index() < self.definite.len() {
            self.definite[local.index()] = false;
            self.possible[local.index()] = false;
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
            for (left, right) in joined.local_escape.iter_mut().zip(state.local_escape) {
                *left = join_escape(*left, right);
            }
            for (left, right) in joined.cell_definite.iter_mut().zip(state.cell_definite) {
                *left &= right;
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
            Some(function) if matches!(function.kind, FunctionKind::Lambda(_)) => cx.push(
                VerifySite::Program,
                VerifyErrorKind::BadFunction(BadFunction::EntryMustBeNamed(entry)),
            ),
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

    for (id, _) in cx.program.type_arena.iter().enumerate() {
        let ty = TypeId::from_index(id);
        cx.verify_type_ref(VerifySite::Type(ty), ty);
    }

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
    for (id, _) in cx.program.extern_types.iter().enumerate() {
        verify_extern_type(cx, ExternTypeId::from_index(id));
    }
    for (id, _) in cx.program.externs.iter().enumerate() {
        verify_extern(cx, ExternId::from_index(id));
    }
    for (id, _) in cx.program.lambdas.iter().enumerate() {
        verify_lambda(cx, LambdaId::from_index(id));
    }
    for (id, _) in cx.program.scoped_borrows.iter().enumerate() {
        verify_scoped_borrow(cx, ScopedBorrowId::from_index(id));
    }
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

fn verify_module(cx: &mut VerifyCx<'_>, id: ModuleId) {
    let module = cx.program.module(id);
    let site = VerifySite::Module(id);
    verify_module_items(cx, &site, id, &module.functions, |cx, item| {
        cx.program
            .functions
            .get(item.index())
            .map(|decl| decl.module)
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
impl_module_ref!(AggregateId, InvalidAggregate, Aggregate);
impl_module_ref!(EnumId, InvalidEnum, Enum);
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
        let binding = lambda_capture_decl_binding(capture);
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

fn lambda_capture_decl_binding(capture: &LambdaCaptureDecl) -> BindingId {
    match capture {
        LambdaCaptureDecl::NoRuntime { binding, .. }
        | LambdaCaptureDecl::ReadonlyLocal { binding, .. }
        | LambdaCaptureDecl::ScopedLocal { binding, .. }
        | LambdaCaptureDecl::ScopedBorrow { binding, .. }
        | LambdaCaptureDecl::CaptureCell { binding, .. } => *binding,
    }
}

fn lambda_capture_decl_ty(capture: &LambdaCaptureDecl) -> TypeId {
    match capture {
        LambdaCaptureDecl::NoRuntime { ty, .. }
        | LambdaCaptureDecl::ReadonlyLocal { ty, .. }
        | LambdaCaptureDecl::ScopedLocal { ty, .. }
        | LambdaCaptureDecl::ScopedBorrow { ty, .. }
        | LambdaCaptureDecl::CaptureCell { ty, .. } => *ty,
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

fn lambda_capture_decl_mutability(capture: &LambdaCaptureDecl) -> Mutability {
    match capture {
        LambdaCaptureDecl::ScopedLocal { mutability, .. }
        | LambdaCaptureDecl::ScopedBorrow { mutability, .. } => *mutability,
        LambdaCaptureDecl::CaptureCell { .. } => Mutability::Mutable,
        LambdaCaptureDecl::NoRuntime { .. } | LambdaCaptureDecl::ReadonlyLocal { .. } => {
            Mutability::Immutable
        }
    }
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
            borrow,
            ty,
            mutability,
            ..
        } => {
            cx.verify_type_ref(site.clone(), *ty);
            match cx.program.scoped_borrows.get(borrow.index()) {
                Some(borrow_decl)
                    if borrow_decl.ty == *ty && borrow_decl.mutability == *mutability => {}
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
    lambda_capture_decl_binding(capture) == binding
        && lambda_capture_decl_ty(capture) == ty
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

fn verify_scoped_borrow(cx: &mut VerifyCx<'_>, id: ScopedBorrowId) {
    let decl = &cx.program.scoped_borrows[id.index()];
    cx.verify_type_ref(VerifySite::ScopedBorrow(id), decl.ty);
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
    cx.verify_module_ref(VerifySite::Global(id), decl.module);
    cx.verify_type_ref(VerifySite::Global(id), decl.ty);
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
        ConstValue::Float(_) => required_const_primitive(cx, site.clone(), PrimitiveKind::Float),
        ConstValue::Bool(_) => required_const_primitive(cx, site.clone(), PrimitiveKind::Bool),
        ConstValue::String(_) => required_const_primitive(cx, site.clone(), PrimitiveKind::String),
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
            if !enm.type_args.is_empty() || !enm.const_args.is_empty() {
                cx.push(site.clone(), VerifyErrorKind::BadEnum(BadEnum::RawGeneric));
            }
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

fn verify_extern_type(cx: &mut VerifyCx<'_>, id: ExternTypeId) {
    let ty = cx.program.extern_type(id);
    let site = VerifySite::ExternType(id);
    cx.verify_module_ref(site.clone(), ty.module);
    verify_decl_listed_once(cx, site.clone(), ty.module, id, |m| &m.extern_types);
    let owner_ty = extern_owner_type(cx, id);
    let mut init_fields = std::collections::HashSet::new();
    for init_field in &ty.init_fields {
        let invalid = !ty.has_init
            || !init_fields.insert(*init_field)
            || ty
                .fields
                .get(init_field.index())
                .is_none_or(|field| field.computed);
        if invalid {
            cx.push(
                site.clone(),
                VerifyErrorKind::BadExtern(BadExtern::InvalidInitField(*init_field)),
            );
        }
    }
    for field in &ty.fields {
        cx.verify_type_ref(site.clone(), field.ty);
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
    for method in &ty.methods {
        if let Some(owner_ty) = owner_ty {
            verify_receiver(cx, site.clone(), owner_ty, method.receiver.ty);
        } else {
            cx.verify_type_ref(site.clone(), method.receiver.ty);
        }
        for param in &method.params {
            cx.verify_type_ref(site.clone(), param.ty);
        }
        cx.verify_type_ref(site.clone(), method.return_type);
    }
    for static_ in &ty.statics {
        for param in &static_.params {
            cx.verify_type_ref(site.clone(), param.ty);
        }
        cx.verify_type_ref(site.clone(), static_.return_type);
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
            cx.verify_type_ref(site.clone(), operand.ty);
        }
        cx.verify_type_ref(site.clone(), op.return_type);
    }
}

fn verify_extern(cx: &mut VerifyCx<'_>, id: ExternId) {
    let ext = cx.program.extern_decl(id);
    let site = VerifySite::Extern(id);
    cx.verify_module_ref(site.clone(), ext.module);
    verify_decl_listed_once(cx, site.clone(), ext.module, id, |m| &m.externs);
    verify_extern_binding(cx, site.clone(), ext);
    verify_extern_member(cx, site.clone(), &ext.member, &ext.params);
    for param in &ext.params {
        cx.verify_type_ref(site.clone(), param.ty);
    }
    cx.verify_type_ref(site, ext.return_type);
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
    if let FunctionKind::Lambda(lambda) = func.kind {
        match cx.program.lambdas.get(lambda.index()) {
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
        }
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
    if verify_air_block(cx, id, &func.body.block, &mut state, &mut Vec::new()).is_some()
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
            verify_air_rvalue_reads(cx, function_id, index, value, state);
            verify_promoted_local_not_initialized(cx, function_id, block_id, index, *local);
            verify_init_stmt(cx, function_id, block_id, index, *local, value);
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
                next.init(*local);
                next.set_escape(
                    *local,
                    rvalue_function_escape(cx.program, function_id, &cx.primitives, value, state),
                );
                return Some(next);
            }
            Some(state.clone())
        }
        AirStmt::Assign { dst, value } => {
            verify_air_rvalue_reads(cx, function_id, index, value, state);
            verify_air_place_write(cx, function_id, index, dst, state);
            verify_assign_stmt(cx, function_id, block_id, index, dst, value);
            let mut next = state.clone();
            if dst.projection.is_empty() {
                match dst.root {
                    PlaceRoot::Local(local) => {
                        next.set_escape(
                            local,
                            rvalue_function_escape(
                                cx.program,
                                function_id,
                                &cx.primitives,
                                value,
                                state,
                            ),
                        );
                    }
                    PlaceRoot::CaptureCell(cell) => next.init_cell(cell),
                    PlaceRoot::LambdaCapture(_)
                    | PlaceRoot::ScopedBorrow(_)
                    | PlaceRoot::Global(_) => {}
                }
            }
            Some(next)
        }
        AirStmt::Eval(value) => {
            verify_air_rvalue_reads(cx, function_id, index, value, state);
            verify_rvalue(cx, function_id, block_id, Some(index), value);
            Some(state.clone())
        }
        AirStmt::If(branch) => verify_air_if(cx, function_id, index, branch, state, loops),
        AirStmt::Loop(loop_) => {
            loops.push(LoopCtx {
                id: loop_.id,
                breaks: Vec::new(),
            });
            let mut body_state = state.clone();
            verify_air_block(cx, function_id, &loop_.body, &mut body_state, loops);
            let loop_ctx = loops.pop().unwrap();
            LocalInit::join(loop_ctx.breaks)
        }
        AirStmt::EnumMatch(match_) => {
            verify_air_match(cx, function_id, index, match_, state, loops)
        }
        AirStmt::OptionalMatch(match_) => {
            verify_air_optional_match(cx, function_id, index, match_, state, loops)
        }
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

fn verify_air_match(
    cx: &mut VerifyCx<'_>,
    function_id: FunctionId,
    index: usize,
    match_: &AirEnumMatch,
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
    let Some(expected_enum) = discr_ty.and_then(|ty| match cx.type_data(ty) {
        Some(TypeData::Enum(id)) if cx.has_enum(*id) => Some(*id),
        Some(TypeData::Enum(_)) | None => None,
        Some(_) => {
            cx.push(
                site.clone(),
                VerifyErrorKind::BadFunction(BadFunction::SwitchDiscriminantMustBeEnum(ty)),
            );
            None
        }
    }) else {
        return Some(state.clone());
    };

    let mut seen = std::collections::HashSet::new();
    let mut fallthrough = Vec::new();
    for arm in &match_.arms {
        if !seen.insert(arm.variant) {
            cx.push(
                site.clone(),
                VerifyErrorKind::BadFunction(BadFunction::DuplicateSwitchArm(arm.variant)),
            );
        }
        if !cx.variant_belongs_to_enum(expected_enum, arm.variant) {
            cx.push(
                site.clone(),
                VerifyErrorKind::BadFunction(BadFunction::SwitchArmVariantMismatch {
                    expected_enum,
                    variant: arm.variant,
                }),
            );
        }
        let mut arm_state = state.clone();
        if let Some(state) = verify_air_block(cx, function_id, &arm.block, &mut arm_state, loops) {
            fallthrough.push(state);
        }
    }
    if let Some(else_block) = &match_.else_block {
        let mut else_state = state.clone();
        if let Some(state) = verify_air_block(cx, function_id, else_block, &mut else_state, loops) {
            fallthrough.push(state);
        }
    } else if seen.len() < cx.program.enum_decl(expected_enum).variants.len() {
        cx.push(
            site,
            VerifyErrorKind::BadFunction(BadFunction::MatchNotExhaustive(expected_enum)),
        );
    }
    LocalInit::join(fallthrough)
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
            if let Some(value) = value {
                verify_air_operand_read(cx, function_id, 0, value, state);
            }
            let function = cx.program.function(function_id);
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
            if !loops.iter().any(|loop_ctx| loop_ctx.id == *id) {
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
        RValue::Use(op)
        | RValue::Stringify { value: op, .. }
        | RValue::Unary { value: op, .. }
        | RValue::OptionalSome { value: op, .. }
        | RValue::Cast { value: op, .. }
        | RValue::Format { value: op, .. } => {
            verify_air_operand_read(cx, function_id, index, op, state);
        }
        RValue::Binary { lhs, rhs, .. } | RValue::SharedRefEq { lhs, rhs, .. } => {
            verify_air_operand_read(cx, function_id, index, lhs, state);
            verify_air_operand_read(cx, function_id, index, rhs, state);
        }
        RValue::Aggregate { fields, .. } | RValue::StringConcat { parts: fields } => {
            for field in fields {
                verify_air_operand_read(cx, function_id, index, field, state);
            }
        }
        RValue::Call { callee, args } => {
            verify_call_escape_args(cx, function_id, index, callee, args, state);
            if let Callee::Lambda(op) = callee {
                verify_air_operand_read(cx, function_id, index, op, state);
            }
            for arg in args {
                match arg {
                    CallArg::Value(op) => {
                        verify_air_operand_read(cx, function_id, index, op, state);
                    }
                    CallArg::SharedBorrow(place) | CallArg::MutBorrow(place) => {
                        verify_air_place_read(cx, function_id, index, place, state);
                    }
                    CallArg::SharedStringConst(_) => {}
                }
            }
        }
        RValue::Len { source } | RValue::ListPop { list: source, .. } => {
            verify_air_place_read(cx, function_id, index, source, state);
        }
        RValue::ListSlice {
            source, start, end, ..
        }
        | RValue::SliceView {
            source, start, end, ..
        } => {
            verify_air_place_read(cx, function_id, index, source, state);
            verify_air_local_read(cx, function_id, index, *start, state);
            verify_air_local_read(cx, function_id, index, *end, state);
        }
        RValue::ListPush { list, value } => {
            verify_air_place_read(cx, function_id, index, list, state);
            verify_air_operand_read(cx, function_id, index, value, state);
        }
        RValue::MapGet { map, key, .. } | RValue::MapRemove { map, key, .. } => {
            verify_air_place_read(cx, function_id, index, map, state);
            verify_air_operand_read(cx, function_id, index, key, state);
        }
        RValue::MapEntryAt {
            map, index: key, ..
        } => {
            verify_air_place_read(cx, function_id, index, map, state);
            verify_air_local_read(cx, function_id, index, *key, state);
        }
        RValue::MapInsert { map, key, value } => {
            verify_air_place_read(cx, function_id, index, map, state);
            verify_air_operand_read(cx, function_id, index, key, state);
            verify_air_operand_read(cx, function_id, index, value, state);
        }
        RValue::FunctionRef { .. } => {}
        RValue::MakeLambda { captures, .. } => {
            for capture in captures {
                verify_air_lambda_capture_read(cx, function_id, index, capture, state);
            }
        }
    }
}

fn verify_air_lambda_capture_read(
    cx: &mut VerifyCx<'_>,
    function_id: FunctionId,
    index: usize,
    capture: &LambdaCaptureArg,
    state: &LocalInit,
) {
    match capture {
        LambdaCaptureArg::NoRuntime => {}
        LambdaCaptureArg::CaptureCell { cell } => {
            verify_air_cell_read(cx, function_id, index, *cell, state);
        }
        LambdaCaptureArg::ReadonlyLocal { value } => {
            verify_air_operand_read(cx, function_id, index, value, state);
        }
        LambdaCaptureArg::ScopedLocal { place } | LambdaCaptureArg::ScopedBorrow { place } => {
            verify_air_place_read(cx, function_id, index, place, state);
        }
    }
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

fn verify_rvalue(
    cx: &mut VerifyCx<'_>,
    function_id: FunctionId,
    block_id: BlockId,
    stmt_index: Option<usize>,
    value: &RValue,
) {
    let stmt_idx = stmt_index.unwrap_or(0);
    let site = VerifyCx::stmt_site(function_id, block_id, stmt_idx);

    match value {
        RValue::Use(op) => {
            verify_operand(cx, function_id, block_id, stmt_index, op);
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
                    _ => false,
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
            if matches!(op, BinaryOp::And | BinaryOp::Or | BinaryOp::Coalesce) {
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
                    _ => false,
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
            verify_operand(cx, function_id, block_id, stmt_index, value);
            cx.verify_type_ref(site.clone(), *ty);
            match (
                typing::optional_inner(cx.program, *ty),
                typing::operand_ty(cx.program, value),
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
                        found: typing::operand_ty(cx.program, value).unwrap_or(*ty),
                    }),
                ),
            }
        }
        RValue::Cast { value: op, target } => {
            verify_operand(cx, function_id, block_id, stmt_index, op);
            cx.verify_type_ref(site.clone(), *target);
            if let Some(value_ty) = typing::operand_ty(cx.program, op)
                && !typing::valid_cast(cx.program, &cx.primitives, value_ty, *target)
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
                AggregateCtor::Extern(id) => {
                    verify_extern_ctor(cx, site.clone(), *id, *ty, fields);
                }
                AggregateCtor::EnumVariant { enum_id, variant } => {
                    verify_enum_ctor(cx, site.clone(), *enum_id, *variant, *ty, fields);
                }
                AggregateCtor::Array => verify_array_ctor(cx, site.clone(), *ty, fields),
                AggregateCtor::List => verify_list_ctor(cx, site.clone(), *ty, fields),
                AggregateCtor::Map => verify_map_ctor(cx, site.clone(), *ty, fields),
                AggregateCtor::Tuple => verify_tuple_ctor(cx, site.clone(), *ty, fields),
            }
            for field in fields {
                verify_operand(cx, function_id, block_id, stmt_index, field);
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
            verify_operand(cx, function_id, block_id, stmt_index, value);
            if let Some(expected_elem) = typing::list_elem(cx.program, list.ty)
                && let Some(value_ty) = typing::operand_ty(cx.program, value)
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
        RValue::ListSlice {
            source,
            start,
            end,
            ty,
            ..
        } => {
            verify_place(cx, function_id, block_id, stmt_index, source);
            cx.verify_type_ref(site.clone(), *ty);
            if let Some(expected_elem) = typing::list_elem(cx.program, source.ty) {
                let valid = typing::list_elem(cx.program, *ty) == Some(expected_elem);
                if !valid {
                    cx.push(
                        site.clone(),
                        VerifyErrorKind::BadFunction(BadFunction::ListSliceResultMustBeList {
                            found: *ty,
                        }),
                    );
                }
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
        RValue::MapInsert { map, key, value } => {
            required_rvalue_primitive(cx, site.clone(), PrimitiveKind::Void);
            verify_place(cx, function_id, block_id, stmt_index, map);
            verify_mutable_place(cx, function_id, &site, map);
            verify_operand(cx, function_id, block_id, stmt_index, key);
            verify_operand(cx, function_id, block_id, stmt_index, value);
            if let Some((expected_key, expected_value)) = typing::map_kv(cx.program, map.ty) {
                verify_map_key(cx, &site, key, expected_key);
                verify_map_value(cx, &site, value, expected_value);
            }
        }
        RValue::MapEntryAt { map, index, ty } => {
            verify_place(cx, function_id, block_id, stmt_index, map);
            verify_slice_index(cx, function_id, block_id, stmt_idx, "index", *index);
            cx.verify_type_ref(site.clone(), *ty);
            match typing::map_kv(cx.program, map.ty) {
                Some((_, value_ty)) if value_ty == *ty => {}
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
            cx.verify_type_ref(site, *ty);
        }
        RValue::FunctionRef { function, ty } => {
            match cx.program.functions.get(function.index()) {
                Some(function_decl) if matches!(function_decl.kind, FunctionKind::Lambda(_)) => {
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
            && !readonly_capture_arg_is_valid(cx.program, function_id, decl_capture, value)
        {
            cx.push(
                site.clone(),
                VerifyErrorKind::BadRValue(BadRValue::ReadonlyCaptureMustBeImmutableOwned {
                    index,
                }),
            );
        }
        if lambda_capture_decl_mutability(decl_capture) == Mutability::Mutable
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

fn place_is_exact_scoped_borrow(place: &Place, borrow: ScopedBorrowId, ty: TypeId) -> bool {
    place.root == PlaceRoot::ScopedBorrow(borrow) && place.projection.is_empty() && place.ty == ty
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
            typing::operand_ty(program, value) == Some(*ty)
                && (source.owner == function_id
                    && readonly_capture_is_immutable_owned(
                        program,
                        function_id,
                        value,
                        Some(*source),
                    )
                    || operand_is_matching_lambda_capture(
                        program,
                        function_id,
                        value,
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
        ) => place_is_exact_scoped_borrow(place, *borrow, *ty),
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
    if place_crosses_dataref(cx.program, function_id, place) {
        return;
    }
    if let Some(cell) = cx.program.capture_cell_root(function_id, place.root)
        && !place.projection.is_empty()
    {
        cx.push(
            site.clone(),
            VerifyErrorKind::BadPlace(BadPlace::UnsupportedCaptureCellProjection(cell)),
        );
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
            verify_operand(cx, function_id, block_id, stmt_index, value);
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
                    Some(lambda_capture_decl_ty(capture))
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
            if let Some(lambda) = lambda_function(cx.program, function_id) {
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
            Some(cx.program.scoped_borrows[id.index()].ty)
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

fn place_mutability(
    program: &Program,
    function_id: FunctionId,
    root: PlaceRoot,
) -> Option<Mutability> {
    if program.capture_cell_root(function_id, root).is_some() {
        return Some(Mutability::Mutable);
    }
    match root {
        PlaceRoot::Local(local) => program
            .function(function_id)
            .locals
            .get(local.index())
            .map(|decl| decl.mutability),
        PlaceRoot::LambdaCapture(slot) => lambda_function(program, function_id)
            .and_then(|lambda| program.lambdas.get(lambda.index()))
            .and_then(|decl| decl.captures.get(slot.index()))
            .map(lambda_capture_decl_mutability),
        PlaceRoot::ScopedBorrow(id) => program
            .scoped_borrows
            .get(id.index())
            .map(|decl| decl.mutability),
        PlaceRoot::CaptureCell(_) => Some(Mutability::Mutable),
        PlaceRoot::Global(id) => program.globals.get(id.index()).map(|decl| decl.mutability),
    }
}

fn place_crosses_dataref(program: &Program, function_id: FunctionId, place: &Place) -> bool {
    let Some(mut ty) = place_root_ty(program, function_id, place.root) else {
        return false;
    };
    for projection in &place.projection {
        let Some(data) = program.type_arena.get(ty) else {
            return false;
        };
        if matches!(data, TypeData::DataRef(_)) {
            return true;
        }
        let Some(next) = projection_ty(program, ty, projection) else {
            return false;
        };
        ty = next;
    }
    false
}

fn place_root_ty(program: &Program, function_id: FunctionId, root: PlaceRoot) -> Option<TypeId> {
    match root {
        PlaceRoot::Local(local) => program
            .function(function_id)
            .locals
            .get(local.index())
            .map(|decl| decl.ty),
        PlaceRoot::LambdaCapture(slot) => lambda_function(program, function_id)
            .and_then(|lambda| program.lambdas.get(lambda.index()))
            .and_then(|decl| decl.captures.get(slot.index()))
            .map(lambda_capture_decl_ty),
        PlaceRoot::ScopedBorrow(id) => program.scoped_borrows.get(id.index()).map(|decl| decl.ty),
        PlaceRoot::CaptureCell(id) => program.capture_cells.get(id.index()).map(|decl| decl.ty),
        PlaceRoot::Global(id) => program.globals.get(id.index()).map(|decl| decl.ty),
    }
}

fn projection_ty(program: &Program, ty: TypeId, projection: &Projection) -> Option<TypeId> {
    match projection {
        Projection::Field(field) => typing::field_by_id(program, ty, *field),
        Projection::TupleField(index) => match program.type_arena.get(ty) {
            Some(TypeData::Tuple(elems)) => elems.get(*index as usize).copied(),
            _ => None,
        },
        Projection::VariantField { variant, field, .. } => {
            typing::enum_variant_field(program, ty, *variant, *field)
        }
        Projection::Index(_) => typing::index_elem(program, ty),
    }
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
            Projection::VariantField {
                enum_id,
                variant,
                field,
            } => match data {
                TypeData::Enum(eid) if *eid == *enum_id => {
                    let Some(enm) = cx.program.enums.get(enum_id.index()) else {
                        cx.push(
                            site.clone(),
                            VerifyErrorKind::BadReference(BadReference::InvalidEnum(*enum_id)),
                        );
                        return None;
                    };
                    let Some(shape) = enm.variants.get(variant.index()).map(|decl| &decl.shape)
                    else {
                        cx.push(
                            site.clone(),
                            VerifyErrorKind::BadReference(BadReference::InvalidVariant {
                                enum_id: *enum_id,
                                variant: *variant,
                            }),
                        );
                        return None;
                    };
                    let Some(ty) =
                        typing::enum_variant_field(cx.program, current_ty, *variant, *field)
                    else {
                        cx.push(
                            site.clone(),
                            VerifyErrorKind::BadPlace(BadPlace::VariantFieldOutOfRange {
                                ty: current_ty,
                                index: *field,
                                len: typing::variant_field_count(shape),
                            }),
                        );
                        return None;
                    };
                    current_ty = ty;
                }
                _ => {
                    cx.push(
                        site.clone(),
                        VerifyErrorKind::BadPlace(BadPlace::VariantProjectionOnNonEnum(current_ty)),
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

fn verify_array_ctor(cx: &mut VerifyCx<'_>, site: VerifySite, ty: TypeId, fields: &[Operand]) {
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

fn verify_list_ctor(cx: &mut VerifyCx<'_>, site: VerifySite, ty: TypeId, fields: &[Operand]) {
    let Some(elem) = typing::list_elem(cx.program, ty) else {
        push_collection_result_mismatch(cx, site, AggregateCtor::List, ty);
        return;
    };
    verify_collection_fields(cx, &site, &AggregateCtor::List, elem, fields);
}

fn verify_map_ctor(cx: &mut VerifyCx<'_>, site: VerifySite, ty: TypeId, fields: &[Operand]) {
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
                &entry[offset],
                expected,
            );
        }
    }
}

fn verify_tuple_ctor(cx: &mut VerifyCx<'_>, site: VerifySite, ty: TypeId, fields: &[Operand]) {
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
        verify_collection_field(cx, &site, &AggregateCtor::Tuple, field, operand, expected);
    }
}

fn verify_collection_fields(
    cx: &mut VerifyCx<'_>,
    site: &VerifySite,
    ctor: &AggregateCtor,
    expected: TypeId,
    fields: &[Operand],
) {
    for (field, operand) in fields.iter().enumerate() {
        verify_collection_field(cx, site, ctor, field, operand, expected);
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
    fields: &[Operand],
    expected: &[TypeId],
    count_mismatch: impl FnOnce(usize, usize) -> VerifyErrorKind,
    mut type_mismatch: impl FnMut(usize, TypeId, TypeId) -> VerifyErrorKind,
) {
    if fields.len() != expected.len() {
        cx.push(site.clone(), count_mismatch(expected.len(), fields.len()));
    }
    for (index, (operand, expected)) in fields.iter().zip(expected).enumerate() {
        if let Some(found) = typing::operand_ty(cx.program, operand)
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
    fields: &[Operand],
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

fn verify_extern_ctor(
    cx: &mut VerifyCx<'_>,
    site: VerifySite,
    extern_id: ExternTypeId,
    ty: TypeId,
    fields: &[Operand],
) {
    let Some(decl) = cx.program.extern_types.get(extern_id.index()) else {
        cx.push(
            site,
            VerifyErrorKind::BadReference(BadReference::InvalidExternType(extern_id)),
        );
        return;
    };
    if cx.type_data(ty) != Some(&TypeData::Extern(extern_id)) {
        cx.push(
            site.clone(),
            VerifyErrorKind::BadRValue(BadRValue::ExternCtorResultTypeMismatch {
                extern_id,
                found: ty,
            }),
        );
    }
    if decl.rep != ExternRep::Inline {
        cx.push(
            site,
            VerifyErrorKind::BadRValue(BadRValue::ExternCtorUnavailable { extern_id }),
        );
        return;
    }
    let Some(expected_fields) = decl.constructor_fields() else {
        cx.push(
            site,
            VerifyErrorKind::BadRValue(BadRValue::ExternCtorUnavailable { extern_id }),
        );
        return;
    };
    let expected_fields = expected_fields
        .map(|(_, field)| field.ty)
        .collect::<Vec<_>>();
    verify_ordered_fields(
        cx,
        &site,
        fields,
        &expected_fields,
        |expected, found| {
            VerifyErrorKind::BadRValue(BadRValue::ExternCtorFieldCountMismatch {
                extern_id,
                expected,
                found,
            })
        },
        |field, expected, found| {
            VerifyErrorKind::BadRValue(BadRValue::ExternCtorFieldTypeMismatch {
                extern_id,
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
    fields: &[Operand],
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
        CallArg::Value(op) => verify_operand(cx, function_id, block_id, stmt_index, op),
        CallArg::SharedBorrow(place) => {
            verify_place(cx, function_id, block_id, stmt_index, place);
        }
        CallArg::MutBorrow(place) => {
            let site = VerifyCx::stmt_site(function_id, block_id, stmt_index.unwrap_or(0));
            verify_place(cx, function_id, block_id, stmt_index, place);
            verify_mutable_place(cx, function_id, &site, place);
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
        CallArg::Value(operand) => operand_function_escape(program, function_id, operand, state),
        CallArg::SharedBorrow(place) | CallArg::MutBorrow(place) => {
            type_function_capability(program, place.ty)
        }
        CallArg::SharedStringConst(_) => FunctionValueCapability::NonFunction,
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
            PlaceRoot::ScopedBorrow(_) | PlaceRoot::CaptureCell(_) | PlaceRoot::Global(_) => {
                type_function_capability(program, place.ty)
            }
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
        program.type_arena.get(lambda_capture_decl_ty(capture)),
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

fn rvalue_function_escape(
    program: &Program,
    function_id: FunctionId,
    primitives: &PrimitiveTypes,
    value: &RValue,
    state: &LocalInit,
) -> FunctionValueCapability {
    match value {
        RValue::Use(operand) => operand_function_escape(program, function_id, operand, state),
        RValue::FunctionRef { .. } => FunctionValueCapability::Escaping,
        RValue::MakeLambda { lambda, .. } => {
            program
                .lambdas
                .get(lambda.index())
                .map_or(FunctionValueCapability::Unknown, |decl| {
                    if decl.escape == LambdaEscape::Escaping {
                        FunctionValueCapability::Escaping
                    } else {
                        FunctionValueCapability::NonEscaping
                    }
                })
        }
        _ => typing::rvalue_ty(program, primitives, value)
            .map_or(FunctionValueCapability::Unknown, |ty| {
                type_function_capability(program, ty)
            }),
    }
}

fn type_function_capability(program: &Program, ty: TypeId) -> FunctionValueCapability {
    if matches!(program.type_arena.get(ty), Some(TypeData::Function(_))) {
        FunctionValueCapability::Unknown
    } else {
        FunctionValueCapability::NonFunction
    }
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
    let borrow_conflict = matches!(
        (left.mode(), right.mode()),
        (ParamMode::SharedBorrow, ParamMode::MutBorrow)
            | (
                ParamMode::MutBorrow,
                ParamMode::SharedBorrow | ParamMode::MutBorrow
            )
    );
    borrow_conflict
        && left
            .place()
            .zip(right.place())
            .is_some_and(|(left, right)| program.places_may_overlap(function_id, left, right))
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
        TypeData::Dyn(contract) => {
            if contract.display_name.is_empty() || contract.method_table_key.is_empty() {
                cx.push(site, VerifyErrorKind::BadType(BadType::EmptyDynContract));
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
