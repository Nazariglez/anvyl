use super::{ids::*, types::ParamMode};
use crate::ast::{BinaryOp, FormatSpec, UnaryOp};

#[derive(Debug, Clone, PartialEq)]
pub struct AirBody {
    pub block: AirBlock,
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct AirBlock {
    pub stmts: Vec<AirStmt>,
    pub tail: AirTail,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AirStmt {
    Init {
        local: LocalId,
        value: RValue,
    },
    Assign {
        dst: Place,
        value: RValue,
    },
    Eval(RValue),
    GlobalEnsure {
        global: GlobalId,
    },
    GlobalSetRoot {
        global: GlobalId,
        value: RValue,
        init: GlobalInitEffect,
    },
    GlobalUpdateRoot {
        global: GlobalId,
        value: RValue,
    },
    If(AirIf),
    Loop(AirLoop),
    CollectionLoan(AirCollectionLoan),
    CollectionSlotScope(AirCollectionSlotScope),
    EnumMatch(AirEnumMatch),
    OptionalMatch(AirOptionalMatch),
    MapEntryMatch(AirMapEntryMatch),
}

#[derive(Debug, Clone, PartialEq, Default)]
pub enum AirTail {
    #[default]
    None,
    Return(Option<Operand>),
    Break(AirLoopId),
    Continue(AirLoopId),
    Unreachable,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GlobalInitEffect {
    InitializeFirst,
    StoreWithoutInit,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AirIf {
    pub cond: Operand,
    pub then_block: AirBlock,
    pub else_block: Option<AirBlock>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AirLoop {
    pub id: AirLoopId,
    pub body: AirBlock,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AirCollectionLoan {
    pub root: Place,
    pub root_kind: AirCollectionRootKind,
    pub mode: AirCollectionLoanMode,
    pub body: AirBlock,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AirCollectionSlotScope {
    pub root: Place,
    pub index: LocalId,
    pub slots: Vec<AirCollectionSlot>,
    pub body: AirBlock,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AirCollectionRootKind {
    List,
    FixedArray,
    Slice,
    Map,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AirCollectionLoanMode {
    ReadonlySequence,
    MutableSequenceElement,
    ReadonlyMap,
    MutableMapValue,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct AirCollectionSlot {
    pub kind: AirCollectionSlotKind,
    pub local: LocalId,
    pub ty: TypeId,
    pub mutable: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AirCollectionSlotKind {
    SequenceElement,
    MapValue,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AirEnumMatch {
    pub discr: Place,
    pub arms: Vec<AirEnumMatchArm>,
    pub else_block: Option<AirBlock>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AirEnumMatchArm {
    pub variant: VariantId,
    pub block: AirBlock,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AirOptionalMatch {
    pub discr: Place,
    pub payload: Option<LocalId>,
    pub payload_ref: bool,
    pub payload_escapes: bool,
    pub some_block: AirBlock,
    pub none_block: AirBlock,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AirMapEntryMatch {
    pub map: Place,
    pub key: Operand,
    pub payload: Option<LocalId>,
    pub payload_escapes: bool,
    pub some_block: AirBlock,
    pub none_block: AirBlock,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Place {
    pub root: PlaceRoot,
    pub projection: Vec<Projection>,
    pub ty: TypeId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PlaceRoot {
    Local(LocalId),
    LambdaCapture(LambdaCaptureSlotId),
    ScopedBorrow(ScopedBorrowId),
    CaptureCell(CaptureCellId),
    Global(GlobalId),
}

impl PlaceRoot {
    pub fn local(self) -> Option<LocalId> {
        match self {
            Self::Local(local) => Some(local),
            Self::LambdaCapture(_)
            | Self::ScopedBorrow(_)
            | Self::CaptureCell(_)
            | Self::Global(_) => None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PlaceReadLocal {
    Root(LocalId),
    Index(LocalId),
}

impl Place {
    pub(crate) fn for_each_read_local(&self, f: &mut impl FnMut(PlaceReadLocal)) {
        if let PlaceRoot::Local(local) = self.root {
            f(PlaceReadLocal::Root(local));
        }
        for projection in &self.projection {
            match projection {
                Projection::Index(local) => f(PlaceReadLocal::Index(*local)),
                Projection::Field(_)
                | Projection::TupleField(_)
                | Projection::VariantField { .. } => {}
            }
        }
    }

    pub(crate) fn may_overlap(&self, other: &Self) -> bool {
        if !place_roots_may_overlap(self.root, other.root) {
            return false;
        }
        for (left, right) in self.projection.iter().zip(&other.projection) {
            if projections_equal(left, right) {
                continue;
            }
            return projection_may_overlap(left) || projection_may_overlap(right);
        }
        true
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Projection {
    Field(FieldId),
    TupleField(u32),
    VariantField {
        enum_id: EnumId,
        variant: VariantId,
        field: u16,
    },
    Index(LocalId),
}

fn place_roots_may_overlap(left: PlaceRoot, right: PlaceRoot) -> bool {
    match (left, right) {
        (PlaceRoot::Local(left), PlaceRoot::Local(right)) => left == right,
        (PlaceRoot::CaptureCell(left), PlaceRoot::CaptureCell(right)) => left == right,
        _ => true,
    }
}

fn projections_equal(left: &Projection, right: &Projection) -> bool {
    match (left, right) {
        (Projection::Field(left), Projection::Field(right)) => left == right,
        (Projection::TupleField(left), Projection::TupleField(right)) => left == right,
        (
            Projection::VariantField {
                enum_id: left_enum,
                variant: left_variant,
                field: left_field,
            },
            Projection::VariantField {
                enum_id: right_enum,
                variant: right_variant,
                field: right_field,
            },
        ) => left_enum == right_enum && left_variant == right_variant && left_field == right_field,
        (Projection::Index(left), Projection::Index(right)) => left == right,
        _ => false,
    }
}

fn projection_may_overlap(projection: &Projection) -> bool {
    matches!(projection, Projection::Index(_))
}

#[derive(Debug, Clone, PartialEq)]
pub enum Operand {
    Place(Place),
    Const(ConstId),
}

#[derive(Debug, Clone, PartialEq)]
pub enum CallArg {
    Value(Operand),
    SharedBorrow(Place),
    SharedStringConst(ConstId),
    MutBorrow(Place),
}

impl CallArg {
    pub fn mode(&self) -> ParamMode {
        match self {
            Self::Value(_) => ParamMode::Value,
            Self::SharedBorrow(_) | Self::SharedStringConst(_) => ParamMode::SharedBorrow,
            Self::MutBorrow(_) => ParamMode::MutBorrow,
        }
    }

    pub(crate) fn place(&self) -> Option<&Place> {
        match self {
            Self::Value(Operand::Place(place))
            | Self::SharedBorrow(place)
            | Self::MutBorrow(place) => Some(place),
            Self::Value(Operand::Const(_)) | Self::SharedStringConst(_) => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum LambdaCaptureArg {
    NoRuntime,
    ReadonlyLocal { value: Operand },
    ScopedLocal { place: Place },
    ScopedBorrow { place: Place },
    CaptureCell { cell: CaptureCellId },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MapWriteKind {
    IndexedAssignment,
    StructuralInsert,
}

#[derive(Debug, Clone, PartialEq)]
pub enum RValue {
    Use(Operand),
    Unary {
        op: UnaryOp,
        value: Operand,
        ty: TypeId,
    },
    Binary {
        op: BinaryOp,
        lhs: Operand,
        rhs: Operand,
        ty: TypeId,
    },
    SharedRefEq {
        lhs: Operand,
        rhs: Operand,
        negated: bool,
    },
    OptionalSome {
        value: Operand,
        ty: TypeId,
    },
    Cast {
        value: Operand,
        target: TypeId,
    },
    Aggregate {
        kind: AggregateCtor,
        fields: Vec<Operand>,
        ty: TypeId,
    },
    Call {
        callee: Callee,
        args: Vec<CallArg>,
    },
    Stringify {
        value: Operand,
        source_ty: TypeId,
    },
    StringConcat {
        parts: Vec<Operand>,
    },
    Format {
        value: Operand,
        spec: FormatSpec,
    },
    Len {
        source: Place,
    },
    ListPush {
        list: Place,
        value: Operand,
    },
    ListPop {
        list: Place,
        ty: TypeId,
    },
    RangeListCopy {
        source: Place,
        start: LocalId,
        end: LocalId,
        inclusive: bool,
        ty: TypeId,
    },
    MapGet {
        map: Place,
        key: Operand,
        ty: TypeId,
    },
    MapInsert {
        map: Place,
        key: Operand,
        value: Operand,
        kind: MapWriteKind,
    },
    MapRemove {
        map: Place,
        key: Operand,
        ty: TypeId,
    },
    MapEntryAt {
        map: Place,
        index: LocalId,
        ty: TypeId,
    },
    SliceView {
        source: Place,
        start: LocalId,
        end: LocalId,
        inclusive: bool,
        ty: TypeId,
    },
    FunctionRef {
        function: FunctionId,
        ty: TypeId,
    },
    MakeLambda {
        lambda: LambdaId,
        captures: Vec<LambdaCaptureArg>,
        ty: TypeId,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Callee {
    Function(FunctionId),
    Extern(ExternId),
    Lambda(Operand),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AggregateCtor {
    Struct(AggregateId),
    Extern(ExternTypeId),
    Tuple,
    EnumVariant { enum_id: EnumId, variant: VariantId },
    List,
    Array,
    Map,
    DataRef(AggregateId),
}

impl AirBody {
    pub fn for_each_rvalue(&self, f: &mut impl FnMut(&RValue)) {
        self.block.for_each_rvalue(f);
    }
}

impl AirBlock {
    pub fn for_each_rvalue(&self, f: &mut impl FnMut(&RValue)) {
        for stmt in &self.stmts {
            stmt.for_each_rvalue(f);
        }
    }
}

impl AirStmt {
    pub fn for_each_rvalue(&self, f: &mut impl FnMut(&RValue)) {
        match self {
            Self::Init { value, .. }
            | Self::Assign { value, .. }
            | Self::Eval(value)
            | Self::GlobalSetRoot { value, .. }
            | Self::GlobalUpdateRoot { value, .. } => f(value),
            Self::GlobalEnsure { .. } => {}
            Self::If(branch) => {
                branch.then_block.for_each_rvalue(f);
                if let Some(block) = &branch.else_block {
                    block.for_each_rvalue(f);
                }
            }
            Self::Loop(loop_) => loop_.body.for_each_rvalue(f),
            Self::CollectionLoan(loan) => loan.body.for_each_rvalue(f),
            Self::CollectionSlotScope(scope) => scope.body.for_each_rvalue(f),
            Self::EnumMatch(match_) => {
                for arm in &match_.arms {
                    arm.block.for_each_rvalue(f);
                }
                if let Some(block) = &match_.else_block {
                    block.for_each_rvalue(f);
                }
            }
            Self::OptionalMatch(match_) => {
                match_.some_block.for_each_rvalue(f);
                match_.none_block.for_each_rvalue(f);
            }
            Self::MapEntryMatch(match_) => {
                match_.some_block.for_each_rvalue(f);
                match_.none_block.for_each_rvalue(f);
            }
        }
    }
}
