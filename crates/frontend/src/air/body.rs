use super::{
    ids::*,
    types::{FunctionValueCapability, ParamMode},
};
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
    RangeFor(AirRangeFor),
    CollectionFor(AirCollectionFor),
    CollectionLoan(AirCollectionLoan),
    CollectionSlotScope(AirCollectionSlotScope),
    PatternMatch(AirPatternMatch),
    DynMatch(AirDynMatch),
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

#[derive(Debug, Clone, PartialEq, Default)]
pub struct AirOrdinalPlan {
    pub adapters: Vec<AirOrdinalAdapter>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AirOrdinalAdapter {
    Rev,
    Skip { count: Operand },
    Take { count: Operand },
    StepBy { step: Operand },
}

#[derive(Debug, Clone, PartialEq)]
pub struct AirRangeFor {
    pub id: AirLoopId,
    pub start: Operand,
    pub end: Operand,
    pub ordinal_plan: AirOrdinalPlan,
    pub inclusive: bool,
    pub ordinal: Option<LocalId>,
    pub item: LocalId,
    pub body: AirBlock,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AirCollectionFor {
    pub id: AirLoopId,
    pub len: LocalId,
    pub ordinal_plan: AirOrdinalPlan,
    pub index: LocalId,
    pub ordinal: Option<LocalId>,
    pub body: AirBlock,
}

impl AirOrdinalPlan {
    pub fn operands(&self) -> impl Iterator<Item = &Operand> {
        self.adapters.iter().filter_map(|adapter| match adapter {
            AirOrdinalAdapter::Rev => None,
            AirOrdinalAdapter::Skip { count }
            | AirOrdinalAdapter::Take { count }
            | AirOrdinalAdapter::StepBy { step: count } => Some(count),
        })
    }
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
pub struct AirDynMatch {
    pub source: AirDynMatchSource,
    pub surface: ContractSurfaceId,
    pub arms: Vec<AirDynMatchArm>,
    pub fallback: AirDynMatchFallback,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AirDynMatchSource {
    Owned { value: Operand, use_: DynOwnedUse },
    Borrowed(DynBorrow),
    Mutable(Place),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DynOwnedUse {
    ConsumeTemporary,
    ReusableRead,
}

impl DynOwnedUse {
    fn value_use(self) -> ValueUse {
        match self {
            Self::ConsumeTemporary => ValueUse::Consume,
            Self::ReusableRead => ValueUse::Read,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct AirDynMatchArm {
    pub target: TypeId,
    pub binding: AirDynMatchTargetBinding,
    pub block: AirBlock,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AirDynMatchTargetBinding {
    Discard,
    Take(LocalId),
    Materialize(LocalId),
    Alias(LocalId),
}

impl AirDynMatchTargetBinding {
    pub fn local(self) -> Option<LocalId> {
        match self {
            Self::Discard => None,
            Self::Take(local) | Self::Materialize(local) | Self::Alias(local) => Some(local),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct AirDynMatchFallback {
    pub binding: AirDynMatchFallbackBinding,
    pub block: AirBlock,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AirDynMatchFallbackBinding {
    Discard,
    Preserve(LocalId),
    Alias(LocalId),
}

impl AirDynMatchFallbackBinding {
    pub fn local(self) -> Option<LocalId> {
        match self {
            Self::Discard => None,
            Self::Preserve(local) | Self::Alias(local) => Some(local),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct AirPatternMatch {
    pub subject: Place,
    pub arms: Vec<AirPatternArm>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AirPatternArm {
    pub alternatives: Vec<AirPatternAlternative>,
    pub block: AirBlock,
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct AirPatternAlternative {
    pub tests: Vec<AirPatternTest>,
    pub bindings: Vec<AirPatternBinding>,
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct AirPatternPath {
    pub steps: Vec<AirPatternPathStep>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AirPatternPathStep {
    Field(FieldId),
    TupleField(u32),
    OptionalSome,
    EnumTupleField {
        enum_id: EnumId,
        variant: VariantId,
        field: u16,
    },
    EnumStructField {
        enum_id: EnumId,
        variant: VariantId,
        field: u16,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AirPatternTest {
    Literal {
        path: AirPatternPath,
        value: ConstId,
    },
    Nil {
        path: AirPatternPath,
    },
    OptionalSome {
        path: AirPatternPath,
    },
    EnumVariant {
        path: AirPatternPath,
        enum_id: EnumId,
        variant: VariantId,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AirPatternBinding {
    pub local: LocalId,
    pub path: AirPatternPath,
    pub ty: TypeId,
    pub mode: AirPatternBindingMode,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AirPatternBindingMode {
    Owned,
    Alias,
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

#[derive(Debug, Clone, PartialEq, Eq)]
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
    DynBorrowParam(DynBorrowParamId),
    CaptureCell(CaptureCellId),
    Global(GlobalId),
}

impl PlaceRoot {
    pub fn local(self) -> Option<LocalId> {
        match self {
            Self::Local(local) => Some(local),
            Self::LambdaCapture(_)
            | Self::ScopedBorrow(_)
            | Self::DynBorrowParam(_)
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
                Projection::Field(_) | Projection::TupleField(_) => {}
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
    InitFieldProvided(Operand),
    InitFieldOmitted,
    SharedBorrow(Place),
    SharedStringConst(ConstId),
    MutBorrow(Place),
    DynBorrow(DynBorrow),
}

impl CallArg {
    pub fn mode(&self) -> ParamMode {
        match self {
            Self::Value(_) | Self::InitFieldProvided(_) | Self::InitFieldOmitted => {
                ParamMode::Value
            }
            Self::SharedBorrow(_) | Self::SharedStringConst(_) => ParamMode::SharedBorrow,
            Self::MutBorrow(_) | Self::DynBorrow(_) => ParamMode::MutBorrow,
        }
    }

    pub(crate) fn place(&self) -> Option<&Place> {
        match self {
            Self::Value(Operand::Place(place))
            | Self::InitFieldProvided(Operand::Place(place))
            | Self::SharedBorrow(place)
            | Self::MutBorrow(place) => Some(place),
            Self::DynBorrow(borrow) => Some(borrow.place()),
            Self::Value(Operand::Const(_))
            | Self::InitFieldProvided(Operand::Const(_))
            | Self::InitFieldOmitted
            | Self::SharedStringConst(_) => None,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IterCountCheck {
    SkipNonNegative,
    TakeNonNegative,
    StepByPositive,
}

#[derive(Debug, Clone, PartialEq)]
pub enum RValue {
    Use(Operand),
    DynPack {
        value: Operand,
        use_: DynOwnedUse,
        witness: ContractWitnessId,
        ty: TypeId,
    },
    DynWeaken {
        value: Operand,
        use_: DynOwnedUse,
        weakening: ContractWeakeningId,
        ty: TypeId,
    },
    DynDowncast {
        value: Operand,
        use_: DynOwnedUse,
        surface: ContractSurfaceId,
        target: TypeId,
        ty: TypeId,
    },
    DynCall {
        receiver: DynReceiver,
        surface: ContractSurfaceId,
        slot: ContractSlotId,
        args: Vec<CallArg>,
    },
    FunctionValue {
        value: Operand,
        capability: FunctionValueCapability,
    },
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
    CheckedIterCount {
        count: Operand,
        check: IterCountCheck,
    },
    MapEntryAt {
        map: Place,
        index: LocalId,
        ty: TypeId,
    },
    MapKeyAt {
        map: Place,
        index: LocalId,
        ty: TypeId,
    },
    MapValueAt {
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DynBorrowSource {
    Concrete {
        place: Place,
        witness: ContractWitnessId,
    },
    Owned(Place),
    Borrowed(Place),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DynBorrow {
    pub source: DynBorrowSource,
    pub ty: TypeId,
    pub surface: ContractSurfaceId,
    pub weakening: Option<ContractWeakeningId>,
}

impl DynBorrow {
    pub fn place(&self) -> &Place {
        match &self.source {
            DynBorrowSource::Concrete { place, .. }
            | DynBorrowSource::Owned(place)
            | DynBorrowSource::Borrowed(place) => place,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum DynReceiver {
    Owned(Operand),
    MutableOwned(Place),
    Borrowed(DynBorrow),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Callee {
    Function(FunctionId),
    Extern(ExternId),
    Lambda(Operand),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ValueUse {
    Read,
    Store,
    CallValue,
    Consume,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PlaceUse {
    Read,
    Mutate,
    Borrow(ParamMode),
}

#[derive(Debug, Clone, Copy)]
pub enum AirChild<'a> {
    RValue {
        value: &'a RValue,
        use_: ValueUse,
    },
    Operand {
        operand: &'a Operand,
        use_: ValueUse,
    },
    Place {
        place: &'a Place,
        use_: PlaceUse,
    },
    CallArg {
        callee: Option<&'a Callee>,
        index: usize,
        arg: &'a CallArg,
        mode: ParamMode,
    },
    LambdaCapture(&'a LambdaCaptureArg),
    DynBorrow(&'a DynBorrow),
    LocalRead(LocalId),
    Block(&'a AirBlock),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AggregateCtor {
    Struct(AggregateId),
    Tuple,
    EnumVariant { enum_id: EnumId, variant: VariantId },
    List,
    Array,
    Map,
    DataRef(AggregateId),
}

impl AirBody {
    pub fn walk_children(&self, f: &mut impl FnMut(AirChild<'_>)) {
        self.block.walk_children(f);
    }

    pub fn for_each_rvalue(&self, f: &mut impl FnMut(&RValue)) {
        self.walk_children(&mut |child| {
            if let AirChild::RValue { value, .. } = child {
                f(value);
            }
        });
    }
}

impl AirBlock {
    pub fn for_each_child(&self, f: &mut impl FnMut(AirChild<'_>)) {
        for stmt in &self.stmts {
            stmt.for_each_child(f);
        }
        if let AirTail::Return(Some(operand)) = &self.tail {
            f(AirChild::Operand {
                operand,
                use_: ValueUse::Consume,
            });
        }
    }

    pub fn walk_children(&self, f: &mut impl FnMut(AirChild<'_>)) {
        self.for_each_child(&mut |child| walk_child(child, f));
    }
}

fn walk_child(child: AirChild<'_>, f: &mut impl FnMut(AirChild<'_>)) {
    f(child);
    match child {
        AirChild::RValue { value, use_ } => {
            value.for_each_child(use_, &mut |child| walk_child(child, f));
        }
        AirChild::Block(block) => block.for_each_child(&mut |child| walk_child(child, f)),
        AirChild::Operand { .. }
        | AirChild::Place { .. }
        | AirChild::CallArg { .. }
        | AirChild::LambdaCapture(_)
        | AirChild::DynBorrow(_)
        | AirChild::LocalRead(_) => {}
    }
}

impl AirStmt {
    pub fn for_each_child(&self, f: &mut impl FnMut(AirChild<'_>)) {
        match self {
            Self::Init { value, .. }
            | Self::GlobalSetRoot { value, .. }
            | Self::GlobalUpdateRoot { value, .. } => f(AirChild::RValue {
                value,
                use_: ValueUse::Store,
            }),
            Self::Assign { dst, value } => {
                f(AirChild::Place {
                    place: dst,
                    use_: PlaceUse::Mutate,
                });
                f(AirChild::RValue {
                    value,
                    use_: ValueUse::Store,
                });
            }
            Self::Eval(value) => f(AirChild::RValue {
                value,
                use_: ValueUse::Read,
            }),
            Self::GlobalEnsure { .. } => {}
            Self::If(branch) => {
                f(AirChild::Operand {
                    operand: &branch.cond,
                    use_: ValueUse::Read,
                });
                f(AirChild::Block(&branch.then_block));
                if let Some(block) = &branch.else_block {
                    f(AirChild::Block(block));
                }
            }
            Self::Loop(loop_) => f(AirChild::Block(&loop_.body)),
            Self::RangeFor(range) => {
                for operand in [&range.start, &range.end] {
                    f(AirChild::Operand {
                        operand,
                        use_: ValueUse::Read,
                    });
                }
                for operand in range.ordinal_plan.operands() {
                    f(AirChild::Operand {
                        operand,
                        use_: ValueUse::Read,
                    });
                }
                f(AirChild::LocalRead(range.item));
                if let Some(ordinal) = range.ordinal {
                    f(AirChild::LocalRead(ordinal));
                }
                f(AirChild::Block(&range.body));
            }
            Self::CollectionFor(for_) => {
                f(AirChild::LocalRead(for_.len));
                for operand in for_.ordinal_plan.operands() {
                    f(AirChild::Operand {
                        operand,
                        use_: ValueUse::Read,
                    });
                }
                f(AirChild::LocalRead(for_.index));
                if let Some(ordinal) = for_.ordinal {
                    f(AirChild::LocalRead(ordinal));
                }
                f(AirChild::Block(&for_.body));
            }
            Self::CollectionLoan(loan) => {
                let mode = match loan.mode {
                    AirCollectionLoanMode::ReadonlySequence
                    | AirCollectionLoanMode::ReadonlyMap => ParamMode::SharedBorrow,
                    AirCollectionLoanMode::MutableSequenceElement
                    | AirCollectionLoanMode::MutableMapValue => ParamMode::MutBorrow,
                };
                f(AirChild::Place {
                    place: &loan.root,
                    use_: PlaceUse::Borrow(mode),
                });
                f(AirChild::Block(&loan.body));
            }
            Self::CollectionSlotScope(scope) => {
                f(AirChild::Place {
                    place: &scope.root,
                    use_: PlaceUse::Read,
                });
                f(AirChild::LocalRead(scope.index));
                f(AirChild::Block(&scope.body));
            }
            Self::PatternMatch(match_) => {
                let has_alias = match_.arms.iter().any(|arm| {
                    arm.alternatives.iter().any(|alternative| {
                        alternative
                            .bindings
                            .iter()
                            .any(|binding| binding.mode == AirPatternBindingMode::Alias)
                    })
                });
                f(AirChild::Place {
                    place: &match_.subject,
                    use_: if has_alias {
                        PlaceUse::Borrow(ParamMode::MutBorrow)
                    } else {
                        PlaceUse::Read
                    },
                });
                for arm in &match_.arms {
                    f(AirChild::Block(&arm.block));
                }
            }
            Self::DynMatch(match_) => {
                match &match_.source {
                    AirDynMatchSource::Owned { value, use_ } => {
                        emit_operand(f, value, use_.value_use());
                    }
                    AirDynMatchSource::Borrowed(borrow) => f(AirChild::DynBorrow(borrow)),
                    AirDynMatchSource::Mutable(place) => {
                        emit_place(f, place, PlaceUse::Borrow(ParamMode::MutBorrow));
                    }
                }
                for arm in &match_.arms {
                    f(AirChild::Block(&arm.block));
                }
                f(AirChild::Block(&match_.fallback.block));
            }
            Self::OptionalMatch(match_) => {
                f(AirChild::Place {
                    place: &match_.discr,
                    use_: if match_.payload_ref {
                        PlaceUse::Borrow(ParamMode::MutBorrow)
                    } else {
                        PlaceUse::Read
                    },
                });
                f(AirChild::Block(&match_.some_block));
                f(AirChild::Block(&match_.none_block));
            }
            Self::MapEntryMatch(match_) => {
                f(AirChild::Place {
                    place: &match_.map,
                    use_: PlaceUse::Mutate,
                });
                f(AirChild::Operand {
                    operand: &match_.key,
                    use_: ValueUse::Read,
                });
                f(AirChild::Block(&match_.some_block));
                f(AirChild::Block(&match_.none_block));
            }
        }
    }
}

fn emit_operand<'a>(f: &mut impl FnMut(AirChild<'a>), operand: &'a Operand, use_: ValueUse) {
    f(AirChild::Operand { operand, use_ });
}

fn emit_place<'a>(f: &mut impl FnMut(AirChild<'a>), place: &'a Place, use_: PlaceUse) {
    f(AirChild::Place { place, use_ });
}

impl RValue {
    pub fn for_each_child(&self, use_: ValueUse, f: &mut impl FnMut(AirChild<'_>)) {
        match self {
            Self::Use(value) | Self::FunctionValue { value, .. } => emit_operand(f, value, use_),
            Self::DynPack { value, use_, .. }
            | Self::DynWeaken { value, use_, .. }
            | Self::DynDowncast { value, use_, .. } => {
                emit_operand(f, value, use_.value_use());
            }
            Self::DynCall { receiver, args, .. } => {
                match receiver {
                    DynReceiver::Owned(value) => emit_operand(f, value, ValueUse::Read),
                    DynReceiver::MutableOwned(place) => emit_place(f, place, PlaceUse::Mutate),
                    DynReceiver::Borrowed(borrow) => f(AirChild::DynBorrow(borrow)),
                }
                for (index, arg) in args.iter().enumerate() {
                    f(AirChild::CallArg {
                        callee: None,
                        index,
                        arg,
                        mode: arg.mode(),
                    });
                }
            }
            Self::Unary { value, .. }
            | Self::OptionalSome { value, .. }
            | Self::Cast { value, .. }
            | Self::Stringify { value, .. }
            | Self::Format { value, .. }
            | Self::CheckedIterCount { count: value, .. } => emit_operand(f, value, ValueUse::Read),
            Self::Binary { lhs, rhs, .. } | Self::SharedRefEq { lhs, rhs, .. } => {
                emit_operand(f, lhs, ValueUse::Read);
                emit_operand(f, rhs, ValueUse::Read);
            }
            Self::Aggregate { fields, .. } => {
                for field in fields {
                    emit_operand(f, field, ValueUse::Store);
                }
            }
            Self::Call { callee, args } => {
                if let Callee::Lambda(value) = callee {
                    emit_operand(f, value, ValueUse::Read);
                }
                for (index, arg) in args.iter().enumerate() {
                    f(AirChild::CallArg {
                        callee: Some(callee),
                        index,
                        arg,
                        mode: arg.mode(),
                    });
                }
            }
            Self::StringConcat { parts } => {
                for part in parts {
                    emit_operand(f, part, ValueUse::Read);
                }
            }
            Self::Len { source } => emit_place(f, source, PlaceUse::Read),
            Self::ListPush { list, value } => {
                emit_place(f, list, PlaceUse::Mutate);
                emit_operand(f, value, ValueUse::Store);
            }
            Self::ListPop { list, .. } => emit_place(f, list, PlaceUse::Mutate),
            Self::RangeListCopy {
                source, start, end, ..
            } => {
                emit_place(f, source, PlaceUse::Read);
                f(AirChild::LocalRead(*start));
                f(AirChild::LocalRead(*end));
            }
            Self::SliceView {
                source, start, end, ..
            } => {
                emit_place(f, source, PlaceUse::Borrow(ParamMode::SharedBorrow));
                f(AirChild::LocalRead(*start));
                f(AirChild::LocalRead(*end));
            }
            Self::MapGet { map, key, .. } => {
                emit_place(f, map, PlaceUse::Read);
                emit_operand(f, key, ValueUse::Read);
            }
            Self::MapInsert {
                map, key, value, ..
            } => {
                emit_place(f, map, PlaceUse::Mutate);
                emit_operand(f, key, ValueUse::Store);
                emit_operand(f, value, ValueUse::Store);
            }
            Self::MapRemove { map, key, .. } => {
                emit_place(f, map, PlaceUse::Mutate);
                emit_operand(f, key, ValueUse::Read);
            }
            Self::MapEntryAt { map, index, .. }
            | Self::MapKeyAt { map, index, .. }
            | Self::MapValueAt { map, index, .. } => {
                emit_place(f, map, PlaceUse::Read);
                f(AirChild::LocalRead(*index));
            }
            Self::FunctionRef { .. } => {}
            Self::MakeLambda { captures, .. } => {
                for capture in captures {
                    f(AirChild::LambdaCapture(capture));
                }
            }
        }
    }
}
