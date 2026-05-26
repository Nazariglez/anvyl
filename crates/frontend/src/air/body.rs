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
    Init { local: LocalId, value: RValue },
    Assign { dst: Place, value: RValue },
    Eval(RValue),
    If(AirIf),
    Loop(AirLoop),
    EnumMatch(AirEnumMatch),
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
pub struct Place {
    pub root: LocalId,
    pub projection: Vec<Projection>,
    pub ty: TypeId,
}

impl Place {
    pub(crate) fn may_overlap(&self, other: &Self) -> bool {
        if self.root != other.root {
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
    TupleField(u16),
    VariantField {
        enum_id: EnumId,
        variant: VariantId,
        field: u16,
    },
    Index(LocalId),
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
    ListSlice {
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
    MakeClosure {
        func: FunctionId,
        captures: Vec<Operand>,
        ty: TypeId,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Callee {
    Function(FunctionId),
    Extern(ExternId),
    Closure(Operand),
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
            Self::Init { value, .. } | Self::Assign { value, .. } | Self::Eval(value) => f(value),
            Self::If(branch) => {
                branch.then_block.for_each_rvalue(f);
                if let Some(block) = &branch.else_block {
                    block.for_each_rvalue(f);
                }
            }
            Self::Loop(loop_) => loop_.body.for_each_rvalue(f),
            Self::EnumMatch(match_) => {
                for arm in &match_.arms {
                    arm.block.for_each_rvalue(f);
                }
                if let Some(block) = &match_.else_block {
                    block.for_each_rvalue(f);
                }
            }
        }
    }
}
