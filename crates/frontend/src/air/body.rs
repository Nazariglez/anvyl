use super::ids::*;
use crate::ast::{BinaryOp, FormatSpec, UnaryOp};

#[derive(Debug, Clone, PartialEq)]
pub struct BasicBlock {
    pub statements: Vec<Statement>,
    pub terminator: Terminator,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Place {
    pub root: LocalId,
    pub projection: Vec<Projection>,
    pub ty: TypeId,
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

#[derive(Debug, Clone, PartialEq)]
pub enum Operand {
    Place(Place),
    Const(ConstId),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Init { local: LocalId, value: RValue },
    Assign { dst: Place, value: RValue },
    Eval(RValue),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Terminator {
    Goto(BlockId),
    If {
        cond: Operand,
        then_bb: BlockId,
        else_bb: BlockId,
    },
    SwitchEnum {
        discr: Place,
        arms: Vec<(VariantId, BlockId)>,
        else_bb: Option<BlockId>,
    },
    Return(Option<Operand>),
    Unreachable,
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
        args: Vec<Operand>,
    },
    ToString {
        value: Operand,
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
    Tuple,
    EnumVariant { enum_id: EnumId, variant: VariantId },
    List,
    Array,
    Map,
    DataRef(AggregateId),
}
