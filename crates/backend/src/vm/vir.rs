use anvyx_frontend::air::{
    ConstId, ExternId, FunctionId, LocalId, Operand, ParamType, Place, TypeId,
};

#[derive(Debug, Clone, PartialEq)]
pub struct VirProgram {
    pub functions: Vec<VirFunction>,
    pub externs: Vec<VirExtern>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VirExtern {
    pub source: ExternId,
    pub params: Vec<ParamType>,
    pub ret: TypeId,
}

#[derive(Debug, Clone, PartialEq)]
pub struct VirFunction {
    pub source: FunctionId,
    pub params: Vec<VirParam>,
    pub calls: Vec<VirCall>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VirCallTarget {
    Function(FunctionId),
    Extern(ExternId),
}

#[derive(Debug, Clone, PartialEq)]
pub enum VirCallArg {
    Value(Operand),
    SharedBorrow(Place),
    SharedStringConst(ConstId),
    MutBorrow(Place),
}

#[derive(Debug, Clone, PartialEq)]
pub struct VirCall {
    pub target: VirCallTarget,
    pub args: Vec<VirCallArg>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VirParam {
    pub local: LocalId,
    pub param: ParamType,
}
