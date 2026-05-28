use anvyx_frontend::air::{
    CallArg, ExternId, FunctionId, LocalId, Operand, ParamMode, Place, TypeId,
};

#[derive(Debug, Clone, PartialEq)]
pub struct VirProgram {
    pub functions: Vec<VirFunction>,
    pub externs: Vec<VirExtern>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VirExtern {
    pub source: ExternId,
    pub params: Vec<VirExternParam>,
    pub ret: TypeId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VirExternParam {
    pub ty: TypeId,
    pub mode: ParamMode,
}

#[derive(Debug, Clone, PartialEq)]
pub struct VirFunction {
    pub source: FunctionId,
    pub params: Vec<VirParam>,
    pub calls: Vec<VirCall>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct VirCall {
    pub callee: VirCallee,
    pub args: Vec<VirCallArg>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum VirCallee {
    Function(FunctionId),
    Extern(ExternId),
    Closure(Operand),
}

#[derive(Debug, Clone, PartialEq)]
pub enum VirCallArg {
    Value(Operand),
    SharedBorrow(Place),
    SharedStringConst(anvyx_frontend::air::ConstId),
    MutBorrow(Place),
}

impl From<&CallArg> for VirCallArg {
    fn from(arg: &CallArg) -> Self {
        match arg {
            CallArg::Value(operand) => Self::Value(operand.clone()),
            CallArg::SharedBorrow(place) => Self::SharedBorrow(place.clone()),
            CallArg::SharedStringConst(id) => Self::SharedStringConst(*id),
            CallArg::MutBorrow(place) => Self::MutBorrow(place.clone()),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VirParam {
    pub local: LocalId,
    pub ty: TypeId,
    pub mode: ParamMode,
}
