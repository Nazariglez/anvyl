use anvyx_frontend::air::{CallArg, Callee, ExternId, FunctionId, LocalId, ParamType, TypeId};

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

#[derive(Debug, Clone, PartialEq)]
pub struct VirCall {
    pub callee: Callee,
    pub args: Vec<CallArg>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VirParam {
    pub local: LocalId,
    pub param: ParamType,
}
