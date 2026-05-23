use anvyx_frontend::air::{
    CallArg, Callee, FunctionId, LocalId, Operand, ParamMode, Place, TypeId,
};

#[derive(Debug, Clone, PartialEq)]
pub struct VirProgram {
    pub functions: Vec<VirFunction>,
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
    pub args: Vec<VirCallArg>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VirParamMode {
    Value,
    SharedBorrow,
    MutBorrow,
}

impl From<ParamMode> for VirParamMode {
    fn from(mode: ParamMode) -> Self {
        match mode {
            ParamMode::Value => Self::Value,
            ParamMode::SharedBorrow => Self::SharedBorrow,
            ParamMode::MutBorrow => Self::MutBorrow,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum VirCallArg {
    Value(Operand),
    SharedBorrow(Place),
    MutBorrow(Place),
}

impl From<&CallArg> for VirCallArg {
    fn from(arg: &CallArg) -> Self {
        match arg {
            CallArg::Value(operand) => Self::Value(operand.clone()),
            CallArg::SharedBorrow(place) => Self::SharedBorrow(place.clone()),
            CallArg::MutBorrow(place) => Self::MutBorrow(place.clone()),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VirParam {
    pub local: LocalId,
    pub ty: TypeId,
    pub mode: VirParamMode,
}
