// exports body type used in Function
pub use super::body::AirBody;
use super::ids::*;
use crate::{
    air::types::{BinaryOp, ParamMode, ReturnMode, UnaryOp},
    ast::Ident,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Module {
    pub path: Vec<Ident>,
    pub functions: Vec<FunctionId>,
    pub aggregates: Vec<AggregateId>,
    pub enums: Vec<EnumId>,
    pub extern_types: Vec<ExternTypeId>,
    pub externs: Vec<ExternId>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub name: Ident,
    pub module: ModuleId,
    pub kind: FunctionKind,
    pub owner: Option<FunctionOwner>,
    pub signature: Signature,
    pub locals: Vec<Local>,
    pub body: AirBody,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionOwner {
    pub name: Ident,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FunctionKind {
    Normal,
    Method,
    ExtendMethod,
    Closure,
    Helper,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Signature {
    pub params: Vec<Param>,
    pub return_mode: ReturnMode,
}

impl Signature {
    pub fn new(params: Vec<Param>, return_type: TypeId) -> Self {
        Self::with_return_mode(params, ReturnMode::Value(return_type))
    }

    pub fn with_return_mode(params: Vec<Param>, return_mode: ReturnMode) -> Self {
        Self {
            params,
            return_mode,
        }
    }

    pub fn return_type(&self) -> TypeId {
        self.return_mode.ty()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Param {
    pub name: Option<Ident>,
    pub ty: TypeId,
    pub mode: ParamMode,
    pub role: ParamRole,
    pub local_id: LocalId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ParamRole {
    Normal,
    Receiver,
    CaptureEnv,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Local {
    pub name: Option<Ident>,
    pub ty: TypeId,
    pub mutability: Mutability,
    pub kind: LocalKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Mutability {
    Immutable,
    Mutable,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LocalKind {
    Arg,
    Return,
    Temp,
    User,
    PatternBinding,
    Capture,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AggregateDecl {
    pub name: Ident,
    pub module: ModuleId,
    pub kind: AggregateKind,
    pub type_args: Vec<TypeId>,
    pub const_args: Vec<String>,
    pub fields: Vec<FieldDecl>,
    pub cycle_capable: bool,
    pub stringify_override: Option<FunctionId>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FieldDecl {
    pub name: Ident,
    pub ty: TypeId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AggregateKind {
    Struct,
    DataRef,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnumDecl {
    pub name: Ident,
    pub module: ModuleId,
    pub type_args: Vec<TypeId>,
    pub const_args: Vec<String>,
    pub variants: Vec<VariantDecl>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VariantDecl {
    pub name: Ident,
    pub shape: VariantShape,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VariantShape {
    Unit,
    Tuple(Vec<TypeId>),
    Struct(Vec<FieldDecl>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExternDecl {
    pub name: Ident,
    pub module: ModuleId,
    pub member: ExternMember,
    pub params: Vec<ExternParamDecl>,
    pub return_type: TypeId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExternParamDecl {
    pub ty: TypeId,
    pub mode: ParamMode,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ExternReceiverDecl {
    pub ty: TypeId,
    pub mode: ParamMode,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExternMember {
    FreeFunction,
    FieldGetter {
        owner: ExternTypeId,
        receiver: ExternReceiverDecl,
        computed: bool,
    },
    FieldSetter {
        owner: ExternTypeId,
        receiver: ExternReceiverDecl,
    },
    Method {
        owner: ExternTypeId,
        receiver: ExternReceiverDecl,
    },
    StaticMethod {
        owner: ExternTypeId,
    },
    Init {
        owner: ExternTypeId,
    },
    UnaryOperator {
        owner: ExternTypeId,
        receiver: ExternReceiverDecl,
        op: UnaryOp,
    },
    BinaryOperator {
        owner: ExternTypeId,
        receiver: ExternReceiverDecl,
        op: BinaryOp,
        self_on_right: bool,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExternTypeDecl {
    pub name: Ident,
    pub module: ModuleId,
    pub type_args: Vec<TypeId>,
    pub const_args: Vec<String>,
    pub rep: ExternRep,
    pub has_init: bool,
    pub fields: Vec<ExternFieldDecl>,
    pub methods: Vec<ExternMethodDecl>,
    pub statics: Vec<ExternStaticDecl>,
    pub operators: Vec<ExternOpDecl>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExternRep {
    Shared,
    Inline,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExternFieldDecl {
    pub name: Ident,
    pub ty: TypeId,
    pub get_receiver: ExternReceiverDecl,
    pub set_receiver: ExternReceiverDecl,
    pub computed: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExternMethodDecl {
    pub name: Ident,
    pub receiver: ExternReceiverDecl,
    pub params: Vec<ExternParamDecl>,
    pub return_type: TypeId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExternStaticDecl {
    pub name: Ident,
    pub params: Vec<ExternParamDecl>,
    pub return_type: TypeId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExternOpDecl {
    pub kind: ExternOp,
    pub receiver: ExternReceiverDecl,
    pub operand: Option<ExternParamDecl>,
    pub return_type: TypeId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExternOp {
    Binary { op: BinaryOp, self_on_right: bool },
    Unary(UnaryOp),
}
