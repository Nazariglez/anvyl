// exports body types used in Function
pub use super::body::{BasicBlock, Terminator};
use super::ids::*;
use crate::{
    air::types::{BinaryOp, UnaryOp},
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
    pub signature: Signature,
    pub locals: Vec<Local>,
    pub body: Vec<BasicBlock>,
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
    pub return_type: TypeId,
}

impl Signature {
    pub fn new(params: Vec<Param>, return_type: TypeId) -> Self {
        Self {
            params,
            return_type,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Param {
    pub name: Option<Ident>,
    pub ty: TypeId,
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
    pub fields: Vec<FieldDecl>,
    pub cycle_capable: bool,
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
    pub params: Vec<TypeId>,
    pub return_type: TypeId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExternMember {
    FreeFunction,
    FieldGetter {
        owner: ExternTypeId,
        computed: bool,
    },
    FieldSetter {
        owner: ExternTypeId,
    },
    Method {
        owner: ExternTypeId,
        receiver_mut: bool,
    },
    StaticMethod {
        owner: ExternTypeId,
    },
    Init {
        owner: ExternTypeId,
    },
    UnaryOperator {
        owner: ExternTypeId,
        op: UnaryOp,
    },
    BinaryOperator {
        owner: ExternTypeId,
        op: BinaryOp,
        self_on_right: bool,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExternTypeDecl {
    pub name: Ident,
    pub module: ModuleId,
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
    pub computed: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExternMethodDecl {
    pub name: Ident,
    pub receiver: MethodReceiver,
    pub params: Vec<TypeId>,
    pub return_type: TypeId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExternStaticDecl {
    pub name: Ident,
    pub params: Vec<TypeId>,
    pub return_type: TypeId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExternOpDecl {
    pub kind: ExternOp,
    pub operand: Option<TypeId>,
    pub return_type: TypeId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExternOp {
    Binary(BinaryOp),
    Unary(UnaryOp),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MethodReceiver {
    Value,
    Mut,
}
