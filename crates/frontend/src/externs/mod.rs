use crate::ast::{BinaryOp, Type, UnaryOp};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum ExternRep {
    #[default]
    Shared,
    Inline,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExternProvider {
    pub name: String,
    pub types: Vec<ExternTypeDecl>,
    pub functions: Vec<ExternFuncDecl>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExternTypeDecl {
    pub name: String,
    pub doc: Option<String>,
    pub rep: ExternRep,
    pub has_init: bool,
    pub fields: Vec<ExternFieldDecl>,
    pub methods: Vec<ExternMethodDecl>,
    pub statics: Vec<ExternStaticDecl>,
    pub operators: Vec<ExternOpDecl>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExternFuncDecl {
    pub name: String,
    pub params: Vec<ExternParam>,
    pub ret: Type,
    pub doc: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExternFieldDecl {
    pub name: String,
    pub ty: Type,
    pub computed: bool,
    pub doc: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExternMethodDecl {
    pub name: String,
    pub doc: Option<String>,
    pub receiver: MethodReceiver,
    pub params: Vec<ExternParam>,
    pub ret: Type,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExternStaticDecl {
    pub name: String,
    pub doc: Option<String>,
    pub params: Vec<ExternParam>,
    pub ret: Type,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExternOpDecl {
    pub kind: ExternOp,
    pub operand: Option<Type>,
    pub ret: Type,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExternOp {
    Binary(BinaryOp),
    Unary(UnaryOp),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExternParam {
    pub name: String,
    pub ty: Type,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MethodReceiver {
    Value,
    Var,
}
