// exports body type used in Function
use anvyx_externs::{ExternBindingKey, ExternEffects, ExternTypeKey, ProviderId};

pub use super::body::AirBody;
use super::ids::*;
use crate::{
    air::types::{BinaryOp, ConstValue, ParamMode, ParamType, ReturnMode, UnaryOp},
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
    pub specialization: Option<FunctionSpecialization>,
    pub signature: Signature,
    pub locals: Vec<Local>,
    pub body: AirBody,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionSpecialization {
    pub type_args: Vec<TypeId>,
    pub const_args: Vec<ConstValue>,
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

impl Param {
    pub fn param_type(&self) -> ParamType {
        ParamType {
            ty: self.ty,
            mode: self.mode,
        }
    }
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum EnumRepr {
    Adt,
    RawInt,
    RawString,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum RawEnumValue {
    Int(i64),
    String(String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnumDecl {
    pub name: Ident,
    pub module: ModuleId,
    pub type_args: Vec<TypeId>,
    pub const_args: Vec<String>,
    pub core: Option<CoreEnumKind>,
    pub repr: EnumRepr,
    pub raw_type: Option<TypeId>,
    pub variants: Vec<VariantDecl>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CoreEnumKind {
    Option,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VariantDecl {
    pub name: Ident,
    pub shape: VariantShape,
    pub raw_value: Option<RawEnumValue>,
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
    pub binding: Option<ExternBindingDecl>,
    pub effects: ExternEffects,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExternBindingDecl {
    pub package: crate::resolve::PackageId,
    pub provider: ProviderId,
    pub key: ExternBindingKey,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExternTypeBindingDecl {
    pub package: crate::resolve::PackageId,
    pub provider: ProviderId,
    pub key: ExternTypeKey,
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

impl ExternDecl {
    pub fn call_params(&self) -> impl Iterator<Item = ParamType> + '_ {
        self.receiver_param()
            .into_iter()
            .chain(self.params.iter().map(ExternParamDecl::param_type))
    }

    pub fn call_arity(&self) -> usize {
        self.params.len() + usize::from(self.receiver_param().is_some())
    }

    pub fn call_param(&self, index: usize) -> Option<ParamType> {
        match self.receiver_param() {
            Some(receiver) if index == 0 => Some(receiver),
            Some(_) => self.params.get(index - 1).map(ExternParamDecl::param_type),
            None => self.params.get(index).map(ExternParamDecl::param_type),
        }
    }

    fn receiver_param(&self) -> Option<ParamType> {
        match &self.member {
            ExternMember::FieldGetter { receiver, .. }
            | ExternMember::FieldSetter { receiver, .. }
            | ExternMember::Method { receiver, .. }
            | ExternMember::UnaryOperator { receiver, .. }
            | ExternMember::BinaryOperator { receiver, .. } => Some(receiver.param_type()),
            ExternMember::FreeFunction
            | ExternMember::StaticMethod { .. }
            | ExternMember::Init { .. } => None,
        }
    }
}

impl ExternParamDecl {
    pub fn param_type(&self) -> ParamType {
        ParamType {
            ty: self.ty,
            mode: self.mode,
        }
    }
}

impl ExternReceiverDecl {
    pub fn param_type(&self) -> ParamType {
        ParamType {
            ty: self.ty,
            mode: self.mode,
        }
    }
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
    pub binding: Option<ExternTypeBindingDecl>,
    pub type_args: Vec<TypeId>,
    pub const_args: Vec<String>,
    pub rep: ExternRep,
    pub has_init: bool,
    pub init_fields: Vec<FieldId>,
    pub fields: Vec<ExternFieldDecl>,
    pub methods: Vec<ExternMethodDecl>,
    pub statics: Vec<ExternStaticDecl>,
    pub operators: Vec<ExternOpDecl>,
}

impl ExternTypeDecl {
    pub fn constructor_fields(&self) -> Option<impl Iterator<Item = (FieldId, &ExternFieldDecl)>> {
        self.has_init.then(|| {
            self.init_fields
                .iter()
                .filter_map(|field| self.fields.get(field.index()).map(|decl| (*field, decl)))
        })
    }
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
    pub readable: bool,
    pub writable: bool,
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
