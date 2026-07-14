mod type_walk;

use std::fmt::Display;

use internment::Intern;
pub(crate) use type_walk::{TypeFolder, TypeVisitor};

use crate::span::{Span, Spanned};

pub type ModulePath = std::rc::Rc<[String]>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ModuleOrigin {
    Module(ModulePath),
    SourceFile {
        package: Option<String>,
        path: String,
    },
    Package {
        package: String,
        path: Option<ModulePath>,
    },
    Provider {
        package: String,
        path: ModulePath,
    },
}

impl ModuleOrigin {
    pub fn module_path(&self) -> Option<&[String]> {
        match self {
            Self::Module(path) => Some(path),
            Self::SourceFile { .. } | Self::Package { .. } | Self::Provider { .. } => None,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Hash, Eq)]
pub struct Ident(pub Intern<String>);

impl Ident {
    pub fn new(s: impl Into<String>) -> Self {
        Self(Intern::new(s.into()))
    }

    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }
}

impl Display for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct TypeVarId(pub u32);

impl Display for TypeVarId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "${}", self.0)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct ConstParamId(pub u32);

impl Display for ConstParamId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "$c{}", self.0)
    }
}

#[derive(Debug, Clone)]
pub enum ConstValue {
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),
    Char(char),
}

impl ConstValue {
    pub fn ty(&self) -> Type {
        match self {
            Self::Int(_) => Type::Int,
            Self::Float(_) => Type::Float,
            Self::Bool(_) => Type::Bool,
            Self::String(_) => Type::String,
            Self::Char(_) => Type::Char,
        }
    }
}

impl PartialEq for ConstValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Int(a), Self::Int(b)) => a == b,
            (Self::Float(a), Self::Float(b)) => a.to_bits() == b.to_bits(),
            (Self::Bool(a), Self::Bool(b)) => a == b,
            (Self::String(a), Self::String(b)) => a == b,
            (Self::Char(a), Self::Char(b)) => a == b,
            _ => false,
        }
    }
}

impl Eq for ConstValue {}

impl std::hash::Hash for ConstValue {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::mem::discriminant(self).hash(state);
        match self {
            Self::Int(value) => value.hash(state),
            Self::Float(value) => value.to_bits().hash(state),
            Self::Bool(value) => value.hash(state),
            Self::String(value) => value.hash(state),
            Self::Char(value) => value.hash(state),
        }
    }
}

impl Display for ConstValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Int(value) => write!(f, "{value}"),
            Self::Float(value) => write!(f, "{value}"),
            Self::Bool(value) => write!(f, "{value}"),
            Self::String(value) => write!(f, "{value:?}"),
            Self::Char(value) => write!(f, "{value:?}"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ConstArg {
    Value(ConstValue),
    Name(Ident),
    Param(ConstParamId),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum GenericArg {
    Type(Type),
    Const(ConstArg),
}

impl Display for GenericArg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Type(ty) => write!(f, "{ty}"),
            Self::Const(arg) => write!(f, "{arg}"),
        }
    }
}

impl Display for ConstArg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Value(value) => write!(f, "{value}"),
            Self::Name(name) => write!(f, "{name}"),
            Self::Param(id) => write!(f, "{id}"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ConstExpr {
    Value(ConstValue),
    Param(ConstParamId),
    Unary(UnaryOp, Box<ConstExpr>),
    Binary(BinaryOp, Box<ConstExpr>, Box<ConstExpr>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ArrayLen {
    Fixed(usize),
    Infer,
    Named(Ident),
    Param(ConstParamId),
    Expr(ConstExpr),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub enum EscapeMode {
    #[default]
    NonEscaping,
    Escaping,
}

impl EscapeMode {
    pub fn is_escaping(self) -> bool {
        matches!(self, Self::Escaping)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FuncParam {
    pub ty: Type,
    pub mutable: bool,
    pub cast_accept: bool,
    pub escape: EscapeMode,
}

impl FuncParam {
    pub fn new(ty: Type, mutable: bool, cast_accept: bool, escape: EscapeMode) -> Self {
        Self {
            ty,
            mutable,
            cast_accept,
            escape,
        }
    }

    pub fn immut(ty: Type) -> Self {
        Self {
            ty,
            mutable: false,
            cast_accept: false,
            escape: EscapeMode::NonEscaping,
        }
    }

    #[must_use]
    pub fn with_ty(&self, ty: Type) -> Self {
        Self::new(ty, self.mutable, self.cast_accept, self.escape)
    }

    #[must_use]
    pub fn map_ty(&self, f: impl FnOnce(&Type) -> Type) -> Self {
        self.with_ty(f(&self.ty))
    }
}

impl Display for FuncParam {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.escape.is_escaping() {
            write!(f, "escaping ")?;
        }
        if self.cast_accept {
            write!(f, "as {}", self.ty)
        } else {
            write!(f, "{}", self.ty)
        }
    }
}

impl Display for ConstExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Value(value) => write!(f, "{value}"),
            Self::Param(id) => write!(f, "{id}"),
            Self::Unary(op, expr) => write!(f, "{op}{expr}"),
            Self::Binary(op, left, right) => write!(f, "({left} {op} {right})"),
        }
    }
}

impl Display for ArrayLen {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ArrayLen::Fixed(n) => write!(f, "{n}"),
            ArrayLen::Infer => write!(f, "_"),
            ArrayLen::Named(ident) => write!(f, "{ident}"),
            ArrayLen::Param(id) => write!(f, "{id}"),
            ArrayLen::Expr(expr) => write!(f, "{expr}"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum NominalKind {
    Struct,
    DataRef,
    Enum,
    Extern,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RawEnumBackingConstraint {
    Int,
    String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ExtendTargetConstraint {
    Enum {
        backing: Option<RawEnumBackingConstraint>,
    },
    Struct,
    DataRef,
}

impl RawEnumBackingConstraint {
    pub fn keyword(self) -> &'static str {
        match self {
            Self::Int => "int",
            Self::String => "string",
        }
    }
}

impl ExtendTargetConstraint {
    pub fn nominal_kind(self) -> NominalKind {
        match self {
            Self::Enum { .. } => NominalKind::Enum,
            Self::Struct => NominalKind::Struct,
            Self::DataRef => NominalKind::DataRef,
        }
    }

    pub fn backing(self) -> Option<RawEnumBackingConstraint> {
        match self {
            Self::Enum { backing } => backing,
            Self::Struct | Self::DataRef => None,
        }
    }

    pub fn keyword(self) -> &'static str {
        self.nominal_kind().keyword()
    }
}

#[derive(Debug, Clone)]
pub struct NominalType {
    pub(crate) id: crate::semantic_id::NominalId,
    pub kind: NominalKind,
    pub name: Ident,
    pub type_args: Vec<Type>,
    pub const_args: Vec<ConstArg>,
    pub origin: Option<ModuleOrigin>,
}

impl NominalType {
    pub(crate) fn with_args(&self, type_args: Vec<Type>, const_args: Vec<ConstArg>) -> Type {
        Type::Nominal(Self {
            id: self.id.clone(),
            kind: self.kind,
            name: self.name,
            type_args,
            const_args,
            origin: self.origin.clone(),
        })
    }
}

impl PartialEq for NominalType {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
            && self.type_args == other.type_args
            && self.const_args == other.const_args
    }
}

impl Eq for NominalType {}

impl std::hash::Hash for NominalType {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state);
        self.type_args.hash(state);
        self.const_args.hash(state);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct DynContractHoleId(pub u32);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ContractRef {
    Named {
        qualifier: Option<Ident>,
        name: Ident,
        origin: Option<ModuleOrigin>,
    },
    Anonymous(AnonymousContract),
    Intersection(Vec<ContractRef>),
    Infer,
    Hole(DynContractHoleId),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AnonymousContract {
    pub requirements: Vec<AnonymousContractRequirement>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AnonymousContractRequirement {
    pub receiver: MethodReceiver,
    pub name: Ident,
    pub params: Vec<AnonymousContractParam>,
    pub ret: ReturnSpec,
}

#[derive(Debug, Clone)]
pub struct AnonymousContractParam {
    pub mutable: bool,
    pub escape: EscapeMode,
    pub name: Ident,
    pub ty: Type,
}

impl PartialEq for AnonymousContractParam {
    fn eq(&self, other: &Self) -> bool {
        self.mutable == other.mutable && self.escape == other.escape && self.ty == other.ty
    }
}

impl Eq for AnonymousContractParam {}

impl std::hash::Hash for AnonymousContractParam {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::hash::Hash::hash(&self.mutable, state);
        std::hash::Hash::hash(&self.escape, state);
        std::hash::Hash::hash(&self.ty, state);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ReturnAccess {
    Value,
    Place,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ReturnKind {
    Value(Type),
    Place(Type),
    Infer,
    Iter,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ReturnSpec {
    pub kind: ReturnKind,
}

impl ReturnSpec {
    pub fn value(ty: Type) -> Self {
        Self {
            kind: ReturnKind::Value(ty),
        }
    }

    pub fn place(ty: Type) -> Self {
        Self {
            kind: ReturnKind::Place(ty),
        }
    }

    pub fn infer() -> Self {
        Self {
            kind: ReturnKind::Infer,
        }
    }

    pub fn iter() -> Self {
        Self {
            kind: ReturnKind::Iter,
        }
    }

    pub fn void() -> Self {
        Self::value(Type::Void)
    }

    #[must_use]
    pub fn with_ty(&self, ty: Type) -> Self {
        match self.kind {
            ReturnKind::Value(_) => Self::value(ty),
            ReturnKind::Place(_) => Self::place(ty),
            ReturnKind::Infer => Self::infer(),
            ReturnKind::Iter => Self::iter(),
        }
    }

    pub fn access(&self) -> ReturnAccess {
        match self.kind {
            ReturnKind::Place(_) => ReturnAccess::Place,
            ReturnKind::Value(_) | ReturnKind::Infer | ReturnKind::Iter => ReturnAccess::Value,
        }
    }

    pub fn ty(&self) -> Type {
        match &self.kind {
            ReturnKind::Value(ty) | ReturnKind::Place(ty) => ty.clone(),
            ReturnKind::Infer => Type::InferReturn,
            ReturnKind::Iter => Type::Infer,
        }
    }

    pub fn ty_ref(&self) -> Option<&Type> {
        match &self.kind {
            ReturnKind::Value(ty) | ReturnKind::Place(ty) => Some(ty),
            ReturnKind::Infer | ReturnKind::Iter => None,
        }
    }

    pub fn is_implicit_void(&self) -> bool {
        self.access() == ReturnAccess::Value && self.is_void()
    }

    pub fn is_place(&self) -> bool {
        self.access() == ReturnAccess::Place
    }

    pub fn is_iter(&self) -> bool {
        matches!(self.kind, ReturnKind::Iter)
    }

    pub fn is_void(&self) -> bool {
        matches!(&self.kind, ReturnKind::Value(ty) | ReturnKind::Place(ty) if *ty == Type::Void)
    }

    pub fn is_infer(&self) -> bool {
        matches!(
            self.kind,
            ReturnKind::Infer
                | ReturnKind::Value(Type::InferReturn)
                | ReturnKind::Place(Type::InferReturn)
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Infer,
    InferReturn,
    Any,
    Int,
    Float,
    Bool,
    String,
    Char,
    Void,
    Func {
        params: Vec<FuncParam>,
        ret: Box<ReturnSpec>,
    },
    Dyn(ContractRef),
    Var(TypeVarId),
    UnresolvedName(Ident),
    UnresolvedNominal {
        qualifier: Option<Ident>,
        name: Ident,
        generic_args: Vec<GenericArg>,
    },
    Tuple(Vec<Type>),
    Nominal(NominalType),
    List {
        elem: Box<Type>,
    },
    Array {
        elem: Box<Type>,
        len: ArrayLen,
    },
    Map {
        key: Box<Type>,
        value: Box<Type>,
    },
    Slice {
        elem: Box<Type>,
    },
    Optional {
        inner: Box<Type>,
    },
}

impl Type {
    pub const OPTION_ENUM_NAME: &'static str = "Option";

    pub fn func(params: Vec<FuncParam>, ret: ReturnSpec) -> Self {
        Self::Func {
            params,
            ret: Box::new(ret),
        }
    }

    #[inline]
    pub fn boxed(&self) -> Box<Self> {
        Box::new(self.clone())
    }

    pub fn as_nominal(&self) -> Option<&NominalType> {
        match self {
            Type::Nominal(nominal) => Some(nominal),
            _ => None,
        }
    }

    #[must_use]
    pub fn bare_unresolved_name(&self) -> Option<Ident> {
        match self {
            Type::UnresolvedName(name) => Some(*name),
            Type::UnresolvedNominal {
                qualifier: None,
                name,
                generic_args,
            } if generic_args.is_empty() => Some(*name),
            _ => None,
        }
    }

    pub fn optional_syntax(inner: Type) -> Type {
        Type::Optional {
            inner: Box::new(inner),
        }
    }

    pub(crate) fn scalar_kind(&self) -> Option<ScalarKind> {
        match self {
            Self::Int => Some(ScalarKind::Int),
            Self::Float => Some(ScalarKind::Float),
            Self::Bool => Some(ScalarKind::Bool),
            Self::String => Some(ScalarKind::String),
            Self::Char => Some(ScalarKind::Char),
            _ => None,
        }
    }

    #[inline]
    pub fn is_str(&self) -> bool {
        matches!(self, Type::String)
    }

    #[inline]
    pub fn is_void(&self) -> bool {
        matches!(self, Type::Void)
    }

    #[inline]
    pub fn is_bool(&self) -> bool {
        matches!(self, Type::Bool)
    }

    #[inline]
    pub fn is_int(&self) -> bool {
        matches!(self, Type::Int)
    }

    #[inline]
    pub fn is_float(&self) -> bool {
        matches!(self, Type::Float)
    }

    pub fn as_aggregate(&self) -> Option<AggregateTypeRef<'_>> {
        let Type::Nominal(nominal) = self else {
            return None;
        };
        let kind = match nominal.kind {
            NominalKind::Struct => AggregateKind::Struct,
            NominalKind::DataRef => AggregateKind::DataRef,
            NominalKind::Enum | NominalKind::Extern => return None,
        };
        Some(AggregateTypeRef {
            kind,
            name: nominal.name,
            type_args: &nominal.type_args,
            const_args: &nominal.const_args,
            origin: nominal.origin.as_ref().and_then(ModuleOrigin::module_path),
        })
    }
}

fn fmt_ordered_generic_args(
    f: &mut std::fmt::Formatter<'_>,
    args: &[GenericArg],
) -> std::fmt::Result {
    if args.is_empty() {
        return Ok(());
    }

    write!(f, "<")?;
    for (i, arg) in args.iter().enumerate() {
        if i > 0 {
            write!(f, ", ")?;
        }
        write!(f, "{arg}")?;
    }
    write!(f, ">")
}

fn fmt_generic_args(
    f: &mut std::fmt::Formatter<'_>,
    type_args: &[Type],
    const_args: &[ConstArg],
) -> std::fmt::Result {
    let has_generic_args = !type_args.is_empty() || !const_args.is_empty();
    if !has_generic_args {
        return Ok(());
    }

    write!(f, "<")?;
    let mut sep = "";
    for ty in type_args {
        write!(f, "{sep}{ty}")?;
        sep = ", ";
    }
    for arg in const_args {
        write!(f, "{sep}{arg}")?;
        sep = ", ";
    }
    write!(f, ">")
}

impl Display for ContractRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Named {
                qualifier, name, ..
            } => {
                if let Some(qualifier) = qualifier {
                    write!(f, "{qualifier}.")?;
                }
                write!(f, "{name}")
            }
            Self::Anonymous(surface) => {
                write!(f, "{{ ")?;
                for requirement in &surface.requirements {
                    write!(f, "fn {}(", requirement.name)?;
                    match requirement.receiver {
                        MethodReceiver::Value => write!(f, "self")?,
                        MethodReceiver::Ref => write!(f, "ref self")?,
                    }
                    for param in &requirement.params {
                        write!(f, ", ")?;
                        if param.mutable {
                            write!(f, "ref ")?;
                        }
                        write!(f, "{}: ", param.name)?;
                        if param.escape.is_escaping() {
                            write!(f, "escaping ")?;
                        }
                        write!(f, "{}", param.ty)?;
                    }
                    if requirement.ret.is_implicit_void() {
                        write!(f, "); ")?;
                    } else {
                        write!(f, ") -> {}; ", requirement.ret)?;
                    }
                }
                write!(f, "}}")
            }
            Self::Intersection(contracts) => {
                for (i, contract) in contracts.iter().enumerate() {
                    if i > 0 {
                        write!(f, " + ")?;
                    }
                    write!(f, "{contract}")?;
                }
                Ok(())
            }
            Self::Infer | Self::Hole(_) => write!(f, "_"),
        }
    }
}

impl Display for ReturnSpec {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.is_place() {
            write!(f, "ref ")?;
        }
        match &self.kind {
            ReturnKind::Value(ty) | ReturnKind::Place(ty) => write!(f, "{ty}"),
            ReturnKind::Infer => write!(f, "_"),
            ReturnKind::Iter => write!(f, "iter"),
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Infer | Self::InferReturn => write!(f, "_"),
            Self::Any => write!(f, "any"),
            Self::Int => write!(f, "int"),
            Self::Float => write!(f, "float"),
            Self::Bool => write!(f, "bool"),
            Self::String => write!(f, "string"),
            Self::Char => write!(f, "char"),
            Self::Void => write!(f, "void"),
            Self::Func { params, ret } => {
                write!(f, "fn(")?;
                for (i, p) in params.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    if p.mutable {
                        write!(f, "ref ")?;
                    }
                    write!(f, "{p}")?;
                }
                write!(f, ")")?;
                if !ret.is_implicit_void() {
                    write!(f, " -> {ret}")?;
                }
                Ok(())
            }
            Self::Dyn(contract) => write!(f, "dyn {contract}"),
            Self::Var(id) => write!(f, "{id}"),
            Self::UnresolvedName(ident) => write!(f, "{ident}"),
            Self::UnresolvedNominal {
                qualifier,
                name,
                generic_args,
            } => {
                if let Some(q) = qualifier {
                    write!(f, "{q}.")?;
                }
                write!(f, "{name}")?;
                fmt_ordered_generic_args(f, generic_args)
            }
            Self::Tuple(elems) => {
                write!(f, "(")?;
                for (i, e) in elems.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{e}")?;
                }
                write!(f, ")")
            }
            Self::Nominal(nominal) => {
                write!(f, "{}", nominal.name)?;
                fmt_generic_args(f, &nominal.type_args, &nominal.const_args)
            }
            Self::List { elem } => write!(f, "[{elem}]"),
            Self::Array { elem, len } => write!(f, "[{elem}; {len}]"),
            Self::Map { key, value } => write!(f, "[{key}: {value}]"),
            Self::Slice { elem } => write!(f, "slice[{elem}]"),
            Self::Optional { inner } => {
                if matches!(inner.as_ref(), Type::Func { .. }) {
                    write!(f, "({inner})?")
                } else {
                    write!(f, "{inner}?")
                }
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ScalarKind {
    Int,
    Float,
    Bool,
    String,
    Char,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Eq,
    NotEq,
    LessThan,
    GreaterThan,
    LessThanEq,
    GreaterThanEq,
    And,
    Or,
    Xor,
    BitAnd,
    BitOr,
    Shl,
    Shr,
    Coalesce,
}

impl BinaryOp {
    pub fn precedence(self) -> ExprPrecedence {
        match self {
            Self::Mul | Self::Div | Self::Rem => ExprPrecedence::Multiplicative,
            Self::Add | Self::Sub => ExprPrecedence::Additive,
            Self::Shl | Self::Shr => ExprPrecedence::Shift,
            Self::LessThan | Self::GreaterThan | Self::LessThanEq | Self::GreaterThanEq => {
                ExprPrecedence::Comparison
            }
            Self::Eq | Self::NotEq => ExprPrecedence::Equality,
            Self::BitAnd => ExprPrecedence::BitAnd,
            Self::Xor => ExprPrecedence::Xor,
            Self::BitOr => ExprPrecedence::BitOr,
            Self::And => ExprPrecedence::LogicalAnd,
            Self::Coalesce => ExprPrecedence::Coalesce,
            Self::Or => ExprPrecedence::LogicalOr,
        }
    }

    pub fn scalar_result(self, lhs: ScalarKind, rhs: ScalarKind) -> Option<ScalarKind> {
        use ScalarKind::{Bool, Char, Float, Int, String};

        match self {
            Self::Add | Self::Sub | Self::Mul | Self::Div | Self::Rem
                if lhs == rhs && matches!(lhs, Int | Float) =>
            {
                Some(lhs)
            }
            Self::LessThan | Self::GreaterThan | Self::LessThanEq | Self::GreaterThanEq
                if lhs == rhs && matches!(lhs, Int | Float | String | Char) =>
            {
                Some(Bool)
            }
            Self::Eq | Self::NotEq if lhs == rhs => Some(Bool),
            Self::Xor | Self::BitAnd | Self::BitOr | Self::Shl | Self::Shr
                if lhs == Int && rhs == Int =>
            {
                Some(Int)
            }
            Self::And | Self::Or if lhs == Bool && rhs == Bool => Some(Bool),
            _ => None,
        }
    }
}

impl Display for BinaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinaryOp::Add => write!(f, "+"),
            BinaryOp::Sub => write!(f, "-"),
            BinaryOp::Mul => write!(f, "*"),
            BinaryOp::Div => write!(f, "/"),
            BinaryOp::Rem => write!(f, "%"),
            BinaryOp::Eq => write!(f, "=="),
            BinaryOp::NotEq => write!(f, "!="),
            BinaryOp::LessThan => write!(f, "<"),
            BinaryOp::GreaterThan => write!(f, ">"),
            BinaryOp::LessThanEq => write!(f, "<="),
            BinaryOp::GreaterThanEq => write!(f, ">="),
            BinaryOp::And => write!(f, "&&"),
            BinaryOp::Or => write!(f, "||"),
            BinaryOp::Xor => write!(f, "^"),
            BinaryOp::BitAnd => write!(f, "&"),
            BinaryOp::BitOr => write!(f, "|"),
            BinaryOp::Shl => write!(f, "<<"),
            BinaryOp::Shr => write!(f, ">>"),
            BinaryOp::Coalesce => write!(f, "??"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UnaryOp {
    Neg,
    Not,
    BitNot,
}

impl UnaryOp {
    pub fn scalar_result(self, value: ScalarKind) -> Option<ScalarKind> {
        match (self, value) {
            (Self::Neg, ScalarKind::Int | ScalarKind::Float)
            | (Self::Not, ScalarKind::Bool)
            | (Self::BitNot, ScalarKind::Int) => Some(value),
            _ => None,
        }
    }
}

impl Display for UnaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnaryOp::Neg => write!(f, "-"),
            UnaryOp::Not => write!(f, "!"),
            UnaryOp::BitNot => write!(f, "~"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FormatAlign {
    Left,
    Right,
    Center,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum FormatSign {
    #[default]
    Default,
    Always,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum FormatKind {
    #[default]
    Default,
    Hex,
    HexUpper,
    Binary,
    Exp,
    ExpUpper,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct FormatSpec {
    pub fill: char,
    pub align: Option<FormatAlign>,
    pub sign: FormatSign,
    pub zero_pad: bool,
    pub width: Option<u32>,
    pub precision: Option<u32>,
    pub kind: FormatKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Hash, Eq, Default)]
pub struct ExprId(pub u64);

#[derive(Debug, Clone, PartialEq)]
pub struct TypeParam {
    pub name: Ident,
    pub id: TypeVarId,
    pub bounds: Vec<ContractRef>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ConstParam {
    pub name: Ident,
    pub id: ConstParamId,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Func {
    pub annotations: Vec<AnnotationNode>,
    pub doc: Option<String>,
    pub name: Ident,
    pub visibility: Visibility,
    pub type_params: Vec<TypeParam>,
    pub const_params: Vec<ConstParam>,
    pub params: Vec<Param>,
    pub ret: ReturnSpec,
    pub body: BlockNode,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Param {
    pub mutability: Mutability,
    pub escape: EscapeMode,
    pub name: Ident,
    pub ty: Type,
    pub ty_span: Span,
    pub default: Option<ExprNode>,
    pub cast_accept: bool,
}

impl Param {
    #[must_use]
    pub fn with_ty(self, ty: Type) -> Self {
        Self { ty, ..self }
    }

    #[must_use]
    pub fn map_ty(self, f: impl FnOnce(&Type) -> Type) -> Self {
        let ty = f(&self.ty);
        self.with_ty(ty)
    }

    #[must_use]
    pub fn func_param(&self) -> FuncParam {
        FuncParam::new(
            self.ty.clone(),
            matches!(self.mutability, Mutability::Mutable),
            self.cast_accept,
            self.escape,
        )
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub stmts: Vec<StmtNode>,
    pub tail: Option<Box<ExprNode>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExternFunc {
    pub annotations: Vec<AnnotationNode>,
    pub doc: Option<String>,
    pub visibility: Visibility,
    pub name: Ident,
    pub params: Vec<Param>,
    pub ret: ReturnSpec,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExternType {
    pub annotations: Vec<AnnotationNode>,
    pub doc: Option<String>,
    pub visibility: Visibility,
    pub name: Ident,
    pub rep: ExternTypeRep,
    pub init: Option<ExternInit>,
    pub members: Vec<ExternTypeMember>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExternInit {
    pub params: Vec<Param>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExternTypeMember {
    Field {
        doc: Option<String>,
        name: Ident,
        ty: Type,
        computed: bool,
    },
    Method {
        doc: Option<String>,
        name: Ident,
        receiver: ExternReceiverMode,
        params: Vec<Param>,
        ret: ReturnSpec,
    },
    StaticMethod {
        doc: Option<String>,
        name: Ident,
        params: Vec<Param>,
        ret: ReturnSpec,
    },
    Operator {
        op: BinaryOp,
        other_ty: Type,
        ret: Type,
        self_on_right: bool,
    },
    UnaryOperator {
        op: UnaryOp,
        ret: Type,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct EmbedSpec {
    pub selector: Option<EmbedSelector>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EmbedSelector {
    pub items: Vec<EmbedSelectorItem>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EmbedSelectorItem {
    pub kind: EmbedSelectorKind,
    pub name: Ident,
    pub alias: Option<Ident>,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EmbedSelectorKind {
    Field,
    Method,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructField {
    pub annotations: Vec<AnnotationNode>,
    pub embed: Option<EmbedSpec>,
    pub span: Span,
    pub name: Ident,
    pub ty: Type,
    pub default: Option<ExprNode>,
    pub doc: Option<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum VariantKind {
    Unit,
    Tuple(Vec<Type>),
    Struct(Vec<StructField>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumVariant {
    pub annotations: Vec<AnnotationNode>,
    pub span: Span,
    pub name: Ident,
    pub kind: VariantKind,
    pub raw_value: Option<ExprNode>,
    pub doc: Option<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumDecl {
    pub annotations: Vec<AnnotationNode>,
    pub doc: Option<String>,
    pub name: Ident,
    pub visibility: Visibility,
    pub type_params: Vec<TypeParam>,
    pub const_params: Vec<ConstParam>,
    pub raw_backing: Option<Spanned<Type>>,
    pub variants: Vec<EnumVariant>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructDecl {
    pub kind: AggregateKind,
    pub annotations: Vec<AnnotationNode>,
    pub doc: Option<String>,
    pub name: Ident,
    pub visibility: Visibility,
    pub type_params: Vec<TypeParam>,
    pub const_params: Vec<ConstParam>,
    pub fields: Vec<StructField>,
    pub methods: Vec<Method>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MethodSig {
    pub name: Ident,
    pub type_params: Vec<TypeParam>,
    pub const_params: Vec<ConstParam>,
    pub receiver: Option<MethodReceiver>,
    pub params: Vec<Param>,
    pub ret: ReturnSpec,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Method {
    pub span: Span,
    pub annotations: Vec<AnnotationNode>,
    pub doc: Option<String>,
    pub visibility: Visibility,
    pub sig: MethodSig,
    pub body: BlockNode,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Import {
    pub visibility: Visibility,
    pub target: ImportTarget,
    pub kind: ImportKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ImportTarget {
    pub root: ImportRoot,
    pub path: PackageModulePath,
}

impl ImportTarget {
    pub fn local(ascend: usize, path: Vec<Ident>) -> Self {
        Self::named(ImportRoot::Local { ascend }, path)
    }

    pub fn package(alias: Ident, path: Vec<Ident>) -> Self {
        Self {
            root: ImportRoot::Package(alias),
            path: PackageModulePath::from_segments(path),
        }
    }

    pub fn native_provider(path: Vec<Ident>) -> Self {
        Self::named(ImportRoot::NativeProvider, path)
    }

    pub fn std(path: Vec<Ident>) -> Self {
        Self::named(ImportRoot::Std, path)
    }

    pub fn local_path(&self) -> Option<(usize, &[Ident])> {
        match (&self.root, &self.path) {
            (ImportRoot::Local { ascend }, PackageModulePath::Named(path)) => Some((*ascend, path)),
            _ => None,
        }
    }

    fn named(root: ImportRoot, path: Vec<Ident>) -> Self {
        Self {
            root,
            path: PackageModulePath::Named(path),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ImportRoot {
    Local { ascend: usize },
    Package(Ident),
    NativeProvider,
    Std,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PackageModulePath {
    Root,
    Named(Vec<Ident>),
}

impl PackageModulePath {
    fn from_segments(path: Vec<Ident>) -> Self {
        if path.is_empty() {
            Self::Root
        } else {
            Self::Named(path)
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ImportKind {
    Module,
    ModuleAs(Ident),
    Selective(Vec<ImportItem>),
    Wildcard,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ImportItemKind {
    Name(Ident),
    SelfModule,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ImportItem {
    pub kind: ImportItemKind,
    pub alias: Option<Ident>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ConstDecl {
    pub annotations: Vec<AnnotationNode>,
    pub doc: Option<String>,
    pub name: Ident,
    pub ty: Option<Type>,
    pub value: ExprNode,
    pub visibility: Visibility,
}

#[derive(Debug, Clone, PartialEq)]
pub struct GlobalDecl {
    pub annotations: Vec<AnnotationNode>,
    pub doc: Option<String>,
    pub visibility: Visibility,
    pub mutability: Mutability,
    pub name: Ident,
    pub ty: Option<Type>,
    pub value: ExprNode,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeAliasDecl {
    pub annotations: Vec<AnnotationNode>,
    pub doc: Option<String>,
    pub visibility: Visibility,
    pub name: Ident,
    pub type_params: Vec<TypeParam>,
    pub const_params: Vec<ConstParam>,
    pub aliased: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ContractDecl {
    pub annotations: Vec<AnnotationNode>,
    pub doc: Option<String>,
    pub visibility: Visibility,
    pub name: Ident,
    pub includes: Vec<Spanned<ContractRef>>,
    pub requirements: Vec<ContractRequirementNode>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ContractRequirement {
    pub sig: MethodSig,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Annotation {
    pub name: Ident,
    pub args: AnnotationArgs,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AnnotationArgs {
    None,
    Positional(Lit),
    Named(Vec<(Ident, Lit)>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExtendMethod {
    pub annotations: Vec<AnnotationNode>,
    pub doc: Option<String>,
    pub sig: MethodSig,
    pub body: BlockNode,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExtendDecl {
    pub visibility: Visibility,
    pub ty: Type,
    pub target_constraint: Option<ExtendTargetConstraint>,
    pub type_params: Vec<TypeParam>,
    pub const_params: Vec<ConstParam>,
    pub methods: Vec<ExtendMethodNode>,
    pub cast_froms: Vec<CastFromNode>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CastKind {
    Total,
    Failable,
}

impl CastKind {
    pub fn syntax(self) -> &'static str {
        match self {
            Self::Total => "cast from",
            Self::Failable => "cast? from",
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct CastFrom {
    pub kind: CastKind,
    pub param: Param,
    pub ret: Option<ReturnSpec>,
    pub body: BlockNode,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Call {
    pub func: Box<ExprNode>,
    pub args: Vec<ExprNode>,
    pub generic_args: Vec<GenericArg>,
    pub safe: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IterSource {
    pub source: Box<ExprNode>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Binary {
    pub left: Box<ExprNode>,
    pub op: BinaryOp,
    pub right: Box<ExprNode>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Unary {
    pub op: UnaryOp,
    pub expr: Box<ExprNode>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Assign {
    pub target: Box<ExprNode>,
    pub op: AssignOp,
    pub value: Box<ExprNode>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Return {
    pub value: Option<ExprNode>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct If {
    pub cond: Box<ExprNode>,
    pub then_block: BlockNode,
    pub else_block: Option<BlockNode>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Ternary {
    pub cond: Box<ExprNode>,
    pub then_expr: Box<ExprNode>,
    pub else_expr: Box<ExprNode>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfLet {
    pub head: ConditionalPatternAccess,
    pub pattern: PatternNode,
    pub value: Box<ExprNode>,
    pub then_block: BlockNode,
    pub else_block: Option<BlockNode>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct While {
    pub cond: ExprNode,
    pub body: BlockNode,
}

#[derive(Debug, Clone, PartialEq)]
pub struct WhileLet {
    pub head: ConditionalPatternAccess,
    pub pattern: PatternNode,
    pub value: ExprNode,
    pub body: BlockNode,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ForBinding {
    pub access: RefAccess,
    pub pattern: PatternNode,
}

#[derive(Debug, Clone, PartialEq)]
pub struct For {
    pub bindings: Vec<ForBinding>,
    pub iterable: ExprNode,
    pub body: BlockNode,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MatchMode {
    Pattern,
    Dynamic,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Match {
    pub access: RefAccess,
    pub mode: MatchMode,
    pub scrutinee: Box<ExprNode>,
    pub arms: Vec<MatchArmNode>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MatchArm {
    pub head: MatchArmHead,
    pub body: ExprNode,
}

#[derive(Debug, Clone, PartialEq)]
pub enum MatchArmHead {
    Pattern(PatternNode),
    DynDowncast(DynDowncastArmNode),
    DynFallback(DynArmBinding),
}

#[derive(Debug, Clone, PartialEq)]
pub struct DynDowncastArm {
    pub id: ExprId,
    pub target: Type,
    pub binding: DynArmBinding,
}

pub type DynArmBinding = Option<Ident>;

#[derive(Debug, Clone, PartialEq)]
pub struct LambdaParam {
    pub name: Ident,
    pub ty: Option<Type>,
    pub mutable: bool,
    pub cast_accept: bool,
    pub escape: EscapeMode,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Lambda {
    pub params: Vec<LambdaParam>,
    pub ret_type: Option<ReturnSpec>,
    pub body: Box<ExprNode>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FieldAccess {
    pub target: Box<ExprNode>,
    pub field: Ident,
    pub safe: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TupleIndex {
    pub target: Box<ExprNode>,
    pub index: u32,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Index {
    pub target: Box<ExprNode>,
    pub index: Box<ExprNode>,
    pub safe: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructLiteral {
    pub qualifier: Option<Ident>,
    pub name: Ident,
    pub generic_args: Vec<GenericArg>,
    pub fields: Vec<(Ident, ExprNode)>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ArrayLiteral {
    pub elements: Vec<ExprNode>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ArrayFill {
    pub value: Box<ExprNode>,
    pub len: Box<ExprNode>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MapLiteral {
    pub entries: Vec<(ExprNode, ExprNode)>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Range {
    Bounded {
        start: Box<ExprNode>,
        end: Box<ExprNode>,
        inclusive: bool,
    },
    From {
        start: Box<ExprNode>,
    },
    To {
        end: Box<ExprNode>,
        inclusive: bool,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Cast {
    pub expr: Box<ExprNode>,
    pub target: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Try {
    pub expr: Box<ExprNode>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Defer {
    pub body: DeferBody,
}

#[derive(Debug, Clone, PartialEq)]
pub enum DeferBody {
    Expr(ExprNode),
    Block(BlockNode),
}

#[derive(Debug, Clone, PartialEq)]
pub struct InferredEnum {
    pub variant: Ident,
    pub args: InferredEnumArgs,
}

#[derive(Debug, Clone, PartialEq)]
pub enum InferredEnumArgs {
    Unit,
    Tuple(Vec<ExprNode>),
    Struct(Vec<(Ident, ExprNode)>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct IntrinsicCall {
    pub name: Ident,
    pub args: Vec<ExprNode>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum EnumPatternPayload {
    Unit,
    Tuple(Vec<PatternNode>),
    Struct {
        fields: Vec<(Ident, PatternNode)>,
        has_rest: bool,
    },
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum OptionalPayloadPattern<'a> {
    Some(&'a PatternNode),
    None,
    NotOptional,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    Ident(Ident),
    Tuple(Vec<PatternNode>),
    Wildcard,
    Struct {
        name: Ident,
        fields: Vec<(Ident, PatternNode)>,
    },
    Enum {
        qualifier: Option<Ident>,
        variant: Ident,
        payload: EnumPatternPayload,
    },
    Range {
        start: Option<Lit>,
        end: Option<Lit>,
        inclusive: bool,
    },
    Lit(Lit),
    Rest,
    Nil,
    Optional(Box<PatternNode>),
    Or(Vec<PatternNode>),
}

impl Pattern {
    pub fn optional_payload(&self) -> OptionalPayloadPattern<'_> {
        match self {
            Self::Optional(inner) => OptionalPayloadPattern::Some(inner),
            Self::Nil => OptionalPayloadPattern::None,
            Self::Enum {
                variant,
                payload: EnumPatternPayload::Tuple(fields),
                ..
            } if *variant == Ident::new("Some") && fields.len() == 1 => {
                OptionalPayloadPattern::Some(&fields[0])
            }
            Self::Enum {
                variant,
                payload: EnumPatternPayload::Unit,
                ..
            } if *variant == Ident::new("None") => OptionalPayloadPattern::None,
            _ => OptionalPayloadPattern::NotOptional,
        }
    }

    pub fn variant_name(&self) -> &'static str {
        match self {
            Self::Ident(_) => "Ident",
            Self::Tuple(_) => "Tuple",
            Self::Wildcard => "Wildcard",
            Self::Struct { .. } => "Struct",
            Self::Enum { .. } => "enum pattern",
            Self::Range { .. } => "range",
            Self::Lit(_) => "literal",
            Self::Rest => "..",
            Self::Nil => "nil",
            Self::Optional(_) => "optional pattern",
            Self::Or(_) => "or pattern",
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Binding {
    pub pattern: PatternNode,
    pub ty: Option<Type>,
    pub mutability: Mutability,
    pub value: ExprNode,
}

#[derive(Debug, Clone, PartialEq)]
pub enum LetElseFallback {
    Block(BlockNode),
    Return(ReturnNode),
    Break,
    Continue,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LetElse {
    pub mutability: Mutability,
    pub pattern: PatternNode,
    pub value: ExprNode,
    pub fallback: LetElseFallbackNode,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum ExprPrecedence {
    Lowest,
    Assignment,
    Ternary,
    LogicalOr,
    Coalesce,
    LogicalAnd,
    BitOr,
    Xor,
    BitAnd,
    Equality,
    Comparison,
    Shift,
    Range,
    Additive,
    Multiplicative,
    Cast,
    Prefix,
    Postfix,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExprChildSide {
    Left,
    Right,
}

pub fn expr_needs_parens(child: &Expr, parent: ExprPrecedence, side: ExprChildSide) -> bool {
    let Some(child) = child.precedence() else {
        return false;
    };
    match side {
        ExprChildSide::Left => child < parent,
        ExprChildSide::Right => child <= parent,
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
    pub id: ExprId,
    pub kind: ExprKind,
}

impl Expr {
    pub fn new(kind: ExprKind, id: ExprId) -> Self {
        Self { id, kind }
    }

    pub fn precedence(&self) -> Option<ExprPrecedence> {
        self.kind.precedence()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    Ident(Ident),
    TypeSubject(Type),
    Block(BlockNode),
    Lit(Lit),
    Call(CallNode),
    IterSource(IterSourceNode),
    Binary(BinaryNode),
    Unary(UnaryNode),
    Assign(AssignNode),
    If(IfNode),
    Ternary(TernaryNode),
    IfLet(IfLetNode),
    Tuple(Vec<ExprNode>),
    TupleIndex(TupleIndexNode),
    Field(FieldAccessNode),
    StructLiteral(StructLiteralNode),
    Range(RangeNode),
    ArrayLiteral(ArrayLiteralNode),
    ArrayFill(ArrayFillNode),
    MapLiteral(MapLiteralNode),
    Index(IndexNode),
    Match(MatchNode),
    StringInterp(Vec<StringPart>),
    Cast(CastNode),
    FailableCast(CastNode),
    Try(TryNode),
    Lambda(LambdaNode),
    InferredEnum(InferredEnumNode),
    IntrinsicCall(IntrinsicCallNode),
}

impl ExprKind {
    pub fn precedence(&self) -> Option<ExprPrecedence> {
        match self {
            Self::Binary(node) => Some(node.node.op.precedence()),
            Self::Range(_) => Some(ExprPrecedence::Range),
            Self::Cast(_) | Self::FailableCast(_) => Some(ExprPrecedence::Cast),
            Self::Unary(_) | Self::Try(_) => Some(ExprPrecedence::Prefix),
            Self::Ternary(_) => Some(ExprPrecedence::Ternary),
            Self::Assign(_) => Some(ExprPrecedence::Assignment),
            _ => None,
        }
    }

    pub fn variant_name(&self) -> &'static str {
        match self {
            Self::Ident(_) => "Ident",
            Self::TypeSubject(_) => "TypeSubject",
            Self::Block(_) => "Block",
            Self::Lit(_) => "Lit",
            Self::Call(_) => "Call",
            Self::IterSource(_) => "IterSource",
            Self::Binary(_) => "Binary",
            Self::Unary(_) => "Unary",
            Self::Assign(_) => "Assign",
            Self::If(_) => "If",
            Self::Ternary(_) => "Ternary",
            Self::IfLet(_) => "if let",
            Self::Tuple(_) => "Tuple",
            Self::TupleIndex(_) => "TupleIndex",
            Self::Field(_) => "Field",
            Self::StructLiteral(_) => "StructLiteral",
            Self::Range(_) => "Range",
            Self::ArrayLiteral(_) => "ArrayLiteral",
            Self::ArrayFill(_) => "ArrayFill",
            Self::MapLiteral(_) => "MapLiteral",
            Self::Index(_) => "Index",
            Self::Match(_) => "Match",
            Self::StringInterp(_) => "StringInterp",
            Self::Cast(_) => "Cast",
            Self::FailableCast(_) => "FailableCast",
            Self::Try(_) => "Try",
            Self::Lambda(_) => "Lambda",
            Self::InferredEnum(_) => "InferredEnum",
            Self::IntrinsicCall(_) => "IntrinsicCall",
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Lit {
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),
    Char(char),
    Nil,
}

impl Lit {
    pub fn const_value(&self) -> Option<ConstValue> {
        match self {
            Self::Int(value) => Some(ConstValue::Int(*value)),
            Self::Float(value) => Some(ConstValue::Float(*value)),
            Self::Bool(value) => Some(ConstValue::Bool(*value)),
            Self::String(value) => Some(ConstValue::String(value.clone())),
            Self::Char(value) => Some(ConstValue::Char(*value)),
            Self::Nil => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum StringPart {
    Text(String),
    Expr(Box<ExprNode>, Option<Spanned<FormatSpec>>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub stmts: Vec<StmtNode>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Import(ImportNode),
    Func(FuncNode),
    ExternFunc(ExternFuncNode),
    ExternType(ExternTypeNode),
    Aggregate(AggregateDeclNode),
    Enum(EnumDeclNode),
    Extend(ExtendDeclNode),
    Const(ConstDeclNode),
    Global(GlobalDeclNode),
    TypeAlias(TypeAliasDeclNode),
    Contract(ContractDeclNode),
    Expr(ExprNode),
    Binding(BindingNode),
    LetElse(LetElseNode),
    Return(ReturnNode),
    While(WhileNode),
    WhileLet(WhileLetNode),
    For(Box<ForNode>),
    Break,
    Continue,
    Defer(DeferNode),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Visibility {
    Public,
    Private,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Mutability {
    Mutable,
    Immutable,
}

impl Mutability {
    pub fn keyword(self) -> &'static str {
        match self {
            Self::Immutable => "let",
            Self::Mutable => "var",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ConditionalPatternAccess {
    Let,
    Var,
    Ref,
}

impl ConditionalPatternAccess {
    pub fn keyword(self) -> &'static str {
        match self {
            Self::Let => "let",
            Self::Var => "var",
            Self::Ref => "ref",
        }
    }

    pub fn is_ref(self) -> bool {
        matches!(self, Self::Ref)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RefAccess {
    Value,
    Ref,
}

impl RefAccess {
    pub fn is_ref(self) -> bool {
        matches!(self, Self::Ref)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AggregateKind {
    Struct,
    DataRef,
}

impl From<AggregateKind> for NominalKind {
    fn from(value: AggregateKind) -> Self {
        match value {
            AggregateKind::Struct => Self::Struct,
            AggregateKind::DataRef => Self::DataRef,
        }
    }
}

impl NominalKind {
    pub fn keyword(self) -> &'static str {
        match self {
            Self::Struct => "struct",
            Self::DataRef => "dataref",
            Self::Enum => "enum",
            Self::Extern => "extern",
        }
    }
}

impl AggregateKind {
    pub fn keyword(self) -> &'static str {
        match self {
            Self::Struct => "struct",
            Self::DataRef => "dataref",
        }
    }

    pub fn is_dataref(self) -> bool {
        matches!(self, Self::DataRef)
    }

    pub fn make_type(
        self,
        name: Ident,
        type_args: Vec<Type>,
        const_args: Vec<ConstArg>,
        origin: Option<ModulePath>,
    ) -> Type {
        debug_assert!(origin.is_none());
        let generic_args = type_args
            .into_iter()
            .map(GenericArg::Type)
            .chain(const_args.into_iter().map(GenericArg::Const))
            .collect();
        Type::UnresolvedNominal {
            qualifier: None,
            name,
            generic_args,
        }
    }
}

pub struct AggregateTypeRef<'a> {
    pub kind: AggregateKind,
    pub name: Ident,
    pub type_args: &'a [Type],
    pub const_args: &'a [ConstArg],
    pub origin: Option<&'a [String]>,
}

impl AggregateTypeRef<'_> {
    pub fn keyword(&self) -> &'static str {
        self.kind.keyword()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExternTypeRep {
    Shared,
    Inline,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExternReceiverMode {
    Value,
    Shared,
    Mutable,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MethodReceiver {
    Value,
    Ref,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum AssignOp {
    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    XorAssign,
    BitAndAssign,
    BitOrAssign,
    ShlAssign,
    ShrAssign,
}

impl Display for AssignOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AssignOp::Assign => write!(f, "="),
            AssignOp::AddAssign => write!(f, "+="),
            AssignOp::SubAssign => write!(f, "-="),
            AssignOp::MulAssign => write!(f, "*="),
            AssignOp::DivAssign => write!(f, "/="),
            AssignOp::XorAssign => write!(f, "^="),
            AssignOp::BitAndAssign => write!(f, "&="),
            AssignOp::BitOrAssign => write!(f, "|="),
            AssignOp::ShlAssign => write!(f, "<<="),
            AssignOp::ShrAssign => write!(f, ">>="),
        }
    }
}

pub type ExprNode = Spanned<Expr>;
pub type StmtNode = Spanned<Stmt>;
pub type FuncNode = Spanned<Func>;
pub type BlockNode = Spanned<Block>;
pub type BindingNode = Spanned<Binding>;
pub type WhileNode = Spanned<While>;
pub type WhileLetNode = Spanned<WhileLet>;
pub type ForNode = Spanned<For>;
pub type BinaryNode = Spanned<Binary>;
pub type UnaryNode = Spanned<Unary>;
pub type CallNode = Spanned<Call>;
pub type IterSourceNode = Spanned<IterSource>;
pub type AssignNode = Spanned<Assign>;
pub type ReturnNode = Spanned<Return>;
pub type IfNode = Spanned<If>;
pub type TernaryNode = Spanned<Ternary>;
pub type IfLetNode = Spanned<IfLet>;
pub type LetElseNode = Spanned<LetElse>;
pub type LetElseFallbackNode = Spanned<LetElseFallback>;
pub type TupleIndexNode = Spanned<TupleIndex>;
pub type PatternNode = Spanned<Pattern>;
pub type FieldAccessNode = Spanned<FieldAccess>;
pub type AggregateDeclNode = Spanned<StructDecl>;
pub type StructLiteralNode = Spanned<StructLiteral>;
pub type EnumDeclNode = Spanned<EnumDecl>;
pub type ExtendDeclNode = Spanned<ExtendDecl>;
pub type ExtendMethodNode = Spanned<ExtendMethod>;
pub type CastFromNode = Spanned<CastFrom>;
pub type RangeNode = Spanned<Range>;
pub type ArrayLiteralNode = Spanned<ArrayLiteral>;
pub type ArrayFillNode = Spanned<ArrayFill>;
pub type MapLiteralNode = Spanned<MapLiteral>;
pub type IndexNode = Spanned<Index>;
pub type MatchNode = Spanned<Match>;
pub type MatchArmNode = Spanned<MatchArm>;
pub type DynDowncastArmNode = Spanned<DynDowncastArm>;
pub type ExternFuncNode = Spanned<ExternFunc>;
pub type ExternTypeNode = Spanned<ExternType>;
pub type ImportNode = Spanned<Import>;
pub type LambdaNode = Spanned<Lambda>;
pub type AnnotationNode = Spanned<Annotation>;
pub type DeferNode = Spanned<Defer>;
pub type IntrinsicCallNode = Spanned<IntrinsicCall>;
pub type InferredEnumNode = Spanned<InferredEnum>;
pub type CastNode = Spanned<Cast>;
pub type FailableCastNode = CastNode;
pub type TryNode = Spanned<Try>;
pub type ConstDeclNode = Spanned<ConstDecl>;
pub type GlobalDeclNode = Spanned<GlobalDecl>;
pub type TypeAliasDeclNode = Spanned<TypeAliasDecl>;
pub type ContractDeclNode = Spanned<ContractDecl>;
pub type ContractRequirementNode = Spanned<ContractRequirement>;

#[cfg(test)]
mod tests {
    use std::rc::Rc;

    use super::{
        AggregateKind, ConstArg, ConstValue, Ident, ModuleOrigin, ModulePath, NominalKind, Type,
    };

    fn origin(name: &str) -> ModulePath {
        Rc::from(vec![name.to_string()].into_boxed_slice())
    }

    fn nominal(site: usize, kind: NominalKind, origin: Option<ModulePath>) -> Type {
        let type_args = (kind != NominalKind::Extern)
            .then_some(Type::Int)
            .into_iter()
            .collect();
        let const_args = (kind != NominalKind::Extern)
            .then_some(ConstArg::Value(ConstValue::Int(1)))
            .into_iter()
            .collect();
        crate::test_support::test_nominal_type(
            crate::test_support::nominal_test_source_id(),
            site,
            kind,
            Ident::new("Foo"),
            type_args,
            const_args,
            origin.map(ModuleOrigin::Module),
        )
    }

    #[test]
    fn nominal_identity_uses_id_and_args() {
        let struct_a = nominal(1, NominalKind::Struct, Some(origin("a")));
        let struct_b = nominal(2, NominalKind::Struct, Some(origin("b")));
        let enum_a = nominal(1, NominalKind::Enum, Some(origin("a")));
        let type_arg = crate::test_support::test_nominal_type(
            crate::test_support::nominal_test_source_id(),
            1,
            NominalKind::Struct,
            Ident::new("Foo"),
            vec![Type::Bool],
            vec![ConstArg::Value(ConstValue::Int(1))],
            Some(ModuleOrigin::Module(origin("a"))),
        );
        let const_arg = crate::test_support::test_nominal_type(
            crate::test_support::nominal_test_source_id(),
            1,
            NominalKind::Struct,
            Ident::new("Foo"),
            vec![Type::Int],
            vec![ConstArg::Value(ConstValue::Int(2))],
            Some(ModuleOrigin::Module(origin("a"))),
        );

        assert_ne!(struct_a, struct_b);
        assert_eq!(struct_a, enum_a);
        assert_ne!(struct_a, type_arg);
        assert_ne!(struct_a, const_arg);
    }

    #[test]
    fn as_nominal_preserves_identity() {
        let ty = nominal(1, NominalKind::Struct, Some(origin("a")));
        let nominal = ty.as_nominal().expect("expected nominal");
        assert_eq!(nominal.kind, NominalKind::Struct);
        assert_eq!(nominal.name, Ident::new("Foo"));
        assert_eq!(nominal.type_args, [Type::Int]);
        assert_eq!(nominal.const_args, [ConstArg::Value(ConstValue::Int(1))]);
        assert_eq!(nominal.origin, Some(ModuleOrigin::Module(origin("a"))));
        assert!(Type::Int.as_nominal().is_none());
    }

    #[test]
    fn as_aggregate_filters_nominal_kinds() {
        let ty = nominal(1, NominalKind::Struct, None);
        let aggregate = ty.as_aggregate().expect("expected aggregate");
        assert_eq!(aggregate.kind, AggregateKind::Struct);
        assert_eq!(aggregate.type_args, [Type::Int]);
        assert_eq!(aggregate.const_args, [ConstArg::Value(ConstValue::Int(1))]);

        assert!(nominal(2, NominalKind::Enum, None).as_aggregate().is_none());
        assert!(
            nominal(3, NominalKind::Extern, None)
                .as_aggregate()
                .is_none()
        );
    }
}
