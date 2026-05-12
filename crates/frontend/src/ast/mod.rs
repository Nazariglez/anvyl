use std::fmt::Display;

use internment::Intern;

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
}

impl PartialEq for ConstValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Int(a), Self::Int(b)) => a == b,
            (Self::Float(a), Self::Float(b)) => a.to_bits() == b.to_bits(),
            (Self::Bool(a), Self::Bool(b)) => a == b,
            (Self::String(a), Self::String(b)) => a == b,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ArrayLen {
    Fixed(usize),
    Infer,
    Named(Ident),
    Param(ConstParamId),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FuncParam {
    pub ty: Type,
    pub mutable: bool,
    pub cast_accept: bool,
}

impl FuncParam {
    pub fn new(ty: Type, mutable: bool, cast_accept: bool) -> Self {
        Self {
            ty,
            mutable,
            cast_accept,
        }
    }
    pub fn immut(ty: Type) -> Self {
        Self {
            ty,
            mutable: false,
            cast_accept: false,
        }
    }
}

impl Display for FuncParam {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.cast_accept {
            write!(f, "as {}", self.ty)
        } else {
            write!(f, "{}", self.ty)
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct NominalType {
    pub kind: NominalKind,
    pub name: Ident,
    pub type_args: Vec<Type>,
    pub const_args: Vec<ConstArg>,
    pub origin: Option<ModuleOrigin>,
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
    pub ret: Type,
}

#[derive(Debug, Clone)]
pub struct AnonymousContractParam {
    pub mutable: bool,
    pub name: Ident,
    pub ty: Type,
}

impl PartialEq for AnonymousContractParam {
    fn eq(&self, other: &Self) -> bool {
        self.mutable == other.mutable && self.ty == other.ty
    }
}

impl Eq for AnonymousContractParam {}

impl std::hash::Hash for AnonymousContractParam {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::hash::Hash::hash(&self.mutable, state);
        std::hash::Hash::hash(&self.ty, state);
    }
}

#[derive(Debug, Clone)]
pub enum Type {
    Infer,
    InferReturn,
    Any,
    Int,
    Float,
    Bool,
    String,
    Void,
    Func {
        params: Vec<FuncParam>,
        ret: Box<Type>,
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
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Infer, Self::Infer)
            | (Self::InferReturn, Self::InferReturn)
            | (Self::Any, Self::Any)
            | (Self::Int, Self::Int)
            | (Self::Float, Self::Float)
            | (Self::Bool, Self::Bool)
            | (Self::String, Self::String)
            | (Self::Void, Self::Void) => true,
            (
                Self::Func {
                    params: p1,
                    ret: r1,
                },
                Self::Func {
                    params: p2,
                    ret: r2,
                },
            ) => p1 == p2 && r1 == r2,
            (Self::Dyn(a), Self::Dyn(b)) => a == b,
            (Self::Var(a), Self::Var(b)) => a == b,
            (Self::UnresolvedName(a), Self::UnresolvedName(b)) => a == b,
            (
                Self::UnresolvedNominal {
                    qualifier: qa,
                    name: na,
                    generic_args: ga,
                },
                Self::UnresolvedNominal {
                    qualifier: qb,
                    name: nb,
                    generic_args: gb,
                },
            ) => qa == qb && na == nb && ga == gb,
            (Self::Tuple(a), Self::Tuple(b)) => a == b,
            (Self::Nominal(a), Self::Nominal(b)) => a == b,
            (Self::List { elem: a }, Self::List { elem: b })
            | (Self::Slice { elem: a }, Self::Slice { elem: b }) => a == b,
            (Self::Array { elem: ea, len: la }, Self::Array { elem: eb, len: lb }) => {
                ea == eb && la == lb
            }
            (Self::Map { key: ka, value: va }, Self::Map { key: kb, value: vb }) => {
                ka == kb && va == vb
            }
            _ => false,
        }
    }
}

impl Eq for Type {}

impl std::hash::Hash for Type {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::mem::discriminant(self).hash(state);
        match self {
            Type::Func { params, ret } => {
                params.hash(state);
                ret.hash(state);
            }
            Type::Dyn(contract) => contract.hash(state),
            Type::Var(id) => id.hash(state),
            Type::UnresolvedName(ident) => ident.hash(state),
            Type::UnresolvedNominal {
                qualifier,
                name,
                generic_args,
            } => {
                qualifier.hash(state);
                name.hash(state);
                generic_args.hash(state);
            }
            Type::Tuple(elems) => elems.hash(state),
            Type::Nominal(nominal) => nominal.hash(state),
            Type::List { elem } | Type::Slice { elem } => elem.hash(state),
            Type::Array { elem, len } => {
                elem.hash(state);
                len.hash(state);
            }
            Type::Map { key, value } => {
                key.hash(state);
                value.hash(state);
            }
            _ => {}
        }
    }
}

impl Type {
    pub const OPTION_ENUM_NAME: &'static str = "Option";

    #[inline]
    pub fn boxed(&self) -> Box<Self> {
        Box::new(self.clone())
    }

    pub fn nominal(
        kind: NominalKind,
        name: Ident,
        type_args: Vec<Type>,
        const_args: Vec<ConstArg>,
        origin: Option<ModulePath>,
    ) -> Type {
        Self::nominal_with_origin(
            kind,
            name,
            type_args,
            const_args,
            origin.map(ModuleOrigin::Module),
        )
    }

    pub fn nominal_with_origin(
        kind: NominalKind,
        name: Ident,
        type_args: Vec<Type>,
        const_args: Vec<ConstArg>,
        origin: Option<ModuleOrigin>,
    ) -> Type {
        if kind == NominalKind::Extern {
            debug_assert!(type_args.is_empty());
            debug_assert!(const_args.is_empty());
        }
        Type::Nominal(NominalType {
            kind,
            name,
            type_args,
            const_args,
            origin,
        })
    }

    pub fn as_nominal(&self) -> Option<&NominalType> {
        match self {
            Type::Nominal(nominal) => Some(nominal),
            _ => None,
        }
    }

    pub fn option_of(inner: Type) -> Type {
        Type::nominal(
            NominalKind::Enum,
            Ident(Intern::new(Type::OPTION_ENUM_NAME.to_string())),
            vec![inner],
            vec![],
            None,
        )
    }

    #[inline]
    pub fn is_option(&self) -> bool {
        self.option_inner().is_some()
    }

    #[inline]
    pub fn option_inner(&self) -> Option<&Type> {
        match self {
            Type::Nominal(nominal)
                if nominal.kind == NominalKind::Enum
                    && nominal.name.0.as_ref() == Type::OPTION_ENUM_NAME =>
            {
                nominal.type_args.first()
            }
            _ => None,
        }
    }

    #[inline]
    pub fn is_str(&self) -> bool {
        matches!(self, Type::String)
    }

    #[inline]
    pub fn is_stringable(&self) -> bool {
        matches!(self, Type::Int | Type::Float | Type::Bool)
    }

    #[inline]
    pub fn is_num(&self) -> bool {
        matches!(self, Type::Int | Type::Float)
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

    #[inline]
    pub fn is_any(&self) -> bool {
        matches!(self, Type::Any)
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
                        MethodReceiver::Var => write!(f, "var self")?,
                    }
                    for param in &requirement.params {
                        write!(f, ", ")?;
                        if param.mutable {
                            write!(f, "var ")?;
                        }
                        write!(f, "{}: {}", param.name, param.ty)?;
                    }
                    if requirement.ret == Type::Void {
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

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Infer | Self::InferReturn => write!(f, "_"),
            Self::Any => write!(f, "any"),
            Self::Int => write!(f, "int"),
            Self::Float => write!(f, "float"),
            Self::Bool => write!(f, "bool"),
            Self::String => write!(f, "string"),
            Self::Void => write!(f, "void"),
            Self::Func { params, ret } => {
                write!(f, "fn(")?;
                for (i, p) in params.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    if p.mutable {
                        write!(f, "mut ")?;
                    }
                    write!(f, "{p}")?;
                }
                write!(f, ")")?;
                if !matches!(**ret, Self::Void) {
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
        }
    }
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
    pub ret: Type,
    pub body: BlockNode,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Param {
    pub mutability: Mutability,
    pub name: Ident,
    pub ty: Type,
    pub default: Option<ExprNode>,
    pub cast_accept: bool,
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
    pub ret: Type,
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
        ret: Type,
    },
    StaticMethod {
        doc: Option<String>,
        name: Ident,
        params: Vec<Param>,
        ret: Type,
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
    pub name: Ident,
    pub kind: VariantKind,
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
    pub ret: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Method {
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
    pub type_params: Vec<TypeParam>,
    pub const_params: Vec<ConstParam>,
    pub methods: Vec<ExtendMethodNode>,
    pub cast_froms: Vec<CastFromNode>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CastFrom {
    pub param: Param,
    pub ret: Option<Type>,
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
    pub head: PatternHead,
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
    pub head: PatternHead,
    pub pattern: PatternNode,
    pub value: ExprNode,
    pub body: BlockNode,
}

#[derive(Debug, Clone, PartialEq)]
pub struct For {
    pub pattern: PatternNode,
    pub iterable: ExprNode,
    pub step: Option<ExprNode>,
    pub reversed: bool,
    pub body: BlockNode,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Match {
    pub head: PatternHead,
    pub scrutinee: Box<ExprNode>,
    pub arms: Vec<MatchArmNode>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MatchArm {
    pub pattern: PatternNode,
    pub body: ExprNode,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LambdaParam {
    pub name: Ident,
    pub ty: Option<Type>,
    pub mutable: bool,
    pub cast_accept: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Lambda {
    pub params: Vec<LambdaParam>,
    pub ret_type: Option<Type>,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PatternHead {
    Let,
    Var,
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
    EnumUnit {
        qualifier: Ident,
        variant: Ident,
    },
    EnumTuple {
        qualifier: Ident,
        variant: Ident,
        fields: Vec<PatternNode>,
    },
    EnumStruct {
        qualifier: Ident,
        variant: Ident,
        fields: Vec<(Ident, PatternNode)>,
        has_rest: bool,
    },
    InferredEnumUnit {
        variant: Ident,
    },
    InferredEnumTuple {
        variant: Ident,
        fields: Vec<PatternNode>,
    },
    InferredEnumStruct {
        variant: Ident,
        fields: Vec<(Ident, PatternNode)>,
        has_rest: bool,
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
    pub fn variant_name(&self) -> &'static str {
        match self {
            Self::Ident(_) => "Ident",
            Self::Tuple(_) => "Tuple",
            Self::Wildcard => "Wildcard",
            Self::Struct { .. } => "Struct",
            Self::EnumUnit { .. } => "EnumUnit",
            Self::EnumTuple { .. } => "EnumTuple",
            Self::EnumStruct { .. } => "EnumStruct",
            Self::InferredEnumUnit { .. } => "InferredEnumUnit",
            Self::InferredEnumTuple { .. } => "InferredEnumTuple",
            Self::InferredEnumStruct { .. } => "InferredEnumStruct",
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
pub struct LetElse {
    pub head: PatternHead,
    pub pattern: PatternNode,
    pub value: ExprNode,
    pub else_block: BlockNode,
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
            Self::Cast(_) => Some(ExprPrecedence::Cast),
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
    Nil,
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
        Type::nominal(self.into(), name, type_args, const_args, origin)
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
    Var,
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
pub type AssignNode = Spanned<Assign>;
pub type ReturnNode = Spanned<Return>;
pub type IfNode = Spanned<If>;
pub type TernaryNode = Spanned<Ternary>;
pub type IfLetNode = Spanned<IfLet>;
pub type LetElseNode = Spanned<LetElse>;
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
pub type ExternFuncNode = Spanned<ExternFunc>;
pub type ExternTypeNode = Spanned<ExternType>;
pub type ImportNode = Spanned<Import>;
pub type LambdaNode = Spanned<Lambda>;
pub type AnnotationNode = Spanned<Annotation>;
pub type DeferNode = Spanned<Defer>;
pub type IntrinsicCallNode = Spanned<IntrinsicCall>;
pub type InferredEnumNode = Spanned<InferredEnum>;
pub type CastNode = Spanned<Cast>;
pub type TryNode = Spanned<Try>;
pub type ConstDeclNode = Spanned<ConstDecl>;
pub type TypeAliasDeclNode = Spanned<TypeAliasDecl>;
pub type ContractDeclNode = Spanned<ContractDecl>;
pub type ContractRequirementNode = Spanned<ContractRequirement>;

#[cfg(test)]
mod tests {
    use std::{
        collections::hash_map::DefaultHasher,
        hash::{Hash, Hasher},
        rc::Rc,
    };

    use super::{
        AggregateKind, ConstArg, ConstParamId, ConstValue, GenericArg, Ident, ModuleOrigin,
        ModulePath, NominalKind, NominalType, Type,
    };

    fn hash<T: Hash>(value: &T) -> u64 {
        let mut hasher = DefaultHasher::new();
        value.hash(&mut hasher);
        hasher.finish()
    }

    fn nominal(
        kind: NominalKind,
        name: &str,
        type_args: Vec<Type>,
        const_args: Vec<ConstArg>,
        origin: Option<ModulePath>,
    ) -> Type {
        Type::nominal(kind, Ident::new(name), type_args, const_args, origin)
    }

    fn buf(n: i64) -> Type {
        nominal(
            NominalKind::Struct,
            "Buf",
            vec![Type::Int],
            vec![ConstArg::Value(ConstValue::Int(n))],
            None,
        )
    }

    fn origin(name: &str) -> ModulePath {
        Rc::from(vec![name.to_string()].into_boxed_slice())
    }

    fn foo(kind: NominalKind, origin: Option<ModulePath>) -> Type {
        let (type_args, const_args) = match kind {
            NominalKind::Extern => (vec![], vec![]),
            _ => (vec![Type::Int], vec![ConstArg::Value(ConstValue::Int(1))]),
        };
        nominal(kind, "Foo", type_args, const_args, origin)
    }

    #[test]
    fn const_arg_fmt() {
        assert_eq!(
            ConstValue::String(String::from("cap")).to_string(),
            "\"cap\""
        );
        assert_eq!(ConstArg::Value(ConstValue::Int(7)).to_string(), "7");
        assert_eq!(ConstArg::Name(Ident::new("CAP")).to_string(), "CAP");
        assert_eq!(ConstArg::Param(ConstParamId(2)).to_string(), "$c2");
        assert_eq!(
            GenericArg::Const(ConstArg::Value(ConstValue::Int(7))).to_string(),
            "7"
        );
    }

    #[test]
    fn nominal_eq() {
        assert_ne!(buf(3), buf(4));
    }

    #[test]
    fn nominal_hash() {
        assert_ne!(hash(&buf(3)), hash(&buf(4)));
    }

    #[test]
    fn nominal_origin_affects_eq() {
        for kind in [
            NominalKind::Struct,
            NominalKind::DataRef,
            NominalKind::Enum,
            NominalKind::Extern,
        ] {
            assert_ne!(foo(kind, Some(origin("a"))), foo(kind, Some(origin("b"))));
        }
    }

    #[test]
    fn nominal_origin_affects_hash() {
        for kind in [
            NominalKind::Struct,
            NominalKind::DataRef,
            NominalKind::Enum,
            NominalKind::Extern,
        ] {
            assert_ne!(
                hash(&foo(kind, Some(origin("a")))),
                hash(&foo(kind, Some(origin("b"))))
            );
        }
    }

    #[test]
    fn nominal_origin_affects_eq_and_hash() {
        let a = foo(NominalKind::Struct, Some(origin("a")));
        let b = foo(NominalKind::Struct, Some(origin("b")));
        assert_ne!(a, b);
        assert_ne!(hash(&a), hash(&b));
    }

    #[test]
    fn nominal_kind_affects_eq_and_hash() {
        let aggregate = foo(NominalKind::Struct, Some(origin("a")));
        let enum_ty = foo(NominalKind::Enum, Some(origin("a")));
        assert_ne!(aggregate, enum_ty);
        assert_ne!(hash(&aggregate), hash(&enum_ty));
    }

    #[test]
    fn nominal_kind_fmt() {
        assert_eq!(foo(NominalKind::Struct, None).to_string(), "Foo<int, 1>");
    }

    #[test]
    fn as_nominal() {
        let ty = foo(NominalKind::Struct, Some(origin("a")));
        assert_eq!(
            ty.as_nominal(),
            Some(&NominalType {
                kind: NominalKind::Struct,
                name: Ident::new("Foo"),
                type_args: vec![Type::Int],
                const_args: vec![ConstArg::Value(ConstValue::Int(1))],
                origin: Some(ModuleOrigin::Module(origin("a"))),
            })
        );
        assert!(Type::Int.as_nominal().is_none());
    }

    #[test]
    fn nominal_fmt() {
        assert_eq!(buf(3).to_string(), "Buf<int, 3>");
    }

    #[test]
    fn as_aggregate() {
        let ty = buf(3);
        let aggregate = ty.as_aggregate().expect("expected aggregate");
        assert_eq!(aggregate.kind, AggregateKind::Struct);
        assert_eq!(aggregate.type_args, [Type::Int]);
        assert_eq!(aggregate.const_args, [ConstArg::Value(ConstValue::Int(3))]);

        let dataref = nominal(NominalKind::DataRef, "Ref", vec![], vec![], None);
        assert_eq!(
            dataref.as_aggregate().expect("expected dataref").kind,
            AggregateKind::DataRef
        );
        assert!(foo(NominalKind::Enum, None).as_aggregate().is_none());
        assert!(foo(NominalKind::Extern, None).as_aggregate().is_none());
    }

    #[test]
    fn option_is_enum_nominal_only() {
        let option = Type::option_of(Type::Int);
        assert!(option.is_option());
        assert_eq!(option.option_inner(), Some(&Type::Int));

        for kind in [
            NominalKind::Struct,
            NominalKind::DataRef,
            NominalKind::Extern,
        ] {
            let type_args = match kind {
                NominalKind::Extern => vec![],
                _ => vec![Type::Int],
            };
            let ty = nominal(kind, Type::OPTION_ENUM_NAME, type_args, vec![], None);
            assert!(!ty.is_option());
            assert!(ty.option_inner().is_none());
        }
    }
}
