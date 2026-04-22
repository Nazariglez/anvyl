use std::fmt::Display;

use internment::Intern;

use crate::span::Spanned;

pub type ModulePath = std::rc::Rc<[String]>;

#[derive(Debug, Copy, Clone, PartialEq, Hash, Eq)]
pub struct Ident(pub Intern<String>);

impl Ident {
    pub fn new(s: impl Into<String>) -> Self {
        Self(Intern::new(s.into()))
    }

    pub fn as_str(&self) -> &str {
        &*self.0
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
}

impl FuncParam {
    pub fn new(ty: Type, mutable: bool) -> Self {
        Self { ty, mutable }
    }
    pub fn immut(ty: Type) -> Self {
        Self { ty, mutable: false }
    }
}

impl Display for FuncParam {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.ty)
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

#[derive(Debug, Clone)]
pub enum Type {
    Infer,
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
    Var(TypeVarId),
    UnresolvedName(Ident),
    Tuple(Vec<Type>),
    NamedTuple(Vec<(Ident, Type)>),
    Struct {
        name: Ident,
        type_args: Vec<Type>,
        origin: Option<ModulePath>,
    },
    DataRef {
        name: Ident,
        type_args: Vec<Type>,
        origin: Option<ModulePath>,
    },
    Enum {
        name: Ident,
        type_args: Vec<Type>,
        origin: Option<ModulePath>,
    },
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
    Extern {
        name: Ident,
        origin: Option<ModulePath>,
    },
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        use Type::*;
        match (self, other) {
            (Infer, Infer)
            | (Any, Any)
            | (Int, Int)
            | (Float, Float)
            | (Bool, Bool)
            | (String, String)
            | (Void, Void) => true,
            (
                Func {
                    params: p1,
                    ret: r1,
                },
                Func {
                    params: p2,
                    ret: r2,
                },
            ) => p1 == p2 && r1 == r2,
            (Var(a), Var(b)) => a == b,
            (UnresolvedName(a), UnresolvedName(b)) => a == b,
            (Tuple(a), Tuple(b)) => a == b,
            (NamedTuple(a), NamedTuple(b)) => a == b,
            (
                Struct {
                    name: n1,
                    type_args: t1,
                    ..
                },
                Struct {
                    name: n2,
                    type_args: t2,
                    ..
                },
            )
            | (
                DataRef {
                    name: n1,
                    type_args: t1,
                    ..
                },
                DataRef {
                    name: n2,
                    type_args: t2,
                    ..
                },
            )
            | (
                Enum {
                    name: n1,
                    type_args: t1,
                    ..
                },
                Enum {
                    name: n2,
                    type_args: t2,
                    ..
                },
            ) => n1 == n2 && t1 == t2,
            (List { elem: a }, List { elem: b }) | (Slice { elem: a }, Slice { elem: b }) => a == b,
            (Array { elem: e1, len: l1 }, Array { elem: e2, len: l2 }) => e1 == e2 && l1 == l2,
            (Map { key: k1, value: v1 }, Map { key: k2, value: v2 }) => k1 == k2 && v1 == v2,
            (Extern { name: n1, .. }, Extern { name: n2, .. }) => n1 == n2,
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
            Type::Var(id) => id.hash(state),
            Type::UnresolvedName(ident) => ident.hash(state),
            Type::Tuple(elems) => elems.hash(state),
            Type::NamedTuple(fields) => fields.hash(state),
            Type::Struct {
                name, type_args, ..
            }
            | Type::DataRef {
                name, type_args, ..
            }
            | Type::Enum {
                name, type_args, ..
            } => {
                name.hash(state);
                type_args.hash(state);
            }
            Type::List { elem } | Type::Slice { elem } => elem.hash(state),
            Type::Array { elem, len } => {
                elem.hash(state);
                len.hash(state);
            }
            Type::Map { key, value } => {
                key.hash(state);
                value.hash(state);
            }
            Type::Extern { name, .. } => name.hash(state),
            _ => {}
        }
    }
}

impl Type {
    pub const OPTION_ENUM_NAME: &'static str = "Option";

    pub fn boxed(&self) -> Box<Self> {
        Box::new(self.clone())
    }

    pub fn option_of(inner: Type) -> Type {
        let name = Ident(Intern::new(Type::OPTION_ENUM_NAME.to_string()));
        Type::Enum {
            name,
            type_args: vec![inner],
            origin: None,
        }
    }

    pub fn is_option(&self) -> bool {
        self.option_inner().is_some()
    }

    pub fn option_inner(&self) -> Option<&Type> {
        match self {
            Type::Enum {
                name, type_args, ..
            } if name.0.as_ref() == Type::OPTION_ENUM_NAME => type_args.first(),
            _ => None,
        }
    }

    pub fn as_aggregate(&self) -> Option<AggregateTypeRef<'_>> {
        match self {
            Type::Struct {
                name,
                type_args,
                origin,
            } => Some(AggregateTypeRef {
                kind: AggregateKind::Struct,
                name: *name,
                type_args,
                origin: origin.as_deref(),
            }),
            Type::DataRef {
                name,
                type_args,
                origin,
            } => Some(AggregateTypeRef {
                kind: AggregateKind::DataRef,
                name: *name,
                type_args,
                origin: origin.as_deref(),
            }),
            _ => None,
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Type::*;
        match self {
            Infer => write!(f, "_"),
            Any => write!(f, "any"),
            Int => write!(f, "int"),
            Float => write!(f, "float"),
            Bool => write!(f, "bool"),
            String => write!(f, "string"),
            Void => write!(f, "void"),
            Func { params, ret } => {
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
                if !matches!(**ret, Void) {
                    write!(f, " -> {ret}")?;
                }
                Ok(())
            }
            Var(id) => write!(f, "{id}"),
            UnresolvedName(ident) => write!(f, "{ident}"),
            Tuple(elems) => {
                write!(f, "(")?;
                for (i, e) in elems.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{e}")?;
                }
                write!(f, ")")
            }
            NamedTuple(fields) => {
                write!(f, "{{")?;
                for (i, (n, t)) in fields.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{n}: {t}")?;
                }
                write!(f, "}}")
            }
            Struct {
                name, type_args, ..
            } => {
                write!(f, "{name}")?;
                if !type_args.is_empty() {
                    write!(f, "<")?;
                    for (i, a) in type_args.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{a}")?;
                    }
                    write!(f, ">")?;
                }
                Ok(())
            }
            DataRef {
                name, type_args, ..
            } => {
                write!(f, "{name}")?;
                if !type_args.is_empty() {
                    write!(f, "<")?;
                    for (i, a) in type_args.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{a}")?;
                    }
                    write!(f, ">")?;
                }
                Ok(())
            }
            Enum {
                name, type_args, ..
            } => {
                write!(f, "{name}")?;
                if !type_args.is_empty() {
                    write!(f, "<")?;
                    for (i, a) in type_args.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{a}")?;
                    }
                    write!(f, ">")?;
                }
                Ok(())
            }
            List { elem } => write!(f, "[{elem}]"),
            Array { elem, len } => write!(f, "[{elem}; {len}]"),
            Map { key, value } => write!(f, "[{key}: {value}]"),
            Slice { elem } => write!(f, "[{elem}; _]"),
            Extern { name, .. } => write!(f, "{name}"),
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FormatSign {
    Default,
    Always,
}

impl Default for FormatSign {
    fn default() -> Self {
        Self::Default
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FormatKind {
    Default,
    Hex,
    HexUpper,
    Binary,
    Exp,
    ExpUpper,
}

impl Default for FormatKind {
    fn default() -> Self {
        Self::Default
    }
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
    pub name: Ident,
    pub params: Vec<Param>,
    pub ret: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExternType {
    pub annotations: Vec<AnnotationNode>,
    pub doc: Option<String>,
    pub name: Ident,
    pub kind: ExternKind,
    pub has_init: bool,
    pub members: Vec<ExternTypeMember>,
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
        receiver: MethodReceiver,
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
pub struct StructField {
    pub annotations: Vec<AnnotationNode>,
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
pub struct Method {
    pub annotations: Vec<AnnotationNode>,
    pub doc: Option<String>,
    pub name: Ident,
    pub visibility: Visibility,
    pub type_params: Vec<TypeParam>,
    pub const_params: Vec<ConstParam>,
    pub receiver: Option<MethodReceiver>,
    pub params: Vec<Param>,
    pub ret: Type,
    pub body: BlockNode,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Import {
    pub visibility: Visibility,
    pub path: Vec<Ident>,
    pub kind: ImportKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ImportKind {
    Module,
    ModuleAs(Ident),
    Selective(Vec<ImportItem>),
    Wildcard,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ImportItem {
    pub name: Ident,
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
    pub name: Ident,
    pub params: Vec<Param>,
    pub ret: Type,
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
    pub type_args: Vec<Type>,
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
pub struct IfLet {
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
pub enum Pattern {
    Ident(Ident),
    Tuple(Vec<PatternNode>),
    NamedTuple(Vec<(Ident, PatternNode)>),
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
    VarIdent(Ident),
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
            Self::NamedTuple(_) => "NamedTuple",
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
            Self::VarIdent(_) => "var binding",
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
    pub pattern: PatternNode,
    pub value: ExprNode,
    pub else_block: BlockNode,
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
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    Ident(Ident),
    Block(BlockNode),
    Lit(Lit),
    Call(CallNode),
    Binary(BinaryNode),
    Unary(UnaryNode),
    Assign(AssignNode),
    If(IfNode),
    IfLet(IfLetNode),
    Tuple(Vec<ExprNode>),
    NamedTuple(Vec<(Ident, ExprNode)>),
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
    Lambda(LambdaNode),
    InferredEnum(InferredEnumNode),
    IntrinsicCall(IntrinsicCallNode),
}

impl ExprKind {
    pub fn variant_name(&self) -> &'static str {
        match self {
            Self::Ident(_) => "Ident",
            Self::Block(_) => "Block",
            Self::Lit(_) => "Lit",
            Self::Call(_) => "Call",
            Self::Binary(_) => "Binary",
            Self::Unary(_) => "Unary",
            Self::Assign(_) => "Assign",
            Self::If(_) => "If",
            Self::IfLet(_) => "if let",
            Self::Tuple(_) => "Tuple",
            Self::NamedTuple(_) => "NamedTuple",
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

    pub fn make_type(self, name: Ident, type_args: Vec<Type>, origin: Option<ModulePath>) -> Type {
        match self {
            Self::Struct => Type::Struct {
                name,
                type_args,
                origin,
            },
            Self::DataRef => Type::DataRef {
                name,
                type_args,
                origin,
            },
        }
    }
}

pub struct AggregateTypeRef<'a> {
    pub kind: AggregateKind,
    pub name: Ident,
    pub type_args: &'a [Type],
    pub origin: Option<&'a [String]>,
}

impl AggregateTypeRef<'_> {
    pub fn keyword(&self) -> &'static str {
        self.kind.keyword()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExternKind {
    InlineCopy,
    SharedIdentity,
}

#[derive(Debug, Clone, Copy, PartialEq)]
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
pub type ConstDeclNode = Spanned<ConstDecl>;
