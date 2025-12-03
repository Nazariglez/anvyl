use crate::span::Spanned;
use internment::Intern;
use std::fmt::Display;

pub type ExprNode = Spanned<Expr>;
pub type StmtNode = Spanned<Stmt>;
pub type FuncNode = Spanned<Func>;
pub type BlockNode = Spanned<Block>;
pub type BindingNode = Spanned<Binding>;
pub type WhileNode = Spanned<While>;
pub type ForNode = Spanned<For>;
pub type BinaryNode = Spanned<Binary>;
pub type UnaryNode = Spanned<Unary>;
pub type CallNode = Spanned<Call>;
pub type AssignNode = Spanned<Assign>;
pub type ReturnNode = Spanned<Return>;
pub type IfNode = Spanned<If>;
pub type TupleIndexNode = Spanned<TupleIndex>;
pub type PatternNode = Spanned<Pattern>;
pub type FieldAccessNode = Spanned<FieldAccess>;
pub type StructDeclNode = Spanned<StructDecl>;
pub type StructLiteralNode = Spanned<StructLiteral>;
pub type RangeNode = Spanned<Range>;
pub type ArrayLiteralNode = Spanned<ArrayLiteral>;
pub type ArrayFillNode = Spanned<ArrayFill>;
pub type IndexNode = Spanned<Index>;

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub stmts: Vec<StmtNode>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Func(FuncNode),
    Struct(StructDeclNode),
    Expr(ExprNode),
    Binding(BindingNode),
    Return(ReturnNode),
    While(WhileNode),
    For(ForNode),
    Break,
    Continue,
}

#[derive(Debug, Clone, Copy, PartialEq, Hash, Eq, Default)]
pub struct ExprId(pub u64);

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
    Tuple(Vec<ExprNode>),
    NamedTuple(Vec<(Ident, ExprNode)>),
    TupleIndex(TupleIndexNode),
    Field(FieldAccessNode),
    StructLiteral(StructLiteralNode),
    Range(RangeNode),
    ArrayLiteral(ArrayLiteralNode),
    ArrayFill(ArrayFillNode),
    Index(IndexNode),
}

#[derive(Debug, Copy, Clone, PartialEq, Hash, Eq)]
pub struct Ident(pub Intern<String>);

impl Display for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// Id for a generic type variable (T, U)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct TypeVarId(pub u32);

impl Display for TypeVarId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "${}", self.0)
    }
}

/// A type parameter declared on a generic function (T in fn foo<T>(...))
#[derive(Debug, Clone, PartialEq)]
pub struct TypeParam {
    pub name: Ident,
    pub id: TypeVarId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ArrayLen {
    Fixed(usize),
    Infer,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    /// Unknown type that needs to be inferred
    Infer,
    /// Int 64 type
    Int,
    /// Float 64 type
    Float,
    /// Boolean type
    Bool,
    /// String type
    String,
    /// Void type
    Void,
    /// Optional type (int? or T?)
    Optional(Box<Type>),
    /// Function type
    Func { params: Vec<Type>, ret: Box<Type> },
    /// Generic type variable (T, U)
    Var(TypeVarId),
    /// Unresolved type name reference (T before being resolved to Var)
    UnresolvedName(Ident),
    /// Tuple type (int, string, bool)
    Tuple(Vec<Type>),
    /// Named tuple type (x: int, y: string)
    NamedTuple(Vec<(Ident, Type)>),
    /// Struct type
    Struct { name: Ident, type_args: Vec<Type> },
    /// List are dynamic arrays
    List { elem: Box<Type> },
    /// Arrays are fixed length [T; N] or [T; _]
    Array { elem: Box<Type>, len: ArrayLen },
    /// Map type (key-value pairs)
    Map { key: Box<Type>, value: Box<Type> },
    /// View/slice type for function parameters ([T; ..])
    ArrayView { elem: Box<Type> },
}

impl Type {
    pub fn is_num(&self) -> bool {
        matches!(self, Type::Int | Type::Float)
    }

    pub fn is_bool(&self) -> bool {
        matches!(self, Type::Bool)
    }

    pub fn is_str(&self) -> bool {
        matches!(self, Type::String)
    }

    pub fn is_void(&self) -> bool {
        matches!(self, Type::Void)
    }

    pub fn is_optional(&self) -> bool {
        matches!(self, Type::Optional(_))
    }

    pub fn is_func(&self) -> bool {
        matches!(self, Type::Func { .. })
    }

    pub fn is_infer(&self) -> bool {
        matches!(self, Type::Infer)
    }

    pub fn is_type_var(&self) -> bool {
        matches!(self, Type::Var(_))
    }

    pub fn is_tuple(&self) -> bool {
        matches!(self, Type::Tuple(_))
    }

    pub fn is_named_tuple(&self) -> bool {
        matches!(self, Type::NamedTuple(_))
    }

    pub fn is_struct(&self) -> bool {
        matches!(self, Type::Struct { .. })
    }

    pub fn is_list(&self) -> bool {
        matches!(self, Type::List { .. })
    }

    pub fn is_array(&self) -> bool {
        matches!(self, Type::Array { .. })
    }

    pub fn is_map(&self) -> bool {
        matches!(self, Type::Map { .. })
    }

    pub fn is_array_view(&self) -> bool {
        matches!(self, Type::ArrayView { .. })
    }

    pub fn tuple_arity(&self) -> Option<usize> {
        match self {
            Type::Tuple(elems) => Some(elems.len()),
            Type::NamedTuple(fields) => Some(fields.len()),
            _ => None,
        }
    }

    pub fn tuple_element_types(&self) -> Option<Vec<Type>> {
        match self {
            Type::Tuple(elems) => Some(elems.clone()),
            Type::NamedTuple(fields) => Some(fields.iter().map(|(_, ty)| ty.clone()).collect()),
            _ => None,
        }
    }

    pub fn boxed(&self) -> Box<Self> {
        Box::new(self.clone())
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Int => write!(f, "int"),
            Type::Float => write!(f, "float"),
            Type::Bool => write!(f, "bool"),
            Type::String => write!(f, "string"),
            Type::Void => write!(f, "void"),
            Type::Optional(ty) => write!(f, "{}?", ty),
            Type::Func { params, ret } => write!(
                f,
                "fn({}) -> {}",
                params
                    .iter()
                    .map(|p| p.to_string())
                    .collect::<Vec<_>>()
                    .join(", "),
                ret
            ),
            Type::Infer => write!(f, "<infer>"),
            Type::Var(id) => write!(f, "{}", id),
            Type::UnresolvedName(ident) => write!(f, "{}", ident),
            Type::Tuple(elements) => {
                let parts = elements
                    .iter()
                    .map(|t| t.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "({parts})")
            }
            Type::NamedTuple(fields) => {
                let parts = fields
                    .iter()
                    .map(|(name, ty)| format!("{}: {}", name, ty))
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "({parts})")
            }
            Type::Struct { name, type_args } => {
                if type_args.is_empty() {
                    write!(f, "{name}")
                } else {
                    let args = type_args
                        .iter()
                        .map(|t| t.to_string())
                        .collect::<Vec<_>>()
                        .join(", ");
                    write!(f, "{name}<{args}>")
                }
            }
            Type::List { elem } => write!(f, "[{elem}]"),
            Type::Array { elem, len } => match len {
                ArrayLen::Fixed(n) => write!(f, "[{elem}; {n}]"),
                ArrayLen::Infer => write!(f, "[{elem}; _]"),
            },
            Type::Map { key, value } => write!(f, "[{key}: {value}]"),
            Type::ArrayView { elem } => write!(f, "[{elem}; ..]"),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Visibility {
    Public,
    Private,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Mutability {
    Mutable,
    Immutable,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Binding {
    pub pattern: PatternNode,
    pub ty: Option<Type>,
    pub mutability: Mutability,
    pub value: ExprNode,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    Ident(Ident),
    Tuple(Vec<PatternNode>),
    NamedTuple(Vec<(Ident, PatternNode)>),
    Wildcard,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Func {
    pub name: Ident,
    pub visibility: Visibility,
    pub type_params: Vec<TypeParam>,
    pub params: Vec<Param>,
    pub ret: Type,
    pub body: BlockNode,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Param {
    pub name: Ident,
    pub ty: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub stmts: Vec<StmtNode>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Lit {
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),
    Nil,
}

#[derive(Debug, Copy, Clone, PartialEq)]
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
            BinaryOp::Coalesce => write!(f, "??"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Binary {
    pub left: Box<ExprNode>,
    pub op: BinaryOp,
    pub right: Box<ExprNode>,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum UnaryOp {
    Neg,
    Not,
}

impl Display for UnaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnaryOp::Neg => write!(f, "-"),
            UnaryOp::Not => write!(f, "!"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Unary {
    pub op: UnaryOp,
    pub expr: Box<ExprNode>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Call {
    pub func: Box<ExprNode>,
    pub args: Vec<ExprNode>,
    pub type_args: Vec<Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Assign {
    pub target: Box<ExprNode>,
    pub op: AssignOp,
    pub value: Box<ExprNode>,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum AssignOp {
    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
}

impl Display for AssignOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AssignOp::Assign => write!(f, "="),
            AssignOp::AddAssign => write!(f, "+="),
            AssignOp::SubAssign => write!(f, "-="),
            AssignOp::MulAssign => write!(f, "*="),
            AssignOp::DivAssign => write!(f, "/="),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Return {
    pub value: Option<ExprNode>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct While {
    pub cond: ExprNode,
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
pub struct If {
    pub cond: Box<ExprNode>,
    pub then_block: BlockNode,
    pub else_block: Option<BlockNode>,
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
}

#[derive(Debug, Clone, PartialEq)]
pub struct FieldAccess {
    pub target: Box<ExprNode>,
    pub field: Ident,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructField {
    pub name: Ident,
    pub ty: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructDecl {
    pub name: Ident,
    pub type_params: Vec<TypeParam>,
    pub fields: Vec<StructField>,
    pub methods: Vec<Method>,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum MethodReceiver {
    Value,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Method {
    pub name: Ident,
    pub visibility: Visibility,
    pub type_params: Vec<TypeParam>,
    pub receiver: Option<MethodReceiver>,
    pub params: Vec<Param>,
    pub ret: Type,
    pub body: BlockNode,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructLiteral {
    pub name: Ident,
    pub fields: Vec<(Ident, ExprNode)>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Range {
    pub start: Box<ExprNode>,
    pub end: Box<ExprNode>,
    pub inclusive: bool,
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
