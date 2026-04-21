use std::cell::Cell;

pub(super) use crate::test_helpers::{dummy_ident, dummy_span};
use crate::{
    ast::*,
    typecheck::{
        LintConfig, check_program_with_modules, error::Diagnostic, types::TypecheckResult,
    },
};

thread_local! {
    static EXPR_ID_COUNTER: Cell<u64> = const { Cell::new(0) };
}

// reset the expression id counter for deterministic test ids
pub(super) fn reset_expr_ids() {
    EXPR_ID_COUNTER.with(|counter| counter.set(0));
}

pub(super) fn next_expr_id() -> ExprId {
    EXPR_ID_COUNTER.with(|counter| {
        let id = counter.get();
        counter.set(id + 1);
        ExprId(id)
    })
}

// ---- ast builder helpers ----

pub(super) fn lit_int(val: i64) -> ExprNode {
    ExprNode {
        node: Expr::new(ExprKind::Lit(Lit::Int(val)), next_expr_id()),
        span: dummy_span(),
    }
}

pub(super) fn lit_float(val: f64) -> ExprNode {
    ExprNode {
        node: Expr::new(
            ExprKind::Lit(Lit::Float {
                value: val,
                suffix: None,
            }),
            next_expr_id(),
        ),
        span: dummy_span(),
    }
}

pub(super) fn lit_bool(val: bool) -> ExprNode {
    ExprNode {
        node: Expr::new(ExprKind::Lit(Lit::Bool(val)), next_expr_id()),
        span: dummy_span(),
    }
}

pub(super) fn lit_string(val: &str) -> ExprNode {
    ExprNode {
        node: Expr::new(ExprKind::Lit(Lit::String(val.to_string())), next_expr_id()),
        span: dummy_span(),
    }
}

pub(super) fn lit_nil() -> ExprNode {
    ExprNode {
        node: Expr::new(ExprKind::Lit(Lit::Nil), next_expr_id()),
        span: dummy_span(),
    }
}

pub(super) fn opt_type(inner: Type) -> Type {
    Type::option_of(inner)
}

pub(super) fn ident_expr(name: &str) -> ExprNode {
    ExprNode {
        node: Expr::new(ExprKind::Ident(dummy_ident(name)), next_expr_id()),
        span: dummy_span(),
    }
}

pub(super) fn binary_expr(left: ExprNode, op: BinaryOp, right: ExprNode) -> ExprNode {
    ExprNode {
        node: Expr::new(
            ExprKind::Binary(BinaryNode {
                node: Binary {
                    left: Box::new(left),
                    op,
                    right: Box::new(right),
                },
                span: dummy_span(),
            }),
            next_expr_id(),
        ),
        span: dummy_span(),
    }
}

pub(super) fn unary_expr(op: UnaryOp, expr: ExprNode) -> ExprNode {
    ExprNode {
        node: Expr::new(
            ExprKind::Unary(UnaryNode {
                node: Unary {
                    op,
                    expr: Box::new(expr),
                },
                span: dummy_span(),
            }),
            next_expr_id(),
        ),
        span: dummy_span(),
    }
}

pub(super) fn call_expr(func: ExprNode, args: Vec<ExprNode>) -> ExprNode {
    ExprNode {
        node: Expr::new(
            ExprKind::Call(CallNode {
                node: Call {
                    func: Box::new(func),
                    args,
                    type_args: vec![],
                    safe: false,
                },
                span: dummy_span(),
            }),
            next_expr_id(),
        ),
        span: dummy_span(),
    }
}

pub(super) fn call_expr_with_type_args(
    func: ExprNode,
    args: Vec<ExprNode>,
    type_args: Vec<Type>,
) -> ExprNode {
    ExprNode {
        node: Expr::new(
            ExprKind::Call(CallNode {
                node: Call {
                    func: Box::new(func),
                    args,
                    type_args,
                    safe: false,
                },
                span: dummy_span(),
            }),
            next_expr_id(),
        ),
        span: dummy_span(),
    }
}

pub(super) fn range_expr(start: ExprNode, inclusive: bool, end: ExprNode) -> ExprNode {
    ExprNode {
        node: Expr::new(
            ExprKind::Range(RangeNode {
                node: Range::Bounded {
                    start: Box::new(start),
                    end: Box::new(end),
                    inclusive,
                },
                span: dummy_span(),
            }),
            next_expr_id(),
        ),
        span: dummy_span(),
    }
}

pub(super) fn assign_expr(target: ExprNode, op: AssignOp, value: ExprNode) -> ExprNode {
    ExprNode {
        node: Expr::new(
            ExprKind::Assign(AssignNode {
                node: Assign {
                    target: Box::new(target),
                    op,
                    value: Box::new(value),
                },
                span: dummy_span(),
            }),
            next_expr_id(),
        ),
        span: dummy_span(),
    }
}

fn split_body(mut stmts: Vec<StmtNode>) -> (Vec<StmtNode>, Option<Box<ExprNode>>) {
    match stmts.last().map(|s| &s.node) {
        Some(Stmt::Expr(_)) => {
            let last = stmts.pop().unwrap();
            let Stmt::Expr(tail) = last.node else {
                unreachable!()
            };
            (stmts, Some(Box::new(tail)))
        }
        _ => (stmts, None),
    }
}

pub(super) fn block_expr(stmts: Vec<StmtNode>, tail: Option<ExprNode>) -> ExprNode {
    ExprNode {
        node: Expr::new(
            ExprKind::Block(BlockNode {
                node: Block {
                    stmts,
                    tail: tail.map(Box::new),
                },
                span: dummy_span(),
            }),
            next_expr_id(),
        ),
        span: dummy_span(),
    }
}

pub(super) fn array_literal(elements: Vec<ExprNode>) -> ExprNode {
    ExprNode {
        node: Expr::new(
            ExprKind::ArrayLiteral(ArrayLiteralNode {
                node: ArrayLiteral { elements },
                span: dummy_span(),
            }),
            next_expr_id(),
        ),
        span: dummy_span(),
    }
}

pub(super) fn array_fill(value: ExprNode, len: ExprNode) -> ExprNode {
    ExprNode {
        node: Expr::new(
            ExprKind::ArrayFill(ArrayFillNode {
                node: ArrayFill {
                    value: Box::new(value),
                    len: Box::new(len),
                },
                span: dummy_span(),
            }),
            next_expr_id(),
        ),
        span: dummy_span(),
    }
}

fn make_index_expr(target: ExprNode, index: ExprNode, safe: bool) -> ExprNode {
    ExprNode {
        node: Expr::new(
            ExprKind::Index(IndexNode {
                node: Index {
                    target: Box::new(target),
                    index: Box::new(index),
                    safe,
                },
                span: dummy_span(),
            }),
            next_expr_id(),
        ),
        span: dummy_span(),
    }
}

pub(super) fn index_expr(target: ExprNode, index: ExprNode) -> ExprNode {
    make_index_expr(target, index, false)
}

pub(super) fn safe_index_expr(target: ExprNode, index: ExprNode) -> ExprNode {
    make_index_expr(target, index, true)
}

pub(super) fn map_literal_expr(entries: Vec<(ExprNode, ExprNode)>) -> ExprNode {
    ExprNode {
        node: Expr::new(
            ExprKind::MapLiteral(MapLiteralNode {
                node: MapLiteral { entries },
                span: dummy_span(),
            }),
            next_expr_id(),
        ),
        span: dummy_span(),
    }
}

pub(super) fn dummy_pattern(name: &str) -> PatternNode {
    PatternNode {
        node: Pattern::Ident(dummy_ident(name)),
        span: dummy_span(),
    }
}

fn binding_stmt(name: &str, ty: Option<Type>, value: ExprNode, mutability: Mutability) -> StmtNode {
    StmtNode {
        node: Stmt::Binding(BindingNode {
            node: Binding {
                pattern: dummy_pattern(name),
                ty,
                mutability,
                value,
            },
            span: dummy_span(),
        }),
        span: dummy_span(),
    }
}

pub(super) fn let_binding(name: &str, ty: Option<Type>, value: ExprNode) -> StmtNode {
    binding_stmt(name, ty, value, Mutability::Immutable)
}

pub(super) fn var_binding(name: &str, ty: Option<Type>, value: ExprNode) -> StmtNode {
    binding_stmt(name, ty, value, Mutability::Mutable)
}

fn build_func_stmt(
    name: &str,
    type_params: Vec<TypeParam>,
    params: Vec<Param>,
    ret: Type,
    stmts: Vec<StmtNode>,
    tail: Option<Box<ExprNode>>,
) -> StmtNode {
    StmtNode {
        node: Stmt::Func(FuncNode {
            node: Func {
                annotations: vec![],
                doc: None,
                name: dummy_ident(name),
                visibility: Visibility::Private,
                type_params,
                const_params: vec![],
                params,
                ret,
                body: BlockNode {
                    node: Block { stmts, tail },
                    span: dummy_span(),
                },
            },
            span: dummy_span(),
        }),
        span: dummy_span(),
    }
}

pub(super) fn fn_decl(
    name: &str,
    params: Vec<(&str, Type)>,
    ret: Type,
    body: Vec<StmtNode>,
) -> StmtNode {
    generic_fn_decl(name, vec![], params, ret, body)
}

pub(super) fn generic_fn_decl(
    name: &str,
    type_params: Vec<TypeParam>,
    params: Vec<(&str, Type)>,
    ret: Type,
    body: Vec<StmtNode>,
) -> StmtNode {
    let param_list = params
        .into_iter()
        .map(|(n, t)| Param {
            mutability: Mutability::Immutable,
            name: dummy_ident(n),
            ty: t,
            default: None,
            cast_accept: false,
        })
        .collect();
    let (stmts, tail) = split_body(body);
    build_func_stmt(name, type_params, param_list, ret, stmts, tail)
}

pub(super) fn func_decl(
    name: &str,
    params: Vec<(&str, Type)>,
    ret: Type,
    body: Vec<StmtNode>,
) -> StmtNode {
    let param_list = params
        .into_iter()
        .map(|(n, t)| Param {
            mutability: Mutability::Immutable,
            name: dummy_ident(n),
            ty: t,
            default: None,
            cast_accept: false,
        })
        .collect();
    let (stmts, tail) = if body.is_empty() && !ret.is_void() {
        (vec![], Some(Box::new(lit_int(0))))
    } else {
        split_body(body)
    };
    build_func_stmt(name, vec![], param_list, ret, stmts, tail)
}

pub(super) fn return_stmt(value: Option<ExprNode>) -> StmtNode {
    StmtNode {
        node: Stmt::Return(ReturnNode {
            node: Return { value },
            span: dummy_span(),
        }),
        span: dummy_span(),
    }
}

pub(super) fn expr_stmt(expr: ExprNode) -> StmtNode {
    StmtNode {
        node: Stmt::Expr(expr),
        span: dummy_span(),
    }
}

pub(super) fn program(stmts: Vec<StmtNode>) -> Program {
    Program { stmts }
}

// ---- type helpers ----

pub(super) fn type_var(id: u32) -> Type {
    Type::Var(TypeVarId(id))
}

pub(super) fn type_param(name: &str, id: u32) -> TypeParam {
    TypeParam {
        name: dummy_ident(name),
        id: TypeVarId(id),
    }
}

pub(super) fn slice_type(elem: &Type) -> Type {
    Type::Slice { elem: elem.boxed() }
}

pub(super) fn field_expr(target: ExprNode, field: &str) -> ExprNode {
    ExprNode {
        node: Expr::new(
            ExprKind::Field(FieldAccessNode {
                node: FieldAccess {
                    target: Box::new(target),
                    field: dummy_ident(field),
                    safe: false,
                },
                span: dummy_span(),
            }),
            next_expr_id(),
        ),
        span: dummy_span(),
    }
}

pub(super) fn struct_literal_expr(name: &str, fields: Vec<(&str, ExprNode)>) -> ExprNode {
    let field_pairs = fields
        .into_iter()
        .map(|(n, e)| (dummy_ident(n), e))
        .collect();
    ExprNode {
        node: Expr::new(
            ExprKind::StructLiteral(StructLiteralNode {
                node: StructLiteral {
                    qualifier: None,
                    name: dummy_ident(name),
                    fields: field_pairs,
                },
                span: dummy_span(),
            }),
            next_expr_id(),
        ),
        span: dummy_span(),
    }
}

pub(super) fn method(
    name: &str,
    receiver: Option<MethodReceiver>,
    params: Vec<(&str, Type)>,
    ret: Type,
    body: Vec<StmtNode>,
) -> Method {
    generic_method(name, vec![], receiver, params, ret, body)
}

pub(super) fn generic_method(
    name: &str,
    type_params: Vec<TypeParam>,
    receiver: Option<MethodReceiver>,
    params: Vec<(&str, Type)>,
    ret: Type,
    body: Vec<StmtNode>,
) -> Method {
    let param_list = params
        .into_iter()
        .map(|(n, ty)| Param {
            name: dummy_ident(n),
            ty,
            mutability: Mutability::Immutable,
            default: None,
            cast_accept: false,
        })
        .collect();
    let (stmts, tail) = split_body(body);
    Method {
        annotations: vec![],
        doc: None,
        name: dummy_ident(name),
        visibility: Visibility::Private,
        type_params,
        const_params: vec![],
        receiver,
        params: param_list,
        ret,
        body: BlockNode {
            node: Block { stmts, tail },
            span: dummy_span(),
        },
    }
}

pub(super) fn struct_decl(name: &str, fields: Vec<(&str, Type)>, methods: Vec<Method>) -> StmtNode {
    generic_struct_decl(name, vec![], fields, methods)
}

pub(super) fn generic_struct_decl(
    name: &str,
    type_params: Vec<TypeParam>,
    fields: Vec<(&str, Type)>,
    methods: Vec<Method>,
) -> StmtNode {
    let struct_fields = fields
        .into_iter()
        .map(|(n, ty)| StructField {
            annotations: vec![],
            name: dummy_ident(n),
            ty,
            default: None,
            doc: None,
        })
        .collect();
    StmtNode {
        node: Stmt::Aggregate(AggregateDeclNode {
            node: StructDecl {
                kind: AggregateKind::Struct,
                annotations: vec![],
                doc: None,
                name: dummy_ident(name),
                visibility: Visibility::Public,
                type_params,
                const_params: vec![],
                fields: struct_fields,
                methods,
            },
            span: dummy_span(),
        }),
        span: dummy_span(),
    }
}

pub(super) fn enum_decl(name: &str, variants: Vec<(&str, VariantKind)>) -> StmtNode {
    let enum_variants = variants
        .into_iter()
        .map(|(n, kind)| EnumVariant {
            annotations: vec![],
            name: dummy_ident(n),
            kind,
            doc: None,
        })
        .collect();
    StmtNode {
        node: Stmt::Enum(EnumDeclNode {
            node: EnumDecl {
                annotations: vec![],
                doc: None,
                name: dummy_ident(name),
                visibility: Visibility::Public,
                type_params: vec![],
                const_params: vec![],
                variants: enum_variants,
            },
            span: dummy_span(),
        }),
        span: dummy_span(),
    }
}

pub(super) fn fn_decl_var_params(
    name: &str,
    params: Vec<(&str, Type, bool)>,
    ret: Type,
    body: Vec<StmtNode>,
) -> StmtNode {
    let param_list = params
        .into_iter()
        .map(|(n, ty, mutable)| Param {
            name: dummy_ident(n),
            ty,
            mutability: if mutable {
                Mutability::Mutable
            } else {
                Mutability::Immutable
            },
            default: None,
            cast_accept: false,
        })
        .collect();
    let (stmts, tail) = split_body(body);
    build_func_stmt(name, vec![], param_list, ret, stmts, tail)
}

// ---- runner helpers ----

fn with_prelude(prog: Program) -> Program {
    const TEST_CORE_PRELUDE: &str = concat!(
        include_str!("../../../../core/src/option.anv"),
        "\n",
        include_str!("../../../../core/src/range.anv"),
    );
    let tokens = crate::lexer::tokenize(TEST_CORE_PRELUDE).expect("core prelude must tokenize");
    let prelude = crate::parser::parse_ast(&tokens).expect("core prelude must parse");
    let mut stmts = prelude.stmts;
    stmts.extend(prog.stmts);
    Program { stmts }
}

#[track_caller]
pub(super) fn run_ok(prog: Program) -> TypecheckResult {
    match check_program_with_modules(&with_prelude(prog), &[], &[], &[], LintConfig::default()) {
        Ok(tcx) => tcx,
        Err(errors) => {
            panic!("Expected Ok, got errors: {errors:?}");
        }
    }
}

#[track_caller]
pub(super) fn run_err(prog: Program) -> Vec<Diagnostic> {
    match check_program_with_modules(&with_prelude(prog), &[], &[], &[], LintConfig::default()) {
        Ok(_) => panic!("Expected Err, got Ok"),
        Err(errors) => errors,
    }
}

pub(super) fn check_src(src: &str) -> Result<TypecheckResult, Vec<Diagnostic>> {
    let user_tokens = crate::lexer::tokenize(src).unwrap();
    let user_ast = crate::parser::parse_ast(&user_tokens).unwrap();
    let prog = Program {
        stmts: user_ast.stmts,
    };
    check_program_with_modules(&with_prelude(prog), &[], &[], &[], LintConfig::default())
}

// ---- assertion helpers ----

#[track_caller]
pub(super) fn assert_expr_type(tcx: &TypecheckResult, id: ExprId, expected: &Type) {
    match tcx.get_type(id) {
        Some((_, ty)) => assert_eq!(
            *ty, *expected,
            "Expression {id:?} has wrong type. Expected {expected:?}, got {ty:?}"
        ),
        None => panic!("Expression {id:?} not found in type map"),
    }
}

pub(super) fn get_expr_id(expr: &ExprNode) -> ExprId {
    expr.node.id
}

pub(super) fn string_interp_expr(parts: Vec<StringPart>) -> ExprNode {
    ExprNode {
        node: Expr::new(ExprKind::StringInterp(parts), next_expr_id()),
        span: dummy_span(),
    }
}

pub(super) fn text_part(s: &str) -> StringPart {
    StringPart::Text(s.to_string())
}

pub(super) fn expr_part(expr: ExprNode) -> StringPart {
    StringPart::Expr(Box::new(expr), None)
}

pub(super) fn cast_expr_node(expr: ExprNode, target: Type) -> ExprNode {
    use crate::span::Spanned;
    let span = dummy_span();
    let cast_node: CastNode = Spanned::new(
        Cast {
            expr: Box::new(expr),
            target,
        },
        span,
    );
    ExprNode {
        node: Expr::new(ExprKind::Cast(cast_node), next_expr_id()),
        span,
    }
}
