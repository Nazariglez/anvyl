use crate::{
    ast::{self, ExprId, TypeVarId},
    lexer::{Delimiter, Keyword, LitToken, Op, SpannedToken, Token},
    span::{Span, Spanned},
};
use chumsky::{
    error::Rich,
    extra::{self, SimpleState},
    prelude::*,
};
use std::collections::HashMap;

#[derive(Debug, Default)]
struct ParserState {
    next_expr_id: ExprId,
    next_type_var_id: TypeVarId,
}

impl ParserState {
    fn new_expr_id(&mut self) -> ExprId {
        let id = ExprId(self.next_expr_id.0);
        self.next_expr_id = ExprId(id.0 + 1);
        id
    }

    fn new_type_var_id(&mut self) -> TypeVarId {
        let id = TypeVarId(self.next_type_var_id.0);
        self.next_type_var_id = TypeVarId(id.0 + 1);
        id
    }
}

type Input<'src> = &'src [SpannedToken];
type Extra<'src> = extra::Full<Rich<'src, SpannedToken>, SimpleState<ParserState>, ()>;
trait AnvParser<'src, T>: chumsky::Parser<'src, Input<'src>, T, Extra<'src>> + Clone + 'src {}
impl<'src, T, P> AnvParser<'src, T> for P where
    P: chumsky::Parser<'src, Input<'src>, T, Extra<'src>> + Clone + 'src
{
}

pub fn parse_ast(tokens: &[SpannedToken]) -> Result<ast::Program, Vec<Rich<'_, SpannedToken>>> {
    let mut state = SimpleState(ParserState::default());
    parser().parse_with_state(tokens, &mut state).into_result()
}

fn parser<'src>() -> impl AnvParser<'src, ast::Program> {
    let stmt = statement();
    let func_decl = function(stmt.clone()).map(|func_node| {
        let span = func_node.span;
        Spanned::new(ast::Stmt::Func(func_node), span)
    });
    let struct_decl = struct_declaration(stmt).map(|struct_node| {
        let span = struct_node.span;
        Spanned::new(ast::Stmt::Struct(struct_node), span)
    });

    choice((func_decl, struct_decl))
        .repeated()
        .collect::<Vec<_>>()
        .map(|stmts| ast::Program { stmts })
        .then_ignore(end())
}

fn statement<'src>() -> impl AnvParser<'src, ast::StmtNode> {
    recursive(|stmt| {
        let expr = expression(stmt.clone());
        let func = function(stmt.clone());
        let bind = binding(stmt.clone());
        let ret = return_stmt(stmt.clone());

        let at_block_end = select! { (Token::Close(Delimiter::Brace), _) => () }.rewind();

        let at_stmt_start = select! {
            (Token::Keyword(Keyword::Let), _) => (),
            (Token::Keyword(Keyword::Var), _) => (),
            (Token::Keyword(Keyword::Return), _) => (),
            (Token::Keyword(Keyword::Fn), _) => (),
            (Token::Keyword(Keyword::If), _) => (),
            (Token::Keyword(Keyword::Struct), _) => (),
        }
        .rewind();

        let expr_stmt = expr
            .then_ignore(
                select! { (Token::Semicolon, _) => () }
                    .or(at_block_end)
                    .or(at_stmt_start),
            )
            .map(|expr_node| {
                let span = expr_node.span;
                Spanned::new(ast::Stmt::Expr(expr_node), span)
            });

        choice((
            func.map(|func_node| {
                let span = func_node.span;
                Spanned::new(ast::Stmt::Func(func_node), span)
            }),
            bind.map(|bind_node| {
                let span = bind_node.span;
                Spanned::new(ast::Stmt::Binding(bind_node), span)
            }),
            ret,
            expr_stmt,
        ))
    })
    .labelled("statement")
    .as_context()
}

fn type_params<'src>() -> impl AnvParser<'src, Vec<ast::TypeParam>> {
    select! {
        (Token::Op(Op::LessThan), _) => (),
    }
    .ignore_then(
        identifier()
            .map_with(|name, e| {
                let id = e.state().new_type_var_id();
                ast::TypeParam { name, id }
            })
            .separated_by(select! {
                (Token::Comma, _) => (),
            })
            .allow_trailing()
            .collect::<Vec<_>>(),
    )
    .then_ignore(select! {
        (Token::Op(Op::GreaterThan), _) => (),
    })
    .or_not()
    .map(|opt| opt.unwrap_or_default())
    .labelled("type parameters")
    .as_context()
}

fn function<'src>(
    stmt: impl AnvParser<'src, ast::StmtNode>,
) -> impl AnvParser<'src, ast::FuncNode> {
    select! {
        (Token::Keyword(Keyword::Fn), _) => (),
    }
    .ignore_then(identifier())
    .then(type_params())
    .then(params())
    .then(return_type())
    .then(block_stmt(stmt))
    .map_with(|((((name, type_params), params), ret), body), e| {
        let s = e.span();
        let type_param_map: HashMap<ast::Ident, ast::TypeVarId> =
            type_params.iter().map(|tp| (tp.name, tp.id)).collect();

        let resolved_params = params
            .into_iter()
            .map(|p| {
                let ty = resolve_type_params(&p.ty, &type_param_map);
                ast::Param { name: p.name, ty }
            })
            .collect();

        let resolved_ret = match ret {
            Some(ty) => resolve_type_params(&ty, &type_param_map),
            None => ast::Type::Void,
        };

        Spanned::new(
            ast::Func {
                name,
                visibility: ast::Visibility::Private,
                type_params,
                params: resolved_params,
                ret: resolved_ret,
                body,
            },
            Span::new(s.start, s.end),
        )
    })
    .labelled("function")
    .as_context()
}

fn struct_field<'src>() -> impl AnvParser<'src, ast::StructField> {
    identifier()
        .then_ignore(select! {
            (Token::Colon, _) => (),
        })
        .then(type_ident())
        .map(|(name, ty)| ast::StructField { name, ty })
        .labelled("struct field")
        .as_context()
}

enum StructMember {
    Field(ast::StructField),
    Method(ast::Method),
}

fn struct_method<'src>(
    stmt: impl AnvParser<'src, ast::StmtNode>,
) -> impl AnvParser<'src, ast::Method> {
    select! {
        (Token::Keyword(Keyword::Fn), _) => (),
    }
    .ignore_then(identifier())
    .then(type_params())
    .then(method_params())
    .then(return_type())
    .then(block_stmt(stmt))
    .map_with(
        |((((name, method_type_params), (receiver, params)), ret), body), e| {
            let s = e.span();

            let type_param_map = method_type_params
                .iter()
                .map(|tp| (tp.name, tp.id))
                .collect();

            let resolved_params = params
                .into_iter()
                .map(|p| {
                    let ty = resolve_type_params(&p.ty, &type_param_map);
                    ast::Param { name: p.name, ty }
                })
                .collect();

            let resolved_ret = match ret {
                Some(ty) => resolve_type_params(&ty, &type_param_map),
                None => ast::Type::Void,
            };

            ast::Method {
                name,
                visibility: ast::Visibility::Private,
                type_params: method_type_params,
                receiver,
                params: resolved_params,
                ret: resolved_ret,
                body: Spanned::new(body.node, Span::new(s.start, s.end)),
            }
        },
    )
    .labelled("method")
    .as_context()
}

fn method_params<'src>() -> impl AnvParser<'src, (Option<ast::MethodReceiver>, Vec<ast::Param>)> {
    select! {
        (Token::Open(Delimiter::Parent), _) => (),
    }
    .ignore_then(
        method_param_list()
            .or_not()
            .map(|opt| opt.unwrap_or_default()),
    )
    .then_ignore(select! {
        (Token::Close(Delimiter::Parent), _) => (),
    })
}

fn self_param<'src>() -> impl AnvParser<'src, ()> {
    identifier()
        .try_map(|ident, span| {
            if ident.0.as_ref() == "self" {
                Ok(())
            } else {
                Err(Rich::custom(span, "expected 'self'"))
            }
        })
        .then_ignore(
            select! { (Token::Colon, _) => () }
                .ignore_then(type_ident())
                .or_not(),
        )
}

fn method_param_list<'src>() -> impl AnvParser<'src, (Option<ast::MethodReceiver>, Vec<ast::Param>)>
{
    let regular_params = param()
        .separated_by(select! { (Token::Comma, _) => () })
        .collect::<Vec<_>>();

    choice((
        self_param()
            .then(
                select! { (Token::Comma, _) => () }
                    .ignore_then(regular_params.clone())
                    .or_not()
                    .map(|opt| opt.unwrap_or_default()),
            )
            .map(|(_, params)| (Some(ast::MethodReceiver::Value), params)),
        regular_params.map(|params| (None, params)),
    ))
}

fn struct_member<'src>(
    stmt: impl AnvParser<'src, ast::StmtNode>,
) -> impl AnvParser<'src, StructMember> {
    choice((
        struct_method(stmt).map(StructMember::Method),
        struct_field().map(StructMember::Field),
    ))
}

fn struct_declaration<'src>(
    stmt: impl AnvParser<'src, ast::StmtNode>,
) -> impl AnvParser<'src, ast::StructDeclNode> {
    select! {
        (Token::Keyword(Keyword::Struct), _) => (),
    }
    .ignore_then(identifier())
    .then(type_params())
    .then(
        select! {
            (Token::Open(Delimiter::Brace), _) => (),
        }
        .ignore_then(
            struct_member(stmt)
                .separated_by(select! { (Token::Comma, _) => () })
                .allow_trailing()
                .collect::<Vec<_>>(),
        )
        .then_ignore(select! {
            (Token::Close(Delimiter::Brace), _) => (),
        }),
    )
    .map_with(|((name, type_params), members), e| {
        let s = e.span();

        let struct_type_param_map: HashMap<ast::Ident, ast::TypeVarId> =
            type_params.iter().map(|tp| (tp.name, tp.id)).collect();

        let self_type = ast::Type::Struct {
            name,
            type_args: type_params.iter().map(|tp| ast::Type::Var(tp.id)).collect(),
        };

        let mut fields = vec![];
        let mut methods = vec![];

        for member in members {
            match member {
                StructMember::Field(f) => {
                    let ty = resolve_type_params_with_self(
                        &f.ty,
                        &struct_type_param_map,
                        Some(&self_type),
                    );
                    fields.push(ast::StructField { name: f.name, ty });
                }
                StructMember::Method(m) => {
                    let mut combined_type_param_map = struct_type_param_map.clone();
                    for tp in &m.type_params {
                        combined_type_param_map.insert(tp.name, tp.id);
                    }

                    let resolved_params = m
                        .params
                        .iter()
                        .map(|p| ast::Param {
                            name: p.name,
                            ty: resolve_type_params_with_self(
                                &p.ty,
                                &combined_type_param_map,
                                Some(&self_type),
                            ),
                        })
                        .collect();

                    let resolved_ret = resolve_type_params_with_self(
                        &m.ret,
                        &combined_type_param_map,
                        Some(&self_type),
                    );
                    methods.push(ast::Method {
                        name: m.name,
                        visibility: m.visibility,
                        type_params: m.type_params,
                        receiver: m.receiver,
                        params: resolved_params,
                        ret: resolved_ret,
                        body: m.body,
                    });
                }
            }
        }

        Spanned::new(
            ast::StructDecl {
                name,
                type_params,
                fields,
                methods,
            },
            Span::new(s.start, s.end),
        )
    })
    .labelled("struct declaration")
    .as_context()
}

fn resolve_type_params_with_self(
    ty: &ast::Type,
    type_param_map: &HashMap<ast::Ident, ast::TypeVarId>,
    self_type: Option<&ast::Type>,
) -> ast::Type {
    use ast::Type::*;
    match ty {
        UnresolvedName(ident) => {
            if let Some(id) = type_param_map.get(ident) {
                return Var(*id);
            }
            if let Some(st) = self_type {
                if ident.0.as_ref() == "Self" {
                    return st.clone();
                }
            }
            ty.clone()
        }

        Optional(inner) => {
            Optional(resolve_type_params_with_self(inner, type_param_map, self_type).boxed())
        }

        Func { params, ret } => {
            let resolved_params = params
                .iter()
                .map(|p| resolve_type_params_with_self(p, type_param_map, self_type))
                .collect::<Vec<_>>();
            Func {
                params: resolved_params,
                ret: resolve_type_params_with_self(ret, type_param_map, self_type).boxed(),
            }
        }

        Tuple(elements) => {
            let resolved_elements = elements
                .iter()
                .map(|el| resolve_type_params_with_self(el, type_param_map, self_type))
                .collect::<Vec<_>>();
            Tuple(resolved_elements)
        }

        NamedTuple(fields) => {
            let resolved_fields: Vec<_> = fields
                .iter()
                .map(|(name, ty)| {
                    (
                        *name,
                        resolve_type_params_with_self(ty, type_param_map, self_type),
                    )
                })
                .collect();
            NamedTuple(resolved_fields)
        }

        Struct { name, type_args } => {
            let resolved_args = type_args
                .iter()
                .map(|arg| resolve_type_params_with_self(arg, type_param_map, self_type))
                .collect::<Vec<_>>();
            Struct {
                name: *name,
                type_args: resolved_args,
            }
        }

        _ => ty.clone(),
    }
}

fn resolve_type_params(
    ty: &ast::Type,
    type_param_map: &HashMap<ast::Ident, ast::TypeVarId>,
) -> ast::Type {
    resolve_type_params_with_self(ty, type_param_map, None)
}

fn block_stmt<'src>(
    stmt: impl AnvParser<'src, ast::StmtNode>,
) -> impl AnvParser<'src, ast::BlockNode> {
    select! {
        (Token::Open(Delimiter::Brace), _) => (),
    }
    .ignore_then(stmt.repeated().collect::<Vec<_>>())
    .then_ignore(select! {
        (Token::Close(Delimiter::Brace), _) => (),
    })
    .map_with(|stmts, e| {
        let s = e.span();
        Spanned::new(ast::Block { stmts }, Span::new(s.start, s.end))
    })
    .labelled("block")
    .as_context()
}

fn if_expr<'src>(
    stmt: impl AnvParser<'src, ast::StmtNode>,
    expr: impl AnvParser<'src, ast::ExprNode>,
) -> impl AnvParser<'src, ast::ExprNode> {
    recursive(|if_parser| {
        let else_branch = select! {
            (Token::Keyword(Keyword::Else), _) => (),
        }
        .ignore_then(choice((
            // else-if wraps the nested if in a block
            if_parser.map_with(|if_expr: ast::ExprNode, _| {
                let span = if_expr.span;
                let stmt = Spanned::new(ast::Stmt::Expr(if_expr), span);
                Spanned::new(ast::Block { stmts: vec![stmt] }, span)
            }),
            // else { ... }
            block_stmt(stmt.clone()),
        )))
        .or_not();

        select! {
            (Token::Keyword(Keyword::If), _) => (),
        }
        .ignore_then(expr.clone())
        .then(block_stmt(stmt.clone()))
        .then(else_branch)
        .map_with(|((cond, then_block), else_block), e| {
            let s = e.span();
            let span = Span::new(s.start, s.end);
            let if_node = Spanned::new(
                ast::If {
                    cond: Box::new(cond),
                    then_block,
                    else_block,
                },
                span,
            );
            let expr_id = e.state().new_expr_id();
            let expr = ast::Expr::new(ast::ExprKind::If(if_node), expr_id);
            Spanned::new(expr, span)
        })
    })
    .labelled("if expression")
    .as_context()
}

fn struct_literal<'src>(
    expr: impl AnvParser<'src, ast::ExprNode>,
) -> impl AnvParser<'src, ast::ExprNode> {
    let field_init = identifier()
        .then_ignore(select! { (Token::Colon, _) => () })
        .then(expr)
        .map(|(name, value)| (name, value));

    identifier()
        .then(
            select! { (Token::Open(Delimiter::Brace), _) => () }
                .ignore_then(
                    field_init
                        .separated_by(select! { (Token::Comma, _) => () })
                        .allow_trailing()
                        .collect::<Vec<_>>(),
                )
                .then_ignore(select! { (Token::Close(Delimiter::Brace), _) => () }),
        )
        .map_with(|(name, fields), e| {
            let s = e.span();
            let span = Span::new(s.start, s.end);
            let lit_node = Spanned::new(ast::StructLiteral { name, fields }, span);
            let expr_id = e.state().new_expr_id();
            let expr = ast::Expr::new(ast::ExprKind::StructLiteral(lit_node), expr_id);
            Spanned::new(expr, span)
        })
        .labelled("struct literal")
        .as_context()
}

fn atom_expr<'src>(
    stmt: impl AnvParser<'src, ast::StmtNode>,
    expr: impl AnvParser<'src, ast::ExprNode>,
) -> impl AnvParser<'src, ast::ExprNode> {
    choice((
        literal().map_with(|lit, e| {
            let s = e.span();
            let span = Span::new(s.start, s.end);
            let id = e.state().new_expr_id();
            let expr = ast::Expr::new(ast::ExprKind::Lit(lit), id);
            Spanned::new(expr, span)
        }),
        struct_literal(expr.clone()),
        identifier().map_with(|ident, e| {
            let s = e.span();
            let span = Span::new(s.start, s.end);
            let expr_id = e.state().new_expr_id();
            let expr = ast::Expr::new(ast::ExprKind::Ident(ident), expr_id);
            Spanned::new(expr, span)
        }),
        if_expr(stmt.clone(), expr.clone()),
        block_stmt(stmt).map_with(|block_node, e| {
            let span = block_node.span;
            let id = e.state().new_expr_id();
            let expr = ast::Expr::new(ast::ExprKind::Block(block_node), id);
            Spanned::new(expr, span)
        }),
        grouped_or_tuple_expr(expr),
    ))
    .labelled("atom")
}

enum TupleShapeResult<T> {
    Empty,
    OneTupleError(T),
    Grouped(T),
    Tuple(Vec<T>),
    UnexpectedComma,
}

fn validate_tuple_shape_raw<T>(
    first: Option<T>,
    mut rest: Vec<T>,
    trailing_comma: bool,
) -> TupleShapeResult<T> {
    match (first, rest.len(), trailing_comma) {
        (None, 0, _) => TupleShapeResult::Empty,
        (Some(single), 0, true) => TupleShapeResult::OneTupleError(single),
        (Some(single), 0, false) => TupleShapeResult::Grouped(single),
        (Some(first), _, _) => {
            rest.insert(0, first);
            TupleShapeResult::Tuple(rest)
        }
        (None, _, _) => TupleShapeResult::UnexpectedComma,
    }
}

enum TupleExprElem {
    Pos(ast::ExprNode),
    Labeled(ast::Ident, ast::ExprNode),
}

fn grouped_or_tuple_expr<'src>(
    expr: impl AnvParser<'src, ast::ExprNode>,
) -> impl AnvParser<'src, ast::ExprNode> {
    let comma = select! { (Token::Comma, _) => () };
    let open_paren = select! { (Token::Open(Delimiter::Parent), _) => () };
    let close_paren = select! { (Token::Close(Delimiter::Parent), _) => () };
    let colon = select! { (Token::Colon, _) => () };

    let labeled_elem = identifier()
        .then_ignore(colon.clone())
        .then(expr.clone())
        .map(|(name, e)| TupleExprElem::Labeled(name, e));

    let pos_elem = expr.map(TupleExprElem::Pos);
    let elem = choice((labeled_elem, pos_elem));

    let first_elem = elem.clone();
    let rest_elems = comma.ignore_then(elem).repeated().collect::<Vec<_>>();

    open_paren
        .ignore_then(first_elem.or_not())
        .then(rest_elems)
        .then(comma.or_not())
        .then_ignore(close_paren)
        .validate(|((first, rest), trailing_comma), e, emitter| {
            let s = e.span();
            let span = Span::new(s.start, s.end);
            let expr_id = e.state().new_expr_id();

            match validate_tuple_shape_raw(first, rest, trailing_comma.is_some()) {
                TupleShapeResult::Empty => {
                    emitter.emit(Rich::custom(s, "empty tuples are not supported"));
                    let dummy = ast::Expr::new(ast::ExprKind::Lit(ast::Lit::Nil), expr_id);
                    Spanned::new(dummy, span)
                }
                TupleShapeResult::OneTupleError(elem) => {
                    emitter.emit(Rich::custom(s, "1-tuples are not supported"));
                    match elem {
                        TupleExprElem::Pos(e) => e,
                        TupleExprElem::Labeled(_, e) => e,
                    }
                }
                TupleShapeResult::Grouped(elem) => match elem {
                    TupleExprElem::Pos(e) => e,
                    TupleExprElem::Labeled(_, e) => e,
                },
                TupleShapeResult::Tuple(elems) => {
                    let all_pos = elems.iter().all(|e| matches!(e, TupleExprElem::Pos(_)));
                    let all_labeled = elems
                        .iter()
                        .all(|e| matches!(e, TupleExprElem::Labeled(_, _)));

                    if all_pos {
                        let exprs: Vec<ast::ExprNode> = elems
                            .into_iter()
                            .map(|e| match e {
                                TupleExprElem::Pos(expr) => expr,
                                TupleExprElem::Labeled(_, expr) => expr,
                            })
                            .collect();
                        let tuple_expr = ast::Expr::new(ast::ExprKind::Tuple(exprs), expr_id);
                        return Spanned::new(tuple_expr, span);
                    }

                    if all_labeled {
                        let fields: Vec<(ast::Ident, ast::ExprNode)> = elems
                            .into_iter()
                            .map(|e| match e {
                                TupleExprElem::Labeled(name, expr) => (name, expr),
                                TupleExprElem::Pos(_) => unreachable!(),
                            })
                            .collect();
                        let named_tuple_expr =
                            ast::Expr::new(ast::ExprKind::NamedTuple(fields), expr_id);
                        return Spanned::new(named_tuple_expr, span);
                    }

                    emitter.emit(Rich::custom(
                        s,
                        "cannot mix labeled and unlabeled elements in tuple literal",
                    ));
                    let dummy = ast::Expr::new(ast::ExprKind::Lit(ast::Lit::Nil), expr_id);
                    return Spanned::new(dummy, span);
                }
                TupleShapeResult::UnexpectedComma => {
                    emitter.emit(Rich::custom(s, "unexpected comma"));
                    let dummy = ast::Expr::new(ast::ExprKind::Lit(ast::Lit::Nil), expr_id);
                    Spanned::new(dummy, span)
                }
            }
        })
        .labelled("tuple or grouped expression")
        .as_context()
}

fn fn_call_args<'src>(
    expr: impl AnvParser<'src, ast::ExprNode>,
) -> impl AnvParser<'src, Vec<ast::ExprNode>> {
    select! {
        (Token::Open(Delimiter::Parent), _) => (),
    }
    .ignore_then(
        expr.separated_by(select! {
            (Token::Comma, _) => (),
        })
        .allow_trailing()
        .collect::<Vec<_>>()
        .or_not()
        .map(|opt| opt.unwrap_or_default()),
    )
    .then_ignore(select! {
        (Token::Close(Delimiter::Parent), _) => (),
    })
    .labelled("function call arguments")
    .as_context()
}

fn call_type_args<'src>() -> impl AnvParser<'src, Vec<ast::Type>> {
    // lookahead for optional generic type arguments (<int, ..>)
    // and rewind to avoid consuming < when its a comparsion op (a < b)
    let generic_lookahead = select! {
        (Token::Op(Op::LessThan), _) => (),
    }
    .ignore_then(
        type_ident()
            .separated_by(select! {
                (Token::Comma, _) => (),
            })
            .allow_trailing()
            .collect::<Vec<_>>(),
    )
    .then_ignore(select! {
        (Token::Op(Op::GreaterThan), _) => (),
    })
    .then_ignore(select! {
        (Token::Open(Delimiter::Parent), _) => (),
    })
    .rewind();

    let generic_list = select! {
        (Token::Op(Op::LessThan), _) => (),
    }
    .ignore_then(
        type_ident()
            .separated_by(select! {
                (Token::Comma, _) => (),
            })
            .allow_trailing()
            .collect::<Vec<_>>(),
    )
    .then_ignore(select! {
        (Token::Op(Op::GreaterThan), _) => (),
    });

    generic_lookahead
        .ignore_then(generic_list)
        .or_not()
        .map(|opt| opt.unwrap_or_default())
        .labelled("type arguments")
        .as_context()
}

enum PostfixOp {
    Call {
        type_args: Vec<ast::Type>,
        args: Vec<ast::ExprNode>,
    },
    Field(ast::Ident),
    TupleIndices(Vec<u32>),
}

fn postfix_expr<'src>(
    atom: impl AnvParser<'src, ast::ExprNode>,
    expr: impl AnvParser<'src, ast::ExprNode>,
) -> impl AnvParser<'src, ast::ExprNode> {
    let call_suffix = call_type_args()
        .then(fn_call_args(expr))
        .map(|(type_args, args)| PostfixOp::Call { type_args, args });

    let single_index = select! {
        (Token::Dot, _) => (),
    }
    .ignore_then(select! {
        (Token::Literal(LitToken::Number(n)), _) => PostfixOp::TupleIndices(vec![n as u32]),
    });

    let chained_index = select! {
        (Token::Dot, _) => (),
    }
    .ignore_then(select! {
        (Token::Literal(LitToken::Float(s)), _) => s,
    })
    .try_map(|s, span| {
        let parts = s.as_ref().split('.').collect::<Vec<_>>();
        let indices = parts
            .iter()
            .map(|p| p.parse::<u32>())
            .collect::<Result<Vec<_>, _>>();
        indices
            .map(PostfixOp::TupleIndices)
            .map_err(|_| Rich::custom(span, "invalid tuple index"))
    });

    let field_access = select! {
        (Token::Dot, _) => (),
    }
    .ignore_then(identifier())
    .map(PostfixOp::Field);

    let postfix_op = choice((call_suffix, chained_index, single_index, field_access));

    atom.foldl_with(postfix_op.repeated(), |target, op, e| {
        let s = e.span();
        let span = Span::new(s.start, s.end);

        match op {
            PostfixOp::Call { type_args, args } => {
                let start = target.span.start;
                let end = args.last().map(|a| a.span.end).unwrap_or(target.span.end);
                let call_span = Span::new(start, end);

                let call_node = Spanned::new(
                    ast::Call {
                        func: Box::new(target),
                        args,
                        type_args,
                    },
                    call_span,
                );

                let expr_id = e.state().new_expr_id();
                let expr = ast::Expr::new(ast::ExprKind::Call(call_node), expr_id);
                Spanned::new(expr, call_span)
            }
            PostfixOp::TupleIndices(indices) => {
                let mut current = target;
                for index in indices {
                    let index_node = Spanned::new(
                        ast::TupleIndex {
                            target: Box::new(current),
                            index,
                        },
                        span,
                    );

                    let expr_id = e.state().new_expr_id();
                    let expr = ast::Expr::new(ast::ExprKind::TupleIndex(index_node), expr_id);
                    current = Spanned::new(expr, span);
                }
                current
            }
            PostfixOp::Field(field) => {
                let field_node = Spanned::new(
                    ast::FieldAccess {
                        target: Box::new(target),
                        field,
                    },
                    span,
                );

                let expr_id = e.state().new_expr_id();
                let expr = ast::Expr::new(ast::ExprKind::Field(field_node), expr_id);
                Spanned::new(expr, span)
            }
        }
    })
    .labelled("postfix expression")
    .as_context()
}

fn unary_expr<'src>(
    expr: impl AnvParser<'src, ast::ExprNode>,
) -> impl AnvParser<'src, ast::ExprNode> {
    select! {
        (Token::Op(Op::Sub), _) => ast::UnaryOp::Neg,
        (Token::Op(Op::Not), _) => ast::UnaryOp::Not,
    }
    .repeated()
    .collect::<Vec<_>>()
    .then(expr)
    .map_with(|(ops, expr), e| {
        let s = e.span();
        let span = Span::new(s.start, s.end);

        let mut expr_node = expr;
        for op in ops.into_iter().rev() {
            let unary_node = Spanned::new(
                ast::Unary {
                    op,
                    expr: Box::new(expr_node),
                },
                span,
            );
            let expr_id = e.state().new_expr_id();
            expr_node = Spanned::new(
                ast::Expr::new(ast::ExprKind::Unary(unary_node), expr_id),
                span,
            );
        }

        expr_node
    })
    .labelled("unary")
    .as_context()
}

fn binary_expr<'src>(
    term: impl AnvParser<'src, ast::ExprNode>,
) -> impl AnvParser<'src, ast::ExprNode> {
    // FIXME: precedence (sum, mul, etc...)

    let op_rhs = binary_op().then(term.clone());
    term.foldl_with(op_rhs.repeated(), |left, (op, right), e| {
        let span = Span::new(left.span.start, right.span.end);
        let bin_node = Spanned::new(
            ast::Binary {
                left: Box::new(left),
                op,
                right: Box::new(right),
            },
            span,
        );

        let expr_id = e.state().new_expr_id();
        let expr = ast::Expr::new(ast::ExprKind::Binary(bin_node), expr_id);
        Spanned::new(expr, span)
    })
    .labelled("expression")
    .as_context()
}

fn ternary_expr<'src>(
    lower: impl AnvParser<'src, ast::ExprNode>,
    expr: impl AnvParser<'src, ast::ExprNode>,
) -> impl AnvParser<'src, ast::ExprNode> {
    let ternary_suffix = select! {
        (Token::Question, _) => (),
    }
    .ignore_then(expr.clone())
    .then_ignore(select! {
        (Token::Colon, _) => (),
    })
    .then(expr);

    lower
        .foldl_with(
            ternary_suffix.repeated(),
            |cond, (then_expr, else_expr), e| {
                let span = Span::new(cond.span.start, else_expr.span.end);

                let then_stmt = Spanned::new(ast::Stmt::Expr(then_expr.clone()), then_expr.span);
                let then_block = Spanned::new(
                    ast::Block {
                        stmts: vec![then_stmt],
                    },
                    then_expr.span,
                );

                let else_stmt = Spanned::new(ast::Stmt::Expr(else_expr.clone()), else_expr.span);
                let else_block = Spanned::new(
                    ast::Block {
                        stmts: vec![else_stmt],
                    },
                    else_expr.span,
                );

                let if_node = Spanned::new(
                    ast::If {
                        cond: Box::new(cond),
                        then_block,
                        else_block: Some(else_block),
                    },
                    span,
                );

                let expr_id = e.state().new_expr_id();
                let expr = ast::Expr::new(ast::ExprKind::If(if_node), expr_id);
                Spanned::new(expr, span)
            },
        )
        .labelled("ternary")
        .as_context()
}

fn expression<'src>(
    stmt: impl AnvParser<'src, ast::StmtNode>,
) -> impl AnvParser<'src, ast::ExprNode> {
    recursive(|expr| {
        let atom = atom_expr(stmt, expr.clone());
        let postfix = postfix_expr(atom, expr.clone());
        let unary = unary_expr(postfix);
        let binary = binary_expr(unary);
        let ternary = ternary_expr(binary, expr.clone());
        let assign = assignment_expr(ternary);
        assign
    })
}

fn identifier<'src>() -> impl AnvParser<'src, ast::Ident> {
    select! {
        (Token::Ident(ident), _) => ident,
    }
    .labelled("identifier")
    .as_context()
}

fn literal<'src>() -> impl AnvParser<'src, ast::Lit> {
    select! {
        (Token::Literal(lit), _) => match lit {
            LitToken::Number(n) => ast::Lit::Int(n),
            LitToken::Float(s) => {
                s.as_ref().parse::<f64>()
                    .map(ast::Lit::Float)
                    .unwrap_or(ast::Lit::Float(0.0))
            }
            LitToken::String(s) => ast::Lit::String(s.to_string()),
        },
        (Token::Keyword(Keyword::True), _) => ast::Lit::Bool(true),
        (Token::Keyword(Keyword::False), _) => ast::Lit::Bool(false),
        (Token::Keyword(Keyword::Nil), _) => ast::Lit::Nil,
    }
    .labelled("literal")
    .as_context()
}

fn params<'src>() -> impl AnvParser<'src, Vec<ast::Param>> {
    select! {
        (Token::Open(Delimiter::Parent), _) => (),
    }
    .ignore_then(
        param()
            .separated_by(select! {
                (Token::Comma, _) => (),
            })
            .collect::<Vec<_>>()
            .or_not()
            .map(|opt| opt.unwrap_or_default()),
    )
    .then_ignore(select! {
        (Token::Close(Delimiter::Parent), _) => (),
    })
}

fn param<'src>() -> impl AnvParser<'src, ast::Param> {
    identifier()
        .then_ignore(select! {
            (Token::Colon, _) => (),
        })
        .then(type_ident())
        .map(|(name, ty)| ast::Param { name, ty })
        .labelled("parameter")
        .as_context()
}

fn return_type<'src>() -> impl AnvParser<'src, Option<ast::Type>> {
    select! {
        (Token::Op(Op::ThinArrow), _) => (),
    }
    .ignore_then(type_ident())
    .or_not()
    .labelled("return type")
    .as_context()
}

fn type_ident<'src>() -> impl AnvParser<'src, ast::Type> {
    recursive(|type_parser| {
        let builtin_typ = select! {
            (Token::Keyword(Keyword::Int), _) => ast::Type::Int,
            (Token::Keyword(Keyword::Float), _) => ast::Type::Float,
            (Token::Keyword(Keyword::Bool), _) => ast::Type::Bool,
            (Token::Keyword(Keyword::String), _) => ast::Type::String,
            (Token::Keyword(Keyword::Void), _) => ast::Type::Void,
        };

        let type_args = select! { (Token::Op(Op::LessThan), _) => () }
            .ignore_then(
                type_parser
                    .clone()
                    .separated_by(select! { (Token::Comma, _) => () })
                    .allow_trailing()
                    .collect::<Vec<_>>(),
            )
            .then_ignore(select! { (Token::Op(Op::GreaterThan), _) => () });

        let type_name_ref = identifier()
            .then(type_args.or_not())
            .map(|(name, args)| match args {
                Some(type_args) => ast::Type::Struct { name, type_args },
                None => ast::Type::UnresolvedName(name),
            });

        // (T) is grouping, (T, T, ...) is tuple
        let paren_type = paren_or_tuple_type(type_parser.clone());

        // try built-in types first, then type name references, then parenthesized/tuple types
        let typ = choice((builtin_typ, type_name_ref, paren_type));
        typ.then(
            select! {
                (Token::Question, _) => (),
            }
            .or_not(),
        )
        .map(|(ty, q)| {
            if q.is_some() {
                ast::Type::Optional(ty.boxed())
            } else {
                ty
            }
        })
    })
    .labelled("type")
    .as_context()
}

enum TupleTypeElem {
    Pos(ast::Type),
    Labeled(ast::Ident, ast::Type),
}

fn paren_or_tuple_type<'src>(
    type_parser: impl AnvParser<'src, ast::Type>,
) -> impl AnvParser<'src, ast::Type> {
    let comma = select! { (Token::Comma, _) => () };
    let open_paren = select! { (Token::Open(Delimiter::Parent), _) => () };
    let close_paren = select! { (Token::Close(Delimiter::Parent), _) => () };
    let colon = select! { (Token::Colon, _) => () };

    let labeled_elem = identifier()
        .then_ignore(colon.clone())
        .then(type_parser.clone())
        .map(|(name, ty)| TupleTypeElem::Labeled(name, ty));

    let pos_elem = type_parser.map(TupleTypeElem::Pos);
    let elem = choice((labeled_elem, pos_elem));

    let first_elem = elem.clone();
    let rest_elems = comma.ignore_then(elem).repeated().collect::<Vec<_>>();

    open_paren
        .ignore_then(first_elem.or_not())
        .then(rest_elems)
        .then(comma.or_not())
        .then_ignore(close_paren)
        .validate(|((first, rest), trailing_comma), e, emitter| {
            let s = e.span();

            match validate_tuple_shape_raw(first, rest, trailing_comma.is_some()) {
                TupleShapeResult::Empty => {
                    emitter.emit(Rich::custom(s, "empty tuples are not supported"));
                    ast::Type::Void
                }
                TupleShapeResult::OneTupleError(elem) => {
                    emitter.emit(Rich::custom(s, "1-tuples are not supported"));
                    match elem {
                        TupleTypeElem::Pos(ty) => ty,
                        TupleTypeElem::Labeled(_, ty) => ty,
                    }
                }
                TupleShapeResult::Grouped(elem) => match elem {
                    TupleTypeElem::Pos(ty) => ty,
                    TupleTypeElem::Labeled(_, ty) => ty,
                },
                TupleShapeResult::Tuple(elems) => {
                    let all_pos = elems.iter().all(|e| matches!(e, TupleTypeElem::Pos(_)));
                    let all_labeled = elems
                        .iter()
                        .all(|e| matches!(e, TupleTypeElem::Labeled(_, _)));

                    if all_pos {
                        let types: Vec<ast::Type> = elems
                            .into_iter()
                            .map(|e| match e {
                                TupleTypeElem::Pos(ty) => ty,
                                TupleTypeElem::Labeled(_, ty) => ty,
                            })
                            .collect();
                        return ast::Type::Tuple(types);
                    }

                    if all_labeled {
                        let fields: Vec<(ast::Ident, ast::Type)> = elems
                            .into_iter()
                            .map(|e| match e {
                                TupleTypeElem::Labeled(name, ty) => (name, ty),
                                TupleTypeElem::Pos(_) => unreachable!(),
                            })
                            .collect();
                        return ast::Type::NamedTuple(fields);
                    }

                    emitter.emit(Rich::custom(
                        s,
                        "cannot mix labeled and unlabeled elements in tuple type",
                    ));
                    return ast::Type::Void;
                }
                TupleShapeResult::UnexpectedComma => {
                    emitter.emit(Rich::custom(s, "unexpected comma"));
                    ast::Type::Void
                }
            }
        })
        .labelled("tuple or grouped type")
        .as_context()
}

fn pattern<'src>() -> impl AnvParser<'src, ast::PatternNode> {
    recursive(|pat| {
        let ident_or_wildcard = identifier().map_with(|ident, e| {
            let s = e.span();
            let span = Span::new(s.start, s.end);
            if ident.0.as_ref() == "_" {
                Spanned::new(ast::Pattern::Wildcard, span)
            } else {
                Spanned::new(ast::Pattern::Ident(ident), span)
            }
        });

        let tuple_pat = tuple_pattern(pat);

        choice((tuple_pat, ident_or_wildcard))
    })
    .labelled("pattern")
    .as_context()
}

enum TuplePatternElem {
    Pos(ast::PatternNode),
    Labeled(ast::Ident, ast::PatternNode),
}

fn tuple_pattern<'src>(
    pat: impl AnvParser<'src, ast::PatternNode>,
) -> impl AnvParser<'src, ast::PatternNode> {
    let comma = select! { (Token::Comma, _) => () };
    let open_paren = select! { (Token::Open(Delimiter::Parent), _) => () };
    let close_paren = select! { (Token::Close(Delimiter::Parent), _) => () };
    let colon = select! { (Token::Colon, _) => () };

    let labeled_elem = identifier()
        .then_ignore(colon.clone())
        .then(pat.clone())
        .map(|(name, p)| TuplePatternElem::Labeled(name, p));

    let pos_elem = pat.map(TuplePatternElem::Pos);
    let elem = choice((labeled_elem, pos_elem));

    let first_elem = elem.clone();
    let rest_elems = comma.ignore_then(elem).repeated().collect::<Vec<_>>();

    open_paren
        .ignore_then(first_elem.or_not())
        .then(rest_elems)
        .then(comma.or_not())
        .then_ignore(close_paren)
        .validate(|((first, rest), trailing_comma), e, emitter| {
            let s = e.span();
            let span = Span::new(s.start, s.end);

            match validate_tuple_shape_raw(first, rest, trailing_comma.is_some()) {
                TupleShapeResult::Empty => {
                    emitter.emit(Rich::custom(s, "empty tuple patterns are not supported"));
                    Spanned::new(ast::Pattern::Wildcard, span)
                }
                TupleShapeResult::OneTupleError(elem) => {
                    emitter.emit(Rich::custom(s, "1-tuple patterns are not supported"));
                    match elem {
                        TuplePatternElem::Pos(p) => p,
                        TuplePatternElem::Labeled(_, p) => p,
                    }
                }
                TupleShapeResult::Grouped(elem) => match elem {
                    TuplePatternElem::Pos(p) => p,
                    TuplePatternElem::Labeled(_, p) => p,
                },
                TupleShapeResult::Tuple(elems) => {
                    let all_pos = elems.iter().all(|e| matches!(e, TuplePatternElem::Pos(_)));
                    let all_labeled = elems
                        .iter()
                        .all(|e| matches!(e, TuplePatternElem::Labeled(_, _)));

                    if all_pos {
                        let pats: Vec<ast::PatternNode> = elems
                            .into_iter()
                            .map(|e| match e {
                                TuplePatternElem::Pos(p) => p,
                                TuplePatternElem::Labeled(_, p) => p,
                            })
                            .collect();
                        return Spanned::new(ast::Pattern::Tuple(pats), span);
                    }

                    if all_labeled {
                        let fields: Vec<(ast::Ident, ast::PatternNode)> = elems
                            .into_iter()
                            .map(|e| match e {
                                TuplePatternElem::Labeled(name, p) => (name, p),
                                TuplePatternElem::Pos(_) => unreachable!(),
                            })
                            .collect();
                        return Spanned::new(ast::Pattern::NamedTuple(fields), span);
                    }

                    emitter.emit(Rich::custom(
                        s,
                        "cannot mix labeled and unlabeled elements in tuple pattern",
                    ));
                    Spanned::new(ast::Pattern::Wildcard, span)
                }
                TupleShapeResult::UnexpectedComma => {
                    emitter.emit(Rich::custom(s, "unexpected comma in pattern"));
                    Spanned::new(ast::Pattern::Wildcard, span)
                }
            }
        })
        .labelled("tuple pattern")
        .as_context()
}

fn binding<'src>(
    stmt: impl AnvParser<'src, ast::StmtNode>,
) -> impl AnvParser<'src, ast::BindingNode> {
    let mutability = select! {
        (Token::Keyword(Keyword::Let), _) => ast::Mutability::Immutable,
        (Token::Keyword(Keyword::Var), _) => ast::Mutability::Mutable,
    };

    mutability
        .then(pattern())
        .then(
            select! {
                (Token::Colon, _) => (),
            }
            .ignore_then(type_ident())
            .or_not(),
        )
        .then_ignore(select! {
            (Token::Op(Op::Assign), _) => (),
        })
        .then(expression(stmt))
        .then_ignore(select! {
            (Token::Semicolon, _) => (),
        })
        .map_with(|(((mutability, pat), ty), value), e| {
            let s = e.span();
            Spanned::new(
                ast::Binding {
                    pattern: pat,
                    ty,
                    mutability,
                    value,
                },
                Span::new(s.start, s.end),
            )
        })
}

fn binary_op<'src>() -> impl AnvParser<'src, ast::BinaryOp> {
    select! {
        (Token::Op(Op::Add), _) => ast::BinaryOp::Add,
        (Token::Op(Op::Sub), _) => ast::BinaryOp::Sub,
        (Token::Op(Op::Mul), _) => ast::BinaryOp::Mul,
        (Token::Op(Op::Div), _) => ast::BinaryOp::Div,
        (Token::Op(Op::Rem), _) => ast::BinaryOp::Rem,
        (Token::Op(Op::Eq), _) => ast::BinaryOp::Eq,
        (Token::Op(Op::NotEq), _) => ast::BinaryOp::NotEq,
        (Token::Op(Op::LessThan), _) => ast::BinaryOp::LessThan,
        (Token::Op(Op::GreaterThan), _) => ast::BinaryOp::GreaterThan,
        (Token::Op(Op::LessThanEq), _) => ast::BinaryOp::LessThanEq,
        (Token::Op(Op::GreaterThanEq), _) => ast::BinaryOp::GreaterThanEq,
        (Token::Op(Op::And), _) => ast::BinaryOp::And,
        (Token::Op(Op::Or), _) => ast::BinaryOp::Or,
        (Token::Op(Op::Coalesce), _) => ast::BinaryOp::Coalesce,
    }
    .labelled("binary op")
    .as_context()
}

fn assign_op<'src>() -> impl AnvParser<'src, ast::AssignOp> {
    select! {
        (Token::Op(Op::Assign), _) => ast::AssignOp::Assign,
        (Token::Op(Op::AddAssign), _) => ast::AssignOp::AddAssign,
        (Token::Op(Op::SubAssign), _) => ast::AssignOp::SubAssign,
        (Token::Op(Op::MulAssign), _) => ast::AssignOp::MulAssign,
        (Token::Op(Op::DivAssign), _) => ast::AssignOp::DivAssign,
    }
    .labelled("assign op")
    .as_context()
}

fn lvalue_expr<'src>() -> impl AnvParser<'src, ast::ExprNode> {
    let base = identifier().map_with(|ident, e| {
        let s = e.span();
        let span = Span::new(s.start, s.end);
        let expr_id = e.state().new_expr_id();
        let expr = ast::Expr::new(ast::ExprKind::Ident(ident), expr_id);
        Spanned::new(expr, span)
    });

    let field_suffix = select! { (Token::Dot, _) => () }.ignore_then(identifier());

    base.foldl_with(field_suffix.repeated(), |target, field, e| {
        let s = e.span();
        let span = Span::new(s.start, s.end);
        let field_node = Spanned::new(
            ast::FieldAccess {
                target: Box::new(target),
                field,
            },
            span,
        );
        let expr_id = e.state().new_expr_id();
        let expr = ast::Expr::new(ast::ExprKind::Field(field_node), expr_id);
        Spanned::new(expr, span)
    })
    .labelled("left value expr")
    .as_context()
}

fn assignment_expr<'src>(
    expr: impl AnvParser<'src, ast::ExprNode>,
) -> impl AnvParser<'src, ast::ExprNode> {
    lvalue_expr()
        .then(assign_op().then(expr.clone()))
        .map_with(|(target, (op, value)), e| {
            let s = e.span();
            let span = Span::new(s.start, s.end);
            let assign_node = Spanned::new(
                ast::Assign {
                    target: Box::new(target),
                    op,
                    value: Box::new(value),
                },
                span,
            );

            let expr_id = e.state().new_expr_id();
            let expr = ast::Expr::new(ast::ExprKind::Assign(assign_node), expr_id);
            Spanned::new(expr, span)
        })
        .or(expr)
        .labelled("assignment")
        .as_context()
}

fn return_stmt<'src>(
    stmt: impl AnvParser<'src, ast::StmtNode>,
) -> impl AnvParser<'src, ast::StmtNode> {
    select! {
        (Token::Keyword(Keyword::Return), _) => (),
    }
    .ignore_then(expression(stmt).or_not())
    .then_ignore(select! {
        (Token::Semicolon, _) => (),
    })
    .map_with(|value_opt, e| {
        let s = e.span();
        let span = Span::new(s.start, s.end);
        let ret = ast::Return { value: value_opt };
        Spanned::new(ast::Stmt::Return(Spanned::new(ret, span)), span)
    })
    .labelled("return")
    .as_context()
}
