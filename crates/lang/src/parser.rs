use crate::{
    ast::{self, ExprId, TypeVarId},
    lexer::{Delimiter, Keyword, LitToken, Op, SpannedToken, Token},
    span::{Span, Spanned},
};
use chumsky::{
    Boxed,
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

// It seems tht rustc chokes trying to compile the parser types, so we need to box them
// in order to reduce the chusmky generic types :(
// I feel this is fine for now, the parser should still be fast enough for my tiny lang
type BoxedParser<'src, T> = Boxed<'src, 'src, Input<'src>, T, Extra<'src>>;

pub fn parse_ast(tokens: &[SpannedToken]) -> Result<ast::Program, Vec<Rich<'_, SpannedToken>>> {
    let mut state = SimpleState(ParserState::default());
    parser().parse_with_state(tokens, &mut state).into_result()
}

fn parser<'src>() -> BoxedParser<'src, ast::Program> {
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
        .boxed()
}

fn statement<'src>() -> BoxedParser<'src, ast::StmtNode> {
    recursive(|stmt| {
        let expr = expression(stmt.clone());
        let func = function(stmt.clone());
        let bind = binding(stmt.clone());
        let ret = return_stmt(stmt.clone());
        let while_s = while_stmt(stmt.clone());
        let for_s = for_stmt(stmt.clone());
        let break_s = break_stmt();
        let continue_s = continue_stmt();

        let at_block_end = select! { (Token::Close(Delimiter::Brace), _) => () }.rewind();

        let at_stmt_start = select! {
            (Token::Keyword(Keyword::Let), _) => (),
            (Token::Keyword(Keyword::Var), _) => (),
            (Token::Keyword(Keyword::Return), _) => (),
            (Token::Keyword(Keyword::Fn), _) => (),
            (Token::Keyword(Keyword::If), _) => (),
            (Token::Keyword(Keyword::Struct), _) => (),
            (Token::Keyword(Keyword::While), _) => (),
            (Token::Keyword(Keyword::For), _) => (),
            (Token::Keyword(Keyword::Break), _) => (),
            (Token::Keyword(Keyword::Continue), _) => (),
        }
        .rewind();

        let at_assign_start = select! { (Token::Ident(_), _) => () }
            .then(select! {
                (Token::Op(Op::Assign), _) => (),
                (Token::Op(Op::AddAssign), _) => (),
                (Token::Op(Op::SubAssign), _) => (),
                (Token::Op(Op::MulAssign), _) => (),
                (Token::Op(Op::DivAssign), _) => (),
            })
            .to(())
            .rewind();

        let expr_stmt = expr
            .then_ignore(
                select! { (Token::Semicolon, _) => () }
                    .or(at_block_end)
                    .or(at_stmt_start)
                    .or(at_assign_start),
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
            while_s,
            for_s,
            break_s,
            continue_s,
            expr_stmt,
        ))
    })
    .labelled("statement")
    .as_context()
    .boxed()
}

fn type_params<'src>() -> BoxedParser<'src, Vec<ast::TypeParam>> {
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
    .boxed()
}

fn function<'src>(stmt: impl AnvParser<'src, ast::StmtNode>) -> BoxedParser<'src, ast::FuncNode> {
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
    .boxed()
}

fn struct_field<'src>() -> BoxedParser<'src, ast::StructField> {
    identifier()
        .then_ignore(select! {
            (Token::Colon, _) => (),
        })
        .then(type_ident())
        .map(|(name, ty)| ast::StructField { name, ty })
        .labelled("struct field")
        .as_context()
        .boxed()
}

enum StructMember {
    Field(ast::StructField),
    Method(ast::Method),
}

fn struct_method<'src>(
    stmt: impl AnvParser<'src, ast::StmtNode>,
) -> BoxedParser<'src, ast::Method> {
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
    .boxed()
}

fn method_params<'src>() -> BoxedParser<'src, (Option<ast::MethodReceiver>, Vec<ast::Param>)> {
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
    .boxed()
}

fn self_param<'src>() -> BoxedParser<'src, ()> {
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
        .boxed()
}

fn method_param_list<'src>() -> BoxedParser<'src, (Option<ast::MethodReceiver>, Vec<ast::Param>)> {
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
    .boxed()
}

fn struct_member<'src>(
    stmt: impl AnvParser<'src, ast::StmtNode>,
) -> BoxedParser<'src, StructMember> {
    choice((
        struct_method(stmt).map(StructMember::Method),
        struct_field().map(StructMember::Field),
    ))
    .boxed()
}

fn struct_declaration<'src>(
    stmt: impl AnvParser<'src, ast::StmtNode>,
) -> BoxedParser<'src, ast::StructDeclNode> {
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
    .boxed()
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

        Array { elem, len } => Array {
            elem: resolve_type_params_with_self(elem, type_param_map, self_type).boxed(),
            len: *len,
        },

        ArrayView { elem } => ArrayView {
            elem: resolve_type_params_with_self(elem, type_param_map, self_type).boxed(),
        },

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
) -> BoxedParser<'src, ast::BlockNode> {
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
    .boxed()
}

fn if_expr<'src>(stmt: impl AnvParser<'src, ast::StmtNode>) -> BoxedParser<'src, ast::ExprNode> {
    recursive(|if_parser| {
        let cond_expr = cond_expression();

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
        .ignore_then(cond_expr)
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
    .boxed()
}

fn struct_literal<'src>(
    expr: impl AnvParser<'src, ast::ExprNode>,
) -> BoxedParser<'src, ast::ExprNode> {
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
        .boxed()
}

fn array_literal<'src>(
    expr: impl AnvParser<'src, ast::ExprNode>,
) -> BoxedParser<'src, ast::ExprNode> {
    let open_bracket = select! { (Token::Open(Delimiter::Bracket), _) => () };
    let close_bracket = select! { (Token::Close(Delimiter::Bracket), _) => () };
    let comma = select! { (Token::Comma, _) => () };
    let semicolon = select! { (Token::Semicolon, _) => () };

    let fill_literal = open_bracket
        .clone()
        .ignore_then(expr.clone())
        .then_ignore(semicolon)
        .then(expr.clone())
        .then_ignore(close_bracket.clone())
        .map_with(|(value, len), e| {
            let s = e.span();
            let span = Span::new(s.start, s.end);
            let fill_node = Spanned::new(
                ast::ArrayFill {
                    value: Box::new(value),
                    len: Box::new(len),
                },
                span,
            );
            let expr_id = e.state().new_expr_id();
            let expr = ast::Expr::new(ast::ExprKind::ArrayFill(fill_node), expr_id);
            Spanned::new(expr, span)
        });

    let element_list = open_bracket
        .ignore_then(
            expr.separated_by(comma)
                .allow_trailing()
                .collect::<Vec<_>>(),
        )
        .then_ignore(close_bracket)
        .map_with(|elements, e| {
            let s = e.span();
            let span = Span::new(s.start, s.end);
            let lit_node = Spanned::new(ast::ArrayLiteral { elements }, span);
            let expr_id = e.state().new_expr_id();
            let expr = ast::Expr::new(ast::ExprKind::ArrayLiteral(lit_node), expr_id);
            Spanned::new(expr, span)
        });

    choice((fill_literal, element_list))
        .labelled("array literal")
        .as_context()
        .boxed()
}

fn atom_expr<'src>(
    stmt: impl AnvParser<'src, ast::StmtNode>,
    expr: impl AnvParser<'src, ast::ExprNode>,
) -> BoxedParser<'src, ast::ExprNode> {
    choice((
        literal().map_with(|lit, e| {
            let s = e.span();
            let span = Span::new(s.start, s.end);
            let id = e.state().new_expr_id();
            let expr = ast::Expr::new(ast::ExprKind::Lit(lit), id);
            Spanned::new(expr, span)
        }),
        struct_literal(expr.clone()),
        array_literal(expr.clone()),
        identifier().map_with(|ident, e| {
            let s = e.span();
            let span = Span::new(s.start, s.end);
            let expr_id = e.state().new_expr_id();
            let expr = ast::Expr::new(ast::ExprKind::Ident(ident), expr_id);
            Spanned::new(expr, span)
        }),
        if_expr(stmt.clone()),
        block_stmt(stmt).map_with(|block_node, e| {
            let span = block_node.span;
            let id = e.state().new_expr_id();
            let expr = ast::Expr::new(ast::ExprKind::Block(block_node), id);
            Spanned::new(expr, span)
        }),
        grouped_or_tuple_expr(expr),
    ))
    .labelled("atom")
    .boxed()
}

fn cond_atom_expr<'src>(
    cond_expr: impl AnvParser<'src, ast::ExprNode>,
) -> BoxedParser<'src, ast::ExprNode> {
    choice((
        literal().map_with(|lit, e| {
            let s = e.span();
            let span = Span::new(s.start, s.end);
            let id = e.state().new_expr_id();
            let expr = ast::Expr::new(ast::ExprKind::Lit(lit), id);
            Spanned::new(expr, span)
        }),
        array_literal(cond_expr.clone()),
        identifier().map_with(|ident, e| {
            let s = e.span();
            let span = Span::new(s.start, s.end);
            let expr_id = e.state().new_expr_id();
            let expr = ast::Expr::new(ast::ExprKind::Ident(ident), expr_id);
            Spanned::new(expr, span)
        }),
        grouped_or_tuple_expr(cond_expr),
    ))
    .labelled("condition atom")
    .boxed()
}

fn cond_expression<'src>() -> BoxedParser<'src, ast::ExprNode> {
    recursive(|cond_expr| {
        let atom = cond_atom_expr(cond_expr.clone());
        let postfix = postfix_expr(atom, cond_expr.clone());
        let unary = unary_expr(postfix);
        let binary = binary_expr(unary);
        let ternary = ternary_expr(binary, cond_expr.clone());
        let assign = assignment_expr(ternary);
        assign
    })
    .boxed()
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
) -> BoxedParser<'src, ast::ExprNode> {
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
        .boxed()
}

fn fn_call_args<'src>(
    expr: impl AnvParser<'src, ast::ExprNode>,
) -> BoxedParser<'src, Vec<ast::ExprNode>> {
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
    .boxed()
}

fn call_type_args<'src>() -> BoxedParser<'src, Vec<ast::Type>> {
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
        .boxed()
}

enum PostfixOp {
    Call {
        type_args: Vec<ast::Type>,
        args: Vec<ast::ExprNode>,
    },
    Field(ast::Ident),
    TupleIndices(Vec<u32>),
    Index(ast::ExprNode),
}

fn postfix_expr<'src>(
    atom: impl AnvParser<'src, ast::ExprNode>,
    expr: impl AnvParser<'src, ast::ExprNode>,
) -> BoxedParser<'src, ast::ExprNode> {
    let call_suffix = call_type_args()
        .then(fn_call_args(expr.clone()))
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

    let index_suffix = select! { (Token::Open(Delimiter::Bracket), _) => () }
        .ignore_then(expr)
        .then_ignore(select! { (Token::Close(Delimiter::Bracket), _) => () })
        .map(PostfixOp::Index);

    let postfix_op = choice((
        call_suffix,
        index_suffix,
        chained_index,
        single_index,
        field_access,
    ));

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
            PostfixOp::Index(index_expr) => {
                let start = target.span.start;
                let end = span.end;
                let index_span = Span::new(start, end);

                let index_node = Spanned::new(
                    ast::Index {
                        target: Box::new(target),
                        index: Box::new(index_expr),
                    },
                    index_span,
                );

                let expr_id = e.state().new_expr_id();
                let expr = ast::Expr::new(ast::ExprKind::Index(index_node), expr_id);
                Spanned::new(expr, index_span)
            }
        }
    })
    .labelled("postfix expression")
    .as_context()
    .boxed()
}

fn unary_expr<'src>(expr: impl AnvParser<'src, ast::ExprNode>) -> BoxedParser<'src, ast::ExprNode> {
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
    .boxed()
}

fn infix_left<'src>(
    lower: impl AnvParser<'src, ast::ExprNode>,
    op: impl AnvParser<'src, ast::BinaryOp>,
) -> BoxedParser<'src, ast::ExprNode> {
    let op_rhs = op.then(lower.clone());
    lower
        .foldl_with(op_rhs.repeated(), |left, (op, right), e| {
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
        .boxed()
}

fn binary_expr<'src>(
    unary: impl AnvParser<'src, ast::ExprNode>,
) -> BoxedParser<'src, ast::ExprNode> {
    let mul = infix_left(unary, mul_div_op());
    let add = infix_left(mul, add_sub_op());
    let range = range_expr(add);
    let cmp = infix_left(range, cmp_op());
    let eq = infix_left(cmp, eq_op());
    let and = infix_left(eq, and_op());
    let coal = infix_left(and, coalesce_op());
    let or = infix_left(coal, or_op());
    or.labelled("expression").as_context().boxed()
}

fn range_expr<'src>(
    lower: impl AnvParser<'src, ast::ExprNode>,
) -> BoxedParser<'src, ast::ExprNode> {
    let op_rhs = select! {
        (Token::Range, _) => false,
        (Token::RangeEq, _) => true,
    }
    .then(lower.clone());

    lower
        .foldl_with(op_rhs.repeated(), |start, (inclusive, end), e| {
            let span = Span::new(start.span.start, end.span.end);
            let range_node = Spanned::new(
                ast::Range {
                    start: Box::new(start),
                    end: Box::new(end),
                    inclusive,
                },
                span,
            );
            let expr_id = e.state().new_expr_id();
            let expr = ast::Expr::new(ast::ExprKind::Range(range_node), expr_id);
            Spanned::new(expr, span)
        })
        .boxed()
}

fn ternary_expr<'src>(
    lower: impl AnvParser<'src, ast::ExprNode>,
    expr: impl AnvParser<'src, ast::ExprNode>,
) -> BoxedParser<'src, ast::ExprNode> {
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
        .boxed()
}

fn expression<'src>(stmt: impl AnvParser<'src, ast::StmtNode>) -> BoxedParser<'src, ast::ExprNode> {
    recursive(|expr| {
        let atom = atom_expr(stmt, expr.clone());
        let postfix = postfix_expr(atom, expr.clone());
        let unary = unary_expr(postfix);
        let binary = binary_expr(unary);
        let ternary = ternary_expr(binary, expr.clone());
        let assign = assignment_expr(ternary);
        assign
    })
    .boxed()
}

fn identifier<'src>() -> BoxedParser<'src, ast::Ident> {
    select! {
        (Token::Ident(ident), _) => ident,
    }
    .labelled("identifier")
    .as_context()
    .boxed()
}

fn literal<'src>() -> BoxedParser<'src, ast::Lit> {
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
    .boxed()
}

fn params<'src>() -> BoxedParser<'src, Vec<ast::Param>> {
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
    .boxed()
}

fn param<'src>() -> BoxedParser<'src, ast::Param> {
    identifier()
        .then_ignore(select! {
            (Token::Colon, _) => (),
        })
        .then(type_ident())
        .map(|(name, ty)| ast::Param { name, ty })
        .labelled("parameter")
        .as_context()
        .boxed()
}

fn return_type<'src>() -> BoxedParser<'src, Option<ast::Type>> {
    select! {
        (Token::Op(Op::ThinArrow), _) => (),
    }
    .ignore_then(type_ident())
    .or_not()
    .labelled("return type")
    .as_context()
    .boxed()
}

#[derive(Clone)]
enum TypeSuffix {
    Optional,
}

fn type_ident<'src>() -> BoxedParser<'src, ast::Type> {
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

        let paren_type = paren_or_tuple_type(type_parser.clone());

        let open_bracket = select! { (Token::Open(Delimiter::Bracket), _) => () };
        let close_bracket = select! { (Token::Close(Delimiter::Bracket), _) => () };
        let semicolon = select! { (Token::Semicolon, _) => () };
        let colon = select! { (Token::Colon, _) => () };

        let array_len_fixed =
            select! { (Token::Literal(LitToken::Number(n)), _) => ast::ArrayLen::Fixed(n as usize) };
        let array_len_infer = identifier().try_map(|ident, span| {
            if ident.0.as_ref() == "_" {
                Ok(ast::ArrayLen::Infer)
            } else {
                Err(Rich::custom(span, "expected '_' or integer literal"))
            }
        });
        let array_len = choice((array_len_fixed, array_len_infer));

        let view_marker = select! { (Token::Range, _) => () };

        let array_or_view_type = open_bracket
            .clone()
            .ignore_then(type_parser.clone())
            .then_ignore(semicolon)
            .then(
                choice((
                    view_marker.to(None),
                    array_len.map(Some),
                )),
            )
            .then_ignore(close_bracket.clone())
            .map(|(elem, len)| match len {
                Some(len) => ast::Type::Array {
                    elem: elem.boxed(),
                    len,
                },
                None => ast::Type::ArrayView { elem: elem.boxed() },
            });

        let map_type = open_bracket
            .clone()
            .ignore_then(type_parser.clone())
            .then_ignore(colon)
            .then(type_parser.clone())
            .then_ignore(close_bracket.clone())
            .map(|(key, value)| ast::Type::Map {
                key: key.boxed(),
                value: value.boxed(),
            });

        let list_type = open_bracket
            .ignore_then(type_parser.clone())
            .then_ignore(close_bracket)
            .map(|elem| ast::Type::List { elem: elem.boxed() });

        let bracketed_type = choice((array_or_view_type, map_type, list_type));
        let primary_type = choice((builtin_typ, type_name_ref, paren_type, bracketed_type));
        let optional_suffix = select! { (Token::Question, _) => TypeSuffix::Optional };
        let type_suffix = optional_suffix;

        primary_type
            .then(type_suffix.repeated().collect::<Vec<_>>())
            .map(|(base, suffixes)| {
                suffixes.into_iter().fold(base, |ty, sfx| match sfx {
                    TypeSuffix::Optional => ast::Type::Optional(ty.boxed()),
                })
            })
    })
    .labelled("type")
    .as_context()
    .boxed()
}

enum TupleTypeElem {
    Pos(ast::Type),
    Labeled(ast::Ident, ast::Type),
}

fn paren_or_tuple_type<'src>(
    type_parser: impl AnvParser<'src, ast::Type>,
) -> BoxedParser<'src, ast::Type> {
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
        .boxed()
}

fn pattern<'src>() -> BoxedParser<'src, ast::PatternNode> {
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
    .boxed()
}

enum TuplePatternElem {
    Pos(ast::PatternNode),
    Labeled(ast::Ident, ast::PatternNode),
}

fn tuple_pattern<'src>(
    pat: impl AnvParser<'src, ast::PatternNode>,
) -> BoxedParser<'src, ast::PatternNode> {
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
        .boxed()
}

fn binding<'src>(stmt: impl AnvParser<'src, ast::StmtNode>) -> BoxedParser<'src, ast::BindingNode> {
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
        .boxed()
}

fn while_stmt<'src>(stmt: impl AnvParser<'src, ast::StmtNode>) -> BoxedParser<'src, ast::StmtNode> {
    let cond_expr = cond_expression();

    select! {
        (Token::Keyword(Keyword::While), _) => (),
    }
    .ignore_then(cond_expr)
    .then(block_stmt(stmt))
    .map_with(|(cond, body), e| {
        let s = e.span();
        let span = Span::new(s.start, s.end);
        let while_node = Spanned::new(ast::While { cond, body }, span);
        Spanned::new(ast::Stmt::While(while_node), span)
    })
    .labelled("while statement")
    .as_context()
    .boxed()
}

// rev key is only valid on for loops
fn contextual_rev<'src>() -> BoxedParser<'src, bool> {
    select! {
        (Token::Ident(ident), _) if ident.0.as_ref() == "rev" => true,
    }
    .or_not()
    .map(|o| o.unwrap_or(false))
    .boxed()
}

// step key is only valid on for loops
fn contextual_step<'src>(
    stmt: impl AnvParser<'src, ast::StmtNode>,
) -> BoxedParser<'src, Option<ast::ExprNode>> {
    select! {
        (Token::Ident(ident), _) if ident.0.as_ref() == "step" => (),
    }
    .ignore_then(expression(stmt))
    .or_not()
    .boxed()
}

fn for_stmt<'src>(stmt: impl AnvParser<'src, ast::StmtNode>) -> BoxedParser<'src, ast::StmtNode> {
    select! {
        (Token::Keyword(Keyword::For), _) => (),
    }
    .ignore_then(pattern())
    .then_ignore(select! {
        (Token::Keyword(Keyword::In), _) => (),
    })
    .then(contextual_rev())
    .then(expression(stmt.clone()))
    .then(contextual_step(stmt.clone()))
    .then(block_stmt(stmt))
    .map_with(|((((pat, reversed), iterable), step), body), e| {
        let s = e.span();
        let span = Span::new(s.start, s.end);
        let for_node = Spanned::new(
            ast::For {
                pattern: pat,
                iterable,
                step,
                reversed,
                body,
            },
            span,
        );
        Spanned::new(ast::Stmt::For(for_node), span)
    })
    .labelled("for statement")
    .as_context()
    .boxed()
}

fn break_stmt<'src>() -> BoxedParser<'src, ast::StmtNode> {
    select! {
        (Token::Keyword(Keyword::Break), _) => (),
    }
    .then_ignore(select! {
        (Token::Semicolon, _) => (),
    })
    .map_with(|(), e| {
        let s: chumsky::span::SimpleSpan<usize> = e.span();
        let span = Span::new(s.start, s.end);
        Spanned::new(ast::Stmt::Break, span)
    })
    .labelled("break statement")
    .as_context()
    .boxed()
}

fn continue_stmt<'src>() -> BoxedParser<'src, ast::StmtNode> {
    select! {
        (Token::Keyword(Keyword::Continue), _) => (),
    }
    .then_ignore(select! {
        (Token::Semicolon, _) => (),
    })
    .map_with(|(), e| {
        let s: chumsky::span::SimpleSpan<usize> = e.span();
        let span = Span::new(s.start, s.end);
        Spanned::new(ast::Stmt::Continue, span)
    })
    .labelled("continue statement")
    .as_context()
    .boxed()
}

fn mul_div_op<'src>() -> BoxedParser<'src, ast::BinaryOp> {
    select! {
        (Token::Op(Op::Mul), _) => ast::BinaryOp::Mul,
        (Token::Op(Op::Div), _) => ast::BinaryOp::Div,
        (Token::Op(Op::Rem), _) => ast::BinaryOp::Rem,
    }
    .labelled("multiplicative op")
    .as_context()
    .boxed()
}

fn add_sub_op<'src>() -> BoxedParser<'src, ast::BinaryOp> {
    select! {
        (Token::Op(Op::Add), _) => ast::BinaryOp::Add,
        (Token::Op(Op::Sub), _) => ast::BinaryOp::Sub,
    }
    .labelled("additive op")
    .as_context()
    .boxed()
}

fn cmp_op<'src>() -> BoxedParser<'src, ast::BinaryOp> {
    select! {
        (Token::Op(Op::LessThan), _) => ast::BinaryOp::LessThan,
        (Token::Op(Op::GreaterThan), _) => ast::BinaryOp::GreaterThan,
        (Token::Op(Op::LessThanEq), _) => ast::BinaryOp::LessThanEq,
        (Token::Op(Op::GreaterThanEq), _) => ast::BinaryOp::GreaterThanEq,
    }
    .labelled("comparison op")
    .as_context()
    .boxed()
}

fn eq_op<'src>() -> BoxedParser<'src, ast::BinaryOp> {
    select! {
        (Token::Op(Op::Eq), _) => ast::BinaryOp::Eq,
        (Token::Op(Op::NotEq), _) => ast::BinaryOp::NotEq,
    }
    .labelled("equality op")
    .as_context()
    .boxed()
}

fn and_op<'src>() -> BoxedParser<'src, ast::BinaryOp> {
    select! {
        (Token::Op(Op::And), _) => ast::BinaryOp::And,
    }
    .labelled("logical and op")
    .as_context()
    .boxed()
}

fn coalesce_op<'src>() -> BoxedParser<'src, ast::BinaryOp> {
    select! {
        (Token::Op(Op::Coalesce), _) => ast::BinaryOp::Coalesce,
    }
    .labelled("coalesce op")
    .as_context()
    .boxed()
}

fn or_op<'src>() -> BoxedParser<'src, ast::BinaryOp> {
    select! {
        (Token::Op(Op::Or), _) => ast::BinaryOp::Or,
    }
    .labelled("logical or op")
    .as_context()
    .boxed()
}

fn assign_op<'src>() -> BoxedParser<'src, ast::AssignOp> {
    select! {
        (Token::Op(Op::Assign), _) => ast::AssignOp::Assign,
        (Token::Op(Op::AddAssign), _) => ast::AssignOp::AddAssign,
        (Token::Op(Op::SubAssign), _) => ast::AssignOp::SubAssign,
        (Token::Op(Op::MulAssign), _) => ast::AssignOp::MulAssign,
        (Token::Op(Op::DivAssign), _) => ast::AssignOp::DivAssign,
    }
    .labelled("assign op")
    .as_context()
    .boxed()
}

fn lvalue_expr<'src>() -> BoxedParser<'src, ast::ExprNode> {
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
    .boxed()
}

fn assignment_expr<'src>(
    expr: impl AnvParser<'src, ast::ExprNode>,
) -> BoxedParser<'src, ast::ExprNode> {
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
        .boxed()
}

fn return_stmt<'src>(
    stmt: impl AnvParser<'src, ast::StmtNode>,
) -> BoxedParser<'src, ast::StmtNode> {
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
    .boxed()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer;
    use chumsky::Parser;

    fn parse_expr(src: &str) -> ast::ExprNode {
        let tokens = lexer::tokenize(src)
            .unwrap_or_else(|errs| panic!("failed to tokenize '{src}': {errs:?}"));
        let stmt_parser = statement();
        let expr_parser = expression(stmt_parser.clone()).then_ignore(end());
        let mut state = SimpleState(ParserState::default());
        expr_parser
            .parse_with_state(&tokens, &mut state)
            .into_result()
            .unwrap_or_else(|errs| panic!("failed to parse '{src}': {errs:?}"))
    }

    fn expect_binary<'a>(
        expr: &'a ast::ExprNode,
        op: ast::BinaryOp,
    ) -> (&'a ast::ExprNode, &'a ast::ExprNode) {
        match &expr.node().kind {
            ast::ExprKind::Binary(bin_node) => {
                let binary = bin_node.node();
                assert_eq!(
                    binary.op, op,
                    "expected binary op {:?}, found {:?}",
                    op, binary.op
                );
                (binary.left.as_ref(), binary.right.as_ref())
            }
            other => panic!("expected binary op {op:?}, found {other:?}"),
        }
    }

    fn expect_range<'a>(
        expr: &'a ast::ExprNode,
        inclusive: bool,
    ) -> (&'a ast::ExprNode, &'a ast::ExprNode) {
        match &expr.node().kind {
            ast::ExprKind::Range(range_node) => {
                let range = range_node.node();
                assert_eq!(
                    range.inclusive, inclusive,
                    "expected inclusive={inclusive}, found {}",
                    range.inclusive
                );
                (&range.start, &range.end)
            }
            other => panic!("expected range expr, found {other:?}"),
        }
    }

    fn expect_ident(expr: &ast::ExprNode, name: &str) {
        match &expr.node().kind {
            ast::ExprKind::Ident(ident) => {
                assert_eq!(ident.0.as_ref(), name, "expected ident '{name}'");
            }
            other => panic!("expected ident '{name}', found {other:?}"),
        }
    }

    fn expect_int(expr: &ast::ExprNode, value: i64) {
        match &expr.node().kind {
            ast::ExprKind::Lit(ast::Lit::Int(v)) => {
                assert_eq!(v, &value, "expected int literal {value}");
            }
            other => panic!("expected int literal {value}, found {other:?}"),
        }
    }

    #[test]
    fn multiplication_outbinds_addition() {
        let expr = parse_expr("1 + 2 * 3");
        let (left, right) = expect_binary(&expr, ast::BinaryOp::Add);
        expect_int(left, 1);
        let (mul_left, mul_right) = expect_binary(right, ast::BinaryOp::Mul);
        expect_int(mul_left, 2);
        expect_int(mul_right, 3);
    }

    #[test]
    fn subtraction_is_left_associative() {
        let expr = parse_expr("a - b - c");
        let (left, right) = expect_binary(&expr, ast::BinaryOp::Sub);
        let (first_left, first_right) = expect_binary(left, ast::BinaryOp::Sub);
        expect_ident(first_left, "a");
        expect_ident(first_right, "b");
        expect_ident(right, "c");
    }

    #[test]
    fn comparison_is_looser_than_arithmetic() {
        let expr = parse_expr("a + b < c * d");
        let (left, right) = expect_binary(&expr, ast::BinaryOp::LessThan);
        let (add_left, add_right) = expect_binary(left, ast::BinaryOp::Add);
        expect_ident(add_left, "a");
        expect_ident(add_right, "b");
        let (mul_left, mul_right) = expect_binary(right, ast::BinaryOp::Mul);
        expect_ident(mul_left, "c");
        expect_ident(mul_right, "d");
    }

    #[test]
    fn equality_is_looser_than_multiplication_and_left_assoc() {
        let expr = parse_expr("a * b == c + d");
        let (left, right) = expect_binary(&expr, ast::BinaryOp::Eq);
        let (mul_left, mul_right) = expect_binary(left, ast::BinaryOp::Mul);
        expect_ident(mul_left, "a");
        expect_ident(mul_right, "b");
        let (add_left, add_right) = expect_binary(right, ast::BinaryOp::Add);
        expect_ident(add_left, "c");
        expect_ident(add_right, "d");

        let chain = parse_expr("x == y == z");
        let (first, tail) = expect_binary(&chain, ast::BinaryOp::Eq);
        let (lhs, rhs) = expect_binary(first, ast::BinaryOp::Eq);
        expect_ident(lhs, "x");
        expect_ident(rhs, "y");
        expect_ident(tail, "z");
    }

    #[test]
    fn logical_and_has_higher_precedence_than_or() {
        let expr = parse_expr("a && b || c");
        let (left, right) = expect_binary(&expr, ast::BinaryOp::Or);
        let (and_left, and_right) = expect_binary(left, ast::BinaryOp::And);
        expect_ident(and_left, "a");
        expect_ident(and_right, "b");
        expect_ident(right, "c");
    }

    #[test]
    fn coalesce_sits_between_and_and_or() {
        let expr = parse_expr("a ?? b || c");
        let (left, right) = expect_binary(&expr, ast::BinaryOp::Or);
        let (coal_left, coal_right) = expect_binary(left, ast::BinaryOp::Coalesce);
        expect_ident(coal_left, "a");
        expect_ident(coal_right, "b");
        expect_ident(right, "c");

        let expr = parse_expr("a && b ?? c");
        let (coal_left, coal_right) = expect_binary(&expr, ast::BinaryOp::Coalesce);
        let (and_left, and_right) = expect_binary(coal_left, ast::BinaryOp::And);
        expect_ident(and_left, "a");
        expect_ident(and_right, "b");
        expect_ident(coal_right, "c");
    }

    #[test]
    fn coalesce_vs_and_and_chain() {
        let expr = parse_expr("a ?? b && c");
        let (left, right) = expect_binary(&expr, ast::BinaryOp::Coalesce);
        expect_ident(left, "a");
        let (and_left, and_right) = expect_binary(right, ast::BinaryOp::And);
        expect_ident(and_left, "b");
        expect_ident(and_right, "c");

        let expr = parse_expr("a ?? b ?? c");
        let (first, tail) = expect_binary(&expr, ast::BinaryOp::Coalesce);
        let (left_left, left_right) = expect_binary(first, ast::BinaryOp::Coalesce);
        expect_ident(left_left, "a");
        expect_ident(left_right, "b");
        expect_ident(tail, "c");
    }

    #[test]
    fn coalesce_interacts_with_equality() {
        let expr = parse_expr("x == y ?? z");
        let (left, right) = expect_binary(&expr, ast::BinaryOp::Coalesce);
        let (eq_left, eq_right) = expect_binary(left, ast::BinaryOp::Eq);
        expect_ident(eq_left, "x");
        expect_ident(eq_right, "y");
        expect_ident(right, "z");

        let expr = parse_expr("x ?? y == z");
        let (left, right) = expect_binary(&expr, ast::BinaryOp::Coalesce);
        expect_ident(left, "x");
        let (eq_left, eq_right) = expect_binary(right, ast::BinaryOp::Eq);
        expect_ident(eq_left, "y");
        expect_ident(eq_right, "z");
    }

    #[test]
    fn complex_expression_respects_all_levels() {
        let expr = parse_expr("a + b ?? c * d && e || f");
        let (left, right) = expect_binary(&expr, ast::BinaryOp::Or);
        expect_ident(right, "f");

        let (coal_left, coal_right) = expect_binary(left, ast::BinaryOp::Coalesce);
        let (add_left, add_right) = expect_binary(coal_left, ast::BinaryOp::Add);
        expect_ident(add_left, "a");
        expect_ident(add_right, "b");

        let (and_left, and_right) = expect_binary(coal_right, ast::BinaryOp::And);
        let (mul_left, mul_right) = expect_binary(and_left, ast::BinaryOp::Mul);
        expect_ident(mul_left, "c");
        expect_ident(mul_right, "d");
        expect_ident(and_right, "e");
    }

    #[test]
    fn range_has_lower_precedence_than_addition() {
        let expr = parse_expr("1 + 2 .. 3");
        let (start, end) = expect_range(&expr, false);
        let (add_left, add_right) = expect_binary(start, ast::BinaryOp::Add);
        expect_int(add_left, 1);
        expect_int(add_right, 2);
        expect_int(end, 3);
    }

    #[test]
    fn range_has_higher_precedence_than_comparison() {
        let expr = parse_expr("a..b < c");
        let (left, right) = expect_binary(&expr, ast::BinaryOp::LessThan);
        let (start, end) = expect_range(left, false);
        expect_ident(start, "a");
        expect_ident(end, "b");
        expect_ident(right, "c");
    }

    #[test]
    fn inclusive_range_parses() {
        let expr = parse_expr("0..=10");
        let (start, end) = expect_range(&expr, true);
        expect_int(start, 0);
        expect_int(end, 10);
    }

    #[test]
    fn range_in_complex_expression_respects_all_levels() {
        let expr = parse_expr("a + b .. c < d && e ?? f || g");
        let (left, right) = expect_binary(&expr, ast::BinaryOp::Or);
        expect_ident(right, "g");

        let (coal_left, coal_right) = expect_binary(left, ast::BinaryOp::Coalesce);
        expect_ident(coal_right, "f");

        let (and_left, and_right) = expect_binary(coal_left, ast::BinaryOp::And);
        expect_ident(and_right, "e");

        let (cmp_left, cmp_right) = expect_binary(and_left, ast::BinaryOp::LessThan);
        expect_ident(cmp_right, "d");

        let (range_start, range_end) = expect_range(cmp_left, false);
        let (add_left, add_right) = expect_binary(range_start, ast::BinaryOp::Add);
        expect_ident(add_left, "a");
        expect_ident(add_right, "b");
        expect_ident(range_end, "c");
    }

    fn parse_program(src: &str) -> ast::Program {
        let tokens = lexer::tokenize(src)
            .unwrap_or_else(|errs| panic!("failed to tokenize '{src}': {errs:?}"));
        let mut state = SimpleState(ParserState::default());
        parser()
            .parse_with_state(&tokens, &mut state)
            .into_result()
            .unwrap_or_else(|errs| panic!("failed to parse '{src}': {errs:?}"))
    }

    #[test]
    fn while_with_binary_cond_parses() {
        let prog = parse_program("fn main() { while x < 3 {} }");
        assert_eq!(prog.stmts.len(), 1);
        let ast::Stmt::Func(func_node) = &prog.stmts[0].node else {
            panic!("expected Func");
        };
        let body_stmts = &func_node.node.body.node.stmts;
        assert_eq!(body_stmts.len(), 1);
        let ast::Stmt::While(while_node) = &body_stmts[0].node else {
            panic!("expected While");
        };
        let cond = &while_node.node.cond;
        match &cond.node.kind {
            ast::ExprKind::Binary(bin) => {
                assert_eq!(bin.node.op, ast::BinaryOp::LessThan);
            }
            other => panic!("expected Binary cond, found {other:?}"),
        }
    }

    #[test]
    fn while_with_ident_cond_parses() {
        let prog = parse_program("fn main() { while x {} }");
        assert_eq!(prog.stmts.len(), 1);
        let ast::Stmt::Func(func_node) = &prog.stmts[0].node else {
            panic!("expected Func");
        };
        let body_stmts = &func_node.node.body.node.stmts;
        assert_eq!(body_stmts.len(), 1);
        let ast::Stmt::While(while_node) = &body_stmts[0].node else {
            panic!("expected While");
        };
        let cond = &while_node.node.cond;
        match &cond.node.kind {
            ast::ExprKind::Ident(ident) => {
                assert_eq!(ident.0.as_ref(), "x");
            }
            other => panic!("expected Ident cond, found {other:?}"),
        }
    }

    #[test]
    fn if_with_ident_cond_parses() {
        let prog = parse_program("fn main() { if x {} }");
        assert_eq!(prog.stmts.len(), 1);
        let ast::Stmt::Func(func_node) = &prog.stmts[0].node else {
            panic!("expected Func");
        };
        let body_stmts = &func_node.node.body.node.stmts;
        assert_eq!(body_stmts.len(), 1);
        let ast::Stmt::Expr(expr_node) = &body_stmts[0].node else {
            panic!("expected Expr stmt");
        };
        let ast::ExprKind::If(if_node) = &expr_node.node.kind else {
            panic!("expected If expr");
        };
        let cond = &if_node.node.cond;
        match &cond.node.kind {
            ast::ExprKind::Ident(ident) => {
                assert_eq!(ident.0.as_ref(), "x");
            }
            other => panic!("expected Ident cond, found {other:?}"),
        }
    }

    #[test]
    fn while_with_inner_break_and_assign_parses() {
        let src = r#"
            fn main() {
                var i: int = 0;
                while true {
                    if i == 3 {
                        break;
                    }
                    i = i + 1;
                }
            }
        "#;
        let prog = parse_program(src);
        assert_eq!(prog.stmts.len(), 1);
        let ast::Stmt::Func(func_node) = &prog.stmts[0].node else {
            panic!("expected Func");
        };
        let body_stmts = &func_node.node.body.node.stmts;
        assert_eq!(body_stmts.len(), 2);

        let ast::Stmt::Binding(_) = &body_stmts[0].node else {
            panic!("expected Binding stmt");
        };

        let ast::Stmt::While(while_node) = &body_stmts[1].node else {
            panic!("expected While stmt");
        };
        let while_body = &while_node.node.body.node.stmts;
        assert_eq!(while_body.len(), 2);

        let ast::Stmt::Expr(if_expr_node) = &while_body[0].node else {
            panic!("expected Expr stmt for if");
        };
        assert!(matches!(&if_expr_node.node.kind, ast::ExprKind::If(_)));

        let ast::Stmt::Expr(assign_expr_node) = &while_body[1].node else {
            panic!("expected Expr stmt for assignment");
        };
        assert!(matches!(
            &assign_expr_node.node.kind,
            ast::ExprKind::Assign(_)
        ));
    }

    #[test]
    fn for_parses_basic_range() {
        let prog = parse_program("fn main() { for n in 0..10 {} }");
        assert_eq!(prog.stmts.len(), 1);
        let ast::Stmt::Func(func_node) = &prog.stmts[0].node else {
            panic!("expected Func");
        };
        let body_stmts = &func_node.node.body.node.stmts;
        assert_eq!(body_stmts.len(), 1);

        let ast::Stmt::For(for_node) = &body_stmts[0].node else {
            panic!("expected For stmt");
        };
        let for_inner = &for_node.node;

        let ast::Pattern::Ident(ident) = &for_inner.pattern.node else {
            panic!("expected Ident pattern");
        };
        assert_eq!(ident.0.as_ref(), "n");

        assert!(!for_inner.reversed);
        assert!(for_inner.step.is_none());

        let ast::ExprKind::Range(range_node) = &for_inner.iterable.node.kind else {
            panic!("expected Range iterable");
        };
        assert!(!range_node.node.inclusive);
    }

    #[test]
    fn for_parses_rev_and_step() {
        let prog = parse_program("fn main() { for n in rev 0..10 step 2 {} }");
        assert_eq!(prog.stmts.len(), 1);
        let ast::Stmt::Func(func_node) = &prog.stmts[0].node else {
            panic!("expected Func");
        };
        let body_stmts = &func_node.node.body.node.stmts;
        assert_eq!(body_stmts.len(), 1);

        let ast::Stmt::For(for_node) = &body_stmts[0].node else {
            panic!("expected For stmt");
        };
        let for_inner = &for_node.node;

        assert!(for_inner.reversed);
        assert!(for_inner.step.is_some());

        let step_expr = for_inner.step.as_ref().unwrap();
        let ast::ExprKind::Lit(ast::Lit::Int(2)) = &step_expr.node.kind else {
            panic!("expected Int(2) step");
        };
    }

    #[test]
    fn for_parses_inclusive_range() {
        let prog = parse_program("fn main() { for n in 0..=10 {} }");
        assert_eq!(prog.stmts.len(), 1);
        let ast::Stmt::Func(func_node) = &prog.stmts[0].node else {
            panic!("expected Func");
        };
        let body_stmts = &func_node.node.body.node.stmts;
        assert_eq!(body_stmts.len(), 1);

        let ast::Stmt::For(for_node) = &body_stmts[0].node else {
            panic!("expected For stmt");
        };
        let for_inner = &for_node.node;

        let ast::ExprKind::Range(range_node) = &for_inner.iterable.node.kind else {
            panic!("expected Range iterable");
        };
        assert!(range_node.node.inclusive);
    }

    fn parse_type(src: &str) -> ast::Type {
        let tokens = lexer::tokenize(src)
            .unwrap_or_else(|errs| panic!("failed to tokenize type '{src}': {errs:?}"));
        let mut state = SimpleState(ParserState::default());
        type_ident()
            .then_ignore(end())
            .parse_with_state(&tokens, &mut state)
            .into_result()
            .unwrap_or_else(|errs| panic!("failed to parse type '{src}': {errs:?}"))
    }

    #[test]
    fn array_type_fixed_len_parses() {
        let ty = parse_type("[int; 3]");
        match ty {
            ast::Type::Array { elem, len } => {
                assert_eq!(*elem, ast::Type::Int);
                assert_eq!(len, ast::ArrayLen::Fixed(3));
            }
            other => panic!("expected array type, found {other:?}"),
        }
    }

    #[test]
    fn list_type_parses() {
        let ty = parse_type("[string]");
        match ty {
            ast::Type::List { elem } => {
                assert_eq!(*elem, ast::Type::String);
            }
            other => panic!("expected list type, found {other:?}"),
        }
    }

    #[test]
    fn array_type_infer_len_parses() {
        let ty = parse_type("[float; _]");
        match ty {
            ast::Type::Array { elem, len } => {
                assert_eq!(*elem, ast::Type::Float);
                assert_eq!(len, ast::ArrayLen::Infer);
            }
            other => panic!("expected infer-length array type, found {other:?}"),
        }
    }

    #[test]
    fn array_type_can_be_optional() {
        let ty = parse_type("[int; 3]?");
        match ty {
            ast::Type::Optional(inner) => match *inner {
                ast::Type::Array { elem, len } => {
                    assert_eq!(*elem, ast::Type::Int);
                    assert_eq!(len, ast::ArrayLen::Fixed(3));
                }
                other => panic!("expected inner array type, found {other:?}"),
            },
            other => panic!("expected optional array type, found {other:?}"),
        }
    }

    #[test]
    fn array_type_with_struct_elem_parses() {
        let ty = parse_type("[MyStruct; 5]");
        match ty {
            ast::Type::Array { elem, len } => {
                assert_eq!(len, ast::ArrayLen::Fixed(5));
                match *elem {
                    ast::Type::UnresolvedName(name) => {
                        assert_eq!(name.0.as_ref(), "MyStruct");
                    }
                    other => panic!("expected unresolved name, found {other:?}"),
                }
            }
            other => panic!("expected array type, found {other:?}"),
        }
    }

    #[test]
    fn map_type_parses() {
        let ty = parse_type("[string: int]");
        match ty {
            ast::Type::Map { key, value } => {
                assert_eq!(*key, ast::Type::String);
                assert_eq!(*value, ast::Type::Int);
            }
            other => panic!("expected map type, found {other:?}"),
        }
    }

    #[test]
    fn nested_array_type_parses() {
        let ty = parse_type("[[int; 3]; 2]");
        match ty {
            ast::Type::Array { elem, len } => {
                assert_eq!(len, ast::ArrayLen::Fixed(2));
                match *elem {
                    ast::Type::Array {
                        elem: inner_elem,
                        len: inner_len,
                    } => {
                        assert_eq!(*inner_elem, ast::Type::Int);
                        assert_eq!(inner_len, ast::ArrayLen::Fixed(3));
                    }
                    other => panic!("expected inner array type, found {other:?}"),
                }
            }
            other => panic!("expected nested array type, found {other:?}"),
        }
    }

    fn expect_array_literal(expr: &ast::ExprNode) -> &[ast::ExprNode] {
        match &expr.node.kind {
            ast::ExprKind::ArrayLiteral(lit) => &lit.node.elements,
            other => panic!("expected ArrayLiteral, found {other:?}"),
        }
    }

    fn expect_array_fill(expr: &ast::ExprNode) -> (&ast::ExprNode, &ast::ExprNode) {
        match &expr.node.kind {
            ast::ExprKind::ArrayFill(fill) => (&fill.node.value, &fill.node.len),
            other => panic!("expected ArrayFill, found {other:?}"),
        }
    }

    #[test]
    fn array_literal_basic_parses() {
        let expr = parse_expr("[1, 2, 3]");
        let elements = expect_array_literal(&expr);
        assert_eq!(elements.len(), 3);
        expect_int(&elements[0], 1);
        expect_int(&elements[1], 2);
        expect_int(&elements[2], 3);
    }

    #[test]
    fn array_literal_trailing_comma_parses() {
        let expr = parse_expr("[1, 2, 3,]");
        let elements = expect_array_literal(&expr);
        assert_eq!(elements.len(), 3);
        expect_int(&elements[0], 1);
        expect_int(&elements[1], 2);
        expect_int(&elements[2], 3);
    }

    #[test]
    fn array_literal_empty_parses() {
        let expr = parse_expr("[]");
        let elements = expect_array_literal(&expr);
        assert_eq!(elements.len(), 0);
    }

    #[test]
    fn array_literal_nested_parses() {
        let expr = parse_expr("[[1, 2], [3, 4]]");
        let outer_elements = expect_array_literal(&expr);
        assert_eq!(outer_elements.len(), 2);

        let inner1 = expect_array_literal(&outer_elements[0]);
        assert_eq!(inner1.len(), 2);
        expect_int(&inner1[0], 1);
        expect_int(&inner1[1], 2);

        let inner2 = expect_array_literal(&outer_elements[1]);
        assert_eq!(inner2.len(), 2);
        expect_int(&inner2[0], 3);
        expect_int(&inner2[1], 4);
    }

    #[test]
    fn array_fill_literal_basic_parses() {
        let expr = parse_expr("[0; 3]");
        let (value, len) = expect_array_fill(&expr);
        expect_int(value, 0);
        expect_int(len, 3);
    }

    #[test]
    fn array_fill_literal_with_expr_len_parses() {
        let expr = parse_expr("[x + 1; n]");
        let (value, len) = expect_array_fill(&expr);

        let (left, right) = expect_binary(value, ast::BinaryOp::Add);
        expect_ident(left, "x");
        expect_int(right, 1);

        expect_ident(len, "n");
    }

    #[test]
    fn type_nested_optional_parses() {
        let ty = parse_type("(int?)?");
        match ty {
            ast::Type::Optional(inner) => match *inner {
                ast::Type::Optional(inner2) => {
                    assert_eq!(*inner2, ast::Type::Int);
                }
                other => panic!("expected Optional(Int), found {other:?}"),
            },
            other => panic!("expected Optional(Optional(Int)), found {other:?}"),
        }
    }

    #[test]
    fn type_optional_array_infer_parses() {
        let ty = parse_type("[int?; _]");
        match ty {
            ast::Type::Array { elem, len } => {
                assert_eq!(len, ast::ArrayLen::Infer);
                match *elem {
                    ast::Type::Optional(inner) => {
                        assert_eq!(*inner, ast::Type::Int);
                    }
                    other => panic!("expected Optional(Int), found {other:?}"),
                }
            }
            other => panic!("expected Array(Optional(Int), Infer), found {other:?}"),
        }
    }

    #[test]
    fn type_optional_list_parses() {
        let ty = parse_type("[int?]");
        match ty {
            ast::Type::List { elem } => match *elem {
                ast::Type::Optional(inner) => {
                    assert_eq!(*inner, ast::Type::Int);
                }
                other => panic!("expected Optional(Int), found {other:?}"),
            },
            other => panic!("expected List(Optional(Int)), found {other:?}"),
        }
    }

    #[test]
    fn type_list_optional_parses() {
        let ty = parse_type("[int]?");
        match ty {
            ast::Type::Optional(inner) => match *inner {
                ast::Type::List { elem } => {
                    assert_eq!(*elem, ast::Type::Int);
                }
                other => panic!("expected List(Int), found {other:?}"),
            },
            other => panic!("expected Optional(List(Int)), found {other:?}"),
        }
    }

    #[test]
    fn type_optional_array_fixed_parses() {
        let ty = parse_type("[int?; 3]");
        match ty {
            ast::Type::Array { elem, len } => {
                assert_eq!(len, ast::ArrayLen::Fixed(3));
                match *elem {
                    ast::Type::Optional(inner) => {
                        assert_eq!(*inner, ast::Type::Int);
                    }
                    other => panic!("expected Optional(Int), found {other:?}"),
                }
            }
            other => panic!("expected Array(Optional(Int), Fixed(3)), found {other:?}"),
        }
    }

    #[test]
    fn view_type_int_parses() {
        let ty = parse_type("[int; ..]");
        match ty {
            ast::Type::ArrayView { elem } => {
                assert_eq!(*elem, ast::Type::Int);
            }
            other => panic!("expected View(Int), found {other:?}"),
        }
    }

    #[test]
    fn view_type_float_parses() {
        let ty = parse_type("[float; ..]");
        match ty {
            ast::Type::ArrayView { elem } => {
                assert_eq!(*elem, ast::Type::Float);
            }
            other => panic!("expected View(Float), found {other:?}"),
        }
    }

    #[test]
    fn view_type_array_parses() {
        let ty = parse_type("[[int; 3]; ..]");
        match ty {
            ast::Type::ArrayView { elem } => match *elem {
                ast::Type::Array { elem: inner, len } => {
                    assert_eq!(*inner, ast::Type::Int);
                    assert_eq!(len, ast::ArrayLen::Fixed(3));
                }
                other => panic!("expected Array(Int, Fixed(3)), found {other:?}"),
            },
            other => panic!("expected View(Array(...)), found {other:?}"),
        }
    }

    #[test]
    fn view_type_list_parses() {
        let ty = parse_type("[[string]; ..]");
        match ty {
            ast::Type::ArrayView { elem } => match *elem {
                ast::Type::List { elem: inner } => {
                    assert_eq!(*inner, ast::Type::String);
                }
                other => panic!("expected List(String), found {other:?}"),
            },
            other => panic!("expected View(List(...)), found {other:?}"),
        }
    }

    #[test]
    fn view_type_optional_parses() {
        let ty = parse_type("[int; ..]?");
        match ty {
            ast::Type::Optional(inner) => match *inner {
                ast::Type::ArrayView { elem } => {
                    assert_eq!(*elem, ast::Type::Int);
                }
                other => panic!("expected View(Int), found {other:?}"),
            },
            other => panic!("expected Optional(View(Int)), found {other:?}"),
        }
    }
}
