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
    function(stmt)
        .map(|func_node| {
            let span = func_node.span;
            Spanned::new(ast::Stmt::Func(func_node), span)
        })
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
            expr.map(|expr_node| {
                let span = expr_node.span;
                Spanned::new(ast::Stmt::Expr(expr_node), span)
            }),
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
    .try_map_with(|((((name, type_params), params), ret), body), e| {
        let s = e.span();
        // build type parameter map
        let type_param_map = type_params
            .iter()
            .map(|tp| (tp.name, tp.id))
            .collect::<HashMap<_, _>>();

        // resolve type parameters variables in signature
        let resolved_params = params
            .into_iter()
            .map(|p| {
                resolve_type_params(&p.ty, &type_param_map)
                    .map(|ty| ast::Param { name: p.name, ty })
            })
            .collect::<Result<Vec<_>, _>>()
            .map_err(|ident| Rich::custom(s.clone(), format!("unknown type '{}'", ident)))?;

        // resolve type parameters in return type
        let resolved_ret = match ret {
            Some(ty) => resolve_type_params(&ty, &type_param_map)
                .map_err(|ident| Rich::custom(s.clone(), format!("unknown type '{}'", ident)))?,
            None => ast::Type::Void,
        };

        Ok(Spanned::new(
            ast::Func {
                name,
                visibility: ast::Visibility::Private,
                type_params,
                params: resolved_params,
                ret: resolved_ret,
                body,
            },
            Span::new(s.start, s.end),
        ))
    })
    .labelled("function")
    .as_context()
}

fn resolve_type_params(
    ty: &ast::Type,
    type_param_map: &HashMap<ast::Ident, ast::TypeVarId>,
) -> Result<ast::Type, ast::Ident> {
    use ast::Type::*;
    match ty {
        // resolve T to Var(id)
        UnresolvedName(ident) => type_param_map
            .get(ident)
            .map(|id| Var(*id))
            .ok_or_else(|| ident.clone()),

        Optional(inner) => Ok(Optional(
            resolve_type_params(inner, type_param_map)?.boxed(),
        )),

        Func { params, ret } => {
            let resolved_params: Result<Vec<_>, _> = params
                .iter()
                .map(|p| resolve_type_params(p, type_param_map))
                .collect();
            Ok(Func {
                params: resolved_params?,
                ret: resolve_type_params(ret, type_param_map)?.boxed(),
            })
        }

        Tuple(elements) => {
            let resolved_elements: Result<Vec<_>, _> = elements
                .iter()
                .map(|el| resolve_type_params(el, type_param_map))
                .collect();
            Ok(Tuple(resolved_elements?))
        }

        // other types pass through unchanged
        _ => Ok(ty.clone()),
    }
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

fn validate_tuple_shape<T>(
    first: Option<T>,
    mut rest: Vec<T>,
    trailing_comma: bool,
    span: chumsky::span::SimpleSpan,
    emitter: &mut chumsky::input::Emitter<Rich<'_, SpannedToken>>,
    make_tuple: impl FnOnce(Vec<T>) -> T,
    make_error_dummy: impl FnOnce() -> T,
) -> T {
    match (first, rest.len(), trailing_comma) {
        // empty ()
        (None, 0, _) => {
            emitter.emit(Rich::custom(span, "empty tuples are not supported"));
            make_error_dummy()
        }
        // 1-tuple (T,)
        (Some(single), 0, true) => {
            emitter.emit(Rich::custom(span, "1-tuples are not supported"));
            single
        }
        // grouped (T) = T
        (Some(single), 0, false) => single,
        // tuple (T, U, ...)
        (Some(first), _, _) => {
            rest.insert(0, first);
            make_tuple(rest)
        }
        // unexpected comma
        (None, _, _) => {
            emitter.emit(Rich::custom(span, "unexpected comma"));
            make_error_dummy()
        }
    }
}

fn grouped_or_tuple_expr<'src>(
    expr: impl AnvParser<'src, ast::ExprNode>,
) -> impl AnvParser<'src, ast::ExprNode> {
    let comma = select! { (Token::Comma, _) => () };
    let open_paren = select! { (Token::Open(Delimiter::Parent), _) => () };
    let close_paren = select! { (Token::Close(Delimiter::Parent), _) => () };

    let first_expr = expr.clone();
    let rest_exprs = comma.ignore_then(expr).repeated().collect::<Vec<_>>();

    open_paren
        .ignore_then(first_expr.or_not())
        .then(rest_exprs)
        .then(comma.or_not())
        .then_ignore(close_paren)
        .validate(|((first, rest), trailing_comma), e, emitter| {
            let s = e.span();
            let span = Span::new(s.start, s.end);
            let expr_id = e.state().new_expr_id();

            validate_tuple_shape(
                first,
                rest,
                trailing_comma.is_some(),
                s,
                emitter,
                |exprs| {
                    let tuple_expr = ast::Expr::new(ast::ExprKind::Tuple(exprs), expr_id);
                    Spanned::new(tuple_expr, span)
                },
                || {
                    let dummy = ast::Expr::new(ast::ExprKind::Lit(ast::Lit::Nil), expr_id);
                    Spanned::new(dummy, span)
                },
            )
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

fn call_expr<'src>(
    atom: impl AnvParser<'src, ast::ExprNode>,
    expr: impl AnvParser<'src, ast::ExprNode>,
) -> impl AnvParser<'src, ast::ExprNode> {
    let type_args = call_type_args();
    let args = fn_call_args(expr);
    // parse my_fn<T, U>(args) or my_fn(args)
    let call_suffix = type_args.then(args);
    atom.foldl_with(call_suffix.repeated(), |callee, (type_args, args), e| {
        let start = callee.span.start;
        let end = args.last().map(|a| a.span.end).unwrap_or(callee.span.end);
        let span = Span::new(start, end);

        let call_node = Spanned::new(
            ast::Call {
                func: Box::new(callee),
                args,
                type_args,
            },
            span,
        );

        let expr_id = e.state().new_expr_id();
        let expr = ast::Expr::new(ast::ExprKind::Call(call_node), expr_id);
        Spanned::new(expr, span)
    })
    .labelled("call expr")
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
        let call = call_expr(atom, expr.clone());
        let indexed = tuple_index_expr(call);
        let unary = unary_expr(indexed);
        let binary = binary_expr(unary);
        let ternary = ternary_expr(binary, expr.clone());
        let assign = assignment_expr(ternary);
        assign
    })
}

fn tuple_index_expr<'src>(
    base: impl AnvParser<'src, ast::ExprNode>,
) -> impl AnvParser<'src, ast::ExprNode> {
    let single_index = select! {
        (Token::Dot, _) => (),
    }
    .ignore_then(select! {
        (Token::Literal(LitToken::Number(n)), _) => vec![n as u32],
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
        indices.map_err(|_| Rich::custom(span, "invalid tuple index"))
    });

    let index_suffix = choice((chained_index, single_index));

    base.foldl_with(index_suffix.repeated(), |target, indices, e| {
        let s = e.span();
        let span = Span::new(s.start, s.end);

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
    })
    .labelled("tuple index")
    .as_context()
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

        // parse type name as unresolved names (like T, U, ...)
        let type_name_ref = identifier().map(ast::Type::UnresolvedName);

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

fn paren_or_tuple_type<'src>(
    type_parser: impl AnvParser<'src, ast::Type>,
) -> impl AnvParser<'src, ast::Type> {
    let comma = select! { (Token::Comma, _) => () };
    let open_paren = select! { (Token::Open(Delimiter::Parent), _) => () };
    let close_paren = select! { (Token::Close(Delimiter::Parent), _) => () };

    let first_type = type_parser.clone();
    let rest_types = comma
        .ignore_then(type_parser)
        .repeated()
        .collect::<Vec<_>>();

    open_paren
        .ignore_then(first_type.or_not())
        .then(rest_types)
        .then(comma.or_not())
        .then_ignore(close_paren)
        .validate(|((first, rest), trailing_comma), e, emitter| {
            let s = e.span();

            validate_tuple_shape(
                first,
                rest,
                trailing_comma.is_some(),
                s,
                emitter,
                ast::Type::Tuple,
                || ast::Type::Void,
            )
        })
        .labelled("tuple or grouped type")
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
        .then(identifier())
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
        .map_with(|(((mutability, name), ty), value), e| {
            let s = e.span();
            Spanned::new(
                ast::Binding {
                    name,
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
    identifier()
        .map_with(|ident, e| {
            let s = e.span();
            let span = Span::new(s.start, s.end);
            let expr_id = e.state().new_expr_id();
            let expr = ast::Expr::new(ast::ExprKind::Ident(ident), expr_id);
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
        .then_ignore(select! {
            (Token::Semicolon, _) => (),
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
