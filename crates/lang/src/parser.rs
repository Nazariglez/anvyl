use crate::{
    ast,
    lexer::{Delimiter, Keyword, LitToken, Op, SpannedToken, Token},
    span::{Span, Spanned},
};
use chumsky::{error::Rich, extra, prelude::*};

type Input<'src> = &'src [SpannedToken];
type Extra<'src> = extra::Full<Rich<'src, SpannedToken>, (), ()>;
trait AnvParser<'src, T>: chumsky::Parser<'src, Input<'src>, T, Extra<'src>> + Clone + 'src {}
impl<'src, T, P> AnvParser<'src, T> for P where
    P: chumsky::Parser<'src, Input<'src>, T, Extra<'src>> + Clone + 'src
{
}

pub fn parse_ast(tokens: &[SpannedToken]) -> Result<ast::Program, Vec<Rich<'_, SpannedToken>>> {
    parser().parse(tokens).into_result()
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

fn function<'src>(
    stmt: impl AnvParser<'src, ast::StmtNode>,
) -> impl AnvParser<'src, ast::FuncNode> {
    select! {
        (Token::Keyword(Keyword::Fn), _) => (),
    }
    .ignore_then(identifier())
    .then(params())
    .then(return_type())
    .then(block_stmt(stmt))
    .map_with(|(((name, params), ret), body), e| {
        let s = e.span();
        Spanned::new(
            ast::Func {
                name,
                visibility: ast::Visibility::Private,
                params,
                ret: ret.unwrap_or(ast::Type::Void),
                body,
            },
            Span::new(s.start, s.end),
        )
    })
    .labelled("function")
    .as_context()
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

fn atom_expr<'src>(
    stmt: impl AnvParser<'src, ast::StmtNode>,
    expr: impl AnvParser<'src, ast::ExprNode>,
) -> impl AnvParser<'src, ast::ExprNode> {
    choice((
        literal().map_with(|lit, e| {
            let s = e.span();
            Spanned::new(ast::Expr::Lit(lit), Span::new(s.start, s.end))
        }),
        identifier().map_with(|id, e| {
            let s = e.span();
            Spanned::new(ast::Expr::Ident(id), Span::new(s.start, s.end))
        }),
        block_stmt(stmt).map(|block_node| {
            let span = block_node.span;
            Spanned::new(ast::Expr::Block(block_node), span)
        }),
        grouped_expr(expr),
    ))
    .labelled("atom")
}

fn grouped_expr<'src>(
    expr: impl AnvParser<'src, ast::ExprNode>,
) -> impl AnvParser<'src, ast::ExprNode> {
    select! {
        (Token::Open(Delimiter::Parent), _) => (),
    }
    .ignore_then(expr.clone())
    .then_ignore(select! {
        (Token::Close(Delimiter::Parent), _) => (),
    })
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

fn call_expr<'src>(
    atom: impl AnvParser<'src, ast::ExprNode>,
    expr: impl AnvParser<'src, ast::ExprNode>,
) -> impl AnvParser<'src, ast::ExprNode> {
    let args = fn_call_args(expr);
    atom.foldl(args.repeated(), |callee, args| {
        let start = callee.span.start;
        let end = args.last().map(|a| a.span.end).unwrap_or(callee.span.end);
        let span = Span::new(start, end);

        let call_node = Spanned::new(
            ast::Call {
                func: Box::new(callee),
                args,
                type_args: vec![], // later: parse `<T, U>` etc.
            },
            span,
        );

        Spanned::new(ast::Expr::Call(call_node), span)
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

        ops.into_iter().rev().fold(expr, |acc, op| {
            let unary_node = Spanned::new(
                ast::Unary {
                    op,
                    expr: Box::new(acc),
                },
                span,
            );

            Spanned::new(ast::Expr::Unary(unary_node), span)
        })
    })
    .labelled("unary")
    .as_context()
}

fn binary_expr<'src>(
    term: impl AnvParser<'src, ast::ExprNode>,
) -> impl AnvParser<'src, ast::ExprNode> {
    // FIXME: precedence (sum, mul, etc...)

    let op_rhs = binary_op().then(term.clone());
    term.foldl(op_rhs.repeated(), |left, (op, right)| {
        let span = Span::new(left.span.start, right.span.end);
        let bin_node = Spanned::new(
            ast::Binary {
                left: Box::new(left),
                op,
                right: Box::new(right),
            },
            span,
        );

        Spanned::new(ast::Expr::Binary(bin_node), span)
    })
    .labelled("expression")
    .as_context()
}

fn expression<'src>(
    stmt: impl AnvParser<'src, ast::StmtNode>,
) -> impl AnvParser<'src, ast::ExprNode> {
    recursive(|expr| {
        let atom = atom_expr(stmt, expr.clone());
        let call = call_expr(atom, expr.clone());
        let unary = unary_expr(call);
        let binary = binary_expr(unary);
        let assign = assignment_expr(binary);
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
    let typ = select! {
        (Token::Keyword(Keyword::Int), _) => ast::Type::Int,
        (Token::Keyword(Keyword::Float), _) => ast::Type::Float,
        (Token::Keyword(Keyword::Bool), _) => ast::Type::Bool,
        (Token::Keyword(Keyword::String), _) => ast::Type::String,
        (Token::Keyword(Keyword::Void), _) => ast::Type::Void,
    };

    typ.then(
        select! {
            (Token::Question, _) => (),
        }
        .or_not(),
    )
    .map(|(ty, q)| {
        if q.is_some() {
            ast::Type::Optional(Box::new(ty))
        } else {
            ty
        }
    })
    .labelled("type")
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

fn assignment_expr<'src>(
    expr: impl AnvParser<'src, ast::ExprNode>,
) -> impl AnvParser<'src, ast::ExprNode> {
    expr.clone()
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

            Spanned::new(ast::Expr::Assign(assign_node), span)
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
        Spanned::new(
            ast::Stmt::Return(Spanned::new(ast::Return { value: value_opt }, span)),
            span,
        )
    })
    .labelled("return")
    .as_context()
}
