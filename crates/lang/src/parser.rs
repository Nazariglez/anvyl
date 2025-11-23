use crate::{
    ast,
    lexer::{Delimiter, Keyword, LitToken, Op, SpannedToken, Token},
    span::{Span, Spanned},
};
use chumsky::{error::Rich, extra, prelude::*};

type Input<'src> = &'src [SpannedToken];
type Extra<'src> = extra::Full<Rich<'src, SpannedToken>, (), ()>;

pub fn parse_ast(tokens: &[SpannedToken]) -> Result<ast::Program, Vec<Rich<'_, SpannedToken>>> {
    parser().parse(tokens).into_result()
}

fn parser<'src>() -> impl Parser<'src, Input<'src>, ast::Program, Extra<'src>> + 'src {
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

fn statement<'src>() -> impl Parser<'src, Input<'src>, ast::StmtNode, Extra<'src>> + 'src {
    recursive(|stmt| {
        let expr = expression(stmt.clone());
        let func = function(stmt.clone());
        let bind = binding(stmt.clone());

        choice((
            func.map(|func_node| {
                let span = func_node.span;
                Spanned::new(ast::Stmt::Func(func_node), span)
            }),
            bind.map(|bind_node| {
                let span = bind_node.span;
                Spanned::new(ast::Stmt::Binding(bind_node), span)
            }),
            expr.map(|expr_node| {
                let span = expr_node.span;
                Spanned::new(ast::Stmt::Expr(expr_node), span)
            }),
        ))
        .boxed()
    })
    .labelled("statement")
    .as_context()
}

fn function<'src>(
    stmt: impl Parser<'src, Input<'src>, ast::StmtNode, Extra<'src>>,
) -> impl Parser<'src, Input<'src>, ast::FuncNode, Extra<'src>> {
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
    stmt: impl Parser<'src, Input<'src>, ast::StmtNode, Extra<'src>>,
) -> impl Parser<'src, Input<'src>, ast::BlockNode, Extra<'src>> {
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

fn expression<'src>(
    stmt: impl Parser<'src, Input<'src>, ast::StmtNode, Extra<'src>>,
) -> impl Parser<'src, Input<'src>, ast::ExprNode, Extra<'src>> {
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
    ))
    .labelled("expression")
    .as_context()
}

fn identifier<'src>() -> impl Parser<'src, Input<'src>, ast::Ident, Extra<'src>> {
    select! {
        (Token::Ident(ident), _) => ident,
    }
    .labelled("identifier")
    .as_context()
}

fn literal<'src>() -> impl Parser<'src, Input<'src>, ast::Lit, Extra<'src>> {
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

fn params<'src>() -> impl Parser<'src, Input<'src>, Vec<ast::Param>, Extra<'src>> {
    select! {
        (Token::Open(Delimiter::Parent), _) => (),
    }
    .ignore_then(param().repeated().collect::<Vec<_>>())
    .then_ignore(select! {
        (Token::Close(Delimiter::Parent), _) => (),
    })
}

fn param<'src>() -> impl Parser<'src, Input<'src>, ast::Param, Extra<'src>> {
    identifier()
        .then(type_ident())
        .map(|(name, ty)| ast::Param { name, ty })
        .labelled("parameter")
        .as_context()
}

fn return_type<'src>() -> impl Parser<'src, Input<'src>, Option<ast::Type>, Extra<'src>> {
    select! {
        (Token::Op(Op::ThinArrow), _) => (),
    }
    .ignore_then(type_ident())
    .or_not()
    .labelled("return type")
    .as_context()
}

fn type_ident<'src>() -> impl Parser<'src, Input<'src>, ast::Type, Extra<'src>> {
    select! {
        (Token::Keyword(Keyword::Int), _) => ast::Type::Int,
        (Token::Keyword(Keyword::Float), _) => ast::Type::Float,
        (Token::Keyword(Keyword::Bool), _) => ast::Type::Bool,
        (Token::Keyword(Keyword::String), _) => ast::Type::String,
        (Token::Keyword(Keyword::Void), _) => ast::Type::Void,
    }
    .labelled("type")
    .as_context()
}

fn binding<'src>(
    stmt: impl Parser<'src, Input<'src>, ast::StmtNode, Extra<'src>>,
) -> impl Parser<'src, Input<'src>, ast::BindingNode, Extra<'src>> {
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
