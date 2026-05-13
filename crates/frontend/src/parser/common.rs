use chumsky::prelude::*;
use internment::Intern;

use super::{
    AnvParser, BoxedParser,
    expr::expression,
    types::{param_type_ident, type_ident},
};
use crate::{
    ast,
    lexer::{Delimiter, Keyword, LitToken, Op, Token},
    span::Spanned,
};

pub(super) fn identifier<'src>() -> BoxedParser<'src, ast::Ident> {
    select! {
        Token::Ident(ident) => ident,
    }
    .labelled("identifier")
    .as_context()
    .boxed()
}

pub(super) fn keyword_as_ident<'src>() -> BoxedParser<'src, ast::Ident> {
    select! {
        Token::Keyword(kw) => ast::Ident(Intern::new(kw.to_string()))
    }
    .labelled("identifier")
    .boxed()
}

pub(super) fn field_name_ident<'src>() -> BoxedParser<'src, ast::Ident> {
    choice((identifier(), keyword_as_ident())).boxed()
}

pub(super) fn literal<'src>() -> BoxedParser<'src, ast::Lit> {
    select! {
        Token::Literal(lit) => match lit {
            LitToken::Number(n) => ast::Lit::Int(n),
            LitToken::Float(s) => {
                let value = s.as_ref().parse::<f64>().unwrap_or(0.0);
                ast::Lit::Float(value)
            }
            LitToken::String(s) => ast::Lit::String(s.to_string()),
        },
        Token::Keyword(Keyword::True) => ast::Lit::Bool(true),
        Token::Keyword(Keyword::False) => ast::Lit::Bool(false),
        Token::Keyword(Keyword::Nil) => ast::Lit::Nil,
    }
    .labelled("literal")
    .as_context()
    .boxed()
}

pub(super) fn params<'src>(
    stmt: impl AnvParser<'src, ast::StmtNode>,
) -> BoxedParser<'src, Vec<ast::Param>> {
    select! {
        Token::Open(Delimiter::Parent) => (),
    }
    .ignore_then(
        param(stmt)
            .separated_by(select! {
                Token::Comma => (),
            })
            .allow_trailing()
            .collect::<Vec<_>>()
            .or_not()
            .map(Option::unwrap_or_default),
    )
    .then_ignore(select! {
        Token::Close(Delimiter::Parent) => (),
    })
    .boxed()
}

pub(super) fn param<'src>(
    stmt: impl AnvParser<'src, ast::StmtNode>,
) -> BoxedParser<'src, ast::Param> {
    let var_kw = select! {
        Token::Keyword(Keyword::Var) => (),
    }
    .or_not()
    .map(|opt| match opt {
        Some(()) => ast::Mutability::Mutable,
        None => ast::Mutability::Immutable,
    });

    let as_kw = select! {
        Token::Keyword(Keyword::As) => (),
    }
    .or_not()
    .map(|opt| opt.is_some());

    var_kw
        .then(identifier())
        .then_ignore(select! {
            Token::Colon => (),
        })
        .then(as_kw)
        .then(param_type_ident())
        .then(
            select! { Token::Op(Op::Assign) => () }
                .ignore_then(expression(stmt))
                .or_not(),
        )
        .map(
            |((((mutability, name), cast_accept), ty), default)| ast::Param {
                mutability,
                name,
                ty,
                default,
                cast_accept,
            },
        )
        .labelled("parameter")
        .as_context()
        .boxed()
}

pub(super) fn return_spec_tail<'src>() -> BoxedParser<'src, ast::ReturnSpec> {
    let inferred =
        select! { Token::Ident(ident) if ident.0.as_ref() == "_" => ast::Type::InferReturn };
    let access = select! { Token::Keyword(Keyword::Var) => ast::ReturnAccess::Place }
        .or_not()
        .map(|access| access.unwrap_or(ast::ReturnAccess::Value));

    access
        .then(choice((inferred, type_ident())))
        .map(|(access, ty)| ast::ReturnSpec { access, ty })
        .labelled("return type")
        .as_context()
        .boxed()
}

pub(super) fn return_spec<'src>() -> BoxedParser<'src, Option<ast::ReturnSpec>> {
    select! {
        Token::Op(Op::ThinArrow) => (),
    }
    .ignore_then(return_spec_tail())
    .or_not()
    .labelled("return type")
    .as_context()
    .boxed()
}

pub(super) fn block_stmt<'src>(
    stmt: impl AnvParser<'src, ast::StmtNode>,
    tail_expr: impl AnvParser<'src, ast::ExprNode>,
) -> BoxedParser<'src, ast::BlockNode> {
    select! {
        Token::Open(Delimiter::Brace) => (),
    }
    .ignore_then(stmt.repeated().collect::<Vec<_>>())
    .then(tail_expr.or_not())
    .then_ignore(select! {
        Token::Close(Delimiter::Brace) => (),
    })
    .map_with(|(stmts, tail), e| {
        let s = e.span();
        Spanned::new(
            ast::Block {
                stmts,
                tail: tail.map(Box::new),
            },
            s.byte(),
        )
    })
    .labelled("block")
    .as_context()
    .boxed()
}

// Shared helper for validating tuple shapes (used by expr and types)
pub(super) enum TupleShapeResult<T> {
    Empty,
    OneTupleError(T),
    Grouped(T),
    Tuple(Vec<T>),
    UnexpectedComma,
}

pub(super) fn validate_tuple_shape_raw<T>(
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
