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

pub(super) fn comma<'src>() -> BoxedParser<'src, ()> {
    select! { Token::Comma => () }.boxed()
}

pub(super) fn colon<'src>() -> BoxedParser<'src, ()> {
    select! { Token::Colon => () }.boxed()
}

pub(super) fn dot<'src>() -> BoxedParser<'src, ()> {
    select! { Token::Dot => () }.boxed()
}

pub(super) fn open_delimiter<'src>(delimiter: Delimiter) -> BoxedParser<'src, ()> {
    select! { Token::Open(found) if found == delimiter => () }.boxed()
}

pub(super) fn close_delimiter<'src>(delimiter: Delimiter) -> BoxedParser<'src, ()> {
    select! { Token::Close(found) if found == delimiter => () }.boxed()
}

pub(super) fn parens<'src, T: 'src>(item: impl AnvParser<'src, T>) -> BoxedParser<'src, T> {
    item.delimited_by(
        open_delimiter(Delimiter::Parent),
        close_delimiter(Delimiter::Parent),
    )
    .boxed()
}

pub(super) fn braces<'src, T: 'src>(item: impl AnvParser<'src, T>) -> BoxedParser<'src, T> {
    item.delimited_by(
        open_delimiter(Delimiter::Brace),
        close_delimiter(Delimiter::Brace),
    )
    .boxed()
}

pub(super) fn brackets<'src, T: 'src>(item: impl AnvParser<'src, T>) -> BoxedParser<'src, T> {
    item.delimited_by(
        open_delimiter(Delimiter::Bracket),
        close_delimiter(Delimiter::Bracket),
    )
    .boxed()
}

pub(super) fn comma_list<'src, T: 'src>(
    item: impl AnvParser<'src, T>,
) -> BoxedParser<'src, Vec<T>> {
    item.separated_by(comma())
        .allow_trailing()
        .collect::<Vec<_>>()
        .or_not()
        .map(Option::unwrap_or_default)
        .boxed()
}

pub(super) fn nonempty_comma_list<'src, T: 'src>(
    item: impl AnvParser<'src, T>,
) -> BoxedParser<'src, Vec<T>> {
    item.separated_by(comma())
        .allow_trailing()
        .at_least(1)
        .collect::<Vec<_>>()
        .boxed()
}

pub(super) fn qualified_name<'src>() -> BoxedParser<'src, (Option<ast::Ident>, ast::Ident)> {
    identifier()
        .then(dot().ignore_then(identifier()).or_not())
        .map(|(first, second)| match second {
            Some(second) => (Some(first), second),
            None => (None, first),
        })
        .boxed()
}

pub(super) fn escaping_kw<'src>() -> BoxedParser<'src, ()> {
    select! { Token::Ident(ident) if ident.0.as_ref() == "escaping" => () }.boxed()
}

pub(super) fn escaping_type<'src>(
    ty: impl AnvParser<'src, ast::Type>,
) -> BoxedParser<'src, (ast::EscapeMode, ast::Type)> {
    choice((
        escaping_kw()
            .ignore_then(ty.clone())
            .map(|ty| (ast::EscapeMode::Escaping, ty)),
        ty.map(|ty| (ast::EscapeMode::NonEscaping, ty)),
    ))
    .boxed()
}

pub(super) fn callable_param_type<'src>(
    ty: impl AnvParser<'src, ast::Type>,
) -> BoxedParser<'src, (bool, ast::EscapeMode, ast::Type)> {
    let as_kw = select! { Token::Keyword(Keyword::As) => () }
        .or_not()
        .map(|opt| opt.is_some());
    choice((
        escaping_kw()
            .ignore_then(as_kw)
            .then(ty.clone())
            .map(|(cast_accept, ty)| (cast_accept, ast::EscapeMode::Escaping, ty)),
        as_kw
            .then(escaping_type(ty))
            .map(|(cast_accept, (escape, ty))| (cast_accept, escape, ty)),
    ))
    .boxed()
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
    parens(comma_list(param(stmt))).boxed()
}

pub(super) fn param<'src>(
    stmt: impl AnvParser<'src, ast::StmtNode>,
) -> BoxedParser<'src, ast::Param> {
    let access = select! {
        Token::Keyword(Keyword::Ref) => ast::Mutability::Mutable,
    }
    .or_not()
    .map(|opt| opt.unwrap_or(ast::Mutability::Immutable));

    let ty =
        callable_param_type(param_type_ident()).map_with(|ty, extra| (ty, extra.span().byte()));

    access
        .then(identifier())
        .then_ignore(colon())
        .then(ty)
        .then(
            select! { Token::Op(Op::Assign) => () }
                .ignore_then(expression(stmt))
                .or_not(),
        )
        .map(
            |(((mutability, name), ((cast_accept, escape, ty), ty_span)), default)| ast::Param {
                mutability,
                escape,
                name,
                ty,
                ty_span,
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
    let access = select! { Token::Keyword(Keyword::Ref) => ast::ReturnAccess::Place }
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
    braces(stmt.repeated().collect::<Vec<_>>().then(tail_expr.or_not()))
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

pub(super) struct TupleParts<T> {
    pub first: Option<T>,
    pub rest: Vec<T>,
    pub trailing_comma: bool,
}

pub(super) fn parenthesized_tuple_parts<'src, T: 'src>(
    elem: impl AnvParser<'src, T>,
) -> BoxedParser<'src, TupleParts<T>> {
    let rest = comma()
        .ignore_then(elem.clone())
        .repeated()
        .collect::<Vec<_>>();
    parens(elem.or_not().then(rest).then(comma().or_not()))
        .map(|((first, rest), trailing_comma)| TupleParts {
            first,
            rest,
            trailing_comma: trailing_comma.is_some(),
        })
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
