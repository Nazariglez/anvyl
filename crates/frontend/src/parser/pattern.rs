use chumsky::{error::Rich, prelude::*};

use super::{
    AnvParser, BoxedParser,
    common::{TupleShapeResult, identifier, literal, validate_tuple_shape_raw},
};
use crate::{
    ast,
    lexer::{Delimiter, Keyword, Op, Token},
    span::{Span, Spanned},
};

pub(super) fn pattern<'src>() -> BoxedParser<'src, ast::PatternNode> {
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

        let rest_pat = select! {
            (Token::Range, s) => Spanned::new(ast::Pattern::Rest, s)
        };

        let var_pat = select! {
            (Token::Keyword(Keyword::Var), _) => ()
        }
        .ignore_then(identifier())
        .map_with(|name, e| {
            let s = e.span();
            let span = Span::new(s.start, s.end);
            Spanned::new(ast::Pattern::VarIdent(name), span)
        });

        let nil_pat = select! {
            (Token::Keyword(Keyword::Nil), s) => Spanned::new(ast::Pattern::Nil, s)
        };

        let minus = select! { (Token::Op(Op::Sub), _) => () };
        let num_lit =
            minus
                .or_not()
                .then(literal())
                .try_map(|(neg, lit), span| match (&neg, &lit) {
                    (Some(()), ast::Lit::Int(n)) => Ok(ast::Lit::Int(-n)),
                    (Some(()), ast::Lit::Float(value)) => Ok(ast::Lit::Float(-value)),
                    (Some(()), _) => Err(Rich::custom(span, "cannot negate non-numeric literal")),
                    (None, _) => Ok(lit),
                });

        let range_op = select! {
            (Token::Range, _) => false,
            (Token::RangeEq, _) => true,
        };

        let prefix_range_pat = range_op
            .then(num_lit.clone())
            .map_with(|(inclusive, end), e| {
                let s = e.span();
                let span = Span::new(s.start, s.end);
                Spanned::new(
                    ast::Pattern::Range {
                        start: None,
                        end: Some(end),
                        inclusive,
                    },
                    span,
                )
            });

        let range_suffix = choice((
            select! { (Token::RangeEq, _) => true }
                .then(num_lit.clone())
                .map(|(inc, end)| (inc, Some(end))),
            select! { (Token::Range, _) => false }
                .then(num_lit.clone().or_not())
                .map(|(inc, end)| (inc, end)),
        ));

        let lit_or_range_pat =
            num_lit
                .clone()
                .then(range_suffix.or_not())
                .map_with(|(start, rest), e| {
                    let s = e.span();
                    let span = Span::new(s.start, s.end);
                    match rest {
                        Some((inclusive, Some(end))) => Spanned::new(
                            ast::Pattern::Range {
                                start: Some(start),
                                end: Some(end),
                                inclusive,
                            },
                            span,
                        ),
                        Some((false, None)) => Spanned::new(
                            ast::Pattern::Range {
                                start: Some(start),
                                end: None,
                                inclusive: false,
                            },
                            span,
                        ),
                        Some((true, None)) => unreachable!(),
                        None => Spanned::new(ast::Pattern::Lit(start), span),
                    }
                });

        let tuple_pat = tuple_pattern(pat.clone());
        let enum_pat = enum_pattern(pat.clone());
        let inferred_enum_pat = inferred_enum_pattern(pat.clone());
        let struct_pat = struct_pattern(pat);

        let question = select! { (Token::Question, _) => () };

        choice((
            prefix_range_pat,
            rest_pat,
            var_pat,
            nil_pat,
            lit_or_range_pat,
            inferred_enum_pat,
            enum_pat,
            struct_pat,
            tuple_pat,
            ident_or_wildcard,
        ))
        .then(question.or_not())
        .map_with(|(pat, q), e| {
            if q.is_some() {
                let s = e.span();
                let span = Span::new(s.start, s.end);
                Spanned::new(ast::Pattern::Optional(Box::new(pat)), span)
            } else {
                pat
            }
        })
    })
    .labelled("pattern")
    .as_context()
    .boxed()
}

pub(super) fn or_pattern<'src>() -> BoxedParser<'src, ast::PatternNode> {
    let pipe = select! { (Token::Op(Op::BitOr), _) => () };
    pattern()
        .separated_by(pipe)
        .at_least(1)
        .collect::<Vec<_>>()
        .map_with(|mut patterns, e| {
            let s = e.span();
            let span = Span::new(s.start, s.end);
            if patterns.len() == 1 {
                patterns.remove(0)
            } else {
                Spanned::new(ast::Pattern::Or(patterns), span)
            }
        })
        .labelled("pattern")
        .as_context()
        .boxed()
}

fn struct_pattern<'src>(
    pat: impl AnvParser<'src, ast::PatternNode>,
) -> BoxedParser<'src, ast::PatternNode> {
    let comma = select! { (Token::Comma, _) => () };
    let colon = select! { (Token::Colon, _) => () };
    let open_brace = select! { (Token::Open(Delimiter::Brace), _) => () };
    let close_brace = select! { (Token::Close(Delimiter::Brace), _) => () };

    let field_with_pattern = identifier()
        .then_ignore(colon)
        .then(pat.clone())
        .map(|(name, p)| (name, p));

    let field_shorthand = identifier().map_with(|name, e| {
        let s = e.span();
        let span = Span::new(s.start, s.end);
        (name, Spanned::new(ast::Pattern::Ident(name), span))
    });

    let field = choice((field_with_pattern, field_shorthand));

    identifier()
        .then(
            open_brace
                .ignore_then(
                    field
                        .separated_by(comma)
                        .allow_trailing()
                        .collect::<Vec<_>>(),
                )
                .then_ignore(close_brace),
        )
        .map_with(|(name, fields), e| {
            let s = e.span();
            let span = Span::new(s.start, s.end);
            Spanned::new(ast::Pattern::Struct { name, fields }, span)
        })
        .labelled("struct pattern")
        .as_context()
        .boxed()
}

#[derive(Clone)]
enum EnumPatternKind {
    Unit,
    Tuple(Vec<ast::PatternNode>),
    Struct(Vec<(ast::Ident, ast::PatternNode)>, bool),
}

fn enum_pattern<'src>(
    pat: impl AnvParser<'src, ast::PatternNode>,
) -> BoxedParser<'src, ast::PatternNode> {
    let dot = select! { (Token::Dot, _) => () };

    let qualified_name = identifier().then_ignore(dot).then(identifier());

    qualified_name
        .then(choice((
            enum_tuple_payload(pat.clone()),
            enum_struct_payload(pat),
            empty().to(EnumPatternKind::Unit),
        )))
        .map_with(|((qualifier, variant), kind), e| {
            let s = e.span();
            let span = Span::new(s.start, s.end);
            let pattern = match kind {
                EnumPatternKind::Unit => ast::Pattern::EnumUnit { qualifier, variant },
                EnumPatternKind::Tuple(fields) => ast::Pattern::EnumTuple {
                    qualifier,
                    variant,
                    fields,
                },
                EnumPatternKind::Struct(fields, has_rest) => ast::Pattern::EnumStruct {
                    qualifier,
                    variant,
                    fields,
                    has_rest,
                },
            };
            Spanned::new(pattern, span)
        })
        .labelled("enum pattern")
        .as_context()
        .boxed()
}

fn inferred_enum_pattern<'src>(
    pat: impl AnvParser<'src, ast::PatternNode>,
) -> BoxedParser<'src, ast::PatternNode> {
    let dot = select! { (Token::Dot, _) => () };

    dot.ignore_then(identifier())
        .then(choice((
            enum_tuple_payload(pat.clone()),
            enum_struct_payload(pat),
            empty().to(EnumPatternKind::Unit),
        )))
        .map_with(|(variant, kind), e| {
            let s = e.span();
            let span = Span::new(s.start, s.end);
            let pattern = match kind {
                EnumPatternKind::Unit => ast::Pattern::InferredEnumUnit { variant },
                EnumPatternKind::Tuple(fields) => {
                    ast::Pattern::InferredEnumTuple { variant, fields }
                }
                EnumPatternKind::Struct(fields, has_rest) => ast::Pattern::InferredEnumStruct {
                    variant,
                    fields,
                    has_rest,
                },
            };
            Spanned::new(pattern, span)
        })
        .labelled("inferred enum pattern")
        .as_context()
        .boxed()
}

fn enum_tuple_payload<'src>(
    pat: impl AnvParser<'src, ast::PatternNode>,
) -> BoxedParser<'src, EnumPatternKind> {
    let comma = select! { (Token::Comma, _) => () };
    let open_paren = select! { (Token::Open(Delimiter::Parent), _) => () };
    let close_paren = select! { (Token::Close(Delimiter::Parent), _) => () };

    open_paren
        .ignore_then(pat.separated_by(comma).allow_trailing().collect::<Vec<_>>())
        .then_ignore(close_paren)
        .map(EnumPatternKind::Tuple)
        .boxed()
}

fn enum_struct_payload<'src>(
    pat: impl AnvParser<'src, ast::PatternNode>,
) -> BoxedParser<'src, EnumPatternKind> {
    let comma = select! { (Token::Comma, _) => () };
    let colon = select! { (Token::Colon, _) => () };
    let open_brace = select! { (Token::Open(Delimiter::Brace), _) => () };
    let close_brace = select! { (Token::Close(Delimiter::Brace), _) => () };
    let rest = select! { (Token::Range, _) => () };

    let field_with_pattern = identifier()
        .then_ignore(colon)
        .then(pat.clone())
        .map(|(name, p)| (name, p));

    let field_shorthand = identifier().map_with(|name, e| {
        let s = e.span();
        let span = Span::new(s.start, s.end);
        (name, Spanned::new(ast::Pattern::Ident(name), span))
    });

    let field = choice((field_with_pattern, field_shorthand));

    open_brace
        .ignore_then(
            field
                .separated_by(comma)
                .allow_trailing()
                .collect::<Vec<_>>(),
        )
        .then(rest.or_not())
        .then_ignore(close_brace)
        .map(|(fields, rest_tok)| EnumPatternKind::Struct(fields, rest_tok.is_some()))
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
        .then_ignore(colon)
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
                        TuplePatternElem::Pos(p) | TuplePatternElem::Labeled(_, p) => p,
                    }
                }
                TupleShapeResult::Grouped(elem) => match elem {
                    TuplePatternElem::Pos(p) | TuplePatternElem::Labeled(_, p) => p,
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
                                TuplePatternElem::Pos(p) | TuplePatternElem::Labeled(_, p) => p,
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
