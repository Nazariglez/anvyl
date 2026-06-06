use chumsky::{error::Rich, prelude::*};

use super::{
    AnvParser, BoxedParser,
    common::{
        TupleShapeResult, colon, field_name_ident, identifier, literal, parenthesized_tuple_parts,
        validate_tuple_shape_raw,
    },
};
use crate::{
    ast,
    lexer::{Delimiter, Keyword, Op, Token},
    span::{SourceSpan, Spanned},
};

fn let_or_var_head<'src>() -> BoxedParser<'src, ast::PatternHead> {
    select! {
        Token::Keyword(Keyword::Let) => ast::PatternHead::Let,
        Token::Keyword(Keyword::Var) => ast::PatternHead::Var,
    }
    .boxed()
}

pub(super) fn binding_pattern<'src>() -> BoxedParser<'src, (ast::PatternHead, ast::PatternNode)> {
    let_or_var_head().then(pattern()).boxed()
}

pub(super) fn pattern<'src>() -> BoxedParser<'src, ast::PatternNode> {
    recursive(|pat| {
        let ident_or_wildcard = identifier().map_with(|ident, e| {
            let s = e.span();
            let span = s.byte();
            if ident.0.as_ref() == "_" {
                Spanned::new(ast::Pattern::Wildcard, span)
            } else {
                Spanned::new(ast::Pattern::Ident(ident), span)
            }
        });

        let rest_pat = select! {
            Token::Range => ast::Pattern::Rest
        }
        .map_with(|pattern, e| {
            let span: SourceSpan = e.span();
            Spanned::new(pattern, span.byte())
        });

        let var_pat = select! {
            Token::Keyword(Keyword::Var) => ()
        }
        .ignore_then(identifier())
        .validate(|name, e, emitter| {
            let s = e.span();
            let span = s.byte();
            emitter.emit(Rich::custom(
                s,
                "`var` is only allowed before the whole pattern",
            ));
            Spanned::new(ast::Pattern::Ident(name), span)
        });

        let nil_pat = select! {
            Token::Keyword(Keyword::Nil) => ast::Pattern::Nil
        }
        .map_with(|pattern, e| {
            let span: SourceSpan = e.span();
            Spanned::new(pattern, span.byte())
        });

        let minus = select! { Token::Op(Op::Sub) => () };
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
            Token::Range => false,
            Token::RangeEq => true,
        };

        let prefix_range_pat = range_op
            .then(num_lit.clone())
            .map_with(|(inclusive, end), e| {
                let s = e.span();
                let span = s.byte();
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
            select! { Token::RangeEq => true }
                .then(num_lit.clone())
                .map(|(inc, end)| (inc, Some(end))),
            select! { Token::Range => false }
                .then(num_lit.clone().or_not())
                .map(|(inc, end)| (inc, end)),
        ));

        let lit_or_range_pat =
            num_lit
                .clone()
                .then(range_suffix.or_not())
                .map_with(|(start, rest), e| {
                    let s = e.span();
                    let span = s.byte();
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

        let question = select! { Token::Question => () };

        let atom = choice((
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
                let span = s.byte();
                Spanned::new(ast::Pattern::Optional(Box::new(pat)), span)
            } else {
                pat
            }
        });

        let pipe = select! { Token::Op(Op::BitOr) => () };
        atom.separated_by(pipe)
            .at_least(1)
            .collect::<Vec<_>>()
            .map_with(|mut patterns, e| {
                let s = e.span();
                let span = s.byte();
                if patterns.len() == 1 {
                    patterns.remove(0)
                } else {
                    Spanned::new(ast::Pattern::Or(patterns), span)
                }
            })
    })
    .labelled("pattern")
    .as_context()
    .boxed()
}

fn pattern_field<'src>(
    pat: impl AnvParser<'src, ast::PatternNode>,
) -> BoxedParser<'src, (ast::Ident, ast::PatternNode)> {
    let field_with_pattern = field_name_ident()
        .then_ignore(colon())
        .then(pat)
        .map(|(name, p)| (name, p));

    let field_shorthand = identifier().map_with(|name, e| {
        let s = e.span();
        let span = s.byte();
        (name, Spanned::new(ast::Pattern::Ident(name), span))
    });

    choice((field_with_pattern, field_shorthand)).boxed()
}

fn struct_pattern<'src>(
    pat: impl AnvParser<'src, ast::PatternNode>,
) -> BoxedParser<'src, ast::PatternNode> {
    let comma = select! { Token::Comma => () };
    let open_brace = select! { Token::Open(Delimiter::Brace) => () };
    let close_brace = select! { Token::Close(Delimiter::Brace) => () };

    let field = pattern_field(pat);

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
            let span = s.byte();
            Spanned::new(ast::Pattern::Struct { name, fields }, span)
        })
        .labelled("struct pattern")
        .as_context()
        .boxed()
}

fn enum_pattern<'src>(
    pat: impl AnvParser<'src, ast::PatternNode>,
) -> BoxedParser<'src, ast::PatternNode> {
    let dot = select! { Token::Dot => () };

    let qualified_name = identifier().then_ignore(dot).then(identifier());

    qualified_name
        .then(choice((
            enum_tuple_payload(pat.clone()),
            enum_struct_payload(pat),
            empty().to(ast::EnumPatternPayload::Unit),
        )))
        .map_with(|((qualifier, variant), payload), e| {
            let s = e.span();
            let span = s.byte();
            Spanned::new(
                ast::Pattern::Enum {
                    qualifier: Some(qualifier),
                    variant,
                    payload,
                },
                span,
            )
        })
        .labelled("enum pattern")
        .as_context()
        .boxed()
}

fn inferred_enum_pattern<'src>(
    pat: impl AnvParser<'src, ast::PatternNode>,
) -> BoxedParser<'src, ast::PatternNode> {
    let dot = select! { Token::Dot => () };

    dot.ignore_then(identifier())
        .then(choice((
            enum_tuple_payload(pat.clone()),
            enum_struct_payload(pat),
            empty().to(ast::EnumPatternPayload::Unit),
        )))
        .map_with(|(variant, payload), e| {
            let s = e.span();
            let span = s.byte();
            Spanned::new(
                ast::Pattern::Enum {
                    qualifier: None,
                    variant,
                    payload,
                },
                span,
            )
        })
        .labelled("inferred enum pattern")
        .as_context()
        .boxed()
}

fn enum_tuple_payload<'src>(
    pat: impl AnvParser<'src, ast::PatternNode>,
) -> BoxedParser<'src, ast::EnumPatternPayload> {
    let comma = select! { Token::Comma => () };
    let open_paren = select! { Token::Open(Delimiter::Parent) => () };
    let close_paren = select! { Token::Close(Delimiter::Parent) => () };

    open_paren
        .ignore_then(pat.separated_by(comma).allow_trailing().collect::<Vec<_>>())
        .then_ignore(close_paren)
        .map(ast::EnumPatternPayload::Tuple)
        .boxed()
}

fn enum_struct_payload<'src>(
    pat: impl AnvParser<'src, ast::PatternNode>,
) -> BoxedParser<'src, ast::EnumPatternPayload> {
    let comma = select! { Token::Comma => () };
    let open_brace = select! { Token::Open(Delimiter::Brace) => () };
    let close_brace = select! { Token::Close(Delimiter::Brace) => () };
    let rest = select! { Token::Range => () };

    let field = pattern_field(pat);

    open_brace
        .ignore_then(
            field
                .separated_by(comma)
                .allow_trailing()
                .collect::<Vec<_>>(),
        )
        .then(rest.or_not())
        .then_ignore(close_brace)
        .map(|(fields, rest_tok)| ast::EnumPatternPayload::Struct {
            fields,
            has_rest: rest_tok.is_some(),
        })
        .boxed()
}

enum TuplePatternElem {
    Pos(ast::PatternNode),
    Labelled(ast::PatternNode),
}

impl TuplePatternElem {
    fn is_labelled(&self) -> bool {
        matches!(self, Self::Labelled(_))
    }

    fn into_pattern(self) -> ast::PatternNode {
        match self {
            Self::Pos(pat) | Self::Labelled(pat) => pat,
        }
    }
}

fn tuple_pattern<'src>(
    pat: impl AnvParser<'src, ast::PatternNode>,
) -> BoxedParser<'src, ast::PatternNode> {
    let labelled_elem = identifier()
        .then_ignore(colon())
        .ignore_then(pat.clone())
        .map(TuplePatternElem::Labelled);

    let pos_elem = pat.map(TuplePatternElem::Pos);
    let elem = choice((labelled_elem, pos_elem));

    parenthesized_tuple_parts(elem)
        .validate(|parts, e, emitter| {
            let s = e.span();
            let span = s.byte();
            let wildcard = || Spanned::new(ast::Pattern::Wildcard, span);

            let has_label = parts
                .first
                .as_ref()
                .is_some_and(TuplePatternElem::is_labelled)
                || parts.rest.iter().any(TuplePatternElem::is_labelled);

            if has_label {
                emitter.emit(Rich::custom(
                    s,
                    "labels are not allowed in tuple patterns; use a struct pattern or positional tuple",
                ));
                return wildcard();
            }

            let first = parts.first.map(TuplePatternElem::into_pattern);
            let rest = parts
                .rest
                .into_iter()
                .map(TuplePatternElem::into_pattern)
                .collect();

            match validate_tuple_shape_raw(first, rest, parts.trailing_comma) {
                TupleShapeResult::Empty => {
                    emitter.emit(Rich::custom(s, "empty tuple patterns are not supported"));
                    wildcard()
                }
                TupleShapeResult::OneTupleError(pat) => {
                    emitter.emit(Rich::custom(s, "1-tuple patterns are not supported"));
                    pat
                }
                TupleShapeResult::Grouped(pat) => pat,
                TupleShapeResult::Tuple(pats) => Spanned::new(ast::Pattern::Tuple(pats), span),
                TupleShapeResult::UnexpectedComma => {
                    emitter.emit(Rich::custom(s, "unexpected comma in pattern"));
                    wildcard()
                }
            }
        })
        .labelled("tuple pattern")
        .as_context()
        .boxed()
}
