use chumsky::{error::Rich, prelude::*};

use super::{
    AnvParser, BoxedParser,
    common::{TupleShapeResult, identifier, validate_tuple_shape_raw},
};
use crate::{
    ast::{self, Type},
    lexer::{Delimiter, Keyword, LitToken, Op, Token},
};

#[derive(Clone)]
enum TypeSuffix {
    Optional,
}

#[derive(Clone, Copy)]
enum TypeContext {
    Ordinary,
    Param,
    ExtendTarget,
    TypeSubject,
}

impl TypeContext {
    fn allows_slice(self) -> bool {
        matches!(self, Self::Param | Self::ExtendTarget | Self::TypeSubject)
    }
}

pub(super) fn type_ident<'src>() -> BoxedParser<'src, Type> {
    type_ident_inner(TypeContext::Ordinary)
}

pub(super) fn param_type_ident<'src>() -> BoxedParser<'src, Type> {
    type_ident_inner(TypeContext::Param)
}

pub(super) fn extend_type_ident<'src>() -> BoxedParser<'src, Type> {
    type_ident_inner(TypeContext::ExtendTarget)
}

pub(super) fn type_subject_type_ident<'src>() -> BoxedParser<'src, Type> {
    type_ident_inner(TypeContext::TypeSubject)
}

fn type_contains_slice(ty: &Type) -> bool {
    match ty {
        Type::Slice { .. } => true,
        Type::Func { params, ret } => {
            params.iter().any(|param| type_contains_slice(&param.ty)) || type_contains_slice(ret)
        }
        Type::Tuple(elems) => elems.iter().any(type_contains_slice),
        Type::Nominal(nominal) => nominal.type_args.iter().any(type_contains_slice),
        Type::List { elem } | Type::Array { elem, .. } => type_contains_slice(elem),
        Type::Map { key, value } => type_contains_slice(key) || type_contains_slice(value),
        Type::UnresolvedNominal { generic_args, .. } => {
            generic_args.iter().any(generic_arg_contains_slice)
        }
        Type::Infer
        | Type::InferReturn
        | Type::Any
        | Type::Int
        | Type::Float
        | Type::Bool
        | Type::String
        | Type::Void
        | Type::Var(_)
        | Type::UnresolvedName(_) => false,
    }
}

fn generic_arg_contains_slice(arg: &ast::GenericArg) -> bool {
    match arg {
        ast::GenericArg::Type(ty) => type_contains_slice(ty),
        ast::GenericArg::Const(_) => false,
    }
}

fn const_value_arg<'src>() -> BoxedParser<'src, ast::ConstArg> {
    select! {
        Token::Literal(LitToken::Number(n)) => ast::ConstArg::Value(ast::ConstValue::Int(n)),
        Token::Literal(LitToken::Float(s)) => {
            let value = s.as_ref().parse::<f64>().unwrap_or(0.0);
            ast::ConstArg::Value(ast::ConstValue::Float(value))
        },
        Token::Literal(LitToken::String(s)) => {
            ast::ConstArg::Value(ast::ConstValue::String(s.to_string()))
        },
        Token::Keyword(Keyword::True) => ast::ConstArg::Value(ast::ConstValue::Bool(true)),
        Token::Keyword(Keyword::False) => ast::ConstArg::Value(ast::ConstValue::Bool(false)),
    }
    .labelled("const argument")
    .as_context()
    .boxed()
}

pub(super) fn generic_arg<'src>(
    ty: impl AnvParser<'src, Type>,
) -> BoxedParser<'src, ast::GenericArg> {
    choice((
        const_value_arg().map(ast::GenericArg::Const),
        ty.map(ast::GenericArg::Type),
    ))
    .boxed()
}

fn type_ident_inner<'src>(context: TypeContext) -> BoxedParser<'src, Type> {
    recursive(move |type_parser| {
        let builtin_typ = select! {
            Token::Keyword(Keyword::Int) => Type::Int,
            Token::Keyword(Keyword::Float) => Type::Float,
            Token::Keyword(Keyword::Bool) => Type::Bool,
            Token::Keyword(Keyword::String) => Type::String,
            Token::Keyword(Keyword::Void) => Type::Void,
            Token::Keyword(Keyword::Any) => Type::Any,
        };

        let generic_args = select! { Token::Op(Op::LessThan) => () }
            .ignore_then(
                generic_arg(type_parser.clone())
                    .validate(|arg, extra, emitter| {
                        if generic_arg_contains_slice(&arg) {
                            emitter.emit(Rich::custom(
                                extra.span(),
                                "slice types are not allowed in generic arguments",
                            ));
                        }
                        arg
                    })
                    .separated_by(select! { Token::Comma => () })
                    .allow_trailing()
                    .collect::<Vec<_>>(),
            )
            .then_ignore(select! { Token::Op(Op::GreaterThan) => () });

        let type_name_ref = identifier()
            .then(
                select! { Token::Dot => () }
                    .ignore_then(identifier())
                    .or_not(),
            )
            .then(generic_args.or_not())
            .map(|((qualifier_ident, name_ident), generic_args)| {
                let (qualifier, name) = match name_ident {
                    Some(name) => (Some(qualifier_ident), name),
                    None => (None, qualifier_ident),
                };
                Type::UnresolvedNominal {
                    qualifier,
                    name,
                    generic_args: generic_args.unwrap_or_default(),
                }
            });

        let paren_type = paren_or_tuple_type(type_parser.clone());

        let open_paren = select! { Token::Open(Delimiter::Parent) => () };
        let close_paren = select! { Token::Close(Delimiter::Parent) => () };
        let comma = select! { Token::Comma => () };
        let arrow = select! { Token::Op(Op::ThinArrow) => () };
        let open_bracket = select! { Token::Open(Delimiter::Bracket) => () };
        let close_bracket = select! { Token::Close(Delimiter::Bracket) => () };
        let semicolon = select! { Token::Semicolon => () };
        let colon = select! { Token::Colon => () };

        let param_type_parser: BoxedParser<'src, Type> = if context.allows_slice() {
            type_parser.clone().boxed()
        } else {
            param_type_ident()
        };

        let array_len_fixed =
            select! { Token::Literal(LitToken::Number(n)) => ast::ArrayLen::Fixed(n as usize) };
        let array_len_ident = identifier().map(|ident| {
            if ident.0.as_ref() == "_" {
                ast::ArrayLen::Infer
            } else {
                ast::ArrayLen::Named(ident)
            }
        });
        let array_len = choice((array_len_fixed, array_len_ident));

        let map_type = open_bracket
            .ignore_then(type_parser.clone())
            .then_ignore(colon)
            .then(type_parser.clone())
            .then_ignore(close_bracket)
            .map(|(key, value)| Type::Map {
                key: key.boxed(),
                value: value.boxed(),
            });

        let list_type = open_bracket
            .ignore_then(type_parser.clone())
            .then_ignore(close_bracket)
            .map(|elem| Type::List { elem: elem.boxed() });

        let array_type = open_bracket
            .ignore_then(type_parser.clone())
            .then_ignore(semicolon)
            .then(array_len)
            .then_ignore(close_bracket)
            .map(|(elem, len)| Type::Array {
                elem: elem.boxed(),
                len,
            });

        let slice_type = select! { Token::Ident(i) if i.0.as_ref() == "slice" => () }
            .ignore_then(open_bracket)
            .ignore_then(type_parser.clone())
            .then_ignore(close_bracket)
            .validate(move |elem, extra, emitter| {
                if !context.allows_slice() {
                    emitter.emit(Rich::custom(
                        extra.span(),
                        "slice types are only allowed in function parameters or extend targets",
                    ));
                }
                Type::Slice { elem: elem.boxed() }
            });

        let bracketed_type = choice((array_type, choice((map_type, list_type))));

        let var_kw = select! { Token::Keyword(Keyword::Var) => () }
            .or_not()
            .map(|opt| opt.is_some());

        let fn_param = var_kw
            .then(param_type_parser)
            .map(|(mutable, ty)| ast::FuncParam::new(ty, mutable, false));

        let fn_type = select! { Token::Keyword(Keyword::Fn) => () }
            .ignore_then(
                open_paren
                    .ignore_then(
                        fn_param
                            .separated_by(comma)
                            .allow_trailing()
                            .collect::<Vec<_>>()
                            .or_not()
                            .map(Option::unwrap_or_default),
                    )
                    .then_ignore(close_paren),
            )
            .then_ignore(arrow)
            .then(type_parser.clone())
            .map(|(params, ret)| Type::Func {
                params,
                ret: ret.boxed(),
            });

        let primary_type = choice((
            builtin_typ,
            slice_type,
            type_name_ref,
            paren_type,
            bracketed_type,
            fn_type,
        ));
        let optional_suffix = select! { Token::Question => TypeSuffix::Optional };
        let type_suffix = optional_suffix;

        primary_type
            .then(type_suffix.repeated().collect::<Vec<_>>())
            .map(|(base, suffixes)| {
                suffixes.into_iter().fold(base, |ty, sfx| match sfx {
                    TypeSuffix::Optional => Type::option_of(ty),
                })
            })
    })
    .labelled("type")
    .as_context()
    .boxed()
}

enum TupleTypeElem {
    Pos(Type),
    Labelled(Type),
}

impl TupleTypeElem {
    fn is_labelled(&self) -> bool {
        matches!(self, Self::Labelled(_))
    }

    fn into_type(self) -> Type {
        match self {
            Self::Pos(ty) | Self::Labelled(ty) => ty,
        }
    }
}

fn paren_or_tuple_type<'src>(type_parser: impl AnvParser<'src, Type>) -> BoxedParser<'src, Type> {
    let comma = select! { Token::Comma => () };
    let open_paren = select! { Token::Open(Delimiter::Parent) => () };
    let close_paren = select! { Token::Close(Delimiter::Parent) => () };
    let colon = select! { Token::Colon => () };

    let labelled_elem = identifier()
        .then_ignore(colon)
        .ignore_then(type_parser.clone())
        .map(TupleTypeElem::Labelled);

    let pos_elem = type_parser.map(TupleTypeElem::Pos);
    let elem = choice((labelled_elem, pos_elem));

    let first_elem = elem.clone();
    let rest_elems = comma.ignore_then(elem).repeated().collect::<Vec<_>>();

    open_paren
        .ignore_then(first_elem.or_not())
        .then(rest_elems)
        .then(comma.or_not())
        .then_ignore(close_paren)
        .validate(|((first, rest), trailing_comma), e, emitter| {
            let s = e.span();
            let has_label = first.as_ref().is_some_and(TupleTypeElem::is_labelled)
                || rest.iter().any(TupleTypeElem::is_labelled);

            if has_label {
                emitter.emit(Rich::custom(
                    s,
                    "labels are not allowed in tuple types; use a struct for field names",
                ));
                return Type::Void;
            }

            let first = first.map(TupleTypeElem::into_type);
            let rest = rest.into_iter().map(TupleTypeElem::into_type).collect();

            match validate_tuple_shape_raw(first, rest, trailing_comma.is_some()) {
                TupleShapeResult::Empty => {
                    emitter.emit(Rich::custom(s, "empty tuples are not supported"));
                    Type::Void
                }
                TupleShapeResult::OneTupleError(ty) => {
                    emitter.emit(Rich::custom(s, "1-tuples are not supported"));
                    ty
                }
                TupleShapeResult::Grouped(ty) => ty,
                TupleShapeResult::Tuple(types) => Type::Tuple(types),
                TupleShapeResult::UnexpectedComma => {
                    emitter.emit(Rich::custom(s, "unexpected comma"));
                    Type::Void
                }
            }
        })
        .labelled("tuple or grouped type")
        .as_context()
        .boxed()
}
