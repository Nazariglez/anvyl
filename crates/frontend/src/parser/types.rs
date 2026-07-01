use chumsky::{error::Rich, prelude::*};

use super::{
    AnvParser, BoxedParser,
    common::{
        TupleShapeResult, colon, escaping_kw, escaping_type, identifier, parenthesized_tuple_parts,
        validate_tuple_shape_raw,
    },
};
use crate::{
    ast::{self, Type, TypeVisitor},
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
    struct SliceVisitor;

    impl TypeVisitor for SliceVisitor {
        fn visit_type_leaf(&mut self, ty: &Type) -> bool {
            matches!(ty, Type::Slice { .. })
        }
    }

    SliceVisitor.visit_type(ty)
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

pub(super) fn contract_ref<'src>() -> BoxedParser<'src, ast::ContractRef> {
    contract_ref_with(named_contract_ref())
}

fn dyn_contract_ref<'src>() -> BoxedParser<'src, ast::ContractRef> {
    let anonymous = select! { Token::Open(Delimiter::Brace) => () }
        .ignore_then(
            any()
                .filter(|token| !matches!(token, Token::Close(Delimiter::Brace)))
                .repeated(),
        )
        .then_ignore(select! { Token::Close(Delimiter::Brace) => () })
        .validate(|(), extra, emitter| {
            emitter.emit(Rich::custom(
                extra.span(),
                "anonymous dynamic contract syntax is not supported; declare a named contract or use dyn _ in a callable parameter",
            ));
            ast::ContractRef::Infer
        });
    contract_ref_with(choice((
        anonymous,
        inferred_contract_ref(),
        named_contract_ref(),
    )))
}

fn contract_ref_with<'src>(
    operand: impl AnvParser<'src, ast::ContractRef>,
) -> BoxedParser<'src, ast::ContractRef> {
    operand
        .separated_by(select! { Token::Op(Op::Add) => () })
        .at_least(1)
        .collect::<Vec<_>>()
        .map(|mut contracts| {
            if contracts.len() == 1 {
                contracts.pop().expect("one contract ref")
            } else {
                ast::ContractRef::Intersection(contracts)
            }
        })
        .labelled("contract reference")
        .as_context()
        .boxed()
}

fn inferred_contract_ref<'src>() -> BoxedParser<'src, ast::ContractRef> {
    select! { Token::Ident(name) if name.0.as_ref() == "_" => ast::ContractRef::Infer }.boxed()
}

fn named_contract_ref<'src>() -> BoxedParser<'src, ast::ContractRef> {
    identifier()
        .then(
            select! { Token::Dot => () }
                .ignore_then(identifier())
                .or_not(),
        )
        .map(|(qualifier_ident, name_ident)| match name_ident {
            Some(name) => (Some(qualifier_ident), name),
            None => (None, qualifier_ident),
        })
        .map(|(qualifier, name)| ast::ContractRef::Named {
            qualifier,
            name,
            origin: None,
        })
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

        let name_ref = identifier()
            .then(
                select! { Token::Dot => () }
                    .ignore_then(identifier())
                    .or_not(),
            )
            .map(|(qualifier_ident, name_ident)| match name_ident {
                Some(name) => (Some(qualifier_ident), name),
                None => (None, qualifier_ident),
            });

        let type_name_ref = name_ref.clone().then(generic_args.or_not()).map(
            |((qualifier, name), generic_args)| Type::UnresolvedNominal {
                qualifier,
                name,
                generic_args: generic_args.unwrap_or_default(),
            },
        );

        let dyn_type = select! { Token::Ident(i) if i.0.as_ref() == "dyn" => () }
            .ignore_then(dyn_contract_ref())
            .map(Type::Dyn);

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

        let invalid_escaping_type =
            escaping_kw()
                .ignore_then(type_parser.clone())
                .validate(|ty, extra, emitter| {
                    emitter.emit(Rich::custom(
                        extra.span(),
                        "`escaping` cannot be used on local variable, field, or return types",
                    ));
                    ty
                });

        let ref_kw = select! { Token::Keyword(Keyword::Ref) => () }
            .or_not()
            .map(|opt| opt.is_some());

        let fn_param = ref_kw
            .then(escaping_type(param_type_parser))
            .map(|(mutable, (escape, ty))| ast::FuncParam::new(ty, mutable, false, escape));

        let return_value_type = choice((
            select! { Token::Ident(ident) if ident.0.as_ref() == "_" => Type::InferReturn },
            type_parser.clone(),
        ));
        let return_access = select! { Token::Keyword(Keyword::Ref) => ast::ReturnAccess::Place }
            .or_not()
            .map(|access| access.unwrap_or(ast::ReturnAccess::Value));
        let fn_return_spec = return_access
            .then(return_value_type)
            .map(|(access, ty)| ast::ReturnSpec { access, ty });

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
            .then(arrow.ignore_then(fn_return_spec).or_not())
            .map(|(params, ret)| Type::Func {
                params,
                ret: Box::new(ret.unwrap_or_else(ast::ReturnSpec::void)),
            });

        let primary_type = choice((
            builtin_typ,
            slice_type,
            dyn_type,
            invalid_escaping_type,
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
                    TypeSuffix::Optional => Type::optional_syntax(ty),
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
    let labelled_elem = identifier()
        .then_ignore(colon())
        .ignore_then(type_parser.clone())
        .map(TupleTypeElem::Labelled);

    let pos_elem = type_parser.map(TupleTypeElem::Pos);
    let elem = choice((labelled_elem, pos_elem));

    parenthesized_tuple_parts(elem)
        .validate(|parts, e, emitter| {
            let s = e.span();
            let has_label = parts.first.as_ref().is_some_and(TupleTypeElem::is_labelled)
                || parts.rest.iter().any(TupleTypeElem::is_labelled);

            if has_label {
                emitter.emit(Rich::custom(
                    s,
                    "labels are not allowed in tuple types; use a struct for field names",
                ));
                return Type::Void;
            }

            let first = parts.first.map(TupleTypeElem::into_type);
            let rest = parts
                .rest
                .into_iter()
                .map(TupleTypeElem::into_type)
                .collect();

            match validate_tuple_shape_raw(first, rest, parts.trailing_comma) {
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
