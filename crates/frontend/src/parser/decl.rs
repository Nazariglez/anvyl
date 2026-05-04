use std::collections::HashMap;

use chumsky::{error::Rich, prelude::*};

use super::{
    AnvParser, BoxedParser,
    common::{block_stmt, field_name_ident, identifier, param, params, return_type},
    expr::expression,
    types::type_ident,
};
use crate::{
    ast,
    lexer::{Delimiter, Keyword, LitToken, Op, Token},
    span::{Span, Spanned},
};

pub const SELF_TYPE: &str = "Self";
pub const SELF_ITEM: &str = "self";

fn annotation_value<'src>() -> BoxedParser<'src, ast::Lit> {
    select! {
        (Token::Literal(LitToken::String(s)), _) => ast::Lit::String(s.to_string()),
        (Token::Literal(LitToken::Number(n)), _) => ast::Lit::Int(n),
        (Token::Keyword(Keyword::True), _) => ast::Lit::Bool(true),
        (Token::Keyword(Keyword::False), _) => ast::Lit::Bool(false),
    }
    .labelled("annotation value")
    .as_context()
    .boxed()
}

fn parse_annotation_args<'src>() -> BoxedParser<'src, ast::AnnotationArgs> {
    let open_paren = select! { (Token::Open(Delimiter::Parent), _) => () };
    let close_paren = select! { (Token::Close(Delimiter::Parent), _) => () };
    let comma = select! { (Token::Comma, _) => () };
    let eq = select! { (Token::Op(Op::Assign), _) => () };

    let positional = select! {
        (Token::Literal(LitToken::String(s)), _) => ast::Lit::String(s.to_string()),
    }
    .map(ast::AnnotationArgs::Positional);

    let mixed_arg = identifier()
        .then(eq.ignore_then(annotation_value()).or_not())
        .map(|(name, value)| {
            let lit = value.unwrap_or(ast::Lit::Bool(true));
            (name, lit)
        });

    let mixed = mixed_arg
        .separated_by(comma)
        .allow_trailing()
        .at_least(1)
        .collect::<Vec<_>>()
        .map(ast::AnnotationArgs::Named);

    let args = open_paren
        .ignore_then(choice((positional, mixed)))
        .then_ignore(close_paren);

    args.or_not()
        .map(|opt| opt.unwrap_or(ast::AnnotationArgs::None))
        .boxed()
}

fn parse_annotation<'src>() -> BoxedParser<'src, ast::AnnotationNode> {
    select! { (Token::At, _) => () }
        .ignore_then(identifier())
        .then(parse_annotation_args())
        .map_with(|(name, args), e| {
            let s = e.span();
            Spanned::new(ast::Annotation { name, args }, Span::new(s.start, s.end))
        })
        .boxed()
}

pub(super) fn annotations<'src>() -> BoxedParser<'src, Vec<ast::AnnotationNode>> {
    parse_annotation().repeated().collect::<Vec<_>>().boxed()
}

pub(super) fn doc_comment_block<'src>() -> BoxedParser<'src, Option<String>> {
    select! { (Token::DocComment(s), _) => s.to_string() }
        .repeated()
        .at_least(1)
        .collect::<Vec<_>>()
        .map(|lines| Some(lines.join("\n")))
        .or_not()
        .map(Option::flatten)
        .boxed()
}

#[derive(Default)]
struct GenericParams {
    type_params: Vec<ast::TypeParam>,
    const_params: Vec<ast::ConstParam>,
}

enum GenericParamItem {
    Type(ast::TypeParam),
    Const(ast::ConstParam),
}

fn required_generic_params<'src>() -> BoxedParser<'src, GenericParams> {
    let colon = select! { (Token::Colon, _) => () };
    let int_kw = select! { (Token::Keyword(Keyword::Int), _) => () };

    let generic_param = identifier()
        .then(colon.ignore_then(int_kw).or_not())
        .map_with(|(name, is_const), e| {
            if is_const.is_some() {
                let id = e.state().new_const_param_id();
                GenericParamItem::Const(ast::ConstParam { name, id })
            } else {
                let id = e.state().new_type_var_id();
                GenericParamItem::Type(ast::TypeParam { name, id })
            }
        });

    select! { (Token::Op(Op::LessThan), _) => () }
        .ignore_then(
            generic_param
                .separated_by(select! { (Token::Comma, _) => () })
                .allow_trailing()
                .collect::<Vec<_>>(),
        )
        .then_ignore(select! { (Token::Op(Op::GreaterThan), _) => () })
        .validate(|items, extra, emitter| {
            let mut type_params = vec![];
            let mut const_params = vec![];
            let mut seen_const = false;

            for item in items {
                match item {
                    GenericParamItem::Type(tp) => {
                        if seen_const {
                            emitter.emit(Rich::custom(
                                extra.span(),
                                "type parameters must come before value parameters",
                            ));
                        }
                        type_params.push(tp);
                    }
                    GenericParamItem::Const(cp) => {
                        seen_const = true;
                        const_params.push(cp);
                    }
                }
            }

            GenericParams {
                type_params,
                const_params,
            }
        })
        .labelled("type parameters")
        .as_context()
        .boxed()
}

fn generic_params<'src>() -> BoxedParser<'src, GenericParams> {
    required_generic_params()
        .or_not()
        .map(Option::unwrap_or_default)
        .labelled("type parameters")
        .as_context()
        .boxed()
}

fn colon_import_target(root: ast::Ident, path: Vec<ast::Ident>) -> Option<ast::ImportTarget> {
    match root.as_str() {
        "pkg" => {
            let (alias, path) = path.split_first().expect("dotted path is non-empty");
            Some(ast::ImportTarget::package(alias.to_owned(), path.to_vec()))
        }
        "std" => Some(ast::ImportTarget::std(path)),
        "ext" => Some(ast::ImportTarget::native_provider(path)),
        _ => None,
    }
}

fn local_import_target(dot_count: usize, path: Vec<ast::Ident>) -> ast::ImportTarget {
    let ascend = dot_count.saturating_sub(1);
    ast::ImportTarget::local(ascend, path)
}

pub(super) fn import_declaration<'src>() -> BoxedParser<'src, ast::StmtNode> {
    let import_kw = select! { (Token::Keyword(Keyword::Import), _) => () };
    let dot = select! { (Token::Dot, _) => () };
    let leading_dot = select! {
        (Token::Dot, _) => 1,
        (Token::Range, _) => 2,
    };
    let colon = select! { (Token::Colon, _) => () };
    let semicolon = select! { (Token::Semicolon, _) => () };
    let as_kw = select! { (Token::Keyword(Keyword::As), _) => () };
    let open_brace = select! { (Token::Open(Delimiter::Brace), _) => () };
    let close_brace = select! { (Token::Close(Delimiter::Brace), _) => () };
    let star = select! { (Token::Op(Op::Mul), _) => () };
    let comma = select! { (Token::Comma, _) => () };

    // FIXME: keywords should not be supprted here, we need to remove this hack and use other names for core modules
    let import_segment = identifier().or(select! {
        (Token::Keyword(Keyword::Int), _) => ast::Ident::new("int"),
        (Token::Keyword(Keyword::Float), _) => ast::Ident::new("float"),
        (Token::Keyword(Keyword::String), _) => ast::Ident::new("string"),
        (Token::Keyword(Keyword::Bool), _) => ast::Ident::new("bool"),
    });

    let dotted_path = import_segment
        .clone()
        .then(
            dot.ignore_then(import_segment)
                .repeated()
                .collect::<Vec<_>>(),
        )
        .map(|(first, mut rest)| {
            rest.insert(0, first);
            rest
        });

    let colon_target = identifier()
        .then_ignore(colon)
        .then(dotted_path.clone())
        .try_map(|(root, path), span| {
            colon_import_target(root, path).ok_or_else(|| {
                Rich::custom(
                    span,
                    "only pkg:, std:, and ext: import roots use colon syntax",
                )
            })
        });

    let local_target = empty()
        .to(0usize)
        .foldl(leading_dot.repeated(), |count, dot| count + dot)
        .then(dotted_path)
        .map(|(dot_count, path)| local_import_target(dot_count, path));

    let import_target = colon_target.or(local_target);

    let self_item = select! {
        (Token::Ident(i), _) if i.0.as_ref() == SELF_ITEM => ast::ImportItemKind::SelfModule
    };

    let import_item = self_item
        .or(identifier().map(ast::ImportItemKind::Name))
        .then(as_kw.ignore_then(identifier()).or_not())
        .map(|(kind, alias)| ast::ImportItem { kind, alias });

    let selective_items = import_item
        .separated_by(comma)
        .allow_trailing()
        .at_least(1)
        .collect::<Vec<_>>();

    let import_tail = choice((
        as_kw
            .ignore_then(identifier())
            .then_ignore(semicolon)
            .map(ast::ImportKind::ModuleAs),
        open_brace
            .ignore_then(choice((
                star.to(ast::ImportKind::Wildcard),
                selective_items.map(ast::ImportKind::Selective),
            )))
            .then_ignore(close_brace)
            .then_ignore(semicolon),
        semicolon.to(ast::ImportKind::Module),
    ));

    visibility()
        .then_ignore(import_kw)
        .then(import_target)
        .then(import_tail)
        .map_with(|((visibility, target), kind), e| {
            let s = e.span();
            let span = Span::new(s.start, s.end);
            let node = Spanned::new(
                ast::Import {
                    visibility,
                    target,
                    kind,
                },
                span,
            );
            Spanned::new(ast::Stmt::Import(node), span)
        })
        .labelled("import declaration")
        .as_context()
        .boxed()
}

pub(super) fn extern_declaration<'src>(
    stmt: impl AnvParser<'src, ast::StmtNode>,
) -> BoxedParser<'src, ast::StmtNode> {
    let semicolon = select! { (Token::Semicolon, _) => () };

    select! { (Token::Keyword(Keyword::Extern), _) => () }
        .ignore_then(choice((
            // extern fn...
            select! { (Token::Keyword(Keyword::Fn), _) => () }
                .ignore_then(identifier())
                .then(params(stmt.clone()))
                .then(return_type())
                .then_ignore(semicolon)
                .map_with(|((name, params), ret), e| {
                    let s = e.span();
                    let resolved_ret = ret.unwrap_or(ast::Type::Void);
                    let node = Spanned::new(
                        ast::ExternFunc {
                            annotations: vec![],
                            doc: None,
                            name,
                            params,
                            ret: resolved_ret,
                        },
                        Span::new(s.start, s.end),
                    );
                    let span = node.span;
                    Spanned::new(ast::Stmt::ExternFunc(node), span)
                }),
            extern_type_declaration(stmt),
        )))
        .labelled("extern declaration")
        .as_context()
        .boxed()
}

fn contextual_ident<'src>(word: &'static str) -> BoxedParser<'src, ()> {
    select! { (Token::Ident(ident), _) if ident.0.as_ref() == word => () }.boxed()
}

fn extern_type_rep<'src>() -> BoxedParser<'src, ast::ExternTypeRep> {
    contextual_ident("rep")
        .ignore_then(choice((
            contextual_ident("shared").to(ast::ExternTypeRep::Shared),
            contextual_ident("inline").to(ast::ExternTypeRep::Inline),
        )))
        .boxed()
}

fn extern_type_declaration<'src>(
    stmt: impl AnvParser<'src, ast::StmtNode>,
) -> BoxedParser<'src, ast::StmtNode> {
    let semicolon = select! { (Token::Semicolon, _) => () };

    select! { (Token::Keyword(Keyword::Type), _) => () }
        .ignore_then(identifier())
        .then(extern_type_rep().or_not())
        .then(choice((
            extern_type_body(stmt).map(Some),
            semicolon.map(|()| None),
        )))
        .map_with(|((name, rep), body), e| {
            let s = e.span();
            let (members, init) = body.unwrap_or((vec![], None));
            let self_type =
                ast::Type::nominal(ast::NominalKind::Extern, name, vec![], vec![], None);
            let empty_map = HashMap::new();
            let empty_const_map = HashMap::new();
            let init = resolve_extern_init(init, &empty_map, &empty_const_map, &self_type);
            let resolved_members =
                resolve_extern_members(members, &empty_map, &empty_const_map, &self_type);
            let node = Spanned::new(
                ast::ExternType {
                    annotations: vec![],
                    doc: None,
                    name,
                    rep: rep.unwrap_or(ast::ExternTypeRep::Shared),
                    init,
                    members: resolved_members,
                },
                Span::new(s.start, s.end),
            );
            let span = node.span;
            Spanned::new(ast::Stmt::ExternType(node), span)
        })
        .boxed()
}

fn resolve_extern_init(
    init: Option<ast::ExternInit>,
    type_param_map: &HashMap<ast::Ident, ast::TypeVarId>,
    const_param_map: &HashMap<ast::Ident, ast::ConstParamId>,
    self_type: &ast::Type,
) -> Option<ast::ExternInit> {
    init.map(|init| ast::ExternInit {
        params: resolve_extern_params(init.params, type_param_map, const_param_map, self_type),
    })
}

fn resolve_extern_params(
    params: Vec<ast::Param>,
    type_param_map: &HashMap<ast::Ident, ast::TypeVarId>,
    const_param_map: &HashMap<ast::Ident, ast::ConstParamId>,
    self_type: &ast::Type,
) -> Vec<ast::Param> {
    params
        .into_iter()
        .map(|p| ast::Param {
            mutability: p.mutability,
            name: p.name,
            ty: resolve_type_params_with_self(
                &p.ty,
                type_param_map,
                const_param_map,
                Some(self_type),
            ),
            default: p.default,
            cast_accept: p.cast_accept,
        })
        .collect()
}

fn resolve_extern_members(
    members: Vec<ast::ExternTypeMember>,
    type_param_map: &HashMap<ast::Ident, ast::TypeVarId>,
    const_param_map: &HashMap<ast::Ident, ast::ConstParamId>,
    self_type: &ast::Type,
) -> Vec<ast::ExternTypeMember> {
    members
        .into_iter()
        .map(|member| match member {
            ast::ExternTypeMember::Field {
                doc,
                name,
                ty,
                access,
                computed,
            } => ast::ExternTypeMember::Field {
                doc,
                name,
                ty: resolve_type_params_with_self(
                    &ty,
                    type_param_map,
                    const_param_map,
                    Some(self_type),
                ),
                access,
                computed,
            },
            ast::ExternTypeMember::Method {
                doc,
                name,
                receiver,
                params,
                ret,
            } => ast::ExternTypeMember::Method {
                doc,
                name,
                receiver,
                params: resolve_extern_params(params, type_param_map, const_param_map, self_type),
                ret: resolve_type_params_with_self(
                    &ret,
                    type_param_map,
                    const_param_map,
                    Some(self_type),
                ),
            },
            ast::ExternTypeMember::StaticMethod {
                doc,
                name,
                params,
                ret,
            } => ast::ExternTypeMember::StaticMethod {
                doc,
                name,
                params: resolve_extern_params(params, type_param_map, const_param_map, self_type),
                ret: resolve_type_params_with_self(
                    &ret,
                    type_param_map,
                    const_param_map,
                    Some(self_type),
                ),
            },
            ast::ExternTypeMember::Operator {
                op,
                other_ty,
                ret,
                self_on_right,
            } => ast::ExternTypeMember::Operator {
                op,
                other_ty: resolve_type_params_with_self(
                    &other_ty,
                    type_param_map,
                    const_param_map,
                    Some(self_type),
                ),
                ret: resolve_type_params_with_self(
                    &ret,
                    type_param_map,
                    const_param_map,
                    Some(self_type),
                ),
                self_on_right,
            },
            ast::ExternTypeMember::UnaryOperator { op, ret } => {
                ast::ExternTypeMember::UnaryOperator {
                    op,
                    ret: resolve_type_params_with_self(
                        &ret,
                        type_param_map,
                        const_param_map,
                        Some(self_type),
                    ),
                }
            }
        })
        .collect()
}

fn extern_type_body<'src>(
    stmt: impl AnvParser<'src, ast::StmtNode>,
) -> BoxedParser<'src, (Vec<ast::ExternTypeMember>, Option<ast::ExternInit>)> {
    enum BodyItem {
        Member(Box<ast::ExternTypeMember>),
        Init(ast::ExternInit),
    }

    let semicolon = select! { (Token::Semicolon, _) => () };
    let init_item = contextual_ident("init")
        .ignore_then(params(stmt.clone()).or_not())
        .then_ignore(semicolon)
        .map(|params| {
            BodyItem::Init(ast::ExternInit {
                params: params.unwrap_or_default(),
            })
        });
    let member_item = extern_type_member(stmt).map(Box::new).map(BodyItem::Member);

    select! { (Token::Open(Delimiter::Brace), _) => () }
        .ignore_then(
            choice((init_item, member_item))
                .repeated()
                .collect::<Vec<_>>(),
        )
        .then_ignore(select! { (Token::Close(Delimiter::Brace), _) => () })
        .validate(|items, extra, emitter| {
            let mut members = vec![];
            let mut init = None;
            for item in items {
                match item {
                    BodyItem::Member(member) => members.push(*member),
                    BodyItem::Init(next_init) => {
                        if init.is_some() {
                            emitter.emit(Rich::custom(
                                extra.span(),
                                "duplicate 'init' in extern type body",
                            ));
                        } else {
                            init = Some(next_init);
                        }
                    }
                }
            }
            (members, init)
        })
        .boxed()
}

fn extern_type_member<'src>(
    stmt: impl AnvParser<'src, ast::StmtNode>,
) -> BoxedParser<'src, ast::ExternTypeMember> {
    let semicolon = select! { (Token::Semicolon, _) => () };

    choice((
        extern_type_op_member().then_ignore(semicolon),
        extern_type_method_member(stmt).then_ignore(semicolon),
        extern_type_field_member().then_ignore(semicolon),
    ))
    .boxed()
}

fn is_self_type(ty: &ast::Type) -> bool {
    bare_type_name(ty).is_some_and(|name| name.0.as_ref() == SELF_TYPE)
}

fn extern_type_op_member<'src>() -> BoxedParser<'src, ast::ExternTypeMember> {
    let op_kw = select! { (Token::Ident(ident), _) if ident.0.as_ref() == "op" => () };
    let arrow = select! { (Token::Op(Op::ThinArrow), _) => () };

    let binary_op_tok = select! {
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
    };

    let unary = select! { (Token::Op(Op::Sub), _) => () }
        .ignore_then(type_ident())
        .then_ignore(arrow)
        .then(type_ident())
        .validate(|(operand, ret), extra, emitter| {
            if !is_self_type(&operand) {
                emitter.emit(Rich::custom(extra.span(), "unary operand must be 'Self'"));
            }
            ast::ExternTypeMember::UnaryOperator {
                op: ast::UnaryOp::Neg,
                ret,
            }
        });

    let binary = type_ident()
        .then(binary_op_tok)
        .then(type_ident())
        .then_ignore(arrow)
        .then(type_ident())
        .validate(|(((lhs, op), rhs), ret), extra, emitter| {
            let (other_ty, self_on_right) = if is_self_type(&lhs) {
                (rhs, false)
            } else if is_self_type(&rhs) {
                (lhs, true)
            } else {
                emitter.emit(Rich::custom(extra.span(), "one operand must be 'Self'"));
                (lhs, false)
            };
            ast::ExternTypeMember::Operator {
                op,
                other_ty,
                ret,
                self_on_right,
            }
        });

    op_kw.ignore_then(choice((unary, binary))).boxed()
}

fn extern_type_field_member<'src>() -> BoxedParser<'src, ast::ExternTypeMember> {
    let var_kw = select! { (Token::Keyword(Keyword::Var), _) => () };
    let let_kw = select! { (Token::Keyword(Keyword::Let), _) => () };

    let direct_access = choice((
        var_kw.to(ast::ExternFieldAccess::ReadWrite),
        let_kw.to(ast::ExternFieldAccess::ReadOnly),
    ))
    .or_not()
    .map(|access| access.unwrap_or(ast::ExternFieldAccess::ReadWrite));

    let computed_access = contextual_ident("computed")
        .ignore_then(var_kw.or_not())
        .map(|var_opt| {
            if var_opt.is_some() {
                ast::ExternFieldAccess::ReadWrite
            } else {
                ast::ExternFieldAccess::ReadOnly
            }
        });

    doc_comment_block()
        .then(choice((
            computed_access.map(|access| (access, true)),
            direct_access.map(|access| (access, false)),
        )))
        .then(identifier())
        .then_ignore(select! { (Token::Colon, _) => () })
        .then(type_ident())
        .map(
            |(((doc, (access, computed)), name), ty)| ast::ExternTypeMember::Field {
                doc,
                name,
                ty,
                access,
                computed,
            },
        )
        .boxed()
}

fn extern_type_method_member<'src>(
    stmt: impl AnvParser<'src, ast::StmtNode>,
) -> BoxedParser<'src, ast::ExternTypeMember> {
    doc_comment_block()
        .then(
            select! { (Token::Keyword(Keyword::Fn), _) => () }
                .ignore_then(field_name_ident())
                .then(extern_method_params(stmt))
                .then(return_type()),
        )
        .map(|(doc, ((name, (receiver, params)), ret))| {
            let ret = ret.unwrap_or(ast::Type::Void);
            match receiver {
                Some(receiver) => ast::ExternTypeMember::Method {
                    doc,
                    name,
                    receiver,
                    params,
                    ret,
                },
                None => ast::ExternTypeMember::StaticMethod {
                    doc,
                    name,
                    params,
                    ret,
                },
            }
        })
        .boxed()
}

fn extern_method_params<'src>(
    stmt: impl AnvParser<'src, ast::StmtNode>,
) -> BoxedParser<'src, (Option<ast::ExternReceiverMode>, Vec<ast::Param>)> {
    select! { (Token::Open(Delimiter::Parent), _) => () }
        .ignore_then(
            extern_method_param_list(stmt)
                .or_not()
                .map(Option::unwrap_or_default),
        )
        .then_ignore(select! { (Token::Close(Delimiter::Parent), _) => () })
        .boxed()
}

fn extern_self_param<'src>() -> BoxedParser<'src, ast::ExternReceiverMode> {
    let value_self = self_ident().to(ast::ExternReceiverMode::Value);
    let shared_self = contextual_ident("shared")
        .ignore_then(self_ident())
        .to(ast::ExternReceiverMode::Shared);
    let mutable_self = select! { (Token::Keyword(Keyword::Var), _) => () }
        .ignore_then(self_ident())
        .to(ast::ExternReceiverMode::Mutable);

    choice((shared_self, mutable_self, value_self)).boxed()
}

fn extern_method_param_list<'src>(
    stmt: impl AnvParser<'src, ast::StmtNode>,
) -> BoxedParser<'src, (Option<ast::ExternReceiverMode>, Vec<ast::Param>)> {
    let regular_params = param(stmt)
        .separated_by(select! { (Token::Comma, _) => () })
        .allow_trailing()
        .collect::<Vec<_>>()
        .validate(|params, extra, emitter| {
            if params.iter().any(|param| param.name.0.as_ref() == SELF_ITEM) {
                emitter.emit(Rich::custom(
                    extra.span(),
                    "extern method receiver must be 'self', 'shared self', or 'var self' without a type annotation",
                ));
            }
            params
        });

    choice((
        extern_self_param()
            .then(
                select! { (Token::Comma, _) => () }
                    .ignore_then(
                        regular_params
                            .clone()
                            .or_not()
                            .map(Option::unwrap_or_default),
                    )
                    .or_not()
                    .map(Option::unwrap_or_default),
            )
            .map(|(receiver, params)| (Some(receiver), params)),
        regular_params.map(|params| (None, params)),
    ))
    .boxed()
}

fn visibility<'src>() -> BoxedParser<'src, ast::Visibility> {
    select! {
        (Token::Keyword(Keyword::Pub), _) => ast::Visibility::Public,
    }
    .or_not()
    .map(|v| v.unwrap_or(ast::Visibility::Private))
    .boxed()
}

pub(super) fn function<'src>(
    stmt: impl AnvParser<'src, ast::StmtNode>,
) -> BoxedParser<'src, ast::FuncNode> {
    let tail_expr = expression(stmt.clone());
    visibility()
        .then_ignore(select! {
            (Token::Keyword(Keyword::Fn), _) => (),
        })
        .then(identifier())
        .then(generic_params())
        .then(params(stmt.clone()))
        .then(return_type())
        .then(block_stmt(stmt, tail_expr))
        .map_with(|(((((vis, name), gp), params), ret), body), e| {
            let s = e.span();
            let GenericParams {
                type_params,
                const_params,
            } = gp;
            let type_param_map: HashMap<ast::Ident, ast::TypeVarId> =
                type_params.iter().map(|tp| (tp.name, tp.id)).collect();
            let const_param_map: HashMap<ast::Ident, ast::ConstParamId> =
                const_params.iter().map(|cp| (cp.name, cp.id)).collect();

            let resolved_params = params
                .into_iter()
                .map(|p| {
                    let ty = resolve_type_params(&p.ty, &type_param_map, &const_param_map);
                    ast::Param {
                        mutability: p.mutability,
                        name: p.name,
                        ty,
                        default: p.default,
                        cast_accept: p.cast_accept,
                    }
                })
                .collect();

            let resolved_ret = match ret {
                Some(ty) => resolve_type_params(&ty, &type_param_map, &const_param_map),
                None => ast::Type::Void,
            };

            Spanned::new(
                ast::Func {
                    annotations: vec![],
                    doc: None,
                    name,
                    visibility: vis,
                    type_params,
                    const_params,
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

fn struct_field<'src>(
    stmt: impl AnvParser<'src, ast::StmtNode>,
) -> BoxedParser<'src, ast::StructField> {
    doc_comment_block()
        .then(annotations())
        .then(identifier())
        .then_ignore(select! {
            (Token::Colon, _) => (),
        })
        .then(type_ident())
        .then(
            select! { (Token::Op(Op::Assign), _) => () }
                .ignore_then(expression(stmt))
                .or_not(),
        )
        .map(
            |((((doc, annotations), name), ty), default)| ast::StructField {
                annotations,
                name,
                ty,
                default,
                doc,
            },
        )
        .labelled("struct field")
        .as_context()
        .boxed()
}

#[derive(Clone, Copy)]
enum MethodSigPolicy {
    Aggregate,
    Extend,
}

fn struct_method<'src>(
    stmt: impl AnvParser<'src, ast::StmtNode>,
) -> BoxedParser<'src, ast::Method> {
    let tail_expr = expression(stmt.clone());
    annotations()
        .then(doc_comment_block())
        .then(method_sig(stmt.clone(), MethodSigPolicy::Aggregate))
        .then(block_stmt(stmt, tail_expr))
        .map_with(|(((annots, doc), sig), body), e| {
            let s = e.span();
            ast::Method {
                annotations: annots,
                doc,
                visibility: ast::Visibility::Private,
                sig,
                body: Spanned::new(body.node, Span::new(s.start, s.end)),
            }
        })
        .labelled("method")
        .as_context()
        .boxed()
}

fn method_sig<'src>(
    stmt: impl AnvParser<'src, ast::StmtNode>,
    policy: MethodSigPolicy,
) -> BoxedParser<'src, ast::MethodSig> {
    let name = match policy {
        MethodSigPolicy::Aggregate => identifier().boxed(),
        MethodSigPolicy::Extend => field_name_ident().boxed(),
    };

    select! { (Token::Keyword(Keyword::Fn), _) => () }
        .ignore_then(name)
        .then(generic_params())
        .then(method_params(stmt))
        .then(return_type())
        .validate(
            move |(((name, gp), (receiver, params)), ret), extra, emitter| {
                let GenericParams {
                    type_params,
                    const_params,
                } = gp;
                if matches!(policy, MethodSigPolicy::Extend) {
                    if receiver.is_none() {
                        emitter.emit(Rich::custom(
                            extra.span(),
                            "extend methods must have 'self' or 'var self' receiver",
                        ));
                    }
                    if !type_params.is_empty() || !const_params.is_empty() {
                        emitter.emit(Rich::custom(
                            extra.span(),
                            "extend methods cannot declare method-level generics yet",
                        ));
                    }
                }
                ast::MethodSig {
                    name,
                    type_params,
                    const_params,
                    receiver,
                    params,
                    ret: ret.unwrap_or(ast::Type::Void),
                }
            },
        )
        .boxed()
}

fn method_params<'src>(
    stmt: impl AnvParser<'src, ast::StmtNode>,
) -> BoxedParser<'src, (Option<ast::MethodReceiver>, Vec<ast::Param>)> {
    select! {
        (Token::Open(Delimiter::Parent), _) => (),
    }
    .ignore_then(
        method_param_list(stmt)
            .or_not()
            .map(Option::unwrap_or_default),
    )
    .then_ignore(select! {
        (Token::Close(Delimiter::Parent), _) => (),
    })
    .boxed()
}

fn method_self_param<'src>() -> BoxedParser<'src, ast::MethodReceiver> {
    let value_self = self_ident().to(ast::MethodReceiver::Value);
    let mutable_self = select! { (Token::Keyword(Keyword::Var), _) => () }
        .ignore_then(self_ident())
        .to(ast::MethodReceiver::Var);

    choice((mutable_self, value_self))
        .then(
            select! { (Token::Colon, _) => () }
                .ignore_then(type_ident())
                .or_not(),
        )
        .validate(|(receiver, annotation), extra, emitter| {
            if annotation.is_some() {
                emitter.emit(Rich::custom(
                    extra.span(),
                    "method receiver must not have a type annotation",
                ));
            }
            receiver
        })
        .boxed()
}

fn self_ident<'src>() -> BoxedParser<'src, ()> {
    identifier()
        .try_map(|ident, span| {
            if ident.0.as_ref() == SELF_ITEM {
                Ok(())
            } else {
                Err(Rich::custom(span, "expected 'self'"))
            }
        })
        .boxed()
}

fn method_param_list<'src>(
    stmt: impl AnvParser<'src, ast::StmtNode>,
) -> BoxedParser<'src, (Option<ast::MethodReceiver>, Vec<ast::Param>)> {
    let regular_params = param(stmt)
        .separated_by(select! { (Token::Comma, _) => () })
        .allow_trailing()
        .collect::<Vec<_>>()
        .validate(|params, extra, emitter| {
            if params.iter().any(|param| param.name.0.as_ref() == SELF_ITEM) {
                emitter.emit(Rich::custom(
                    extra.span(),
                    "method receiver must be first and must be 'self' or 'var self' without a type annotation",
                ));
            }
            params
        });

    choice((
        method_self_param()
            .then(
                select! { (Token::Comma, _) => () }
                    .ignore_then(
                        regular_params
                            .clone()
                            .or_not()
                            .map(Option::unwrap_or_default),
                    )
                    .or_not()
                    .map(Option::unwrap_or_default),
            )
            .map(|(receiver, params)| (Some(receiver), params)),
        regular_params.map(|params| (None, params)),
    ))
    .boxed()
}

fn aggregate_declaration<'src>(
    stmt: impl AnvParser<'src, ast::StmtNode>,
    kind: ast::AggregateKind,
) -> BoxedParser<'src, ast::AggregateDeclNode> {
    visibility()
        .then_ignore(any().filter(move |(t, _)| match kind {
            ast::AggregateKind::Struct => matches!(t, Token::Keyword(Keyword::Struct)),
            ast::AggregateKind::DataRef => matches!(t, Token::Keyword(Keyword::DataRef)),
        }))
        .then(identifier())
        .then(generic_params())
        .then(
            select! {
                (Token::Open(Delimiter::Brace), _) => (),
            }
            .ignore_then(
                struct_field(stmt.clone())
                    .separated_by(select! { (Token::Comma, _) => () })
                    .allow_trailing()
                    .collect::<Vec<_>>(),
            )
            .then(struct_method(stmt).repeated().collect::<Vec<_>>())
            .then_ignore(select! {
                (Token::Close(Delimiter::Brace), _) => (),
            }),
        )
        .map_with(move |(((vis, name), gp), (raw_fields, raw_methods)), e| {
            let s = e.span();
            let GenericParams {
                type_params,
                const_params,
            } = gp;

            let struct_type_param_map: HashMap<ast::Ident, ast::TypeVarId> =
                type_params.iter().map(|tp| (tp.name, tp.id)).collect();
            let struct_const_param_map: HashMap<ast::Ident, ast::ConstParamId> =
                const_params.iter().map(|cp| (cp.name, cp.id)).collect();

            let self_type = kind.make_type(
                name,
                type_params.iter().map(|tp| ast::Type::Var(tp.id)).collect(),
                const_params
                    .iter()
                    .map(|cp| ast::ConstArg::Param(cp.id))
                    .collect(),
                None,
            );

            let fields = raw_fields
                .into_iter()
                .map(|f| {
                    let ty = resolve_type_params_with_self(
                        &f.ty,
                        &struct_type_param_map,
                        &struct_const_param_map,
                        Some(&self_type),
                    );
                    ast::StructField {
                        annotations: f.annotations,
                        name: f.name,
                        ty,
                        default: f.default,
                        doc: f.doc,
                    }
                })
                .collect();

            let methods = raw_methods
                .into_iter()
                .map(|m| {
                    let mut combined_type_param_map = struct_type_param_map.clone();
                    let mut combined_const_param_map = struct_const_param_map.clone();
                    for tp in &m.sig.type_params {
                        combined_type_param_map.insert(tp.name, tp.id);
                    }
                    for cp in &m.sig.const_params {
                        combined_const_param_map.insert(cp.name, cp.id);
                    }

                    let resolved_params = m
                        .sig
                        .params
                        .iter()
                        .map(|p| ast::Param {
                            mutability: p.mutability,
                            name: p.name,
                            ty: resolve_type_params_with_self(
                                &p.ty,
                                &combined_type_param_map,
                                &combined_const_param_map,
                                Some(&self_type),
                            ),
                            default: p.default.clone(),
                            cast_accept: p.cast_accept,
                        })
                        .collect();

                    let resolved_ret = resolve_type_params_with_self(
                        &m.sig.ret,
                        &combined_type_param_map,
                        &combined_const_param_map,
                        Some(&self_type),
                    );

                    ast::Method {
                        annotations: m.annotations,
                        doc: m.doc,
                        visibility: m.visibility,
                        sig: ast::MethodSig {
                            name: m.sig.name,
                            type_params: m.sig.type_params,
                            const_params: m.sig.const_params,
                            receiver: m.sig.receiver,
                            params: resolved_params,
                            ret: resolved_ret,
                        },
                        body: m.body,
                    }
                })
                .collect();

            Spanned::new(
                ast::StructDecl {
                    kind,
                    annotations: vec![],
                    doc: None,
                    name,
                    visibility: vis,
                    type_params,
                    const_params,
                    fields,
                    methods,
                },
                Span::new(s.start, s.end),
            )
        })
        .labelled(match kind {
            ast::AggregateKind::Struct => "struct declaration",
            ast::AggregateKind::DataRef => "dataref declaration",
        })
        .as_context()
        .boxed()
}

pub(super) fn struct_declaration<'src>(
    stmt: impl AnvParser<'src, ast::StmtNode>,
) -> BoxedParser<'src, ast::AggregateDeclNode> {
    aggregate_declaration(stmt, ast::AggregateKind::Struct)
}

pub(super) fn dataref_declaration<'src>(
    stmt: impl AnvParser<'src, ast::StmtNode>,
) -> BoxedParser<'src, ast::AggregateDeclNode> {
    aggregate_declaration(stmt, ast::AggregateKind::DataRef)
}

fn enum_variant_tuple_payload<'src>() -> BoxedParser<'src, ast::VariantKind> {
    select! { (Token::Open(Delimiter::Parent), _) => () }
        .ignore_then(
            type_ident()
                .separated_by(select! { (Token::Comma, _) => () })
                .allow_trailing()
                .collect::<Vec<_>>(),
        )
        .then_ignore(select! { (Token::Close(Delimiter::Parent), _) => () })
        .map(ast::VariantKind::Tuple)
        .boxed()
}

fn enum_variant_struct_payload<'src>(
    stmt: impl AnvParser<'src, ast::StmtNode>,
) -> BoxedParser<'src, ast::VariantKind> {
    select! { (Token::Open(Delimiter::Brace), _) => () }
        .ignore_then(
            struct_field(stmt)
                .separated_by(select! { (Token::Comma, _) => () })
                .allow_trailing()
                .collect::<Vec<_>>(),
        )
        .then_ignore(select! { (Token::Close(Delimiter::Brace), _) => () })
        .map(ast::VariantKind::Struct)
        .boxed()
}

fn enum_variant<'src>(
    stmt: impl AnvParser<'src, ast::StmtNode>,
) -> BoxedParser<'src, ast::EnumVariant> {
    doc_comment_block()
        .then(annotations())
        .then(identifier())
        .then(choice((
            enum_variant_tuple_payload(),
            enum_variant_struct_payload(stmt),
            empty().to(ast::VariantKind::Unit),
        )))
        .map(|(((doc, annotations), name), kind)| ast::EnumVariant {
            annotations,
            name,
            kind,
            doc,
        })
        .labelled("enum variant")
        .as_context()
        .boxed()
}

pub(super) fn enum_declaration<'src>(
    stmt: impl AnvParser<'src, ast::StmtNode>,
) -> BoxedParser<'src, ast::EnumDeclNode> {
    visibility()
        .then_ignore(select! { (Token::Keyword(Keyword::Enum), _) => () })
        .then(identifier())
        .then(generic_params())
        .then(
            select! { (Token::Open(Delimiter::Brace), _) => () }
                .ignore_then(
                    enum_variant(stmt)
                        .separated_by(select! { (Token::Comma, _) => () })
                        .allow_trailing()
                        .collect::<Vec<_>>(),
                )
                .then_ignore(select! { (Token::Close(Delimiter::Brace), _) => () }),
        )
        .map_with(|(((vis, name), gp), variants), e| {
            let s = e.span();
            let GenericParams {
                type_params,
                const_params,
            } = gp;

            let type_param_map: HashMap<ast::Ident, ast::TypeVarId> =
                type_params.iter().map(|tp| (tp.name, tp.id)).collect();
            let const_param_map: HashMap<ast::Ident, ast::ConstParamId> =
                const_params.iter().map(|cp| (cp.name, cp.id)).collect();

            let resolved_variants = variants
                .into_iter()
                .map(|v| {
                    let resolved_kind = match v.kind {
                        ast::VariantKind::Unit => ast::VariantKind::Unit,
                        ast::VariantKind::Tuple(types) => {
                            let resolved = types
                                .iter()
                                .map(|ty| {
                                    resolve_type_params(ty, &type_param_map, &const_param_map)
                                })
                                .collect();
                            ast::VariantKind::Tuple(resolved)
                        }
                        ast::VariantKind::Struct(fields) => {
                            let resolved = fields
                                .iter()
                                .map(|f| ast::StructField {
                                    annotations: f.annotations.clone(),
                                    name: f.name,
                                    ty: resolve_type_params(
                                        &f.ty,
                                        &type_param_map,
                                        &const_param_map,
                                    ),
                                    default: None,
                                    doc: f.doc.clone(),
                                })
                                .collect();
                            ast::VariantKind::Struct(resolved)
                        }
                    };
                    ast::EnumVariant {
                        annotations: v.annotations,
                        name: v.name,
                        kind: resolved_kind,
                        doc: v.doc,
                    }
                })
                .collect();

            Spanned::new(
                ast::EnumDecl {
                    annotations: vec![],
                    doc: None,
                    name,
                    visibility: vis,
                    type_params,
                    const_params,
                    variants: resolved_variants,
                },
                Span::new(s.start, s.end),
            )
        })
        .labelled("enum declaration")
        .as_context()
        .boxed()
}

fn extend_method<'src>(
    stmt: impl AnvParser<'src, ast::StmtNode>,
) -> BoxedParser<'src, ast::ExtendMethodNode> {
    let tail_expr = expression(stmt.clone());
    annotations()
        .then(doc_comment_block())
        .then(method_sig(stmt.clone(), MethodSigPolicy::Extend))
        .then(block_stmt(stmt, tail_expr))
        .map_with(|(((annots, doc), sig), body), e| {
            let s = e.span();
            Spanned::new(
                ast::ExtendMethod {
                    annotations: annots,
                    doc,
                    sig,
                    body: Spanned::new(body.node, Span::new(s.start, s.end)),
                },
                Span::new(s.start, s.end),
            )
        })
        .labelled("extend method")
        .as_context()
        .boxed()
}

fn cast_from_decl<'src>(
    stmt: impl AnvParser<'src, ast::StmtNode>,
) -> BoxedParser<'src, ast::CastFromNode> {
    let tail_expr = expression(stmt.clone());
    let cast_kw = select! { (Token::Ident(id), _) if id.0.as_ref() == "cast" => () };
    let from_kw = select! { (Token::Ident(id), _) if id.0.as_ref() == "from" => () };

    cast_kw
        .ignore_then(from_kw)
        .ignore_then(params(stmt.clone()))
        .then(return_type())
        .then(block_stmt(stmt, tail_expr))
        .validate(|((param_list, ret), body), extra, emitter| {
            let s = extra.span();
            if param_list.len() != 1 {
                emitter.emit(Rich::custom(s, "cast from requires exactly one parameter"));
            }
            let param = param_list.into_iter().next().unwrap_or_else(|| ast::Param {
                mutability: ast::Mutability::Immutable,
                name: ast::Ident(internment::Intern::new("_".to_string())),
                ty: ast::Type::Infer,
                default: None,
                cast_accept: false,
            });
            if param.ty == ast::Type::Infer {
                emitter.emit(Rich::custom(
                    s,
                    "cast from parameter must have an explicit type annotation",
                ));
            }
            if param.default.is_some() {
                emitter.emit(Rich::custom(
                    s,
                    "cast from parameter cannot have a default value",
                ));
            }
            if param.cast_accept {
                emitter.emit(Rich::custom(
                    s,
                    "cast from parameter cannot use the `as` modifier",
                ));
            }
            Spanned::new(
                ast::CastFrom {
                    param,
                    ret,
                    body: Spanned::new(body.node, Span::new(s.start, s.end)),
                },
                Span::new(s.start, s.end),
            )
        })
        .labelled("cast from declaration")
        .as_context()
        .boxed()
}

pub(super) fn extend_declaration<'src>(
    stmt: impl AnvParser<'src, ast::StmtNode>,
) -> BoxedParser<'src, ast::ExtendDeclNode> {
    enum ExtendMember {
        Method(Box<ast::ExtendMethodNode>),
        CastFrom(Box<ast::CastFromNode>),
    }

    let extend_head = choice((
        // extend<T, ...> type_expr, explicit type params followed by any type expression
        required_generic_params()
            .then(type_ident())
            .map(|(gp, ty)| (ty, gp)),
        // dataref keyword followed by identifier targets that dataref type
        select! { (Token::Keyword(Keyword::DataRef), _) => () }
            .ignore_then(identifier())
            .then(generic_params())
            .map(|(name, gp)| {
                (
                    ast::Type::nominal(ast::NominalKind::DataRef, name, vec![], vec![], None),
                    gp,
                )
            }),
        // target type expression without extend params
        type_ident().map(|ty| (ty, GenericParams::default())),
    ));

    visibility()
        .then_ignore(select! {
            (Token::Keyword(Keyword::Extend), _) => (),
        })
        .then(extend_head)
        .then(
            select! { (Token::Open(Delimiter::Brace), _) => () }
                .ignore_then(
                    choice((
                        cast_from_decl(stmt.clone()).map(|cf| ExtendMember::CastFrom(Box::new(cf))),
                        extend_method(stmt).map(|method| ExtendMember::Method(Box::new(method))),
                    ))
                    .repeated()
                    .collect::<Vec<_>>(),
                )
                .then_ignore(select! { (Token::Close(Delimiter::Brace), _) => () }),
        )
        .map_with(|((vis, (ty, gp)), members), e| {
            let s = e.span();
            let GenericParams {
                type_params,
                const_params,
            } = gp;
            let mut methods = vec![];
            let mut cast_froms = vec![];
            for m in members {
                match m {
                    ExtendMember::Method(method) => methods.push(*method),
                    ExtendMember::CastFrom(cf) => cast_froms.push(*cf),
                }
            }
            Spanned::new(
                ast::ExtendDecl {
                    visibility: vis,
                    ty,
                    type_params,
                    const_params,
                    methods,
                    cast_froms,
                },
                Span::new(s.start, s.end),
            )
        })
        .labelled("extend declaration")
        .as_context()
        .boxed()
}

pub(super) fn const_decl<'src>(
    stmt: impl AnvParser<'src, ast::StmtNode>,
) -> BoxedParser<'src, ast::StmtNode> {
    visibility()
        .then_ignore(select! {
            (Token::Keyword(Keyword::Const), _) => (),
        })
        .then(identifier())
        .then(
            select! { (Token::Colon, _) => () }
                .ignore_then(type_ident())
                .or_not(),
        )
        .then_ignore(select! { (Token::Op(Op::Assign), _) => () })
        .then(expression(stmt))
        .then_ignore(select! { (Token::Semicolon, _) => () })
        .map_with(|(((vis, name), ty), value), e| {
            let s = e.span();
            let span = Span::new(s.start, s.end);
            let node = Spanned::new(
                ast::ConstDecl {
                    annotations: vec![],
                    doc: None,
                    name,
                    ty,
                    value,
                    visibility: vis,
                },
                span,
            );
            Spanned::new(ast::Stmt::Const(node), span)
        })
        .labelled("const declaration")
        .as_context()
        .boxed()
}

fn resolve_const_arg(
    arg: &ast::ConstArg,
    const_param_map: &HashMap<ast::Ident, ast::ConstParamId>,
) -> ast::ConstArg {
    match arg {
        ast::ConstArg::Name(name) => const_param_map
            .get(name)
            .map_or_else(|| arg.clone(), |id| ast::ConstArg::Param(*id)),
        ast::ConstArg::Value(_) | ast::ConstArg::Param(_) => arg.clone(),
    }
}

fn bare_type_name(ty: &ast::Type) -> Option<ast::Ident> {
    match ty {
        ast::Type::UnresolvedName(name) => Some(*name),
        ast::Type::UnresolvedNominal {
            qualifier: None,
            name,
            generic_args,
        } if generic_args.is_empty() => Some(*name),
        _ => None,
    }
}

fn resolve_generic_args(
    args: &[ast::GenericArg],
    type_param_map: &HashMap<ast::Ident, ast::TypeVarId>,
    const_param_map: &HashMap<ast::Ident, ast::ConstParamId>,
    self_type: Option<&ast::Type>,
) -> Vec<ast::GenericArg> {
    args.iter()
        .map(|arg| match arg {
            ast::GenericArg::Type(ty) => {
                match bare_type_name(ty).and_then(|name| const_param_map.get(&name).copied()) {
                    Some(id) => ast::GenericArg::Const(ast::ConstArg::Param(id)),
                    None => ast::GenericArg::Type(resolve_type_params_with_self(
                        ty,
                        type_param_map,
                        const_param_map,
                        self_type,
                    )),
                }
            }
            ast::GenericArg::Const(arg) => {
                ast::GenericArg::Const(resolve_const_arg(arg, const_param_map))
            }
        })
        .collect()
}

fn resolve_split_generic_args(
    type_args: &[ast::Type],
    const_args: &[ast::ConstArg],
    type_param_map: &HashMap<ast::Ident, ast::TypeVarId>,
    const_param_map: &HashMap<ast::Ident, ast::ConstParamId>,
    self_type: Option<&ast::Type>,
) -> (Vec<ast::Type>, Vec<ast::ConstArg>) {
    let generic_args = type_args
        .iter()
        .cloned()
        .map(ast::GenericArg::Type)
        .chain(const_args.iter().cloned().map(ast::GenericArg::Const))
        .collect::<Vec<_>>();
    let mut resolved_types = vec![];
    let mut resolved_consts = vec![];
    for arg in resolve_generic_args(&generic_args, type_param_map, const_param_map, self_type) {
        match arg {
            ast::GenericArg::Type(ty) => resolved_types.push(ty),
            ast::GenericArg::Const(arg) => resolved_consts.push(arg),
        }
    }
    (resolved_types, resolved_consts)
}

fn resolve_type_params_with_self(
    ty: &ast::Type,
    type_param_map: &HashMap<ast::Ident, ast::TypeVarId>,
    const_param_map: &HashMap<ast::Ident, ast::ConstParamId>,
    self_type: Option<&ast::Type>,
) -> ast::Type {
    use ast::Type::{
        Array, Func, List, Map, Nominal, Slice, Tuple, UnresolvedName, UnresolvedNominal, Var,
    };
    match ty {
        UnresolvedName(ident) => {
            if let Some(id) = type_param_map.get(ident) {
                return Var(*id);
            }
            if let Some(st) = self_type
                && ident.0.as_ref() == SELF_TYPE
            {
                return st.clone();
            }
            ty.clone()
        }
        UnresolvedNominal {
            qualifier,
            name,
            generic_args,
        } => {
            let can_resolve_bare_name = qualifier.is_none() && generic_args.is_empty();
            let type_param = can_resolve_bare_name
                .then(|| type_param_map.get(name))
                .flatten();
            if let Some(id) = type_param {
                return Var(*id);
            }
            let is_self_type = can_resolve_bare_name && name.0.as_ref() == SELF_TYPE;
            if let (true, Some(st)) = (is_self_type, self_type) {
                return st.clone();
            }
            UnresolvedNominal {
                qualifier: *qualifier,
                name: *name,
                generic_args: resolve_generic_args(
                    generic_args,
                    type_param_map,
                    const_param_map,
                    self_type,
                ),
            }
        }

        Nominal(nominal) => {
            if nominal.kind == ast::NominalKind::Extern {
                debug_assert!(nominal.type_args.is_empty());
                debug_assert!(nominal.const_args.is_empty());
            }
            let (type_args, const_args) = resolve_split_generic_args(
                &nominal.type_args,
                &nominal.const_args,
                type_param_map,
                const_param_map,
                self_type,
            );
            ast::Type::nominal_with_origin(
                nominal.kind,
                nominal.name,
                type_args,
                const_args,
                nominal.origin.clone(),
            )
        }

        Func { params, ret } => {
            let resolved_params = params
                .iter()
                .map(|p| {
                    ast::FuncParam::new(
                        resolve_type_params_with_self(
                            &p.ty,
                            type_param_map,
                            const_param_map,
                            self_type,
                        ),
                        p.mutable,
                    )
                })
                .collect::<Vec<_>>();
            Func {
                params: resolved_params,
                ret: resolve_type_params_with_self(ret, type_param_map, const_param_map, self_type)
                    .boxed(),
            }
        }

        Tuple(elements) => {
            let resolved_elements = elements
                .iter()
                .map(|el| {
                    resolve_type_params_with_self(el, type_param_map, const_param_map, self_type)
                })
                .collect::<Vec<_>>();
            Tuple(resolved_elements)
        }
        Array { elem, len } => {
            let resolved_len = match len {
                ast::ArrayLen::Named(ident) => {
                    if let Some(&id) = const_param_map.get(ident) {
                        ast::ArrayLen::Param(id)
                    } else {
                        *len
                    }
                }
                _ => *len,
            };
            Array {
                elem: resolve_type_params_with_self(
                    elem,
                    type_param_map,
                    const_param_map,
                    self_type,
                )
                .boxed(),
                len: resolved_len,
            }
        }

        Slice { elem } => Slice {
            elem: resolve_type_params_with_self(elem, type_param_map, const_param_map, self_type)
                .boxed(),
        },

        List { elem } => List {
            elem: resolve_type_params_with_self(elem, type_param_map, const_param_map, self_type)
                .boxed(),
        },

        Map { key, value } => Map {
            key: resolve_type_params_with_self(key, type_param_map, const_param_map, self_type)
                .boxed(),
            value: resolve_type_params_with_self(value, type_param_map, const_param_map, self_type)
                .boxed(),
        },

        _ => ty.clone(),
    }
}

fn resolve_type_params(
    ty: &ast::Type,
    type_param_map: &HashMap<ast::Ident, ast::TypeVarId>,
    const_param_map: &HashMap<ast::Ident, ast::ConstParamId>,
) -> ast::Type {
    resolve_type_params_with_self(ty, type_param_map, const_param_map, None)
}
