use chumsky::{error::Rich, prelude::*};

use super::{
    AnvParser, BoxedParser,
    common::{
        TupleShapeResult, block_stmt, braces, brackets, callable_param_type, colon, comma_list,
        field_name_ident, identifier, literal, nonempty_comma_list, parens,
        parenthesized_tuple_parts, qualified_name, return_spec, validate_tuple_shape_raw,
    },
    new_expr_id,
    ops::{
        add_sub_op, and_op, assign_op, bit_and_op, bit_or_op, cmp_op, coalesce_op, eq_op,
        infix_left, mul_div_op, or_op, shift_op, xor_op,
    },
    pattern::{conditional_pattern, pattern},
    types::{generic_arg, type_ident, type_subject_type_ident},
};
use crate::{
    ast,
    lexer::{Delimiter, InterpToken, Keyword, LitToken, Op, Token},
    span::Spanned,
};

pub(super) fn expression<'src>(
    stmt: impl AnvParser<'src, ast::StmtNode>,
) -> BoxedParser<'src, ast::ExprNode> {
    recursive(|expr| {
        let atom = atom_expr(stmt, expr.clone());
        let postfix = postfix_expr(atom, expr.clone());
        let unary = unary_expr(postfix.clone());
        let cast = cast_expr(unary);
        let binary = binary_expr(cast);
        let ternary = ternary_expr(binary, expr.clone());
        assignment_expr(ternary, postfix)
    })
    .boxed()
}

pub(super) fn for_header_expression<'src>(
    stmt: impl AnvParser<'src, ast::StmtNode>,
) -> BoxedParser<'src, ast::ExprNode> {
    let full_expr = expression(stmt.clone());
    let atom = for_header_atom_expr(stmt, full_expr.clone());
    let postfix = postfix_expr(atom, full_expr.clone());
    let unary = unary_expr(postfix.clone());
    let cast = cast_expr(unary);
    let binary = binary_expr(cast);
    let ternary = ternary_expr(binary, full_expr);
    assignment_expr(ternary, postfix)
}

fn for_header_atom_expr<'src>(
    stmt: impl AnvParser<'src, ast::StmtNode>,
    expr: impl AnvParser<'src, ast::ExprNode>,
) -> BoxedParser<'src, ast::ExprNode> {
    choice((
        inferred_enum_expr(expr.clone()),
        lambda_expr(stmt.clone(), expr.clone()),
        intrinsic_call_expr(expr.clone()),
        string_interp(expr.clone()),
        literal().map_with(|lit, e| {
            let s = e.span();
            let span = s.byte();
            let id = new_expr_id();
            let expr = ast::Expr::new(ast::ExprKind::Lit(lit), id);
            Spanned::new(expr, span)
        }),
        array_literal(expr.clone()),
        type_subject_expr(),
        iter_source_expr(expr.clone()),
        identifier().map_with(|ident, e| {
            let s = e.span();
            let span = s.byte();
            let expr_id = new_expr_id();
            let expr = ast::Expr::new(ast::ExprKind::Ident(ident), expr_id);
            Spanned::new(expr, span)
        }),
        if_expr(stmt.clone(), expr.clone()),
        match_expr(stmt.clone(), expr.clone()),
        block_stmt(stmt, expr.clone()).map(|block_node| {
            let span = block_node.span;
            let id = new_expr_id();
            let block_expr = ast::Expr::new(ast::ExprKind::Block(block_node), id);
            Spanned::new(block_expr, span)
        }),
        grouped_or_tuple_expr(expr),
    ))
    .labelled("for header atom")
    .boxed()
}

pub(super) fn cond_expression<'src>() -> BoxedParser<'src, ast::ExprNode> {
    recursive(|cond_expr| {
        let atom = cond_atom_expr(cond_expr.clone());
        let postfix = postfix_expr(atom, cond_expr.clone());
        let unary = unary_expr(postfix.clone());
        let cast = cast_expr(unary);
        let binary = binary_expr(cast);
        let ternary = ternary_expr(binary, cond_expr.clone());
        assignment_expr(ternary, postfix)
    })
    .boxed()
}

fn if_expr<'src>(
    stmt: impl AnvParser<'src, ast::StmtNode>,
    expr: impl AnvParser<'src, ast::ExprNode>,
) -> BoxedParser<'src, ast::ExprNode> {
    recursive(|if_parser| {
        let else_branch = select! {
            Token::Keyword(Keyword::Else) => (),
        }
        .ignore_then(choice((
            // else-if wraps the nested if in a block with the if as the tail
            if_parser.map_with(|nested_if: ast::ExprNode, _| {
                let span = nested_if.span;
                Spanned::new(
                    ast::Block {
                        stmts: vec![],
                        tail: Some(Box::new(nested_if)),
                    },
                    span,
                )
            }),
            // else { ... }
            block_stmt(stmt.clone(), expr.clone()),
        )))
        .or_not();

        let let_value = conditional_pattern()
            .then_ignore(select! { Token::Op(Op::Assign) => () })
            .then(cond_expression());

        let if_let = select! { Token::Keyword(Keyword::If) => () }
            .ignore_then(let_value)
            .then(block_stmt(stmt.clone(), expr.clone()))
            .then(else_branch.clone())
            .map_with(|((((head, pat), value), then_block), else_block), e| {
                let s = e.span();
                let span = s.byte();
                let if_let_node = Spanned::new(
                    ast::IfLet {
                        head,
                        pattern: pat,
                        value: Box::new(value),
                        then_block,
                        else_block,
                    },
                    span,
                );
                let expr_id = new_expr_id();
                let expr = ast::Expr::new(ast::ExprKind::IfLet(if_let_node), expr_id);
                Spanned::new(expr, span)
            });

        let if_cond = select! {
            Token::Keyword(Keyword::If) => (),
        }
        .ignore_then(cond_expression())
        .then(block_stmt(stmt.clone(), expr.clone()))
        .then(else_branch)
        .map_with(|((cond, then_block), else_block), e| {
            let s = e.span();
            let span = s.byte();
            let if_node = Spanned::new(
                ast::If {
                    cond: Box::new(cond),
                    then_block,
                    else_block,
                },
                span,
            );
            let expr_id = new_expr_id();
            let expr = ast::Expr::new(ast::ExprKind::If(if_node), expr_id);
            Spanned::new(expr, span)
        });

        choice((if_let, if_cond))
    })
    .labelled("if expression")
    .as_context()
    .boxed()
}

fn match_expr<'src>(
    stmt: impl AnvParser<'src, ast::StmtNode>,
    expr: impl AnvParser<'src, ast::ExprNode>,
) -> BoxedParser<'src, ast::ExprNode> {
    let arm_body = choice((
        block_stmt(stmt.clone(), expr.clone()).map(|block_node| {
            let span = block_node.span;
            let id = new_expr_id();
            let arm_expr = ast::Expr::new(ast::ExprKind::Block(block_node), id);
            Spanned::new(arm_expr, span)
        }),
        expr.clone(),
    ));

    let dyn_binding = identifier().map(|ident| (ident.0.as_ref() != "_").then_some(ident));
    let dynamic_arm_head = type_ident()
        .then(
            select! { Token::Open(Delimiter::Parent) => () }
                .ignore_then(dyn_binding.clone())
                .then_ignore(select! { Token::Close(Delimiter::Parent) => () }),
        )
        .map_with(|(target, binding), e| {
            ast::MatchArmHead::DynDowncast(Spanned::new(
                ast::DynDowncastArm {
                    id: new_expr_id(),
                    target,
                    binding,
                },
                e.span().byte(),
            ))
        })
        .or(dyn_binding.map(ast::MatchArmHead::DynFallback));

    let dynamic_marker = select! { Token::Keyword(Keyword::As) => () }
        .then_ignore(select! { Token::Question => () })
        .then_ignore(select! { Token::Open(Delimiter::Brace) => () }.rewind())
        .to(ast::MatchMode::Dynamic);
    let dynamic_arms = dynamic_marker.then(match_arm_list(match_arm(
        dynamic_arm_head,
        arm_body.clone(),
    )));
    let pattern_arms = match_arm_list(match_arm(
        pattern().map(ast::MatchArmHead::Pattern),
        arm_body,
    ))
    .map(|arms| (ast::MatchMode::Pattern, arms));

    let cond_expr = cond_expression();
    let match_access = select! { Token::Keyword(Keyword::Ref) => ast::RefAccess::Ref }
        .or_not()
        .map(|access| access.unwrap_or(ast::RefAccess::Value));

    select! { Token::Keyword(Keyword::Match) => () }
        .ignore_then(match_access)
        .then(cond_expr)
        .then(dynamic_arms.or(pattern_arms))
        .map_with(|((access, scrutinee), (mode, arms)), e| {
            let span = e.span().byte();
            let match_node = Spanned::new(
                ast::Match {
                    access,
                    mode,
                    scrutinee: Box::new(scrutinee),
                    arms,
                },
                span,
            );
            let expr_id = new_expr_id();
            let expr = ast::Expr::new(ast::ExprKind::Match(match_node), expr_id);
            Spanned::new(expr, span)
        })
        .labelled("match expression")
        .as_context()
        .boxed()
}

fn match_arm<'src>(
    head: impl AnvParser<'src, ast::MatchArmHead>,
    body: impl AnvParser<'src, ast::ExprNode>,
) -> BoxedParser<'src, ast::MatchArmNode> {
    head.then_ignore(select! { Token::Op(Op::FatArrow) => () })
        .then(body)
        .map_with(|(head, body), e| Spanned::new(ast::MatchArm { head, body }, e.span().byte()))
        .boxed()
}

fn match_arm_list<'src>(
    arm: impl AnvParser<'src, ast::MatchArmNode>,
) -> BoxedParser<'src, Vec<ast::MatchArmNode>> {
    braces(comma_list(arm)).boxed()
}

fn expr_field<'src>(
    expr: impl AnvParser<'src, ast::ExprNode>,
) -> BoxedParser<'src, (ast::Ident, ast::ExprNode)> {
    let field_with_expr = field_name_ident()
        .then_ignore(colon())
        .then(expr)
        .map(|(name, value)| (name, value));

    let field_shorthand = identifier().map_with(|name, e| {
        let s = e.span();
        let span = s.byte();
        let expr_id = new_expr_id();
        let ident_expr = ast::Expr::new(ast::ExprKind::Ident(name), expr_id);
        let value = Spanned::new(ident_expr, span);
        (name, value)
    });

    choice((field_with_expr, field_shorthand)).boxed()
}

fn struct_literal<'src>(
    expr: impl AnvParser<'src, ast::ExprNode>,
) -> BoxedParser<'src, ast::ExprNode> {
    let field_init = expr_field(expr);

    let generic_args = select! { Token::Op(Op::LessThan) => () }
        .ignore_then(comma_list(generic_arg(type_ident())))
        .then_ignore(select! { Token::Op(Op::GreaterThan) => () })
        .then_ignore(select! { Token::Open(Delimiter::Brace) => () }.rewind())
        .or_not()
        .map(Option::unwrap_or_default);

    qualified_name()
        .then(generic_args)
        .then(braces(comma_list(field_init)))
        .map_with(|(((qualifier, name), generic_args), fields), e| {
            let s = e.span();
            let span = s.byte();
            let lit_node = Spanned::new(
                ast::StructLiteral {
                    qualifier,
                    name,
                    generic_args,
                    fields,
                },
                span,
            );
            let expr_id = new_expr_id();
            let expr = ast::Expr::new(ast::ExprKind::StructLiteral(lit_node), expr_id);
            Spanned::new(expr, span)
        })
        .labelled("struct literal")
        .as_context()
        .boxed()
}

fn array_literal<'src>(
    expr: impl AnvParser<'src, ast::ExprNode>,
) -> BoxedParser<'src, ast::ExprNode> {
    let semicolon = select! { Token::Semicolon => () };

    // array fill literal [value; len]
    let fill_literal = brackets(expr.clone().then_ignore(semicolon).then(expr.clone())).map_with(
        |(value, len), e| {
            let s = e.span();
            let span = s.byte();
            let fill_node = Spanned::new(
                ast::ArrayFill {
                    value: Box::new(value),
                    len: Box::new(len),
                },
                span,
            );
            let expr_id = new_expr_id();
            let expr = ast::Expr::new(ast::ExprKind::ArrayFill(fill_node), expr_id);
            Spanned::new(expr, span)
        },
    );

    // map entry 'key: value'
    let map_entry = expr.clone().then_ignore(colon()).then(expr.clone());

    // non empty map literal [key: value, ...]
    let map_literal = brackets(nonempty_comma_list(map_entry)).map_with(|entries, e| {
        let s = e.span();
        let span = s.byte();
        let lit_node = Spanned::new(ast::MapLiteral { entries }, span);
        let expr_id = new_expr_id();
        let expr = ast::Expr::new(ast::ExprKind::MapLiteral(lit_node), expr_id);
        Spanned::new(expr, span)
    });

    // empty map literal [:]
    // use a dummy nil literal to provide type context for the parser
    let empty_map = brackets(colon()).to(ast::Lit::Nil).map_with(|_, e| {
        let s = e.span();
        let span = s.byte();
        let lit_node = Spanned::new(ast::MapLiteral { entries: vec![] }, span);
        let expr_id = new_expr_id();
        let expr = ast::Expr::new(ast::ExprKind::MapLiteral(lit_node), expr_id);
        Spanned::new(expr, span)
    });

    // array/list literal [elem, ...] or []
    let element_list = brackets(comma_list(expr)).map_with(|elements, e| {
        let s = e.span();
        let span = s.byte();
        let lit_node = Spanned::new(ast::ArrayLiteral { elements }, span);
        let expr_id = new_expr_id();
        let expr = ast::Expr::new(ast::ExprKind::ArrayLiteral(lit_node), expr_id);
        Spanned::new(expr, span)
    });

    // the order matters here, i need to put more specific patterns first
    // [v; n] -> [:] -> [k: v, ...] -> ([e, ...] or [])
    choice((fill_literal, empty_map, map_literal, element_list))
        .labelled("array or map literal")
        .as_context()
        .boxed()
}

fn is_align_char(c: char) -> bool {
    matches!(c, '<' | '>' | '^')
}

fn parse_align_char(c: char) -> ast::FormatAlign {
    match c {
        '<' => ast::FormatAlign::Left,
        '>' => ast::FormatAlign::Right,
        '^' => ast::FormatAlign::Center,
        _ => unreachable!(),
    }
}

fn parse_format_spec(raw: &str) -> Result<ast::FormatSpec, String> {
    if raw.is_empty() {
        return Err("empty format specifier after ':'".to_string());
    }

    let chars: Vec<char> = raw.chars().collect();
    let len = chars.len();
    let mut pos = 0;
    let mut spec = ast::FormatSpec::default();

    // fill + align
    if pos + 1 < len && is_align_char(chars[pos + 1]) {
        spec.fill = chars[pos];
        spec.align = Some(parse_align_char(chars[pos + 1]));
        pos += 2;
    } else if pos < len && is_align_char(chars[pos]) {
        spec.align = Some(parse_align_char(chars[pos]));
        pos += 1;
    }

    // sign
    if pos < len && chars[pos] == '+' {
        spec.sign = ast::FormatSign::Always;
        pos += 1;
    }

    // zero-pad
    if pos < len && chars[pos] == '0' {
        spec.zero_pad = true;
        if spec.align.is_none() {
            spec.fill = '0';
            spec.align = Some(ast::FormatAlign::Right);
        }
        pos += 1;
    }

    // width
    let width_start = pos;
    while pos < len && chars[pos].is_ascii_digit() {
        pos += 1;
    }
    if pos > width_start {
        let width_str: String = chars[width_start..pos].iter().collect();
        spec.width = Some(
            width_str
                .parse::<u32>()
                .map_err(|_| "invalid width".to_string())?,
        );
    }

    // precision
    if pos < len && chars[pos] == '.' {
        pos += 1;
        let prec_start = pos;
        while pos < len && chars[pos].is_ascii_digit() {
            pos += 1;
        }
        if pos == prec_start {
            return Err("expected precision value after '.'".to_string());
        }
        let prec_str: String = chars[prec_start..pos].iter().collect();
        spec.precision = Some(
            prec_str
                .parse::<u32>()
                .map_err(|_| "invalid precision".to_string())?,
        );
    }

    // type
    if pos < len {
        spec.kind = match chars[pos] {
            'x' => ast::FormatKind::Hex,
            'X' => ast::FormatKind::HexUpper,
            'b' => ast::FormatKind::Binary,
            'e' => ast::FormatKind::Exp,
            'E' => ast::FormatKind::ExpUpper,
            c => return Err(format!("unknown format type '{c}'")),
        };
        pos += 1;
    }

    if pos < len {
        return Err(format!(
            "unexpected character '{}' in format specifier",
            chars[pos]
        ));
    }

    Ok(spec)
}

fn string_interp<'src>(
    expr: impl AnvParser<'src, ast::ExprNode>,
) -> BoxedParser<'src, ast::ExprNode> {
    let interp_start = select! { Token::Interp(InterpToken::Start) => () };
    let interp_end = select! { Token::Interp(InterpToken::End) => () };
    let expr_start = select! { Token::Interp(InterpToken::ExprStart) => () };
    let expr_end = select! { Token::Interp(InterpToken::ExprEnd) => () };
    let text_part = select! {
        Token::Interp(InterpToken::Text(s)) => ast::StringPart::Text(s.to_string()),
    };
    let fmt_spec = select! {
        Token::Interp(InterpToken::FormatSpec(s)) => s.clone(),
    }
    .map_with(|s, e| (s, e.span()))
    .or_not();
    let expr_part = expr_start
        .ignore_then(expr)
        .then_ignore(expr_end)
        .then(fmt_spec)
        .validate(|(expr, fmt), _extra, emitter| {
            let parsed_spec = fmt.and_then(|(raw, span)| match parse_format_spec(&raw) {
                Ok(spec) => Some(Spanned::new(spec, span.byte())),
                Err(mut msg) => {
                    if raw.contains(' ') {
                        msg.push_str(" (if this is a ternary expression, wrap it in parentheses)");
                    }
                    emitter.emit(Rich::custom(span, msg));
                    None
                }
            });
            ast::StringPart::Expr(Box::new(expr), parsed_spec)
        });

    interp_start
        .ignore_then(
            choice((text_part, expr_part))
                .repeated()
                .collect::<Vec<_>>(),
        )
        .then_ignore(interp_end)
        .map_with(|parts, e| {
            let s = e.span();
            let span = s.byte();
            let id = new_expr_id();
            let expr = ast::Expr::new(ast::ExprKind::StringInterp(parts), id);
            Spanned::new(expr, span)
        })
        .boxed()
}

fn lambda_expr<'src>(
    stmt: impl AnvParser<'src, ast::StmtNode>,
    expr: impl AnvParser<'src, ast::ExprNode>,
) -> BoxedParser<'src, ast::ExprNode> {
    let pipe = select! { Token::Op(Op::BitOr) => () };
    let or_op = select! { Token::Op(Op::Or) => () };
    let comma = select! { Token::Comma => () };
    let colon = select! { Token::Colon => () };
    let assign = select! { Token::Op(Op::Assign) => () };

    let ref_kw = select! {
        Token::Keyword(Keyword::Ref) => (),
    }
    .or_not()
    .map(|opt| opt.is_some());

    let lambda_param = ref_kw
        .then(identifier())
        .then(
            colon
                .ignore_then(callable_param_type(type_ident()))
                .or_not()
                .map(|opt| match opt {
                    Some((cast_accept, escape, ty)) => (Some(ty), cast_accept, escape),
                    None => (None, false, ast::EscapeMode::NonEscaping),
                }),
        )
        .then(
            assign
                .ignore_then(
                    any()
                        .filter(|token| !matches!(token, Token::Op(Op::BitOr) | Token::Comma))
                        .repeated()
                        .at_least(1),
                )
                .or_not(),
        )
        .validate(
            |(((mutable, name), (ty, cast_accept, escape)), default), extra, emitter| {
                if default.is_some() {
                    emitter.emit(Rich::custom(
                        extra.span(),
                        "lambda parameters cannot have defaults",
                    ));
                }
                ast::LambdaParam {
                    name,
                    ty,
                    mutable,
                    cast_accept,
                    escape,
                }
            },
        );

    // |param, param: Type| or ||
    let with_params = pipe
        .ignore_then(
            lambda_param
                .separated_by(comma)
                .allow_trailing()
                .collect::<Vec<_>>(),
        )
        .then_ignore(pipe);

    let zero_params = or_op.map(|()| vec![]);

    let params = choice((zero_params, with_params));

    let ret_type = return_spec();

    let block_body = block_stmt(stmt, expr.clone()).map(|block_node| {
        let span = block_node.span;
        let id = new_expr_id();
        let block_expr = ast::Expr::new(ast::ExprKind::Block(block_node), id);
        Spanned::new(block_expr, span)
    });

    let body = choice((block_body, expr));

    params
        .then(ret_type)
        .then(body)
        .map_with(|((params, ret_type), body), e| {
            let s = e.span();
            let span = s.byte();
            let id = new_expr_id();
            let lambda = ast::Lambda {
                params,
                ret_type,
                body: Box::new(body),
            };
            let lambda_node = Spanned::new(lambda, span);
            let expr = ast::Expr::new(ast::ExprKind::Lambda(lambda_node), id);
            Spanned::new(expr, span)
        })
        .labelled("lambda")
        .boxed()
}

fn inferred_enum_expr<'src>(
    expr: impl AnvParser<'src, ast::ExprNode>,
) -> BoxedParser<'src, ast::ExprNode> {
    let comma = select! { Token::Comma => () };

    let field_init = expr_field(expr.clone());

    let tuple_args = select! { Token::Open(Delimiter::Parent) => () }
        .ignore_then(
            expr.separated_by(comma)
                .allow_trailing()
                .collect::<Vec<_>>(),
        )
        .then_ignore(select! { Token::Close(Delimiter::Parent) => () })
        .map(ast::InferredEnumArgs::Tuple);

    let struct_fields = select! { Token::Open(Delimiter::Brace) => () }
        .ignore_then(
            field_init
                .separated_by(comma)
                .allow_trailing()
                .collect::<Vec<_>>(),
        )
        .then_ignore(select! { Token::Close(Delimiter::Brace) => () })
        .map(ast::InferredEnumArgs::Struct);

    select! { Token::Dot => () }
        .ignore_then(identifier())
        .then(choice((tuple_args, struct_fields)).or_not())
        .map_with(|(variant, args), e| {
            let s = e.span();
            let span = s.byte();
            let args = args.unwrap_or(ast::InferredEnumArgs::Unit);
            let node = Spanned::new(ast::InferredEnum { variant, args }, span);
            let expr_id = new_expr_id();
            let expr = ast::Expr::new(ast::ExprKind::InferredEnum(node), expr_id);
            Spanned::new(expr, span)
        })
        .labelled("inferred enum variant")
        .boxed()
}

fn intrinsic_call_expr<'src>(
    expr: impl AnvParser<'src, ast::ExprNode>,
) -> BoxedParser<'src, ast::ExprNode> {
    let comma = select! { Token::Comma => () };

    select! { Token::Hash => () }
        .ignore_then(identifier())
        .then(
            select! { Token::Open(Delimiter::Parent) => () }
                .ignore_then(
                    expr.separated_by(comma)
                        .allow_trailing()
                        .collect::<Vec<_>>(),
                )
                .then_ignore(select! { Token::Close(Delimiter::Parent) => () }),
        )
        .map_with(|(name, args), e| {
            let s = e.span();
            let span = s.byte();
            let node = Spanned::new(ast::IntrinsicCall { name, args }, span);
            let expr_id = new_expr_id();
            let expr = ast::Expr::new(ast::ExprKind::IntrinsicCall(node), expr_id);
            Spanned::new(expr, span)
        })
        .labelled("intrinsic call")
        .boxed()
}

fn type_subject_expr<'src>() -> BoxedParser<'src, ast::ExprNode> {
    let explicit = select! { Token::Op(Op::LessThan) => () }
        .ignore_then(type_subject_type_ident())
        .then_ignore(select! { Token::Op(Op::GreaterThan) => () });

    let primitive = select! {
        Token::Keyword(Keyword::Int) => ast::Type::Int,
        Token::Keyword(Keyword::Float) => ast::Type::Float,
        Token::Keyword(Keyword::Bool) => ast::Type::Bool,
        Token::Keyword(Keyword::String) => ast::Type::String,
        Token::Keyword(Keyword::Char) => ast::Type::Char,
    };

    choice((explicit, primitive))
        .map_with(|ty, e| {
            let s = e.span();
            let span = s.byte();
            let expr_id = new_expr_id();
            let expr = ast::Expr::new(ast::ExprKind::TypeSubject(ty), expr_id);
            Spanned::new(expr, span)
        })
        .labelled("type subject")
        .boxed()
}

fn iter_source_expr<'src>(
    expr: impl AnvParser<'src, ast::ExprNode>,
) -> BoxedParser<'src, ast::ExprNode> {
    select! { Token::Keyword(Keyword::Iter) => () }
        .ignore_then(parens(expr))
        .map_with(|source, e| {
            let span = e.span().byte();
            let node = Spanned::new(
                ast::IterSource {
                    source: Box::new(source),
                },
                span,
            );
            let expr_id = new_expr_id();
            let expr = ast::Expr::new(ast::ExprKind::IterSource(node), expr_id);
            Spanned::new(expr, span)
        })
        .labelled("iterator source")
        .boxed()
}

fn atom_expr<'src>(
    stmt: impl AnvParser<'src, ast::StmtNode>,
    expr: impl AnvParser<'src, ast::ExprNode>,
) -> BoxedParser<'src, ast::ExprNode> {
    choice((
        inferred_enum_expr(expr.clone()),
        lambda_expr(stmt.clone(), expr.clone()),
        intrinsic_call_expr(expr.clone()),
        string_interp(expr.clone()),
        literal().map_with(|lit, e| {
            let s = e.span();
            let span = s.byte();
            let id = new_expr_id();
            let expr = ast::Expr::new(ast::ExprKind::Lit(lit), id);
            Spanned::new(expr, span)
        }),
        struct_literal(expr.clone()),
        array_literal(expr.clone()),
        type_subject_expr(),
        iter_source_expr(expr.clone()),
        identifier().map_with(|ident, e| {
            let s = e.span();
            let span = s.byte();
            let expr_id = new_expr_id();
            let expr = ast::Expr::new(ast::ExprKind::Ident(ident), expr_id);
            Spanned::new(expr, span)
        }),
        if_expr(stmt.clone(), expr.clone()),
        match_expr(stmt.clone(), expr.clone()),
        block_stmt(stmt, expr.clone()).map(|block_node| {
            let span = block_node.span;
            let id = new_expr_id();
            let block_expr = ast::Expr::new(ast::ExprKind::Block(block_node), id);
            Spanned::new(block_expr, span)
        }),
        grouped_or_tuple_expr(expr),
    ))
    .labelled("atom")
    .boxed()
}

fn cond_atom_expr<'src>(
    cond_expr: impl AnvParser<'src, ast::ExprNode>,
) -> BoxedParser<'src, ast::ExprNode> {
    choice((
        inferred_enum_expr(cond_expr.clone()),
        intrinsic_call_expr(cond_expr.clone()),
        literal().map_with(|lit, e| {
            let s = e.span();
            let span = s.byte();
            let id = new_expr_id();
            let expr = ast::Expr::new(ast::ExprKind::Lit(lit), id);
            Spanned::new(expr, span)
        }),
        array_literal(cond_expr.clone()),
        type_subject_expr(),
        iter_source_expr(cond_expr.clone()),
        identifier().map_with(|ident, e| {
            let s = e.span();
            let span = s.byte();
            let expr_id = new_expr_id();
            let expr = ast::Expr::new(ast::ExprKind::Ident(ident), expr_id);
            Spanned::new(expr, span)
        }),
        grouped_or_tuple_expr(cond_expr),
    ))
    .labelled("condition atom")
    .boxed()
}

enum TupleExprElem {
    Pos(ast::ExprNode),
    Labelled(ast::ExprNode),
}

impl TupleExprElem {
    fn is_labelled(&self) -> bool {
        matches!(self, Self::Labelled(_))
    }

    fn into_expr(self) -> ast::ExprNode {
        match self {
            Self::Pos(expr) | Self::Labelled(expr) => expr,
        }
    }
}

fn grouped_or_tuple_expr<'src>(
    expr: impl AnvParser<'src, ast::ExprNode>,
) -> BoxedParser<'src, ast::ExprNode> {
    let labelled_elem = identifier()
        .then_ignore(colon())
        .ignore_then(expr.clone())
        .map(TupleExprElem::Labelled);

    let pos_elem = expr.map(TupleExprElem::Pos);
    let elem = choice((labelled_elem, pos_elem));

    parenthesized_tuple_parts(elem)
        .validate(|parts, e, emitter| {
            let s = e.span();
            let span = s.byte();
            let expr_id = new_expr_id();
            let dummy_expr = || ast::Expr::new(ast::ExprKind::Lit(ast::Lit::Nil), expr_id);

            let has_label = parts.first.as_ref().is_some_and(TupleExprElem::is_labelled)
                || parts.rest.iter().any(TupleExprElem::is_labelled);

            if has_label {
                emitter.emit(Rich::custom(
                    s,
                    "labels are not allowed in tuple literals; use a struct literal or positional tuple",
                ));
                return Spanned::new(dummy_expr(), span);
            }

            let first = parts.first.map(TupleExprElem::into_expr);
            let rest = parts
                .rest
                .into_iter()
                .map(TupleExprElem::into_expr)
                .collect();

            match validate_tuple_shape_raw(first, rest, parts.trailing_comma) {
                TupleShapeResult::Empty => {
                    emitter.emit(Rich::custom(s, "empty tuples are not supported"));
                    Spanned::new(dummy_expr(), span)
                }
                TupleShapeResult::OneTupleError(expr) => {
                    emitter.emit(Rich::custom(s, "1-tuples are not supported"));
                    expr
                }
                TupleShapeResult::Grouped(expr) => expr,
                TupleShapeResult::Tuple(exprs) => {
                    let tuple_expr = ast::Expr::new(ast::ExprKind::Tuple(exprs), expr_id);
                    Spanned::new(tuple_expr, span)
                }
                TupleShapeResult::UnexpectedComma => {
                    emitter.emit(Rich::custom(s, "unexpected comma"));
                    Spanned::new(dummy_expr(), span)
                }
            }
        })
        .labelled("tuple or grouped expression")
        .as_context()
        .boxed()
}

fn fn_call_args<'src>(
    expr: impl AnvParser<'src, ast::ExprNode>,
) -> BoxedParser<'src, Vec<ast::ExprNode>> {
    parens(comma_list(expr))
        .labelled("function call arguments")
        .as_context()
        .boxed()
}

fn call_generic_args<'src>() -> BoxedParser<'src, Vec<ast::GenericArg>> {
    // lookahead for optional generic arguments (<int, ..>)
    // and rewind to avoid consuming < when its a comparsion op (a < b)
    let generic_lookahead = select! {
        Token::Op(Op::LessThan) => (),
    }
    .ignore_then(comma_list(generic_arg(type_ident())))
    .then_ignore(select! {
        Token::Op(Op::GreaterThan) => (),
    })
    .then_ignore(select! {
        Token::Open(Delimiter::Parent) => (),
    })
    .rewind();

    let generic_list = select! {
        Token::Op(Op::LessThan) => (),
    }
    .ignore_then(comma_list(generic_arg(type_ident())))
    .then_ignore(select! {
        Token::Op(Op::GreaterThan) => (),
    });

    generic_lookahead
        .ignore_then(generic_list)
        .or_not()
        .map(Option::unwrap_or_default)
        .labelled("generic arguments")
        .as_context()
        .boxed()
}

enum PostfixOp {
    Call {
        generic_args: Vec<ast::GenericArg>,
        args: Vec<ast::ExprNode>,
        safe: bool,
    },
    Field {
        ident: ast::Ident,
        safe: bool,
    },
    TupleIndices(Vec<u32>),
    Index {
        expr: ast::ExprNode,
        safe: bool,
    },
}

fn postfix_expr<'src>(
    atom: impl AnvParser<'src, ast::ExprNode>,
    expr: impl AnvParser<'src, ast::ExprNode>,
) -> BoxedParser<'src, ast::ExprNode> {
    let call_suffix =
        call_generic_args()
            .then(fn_call_args(expr.clone()))
            .map(|(generic_args, args)| PostfixOp::Call {
                generic_args,
                args,
                safe: false,
            });

    let safe_call_suffix = select! { Token::Question => () }
        .ignore_then(call_generic_args())
        .then(fn_call_args(expr.clone()))
        .map(|(generic_args, args)| PostfixOp::Call {
            generic_args,
            args,
            safe: true,
        });

    let single_index = select! {
        Token::Dot => (),
    }
    .ignore_then(select! {
        Token::Literal(LitToken::Number(n)) => n,
    })
    .try_map(|n, span| {
        u32::try_from(*n.value())
            .map(|n| PostfixOp::TupleIndices(vec![n]))
            .map_err(|_| Rich::custom(span, "invalid tuple index"))
    });

    let chained_index = select! {
        Token::Dot => (),
    }
    .ignore_then(select! {
        Token::Literal(LitToken::Float(n)) => n,
    })
    .try_map(|n, span| {
        let parts = n.spelling().split('.').collect::<Vec<_>>();
        let indices = parts
            .iter()
            .map(|p| p.replace('_', "").parse::<u32>())
            .collect::<Result<Vec<_>, _>>();
        indices
            .map(PostfixOp::TupleIndices)
            .map_err(|_| Rich::custom(span, "invalid tuple index"))
    });

    let safe_field_access = select! { Token::Question => () }
        .ignore_then(select! { Token::Dot => () })
        .ignore_then(field_name_ident())
        .map(|ident| PostfixOp::Field { ident, safe: true });

    let field_access = select! {
        Token::Dot => (),
    }
    .ignore_then(field_name_ident())
    .map(|ident| PostfixOp::Field { ident, safe: false });

    let safe_index_suffix = select! { Token::Question => () }
        .ignore_then(select! { Token::Open(Delimiter::Bracket) => () })
        .ignore_then(expr.clone())
        .then_ignore(select! { Token::Close(Delimiter::Bracket) => () })
        .map(|index_expr| PostfixOp::Index {
            expr: index_expr,
            safe: true,
        });

    let index_suffix = select! { Token::Open(Delimiter::Bracket) => () }
        .ignore_then(expr)
        .then_ignore(select! { Token::Close(Delimiter::Bracket) => () })
        .map(|index_expr| PostfixOp::Index {
            expr: index_expr,
            safe: false,
        });

    let postfix_op = choice((
        safe_call_suffix,
        call_suffix,
        safe_index_suffix,
        index_suffix,
        chained_index,
        single_index,
        safe_field_access,
        field_access,
    ));

    atom.foldl_with(postfix_op.repeated(), |target, op, e| {
        let s = e.span();
        let span = s.byte();

        match op {
            PostfixOp::Call {
                generic_args,
                args,
                safe,
            } => {
                let call_node = Spanned::new(
                    ast::Call {
                        func: Box::new(target),
                        args,
                        generic_args,
                        safe,
                    },
                    span,
                );

                let expr_id = new_expr_id();
                let expr = ast::Expr::new(ast::ExprKind::Call(call_node), expr_id);
                Spanned::new(expr, span)
            }
            PostfixOp::TupleIndices(indices) => {
                let mut current = target;
                for index in indices {
                    let index_node = Spanned::new(
                        ast::TupleIndex {
                            target: Box::new(current),
                            index,
                        },
                        span,
                    );

                    let expr_id = new_expr_id();
                    let expr = ast::Expr::new(ast::ExprKind::TupleIndex(index_node), expr_id);
                    current = Spanned::new(expr, span);
                }
                current
            }
            PostfixOp::Field { ident: field, safe } => {
                let field_node = Spanned::new(
                    ast::FieldAccess {
                        target: Box::new(target),
                        field,
                        safe,
                    },
                    span,
                );

                let expr_id = new_expr_id();
                let expr = ast::Expr::new(ast::ExprKind::Field(field_node), expr_id);
                Spanned::new(expr, span)
            }
            PostfixOp::Index {
                expr: index_expr,
                safe,
            } => {
                let index_span = target.span.union(span);

                let index_node = Spanned::new(
                    ast::Index {
                        target: Box::new(target),
                        index: Box::new(index_expr),
                        safe,
                    },
                    index_span,
                );

                let expr_id = new_expr_id();
                let expr = ast::Expr::new(ast::ExprKind::Index(index_node), expr_id);
                Spanned::new(expr, index_span)
            }
        }
    })
    .labelled("postfix expression")
    .as_context()
    .boxed()
}

fn cast_expr<'src>(unary: impl AnvParser<'src, ast::ExprNode>) -> BoxedParser<'src, ast::ExprNode> {
    let cast_op = select! { Token::Keyword(Keyword::As) => () }
        .ignore_then(select! { Token::Question => CastOp::Failable }.or_not())
        .then(type_ident())
        .map(|(op, target)| (op.unwrap_or(CastOp::Ordinary), target));
    unary
        .foldl_with(cast_op.repeated(), |expr, (op, target), e| {
            let s = e.span();
            let span = s.byte();
            let id = new_expr_id();
            let kind = match op {
                CastOp::Ordinary => ast::ExprKind::Cast(Spanned::new(
                    ast::Cast {
                        expr: Box::new(expr),
                        target,
                    },
                    span,
                )),
                CastOp::Failable => ast::ExprKind::FailableCast(Spanned::new(
                    ast::Cast {
                        expr: Box::new(expr),
                        target,
                    },
                    span,
                )),
            };
            Spanned::new(ast::Expr::new(kind, id), span)
        })
        .boxed()
}

#[derive(Clone, Copy)]
enum CastOp {
    Ordinary,
    Failable,
}

enum PrefixOp {
    Unary(ast::UnaryOp),
    Try,
}

fn unary_expr<'src>(expr: impl AnvParser<'src, ast::ExprNode>) -> BoxedParser<'src, ast::ExprNode> {
    choice((
        select! {
            Token::Op(Op::Sub) => ast::UnaryOp::Neg,
            Token::Op(Op::Not) => ast::UnaryOp::Not,
            Token::Op(Op::Tilde) => ast::UnaryOp::BitNot,
        }
        .map(PrefixOp::Unary),
        select! { Token::Keyword(Keyword::Try) => PrefixOp::Try },
    ))
    .repeated()
    .collect::<Vec<_>>()
    .then(expr)
    .map_with(|(ops, expr), e| {
        let s = e.span();
        let span = s.byte();

        let mut expr_node = expr;
        for op in ops.into_iter().rev() {
            let expr_id = new_expr_id();
            expr_node = match op {
                PrefixOp::Unary(op) => {
                    let unary_node = Spanned::new(
                        ast::Unary {
                            op,
                            expr: Box::new(expr_node),
                        },
                        span,
                    );
                    Spanned::new(
                        ast::Expr::new(ast::ExprKind::Unary(unary_node), expr_id),
                        span,
                    )
                }
                PrefixOp::Try => {
                    let try_node = Spanned::new(
                        ast::Try {
                            expr: Box::new(expr_node),
                        },
                        span,
                    );
                    Spanned::new(ast::Expr::new(ast::ExprKind::Try(try_node), expr_id), span)
                }
            };
        }

        expr_node
    })
    .labelled("unary")
    .as_context()
    .boxed()
}

fn binary_expr<'src>(
    unary: impl AnvParser<'src, ast::ExprNode>,
) -> BoxedParser<'src, ast::ExprNode> {
    let mul = infix_left(unary, mul_div_op());
    let add = infix_left(mul, add_sub_op());
    let range = range_expr(add);
    let shift = infix_left(range, shift_op());
    let cmp = infix_left(shift, cmp_op());
    let eq = infix_left(cmp, eq_op());
    let bit_and = infix_left(eq, bit_and_op());
    let xor = infix_left(bit_and, xor_op());
    let bit_or = infix_left(xor, bit_or_op());
    let and = infix_left(bit_or, and_op());
    let coal = infix_left(and, coalesce_op());
    let or = infix_left(coal, or_op());
    or.labelled("expression").as_context().boxed()
}

fn range_expr<'src>(
    lower: impl AnvParser<'src, ast::ExprNode>,
) -> BoxedParser<'src, ast::ExprNode> {
    let prefix_range = select! {
        Token::Range => false,
        Token::RangeEq => true,
    }
    .then(lower.clone())
    .map_with(|(inclusive, end), e| {
        let s = e.span();
        let span = s.byte();
        let expr_id = new_expr_id();
        let expr = ast::Expr::new(
            ast::ExprKind::Range(Spanned::new(
                ast::Range::To {
                    end: Box::new(end),
                    inclusive,
                },
                span,
            )),
            expr_id,
        );
        Spanned::new(expr, span)
    });

    let op_rhs_inclusive = select! { Token::RangeEq => () }
        .ignore_then(lower.clone())
        .map(|end| (true, Some(end)));

    let op_rhs_exclusive = select! { Token::Range => () }
        .ignore_then(lower.clone().or_not())
        .map(|end| (false, end));

    let op_rhs = choice((op_rhs_inclusive, op_rhs_exclusive));

    let infix_range = lower.foldl_with(op_rhs.repeated(), |start, (inclusive, end), e| {
        if let Some(end) = end {
            let span = start.span.union(end.span);
            let range_node = Spanned::new(
                ast::Range::Bounded {
                    start: Box::new(start),
                    end: Box::new(end),
                    inclusive,
                },
                span,
            );
            let expr_id = new_expr_id();
            let expr = ast::Expr::new(ast::ExprKind::Range(range_node), expr_id);
            Spanned::new(expr, span)
        } else {
            let s = e.span();
            let span = s.byte();
            let expr_id = new_expr_id();
            let expr = ast::Expr::new(
                ast::ExprKind::Range(Spanned::new(
                    ast::Range::From {
                        start: Box::new(start),
                    },
                    span,
                )),
                expr_id,
            );
            Spanned::new(expr, span)
        }
    });

    choice((prefix_range, infix_range)).boxed()
}

fn ternary_expr<'src>(
    lower: impl AnvParser<'src, ast::ExprNode>,
    expr: impl AnvParser<'src, ast::ExprNode>,
) -> BoxedParser<'src, ast::ExprNode> {
    let ternary_suffix = select! {
        Token::Question => (),
    }
    .ignore_then(expr.clone())
    .then_ignore(select! {
        Token::Colon => (),
    })
    .then(expr);

    lower
        .foldl(ternary_suffix.repeated(), |cond, (then_expr, else_expr)| {
            let span = cond.span.union(else_expr.span);
            let ternary = Spanned::new(
                ast::Ternary {
                    cond: Box::new(cond),
                    then_expr: Box::new(then_expr),
                    else_expr: Box::new(else_expr),
                },
                span,
            );

            let expr_id = new_expr_id();
            let expr = ast::Expr::new(ast::ExprKind::Ternary(ternary), expr_id);
            Spanned::new(expr, span)
        })
        .labelled("ternary")
        .as_context()
        .boxed()
}

fn assignment_expr<'src>(
    expr: impl AnvParser<'src, ast::ExprNode>,
    target: impl AnvParser<'src, ast::ExprNode>,
) -> BoxedParser<'src, ast::ExprNode> {
    target
        .then(assign_op().then(expr.clone()))
        .map_with(|(target, (op, value)), e| {
            let s = e.span();
            let span = s.byte();
            let assign_node = Spanned::new(
                ast::Assign {
                    target: Box::new(target),
                    op,
                    value: Box::new(value),
                },
                span,
            );

            let expr_id = new_expr_id();
            let expr = ast::Expr::new(ast::ExprKind::Assign(assign_node), expr_id);
            Spanned::new(expr, span)
        })
        .or(expr)
        .labelled("assignment")
        .as_context()
        .boxed()
}
