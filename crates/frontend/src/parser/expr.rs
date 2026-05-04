use chumsky::{error::Rich, prelude::*};

use super::{
    AnvParser, BoxedParser,
    common::{
        TupleShapeResult, block_stmt, field_name_ident, identifier, literal,
        validate_tuple_shape_raw,
    },
    new_expr_id,
    ops::{
        add_sub_op, and_op, assign_op, bit_and_op, bit_or_op, cmp_op, coalesce_op, eq_op,
        infix_left, mul_div_op, or_op, shift_op, xor_op,
    },
    pattern::{or_pattern, pattern},
    types::{generic_arg, type_ident},
};
use crate::{
    ast,
    lexer::{Delimiter, InterpToken, Keyword, LitToken, Op, Token},
    span::{Span, Spanned},
};

pub(super) fn expression<'src>(
    stmt: impl AnvParser<'src, ast::StmtNode>,
) -> BoxedParser<'src, ast::ExprNode> {
    recursive(|expr| {
        let atom = atom_expr(stmt, expr.clone());
        let postfix = postfix_expr(atom, expr.clone());
        let unary = unary_expr(postfix);
        let cast = cast_expr(unary);
        let binary = binary_expr(cast);
        let ternary = ternary_expr(binary, expr.clone());
        assignment_expr(ternary)
    })
    .boxed()
}

pub(super) fn for_header_expression<'src>(
    stmt: impl AnvParser<'src, ast::StmtNode>,
) -> BoxedParser<'src, ast::ExprNode> {
    let full_expr = expression(stmt.clone());
    let atom = for_header_atom_expr(stmt, full_expr.clone());
    let postfix = postfix_expr(atom, full_expr.clone());
    let unary = unary_expr(postfix);
    let cast = cast_expr(unary);
    let binary = binary_expr(cast);
    let ternary = ternary_expr(binary, full_expr);
    assignment_expr(ternary)
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
            let span = Span::new(s.start, s.end);
            let id = new_expr_id();
            let expr = ast::Expr::new(ast::ExprKind::Lit(lit), id);
            Spanned::new(expr, span)
        }),
        array_literal(expr.clone()),
        identifier().map_with(|ident, e| {
            let s = e.span();
            let span = Span::new(s.start, s.end);
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
        let unary = unary_expr(postfix);
        let binary = binary_expr(unary);
        let ternary = ternary_expr(binary, cond_expr.clone());
        assignment_expr(ternary)
    })
    .boxed()
}

fn if_expr<'src>(
    stmt: impl AnvParser<'src, ast::StmtNode>,
    expr: impl AnvParser<'src, ast::ExprNode>,
) -> BoxedParser<'src, ast::ExprNode> {
    recursive(|if_parser| {
        let else_branch = select! {
            (Token::Keyword(Keyword::Else), _) => (),
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

        let if_let = select! { (Token::Keyword(Keyword::If), _) => () }
            .ignore_then(select! { (Token::Keyword(Keyword::Let), _) => () })
            .ignore_then(pattern())
            .then_ignore(select! { (Token::Op(Op::Assign), _) => () })
            .then(cond_expression())
            .then(block_stmt(stmt.clone(), expr.clone()))
            .then(else_branch.clone())
            .map_with(|(((pat, value), then_block), else_block), e| {
                let s = e.span();
                let span = Span::new(s.start, s.end);
                let if_let_node = Spanned::new(
                    ast::IfLet {
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
            (Token::Keyword(Keyword::If), _) => (),
        }
        .ignore_then(cond_expression())
        .then(block_stmt(stmt.clone(), expr.clone()))
        .then(else_branch)
        .map_with(|((cond, then_block), else_block), e| {
            let s = e.span();
            let span = Span::new(s.start, s.end);
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
    let comma = select! { (Token::Comma, _) => () };
    let open_brace = select! { (Token::Open(Delimiter::Brace), _) => () };
    let close_brace = select! { (Token::Close(Delimiter::Brace), _) => () };
    let fat_arrow = select! { (Token::Op(Op::FatArrow), _) => () };

    let arm_body = choice((
        block_stmt(stmt.clone(), expr.clone()).map(|block_node| {
            let span = block_node.span;
            let id = new_expr_id();
            let arm_expr = ast::Expr::new(ast::ExprKind::Block(block_node), id);
            Spanned::new(arm_expr, span)
        }),
        expr.clone(),
    ));

    let match_arm =
        or_pattern()
            .then_ignore(fat_arrow)
            .then(arm_body)
            .map_with(|(pat, body), e| {
                let s = e.span();
                let span = Span::new(s.start, s.end);
                Spanned::new(ast::MatchArm { pattern: pat, body }, span)
            });

    let cond_expr = cond_expression();

    select! { (Token::Keyword(Keyword::Match), _) => () }
        .ignore_then(cond_expr)
        .then(
            open_brace
                .ignore_then(
                    match_arm
                        .separated_by(comma)
                        .allow_trailing()
                        .collect::<Vec<_>>(),
                )
                .then_ignore(close_brace),
        )
        .map_with(|(scrutinee, arms), e| {
            let s = e.span();
            let span = Span::new(s.start, s.end);
            let match_node = Spanned::new(
                ast::Match {
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

fn struct_literal<'src>(
    expr: impl AnvParser<'src, ast::ExprNode>,
) -> BoxedParser<'src, ast::ExprNode> {
    let field_init = choice((
        field_name_ident()
            .then_ignore(select! { (Token::Colon, _) => () })
            .then(expr)
            .map(|(name, value)| (name, value)),
        identifier().map_with(|name, e| {
            let s = e.span();
            let span = Span::new(s.start, s.end);
            let expr_id = new_expr_id();
            let ident_expr = ast::Expr::new(ast::ExprKind::Ident(name), expr_id);
            let value = Spanned::new(ident_expr, span);
            (name, value)
        }),
    ));

    // parse qualified name like Enum.Variant or Struct
    let qualified_name = identifier()
        .then(
            select! { (Token::Dot, _) => () }
                .ignore_then(identifier())
                .or_not(),
        )
        .map(|(first, second)| match second {
            Some(name) => (Some(first), name), // is a enum variant
            None => (None, first),             // struc
        });

    let generic_args = select! { (Token::Op(Op::LessThan), _) => () }
        .ignore_then(
            generic_arg(type_ident())
                .separated_by(select! { (Token::Comma, _) => () })
                .allow_trailing()
                .collect::<Vec<_>>(),
        )
        .then_ignore(select! { (Token::Op(Op::GreaterThan), _) => () })
        .then_ignore(select! { (Token::Open(Delimiter::Brace), _) => () }.rewind())
        .or_not()
        .map(Option::unwrap_or_default);

    qualified_name
        .then(generic_args)
        .then(
            select! { (Token::Open(Delimiter::Brace), _) => () }
                .ignore_then(
                    field_init
                        .separated_by(select! { (Token::Comma, _) => () })
                        .allow_trailing()
                        .collect::<Vec<_>>(),
                )
                .then_ignore(select! { (Token::Close(Delimiter::Brace), _) => () }),
        )
        .map_with(|(((qualifier, name), generic_args), fields), e| {
            let s = e.span();
            let span = Span::new(s.start, s.end);
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
    let open_bracket = select! { (Token::Open(Delimiter::Bracket), _) => () };
    let close_bracket = select! { (Token::Close(Delimiter::Bracket), _) => () };
    let comma = select! { (Token::Comma, _) => () };
    let semicolon = select! { (Token::Semicolon, _) => () };
    let colon = select! { (Token::Colon, _) => () };

    // array fill literal [value; len]
    let fill_literal = open_bracket
        .ignore_then(expr.clone())
        .then_ignore(semicolon)
        .then(expr.clone())
        .then_ignore(close_bracket)
        .map_with(|(value, len), e| {
            let s = e.span();
            let span = Span::new(s.start, s.end);
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
        });

    // map entry 'key: value'
    let map_entry = expr.clone().then_ignore(colon).then(expr.clone());

    // non empty map literal [key: value, ...]
    let map_literal = open_bracket
        .ignore_then(
            map_entry
                .separated_by(comma)
                .allow_trailing()
                .at_least(1)
                .collect::<Vec<_>>(),
        )
        .then_ignore(close_bracket)
        .map_with(|entries, e| {
            let s = e.span();
            let span = Span::new(s.start, s.end);
            let lit_node = Spanned::new(ast::MapLiteral { entries }, span);
            let expr_id = new_expr_id();
            let expr = ast::Expr::new(ast::ExprKind::MapLiteral(lit_node), expr_id);
            Spanned::new(expr, span)
        });

    // empty map literal [:]
    // use a dummy nil literal to provide type context for the parser
    let empty_map = open_bracket
        .ignore_then(colon)
        .ignore_then(close_bracket)
        .to(ast::Lit::Nil)
        .map_with(|_, e| {
            let s = e.span();
            let span = Span::new(s.start, s.end);
            let lit_node = Spanned::new(ast::MapLiteral { entries: vec![] }, span);
            let expr_id = new_expr_id();
            let expr = ast::Expr::new(ast::ExprKind::MapLiteral(lit_node), expr_id);
            Spanned::new(expr, span)
        });

    // array/list literal [elem, ...] or []
    let element_list = open_bracket
        .ignore_then(
            expr.separated_by(comma)
                .allow_trailing()
                .collect::<Vec<_>>(),
        )
        .then_ignore(close_bracket)
        .map_with(|elements, e| {
            let s = e.span();
            let span = Span::new(s.start, s.end);
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
    let interp_start = select! { (Token::Interp(InterpToken::Start), _) => () };
    let interp_end = select! { (Token::Interp(InterpToken::End), _) => () };
    let expr_start = select! { (Token::Interp(InterpToken::ExprStart), _) => () };
    let expr_end = select! { (Token::Interp(InterpToken::ExprEnd), _) => () };
    let text_part = select! {
        (Token::Interp(InterpToken::Text(s)), _) => ast::StringPart::Text(s.to_string()),
    };
    let fmt_spec = select! {
        (Token::Interp(InterpToken::FormatSpec(s)), _) => s.clone(),
    }
    .map_with(|s, e| (s, e.span()))
    .or_not();
    let expr_part = expr_start
        .ignore_then(expr)
        .then_ignore(expr_end)
        .then(fmt_spec)
        .validate(|(expr, fmt), _extra, emitter| {
            let parsed_spec = fmt.and_then(|(raw, span)| match parse_format_spec(&raw) {
                Ok(spec) => Some(Spanned::new(spec, Span::new(span.start, span.end))),
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
            let span = Span::new(s.start, s.end);
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
    let pipe = select! { (Token::Op(Op::BitOr), _) => () };
    let or_op = select! { (Token::Op(Op::Or), _) => () };
    let comma = select! { (Token::Comma, _) => () };
    let colon = select! { (Token::Colon, _) => () };
    let thin_arrow = select! { (Token::Op(Op::ThinArrow), _) => () };

    let var_kw = select! {
        (Token::Keyword(Keyword::Var), _) => (),
    }
    .or_not()
    .map(|opt| opt.is_some());

    let as_kw = select! {
        (Token::Keyword(Keyword::As), _) => (),
    }
    .or_not()
    .map(|opt| opt.is_some());

    let lambda_param = var_kw
        .then(identifier())
        .then(
            colon
                .ignore_then(as_kw.then(type_ident()))
                .or_not()
                .map(|opt| match opt {
                    Some((cast_accept, ty)) => (Some(ty), cast_accept),
                    None => (None, false),
                }),
        )
        .map(|((mutable, name), (ty, cast_accept))| ast::LambdaParam {
            name,
            ty,
            mutable,
            cast_accept,
        });

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

    let ret_type = thin_arrow.ignore_then(type_ident()).or_not();

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
            let span = Span::new(s.start, s.end);
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
    let comma = select! { (Token::Comma, _) => () };

    let field_init = choice((
        field_name_ident()
            .then_ignore(select! { (Token::Colon, _) => () })
            .then(expr.clone()),
        identifier().map_with(|name, e| {
            let s = e.span();
            let span = Span::new(s.start, s.end);
            let expr_id = new_expr_id();
            let ident_expr = ast::Expr::new(ast::ExprKind::Ident(name), expr_id);
            let value = Spanned::new(ident_expr, span);
            (name, value)
        }),
    ));

    let tuple_args = select! { (Token::Open(Delimiter::Parent), _) => () }
        .ignore_then(
            expr.separated_by(comma)
                .allow_trailing()
                .collect::<Vec<_>>(),
        )
        .then_ignore(select! { (Token::Close(Delimiter::Parent), _) => () })
        .map(ast::InferredEnumArgs::Tuple);

    let struct_fields = select! { (Token::Open(Delimiter::Brace), _) => () }
        .ignore_then(
            field_init
                .separated_by(comma)
                .allow_trailing()
                .collect::<Vec<_>>(),
        )
        .then_ignore(select! { (Token::Close(Delimiter::Brace), _) => () })
        .map(ast::InferredEnumArgs::Struct);

    select! { (Token::Dot, _) => () }
        .ignore_then(identifier())
        .then(choice((tuple_args, struct_fields)).or_not())
        .map_with(|(variant, args), e| {
            let s = e.span();
            let span = Span::new(s.start, s.end);
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
    let comma = select! { (Token::Comma, _) => () };

    select! { (Token::Hash, _) => () }
        .ignore_then(identifier())
        .then(
            select! { (Token::Open(Delimiter::Parent), _) => () }
                .ignore_then(
                    expr.separated_by(comma)
                        .allow_trailing()
                        .collect::<Vec<_>>(),
                )
                .then_ignore(select! { (Token::Close(Delimiter::Parent), _) => () }),
        )
        .map_with(|(name, args), e| {
            let s = e.span();
            let span = Span::new(s.start, s.end);
            let node = Spanned::new(ast::IntrinsicCall { name, args }, span);
            let expr_id = new_expr_id();
            let expr = ast::Expr::new(ast::ExprKind::IntrinsicCall(node), expr_id);
            Spanned::new(expr, span)
        })
        .labelled("intrinsic call")
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
            let span = Span::new(s.start, s.end);
            let id = new_expr_id();
            let expr = ast::Expr::new(ast::ExprKind::Lit(lit), id);
            Spanned::new(expr, span)
        }),
        struct_literal(expr.clone()),
        array_literal(expr.clone()),
        identifier().map_with(|ident, e| {
            let s = e.span();
            let span = Span::new(s.start, s.end);
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
            let span = Span::new(s.start, s.end);
            let id = new_expr_id();
            let expr = ast::Expr::new(ast::ExprKind::Lit(lit), id);
            Spanned::new(expr, span)
        }),
        array_literal(cond_expr.clone()),
        identifier().map_with(|ident, e| {
            let s = e.span();
            let span = Span::new(s.start, s.end);
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
    Labeled(ast::Ident, ast::ExprNode),
}

fn grouped_or_tuple_expr<'src>(
    expr: impl AnvParser<'src, ast::ExprNode>,
) -> BoxedParser<'src, ast::ExprNode> {
    let comma = select! { (Token::Comma, _) => () };
    let open_paren = select! { (Token::Open(Delimiter::Parent), _) => () };
    let close_paren = select! { (Token::Close(Delimiter::Parent), _) => () };
    let colon = select! { (Token::Colon, _) => () };

    let labeled_elem = identifier()
        .then_ignore(colon)
        .then(expr.clone())
        .map(|(name, e)| TupleExprElem::Labeled(name, e));

    let pos_elem = expr.map(TupleExprElem::Pos);
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
            let expr_id = new_expr_id();

            match validate_tuple_shape_raw(first, rest, trailing_comma.is_some()) {
                TupleShapeResult::Empty => {
                    emitter.emit(Rich::custom(s, "empty tuples are not supported"));
                    let dummy = ast::Expr::new(ast::ExprKind::Lit(ast::Lit::Nil), expr_id);
                    Spanned::new(dummy, span)
                }
                TupleShapeResult::OneTupleError(elem) => {
                    emitter.emit(Rich::custom(s, "1-tuples are not supported"));
                    match elem {
                        TupleExprElem::Pos(e) | TupleExprElem::Labeled(_, e) => e,
                    }
                }
                TupleShapeResult::Grouped(elem) => match elem {
                    TupleExprElem::Pos(e) | TupleExprElem::Labeled(_, e) => e,
                },
                TupleShapeResult::Tuple(elems) => {
                    let all_pos = elems.iter().all(|e| matches!(e, TupleExprElem::Pos(_)));
                    let all_labeled = elems
                        .iter()
                        .all(|e| matches!(e, TupleExprElem::Labeled(_, _)));

                    if all_pos {
                        let exprs: Vec<ast::ExprNode> = elems
                            .into_iter()
                            .map(|e| match e {
                                TupleExprElem::Pos(expr) | TupleExprElem::Labeled(_, expr) => expr,
                            })
                            .collect();
                        let tuple_expr = ast::Expr::new(ast::ExprKind::Tuple(exprs), expr_id);
                        return Spanned::new(tuple_expr, span);
                    }

                    if all_labeled {
                        let fields: Vec<(ast::Ident, ast::ExprNode)> = elems
                            .into_iter()
                            .map(|e| match e {
                                TupleExprElem::Labeled(name, expr) => (name, expr),
                                TupleExprElem::Pos(_) => unreachable!(),
                            })
                            .collect();
                        let named_tuple_expr =
                            ast::Expr::new(ast::ExprKind::NamedTuple(fields), expr_id);
                        return Spanned::new(named_tuple_expr, span);
                    }

                    emitter.emit(Rich::custom(
                        s,
                        "cannot mix labeled and unlabeled elements in tuple literal",
                    ));
                    let dummy = ast::Expr::new(ast::ExprKind::Lit(ast::Lit::Nil), expr_id);
                    Spanned::new(dummy, span)
                }
                TupleShapeResult::UnexpectedComma => {
                    emitter.emit(Rich::custom(s, "unexpected comma"));
                    let dummy = ast::Expr::new(ast::ExprKind::Lit(ast::Lit::Nil), expr_id);
                    Spanned::new(dummy, span)
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
        .map(Option::unwrap_or_default),
    )
    .then_ignore(select! {
        (Token::Close(Delimiter::Parent), _) => (),
    })
    .labelled("function call arguments")
    .as_context()
    .boxed()
}

fn call_generic_args<'src>() -> BoxedParser<'src, Vec<ast::GenericArg>> {
    // lookahead for optional generic arguments (<int, ..>)
    // and rewind to avoid consuming < when its a comparsion op (a < b)
    let generic_lookahead = select! {
        (Token::Op(Op::LessThan), _) => (),
    }
    .ignore_then(
        generic_arg(type_ident())
            .separated_by(select! {
                (Token::Comma, _) => (),
            })
            .allow_trailing()
            .collect::<Vec<_>>(),
    )
    .then_ignore(select! {
        (Token::Op(Op::GreaterThan), _) => (),
    })
    .then_ignore(select! {
        (Token::Open(Delimiter::Parent), _) => (),
    })
    .rewind();

    let generic_list = select! {
        (Token::Op(Op::LessThan), _) => (),
    }
    .ignore_then(
        generic_arg(type_ident())
            .separated_by(select! {
                (Token::Comma, _) => (),
            })
            .allow_trailing()
            .collect::<Vec<_>>(),
    )
    .then_ignore(select! {
        (Token::Op(Op::GreaterThan), _) => (),
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

    let safe_call_suffix = select! { (Token::Question, _) => () }
        .ignore_then(call_generic_args())
        .then(fn_call_args(expr.clone()))
        .map(|(generic_args, args)| PostfixOp::Call {
            generic_args,
            args,
            safe: true,
        });

    let single_index = select! {
        (Token::Dot, _) => (),
    }
    .ignore_then(select! {
        (Token::Literal(LitToken::Number(n)), _) => PostfixOp::TupleIndices(vec![n as u32]),
    });

    let chained_index = select! {
        (Token::Dot, _) => (),
    }
    .ignore_then(select! {
        (Token::Literal(LitToken::Float(s)), _) => s,
    })
    .try_map(|s, span| {
        let parts = s.as_ref().split('.').collect::<Vec<_>>();
        let indices = parts
            .iter()
            .map(|p| p.parse::<u32>())
            .collect::<Result<Vec<_>, _>>();
        indices
            .map(PostfixOp::TupleIndices)
            .map_err(|_| Rich::custom(span, "invalid tuple index"))
    });

    let safe_field_access = select! { (Token::Question, _) => () }
        .ignore_then(select! { (Token::Dot, _) => () })
        .ignore_then(field_name_ident())
        .map(|ident| PostfixOp::Field { ident, safe: true });

    let field_access = select! {
        (Token::Dot, _) => (),
    }
    .ignore_then(field_name_ident())
    .map(|ident| PostfixOp::Field { ident, safe: false });

    let safe_index_suffix = select! { (Token::Question, _) => () }
        .ignore_then(select! { (Token::Open(Delimiter::Bracket), _) => () })
        .ignore_then(expr.clone())
        .then_ignore(select! { (Token::Close(Delimiter::Bracket), _) => () })
        .map(|index_expr| PostfixOp::Index {
            expr: index_expr,
            safe: true,
        });

    let index_suffix = select! { (Token::Open(Delimiter::Bracket), _) => () }
        .ignore_then(expr)
        .then_ignore(select! { (Token::Close(Delimiter::Bracket), _) => () })
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
        let span = Span::new(s.start, s.end);

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
                let start = target.span.start;
                let end = span.end;
                let index_span = Span::new(start, end);

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
    let as_kw = select! { (Token::Keyword(Keyword::As), _) => () };
    unary
        .foldl_with(
            as_kw.ignore_then(type_ident()).repeated(),
            |expr, target, e| {
                let s = e.span();
                let span = Span::new(s.start, s.end);
                let cast_node = Spanned::new(
                    ast::Cast {
                        expr: Box::new(expr),
                        target,
                    },
                    span,
                );
                let id = new_expr_id();
                let node = ast::Expr::new(ast::ExprKind::Cast(cast_node), id);
                Spanned::new(node, span)
            },
        )
        .boxed()
}

fn unary_expr<'src>(expr: impl AnvParser<'src, ast::ExprNode>) -> BoxedParser<'src, ast::ExprNode> {
    select! {
        (Token::Op(Op::Sub), _) => ast::UnaryOp::Neg,
        (Token::Op(Op::Not), _) => ast::UnaryOp::Not,
        (Token::Op(Op::Tilde), _) => ast::UnaryOp::BitNot,
    }
    .repeated()
    .collect::<Vec<_>>()
    .then(expr)
    .map_with(|(ops, expr), e| {
        let s = e.span();
        let span = Span::new(s.start, s.end);

        let mut expr_node = expr;
        for op in ops.into_iter().rev() {
            let unary_node = Spanned::new(
                ast::Unary {
                    op,
                    expr: Box::new(expr_node),
                },
                span,
            );
            let expr_id = new_expr_id();
            expr_node = Spanned::new(
                ast::Expr::new(ast::ExprKind::Unary(unary_node), expr_id),
                span,
            );
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
        (Token::Range, _) => false,
        (Token::RangeEq, _) => true,
    }
    .then(lower.clone())
    .map_with(|(inclusive, end), e| {
        let s = e.span();
        let span = Span::new(s.start, s.end);
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

    let op_rhs_inclusive = select! { (Token::RangeEq, _) => () }
        .ignore_then(lower.clone())
        .map(|end| (true, Some(end)));

    let op_rhs_exclusive = select! { (Token::Range, _) => () }
        .ignore_then(lower.clone().or_not())
        .map(|end| (false, end));

    let op_rhs = choice((op_rhs_inclusive, op_rhs_exclusive));

    let infix_range = lower.foldl_with(op_rhs.repeated(), |start, (inclusive, end), e| {
        if let Some(end) = end {
            let span = Span::new(start.span.start, end.span.end);
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
            let span = Span::new(s.start, s.end);
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
        (Token::Question, _) => (),
    }
    .ignore_then(expr.clone())
    .then_ignore(select! {
        (Token::Colon, _) => (),
    })
    .then(expr);

    lower
        .foldl(ternary_suffix.repeated(), |cond, (then_expr, else_expr)| {
            let span = Span::new(cond.span.start, else_expr.span.end);
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

enum LvalueSuffix {
    Field(ast::Ident),
    TupleIndices(Vec<u32>),
    Index(Box<ast::ExprNode>),
}

fn lvalue_expr<'src>() -> BoxedParser<'src, ast::ExprNode> {
    let base = identifier().map_with(|ident, e| {
        let s = e.span();
        let span = Span::new(s.start, s.end);
        let expr_id = new_expr_id();
        let expr = ast::Expr::new(ast::ExprKind::Ident(ident), expr_id);
        Spanned::new(expr, span)
    });

    let index_atom = choice((
        literal().map_with(|lit, e| {
            let s = e.span();
            let span = Span::new(s.start, s.end);
            let expr_id = new_expr_id();
            let expr = ast::Expr::new(ast::ExprKind::Lit(lit), expr_id);
            Spanned::new(expr, span)
        }),
        identifier().map_with(|ident, e| {
            let s = e.span();
            let span = Span::new(s.start, s.end);
            let expr_id = new_expr_id();
            let expr = ast::Expr::new(ast::ExprKind::Ident(ident), expr_id);
            Spanned::new(expr, span)
        }),
    ));

    let field_suffix = select! { (Token::Dot, _) => () }
        .ignore_then(identifier())
        .map(LvalueSuffix::Field);

    let single_tuple_index = select! {
        (Token::Dot, _) => (),
    }
    .ignore_then(select! {
        (Token::Literal(LitToken::Number(n)), _) => LvalueSuffix::TupleIndices(vec![n as u32]),
    });

    let chained_tuple_index = select! {
        (Token::Dot, _) => (),
    }
    .ignore_then(select! {
        (Token::Literal(LitToken::Float(s)), _) => s,
    })
    .try_map(|s, span| {
        let parts = s.as_ref().split('.').collect::<Vec<_>>();
        let indices = parts
            .iter()
            .map(|p| p.parse::<u32>())
            .collect::<Result<Vec<_>, _>>();
        indices
            .map(LvalueSuffix::TupleIndices)
            .map_err(|_| Rich::custom(span, "invalid tuple index"))
    });

    let index_suffix = select! { (Token::Open(Delimiter::Bracket), _) => () }
        .ignore_then(index_atom)
        .then_ignore(select! { (Token::Close(Delimiter::Bracket), _) => () })
        .map(|e| LvalueSuffix::Index(Box::new(e)));

    let suffix = choice((
        chained_tuple_index,
        single_tuple_index,
        field_suffix,
        index_suffix,
    ));

    base.foldl_with(suffix.repeated(), |target, suf, e| {
        let s = e.span();
        let span = Span::new(s.start, s.end);
        match suf {
            LvalueSuffix::Field(field) => {
                let field_node = Spanned::new(
                    ast::FieldAccess {
                        target: Box::new(target),
                        field,
                        safe: false,
                    },
                    span,
                );
                let expr_id = new_expr_id();
                let expr = ast::Expr::new(ast::ExprKind::Field(field_node), expr_id);
                Spanned::new(expr, span)
            }
            LvalueSuffix::TupleIndices(indices) => {
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
            LvalueSuffix::Index(index_expr) => {
                let index_node = Spanned::new(
                    ast::Index {
                        target: Box::new(target),
                        index: index_expr,
                        safe: false,
                    },
                    span,
                );
                let expr_id = new_expr_id();
                let expr = ast::Expr::new(ast::ExprKind::Index(index_node), expr_id);
                Spanned::new(expr, span)
            }
        }
    })
    .labelled("left value expr")
    .as_context()
    .boxed()
}

fn assignment_expr<'src>(
    expr: impl AnvParser<'src, ast::ExprNode>,
) -> BoxedParser<'src, ast::ExprNode> {
    lvalue_expr()
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

            let expr_id = new_expr_id();
            let expr = ast::Expr::new(ast::ExprKind::Assign(assign_node), expr_id);
            Spanned::new(expr, span)
        })
        .or(expr)
        .labelled("assignment")
        .as_context()
        .boxed()
}
