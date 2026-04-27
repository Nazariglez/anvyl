use chumsky::{extra::SimpleState, prelude::*};

use crate::{
    ast, lexer,
    parser::{
        ParserState,
        expr::expression,
        parser,
        stmt::statement,
        types::{param_type_ident, type_ident},
    },
};

pub(super) fn parse_expr(src: &str) -> ast::ExprNode {
    let tokens =
        lexer::tokenize(src).unwrap_or_else(|errs| panic!("failed to tokenize '{src}': {errs:?}"));
    let stmt_parser = statement();
    let expr_parser = expression(stmt_parser.clone()).then_ignore(end());
    let mut state = SimpleState(ParserState::default());
    expr_parser
        .parse_with_state(&tokens, &mut state)
        .into_result()
        .unwrap_or_else(|errs| panic!("failed to parse '{src}': {errs:?}"))
}

pub(super) fn parse_program(src: &str) -> ast::Program {
    let tokens =
        lexer::tokenize(src).unwrap_or_else(|errs| panic!("failed to tokenize '{src}': {errs:?}"));
    let mut state = SimpleState(ParserState::default());
    parser()
        .parse_with_state(&tokens, &mut state)
        .into_result()
        .unwrap_or_else(|errs| panic!("failed to parse '{src}': {errs:?}"))
}

pub(super) fn parse_program_err(src: &str) {
    let Ok(tokens) = lexer::tokenize(src) else {
        return; // lex error is also acceptable
    };
    let mut state = SimpleState(ParserState::default());
    let result = parser().parse_with_state(&tokens, &mut state).into_result();
    assert!(
        result.is_err(),
        "expected parse error for '{src}' but it succeeded"
    );
}

pub(super) fn parse_type(src: &str) -> ast::Type {
    let tokens = lexer::tokenize(src)
        .unwrap_or_else(|errs| panic!("failed to tokenize type '{src}': {errs:?}"));
    let mut state = SimpleState(ParserState::default());
    type_ident()
        .then_ignore(end())
        .parse_with_state(&tokens, &mut state)
        .into_result()
        .unwrap_or_else(|errs| panic!("failed to parse type '{src}': {errs:?}"))
}

pub(super) fn parse_param_type(src: &str) -> ast::Type {
    let tokens = lexer::tokenize(src)
        .unwrap_or_else(|errs| panic!("failed to tokenize type '{src}': {errs:?}"));
    let mut state = SimpleState(ParserState::default());
    param_type_ident()
        .then_ignore(end())
        .parse_with_state(&tokens, &mut state)
        .into_result()
        .unwrap_or_else(|errs| panic!("failed to parse param type '{src}': {errs:?}"))
}

pub(super) fn expect_nominal<'a>(ty: &'a ast::Type, expected: &str) -> &'a [ast::GenericArg] {
    match ty {
        ast::Type::UnresolvedNominal {
            name, generic_args, ..
        } => {
            assert_eq!(name.0.as_ref(), expected);
            generic_args
        }
        other => panic!("expected nominal {expected}, found {other:?}"),
    }
}

pub(super) fn expect_binary(
    expr: &ast::ExprNode,
    op: ast::BinaryOp,
) -> (&ast::ExprNode, &ast::ExprNode) {
    match &expr.node().kind {
        ast::ExprKind::Binary(bin_node) => {
            let binary = bin_node.node();
            assert_eq!(
                binary.op, op,
                "expected binary op {:?}, found {:?}",
                op, binary.op
            );
            (binary.left.as_ref(), binary.right.as_ref())
        }
        other => panic!("expected binary op {op:?}, found {other:?}"),
    }
}

pub(super) fn expect_range(
    expr: &ast::ExprNode,
    inclusive: bool,
) -> (&ast::ExprNode, &ast::ExprNode) {
    match &expr.node().kind {
        ast::ExprKind::Range(range_node) => {
            let ast::Range::Bounded {
                start,
                end,
                inclusive: found_inclusive,
            } = range_node.node()
            else {
                panic!("expected bounded range, found open-ended range");
            };
            assert_eq!(
                *found_inclusive, inclusive,
                "expected inclusive={inclusive}, found {found_inclusive}",
            );
            (start, end)
        }
        other => panic!("expected range expr, found {other:?}"),
    }
}

pub(super) fn expect_ident(expr: &ast::ExprNode, name: &str) {
    match &expr.node().kind {
        ast::ExprKind::Ident(ident) => {
            assert_eq!(ident.0.as_ref(), name, "expected ident '{name}'");
        }
        other => panic!("expected ident '{name}', found {other:?}"),
    }
}

pub(super) fn expect_int(expr: &ast::ExprNode, value: i64) {
    match &expr.node().kind {
        ast::ExprKind::Lit(ast::Lit::Int(v)) => {
            assert_eq!(v, &value, "expected int literal {value}");
        }
        other => panic!("expected int literal {value}, found {other:?}"),
    }
}

pub(super) fn expect_float(expr: &ast::ExprNode, value: f64) {
    match &expr.node.kind {
        ast::ExprKind::Lit(ast::Lit::Float(v)) => {
            assert_eq!(*v, value, "expected float literal {value}");
        }
        other => panic!("expected float literal {value}, found {other:?}"),
    }
}

pub(super) fn expect_unary(expr: &ast::ExprNode, op: ast::UnaryOp) -> &ast::ExprNode {
    match &expr.node.kind {
        ast::ExprKind::Unary(node) => {
            assert_eq!(node.node.op, op, "expected unary op {op:?}");
            node.node.expr.as_ref()
        }
        other => panic!("expected unary expression, found {other:?}"),
    }
}

pub(super) fn expect_field<'a>(
    expr: &'a ast::ExprNode,
    name: &str,
    safe: bool,
) -> &'a ast::ExprNode {
    match &expr.node.kind {
        ast::ExprKind::Field(field_node) => {
            let node = field_node.node();
            assert_eq!(node.field.0.as_ref(), name, "expected field '{name}'");
            assert_eq!(
                node.safe, safe,
                "expected field '{name}' safe={safe}, found {}",
                node.safe
            );
            node.target.as_ref()
        }
        other => panic!("expected field access '{name}', found {other:?}"),
    }
}

pub(super) fn expect_index(expr: &ast::ExprNode, safe: bool) -> (&ast::ExprNode, &ast::ExprNode) {
    match &expr.node.kind {
        ast::ExprKind::Index(index_node) => {
            let node = index_node.node();
            assert_eq!(
                node.safe, safe,
                "expected index safe={safe}, found {}",
                node.safe
            );
            (node.target.as_ref(), node.index.as_ref())
        }
        other => panic!("expected index expr, found {other:?}"),
    }
}

pub(super) fn expect_tuple_index(expr: &ast::ExprNode, value: u32) -> &ast::ExprNode {
    match &expr.node.kind {
        ast::ExprKind::TupleIndex(index_node) => {
            assert_eq!(
                index_node.node.index, value,
                "expected tuple index {value}, found {}",
                index_node.node.index
            );
            index_node.node.target.as_ref()
        }
        other => panic!("expected tuple index {value}, found {other:?}"),
    }
}

pub(super) fn expect_call(expr: &ast::ExprNode, safe: bool) -> (&ast::ExprNode, &[ast::ExprNode]) {
    let (func, args, _) = expect_generic_call(expr, safe);
    (func, args)
}

pub(super) fn expect_generic_call(
    expr: &ast::ExprNode,
    safe: bool,
) -> (&ast::ExprNode, &[ast::ExprNode], &[ast::GenericArg]) {
    match &expr.node.kind {
        ast::ExprKind::Call(call_node) => {
            let node = call_node.node();
            assert_eq!(
                node.safe, safe,
                "expected call safe={safe}, found {}",
                node.safe
            );
            (
                node.func.as_ref(),
                node.args.as_slice(),
                node.generic_args.as_slice(),
            )
        }
        other => panic!("expected call expr, found {other:?}"),
    }
}

pub(super) fn expect_array_literal(expr: &ast::ExprNode) -> &[ast::ExprNode] {
    match &expr.node.kind {
        ast::ExprKind::ArrayLiteral(lit) => &lit.node.elements,
        other => panic!("expected ArrayLiteral, found {other:?}"),
    }
}

pub(super) fn expect_array_fill(expr: &ast::ExprNode) -> (&ast::ExprNode, &ast::ExprNode) {
    match &expr.node.kind {
        ast::ExprKind::ArrayFill(fill) => (&fill.node.value, &fill.node.len),
        other => panic!("expected ArrayFill, found {other:?}"),
    }
}

pub(super) fn expect_map_literal(expr: &ast::ExprNode) -> &[(ast::ExprNode, ast::ExprNode)] {
    match &expr.node.kind {
        ast::ExprKind::MapLiteral(lit) => &lit.node.entries,
        other => panic!("expected MapLiteral, found {other:?}"),
    }
}

pub(super) fn expect_string(expr: &ast::ExprNode, value: &str) {
    match &expr.node.kind {
        ast::ExprKind::Lit(ast::Lit::String(s)) => {
            assert_eq!(s, value, "expected string literal '{value}'");
        }
        other => panic!("expected string literal '{value}', found {other:?}"),
    }
}

pub(super) fn expect_string_interp(expr: &ast::ExprNode) -> &[ast::StringPart] {
    match &expr.node.kind {
        ast::ExprKind::StringInterp(parts) => parts.as_slice(),
        other => panic!("expected StringInterp, found {other:?}"),
    }
}

pub(super) fn expect_cast(expr: &ast::ExprNode) -> (&ast::ExprNode, &ast::Type) {
    match &expr.node.kind {
        ast::ExprKind::Cast(node) => (node.node.expr.as_ref(), &node.node.target),
        other => panic!("expected Cast, found {other:?}"),
    }
}

pub(super) fn expect_intrinsic_call<'a>(
    expr: &'a ast::ExprNode,
    name: &str,
) -> &'a [ast::ExprNode] {
    match &expr.node.kind {
        ast::ExprKind::IntrinsicCall(node) => {
            assert_eq!(
                node.node.name.0.as_ref(),
                name,
                "expected intrinsic '#{name}'"
            );
            &node.node.args
        }
        other => panic!("expected IntrinsicCall '#{name}', found {other:?}"),
    }
}
