use crate::{
    ast::{self, ExprKind, Stmt},
    lexer, parser,
    source::{SourceId, SourceKind, SourceTable},
    span::{ByteSpan, SourceSpan},
};

fn tokenize(src: &str) -> (SourceId, lexer::TokenStream) {
    let mut sources = SourceTable::default();
    let source = sources.add(SourceKind::Virtual, "test", None, src);
    let tokens = lexer::tokenize(source, src).expect("lex failed");
    (source, tokens)
}

fn parse_program(src: &str) -> (SourceId, ast::Program) {
    let (source, tokens) = tokenize(src);
    let program = parser::parse_ast(&tokens).expect("parse failed");
    (source, program)
}

fn parse_expr(src: &str) -> ast::ExprNode {
    super::helpers::parse_expr(src)
}

fn binary(expr: &ast::ExprNode) -> &ast::BinaryNode {
    let ExprKind::Binary(binary) = &expr.node.kind else {
        panic!("expected binary expression, found {:?}", expr.node.kind);
    };
    binary
}

#[test]
fn function_span_uses_source_bytes() {
    let src = "// é\nfn main() {}";
    let (_, program) = parse_program(src);
    let Stmt::Func(func) = &program.stmts[0].node else {
        panic!("expected function");
    };

    assert_eq!(func.span, ByteSpan::new(6, src.len()));
    assert_eq!(func.node.body.span, ByteSpan::new(16, src.len()));
}

#[test]
fn method_body_spans_stay_on_block_delimiters() {
    let src = "struct S { fn f(self) { self; } }";
    let (_, program) = parse_program(src);
    let Stmt::Aggregate(aggregate) = &program.stmts[0].node else {
        panic!("expected aggregate");
    };
    let body = &aggregate.node.methods[0].body;

    assert_eq!(body.span, ByteSpan::new(22, 31));
}

#[test]
fn expression_spans_after_multibyte_input_are_bytes() {
    let expr = parse_expr("é + x");
    let binary = binary(&expr);

    assert_eq!(expr.span, ByteSpan::new(0, 6));
    assert_eq!(binary.node.left.span, ByteSpan::new(0, 2));
    assert_eq!(binary.node.right.span, ByteSpan::new(5, 6));
}

#[test]
fn emoji_before_token_keeps_utf8_byte_offsets() {
    let src = "// 😀\nfn main() {}";
    let (_, program) = parse_program(src);
    let Stmt::Func(func) = &program.stmts[0].node else {
        panic!("expected function");
    };

    assert_eq!(func.span, ByteSpan::new(8, src.len()));
}

#[test]
fn nested_expression_span_is_byte_correct() {
    let cases = [
        "foo(é + x).bar[0]",
        "cond ? a + é : b * c",
        "1 + é .. 10",
        "{ let x = é; x }",
        "match é { 1 => x, _ => y }",
        "|é| é + 1",
    ];

    for src in cases {
        let expr = parse_expr(src);
        assert_eq!(expr.span, ByteSpan::new(0, src.len()), "{src}");
    }
}

#[test]
fn unexpected_token_error_uses_token_source_span() {
    let src = "fn main( {}";
    let (source, tokens) = tokenize(src);
    let errors = parser::parse_ast(&tokens).expect_err("expected parse error");

    assert!(
        errors
            .iter()
            .any(|error| *error.span() == SourceSpan::new(source, 9, 10))
    );
}

#[test]
fn eof_error_uses_eof_source_span() {
    let src = "fn";
    let (source, tokens) = tokenize(src);
    let errors = parser::parse_ast(&tokens).expect_err("expected parse error");

    assert!(
        errors
            .iter()
            .any(|error| *error.span() == SourceSpan::empty(source, src.len()))
    );
}
