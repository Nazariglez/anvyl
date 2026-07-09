use chumsky::{extra::SimpleState, prelude::*};

use crate::{
    ast, lexer,
    parser::{ParserState, expr::expression, parser, stmt::statement, token_input},
    source::{SourceKind, SourceTable},
};

fn tokens(src: &str) -> lexer::TokenStream {
    let mut sources = SourceTable::default();
    let source = sources.add(SourceKind::Virtual, "test", None, src);
    lexer::tokenize(source, src)
        .unwrap_or_else(|errs| panic!("failed to tokenize '{src}': {errs:?}"))
}

pub(super) fn parse_expr(src: &str) -> ast::ExprNode {
    let tokens = tokens(src);
    let stmt_parser = statement();
    let expr_parser = expression(stmt_parser.clone()).then_ignore(end());
    let mut state = SimpleState(ParserState::default());
    expr_parser
        .parse_with_state(token_input(&tokens), &mut state)
        .into_result()
        .unwrap_or_else(|errs| panic!("failed to parse '{src}': {errs:?}"))
}

pub(super) fn parse_program(src: &str) -> ast::Program {
    let tokens = tokens(src);
    let mut state = SimpleState(ParserState::default());
    parser()
        .parse_with_state(token_input(&tokens), &mut state)
        .into_result()
        .unwrap_or_else(|errs| panic!("failed to parse '{src}': {errs:?}"))
}

pub(super) fn parse_program_err(src: &str) {
    let Ok(tokens) = try_tokens(src) else {
        return;
    };
    let mut state = SimpleState(ParserState::default());
    let result = parser()
        .parse_with_state(token_input(&tokens), &mut state)
        .into_result();
    assert!(
        result.is_err(),
        "expected parse error for '{src}' but it succeeded"
    );
}

pub(super) fn first_import(src: &str) -> ast::Import {
    let prog = parse_program(src);
    let ast::Stmt::Import(node) = &prog.stmts[0].node else {
        panic!("expected Import statement, found {:?}", prog.stmts[0].node);
    };
    node.node.clone()
}

pub(super) fn assert_named_path(path: &ast::PackageModulePath, expected: &[&str]) {
    let ast::PackageModulePath::Named(path) = path else {
        panic!("expected named path, found {path:?}");
    };
    assert_eq!(
        path.iter().map(ast::Ident::as_str).collect::<Vec<_>>(),
        expected
    );
}

fn try_tokens(src: &str) -> Result<lexer::TokenStream, Vec<Rich<'_, char>>> {
    let mut sources = SourceTable::default();
    let source = sources.add(SourceKind::Virtual, "test", None, src);
    lexer::tokenize(source, src)
}
