use std::ops::Range;

use crate::{
    lexer::{SpannedToken, Token},
    typecheck::{TypeErr, TypeErrKind},
};
use ariadne::{Color, Label, Report, ReportKind, Source};
use chumsky::error::{Rich, RichPattern};

pub fn report_lexer_errors(src: &str, file_path: &str, errors: Vec<Rich<'_, char>>) {
    for e in errors {
        let span = e.span();
        let byte_range = span.start..span.end;

        let last_context = last_ctx(&e)
            .map(|s| format!("while lexing a {}", s))
            .unwrap_or_default();
        let (msg_title, msg_body) = if let Some(found_char) = e.found() {
            (
                format!("Unexpected character {}", last_context),
                format!("'{}'", found_char),
            )
        } else {
            (
                format!("Unexpected end of input {}", last_context),
                "end of file".to_string(),
            )
        };

        emit_report(src, file_path, byte_range, msg_title, msg_body);
    }
}

pub fn report_parse_errors(
    src: &str,
    file_path: &str,
    tokens: &[SpannedToken],
    errors: Vec<Rich<SpannedToken>>,
) {
    for e in errors {
        let token_span = e.span();

        let byte_range = token_span_to_byte_range(tokens, token_span.start..token_span.end);

        let last_context = last_ctx(&e)
            .map(|s| format!("while parsing a {}", s))
            .unwrap_or_default();
        let (msg_title, msg_body) = if let Some((found_token, _)) = e.found() {
            let token_desc = describe_token(found_token);
            (format!("Unexpected token {}", last_context), token_desc)
        } else {
            (
                format!("Unexpected end of input {}", last_context),
                "end of file".to_string(),
            )
        };

        emit_report(src, file_path, byte_range, msg_title, msg_body);
    }
}

pub fn report_typecheck_errors(
    src: &str,
    file_path: &str,
    tokens: &[SpannedToken],
    errors: Vec<TypeErr>,
) {
    for e in errors {
        let span = e.span;
        let byte_range = token_span_to_byte_range(tokens, span.start..span.end);

        let (title, body) = match &e.kind {
            TypeErrKind::UnknownVariable { name } => (
                format!("Unknown variable '{name}'"),
                "This variable is not in scope".to_string(),
            ),
            TypeErrKind::UnknownFunction { name } => (
                format!("Unknown function '{name}'"),
                "This function is not defined in this scope".to_string(),
            ),
            TypeErrKind::MismatchedTypes { expected, found } => (
                "Mismatched types".to_string(),
                format!("expected '{expected}', found '{found}'"),
            ),
            TypeErrKind::InvalidOperand { op, operand_type } => (
                "Invalid operand type".to_string(),
                format!("operator '{op}' cannot be applied to '{operand_type}'"),
            ),
            TypeErrKind::NotAFunction { expr_type } => (
                "Not a function".to_string(),
                format!("expression of type '{expr_type}' is not callable"),
            ),
            TypeErrKind::UnresolvedInfer => (
                "Could not infer type".to_string(),
                "type inference could not resolve this expression".to_string(),
            ),
            TypeErrKind::GenericArgNumMismatch { expected, found } => (
                "Wrong number of type arguments".to_string(),
                format!("expected {expected} type argument(s), found {found}"),
            ),
            TypeErrKind::NotGenericFunction => (
                "Function is not generic".to_string(),
                "type arguments cannot be provided for a non-generic function".to_string(),
            ),
        };

        emit_report(src, file_path, byte_range, title, body);
    }
}

fn token_span_to_byte_range(tokens: &[SpannedToken], span: Range<usize>) -> Range<usize> {
    let start_byte = tokens.get(span.start).map(|(_, s)| s.start).unwrap_or(0);
    let end_byte = tokens
        .get(span.end)
        .map(|(_, s)| s.end.saturating_sub(1))
        .unwrap_or(start_byte);

    start_byte..end_byte
}

fn last_ctx<T>(ctx: &Rich<'_, T>) -> Option<String> {
    ctx.contexts()
        .filter_map(|(pat, span)| match pat {
            RichPattern::Label(s) => Some((s.as_ref(), span)),
            _ => None,
        })
        .last()
        .map(|(s, _)| s.to_string())
}

fn emit_report(src: &str, file_path: &str, range: Range<usize>, title: String, body: String) {
    let report = Report::build(ReportKind::Error, (file_path, range.clone()))
        .with_message(title)
        .with_label(
            Label::new((file_path, range.clone()))
                .with_color(Color::Red)
                .with_message(body),
        );
    let _ = report.finish().print((file_path, Source::from(src)));
}

fn describe_token(token: &Token) -> String {
    match token {
        Token::Keyword(keyword) => format!("'{}' keyword", keyword),
        Token::Open(..) | Token::Close(..) => format!("'{}' delimiter", token),
        Token::Ident(_) => "identifier".to_string(),
        Token::Literal(lit) => format!("'{}' literal", lit),
        Token::Op(op) => format!("'{}' operator", op),
        _ => format!("'{}' token", token),
    }
}
