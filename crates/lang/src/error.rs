use std::ops::Range;

use crate::lexer::{SpannedToken, Token};
use ariadne::{Color, Label, Report, ReportKind, Source};
use chumsky::error::{Rich, RichPattern};

pub fn report_lexer_errors(src: &str, errors: Vec<Rich<'_, char>>) {
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

        emit_report(src, byte_range, msg_title, msg_body);
    }
}

pub fn report_parse_errors(src: &str, tokens: &[SpannedToken], errors: Vec<Rich<SpannedToken>>) {
    for e in errors {
        let token_span = e.span();

        let start_byte = tokens
            .get(token_span.start)
            .map(|(_, span)| span.start)
            .unwrap_or(0);
        let end_byte = tokens
            .get(token_span.end)
            .map(|(_, span)| span.end.saturating_sub(1))
            .unwrap_or(start_byte);

        let byte_range = start_byte..end_byte;

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

        emit_report(src, byte_range, msg_title, msg_body);
    }
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

fn emit_report(src: &str, range: Range<usize>, title: String, body: String) {
    let report = Report::build(ReportKind::Error, range.clone())
        .with_message(title)
        .with_label(
            Label::new(range.clone())
                .with_color(Color::Red)
                .with_message(body),
        );
    let _ = report.finish().print(Source::from(src));
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
