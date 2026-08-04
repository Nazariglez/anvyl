mod printer;
mod trivia;

use anvyx_frontend::{
    lexer::{self, Token},
    parser,
    source::{SourceKind, SourceTable},
    span::SourceSpan,
};
use chumsky::error::{Rich, RichPattern};

pub enum FormatError {
    Lex(String),
    Parse(String),
    Directive(String),
}

impl std::fmt::Display for FormatError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FormatError::Lex(msg) => write!(f, "Lex error: {msg}"),
            FormatError::Parse(msg) => write!(f, "Parse error: {msg}"),
            FormatError::Directive(msg) => write!(f, "Formatter directive error: {msg}"),
        }
    }
}

impl std::fmt::Debug for FormatError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self, f)
    }
}

fn format_lex_errors(errors: &[Rich<'_, char>]) -> String {
    errors
        .iter()
        .take(5)
        .map(|e| {
            let span = e.span();
            let found = e
                .found()
                .map_or("end of input".to_string(), |c| format!("'{c}'"));
            let context = extract_context_label(e);
            let prefix = if context.is_empty() {
                String::new()
            } else {
                format!(" while {context}")
            };
            format!(
                "byte {}..{}: unexpected {found}{prefix}",
                span.start, span.end
            )
        })
        .collect::<Vec<_>>()
        .join("\n")
}

fn format_parse_errors(errors: &[Rich<'_, Token, SourceSpan>]) -> String {
    errors
        .iter()
        .take(5)
        .map(|e| {
            let found = e
                .found()
                .map_or("end of input".to_string(), |tok| format!("{tok:?}"));
            let context = extract_context_label(e);
            let prefix = if context.is_empty() {
                String::new()
            } else {
                format!(" while {context}")
            };
            format!("unexpected {found}{prefix}")
        })
        .collect::<Vec<_>>()
        .join("\n")
}

fn extract_context_label<T, S>(rich: &Rich<'_, T, S>) -> String {
    rich.contexts()
        .filter_map(|(pat, _)| match pat {
            RichPattern::Label(s) => Some(s.to_string()),
            _ => None,
        })
        .last()
        .unwrap_or_default()
}

pub fn format_source(source: &str) -> Result<String, FormatError> {
    let mut sources = SourceTable::default();
    let source_id = sources.add(SourceKind::Virtual, "formatter", None, source);
    let tokens = lexer::tokenize(source_id, source)
        .map_err(|errors| FormatError::Lex(format_lex_errors(&errors)))?;

    let ast = parser::parse_ast(&tokens)
        .map_err(|errors| FormatError::Parse(format_parse_errors(&errors)))?;

    let trivia = trivia::scan_trivia(source, &tokens.tokens);
    let mut printer = printer::Printer::new(source, &trivia, &tokens.tokens);
    printer
        .format_program(&ast)
        .map_err(|error| FormatError::Directive(error.to_string()))?;
    Ok(printer.finish())
}
