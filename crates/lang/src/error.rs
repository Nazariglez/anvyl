use std::ops::Range;

use crate::{
    lexer::{SpannedToken, Token},
    typecheck::{TypeErr, TypeErrKind},
};
use ariadne::{Color, Label, Report, ReportKind, Source};
use chumsky::error::{Rich, RichPattern, RichReason};

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

        let custom_msg = match e.reason() {
            RichReason::Custom(msg) => Some(msg.to_string()),
            _ => None,
        };

        let last_context = last_ctx(&e)
            .map(|s| format!("while parsing a {}", s))
            .unwrap_or_default();

        let (msg_title, msg_body) = if let Some(msg) = custom_msg {
            (msg, String::new())
        } else if let Some((found_token, _)) = e.found() {
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
            TypeErrKind::IfConditionNotBool { found } => (
                "Condition of if expression must be bool".to_string(),
                format!("found '{found}'"),
            ),
            TypeErrKind::IfMissingElse => (
                "if expression used as value must have an else branch".to_string(),
                "add an else branch to use this if expression as a value".to_string(),
            ),
            TypeErrKind::WhileConditionNotBool { found } => (
                "Condition of while must be bool".to_string(),
                format!("found '{found}'"),
            ),
            TypeErrKind::BreakOutsideLoop => (
                "break outside of loop".to_string(),
                "break can only be used inside a loop".to_string(),
            ),
            TypeErrKind::ContinueOutsideLoop => (
                "continue outside of loop".to_string(),
                "continue can only be used inside a loop".to_string(),
            ),
            TypeErrKind::TupleIndexOnNonTuple { found, index } => (
                format!("cannot index non-tuple type with .{index}"),
                format!("expression has type '{found}', which is not a tuple"),
            ),
            TypeErrKind::TupleIndexOutOfBounds {
                tuple_type,
                index,
                len,
            } => (
                format!("tuple index {index} is out of bounds"),
                format!(
                    "tuple type '{tuple_type}' has {len} elements (indices 0..{})",
                    len - 1
                ),
            ),
            TypeErrKind::TuplePatternArityMismatch { expected, found } => (
                "tuple pattern arity mismatch".to_string(),
                format!("expected {expected} elements, found {found}"),
            ),
            TypeErrKind::NonTupleInTuplePattern {
                found,
                pattern_arity,
            } => (
                format!("cannot destructure non-tuple type with {pattern_arity}-element pattern"),
                format!("expression has type '{found}', which is not a tuple"),
            ),
            TypeErrKind::TuplePatternLabelMismatch { expected, found } => (
                "tuple pattern label mismatch".to_string(),
                format!("expected label '{expected}', found '{found}'"),
            ),
            TypeErrKind::NamedPatternOnPositionalTuple => (
                "cannot use named tuple pattern on positional tuple".to_string(),
                "use positional pattern '(a, b, ...)' instead".to_string(),
            ),
            TypeErrKind::DuplicateTupleLabel { label } => (
                format!("duplicate field '{label}' in named tuple"),
                "each field label must be unique".to_string(),
            ),
            TypeErrKind::NoSuchFieldOnTuple { field, tuple_type } => (
                format!("tuple has no field '{field}'"),
                format!("type '{tuple_type}' does not contain a field named '{field}'"),
            ),
            TypeErrKind::FieldAccessOnNonNamedTuple { field, found } => (
                format!("cannot access field '{field}' on non-named tuple"),
                format!("type '{found}' has no named fields; use '.0', '.1', ... instead"),
            ),
            TypeErrKind::UnknownStruct { name } => (
                format!("Unknown struct '{name}'"),
                "This struct is not defined in this scope".to_string(),
            ),
            TypeErrKind::StructMissingField { struct_name, field } => (
                format!("Missing field '{field}' in struct literal"),
                format!("struct '{struct_name}' requires field '{field}'"),
            ),
            TypeErrKind::StructUnknownField { struct_name, field } => (
                format!("Unknown field '{field}' for struct '{struct_name}'"),
                format!("struct '{struct_name}' has no field named '{field}'"),
            ),
            TypeErrKind::StructDuplicateField { struct_name, field } => (
                format!("Duplicate field '{field}' in struct literal"),
                format!("field '{field}' is specified more than once in '{struct_name}'"),
            ),
            TypeErrKind::UnknownMethod {
                struct_name,
                method,
            } => (
                format!("Unknown method '{method}' for struct '{struct_name}'"),
                format!("struct '{struct_name}' has no method named '{method}'"),
            ),
            TypeErrKind::StaticMethodOnValue {
                struct_name,
                method,
            } => (
                format!("Cannot call static method '{method}' on a value"),
                format!("'{method}' is a static method; call it as '{struct_name}.{method}(...)'"),
            ),
            TypeErrKind::InstanceMethodOnType {
                struct_name,
                method,
            } => (
                format!("Cannot call instance method '{method}' on a type"),
                format!("'{method}' requires an instance of '{struct_name}'; create one first"),
            ),
            TypeErrKind::ReadonlySelfMutation { struct_name, field } => (
                format!("Cannot assign to field '{field}' in readonly method"),
                format!(
                    "'self' is readonly in this method of '{struct_name}'; use '&self' for mutating methods"
                ),
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
