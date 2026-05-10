use crate::{
    config::{CompilationContext, PredicateError},
    span::ByteSpan,
};

struct Frame {
    parent_active: bool,
    active: bool,
    taken: bool,
    else_state: ElseState,
    if_span: ByteSpan,
}

#[derive(PartialEq, Eq)]
enum ElseState {
    Open,
    Seen,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct ConditionalError {
    pub message: String,
    pub label: String,
    pub span: ByteSpan,
}

impl ConditionalError {
    fn new(message: impl Into<String>, label: impl Into<String>, span: ByteSpan) -> Self {
        Self {
            message: message.into(),
            label: label.into(),
            span,
        }
    }
}

pub(crate) fn filter_with_context(
    source: &str,
    ctx: &CompilationContext,
) -> Result<String, Vec<ConditionalError>> {
    let mut out = String::with_capacity(source.len());
    let mut stack: Vec<Frame> = vec![];
    let mut errors = vec![];
    let mut line_start = 0;

    for line in source.split_inclusive('\n') {
        let trimmed = line.trim_start();
        let directive_start = line_start + line.len() - trimmed.len();
        match directive(trimmed, directive_start) {
            Some(Directive::If(cond)) => {
                let active_cond = eval_condition(cond, ctx, &mut errors).unwrap_or(false);
                let parent_active = active(&stack);
                stack.push(Frame {
                    parent_active,
                    active: parent_active && active_cond,
                    taken: active_cond,
                    else_state: ElseState::Open,
                    if_span: cond.directive_span,
                });
                mask_line(line, &mut out);
            }
            Some(Directive::Elif(cond)) => {
                match stack.last_mut() {
                    Some(frame) if frame.else_state == ElseState::Seen => {
                        errors.push(ConditionalError::new(
                            "#elif after #else",
                            "#elif after #else",
                            cond.directive_span,
                        ));
                    }
                    Some(frame) => {
                        let active_cond = eval_condition(cond, ctx, &mut errors).unwrap_or(false);
                        frame.active = frame.parent_active && !frame.taken && active_cond;
                        frame.taken |= active_cond;
                    }
                    None => errors.push(ConditionalError::new(
                        "#elif without matching #if",
                        "unmatched #elif",
                        cond.directive_span,
                    )),
                }
                mask_line(line, &mut out);
            }
            Some(Directive::Else { span }) => {
                match stack.last_mut() {
                    Some(frame) if frame.else_state == ElseState::Seen => {
                        errors.push(ConditionalError::new(
                            "duplicate #else",
                            "duplicate #else",
                            span,
                        ));
                    }
                    Some(frame) => {
                        frame.active = frame.parent_active && !frame.taken;
                        frame.taken = true;
                        frame.else_state = ElseState::Seen;
                    }
                    None => errors.push(ConditionalError::new(
                        "#else without matching #if",
                        "unmatched #else",
                        span,
                    )),
                }
                mask_line(line, &mut out);
            }
            Some(Directive::End { span }) => {
                if stack.pop().is_none() {
                    errors.push(ConditionalError::new(
                        "#end without matching #if",
                        "unmatched #end",
                        span,
                    ));
                }
                mask_line(line, &mut out);
            }
            None if active(&stack) => out.push_str(line),
            None => mask_line(line, &mut out),
        }
        line_start += line.len();
    }

    for frame in stack {
        errors.push(ConditionalError::new(
            "unterminated #if",
            "unterminated #if",
            frame.if_span,
        ));
    }

    debug_assert_eq!(out.len(), source.len());

    if errors.is_empty() {
        Ok(out)
    } else {
        Err(errors)
    }
}

fn active(stack: &[Frame]) -> bool {
    stack.last().is_none_or(|frame| frame.active)
}

fn mask_line(line: &str, out: &mut String) {
    for byte in line.bytes() {
        match byte {
            b'\r' | b'\n' => out.push(byte as char),
            _ => out.push(' '),
        }
    }
}

#[derive(Clone, Copy)]
struct Condition<'a> {
    text: &'a str,
    keyword: &'static str,
    directive_span: ByteSpan,
    span: ByteSpan,
}

enum Directive<'a> {
    If(Condition<'a>),
    Elif(Condition<'a>),
    Else { span: ByteSpan },
    End { span: ByteSpan },
}

fn directive(line: &str, start: usize) -> Option<Directive<'_>> {
    let line = line.trim_end();
    if let Some(cond) = directive_condition(line, start, "#if") {
        return Some(Directive::If(cond));
    }
    if let Some(cond) = directive_condition(line, start, "#elif") {
        return Some(Directive::Elif(cond));
    }
    if line == "#else" {
        return Some(Directive::Else {
            span: ByteSpan::new(start, start + "#else".len()),
        });
    }
    if line == "#end" {
        return Some(Directive::End {
            span: ByteSpan::new(start, start + "#end".len()),
        });
    }
    None
}

fn directive_condition<'a>(
    line: &'a str,
    start: usize,
    keyword: &'static str,
) -> Option<Condition<'a>> {
    let rest = line.strip_prefix(keyword)?;
    let directive_span = ByteSpan::new(start, start + keyword.len());
    if rest.is_empty() {
        let span = ByteSpan::empty(directive_span.end);
        return Some(Condition {
            text: "",
            keyword,
            directive_span,
            span,
        });
    }
    if !rest.chars().next().is_some_and(char::is_whitespace) {
        return None;
    }
    let condition = rest.trim();
    let leading = rest.len() - rest.trim_start().len();
    let condition_start = start + keyword.len() + leading;
    let span = ByteSpan::new(condition_start, condition_start + condition.len());
    Some(Condition {
        text: condition,
        keyword,
        directive_span,
        span,
    })
}

fn eval_condition(
    cond: Condition<'_>,
    ctx: &CompilationContext,
    errors: &mut Vec<ConditionalError>,
) -> Option<bool> {
    if cond.text.is_empty() {
        errors.push(missing_condition_error(cond));
        return None;
    }

    let Some(open) = cond.text.find('(') else {
        errors.push(ConditionalError::new(
            "unexpected token in conditional directive",
            "malformed condition",
            cond.span,
        ));
        return None;
    };
    if !cond.text.ends_with(')') || cond.text[open + 1..cond.text.len() - 1].contains(['(', ')']) {
        errors.push(ConditionalError::new(
            "unexpected token in conditional directive",
            "malformed condition",
            cond.span,
        ));
        return None;
    }

    let pred_text = &cond.text[..open];
    let arg_text = &cond.text[open + 1..cond.text.len() - 1];
    let pred = pred_text.trim();
    let arg = arg_text.trim();
    let pred_span = trimmed_span(cond.span.start, pred_text);
    let arg_span = trimmed_span(cond.span.start + open + 1, arg_text);
    if pred.is_empty() || arg.is_empty() {
        errors.push(missing_condition_error(cond));
        return None;
    }

    match ctx.eval_predicate(pred, arg) {
        Ok(value) => Some(value),
        Err(PredicateError::UnknownPredicate) => {
            errors.push(ConditionalError::new(
                "unknown conditional predicate",
                "unknown predicate",
                pred_span,
            ));
            None
        }
        Err(PredicateError::UnknownValue) => {
            errors.push(ConditionalError::new(
                format!("unknown {pred}"),
                "unknown predicate value",
                arg_span,
            ));
            None
        }
    }
}

fn missing_condition_error(cond: Condition<'_>) -> ConditionalError {
    ConditionalError::new(
        format!("expected condition after {}", cond.keyword),
        "missing condition",
        cond.span,
    )
}

fn trimmed_span(start: usize, text: &str) -> ByteSpan {
    let trimmed = text.trim();
    let leading = text.len() - text.trim_start().len();
    ByteSpan::new(start + leading, start + leading + trimmed.len())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn filter(source: &str) -> Result<String, Vec<ConditionalError>> {
        filter_with_context(source, &CompilationContext::default())
    }

    fn filtered(source: &str) -> String {
        filter(source).expect("filter failed")
    }

    fn errors(source: &str) -> Vec<ConditionalError> {
        filter(source).unwrap_err()
    }

    fn assert_masked(out: &str, source: &str, text: &str) {
        let start = source.find(text).expect("missing segment");
        let end = start + text.len();
        assert!(out[start..end].bytes().all(|byte| byte == b' '));
    }

    fn assert_error(source: &str, message: &str, label: &str, slice: &str) {
        let error = errors(source)
            .into_iter()
            .find(|error| error.message == message)
            .expect("expected conditional error");
        assert_eq!(error.label, label);
        assert_eq!(&source[error.span.start..error.span.end], slice);
    }

    #[test]
    fn masks_inactive_branch_without_shifting_offsets() {
        let source = "fn main() {\n#if profile(release)\nbroken();\n#else\nok();\n#end\n}\n";
        let out = filtered(source);
        assert_eq!(out.len(), source.len());
        assert_masked(&out, source, "#if profile(release)");
        assert_masked(&out, source, "broken();");
        assert_masked(&out, source, "#else");
        assert_masked(&out, source, "#end");
        assert!(out.contains("ok();"));
    }

    #[test]
    fn masks_multibyte_inactive_bytes_and_preserves_crlf() {
        let source = "#if profile(release)\r\né();\r\n#else\r\nok();\r\n#end\r\n";
        let out = filtered(source);

        assert_eq!(out.len(), source.len());
        assert_eq!(out.matches("\r\n").count(), source.matches("\r\n").count());
        assert_masked(&out, source, "é();");
        assert!(out.contains("ok();"));
    }

    #[test]
    fn filtered_spans_still_slice_original_source() {
        let source = "#if profile(release)\nééé();\n#end\nfn main( {}\n";
        let out = filtered(source);
        let mut sources = crate::source::SourceTable::default();
        let source_id = sources.add(crate::source::SourceKind::Virtual, "test", None, source);
        let tokens = crate::lexer::tokenize(source_id, &out).expect("lex failed");
        let fn_span = tokens.tokens[0].1.byte();
        let errors = crate::parser::parse_ast(&tokens).expect_err("expected parse error");

        assert_eq!(&source[fn_span.start..fn_span.end], "fn");
        assert!(errors.iter().any(|error| {
            let span = error.span().byte();
            &source[span.start..span.end] == "{"
        }));
    }

    #[test]
    fn validates_inactive_branch_structure() {
        assert_error(
            "#if profile(release)\n#else\n#else\n#end\n",
            "duplicate #else",
            "duplicate #else",
            "#else",
        );
    }

    #[test]
    fn spans_unmatched_and_unterminated_directives() {
        assert_error(
            "  #elif profile(debug)\n",
            "#elif without matching #if",
            "unmatched #elif",
            "#elif",
        );
        assert_error(
            "#else\n",
            "#else without matching #if",
            "unmatched #else",
            "#else",
        );
        assert_error(
            "#end\n",
            "#end without matching #if",
            "unmatched #end",
            "#end",
        );
        assert_error(
            "#if profile(debug)\n",
            "unterminated #if",
            "unterminated #if",
            "#if",
        );
    }

    #[test]
    fn ignores_hash_prefixed_identifiers() {
        let source = "#ifdef profile(debug)\n";
        assert_eq!(filtered(source), source);
    }

    #[test]
    fn rejects_unknown_predicate() {
        assert_error(
            "#if platform(macos)\n#end\n",
            "unknown conditional predicate",
            "unknown predicate",
            "platform",
        );
    }

    #[test]
    fn spans_condition_errors() {
        assert_error(
            "#if\n#end\n",
            "expected condition after #if",
            "missing condition",
            "",
        );
        assert_error(
            "#if os(macos)\n#elif\n#end\n",
            "expected condition after #elif",
            "missing condition",
            "",
        );
        assert_error(
            "#if profile release\n#end\n",
            "unexpected token in conditional directive",
            "malformed condition",
            "profile release",
        );
        assert_error(
            "#if os(beos)\n#end\n",
            "unknown os",
            "unknown predicate value",
            "beos",
        );
    }
}
