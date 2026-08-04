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
