use crate::config::{CompilationContext, PredicateError};

struct Frame {
    parent_active: bool,
    active: bool,
    taken: bool,
    else_state: ElseState,
}

#[derive(PartialEq, Eq)]
enum ElseState {
    Open,
    Seen,
}

pub(crate) fn filter_with_context(
    source: &str,
    ctx: &CompilationContext,
) -> Result<String, Vec<String>> {
    let mut out = String::with_capacity(source.len());
    let mut stack: Vec<Frame> = vec![];
    let mut errors = vec![];

    for line in source.split_inclusive('\n') {
        let trimmed = line.trim_start();
        match directive(trimmed) {
            Some(Directive::If(cond)) => {
                let cond = eval_condition(cond, ctx, &mut errors).unwrap_or(false);
                let parent_active = active(&stack);
                stack.push(Frame {
                    parent_active,
                    active: parent_active && cond,
                    taken: cond,
                    else_state: ElseState::Open,
                });
                mask_line(line, &mut out);
            }
            Some(Directive::Elif(cond)) => {
                let cond = eval_condition(cond, ctx, &mut errors).unwrap_or(false);
                match stack.last_mut() {
                    Some(frame) if frame.else_state == ElseState::Seen => {
                        errors.push("#elif after #else".into());
                    }
                    Some(frame) => {
                        frame.active = frame.parent_active && !frame.taken && cond;
                        frame.taken |= cond;
                    }
                    None => errors.push("#elif without matching #if".into()),
                }
                mask_line(line, &mut out);
            }
            Some(Directive::Else) => {
                match stack.last_mut() {
                    Some(frame) if frame.else_state == ElseState::Seen => {
                        errors.push("duplicate #else".into());
                    }
                    Some(frame) => {
                        frame.active = frame.parent_active && !frame.taken;
                        frame.taken = true;
                        frame.else_state = ElseState::Seen;
                    }
                    None => errors.push("#else without matching #if".into()),
                }
                mask_line(line, &mut out);
            }
            Some(Directive::End) => {
                if stack.pop().is_none() {
                    errors.push("#end without matching #if".into());
                }
                mask_line(line, &mut out);
            }
            None if active(&stack) => out.push_str(line),
            None => mask_line(line, &mut out),
        }
    }

    if !stack.is_empty() {
        errors.push("unterminated #if".into());
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

enum Directive<'a> {
    If(&'a str),
    Elif(&'a str),
    Else,
    End,
}

fn directive(line: &str) -> Option<Directive<'_>> {
    let line = line.trim_end();
    if let Some(rest) = directive_condition(line, "#if") {
        return Some(Directive::If(rest));
    }
    if let Some(rest) = directive_condition(line, "#elif") {
        return Some(Directive::Elif(rest));
    }
    if line == "#else" {
        return Some(Directive::Else);
    }
    if line == "#end" {
        return Some(Directive::End);
    }
    None
}

fn directive_condition<'a>(line: &'a str, keyword: &str) -> Option<&'a str> {
    let rest = line.strip_prefix(keyword)?;
    if rest.is_empty() {
        return Some("");
    }
    rest.chars()
        .next()
        .is_some_and(char::is_whitespace)
        .then(|| rest.trim())
}

fn eval_condition(cond: &str, ctx: &CompilationContext, errors: &mut Vec<String>) -> Option<bool> {
    if cond.is_empty() {
        errors.push("expected condition after #if".into());
        return None;
    }

    let Some(open) = cond.find('(') else {
        errors.push("unexpected token in conditional directive".into());
        return None;
    };
    if !cond.ends_with(')') || cond[open + 1..cond.len() - 1].contains(['(', ')']) {
        errors.push("unexpected token in conditional directive".into());
        return None;
    }

    let pred = cond[..open].trim();
    let arg = cond[open + 1..cond.len() - 1].trim();
    if pred.is_empty() || arg.is_empty() {
        errors.push("expected condition after #if".into());
        return None;
    }

    match ctx.eval_predicate(pred, arg) {
        Ok(value) => Some(value),
        Err(PredicateError::UnknownPredicate) => {
            errors.push("unknown conditional predicate".into());
            None
        }
        Err(PredicateError::UnknownValue) => {
            errors.push(format!("unknown {pred}"));
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn filter(source: &str) -> Result<String, Vec<String>> {
        filter_with_context(source, &CompilationContext::default())
    }

    fn filtered(source: &str) -> String {
        filter(source).expect("filter failed")
    }

    fn assert_masked(out: &str, source: &str, text: &str) {
        let start = source.find(text).expect("missing segment");
        let end = start + text.len();
        assert!(out[start..end].bytes().all(|byte| byte == b' '));
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
        let errors = filter("#if profile(release)\n#else\n#else\n#end\n").unwrap_err();
        assert!(errors.iter().any(|err| err == "duplicate #else"));
    }

    #[test]
    fn ignores_hash_prefixed_identifiers() {
        let source = "#ifdef profile(debug)\n";
        assert_eq!(filtered(source), source);
    }

    #[test]
    fn rejects_unknown_predicate() {
        let errors = filter("#if platform(macos)\n#end\n").unwrap_err();
        assert!(
            errors
                .iter()
                .any(|err| err == "unknown conditional predicate")
        );
    }
}
