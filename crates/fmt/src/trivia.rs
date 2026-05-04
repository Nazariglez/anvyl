use anvyx_frontend::{lexer::SpannedToken, span::Span};

pub struct TriviaItem {
    pub kind: TriviaKind,
    pub span: Span,
    pub text: String,
}

#[derive(Debug, PartialEq, Eq)]
pub enum TriviaKind {
    LineComment,
    BlankLine,
}

// doc comments (`///`) are tokenized separately and won't appear in the gaps we scan here
pub fn scan_trivia(source: &str, tokens: &[SpannedToken]) -> Vec<TriviaItem> {
    let mut items = Vec::new();

    if tokens.is_empty() {
        scan_gap(source, 0, source.len(), &mut items);
        return items;
    }

    scan_gap(source, 0, tokens[0].1.start, &mut items);

    for pair in tokens.windows(2) {
        scan_gap(source, pair[0].1.end, pair[1].1.start, &mut items);
    }

    let start = tokens.last().unwrap().1.end;
    scan_gap(source, start, source.len(), &mut items);

    items
}

fn scan_gap(source: &str, start: usize, end: usize, items: &mut Vec<TriviaItem>) {
    if start >= end {
        return;
    }

    let gap = &source[start..end];

    // no newline means no blank line possible, but may still have a trailing comment
    if !gap.contains('\n') {
        let trimmed = gap.trim();
        if trimmed.starts_with("//") {
            items.push(TriviaItem {
                kind: TriviaKind::LineComment,
                span: Span::new(start, end),
                text: trimmed.to_string(),
            });
        }
        return;
    }

    let line_count = gap.matches('\n').count() + 1;
    let last_idx = line_count - 1;
    let mut offset = 0;

    for (i, line) in gap.split('\n').enumerate() {
        let line_start = start + offset;
        let line_end = if i == last_idx {
            end
        } else {
            start + offset + line.len()
        };

        let trimmed = line.trim();
        if trimmed.is_empty() {
            // skip first and last segments, those are just surrounding whitespace not blank lines
            if i > 0 && i < last_idx {
                items.push(TriviaItem {
                    kind: TriviaKind::BlankLine,
                    span: Span::new(line_start, line_end),
                    text: String::new(),
                });
            }
        } else if trimmed.starts_with("//") {
            items.push(TriviaItem {
                kind: TriviaKind::LineComment,
                span: Span::new(line_start, line_end),
                text: trimmed.to_string(),
            });
        }

        offset += line.len() + 1; // +1 for the '\n'
    }
}

#[cfg(test)]
mod tests {
    use anvyx_frontend::lexer;

    use super::*;

    fn tokenize_test(source: &str) -> Vec<SpannedToken> {
        lexer::tokenize(source).expect("tokenize failed")
    }

    #[test]
    fn empty_source() {
        let tokens = tokenize_test("");
        let trivia = scan_trivia("", &tokens);
        assert!(trivia.is_empty());
    }

    #[test]
    fn comment_between_tokens() {
        let source = "let x = 5; // comment\nlet y = 10;";
        let tokens = tokenize_test(source);
        let trivia = scan_trivia(source, &tokens);
        assert_eq!(trivia.len(), 1);
        assert_eq!(trivia[0].kind, TriviaKind::LineComment);
        assert_eq!(trivia[0].text, "// comment");
    }

    #[test]
    fn blank_lines() {
        let source = "let x = 5;\n\n\nlet y = 10;";
        let tokens = tokenize_test(source);
        let trivia = scan_trivia(source, &tokens);
        let blank_count = trivia
            .iter()
            .filter(|t| matches!(t.kind, TriviaKind::BlankLine))
            .count();
        assert_eq!(blank_count, 2);
    }

    #[test]
    fn leading_comment() {
        let source = "// file comment\nfn main() {}";
        let tokens = tokenize_test(source);
        let trivia = scan_trivia(source, &tokens);
        assert_eq!(trivia.len(), 1);
        assert_eq!(trivia[0].kind, TriviaKind::LineComment);
        assert_eq!(trivia[0].text, "// file comment");
    }

    #[test]
    fn trailing_comment() {
        let source = "fn main() {}\n// end comment";
        let tokens = tokenize_test(source);
        let trivia = scan_trivia(source, &tokens);
        assert_eq!(trivia.len(), 1);
        assert_eq!(trivia[0].kind, TriviaKind::LineComment);
        assert_eq!(trivia[0].text, "// end comment");
    }

    #[test]
    fn no_doc_comment_in_trivia() {
        let source = "/// doc comment\nfn main() {}";
        let tokens = tokenize_test(source);
        let trivia = scan_trivia(source, &tokens);
        assert!(trivia.is_empty());
    }

    #[test]
    fn comment_block() {
        let source = "// line1\n// line2\nfn main() {}";
        let tokens = tokenize_test(source);
        let trivia = scan_trivia(source, &tokens);
        assert_eq!(trivia.len(), 2);
        assert_eq!(trivia[0].kind, TriviaKind::LineComment);
        assert_eq!(trivia[0].text, "// line1");
        assert_eq!(trivia[1].kind, TriviaKind::LineComment);
        assert_eq!(trivia[1].text, "// line2");
    }

    #[test]
    fn adjacent_tokens() {
        let source = "let x=5;";
        let tokens = tokenize_test(source);
        let trivia = scan_trivia(source, &tokens);
        assert!(
            !trivia
                .iter()
                .any(|t| matches!(t.kind, TriviaKind::LineComment))
        );
    }
}
