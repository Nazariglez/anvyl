use anvyx_frontend::{lexer::LexedToken, span::Span};

pub(super) struct TriviaItem {
    pub(super) kind: TriviaKind,
    pub(super) span: Span,
    pub(super) text: String,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) enum TriviaKind {
    LineComment,
    SkipDirective,
    BlankLine,
}

const SKIP_DIRECTIVE: &str = "// fmt: skip";

// doc comments (`///`) are tokenized separately and won't appear in the gaps we scan here
pub(super) fn scan_trivia(source: &str, tokens: &[LexedToken]) -> Vec<TriviaItem> {
    let mut items = Vec::new();

    if tokens.is_empty() {
        scan_gap(source, 0, source.len(), &mut items);
        return items;
    }

    scan_gap(source, 0, tokens[0].1.start(), &mut items);

    for pair in tokens.windows(2) {
        scan_gap(source, pair[0].1.end(), pair[1].1.start(), &mut items);
    }

    let start = tokens.last().unwrap().1.end();
    scan_gap(source, start, source.len(), &mut items);

    items
}

fn scan_gap(source: &str, start: usize, end: usize, items: &mut Vec<TriviaItem>) {
    if start >= end {
        return;
    }

    let mut line_start = start;

    for (i, line) in source[start..end].split('\n').enumerate() {
        let line_end = line_start + line.len();
        if line.trim().is_empty() {
            if i > 0 && line_end < end {
                items.push(TriviaItem {
                    kind: TriviaKind::BlankLine,
                    span: Span::new(line_start, line_end),
                    text: String::new(),
                });
            }
        } else {
            let standalone = i > 0 || {
                let physical_start = source[..start].rfind('\n').map_or(0, |pos| pos + 1);
                source[physical_start..start].trim().is_empty()
            };
            push_comment(line_start, line, standalone, items);
        }
        line_start = line_end + 1;
    }
}

fn push_comment(line_start: usize, line: &str, standalone: bool, items: &mut Vec<TriviaItem>) {
    let line = line.strip_suffix('\r').unwrap_or(line);
    let comment = line.trim_start();
    if !comment.starts_with("//") {
        return;
    }

    let comment_start = line_start + line.len() - comment.len();
    let is_directive = standalone && comment == SKIP_DIRECTIVE;

    items.push(TriviaItem {
        kind: if is_directive {
            TriviaKind::SkipDirective
        } else {
            TriviaKind::LineComment
        },
        span: Span::new(
            if is_directive {
                comment_start
            } else {
                line_start
            },
            line_start + line.len(),
        ),
        text: comment.trim_end().to_string(),
    });
}

#[cfg(test)]
mod tests {
    use anvyx_frontend::lexer;

    use super::*;

    fn tokenize_test(source: &str) -> Vec<LexedToken> {
        let mut sources = anvyx_frontend::source::SourceTable::default();
        let source_id = sources.add(
            anvyx_frontend::source::SourceKind::Virtual,
            "test",
            None,
            source,
        );
        lexer::tokenize(source_id, source)
            .expect("tokenize failed")
            .tokens
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
    fn exact_standalone_skip_directive() {
        for source in [
            "// fmt: skip\nfn main() {}",
            "    // fmt: skip\nfn main() {}",
            "// fmt: skip\r\nfn main() {}",
        ] {
            let trivia = scan_trivia(source, &tokenize_test(source));
            assert_eq!(trivia[0].kind, TriviaKind::SkipDirective);
            assert_eq!(
                &source[trivia[0].span.start..trivia[0].span.end],
                "// fmt: skip"
            );
        }
    }

    #[test]
    fn similar_comments_are_not_directives() {
        for source in [
            "const X = 1; // fmt: skip",
            "// fmt: skip this\nfn main() {}",
            "// fmt: skip   \nfn main() {}",
            "// fmt: skip\t\nfn main() {}",
            "const X = \"// fmt: skip\";",
        ] {
            let trivia = scan_trivia(source, &tokenize_test(source));
            assert!(
                !trivia
                    .iter()
                    .any(|item| item.kind == TriviaKind::SkipDirective)
            );
        }
    }
}
