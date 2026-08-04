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
