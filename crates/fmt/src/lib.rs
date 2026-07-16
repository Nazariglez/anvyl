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

#[cfg(test)]
mod tests {
    use super::*;

    fn assert_fmt(source: &str, expected: &str) {
        let formatted = format_source(source).expect("format failed");
        assert_eq!(formatted, expected);
    }

    fn assert_idempotent(source: &str) -> String {
        let formatted = format_source(source).expect("format failed");
        assert_eq!(format_source(&formatted).unwrap(), formatted);
        formatted
    }

    fn assert_directive_error(source: &str) {
        assert!(matches!(
            format_source(source),
            Err(FormatError::Directive(_))
        ));
    }

    #[test]
    fn mixed_nested_declarations_restore_generic_names() {
        let source = "fn outer<T>(value:T)->T{type Saved=T;struct Local<U>{outer:T,item:U}enum State<V>{Some(V),None}fn inner<U>(value:U)->U{value}inner<T>(value)}struct Box<V>{value:V,fn get(self)->V{self.value}}enum Choice<W>{Some(W),None}";
        let expected = "fn outer<T>(value: T) -> T {\n    type Saved = T;\n    struct Local<U> {\n        outer: T,\n        item: U,\n    }\n    enum State<V> {\n        Some(V),\n        None,\n    }\n    fn inner<U>(value: U) -> U { value }\n    inner<T>(value)\n}\n\nstruct Box<V> {\n    value: V,\n\n    fn get(self) -> V { self.value }\n}\n\nenum Choice<W> {\n    Some(W),\n    None,\n}\n";

        let formatted = format_source(source).expect("format failed");
        assert_eq!(formatted, expected);
        assert_eq!(
            format_source(&formatted).expect("reformat failed"),
            formatted
        );
    }

    #[test]
    fn preserves_comment_after_non_ascii() {
        assert_fmt(
            "fn main() { let café = 1; // comment\ncafé; }",
            "fn main() {\n    let café = 1; // comment\n    café;\n}\n",
        );
    }

    #[test]
    fn skips_module_and_nested_block_statements() {
        let source = "// fmt: skip\nconst MATRIX = [\n [ 1.0,  0.0 ], // first\n [ 0.0,  1.0 ],\n];\nfn main(){if true {\n// fmt: skip\n        let row = [ 1,  2,  3 ]; // aligned\nlet x=1;\n}\n}";
        let expected = "// fmt: skip\nconst MATRIX = [\n [ 1.0,  0.0 ], // first\n [ 0.0,  1.0 ],\n];\n\nfn main() {\n    if true {\n        // fmt: skip\n        let row = [ 1,  2,  3 ]; // aligned\n        let x = 1;\n    }\n}\n";

        assert_eq!(assert_idempotent(source), expected);
    }

    #[test]
    fn skip_directive_preserves_surrounding_trivia_once() {
        let source = "fn main() {\n// before\n// fmt: skip\n\n    // attached\n  let values = [ 1,  2 ]; // after\n// next\nlet x=1;\n}";
        let formatted = assert_idempotent(source);

        assert!(formatted.contains("    // before\n    // fmt: skip\n\n    // attached\n  let values = [ 1,  2 ]; // after\n    // next\n    let x = 1;"));
        for comment in ["// before", "// attached", "// after", "// next"] {
            assert_eq!(formatted.matches(comment).count(), 1, "{comment}");
        }
    }

    #[test]
    fn invalid_skip_directive_placement() {
        for source in [
            "// fmt: skip",
            "fn main() {\n// fmt: skip\n}",
            "// fmt: skip\n// fmt: skip\nconst X = 1;",
            "fn main() {\n// fmt: skip\nlet x = 1; let y = 2;\n}",
            "fn main() {\n// fmt: skip\nlet x = 1; }",
            "fn main() {\n// fmt: skip\n1\n}",
            "@deprecated\n// fmt: skip\nfn main() {}",
            "// fmt: skip\nconst X = 1; @deprecated\nfn next() {}",
            "// fmt: skip\nconst X = 1; /// Docs\nfn next() {}",
            "fn main() {\nif true {\n// fmt: skip\n}\nlet x = 1;\n}",
            "fn main() {\nuse([])\n// fmt: skip\n;\nlet x = 1;\n}",
        ] {
            assert_directive_error(source);
        }
    }

    #[test]
    fn skip_directive_does_not_hide_parse_errors() {
        let error = format_source("// fmt: skip\nconst X = [;").unwrap_err();
        assert!(matches!(error, FormatError::Parse(_)));
    }

    #[test]
    fn similar_comments_format_normally() {
        assert_fmt(
            "fn main(){\nlet x=1; // fmt: skip\n// fmt: skip later\nlet y=2;\n}",
            "fn main() {\n    let x = 1; // fmt: skip\n    // fmt: skip later\n    let y = 2;\n}\n",
        );
    }

    #[test]
    fn skipped_declaration_includes_metadata() {
        for source in [
            "// fmt: skip\n/// Kept docs.\nfn  main( ) {\n}",
            "// fmt: skip\n@deprecated\nfn  main( ) {\n}",
            "// fmt: skip\npub\nfn  main( ) {\n}",
        ] {
            let formatted = assert_idempotent(source);
            assert!(formatted.contains(&source["// fmt: skip\n".len()..]));
        }
    }

    #[test]
    fn skipped_metadata_prefix_trivia_is_copied_once() {
        let source = "// fmt: skip\n@deprecated\n// between\n\n/// Docs\npub\nfn  main( ) {\n}\n";
        let formatted = assert_idempotent(source);
        assert_eq!(formatted, source);
        assert_eq!(formatted.matches("// between").count(), 1);
    }

    #[test]
    fn preserves_trailing_whitespace_inside_skipped_line() {
        let source = "fn main() {\n// fmt: skip\n  let x = [ 1 ];   \t\n}\n";
        let formatted = assert_idempotent(source);
        assert!(formatted.contains("  let x = [ 1 ];   \t\n"));
    }

    #[test]
    fn metadata_trivia_without_directive_is_preserved() {
        assert_fmt(
            "@deprecated\n// between\npub\nfn main() {}\n",
            "// between\n@deprecated\npub fn main() {}\n",
        );
    }

    #[test]
    fn outer_skip_still_validates_nested_directives() {
        assert_directive_error("// fmt: skip\nfn main() {\n// fmt: skip\n}\n");

        let source = "// fmt: skip\nfn main() {\n// fmt: skip\n  let x = [ 1 ];\n}\n";
        assert_eq!(assert_idempotent(source), source);
    }

    #[test]
    fn skipped_expression_statement_keeps_terminator_and_comment() {
        let source = "fn main() {\n// fmt: skip\n  use( [ 1,  2 ] ); // kept\nlet x=1;\n}";
        let formatted = assert_idempotent(source);
        assert!(formatted.contains("  use( [ 1,  2 ] ); // kept\n    let x = 1;"));
    }

    #[test]
    fn skipped_expression_keeps_split_terminator() {
        let source = "fn main() {\r\n// fmt: skip\r\n  use([\"café\"] )\r\n;\r\n}\r\n";
        let expected = "fn main() {\n    // fmt: skip\n  use([\"café\"] )\r\n;\r\n}\n";
        assert_eq!(assert_idempotent(source), expected);
    }

    #[test]
    fn speculative_render_restores_directive_state() {
        let source = "fn main() {\nlet funcs = [|| {\n// fmt: skip\n  let values = [ 1,  2 ]; // kept\n}];\n}";
        let formatted = assert_idempotent(source);
        assert_eq!(formatted.matches("// fmt: skip").count(), 1);
        assert_eq!(formatted.matches("// kept").count(), 1);
    }

    #[test]
    fn parse_error() {
        let source = "fn main() {";
        let result = format_source(source);
        assert!(result.is_err());
        match result.unwrap_err() {
            FormatError::Parse(_) => {}
            other => panic!("expected Parse error, got {other:?}"),
        }
    }

    #[test]
    fn expr_parens_unary_binary() {
        let source = "fn f(t: bool, f: bool) -> bool { !(t && f) }";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("!(t && f)"));
    }

    #[test]
    fn expr_parens_precedence() {
        let source = "fn f(a: int, b: int, c: int) -> int { (a + b) * c }";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("(a + b) * c"));
    }

    #[test]
    fn expr_parens_not_needed() {
        let source = "fn f(a: int, b: int, c: int) -> int { a + b * c }";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("{ a + b * c }"));
    }

    #[test]
    fn expr_parens_right_assoc() {
        let source = "fn f(a: int, b: int, c: int) -> int { a - (b - c) }";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("a - (b - c)"));
    }

    #[test]
    fn expr_parens_left_assoc_no_parens() {
        let source = "fn f(a: int, b: int, c: int) -> int { a - b - c }";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("{ a - b - c }"));
    }

    #[test]
    fn expr_parens_cast_child() {
        let source = "fn f(a: int, b: int) -> float { (a + b) as float }";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("(a + b) as float"));
    }

    #[test]
    fn expr_parens_precedence_boundaries() {
        let cases = [
            ("fn f() { let x = a ?? b || c; }", "a ?? b || c"),
            ("fn f() { let x = a && b ?? c; }", "a && b ?? c"),
            ("fn f() { let x = (a ? b : c).x; }", "(a ? b : c).x"),
            ("fn f() { let x = a ? b : c ? d : e; }", "a ? b : c ? d : e"),
            (
                "fn f() { let x = (a ? b : c) ? d : e; }",
                "(a ? b : c) ? d : e",
            ),
            (
                "fn f() { let x = (try read()).value; }",
                "(try read()).value",
            ),
            ("fn f() { let x = 0..10 + 1; }", "0..10 + 1"),
            ("fn f() { let x = (0..10) + 1; }", "(0..10) + 1"),
        ];

        for (source, expected) in cases {
            let formatted = format_source(source).expect("format failed");
            assert!(
                formatted.contains(expected),
                "expected `{expected}` in `{formatted}`"
            );
        }
    }

    // -------------------------------------------------------------------------
    // Blank line normalization
    // -------------------------------------------------------------------------

    #[test]
    fn collapse_multiple_blank_lines() {
        let source = "fn a() {}\n\n\n\nfn b() {}";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("fn a() {}\n\nfn b()"));
        assert!(!formatted.contains("\n\n\n"));
    }

    #[test]
    fn collapse_blank_lines_in_block() {
        let source = "fn main() {\n    let x = 1;\n\n\n\n    let y = 2;\n}";
        let formatted = format_source(source).expect("format failed");
        assert!(!formatted.contains("\n\n\n"));
    }

    #[test]
    fn blank_line_between_functions() {
        let source = "fn a() {}\nfn b() {}";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("fn a() {}\n\nfn b()"));
    }

    #[test]
    fn blank_line_between_struct_and_fn() {
        let source = "struct A {\n    x: int,\n}\nfn main() {}";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("}\n\nfn main()"));
    }

    #[test]
    fn no_blank_line_between_imports() {
        let source = "import foo;\nimport bar;\n\nfn main() {}";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("import foo;\nimport bar;\n"));
    }

    #[test]
    fn no_blank_line_between_consts() {
        let source = "const A = 1;\nconst B = 2;\n\nfn main() {}";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("const A = 1;\nconst B = 2;\n"));
    }

    #[test]
    fn blank_line_between_import_and_fn() {
        let source = "import foo;\nfn main() {}";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("import foo;\n\nfn main()"));
    }

    // -------------------------------------------------------------------------
    // Idempotency test
    // -------------------------------------------------------------------------

    #[test]
    fn idempotency() {
        let manifest_dir = std::path::Path::new(env!("CARGO_MANIFEST_DIR"));
        let tests_dir = manifest_dir
            .parent()
            .unwrap()
            .parent()
            .unwrap()
            .join("tests");

        let files = walk_anv_files(&tests_dir);
        let mut tested = 0;
        let mut skipped = 0;

        for entry in &files {
            let source = std::fs::read_to_string(entry).unwrap();

            if source.starts_with("// @helper") {
                skipped += 1;
                continue;
            }

            let Ok(first) = format_source(&source) else {
                skipped += 1;
                continue;
            };

            let second = format_source(&first).unwrap_or_else(|e| {
                panic!(
                    "Formatted output failed to re-parse: {}\nFile: {}",
                    e,
                    entry.display()
                );
            });

            assert_eq!(
                first,
                second,
                "Idempotency failure: {}\n--- first ---\n{}\n--- second ---\n{}",
                entry.display(),
                first,
                second
            );
            tested += 1;
        }

        assert!(
            tested > 1000,
            "Expected >1000 files tested, got only {tested}"
        );
        eprintln!("Idempotency: {tested} tested, {skipped} skipped");
    }

    fn walk_anv_files(dir: &std::path::Path) -> Vec<std::path::PathBuf> {
        let mut files = vec![];
        walk_anv_files_rec(dir, &mut files);
        files.sort();
        files
    }

    fn walk_anv_files_rec(dir: &std::path::Path, files: &mut Vec<std::path::PathBuf>) {
        let Ok(entries) = std::fs::read_dir(dir) else {
            return;
        };
        for entry in entries {
            let Ok(entry) = entry else {
                continue;
            };
            let path = entry.path();
            if path.is_dir() {
                let name = entry.file_name();
                if !name.to_str().is_some_and(|s| s.starts_with('.')) {
                    walk_anv_files_rec(&path, files);
                }
            } else if path.extension().is_some_and(|ext| ext == "anv") {
                files.push(path);
            }
        }
    }
}
