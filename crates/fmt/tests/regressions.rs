use anvyx_fmt::{FormatError, format_source};

fn assert_fmt(source: &str, expected: &str) {
    assert_eq!(format_source(source).expect("format failed"), expected);
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
    let source =
        "fn main() {\nlet funcs = [|| {\n// fmt: skip\n  let values = [ 1,  2 ]; // kept\n}];\n}";
    let formatted = assert_idempotent(source);
    assert_eq!(formatted.matches("// fmt: skip").count(), 1);
    assert_eq!(formatted.matches("// kept").count(), 1);
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

#[test]
fn comments_remain_visible_through_public_formatting() {
    let source = "fn main() {}\n// first\n// second\n";
    assert_eq!(format_source(source).unwrap(), source);
    assert_fmt(
        "fn main(){\nlet x=1; // fmt: skip\n// fmt: skip later\nlet y=2;\n}",
        "fn main() {\n    let x = 1; // fmt: skip\n    // fmt: skip later\n    let y = 2;\n}\n",
    );
}
