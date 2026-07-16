use std::{fs, path::Path};

#[test]
fn formatter_snapshots() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/snapshots");
    let mut inputs = fs::read_dir(&root)
        .unwrap()
        .map(|entry| entry.unwrap().path())
        .filter(|path| path.extension().is_some_and(|ext| ext == "anv"))
        .filter(|path| {
            path.file_name()
                .unwrap()
                .to_string_lossy()
                .ends_with(".input.anv")
        })
        .collect::<Vec<_>>();
    inputs.sort();

    for input in inputs {
        let name = input.file_name().unwrap().to_string_lossy();
        let expected = input.with_file_name(name.replace(".input.anv", ".expected.anv"));
        let source = fs::read_to_string(&input).unwrap();
        let expected = fs::read_to_string(&expected).unwrap();
        let formatted = anvyx_fmt::format_source(&source).unwrap();
        assert_eq!(formatted, expected, "{}", input.display());
        let requires_idempotency = name.starts_with("format_skip_")
            || name.starts_with("numeric_literals_")
            || name.starts_with("enum_numeric_")
            || name.starts_with("enum_decls_");
        if requires_idempotency {
            assert_eq!(
                anvyx_fmt::format_source(&formatted).unwrap(),
                formatted,
                "{} is not idempotent",
                input.display()
            );
        }
    }
}
