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
        let expected = input.with_file_name(
            input
                .file_name()
                .unwrap()
                .to_string_lossy()
                .replace(".input.anv", ".expected.anv"),
        );
        let source = fs::read_to_string(&input).unwrap();
        let expected = fs::read_to_string(&expected).unwrap();
        let formatted = anvyx_fmt::format_source(&source).unwrap();
        assert_eq!(formatted, expected, "{}", input.display());
    }
}
