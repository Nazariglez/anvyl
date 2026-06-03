use std::{fs, path::Path, process::Command};

fn write(root: &Path, relative: &str, text: &str) {
    let path = root.join(relative);
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent).unwrap();
    }
    fs::write(path, text).unwrap();
}

fn anvyx() -> Command {
    Command::new(env!("CARGO_BIN_EXE_anvyx"))
}

fn assert_in_order(text: &str, needles: &[&str]) {
    let mut rest = text;
    for needle in needles {
        let index = rest
            .find(needle)
            .unwrap_or_else(|| panic!("missing {needle:?} in order in:\n{text}"));
        rest = &rest[index + needle.len()..];
    }
}

fn assert_frontend_parse_failure(stderr: &str, checking: &str) {
    for expected in [checking, "Unexpected token", "Failed to parse program"] {
        assert!(stderr.contains(expected), "stderr:\n{stderr}");
    }
    for phase in [
        "Building",
        "Generating Rust",
        "Compiling generated Rust",
        "Running",
    ] {
        assert!(!stderr.contains(phase), "stderr:\n{stderr}");
    }
}

#[test]
fn package_import_smoke() {
    let temp = tempfile::tempdir().unwrap();
    let root = temp.path();

    write(
        root,
        "game/anvyx.toml",
        "[project]\nentry = \"src/main.anv\"\n\n[dependencies]\nmath = { path = \"../math\" }\n",
    );
    write(
        root,
        "math/anvyx.toml",
        "[project]\nentry = \"src/lib.anv\"\n",
    );
    write(
        root,
        "game/src/main.anv",
        "import helper { local };\nimport .helper { local as again };\nimport pkg:math { add };\nimport pkg:math.util { mul };\nimport std:mem;\n\nfn main() {\n    let x: int = local() + again() + add() + mul();\n}\n",
    );
    write(root, "game/src/helper.anv", "pub fn local() -> int { 1 }\n");
    write(
        root,
        "math/src/lib.anv",
        "pub import util;\n\npub fn add() -> int { 1 }\n",
    );
    write(root, "math/src/util.anv", "pub fn mul() -> int { 2 }\n");

    let output = anvyx()
        .current_dir(root.join("game"))
        .args(["check", "--new-frontend"])
        .output()
        .unwrap();

    assert!(
        output.status.success(),
        "stdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
}

#[test]
fn clean_rust_run_renders_frontend_parse_report() {
    let temp = tempfile::tempdir().unwrap();
    let root = temp.path();
    write(root, "main.anv", "fn main() {\n    }\n}\n");

    let output = anvyx()
        .current_dir(root)
        .args(["run", "--new-frontend", "--backend", "rust", "main.anv"])
        .output()
        .unwrap();
    let stderr = String::from_utf8_lossy(&output.stderr);

    assert!(!output.status.success());
    assert_frontend_parse_failure(&stderr, "Checking main.anv");
}

#[test]
fn clean_rust_build_renders_frontend_parse_report() {
    let temp = tempfile::tempdir().unwrap();
    let root = temp.path();
    write(
        root,
        "anvyx.toml",
        "[project]\nname = \"demo\"\nentry = \"src/main.anv\"\n",
    );
    write(root, "src/main.anv", "fn main() {\n    }\n}\n");

    let output = anvyx()
        .current_dir(root)
        .args(["build", "--new-frontend", "--backend", "rust"])
        .output()
        .unwrap();
    let stderr = String::from_utf8_lossy(&output.stderr);

    assert!(!output.status.success());
    assert_frontend_parse_failure(&stderr, "Checking src/main.anv");
}

#[test]
fn clean_rust_run_renders_warnings_before_later_phases() {
    let temp = tempfile::tempdir().unwrap();
    let root = temp.path();
    write(root, "main.anv", "import helper;\n\nfn main() {}\n");
    write(root, "helper.anv", "pub fn f() {}\n");

    let output = anvyx()
        .current_dir(root)
        .args(["run", "--new-frontend", "--backend", "rust", "main.anv"])
        .output()
        .unwrap();
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    assert!(
        output.status.success(),
        "stdout:\n{stdout}\nstderr:\n{stderr}"
    );
    assert_eq!(stdout, "");
    assert_in_order(
        &stderr,
        &[
            "Checking",
            "Warning: unused import from 'helper'",
            "Generating Rust",
            "Compiling generated Rust",
            "Running",
        ],
    );
}

#[test]
fn pkg_requires_context() {
    let temp = tempfile::tempdir().unwrap();
    let root = temp.path();
    write(root, "main.anv", "import pkg:math;\nfn main() {}\n");

    let output = anvyx()
        .current_dir(root)
        .args(["check", "--new-frontend", "main.anv"])
        .output()
        .unwrap();
    let stderr = String::from_utf8_lossy(&output.stderr);

    assert!(!output.status.success());
    assert!(
        stderr.contains("has no package dependency named 'math'"),
        "stderr:\n{stderr}",
    );
}
