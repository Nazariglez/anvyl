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
