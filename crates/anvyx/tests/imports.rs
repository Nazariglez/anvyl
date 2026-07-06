use std::{
    fs,
    path::{Path, PathBuf},
    process::{Command, Output},
};

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

fn assert_success(output: &Output) {
    assert!(
        output.status.success(),
        "stdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
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

fn host_exe(name: &str) -> String {
    format!("{name}{}", std::env::consts::EXE_SUFFIX)
}

fn generated_manifest(root: &Path) -> PathBuf {
    let crates = root.join(".anvyx/cache/rust/crates");
    let mut manifests = fs::read_dir(&crates)
        .unwrap()
        .map(|entry| entry.unwrap().path().join("Cargo.toml"))
        .filter(|path| path.exists())
        .collect::<Vec<_>>();
    manifests.sort();
    assert_eq!(manifests.len(), 1);
    manifests.remove(0)
}

#[test]
fn run_renders_frontend_parse_report() {
    let temp = tempfile::tempdir().unwrap();
    let root = temp.path();
    write(root, "main.anv", "fn main() {\n    }\n}\n");

    let output = anvyx()
        .current_dir(root)
        .args(["run", "main.anv"])
        .output()
        .unwrap();
    let stderr = String::from_utf8_lossy(&output.stderr);

    assert!(!output.status.success());
    assert_frontend_parse_failure(&stderr, "Checking main.anv");
}

#[test]
fn build_renders_frontend_parse_report() {
    let temp = tempfile::tempdir().unwrap();
    let root = temp.path();
    write(
        root,
        "anvyx.toml",
        "[project]\nname = \"demo\"\nentry = \"src/main.anv\"\n",
    );
    write(root, "src/main.anv", "fn main() {\n    }\n}\n");

    let output = anvyx().current_dir(root).args(["build"]).output().unwrap();
    let stderr = String::from_utf8_lossy(&output.stderr);

    assert!(!output.status.success());
    assert_frontend_parse_failure(&stderr, "Checking src/main.anv");
}

#[test]
fn run_renders_warnings_before_later_phases() {
    let temp = tempfile::tempdir().unwrap();
    let root = temp.path();
    write(root, "main.anv", "import helper;\n\nfn main() {}\n");
    write(root, "helper.anv", "pub fn f() {}\n");

    let output = anvyx()
        .current_dir(root)
        .args(["run", "main.anv"])
        .output()
        .unwrap();
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    assert_success(&output);
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
fn build_copies_debug_and_release_artifacts() {
    let temp = tempfile::tempdir().unwrap();
    let root = temp.path();
    write(
        root,
        "anvyx.toml",
        "[project]\nname = \"demo\"\nentry = \"src/main.anv\"\n",
    );
    write(root, "src/main.anv", "fn main() {}\n");

    let debug = anvyx().current_dir(root).args(["build"]).output().unwrap();
    assert_success(&debug);
    assert!(root.join("build/debug").join(host_exe("demo")).exists());

    let release = anvyx()
        .current_dir(root)
        .args(["build", "--release"])
        .output()
        .unwrap();
    assert_success(&release);
    assert!(root.join("build/release").join(host_exe("demo")).exists());
}

#[test]
fn build_manifest_uses_project_version_or_default() {
    let default_root = tempfile::tempdir().unwrap();
    write(
        default_root.path(),
        "anvyx.toml",
        "[project]\nname = \"default_version\"\nentry = \"src/main.anv\"\n",
    );
    write(default_root.path(), "src/main.anv", "fn main() {}\n");

    let default_output = anvyx()
        .current_dir(default_root.path())
        .args(["build"])
        .output()
        .unwrap();
    assert_success(&default_output);
    let default_manifest = fs::read_to_string(generated_manifest(default_root.path())).unwrap();
    assert!(default_manifest.contains("version = \"0.0.0\""));

    let versioned_root = tempfile::tempdir().unwrap();
    write(
        versioned_root.path(),
        "anvyx.toml",
        "[project]\nname = \"versioned\"\nversion = \"1.2.3\"\nentry = \"src/main.anv\"\n",
    );
    write(versioned_root.path(), "src/main.anv", "fn main() {}\n");

    let versioned_output = anvyx()
        .current_dir(versioned_root.path())
        .args(["build"])
        .output()
        .unwrap();
    assert_success(&versioned_output);
    let versioned_manifest = fs::read_to_string(generated_manifest(versioned_root.path())).unwrap();
    assert!(versioned_manifest.contains("version = \"1.2.3\""));
    assert!(versioned_manifest.contains("edition = \"2024\""));
}

#[test]
fn build_sanitizes_public_artifact_name() {
    let temp = tempfile::tempdir().unwrap();
    let root = temp.path();
    write(
        root,
        "anvyx.toml",
        "[project]\nname = \"../Bad Name\"\nentry = \"src/main.anv\"\n",
    );
    write(root, "src/main.anv", "fn main() {}\n");

    let output = anvyx().current_dir(root).args(["build"]).output().unwrap();
    assert_success(&output);
    assert!(root.join("build/debug").join(host_exe("bad-name")).exists());
}
