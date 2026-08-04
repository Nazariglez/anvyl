use std::{
    fs,
    path::Path,
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

fn provider_probe_project(root: &Path) {
    write(
        root,
        "anvyx.toml",
        "[project]\nentry = \"src/main.anv\"\n\n[dependencies]\nhost = { path = \"../host\" }\n",
    );
    write(root, "src/main.anv", "fn main() {}\n");
    write(
        root.parent().unwrap(),
        "host/anvyx.toml",
        "[project]\nname = \"host\"\n",
    );
    write(
        root.parent().unwrap(),
        "host/Cargo.toml",
        "[package]\nname = \"host\"\nversion = \"0.1.0\"\nedition = \"2024\"\n",
    );
    write(root.parent().unwrap(), "host/src/lib.rs", "");
}

fn output_text(output: &Output) -> String {
    format!(
        "stdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    )
}

#[test]
fn check_cli_reports_lints_warnings_and_json_diagnostics() {
    let listed = anvyx().args(["check", "--list-lints"]).output().unwrap();
    assert!(listed.status.success(), "{}", output_text(&listed));
    let lints = String::from_utf8_lossy(&listed.stdout);
    assert!(lints.contains("internal_access"));
    assert!(lints.contains("unused_import"));

    let temp = tempfile::tempdir().unwrap();
    write(temp.path(), "main.anv", "import helper;\nfn main() {}\n");
    write(temp.path(), "helper.anv", "pub fn value() -> int { 1 }\n");

    let text = anvyx()
        .current_dir(temp.path())
        .args(["check", "--warn-as-error", "main.anv"])
        .output()
        .unwrap();
    assert!(!text.status.success(), "{}", output_text(&text));
    let stderr = String::from_utf8_lossy(&text.stderr);
    assert!(stderr.contains("warnings treated as errors"), "{stderr}");

    let json = anvyx()
        .current_dir(temp.path())
        .args(["check", "--format", "json", "main.anv"])
        .output()
        .unwrap();
    assert!(json.status.success(), "{}", output_text(&json));
    let report: serde_json::Value = serde_json::from_slice(&json.stdout).unwrap();
    let diagnostics = report["diagnostics"].as_array().unwrap();
    assert!(diagnostics.iter().any(|diagnostic| {
        diagnostic["severity"] == "warning"
            && diagnostic["code"] == "unused_import"
            && diagnostic["tags"] == serde_json::json!(["unnecessary"])
    }));

    write(
        temp.path(),
        "main.anv",
        "fn main() { let x: int = true; }\n",
    );
    let json = anvyx()
        .current_dir(temp.path())
        .args(["check", "--format", "json", "main.anv"])
        .output()
        .unwrap();
    assert!(!json.status.success(), "{}", output_text(&json));
    let report: serde_json::Value = serde_json::from_slice(&json.stdout).unwrap();
    let diagnostics = report["diagnostics"].as_array().unwrap();
    assert!(diagnostics.iter().any(|diagnostic| {
        diagnostic["severity"] == "error"
            && !diagnostic["labels"].as_array().unwrap().is_empty()
            && diagnostic["sources"].is_null()
    }));
}

#[test]
fn provider_probe_timeout_is_validated_through_the_cli() {
    let temp = tempfile::tempdir().unwrap();
    let root = temp.path().join("game");
    provider_probe_project(&root);

    for value in ["abc", "0"] {
        let output = anvyx()
            .current_dir(&root)
            .env("ANVYX_PROVIDER_PROBE_TIMEOUT_SECS", value)
            .args(["check", "src/main.anv"])
            .output()
            .unwrap();
        assert!(!output.status.success(), "{}", output_text(&output));
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(
            stderr.contains("ANVYX_PROVIDER_PROBE_TIMEOUT_SECS"),
            "{stderr}"
        );
        assert!(
            stderr.contains("expected positive integer seconds"),
            "{stderr}"
        );
    }

    let output = anvyx()
        .current_dir(&root)
        .env("ANVYX_PROVIDER_PROBE_TIMEOUT_SECS", "1")
        .args(["check", "src/main.anv"])
        .output()
        .unwrap();
    assert!(!output.status.success(), "{}", output_text(&output));
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        !stderr.contains("expected positive integer seconds"),
        "{stderr}"
    );
    assert!(stderr.contains("native provider package"), "{stderr}");
}

#[test]
fn manifest_lint_configuration_is_checked_by_the_cli() {
    let temp = tempfile::tempdir().unwrap();
    write(
        temp.path(),
        "anvyx.toml",
        "[project]\nentry = \"src/main.anv\"\n\n[lint]\npublic_inferred_dyn_contract = \"error\"\n",
    );
    write(
        temp.path(),
        "src/main.anv",
        "struct Actor { fn draw(self) {} }\npub fn take(actor: dyn _) { actor.draw(); }\n",
    );
    let output = anvyx()
        .current_dir(temp.path())
        .args(["check", "src/main.anv"])
        .output()
        .unwrap();
    assert!(!output.status.success(), "{}", output_text(&output));
    assert!(String::from_utf8_lossy(&output.stderr).contains("public_inferred_dyn_contract"));

    write(
        temp.path(),
        "anvyx.toml",
        "[project]\nentry = \"src/main.anv\"\n\n[lint]\nunused_variable = true\n",
    );
    let output = anvyx()
        .current_dir(temp.path())
        .args(["check", "src/main.anv"])
        .output()
        .unwrap();
    assert!(!output.status.success(), "{}", output_text(&output));
    assert!(String::from_utf8_lossy(&output.stderr).contains("invalid type: boolean"));
}
