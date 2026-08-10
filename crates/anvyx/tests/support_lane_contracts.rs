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
    let runtime = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../runtime")
        .canonicalize()
        .unwrap();
    write(
        root.parent().unwrap(),
        "host/Cargo.toml",
        &format!(
            "[package]\nname = \"host\"\nversion = \"0.1.0\"\nedition = \"2024\"\n\n[dependencies]\nanvyx-runtime = {{ path = '{}' }}\n",
            runtime.display()
        ),
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

#[cfg(unix)]
#[test]
fn provider_probe_rejects_stale_schema() {
    const MARKER: &str = "probe-replacement-schema-5";
    let temp = tempfile::tempdir().unwrap();
    let root = temp.path().join("game");
    let cache = temp.path().join("cache");
    let host = root.parent().unwrap().join("host");
    provider_probe_project(&root);
    write(
        &host,
        "src/lib.rs",
        "use anvyx_runtime::function;\n\n#[function]\npub fn ping() {}\n\nanvyx_runtime::builtin_module! { name: \"host\", exports: [ping] }\n",
    );

    let first = anvyx()
        .current_dir(&root)
        .env("ANVYX_CACHE_DIR", &cache)
        .args(["check", "src/main.anv"])
        .output()
        .unwrap();
    assert!(first.status.success(), "{}", output_text(&first));

    let probes = fs::read_dir(cache.join("crates"))
        .unwrap()
        .map(Result::unwrap)
        .map(|entry| entry.path())
        .filter(|path| path.join("src/main.rs").is_file())
        .collect::<Vec<_>>();
    let [probe] = probes.as_slice() else {
        panic!("expected one cached provider probe, found {probes:?}");
    };
    let manifest = probe.join("Cargo.toml");
    let source = probe.join("src/main.rs");
    let manifest_text = fs::read_to_string(&manifest).unwrap();
    let binary_name = manifest_text
        .lines()
        .find_map(|line| line.trim().strip_prefix("name = \"")?.strip_suffix('\"'))
        .expect("probe manifest package name");
    use std::os::unix::fs::PermissionsExt;
    let candidates = fs::read_dir(cache.join("target/debug/deps"))
        .unwrap()
        .map(Result::unwrap)
        .map(|entry| entry.path())
        .filter(|path| {
            path.file_name()
                .and_then(|name| name.to_str())
                .is_some_and(|name| name.starts_with(&format!("{binary_name}-")))
                && path.is_file()
                && fs::metadata(path)
                    .is_ok_and(|metadata| metadata.permissions().mode() & 0o111 != 0)
        })
        .collect::<Vec<_>>();
    let [binary] = candidates.as_slice() else {
        panic!("expected one cached provider probe binary, found {candidates:?}");
    };

    let original_modified = fs::metadata(binary).unwrap().modified().unwrap();
    let replacement_source = temp.path().join("replacement.rs");
    write(
        temp.path(),
        "replacement.rs",
        &format!(
            r##"fn main() {{ print!("{{}}", r#"{{"schema":5,"package":{{"exports":[]}},"marker":"{MARKER}"}}"#); }}"##
        ),
    );
    let replacement_binary = temp.path().join("replacement-probe");
    let replacement = Command::new("rustc")
        .args([
            replacement_source.as_os_str(),
            std::ffi::OsStr::new("-o"),
            replacement_binary.as_os_str(),
        ])
        .output()
        .unwrap();
    assert!(
        replacement.status.success(),
        "{}",
        output_text(&replacement)
    );
    fs::copy(&replacement_binary, binary).unwrap();
    fs::OpenOptions::new()
        .write(true)
        .open(binary)
        .unwrap()
        .set_times(fs::FileTimes::new().set_modified(original_modified))
        .unwrap();
    let probe_output = Command::new(binary).output().unwrap();
    assert!(
        probe_output.status.success(),
        "{}",
        output_text(&probe_output)
    );
    assert!(
        String::from_utf8_lossy(&probe_output.stdout).contains(MARKER),
        "replacement probe marker missing"
    );

    let source_before = fs::read(&source).unwrap();
    let manifest_before = fs::read(&manifest).unwrap();
    let binary_before = fs::read(binary).unwrap();
    let output = anvyx()
        .current_dir(&root)
        .env("ANVYX_CACHE_DIR", &cache)
        .args(["check", "src/main.anv"])
        .output()
        .unwrap();
    assert!(!output.status.success(), "{}", output_text(&output));
    assert_eq!(fs::read(&source).unwrap(), source_before);
    assert_eq!(fs::read(&manifest).unwrap(), manifest_before);
    assert_eq!(fs::read(binary).unwrap(), binary_before);
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("unsupported schema 5; expected 4"),
        "{stderr}"
    );
    assert!(
        stderr.contains("native provider package") && stderr.contains("host"),
        "{stderr}"
    );
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
