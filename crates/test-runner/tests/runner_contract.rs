use std::{
    fs,
    io::Read,
    path::{Path, PathBuf},
    process::{Command, Output, Stdio},
    thread,
    time::{Duration, Instant},
};

use serde_json::Value;
use tempfile::TempDir;

fn runner(args: &[String]) -> Output {
    let mut command = Command::new(env!("CARGO_BIN_EXE_test-runner"));
    command
        .current_dir(Path::new(env!("CARGO_MANIFEST_DIR")).join("../.."))
        .args(args)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped());
    #[cfg(unix)]
    {
        use std::os::unix::process::CommandExt;
        command.process_group(0);
    }
    let mut child = command.spawn().expect("test runner should launch");
    let mut stdout = child.stdout.take().unwrap();
    let mut stderr = child.stderr.take().unwrap();
    let stdout = thread::spawn(move || {
        let mut bytes = vec![];
        stdout.read_to_end(&mut bytes).unwrap();
        bytes
    });
    let stderr = thread::spawn(move || {
        let mut bytes = vec![];
        stderr.read_to_end(&mut bytes).unwrap();
        bytes
    });
    let deadline = Instant::now() + Duration::from_secs(60);
    let status = loop {
        if let Some(status) = child.try_wait().unwrap() {
            break status;
        }
        if Instant::now() >= deadline {
            terminate(&mut child);
            let stdout = stdout.join().unwrap();
            let stderr = stderr.join().unwrap();
            panic!(
                "test runner process exceeded watchdog timeout\nstdout:\n{}\nstderr:\n{}",
                String::from_utf8_lossy(&stdout),
                String::from_utf8_lossy(&stderr)
            );
        }
        thread::sleep(Duration::from_millis(10));
    };
    Output {
        status,
        stdout: stdout.join().unwrap(),
        stderr: stderr.join().unwrap(),
    }
}

fn terminate(child: &mut std::process::Child) {
    #[cfg(unix)]
    {
        let process_group = -(child.id() as i32);
        // SAFETY: the runner starts in a dedicated process group above.
        let killed_group = unsafe { libc::kill(process_group, libc::SIGKILL) } == 0;
        if !killed_group {
            let _ = child.kill();
        }
    }
    #[cfg(not(unix))]
    let _ = child.kill();
    let _ = child.wait();
}

fn json(args: &[String]) -> (Output, Value) {
    let output = runner(args);
    let report = serde_json::from_slice(&output.stdout).unwrap_or_else(|error| {
        panic!(
            "test runner should emit JSON: {error}\nstdout:\n{}\nstderr:\n{}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr),
        )
    });
    (output, report)
}

fn write(dir: &Path, name: &str, source: &str) -> PathBuf {
    let path = dir.join(name);
    fs::write(&path, source).expect("fixture should write");
    path
}

fn args(items: impl IntoIterator<Item = impl AsRef<str>>) -> Vec<String> {
    items
        .into_iter()
        .map(|item| item.as_ref().to_string())
        .collect()
}

fn write_stderr_provider(dir: &Path) -> PathBuf {
    let provider = dir.join("provider");
    fs::create_dir_all(provider.join("src")).expect("provider source directory");
    fs::write(
        dir.join("anvyx.toml"),
        "[project]\nname = \"test-runner-contract\"\nentry = \"src/main.anv\"\n\n[dependencies]\nhost = { path = \"provider\" }\n",
    )
    .expect("project manifest");
    fs::write(provider.join("anvyx.toml"), "[project]\nname = \"host\"\n")
        .expect("provider manifest");
    fs::write(
        provider.join("Cargo.toml"),
        format!(
            "[package]\nname = \"test-runner-contract-host\"\nversion = \"0.1.0\"\nedition = \"2024\"\n\n[dependencies]\nanvyx-runtime = {{ path = \"{}\" }}\n",
            Path::new(env!("CARGO_MANIFEST_DIR"))
                .join("../runtime")
                .display()
        ),
    )
    .expect("provider Cargo manifest");
    fs::write(
        provider.join("src/lib.rs"),
        "use anvyx_runtime::function;\n\n#[function]\npub fn stderr_probe() -> i64 {\n    eprintln!(\"runner stderr\");\n    0\n}\n\nanvyx_runtime::builtin_module! {\n    name: \"host\",\n    source: \"\",\n    exports: [stderr_probe],\n}\n",
    )
    .expect("provider source");
    let source = dir.join("src");
    fs::create_dir(&source).expect("project source directory");
    fs::write(source.join("main.anv"), "fn main() {}\n").expect("project entry source");
    let fixtures = source.join("fixtures");
    fs::create_dir(&fixtures).expect("fixture directory");
    fixtures
}

#[test]
fn command_line_contract_is_exercised_through_the_binary() {
    for (input, expected) in [
        (args(["--unknown", "tests"]), "Unknown option: --unknown"),
        (args(["--timeout"]), "Missing value for --timeout"),
        (
            args(["--timeout", "nope", "tests"]),
            "Invalid --timeout value: 'nope'",
        ),
        (
            args(["--jobs", "0", "tests"]),
            "--jobs must be greater than zero",
        ),
        (
            args(["--quiet"]),
            "Provide one or more directories or files as arguments",
        ),
    ] {
        let output = runner(&input);
        assert!(!output.status.success());
        assert!(String::from_utf8_lossy(&output.stderr).contains(expected));
    }

    let dir = TempDir::new().expect("temporary fixtures");
    let helper = write(dir.path(), "helper.anv", "// @helper\n");
    let input = args([
        "--quiet",
        "--report-json",
        "--timeout",
        "7",
        "--timeout",
        "9",
        helper.to_str().expect("utf8 fixture path"),
        "--compile-timeout",
        "11",
        "--compile-timeout",
        "13",
        "--jobs",
        "1",
    ]);
    let (output, report) = json(&input);

    assert!(output.status.success());
    assert_eq!(report["runtime_timeout_ms"], 9);
    assert_eq!(report["compile_timeout_ms"], 13);
    assert_eq!(report["helpers"], 1);
    assert_eq!(report["issues"].as_array().expect("issues array").len(), 0);

    let release = args([
        "--release",
        "--quiet",
        "--report-json",
        helper.to_str().expect("utf8 fixture path"),
    ]);
    let (output, report) = json(&release);
    assert!(output.status.success());
    assert_eq!(report["helpers"], 1);
}

#[test]
fn directive_validation_is_exercised_through_fixture_files() {
    let dir = TempDir::new().expect("temporary fixtures");
    let cases = [
        (
            "exit_check",
            "// @mode: check\n// @expect: success\n// @exit-code: 7\n",
            "@exit-code is only valid in @mode: run",
        ),
        (
            "exit_range",
            "// @mode: run\n// @expect: success\n// @exit-code: 999\n",
            "invalid @exit-code value: 999",
        ),
        (
            "exit_expect",
            "// @mode: run\n// @expect: error\n// @exit-code: 7\n",
            "@exit-code requires @expect: success",
        ),
        (
            "invalid_mode",
            "// @mode: nope\n// @expect: success\n",
            "Invalid mode: nope",
        ),
        (
            "invalid_expect",
            "// @mode: run\n// @expect: nope\n",
            "Invalid expected result: nope",
        ),
        (
            "unknown",
            "// @unknown: value\n",
            "unknown directive @unknown",
        ),
        (
            "missing_value",
            "// @mode\n",
            "@mode requires a value: check|run",
        ),
        (
            "empty_value",
            "// @mode:   \n",
            "@mode requires a value: check|run",
        ),
        (
            "flag_value",
            "// @helper: nope\n",
            "@helper does not take a value",
        ),
        (
            "duplicate",
            "// @mode: run\n// @mode: check\n",
            "duplicate @mode directive",
        ),
        (
            "unclosed",
            "// @match-begin\n// A\n// B\n",
            "unclosed @match-begin block",
        ),
        (
            "non_comment",
            "// @match-begin\n// A\nfn main() {}\n",
            "non-comment line inside @match-begin block",
        ),
        (
            "end_without_begin",
            "// @match-end\n",
            "@match-end without @match-begin",
        ),
        (
            "nested",
            "// @match-begin\n// @match-begin\n",
            "nested @match-begin directive",
        ),
        (
            "short_match",
            "// @match-begin\n// A\n// @match-end\n",
            "@match-begin block requires at least two lines",
        ),
        (
            "missing_mode",
            "// @expect: success\n",
            "missing @mode directive",
        ),
        (
            "missing_expect",
            "// @mode: run\n",
            "missing @expect directive",
        ),
        (
            "helper_mixed",
            "// @helper\n// @mode: run\n",
            "@helper cannot be combined with other directives",
        ),
        (
            "stdin_check",
            "// @mode: check\n// @expect: success\n// @stdin: input\n",
            "@stdin is only valid in @mode: run",
        ),
        (
            "stdin_empty_check",
            "// @mode: check\n// @expect: success\n// @stdin-empty-line\n",
            "@stdin is only valid in @mode: run",
        ),
        (
            "warn_error",
            "// @mode: run\n// @expect: error\n// @warn-contains: warning\n",
            "@warn-contains requires @expect: success",
        ),
        (
            "match_contains",
            "// @mode: run\n// @expect: success\n// @match: exact\n// @contains: partial\n",
            "@contains conflicts with exact output matching",
        ),
        (
            "block_contains",
            "// @mode: run\n// @expect: success\n// @match-begin\n// A\n// B\n// @match-end\n// @contains: A\n",
            "@contains conflicts with exact output matching",
        ),
        (
            "match_block",
            "// @mode: run\n// @expect: success\n// @match: exact\n// @match-begin\n// A\n// B\n// @match-end\n",
            "@match conflicts with @match-begin",
        ),
        (
            "stderr_conflict",
            "// @mode: run\n// @expect: success\n// @stderr-match: exact\n// @stderr-contains: partial\n",
            "@stderr-contains conflicts with @stderr-match",
        ),
    ];
    for (name, source, _) in &cases {
        write(dir.path(), &format!("{name}.anv"), source);
    }

    let input = args([
        "--quiet",
        "--report-json",
        dir.path().to_str().expect("utf8 fixture path"),
    ]);
    let (output, report) = json(&input);
    assert!(!output.status.success());
    assert_eq!(report["failed"], cases.len() as u64);
    assert_eq!(report["compile_failed"], cases.len() as u64);

    let messages = report["issues"]
        .as_array()
        .expect("issues array")
        .iter()
        .map(|issue| issue["message"].as_str().expect("issue message"))
        .collect::<Vec<_>>();
    for (_, _, expected) in cases {
        assert!(messages.iter().any(|message| message.contains(expected)));
    }
}

#[test]
fn runner_reports_observable_fixture_outcomes() {
    let dir = TempDir::new().expect("temporary fixtures");
    write(
        dir.path(),
        "a_compile_failure.anv",
        "// @mode: check\n// @expect: success\nfn main( {\n",
    );
    write(
        dir.path(),
        "b_assertion_failure.anv",
        "// @mode: run\n// @expect: success\n// @match: 2\nfn main() { println(1); }\n",
    );
    write(
        dir.path(),
        "c_runtime_failure.anv",
        "// @mode: run\n// @expect: success\nfn main() { var xs: [int; 2] = [0, 1]; let i = 2; println(xs[i]); }\n",
    );
    write(
        dir.path(),
        "d_expected_error.anv",
        "// @mode: run\n// @expect: error\n// @stderr-contains: array index 2 out of bounds for len 2\nfn main() { var xs: [int; 2] = [0, 1]; let i = 2; println(xs[i]); }\n",
    );
    write(
        dir.path(),
        "e_expectation_mismatch.anv",
        "// @mode: run\n// @expect: error\nfn main() { println(1); }\n",
    );
    write(dir.path(), "f_helper.anv", "// @helper\n");
    write(
        dir.path(),
        "g_skip.anv",
        "// @mode: check\n// @expect: success\n// @skip: unavailable on this platform\n",
    );

    let input = args([
        "--quiet",
        "--report-json",
        "--jobs",
        "1",
        dir.path().to_str().expect("utf8 fixture path"),
    ]);
    let (output, report) = json(&input);
    assert!(!output.status.success());
    assert_eq!(report["passed"], 1, "{report}");
    assert_eq!(report["failed"], 4, "{report}");
    assert_eq!(report["compile_failed"], 2, "{report}");
    assert_eq!(report["runtime_failed"], 2, "{report}");
    assert_eq!(report["skipped"], 1);
    assert_eq!(report["helpers"], 1);

    let issues = report["issues"].as_array().expect("issues array");
    let paths = issues
        .iter()
        .map(|issue| issue["path"].as_str().expect("issue path"))
        .collect::<Vec<_>>();
    let mut sorted = paths.clone();
    sorted.sort_unstable();
    assert_eq!(paths, sorted);
    assert!(issues.iter().any(|issue| {
        issue["message"]
            .as_str()
            .expect("issue message")
            .contains("Expected stdout")
    }));
}

#[test]
fn stderr_and_warning_assertions_are_exercised_through_a_provider_fixture() {
    let dir = TempDir::new().expect("temporary project");
    let fixtures_dir = write_stderr_provider(dir.path());
    let source =
        "import pkg:host.host { stderr_probe };\n\nfn main() { println(stderr_probe()); }\n";
    let fixtures = [
        (
            "exact.anv",
            "// @mode: run\n// @expect: success\n// @match: 0\n// @stderr-match: runner stderr\n",
        ),
        (
            "contains.anv",
            "// @mode: run\n// @expect: success\n// @stderr-contains: runner\n",
        ),
        (
            "selected_stream.anv",
            "// @mode: run\n// @expect: success\n// @contains: runner stderr\n",
        ),
        (
            "warning.anv",
            "// @mode: run\n// @expect: success\n// @warn-contains: runner stderr\n",
        ),
    ];
    for (name, directives) in fixtures {
        write(&fixtures_dir, name, &format!("{directives}{source}"));
    }
    write(
        &fixtures_dir,
        "warning_stream.anv",
        "// @mode: run\n// @expect: success\n// @warn-contains: 987654321\nfn main() { println(987654321); }\n",
    );

    let input = args([
        "--quiet",
        "--report-json",
        "--jobs",
        "1",
        fixtures_dir.to_str().expect("utf8 fixture path"),
    ]);
    let (output, report) = json(&input);
    assert!(!output.status.success());
    assert_eq!(report["passed"], 2, "{report}");
    assert_eq!(report["failed"], 3, "{report}");
    assert_eq!(report["compile_failed"], 1, "{report}");
    assert_eq!(report["runtime_failed"], 2, "{report}");
    let issues = report["issues"].as_array().expect("issues array");
    assert!(issues.iter().any(|issue| {
        issue["message"]
            .as_str()
            .expect("issue message")
            .contains("Expected stdout to contain")
    }));
}

#[test]
fn timeout_expectations_are_exercised_through_runner_processes() {
    let dir = TempDir::new().expect("temporary fixtures");
    let check = write(
        dir.path(),
        "check_timeout.anv",
        "// @mode: check\n// @expect: timeout\nfn main() {}\n",
    );
    let run = write(
        dir.path(),
        "run_timeout.anv",
        "// @mode: run\n// @expect: timeout\nfn main() { while true {} }\n",
    );

    let (output, report) = json(&args([
        "--quiet",
        "--report-json",
        "--compile-timeout",
        "1",
        check.to_str().expect("utf8 fixture path"),
    ]));
    assert!(output.status.success(), "{report}");
    assert_eq!(report["passed"], 1, "{report}");

    let (output, report) = json(&args([
        "--quiet",
        "--report-json",
        "--timeout",
        "1",
        "--compile-timeout",
        "1",
        run.to_str().expect("utf8 fixture path"),
    ]));
    assert!(output.status.success(), "{report}");
    assert_eq!(report["passed"], 1, "{report}");
}
