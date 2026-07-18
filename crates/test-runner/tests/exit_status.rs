use std::{fs, process::Command};

fn run(report_json: bool) -> std::process::Output {
    let dir = tempfile::tempdir().unwrap();
    let fixture = dir.path().join("unexpected_failure.anv");
    fs::write(&fixture, "fn main() {}\n").unwrap();

    let mut command = Command::new(env!("CARGO_BIN_EXE_test-runner"));
    if report_json {
        command.arg("--report-json");
    }
    command.arg(fixture).output().unwrap()
}

#[test]
fn unexpected_failure_returns_nonzero_after_human_report() {
    let output = run(false);
    let stderr = String::from_utf8(output.stderr).unwrap();

    assert!(!output.status.success());
    assert!(stderr.contains("Test Result:"));
    assert!(stderr.contains("FAILED"));
    assert!(stderr.contains("failed"));
}

#[test]
fn unexpected_failure_returns_nonzero_after_json_report() {
    let output = run(true);
    let report: serde_json::Value = serde_json::from_slice(&output.stdout).unwrap();

    assert!(!output.status.success());
    assert_eq!(report["failed"], 1);
    assert_eq!(report["issues"][0]["kind"], "compile_failed");
}
