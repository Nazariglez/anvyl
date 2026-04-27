use std::{
    io::Read,
    path::{Path, PathBuf},
    time::{Duration, Instant},
};

use wait_timeout::ChildExt;

use crate::directives::Directives;

const ORANGE: &str = "\x1b[93m";
const RESET: &str = "\x1b[0m";

pub struct RunTestResult {
    pub result: TestResult,
    pub mode: Mode,
    pub backend: Option<&'static str>,
    pub duration: Duration,
}

pub fn run_test_file(
    cmd: &str,
    file: &Path,
    timeout: Duration,
    backend: Option<&'static str>,
) -> Result<RunTestResult, String> {
    let src = std::fs::read_to_string(file).map_err(|e| e.to_string())?;
    let directives = Directives::new(&src);
    if directives.helper {
        return Ok(RunTestResult {
            result: TestResult::Helper,
            mode: directives.mode,
            backend: None,
            duration: Duration::from_secs(0),
        });
    }
    if let Some(reason) = &directives.skip {
        return Ok(RunTestResult {
            result: TestResult::Skip {
                message: reason.clone(),
            },
            mode: directives.mode,
            backend: None,
            duration: Duration::from_secs(0),
        });
    }

    let effective_backend = match directives.mode {
        Mode::Run => backend,
        Mode::Check => None,
    };

    let start_time = Instant::now();
    let outcome = spawn_test_process(
        cmd,
        file,
        timeout,
        directives.mode,
        effective_backend,
        &directives.lint,
        &directives.feature,
        &directives.cfg,
    )?;
    let elapsed = start_time.elapsed();

    let res = match (outcome, directives.expect, directives.mode) {
        (ProcessOutcome::Pass { stdout, stderr }, ExpectedResult::Success, Mode::Run) => {
            let res = match_output(&stdout, &directives);
            match res {
                TestResult::Pass => check_warn_contains(&stderr, &directives),
                other => other,
            }
        }
        (ProcessOutcome::Pass { stdout, stderr }, ExpectedResult::Success, Mode::Check) => {
            let merged = format!("{stdout}{stderr}");
            let res = match_output(&merged, &directives);
            match res {
                TestResult::Pass => check_warn_contains(&stderr, &directives),
                other => other,
            }
        }
        (ProcessOutcome::Pass { .. }, ExpectedResult::Error, _) => TestResult::Fail {
            message: "Expected error but got success".to_string(),
        },
        (ProcessOutcome::Pass { stdout, stderr }, ExpectedResult::Timeout, _) => {
            let merged = format!("{stdout}{stderr}");
            TestResult::Fail {
                message: format!("Expected timeout but got success:\n{merged}"),
            }
        }
        (ProcessOutcome::Fail { stdout, stderr }, ExpectedResult::Success, _) => {
            let merged = format!("{stdout}{stderr}");
            TestResult::Fail {
                message: format!("Expected success but got error:\n{merged}"),
            }
        }
        (ProcessOutcome::Fail { stdout, stderr }, ExpectedResult::Timeout, _) => {
            let merged = format!("{stdout}{stderr}");
            TestResult::Fail {
                message: format!("Expected timeout but got error:\n{merged}"),
            }
        }
        (ProcessOutcome::Fail { stderr, .. }, ExpectedResult::Error, Mode::Run) => {
            match_output(&stderr, &directives)
        }
        (ProcessOutcome::Fail { stdout, stderr }, ExpectedResult::Error, Mode::Check) => {
            let merged = format!("{stdout}{stderr}");
            match_output(&merged, &directives)
        }
        (ProcessOutcome::Timeout, ExpectedResult::Success | ExpectedResult::Error, _) => {
            TestResult::Timeout
        }
        (ProcessOutcome::Timeout, ExpectedResult::Timeout, _) => TestResult::Pass,
    };

    Ok(RunTestResult {
        result: res,
        mode: directives.mode,
        backend: effective_backend,
        duration: elapsed,
    })
}

fn check_warn_contains(stderr: &str, directives: &Directives) -> TestResult {
    for expected in &directives.warn_contains {
        if !stderr.lines().any(|ln| ln.contains(expected.as_str())) {
            return TestResult::Fail {
                message: format!(
                    "* Expected warning containing:\n{expected}\n* Got stderr:\n{stderr}"
                ),
            };
        }
    }
    TestResult::Pass
}

fn match_output(output: &str, directives: &Directives) -> TestResult {
    // exact match, multilnne or not
    if let Some(expected) = &directives.match_exact {
        let expected_lines = expected.lines();
        let lns = output.lines();
        let same_lines_num = lns.count() == expected_lines.count();
        if !same_lines_num {
            return TestResult::Fail {
                message: format!("* Expected:\n{expected}\n* Got:\n{output}"),
            };
        }

        let expected_lines = expected.lines();
        let lns = output.lines();
        let join_iter = lns.zip(expected_lines);
        for (idx, (ln, expected_ln)) in join_iter.enumerate() {
            if ln != expected_ln {
                return TestResult::Fail {
                    message: format!(
                        "* Line {idx} failed\n* Expected:\n{expected}\n* Got:\n{output}",
                    ),
                };
            }
        }

        return TestResult::Pass;
    }

    // check if any line contains the expected text
    for expected_ln in &directives.contains {
        let found = output.lines().any(|ln| ln.contains(expected_ln));
        if !found {
            return TestResult::Fail {
                message: format!("* Expected output to contain:\n{expected_ln}\n* Got:\n{output}"),
            };
        }
    }

    TestResult::Pass
}

#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub enum Mode {
    #[default]
    Run,
    Check,
}

impl Mode {
    pub fn from_str(s: &str) -> Self {
        match s {
            "check" => Self::Check,
            "run" => Self::Run,
            _ => panic!("Invalid mode: {s}"),
        }
    }
}

impl std::fmt::Display for Mode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Run => write!(f, "run"),
            Self::Check => write!(f, "check"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum ExpectedResult {
    #[default]
    Success,
    Error,
    Timeout,
}

impl ExpectedResult {
    pub fn from_str(s: &str) -> Self {
        match s {
            "success" => Self::Success,
            "error" => Self::Error,
            "timeout" => Self::Timeout,
            _ => panic!("Invalid expected result: {s}"),
        }
    }
}

impl std::fmt::Display for ExpectedResult {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

#[derive(Debug)]
pub enum TestResult {
    Pass,
    Fail { message: String },
    Timeout,
    Skip { message: String },
    Helper,
}

#[derive(Debug)]
enum ProcessOutcome {
    Pass { stdout: String, stderr: String },
    Fail { stdout: String, stderr: String },
    Timeout,
}

pub fn compile_lang(release: bool) -> Result<String, String> {
    // TODO: release? backend?
    println!(
        "{ORANGE}Compiling anvyx{}{RESET}",
        if release { " (release)..." } else { "..." }
    );
    let mut command = std::process::Command::new("cargo");
    command.arg("build").arg("--package").arg("anvyx");
    if release {
        command.arg("--release");
    }
    let output = command.output().map_err(|e| e.to_string())?;
    if !output.status.success() {
        let stdout = String::from_utf8_lossy(&output.stdout);
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(format!("Build failed\n{stdout}{stderr}"));
    }

    let profile = if release { "release" } else { "debug" };
    let exe_name = if cfg!(target_os = "windows") {
        "anvyx.exe"
    } else {
        "anvyx"
    };
    let target_root = std::env::var("CARGO_TARGET_DIR").unwrap_or_else(|_| "target".to_string());
    let exe_path = PathBuf::from(target_root).join(profile).join(exe_name);
    Ok(exe_path.display().to_string())
}

fn spawn_test_process(
    cmd: &str,
    file: &Path,
    timeout: Duration,
    mode: Mode,
    backend: Option<&str>,
    lint_overrides: &[String],
    feature_overrides: &[String],
    cfg_overrides: &[String],
) -> Result<ProcessOutcome, String> {
    let subcommand = match mode {
        Mode::Check => "check",
        Mode::Run => "run",
    };

    let mut command = std::process::Command::new(cmd);
    command.arg(subcommand);

    if let Some(b) = backend {
        command.args(["--backend", b]);
    }

    for lint_arg in lint_overrides {
        command.args(["--lint", lint_arg]);
    }
    for feat_arg in feature_overrides {
        command.args(["--feature", feat_arg]);
    }
    for cfg_arg in cfg_overrides {
        command.args(["--cfg", cfg_arg]);
    }

    let mut child = command
        .arg(file.display().to_string())
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .spawn()
        .map_err(|e| e.to_string())?;

    let res = child.wait_timeout(timeout).map_err(|e| e.to_string())?;
    if let Some(status) = res {
        let mut stdout = String::new();
        let mut stderr = String::new();

        if let Some(mut out) = child.stdout.take() {
            let _ = out.read_to_string(&mut stdout);
        }
        if let Some(mut err) = child.stderr.take() {
            let _ = err.read_to_string(&mut stderr);
        }

        if status.success() {
            Ok(ProcessOutcome::Pass { stdout, stderr })
        } else {
            Ok(ProcessOutcome::Fail { stdout, stderr })
        }
    } else {
        let _ = child.kill();
        let _ = child.wait();
        Ok(ProcessOutcome::Timeout)
    }
}
