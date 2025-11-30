use wait_timeout::ChildExt;

use crate::directives::Directives;
use std::{
    io::Read,
    path::PathBuf,
    time::{Duration, Instant},
};

const ORANGE: &str = "\x1b[93m";
const RESET: &str = "\x1b[0m";

pub struct RunTestResult {
    pub result: TestResult,
    pub mode: Mode,
    pub duration: Duration,
}

pub fn run_test_file(
    cmd: &str,
    file: &PathBuf,
    timeout: Duration,
) -> Result<RunTestResult, String> {
    let src = std::fs::read_to_string(file).map_err(|e| e.to_string())?;
    let directives = Directives::new(&src);
    if directives.skip.is_some() {
        return Ok(RunTestResult {
            result: TestResult::Skip {
                message: directives.skip.unwrap(),
            },
            mode: directives.mode,
            duration: Duration::from_secs(0),
        });
    }

    let start_time = Instant::now();
    let outcome = spawn_test_process(cmd, file, timeout, directives.mode)?;
    let elapsed = start_time.elapsed();

    let res = match (outcome, directives.expect) {
        (ProcessOutcome::Pass { output }, ExpectedResult::Success) => {
            match_output(&output, &directives)?
        }
        (ProcessOutcome::Pass { .. }, ExpectedResult::Error) => TestResult::Fail {
            message: format!("Expected error but got success"),
        },
        (ProcessOutcome::Pass { output }, ExpectedResult::Timeout) => TestResult::Fail {
            message: format!("Expected timeout but got success:\n{output}"),
        },
        (ProcessOutcome::Fail { message }, ExpectedResult::Success) => TestResult::Fail {
            message: format!("Expected success but got error:\n{message}"),
        },
        (ProcessOutcome::Fail { message }, ExpectedResult::Timeout) => TestResult::Fail {
            message: format!("Expected timeout but got error:\n{message}"),
        },
        (ProcessOutcome::Fail { message }, ExpectedResult::Error) => {
            match_output(&message, &directives)?
        }
        (ProcessOutcome::Timeout, ExpectedResult::Success) => TestResult::Timeout,
        (ProcessOutcome::Timeout, ExpectedResult::Error) => TestResult::Timeout,
        (ProcessOutcome::Timeout, ExpectedResult::Timeout) => TestResult::Pass,
    };

    Ok(RunTestResult {
        result: res,
        mode: directives.mode,
        duration: elapsed,
    })
}

fn match_output(output: &str, directives: &Directives) -> Result<TestResult, String> {
    // exact match, multilnne or not
    if let Some(expected) = &directives.match_exact {
        let expected_lines = expected.lines();
        let lns = output.lines();
        let same_lines_num = lns.count() == expected_lines.count();
        if !same_lines_num {
            return Ok(TestResult::Fail {
                message: format!("* Expected:\n{expected}\n* Got:\n{output}"),
            });
        }

        let expected_lines = expected.lines();
        let lns = output.lines();
        let join_iter = lns.zip(expected_lines);
        for (idx, (ln, expected_ln)) in join_iter.enumerate() {
            if ln != expected_ln {
                return Ok(TestResult::Fail {
                    message: format!(
                        "* Line {idx} failed\n* Expected:\n{expected}\n* Got:\n{output}",
                    ),
                });
            }
        }

        return Ok(TestResult::Pass);
    }

    // check if any line contains the expected text
    for expected_ln in directives.contains.iter() {
        let found = output.lines().any(|ln| ln.contains(expected_ln));
        if !found {
            return Ok(TestResult::Fail {
                message: format!("* Expected output to contain:\n{expected_ln}\n* Got:\n{output}",),
            });
        }
    }

    Ok(TestResult::Pass)
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
            _ => panic!("Invalid mode: {}", s),
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
            _ => panic!("Invalid expected result: {}", s),
        }
    }
}

impl std::fmt::Display for ExpectedResult {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(Debug)]
pub enum TestResult {
    Pass,
    Fail { message: String },
    Timeout,
    Skip { message: String },
}

#[derive(Debug)]
enum ProcessOutcome {
    Pass { output: String },
    Fail { message: String },
    Timeout,
}

pub fn compile_lang(release: bool) -> Result<String, String> {
    // TODO: release? backend?
    println!(
        "{ORANGE}Compiling anvyl{}{RESET}",
        if release { " (release)..." } else { "..." }
    );
    let mut child = std::process::Command::new("cargo")
        .arg("build")
        .arg("--package")
        .arg("anvyl")
        .args(if release { vec!["--release"] } else { vec![] })
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .spawn()
        .map_err(|e| e.to_string())?;

    let status = child.wait().map_err(|e| e.to_string())?;
    if !status.success() {
        return Err("Build failed".to_string());
    }

    let profile = if release { "release" } else { "debug" };
    let exe_name = if cfg!(target_os = "windows") {
        "anvyl.exe"
    } else {
        "anvyl"
    };
    let exe_path = PathBuf::from("target").join(profile).join(exe_name);
    Ok(exe_path.display().to_string())
}

fn spawn_test_process(
    cmd: &str,
    file: &PathBuf,
    timeout: Duration,
    mode: Mode,
) -> Result<ProcessOutcome, String> {
    // TODO: allows to set backend? debug or release?
    let mut child = std::process::Command::new(cmd)
        .arg(match mode {
            Mode::Check => "check",
            Mode::Run => "run",
        })
        .arg(file.display().to_string())
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .spawn()
        .map_err(|e| e.to_string())?;

    let res = child.wait_timeout(timeout).map_err(|e| e.to_string())?;
    match res {
        Some(status) => {
            let mut msg = String::new();

            if let Some(mut output) = child.stdout.take() {
                let _ = output.read_to_string(&mut msg);
            }
            if let Some(mut stderr) = child.stderr.take() {
                let _ = stderr.read_to_string(&mut msg);
            }

            if status.success() {
                Ok(ProcessOutcome::Pass { output: msg })
            } else {
                Ok(ProcessOutcome::Fail { message: msg })
            }
        }
        None => {
            let _ = child.kill();
            let _ = child.wait();
            Ok(ProcessOutcome::Timeout)
        }
    }
}
