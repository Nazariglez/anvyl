mod assertions;
mod classifier;
mod cli;

use std::{
    io::Read,
    path::{Path, PathBuf},
    process::Stdio,
    time::{Duration, Instant},
};

pub(crate) use cli::Cli;

use self::classifier::classify_test_result;
use crate::{
    directives::{Assertions, CliOptions, Directives, TestContract},
    model::{ExpectedResult, FailurePhase, Mode, RunTestResult, TestResult},
};

#[derive(Debug)]
pub(super) enum ProcessOutcome {
    Completed {
        exit_code: Option<i32>,
        stdout: String,
        stderr: String,
    },
    Timeout {
        phase: FailurePhase,
    },
}

pub(super) struct CliCase {
    pub(super) file: PathBuf,
    pub(super) mode: Mode,
    pub(super) runtime_timeout: Duration,
    pub(super) compile_timeout: Duration,
    pub(super) cli_options: CliOptions,
    pub(super) stdin: String,
}

pub(crate) enum TestPlan {
    Done(RunTestResult),
    Run {
        contract: TestContract,
        assertions: Assertions,
        case: CliCase,
    },
}

pub(crate) fn plan_test_file(
    file: &Path,
    runtime_timeout: Duration,
    compile_timeout: Duration,
) -> Result<TestPlan, String> {
    let src = std::fs::read_to_string(file).map_err(|e| e.to_string())?;
    let directives = Directives::parse(&src)?;
    Ok(plan_test(
        file,
        directives,
        runtime_timeout,
        compile_timeout,
    ))
}

pub(crate) fn run_test_file(
    file: &Path,
    runtime_timeout: Duration,
    compile_timeout: Duration,
    cli: &Cli,
) -> Result<RunTestResult, String> {
    let (contract, assertions, case) = match plan_test_file(file, runtime_timeout, compile_timeout)?
    {
        TestPlan::Done(result) => return Ok(result),
        TestPlan::Run {
            contract,
            assertions,
            case,
        } => (contract, assertions, case),
    };

    let start_time = Instant::now();
    let mode = case.mode;
    let outcome = cli.run(&case)?;
    let elapsed = start_time.elapsed();
    let result = classify_test_result(outcome, contract, &assertions);

    Ok(RunTestResult {
        result,
        mode,
        duration: elapsed,
    })
}

fn plan_test(
    file: &Path,
    directives: Directives,
    runtime_timeout: Duration,
    compile_timeout: Duration,
) -> TestPlan {
    let contract = directives.contract;
    if directives.helper {
        return TestPlan::Done(done(TestResult::Helper, contract.mode));
    }
    if let Some(reason) = directives.skip {
        return TestPlan::Done(done(TestResult::Skip { message: reason }, contract.mode));
    }

    TestPlan::Run {
        contract,
        assertions: directives.assertions,
        case: CliCase {
            file: file.to_path_buf(),
            mode: contract.mode,
            runtime_timeout,
            compile_timeout,
            cli_options: directives.cli_options,
            stdin: directives.stdin.text(),
        },
    }
}

pub(crate) fn is_batch_eligible(plan: &TestPlan) -> bool {
    match plan {
        TestPlan::Run { contract, case, .. } => {
            contract.mode == Mode::Run
                && contract.expect == ExpectedResult::Success
                && case.cli_options.is_empty()
                && case.stdin.is_empty()
                && !case_blocks_batch(&case.file)
        }
        TestPlan::Done(_) => false,
    }
}

fn case_blocks_batch(file: &Path) -> bool {
    !matches!(
        anvyx_project::manifest::find_nearest_manifest(file),
        Ok(None)
    )
}

pub(crate) fn run_binary_case(plan: TestPlan, binary: &Path) -> Result<RunTestResult, String> {
    let TestPlan::Run {
        contract,
        assertions,
        case,
    } = plan
    else {
        return Err("expected runnable batch case".to_string());
    };
    let start_time = Instant::now();
    let outcome = spawn_binary(binary, case.runtime_timeout)?;
    let elapsed = start_time.elapsed();
    let result = classify_test_result(outcome, contract, &assertions);
    Ok(RunTestResult {
        result,
        mode: case.mode,
        duration: elapsed,
    })
}

fn spawn_binary(binary: &Path, timeout: Duration) -> Result<ProcessOutcome, String> {
    let mut child = std::process::Command::new(binary)
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .map_err(|e| e.to_string())?;
    let res =
        wait_timeout::ChildExt::wait_timeout(&mut child, timeout).map_err(|e| e.to_string())?;
    if let Some(status) = res {
        let (stdout, stderr) = read_child_output(&mut child);
        return Ok(ProcessOutcome::Completed {
            exit_code: status.code(),
            stdout,
            stderr,
        });
    }
    let _ = child.kill();
    let _ = child.wait();
    Ok(ProcessOutcome::Timeout {
        phase: FailurePhase::Runtime,
    })
}

fn read_child_output(child: &mut std::process::Child) -> (String, String) {
    let mut stdout = String::new();
    let mut stderr = String::new();
    if let Some(mut out) = child.stdout.take() {
        let _ = out.read_to_string(&mut stdout);
    }
    if let Some(mut err) = child.stderr.take() {
        let _ = err.read_to_string(&mut stderr);
    }
    (stdout, stderr)
}

fn done(result: TestResult, mode: Mode) -> RunTestResult {
    RunTestResult {
        result,
        mode,
        duration: Duration::ZERO,
    }
}

#[cfg(test)]
mod tests {
    use std::{path::Path, time::Duration};

    use super::{TestPlan, is_batch_eligible, plan_test};
    use crate::directives::Directives;

    fn directives(src: &str) -> Directives {
        Directives::parse(src).unwrap()
    }

    fn plan(src: &str) -> TestPlan {
        plan_file(Path::new("test.anv"), src)
    }

    fn plan_file(file: &Path, src: &str) -> TestPlan {
        plan_test(
            file,
            directives(src),
            Duration::from_millis(1),
            Duration::from_millis(2),
        )
    }

    #[test]
    fn stdin_blocks_batch() {
        let plan = plan("// @mode: run\n// @expect: success\n// @stdin: hello\n");

        assert!(!is_batch_eligible(&plan));
    }
}
