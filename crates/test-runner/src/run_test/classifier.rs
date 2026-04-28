use super::{
    ProcessOutcome,
    assertions::{self, AssertionInput},
};
use crate::{
    directives::{Assertions, TestContract},
    model::{ExpectedResult, FailurePhase, Mode, TestResult},
};

const STDERR_COMPILE_ERROR_MARKER: &str = "Compile error";
const STDERR_RUNTIME_ERROR_MARKER: &str = "Runtime error";

pub(super) fn classify_test_result(
    outcome: ProcessOutcome,
    contract: TestContract,
    assertions: &Assertions,
) -> TestResult {
    let actual = classify_process_outcome(outcome, contract);
    classify_text_outcome(actual, contract.expect, contract.mode, assertions)
}

fn classify_text_outcome(
    actual: ActualOutcome,
    expected: ExpectedResult,
    mode: Mode,
    assertions: &Assertions,
) -> TestResult {
    match (actual, expected) {
        (ActualOutcome::Success { stdout, stderr }, ExpectedResult::Success) => {
            let input = AssertionInput::success(mode, &stdout, &stderr);
            assertions::check(assertions, &input)
        }
        (
            ActualOutcome::Error {
                phase,
                stdout,
                stderr,
            },
            ExpectedResult::Error,
        ) => {
            let input = AssertionInput::error(mode, phase, &stdout, &stderr);
            assertions::check(assertions, &input)
        }
        (ActualOutcome::Timeout { .. }, ExpectedResult::Timeout) => TestResult::Pass,
        (actual, expected) => classify_mismatch(actual, expected, mode),
    }
}

fn classify_process_outcome(outcome: ProcessOutcome, contract: TestContract) -> ActualOutcome {
    match outcome {
        ProcessOutcome::Completed {
            exit_code,
            stdout,
            stderr,
        } => classify_exit(exit_code, stdout, stderr, contract),
        ProcessOutcome::Timeout { phase } => ActualOutcome::Timeout { phase },
    }
}

fn classify_exit(
    exit_code: Option<i32>,
    stdout: String,
    stderr: String,
    contract: TestContract,
) -> ActualOutcome {
    let Some(exit_code) = exit_code else {
        return ActualOutcome::Error {
            phase: failure_phase_for_output(contract.mode, &stderr),
            stdout,
            stderr,
        };
    };

    if exit_code == contract.success_exit_code() {
        return ActualOutcome::Success { stdout, stderr };
    }
    if let Some(expected) = contract.user_exit_code() {
        return ActualOutcome::ExitMismatch {
            expected,
            actual: exit_code,
        };
    }

    ActualOutcome::Error {
        phase: failure_phase_for_output(contract.mode, &stderr),
        stdout,
        stderr,
    }
}

fn classify_mismatch(actual: ActualOutcome, expected: ExpectedResult, mode: Mode) -> TestResult {
    match (actual, expected) {
        (ActualOutcome::Success { .. }, ExpectedResult::Error) => TestResult::Fail {
            phase: expectation_mismatch_phase(mode),
            message: "Expected error but got success".to_string(),
        },
        (ActualOutcome::Success { stdout, stderr }, ExpectedResult::Timeout) => {
            let merged = format!("{stdout}{stderr}");
            TestResult::Fail {
                phase: expectation_mismatch_phase(mode),
                message: format!("Expected timeout but got success:\n{merged}"),
            }
        }
        (
            ActualOutcome::Error {
                phase,
                stdout,
                stderr,
            },
            ExpectedResult::Success,
        ) => {
            let merged = format!("{stdout}{stderr}");
            TestResult::Fail {
                phase,
                message: format!("Expected success but got error:\n{merged}"),
            }
        }
        (
            ActualOutcome::Error {
                phase,
                stdout,
                stderr,
            },
            ExpectedResult::Timeout,
        ) => {
            let merged = format!("{stdout}{stderr}");
            TestResult::Fail {
                phase,
                message: format!("Expected timeout but got error:\n{merged}"),
            }
        }
        (ActualOutcome::Timeout { phase }, ExpectedResult::Success | ExpectedResult::Error) => {
            TestResult::Timeout { phase }
        }
        (ActualOutcome::ExitMismatch { expected, actual }, _) => TestResult::Fail {
            phase: FailurePhase::Runtime,
            message: format!("Expected exit code {expected} but got {actual}"),
        },
        _ => unreachable!("classify_mismatch called for matching outcome"),
    }
}

fn expectation_mismatch_phase(mode: Mode) -> FailurePhase {
    match mode {
        Mode::Check => FailurePhase::Compile,
        Mode::Run => FailurePhase::Runtime,
    }
}

fn failure_phase_for_mode(mode: Mode) -> FailurePhase {
    match mode {
        Mode::Check | Mode::Run => FailurePhase::Compile,
    }
}

fn failure_phase_for_output(mode: Mode, stderr: &str) -> FailurePhase {
    phase_from_stderr_marker(stderr).unwrap_or_else(|| failure_phase_for_mode(mode))
}

pub(super) fn phase_from_stderr_marker(stderr: &str) -> Option<FailurePhase> {
    if stderr.contains(STDERR_RUNTIME_ERROR_MARKER) {
        return Some(FailurePhase::Runtime);
    }
    if stderr.contains(STDERR_COMPILE_ERROR_MARKER) {
        return Some(FailurePhase::Compile);
    }
    None
}

#[derive(Debug)]
enum ActualOutcome {
    Success {
        stdout: String,
        stderr: String,
    },
    Error {
        phase: FailurePhase,
        stdout: String,
        stderr: String,
    },
    Timeout {
        phase: FailurePhase,
    },
    ExitMismatch {
        expected: i32,
        actual: i32,
    },
}

#[cfg(test)]
mod tests {
    use super::classify_test_result;
    use crate::{
        directives::Directives,
        model::{FailurePhase, TestResult},
        run_test::ProcessOutcome,
    };

    const USER_EXIT_CODE: i32 = 121;
    const MARKER_IGNORED_EXIT_CODE: i32 = 120;

    fn directives(src: &str) -> Directives {
        let mut fixture = String::new();
        if !src.contains("// @mode:") {
            fixture.push_str("// @mode: run\n");
        }
        if !src.contains("// @expect:") {
            fixture.push_str("// @expect: success\n");
        }
        fixture.push_str(src);
        Directives::parse(&fixture).expect("directives should parse")
    }

    fn completed(exit_code: Option<i32>, stdout: &str, stderr: &str) -> ProcessOutcome {
        ProcessOutcome::Completed {
            exit_code,
            stdout: stdout.to_string(),
            stderr: stderr.to_string(),
        }
    }

    fn classify(outcome: ProcessOutcome, directives: &Directives) -> TestResult {
        classify_test_result(outcome, directives.contract, &directives.assertions)
    }

    #[test]
    fn process_exit_preserves_missing_exit_code() {
        let directives = directives("// @exit-code: 1\n");
        let result = classify(completed(None, "", ""), &directives);

        assert!(matches!(
            result,
            TestResult::Fail {
                phase: FailurePhase::Compile,
                ..
            }
        ));
    }

    #[test]
    fn exit_code_directive_accepts_matching_user_exit() {
        let directives = directives("// @exit-code: 7\n");
        let result = classify(completed(Some(7), "", ""), &directives);

        assert!(matches!(result, TestResult::Pass));
    }

    #[test]
    fn exit_code_directive_accepts_any_matching_user_exit() {
        let directives = directives(&format!("// @exit-code: {USER_EXIT_CODE}\n"));
        let result = classify(completed(Some(USER_EXIT_CODE), "", ""), &directives);

        assert!(matches!(result, TestResult::Pass));
    }

    #[test]
    fn exit_code_directive_rejects_mismatched_user_exit() {
        let directives = directives("// @exit-code: 7\n");
        let result = classify(completed(Some(8), "", ""), &directives);

        assert!(matches!(
            result,
            TestResult::Fail {
                phase: FailurePhase::Runtime,
                message,
            } if message == "Expected exit code 7 but got 8"
        ));
    }

    #[test]
    fn stderr_marker_classifies_runtime_before_mode_fallback() {
        let directives = directives("// @mode: check\n");
        let result = classify(completed(Some(1), "", "Runtime error: boom\n"), &directives);

        assert!(matches!(
            result,
            TestResult::Fail {
                phase: FailurePhase::Runtime,
                ..
            }
        ));
    }

    #[test]
    fn stderr_marker_classifies_compile_before_mode_fallback() {
        let directives = directives("// @mode: run\n");
        let result = classify(
            completed(Some(1), "", "Compile error: type mismatch\n"),
            &directives,
        );

        assert!(matches!(
            result,
            TestResult::Fail {
                phase: FailurePhase::Compile,
                ..
            }
        ));
    }

    #[test]
    fn stderr_marker_wins_over_exit_code_value() {
        let directives = directives("");
        let result = classify(
            completed(
                Some(MARKER_IGNORED_EXIT_CODE),
                "",
                "Runtime error: marker\n",
            ),
            &directives,
        );

        assert!(matches!(
            result,
            TestResult::Fail {
                phase: FailurePhase::Runtime,
                ..
            }
        ));
    }

    #[test]
    fn unknown_nonzero_run_failure_defaults_to_compile_without_marker() {
        let directives = directives("");
        let result = classify(completed(Some(1), "", ""), &directives);

        assert!(matches!(
            result,
            TestResult::Fail {
                phase: FailurePhase::Compile,
                ..
            }
        ));
    }

    #[test]
    fn timeout_expectation_accepts_compile_and_runtime_timeouts() {
        let directives = directives("// @expect: timeout\n");

        let compile = classify(
            ProcessOutcome::Timeout {
                phase: FailurePhase::Compile,
            },
            &directives,
        );
        let runtime = classify(
            ProcessOutcome::Timeout {
                phase: FailurePhase::Runtime,
            },
            &directives,
        );

        assert!(matches!(compile, TestResult::Pass));
        assert!(matches!(runtime, TestResult::Pass));
    }

    #[test]
    fn expectation_mismatch_fails_without_text_assertions() {
        let directives = directives("// @expect: error\n");
        let result = classify(completed(Some(0), "", ""), &directives);

        assert!(matches!(
            result,
            TestResult::Fail {
                phase: FailurePhase::Runtime,
                ..
            }
        ));
    }

    #[test]
    fn expected_error_run_mode_checks_stderr() {
        let directives = directives("// @expect: error\n// @contains: runtime failed\n");
        let result = classify(
            completed(Some(1), "", "Runtime error\nruntime failed\n"),
            &directives,
        );

        assert!(matches!(result, TestResult::Pass));
    }

    #[test]
    fn expected_error_check_mode_checks_merged_output() {
        let directives =
            directives("// @mode: check\n// @expect: error\n// @contains: type mismatch\n");
        let result = classify(completed(Some(1), "type ", "mismatch\n"), &directives);

        assert!(matches!(result, TestResult::Pass));
    }
}
