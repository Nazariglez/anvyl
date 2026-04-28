use std::borrow::Cow;

use crate::{
    directives::{Assertions, ContainsAssertions, StreamAssertions},
    model::{FailurePhase, Mode, TestResult},
};

pub(super) struct NamedStream<'a> {
    label: &'static str,
    text: Cow<'a, str>,
    phase: FailurePhase,
}

impl<'a> NamedStream<'a> {
    fn borrowed(label: &'static str, text: &'a str, phase: FailurePhase) -> Self {
        Self {
            label,
            text: Cow::Borrowed(text),
            phase,
        }
    }

    fn owned(label: &'static str, text: String, phase: FailurePhase) -> Self {
        Self {
            label,
            text: Cow::Owned(text),
            phase,
        }
    }
}

pub(super) struct AssertionInput<'a> {
    selected: NamedStream<'a>,
    stderr: NamedStream<'a>,
    warnings: NamedStream<'a>,
}

impl<'a> AssertionInput<'a> {
    pub(super) fn success(mode: Mode, stdout: &'a str, stderr: &'a str) -> Self {
        match mode {
            Mode::Run => Self {
                selected: NamedStream::borrowed("stdout", stdout, FailurePhase::Runtime),
                stderr: NamedStream::borrowed("stderr", stderr, FailurePhase::Runtime),
                warnings: NamedStream::borrowed("stderr warning", stderr, FailurePhase::Compile),
            },
            Mode::Check => Self {
                selected: NamedStream::owned(
                    "output",
                    format!("{stdout}{stderr}"),
                    FailurePhase::Compile,
                ),
                stderr: NamedStream::borrowed("stderr", stderr, FailurePhase::Compile),
                warnings: NamedStream::borrowed("stderr warning", stderr, FailurePhase::Compile),
            },
        }
    }

    pub(super) fn error(mode: Mode, phase: FailurePhase, stdout: &'a str, stderr: &'a str) -> Self {
        match mode {
            Mode::Run => Self {
                selected: NamedStream::borrowed("stderr", stderr, phase),
                stderr: NamedStream::borrowed("stderr", stderr, phase),
                warnings: NamedStream::borrowed("stderr warning", "", phase),
            },
            Mode::Check => Self {
                selected: NamedStream::owned("output", format!("{stdout}{stderr}"), phase),
                stderr: NamedStream::borrowed("stderr", stderr, phase),
                warnings: NamedStream::borrowed("stderr warning", "", phase),
            },
        }
    }
}

pub(super) fn check(assertions: &Assertions, input: &AssertionInput<'_>) -> TestResult {
    let result = check_stream(&input.selected, &assertions.selected);
    let result = and_then_pass(result, || check_stream(&input.stderr, &assertions.stderr));
    and_then_pass(result, || {
        check_contains(&input.warnings, &assertions.warnings)
    })
}

fn and_then_pass(result: TestResult, next: impl FnOnce() -> TestResult) -> TestResult {
    match result {
        TestResult::Pass => next(),
        other => other,
    }
}

fn check_stream(stream: &NamedStream<'_>, assertions: &StreamAssertions) -> TestResult {
    check_program_stream(
        stream.label,
        &stream.text,
        assertions.exact.as_deref(),
        &assertions.contains,
        stream.phase,
    )
}

fn check_contains(stream: &NamedStream<'_>, assertions: &ContainsAssertions) -> TestResult {
    check_program_stream(
        stream.label,
        &stream.text,
        None,
        &assertions.contains,
        stream.phase,
    )
}

fn check_program_stream(
    label: &str,
    output: &str,
    match_exact: Option<&str>,
    contains: &[String],
    phase: FailurePhase,
) -> TestResult {
    if let Some(expected) = match_exact {
        let expected_lines = expected.lines();
        let output_lines = output.lines();
        if output_lines.count() != expected_lines.count() {
            return TestResult::Fail {
                phase,
                message: format!("* Expected {label}:\n{expected}\n* Got {label}:\n{output}"),
            };
        }

        for (idx, (line, expected_line)) in output.lines().zip(expected.lines()).enumerate() {
            if line != expected_line {
                return TestResult::Fail {
                    phase,
                    message: format!(
                        "* {label} line {idx} failed\n* Expected {label}:\n{expected}\n* Got {label}:\n{output}",
                    ),
                };
            }
        }

        return TestResult::Pass;
    }

    for expected_line in contains {
        if !output.lines().any(|line| line.contains(expected_line)) {
            return TestResult::Fail {
                phase,
                message: format!(
                    "* Expected {label} to contain:\n{expected_line}\n* Got {label}:\n{output}"
                ),
            };
        }
    }

    TestResult::Pass
}

#[cfg(test)]
mod tests {
    use super::{AssertionInput, check, check_program_stream};
    use crate::{
        directives::{Directives, StreamAssertions},
        model::{FailurePhase, Mode, TestResult},
    };

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

    #[test]
    fn stderr_contains_checks_run_success_stderr() {
        let directives = directives("// @stderr-contains: expected\n");
        let input = AssertionInput::success(Mode::Run, "", "program expected stderr\n");

        assert!(matches!(
            check(&directives.assertions, &input),
            TestResult::Pass
        ));
    }

    #[test]
    fn stderr_match_checks_exact_run_success_stderr_lines() {
        let directives = directives("// @stderr-match: exact stderr\n");
        let input = AssertionInput::success(Mode::Run, "", "exact stderr\n");

        assert!(matches!(
            check(&directives.assertions, &input),
            TestResult::Pass
        ));
    }

    #[test]
    fn stdout_contains_does_not_match_run_success_stderr() {
        let directives = directives("// @contains: stderr only\n");
        let input = AssertionInput::success(Mode::Run, "", "stderr only\n");

        assert!(matches!(
            check(&directives.assertions, &input),
            TestResult::Fail {
                phase: FailurePhase::Runtime,
                ..
            }
        ));
    }

    #[test]
    fn check_mode_contains_matches_merged_stdout_and_stderr() {
        let directives = directives("// @contains: diagnostic\n");
        let input = AssertionInput::success(Mode::Check, "prefix ", "diagnostic\n");

        assert!(matches!(
            check(&directives.assertions, &input),
            TestResult::Pass
        ));
    }

    #[test]
    fn expected_error_run_mode_checks_stderr() {
        let directives = directives("// @expect: error\n// @contains: runtime failed\n");
        let input = AssertionInput::error(Mode::Run, FailurePhase::Runtime, "", "runtime failed\n");

        assert!(matches!(
            check(&directives.assertions, &input),
            TestResult::Pass
        ));
    }

    #[test]
    fn expected_error_check_mode_checks_merged_output() {
        let directives = directives("// @expect: error\n// @contains: type mismatch\n");
        let input =
            AssertionInput::error(Mode::Check, FailurePhase::Compile, "type ", "mismatch\n");

        assert!(matches!(
            check(&directives.assertions, &input),
            TestResult::Pass
        ));
    }

    #[test]
    fn stream_matcher_uses_caller_phase() {
        let contains = vec!["missing".to_string()];
        let result =
            check_program_stream("stdout", "actual\n", None, &contains, FailurePhase::Runtime);

        assert!(matches!(
            result,
            TestResult::Fail {
                phase: FailurePhase::Runtime,
                ..
            }
        ));
    }

    #[test]
    fn warn_contains_checks_stderr_only() {
        let directives = directives("// @warn-contains: warning text\n");
        let input = AssertionInput::success(Mode::Check, "warning text\n", "");

        assert!(matches!(
            check(&directives.assertions, &input),
            TestResult::Fail {
                phase: FailurePhase::Compile,
                ..
            }
        ));
    }

    #[test]
    fn exact_match_reports_line_mismatch() {
        let stream = super::NamedStream::borrowed("stdout", "actual\n", FailurePhase::Runtime);
        let assertions = StreamAssertions {
            exact: Some("expected".to_string()),
            contains: vec![],
        };

        assert!(matches!(
            super::check_stream(&stream, &assertions),
            TestResult::Fail {
                phase: FailurePhase::Runtime,
                ..
            }
        ));
    }
}
