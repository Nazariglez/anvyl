use std::{
    path::{Path, PathBuf},
    time::{Duration, Instant},
};

use serde::Serialize;

use crate::{
    args::RunnerArgs,
    model::{FailurePhase, Mode, RunTestResult, TestResult},
};

const JSON_SCHEMA_VERSION: u32 = 3;

const GREEN: &str = "\x1b[32m";
const RED: &str = "\x1b[31m";
const YELLOW: &str = "\x1b[33m";
const CYAN: &str = "\x1b[36m";
const BLUE: &str = "\x1b[34m";
const RESET: &str = "\x1b[0m";
const GREY: &str = "\x1b[90m";

pub(crate) fn print_start(test_count: usize) {
    println!();
    println!("{CYAN}Detected {test_count} test cases...{RESET}");
    println!();
}

pub(crate) fn print_test_run() {
    println!("Running test cases...");
}

pub(crate) fn print_rust_batch_compile(count: usize) {
    println!("Compiling {count} Rust run tests...");
}

#[derive(Debug, Default)]
pub(crate) struct Summary {
    events: Vec<SummaryEvent>,
}

#[derive(Debug)]
struct SummaryEvent {
    file: PathBuf,
    mode: Mode,
    duration: Duration,
    outcome: SummaryOutcome,
}

#[derive(Debug)]
enum SummaryOutcome {
    Passed,
    Failed {
        phase: FailurePhase,
        message: String,
    },
    TimedOut {
        phase: FailurePhase,
    },
    Skipped {
        message: String,
    },
    Helper,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum IssueKind {
    CompileFailed,
    RuntimeFailed,
    CompileTimedOut,
    RuntimeTimedOut,
    Skipped,
}

impl Summary {
    pub(crate) fn add(&mut self, file: PathBuf, result: RunTestResult, quiet: bool) {
        let event = SummaryEvent::new(file, result);
        event.print(quiet);
        self.events.push(event);
    }

    fn passed(&self) -> usize {
        self.count(SummaryOutcome::is_passed)
    }

    fn failed(&self) -> usize {
        self.count(SummaryOutcome::is_failed)
    }

    fn timed_out(&self) -> usize {
        self.count(SummaryOutcome::is_timed_out)
    }

    fn skipped(&self) -> usize {
        self.count(SummaryOutcome::is_skipped)
    }

    fn helpers(&self) -> usize {
        self.count(SummaryOutcome::is_helper)
    }

    fn compile_failed(&self) -> usize {
        self.failures(FailurePhase::Compile).count()
    }

    fn runtime_failed(&self) -> usize {
        self.failures(FailurePhase::Runtime).count()
    }

    fn compile_timed_out(&self) -> usize {
        self.timeouts(FailurePhase::Compile).count()
    }

    fn runtime_timed_out(&self) -> usize {
        self.timeouts(FailurePhase::Runtime).count()
    }

    fn count(&self, predicate: impl Fn(&SummaryOutcome) -> bool) -> usize {
        self.events
            .iter()
            .filter(|event| predicate(&event.outcome))
            .count()
    }

    fn failures(&self, phase: FailurePhase) -> impl Iterator<Item = (&Path, &str)> {
        self.events
            .iter()
            .filter_map(move |event| match &event.outcome {
                SummaryOutcome::Failed {
                    phase: event_phase,
                    message,
                } if *event_phase == phase => Some((event.file.as_path(), message.as_str())),
                _ => None,
            })
    }

    fn timeouts(&self, phase: FailurePhase) -> impl Iterator<Item = &Path> {
        self.events
            .iter()
            .filter_map(move |event| match event.outcome {
                SummaryOutcome::TimedOut { phase: event_phase } if event_phase == phase => {
                    Some(event.file.as_path())
                }
                _ => None,
            })
    }

    fn skips(&self) -> impl Iterator<Item = (&Path, &str)> {
        self.events.iter().filter_map(|event| match &event.outcome {
            SummaryOutcome::Skipped { message } => Some((event.file.as_path(), message.as_str())),
            _ => None,
        })
    }

    pub(crate) fn print_summary(&self, start_time: Instant) {
        let passed = self.passed();
        let failed = self.failed();
        let timed_out = self.timed_out();
        let skipped = self.skipped();
        let helpers = self.helpers();
        let compile_failed = self.compile_failed();
        let runtime_failed = self.runtime_failed();
        let compile_timed_out = self.compile_timed_out();
        let runtime_timed_out = self.runtime_timed_out();

        println!();
        println!("{CYAN}Summary: {RESET}");
        println!();

        if skipped > 0 {
            println!("* {YELLOW}Skipped:{RESET} {skipped}");
            for (file, message) in self.skips() {
                println!("{YELLOW}  - {}:{RESET}", file.display());
                tab_print(4, message, false);
            }
            println!();
        }

        if compile_timed_out > 0 {
            eprintln!("* {BLUE}Compile timed out:{RESET} {compile_timed_out}");
            for file in self.timeouts(FailurePhase::Compile) {
                eprintln!("{BLUE}  - {}{RESET}", file.display());
            }
            println!();
        }

        if runtime_timed_out > 0 {
            eprintln!("* {BLUE}Runtime timed out:{RESET} {runtime_timed_out}");
            for file in self.timeouts(FailurePhase::Runtime) {
                eprintln!("{BLUE}  - {}{RESET}", file.display());
            }
            println!();
        }

        if compile_failed > 0 {
            eprintln!("* {RED}Compile failed:{RESET} {compile_failed}");
            for (file, message) in self.failures(FailurePhase::Compile) {
                println!();
                eprintln!("{RED}  - {}:{RESET}", file.display());
                tab_print(4, message, true);
            }
            println!();
        }

        if runtime_failed > 0 {
            eprintln!("* {RED}Runtime failed:{RESET} {runtime_failed}");
            for (file, message) in self.failures(FailurePhase::Runtime) {
                println!();
                eprintln!("{RED}  - {}:{RESET}", file.display());
                tab_print(4, message, true);
            }
            println!();
        }

        println!("* {GREEN}Passed:{RESET} {passed}");
        println!();
        let result = format!(
            "{GREEN}{passed}{RESET} passed; {RED}{failed}{RESET} failed ({compile_failed} compile, {runtime_failed} runtime); {BLUE}{timed_out}{RESET} timed out ({compile_timed_out} compile, {runtime_timed_out} runtime); {YELLOW}{skipped}{RESET} skipped; {GREY}{helpers}{RESET} helpers; finished in: {CYAN}{:.2}s{RESET}",
            start_time.elapsed().as_secs_f64()
        );
        if failed > 0 || timed_out > 0 {
            eprintln!("Test Result: {RED}FAILED{RESET}. -- {result}");
        } else {
            eprintln!("Test Result: {GREEN}OK{RESET}. -- {result}");
        }
    }

    pub(crate) fn print_json(
        &self,
        args: &RunnerArgs,
        start_time: Instant,
    ) -> Result<(), serde_json::Error> {
        let report = self.json_report(args, start_time);
        print!("{}", serde_json::to_string_pretty(&report)?);
        Ok(())
    }

    fn json_report(&self, args: &RunnerArgs, start_time: Instant) -> JsonReport {
        let mut issues = self
            .events
            .iter()
            .filter_map(JsonIssue::from_event)
            .collect::<Vec<_>>();
        sort_issues(&mut issues);

        JsonReport {
            schema_version: JSON_SCHEMA_VERSION,
            input_paths: args
                .paths
                .iter()
                .map(|path| path.display().to_string())
                .collect(),
            runtime_timeout_ms: args.timeout_ms,
            compile_timeout_ms: args.compile_timeout_ms,
            passed: self.passed(),
            failed: self.failed(),
            timed_out: self.timed_out(),
            skipped: self.skipped(),
            helpers: self.helpers(),
            compile_failed: self.compile_failed(),
            runtime_failed: self.runtime_failed(),
            compile_timed_out: self.compile_timed_out(),
            runtime_timed_out: self.runtime_timed_out(),
            elapsed_seconds: start_time.elapsed().as_secs_f64(),
            issues,
        }
    }
}

impl SummaryEvent {
    fn new(file: PathBuf, result: RunTestResult) -> Self {
        let RunTestResult {
            result,
            mode,
            duration,
        } = result;
        let outcome = SummaryOutcome::from(result);

        Self {
            file,
            mode,
            duration,
            outcome,
        }
    }

    fn print(&self, quiet: bool) {
        let Some((color, label, is_error)) = self.outcome.label() else {
            return;
        };
        event_msg(
            &self.file,
            quiet,
            self.mode,
            self.duration,
            color,
            label,
            is_error,
        );
    }
}

impl SummaryOutcome {
    fn is_passed(&self) -> bool {
        matches!(self, Self::Passed)
    }

    fn is_failed(&self) -> bool {
        matches!(self, Self::Failed { .. })
    }

    fn is_timed_out(&self) -> bool {
        matches!(self, Self::TimedOut { .. })
    }

    fn is_skipped(&self) -> bool {
        matches!(self, Self::Skipped { .. })
    }

    fn is_helper(&self) -> bool {
        matches!(self, Self::Helper)
    }

    fn label(&self) -> Option<(&'static str, &'static str, bool)> {
        match self {
            Self::Passed => Some((GREEN, "PASS", false)),
            Self::Failed { .. } => Some((RED, "FAIL", true)),
            Self::TimedOut { .. } => Some((BLUE, "TIMEOUT", true)),
            Self::Skipped { .. } => Some((YELLOW, "SKIP", false)),
            Self::Helper => None,
        }
    }

    fn issue_kind(&self) -> Option<IssueKind> {
        match self {
            Self::Failed {
                phase: FailurePhase::Compile,
                ..
            } => Some(IssueKind::CompileFailed),
            Self::Failed {
                phase: FailurePhase::Runtime,
                ..
            } => Some(IssueKind::RuntimeFailed),
            Self::TimedOut {
                phase: FailurePhase::Compile,
            } => Some(IssueKind::CompileTimedOut),
            Self::TimedOut {
                phase: FailurePhase::Runtime,
            } => Some(IssueKind::RuntimeTimedOut),
            Self::Skipped { .. } => Some(IssueKind::Skipped),
            Self::Passed | Self::Helper => None,
        }
    }

    fn issue_message(&self) -> Option<String> {
        match self {
            Self::Failed { message, .. } | Self::Skipped { message } => Some(message.clone()),
            Self::TimedOut { phase } => Some(format!("{} timeout", phase.as_str())),
            Self::Passed | Self::Helper => None,
        }
    }
}

impl From<TestResult> for SummaryOutcome {
    fn from(result: TestResult) -> Self {
        match result {
            TestResult::Pass => Self::Passed,
            TestResult::Fail { phase, message } => Self::Failed { phase, message },
            TestResult::Timeout { phase } => Self::TimedOut { phase },
            TestResult::Skip { message } => Self::Skipped { message },
            TestResult::Helper => Self::Helper,
        }
    }
}

impl IssueKind {
    fn as_str(self) -> &'static str {
        match self {
            Self::CompileFailed => "compile_failed",
            Self::RuntimeFailed => "runtime_failed",
            Self::CompileTimedOut => "compile_timed_out",
            Self::RuntimeTimedOut => "runtime_timed_out",
            Self::Skipped => "skipped",
        }
    }

    fn phase(self) -> Option<FailurePhase> {
        match self {
            Self::CompileFailed | Self::CompileTimedOut => Some(FailurePhase::Compile),
            Self::RuntimeFailed | Self::RuntimeTimedOut => Some(FailurePhase::Runtime),
            Self::Skipped => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize)]
struct JsonReport {
    schema_version: u32,
    input_paths: Vec<String>,
    runtime_timeout_ms: u64,
    compile_timeout_ms: u64,
    passed: usize,
    failed: usize,
    timed_out: usize,
    skipped: usize,
    helpers: usize,
    compile_failed: usize,
    runtime_failed: usize,
    compile_timed_out: usize,
    runtime_timed_out: usize,
    elapsed_seconds: f64,
    issues: Vec<JsonIssue>,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
struct JsonIssue {
    kind: &'static str,
    mode: String,
    phase: Option<String>,
    path: String,
    message: String,
    duration_seconds: f64,
}

impl JsonIssue {
    fn from_event(event: &SummaryEvent) -> Option<Self> {
        let kind = event.outcome.issue_kind()?;

        Some(Self {
            kind: kind.as_str(),
            mode: event.mode.to_string(),
            phase: kind.phase().map(|phase| phase.as_str().to_string()),
            path: event.file.display().to_string(),
            message: event.outcome.issue_message()?,
            duration_seconds: event.duration.as_secs_f64(),
        })
    }
}

fn sort_issues(issues: &mut [JsonIssue]) {
    issues.sort_by(|left, right| {
        left.path
            .cmp(&right.path)
            .then(left.mode.cmp(&right.mode))
            .then(left.kind.cmp(right.kind))
            .then(left.phase.cmp(&right.phase))
            .then(left.message.cmp(&right.message))
    });
}

fn event_msg(
    file: &Path,
    quiet: bool,
    mode: Mode,
    duration: Duration,
    color: &str,
    label: &str,
    is_error: bool,
) {
    if quiet {
        return;
    }
    let line = format!(
        "{color}[{label}]{RESET} {} {GREY}({mode} - {:.3}s){RESET}",
        file.display(),
        duration.as_secs_f32()
    );
    if is_error {
        eprintln!("{line}");
    } else {
        println!("{line}");
    }
}

fn tab_print(spaces: usize, message: &str, is_error: bool) {
    let message = message.replace("\\n", "\n");
    for line in message.lines() {
        if is_error {
            eprintln!("{:>spaces$}| {line}", "");
        } else {
            println!("{:>spaces$}| {line}", "");
        }
    }
}

#[cfg(test)]
mod tests {
    use std::{
        path::PathBuf,
        time::{Duration, Instant},
    };

    use super::Summary;
    use crate::{
        args::{DEFAULT_COMPILE_TIMEOUT_MS, DEFAULT_RUNTIME_TIMEOUT_MS, RunnerArgs},
        model::{FailurePhase, Mode, RunTestResult, TestResult},
    };

    fn runner_args() -> RunnerArgs {
        RunnerArgs {
            paths: vec![PathBuf::from("tests")],
            timeout_ms: DEFAULT_RUNTIME_TIMEOUT_MS,
            compile_timeout_ms: DEFAULT_COMPILE_TIMEOUT_MS,
            jobs: None,
            quiet: true,
            report_json: true,
            release: false,
        }
    }

    fn result(result: TestResult, mode: Mode) -> RunTestResult {
        RunTestResult {
            result,
            mode,
            duration: Duration::from_millis(250),
        }
    }

    #[test]
    fn json_report_includes_issue_context_and_sorts_deterministically() {
        let mut summary = Summary::default();
        summary.add(
            PathBuf::from("b.anv"),
            result(
                TestResult::Fail {
                    phase: FailurePhase::Compile,
                    message: "compile fail".to_string(),
                },
                Mode::Run,
            ),
            true,
        );
        summary.add(
            PathBuf::from("a.anv"),
            result(
                TestResult::Skip {
                    message: "skip me".to_string(),
                },
                Mode::Check,
            ),
            true,
        );

        let report = summary.json_report(&runner_args(), Instant::now());

        assert_eq!(report.schema_version, 3);
        assert_eq!(report.issues.len(), 2);
        assert_eq!(report.issues[0].path, "a.anv");
        assert_eq!(report.issues[0].mode, "check");
        assert_eq!(report.issues[0].phase, None);
        assert_eq!(report.issues[0].kind, "skipped");
        assert_eq!(report.issues[0].message, "skip me");
        assert_eq!(report.issues[1].path, "b.anv");
        assert_eq!(report.issues[1].kind, "compile_failed");
        assert_eq!(report.issues[1].phase.as_deref(), Some("compile"));
        assert_eq!(report.issues[1].message, "compile fail");
        assert!(report.issues[1].duration_seconds > 0.0);
    }

    #[test]
    fn summary_derives_counts_from_events() {
        let mut summary = Summary::default();
        summary.add(
            PathBuf::from("compile_fail.anv"),
            result(
                TestResult::Fail {
                    phase: FailurePhase::Compile,
                    message: "compile fail".to_string(),
                },
                Mode::Check,
            ),
            true,
        );
        summary.add(
            PathBuf::from("runtime_fail.anv"),
            result(
                TestResult::Fail {
                    phase: FailurePhase::Runtime,
                    message: "runtime fail".to_string(),
                },
                Mode::Run,
            ),
            true,
        );
        summary.add(
            PathBuf::from("compile_timeout.anv"),
            result(
                TestResult::Timeout {
                    phase: FailurePhase::Compile,
                },
                Mode::Check,
            ),
            true,
        );
        summary.add(
            PathBuf::from("runtime_timeout.anv"),
            result(
                TestResult::Timeout {
                    phase: FailurePhase::Runtime,
                },
                Mode::Run,
            ),
            true,
        );

        let report = summary.json_report(&runner_args(), Instant::now());
        let kinds = report
            .issues
            .iter()
            .map(|issue| issue.kind)
            .collect::<Vec<_>>();

        assert_eq!(report.failed, 2);
        assert_eq!(report.timed_out, 2);
        assert_eq!(report.compile_failed, 1);
        assert_eq!(report.runtime_failed, 1);
        assert_eq!(report.compile_timed_out, 1);
        assert_eq!(report.runtime_timed_out, 1);
        assert!(kinds.contains(&"compile_failed"));
        assert!(kinds.contains(&"runtime_failed"));
        assert!(kinds.contains(&"compile_timed_out"));
        assert!(kinds.contains(&"runtime_timed_out"));
    }

    #[test]
    fn json_report_omits_passes_and_helpers_from_issues() {
        let mut summary = Summary::default();
        summary.add(
            PathBuf::from("pass.anv"),
            result(TestResult::Pass, Mode::Run),
            true,
        );
        summary.add(
            PathBuf::from("helper.anv"),
            result(TestResult::Helper, Mode::Check),
            true,
        );

        let report = summary.json_report(&runner_args(), Instant::now());

        assert_eq!(report.passed, 1);
        assert_eq!(report.helpers, 1);
        assert!(report.issues.is_empty());
    }
}
