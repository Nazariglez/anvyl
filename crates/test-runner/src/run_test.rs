mod assertions;
mod classifier;
mod cli;

use std::{
    path::{Path, PathBuf},
    time::{Duration, Instant},
};

pub(crate) use cli::Cli;

use self::classifier::classify_test_result;
use crate::{
    directives::{Assertions, CliOptions, Directives, TestContract},
    model::{FailurePhase, Mode, RunTestResult, TestResult},
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
    pub(super) backend: Option<&'static str>,
    pub(super) runtime_timeout: Duration,
    pub(super) compile_timeout: Duration,
    pub(super) cli_options: CliOptions,
    pub(super) stdin: String,
}

enum TestPlan {
    Done(RunTestResult),
    Run {
        contract: TestContract,
        assertions: Assertions,
        case: CliCase,
    },
}

pub(crate) fn run_test_file(
    file: &Path,
    runtime_timeout: Duration,
    compile_timeout: Duration,
    backend: Option<&'static str>,
    cli: &Cli,
) -> Result<RunTestResult, String> {
    let src = std::fs::read_to_string(file).map_err(|e| e.to_string())?;
    let directives = Directives::parse(&src)?;

    let (contract, assertions, case) = match plan_test(
        file,
        directives,
        runtime_timeout,
        compile_timeout,
        backend,
        cli.new_frontend(),
    ) {
        TestPlan::Done(result) => return Ok(result),
        TestPlan::Run {
            contract,
            assertions,
            case,
        } => (contract, assertions, case),
    };

    let start_time = Instant::now();
    let backend = case.backend;
    let mode = case.mode;
    let outcome = cli.run(&case)?;
    let elapsed = start_time.elapsed();
    let result = classify_test_result(outcome, contract, &assertions);

    Ok(RunTestResult {
        result,
        mode,
        backend,
        duration: elapsed,
    })
}

fn plan_test(
    file: &Path,
    directives: Directives,
    runtime_timeout: Duration,
    compile_timeout: Duration,
    backend: Option<&'static str>,
    new_frontend: bool,
) -> TestPlan {
    let contract = directives.contract;
    if directives.helper {
        return TestPlan::Done(done(TestResult::Helper, contract.mode));
    }
    if let Some(reason) = directives.skip {
        return TestPlan::Done(done(TestResult::Skip { message: reason }, contract.mode));
    }
    if let Some(reason) = directives.frontend.skip_reason(new_frontend) {
        return TestPlan::Done(done(
            TestResult::Skip {
                message: reason.to_string(),
            },
            contract.mode,
        ));
    }
    if new_frontend && contract.mode == Mode::Run && backend != Some("rust") {
        return TestPlan::Done(done(
            TestResult::Skip {
                message: "new frontend run requires --backend rust".to_string(),
            },
            contract.mode,
        ));
    }

    let backend = match contract.mode {
        Mode::Run => backend,
        Mode::Check => None,
    };

    TestPlan::Run {
        contract,
        assertions: directives.assertions,
        case: CliCase {
            file: file.to_path_buf(),
            mode: contract.mode,
            backend,
            runtime_timeout,
            compile_timeout,
            cli_options: directives.cli_options,
            stdin: directives.stdin.text(),
        },
    }
}

fn done(result: TestResult, mode: Mode) -> RunTestResult {
    RunTestResult {
        result,
        mode,
        backend: None,
        duration: Duration::ZERO,
    }
}

#[cfg(test)]
mod tests {
    use std::{path::Path, time::Duration};

    use super::{TestPlan, plan_test};
    use crate::{
        directives::Directives,
        model::{Mode, TestResult},
    };

    fn directives(src: &str) -> Directives {
        Directives::parse(src).unwrap()
    }

    fn plan(src: &str, new_frontend: bool) -> TestPlan {
        plan_with_backend(src, new_frontend, Some("rust"))
    }

    fn plan_with_backend(src: &str, new_frontend: bool, backend: Option<&'static str>) -> TestPlan {
        plan_test(
            Path::new("test.anv"),
            directives(src),
            Duration::from_millis(1),
            Duration::from_millis(2),
            backend,
            new_frontend,
        )
    }

    fn case(src: &str) -> super::CliCase {
        match plan(src, false) {
            TestPlan::Run { case, .. } => case,
            TestPlan::Done(_) => panic!("expected runnable plan"),
        }
    }

    #[test]
    fn helper_skips_cli() {
        let TestPlan::Done(result) = plan("// @helper\n", true) else {
            panic!("expected done plan");
        };

        assert!(matches!(result.result, TestResult::Helper));
    }

    #[test]
    fn skip_skips_cli() {
        let TestPlan::Done(result) = plan(
            "// @mode: run\n// @expect: success\n// @skip: not today\n",
            false,
        ) else {
            panic!("expected done plan");
        };

        assert!(matches!(result.result, TestResult::Skip { .. }));
    }

    #[test]
    fn new_frontend_skips_non_rust_run() {
        let TestPlan::Done(result) =
            plan_with_backend("// @mode: run\n// @expect: success\n", true, Some("vm"))
        else {
            panic!("expected done plan");
        };

        assert!(matches!(result.result, TestResult::Skip { .. }));
    }

    #[test]
    fn new_frontend_allows_rust_run() {
        let TestPlan::Run { .. } = plan("// @mode: run\n// @expect: success\n", true) else {
            panic!("expected runnable plan");
        };
    }

    #[test]
    fn helper_before_mode_support() {
        let TestPlan::Done(result) = plan("// @helper\n", true) else {
            panic!("expected done plan");
        };

        assert!(matches!(result.result, TestResult::Helper));
    }

    #[test]
    fn skip_before_mode_support() {
        let TestPlan::Done(result) = plan(
            "// @mode: run\n// @expect: success\n// @contains: ignored\n// @skip: no\n",
            true,
        ) else {
            panic!("expected done plan");
        };

        assert!(matches!(result.result, TestResult::Skip { .. }));
    }

    #[test]
    fn frontend_requirement_skips_incompatible_runner() {
        let TestPlan::Done(result) = plan(
            "// @mode: check\n// @expect: success\n// @frontend: new\n",
            false,
        ) else {
            panic!("expected done plan");
        };

        assert!(
            matches!(result.result, TestResult::Skip { message } if message == "requires new frontend")
        );
    }

    #[test]
    fn frontend_requirement_allows_matching_runner() {
        let TestPlan::Run { .. } = plan(
            "// @mode: check\n// @expect: success\n// @frontend: new\n",
            true,
        ) else {
            panic!("expected runnable plan");
        };
    }

    #[test]
    fn default_frontend_requirement_skips_new_frontend_runner() {
        let TestPlan::Done(result) = plan(
            "// @mode: check\n// @expect: success\n// @frontend: default\n",
            true,
        ) else {
            panic!("expected done plan");
        };

        assert!(
            matches!(result.result, TestResult::Skip { message } if message == "requires default frontend")
        );
    }

    #[test]
    fn run_uses_backend() {
        let case = case(
            "// @mode: run\n// @expect: success\n// @stdin: input\n// @lint: unused\n// @feature: gc\n// @cfg: debug\n",
        );

        assert_eq!(case.mode, Mode::Run);
        assert_eq!(case.backend, Some("rust"));
        assert_eq!(case.runtime_timeout, Duration::from_millis(1));
        assert_eq!(case.compile_timeout, Duration::from_millis(2));
        let mut args = vec![];
        case.cli_options.append_args(&mut args);
        assert_eq!(
            args,
            vec!["--lint", "unused", "--feature", "gc", "--cfg", "debug"]
        );
        assert_eq!(case.stdin, "input\n");
    }

    #[test]
    fn check_omits_backend() {
        let case = case("// @mode: check\n// @expect: success\n");

        assert_eq!(case.mode, Mode::Check);
        assert_eq!(case.backend, None);
    }

    #[test]
    fn check_allows_text_assertions() {
        let TestPlan::Run { assertions, .. } = plan(
            "// @mode: check\n// @expect: success\n// @contains: available\n",
            true,
        ) else {
            panic!("expected runnable plan");
        };

        assert_eq!(assertions.selected.contains, ["available"]);
    }
}
