mod assertions;
mod classifier;
mod cli_driver;
mod driver;
mod frontend_driver;

use std::{
    path::Path,
    time::{Duration, Instant},
};

pub(crate) use cli_driver::CliDriver;
pub(crate) use driver::TestDriver;
pub(crate) use frontend_driver::FrontendDriver;

use self::{classifier::classify_test_result, driver::TestCase};
use crate::{
    directives::Directives,
    model::{Mode, RunTestResult, TestResult},
};

pub(crate) fn run_test_file(
    file: &Path,
    runtime_timeout: Duration,
    compile_timeout: Duration,
    backend: Option<&'static str>,
    driver: &dyn TestDriver,
) -> Result<RunTestResult, String> {
    let src = std::fs::read_to_string(file).map_err(|e| e.to_string())?;
    let directives = Directives::parse(&src)?;
    let contract = directives.contract;
    if directives.helper {
        return Ok(RunTestResult {
            result: TestResult::Helper,
            mode: contract.mode,
            backend: None,
            duration: Duration::ZERO,
        });
    }
    if let Some(reason) = &directives.skip {
        return Ok(RunTestResult {
            result: TestResult::Skip {
                message: reason.clone(),
            },
            mode: contract.mode,
            backend: None,
            duration: Duration::ZERO,
        });
    }

    let capabilities = driver.capabilities();
    if let Some(message) = capabilities.unsupported_mode(contract.mode) {
        return Ok(RunTestResult {
            result: TestResult::Skip {
                message: message.to_string(),
            },
            mode: contract.mode,
            backend: None,
            duration: Duration::ZERO,
        });
    }
    capabilities.validate_assertions(&directives.assertions)?;

    let effective_backend = match contract.mode {
        Mode::Run => backend,
        Mode::Check => None,
    };

    let stdin_text = directives.stdin.text();
    let case = TestCase {
        file,
        source: &src,
        mode: contract.mode,
        backend: effective_backend,
        runtime_timeout,
        compile_timeout,
        driver_options: &directives.driver_options,
        stdin_text: &stdin_text,
    };
    let start_time = Instant::now();
    let outcome = driver.run(&case)?;
    let elapsed = start_time.elapsed();
    let result = classify_test_result(outcome, contract, &directives.assertions);

    Ok(RunTestResult {
        result,
        mode: contract.mode,
        backend: effective_backend,
        duration: elapsed,
    })
}

#[cfg(test)]
mod tests {
    use std::{fs, path::PathBuf, sync::Mutex, time::Duration};

    use super::{
        TestDriver,
        driver::{DriverCapabilities, ProcessOutcome, TestCase},
        run_test_file,
    };
    use crate::{
        directives::DriverOptions,
        model::{Mode, TestResult},
    };

    #[derive(Debug)]
    struct RecordedCase {
        mode: Mode,
        backend: Option<&'static str>,
        runtime_timeout: Duration,
        compile_timeout: Duration,
        driver_options: DriverOptions,
        stdin_text: String,
    }

    struct FakeDriver {
        calls: Mutex<Vec<RecordedCase>>,
        capabilities: DriverCapabilities,
    }

    impl FakeDriver {
        fn new() -> Self {
            Self::with_capabilities(DriverCapabilities {
                supports_run: true,
                supports_text_diagnostics: true,
            })
        }

        fn with_capabilities(capabilities: DriverCapabilities) -> Self {
            Self {
                calls: Mutex::new(Vec::new()),
                capabilities,
            }
        }

        fn check_only() -> Self {
            Self::with_capabilities(DriverCapabilities {
                supports_run: false,
                supports_text_diagnostics: true,
            })
        }

        fn outcome_only() -> Self {
            Self::with_capabilities(DriverCapabilities {
                supports_run: false,
                supports_text_diagnostics: false,
            })
        }

        fn calls(&self) -> std::sync::MutexGuard<'_, Vec<RecordedCase>> {
            self.calls.lock().unwrap()
        }
    }

    impl TestDriver for FakeDriver {
        fn run(&self, case: &TestCase<'_>) -> Result<ProcessOutcome, String> {
            self.calls.lock().unwrap().push(RecordedCase {
                mode: case.mode,
                backend: case.backend,
                runtime_timeout: case.runtime_timeout,
                compile_timeout: case.compile_timeout,
                driver_options: case.driver_options.clone(),
                stdin_text: case.stdin_text.to_string(),
            });
            Ok(ProcessOutcome::Pass {
                stdout: String::new(),
                stderr: String::new(),
            })
        }

        fn capabilities(&self) -> DriverCapabilities {
            self.capabilities
        }
    }

    fn fixture(name: &str, src: &str) -> PathBuf {
        let path = std::env::temp_dir().join(format!(
            "anvyx-test-runner-{}-{name}.anv",
            std::process::id()
        ));
        fs::write(&path, src).unwrap();
        path
    }

    #[test]
    fn helper_result_does_not_call_driver() {
        let file = fixture("helper", "// @helper\n");
        let driver = FakeDriver::new();

        let result = run_test_file(
            &file,
            Duration::from_millis(1),
            Duration::from_millis(2),
            Some("vm"),
            &driver,
        )
        .unwrap();

        assert!(matches!(result.result, TestResult::Helper));
        assert!(driver.calls().is_empty());
        let _ = fs::remove_file(file);
    }

    #[test]
    fn skip_result_does_not_call_driver() {
        let file = fixture(
            "skip",
            "// @mode: run\n// @expect: success\n// @skip: not today\n",
        );
        let driver = FakeDriver::new();

        let result = run_test_file(
            &file,
            Duration::from_millis(1),
            Duration::from_millis(2),
            Some("vm"),
            &driver,
        )
        .unwrap();

        assert!(matches!(result.result, TestResult::Skip { .. }));
        assert!(driver.calls().is_empty());
        let _ = fs::remove_file(file);
    }

    #[test]
    fn unsupported_mode_skips_before_running_driver() {
        let file = fixture("unsupported-run", "// @mode: run\n// @expect: success\n");
        let driver = FakeDriver::check_only();

        let result = run_test_file(
            &file,
            Duration::from_millis(1),
            Duration::from_millis(2),
            Some("vm"),
            &driver,
        )
        .unwrap();

        assert!(matches!(result.result, TestResult::Skip { .. }));
        assert!(driver.calls().is_empty());
        let _ = fs::remove_file(file);
    }

    #[test]
    fn helper_result_skips_before_capability_validation() {
        let file = fixture("helper-unsupported", "// @helper\n");
        let driver = FakeDriver::outcome_only();

        let result = run_test_file(
            &file,
            Duration::from_millis(1),
            Duration::from_millis(2),
            Some("vm"),
            &driver,
        )
        .unwrap();

        assert!(matches!(result.result, TestResult::Helper));
        assert!(driver.calls().is_empty());
        let _ = fs::remove_file(file);
    }

    #[test]
    fn skip_with_assertions_skips_before_capability_validation() {
        let file = fixture(
            "skip-assertions",
            "// @mode: check\n// @expect: success\n// @contains: ignored\n// @skip: no\n",
        );
        let driver = FakeDriver::outcome_only();

        let result = run_test_file(
            &file,
            Duration::from_millis(1),
            Duration::from_millis(2),
            Some("vm"),
            &driver,
        )
        .unwrap();

        assert!(matches!(result.result, TestResult::Skip { .. }));
        assert!(driver.calls().is_empty());
        let _ = fs::remove_file(file);
    }

    #[test]
    fn run_mode_passes_effective_backend_to_driver() {
        let file = fixture(
            "run-backend",
            "// @mode: run\n// @expect: success\n// @stdin: input\n// @lint: unused\n// @feature: gc\n// @cfg: debug\n",
        );
        let driver = FakeDriver::new();

        let result = run_test_file(
            &file,
            Duration::from_millis(3),
            Duration::from_millis(4),
            Some("rust"),
            &driver,
        )
        .unwrap();

        assert!(matches!(result.result, TestResult::Pass));
        assert_eq!(result.backend, Some("rust"));
        let calls = driver.calls();
        assert_eq!(calls.len(), 1);
        let case = &calls[0];
        assert_eq!(case.mode, Mode::Run);
        assert_eq!(case.backend, Some("rust"));
        assert_eq!(case.runtime_timeout, Duration::from_millis(3));
        assert_eq!(case.compile_timeout, Duration::from_millis(4));
        let mut forwarded_args = vec![];
        case.driver_options.append_cli_args(&mut forwarded_args);
        assert_eq!(
            forwarded_args,
            vec!["--lint", "unused", "--feature", "gc", "--cfg", "debug"]
        );
        assert_eq!(case.stdin_text, "input\n");
        drop(calls);
        let _ = fs::remove_file(file);
    }

    #[test]
    fn check_mode_passes_no_effective_backend_to_driver() {
        let file = fixture("check-backend", "// @mode: check\n// @expect: success\n");
        let driver = FakeDriver::new();

        let result = run_test_file(
            &file,
            Duration::from_millis(3),
            Duration::from_millis(4),
            Some("rust"),
            &driver,
        )
        .unwrap();

        assert!(matches!(result.result, TestResult::Pass));
        assert_eq!(result.backend, None);
        let calls = driver.calls();
        assert_eq!(calls.len(), 1);
        assert_eq!(calls[0].mode, Mode::Check);
        assert_eq!(calls[0].backend, None);
        drop(calls);
        let _ = fs::remove_file(file);
    }

    #[test]
    fn outcome_only_driver_rejects_text_assertions() {
        let file = fixture(
            "reject-text-assertions",
            "// @mode: check\n// @expect: success\n// @contains: missing\n",
        );
        let driver = FakeDriver::outcome_only();

        let result = run_test_file(
            &file,
            Duration::from_millis(1),
            Duration::from_millis(2),
            Some("vm"),
            &driver,
        );
        let Err(err) = result else {
            panic!("expected text assertion rejection");
        };

        assert!(err.contains("text assertions"));
        assert!(driver.calls().is_empty());
        let _ = fs::remove_file(file);
    }

    #[test]
    fn outcome_only_driver_accepts_outcome_only_check_tests() {
        let file = fixture("outcome-only", "// @mode: check\n// @expect: success\n");
        let driver = FakeDriver::outcome_only();

        let result = run_test_file(
            &file,
            Duration::from_millis(1),
            Duration::from_millis(2),
            Some("vm"),
            &driver,
        )
        .unwrap();

        assert!(matches!(result.result, TestResult::Pass));
        assert_eq!(driver.calls().len(), 1);
        let _ = fs::remove_file(file);
    }
}
