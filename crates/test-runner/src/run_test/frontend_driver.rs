use anvyx_frontend::pipeline::{self, ProgramInput, Source, SourceLoader};

use super::driver::{DriverCapabilities, ProcessOutcome, TestCase, TestDriver};
use crate::model::FailurePhase;

pub(crate) struct FrontendDriver;

impl TestDriver for FrontendDriver {
    fn run(&self, case: &TestCase<'_>) -> Result<ProcessOutcome, String> {
        let mut source_loader = EmptySourceLoader;
        let input = ProgramInput {
            main: Source {
                code: case.source.to_string(),
                path: case.file.display().to_string(),
            },
            prelude: String::new(),
            core_modules: vec![],
            source_loader: &mut source_loader,
        };

        match pipeline::check(input) {
            Ok(_) => Ok(ProcessOutcome::Pass {
                stdout: String::new(),
                stderr: String::new(),
            }),
            Err(error) => Ok(ProcessOutcome::Fail {
                phase: FailurePhase::Compile,
                stdout: String::new(),
                stderr: format!("frontend check failed: {error:?}"),
            }),
        }
    }

    fn capabilities(&self) -> DriverCapabilities {
        DriverCapabilities {
            supports_run: false,
            supports_text_diagnostics: false,
        }
    }
}

struct EmptySourceLoader;

impl SourceLoader for EmptySourceLoader {
    fn load(&mut self, _module_path: &[String]) -> Result<Option<Source>, String> {
        Ok(None)
    }
}

#[cfg(test)]
mod tests {
    use super::{FrontendDriver, TestDriver};
    use crate::{directives::Directives, model::Mode};

    #[test]
    fn frontend_driver_rejects_text_assertions() {
        let directives = Directives::parse(
            "// @mode: check\n// @expect: success\n// @contains: missing\nfn main() {}\n",
        )
        .unwrap();
        let capabilities = FrontendDriver.capabilities();

        assert!(
            capabilities
                .validate_assertions(&directives.assertions)
                .is_err()
        );
    }

    #[test]
    fn frontend_driver_accepts_outcome_only_check_tests() {
        let directives =
            Directives::parse("// @mode: check\n// @expect: success\nfn main() {}\n").unwrap();
        let capabilities = FrontendDriver.capabilities();

        assert!(capabilities.unsupported_mode(Mode::Check).is_none());
        assert!(
            capabilities
                .validate_assertions(&directives.assertions)
                .is_ok()
        );
    }
}
