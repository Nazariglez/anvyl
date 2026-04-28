use std::{path::Path, time::Duration};

use crate::{
    directives::{Assertions, DriverOptions},
    model::{FailurePhase, Mode},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct DriverCapabilities {
    pub(crate) supports_run: bool,
    pub(crate) supports_text_diagnostics: bool,
}

impl DriverCapabilities {
    pub(crate) fn unsupported_mode(self, mode: Mode) -> Option<&'static str> {
        match (self.supports_run, mode) {
            (false, Mode::Run) => Some("selected driver only supports @mode: check"),
            _ => None,
        }
    }

    pub(crate) fn validate_assertions(self, assertions: &Assertions) -> Result<(), String> {
        if self.supports_text_diagnostics || assertions.is_empty() {
            return Ok(());
        }

        Err("selected driver does not support text assertions".to_string())
    }
}

#[derive(Debug)]
pub(crate) enum ProcessOutcome {
    Completed {
        exit_code: Option<i32>,
        stdout: String,
        stderr: String,
    },
    Pass {
        stdout: String,
        stderr: String,
    },
    Fail {
        phase: FailurePhase,
        stdout: String,
        stderr: String,
    },
    Timeout {
        phase: FailurePhase,
    },
}

pub(crate) struct TestCase<'a> {
    pub(crate) file: &'a Path,
    pub(crate) source: &'a str,
    pub(crate) mode: Mode,
    pub(crate) backend: Option<&'static str>,
    pub(crate) runtime_timeout: Duration,
    pub(crate) compile_timeout: Duration,
    pub(crate) driver_options: &'a DriverOptions,
    pub(crate) stdin_text: &'a str,
}

pub(crate) trait TestDriver: Sync {
    fn run(&self, case: &TestCase<'_>) -> Result<ProcessOutcome, String>;
    fn capabilities(&self) -> DriverCapabilities;
}
