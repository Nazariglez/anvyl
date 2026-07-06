use std::time::Duration;

#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub(crate) enum Mode {
    #[default]
    Run,
    Check,
}

impl Mode {
    pub(crate) fn from_str(s: &str) -> Result<Self, String> {
        match s {
            "check" => Ok(Self::Check),
            "run" => Ok(Self::Run),
            _ => Err(format!("Invalid mode: {s}")),
        }
    }

    pub(crate) fn as_str(self) -> &'static str {
        match self {
            Self::Run => "run",
            Self::Check => "check",
        }
    }
}

impl std::fmt::Display for Mode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub(crate) enum ExpectedResult {
    #[default]
    Success,
    Error,
    Timeout,
}

impl ExpectedResult {
    pub(crate) fn from_str(s: &str) -> Result<Self, String> {
        match s {
            "success" => Ok(Self::Success),
            "error" => Ok(Self::Error),
            "timeout" => Ok(Self::Timeout),
            _ => Err(format!("Invalid expected result: {s}")),
        }
    }
}

impl std::fmt::Display for ExpectedResult {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum FailurePhase {
    Compile,
    Runtime,
}

impl FailurePhase {
    pub(crate) fn as_str(self) -> &'static str {
        match self {
            Self::Compile => "compile",
            Self::Runtime => "runtime",
        }
    }
}

#[derive(Debug)]
pub(crate) enum TestResult {
    Pass,
    Fail {
        phase: FailurePhase,
        message: String,
    },
    Timeout {
        phase: FailurePhase,
    },
    Skip {
        message: String,
    },
    Helper,
}

pub(crate) struct RunTestResult {
    pub(crate) result: TestResult,
    pub(crate) mode: Mode,
    pub(crate) duration: Duration,
}
