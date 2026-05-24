use std::path::PathBuf;

pub const DEFAULT_RUNTIME_TIMEOUT_MS: u64 = 2_000;
pub const DEFAULT_COMPILE_TIMEOUT_MS: u64 = 300_000;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BackendArg {
    Vm,
    Rust,
    Both,
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub enum FrontendArg {
    #[default]
    Default,
    New,
}

impl FrontendArg {
    pub fn is_new(self) -> bool {
        self == Self::New
    }
}

impl BackendArg {
    fn from_str(s: &str) -> Result<Self, String> {
        match s {
            "vm" => Ok(Self::Vm),
            "rust" => Ok(Self::Rust),
            "both" => Ok(Self::Both),
            _ => Err(format!(
                "Unknown backend: '{s}'. Expected 'vm', 'rust', or 'both'"
            )),
        }
    }

    pub fn expand(self) -> &'static [&'static str] {
        match self {
            Self::Vm => &["vm"],
            Self::Rust => &["rust"],
            Self::Both => &["vm", "rust"],
        }
    }

    pub fn as_str(self) -> &'static str {
        match self {
            Self::Vm => "vm",
            Self::Rust => "rust",
            Self::Both => "both",
        }
    }
}

#[derive(Debug)]
pub struct RunnerArgs {
    pub paths: Vec<PathBuf>,
    pub timeout_ms: u64,
    pub compile_timeout_ms: u64,
    pub jobs: Option<usize>,
    pub quiet: bool,
    pub report_json: bool,
    pub release: bool,
    pub backend: BackendArg,
    pub frontend: FrontendArg,
}

impl RunnerArgs {
    pub fn new_frontend(&self) -> bool {
        self.frontend.is_new()
    }
}

pub fn usage() -> String {
    format!(
        "\
Usage: test-runner [OPTIONS] <PATH>...

Arguments:
  <PATH>...  One or more test files or directories

Options:
  --backend <vm|rust|both>  Backend to test (default: vm)
  --new-frontend            Run check fixtures through the clean frontend and rust run fixtures through the clean Rust backend
  --timeout <ms>            Runtime timeout in milliseconds (default: {DEFAULT_RUNTIME_TIMEOUT_MS})
  --compile-timeout <ms>    Compile timeout in milliseconds (default: {DEFAULT_COMPILE_TIMEOUT_MS})
  --jobs <n>                Maximum tests to run in parallel (default: rayon default)
  --quiet                   Suppress individual test output
  --report-json             Emit machine-readable JSON instead of human output
  --release                 Build in release mode"
    )
}

impl RunnerArgs {
    pub fn new() -> Result<Self, String> {
        Self::parse(std::env::args())
    }

    fn parse(args: impl IntoIterator<Item = String>) -> Result<Self, String> {
        let mut parsed = ParsedArgs::default();
        let mut iter = args.into_iter();
        let _program = iter.next();

        while let Some(arg) = iter.next() {
            match arg.as_str() {
                "--quiet" => parsed.quiet = true,
                "--report-json" => parsed.report_json = true,
                "--release" => parsed.release = true,
                "--new-frontend" => parsed.frontend = FrontendArg::New,
                "--timeout" => {
                    let value = parse_value(&mut iter, "--timeout")?;
                    parsed.timeout_ms = parse_u64("--timeout", &value)?;
                }
                "--compile-timeout" => {
                    let value = parse_value(&mut iter, "--compile-timeout")?;
                    parsed.compile_timeout_ms = parse_u64("--compile-timeout", &value)?;
                }
                "--jobs" => {
                    let value = parse_value(&mut iter, "--jobs")?;
                    parsed.jobs = Some(parse_jobs(&value)?);
                }
                "--backend" => {
                    let value = parse_value(&mut iter, "--backend")?;
                    parsed.backend = BackendArg::from_str(&value)?;
                }
                flag if flag.starts_with("--") => return Err(format!("Unknown option: {flag}")),
                path => parsed.paths.push(PathBuf::from(path)),
            }
        }

        parsed.finish()
    }
}

struct ParsedArgs {
    paths: Vec<PathBuf>,
    timeout_ms: u64,
    compile_timeout_ms: u64,
    jobs: Option<usize>,
    quiet: bool,
    report_json: bool,
    release: bool,
    backend: BackendArg,
    frontend: FrontendArg,
}

impl Default for ParsedArgs {
    fn default() -> Self {
        Self {
            paths: vec![],
            timeout_ms: DEFAULT_RUNTIME_TIMEOUT_MS,
            compile_timeout_ms: DEFAULT_COMPILE_TIMEOUT_MS,
            jobs: None,
            quiet: false,
            report_json: false,
            release: false,
            backend: BackendArg::Vm,
            frontend: FrontendArg::Default,
        }
    }
}

impl ParsedArgs {
    fn finish(self) -> Result<RunnerArgs, String> {
        if self.paths.is_empty() {
            return Err("Provide one or more directories or files as arguments".to_string());
        }

        for path in &self.paths {
            if !path.is_file() && !path.is_dir() {
                return Err(format!("Path not found: {}", path.display()));
            }
        }

        Ok(RunnerArgs {
            paths: self.paths,
            timeout_ms: self.timeout_ms,
            compile_timeout_ms: self.compile_timeout_ms,
            jobs: self.jobs,
            quiet: self.quiet,
            report_json: self.report_json,
            release: self.release,
            backend: self.backend,
            frontend: self.frontend,
        })
    }
}

fn parse_value(iter: &mut impl Iterator<Item = String>, flag: &str) -> Result<String, String> {
    let Some(value) = iter.next() else {
        return Err(missing_value(flag));
    };
    if value.starts_with("--") {
        return Err(missing_value(flag));
    }

    Ok(value)
}

fn missing_value(flag: &str) -> String {
    format!("Missing value for {flag}")
}

fn parse_u64(flag: &str, value: &str) -> Result<u64, String> {
    value
        .parse::<u64>()
        .map_err(|_| format!("Invalid {flag} value: '{value}'"))
}

fn parse_jobs(value: &str) -> Result<usize, String> {
    let jobs = value
        .parse::<usize>()
        .map_err(|_| format!("Invalid --jobs value: '{value}'"))?;
    if jobs == 0 {
        return Err("--jobs must be greater than zero".to_string());
    }

    Ok(jobs)
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use super::{
        BackendArg, DEFAULT_COMPILE_TIMEOUT_MS, DEFAULT_RUNTIME_TIMEOUT_MS, FrontendArg,
        RunnerArgs, usage,
    };

    fn args(items: &[&str]) -> Vec<String> {
        items.iter().map(ToString::to_string).collect()
    }

    fn parse(items: &[&str]) -> Result<RunnerArgs, String> {
        RunnerArgs::parse(args(items))
    }

    #[test]
    fn parses_defaults() {
        let parsed = parse(&["test-runner", "."]).unwrap();

        assert_eq!(parsed.paths, [PathBuf::from(".")]);
        assert_eq!(parsed.timeout_ms, DEFAULT_RUNTIME_TIMEOUT_MS);
        assert_eq!(parsed.compile_timeout_ms, DEFAULT_COMPILE_TIMEOUT_MS);
        assert_eq!(parsed.jobs, None);
        assert!(!parsed.quiet);
        assert!(!parsed.report_json);
        assert!(!parsed.release);
        assert_eq!(parsed.backend, BackendArg::Vm);
        assert_eq!(parsed.frontend, FrontendArg::Default);
    }

    #[test]
    fn parses_all_options_in_one_pass() {
        let parsed = parse(&[
            "test-runner",
            "--quiet",
            "--report-json",
            "--release",
            "--new-frontend",
            "--timeout",
            "1234",
            ".",
            "--compile-timeout",
            "5678",
            "--jobs",
            "4",
            "--backend",
            "both",
        ])
        .unwrap();

        assert_eq!(parsed.timeout_ms, 1234);
        assert_eq!(parsed.compile_timeout_ms, 5678);
        assert_eq!(parsed.jobs, Some(4));
        assert!(parsed.quiet);
        assert!(parsed.report_json);
        assert!(parsed.release);
        assert_eq!(parsed.backend, BackendArg::Both);
        assert_eq!(parsed.frontend, FrontendArg::New);
    }

    #[test]
    fn accepts_options_before_and_after_paths() {
        let parsed = parse(&[
            "test-runner",
            "--timeout",
            "10",
            ".",
            "--compile-timeout",
            "20",
        ])
        .unwrap();

        assert_eq!(parsed.paths, [PathBuf::from(".")]);
        assert_eq!(parsed.timeout_ms, 10);
        assert_eq!(parsed.compile_timeout_ms, 20);
    }

    #[test]
    fn repeated_value_options_use_last_value() {
        let parsed = parse(&[
            "test-runner",
            ".",
            "--timeout",
            "10",
            "--timeout",
            "20",
            "--backend",
            "vm",
            "--backend",
            "rust",
        ])
        .unwrap();

        assert_eq!(parsed.timeout_ms, 20);
        assert_eq!(parsed.backend, BackendArg::Rust);
    }

    #[test]
    fn rejects_unknown_options() {
        assert_eq!(
            parse(&["test-runner", ".", "--unknown"]).unwrap_err(),
            "Unknown option: --unknown"
        );
    }

    #[test]
    fn rejects_driver() {
        assert_eq!(
            parse(&["test-runner", ".", "--driver", "cli"]).unwrap_err(),
            "Unknown option: --driver"
        );
    }

    #[test]
    fn rejects_missing_values() {
        assert_eq!(
            parse(&["test-runner", ".", "--timeout", "--quiet"]).unwrap_err(),
            "Missing value for --timeout"
        );
        assert_eq!(
            parse(&["test-runner", ".", "--jobs"]).unwrap_err(),
            "Missing value for --jobs"
        );
    }

    #[test]
    fn rejects_invalid_values() {
        assert_eq!(
            parse(&["test-runner", ".", "--timeout", "slow"]).unwrap_err(),
            "Invalid --timeout value: 'slow'"
        );
        assert_eq!(
            parse(&["test-runner", ".", "--jobs", "0"]).unwrap_err(),
            "--jobs must be greater than zero"
        );
        assert_eq!(
            parse(&["test-runner", ".", "--backend", "unknown"]).unwrap_err(),
            "Unknown backend: 'unknown'. Expected 'vm', 'rust', or 'both'"
        );
    }

    #[test]
    fn rejects_missing_paths() {
        assert_eq!(
            parse(&["test-runner", "--quiet"]).unwrap_err(),
            "Provide one or more directories or files as arguments"
        );
    }

    #[test]
    fn rejects_missing_path_targets() {
        assert!(parse(&["test-runner", "definitely-not-an-anvyx-test-path"]).is_err());
    }

    #[test]
    fn expands_backend_args() {
        assert_eq!(BackendArg::Vm.expand(), &["vm"]);
        assert_eq!(BackendArg::Rust.expand(), &["rust"]);
        assert_eq!(BackendArg::Both.expand(), &["vm", "rust"]);
    }

    #[test]
    fn labels_backend_args() {
        assert_eq!(BackendArg::Vm.as_str(), "vm");
        assert_eq!(BackendArg::Rust.as_str(), "rust");
        assert_eq!(BackendArg::Both.as_str(), "both");
    }

    #[test]
    fn usage_documents_new_frontend() {
        assert!(usage().contains("--new-frontend"));
    }

    #[test]
    fn usage_omits_driver() {
        assert!(!usage().contains("--driver"));
    }
}
