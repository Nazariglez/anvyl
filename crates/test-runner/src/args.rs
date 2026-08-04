use std::path::PathBuf;

pub const DEFAULT_RUNTIME_TIMEOUT_MS: u64 = 2_000;
pub const DEFAULT_COMPILE_TIMEOUT_MS: u64 = 300_000;

#[derive(Debug)]
pub struct RunnerArgs {
    pub paths: Vec<PathBuf>,
    pub timeout_ms: u64,
    pub compile_timeout_ms: u64,
    pub jobs: Option<usize>,
    pub quiet: bool,
    pub report_json: bool,
    pub release: bool,
}

pub fn usage() -> String {
    format!(
        "\
Usage: test-runner [OPTIONS] <PATH>...

Arguments:
  <PATH>...  One or more test files or directories

Options:
  --timeout <ms>            Runtime timeout in milliseconds (default: {DEFAULT_RUNTIME_TIMEOUT_MS})
  --compile-timeout <ms>    Compile timeout in milliseconds (default: {DEFAULT_COMPILE_TIMEOUT_MS})
  --jobs <n>                Maximum tests to run in parallel (default: rayon default)
  --quiet                   Suppress individual test output
  --report-json             Emit machine-readable JSON instead of human output
  --release                 Build the CLI and run fixtures in release mode"
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
