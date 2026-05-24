use std::{
    error::Error,
    fmt, fs,
    path::PathBuf,
    process::Command,
    time::{SystemTime, UNIX_EPOCH},
};

use super::emit::RustSource;

#[derive(Debug, Clone)]
pub struct RustSourceJob {
    pub source: RustSource,
    pub work_dir: Option<PathBuf>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RustSourceJobOutput {
    pub status: SourceJobStatus,
    pub stdout: String,
    pub stderr: String,
    pub artifact: PathBuf,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SourceJobStatus {
    Success,
    CompileFailed(Option<i32>),
    RunFailed(Option<i32>),
}

#[derive(Debug)]
pub enum RustSourceJobError {
    Io(std::io::Error),
    RustcUnavailable,
}

impl fmt::Display for RustSourceJobError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Io(error) => write!(f, "source job I/O failed: {error}"),
            Self::RustcUnavailable => write!(f, "rustc is unavailable"),
        }
    }
}

impl Error for RustSourceJobError {}

impl From<std::io::Error> for RustSourceJobError {
    fn from(error: std::io::Error) -> Self {
        Self::Io(error)
    }
}

pub fn compile_and_run(job: &RustSourceJob) -> Result<RustSourceJobOutput, RustSourceJobError> {
    let dir = match &job.work_dir {
        Some(path) => path.clone(),
        None => std::env::temp_dir().join(format!("anvyx-rust-job-{}", unique_id())),
    };
    fs::create_dir_all(&dir)?;
    let source = dir.join("main.rs");
    let artifact = dir.join("main");
    fs::write(&source, job.source.as_str())?;
    let compile = Command::new("rustc")
        .arg("--edition=2021")
        .arg(&source)
        .arg("-o")
        .arg(&artifact)
        .output()
        .map_err(|error| {
            if error.kind() == std::io::ErrorKind::NotFound {
                RustSourceJobError::RustcUnavailable
            } else {
                RustSourceJobError::Io(error)
            }
        })?;
    if !compile.status.success() {
        return Ok(output(
            SourceJobStatus::CompileFailed(compile.status.code()),
            compile.stdout,
            compile.stderr,
            artifact,
        ));
    }
    let run = Command::new(&artifact).output()?;
    let status = if run.status.success() {
        SourceJobStatus::Success
    } else {
        SourceJobStatus::RunFailed(run.status.code())
    };
    Ok(output(status, run.stdout, run.stderr, artifact))
}

fn output(
    status: SourceJobStatus,
    stdout: Vec<u8>,
    stderr: Vec<u8>,
    artifact: PathBuf,
) -> RustSourceJobOutput {
    RustSourceJobOutput {
        status,
        stdout: String::from_utf8_lossy(&stdout).into_owned(),
        stderr: String::from_utf8_lossy(&stderr).into_owned(),
        artifact,
    }
}

fn unique_id() -> u128 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_nanos()
}
