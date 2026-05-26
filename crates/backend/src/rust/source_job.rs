use std::path::PathBuf;

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
