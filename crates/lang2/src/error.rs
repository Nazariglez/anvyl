use std::{fmt, path::PathBuf};

pub type CheckOutput = anvyx_frontend::pipeline::CheckOutput;
pub type CheckStatus = anvyx_frontend::pipeline::CheckStatus;
pub type CheckPhase = anvyx_frontend::pipeline::CheckPhase;
pub type CheckResult = Result<CheckOutput, CheckError>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CheckError {
    InvalidInput(String),
    ReadMain { path: PathBuf, message: String },
    ReadModule { path: PathBuf, message: String },
}

impl fmt::Display for CheckError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::InvalidInput(message) => write!(f, "invalid input: {message}"),
            Self::ReadMain { path, message } => {
                write!(
                    f,
                    "failed to read main source '{}': {message}",
                    path.display()
                )
            }
            Self::ReadModule { path, message } => {
                write!(
                    f,
                    "failed to read module source '{}': {message}",
                    path.display()
                )
            }
        }
    }
}

impl std::error::Error for CheckError {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn display_invalid_input() {
        let error = CheckError::InvalidInput("bad path".to_string());
        assert_eq!(error.to_string(), "invalid input: bad path");
    }

    #[test]
    fn display_read_main_includes_path_and_message() {
        let error = CheckError::ReadMain {
            path: PathBuf::from("src/main.anv"),
            message: "permission denied".to_string(),
        };

        let message = error.to_string();
        assert!(message.contains("src/main.anv"));
        assert!(message.contains("permission denied"));
    }
}
