use std::{fmt, path::PathBuf};

use anvyx_frontend::pipeline::CheckError as FCheckError;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CheckOk;

pub type CheckResult<T = CheckOk> = Result<T, CheckError>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CheckError {
    InvalidInput(String),
    ReadMain { path: PathBuf, message: String },
    ReadModule { path: PathBuf, message: String },
    Frontend(FCheckError<CheckError>),
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
            Self::Frontend(error) => display_frontend_error(f, error),
        }
    }
}

fn display_frontend_error(
    f: &mut fmt::Formatter<'_>,
    error: &FCheckError<CheckError>,
) -> fmt::Result {
    match error {
        FCheckError::Lex { label, messages } => {
            write_messages(f, &format!("frontend lex failed in {label}"), messages)
        }
        FCheckError::Parse { label, messages } => {
            write_messages(f, &format!("frontend parse failed in {label}"), messages)
        }
        FCheckError::Resolve { messages } => write_messages(f, "frontend resolve failed", messages),
        FCheckError::Type { messages } => write_messages(f, "frontend typecheck failed", messages),
        FCheckError::Source(error) => write!(f, "{error}"),
    }
}

fn write_messages(f: &mut fmt::Formatter<'_>, header: &str, messages: &[String]) -> fmt::Result {
    write!(f, "{header}:")?;
    for message in messages {
        write!(f, "\n- {message}")?;
    }
    Ok(())
}

impl std::error::Error for CheckError {}

impl From<FCheckError<CheckError>> for CheckError {
    fn from(error: FCheckError<CheckError>) -> Self {
        match error {
            FCheckError::Source(error) => *error,
            error => Self::Frontend(error),
        }
    }
}

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

    #[test]
    fn display_frontend_resolve_error() {
        let frontend = anvyx_frontend::pipeline::CheckError::Resolve {
            messages: vec!["missing module".to_string()],
        };
        let error = CheckError::Frontend(frontend);

        assert_eq!(
            error.to_string(),
            "frontend resolve failed:\n- missing module"
        );
    }

    #[test]
    fn display_frontend_parse_error_includes_label() {
        let frontend = anvyx_frontend::pipeline::CheckError::Parse {
            label: "main.anv".to_string(),
            messages: vec!["expected expression".to_string()],
        };
        let error = CheckError::Frontend(frontend);

        assert_eq!(
            error.to_string(),
            "frontend parse failed in main.anv:\n- expected expression"
        );
    }

    #[test]
    fn frontend_error_converts_from_frontend_check_error() {
        let frontend = anvyx_frontend::pipeline::CheckError::Type {
            messages: vec!["bad type".to_string()],
        };
        let error = CheckError::from(frontend.clone());

        assert_eq!(error, CheckError::Frontend(frontend));
    }

    #[test]
    fn check_error_implements_std_error() {
        fn assert_error<E: std::error::Error>() {}
        assert_error::<CheckError>();
    }
}
