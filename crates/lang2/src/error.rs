use std::{fmt, path::PathBuf};

use anvyx_frontend::pipeline::{CheckError as FCheckError, Diagnostic};

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
        FCheckError::Lex { label, diagnostics } => {
            write_diagnostics(f, &format!("frontend lex failed in {label}"), diagnostics)
        }
        FCheckError::Parse { label, diagnostics } => {
            write_diagnostics(f, &format!("frontend parse failed in {label}"), diagnostics)
        }
        FCheckError::Resolve { diagnostics } => {
            write_diagnostics(f, "frontend resolve failed", diagnostics)
        }
        FCheckError::Type { diagnostics } => {
            write_diagnostics(f, "frontend typecheck failed", diagnostics)
        }
        FCheckError::Source(error) => write!(f, "{error}"),
    }
}

fn write_diagnostics(
    f: &mut fmt::Formatter<'_>,
    header: &str,
    diagnostics: &[Diagnostic],
) -> fmt::Result {
    write!(f, "{header}:")?;
    for diagnostic in diagnostics {
        write!(f, "\n- {diagnostic}")?;
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
            diagnostics: vec![Diagnostic::error("missing module")],
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
            diagnostics: vec![Diagnostic::error("expected expression")],
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
            diagnostics: vec![Diagnostic::error("bad type")],
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
