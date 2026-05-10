use std::{fmt, path::PathBuf};

use anvyx_frontend::pipeline::{CheckError as FCheckError, Diagnostic, DiagnosticReport};

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct CheckOk {
    pub report: DiagnosticReport,
}

impl From<anvyx_frontend::pipeline::CheckOk> for CheckOk {
    fn from(ok: anvyx_frontend::pipeline::CheckOk) -> Self {
        Self { report: ok.report }
    }
}

pub type CheckResult<T = CheckOk> = Result<T, CheckError>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CheckError {
    InvalidInput(String),
    ReadMain { path: PathBuf, message: String },
    ReadModule { path: PathBuf, message: String },
    Frontend(FCheckError<CheckError>),
}

impl CheckError {
    #[must_use]
    pub fn summary(&self) -> String {
        match self {
            Self::InvalidInput(message) => format!("invalid input: {message}"),
            Self::ReadMain { path, .. } => {
                format!("failed to read main source '{}'", path.display())
            }
            Self::ReadModule { path, .. } => {
                format!("failed to read module source '{}'", path.display())
            }
            Self::Frontend(error) => frontend_error_summary(error),
        }
    }
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
        FCheckError::Lex { report, .. }
        | FCheckError::Parse { report, .. }
        | FCheckError::Resolve { report }
        | FCheckError::Type { report }
        | FCheckError::Extern { report } => {
            write_diagnostics(f, &frontend_error_summary(error), report.diagnostics())
        }
        FCheckError::Source(error) => write!(f, "{error}"),
    }
}

fn frontend_error_summary(error: &FCheckError<CheckError>) -> String {
    match error {
        FCheckError::Lex { label, .. } => format!("frontend lex failed in {label}"),
        FCheckError::Parse { label, .. } => format!("frontend parse failed in {label}"),
        FCheckError::Resolve { .. } => "frontend resolve failed".to_string(),
        FCheckError::Type { .. } => "frontend typecheck failed".to_string(),
        FCheckError::Extern { .. } => "frontend extern input failed".to_string(),
        FCheckError::Source(error) => error.summary(),
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
    use anvyx_frontend::{
        source::{SourceKind, SourceTable},
        span::SourceSpan,
    };

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
            report: report([Diagnostic::error("missing module")]),
        };
        let error = CheckError::Frontend(frontend);

        assert_eq!(
            error.to_string(),
            "frontend resolve failed:\n- missing module"
        );
    }

    #[test]
    fn summary_omits_frontend_diagnostic_bullets() {
        let frontend = anvyx_frontend::pipeline::CheckError::Type {
            report: report([Diagnostic::error("bad type")]),
        };
        let error = CheckError::Frontend(frontend);

        assert_eq!(error.summary(), "frontend typecheck failed");
    }

    #[test]
    fn display_frontend_parse_error_includes_label() {
        let frontend = anvyx_frontend::pipeline::CheckError::Parse {
            label: "main.anv".to_string(),
            report: report([Diagnostic::error("expected expression")]),
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
            report: report([Diagnostic::error("bad type")]),
        };
        let error = CheckError::from(frontend.clone());

        assert_eq!(error, CheckError::Frontend(frontend));
    }

    #[test]
    fn frontend_ok_preserves_report() {
        let report = report([Diagnostic::warning("careful")]);
        let ok = CheckOk::from(anvyx_frontend::pipeline::CheckOk {
            report: report.clone(),
        });

        assert_eq!(ok.report, report);
        assert_eq!(ok.report.diagnostics()[0].message(), "careful");
    }

    #[test]
    fn frontend_conversion_preserves_sources_and_labels() {
        let mut sources = SourceTable::default();
        let source = sources.add(SourceKind::Virtual, "main.anv", None, "x");
        let span = SourceSpan::new(source, 0, 1);
        let report = DiagnosticReport {
            sources,
            diagnostics: vec![Diagnostic::error("bad").with_primary(span)],
        };
        let frontend = anvyx_frontend::pipeline::CheckError::Type {
            report: report.clone(),
        };

        let error = CheckError::from(frontend);
        let CheckError::Frontend(FCheckError::Type { report: converted }) = error else {
            panic!("expected frontend type error");
        };
        assert_eq!(converted.sources.len(), 1);
        assert_eq!(converted.diagnostics()[0].labels()[0].span, span);
    }

    fn report(diagnostics: impl IntoIterator<Item = Diagnostic>) -> DiagnosticReport {
        DiagnosticReport {
            sources: Default::default(),
            diagnostics: diagnostics.into_iter().collect(),
        }
    }

    #[test]
    fn check_error_implements_std_error() {
        fn assert_error<E: std::error::Error>() {}
        assert_error::<CheckError>();
    }
}
