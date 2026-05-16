use std::{fmt, path::PathBuf};

use anvyx_frontend::pipeline::{CheckError as FCheckError, DiagnosticReport};

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
    pub fn report(&self) -> Option<&DiagnosticReport> {
        match self {
            Self::Frontend(error) => error.report(),
            Self::InvalidInput(_) | Self::ReadMain { .. } | Self::ReadModule { .. } => None,
        }
    }

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
            Self::Frontend(error) => f.write_str(&frontend_error_summary(error)),
        }
    }
}

fn frontend_error_summary(error: &FCheckError<CheckError>) -> String {
    match error {
        FCheckError::Lex { .. } => "Failed to lex program".to_string(),
        FCheckError::Parse { .. } => "Failed to parse program".to_string(),
        FCheckError::Resolve { .. } => "Failed to resolve program".to_string(),
        FCheckError::Type { .. } => "Failed to typecheck program".to_string(),
        FCheckError::Extern { .. } => "Failed to ingest extern inputs".to_string(),
        FCheckError::Source(error) => error.summary(),
    }
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
        pipeline::Diagnostic,
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
    fn display_frontend_resolve_error_is_summary_only() {
        let frontend = anvyx_frontend::pipeline::CheckError::Resolve {
            report: report([Diagnostic::error("missing module")]),
        };
        let error = CheckError::Frontend(frontend);

        assert_eq!(error.to_string(), "Failed to resolve program");
    }

    #[test]
    fn summary_omits_frontend_diagnostic_bullets() {
        let frontend = anvyx_frontend::pipeline::CheckError::Type {
            report: report([Diagnostic::error("bad type")]),
        };
        let error = CheckError::Frontend(frontend);

        assert_eq!(error.summary(), "Failed to typecheck program");
    }

    #[test]
    fn display_frontend_parse_error_uses_user_summary() {
        let frontend = anvyx_frontend::pipeline::CheckError::Parse {
            label: "main.anv".to_string(),
            report: report([Diagnostic::error("expected expression")]),
        };
        let error = CheckError::Frontend(frontend);

        assert_eq!(error.to_string(), "Failed to parse program");
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
    fn report_preserves_sources_and_labels() {
        for frontend in report_errors() {
            let error = CheckError::Frontend(frontend);
            let report = error.report().expect("frontend report should be preserved");

            assert_eq!(report.sources.len(), 1);
            assert_eq!(report.diagnostics()[0].message(), "bad");
            assert_eq!(report.diagnostics()[0].labels()[0].span.start(), 0);
        }
    }

    #[test]
    fn non_frontend_errors_have_no_report() {
        let errors = [
            CheckError::InvalidInput("bad".to_string()),
            CheckError::ReadMain {
                path: PathBuf::from("main.anv"),
                message: "missing".to_string(),
            },
            CheckError::ReadModule {
                path: PathBuf::from("mod.anv"),
                message: "missing".to_string(),
            },
        ];

        for error in errors {
            assert!(error.report().is_none());
        }
    }

    #[test]
    fn frontend_conversion_preserves_report() {
        let frontend = anvyx_frontend::pipeline::CheckError::Type {
            report: labelled_report(),
        };
        let error = CheckError::from(frontend);

        assert!(error.report().is_some());
    }

    fn report_errors() -> [FCheckError<CheckError>; 5] {
        [
            FCheckError::Lex {
                label: "main.anv".to_string(),
                report: labelled_report(),
            },
            FCheckError::Parse {
                label: "main.anv".to_string(),
                report: labelled_report(),
            },
            FCheckError::Resolve {
                report: labelled_report(),
            },
            FCheckError::Type {
                report: labelled_report(),
            },
            FCheckError::Extern {
                report: labelled_report(),
            },
        ]
    }

    fn labelled_report() -> DiagnosticReport {
        let mut sources = SourceTable::default();
        let source = sources.add(SourceKind::Virtual, "main.anv", None, "x");
        let span = SourceSpan::new(source, 0, 1);
        DiagnosticReport {
            sources,
            diagnostics: vec![Diagnostic::error("bad").with_primary(span)],
        }
    }

    fn report(diagnostics: impl IntoIterator<Item = Diagnostic>) -> DiagnosticReport {
        DiagnosticReport {
            sources: SourceTable::default(),
            diagnostics: diagnostics.into_iter().collect(),
        }
    }

    #[test]
    fn check_error_implements_std_error() {
        fn assert_error<E: std::error::Error>() {}
        assert_error::<CheckError>();
    }
}
