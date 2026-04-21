use ariadne::{Color, Config, IndexType, Label, Report, ReportKind};

use crate::{ast, lexer::SpannedToken, span::Span};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Severity {
    Error,
    Warning,
    Note,
}

#[derive(Debug, Clone)]
pub struct DiagnosticLabel {
    pub file: String,
    pub span: Span,
    pub message: String,
}

#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub severity: Severity,
    pub message: String,
    pub primary: DiagnosticLabel,
    pub related: Vec<DiagnosticLabel>,
    pub notes: Vec<String>,
    pub help: Option<String>,
}

#[derive(Debug, Clone)]
pub struct DiagnosticFile {
    pub path: String,
    pub source: String,
}

#[derive(Debug, Clone, Default)]
pub struct DiagnosticReport {
    pub diagnostics: Vec<Diagnostic>,
    pub files: Vec<DiagnosticFile>,
}

#[derive(Debug)]
pub struct CompileOutput<T> {
    pub value: T,
    pub report: DiagnosticReport,
}

pub type CompileResult<T> = Result<CompileOutput<T>, DiagnosticReport>;

#[derive(Debug)]
pub struct RunOutput {
    pub stdout: String,
    pub report: DiagnosticReport,
}

#[derive(Debug)]
pub enum RunError {
    Compile(DiagnosticReport),
    Runtime(String),
}

pub(crate) struct LoadedFile {
    pub path: String,
    pub source: String,
    pub tokens: Vec<SpannedToken>,
}

impl LoadedFile {
    pub(crate) fn to_source_location_info(&self) -> crate::intrinsic::SourceLocationInfo {
        crate::intrinsic::SourceLocationInfo::new(self.path.clone(), &self.source, &self.tokens)
    }
}

pub(crate) struct ParsedFile {
    pub file: LoadedFile,
    pub ast: ast::Program,
}

impl DiagnosticReport {
    pub(crate) fn add_file(&mut self, path: &str, source: &str) {
        if !self.files.iter().any(|f| f.path == path) {
            self.files.push(DiagnosticFile {
                path: path.to_string(),
                source: source.to_string(),
            });
        }
    }

    pub(crate) fn merge(&mut self, other: DiagnosticReport) {
        for file in other.files {
            if !self.files.iter().any(|f| f.path == file.path) {
                self.files.push(file);
            }
        }
        self.diagnostics.extend(other.diagnostics);
    }

    pub fn print_ariadne(&self) {
        let mut cache = ariadne::sources(
            self.files
                .iter()
                .map(|f| (f.path.clone(), f.source.as_str())),
        );

        for diag in &self.diagnostics {
            let (kind, color) = match diag.severity {
                Severity::Error => (ReportKind::Error, Color::Red),
                Severity::Warning | Severity::Note => (ReportKind::Warning, Color::Yellow),
            };

            let primary_range = diag.primary.span.start..diag.primary.span.end;
            let mut report =
                Report::build(kind, (diag.primary.file.clone(), primary_range.clone()))
                    .with_config(Config::default().with_index_type(IndexType::Byte))
                    .with_message(&diag.message)
                    .with_label(
                        Label::new((diag.primary.file.clone(), primary_range))
                            .with_color(color)
                            .with_message(&diag.primary.message),
                    );

            for related in &diag.related {
                let range = related.span.start..related.span.end;
                report = report.with_label(
                    Label::new((related.file.clone(), range))
                        .with_color(Color::Blue)
                        .with_message(&related.message),
                );
            }

            for note in &diag.notes {
                report = report.with_note(note);
            }

            if let Some(h) = &diag.help {
                report = report.with_help(h);
            }

            let finished = report.finish();
            let _ = match diag.severity {
                Severity::Error => finished.print(&mut cache),
                Severity::Warning | Severity::Note => finished.eprint(&mut cache),
            };
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::span::Span;

    #[test]
    fn print_ariadne_smoke() {
        let report = DiagnosticReport {
            files: vec![DiagnosticFile {
                path: "<test>".to_string(),
                source: "let x = 1;".to_string(),
            }],
            diagnostics: vec![Diagnostic {
                severity: Severity::Error,
                message: "test error".to_string(),
                primary: DiagnosticLabel {
                    file: "<test>".to_string(),
                    span: Span::new(4, 5),
                    message: "here".to_string(),
                },
                related: vec![],
                notes: vec!["this is a note".to_string()],
                help: Some("try something else".to_string()),
            }],
        };

        report.print_ariadne();
    }
}
