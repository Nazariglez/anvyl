use std::fmt;

pub mod render;

use crate::{source::SourceTable, span::SourceSpan};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Severity {
    Error,
    Warning,
}

pub use Severity as DiagnosticSeverity;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LabelStyle {
    Primary,
    Secondary,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DiagnosticLabel {
    pub style: LabelStyle,
    pub span: SourceSpan,
    pub message: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Diagnostic {
    severity: Severity,
    message: String,
    labels: Vec<DiagnosticLabel>,
    notes: Vec<String>,
    help: Option<String>,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct DiagnosticReport {
    pub sources: SourceTable,
    pub diagnostics: Vec<Diagnostic>,
}

impl DiagnosticReport {
    #[must_use]
    pub fn diagnostics(&self) -> &[Diagnostic] {
        &self.diagnostics
    }
}

impl Diagnostic {
    #[must_use]
    pub fn error(message: impl Into<String>) -> Self {
        Self::new(Severity::Error, message)
    }

    #[must_use]
    pub fn warning(message: impl Into<String>) -> Self {
        Self::new(Severity::Warning, message)
    }

    fn new(severity: Severity, message: impl Into<String>) -> Self {
        Self {
            severity,
            message: message.into(),
            labels: vec![],
            notes: vec![],
            help: None,
        }
    }

    #[must_use]
    pub fn with_primary(self, span: SourceSpan) -> Self {
        self.with_label(LabelStyle::Primary, span, None)
    }

    #[must_use]
    pub fn with_primary_message(self, span: SourceSpan, message: impl Into<String>) -> Self {
        self.with_label(LabelStyle::Primary, span, Some(message.into()))
    }

    #[must_use]
    pub fn with_secondary_message(self, span: SourceSpan, message: impl Into<String>) -> Self {
        self.with_label(LabelStyle::Secondary, span, Some(message.into()))
    }

    #[must_use]
    pub fn with_note(mut self, note: impl Into<String>) -> Self {
        self.notes.push(note.into());
        self
    }

    #[must_use]
    pub fn with_help(mut self, help: impl Into<String>) -> Self {
        self.help = Some(help.into());
        self
    }

    #[must_use]
    pub fn severity(&self) -> Severity {
        self.severity
    }

    #[must_use]
    pub fn message(&self) -> &str {
        &self.message
    }

    #[must_use]
    pub fn labels(&self) -> &[DiagnosticLabel] {
        &self.labels
    }

    #[must_use]
    pub fn notes(&self) -> &[String] {
        &self.notes
    }

    #[must_use]
    pub fn help(&self) -> Option<&str> {
        self.help.as_deref()
    }

    fn with_label(mut self, style: LabelStyle, span: SourceSpan, message: Option<String>) -> Self {
        self.labels.push(DiagnosticLabel {
            style,
            span,
            message,
        });
        self
    }
}

impl fmt::Display for Diagnostic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.message)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::source::{SourceKind, SourceTable};

    fn span(start: usize, end: usize) -> SourceSpan {
        let mut sources = SourceTable::default();
        let source = sources.add(SourceKind::Virtual, "test", None, "test source");
        SourceSpan::new(source, start, end)
    }

    #[test]
    fn constructors_preserve_plain_messages() {
        let error = Diagnostic::error("bad");
        let warning = Diagnostic::warning("careful");

        assert_eq!(error.severity(), Severity::Error);
        assert_eq!(warning.severity(), Severity::Warning);
        assert_eq!(error.message(), "bad");
        assert_eq!(warning.to_string(), "careful");
        assert!(error.labels().is_empty());
    }

    #[test]
    fn label_builders_append_labels() {
        let primary = span(1, 3);
        let secondary = span(5, 8);
        let diagnostic = Diagnostic::error("bad")
            .with_primary(primary)
            .with_secondary_message(secondary, "related");

        assert_eq!(diagnostic.labels().len(), 2);
        assert_eq!(diagnostic.labels()[0].style, LabelStyle::Primary);
        assert_eq!(diagnostic.labels()[0].span, primary);
        assert_eq!(diagnostic.labels()[0].message, None);
        assert_eq!(diagnostic.labels()[1].style, LabelStyle::Secondary);
        assert_eq!(diagnostic.labels()[1].message.as_deref(), Some("related"));
    }

    #[test]
    fn note_and_help_builders_append_adapter_data() {
        let diagnostic = Diagnostic::warning("deprecated")
            .with_note("since 1.0")
            .with_note("use the new API")
            .with_help("rename it");

        assert_eq!(diagnostic.notes(), &["since 1.0", "use the new API"]);
        assert_eq!(diagnostic.help(), Some("rename it"));
    }

    #[test]
    fn report_carries_sources_with_diagnostics() {
        let mut sources = SourceTable::default();
        let source = sources.add(SourceKind::Virtual, "test", None, "test source");
        let diagnostic = Diagnostic::error("bad").with_primary(SourceSpan::new(source, 0, 4));
        let report = DiagnosticReport {
            sources,
            diagnostics: vec![diagnostic],
        };

        assert_eq!(report.sources.len(), 1);
        assert_eq!(report.diagnostics[0].message(), "bad");
        assert_eq!(report.diagnostics[0].labels()[0].span.source(), source);
    }
}
