use std::fmt;

pub mod render;

use crate::{
    source::{SourceFile, SourceId, SourceTable},
    span::SourceSpan,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Severity {
    Error,
    Warning,
}

impl Severity {
    #[must_use]
    pub fn as_str(self) -> &'static str {
        match self {
            Self::Error => "error",
            Self::Warning => "warning",
        }
    }
}

pub use Severity as DiagnosticSeverity;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LabelStyle {
    Primary,
    Secondary,
}

impl LabelStyle {
    #[must_use]
    pub fn as_str(self) -> &'static str {
        match self {
            Self::Primary => "primary",
            Self::Secondary => "secondary",
        }
    }
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

    #[must_use]
    pub fn source(&self, id: SourceId) -> Option<&SourceFile> {
        self.sources.get(id)
    }

    #[must_use]
    pub fn anchor_label<'a>(&'a self, diagnostic: &'a Diagnostic) -> Option<&'a DiagnosticLabel> {
        let mut fallback = None;
        for label in diagnostic.labels() {
            if self.source(label.span.source()).is_none() {
                continue;
            }
            if label.style == LabelStyle::Primary {
                return Some(label);
            }
            fallback = fallback.or(Some(label));
        }
        fallback
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
    pub fn primary_label(&self) -> Option<&DiagnosticLabel> {
        self.labels
            .iter()
            .find(|label| label.style == LabelStyle::Primary)
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
    fn primary_label_skips_secondary_labels() {
        let secondary = span(1, 3);
        let primary = span(5, 8);
        let diagnostic = Diagnostic::error("bad")
            .with_secondary_message(secondary, "related")
            .with_primary(primary);

        assert_eq!(diagnostic.primary_label().unwrap().span, primary);
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
    fn report_anchor_prefers_valid_primary_label() {
        let mut sources = SourceTable::default();
        let source = sources.add(SourceKind::Virtual, "test", None, "abcdef");
        let primary = SourceSpan::new(source, 0, 1);
        let secondary = SourceSpan::new(source, 2, 3);
        let diagnostic = Diagnostic::error("bad")
            .with_secondary_message(secondary, "related")
            .with_primary(primary);
        let report = DiagnosticReport {
            sources,
            diagnostics: vec![diagnostic],
        };

        assert_eq!(report.source(source).unwrap().label(), "test");
        assert_eq!(
            report.anchor_label(&report.diagnostics[0]).unwrap().span,
            primary
        );
    }

    #[test]
    fn report_anchor_falls_back_to_valid_secondary_label() {
        let mut report_sources = SourceTable::default();
        let valid_source = report_sources.add(SourceKind::Virtual, "test", None, "abcdef");
        let mut other_sources = SourceTable::default();
        other_sources.add(SourceKind::Virtual, "other", None, "x");
        let invalid_source = other_sources.add(SourceKind::Virtual, "missing", None, "x");
        let primary = SourceSpan::new(invalid_source, 0, 1);
        let secondary = SourceSpan::new(valid_source, 2, 3);
        let diagnostic = Diagnostic::error("bad")
            .with_primary(primary)
            .with_secondary_message(secondary, "related");
        let report = DiagnosticReport {
            sources: report_sources,
            diagnostics: vec![diagnostic],
        };

        assert_eq!(
            report.anchor_label(&report.diagnostics[0]).unwrap().span,
            secondary
        );
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
