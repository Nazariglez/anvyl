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
pub struct DiagnosticCode {
    pub source: &'static str,
    pub code: String,
    pub kind: DiagnosticCodeKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DiagnosticCodeKind {
    Plain,
    Lint {
        level: crate::lint::LintLevel,
        origin: crate::config::LintLevelOrigin,
    },
}

impl DiagnosticCode {
    #[must_use]
    pub fn metadata_note(&self) -> Option<String> {
        let DiagnosticCodeKind::Lint { level, origin } = self.kind else {
            return None;
        };
        let name = &self.code;
        Some(match origin {
            crate::config::LintLevelOrigin::Default => format!("lint `{name}` is on by default"),
            crate::config::LintLevelOrigin::Configured => {
                let level = match level {
                    crate::lint::LintLevel::Allow => "allow",
                    crate::lint::LintLevel::Warn => "warning",
                    crate::lint::LintLevel::Error => "error",
                };
                format!("lint `{name}` was set to {level}")
            }
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DiagnosticTag {
    Deprecated,
    Unnecessary,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Diagnostic {
    severity: Severity,
    code: Option<DiagnosticCode>,
    tags: Vec<DiagnosticTag>,
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

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct DiagnosticProjection<'a> {
    pub severity: Option<Severity>,
    pub extra_note: Option<&'a str>,
}

impl DiagnosticProjection<'_> {
    #[must_use]
    pub fn severity_for(self, diagnostic: &Diagnostic) -> Severity {
        self.severity.unwrap_or_else(|| diagnostic.severity())
    }

    #[must_use]
    pub fn notes_for(self, diagnostic: &Diagnostic) -> Vec<String> {
        let mut notes = diagnostic.notes_with_metadata();
        if let Some(note) = self.extra_note {
            notes.push(note.to_string());
        }
        notes
    }
}

impl DiagnosticReport {
    #[must_use]
    pub fn new(sources: SourceTable, diagnostics: Vec<Diagnostic>) -> Self {
        Self {
            sources,
            diagnostics,
        }
    }

    #[must_use]
    pub fn diagnostics(&self) -> &[Diagnostic] {
        &self.diagnostics
    }

    #[must_use]
    pub fn source(&self, id: SourceId) -> Option<&SourceFile> {
        self.sources.get(id)
    }

    #[must_use]
    pub fn has_errors(&self) -> bool {
        self.diagnostics
            .iter()
            .any(|diagnostic| diagnostic.severity() == Severity::Error)
    }

    #[must_use]
    pub fn has_warnings(&self) -> bool {
        self.diagnostics
            .iter()
            .any(|diagnostic| diagnostic.severity() == Severity::Warning)
    }

    #[must_use]
    pub fn sorted(mut self) -> Self {
        let sources = &self.sources;
        self.diagnostics.sort_by(|left, right| {
            diagnostic_sort_key(sources, left).cmp(&diagnostic_sort_key(sources, right))
        });
        self
    }

    #[must_use]
    pub fn anchor_label<'a>(&'a self, diagnostic: &'a Diagnostic) -> Option<&'a DiagnosticLabel> {
        anchor_label(&self.sources, diagnostic)
    }
}

fn anchor_label<'a>(
    sources: &SourceTable,
    diagnostic: &'a Diagnostic,
) -> Option<&'a DiagnosticLabel> {
    let mut fallback = None;
    for label in diagnostic.labels() {
        if sources.get(label.span.source()).is_none() {
            continue;
        }
        if label.style == LabelStyle::Primary {
            return Some(label);
        }
        fallback = fallback.or(Some(label));
    }
    fallback
}

fn diagnostic_sort_key<'a>(
    sources: &SourceTable,
    diagnostic: &'a Diagnostic,
) -> ((usize, usize, usize), usize, &'a str, &'a str, &'a str) {
    let location = anchor_label(sources, diagnostic).map_or((1, usize::MAX, usize::MAX), |label| {
        (0, label.span.source().index(), label.span.start())
    });
    let severity = match diagnostic.severity() {
        Severity::Error => 0,
        Severity::Warning => 1,
    };
    let (code_source, code) = diagnostic
        .code()
        .map_or(("", ""), |code| (code.source, code.code.as_str()));
    (location, severity, code_source, code, diagnostic.message())
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
            code: None,
            tags: vec![],
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
    pub fn with_code(mut self, source: &'static str, code: impl Into<String>) -> Self {
        self.code = Some(DiagnosticCode {
            source,
            code: code.into(),
            kind: DiagnosticCodeKind::Plain,
        });
        self
    }

    #[must_use]
    pub(crate) fn with_lint_code(
        mut self,
        source: &'static str,
        code: impl Into<String>,
        level: crate::lint::LintLevel,
        origin: crate::config::LintLevelOrigin,
    ) -> Self {
        self.code = Some(DiagnosticCode {
            source,
            code: code.into(),
            kind: DiagnosticCodeKind::Lint { level, origin },
        });
        self
    }

    #[must_use]
    pub fn with_tag(mut self, tag: DiagnosticTag) -> Self {
        self.tags.push(tag);
        self
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
    pub fn code(&self) -> Option<&DiagnosticCode> {
        self.code.as_ref()
    }

    #[must_use]
    pub fn tags(&self) -> &[DiagnosticTag] {
        &self.tags
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
    pub fn notes_with_metadata(&self) -> Vec<String> {
        let mut notes = self.notes.clone();
        if let Some(note) = self.code.as_ref().and_then(DiagnosticCode::metadata_note) {
            notes.push(note);
        }
        notes
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
        assert!(error.code().is_none());
        assert!(error.tags().is_empty());
    }

    #[test]
    fn metadata_builders_preserve_plain_messages() {
        let diagnostic = Diagnostic::warning("deprecated")
            .with_code("anvyx", "deprecated")
            .with_tag(DiagnosticTag::Deprecated)
            .with_tag(DiagnosticTag::Unnecessary);

        let code = diagnostic.code().unwrap();
        assert_eq!(code.source, "anvyx");
        assert_eq!(code.code, "deprecated");
        assert_eq!(code.kind, DiagnosticCodeKind::Plain);
        assert_eq!(
            diagnostic.tags(),
            &[DiagnosticTag::Deprecated, DiagnosticTag::Unnecessary]
        );
        assert_eq!(diagnostic.message(), "deprecated");
        assert_eq!(diagnostic.to_string(), "deprecated");
    }

    #[test]
    fn lint_code_generates_metadata_notes() {
        let default = Diagnostic::warning("lint")
            .with_lint_code(
                "anvyx",
                "unused_import",
                crate::lint::LintLevel::Warn,
                crate::config::LintLevelOrigin::Default,
            )
            .with_note("real note");
        let configured = Diagnostic::error("lint").with_lint_code(
            "anvyx",
            "unused_import",
            crate::lint::LintLevel::Error,
            crate::config::LintLevelOrigin::Configured,
        );

        assert_eq!(default.notes(), &["real note"]);
        assert_eq!(
            default.notes_with_metadata(),
            vec![
                "real note".to_string(),
                "lint `unused_import` is on by default".to_string()
            ]
        );
        assert_eq!(
            configured.notes_with_metadata(),
            vec!["lint `unused_import` was set to error".to_string()]
        );
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
        let report = DiagnosticReport::new(sources, vec![diagnostic]);

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
        let report = DiagnosticReport::new(report_sources, vec![diagnostic]);

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
        let report = DiagnosticReport::new(sources, vec![diagnostic]);

        assert_eq!(report.sources.len(), 1);
        assert_eq!(report.diagnostics[0].message(), "bad");
        assert_eq!(report.diagnostics[0].labels()[0].span.source(), source);
    }

    #[test]
    fn report_sorts_by_location_severity_code_and_message() {
        let mut sources = SourceTable::default();
        let first = sources.add(SourceKind::Virtual, "first", None, "abcdef");
        let second = sources.add(SourceKind::Virtual, "second", None, "abcdef");
        let diagnostics = vec![
            Diagnostic::warning("unsourced"),
            Diagnostic::warning("b warning").with_primary(SourceSpan::new(first, 1, 2)),
            Diagnostic::error("a error").with_primary(SourceSpan::new(first, 1, 2)),
            Diagnostic::error("second source").with_primary(SourceSpan::new(second, 0, 1)),
            Diagnostic::error("coded b")
                .with_primary(SourceSpan::new(first, 1, 2))
                .with_code("anvyx", "b"),
            Diagnostic::error("coded a")
                .with_primary(SourceSpan::new(first, 1, 2))
                .with_code("anvyx", "a"),
            Diagnostic::error("first source").with_primary(SourceSpan::new(first, 0, 1)),
        ];
        let report = DiagnosticReport::new(sources, diagnostics).sorted();
        let messages = report
            .diagnostics()
            .iter()
            .map(Diagnostic::message)
            .collect::<Vec<_>>();

        assert_eq!(
            messages,
            [
                "first source",
                "a error",
                "coded a",
                "coded b",
                "b warning",
                "second source",
                "unsourced",
            ]
        );
    }
}
