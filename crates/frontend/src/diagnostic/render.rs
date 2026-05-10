use std::fmt;

use ariadne::{Color, Config, IndexType, Label, Report, ReportKind};

use super::{Diagnostic, DiagnosticReport, LabelStyle, Severity};
use crate::source::{SourceFile, SourceId};

pub fn render_plain_diagnostic(diagnostic: &Diagnostic) -> String {
    let severity = diagnostic.severity().as_str();
    format!("{severity}: {}", diagnostic.message())
}

pub fn render_plain_report(report: &DiagnosticReport) -> String {
    report
        .diagnostics()
        .iter()
        .map(render_plain_diagnostic)
        .collect::<Vec<_>>()
        .join("\n")
}

pub fn render_rich_report(report: &DiagnosticReport) -> String {
    let mut output = String::new();
    for diagnostic in report.diagnostics() {
        if !output.is_empty() {
            output.push('\n');
        }
        output.push_str(&render_rich_diagnostic(report, diagnostic));
    }
    output
}

fn render_rich_diagnostic(report: &DiagnosticReport, diagnostic: &Diagnostic) -> String {
    let Some(anchor) = report.anchor_label(diagnostic) else {
        return render_plain_diagnostic(diagnostic);
    };
    let Some(anchor_file) = report.source(anchor.span.source()) else {
        return render_plain_diagnostic(diagnostic);
    };

    let kind = match diagnostic.severity() {
        Severity::Error => ReportKind::Error,
        Severity::Warning => ReportKind::Warning,
    };
    let anchor_source = source_key(anchor_file);
    let anchor_range = anchor.span.start()..anchor.span.end();
    let mut builder = Report::build(kind, (anchor_source.clone(), anchor_range))
        .with_config(
            Config::default()
                .with_color(false)
                .with_index_type(IndexType::Byte),
        )
        .with_message(diagnostic.message());

    for label in diagnostic.labels() {
        let Some(file) = report.source(label.span.source()) else {
            continue;
        };
        let color = match label.style {
            LabelStyle::Primary => Color::Red,
            LabelStyle::Secondary => Color::Blue,
        };
        let mut ariadne_label =
            Label::new((source_key(file), label.span.start()..label.span.end())).with_color(color);
        if let Some(message) = &label.message {
            ariadne_label = ariadne_label.with_message(message);
        }
        builder = builder.with_label(ariadne_label);
    }

    for note in diagnostic.notes() {
        builder = builder.with_note(note);
    }
    if let Some(help) = diagnostic.help() {
        builder = builder.with_help(help);
    }

    let mut cache = ariadne::sources(
        report
            .sources
            .iter()
            .map(|file| (source_key(file), file.text())),
    );
    let mut bytes = Vec::new();
    if builder.finish().write(&mut cache, &mut bytes).is_err() {
        return render_plain_diagnostic(diagnostic);
    }
    String::from_utf8(bytes).unwrap_or_else(|_| render_plain_diagnostic(diagnostic))
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct RenderSource {
    id: SourceId,
    name: String,
}

impl fmt::Display for RenderSource {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.name)
    }
}

fn source_key(file: &SourceFile) -> RenderSource {
    RenderSource {
        id: file.id(),
        name: file.label().to_string(),
    }
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use super::{render_plain_diagnostic, render_rich_report};
    use crate::{
        diagnostic::{Diagnostic, DiagnosticReport},
        source::{SourceKind, SourceTable},
        span::SourceSpan,
    };

    fn report(text: &str, diagnostic: Diagnostic) -> DiagnosticReport {
        let mut sources = SourceTable::default();
        sources.add(SourceKind::Virtual, "main.anv", None, text);
        DiagnosticReport {
            sources,
            diagnostics: vec![diagnostic],
        }
    }

    fn report_with_source(
        text: &str,
        make: impl FnOnce(crate::source::SourceId) -> Diagnostic,
    ) -> DiagnosticReport {
        let mut sources = SourceTable::default();
        let source = sources.add(SourceKind::Virtual, "main.anv", None, text);
        DiagnosticReport {
            sources,
            diagnostics: vec![make(source)],
        }
    }

    #[test]
    fn plain_renderer_keeps_message_shape() {
        assert_eq!(
            render_plain_diagnostic(&Diagnostic::warning("careful")),
            "warning: careful"
        );
    }

    #[test]
    fn renders_primary_label_message() {
        let report = report_with_source("let x = true;", |source| {
            Diagnostic::error("bad bool")
                .with_primary_message(SourceSpan::new(source, 8, 12), "expected int, found bool")
        });

        let rendered = render_rich_report(&report);

        assert!(rendered.contains("Error: bad bool"), "{rendered}");
        assert!(rendered.contains("main.anv"), "{rendered}");
        assert!(rendered.contains("let x = true;"), "{rendered}");
        assert!(rendered.contains("expected int, found bool"), "{rendered}");
    }

    #[test]
    fn renders_source_label_without_message() {
        let report = report_with_source("let x = true;", |source| {
            Diagnostic::error("bad bool").with_primary(SourceSpan::new(source, 8, 12))
        });

        let rendered = render_rich_report(&report);

        assert!(rendered.contains("Error: bad bool"), "{rendered}");
        assert!(rendered.contains("main.anv"), "{rendered}");
        assert!(rendered.contains("let x = true;"), "{rendered}");
    }

    #[test]
    fn renders_secondary_anchor_when_no_primary_exists() {
        let report = report_with_source("let x = true;", |source| {
            Diagnostic::error("bad bool")
                .with_secondary_message(SourceSpan::new(source, 8, 12), "related bool")
        });

        let rendered = render_rich_report(&report);

        assert!(rendered.contains("Error: bad bool"), "{rendered}");
        assert!(rendered.contains("related bool"), "{rendered}");
    }

    #[test]
    fn rich_renderer_uses_source_label_not_machine_path() {
        let mut sources = SourceTable::default();
        let source = sources.add(
            SourceKind::Root,
            "typed/main.anv",
            Some(PathBuf::from("/canonical/main.anv")),
            "let x = true;",
        );
        let diagnostic = Diagnostic::error("bad bool")
            .with_primary_message(SourceSpan::new(source, 8, 12), "expected int, found bool");
        let report = DiagnosticReport {
            sources,
            diagnostics: vec![diagnostic],
        };

        let rendered = render_rich_report(&report);

        assert!(rendered.contains("typed/main.anv"), "{rendered}");
        assert!(!rendered.contains("/canonical/main.anv"), "{rendered}");
    }

    #[test]
    fn renders_secondary_labels_notes_and_help() {
        let mut sources = SourceTable::default();
        let source = sources.add(
            SourceKind::Virtual,
            "main.anv",
            None,
            "let x = 1;\nlet y = x;",
        );
        let diagnostic = Diagnostic::error("duplicate")
            .with_primary_message(SourceSpan::new(source, 15, 16), "redefined here")
            .with_secondary_message(SourceSpan::new(source, 4, 5), "first defined here")
            .with_note("names share a scope")
            .with_help("rename one binding");
        let report = DiagnosticReport {
            sources,
            diagnostics: vec![diagnostic],
        };

        let rendered = render_rich_report(&report);

        assert!(rendered.contains("redefined here"), "{rendered}");
        assert!(rendered.contains("first defined here"), "{rendered}");
        assert!(rendered.contains("names share a scope"), "{rendered}");
        assert!(rendered.contains("rename one binding"), "{rendered}");
    }

    #[test]
    fn renders_empty_eof_span() {
        let report = report_with_source("fn", |source| {
            Diagnostic::error("Unexpected end of input").with_primary(SourceSpan::empty(source, 2))
        });

        let rendered = render_rich_report(&report);

        assert!(rendered.contains("Unexpected end of input"), "{rendered}");
        assert!(rendered.contains("fn"), "{rendered}");
    }

    #[test]
    fn renders_non_ascii_byte_offsets() {
        let report = report_with_source("let café = 1;\nlet x = café;", |source| {
            Diagnostic::error("bad name").with_primary(SourceSpan::new(source, 4, 9))
        });

        let rendered = render_rich_report(&report);

        assert!(rendered.contains("let café = 1;"), "{rendered}");
        assert!(rendered.contains("1:5"), "{rendered}");
    }

    #[test]
    fn message_only_diagnostic_uses_plain_shape() {
        let report = report("", Diagnostic::error("provider failed"));

        assert_eq!(render_rich_report(&report), "error: provider failed");
    }
}
