use std::fmt;

use ariadne::{Color, Config, IndexType, Label, Report, ReportKind};

use super::{Diagnostic, DiagnosticProjection, DiagnosticReport, LabelStyle, Severity};
use crate::source::{SourceFile, SourceId};

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct RenderConfig {
    pub color: bool,
}

pub fn render_rich_report(report: &DiagnosticReport) -> String {
    render_rich_report_with_config(report, RenderConfig::default())
}

pub fn render_rich_report_with_config(report: &DiagnosticReport, config: RenderConfig) -> String {
    render_rich_report_with_overrides(report, config, |_| DiagnosticProjection::default())
}

pub fn render_rich_report_with_overrides<'a>(
    report: &'a DiagnosticReport,
    config: RenderConfig,
    overrides: impl Fn(&'a Diagnostic) -> DiagnosticProjection<'a>,
) -> String {
    let mut output = String::new();
    for diagnostic in report.diagnostics() {
        if !output.is_empty() {
            output.push('\n');
        }
        output.push_str(&render_rich_diagnostic(
            report,
            diagnostic,
            config,
            overrides(diagnostic),
        ));
    }
    output
}

fn render_rich_diagnostic<'a>(
    report: &DiagnosticReport,
    diagnostic: &'a Diagnostic,
    render_config: RenderConfig,
    render_override: DiagnosticProjection<'a>,
) -> String {
    let Some(anchor) = report.anchor_label(diagnostic) else {
        return render_summary(diagnostic, render_override);
    };
    let anchor_file = report
        .source(anchor.span.source())
        .expect("anchor label source must exist");

    let severity = render_override.severity_for(diagnostic);
    let kind = match severity {
        Severity::Error => ReportKind::Error,
        Severity::Warning => ReportKind::Warning,
    };
    let anchor_source = source_key(anchor_file);
    let anchor_range = anchor.span.start()..anchor.span.end();
    let mut builder = Report::build(kind, (anchor_source.clone(), anchor_range))
        .with_config(
            Config::default()
                .with_color(render_config.color)
                .with_index_type(IndexType::Byte),
        )
        .with_message(diagnostic.message());
    for label in diagnostic.labels() {
        let Some(file) = report.source(label.span.source()) else {
            continue;
        };
        let color = match label.style {
            LabelStyle::Primary => primary_label_color(severity),
            LabelStyle::Secondary => Color::Blue,
        };
        let mut ariadne_label =
            Label::new((source_key(file), label.span.start()..label.span.end())).with_color(color);
        if let Some(message) = &label.message {
            ariadne_label = ariadne_label.with_message(message);
        }
        builder = builder.with_label(ariadne_label);
    }

    for note in render_override.notes_for(diagnostic) {
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
        return render_summary(diagnostic, render_override);
    }
    String::from_utf8(bytes).unwrap_or_else(|_| render_summary(diagnostic, render_override))
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

fn render_summary(diagnostic: &Diagnostic, render_override: DiagnosticProjection<'_>) -> String {
    let severity = render_override.severity_for(diagnostic);
    let mut rendered = format!("{}: {}", severity.as_str(), diagnostic.message());
    for note in render_override.notes_for(diagnostic) {
        rendered.push_str("\nnote: ");
        rendered.push_str(&note);
    }
    if let Some(help) = diagnostic.help() {
        rendered.push_str("\nhelp: ");
        rendered.push_str(help);
    }
    rendered
}

fn primary_label_color(severity: Severity) -> Color {
    match severity {
        Severity::Error => Color::Red,
        Severity::Warning => Color::Yellow,
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

    use super::{RenderConfig, render_rich_report, render_rich_report_with_config};
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
    fn configured_color_uses_ariadne_color() {
        let report = report_with_source("let x = true;", |source| {
            Diagnostic::warning("careful")
                .with_primary_message(SourceSpan::new(source, 0, 3), "lint happened here")
        });

        let plain = render_rich_report(&report);
        let colored = render_rich_report_with_config(&report, RenderConfig { color: true });

        assert!(!plain.contains("\u{1b}["), "{plain}");
        assert!(colored.contains("\u{1b}["), "{colored:?}");
        assert!(colored.contains("Warning"), "{colored:?}");
        assert!(colored.contains("lint happened here"), "{colored:?}");
    }

    #[test]
    fn lint_code_does_not_lead_human_header() {
        let report = report_with_source("let x = true;", |source| {
            Diagnostic::warning("careful")
                .with_lint_code(
                    "anvyx",
                    "lint_id",
                    crate::lint::LintLevel::Warn,
                    crate::config::LintLevelOrigin::Default,
                )
                .with_primary_message(SourceSpan::new(source, 0, 3), "lint happened here")
        });

        let rendered = render_rich_report(&report);

        assert!(rendered.contains("Warning: careful"), "{rendered}");
        assert!(rendered.contains("lint happened here"), "{rendered}");
        assert!(
            rendered.contains("lint `lint_id` is on by default"),
            "{rendered}"
        );
        assert!(!rendered.contains("[lint_id] Warning"), "{rendered}");
        assert!(!rendered.contains("warning[lint_id]"), "{rendered}");
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
        assert!(rendered.contains("main.anv"), "{rendered}");
        assert!(rendered.contains("let x = true;"), "{rendered}");
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
            Diagnostic::error("Unexpected end of input")
                .with_primary_message(SourceSpan::empty(source, 2), "expected declaration")
        });

        let rendered = render_rich_report(&report);

        assert!(rendered.contains("Unexpected end of input"), "{rendered}");
        assert!(rendered.contains("main.anv"), "{rendered}");
        assert!(rendered.contains("fn"), "{rendered}");
        assert!(rendered.contains("expected declaration"), "{rendered}");
    }

    #[test]
    fn renders_non_ascii_byte_offsets() {
        let report = report_with_source("let café = 1;\nlet x = café;", |source| {
            Diagnostic::error("bad name")
                .with_primary_message(SourceSpan::new(source, 4, 9), "identifier is here")
        });

        let rendered = render_rich_report(&report);

        assert!(rendered.contains("bad name"), "{rendered}");
        assert!(rendered.contains("let café = 1;"), "{rendered}");
        assert!(rendered.contains("identifier is here"), "{rendered}");
    }

    #[test]
    fn renders_cross_file_secondary_labels() {
        let mut sources = SourceTable::default();
        let main = sources.add(SourceKind::Virtual, "main.anv", None, "use helper;\n");
        let helper = sources.add(SourceKind::Virtual, "helper.anv", None, "fn helper() {}\n");
        let diagnostic = Diagnostic::error("duplicate")
            .with_primary_message(SourceSpan::new(main, 0, 10), "imported here")
            .with_secondary_message(SourceSpan::new(helper, 0, 2), "defined here");
        let report = DiagnosticReport {
            sources,
            diagnostics: vec![diagnostic],
        };

        let rendered = render_rich_report(&report);

        assert!(rendered.contains("main.anv"), "{rendered}");
        assert!(rendered.contains("helper.anv"), "{rendered}");
        assert!(rendered.contains("use helper;"), "{rendered}");
        assert!(rendered.contains("fn helper() {}"), "{rendered}");
        assert!(rendered.contains("imported here"), "{rendered}");
        assert!(rendered.contains("defined here"), "{rendered}");
    }

    #[test]
    fn ignores_missing_source_ids() {
        let mut other_sources = SourceTable::default();
        let other = other_sources.add(SourceKind::Virtual, "other.anv", None, "let x = true;");
        let report = DiagnosticReport {
            sources: SourceTable::default(),
            diagnostics: vec![
                Diagnostic::error("bad bool")
                    .with_primary_message(SourceSpan::new(other, 8, 12), "missing source"),
            ],
        };

        let rendered = render_rich_report(&report);

        assert_eq!(rendered, "error: bad bool");
    }

    #[test]
    fn message_only_diagnostic_uses_plain_shape() {
        let report = report("", Diagnostic::error("provider failed"));
        let rendered = render_rich_report(&report);

        assert_eq!(rendered, "error: provider failed");
        assert!(!rendered.contains("main.anv"), "{rendered}");
    }
}
