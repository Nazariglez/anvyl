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
