use std::{collections::HashSet, io::IsTerminal, path::Path};

use anvyx_lang::{
    CompilationContext, Diagnostic, DiagnosticLabel, DiagnosticProjection, DiagnosticReport,
    DiagnosticSeverity, DiagnosticTag, FrontendConfig, LineCol, LintConfig, SourceFile,
    implemented_lints, render_rich_report_with_overrides,
};
use clap::ValueEnum;
use serde::Serialize;

#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
pub enum CheckOutputFormat {
    Text,
    Json,
}

pub fn render_lint_list() -> String {
    let mut lines = vec!["Lint                          Default  Group  Description".to_string()];
    for lint in implemented_lints() {
        let default = lint.default_level().to_string();
        lines.push(format!(
            "{:<29} {:<8} {:<6} {}",
            lint.name(),
            default,
            lint.group(),
            lint.description()
        ));
    }
    lines.join("\n")
}

pub fn cmd(
    file: &Path,
    lint: LintConfig,
    ctx: &CompilationContext,
    format: CheckOutputFormat,
    warnings_are_errors: bool,
) -> Result<(), String> {
    let output = match anvyx_project::check::check_path(file, frontend_config(lint, ctx)) {
        Ok(output) => output,
        Err(error) => return emit_host_error(error, format),
    };
    emit_check_output(&output, format, warnings_are_errors)
}

fn emit_host_error(error: String, format: CheckOutputFormat) -> Result<(), String> {
    if format == CheckOutputFormat::Json {
        println!(
            "{}",
            render_json_report(&message_report(error.as_str()), false)?
        );
    }
    Err(error)
}

fn emit_check_output(
    output: &anvyx_lang::CheckOutput,
    format: CheckOutputFormat,
    warnings_are_errors: bool,
) -> Result<(), String> {
    emit_diagnostic_report(&output.report, format, warnings_are_errors)?;
    if output.has_errors() {
        return Err(output.summary().to_string());
    }
    if warnings_are_errors && output.report.has_warnings() {
        return Err("warnings treated as errors".to_string());
    }
    Ok(())
}

pub(crate) fn frontend_config(lint: LintConfig, ctx: &CompilationContext) -> FrontendConfig {
    FrontendConfig {
        lint,
        context: ctx.clone(),
        ..FrontendConfig::default()
    }
}

fn emit_diagnostic_report(
    report: &DiagnosticReport,
    format: CheckOutputFormat,
    warnings_are_errors: bool,
) -> Result<(), String> {
    match format {
        CheckOutputFormat::Text => emit_text_report(report, warnings_are_errors),
        CheckOutputFormat::Json => println!("{}", render_json_report(report, warnings_are_errors)?),
    }
    Ok(())
}

pub(crate) fn emit_text_diagnostic_report(report: &DiagnosticReport) {
    emit_text_report(report, false);
}

const WARN_AS_ERROR_NOTE: &str = "warning promoted to error by --warn-as-error";

fn diagnostic_projection(
    diagnostic: &Diagnostic,
    warnings_are_errors: bool,
) -> DiagnosticProjection<'static> {
    if warnings_are_errors && diagnostic.severity() == DiagnosticSeverity::Warning {
        DiagnosticProjection {
            severity: Some(DiagnosticSeverity::Error),
            extra_note: Some(WARN_AS_ERROR_NOTE),
        }
    } else {
        DiagnosticProjection::default()
    }
}

fn emit_text_report(report: &DiagnosticReport, warnings_are_errors: bool) {
    if let Some(rendered) = render_text_report(report, warnings_are_errors) {
        eprint!("{rendered}");
    }
}

fn render_text_report(report: &DiagnosticReport, warnings_are_errors: bool) -> Option<String> {
    render_text_report_with_color(report, warnings_are_errors, std::io::stderr().is_terminal())
}

fn render_text_report_with_color(
    report: &DiagnosticReport,
    warnings_are_errors: bool,
    color: bool,
) -> Option<String> {
    let mut rendered = render_rich_report_with_overrides(
        report,
        anvyx_lang::RenderConfig { color },
        |diagnostic| diagnostic_projection(diagnostic, warnings_are_errors),
    );
    if rendered.is_empty() {
        return None;
    }
    if !rendered.ends_with('\n') {
        rendered.push('\n');
    }
    Some(rendered)
}

fn message_report(message: impl Into<String>) -> DiagnosticReport {
    DiagnosticReport::new(
        anvyx_lang::SourceTable::default(),
        vec![Diagnostic::error(message)],
    )
}

fn render_json_report(
    report: &DiagnosticReport,
    warnings_are_errors: bool,
) -> Result<String, String> {
    serde_json::to_string_pretty(&JsonReport::new(report, warnings_are_errors))
        .map_err(|error| format!("failed to serialize diagnostics: {error}"))
}

#[derive(Serialize)]
struct JsonReport<'a> {
    sources: Vec<JsonSource<'a>>,
    diagnostics: Vec<JsonDiagnostic<'a>>,
}

#[derive(Serialize)]
struct JsonSource<'a> {
    id: usize,
    label: &'a str,
    path: Option<String>,
}

#[derive(Serialize)]
struct JsonDiagnostic<'a> {
    severity: &'static str,
    source: Option<&'static str>,
    code: Option<&'a str>,
    tags: Vec<&'static str>,
    message: &'a str,
    labels: Vec<JsonLabel<'a>>,
    notes: Vec<String>,
    help: Option<&'a str>,
}

#[derive(Serialize)]
struct JsonLabel<'a> {
    style: &'static str,
    source_id: usize,
    start: usize,
    end: usize,
    line: u32,
    column: u32,
    end_line: u32,
    end_column: u32,
    excerpts: Vec<JsonExcerpt<'a>>,
    message: Option<&'a str>,
}

#[derive(Serialize)]
struct JsonExcerpt<'a> {
    line: u32,
    text: &'a str,
    highlight_start: u32,
    highlight_end: u32,
}

impl<'a> JsonReport<'a> {
    fn new(report: &'a DiagnosticReport, warnings_are_errors: bool) -> Self {
        let diagnostics = report
            .diagnostics()
            .iter()
            .map(|diagnostic| JsonDiagnostic::new(diagnostic, report, warnings_are_errors))
            .collect::<Vec<_>>();
        let source_ids = diagnostics
            .iter()
            .flat_map(|diagnostic| diagnostic.labels.iter().map(|label| label.source_id))
            .collect::<HashSet<_>>();
        Self {
            sources: report
                .sources
                .iter()
                .filter(|source| source_ids.contains(&source.id().index()))
                .map(|source| JsonSource {
                    id: source.id().index(),
                    label: source.label(),
                    path: source.path().map(|path| path.display().to_string()),
                })
                .collect(),
            diagnostics,
        }
    }
}

impl<'a> JsonDiagnostic<'a> {
    fn new(
        diagnostic: &'a Diagnostic,
        report: &'a DiagnosticReport,
        warnings_are_errors: bool,
    ) -> Self {
        let projection = diagnostic_projection(diagnostic, warnings_are_errors);
        let severity = projection.severity_for(diagnostic);
        let notes = projection.notes_for(diagnostic);
        Self {
            severity: severity.as_str(),
            source: diagnostic.code().map(|code| code.source),
            code: diagnostic.code().map(|code| code.code.as_str()),
            tags: diagnostic.tags().iter().copied().map(json_tag).collect(),
            message: diagnostic.message(),
            labels: diagnostic
                .labels()
                .iter()
                .filter_map(|label| JsonLabel::new(label, report))
                .collect(),
            notes,
            help: diagnostic.help(),
        }
    }
}

fn json_tag(tag: DiagnosticTag) -> &'static str {
    match tag {
        DiagnosticTag::Deprecated => "deprecated",
        DiagnosticTag::Unnecessary => "unnecessary",
    }
}

impl<'a> JsonLabel<'a> {
    fn new(label: &'a DiagnosticLabel, report: &'a DiagnosticReport) -> Option<Self> {
        let (source, start, end) = json_label_source(report, label)?;
        Some(Self {
            style: label.style.as_str(),
            source_id: label.span.source().index(),
            start: label.span.start(),
            end: label.span.end(),
            line: start.line + 1,
            column: start.column + 1,
            end_line: end.line + 1,
            end_column: end.column + 1,
            excerpts: json_excerpts(source, label, start),
            message: label.message.as_deref(),
        })
    }
}

fn json_label_source<'a>(
    report: &'a DiagnosticReport,
    label: &DiagnosticLabel,
) -> Option<(&'a SourceFile, LineCol, LineCol)> {
    if label.span.start() > label.span.end() {
        return None;
    }
    let source = report.source(label.span.source())?;
    let index = source.line_index();
    Some((
        source,
        index.byte_to_line_col(label.span.start())?,
        index.byte_to_line_col(label.span.end())?,
    ))
}

fn json_excerpts<'a>(
    source: &'a SourceFile,
    label: &DiagnosticLabel,
    start: LineCol,
) -> Vec<JsonExcerpt<'a>> {
    let index = source.line_index();
    let end_byte = if label.span.end() > label.span.start() {
        label.span.end() - 1
    } else {
        label.span.end()
    };
    let Some(last_covered) = index.byte_to_line_col(end_byte) else {
        return vec![];
    };

    let mut excerpts = vec![json_excerpt(source, label, start.line)];
    if last_covered.line != start.line {
        excerpts.push(json_excerpt(source, label, last_covered.line));
    }
    excerpts.into_iter().flatten().collect()
}

fn json_excerpt<'a>(
    source: &'a SourceFile,
    label: &DiagnosticLabel,
    line: u32,
) -> Option<JsonExcerpt<'a>> {
    let index = source.line_index();
    let line_start = index.line_start(line)?;
    let line_end = index.line_end(line)?;
    let highlight_start = label
        .span
        .start()
        .saturating_sub(line_start)
        .min(line_end - line_start);
    let highlight_end = label
        .span
        .end()
        .saturating_sub(line_start)
        .min(line_end - line_start);
    Some(JsonExcerpt {
        line: line + 1,
        text: source.line_text(line)?,
        highlight_start: highlight_start as u32,
        highlight_end: highlight_end.max(highlight_start) as u32,
    })
}
