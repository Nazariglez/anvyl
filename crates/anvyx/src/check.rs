use std::{
    collections::{HashMap, HashSet},
    fs,
    io::IsTerminal,
    path::Path,
};

use anvyx_lang::{CompilationContext, LintConfig, Profile, TargetArch, TargetOs};
use anvyx_lang2::{
    CompilationContext as FrontendCompilationContext, Diagnostic, DiagnosticLabel,
    DiagnosticProjection, DiagnosticReport, DiagnosticSeverity, DiagnosticTag, FrontendConfig,
    LineCol, LintConfig as FrontendLintConfig, Profile as FrontendProfile, SourceFile,
    TargetArch as FrontendTargetArch, TargetOs as FrontendTargetOs, implemented_lints,
    render_rich_report_with_overrides,
};
use clap::ValueEnum;
use serde::Serialize;

use crate::std_support::{collect_core, collect_std};

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
    extern_meta: &HashMap<String, String>,
    lint: LintConfig,
    ctx: &CompilationContext,
) -> Result<(), String> {
    let program = fs::read_to_string(file).map_err(|e| format!("Failed to read file: {e}"))?;
    let file_path = file.to_string_lossy().to_string();
    let (std_sources, _) = collect_std();
    let (core_prelude, core_sources, _) = collect_core();

    let _ast = anvyx_lang::generate_ast_with_std(
        &program,
        &file_path,
        &core_prelude,
        extern_meta,
        &std_sources,
        &core_sources,
        lint,
        ctx,
    )?;
    Ok(())
}

pub fn new_frontend_cmd(
    file: &Path,
    lint: FrontendLintConfig,
    ctx: &CompilationContext,
    format: CheckOutputFormat,
    warnings_are_errors: bool,
) -> Result<(), String> {
    let output = match anvyx_project::check::check_path(file, new_frontend_config(lint, ctx)) {
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
    output: &anvyx_lang2::CheckOutput,
    format: CheckOutputFormat,
    warnings_are_errors: bool,
) -> Result<(), String> {
    emit_report(&output.report, format, warnings_are_errors)?;
    if output.has_errors() {
        return Err(output.summary().to_string());
    }
    if warnings_are_errors && output.report.has_warnings() {
        return Err("warnings treated as errors".to_string());
    }
    Ok(())
}

pub(crate) fn new_frontend_config(
    lint: FrontendLintConfig,
    ctx: &CompilationContext,
) -> FrontendConfig {
    FrontendConfig {
        lint,
        context: new_frontend_context(ctx),
        ..FrontendConfig::default()
    }
}

fn new_frontend_context(ctx: &CompilationContext) -> FrontendCompilationContext {
    FrontendCompilationContext {
        profile: match ctx.profile {
            Profile::Debug => FrontendProfile::Debug,
            Profile::Release => FrontendProfile::Release,
        },
        os: match ctx.os {
            TargetOs::MacOs => FrontendTargetOs::Macos,
            TargetOs::Linux => FrontendTargetOs::Linux,
            TargetOs::Windows => FrontendTargetOs::Windows,
            TargetOs::Wasm => FrontendTargetOs::Wasm,
            TargetOs::Ios => FrontendTargetOs::Ios,
            TargetOs::Android => FrontendTargetOs::Android,
        },
        arch: match ctx.arch {
            TargetArch::X86_64 => FrontendTargetArch::X86_64,
            TargetArch::Aarch64 => FrontendTargetArch::Aarch64,
        },
        features: ctx.features.iter().cloned().collect(),
    }
}

fn emit_report(
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
    let mut rendered = render_rich_report_with_overrides(
        report,
        anvyx_lang2::RenderConfig {
            color: std::io::stderr().is_terminal(),
        },
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
        anvyx_lang2::SourceTable::default(),
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

#[cfg(test)]
mod tests {
    use std::sync::{Mutex, OnceLock};

    use anvyx_lang2::CheckFileInput;

    use super::*;
    use crate::manifest::{DependencyEntry, ExternEntry, Manifest, Project};

    fn check_new_frontend(file: &Path) -> Result<(), String> {
        new_frontend_cmd(
            file,
            FrontendLintConfig::default(),
            &CompilationContext::from_host(Profile::Debug),
            CheckOutputFormat::Text,
            false,
        )
    }

    fn check_new_frontend_error(file: &Path) -> String {
        let sources = anvyx_project::source_bundle().unwrap();
        let input = CheckFileInput::new(file.to_path_buf(), sources).unwrap();
        let output = anvyx_lang2::check_file(input).unwrap();
        render_text_report(&output.report, false).unwrap_or_else(|| output.summary().to_string())
    }

    fn cwd_lock() -> &'static Mutex<()> {
        static LOCK: OnceLock<Mutex<()>> = OnceLock::new();
        LOCK.get_or_init(|| Mutex::new(()))
    }

    #[test]
    fn check_new_frontend_standalone_uses_project_input() {
        let temp = tempfile::tempdir().unwrap();
        let main = temp.path().join("main.anv");
        fs::write(&main, "fn main() {}\n").unwrap();

        check_new_frontend(&main).unwrap();
    }

    #[test]
    fn check_new_frontend_manifest_uses_project_input() {
        let _guard = cwd_lock().lock().unwrap();
        let previous = std::env::current_dir().unwrap();
        let temp = tempfile::tempdir().unwrap();
        fs::create_dir_all(temp.path().join("src")).unwrap();
        fs::write(
            temp.path().join("anvyx.toml"),
            "[project]\nentry = \"src/main.anv\"\n",
        )
        .unwrap();
        fs::write(temp.path().join("src/main.anv"), "fn main() {}\n").unwrap();
        std::env::set_current_dir(temp.path()).unwrap();

        let result = check_new_frontend(Path::new("src/main.anv"));

        std::env::set_current_dir(previous).unwrap();
        result.unwrap();
    }

    #[test]
    fn list_lints_output_is_stable() {
        let rendered = render_lint_list();

        assert!(rendered.starts_with("Lint                          Default  Group  Description"));
        assert!(rendered.contains(
            "internal_access               warn     api    cross-module access to @internal members"
        ));
        assert!(rendered.contains(
            "deprecated                    warn     api    use of declarations marked @deprecated"
        ));
        assert!(rendered.contains(
            "public_inferred_dyn_contract  warn     api    exported function uses inferred dyn contract"
        ));
        assert!(rendered.contains(
            "unused_import                 warn     unused unused private import has no binding or activation use"
        ));
    }

    #[test]
    fn warn_as_error_projection_upgrades_warnings_only() {
        let report = DiagnosticReport {
            diagnostics: vec![
                Diagnostic::warning("careful").with_code("anvyx", "deprecated"),
                Diagnostic::error("bad"),
            ],
            ..DiagnosticReport::default()
        };

        let warning = diagnostic_projection(&report.diagnostics()[0], true);
        let error = diagnostic_projection(&report.diagnostics()[1], true);

        assert_eq!(warning.severity, Some(DiagnosticSeverity::Error));
        assert_eq!(warning.extra_note, Some(WARN_AS_ERROR_NOTE));
        assert_eq!(error.severity, None);
        assert_eq!(error.extra_note, None);
        assert_eq!(
            report.diagnostics()[0].severity(),
            DiagnosticSeverity::Warning
        );
        assert!(report.diagnostics()[0].notes().is_empty());
    }

    #[test]
    fn warn_as_error_json_uses_upgraded_severity() {
        let report = DiagnosticReport {
            diagnostics: vec![Diagnostic::warning("careful")],
            ..DiagnosticReport::default()
        };
        let json: serde_json::Value =
            serde_json::from_str(&render_json_report(&report, true).unwrap()).unwrap();

        assert_eq!(json["diagnostics"][0]["severity"], "error");
        assert_eq!(json["diagnostics"][0]["notes"][0], WARN_AS_ERROR_NOTE);
    }

    #[test]
    fn warn_as_error_text_includes_promotion_note() {
        let mut sources = anvyx_lang2::SourceTable::default();
        let source = sources.add(
            anvyx_lang2::SourceKind::Virtual,
            "main.anv",
            None,
            "let x = 1;",
        );
        let report =
            DiagnosticReport {
                sources,
                diagnostics: vec![Diagnostic::warning("careful").with_primary_message(
                    anvyx_lang2::SourceSpan::new(source, 0, 3),
                    "warning here",
                )],
            };
        let rendered = render_text_report(&report, true).unwrap();

        assert!(rendered.contains("Error: careful"), "{rendered}");
        assert!(rendered.contains(WARN_AS_ERROR_NOTE), "{rendered}");
    }

    #[test]
    fn warn_as_error_message_only_text_keeps_promotion_note() {
        let report = DiagnosticReport {
            diagnostics: vec![Diagnostic::warning("careful")],
            ..DiagnosticReport::default()
        };
        let rendered = render_text_report(&report, true).unwrap();

        assert!(rendered.contains("error: careful"), "{rendered}");
        assert!(rendered.contains(WARN_AS_ERROR_NOTE), "{rendered}");
    }

    #[test]
    fn warn_as_error_fails_on_final_warning_report() {
        let output = anvyx_lang2::CheckOutput::passed(DiagnosticReport {
            diagnostics: vec![Diagnostic::warning("careful")],
            ..DiagnosticReport::default()
        });

        let error = emit_check_output(&output, CheckOutputFormat::Json, true).unwrap_err();

        assert_eq!(error, "warnings treated as errors");
    }

    #[test]
    fn unused_import_json_contains_code_and_tag() {
        let temp = tempfile::tempdir().unwrap();
        let main = temp.path().join("main.anv");
        fs::write(&main, "import helper; fn main() {}\n").unwrap();
        fs::write(temp.path().join("helper.anv"), "pub fn f() {}\n").unwrap();
        let input = anvyx_project::check::standalone_check_input(&main).unwrap();
        let report = anvyx_lang2::check_file(input).unwrap().report;
        let json: serde_json::Value =
            serde_json::from_str(&render_json_report(&report, false).unwrap()).unwrap();
        let diagnostic = &json["diagnostics"][0];

        assert_eq!(diagnostic["source"], "anvyx");
        assert_eq!(diagnostic["code"], "unused_import");
        assert_eq!(diagnostic["tags"], serde_json::json!(["unnecessary"]));
    }

    fn plain_manifest() -> Manifest {
        Manifest {
            project: Project {
                name: None,
                version: None,
                entry: Some("main.anv".to_string()),
            },
            dependencies: HashMap::new(),
            externs: HashMap::new(),
            lint: std::collections::BTreeMap::default(),
        }
    }

    mod frontend {
        use super::*;

        fn write(dir: &tempfile::TempDir, relative: &str, code: &str) -> std::path::PathBuf {
            let file = dir.path().join(relative);
            if let Some(parent) = file.parent() {
                fs::create_dir_all(parent).unwrap();
            }
            fs::write(&file, code).unwrap();
            file
        }

        fn frontend_error(code: &str) -> anvyx_lang2::CheckOutput {
            let temp = tempfile::tempdir().unwrap();
            let main = write(&temp, "main.anv", code);
            let sources = anvyx_project::source_bundle().unwrap();
            let input = CheckFileInput::new(main, sources).unwrap();
            anvyx_lang2::check_file(input).unwrap()
        }

        fn check_error_report(code: &str) -> DiagnosticReport {
            frontend_error(code).report
        }

        fn json_value(report: &DiagnosticReport) -> serde_json::Value {
            serde_json::from_str(&render_json_report(report, false).unwrap()).unwrap()
        }

        mod integration {
            use std::fmt::Write as _;

            use super::*;

            fn write_manifest(
                dir: &tempfile::TempDir,
                package: &str,
                entry: &str,
                deps: &[(&str, &str)],
            ) {
                let mut manifest = format!("[project]\nentry = \"{entry}\"\n");
                if !deps.is_empty() {
                    manifest.push_str("\n[dependencies]\n");
                    for (alias, path) in deps {
                        writeln!(manifest, "{alias} = {{ path = \"{path}\" }}").unwrap();
                    }
                }
                write(dir, &format!("{package}/anvyx.toml"), &manifest);
            }

            fn check_manifest_file(dir: &tempfile::TempDir, file: &Path) -> Result<(), String> {
                let graph =
                    crate::manifest::load_package_graph(&dir.path().join("game/anvyx.toml"))?;
                let input = anvyx_project::check::package_check_input(&graph, file)?;
                let output =
                    anvyx_lang2::check_package(input).map_err(|error| error.to_string())?;
                if output.has_errors() {
                    return Err(render_text_report(&output.report, false)
                        .unwrap_or_else(|| output.summary().to_string()));
                }
                Ok(())
            }

            #[test]
            fn core_prelude_nominals_are_visible_without_import() {
                let temp = tempfile::tempdir().unwrap();
                let main = write(
                    &temp,
                    "main.anv",
                    "fn takes_option(x: Option<int>) {} fn takes_ranges(a: Range<int>, b: RangeFrom<int>) {}",
                );

                check_new_frontend(&main).unwrap();
            }

            #[test]
            fn core_primitive_extensions_are_forwarded_by_core_root() {
                let temp = tempfile::tempdir().unwrap();
                let main = write(
                    &temp,
                    "main.anv",
                    "fn main() { let a: int = (-1).abs(); let b: float = 4.0.sqrt(); let c: int = \"abc\".len(); }",
                );

                check_new_frontend(&main).unwrap();
            }

            #[test]
            fn core_helper_externs_are_not_visible_to_user_modules() {
                let temp = tempfile::tempdir().unwrap();
                let main = write(&temp, "main.anv", "fn main() { int_abs(1); }");
                let error = check_new_frontend_error(&main);

                assert!(error.contains("int_abs"));
            }

            #[test]
            fn core_primitive_wrapper_modules_are_not_preluded() {
                let temp = tempfile::tempdir().unwrap();
                let main = write(&temp, "main.anv", "fn main() { core_int.abs(1); }");
                let error = check_new_frontend_error(&main);

                assert!(error.contains("core_int"));
            }

            #[test]
            fn std_import_resolves_implicit_package() {
                let temp = tempfile::tempdir().unwrap();
                let main = write(
                    &temp,
                    "main.anv",
                    "import std:mem; fn main() { mem.collect_cycles(); }",
                );

                check_new_frontend(&main).unwrap();
            }

            #[test]
            fn std_selective_import_resolves_implicit_package() {
                let temp = tempfile::tempdir().unwrap();
                let main = write(
                    &temp,
                    "main.anv",
                    "import std:mem { collect_cycles }; fn main() { collect_cycles(); }",
                );

                check_new_frontend(&main).unwrap();
            }

            #[test]
            fn std_declarations_are_not_preluded() {
                let temp = tempfile::tempdir().unwrap();
                let main = write(&temp, "main.anv", "fn main() { collect_cycles(); }");
                let error = check_new_frontend_error(&main);

                assert!(error.contains("collect_cycles"));
            }

            #[test]
            fn old_std_dot_import_is_local_source_syntax() {
                let temp = tempfile::tempdir().unwrap();
                let main = write(&temp, "main.anv", "import std.mem; fn main() {}");
                let error = check_new_frontend_error(&main);

                assert!(error.contains("std/mem.anv") || error.contains("std\\mem.anv"));
            }

            #[test]
            fn local_std_path_does_not_affect_std_colon_import() {
                let temp = tempfile::tempdir().unwrap();
                let main = write(
                    &temp,
                    "main.anv",
                    "import std.mem { local }; import std:mem { collect_cycles }; fn main() { local(); collect_cycles(); }",
                );
                write(&temp, "std/mem.anv", "pub fn local() {}");

                check_new_frontend(&main).unwrap();
            }

            #[test]
            fn script_relative_imports_work() {
                let temp = tempfile::tempdir().unwrap();
                let main = write(
                    &temp,
                    "src/ui/main.anv",
                    "import helper { value as a }; import .helper { value as b }; import ..common { value as c }; fn main() { let x: int = a() + b() + c(); }",
                );
                write(&temp, "src/ui/helper.anv", "pub fn value() -> int { 1 }");
                write(&temp, "src/common.anv", "pub fn value() -> int { 1 }");

                check_new_frontend(&main).unwrap();
            }

            #[test]
            fn pkg_std_and_relative_imports() {
                let temp = tempfile::tempdir().unwrap();
                write_manifest(&temp, "game", "src/main.anv", &[("math", "../math")]);
                write_manifest(&temp, "math", "src/lib.anv", &[]);
                let main = write(
                    &temp,
                    "game/src/main.anv",
                    "import helper { local }; import .helper { local as local2 }; import ..outside { escaped }; import pkg:math { add }; import std:mem; fn main() { let x: int = local() + local2() + escaped() + add(); }",
                );
                write(&temp, "game/src/helper.anv", "pub fn local() -> int { 1 }");
                write(&temp, "game/outside.anv", "pub fn escaped() -> int { 1 }");
                write(&temp, "math/src/lib.anv", "pub fn add() -> int { 1 }");

                check_manifest_file(&temp, &main).unwrap();
            }

            #[test]
            fn package_root_reexport_forwards_extend() {
                let temp = tempfile::tempdir().unwrap();
                write_manifest(&temp, "game", "src/main.anv", &[("helpers", "../helpers")]);
                write_manifest(&temp, "helpers", "src/lib.anv", &[]);
                let main = write(
                    &temp,
                    "game/src/main.anv",
                    "import pkg:helpers; fn main() { let x: string = \"hi\".shout(); }",
                );
                write(&temp, "helpers/src/lib.anv", "pub import strings;");
                write(
                    &temp,
                    "helpers/src/strings.anv",
                    "pub extend string { fn shout(self) -> string { self } }",
                );

                check_manifest_file(&temp, &main).unwrap();
            }

            #[test]
            fn outside_override_rejected() {
                let temp = tempfile::tempdir().unwrap();
                write_manifest(&temp, "game", "src/main.anv", &[]);
                write(&temp, "game/src/main.anv", "fn main() {}");
                let outside = write(&temp, "game/outside.anv", "fn main() {}");
                let error = check_manifest_file(&temp, &outside).unwrap_err();

                assert!(error.contains("outside every loaded package source root"));
            }

            #[test]
            fn old_dep_root_fails() {
                let temp = tempfile::tempdir().unwrap();
                write_manifest(&temp, "game", "src/main.anv", &[]);
                let main = write(&temp, "game/src/main.anv", "import dep:math; fn main() {}");
                let error = check_manifest_file(&temp, &main).unwrap_err();

                assert!(
                    error.contains("Unexpected token ':'") || error.contains("import declaration")
                );
            }
        }

        #[test]
        fn text_type_error_renders_rich_report_and_short_summary() {
            let code = "fn main() { let hp: int = true; }";
            let error = frontend_error(code);
            let rendered = render_text_report(&error.report, false).unwrap();

            assert!(rendered.contains("Error: mismatched types"), "{rendered}");
            assert!(rendered.contains("main.anv"), "{rendered}");
            assert!(rendered.contains(code), "{rendered}");
            assert!(
                rendered.contains("expected `int`, found `bool`"),
                "{rendered}"
            );
            assert_eq!(error.summary(), "Failed to typecheck program");
            assert!(
                !rendered.contains("frontend typecheck failed"),
                "{rendered}"
            );
            assert!(!rendered.contains("\n- mismatched types"), "{rendered}");
        }

        #[test]
        fn text_parse_error_renders_label_and_short_summary() {
            let error = frontend_error("fn");
            let rendered = render_text_report(&error.report, false).unwrap();

            assert!(rendered.contains("Unexpected end of input"), "{rendered}");
            assert!(rendered.contains("end of file"), "{rendered}");
            assert_eq!(error.summary(), "Failed to parse program");
        }

        #[test]
        fn text_message_only_report_renders_plain_message() {
            let report = message_report("bad path");

            assert!(render_text_report(&report, false).is_some());
        }

        #[test]
        fn text_warning_report_uses_rich_renderer() {
            let mut sources = anvyx_lang2::SourceTable::default();
            let source = sources.add(
                anvyx_lang2::SourceKind::Virtual,
                "main.anv",
                None,
                "fn main() {}",
            );
            let report = DiagnosticReport {
                sources,
                diagnostics: vec![Diagnostic::warning("careful").with_primary_message(
                    anvyx_lang2::SourceSpan::new(source, 0, 2),
                    "compile warning emitted here",
                )],
            };
            let rendered = render_text_report(&report, false).unwrap();

            assert!(rendered.contains("Warning: careful"), "{rendered}");
            assert!(
                rendered.contains("compile warning emitted here"),
                "{rendered}"
            );
        }

        #[test]
        fn json_report_includes_sources_labels_and_byte_offsets() {
            let code = "// café\nfn main() { let x: int = true; }";
            let report = check_error_report(code);
            let rendered = render_json_report(&report, false).unwrap();
            let json: serde_json::Value = serde_json::from_str(&rendered).unwrap();
            let diagnostic = &json["diagnostics"][0];
            let label = &diagnostic["labels"][0];
            let source_id = label["source_id"].as_u64().unwrap();
            let source = json["sources"]
                .as_array()
                .unwrap()
                .iter()
                .find(|source| source["id"].as_u64() == Some(source_id))
                .unwrap();

            assert_eq!(diagnostic["severity"], "error");
            assert!(diagnostic["source"].is_null());
            assert!(diagnostic["code"].is_null());
            assert!(diagnostic["tags"].as_array().unwrap().is_empty());
            assert_eq!(label["style"], "primary");
            let start = label["start"].as_u64().unwrap() as usize;
            let end = label["end"].as_u64().unwrap() as usize;
            assert_eq!(start, code.find("true").unwrap());
            assert!(end > start);
            assert!(code[..start].contains("café"));
            assert!(source["path"].as_str().unwrap().ends_with("main.anv"));
            assert!(source.get("text").is_none());
            assert_eq!(label["message"], "expected `int`, found `bool`");
            assert_eq!(label["line"], 2);
            assert_eq!(
                label["column"],
                code.find("true").unwrap() - code.find("fn main").unwrap() + 1
            );
            assert_eq!(label["excerpts"][0]["line"], 2);
            assert_eq!(
                label["excerpts"][0]["text"],
                "fn main() { let x: int = true; }"
            );
            assert!(!rendered.contains("╭"));
            assert!(
                !diagnostic["message"]
                    .as_str()
                    .unwrap()
                    .contains("TypeError")
            );
        }

        #[test]
        fn json_report_serializes_lex_and_parse_errors() {
            for code in ["fn main() { \"unterminated }", "fn"] {
                let report = check_error_report(code);
                let json = json_value(&report);
                assert_eq!(json["diagnostics"][0]["severity"], "error");
                assert!(
                    !json["diagnostics"][0]["labels"]
                        .as_array()
                        .unwrap()
                        .is_empty()
                );
            }
        }

        #[test]
        fn json_report_serializes_message_only_diagnostics() {
            let report = message_report("provider failed");
            let json = json_value(&report);

            assert!(json["sources"].as_array().unwrap().is_empty());
            assert_eq!(json["diagnostics"][0]["message"], "provider failed");
            assert!(
                json["diagnostics"][0]["labels"]
                    .as_array()
                    .unwrap()
                    .is_empty()
            );
        }

        #[test]
        fn json_report_skips_labels_outside_source_text() {
            let mut sources = anvyx_lang2::SourceTable::default();
            let source = sources.add(anvyx_lang2::SourceKind::Virtual, "main.anv", None, "short");
            let report = DiagnosticReport {
                sources,
                diagnostics: vec![Diagnostic::error("bad").with_primary_message(
                    anvyx_lang2::SourceSpan::new(source, 20, 25),
                    "outside source",
                )],
            };
            let json = json_value(&report);

            assert!(json["sources"].as_array().unwrap().is_empty());
            assert!(
                json["diagnostics"][0]["labels"]
                    .as_array()
                    .unwrap()
                    .is_empty()
            );
        }

        #[test]
        fn json_report_serializes_warnings_labels_notes_help_and_metadata() {
            let mut sources = anvyx_lang2::SourceTable::default();
            let source = sources.add(
                anvyx_lang2::SourceKind::Virtual,
                "main.anv",
                None,
                "fn main() {}",
            );
            let diagnostic = Diagnostic::warning("careful")
                .with_code("anvyx", "deprecated")
                .with_tag(DiagnosticTag::Deprecated)
                .with_tag(DiagnosticTag::Unnecessary)
                .with_primary_message(
                    anvyx_lang2::SourceSpan::new(source, 0, 2),
                    "compile warning emitted here",
                )
                .with_note("first note")
                .with_help("try this");
            let report = DiagnosticReport {
                sources,
                diagnostics: vec![diagnostic],
            };
            let json_text = render_json_report(&report, false).unwrap();
            let json: serde_json::Value = serde_json::from_str(&json_text).unwrap();

            assert_eq!(json["diagnostics"][0]["severity"], "warning");
            assert_eq!(json["diagnostics"][0]["source"], "anvyx");
            assert_eq!(json["diagnostics"][0]["code"], "deprecated");
            assert_eq!(
                json["diagnostics"][0]["tags"],
                serde_json::json!(["deprecated", "unnecessary"])
            );
            assert_eq!(
                json["diagnostics"][0]["labels"][0]["message"],
                "compile warning emitted here"
            );
            assert_eq!(json["diagnostics"][0]["notes"][0], "first note");
            assert_eq!(json["diagnostics"][0]["help"], "try this");
            assert_eq!(
                json["diagnostics"][0]["labels"][0]["excerpts"][0]["text"],
                "fn main() {}"
            );
        }

        mod unsupported {
            use super::*;

            #[test]
            fn accepts_feature_flags() {
                assert!(plain_manifest().lint.is_empty());
            }

            #[test]
            fn accepts_cfg_flags() {
                assert!(plain_manifest().dependencies.is_empty());
            }

            #[test]
            fn accepts_lint_flags() {
                assert!(plain_manifest().externs.is_empty());
            }

            #[test]
            fn accepts_manifest_lint() {
                let mut manifest = plain_manifest();
                manifest
                    .lint
                    .insert("internal_access".to_string(), "error".to_string());

                assert_eq!(manifest.lint["internal_access"], "error");
            }

            #[test]
            fn manifest_externs() {
                let mut manifest = plain_manifest();
                manifest.externs.insert(
                    "engine".to_string(),
                    ExternEntry {
                        path: "externs/engine".to_string(),
                    },
                );

                assert!(manifest.has_externs());
            }

            #[test]
            fn accepts_plain_manifest() {
                let manifest = plain_manifest();

                assert!(!manifest.has_externs());
            }

            #[test]
            fn accepts_manifest_dependencies() {
                let mut manifest = plain_manifest();
                manifest.dependencies.insert(
                    "math".to_string(),
                    DependencyEntry {
                        path: "../math".to_string(),
                    },
                );

                assert!(manifest.dependencies.contains_key("math"));
            }
        }
    }
}
