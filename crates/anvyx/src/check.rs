use std::{collections::HashMap, fs, path::Path};

use anvyx_lang::{CompilationContext, LintConfig, Profile, TargetArch, TargetOs};
use anvyx_lang2::{
    CheckError as FrontendCheckError, CompilationContext as FrontendCompilationContext, Diagnostic,
    DiagnosticLabel, DiagnosticReport, DiagnosticSeverity, DiagnosticTag, FrontendConfig,
    LintConfig as FrontendLintConfig, Profile as FrontendProfile, TargetArch as FrontendTargetArch,
    TargetOs as FrontendTargetOs, implemented_lints, render_rich_report,
};
use clap::ValueEnum;
use serde::Serialize;

use crate::{
    manifest::Manifest,
    std_support::{collect_core, collect_std},
};

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
    let result = anvyx_project::check::check_path(file, new_frontend_config(lint, ctx))?;

    match result {
        Ok(ok) => emit_final_report(&ok.report, format, warnings_are_errors),
        Err(error) => {
            emit_error_report(&error, format, warnings_are_errors)?;
            if error.report().is_some() {
                Err(error.summary())
            } else {
                Err(error.to_string())
            }
        }
    }
}

fn new_frontend_config(lint: FrontendLintConfig, ctx: &CompilationContext) -> FrontendConfig {
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

pub fn reject_new_frontend_inputs(manifest: Option<&Manifest>) -> Result<(), String> {
    anvyx_project::manifest::reject_clean_frontend_inputs(manifest)
}

fn emit_final_report(
    report: &DiagnosticReport,
    format: CheckOutputFormat,
    warnings_are_errors: bool,
) -> Result<(), String> {
    let has_warning = report
        .diagnostics()
        .iter()
        .any(|diagnostic| diagnostic.severity() == DiagnosticSeverity::Warning);
    let report = report_for_cli(report, warnings_are_errors);
    emit_report(&report, format)?;
    if warnings_are_errors && has_warning {
        return Err("warnings treated as errors".to_string());
    }
    Ok(())
}

fn emit_report(report: &DiagnosticReport, format: CheckOutputFormat) -> Result<(), String> {
    match format {
        CheckOutputFormat::Text => emit_text_report(report),
        CheckOutputFormat::Json => println!("{}", render_json_report(report)?),
    }
    Ok(())
}

fn emit_error_report(
    error: &FrontendCheckError,
    format: CheckOutputFormat,
    warnings_are_errors: bool,
) -> Result<(), String> {
    match (format, error.report()) {
        (CheckOutputFormat::Text, Some(report)) => {
            emit_text_report(&report_for_cli(report, warnings_are_errors));
        }
        (CheckOutputFormat::Text, None) => {}
        (CheckOutputFormat::Json, Some(report)) => println!(
            "{}",
            render_json_report(&report_for_cli(report, warnings_are_errors))?
        ),
        (CheckOutputFormat::Json, None) => {
            println!(
                "{}",
                render_json_report(&message_report(error.to_string()))?
            );
        }
    }
    Ok(())
}

fn report_for_cli(report: &DiagnosticReport, warnings_are_errors: bool) -> DiagnosticReport {
    if !warnings_are_errors {
        return report.clone();
    }
    let mut report = report.clone();
    for diagnostic in &mut report.diagnostics {
        if diagnostic.severity() == DiagnosticSeverity::Warning {
            *diagnostic = diagnostic.clone().with_severity(DiagnosticSeverity::Error);
        }
    }
    report
}

fn emit_text_report(report: &DiagnosticReport) {
    if let Some(rendered) = render_text_report(report) {
        eprint!("{rendered}");
    }
}

fn render_text_report(report: &DiagnosticReport) -> Option<String> {
    let mut rendered = render_rich_report(report);
    if rendered.is_empty() {
        return None;
    }
    if !rendered.ends_with('\n') {
        rendered.push('\n');
    }
    Some(rendered)
}

fn message_report(message: String) -> DiagnosticReport {
    DiagnosticReport {
        diagnostics: vec![Diagnostic::error(message)],
        ..DiagnosticReport::default()
    }
}

fn render_json_report(report: &DiagnosticReport) -> Result<String, String> {
    serde_json::to_string_pretty(&JsonReport::from(report))
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
    notes: &'a [String],
    help: Option<&'a str>,
}

#[derive(Serialize)]
struct JsonLabel<'a> {
    style: &'static str,
    source_id: usize,
    start: usize,
    end: usize,
    message: Option<&'a str>,
}

impl<'a> From<&'a DiagnosticReport> for JsonReport<'a> {
    fn from(report: &'a DiagnosticReport) -> Self {
        Self {
            sources: report
                .sources
                .iter()
                .map(|source| JsonSource {
                    id: source.id().index(),
                    label: source.label(),
                    path: source.path().map(|path| path.display().to_string()),
                })
                .collect(),
            diagnostics: report
                .diagnostics()
                .iter()
                .map(JsonDiagnostic::from)
                .collect(),
        }
    }
}

impl<'a> From<&'a Diagnostic> for JsonDiagnostic<'a> {
    fn from(diagnostic: &'a Diagnostic) -> Self {
        Self {
            severity: diagnostic.severity().as_str(),
            source: diagnostic.code().map(|code| code.source),
            code: diagnostic.code().map(|code| code.code.as_str()),
            tags: diagnostic.tags().iter().copied().map(json_tag).collect(),
            message: diagnostic.message(),
            labels: diagnostic.labels().iter().map(JsonLabel::from).collect(),
            notes: diagnostic.notes(),
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

impl<'a> From<&'a DiagnosticLabel> for JsonLabel<'a> {
    fn from(label: &'a DiagnosticLabel) -> Self {
        Self {
            style: label.style.as_str(),
            source_id: label.span.source().index(),
            start: label.span.start(),
            end: label.span.end(),
            message: label.message.as_deref(),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::sync::{Mutex, OnceLock};

    use anvyx_lang2::CheckFileInput;

    use super::*;
    use crate::manifest::{DependencyEntry, ExternEntry, Project};

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
        anvyx_lang2::check_file(input).unwrap_err().to_string()
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

        let projected = report_for_cli(&report, true);

        assert_eq!(
            projected.diagnostics()[0].severity(),
            DiagnosticSeverity::Error
        );
        assert_eq!(
            projected.diagnostics()[0].code().unwrap().code,
            "deprecated"
        );
        assert_eq!(
            projected.diagnostics()[1].severity(),
            DiagnosticSeverity::Error
        );
        assert_eq!(
            report.diagnostics()[0].severity(),
            DiagnosticSeverity::Warning
        );
    }

    #[test]
    fn warn_as_error_json_uses_upgraded_severity() {
        let report = DiagnosticReport {
            diagnostics: vec![Diagnostic::warning("careful")],
            ..DiagnosticReport::default()
        };
        let projected = report_for_cli(&report, true);
        let json: serde_json::Value =
            serde_json::from_str(&render_json_report(&projected).unwrap()).unwrap();

        assert_eq!(json["diagnostics"][0]["severity"], "error");
    }

    #[test]
    fn warn_as_error_fails_on_final_warning_report() {
        let report = DiagnosticReport {
            diagnostics: vec![Diagnostic::warning("careful")],
            ..DiagnosticReport::default()
        };

        let error = emit_final_report(&report, CheckOutputFormat::Json, true).unwrap_err();

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
            serde_json::from_str(&render_json_report(&report).unwrap()).unwrap();
        let diagnostic = &json["diagnostics"][0];

        assert_eq!(diagnostic["source"], "anvyx");
        assert_eq!(diagnostic["code"], "unused_import");
        assert_eq!(diagnostic["tags"], serde_json::json!(["unnecessary"]));
    }

    #[test]
    fn unused_import_error_level_fails_check() {
        let temp = tempfile::tempdir().unwrap();
        let main = temp.path().join("main.anv");
        fs::write(&main, "import helper; fn main() {}\n").unwrap();
        fs::write(temp.path().join("helper.anv"), "pub fn f() {}\n").unwrap();
        let mut lint = FrontendLintConfig::default();
        lint.set(
            anvyx_lang2::LintId::UnusedImport,
            anvyx_lang2::LintLevel::Error,
        );

        let error = new_frontend_cmd(
            &main,
            lint,
            &CompilationContext::from_host(Profile::Debug),
            CheckOutputFormat::Json,
            false,
        )
        .unwrap_err();

        assert!(error.contains("typecheck"), "{error}");
    }

    fn plain_manifest() -> Manifest {
        Manifest {
            project: Project {
                name: None,
                entry: Some("main.anv".to_string()),
            },
            dependencies: HashMap::new(),
            externs: HashMap::new(),
            lint: Default::default(),
        }
    }

    fn unsupported_error(manifest: Option<&Manifest>) -> String {
        reject_new_frontend_inputs(manifest).expect_err("input should be unsupported")
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

        fn frontend_error(code: &str) -> FrontendCheckError {
            let temp = tempfile::tempdir().unwrap();
            let main = write(&temp, "main.anv", code);
            let sources = anvyx_project::source_bundle().unwrap();
            let input = CheckFileInput::new(main, sources).unwrap();
            anvyx_lang2::check_file(input).unwrap_err()
        }

        fn render_error_text_report(error: &FrontendCheckError) -> Option<String> {
            render_text_report(error.report()?)
        }

        fn check_error_report(code: &str) -> DiagnosticReport {
            frontend_error(code).report().unwrap().clone()
        }

        fn json_value(report: &DiagnosticReport) -> serde_json::Value {
            serde_json::from_str(&render_json_report(report).unwrap()).unwrap()
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
                anvyx_lang2::check_package(input)
                    .map(|_| ())
                    .map_err(|error| error.to_string())
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
            let rendered = render_error_text_report(&error).unwrap();

            assert!(rendered.contains("Error: Mismatched types"), "{rendered}");
            assert!(rendered.contains("main.anv"), "{rendered}");
            assert!(rendered.contains(code), "{rendered}");
            assert!(
                rendered.contains("expected 'int', found 'bool'"),
                "{rendered}"
            );
            assert_eq!(error.summary(), "Failed to typecheck program");
            assert!(
                !rendered.contains("frontend typecheck failed"),
                "{rendered}"
            );
            assert!(!rendered.contains("\n- Mismatched types"), "{rendered}");
        }

        #[test]
        fn text_parse_error_renders_label_and_short_summary() {
            let error = frontend_error("fn");
            let rendered = render_error_text_report(&error).unwrap();

            assert!(rendered.contains("Unexpected end of input"), "{rendered}");
            assert!(rendered.contains("end of file"), "{rendered}");
            assert_eq!(error.summary(), "Failed to parse program");
        }

        #[test]
        fn text_message_only_error_has_no_rich_report() {
            let error = FrontendCheckError::InvalidInput("bad path".to_string());

            assert!(render_error_text_report(&error).is_none());
            assert_eq!(error.to_string(), "invalid input: bad path");
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
            let rendered = render_text_report(&report).unwrap();

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
            let rendered = render_json_report(&report).unwrap();
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
            assert_eq!(label["message"], "expected 'int', found 'bool'");
            assert!(!rendered.contains(code));
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
            let report = message_report("provider failed".to_string());
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
            let json_text = render_json_report(&report).unwrap();
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
            assert!(!json_text.contains("fn main() {}"));
        }

        mod unsupported {
            use super::*;

            #[test]
            fn accepts_feature_flags() {
                reject_new_frontend_inputs(None).unwrap();
            }

            #[test]
            fn accepts_cfg_flags() {
                reject_new_frontend_inputs(None).unwrap();
            }

            #[test]
            fn accepts_lint_flags() {
                reject_new_frontend_inputs(None).unwrap();
            }

            #[test]
            fn accepts_manifest_lint() {
                let mut manifest = plain_manifest();
                manifest
                    .lint
                    .insert("internal_access".to_string(), "error".to_string());

                reject_new_frontend_inputs(Some(&manifest)).unwrap();
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

                let error = unsupported_error(Some(&manifest));

                assert_eq!(
                    error,
                    "clean frontend does not support extern providers yet"
                );
            }

            #[test]
            fn accepts_plain_manifest() {
                let manifest = plain_manifest();

                reject_new_frontend_inputs(Some(&manifest)).unwrap();
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

                reject_new_frontend_inputs(Some(&manifest)).unwrap();
            }
        }
    }
}
