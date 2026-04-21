mod builtin;
mod conditional;
mod error;
mod hir;
mod intrinsic_resolve;
mod lower;
mod resolve;
mod rust;
mod std_module;
mod typecheck;
mod vm;

pub mod ast;
pub mod diagnostic;
pub mod intrinsic;
pub mod lexer;
pub mod parser;
pub mod span;

pub(crate) mod backend_names;
pub(crate) mod ir_meta;

pub mod metadata;
pub mod prelude_enums;

pub use anvyx_macros::{export_fn, export_methods, export_type, provider};
pub use diagnostic::{
    CompileOutput, CompileResult, DiagnosticFile, DiagnosticLabel, DiagnosticReport, RunError,
    RunOutput, Severity,
};
pub use intrinsic::{CompilationContext, SourceLocationInfo, TargetArch, TargetOs};
pub use metadata::{
    ExternDecl, ExternFieldDecl, ExternFuncMeta, ExternMethodDecl, ExternOpDecl, ExternOpMeta,
    ExternProviderMeta, ExternStaticMethodDecl, ExternTypeDecl, ExternTypeDeclConst,
    exports_to_json, parse_provider_json,
};
pub use prelude_enums::{OPTION_TYPE_ID, option_none, option_some};
pub use typecheck::{LintConfig, LintLevel, map_type_structure, walk_type_structure};
pub use vm::{
    AnvyxConvert, AnvyxExternType, AnvyxFn, CompiledProgram, DisplayDetect, DisplayDetectFallback,
    EnumData, ExternHandle, ExternHandleData, ExternHandler, ExternRegistry, HandleStore,
    ManagedRc, MapStorage, RuntimeError, StructData, VM, Value, VmContext, compile_with_externs,
    extern_handle, with_callback_ctx,
};

pub mod cycle_collector {
    pub use crate::vm::cycle_collector::{collect_cycles, set_auto_collect};
}

pub use std_module::{StdModule, init_std_modules};

pub struct StdModuleSource {
    pub anv_source: String,
}

pub struct CoreSource {
    pub prelude: String,
    pub modules: std::collections::HashMap<String, StdModuleSource>,
}

#[derive(Clone, Copy)]
pub struct CompileOptions<'a> {
    pub lint: LintConfig,
    pub compilation_ctx: &'a CompilationContext,
}

#[cfg(test)]
mod test_helpers;

pub(crate) fn parse_source(
    source: &str,
    file_path: &str,
    ctx: &CompilationContext,
) -> Result<diagnostic::ParsedFile, DiagnosticReport> {
    let make_report = |diagnostics: Vec<diagnostic::Diagnostic>| DiagnosticReport {
        diagnostics,
        files: vec![DiagnosticFile {
            path: file_path.to_string(),
            source: source.to_string(),
        }],
    };

    let tokens = match lexer::tokenize(source) {
        Ok(tokens) => tokens,
        Err(errors) => {
            return Err(make_report(error::collect_lexer_diagnostics(
                file_path, errors,
            )));
        }
    };

    let tokens = match conditional::filter_tokens(&tokens, ctx) {
        Ok(filtered) => filtered,
        Err(errors) => {
            return Err(make_report(error::collect_conditional_diagnostics(
                file_path, &errors,
            )));
        }
    };

    let ast = match parser::parse_ast(&tokens) {
        Ok(ast) => ast,
        Err(errors) => {
            return Err(make_report(error::collect_parse_diagnostics(
                file_path, &tokens, errors,
            )));
        }
    };

    Ok(diagnostic::ParsedFile {
        file: diagnostic::LoadedFile {
            path: file_path.to_string(),
            source: source.to_string(),
            tokens,
        },
        ast,
    })
}

type AnalyzeResult = Result<
    (
        ast::Program,
        typecheck::TypecheckResult,
        Vec<lexer::SpannedToken>,
        Vec<(Vec<String>, Vec<ast::StmtNode>)>,
    ),
    String,
>;

fn analyze_with_extern_meta(
    program: &str,
    file_path: &str,
    core_source: &str,
    extern_metadata: &std::collections::HashMap<String, String>,
    std_modules: &std::collections::HashMap<String, StdModuleSource>,
    core_modules: &std::collections::HashMap<String, StdModuleSource>,
    lint: LintConfig,
    compilation_ctx: &CompilationContext,
) -> AnalyzeResult {
    use std::collections::HashSet;

    let mut report = DiagnosticReport::default();

    let core_ast = parse_source(core_source, "<core>", compilation_ctx)
        .map_err(|r| {
            r.print_ariadne();
            "Failed to parse core source (internal error)".to_string()
        })?
        .ast;

    let diagnostic::ParsedFile {
        file: user_file,
        ast: user_ast,
    } = parse_source(program, file_path, compilation_ctx).map_err(|r| {
        r.print_ariadne();
        "Failed to parse program".to_string()
    })?;
    report.add_file(file_path, program);

    // parse extern metadata JSON and extract provider names for resolve skip list
    let mut parsed_providers = std::collections::HashMap::new();
    for (name, json) in extern_metadata {
        let meta = parse_provider_json(json)
            .map_err(|e| format!("Failed to parse metadata for extern '{name}': {e}"))?;
        parsed_providers.insert(name.clone(), meta);
    }
    let extern_names: HashSet<String> = parsed_providers.keys().cloned().collect();

    // resolve imports (local files + std modules)
    // for synthetic paths like "<test>", use "." as project root so local file imports
    // would fail naturally, std module resolution does not need a real project root
    let project_root = {
        let p = std::path::Path::new(file_path);
        if file_path.starts_with('<') {
            std::path::Path::new(".").to_path_buf()
        } else {
            p.parent()
                .unwrap_or_else(|| std::path::Path::new("."))
                .to_path_buf()
        }
    };

    let resolve_result = match resolve::resolve_imports(
        &user_ast.stmts,
        &project_root,
        &extern_names,
        std_modules,
        compilation_ctx,
    ) {
        Ok(result) => result,
        Err(resolve_errors) => {
            for r in resolve_errors.parse_reports {
                report.merge(r);
            }
            if !resolve_errors.import_errors.is_empty() {
                let diags = error::collect_import_diagnostics(
                    file_path,
                    &user_file.tokens,
                    &resolve_errors.import_errors,
                );
                report.diagnostics.extend(diags);
            }
            report.print_ariadne();
            return Err("Failed to resolve imports".to_string());
        }
    };

    let mut module_list: Vec<(Vec<String>, Vec<ast::StmtNode>)> = vec![];
    let mut module_loaded_files: Vec<Option<diagnostic::LoadedFile>> = vec![];
    let mut scc_groups: Vec<Vec<usize>> = vec![];
    for group in resolve_result.module_groups {
        let group_indices = group
            .into_iter()
            .map(|module| {
                let idx = module_list.len();
                module_loaded_files.push(module.loaded_file);
                module_list.push((module.path_key, module.stmts));
                idx
            })
            .collect();
        scc_groups.push(group_indices);
    }

    // convert extern metadata into synthetic ExternFunc stmts and append to module_list
    for (name, meta) in &parsed_providers {
        let stmts = metadata::metadata_to_extern_stmts(meta)
            .map_err(|e| format!("Failed to create extern stmts for '{name}': {e}"))?;
        scc_groups.push(vec![module_list.len()]);
        module_loaded_files.push(None);
        module_list.push((vec![name.clone()], stmts));
    }

    // route core modules through the module system (extends auto-activated, externs scoped)
    let mut auto_use_modules: Vec<Vec<String>> = vec![];
    let mut core_count = 0usize;
    for (name, source) in core_modules {
        let file_label = format!("<core.{name}>");
        let parsed =
            parse_source(&source.anv_source, &file_label, compilation_ctx).map_err(|r| {
                r.print_ariadne();
                format!("Failed to parse core module '{name}' (internal error)")
            })?;
        let path_key = vec![name.clone()];
        module_list.insert(0, (path_key.clone(), parsed.ast.stmts));
        module_loaded_files.insert(0, Some(parsed.file));
        auto_use_modules.push(path_key);
        core_count += 1;
    }
    if core_count > 0 {
        for group in &mut scc_groups {
            for idx in group.iter_mut() {
                *idx += core_count;
            }
        }
        let core_groups: Vec<Vec<usize>> = (0..core_count).map(|i| vec![i]).collect();
        scc_groups.splice(0..0, core_groups);
    }

    let mut combined_stmts = core_ast.stmts;
    combined_stmts.extend(user_ast.stmts);
    let mut combined = ast::Program {
        stmts: combined_stmts,
    };

    // resolve intrinsics in module ast
    for ((_, stmts), loaded_file) in module_list.iter_mut().zip(module_loaded_files.iter()) {
        let source_loc_info = loaded_file
            .as_ref()
            .map(diagnostic::LoadedFile::to_source_location_info);
        let mut module_prog = ast::Program {
            stmts: std::mem::take(stmts),
        };
        let resolve_result = intrinsic_resolve::resolve_intrinsics(
            &mut module_prog,
            compilation_ctx,
            source_loc_info.as_ref(),
        );

        if let Some(lf) = loaded_file {
            if !resolve_result.diagnostics.is_empty() {
                let diags = error::collect_intrinsic_diagnostics(
                    &lf.path,
                    &lf.tokens,
                    &resolve_result.diagnostics,
                );
                report.add_file(&lf.path, &lf.source);
                report.diagnostics.extend(diags);
            }
            if !resolve_result.errors.is_empty() {
                let diags =
                    error::collect_intrinsic_errors(&lf.path, &lf.tokens, &resolve_result.errors);
                report.add_file(&lf.path, &lf.source);
                report.diagnostics.extend(diags);
                report.print_ariadne();
                return Err("Failed to resolve intrinsics".to_string());
            }
        } else {
            // extern provider modules — no source file, keep eprintln fallback
            for diag in &resolve_result.diagnostics {
                let prefix = match diag.level {
                    intrinsic::IntrinsicDiagnosticLevel::Warning => "warning",
                    intrinsic::IntrinsicDiagnosticLevel::Error => "error",
                    intrinsic::IntrinsicDiagnosticLevel::Note => "note",
                };
                eprintln!("{prefix}: {}", diag.message);
            }
            if !resolve_result.errors.is_empty() {
                let msg = resolve_result
                    .errors
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join("\n");
                report.print_ariadne();
                return Err(msg);
            }
        }

        if resolve_result.has_error_diagnostic() {
            report.print_ariadne();
            return Err("Compilation halted by #error directive".to_string());
        }

        *stmts = module_prog.stmts;
    }

    // resolve intrinsics in combined (core + user) ast
    let user_source_loc =
        SourceLocationInfo::new(file_path.to_string(), program, &user_file.tokens);
    let resolve_result = intrinsic_resolve::resolve_intrinsics(
        &mut combined,
        compilation_ctx,
        Some(&user_source_loc),
    );

    if !resolve_result.diagnostics.is_empty() {
        let diags = error::collect_intrinsic_diagnostics(
            file_path,
            &user_file.tokens,
            &resolve_result.diagnostics,
        );
        report.diagnostics.extend(diags);
    }

    if !resolve_result.errors.is_empty() {
        let diags =
            error::collect_intrinsic_errors(file_path, &user_file.tokens, &resolve_result.errors);
        report.diagnostics.extend(diags);
        report.print_ariadne();
        return Err("Failed to resolve intrinsics".to_string());
    }

    if resolve_result.has_error_diagnostic() {
        report.print_ariadne();
        return Err("Compilation halted by #error directive".to_string());
    }

    let tcx = match typecheck::check_program_with_modules(
        &combined,
        &module_list,
        &scc_groups,
        &auto_use_modules,
        lint,
    ) {
        Ok(tcx) => {
            if !tcx.warnings().is_empty() {
                let diags = error::collect_typecheck_diagnostics(
                    file_path,
                    &user_file.tokens,
                    tcx.warnings().to_vec(),
                );
                report.diagnostics.extend(diags);
            }
            tcx
        }
        Err(errors) => {
            let diags = error::collect_typecheck_diagnostics(file_path, &user_file.tokens, errors);
            report.diagnostics.extend(diags);
            report.print_ariadne();
            return Err("Failed to typecheck program".to_string());
        }
    };

    if !report.diagnostics.is_empty() {
        report.print_ariadne();
    }

    Ok((combined, tcx, user_file.tokens, module_list))
}

pub fn generate_ast(
    program: &str,
    file_path: &str,
    core_source: &str,
    lint: LintConfig,
    compilation_ctx: &CompilationContext,
) -> Result<ast::Program, String> {
    generate_ast_with_externs(
        program,
        file_path,
        core_source,
        &std::collections::HashMap::new(),
        lint,
        compilation_ctx,
    )
}

pub fn generate_ast_with_externs(
    program: &str,
    file_path: &str,
    core_source: &str,
    extern_metadata: &std::collections::HashMap<String, String>,
    lint: LintConfig,
    compilation_ctx: &CompilationContext,
) -> Result<ast::Program, String> {
    generate_ast_with_std(
        program,
        file_path,
        core_source,
        extern_metadata,
        &std::collections::HashMap::new(),
        &std::collections::HashMap::new(),
        lint,
        compilation_ctx,
    )
}

pub fn generate_ast_with_std(
    program: &str,
    file_path: &str,
    core_source: &str,
    extern_metadata: &std::collections::HashMap<String, String>,
    std_modules: &std::collections::HashMap<String, StdModuleSource>,
    core_modules: &std::collections::HashMap<String, StdModuleSource>,
    lint: LintConfig,
    compilation_ctx: &CompilationContext,
) -> Result<ast::Program, String> {
    let (ast, _, _, _) = analyze_with_extern_meta(
        program,
        file_path,
        core_source,
        extern_metadata,
        std_modules,
        core_modules,
        lint,
        compilation_ctx,
    )?;
    Ok(ast)
}

pub(crate) fn generate_hir_with_std(
    program: &str,
    file_path: &str,
    core_source: &str,
    extern_metadata: &std::collections::HashMap<String, String>,
    std_modules: &std::collections::HashMap<String, StdModuleSource>,
    core_modules: &std::collections::HashMap<String, StdModuleSource>,
    lint: LintConfig,
    compilation_ctx: &CompilationContext,
) -> Result<hir::Program, String> {
    let (ast, tcx, _, module_list) = analyze_with_extern_meta(
        program,
        file_path,
        core_source,
        extern_metadata,
        std_modules,
        core_modules,
        lint,
        compilation_ctx,
    )?;
    lower::lower_program(&ast, &tcx, &module_list).map_err(|e| format!("Lowering error: {e}"))
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Hash)]
pub enum Profile {
    #[default]
    Debug,
    Release,
}

impl Profile {
    pub const ALL: &[&str] = &["debug", "release"];

    pub fn as_str(self) -> &'static str {
        match self {
            Self::Debug => "debug",
            Self::Release => "release",
        }
    }
}

impl std::str::FromStr for Profile {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, ()> {
        match s {
            "debug" => Ok(Self::Debug),
            "release" => Ok(Self::Release),
            _ => Err(()),
        }
    }
}

#[derive(Default)]
pub struct RustBackendConfig {
    pub profile: Profile,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum Backend {
    #[default]
    Vm,
    Rust,
}

impl std::str::FromStr for Backend {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "vm" => Ok(Self::Vm),
            "rust" => Ok(Self::Rust),
            _ => Err(format!("Unknown backend: '{s}'. Expected 'vm' or 'rust'")),
        }
    }
}

pub fn run_program(
    program: &str,
    file_path: &str,
    core_source: &str,
    backend: Backend,
    rust_config: &RustBackendConfig,
    lint: LintConfig,
    compilation_ctx: &CompilationContext,
) -> Result<String, String> {
    run_program_with_externs(
        program,
        file_path,
        core_source,
        backend,
        std::collections::HashMap::new(),
        &std::collections::HashMap::new(),
        rust_config,
        lint,
        compilation_ctx,
    )
}

pub fn run_program_with_externs(
    program: &str,
    file_path: &str,
    core_source: &str,
    backend: Backend,
    externs: std::collections::HashMap<String, ExternHandler>,
    extern_metadata: &std::collections::HashMap<String, String>,
    rust_config: &RustBackendConfig,
    lint: LintConfig,
    compilation_ctx: &CompilationContext,
) -> Result<String, String> {
    let core = CoreSource {
        prelude: core_source.to_string(),
        modules: std::collections::HashMap::new(),
    };
    run_program_with_std(
        program,
        file_path,
        backend,
        externs,
        extern_metadata,
        &std::collections::HashMap::new(),
        &core,
        rust_config,
        CompileOptions {
            lint,
            compilation_ctx,
        },
    )
}

fn filter_externs_by_hir(
    externs: std::collections::HashMap<String, ExternHandler>,
    hir: &hir::Program,
) -> std::collections::HashMap<String, ExternHandler> {
    let declared: std::collections::HashSet<String> =
        hir.externs.iter().map(|e| e.name.to_string()).collect();
    externs
        .into_iter()
        .filter(|(name, _)| declared.contains(name))
        .collect()
}

/// Compiles Anvyx code for the VM and returns the program with its extern registry.
///
/// Use this when the host wants to run the VM and call closures later.
pub fn compile_vm_with_externs(
    program: &str,
    file_path: &str,
    core_source: &str,
    externs: std::collections::HashMap<String, ExternHandler>,
    lint: LintConfig,
    compilation_ctx: &CompilationContext,
) -> Result<(CompiledProgram, ExternRegistry), String> {
    use std::collections::HashMap;

    let hir = generate_hir_with_std(
        program,
        file_path,
        core_source,
        &HashMap::new(),
        &HashMap::new(),
        &HashMap::new(),
        lint,
        compilation_ctx,
    )?;

    let filtered = filter_externs_by_hir(externs, &hir);
    compile_with_externs(&hir, filtered, Profile::default())
}

pub fn run_program_with_std(
    program: &str,
    file_path: &str,
    backend: Backend,
    externs: std::collections::HashMap<String, ExternHandler>,
    extern_metadata: &std::collections::HashMap<String, String>,
    std_modules: &std::collections::HashMap<String, StdModuleSource>,
    core: &CoreSource,
    rust_config: &RustBackendConfig,
    options: CompileOptions<'_>,
) -> Result<String, String> {
    let hir = generate_hir_with_std(
        program,
        file_path,
        &core.prelude,
        extern_metadata,
        std_modules,
        &core.modules,
        options.lint,
        options.compilation_ctx,
    )?;

    let filtered = filter_externs_by_hir(externs, &hir);

    match backend {
        Backend::Vm => vm::run_with_externs(&hir, filtered, rust_config.profile),
        Backend::Rust => rust::run(&hir, rust_config),
    }
}

#[cfg(test)]
pub(crate) const TEST_CORE_SOURCE: &str = "";

#[cfg(test)]
mod extern_import_tests {
    use std::collections::HashMap;

    use super::*;

    fn sample_metadata() -> HashMap<String, String> {
        let json = r#"{"types":[],"functions":[
            {"name":"add","params":[["a","int"],["b","int"]],"ret":"int"},
            {"name":"greet","params":[["name","string"]],"ret":"string"}
        ]}"#;
        let mut m = HashMap::new();
        m.insert("my_extern".to_string(), json.to_string());
        m
    }

    fn analyze_ok(src: &str) {
        let result = analyze_with_extern_meta(
            src,
            "<test>",
            TEST_CORE_SOURCE,
            &sample_metadata(),
            &HashMap::new(),
            &HashMap::new(),
            LintConfig::default(),
            &CompilationContext::from_host(Profile::Debug),
        );
        assert!(result.is_ok(), "unexpected error: {:?}", result.err());
    }

    fn analyze_err(src: &str) {
        let result = analyze_with_extern_meta(
            src,
            "<test>",
            TEST_CORE_SOURCE,
            &sample_metadata(),
            &HashMap::new(),
            &HashMap::new(),
            LintConfig::default(),
            &CompilationContext::from_host(Profile::Debug),
        );
        assert!(result.is_err());
    }

    #[test]
    fn selective_import_typechecks() {
        analyze_ok("import my_extern { add };\nfn main() { let x = add(3, 4); }");
    }

    #[test]
    fn qualified_import_typechecks() {
        analyze_ok("import my_extern;\nfn main() { let x = my_extern.add(3, 4); }");
    }

    #[test]
    fn wildcard_import_typechecks() {
        analyze_ok("import my_extern { * };\nfn main() { let x = add(3, 4); }");
    }

    #[test]
    fn alias_import_typechecks() {
        analyze_ok("import my_extern { add as plus };\nfn main() { let x = plus(3, 4); }");
    }

    #[test]
    fn module_alias_import_typechecks() {
        analyze_ok("import my_extern as ext;\nfn main() { let x = ext.add(3, 4); }");
    }

    #[test]
    fn unknown_member_error() {
        analyze_err("import my_extern { nonexistent };\nfn main() {}");
    }

    #[test]
    fn wrong_arg_type_error() {
        analyze_err("import my_extern { add };\nfn main() { let x = add(true, 4); }");
    }

    #[test]
    fn hir_contains_extern_decls() {
        let src = "import my_extern { add, greet };\nfn main() { let x = add(1, 2); }";
        let hir = generate_hir_with_std(
            src,
            "<test>",
            TEST_CORE_SOURCE,
            &sample_metadata(),
            &HashMap::new(),
            &HashMap::new(),
            LintConfig::default(),
            &CompilationContext::from_host(Profile::Debug),
        );
        assert!(hir.is_ok(), "unexpected error: {:?}", hir.err());
        let program = hir.unwrap();
        assert!(!program.externs.is_empty(), "expected extern decls in HIR");
    }

    #[test]
    fn no_extern_meta_still_works() {
        let result = analyze_with_extern_meta(
            "fn main() { let x = 1; }",
            "<test>",
            TEST_CORE_SOURCE,
            &HashMap::new(),
            &HashMap::new(),
            &HashMap::new(),
            LintConfig::default(),
            &CompilationContext::from_host(Profile::Debug),
        );
        assert!(result.is_ok());
    }
}

#[cfg(test)]
mod std_import_tests {
    use std::collections::HashMap;

    use super::*;

    fn math_std_modules() -> HashMap<String, StdModuleSource> {
        let mut m = HashMap::new();
        m.insert(
            "math".to_string(),
            StdModuleSource {
                anv_source: "pub const PI = 3.14159265358979323846;\n".to_string(),
            },
        );
        m
    }

    fn analyze_ok(src: &str) {
        let result = analyze_with_extern_meta(
            src,
            "<test>",
            TEST_CORE_SOURCE,
            &HashMap::new(),
            &math_std_modules(),
            &HashMap::new(),
            LintConfig::default(),
            &CompilationContext::from_host(Profile::Debug),
        );
        assert!(result.is_ok(), "unexpected error: {:?}", result.err());
    }

    fn analyze_err(src: &str) {
        let result = analyze_with_extern_meta(
            src,
            "<test>",
            TEST_CORE_SOURCE,
            &HashMap::new(),
            &math_std_modules(),
            &HashMap::new(),
            LintConfig::default(),
            &CompilationContext::from_host(Profile::Debug),
        );
        assert!(result.is_err());
    }

    #[test]
    fn std_qualified_import_typechecks() {
        analyze_ok("import std.math;\nfn main() { let x = math.PI; }");
    }

    #[test]
    fn std_selective_import_typechecks() {
        analyze_ok("import std.math { PI };\nfn main() { let x = PI; }");
    }

    #[test]
    fn std_unknown_module_errors() {
        analyze_err("import std.nonexistent;\nfn main() {}");
    }

    #[test]
    fn std_bare_import_errors() {
        analyze_err("import std;\nfn main() {}");
    }
}
