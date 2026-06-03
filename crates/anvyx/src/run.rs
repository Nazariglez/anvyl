use std::{collections::HashMap, fs, path::Path};

use anvyx_lang::{
    Backend, CompilationContext, CompileOptions, CoreSource, LintConfig, RustBackendConfig,
    run_program_with_std,
};
use anvyx_project::rust::{CleanRustRunInput, RustCargoProfile};

use crate::std_support::{collect_core, collect_std};

pub fn new_frontend_cmd(
    file: &Path,
    lint: anvyx_lang2::LintConfig,
    ctx: &CompilationContext,
    cargo_profile: RustCargoProfile,
) -> Result<(), String> {
    let output = crate::clean_rust::run(CleanRustRunInput {
        file: file.to_path_buf(),
        frontend: crate::check::new_frontend_config(lint, ctx),
        cargo_profile,
        cache_root: None,
    })?;
    print!("{}", output.stdout);
    eprint!("{}", output.stderr);
    Ok(())
}

pub fn cmd(
    file: &Path,
    backend: &str,
    lint: LintConfig,
    ctx: &CompilationContext,
) -> Result<(), String> {
    let backend = backend.parse::<Backend>()?;
    let rust_config = RustBackendConfig {
        profile: ctx.profile,
    };
    let program = fs::read_to_string(file).map_err(|e| format!("Failed to read file: {e}"))?;
    let file_path = file.to_string_lossy().to_string();
    let (std_sources, mut handlers) = collect_std();
    let (core_prelude, core_sources, core_handlers) = collect_core();
    handlers.extend(core_handlers);

    let core = CoreSource {
        prelude: core_prelude,
        modules: core_sources,
    };
    let output = run_program_with_std(
        &program,
        &file_path,
        backend,
        handlers,
        &HashMap::new(),
        &std_sources,
        &core,
        &rust_config,
        CompileOptions {
            lint,
            compilation_ctx: ctx,
        },
    )?;
    print!("{output}");
    Ok(())
}
