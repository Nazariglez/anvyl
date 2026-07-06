use std::path::Path;

use anvyx_lang::{CompilationContext, LintConfig};
use anvyx_project::rust::{RunInput, RustCargoProfile};

pub fn cmd(
    file: &Path,
    lint: LintConfig,
    ctx: &CompilationContext,
    cargo_profile: RustCargoProfile,
) -> Result<(), String> {
    let output = crate::rust_backend::run(RunInput {
        file: file.to_path_buf(),
        frontend: crate::check::frontend_config(lint, ctx),
        cargo_profile,
        cache_root: None,
    })?;
    print!("{}", output.stdout);
    eprint!("{}", output.stderr);
    Ok(())
}
