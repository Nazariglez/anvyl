mod build;
mod check;
mod clean;
mod fmt;
mod frontend_sources;
mod init;
mod lsp;
mod manifest;
mod progress;
mod run;
mod std_support;

use std::{collections::HashMap, path::PathBuf};

use anvyx_lang::{CompilationContext, LintConfig, Profile};
use clap::{Parser, Subcommand};

use crate::manifest::Manifest;

#[derive(Parser, Debug)]
#[command(
    name = env!("CARGO_PKG_NAME"),
    version = env!("CARGO_PKG_VERSION"),
    about = env!("CARGO_PKG_DESCRIPTION")
)]
struct Cli {
    #[command(subcommand)]
    command: Command,
}

#[derive(Subcommand, Debug)]
enum Command {
    #[command(about = "Run an Anvyx program")]
    Run {
        file: Option<PathBuf>,
        #[arg(long, default_value = "vm")]
        backend: String,
        #[arg(long)]
        release: bool,
        #[arg(long, value_name = "KEY=VALUE")]
        lint: Vec<String>,
        #[arg(long, value_delimiter = ',')]
        feature: Vec<String>,
        #[arg(long, value_name = "KEY=VALUE")]
        cfg: Vec<String>,
    },
    #[command(about = "Check an Anvyx file")]
    Check {
        file: Option<PathBuf>,
        #[arg(long)]
        new_frontend: bool,
        #[arg(long, default_value = "text")]
        format: check::CheckOutputFormat,
        #[arg(long, value_name = "KEY=VALUE")]
        lint: Vec<String>,
        #[arg(long, value_delimiter = ',')]
        feature: Vec<String>,
        #[arg(long, value_name = "KEY=VALUE")]
        cfg: Vec<String>,
    },
    #[command(about = "Create a new Anvyx project")]
    Init { name: Option<String> },
    #[command(about = "Run the Anvyx language server")]
    Lsp,
    #[command(about = "Build an Anvyx project for distribution")]
    Build {
        #[arg(long)]
        release: bool,
        #[arg(long, value_delimiter = ',')]
        feature: Vec<String>,
        #[arg(long, value_name = "KEY=VALUE")]
        cfg: Vec<String>,
    },
    #[command(about = "Remove build cache")]
    Clean,
    #[command(about = "Format Anvyx source files")]
    Fmt {
        path: Option<PathBuf>,
        #[arg(long)]
        check: bool,
        #[arg(long)]
        stdin: bool,
    },
}

fn main() {
    let cli = Cli::parse();
    if let Err(e) = run(cli) {
        progress::error(&e);
        std::process::exit(1);
    }
}

fn build_compilation_ctx(
    release: bool,
    features: &[String],
    cfgs: &[String],
) -> Result<CompilationContext, String> {
    let profile = if release {
        Profile::Release
    } else {
        Profile::Debug
    };
    let mut ctx = CompilationContext::from_host(profile);
    ctx.features = features.to_vec();
    for pair in cfgs {
        let (key, value) = pair.split_once('=').ok_or_else(|| {
            format!("invalid --cfg format: '{pair}'. Expected KEY=VALUE (e.g. --cfg os=wasm)")
        })?;

        let v = value.trim();
        match key.trim() {
            "os" => {
                ctx.os = v.parse().map_err(|()| {
                    format!(
                        "unknown os value: '{v}'. Expected: macos, linux, windows, wasm, ios, android",
                    )
                })?;
            }
            "arch" => {
                ctx.arch = v.parse().map_err(|()| {
                    format!("unknown arch value: '{v}'. Expected: x86_64, aarch64")
                })?;
            }
            "profile" => {
                ctx.profile = v.parse().map_err(|()| {
                    format!("unknown profile value: '{v}'. Expected: debug, release")
                })?;
            }
            "feature" => {
                ctx.features.push(v.to_string());
            }
            other => {
                return Err(format!(
                    "unknown --cfg category: '{other}'. Expected: os, arch, profile, feature"
                ));
            }
        }
    }
    Ok(ctx)
}

fn run(cli: Cli) -> Result<(), String> {
    match cli.command {
        Command::Run {
            file,
            backend,
            release,
            lint,
            feature,
            cfg,
        } => {
            let manifest = manifest::parse_manifest()?;
            let path = resolve_entry(file, manifest.as_ref())?;
            let lint_config = resolve_lint_config(manifest.as_ref(), &lint)?;
            let compilation_ctx = build_compilation_ctx(release, &feature, &cfg)?;

            let has_externs = manifest.as_ref().is_some_and(Manifest::has_externs);
            if has_externs {
                let manifest = manifest.as_ref().unwrap();
                let ctx = prepare_externs(manifest)?;

                progress::status("Checking", &format!("{}...", path.display()));
                progress::status("Running", &format!("{}...", path.display()));
                build::execute_runner(
                    &ctx.cwd,
                    &path,
                    &backend,
                    release,
                    lint_config,
                    &feature,
                    &cfg,
                )?;
            } else {
                progress::status("Checking", &format!("{}...", path.display()));
                progress::status("Running", &format!("{}...", path.display()));
                run::cmd(&path, &backend, lint_config, &compilation_ctx)?;
            }
        }
        Command::Check {
            file,
            new_frontend,
            format,
            lint,
            feature,
            cfg,
        } => {
            let manifest = manifest::parse_manifest()?;
            let path = resolve_entry(file, manifest.as_ref())?;

            let lint_config = resolve_lint_config(manifest.as_ref(), &lint)?;

            if new_frontend {
                let compilation_ctx = build_compilation_ctx(false, &feature, &cfg)?;
                check::reject_new_frontend_inputs(manifest.as_ref())?;
                progress::status("Checking", &format!("{}...", path.display()));
                check::new_frontend_cmd(&path, lint_config, &compilation_ctx, format)?;
                progress::status(
                    "Finished",
                    &format!("{} checked successfully", path.display()),
                );
                return Ok(());
            }
            if format != check::CheckOutputFormat::Text {
                return Err("--format json requires --new-frontend".to_string());
            }
            let compilation_ctx = build_compilation_ctx(false, &feature, &cfg)?;

            let has_externs = manifest.as_ref().is_some_and(Manifest::has_externs);
            let extern_meta = if has_externs {
                let manifest = manifest.as_ref().unwrap();
                let ctx = prepare_externs(manifest)?;
                ctx.metadata
            } else {
                HashMap::new()
            };

            progress::status("Checking", &format!("{}...", path.display()));
            check::cmd(&path, &extern_meta, lint_config, &compilation_ctx)?;
            progress::status(
                "Finished",
                &format!("{} checked successfully", path.display()),
            );
        }
        Command::Init { name } => {
            init::cmd(name.as_deref())?;
        }
        Command::Lsp => {
            lsp::cmd()?;
        }
        Command::Clean => {
            clean::cmd()?;
        }
        Command::Fmt { path, check, stdin } => {
            fmt::cmd(path, check, stdin)?;
        }
        Command::Build { release, .. } => {
            let manifest =
                manifest::parse_manifest()?.ok_or("anvyx build requires an anvyx.toml manifest")?;
            let cwd = std::env::current_dir()
                .map_err(|e| format!("Failed to get current directory: {e}"))?;
            let project_name = build::resolve_project_name(&manifest, &cwd);

            if manifest.has_externs() {
                prepare_externs(&manifest)?;
            }

            let runner_dir = build::generate_build_runner_crate(&cwd, &manifest, release)?;

            let spinner = progress::start_spinner("Bundling", "distribution...");
            build::build_runner(&runner_dir)?;
            progress::finish_spinner(&spinner);

            progress::status("Assembling", "distribution...");
            let dist_dir = build::assemble_dist(&cwd, &project_name)?;
            build::bundle_sources(&cwd, &dist_dir, &manifest)?;
            progress::status("Finished", &format!("{}", dist_dir.display()));
        }
    }

    Ok(())
}

fn resolve_entry(file: Option<PathBuf>, manifest: Option<&Manifest>) -> Result<PathBuf, String> {
    if let Some(f) = file {
        Ok(f)
    } else {
        let m =
            manifest.ok_or("No file provided and no anvyx.toml found in the current directory")?;
        let entry = m
            .project
            .entry
            .as_deref()
            .ok_or("No file provided and project.entry is missing from anvyx.toml")?;
        Ok(PathBuf::from(entry))
    }
}

struct ExternContext {
    cwd: PathBuf,
    metadata: HashMap<String, String>,
}

fn prepare_externs(manifest: &Manifest) -> Result<ExternContext, String> {
    let cwd =
        std::env::current_dir().map_err(|e| format!("Failed to get current directory: {e}"))?;

    for name in manifest.externs.keys() {
        progress::status("Loading", &format!("extern {name}..."));
    }
    let runner_dir = build::generate_runner_crate(&cwd, manifest)?;

    let spinner = progress::start_spinner("Compiling", "externs...");
    build::build_runner(&runner_dir)?;
    progress::finish_spinner(&spinner);

    progress::status("Resolving", "extern types...");
    build::extract_metadata(&cwd)?;

    let metadata = build::read_metadata(&cwd, manifest)?;
    Ok(ExternContext { cwd, metadata })
}

fn resolve_lint_config(
    manifest: Option<&Manifest>,
    lint_overrides: &[String],
) -> Result<LintConfig, String> {
    let mut config = manifest.map(|m| m.lint).unwrap_or_default();
    for pair in lint_overrides {
        let (key, value) = pair.split_once('=').ok_or_else(|| {
            format!(
                "invalid --lint format: '{pair}'. Expected key=value (e.g. --lint internal_access=error)"
            )
        })?;
        let level = value.parse()?;
        match key.trim() {
            "internal_access" => config.internal_access = level,
            other => {
                return Err(format!(
                    "unknown lint: '{other}'. Available: internal_access"
                ));
            }
        }
    }
    Ok(config)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn check_accepts_new_frontend_flag() {
        let cli = Cli::parse_from(["anvyx", "check", "--new-frontend", "main.anv"]);

        match cli.command {
            Command::Check {
                file, new_frontend, ..
            } => {
                assert!(new_frontend);
                assert_eq!(file, Some(PathBuf::from("main.anv")));
            }
            other => panic!("expected check command, got {other:?}"),
        }
    }

    #[test]
    fn check_defaults_new_frontend_to_false() {
        let cli = Cli::parse_from(["anvyx", "check", "main.anv"]);

        match cli.command {
            Command::Check { new_frontend, .. } => assert!(!new_frontend),
            other => panic!("expected check command, got {other:?}"),
        }
    }

    #[test]
    fn run_rejects_new_frontend_flag() {
        let error = Cli::try_parse_from(["anvyx", "run", "--new-frontend", "main.anv"])
            .expect_err("run must reject check-only flag");

        assert_eq!(error.kind(), clap::error::ErrorKind::UnknownArgument);
    }
}
