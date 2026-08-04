mod check;
mod clean;
mod fmt;
mod init;
mod lsp;
mod manifest;
mod progress;
mod run;
mod rust_backend;

use std::path::{Path, PathBuf};

use anvyx_lang::{CompilationContext, Profile, TargetArch, TargetOs};
use anvyx_project::rust::{BuildInput, RustCargoProfile, sanitize_artifact_name};
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
        #[arg(long, default_value = "text")]
        format: check::CheckOutputFormat,
        #[arg(long)]
        list_lints: bool,
        #[arg(long)]
        warn_as_error: bool,
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
    let mut ctx = CompilationContext {
        profile,
        ..CompilationContext::default()
    };
    ctx.features.extend(features.iter().cloned());
    for pair in cfgs {
        let (key, value) = pair.split_once('=').ok_or_else(|| {
            format!("invalid --cfg format: '{pair}'. Expected KEY=VALUE (e.g. --cfg os=wasm)")
        })?;

        let v = value.trim();
        match key.trim() {
            "os" => {
                ctx.os = TargetOs::parse(v).ok_or_else(|| {
                    format!(
                        "unknown os value: '{v}'. Expected: macos, linux, windows, wasm, ios, android",
                    )
                })?;
            }
            "arch" => {
                ctx.arch = TargetArch::parse(v).ok_or_else(|| {
                    format!("unknown arch value: '{v}'. Expected: x86_64, aarch64")
                })?;
            }
            "profile" => {
                ctx.profile = match v {
                    "debug" => Profile::Debug,
                    "release" => Profile::Release,
                    _ => {
                        return Err(format!(
                            "unknown profile value: '{v}'. Expected: debug, release"
                        ));
                    }
                };
            }
            "feature" => {
                ctx.features.insert(v.to_string());
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
            release,
            lint,
            feature,
            cfg,
        } => {
            let compilation_ctx = build_compilation_ctx(release, &feature, &cfg)?;
            let manifest = check_manifest(file.as_deref())?;
            let path = resolve_entry(file, manifest.as_ref())?;
            let lint_config = manifest::lint_config(manifest.as_ref(), &lint)?;
            run::cmd(
                &path,
                lint_config,
                &compilation_ctx,
                RustCargoProfile::from_release(release),
            )?;
        }
        Command::Check {
            file,
            format,
            list_lints,
            warn_as_error,
            lint,
            feature,
            cfg,
        } => {
            if list_lints {
                println!("{}", check::render_lint_list());
                return Ok(());
            }

            let manifest = check_manifest(file.as_deref())?;
            let path = resolve_entry(file, manifest.as_ref())?;
            let lint_config = manifest::lint_config(manifest.as_ref(), &lint)?;
            let compilation_ctx = build_compilation_ctx(false, &feature, &cfg)?;
            progress::status("Checking", &format!("{}...", path.display()));
            check::cmd(&path, lint_config, &compilation_ctx, format, warn_as_error)?;
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
        Command::Build {
            release,
            feature,
            cfg,
        } => {
            let manifest =
                manifest::parse_manifest()?.ok_or("anvyx build requires an anvyx.toml manifest")?;
            let cwd = std::env::current_dir()
                .map_err(|e| format!("Failed to get current directory: {e}"))?;
            let project_name = resolve_project_name(&manifest, &cwd);
            let entry = manifest
                .project
                .entry
                .as_deref()
                .ok_or("project.entry is required for build")?;
            let compilation_ctx = build_compilation_ctx(release, &feature, &cfg)?;
            let lint_config = manifest::lint_config(Some(&manifest), &[] as &[&str])?;
            let output = rust_backend::build(BuildInput {
                file: PathBuf::from(entry),
                project_root: cwd.clone(),
                project_name,
                frontend: check::frontend_config(lint_config, &compilation_ctx),
                cargo_profile: RustCargoProfile::from_release(release),
                cache_root: None,
                output_root: PathBuf::from("build"),
            })?;
            progress::status("Finished", &format!("{}", output.artifact.display()));
        }
    }

    Ok(())
}

fn check_manifest(file: Option<&Path>) -> Result<Option<Manifest>, String> {
    let Some(file) = file else {
        return manifest::parse_manifest();
    };
    let Some(path) = manifest::find_nearest_manifest(file)? else {
        return Ok(None);
    };
    manifest::parse_manifest_file(&path).map(Some)
}

fn resolve_entry(file: Option<PathBuf>, manifest: Option<&Manifest>) -> Result<PathBuf, String> {
    if let Some(f) = file {
        return Ok(f);
    }
    let m = manifest.ok_or("No file provided and no anvyx.toml found in the current directory")?;
    let entry = m
        .project
        .entry
        .as_deref()
        .ok_or("No file provided and project.entry is missing from anvyx.toml")?;
    Ok(PathBuf::from(entry))
}

fn resolve_project_name(manifest: &Manifest, project_root: &Path) -> String {
    let raw = manifest.project.name.as_deref().unwrap_or_else(|| {
        project_root
            .file_name()
            .and_then(|name| name.to_str())
            .unwrap_or("anvyx-project")
    });
    sanitize_artifact_name(raw)
}
