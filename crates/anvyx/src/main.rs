mod build;
mod check;
mod clean;
mod fmt;
mod init;
mod lsp;
mod manifest;
mod progress;
mod run;
mod std_support;

use std::{
    collections::HashMap,
    path::{Path, PathBuf},
};

use anvyx_lang::{CompilationContext, LintConfig as LegacyLintConfig, Profile};
use anvyx_lang2::{LintConfig, LintId, LintLevel, expand_group, find_lint};
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
            let legacy_lint = resolve_legacy_lint_config(manifest.as_ref(), &lint)?;
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
                    legacy_lint,
                    &feature,
                    &cfg,
                )?;
            } else {
                progress::status("Checking", &format!("{}...", path.display()));
                progress::status("Running", &format!("{}...", path.display()));
                run::cmd(&path, &backend, legacy_lint, &compilation_ctx)?;
            }
        }
        Command::Check {
            file,
            new_frontend,
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

            if new_frontend {
                let manifest = check_manifest(file.as_deref())?;
                let path = resolve_entry(file, manifest.as_ref())?;
                let lint_config = resolve_lint_config(manifest.as_ref(), &lint)?;
                let compilation_ctx = build_compilation_ctx(false, &feature, &cfg)?;
                check::reject_new_frontend_inputs(manifest.as_ref())?;
                progress::status("Checking", &format!("{}...", path.display()));
                check::new_frontend_cmd(
                    &path,
                    lint_config,
                    &compilation_ctx,
                    format,
                    warn_as_error,
                )?;
                progress::status(
                    "Finished",
                    &format!("{} checked successfully", path.display()),
                );
                return Ok(());
            }

            if warn_as_error {
                return Err("--warn-as-error requires --new-frontend".to_string());
            }
            let manifest = manifest::parse_manifest()?;
            let path = resolve_entry(file, manifest.as_ref())?;
            let legacy_lint = resolve_legacy_lint_config(manifest.as_ref(), &lint)?;
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
            check::cmd(&path, &extern_meta, legacy_lint, &compilation_ctx)?;
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
    manifest::lint_config(manifest, lint_overrides)
}

fn resolve_legacy_lint_config(
    manifest: Option<&Manifest>,
    lint_overrides: &[String],
) -> Result<LegacyLintConfig, String> {
    reject_unsupported_legacy_lints(manifest, lint_overrides)?;
    let config = resolve_lint_config(manifest, lint_overrides)?;
    Ok(legacy_lint_config(&config))
}

fn reject_unsupported_legacy_lints(
    manifest: Option<&Manifest>,
    lint_overrides: &[String],
) -> Result<(), String> {
    if let Some(manifest) = manifest {
        for name in manifest.lint.keys() {
            reject_unsupported_legacy_lint_name(name)?;
        }
    }
    for override_text in lint_overrides {
        let Some((name, _)) = override_text.split_once('=') else {
            continue;
        };
        reject_unsupported_legacy_lint_name(name)?;
    }
    Ok(())
}

fn reject_unsupported_legacy_lint_name(name: &str) -> Result<(), String> {
    if name == LintId::InternalAccess.name() {
        return Ok(());
    }
    if find_lint(name).is_some() || expand_group(name).is_some() {
        return Err(format!(
            "lint override '{name}' is not supported by the legacy frontend; use --new-frontend or only internal_access"
        ));
    }
    Ok(())
}

fn legacy_lint_config(config: &LintConfig) -> LegacyLintConfig {
    LegacyLintConfig {
        internal_access: match config.level(LintId::InternalAccess) {
            LintLevel::Allow => anvyx_lang::LintLevel::Allow,
            LintLevel::Warn => anvyx_lang::LintLevel::Warn,
            LintLevel::Error => anvyx_lang::LintLevel::Error,
        },
    }
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

    #[test]
    fn check_accepts_list_lints_without_file() {
        let cli = Cli::parse_from(["anvyx", "check", "--list-lints"]);

        match cli.command {
            Command::Check {
                file, list_lints, ..
            } => {
                assert_eq!(file, None);
                assert!(list_lints);
            }
            other => panic!("expected check command, got {other:?}"),
        }
    }

    #[test]
    fn check_accepts_warn_as_error() {
        let cli = Cli::parse_from(["anvyx", "check", "--warn-as-error", "main.anv"]);

        match cli.command {
            Command::Check { warn_as_error, .. } => assert!(warn_as_error),
            other => panic!("expected check command, got {other:?}"),
        }
    }

    #[test]
    fn check_rejects_warn_as_error_aliases() {
        for alias in ["--werror", "--warnings-as-errors", "--fail-on-warn"] {
            let error = Cli::try_parse_from(["anvyx", "check", alias, "main.anv"])
                .expect_err("alias must be rejected");

            assert_eq!(error.kind(), clap::error::ErrorKind::UnknownArgument);
        }
    }

    #[test]
    fn legacy_check_rejects_warn_as_error() {
        let cli = Cli::parse_from(["anvyx", "check", "--warn-as-error", "main.anv"]);

        let error = run(cli).unwrap_err();

        assert_eq!(error, "--warn-as-error requires --new-frontend");
    }

    #[test]
    fn list_lints_returns_before_resolving_inputs() {
        let cli = Cli::parse_from(["anvyx", "check", "--list-lints"]);

        run(cli).unwrap();
    }

    fn manifest_with_lints(lints: &[(&str, &str)]) -> Manifest {
        Manifest {
            project: manifest::Project {
                name: None,
                entry: Some("main.anv".to_string()),
            },
            dependencies: Default::default(),
            externs: Default::default(),
            lint: lints
                .iter()
                .map(|(name, level)| ((*name).to_string(), (*level).to_string()))
                .collect(),
        }
    }

    #[test]
    fn lint_config_applies_manifest_values() {
        let manifest = manifest_with_lints(&[("deprecated", "allow"), ("api", "error")]);

        let config = resolve_lint_config(Some(&manifest), &[]).unwrap();

        assert_eq!(config.level(LintId::InternalAccess), LintLevel::Error);
        assert_eq!(config.level(LintId::Deprecated), LintLevel::Allow);
        assert_eq!(
            config.level(LintId::PublicInferredDynContract),
            LintLevel::Error
        );
    }

    #[test]
    fn lint_config_applies_cli_overrides_after_manifest() {
        let manifest = manifest_with_lints(&[("api", "error")]);

        let config = resolve_lint_config(
            Some(&manifest),
            &[
                "deprecated=allow".to_string(),
                "deprecated=warn".to_string(),
            ],
        )
        .unwrap();

        assert_eq!(config.level(LintId::InternalAccess), LintLevel::Error);
        assert_eq!(config.level(LintId::Deprecated), LintLevel::Warn);
    }

    #[test]
    fn check_manifest_for_file_applies_nearest_manifest_lints() {
        let temp = tempfile::tempdir().unwrap();
        let src = temp.path().join("game/src");
        std::fs::create_dir_all(&src).unwrap();
        std::fs::write(
            temp.path().join("game/anvyx.toml"),
            "[project]\nentry = \"src/main.anv\"\n\n[lint]\npublic_inferred_dyn_contract = \"error\"\n",
        )
        .unwrap();
        let file = src.join("main.anv");
        std::fs::write(&file, "fn main() {}\n").unwrap();

        let manifest = check_manifest(Some(&file)).unwrap().unwrap();
        let config = resolve_lint_config(Some(&manifest), &[]).unwrap();

        assert_eq!(
            config.level(LintId::PublicInferredDynContract),
            LintLevel::Error
        );
    }

    #[test]
    fn lint_config_rejects_unknown_lints() {
        let error = resolve_lint_config(None, &["unused_variable=warn".to_string()]).unwrap_err();

        assert!(
            error.contains("unknown lint or group 'unused_variable'"),
            "{error}"
        );
        assert!(error.contains("internal_access"), "{error}");
    }

    #[test]
    fn manifest_lints_reject_warn_as_error_policy() {
        for name in ["warn_as_error", "warnings_as_errors"] {
            let manifest = manifest_with_lints(&[(name, "warn")]);
            let error = resolve_lint_config(Some(&manifest), &[]).unwrap_err();

            assert!(
                error.contains(&format!("unknown lint or group '{name}'")),
                "{error}"
            );
        }
    }

    #[test]
    fn legacy_lint_config_converts_internal_access_only() {
        let mut config = LintConfig::default();
        config.apply_override("api=error").unwrap();
        config.apply_override("deprecated=allow").unwrap();

        let legacy = legacy_lint_config(&config);

        assert_eq!(legacy.internal_access, anvyx_lang::LintLevel::Error);
    }

    #[test]
    fn legacy_lint_config_accepts_internal_access() {
        let lint =
            resolve_legacy_lint_config(None, &["internal_access=error".to_string()]).unwrap();

        assert_eq!(lint.internal_access, anvyx_lang::LintLevel::Error);
    }

    #[test]
    fn legacy_lint_config_rejects_clean_lints_and_groups() {
        for override_text in ["deprecated=allow", "api=error", "all=warn"] {
            let error = resolve_legacy_lint_config(None, &[override_text.to_string()]).unwrap_err();

            assert!(
                error.contains("is not supported by the legacy frontend"),
                "{error}"
            );
        }
    }

    #[test]
    fn legacy_lint_config_rejects_manifest_clean_lints() {
        let manifest = manifest_with_lints(&[("deprecated", "allow")]);

        let error = resolve_legacy_lint_config(Some(&manifest), &[]).unwrap_err();

        assert!(
            error.contains("is not supported by the legacy frontend"),
            "{error}"
        );
    }
}
