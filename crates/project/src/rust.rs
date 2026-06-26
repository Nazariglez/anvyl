use std::{
    fs,
    path::{Path, PathBuf},
    time::{Duration, SystemTime, UNIX_EPOCH},
};

pub use anvyx_backend::rust::cargo_job::RustCargoProfile;
use anvyx_backend::rust::{
    cargo_job::{
        self, RustCargoBatchCase, RustCargoBatchOutput, RustCargoCrateIdentityInput,
        RustCargoDependency, RustCargoDependencySource, RustCargoEvent, RustCargoFailure,
        RustCargoMode, RustCargoName, RustCargoOutput, RustCargoPackageMetadata,
        RustCargoPackageName, RustCargoSuccess, host_executable_name,
    },
    emit::RustSource,
};
use anvyx_lang2::{AirBuildError, AirBuildOutput, DiagnosticReport, FrontendConfig};
use anvyx_runtime::{RustProviderSupport, validate_rust_provider_support};

use crate::{
    cache,
    check::{air_error_ref, build_air_path_typed, build_air_path_with_graph_typed},
    rust_deps,
};

#[derive(Debug, Clone)]
pub struct CleanRustRunInput {
    pub file: PathBuf,
    pub frontend: FrontendConfig,
    pub cargo_profile: RustCargoProfile,
    pub cache_root: Option<PathBuf>,
}

#[derive(Debug, Clone)]
pub enum CleanRustEvent {
    Checking { file: PathBuf },
    Checked { report: DiagnosticReport },
    GeneratingRust,
    CompilingRust,
    Running { file: PathBuf },
}

#[derive(Debug, Clone)]
pub struct CleanRustBatchInput {
    pub cases: Vec<CleanRustBatchCase>,
    pub cargo_profile: RustCargoProfile,
    pub cache_root: Option<PathBuf>,
    pub timeout: Option<Duration>,
}

#[derive(Debug, Clone)]
pub struct CleanRustBatchCase {
    pub file: PathBuf,
    pub frontend: FrontendConfig,
}

pub struct CleanRustBatchOutput {
    pub binaries: Vec<CleanRustBatchBinary>,
}

pub struct CleanRustBatchBinary {
    pub file: PathBuf,
    pub binary: PathBuf,
}

#[derive(Debug, Clone)]
pub struct CleanRustBuildInput {
    pub file: PathBuf,
    pub project_root: PathBuf,
    pub project_name: String,
    pub frontend: FrontendConfig,
    pub cargo_profile: RustCargoProfile,
    pub cache_root: Option<PathBuf>,
    pub output_root: PathBuf,
}

#[derive(Debug)]
pub enum CleanRustError {
    Air(AirBuildError<anvyx_lang2::CheckError>),
    Plan(anvyx_backend::rust::RustPlanError),
    Cargo(cargo_job::RustCargoError),
    Dependency(String),
    CargoBuild(Box<RustCargoFailure>),
    Run(Box<RustCargoFailure>),
}

impl std::fmt::Display for CleanRustError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Air(error) => write!(f, "{}", air_error_ref(error)),
            Self::Plan(error) => write!(f, "{error}"),
            Self::Cargo(error) => write!(f, "{error}"),
            Self::Dependency(error) => write!(f, "{error}"),
            Self::CargoBuild(output) => {
                write!(f, "generated Rust Cargo build failed")?;
                if !output.stderr.is_empty() {
                    write!(f, "\n{}", output.stderr)?;
                }
                Ok(())
            }
            Self::Run(output) => {
                write!(f, "generated Rust binary failed")?;
                if !output.stderr.is_empty() {
                    write!(f, "\n{}", output.stderr)?;
                }
                Ok(())
            }
        }
    }
}

impl std::error::Error for CleanRustError {}

pub struct CleanRustRunOutput {
    pub stdout: String,
    pub stderr: String,
}

pub struct CleanRustBuildOutput {
    pub generated_manifest: PathBuf,
    pub generated_source: PathBuf,
    pub cargo_binary: PathBuf,
    pub artifact: PathBuf,
}

pub fn run_clean_rust(input: CleanRustRunInput) -> Result<CleanRustRunOutput, CleanRustError> {
    run_clean_rust_with_events(input, |_| {})
}

pub fn run_clean_rust_with_events(
    input: CleanRustRunInput,
    mut events: impl FnMut(CleanRustEvent),
) -> Result<CleanRustRunOutput, CleanRustError> {
    let cache_root = input
        .cache_root
        .unwrap_or_else(|| default_run_cache_root(&input.file));
    let output = execute_clean_rust(
        &input.file,
        input.frontend,
        cache_root,
        input.cargo_profile,
        RustCargoMode::Run,
        &mut events,
    )?;

    Ok(CleanRustRunOutput {
        stdout: output.stdout,
        stderr: output.stderr,
    })
}

pub fn build_clean_rust_batch(input: CleanRustBatchInput) -> Result<CleanRustBatchOutput, String> {
    let cargo_profile = input.cargo_profile;
    let cache_root = input
        .cache_root
        .unwrap_or_else(cache::default_rust_cache_root);
    let mut emitted_cases = vec![];
    let mut native_providers = vec![];
    let mut semantic_profile = None;
    for (index, case) in input.cases.into_iter().enumerate() {
        let profile = semantic_profile_name(case.frontend.context.profile);
        match semantic_profile {
            Some(existing) if existing != profile => {
                return Err("clean Rust batch cases must share a semantic profile".to_string());
            }
            None => semantic_profile = Some(profile),
            Some(_) => {}
        }
        let file = case.file;
        let native = native_provider_context_for_file(&file, &cache_root)
            .map_err(|error| error.to_string())?;
        let emitted =
            emit_source(&file, case.frontend, &native).map_err(|error| error.to_string())?;
        native_providers.extend(emitted.native_providers);
        emitted_cases.push((index, file, profile, emitted.source));
    }

    let dependencies =
        native_provider_dependencies(&native_providers).map_err(|error| error.to_string())?;
    let mut files = std::collections::HashMap::new();
    let mut cases = vec![];
    for (index, file, profile, source) in emitted_cases {
        let seed = format!("{index}\0{}\0{}", file.display(), source.as_str());
        let fingerprint = cargo_job::cargo_fingerprint(&cargo_job::RustCargoFingerprintInput {
            source: &seed,
            manifest_template: "batch-case-bin-name",
            semantic_profile: profile,
            cargo_profile,
            dependencies: &dependencies,
        });
        let name = cargo_job::batch_case_name(&fingerprint);
        if files.insert(name.clone(), file).is_some() {
            return Err(format!("duplicate clean Rust batch case name `{name}`"));
        }
        cases.push(RustCargoBatchCase { name, source });
    }
    let job = cargo_job::batch_job_with_dependencies(
        cases,
        cache_root,
        cargo_profile,
        semantic_profile.unwrap_or("debug"),
        dependencies,
    );
    match cargo_job::execute_batch_with_timeout(&job, input.timeout)
        .map_err(|error| error.to_string())?
    {
        RustCargoBatchOutput::Success(output) => Ok(CleanRustBatchOutput {
            binaries: output
                .binaries
                .into_iter()
                .map(|(name, binary)| CleanRustBatchBinary {
                    file: files
                        .remove(&name)
                        .expect("batch binary maps to input file"),
                    binary,
                })
                .collect(),
        }),
        RustCargoBatchOutput::CargoFailed(output) => Err(format!(
            "generated Rust Cargo batch build failed\n{}",
            output.stderr
        )),
    }
}

pub fn build_clean_rust(
    input: CleanRustBuildInput,
) -> Result<CleanRustBuildOutput, CleanRustError> {
    build_clean_rust_with_events(input, |_| {})
}

pub fn build_clean_rust_with_events(
    input: CleanRustBuildInput,
    mut events: impl FnMut(CleanRustEvent),
) -> Result<CleanRustBuildOutput, CleanRustError> {
    let cache_root = input
        .cache_root
        .unwrap_or_else(|| cache::rust_cache_root_for(&input.project_root));
    let output = execute_clean_rust(
        &input.file,
        input.frontend,
        cache_root,
        input.cargo_profile,
        RustCargoMode::Build,
        &mut events,
    )?;
    let project_name = clean_artifact_name(&input.project_name);
    let artifact = public_artifact_path(&input.output_root, input.cargo_profile, &project_name);
    publish_artifact(&output.binary_path, &artifact).map_err(CleanRustError::Cargo)?;

    Ok(CleanRustBuildOutput {
        generated_manifest: output.manifest_path,
        generated_source: output.source_path,
        cargo_binary: output.binary_path,
        artifact,
    })
}

fn execute_clean_rust(
    file: &Path,
    frontend: FrontendConfig,
    cache_root: PathBuf,
    cargo_profile: RustCargoProfile,
    mode: RustCargoMode,
    events: &mut impl FnMut(CleanRustEvent),
) -> Result<RustCargoSuccess, CleanRustError> {
    let semantic_profile = semantic_profile_name(frontend.context.profile);
    let native = native_provider_context_for_file(file, &cache_root)?;
    let metadata = cargo_metadata_for_file(file)?;
    let emitted = emit_source_with_events(file, frontend, &native, events)?;
    let dependencies = native_provider_dependencies(&emitted.native_providers)?;
    let seed = stable_file_seed(file)?;
    let crate_identity = cargo_job::single_program_crate_identity(&RustCargoCrateIdentityInput {
        seed: &seed,
        semantic_profile,
        cargo_profile,
        dependencies: &dependencies,
    });
    let job = cargo_job::single_program_job_configured(
        emitted.source,
        cache_root,
        cargo_profile,
        mode,
        semantic_profile,
        dependencies,
        metadata,
        Some(crate_identity),
    );

    match cargo_job::execute_with_events(&job, |event| match event {
        RustCargoEvent::Compiling => events(CleanRustEvent::CompilingRust),
        RustCargoEvent::Running => events(CleanRustEvent::Running {
            file: file.to_path_buf(),
        }),
    })
    .map_err(CleanRustError::Cargo)?
    {
        RustCargoOutput::Success(output) => Ok(output),
        RustCargoOutput::CargoFailed(output) => Err(CleanRustError::CargoBuild(Box::new(output))),
        RustCargoOutput::RunFailed(output) => Err(CleanRustError::Run(Box::new(output))),
    }
}

fn default_run_cache_root(file: &Path) -> PathBuf {
    match crate::manifest::find_nearest_manifest(file) {
        Ok(Some(manifest)) => manifest
            .parent()
            .map_or_else(cache::default_rust_cache_root, cache::rust_cache_root_for),
        Ok(None) | Err(_) => cache::default_rust_cache_root(),
    }
}

fn cargo_metadata_for_file(file: &Path) -> Result<RustCargoPackageMetadata, CleanRustError> {
    let Some(path) =
        crate::manifest::find_nearest_manifest(file).map_err(CleanRustError::Dependency)?
    else {
        return Ok(RustCargoPackageMetadata::default());
    };
    let manifest =
        crate::manifest::parse_manifest_file(&path).map_err(CleanRustError::Dependency)?;
    Ok(RustCargoPackageMetadata {
        version: manifest
            .project
            .version
            .unwrap_or_else(|| cargo_job::DEFAULT_CARGO_VERSION.to_string()),
        ..RustCargoPackageMetadata::default()
    })
}

fn stable_file_seed(file: &Path) -> Result<String, CleanRustError> {
    let file = file.canonicalize().map_err(|error| {
        CleanRustError::Dependency(format!(
            "failed to canonicalize {}: {error}",
            file.display()
        ))
    })?;
    if let Some(manifest) =
        crate::manifest::find_nearest_manifest(&file).map_err(CleanRustError::Dependency)?
    {
        let root = manifest.parent().expect("manifest path has parent");
        let relative = file.strip_prefix(root).unwrap_or(&file);
        return Ok(format!(
            "project\0{}\0{}",
            root.display(),
            relative.display()
        ));
    }
    Ok(format!("file\0{}", file.display()))
}

fn public_artifact_path(
    output_root: &Path,
    profile: RustCargoProfile,
    project_name: &str,
) -> PathBuf {
    output_root
        .join(profile.dir_name())
        .join(host_executable_name(project_name))
}

pub fn clean_artifact_name(raw: &str) -> String {
    let name = raw
        .to_lowercase()
        .chars()
        .map(|c| if c == ' ' { '-' } else { c })
        .filter(|c| c.is_ascii_alphanumeric() || *c == '-' || *c == '_')
        .collect::<String>();
    if name.is_empty() {
        "anvyx-project".to_string()
    } else {
        name
    }
}

fn publish_artifact(source: &Path, artifact: &Path) -> Result<(), cargo_job::RustCargoError> {
    let dir = artifact.parent().expect("artifact path has parent");
    fs::create_dir_all(dir)?;
    cargo_job::with_lock(artifact.with_extension("lock"), None, || {
        let stamp = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap_or_default()
            .as_nanos();
        let tmp = artifact.with_file_name(format!(
            ".{}.{}.tmp",
            artifact
                .file_name()
                .and_then(|name| name.to_str())
                .unwrap_or("artifact"),
            stamp
        ));
        fs::copy(source, &tmp)?;
        let _ = fs::remove_file(artifact);
        fs::rename(tmp, artifact)?;
        Ok(())
    })
}

fn semantic_profile_name(profile: anvyx_lang2::Profile) -> &'static str {
    match profile {
        anvyx_lang2::Profile::Debug => "debug",
        anvyx_lang2::Profile::Release => "release",
    }
}

struct EmittedRustSource {
    source: RustSource,
    native_providers: Vec<RustProviderSupport>,
}

struct NativeProviderContext {
    supports: Vec<RustProviderSupport>,
    graph: Option<crate::manifest::PackageGraph>,
}

fn emit_source(
    file: &Path,
    frontend: FrontendConfig,
    native: &NativeProviderContext,
) -> Result<EmittedRustSource, CleanRustError> {
    emit_source_with_events(file, frontend, native, &mut |_| {})
}

fn emit_source_with_events(
    file: &Path,
    frontend: FrontendConfig,
    native: &NativeProviderContext,
    events: &mut impl FnMut(CleanRustEvent),
) -> Result<EmittedRustSource, CleanRustError> {
    events(CleanRustEvent::Checking {
        file: file.to_path_buf(),
    });
    let output = match &native.graph {
        Some(graph) => build_air_path_with_graph_typed(file, frontend, graph),
        None => build_air_path_typed(file, frontend),
    }
    .map_err(CleanRustError::Air)?;
    let AirBuildOutput { report, air } = output;
    events(CleanRustEvent::Checked { report });
    let native_providers = used_native_provider_supports(&air, &native.supports);
    events(CleanRustEvent::GeneratingRust);
    let plan = anvyx_backend::rust::plan(
        &air.as_verified(),
        anvyx_backend::rust::RustPlanConfig {
            native_providers: native_providers.clone(),
            ..anvyx_backend::rust::RustPlanConfig::default()
        },
    )
    .map_err(CleanRustError::Plan)?;
    Ok(EmittedRustSource {
        source: anvyx_backend::rust::emit::emit(&plan.verified()),
        native_providers,
    })
}

fn native_provider_context_for_file(
    file: &Path,
    cache_root: &Path,
) -> Result<NativeProviderContext, CleanRustError> {
    let mut supports = system_native_provider_supports()?;
    let graph = load_provider_graph(file, cache_root)?;
    if let Some(graph) = &graph {
        supports.extend(graph.rust_provider_supports());
    }
    Ok(NativeProviderContext { supports, graph })
}

fn load_provider_graph(
    file: &Path,
    cache_root: &Path,
) -> Result<Option<crate::manifest::PackageGraph>, CleanRustError> {
    let Some(manifest_path) =
        crate::manifest::find_nearest_manifest(file).map_err(CleanRustError::Dependency)?
    else {
        return Ok(None);
    };
    crate::manifest::load_package_graph_with_rust_cache(&manifest_path, cache_root.to_path_buf())
        .map(Some)
        .map_err(CleanRustError::Dependency)
}

fn system_native_provider_supports() -> Result<Vec<RustProviderSupport>, CleanRustError> {
    let descriptors = anvyx_core2::provider_descriptors()
        .into_iter()
        .chain(anvyx_stdlib2::provider_descriptors())
        .collect::<Vec<_>>();
    let supports = anvyx_core2::rust_provider_supports()
        .into_iter()
        .chain(anvyx_stdlib2::rust_provider_supports())
        .collect::<Vec<_>>();
    validate_rust_provider_support(&descriptors, &supports).map_err(CleanRustError::Dependency)?;
    Ok(supports)
}

fn used_native_provider_supports(
    air: &anvyx_lang2::OwnedVerifiedProgram,
    supports: &[RustProviderSupport],
) -> Vec<RustProviderSupport> {
    let used = used_native_provider_keys(air);
    supports
        .iter()
        .filter(|support| used.contains(&(support.package.clone(), support.provider.name.clone())))
        .cloned()
        .collect()
}

fn used_native_provider_keys(
    air: &anvyx_lang2::OwnedVerifiedProgram,
) -> std::collections::BTreeSet<(String, String)> {
    let program = air.program();
    let mut keys = std::collections::BTreeSet::new();
    for decl in &program.externs {
        if let Some(binding) = &decl.binding {
            keys.insert((binding.package.to_string(), binding.provider.name.clone()));
        }
    }
    for decl in &program.extern_types {
        if let Some(binding) = &decl.binding {
            keys.insert((binding.package.to_string(), binding.provider.name.clone()));
        }
    }
    keys
}

fn native_provider_dependencies(
    supports: &[RustProviderSupport],
) -> Result<Vec<RustCargoDependency>, CleanRustError> {
    let mut dependencies = std::collections::BTreeMap::new();
    merge_dependency(&mut dependencies, rust_deps::runtime_dependency())?;
    for support in supports {
        let cargo = support.cargo.clone();
        let source = cargo
            .path
            .unwrap_or_else(|| system_provider_path(&cargo.manifest_key));
        let dep = RustCargoDependency {
            name: RustCargoName::parse(cargo.manifest_key.clone())
                .map_err(CleanRustError::Dependency)?,
            package: cargo
                .package
                .map(RustCargoPackageName::parse)
                .transpose()
                .map_err(CleanRustError::Dependency)?,
            source: RustCargoDependencySource::Path(source.display().to_string()),
            features: cargo.features,
            default_features: cargo.default_features,
        };
        merge_dependency(&mut dependencies, dep)?;
    }
    Ok(dependencies.into_values().collect())
}

fn merge_dependency(
    dependencies: &mut std::collections::BTreeMap<String, RustCargoDependency>,
    dep: RustCargoDependency,
) -> Result<(), CleanRustError> {
    let key = dep.name.as_str().to_string();
    let Some(existing) = dependencies.get_mut(&key) else {
        dependencies.insert(key, dep);
        return Ok(());
    };
    if existing.package != dep.package
        || existing.source != dep.source
        || existing.default_features != dep.default_features
    {
        return Err(CleanRustError::Dependency(format!(
            "conflicting native provider Cargo dependency `{key}`"
        )));
    }
    existing.features.extend(dep.features);
    existing.features.sort();
    existing.features.dedup();
    Ok(())
}

fn system_provider_path(manifest_key: &str) -> PathBuf {
    match manifest_key {
        "anvyx_core2" => rust_deps::workspace_crate_path("core2"),
        "anvyx_stdlib2" => rust_deps::workspace_crate_path("stdlib2"),
        _ => rust_deps::workspace_crate_path(manifest_key),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn write_project(root: &Path, name: &str) -> PathBuf {
        fs::write(
            root.join("anvyx.toml"),
            format!("[project]\nname = \"{name}\"\nentry = \"main.anv\"\n"),
        )
        .unwrap();
        let file = root.join("main.anv");
        fs::write(&file, "fn main() { println(42); }\n").unwrap();
        file
    }

    fn write_versioned_project(root: &Path, version: &str) -> PathBuf {
        fs::write(
            root.join("anvyx.toml"),
            format!("[project]\nname = \"demo\"\nversion = \"{version}\"\nentry = \"main.anv\"\n"),
        )
        .unwrap();
        let file = root.join("main.anv");
        fs::write(&file, "fn main() { println(42); }\n").unwrap();
        file
    }

    fn write_provider_crate(root: &Path, package: &str, cargo_name: &str, module: &str) {
        let dir = root.join(package);
        fs::create_dir_all(dir.join("src")).unwrap();
        fs::write(
            dir.join("Cargo.toml"),
            format!(
                "[package]\nname = \"{cargo_name}\"\nversion = \"0.1.0\"\nedition = \"2024\"\n\n[dependencies]\nanvyx-runtime = {{ path = \"{}\" }}\n",
                rust_deps::workspace_crate_path("runtime").display()
            ),
        )
        .unwrap();
        fs::write(
            dir.join("src/lib.rs"),
            format!(
                r#"use anvyx_runtime::function;

#[function]
pub fn ping() -> i64 {{ 1 }}

anvyx_runtime::builtin_module! {{
    name: "{module}",
    source: "",
    exports: [ping],
}}
"#
            ),
        )
        .unwrap();
    }

    fn write_native_dependency_project(root: &Path, main: &str) -> PathBuf {
        write_native_dependency_project_with_cargo(root, main, "native-host")
    }

    fn write_native_dependency_project_with_cargo(
        root: &Path,
        main: &str,
        cargo_name: &str,
    ) -> PathBuf {
        let game = root.join("game");
        fs::create_dir_all(game.join("src")).unwrap();
        fs::write(
            game.join("anvyx.toml"),
            "[project]\nname = \"game\"\nentry = \"src/main.anv\"\n\n[dependencies]\nhost = { path = \"../host\" }\n",
        )
        .unwrap();
        let file = game.join("src/main.anv");
        fs::write(&file, main).unwrap();
        fs::create_dir_all(root.join("host")).unwrap();
        fs::write(root.join("host/anvyx.toml"), "[project]\nname = \"host\"\n").unwrap();
        write_provider_crate(root, "host", cargo_name, "host");
        file
    }

    fn write_source_native_wrapper_project(root: &Path, main: &str) -> PathBuf {
        let game = root.join("game");
        fs::create_dir_all(game.join("src")).unwrap();
        fs::create_dir_all(root.join("colors/src")).unwrap();
        fs::write(
            game.join("anvyx.toml"),
            "[project]\nname = \"game\"\nentry = \"src/main.anv\"\n\n[dependencies]\ncolors = { path = \"../colors\" }\n",
        )
        .unwrap();
        let file = game.join("src/main.anv");
        fs::write(&file, main).unwrap();
        fs::write(
            root.join("colors/anvyx.toml"),
            "[project]\nname = \"colors\"\nentry = \"src/lib.anv\"\n",
        )
        .unwrap();
        fs::write(
            root.join("colors/src/lib.anv"),
            "import ext:colors_native { ping };\npub fn mix() -> int { ping() }\n",
        )
        .unwrap();
        write_provider_crate(root, "colors", "native-colors", "colors_native");
        file
    }

    fn build(root: &Path, profile: RustCargoProfile) -> CleanRustBuildOutput {
        build_clean_rust(CleanRustBuildInput {
            file: write_project(root, "demo"),
            project_root: root.to_path_buf(),
            project_name: "demo".to_string(),
            frontend: FrontendConfig::default(),
            cargo_profile: profile,
            cache_root: None,
            output_root: root.join("build"),
        })
        .unwrap()
    }

    fn system_native_context() -> NativeProviderContext {
        NativeProviderContext {
            supports: system_native_provider_supports().unwrap(),
            graph: None,
        }
    }

    #[test]
    fn emit_source_reports_pre_cargo_event_order() {
        let temp = tempfile::tempdir().unwrap();
        let file = temp.path().join("main.anv");
        fs::write(&file, "import helper;\n\nfn main() {}\n").unwrap();
        fs::write(temp.path().join("helper.anv"), "pub fn f() {}\n").unwrap();
        let mut events = vec![];
        let mut checked_has_warnings = false;

        emit_source_with_events(
            &file,
            FrontendConfig::default(),
            &system_native_context(),
            &mut |event| {
                if let CleanRustEvent::Checked { report } = &event {
                    checked_has_warnings = report.has_warnings();
                }
                events.push(clean_rust_event_name(&event));
            },
        )
        .unwrap();

        assert_eq!(events, ["checking", "checked", "generating"]);
        assert!(checked_has_warnings);
    }

    #[test]
    fn emit_source_reports_only_checking_before_frontend_failure() {
        let temp = tempfile::tempdir().unwrap();
        let file = temp.path().join("main.anv");
        fs::write(&file, "fn main() { } }\n").unwrap();
        let mut events = vec![];

        let Err(error) = emit_source_with_events(
            &file,
            FrontendConfig::default(),
            &system_native_context(),
            &mut |event| events.push(clean_rust_event_name(&event)),
        ) else {
            panic!("malformed source should fail");
        };

        assert!(matches!(
            error,
            CleanRustError::Air(AirBuildError::Diagnostic(_))
        ));
        assert_eq!(events, ["checking"]);
    }

    fn clean_rust_event_name(event: &CleanRustEvent) -> &'static str {
        match event {
            CleanRustEvent::Checking { .. } => "checking",
            CleanRustEvent::Checked { .. } => "checked",
            CleanRustEvent::GeneratingRust => "generating",
            CleanRustEvent::CompilingRust => "compiling",
            CleanRustEvent::Running { .. } => "running",
        }
    }

    #[test]
    fn native_provider_support_matches_descriptors() {
        validate_rust_provider_support(
            &anvyx_core2::provider_descriptors(),
            &anvyx_core2::rust_provider_supports(),
        )
        .unwrap();
    }

    #[test]
    fn clean_run_cache_uses_nearest_manifest_root() {
        let project = tempfile::tempdir().unwrap();
        let outside = tempfile::tempdir().unwrap();
        let file = write_project(project.path(), "demo");
        let old_dir = std::env::current_dir().unwrap();
        std::env::set_current_dir(outside.path()).unwrap();

        let root = default_run_cache_root(&file);

        std::env::set_current_dir(old_dir).unwrap();
        assert_eq!(
            root,
            project
                .path()
                .canonicalize()
                .unwrap()
                .join(".anvyx/cache/rust")
        );
    }

    #[test]
    fn batch_binary_names_are_fingerprinted() {
        let temp = tempfile::tempdir().unwrap();
        let first = temp.path().join("first.anv");
        let second = temp.path().join("second.anv");
        fs::write(&first, "fn main() { println(1); }\n").unwrap();
        fs::write(&second, "fn main() { println(2); }\n").unwrap();

        let output = build_clean_rust_batch(CleanRustBatchInput {
            cases: [first, second]
                .into_iter()
                .map(|file| CleanRustBatchCase {
                    file,
                    frontend: FrontendConfig::default(),
                })
                .collect(),
            cargo_profile: RustCargoProfile::Dev,
            cache_root: Some(temp.path().join("cache")),
            timeout: Some(Duration::from_mins(5)),
        })
        .unwrap();
        let names = output
            .binaries
            .iter()
            .map(|binary| {
                binary
                    .binary
                    .file_stem()
                    .unwrap()
                    .to_string_lossy()
                    .into_owned()
            })
            .collect::<Vec<_>>();

        assert_eq!(names.len(), 2);
        assert!(names.iter().all(|name| name.starts_with("case_")));
        assert!(
            !names
                .iter()
                .any(|name| name == "case_0" || name == "case_1")
        );
        assert_ne!(names[0], names[1]);
    }

    #[test]
    fn batch_builds_mixed_source_and_native_dependency_cases() {
        let source = tempfile::tempdir().unwrap();
        let source_file = source.path().join("main.anv");
        fs::write(&source_file, "fn main() { println(1); }\n").unwrap();

        let native = tempfile::tempdir().unwrap();
        let native_file = write_native_dependency_project(
            native.path(),
            "import pkg:host.host { ping }; fn main() { println(ping()); }\n",
        );

        let output = build_clean_rust_batch(CleanRustBatchInput {
            cases: [source_file, native_file]
                .into_iter()
                .map(|file| CleanRustBatchCase {
                    file,
                    frontend: FrontendConfig::default(),
                })
                .collect(),
            cargo_profile: RustCargoProfile::Dev,
            cache_root: Some(native.path().join("cache")),
            timeout: Some(Duration::from_mins(5)),
        })
        .unwrap();

        assert_eq!(output.binaries.len(), 2);
    }

    #[test]
    fn batch_native_provider_aliases_do_not_collide_across_graphs() {
        let first = tempfile::tempdir().unwrap();
        let first_file = write_native_dependency_project_with_cargo(
            first.path(),
            "import pkg:host.host { ping }; fn main() { println(ping()); }\n",
            "native-host-a",
        );
        let second = tempfile::tempdir().unwrap();
        let second_file = write_native_dependency_project_with_cargo(
            second.path(),
            "import pkg:host.host { ping }; fn main() { println(ping()); }\n",
            "native-host-b",
        );

        let output = build_clean_rust_batch(CleanRustBatchInput {
            cases: [first_file, second_file]
                .into_iter()
                .map(|file| CleanRustBatchCase {
                    file,
                    frontend: FrontendConfig::default(),
                })
                .collect(),
            cargo_profile: RustCargoProfile::Dev,
            cache_root: Some(first.path().join("cache")),
            timeout: Some(Duration::from_mins(5)),
        })
        .unwrap();

        assert_eq!(output.binaries.len(), 2);
    }

    #[test]
    fn native_provider_dependencies_include_system_crates_once() {
        let providers = system_native_provider_supports().unwrap();
        let deps = native_provider_dependencies(&providers).unwrap();

        assert_eq!(deps.len(), 3);
        let runtime = deps
            .iter()
            .find(|dep| dep.name.as_str() == "anvyx_runtime")
            .expect("runtime dependency");
        assert_eq!(runtime.package.as_ref().unwrap().as_str(), "anvyx-runtime");
        assert!(matches!(runtime.source, RustCargoDependencySource::Path(_)));
        let core = deps
            .iter()
            .find(|dep| dep.name.as_str() == "anvyx_core2")
            .expect("core dependency");
        assert_eq!(core.package.as_ref().unwrap().as_str(), "anvyx-core2");
        assert!(matches!(core.source, RustCargoDependencySource::Path(_)));
        let std = deps
            .iter()
            .find(|dep| dep.name.as_str() == "anvyx_stdlib2")
            .expect("stdlib dependency");
        assert_eq!(std.package.as_ref().unwrap().as_str(), "anvyx-stdlib2");
        assert!(matches!(std.source, RustCargoDependencySource::Path(_)));
    }

    #[test]
    fn emitted_dependencies_include_runtime_without_used_providers() {
        let temp = tempfile::tempdir().unwrap();
        let file = temp.path().join("main.anv");
        fs::write(&file, "fn main() {}\n").unwrap();

        let emitted =
            emit_source(&file, FrontendConfig::default(), &system_native_context()).unwrap();
        let deps = native_provider_dependencies(&emitted.native_providers).unwrap();

        assert_eq!(deps.len(), 1);
        assert_eq!(deps[0].name.as_str(), "anvyx_runtime");
    }

    #[test]
    fn native_provider_dependency_merge_unions_features_and_rejects_conflicts() {
        let mut deps = std::collections::BTreeMap::new();
        let mut a = RustCargoDependency {
            name: RustCargoName::parse("native_dep").unwrap(),
            package: Some(RustCargoPackageName::parse("native-dep").unwrap()),
            source: RustCargoDependencySource::Path("../native".to_string()),
            features: vec!["a".to_string()],
            default_features: true,
        };
        let mut b = a.clone();
        b.features = vec!["b".to_string(), "a".to_string()];

        merge_dependency(&mut deps, a.clone()).unwrap();
        merge_dependency(&mut deps, b).unwrap();
        assert_eq!(deps["native_dep"].features, ["a", "b"]);

        a.default_features = false;
        assert!(merge_dependency(&mut deps, a).is_err());
    }

    #[test]
    fn clean_rust_emits_core_string_option_return_conversion() {
        let temp = tempfile::tempdir().unwrap();
        let file = temp.path().join("main.anv");
        fs::write(&file, "fn main() { \"abcd\".substring(1, 2); }\n").unwrap();

        let source = emit_source(&file, FrontendConfig::default(), &system_native_context())
            .unwrap()
            .source
            .into_string();

        assert!(source.contains("str_substring"));
        assert!(source.contains(".map(|value|"));
    }

    #[test]
    fn clean_rust_emits_core_string_list_return_conversion() {
        let temp = tempfile::tempdir().unwrap();
        let file = temp.path().join("main.anv");
        fs::write(&file, "fn main() { \"a,b\".split(\",\"); }\n").unwrap();

        let source = emit_source(&file, FrontendConfig::default(), &system_native_context())
            .unwrap()
            .source
            .into_string();

        assert!(source.contains("str_split"));
        assert!(source.contains("AnvList::from_elems"));
        assert!(source.contains("AnvString::from"));
    }

    #[test]
    fn clean_run_calls_core2_runtime_native_extern() {
        let temp = tempfile::tempdir().unwrap();
        let file = temp.path().join("main.anv");
        fs::write(&file, "fn main() { println(42); }\n").unwrap();

        let mut events = vec![];
        let output = run_clean_rust_with_events(
            CleanRustRunInput {
                file,
                frontend: FrontendConfig::default(),
                cargo_profile: RustCargoProfile::Dev,
                cache_root: Some(temp.path().join("cache")),
            },
            |event| events.push(clean_rust_event_name(&event)),
        )
        .unwrap();

        assert_eq!(
            events,
            ["checking", "checked", "generating", "compiling", "running"]
        );
        assert_eq!(output.stdout, "42\n");
    }

    #[test]
    fn clean_build_copies_debug_artifact() {
        let temp = tempfile::tempdir().unwrap();
        let mut events = vec![];

        let output = build_clean_rust_with_events(
            CleanRustBuildInput {
                file: write_project(temp.path(), "demo"),
                project_root: temp.path().to_path_buf(),
                project_name: "demo".to_string(),
                frontend: FrontendConfig::default(),
                cargo_profile: RustCargoProfile::Dev,
                cache_root: None,
                output_root: temp.path().join("build"),
            },
            |event| events.push(clean_rust_event_name(&event)),
        )
        .unwrap();

        assert_eq!(events, ["checking", "checked", "generating", "compiling"]);
        assert_eq!(
            output.artifact,
            temp.path()
                .join("build")
                .join("debug")
                .join(host_executable_name("demo"))
        );
        assert!(output.artifact.exists());
        assert!(
            output
                .generated_manifest
                .starts_with(temp.path().join(".anvyx/cache/rust"))
        );
        assert!(!temp.path().join("build/runner").exists());
        assert!(!temp.path().join("build/dist").exists());
    }

    #[test]
    fn native_dependency_provider_cargo_uses_local_path_dependency() {
        let temp = tempfile::tempdir().unwrap();
        let file = write_native_dependency_project(
            temp.path(),
            "import pkg:host.host { ping }; fn main() { println(ping()); }\n",
        );

        let output = build_clean_rust(CleanRustBuildInput {
            file,
            project_root: temp.path().join("game"),
            project_name: "native_provider_game".to_string(),
            frontend: FrontendConfig::default(),
            cargo_profile: RustCargoProfile::Dev,
            cache_root: Some(temp.path().join("cache")),
            output_root: temp.path().join("build"),
        })
        .unwrap();
        let manifest = fs::read_to_string(output.generated_manifest).unwrap();

        assert!(manifest.contains("anvyx_provider_host_"));
        assert!(manifest.contains("package = \"native-host\""));
        assert!(manifest.contains(&temp.path().join("host").display().to_string()));
        assert!(temp.path().join("cache/target").is_dir());
        assert!(!temp.path().join("game/.anvyx").exists());
    }

    #[test]
    fn native_dependency_provider_support_matches_air_binding_package() {
        let temp = tempfile::tempdir().unwrap();
        let file = write_native_dependency_project(
            temp.path(),
            "import pkg:host.host { ping }; fn main() { let x: int = ping(); }\n",
        );
        let manifest = crate::manifest::find_nearest_manifest(&file)
            .unwrap()
            .unwrap();
        let graph = crate::manifest::load_package_graph(&manifest).unwrap();
        let air = build_air_path_with_graph_typed(&file, FrontendConfig::default(), &graph)
            .unwrap()
            .air;
        let used = used_native_provider_keys(&air);
        let supports = graph.rust_provider_supports();

        assert!(used.iter().all(|key| {
            supports
                .iter()
                .any(|support| (support.package.clone(), support.provider.name.clone()) == *key)
        }));
    }

    #[test]
    fn source_native_provider_dependency_is_emitted_only_when_used() {
        let unused = tempfile::tempdir().unwrap();
        let unused_file = write_source_native_wrapper_project(unused.path(), "fn main() {}\n");
        let unused_output = build_clean_rust(CleanRustBuildInput {
            file: unused_file,
            project_root: unused.path().join("game"),
            project_name: "unused".to_string(),
            frontend: FrontendConfig::default(),
            cargo_profile: RustCargoProfile::Dev,
            cache_root: Some(unused.path().join("cache")),
            output_root: unused.path().join("build"),
        })
        .unwrap();
        let unused_manifest = fs::read_to_string(unused_output.generated_manifest).unwrap();
        assert!(!unused_manifest.contains("native-colors"));

        let used = tempfile::tempdir().unwrap();
        let used_file = write_source_native_wrapper_project(
            used.path(),
            "import pkg:colors { mix }; fn main() { println(mix()); }\n",
        );
        let used_output = build_clean_rust(CleanRustBuildInput {
            file: used_file,
            project_root: used.path().join("game"),
            project_name: "used".to_string(),
            frontend: FrontendConfig::default(),
            cargo_profile: RustCargoProfile::Dev,
            cache_root: Some(used.path().join("cache")),
            output_root: used.path().join("build"),
        })
        .unwrap();
        let used_manifest = fs::read_to_string(used_output.generated_manifest).unwrap();
        assert!(used_manifest.contains("native-colors"));
    }

    #[test]
    fn clean_run_calls_native_dependency_provider() {
        let temp = tempfile::tempdir().unwrap();
        let file = write_native_dependency_project(
            temp.path(),
            "import pkg:host.host { ping }; fn main() { println(ping()); }\n",
        );

        let output = run_clean_rust(CleanRustRunInput {
            file,
            frontend: FrontendConfig::default(),
            cargo_profile: RustCargoProfile::Dev,
            cache_root: Some(temp.path().join("cache")),
        })
        .unwrap();

        assert_eq!(output.stdout, "1\n");
    }

    #[test]
    fn clean_build_copies_release_artifact() {
        let temp = tempfile::tempdir().unwrap();

        let output = build(temp.path(), RustCargoProfile::Release);

        assert_eq!(
            output.artifact,
            temp.path()
                .join("build")
                .join("release")
                .join(host_executable_name("demo"))
        );
        assert!(output.artifact.exists());
    }

    #[test]
    fn clean_build_repeats_with_stable_public_path() {
        let temp = tempfile::tempdir().unwrap();

        let first = build(temp.path(), RustCargoProfile::Dev);
        let second = build(temp.path(), RustCargoProfile::Dev);

        assert_eq!(first.artifact, second.artifact);
        assert_eq!(first.generated_manifest, second.generated_manifest);
    }

    #[test]
    fn clean_rust_metadata_defaults_without_project_version() {
        let standalone_root = tempfile::tempdir().unwrap();
        let project_root = tempfile::tempdir().unwrap();
        let standalone = standalone_root.path().join("standalone.anv");
        fs::write(&standalone, "fn main() {}\n").unwrap();
        let project = write_project(project_root.path(), "demo");

        assert_eq!(
            cargo_metadata_for_file(&standalone).unwrap().version,
            cargo_job::DEFAULT_CARGO_VERSION
        );
        assert_eq!(
            cargo_metadata_for_file(&project).unwrap().version,
            cargo_job::DEFAULT_CARGO_VERSION
        );
    }

    #[test]
    fn clean_rust_manifest_uses_project_version() {
        let temp = tempfile::tempdir().unwrap();
        let file = write_versioned_project(temp.path(), "1.2.3");

        let output = build_clean_rust(CleanRustBuildInput {
            file,
            project_root: temp.path().to_path_buf(),
            project_name: "demo".to_string(),
            frontend: FrontendConfig::default(),
            cargo_profile: RustCargoProfile::Dev,
            cache_root: None,
            output_root: temp.path().join("build"),
        })
        .unwrap();
        let manifest = fs::read_to_string(output.generated_manifest).unwrap();

        assert!(manifest.contains("version = \"1.2.3\""));
        assert!(manifest.contains("edition = \"2024\""));
    }

    #[test]
    fn clean_rust_reuses_generated_crate_after_source_edit() {
        let temp = tempfile::tempdir().unwrap();
        let file = write_project(temp.path(), "demo");
        let build_input = |file: PathBuf| CleanRustBuildInput {
            file,
            project_root: temp.path().to_path_buf(),
            project_name: "demo".to_string(),
            frontend: FrontendConfig::default(),
            cargo_profile: RustCargoProfile::Dev,
            cache_root: None,
            output_root: temp.path().join("build"),
        };

        let first = build_clean_rust(build_input(file.clone())).unwrap();
        fs::write(&file, "fn main() { println(7); }\n").unwrap();
        let second = build_clean_rust(build_input(file)).unwrap();

        assert_eq!(first.generated_manifest, second.generated_manifest);
        assert_eq!(first.generated_source, second.generated_source);
        assert!(
            fs::read_to_string(second.generated_source)
                .unwrap()
                .contains('7')
        );
    }

    #[test]
    fn clean_build_sanitizes_public_artifact_name() {
        let temp = tempfile::tempdir().unwrap();
        let output = build_clean_rust(CleanRustBuildInput {
            file: write_project(temp.path(), "../Bad Name"),
            project_root: temp.path().to_path_buf(),
            project_name: "../Bad Name".to_string(),
            frontend: FrontendConfig::default(),
            cargo_profile: RustCargoProfile::Dev,
            cache_root: None,
            output_root: temp.path().join("build"),
        })
        .unwrap();

        assert_eq!(
            output.artifact,
            temp.path()
                .join("build")
                .join("debug")
                .join(host_executable_name("bad-name"))
        );
    }
}
