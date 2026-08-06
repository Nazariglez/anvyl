use std::{
    fs,
    path::{Path, PathBuf},
    time::{Duration, SystemTime, UNIX_EPOCH},
};

pub use anvyx_backend::rust::cargo_job::RustCargoProfile;
use anvyx_backend::rust::{
    RustPlanError, RustSource, RustTargetGap, RustTargetGaps,
    cargo_job::{
        self, RustCargoBatchCase, RustCargoBatchOutput, RustCargoCrateIdentityInput,
        RustCargoDependency, RustCargoDependencySource, RustCargoEvent, RustCargoFailure,
        RustCargoMode, RustCargoName, RustCargoOutput, RustCargoPackageMetadata,
        RustCargoPackageName, RustCargoSuccess, host_executable_name,
    },
};
use anvyx_lang::{AirBuildError, AirBuildOutput, DiagnosticReport, FrontendConfig};
use anvyx_runtime::{RustProviderSupport, validate_rust_provider_support};

use crate::{
    cache,
    check::{air_error_ref, build_air_path_typed, build_air_path_with_graph_typed},
    rust_deps,
};

#[derive(Debug, Clone)]
pub struct RunInput {
    pub file: PathBuf,
    pub frontend: FrontendConfig,
    pub cargo_profile: RustCargoProfile,
    pub cache_root: Option<PathBuf>,
}

#[derive(Debug, Clone)]
pub enum Event<'a> {
    Checking { file: &'a Path },
    Checked { report: &'a DiagnosticReport },
    GeneratingRust,
    CompilingRust,
    Running { file: &'a Path },
}

#[derive(Debug, Clone)]
pub struct BatchInput {
    pub cases: Vec<BatchCase>,
    pub cargo_profile: RustCargoProfile,
    pub cache_root: Option<PathBuf>,
    pub timeout: Option<Duration>,
}

#[derive(Debug, Clone)]
pub struct BatchCase {
    pub file: PathBuf,
    pub frontend: FrontendConfig,
}

pub struct BatchOutput {
    pub binaries: Vec<BatchBinary>,
}

pub struct BatchBinary {
    pub file: PathBuf,
    pub binary: PathBuf,
}

#[derive(Debug, Clone)]
pub struct BuildInput {
    pub file: PathBuf,
    pub project_root: PathBuf,
    pub project_name: String,
    pub frontend: FrontendConfig,
    pub cargo_profile: RustCargoProfile,
    pub cache_root: Option<PathBuf>,
    pub output_root: PathBuf,
}

#[derive(Debug)]
pub struct RustTargetDiagnostics {
    pub gaps: RustTargetGaps,
    pub report: DiagnosticReport,
}

impl std::fmt::Display for RustTargetDiagnostics {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.gaps)
    }
}

#[derive(Debug)]
pub enum Error {
    Air(AirBuildError<anvyx_lang::CheckError>),
    TargetDiagnostics(RustTargetDiagnostics),
    Plan(RustPlanError),
    Cargo(cargo_job::RustCargoError),
    Dependency(String),
    CargoBuild(Box<RustCargoFailure>),
    Run(Box<RustCargoFailure>),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Air(error) => write!(f, "{}", air_error_ref(error)),
            Self::TargetDiagnostics(output) => write!(f, "{output}"),
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

impl std::error::Error for Error {}

pub struct RunOutput {
    pub stdout: String,
    pub stderr: String,
}

pub struct BuildOutput {
    pub generated_manifest: PathBuf,
    pub generated_source: PathBuf,
    pub cargo_binary: PathBuf,
    pub artifact: PathBuf,
}

pub fn run(input: RunInput) -> Result<RunOutput, Error> {
    run_with_events(input, |_| {})
}

pub fn run_with_events(
    input: RunInput,
    mut events: impl for<'a> FnMut(&Event<'a>),
) -> Result<RunOutput, Error> {
    let cache_root = input
        .cache_root
        .unwrap_or_else(|| default_run_cache_root(&input.file));
    let output = execute(
        &input.file,
        input.frontend,
        cache_root,
        input.cargo_profile,
        RustCargoMode::Run,
        &mut events,
    )?;

    Ok(RunOutput {
        stdout: output.stdout,
        stderr: output.stderr,
    })
}

pub fn build_batch(input: BatchInput) -> Result<BatchOutput, String> {
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
                return Err("Rust batch cases must share a semantic profile".to_string());
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
            return Err(format!("duplicate Rust batch case name `{name}`"));
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
        RustCargoBatchOutput::Success(output) => Ok(BatchOutput {
            binaries: output
                .binaries
                .into_iter()
                .map(|(name, binary)| BatchBinary {
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

pub fn build(input: BuildInput) -> Result<BuildOutput, Error> {
    build_with_events(input, |_| {})
}

pub fn build_with_events(
    input: BuildInput,
    mut events: impl for<'a> FnMut(&Event<'a>),
) -> Result<BuildOutput, Error> {
    let cache_root = input
        .cache_root
        .unwrap_or_else(|| cache::rust_cache_root_for(&input.project_root));
    let output = execute(
        &input.file,
        input.frontend,
        cache_root,
        input.cargo_profile,
        RustCargoMode::Build,
        &mut events,
    )?;
    let project_name = sanitize_artifact_name(&input.project_name);
    let artifact = public_artifact_path(&input.output_root, input.cargo_profile, &project_name);
    publish_artifact(&output.binary_path, &artifact).map_err(Error::Cargo)?;

    Ok(BuildOutput {
        generated_manifest: output.manifest_path,
        generated_source: output.source_path,
        cargo_binary: output.binary_path,
        artifact,
    })
}

fn execute(
    file: &Path,
    frontend: FrontendConfig,
    cache_root: PathBuf,
    cargo_profile: RustCargoProfile,
    mode: RustCargoMode,
    events: &mut impl for<'a> FnMut(&Event<'a>),
) -> Result<RustCargoSuccess, Error> {
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
        RustCargoEvent::Compiling => events(&Event::CompilingRust),
        RustCargoEvent::Running => events(&Event::Running { file }),
    })
    .map_err(Error::Cargo)?
    {
        RustCargoOutput::Success(output) => Ok(output),
        RustCargoOutput::CargoFailed(output) => Err(Error::CargoBuild(Box::new(output))),
        RustCargoOutput::RunFailed(output) => Err(Error::Run(Box::new(output))),
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

fn cargo_metadata_for_file(file: &Path) -> Result<RustCargoPackageMetadata, Error> {
    let Some(path) = crate::manifest::find_nearest_manifest(file).map_err(Error::Dependency)?
    else {
        return Ok(RustCargoPackageMetadata::default());
    };
    let manifest = crate::manifest::parse_manifest_file(&path).map_err(Error::Dependency)?;
    Ok(RustCargoPackageMetadata {
        version: manifest
            .project
            .version
            .unwrap_or_else(|| cargo_job::DEFAULT_CARGO_VERSION.to_string()),
        ..RustCargoPackageMetadata::default()
    })
}

fn stable_file_seed(file: &Path) -> Result<String, Error> {
    let file = file.canonicalize().map_err(|error| {
        Error::Dependency(format!(
            "failed to canonicalize {}: {error}",
            file.display()
        ))
    })?;
    if let Some(manifest) =
        crate::manifest::find_nearest_manifest(&file).map_err(Error::Dependency)?
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

pub fn sanitize_artifact_name(raw: &str) -> String {
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

fn semantic_profile_name(profile: anvyx_lang::Profile) -> &'static str {
    match profile {
        anvyx_lang::Profile::Debug => "debug",
        anvyx_lang::Profile::Release => "release",
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
) -> Result<EmittedRustSource, Error> {
    emit_source_with_events(file, frontend, native, &mut |_| {})
}

fn emit_source_with_events(
    file: &Path,
    frontend: FrontendConfig,
    native: &NativeProviderContext,
    events: &mut impl for<'a> FnMut(&Event<'a>),
) -> Result<EmittedRustSource, Error> {
    events(&Event::Checking { file });
    let output = match &native.graph {
        Some(graph) => build_air_path_with_graph_typed(file, frontend, graph),
        None => build_air_path_typed(file, frontend),
    }
    .map_err(Error::Air)?;
    let AirBuildOutput { report, air } = output;
    events(&Event::Checked { report: &report });
    let sources = report.sources;
    let native_providers = used_native_provider_supports(&air, &native.supports);
    events(&Event::GeneratingRust);
    let source = anvyx_backend::rust::generate(
        &air.as_verified(),
        anvyx_backend::rust::RustPlanConfig {
            native_providers: native_providers.clone(),
            ..anvyx_backend::rust::RustPlanConfig::default()
        },
    )
    .map_err(|error| match error {
        RustPlanError::TargetGaps(gaps) => {
            let diagnostics = gaps.iter().map(RustTargetGap::diagnostic).collect();
            Error::TargetDiagnostics(RustTargetDiagnostics {
                gaps,
                report: DiagnosticReport::new(sources, diagnostics).sorted(),
            })
        }
        error @ RustPlanError::InvalidPlan(_) => Error::Plan(error),
    })?;
    Ok(EmittedRustSource {
        source,
        native_providers,
    })
}

fn native_provider_context_for_file(
    file: &Path,
    cache_root: &Path,
) -> Result<NativeProviderContext, Error> {
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
) -> Result<Option<crate::manifest::PackageGraph>, Error> {
    let Some(manifest_path) =
        crate::manifest::find_nearest_manifest(file).map_err(Error::Dependency)?
    else {
        return Ok(None);
    };
    crate::manifest::load_package_graph_with_rust_cache(&manifest_path, cache_root.to_path_buf())
        .map(Some)
        .map_err(Error::Dependency)
}

fn system_native_provider_supports() -> Result<Vec<RustProviderSupport>, Error> {
    let descriptors = anvyx_core::provider_descriptors()
        .into_iter()
        .chain(anvyx_stdlib::provider_descriptors())
        .collect::<Vec<_>>();
    let supports = anvyx_core::rust_provider_supports()
        .into_iter()
        .chain(anvyx_stdlib::rust_provider_supports())
        .collect::<Vec<_>>();
    validate_rust_provider_support(&descriptors, &supports).map_err(Error::Dependency)?;
    Ok(supports)
}

fn used_native_provider_supports(
    air: &anvyx_lang::OwnedVerifiedProgram,
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
    air: &anvyx_lang::OwnedVerifiedProgram,
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
) -> Result<Vec<RustCargoDependency>, Error> {
    let mut dependencies = std::collections::BTreeMap::new();
    merge_dependency(&mut dependencies, rust_deps::runtime_dependency())?;
    for support in supports {
        let cargo = support.cargo.clone();
        let source = cargo
            .path
            .unwrap_or_else(|| system_provider_path(&cargo.manifest_key));
        let dep = RustCargoDependency {
            name: RustCargoName::parse(cargo.manifest_key.clone()).map_err(Error::Dependency)?,
            package: cargo
                .package
                .map(RustCargoPackageName::parse)
                .transpose()
                .map_err(Error::Dependency)?,
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
) -> Result<(), Error> {
    let key = dep.name.as_str().to_string();
    let Some(existing) = dependencies.get_mut(&key) else {
        dependencies.insert(key, dep);
        return Ok(());
    };
    if existing.package != dep.package
        || existing.source != dep.source
        || existing.default_features != dep.default_features
    {
        return Err(Error::Dependency(format!(
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
        "anvyx_core" => rust_deps::workspace_crate_path("core"),
        "anvyx_stdlib" => rust_deps::workspace_crate_path("stdlib"),
        _ => rust_deps::workspace_crate_path(manifest_key),
    }
}
