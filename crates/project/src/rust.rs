use std::{
    fs,
    path::{Path, PathBuf},
    time::{Duration, SystemTime, UNIX_EPOCH},
};

const STALE_ARTIFACT_LOCK_AFTER: Duration = Duration::from_mins(30);

pub use anvyx_backend::rust::cargo_job::RustCargoProfile;
use anvyx_backend::rust::{
    cargo_job::{
        self, LockFile, RustCargoBatchCase, RustCargoBatchOutput, RustCargoCrateIdentityInput,
        RustCargoDependency, RustCargoDependencySource, RustCargoFailure, RustCargoMode,
        RustCargoName, RustCargoOutput, RustCargoPackageMetadata, RustCargoPackageName,
        RustCargoSuccess, host_executable_name,
    },
    emit::RustSource,
};
use anvyx_lang2::{AirBuildError, FrontendConfig};
use anvyx_runtime::{RustProviderCargo, RustProviderSupport, validate_rust_provider_support};

use crate::check::{air_error_ref, build_air_path_typed};

#[derive(Debug, Clone)]
pub struct CleanRustRunInput {
    pub file: PathBuf,
    pub frontend: FrontendConfig,
    pub cargo_profile: RustCargoProfile,
    pub cache_root: Option<PathBuf>,
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
    CargoBuild(RustCargoFailure),
    Run(RustCargoFailure),
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
    let cache_root = input
        .cache_root
        .unwrap_or_else(|| default_run_cache_root(&input.file));
    let output = execute_clean_rust(
        input.file,
        input.frontend,
        cache_root,
        input.cargo_profile,
        RustCargoMode::Run,
    )?;

    Ok(CleanRustRunOutput {
        stdout: output.stdout,
        stderr: output.stderr,
    })
}

pub fn build_clean_rust_batch(input: CleanRustBatchInput) -> Result<CleanRustBatchOutput, String> {
    let cargo_profile = input.cargo_profile;
    let cache_root = input.cache_root.unwrap_or_else(default_cache_root);
    reject_manifest_provider_batch_cases(&input.cases)?;
    let native = NativeProviderContext {
        supports: system_native_provider_supports().map_err(|error| error.to_string())?,
        manifest: None,
    };
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
        let emitted =
            emit_source(file.clone(), case.frontend, &native).map_err(|error| error.to_string())?;
        native_providers.extend(emitted.native_providers.clone());
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
    let cache_root = input
        .cache_root
        .unwrap_or_else(|| default_cache_root_for(&input.project_root));
    let output = execute_clean_rust(
        input.file,
        input.frontend,
        cache_root,
        input.cargo_profile,
        RustCargoMode::Build,
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
    file: PathBuf,
    frontend: FrontendConfig,
    cache_root: PathBuf,
    cargo_profile: RustCargoProfile,
    mode: RustCargoMode,
) -> Result<RustCargoSuccess, CleanRustError> {
    let semantic_profile = semantic_profile_name(frontend.context.profile);
    let native = native_provider_context_for_file(&file)?;
    let metadata = cargo_metadata_for_file(&file)?;
    let emitted = emit_source(file.clone(), frontend, &native)?;
    let dependencies = native_provider_dependencies(&emitted.native_providers)?;
    let seed = stable_file_seed(&file)?;
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

    match cargo_job::execute(&job).map_err(CleanRustError::Cargo)? {
        RustCargoOutput::Success(output) => Ok(output),
        RustCargoOutput::CargoFailed(output) => Err(CleanRustError::CargoBuild(output)),
        RustCargoOutput::RunFailed(output) => Err(CleanRustError::Run(output)),
    }
}

fn default_cache_root() -> PathBuf {
    std::env::current_dir()
        .map_or_else(|_| std::env::temp_dir(), |dir| default_cache_root_for(&dir))
}

fn default_run_cache_root(file: &Path) -> PathBuf {
    match crate::manifest::find_nearest_manifest(file) {
        Ok(Some(manifest)) => manifest
            .parent()
            .map_or_else(default_cache_root, default_cache_root_for),
        Ok(None) | Err(_) => default_cache_root(),
    }
}

fn default_cache_root_for(project_root: &Path) -> PathBuf {
    if let Some(path) = std::env::var_os("ANVYX_CACHE_DIR") {
        return PathBuf::from(path);
    }
    project_root.join(".anvyx").join("cache").join("rust")
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

fn clean_artifact_name(raw: &str) -> String {
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
    let _lock = LockFile::acquire_stale(
        artifact.with_extension("lock"),
        None,
        STALE_ARTIFACT_LOCK_AFTER,
    )?;
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
}

fn semantic_profile_name(profile: anvyx_lang2::Profile) -> &'static str {
    match profile {
        anvyx_lang2::Profile::Debug => "debug",
        anvyx_lang2::Profile::Release => "release",
    }
}

fn reject_manifest_provider_batch_cases(cases: &[CleanRustBatchCase]) -> Result<(), String> {
    for case in cases {
        let Some(path) = crate::manifest::find_nearest_manifest(&case.file)? else {
            continue;
        };
        let manifest = crate::manifest::parse_manifest_file(&path)?;
        if manifest.has_externs() {
            return Err(format!(
                "clean Rust batch does not support manifest extern providers: {}",
                case.file.display()
            ));
        }
    }
    Ok(())
}

struct EmittedRustSource {
    source: RustSource,
    native_providers: Vec<RustProviderSupport>,
}

struct NativeProviderContext {
    supports: Vec<RustProviderSupport>,
    manifest: Option<ManifestNativeProviderContext>,
}

struct ManifestNativeProviderContext {
    path: PathBuf,
    load: crate::manifest::NativeProviderLoad,
}

fn emit_source(
    file: PathBuf,
    frontend: FrontendConfig,
    native: &NativeProviderContext,
) -> Result<EmittedRustSource, CleanRustError> {
    let output = match &native.manifest {
        Some(manifest) => crate::check::build_air_path_with_loaded_native_providers_typed(
            &file,
            frontend,
            &manifest.path,
            &manifest.load,
        ),
        None => build_air_path_typed(&file, frontend),
    }
    .map_err(CleanRustError::Air)?;
    let native_providers = used_native_provider_supports(&output.air, &native.supports);
    let plan = anvyx_backend::rust::plan(
        &output.air.as_verified(),
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

fn native_provider_context_for_file(file: &Path) -> Result<NativeProviderContext, CleanRustError> {
    let mut supports = system_native_provider_supports()?;
    let manifest = manifest_native_provider_context(file)?;
    if let Some(manifest) = &manifest {
        supports.extend(manifest_native_provider_supports(
            &manifest.path,
            &manifest.load,
        )?);
    }
    Ok(NativeProviderContext { supports, manifest })
}

fn manifest_native_provider_context(
    file: &Path,
) -> Result<Option<ManifestNativeProviderContext>, CleanRustError> {
    let Some(manifest_path) =
        crate::manifest::find_nearest_manifest(file).map_err(CleanRustError::Dependency)?
    else {
        return Ok(None);
    };
    let manifest =
        crate::manifest::parse_manifest_file(&manifest_path).map_err(CleanRustError::Dependency)?;
    let load = crate::manifest::load_native_providers(&manifest_path, &manifest)
        .map_err(CleanRustError::Dependency)?;
    Ok(Some(ManifestNativeProviderContext {
        path: manifest_path,
        load,
    }))
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

fn manifest_native_provider_supports(
    manifest_path: &Path,
    load: &crate::manifest::NativeProviderLoad,
) -> Result<Vec<RustProviderSupport>, CleanRustError> {
    if load.providers.is_empty() {
        return Ok(vec![]);
    }
    let graph =
        crate::manifest::load_package_graph(manifest_path).map_err(CleanRustError::Dependency)?;
    let package = graph.root().id.to_string();
    let descriptors = load
        .providers
        .iter()
        .map(|provider| provider.descriptor.clone())
        .collect::<Vec<_>>();
    let supports = load
        .providers
        .iter()
        .map(|provider| {
            let mut modules = provider.supports.clone();
            for module in &mut modules {
                for ty in &mut module.types {
                    ty.path.crate_name.clone_from(&provider.cargo_alias);
                }
                for binding in &mut module.bindings {
                    binding.path.crate_name.clone_from(&provider.cargo_alias);
                }
            }
            RustProviderSupport {
                package: package.clone(),
                provider: provider.descriptor.provider.clone(),
                cargo: RustProviderCargo {
                    manifest_key: provider.cargo_alias.clone(),
                    package: Some(provider.cargo_package.clone()),
                    path: Some(provider.crate_root.clone()),
                    features: vec![],
                    default_features: true,
                },
                modules,
            }
        })
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
    merge_dependency(&mut dependencies, runtime_dependency())?;
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

fn runtime_dependency() -> RustCargoDependency {
    RustCargoDependency {
        name: RustCargoName::parse("anvyx_runtime").expect("valid runtime crate name"),
        package: Some(
            RustCargoPackageName::parse("anvyx-runtime").expect("valid runtime package name"),
        ),
        source: RustCargoDependencySource::Path(
            workspace_crate_path("runtime").display().to_string(),
        ),
        features: vec![],
        default_features: true,
    }
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
        "anvyx_core2" => workspace_crate_path("core2"),
        "anvyx_stdlib2" => workspace_crate_path("stdlib2"),
        _ => workspace_crate_path(manifest_key),
    }
}

fn workspace_crate_path(name: &str) -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("project crate lives below workspace crates directory")
        .join(name)
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
            manifest: None,
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
            timeout: Some(Duration::from_mins(1)),
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
            emit_source(file, FrontendConfig::default(), &system_native_context()).unwrap();
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

        let source = emit_source(file, FrontendConfig::default(), &system_native_context())
            .unwrap()
            .source
            .into_string();

        assert!(source.contains("str_substring"));
        assert!(source.contains("Some(value)"));
        assert!(source.contains("None =>"));
    }

    #[test]
    fn clean_rust_rejects_core_string_list_return_before_emission() {
        let temp = tempfile::tempdir().unwrap();
        let file = temp.path().join("main.anv");
        fs::write(&file, "fn main() { \"a,b\".split(\",\"); }\n").unwrap();

        let Err(CleanRustError::Plan(anvyx_backend::rust::RustPlanError::TargetGaps(gaps))) =
            emit_source(file, FrontendConfig::default(), &system_native_context())
        else {
            panic!("str_split should be rejected by Rust backend planning");
        };

        assert!(
            gaps.iter().any(|gap| {
                gap.kind == anvyx_backend::rust::RustTargetGapKind::UnsupportedRustAbi
            })
        );
    }

    #[test]
    fn clean_run_calls_core2_runtime_native_extern() {
        let temp = tempfile::tempdir().unwrap();
        let file = temp.path().join("main.anv");
        fs::write(&file, "fn main() { println(42); }\n").unwrap();

        let output = run_clean_rust(CleanRustRunInput {
            file,
            frontend: FrontendConfig::default(),
            cargo_profile: RustCargoProfile::Dev,
            cache_root: Some(temp.path().join("cache")),
        })
        .unwrap();

        assert_eq!(output.stdout, "42\n");
    }

    #[test]
    fn clean_build_copies_debug_artifact() {
        let temp = tempfile::tempdir().unwrap();

        let output = build(temp.path(), RustCargoProfile::Dev);

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
    fn manifest_provider_cargo_uses_local_path_dependency() {
        let root = Path::new(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .and_then(Path::parent)
            .expect("project crate lives below workspace root");
        let project = root.join("tests/run2/externs/local_provider");
        let temp = tempfile::tempdir().unwrap();
        let output = build_clean_rust(CleanRustBuildInput {
            file: project.join("src/main.anv"),
            project_root: project.clone(),
            project_name: "local_provider_game".to_string(),
            frontend: FrontendConfig::default(),
            cargo_profile: RustCargoProfile::Dev,
            cache_root: Some(temp.path().join("cache")),
            output_root: temp.path().join("build"),
        })
        .unwrap();
        let manifest = fs::read_to_string(output.generated_manifest).unwrap();

        assert!(manifest.contains("anvyx_provider_host"));
        assert!(manifest.contains("package = \"anvyx-test-host\""));
        assert!(manifest.contains("local_provider/provider"));
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
