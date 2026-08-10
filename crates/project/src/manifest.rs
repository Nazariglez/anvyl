use std::{
    collections::{BTreeMap, HashMap},
    fs,
    path::{Path, PathBuf},
    time::Duration,
};

use anvyx_backend::rust::{
    RustSource,
    cargo_job::{
        self, RustCargoCrateIdentity, RustCargoCrateIdentityInput, RustCargoDependency,
        RustCargoDependencySource, RustCargoError, RustCargoJob, RustCargoMode, RustCargoName,
        RustCargoOutput, RustCargoPackageMetadata, RustCargoPackageName, RustCargoProfile,
    },
};
use anvyx_externs::{ProviderCatalog, ProviderPackageKey, RawProviderPackage};
use anvyx_lang::LintConfig;
use serde::Deserialize;

use crate::{cache, rust_deps};

const PROVIDER_PROBE_TIMEOUT_ENV: &str = "ANVYX_PROVIDER_PROBE_TIMEOUT_SECS";
const DEFAULT_PROVIDER_PROBE_TIMEOUT_SECS: u64 = 600;
const PROVIDER_PROBE_PROFILE: &str = "provider-probe-v4";
const PROVIDER_PROBE_SCHEMA: u32 = 4;

fn provider_probe_timeout() -> Result<Duration, String> {
    parse_provider_probe_timeout(std::env::var(PROVIDER_PROBE_TIMEOUT_ENV).ok().as_deref())
}

fn parse_provider_probe_timeout(value: Option<&str>) -> Result<Duration, String> {
    let Some(value) = value else {
        return Ok(Duration::from_secs(DEFAULT_PROVIDER_PROBE_TIMEOUT_SECS));
    };
    let seconds = value
        .parse::<u64>()
        .ok()
        .filter(|seconds| *seconds > 0)
        .ok_or_else(|| {
            format!(
                "invalid {PROVIDER_PROBE_TIMEOUT_ENV} '{value}': expected positive integer seconds"
            )
        })?;
    Ok(Duration::from_secs(seconds))
}

pub type ManifestLint = BTreeMap<String, String>;

#[derive(Debug, Deserialize)]
pub struct Manifest {
    pub project: Project,
    #[serde(default)]
    pub dependencies: HashMap<String, DependencyEntry>,
    #[serde(default)]
    pub lint: ManifestLint,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct NativeProviderProbeInput {
    package: PackageId,
    crate_root: PathBuf,
    cargo_package: String,
    cargo_alias: String,
}

fn native_provider_probe_input(
    package: PackageId,
    native: &NativePackageInfo,
) -> Result<NativeProviderProbeInput, String> {
    let cargo = parse_provider_cargo(&native.cargo_manifest, &package.to_string())?;
    Ok(NativeProviderProbeInput {
        cargo_alias: generated_provider_cargo_alias(&package),
        package,
        crate_root: native.crate_root.clone(),
        cargo_package: cargo.package.name,
    })
}

#[derive(Debug, Deserialize)]
struct ProviderCargoManifest {
    package: ProviderCargoPackage,
}

#[derive(Debug, Deserialize)]
struct ProviderCargoPackage {
    name: String,
}

#[derive(Debug, Deserialize)]
struct ProviderProbeEnvelope {
    schema: u32,
    package: RawProviderPackage,
}

fn parse_provider_cargo(path: &Path, label: &str) -> Result<ProviderCargoManifest, String> {
    let text = fs::read_to_string(path).map_err(|error| {
        format!(
            "failed to read native provider package `{label}` Cargo.toml {}: {error}",
            path.display()
        )
    })?;
    toml::from_str(&text).map_err(|error| {
        format!(
            "failed to parse native provider package `{label}` Cargo.toml {}: {error}",
            path.display()
        )
    })
}

fn load_native_package_provider(
    probe: NativeProviderProbeInput,
    cache_root: &Path,
) -> Result<ProviderPackageLoad, String> {
    let envelope = run_provider_probe(&probe, cache_root)?;
    if envelope.schema != PROVIDER_PROBE_SCHEMA {
        return Err(format!(
            "native provider package `{}` probe has unsupported schema {}; expected {PROVIDER_PROBE_SCHEMA}",
            probe.package, envelope.schema
        ));
    }
    Ok(ProviderPackageLoad {
        cargo: ProviderCargoSidecar {
            crate_alias: probe.cargo_alias.clone(),
            cargo_package: probe.cargo_package,
            crate_root: probe.crate_root,
        },
        input: (
            ProviderPackageKey(probe.package.to_string()),
            envelope.package,
            Some(probe.cargo_alias),
        ),
    })
}

fn run_provider_probe(
    provider: &NativeProviderProbeInput,
    cache_root: &Path,
) -> Result<ProviderProbeEnvelope, String> {
    let timeout = provider_probe_timeout()?;
    let job = provider_probe_job(provider, cache_root)?;
    let paths = job.paths();
    let label = provider.package.to_string();
    match cargo_job::execute_with_timeout(&job, timeout) {
        Ok(RustCargoOutput::Success(output)) => {
            serde_json::from_str(&output.stdout).map_err(|error| {
                format!(
                    "native provider package `{label}` probe emitted invalid metadata: {error}\nmanifest: {}\nstdout: {}",
                    output.manifest_path.display(),
                    text_excerpt(&output.stdout)
                )
            })
        }
        Ok(RustCargoOutput::CargoFailed(output)) => Err(format!(
            "native provider package `{label}` probe build failed{}\nmanifest: {}\ntarget: {}\nstderr: {}\nstdout: {}",
            status_suffix(output.status),
            output.manifest_path.display(),
            output.target_dir.display(),
            text_excerpt(&output.stderr),
            text_excerpt(&output.stdout)
        )),
        Ok(RustCargoOutput::RunFailed(output)) => Err(format!(
            "native provider package `{label}` probe failed{}\nbinary: {}\nstderr: {}\nstdout: {}",
            status_suffix(output.status),
            paths.binary_path.display(),
            text_excerpt(&output.stderr),
            text_excerpt(&output.stdout)
        )),
        Err(RustCargoError::Timeout) => Err(provider_probe_timeout_error(
            &label,
            cache_root,
            &paths.target_dir,
            timeout,
            None,
        )),
        Err(RustCargoError::LockTimeout(path)) => Err(provider_probe_timeout_error(
            &label,
            cache_root,
            &paths.target_dir,
            timeout,
            Some(&path),
        )),
        Err(error) => Err(format!(
            "failed to run native provider package `{label}` probe\nmanifest: {}\ntarget: {}\n{error}",
            paths.manifest_path.display(),
            paths.target_dir.display()
        )),
    }
}

fn provider_probe_timeout_error(
    label: &str,
    cache_root: &Path,
    target_dir: &Path,
    timeout: Duration,
    lock_path: Option<&Path>,
) -> String {
    let wait = lock_path.map_or_else(
        || format!(" after {}s", timeout.as_secs()),
        |path| format!(" waiting for lock {}", path.display()),
    );
    format!(
        "native provider package `{label}` probe timed out{wait}\ncache: {}\ntarget: {}\nset {PROVIDER_PROBE_TIMEOUT_ENV} to allow a longer cold build",
        cache_root.display(),
        target_dir.display()
    )
}

fn status_suffix(status: Option<i32>) -> String {
    status.map_or_else(String::new, |code| format!(" with status {code}"))
}

fn text_excerpt(text: &str) -> String {
    const LIMIT: usize = 400;
    if text.is_empty() {
        return "<empty>".to_string();
    }
    let mut chars = text.chars();
    let mut excerpt = chars.by_ref().take(LIMIT).collect::<String>();
    if chars.next().is_some() {
        excerpt.push_str("...");
    }
    excerpt
}

fn provider_probe_main(provider: &NativeProviderProbeInput) -> String {
    include_str!("templates/provider_probe_main.rs.in")
        .replace("{{provider_crate}}", &provider.cargo_alias)
}

fn provider_probe_dependencies(
    provider: &NativeProviderProbeInput,
) -> Result<Vec<RustCargoDependency>, String> {
    Ok(vec![
        rust_deps::runtime_dependency(),
        registry_probe_dependency("serde", &["derive"]),
        registry_probe_dependency("serde_json", &[]),
        RustCargoDependency {
            name: RustCargoName::parse(&provider.cargo_alias)
                .expect("generated provider Cargo alias is valid"),
            package: Some(
                RustCargoPackageName::parse(&provider.cargo_package).map_err(|error| {
                    format!(
                        "invalid native provider package `{}` Cargo package `{}`: {error}",
                        provider.package, provider.cargo_package
                    )
                })?,
            ),
            source: RustCargoDependencySource::Path(provider.crate_root.display().to_string()),
            features: vec![],
            default_features: true,
        },
    ])
}

fn registry_probe_dependency(name: &str, features: &[&str]) -> RustCargoDependency {
    RustCargoDependency {
        name: RustCargoName::parse(name).expect("valid probe dependency name"),
        package: None,
        source: RustCargoDependencySource::Registry {
            version: "1".to_string(),
        },
        features: features
            .iter()
            .map(|feature| (*feature).to_string())
            .collect(),
        default_features: true,
    }
}

fn provider_probe_crate_identity(
    provider: &NativeProviderProbeInput,
    dependencies: &[RustCargoDependency],
) -> RustCargoCrateIdentity {
    let seed = format!(
        "{PROVIDER_PROBE_PROFILE}\n{}\n{}\n{}",
        provider.package.manifest_path().display(),
        provider.cargo_package,
        provider.cargo_alias
    );
    cargo_job::single_program_crate_identity(&RustCargoCrateIdentityInput {
        seed: &seed,
        semantic_profile: PROVIDER_PROBE_PROFILE,
        cargo_profile: RustCargoProfile::Dev,
        dependencies,
    })
}

fn provider_probe_job(
    provider: &NativeProviderProbeInput,
    cache_root: &Path,
) -> Result<RustCargoJob, String> {
    let dependencies = provider_probe_dependencies(provider)?;
    let identity = provider_probe_crate_identity(provider, &dependencies);
    Ok(cargo_job::single_program_job_configured(
        RustSource::new(provider_probe_main(provider)),
        cache_root.to_path_buf(),
        RustCargoProfile::Dev,
        RustCargoMode::Run,
        PROVIDER_PROBE_PROFILE,
        dependencies,
        RustCargoPackageMetadata::default(),
        Some(identity),
    ))
}

fn generated_provider_cargo_alias(package: &PackageId) -> String {
    let manifest = package.manifest_path().display().to_string();
    let stem = package
        .manifest_path()
        .parent()
        .and_then(Path::file_name)
        .and_then(|name| name.to_str())
        .unwrap_or("package");
    format!(
        "anvyx_provider_{}_{}",
        rust_identifier_component(stem),
        stable_hash_hex(&manifest)
    )
}

fn rust_identifier_component(text: &str) -> String {
    let mut out = String::new();
    for ch in text.chars() {
        if ch.is_ascii_alphanumeric() {
            out.push(ch.to_ascii_lowercase());
        } else if !out.ends_with('_') {
            out.push('_');
        }
    }
    let out = out.trim_matches('_');
    if out.is_empty() {
        "package".to_string()
    } else {
        out.to_string()
    }
}

fn stable_hash_hex(text: &str) -> String {
    const OFFSET: u64 = 0xcbf_29ce_4842_2325;
    const PRIME: u64 = 0x0000_0100_0000_01b3;
    let mut hash = OFFSET;
    for byte in text.as_bytes() {
        hash ^= u64::from(*byte);
        hash = hash.wrapping_mul(PRIME);
    }
    format!("{hash:016x}")
}

#[derive(Debug, Deserialize)]
pub struct Project {
    pub name: Option<String>,
    pub version: Option<String>,
    pub entry: Option<String>,
}

#[derive(Debug, Deserialize)]
pub struct DependencyEntry {
    pub path: String,
}

pub fn parse_manifest() -> Result<Option<Manifest>, String> {
    let manifest_path = Path::new("anvyx.toml");
    if !manifest_path.exists() {
        return Ok(None);
    }

    parse_manifest_file(manifest_path).map(Some)
}

pub fn parse_manifest_file(path: &Path) -> Result<Manifest, String> {
    let contents =
        fs::read_to_string(path).map_err(|e| format!("Failed to read {}: {e}", path.display()))?;
    let manifest = toml::from_str(&contents)
        .map_err(|e| format!("Failed to parse {}: {e}", path.display()))?;
    validate_manifest(&manifest).map_err(|error| format!("Invalid {}: {error}", path.display()))?;
    Ok(manifest)
}

fn validate_manifest(manifest: &Manifest) -> Result<(), String> {
    if let Some(version) = &manifest.project.version {
        validate_project_version(version)?;
    }
    Ok(())
}

fn validate_project_version(version: &str) -> Result<(), String> {
    let parts = version.split('.').collect::<Vec<_>>();
    if parts.len() == 3 && parts.iter().all(|part| valid_version_number(part)) {
        return Ok(());
    }
    Err(format!(
        "project.version must be MAJOR.MINOR.PATCH, got `{version}`"
    ))
}

fn valid_version_number(part: &str) -> bool {
    !part.is_empty()
        && (part == "0" || !part.starts_with('0'))
        && part.bytes().all(|b| b.is_ascii_digit())
        && part.parse::<u64>().is_ok()
}

pub fn lint_config(
    manifest: Option<&Manifest>,
    overrides: &[impl AsRef<str>],
) -> Result<LintConfig, String> {
    let mut config = LintConfig::default();
    if let Some(manifest) = manifest {
        for (name, level) in &manifest.lint {
            config
                .apply_override(&format!("{name}={level}"))
                .map_err(|error| error.to_string())?;
        }
    }
    for override_text in overrides {
        config
            .apply_override(override_text.as_ref())
            .map_err(|error| error.to_string())?;
    }
    Ok(config)
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PackageId {
    manifest_path: PathBuf,
}

impl PackageId {
    pub fn manifest_path(&self) -> &Path {
        &self.manifest_path
    }
}

impl std::fmt::Display for PackageId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.manifest_path.display())
    }
}

pub fn package_frontend_id(package: &PackageId) -> anvyx_lang::PackageId {
    anvyx_lang::PackageId::new(package.to_string())
}

#[derive(Debug, Clone)]
pub struct PackageSourceInfo {
    pub entry: PathBuf,
    pub source_root: PathBuf,
}

#[derive(Debug, Clone)]
pub struct NativePackageInfo {
    pub crate_root: PathBuf,
    pub cargo_manifest: PathBuf,
}

#[derive(Debug, Clone)]
pub struct PackageNode {
    pub id: PackageId,
    pub source: Option<PackageSourceInfo>,
    pub dependencies: HashMap<String, PackageId>,
}

#[derive(Debug, Clone)]
pub(crate) struct ProviderCargoSidecar {
    pub(crate) crate_alias: String,
    pub(crate) cargo_package: String,
    pub(crate) crate_root: PathBuf,
}

struct ProviderPackageLoad {
    input: (ProviderPackageKey, RawProviderPackage, Option<String>),
    cargo: ProviderCargoSidecar,
}

#[derive(Debug)]
pub(crate) struct ProviderWorld {
    pub(crate) catalog: ProviderCatalog,
    pub(crate) cargo: HashMap<ProviderPackageKey, ProviderCargoSidecar>,
}

fn provider_world(loads: Vec<ProviderPackageLoad>) -> Result<ProviderWorld, String> {
    let mut inputs = vec![];
    let mut cargo = HashMap::new();
    for ProviderPackageLoad {
        input,
        cargo: sidecar,
    } in loads
    {
        cargo.insert(input.0.clone(), sidecar);
        inputs.push(input);
    }
    let catalog = ProviderCatalog::try_new(inputs).map_err(|error| error.to_string())?;
    Ok(ProviderWorld { catalog, cargo })
}

#[derive(Debug)]
pub struct PackageGraph {
    root: PackageId,
    packages: Vec<PackageNode>,
    providers: ProviderWorld,
}

impl PackageGraph {
    pub fn root(&self) -> &PackageNode {
        self.packages
            .iter()
            .find(|package| package.id == self.root)
            .expect("root package is loaded")
    }

    pub fn packages(&self) -> &[PackageNode] {
        &self.packages
    }

    pub(crate) fn provider_world(&self) -> &ProviderWorld {
        &self.providers
    }
}

pub fn load_package_graph(manifest_path: &Path) -> Result<PackageGraph, String> {
    let root = package_id(manifest_path)?;
    let root_dir = root
        .manifest_path()
        .parent()
        .expect("canonical manifest path has a parent");
    let cache_root = cache::rust_cache_root_for(root_dir);
    load_package_graph_with_probe(root, cache_root)
}

pub(crate) fn load_package_graph_with_rust_cache(
    manifest_path: &Path,
    cache_root: PathBuf,
) -> Result<PackageGraph, String> {
    load_package_graph_with_probe(package_id(manifest_path)?, cache_root)
}

fn load_package_graph_with_probe(
    root: PackageId,
    cache_root: PathBuf,
) -> Result<PackageGraph, String> {
    PackageGraphLoader::new(cache_root).load(root)
}

pub fn find_nearest_manifest(start: &Path) -> Result<Option<PathBuf>, String> {
    let mut dir = start_dir(start)?;
    loop {
        let manifest = dir.join("anvyx.toml");
        if manifest.is_file() {
            return manifest.canonicalize().map(Some).map_err(|error| {
                format!("failed to canonicalize {}: {error}", manifest.display())
            });
        }
        if !dir.pop() {
            return Ok(None);
        }
    }
}

fn start_dir(start: &Path) -> Result<PathBuf, String> {
    if start.is_dir() {
        return start
            .canonicalize()
            .map_err(|error| format!("failed to canonicalize {}: {error}", start.display()));
    }

    let mut probe = if start.exists() {
        start
            .parent()
            .unwrap_or_else(|| Path::new("."))
            .to_path_buf()
    } else {
        start.to_path_buf()
    };
    while !probe.exists() {
        if !probe.pop() {
            return std::env::current_dir().map_err(|error| error.to_string());
        }
    }
    if !probe.is_dir() {
        probe.pop();
    }
    probe
        .canonicalize()
        .map_err(|error| format!("failed to canonicalize {}: {error}", probe.display()))
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum VisitState {
    Visiting,
    Done,
}

struct PackageGraphLoader {
    states: HashMap<PackageId, VisitState>,
    stack: Vec<PackageId>,
    packages: Vec<PackageNode>,
    provider_loads: Vec<ProviderPackageLoad>,
    cache_root: PathBuf,
}

impl PackageGraphLoader {
    fn new(cache_root: PathBuf) -> Self {
        Self {
            states: HashMap::new(),
            stack: vec![],
            packages: vec![],
            provider_loads: vec![],
            cache_root,
        }
    }

    fn load(mut self, root: PackageId) -> Result<PackageGraph, String> {
        self.load_package(&root)?;
        self.provider_loads.extend(system_provider_loads());
        Ok(PackageGraph {
            root,
            packages: self.packages,
            providers: provider_world(self.provider_loads)?,
        })
    }

    fn load_package(&mut self, id: &PackageId) -> Result<(), String> {
        match self.states.get(id) {
            Some(VisitState::Done) => return Ok(()),
            Some(VisitState::Visiting) => return Err(self.cycle_error(id)),
            None => {}
        }

        self.states.insert(id.clone(), VisitState::Visiting);
        self.stack.push(id.clone());

        let manifest = parse_manifest_file(id.manifest_path())?;
        let dir = id
            .manifest_path()
            .parent()
            .expect("canonical manifest path has a parent")
            .to_path_buf();
        let dependencies = self.load_dependencies(id, &dir, &manifest)?;
        let source = manifest.project.entry.as_ref().map(|entry| {
            let entry = dir.join(entry);
            let source_root = entry
                .parent()
                .filter(|parent| !parent.as_os_str().is_empty())
                .unwrap_or_else(|| Path::new("."))
                .to_path_buf();
            PackageSourceInfo { entry, source_root }
        });
        let cargo_manifest = dir.join("Cargo.toml");
        let native = cargo_manifest.is_file().then(|| NativePackageInfo {
            crate_root: dir.clone(),
            cargo_manifest,
        });
        if source.is_none() && native.is_none() {
            return Err(format!(
                "package {id} has no project.entry and no Cargo.toml native marker"
            ));
        }
        if let Some(native) = &native {
            let probe = native_provider_probe_input(id.clone(), native)?;
            self.provider_loads
                .push(load_native_package_provider(probe, &self.cache_root)?);
        }
        self.packages.push(PackageNode {
            id: id.clone(),
            source,
            dependencies,
        });

        self.stack.pop();
        self.states.insert(id.clone(), VisitState::Done);
        Ok(())
    }

    fn load_dependencies(
        &mut self,
        id: &PackageId,
        dir: &Path,
        manifest: &Manifest,
    ) -> Result<HashMap<String, PackageId>, String> {
        let mut entries = manifest.dependencies.iter().collect::<Vec<_>>();
        entries.sort_unstable_by_key(|entry| entry.0);

        let mut seen = HashMap::new();
        let mut dependencies = HashMap::new();
        for (alias, entry) in entries {
            validate_dependency_alias(alias, id)?;
            let dep_id = dependency_package_id(id, dir, alias, entry)?;
            if let Some(previous) = seen.insert(dep_id.clone(), alias.clone()) {
                return Err(format!(
                    "package {id} declares the same package dependency twice: `{previous}` and `{alias}` both point to {dep_id}"
                ));
            }
            self.load_package(&dep_id)?;
            dependencies.insert(alias.clone(), dep_id);
        }

        Ok(dependencies)
    }

    fn cycle_error(&self, id: &PackageId) -> String {
        let start = self
            .stack
            .iter()
            .position(|active| active == id)
            .unwrap_or(0);
        let mut cycle = self.stack[start..]
            .iter()
            .map(ToString::to_string)
            .collect::<Vec<_>>();
        cycle.push(id.to_string());
        format!("package dependency cycle: {}", cycle.join(" -> "))
    }
}

pub(crate) fn system_provider_world() -> Result<ProviderWorld, String> {
    provider_world(system_provider_loads())
}

fn system_provider_loads() -> Vec<ProviderPackageLoad> {
    vec![
        system_provider_load(
            "<core>",
            "anvyx_core",
            "anvyx-core",
            "core",
            anvyx_core::rust_providers(),
        ),
        system_provider_load(
            "<std>",
            "anvyx_stdlib",
            "anvyx-stdlib",
            "stdlib",
            anvyx_stdlib::rust_providers(),
        ),
    ]
}

fn system_provider_load(
    key: &str,
    alias: &str,
    cargo_package: &str,
    workspace_crate: &str,
    package: RawProviderPackage,
) -> ProviderPackageLoad {
    ProviderPackageLoad {
        input: (
            ProviderPackageKey(key.to_string()),
            package,
            Some(alias.to_string()),
        ),
        cargo: ProviderCargoSidecar {
            crate_alias: alias.to_string(),
            cargo_package: cargo_package.to_string(),
            crate_root: rust_deps::workspace_crate_path(workspace_crate),
        },
    }
}

fn package_id(manifest_path: &Path) -> Result<PackageId, String> {
    let manifest_path = manifest_path.canonicalize().map_err(|e| {
        format!(
            "failed to canonicalize package manifest '{}': {e}",
            manifest_path.display()
        )
    })?;
    Ok(PackageId { manifest_path })
}

fn dependency_package_id(
    package: &PackageId,
    dir: &Path,
    alias: &str,
    entry: &DependencyEntry,
) -> Result<PackageId, String> {
    let manifest_path = dir.join(&entry.path).join("anvyx.toml");
    if !manifest_path.is_file() {
        return Err(format!(
            "dependency `{alias}` in package {package} must point to a directory containing anvyx.toml: {}",
            manifest_path.display()
        ));
    }
    package_id(&manifest_path).map_err(|message| {
        format!("failed to load dependency `{alias}` in package {package}: {message}")
    })
}

fn validate_dependency_alias(alias: &str, package: &PackageId) -> Result<(), String> {
    match anvyx_lang::validate_dependency_alias(alias) {
        Ok(()) => Ok(()),
        Err(anvyx_lang::DependencyAliasError::Invalid) => Err(format!(
            "package {package} uses invalid dependency alias `{alias}`"
        )),
    }
}
