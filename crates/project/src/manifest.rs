use std::{
    collections::{BTreeMap, HashMap},
    fs,
    path::{Path, PathBuf},
    process::{Command, Stdio},
    time::{Duration, SystemTime, UNIX_EPOCH},
};

use anvyx_lang2::LintConfig;
use anvyx_runtime::{
    ProviderDescriptor, RustModuleSupport, RustProviderCargo, RustProviderSupport,
    validate_rust_provider_support,
};
use serde::Deserialize;
use wait_timeout::ChildExt;

const PROVIDER_PROBE_TIMEOUT: Duration = Duration::from_mins(2);

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
struct LoadedNativeProvider {
    descriptor: ProviderDescriptor,
    support: RustProviderSupport,
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
struct ProviderProbeOutput {
    descriptor: ProviderDescriptor,
    supports: Vec<RustModuleSupport>,
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
    package: PackageId,
    native: &NativePackageInfo,
) -> Result<LoadedNativeProvider, String> {
    let probe = native_provider_probe_input(package, native)?;
    let label = probe.package.to_string();
    let output = run_provider_probe(&probe, &label)?;
    native_provider_from_probe(probe, output)
}

fn native_provider_from_probe(
    probe: NativeProviderProbeInput,
    output: ProviderProbeOutput,
) -> Result<LoadedNativeProvider, String> {
    anvyx_externs::validate(&output.descriptor).map_err(|errors| {
        format!(
            "native provider package `{}` has invalid provider descriptor: {errors:?}",
            probe.package
        )
    })?;
    let support = rust_provider_support(&probe, &output);
    validate_rust_provider_support(
        std::slice::from_ref(&output.descriptor),
        std::slice::from_ref(&support),
    )
    .map_err(|error| {
        format!(
            "native provider package `{}` has invalid native support: {error}",
            probe.package
        )
    })?;
    Ok(LoadedNativeProvider {
        descriptor: output.descriptor,
        support,
    })
}

fn rust_provider_support(
    probe: &NativeProviderProbeInput,
    output: &ProviderProbeOutput,
) -> RustProviderSupport {
    let mut modules = output.supports.clone();
    retarget_rust_modules(&mut modules, &probe.cargo_alias);
    RustProviderSupport {
        package: package_lang_id(&probe.package),
        provider: output.descriptor.provider.clone(),
        cargo: RustProviderCargo {
            manifest_key: probe.cargo_alias.clone(),
            package: Some(probe.cargo_package.clone()),
            path: Some(probe.crate_root.clone()),
            features: vec![],
            default_features: true,
        },
        modules,
    }
}

fn retarget_rust_modules(modules: &mut [RustModuleSupport], cargo_alias: &str) {
    for module in modules {
        for ty in &mut module.types {
            ty.path.crate_name = cargo_alias.to_string();
        }
        for binding in &mut module.bindings {
            binding.path.crate_name = cargo_alias.to_string();
        }
    }
}

fn run_provider_probe(
    provider: &NativeProviderProbeInput,
    label: &str,
) -> Result<ProviderProbeOutput, String> {
    let dir = provider_probe_dir(provider);
    fs::create_dir_all(dir.join("src")).map_err(|error| {
        format!(
            "failed to create native provider package `{label}` probe at {}: {error}",
            dir.display()
        )
    })?;
    fs::write(dir.join("Cargo.toml"), provider_probe_manifest(provider)).map_err(|error| {
        format!("failed to write native provider package `{label}` probe manifest: {error}")
    })?;
    fs::write(dir.join("src/main.rs"), provider_probe_main(provider)).map_err(|error| {
        format!("failed to write native provider package `{label}` probe main: {error}")
    })?;

    let stdout_path = dir.join("stdout.json");
    let stderr_path = dir.join("stderr.txt");
    let stdout_file = fs::File::create(&stdout_path).map_err(|error| {
        format!("failed to create native provider package `{label}` probe stdout file: {error}")
    })?;
    let stderr_file = fs::File::create(&stderr_path).map_err(|error| {
        format!("failed to create native provider package `{label}` probe stderr file: {error}")
    })?;
    let mut child = Command::new("cargo")
        .args(["run", "--quiet"])
        .current_dir(&dir)
        .stdin(Stdio::null())
        .stdout(Stdio::from(stdout_file))
        .stderr(Stdio::from(stderr_file))
        .spawn()
        .map_err(|error| {
            format!("failed to run native provider package `{label}` probe: {error}")
        })?;
    let Some(status) = child
        .wait_timeout(PROVIDER_PROBE_TIMEOUT)
        .map_err(|error| {
            format!("failed to wait for native provider package `{label}` probe: {error}")
        })?
    else {
        let _ = child.kill();
        let _ = child.wait();
        let _ = fs::remove_dir_all(&dir);
        return Err(format!("native provider package `{label}` probe timed out"));
    };
    let stdout = fs::read(&stdout_path).unwrap_or_default();
    let stderr = fs::read(&stderr_path).unwrap_or_default();
    let _ = fs::remove_dir_all(&dir);
    if !status.success() {
        let stderr = String::from_utf8_lossy(&stderr);
        return Err(format!(
            "native provider package `{label}` probe failed\n{stderr}"
        ));
    }
    serde_json::from_slice(&stdout).map_err(|error| {
        format!("native provider package `{label}` probe emitted invalid metadata: {error}")
    })
}

fn provider_probe_dir(provider: &NativeProviderProbeInput) -> PathBuf {
    let stamp = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("system time is after unix epoch")
        .as_nanos();
    let hash = stable_hash_hex(&provider.crate_root.display().to_string());
    std::env::temp_dir().join(format!(
        "anvyx-provider-{}-{hash}-{stamp}",
        std::process::id()
    ))
}

fn provider_probe_manifest(provider: &NativeProviderProbeInput) -> String {
    let dependency = format!(
        "{} = {{ package = {}, path = {} }}",
        provider.cargo_alias,
        toml_string(&provider.cargo_package),
        toml_string(&provider.crate_root.display().to_string())
    );
    render_probe_template(
        include_str!("templates/provider_probe_manifest.toml.in"),
        &[
            (
                "runtime_path",
                toml_string(&workspace_crate_path("runtime").display().to_string()),
            ),
            ("provider_dependency", dependency),
        ],
    )
}

fn provider_probe_main(provider: &NativeProviderProbeInput) -> String {
    render_probe_template(
        include_str!("templates/provider_probe_main.rs.in"),
        &[("provider_crate", provider.cargo_alias.clone())],
    )
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

fn toml_string(text: &str) -> String {
    let mut escaped = String::with_capacity(text.len() + 2);
    escaped.push('"');
    for ch in text.chars() {
        match ch {
            '\\' => escaped.push_str("\\\\"),
            '"' => escaped.push_str("\\\""),
            '\n' => escaped.push_str("\\n"),
            '\r' => escaped.push_str("\\r"),
            '\t' => escaped.push_str("\\t"),
            '\u{08}' => escaped.push_str("\\b"),
            '\u{0c}' => escaped.push_str("\\f"),
            ch if ch.is_control() => {
                use std::fmt::Write as _;
                write!(escaped, "\\u{:04X}", ch as u32).expect("write to string succeeds");
            }
            ch => escaped.push(ch),
        }
    }
    escaped.push('"');
    escaped
}

fn render_probe_template(template: &str, values: &[(&str, String)]) -> String {
    let mut rendered = template.to_string();
    for (key, value) in values {
        rendered = rendered.replace(&format!("{{{{{key}}}}}"), value);
    }
    rendered
}

fn workspace_crate_path(name: &str) -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("project crate lives below workspace crates directory")
        .join(name)
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

pub fn package_lang_id(package: &PackageId) -> String {
    package.manifest_path().display().to_string()
}

pub fn package_frontend_id(package: &PackageId) -> anvyx_lang2::PackageId {
    anvyx_lang2::PackageId::new(package_lang_id(package))
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
    pub native: Option<NativePackageInfo>,
    providers: Vec<LoadedNativeProvider>,
    pub dependencies: HashMap<String, PackageId>,
}

#[derive(Debug, Clone)]
pub struct PackageGraph {
    root: PackageId,
    packages: Vec<PackageNode>,
}

impl PackageGraph {
    pub fn root(&self) -> &PackageNode {
        self.package(&self.root).expect("root package is loaded")
    }

    pub fn packages(&self) -> &[PackageNode] {
        &self.packages
    }

    pub fn package(&self, id: &PackageId) -> Option<&PackageNode> {
        self.packages.iter().find(|package| &package.id == id)
    }

    pub fn package_externs(&self) -> Vec<(anvyx_lang2::PackageId, Vec<ProviderDescriptor>)> {
        self.packages
            .iter()
            .filter(|package| !package.providers.is_empty())
            .map(|package| {
                let providers = package
                    .providers
                    .iter()
                    .map(|provider| provider.descriptor.clone())
                    .collect();
                (package_frontend_id(&package.id), providers)
            })
            .collect()
    }

    pub fn rust_provider_supports(&self) -> Vec<RustProviderSupport> {
        self.packages
            .iter()
            .flat_map(|package| package.providers.iter())
            .map(|provider| provider.support.clone())
            .collect()
    }
}

pub fn load_package_graph(manifest_path: &Path) -> Result<PackageGraph, String> {
    PackageGraphLoader::default().load(manifest_path)
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

#[derive(Default)]
struct PackageGraphLoader {
    states: HashMap<PackageId, VisitState>,
    stack: Vec<PackageId>,
    packages: Vec<PackageNode>,
}

impl PackageGraphLoader {
    fn load(mut self, manifest_path: &Path) -> Result<PackageGraph, String> {
        let root = package_id(manifest_path)?;
        self.load_package(&root)?;
        Ok(PackageGraph {
            root,
            packages: self.packages,
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
        let providers = match &native {
            Some(native) => vec![load_native_package_provider(id.clone(), native)?],
            None => vec![],
        };
        self.packages.push(PackageNode {
            id: id.clone(),
            source,
            native,
            providers,
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
    match anvyx_lang2::validate_dependency_alias(alias) {
        Ok(()) => Ok(()),
        Err(anvyx_lang2::DependencyAliasError::Invalid) => Err(format!(
            "package {package} uses invalid dependency alias `{alias}`"
        )),
    }
}

#[cfg(test)]
mod tests {
    use std::{
        fmt::Write as _,
        fs,
        path::{Path, PathBuf},
    };

    use super::*;

    fn parse(toml: &str) -> Result<Manifest, String> {
        let manifest = toml::from_str(toml).map_err(|e| format!("Failed to parse: {e}"))?;
        validate_manifest(&manifest)?;
        Ok(manifest)
    }

    fn synthetic_package_id(manifest_path: PathBuf) -> PackageId {
        PackageId { manifest_path }
    }

    #[test]
    fn parse_manifest_ignores_stale_externs_table() {
        let manifest = parse(
            r#"
            [project]
            entry = "src/main.anv"

            [externs.engine]
            path = "my_externs/engine"
            "#,
        )
        .unwrap();

        assert_eq!(manifest.project.entry.as_deref(), Some("src/main.anv"));
    }

    #[test]
    fn parse_manifest_missing_project_errors() {
        let result = parse(
            r#"
            [externs.engine]
            path = "my_externs/engine"
            "#,
        );

        assert!(result.is_err());
    }

    #[test]
    fn parse_manifest_with_optional_name() {
        let with_name = parse(
            r#"
            [project]
            name = "my_game"
            entry = "src/main.anv"
            "#,
        )
        .unwrap();

        assert_eq!(with_name.project.name.as_deref(), Some("my_game"));

        let without_name = parse(
            r#"
            [project]
            entry = "src/main.anv"
            "#,
        )
        .unwrap();

        assert!(without_name.project.name.is_none());
    }

    #[test]
    fn parse_manifest_with_optional_version() {
        let manifest = parse(
            r#"
            [project]
            version = "1.2.3"
            entry = "src/main.anv"
            "#,
        )
        .unwrap();

        assert_eq!(manifest.project.version.as_deref(), Some("1.2.3"));
        for version in ["1.2", "01.2.3", "1.02.3", "1.2.03"] {
            assert!(parse(&format!("[project]\nversion = \"{version}\"\n")).is_err());
        }
    }

    #[test]
    fn parse_manifest_lint_values_as_raw_strings() {
        let manifest = parse(
            r#"
            [project]
            entry = "src/main.anv"

            [lint]
            internal_access = "error"
            deprecated = "allow"
            "#,
        )
        .unwrap();

        assert_eq!(manifest.lint["internal_access"], "error");
        assert_eq!(manifest.lint["deprecated"], "allow");
    }

    #[test]
    fn parse_manifest_lint_default() {
        let manifest = parse(
            r#"
            [project]
            entry = "src/main.anv"
            "#,
        )
        .unwrap();

        assert!(manifest.lint.is_empty());
    }

    #[test]
    fn lint_config_applies_manifest_and_overrides() {
        let manifest = parse(
            r#"
            [project]
            entry = "src/main.anv"

            [lint]
            internal_access = "error"
            deprecated = "allow"
            "#,
        )
        .unwrap();

        let config = lint_config(Some(&manifest), &["deprecated=warn"]).unwrap();

        assert_eq!(
            config.level(anvyx_lang2::LintId::InternalAccess),
            anvyx_lang2::LintLevel::Error
        );
        assert_eq!(
            config.level(anvyx_lang2::LintId::Deprecated),
            anvyx_lang2::LintLevel::Warn
        );
    }

    #[test]
    fn lint_config_rejects_non_string_manifest_values() {
        let error = parse(
            r#"
            [project]
            entry = "src/main.anv"

            [lint]
            internal_access = true
            "#,
        )
        .unwrap_err();

        assert!(error.contains("invalid type"), "{error}");
    }

    #[test]
    fn generated_provider_alias_is_stable_rust_identifier() {
        let package = synthetic_package_id(PathBuf::from("/tmp/Game Host-1/anvyx.toml"));

        let alias = generated_provider_cargo_alias(&package);

        assert!(alias.starts_with("anvyx_provider_game_host_1_"));
        assert!(
            alias
                .bytes()
                .all(|byte| byte.is_ascii_lowercase() || byte.is_ascii_digit() || byte == b'_')
        );
        assert_eq!(alias, generated_provider_cargo_alias(&package));
    }

    #[test]
    fn package_probe_manifest_uses_generated_alias_and_local_path() {
        let provider = package_provider_fixture("Game Host", "host-provider");

        let manifest = provider_probe_manifest(&provider);

        assert!(manifest.contains("anvyx_provider_game_host_"));
        assert!(
            manifest.contains(" = { package = \"host-provider\", path = \"/tmp/host-provider\" }")
        );
        assert!(!manifest.contains("anvyx_provider_host-provider"));
    }

    #[test]
    fn package_attachment_uses_package_id_and_retargeted_crate_paths() {
        let probe = package_provider_fixture("Game Host", "host-provider");
        let output = ProviderProbeOutput {
            descriptor: provider_descriptor("host"),
            supports: vec![RustModuleSupport {
                module: module_path("host"),
                types: vec![rust_type_binding("host_provider")],
                bindings: vec![rust_binding("host_provider")],
            }],
        };

        let provider = native_provider_from_probe(probe.clone(), output).unwrap();
        let support = provider.support;

        assert_eq!(support.package, package_lang_id(&probe.package));
        assert_eq!(support.cargo.manifest_key, probe.cargo_alias);
        assert_eq!(support.cargo.package.as_deref(), Some("host-provider"));
        assert_eq!(
            support.cargo.path.as_deref(),
            Some(probe.crate_root.as_path())
        );
        assert_eq!(
            support.modules[0].types[0].path.crate_name,
            probe.cargo_alias
        );
        assert_eq!(
            support.modules[0].bindings[0].path.crate_name,
            probe.cargo_alias
        );
    }

    #[test]
    fn provider_probe_manifest_renders_template_package_shape() {
        let provider = provider_fixture("physics", "physics-provider");

        let manifest = provider_probe_manifest(&provider);

        assert!(manifest.contains("[package]\nname = \"anvyx-provider-probe\""));
        assert!(manifest.contains("anvyx-runtime = { path = "));
        assert!(manifest.contains("serde = { version = \"1\", features = [\"derive\"] }"));
        assert!(manifest.contains("serde_json = \"1\""));
        assert!(manifest.contains(
            "anvyx_provider_physics = { package = \"physics-provider\", path = \"/tmp/physics-provider\" }"
        ));
    }

    #[test]
    fn provider_probe_manifest_escapes_toml_paths() {
        let mut provider = provider_fixture("physics", "physics-provider");
        provider.crate_root = PathBuf::from("/tmp/physics \"quoted\"");

        let manifest = provider_probe_manifest(&provider);

        assert!(manifest.contains("path = \"/tmp/physics \\\"quoted\\\"\""));
    }

    #[test]
    fn provider_probe_main_calls_descriptor_and_supports_through_alias() {
        let provider = provider_fixture("physics", "type");

        let main = provider_probe_main(&provider);

        assert!(main.contains("descriptor: anvyx_provider_physics::provider_descriptor(),"));
        assert!(main.contains("supports: anvyx_provider_physics::rust_module_supports(),"));
        assert!(!main.contains("type::"));
    }

    fn provider_fixture(name: &str, cargo_package: &str) -> NativeProviderProbeInput {
        let root = PathBuf::from(format!("/tmp/{cargo_package}"));
        NativeProviderProbeInput {
            package: synthetic_package_id(PathBuf::from(format!("/tmp/{name}/anvyx.toml"))),
            crate_root: root,
            cargo_package: cargo_package.to_string(),
            cargo_alias: format!("anvyx_provider_{name}"),
        }
    }

    fn package_provider_fixture(package: &str, cargo_package: &str) -> NativeProviderProbeInput {
        let package = synthetic_package_id(PathBuf::from(format!("/tmp/{package}/anvyx.toml")));
        let root = PathBuf::from(format!("/tmp/{cargo_package}"));
        NativeProviderProbeInput {
            cargo_alias: generated_provider_cargo_alias(&package),
            package,
            crate_root: root,
            cargo_package: cargo_package.to_string(),
        }
    }

    fn loaded_provider(package: &str, cargo_package: &str) -> LoadedNativeProvider {
        let probe = package_provider_fixture(package, cargo_package);
        native_provider_from_probe(
            probe,
            ProviderProbeOutput {
                descriptor: provider_descriptor(package),
                supports: vec![RustModuleSupport {
                    module: module_path("host"),
                    types: vec![rust_type_binding("host_provider")],
                    bindings: vec![rust_binding("host_provider")],
                }],
            },
        )
        .unwrap()
    }

    fn provider_descriptor(name: &str) -> ProviderDescriptor {
        ProviderDescriptor {
            provider: anvyx_runtime::ProviderId {
                name: name.to_string(),
            },
            modules: vec![anvyx_runtime::ExternModuleDescriptor {
                path: module_path("host"),
                types: vec![anvyx_runtime::ExternTypeDescriptor {
                    name: "Handle".to_string(),
                    doc: None,
                    rep: anvyx_runtime::ExternRep::Shared,
                    fields: vec![],
                    init: None,
                    methods: vec![],
                    statics: vec![],
                    operators: vec![],
                }],
                functions: vec![anvyx_runtime::ExternFunctionDescriptor {
                    name: "ping".to_string(),
                    doc: None,
                    signature: anvyx_runtime::ExternSignature {
                        params: vec![],
                        ret: anvyx_runtime::ExternTypeExpr::Void,
                    },
                    effects: anvyx_runtime::ExternEffects::default(),
                }],
            }],
        }
    }

    fn module_path(name: &str) -> anvyx_runtime::ModulePath {
        anvyx_runtime::ModulePath {
            segments: vec![name.to_string()],
        }
    }

    fn rust_type_binding(crate_name: &str) -> anvyx_runtime::RustTypeBinding {
        anvyx_runtime::RustTypeBinding {
            key: anvyx_runtime::ExternTypeKey {
                module: module_path("host"),
                name: "Handle".to_string(),
            },
            path: anvyx_runtime::RustPath {
                crate_name: crate_name.to_string(),
                segments: vec!["Handle".to_string()],
            },
        }
    }

    fn rust_binding(crate_name: &str) -> anvyx_runtime::RustExternBinding {
        anvyx_runtime::RustExternBinding {
            key: anvyx_runtime::ExternBindingKey {
                target: anvyx_runtime::ExternBindingTarget::Function(
                    anvyx_runtime::ExternFunctionKey {
                        module: module_path("host"),
                        name: "ping".to_string(),
                    },
                ),
                operation: anvyx_runtime::ExternBindingOp::Call,
            },
            path: anvyx_runtime::RustPath {
                crate_name: crate_name.to_string(),
                segments: vec!["ping".to_string()],
            },
            abi: anvyx_runtime::RustExternAbi {
                params: vec![],
                ret: anvyx_runtime::RustReturnAbi::Void,
                fallible: false,
                support: anvyx_runtime::RustAbiSupport::Direct,
            },
        }
    }

    struct PackageFixture {
        root: tempfile::TempDir,
    }

    impl Default for PackageFixture {
        fn default() -> Self {
            Self {
                root: tempfile::tempdir().unwrap(),
            }
        }
    }

    impl PackageFixture {
        fn write_package(&self, package: &str, deps: &[(&str, &str)]) {
            self.write_named_package(package, None, deps);
        }

        fn write_named_package(&self, package: &str, name: Option<&str>, deps: &[(&str, &str)]) {
            let mut manifest = "[project]\n".to_string();
            if let Some(name) = name {
                writeln!(manifest, "name = \"{name}\"").unwrap();
            }
            manifest.push_str("entry = \"src/lib.anv\"\n");
            if !deps.is_empty() {
                manifest.push_str("\n[dependencies]\n");
                for (alias, path) in deps {
                    writeln!(manifest, "{alias} = {{ path = \"{path}\" }}").unwrap();
                }
            }
            self.write_raw_manifest(package, &manifest);
        }

        fn write_native_package(&self, package: &str, deps: &[(&str, &str)]) {
            self.write_native_package_with_entry(package, None, deps);
        }

        fn write_source_native_package(&self, package: &str, deps: &[(&str, &str)]) {
            self.write_native_package_with_entry(package, Some("src/lib.anv"), deps);
            let source = self.root.path().join(package).join("src/lib.anv");
            fs::write(source, "pub fn marker() -> int { 1 }").unwrap();
        }

        fn write_native_package_with_entry(
            &self,
            package: &str,
            entry: Option<&str>,
            deps: &[(&str, &str)],
        ) {
            let mut manifest = "[project]\nname = \"native\"\n".to_string();
            if let Some(entry) = entry {
                writeln!(manifest, "entry = \"{entry}\"").unwrap();
            }
            if !deps.is_empty() {
                manifest.push_str("\n[dependencies]\n");
                for (alias, path) in deps {
                    writeln!(manifest, "{alias} = {{ path = \"{path}\" }}").unwrap();
                }
            }
            self.write_raw_manifest(package, &manifest);
            self.write_provider_crate(package, "native");
        }

        fn write_invalid_native_package(&self, package: &str) {
            self.write_raw_manifest(package, "[project]\nname = \"native\"\n");
            let dir = self.root.path().join(package);
            fs::create_dir_all(dir.join("src")).unwrap();
            fs::write(
                dir.join("Cargo.toml"),
                format!(
                    "[package]\nname = \"native\"\nversion = \"0.1.0\"\nedition = \"2024\"\n\n[dependencies]\nanvyx-runtime = {{ path = {} }}\n",
                    toml_string(&workspace_crate_path("runtime").display().to_string())
                ),
            )
            .unwrap();
            fs::write(
                dir.join("src/lib.rs"),
                r#"pub fn provider_descriptor() -> anvyx_runtime::ProviderDescriptor {
    anvyx_runtime::ProviderDescriptor {
        provider: anvyx_runtime::ProviderId { name: "Bad Name".to_string() },
        modules: vec![],
    }
}

pub fn rust_module_supports() -> Vec<anvyx_runtime::RustModuleSupport> {
    vec![]
}
"#,
            )
            .unwrap();
        }

        fn write_provider_crate(&self, package: &str, provider: &str) {
            let dir = self.root.path().join(package);
            fs::create_dir_all(dir.join("src")).unwrap();
            fs::write(
                dir.join("Cargo.toml"),
                format!(
                    "[package]\nname = \"{provider}\"\nversion = \"0.1.0\"\nedition = \"2024\"\n\n[dependencies]\nanvyx-runtime = {{ path = {} }}\n",
                    toml_string(&workspace_crate_path("runtime").display().to_string())
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
    name: "{provider}",
    source: "",
    exports: [ping],
}}
"#
                ),
            )
            .unwrap();
        }

        fn write_raw_manifest(&self, package: &str, contents: &str) {
            let dir = self.root.path().join(package);
            fs::create_dir_all(&dir).unwrap();
            fs::write(dir.join("anvyx.toml"), contents).unwrap();
        }

        fn manifest(&self, package: &str) -> PathBuf {
            self.root.path().join(package).join("anvyx.toml")
        }
    }

    fn validate_ok(path: impl AsRef<Path>) {
        load_package_graph(path.as_ref()).unwrap();
    }

    fn validate_err(path: impl AsRef<Path>, contains: &str) {
        let error = load_package_graph(path.as_ref()).expect_err("package graph should fail");
        assert!(
            error.contains(contains),
            "expected error to contain {contains:?}, got {error:?}"
        );
    }

    #[test]
    fn nearest_manifest_finds_root_from_nested_source_file() {
        let fixture = PackageFixture::default();
        fixture.write_package("game", &[]);
        let source = fixture.root.path().join("game/src/nested/main.anv");
        fs::create_dir_all(source.parent().unwrap()).unwrap();
        fs::write(&source, "fn main() {}").unwrap();

        let manifest = find_nearest_manifest(&source).unwrap().unwrap();

        assert_eq!(manifest, fixture.manifest("game").canonicalize().unwrap());
    }

    #[test]
    fn nearest_manifest_accepts_manifest_directory() {
        let fixture = PackageFixture::default();
        fixture.write_package("game", &[]);

        let manifest = find_nearest_manifest(&fixture.root.path().join("game"))
            .unwrap()
            .unwrap();

        assert_eq!(manifest, fixture.manifest("game").canonicalize().unwrap());
    }

    #[test]
    fn nearest_manifest_returns_none_without_manifest() {
        let fixture = PackageFixture::default();
        let dir = fixture.root.path().join("empty/src");
        fs::create_dir_all(&dir).unwrap();

        assert_eq!(find_nearest_manifest(&dir).unwrap(), None);
    }

    #[test]
    fn nearest_manifest_handles_unsaved_missing_source_file() {
        let fixture = PackageFixture::default();
        fixture.write_package("game", &[]);
        let source = fixture.root.path().join("game/src/missing/new.anv");

        let manifest = find_nearest_manifest(&source).unwrap().unwrap();

        assert_eq!(manifest, fixture.manifest("game").canonicalize().unwrap());
    }

    #[test]
    fn parse_native_only_manifest_allows_missing_entry() {
        let manifest = parse(
            r#"
            [project]
            name = "host"
            "#,
        )
        .unwrap();

        assert_eq!(manifest.project.entry, None);
    }

    #[test]
    fn parse_manifest_dependencies() {
        let manifest = parse(
            r#"
            [project]
            entry = "src/main.anv"

            [dependencies]
            math = { path = "../math" }
            physics = { path = "../physics" }
            "#,
        )
        .unwrap();

        assert_eq!(manifest.dependencies.len(), 2);
        assert_eq!(manifest.dependencies["math"].path, "../math");
        assert_eq!(manifest.dependencies["physics"].path, "../physics");
    }

    #[test]
    fn graph_loading_ignores_manifest_externs() {
        let fixture = PackageFixture::default();
        fixture.write_raw_manifest(
            "game",
            r#"
            [project]
            entry = "src/lib.anv"

            [externs.missing]
            path = "missing-provider"
            "#,
        );

        let graph = load_package_graph(&fixture.manifest("game")).unwrap();

        assert!(graph.root().providers.is_empty());
    }

    #[test]
    fn project_name_does_not_control_dependency_alias() {
        let fixture = PackageFixture::default();
        fixture.write_named_package("game", Some("game"), &[("p", "../physics")]);
        fixture.write_named_package("physics", Some("not_the_import_name"), &[]);

        validate_ok(fixture.manifest("game"));
    }

    #[test]
    fn package_dependency_path_is_relative_to_declaring_manifest() {
        let fixture = PackageFixture::default();
        fixture.write_named_package("game", Some("game"), &[("physics", "../libs/physics")]);
        fixture.write_named_package("libs/physics", Some("physics"), &[]);

        validate_ok(fixture.manifest("game"));
    }

    #[test]
    fn package_dependency_path_must_contain_manifest() {
        let fixture = PackageFixture::default();
        fixture.write_package("game", &[("missing", "../missing")]);

        validate_err(fixture.manifest("game"), "anvyx.toml");
    }

    #[test]
    fn native_only_dependency_gets_provider_attachment() {
        let fixture = PackageFixture::default();
        fixture.write_package("game", &[("host_alias", "../host")]);
        fixture.write_native_package("host", &[]);

        let graph = load_package_graph(&fixture.manifest("game")).unwrap();
        let host = graph
            .packages()
            .iter()
            .find(|package| package.source.is_none())
            .expect("native dependency package");
        assert!(host.dependencies.is_empty());
        let native = host.native.as_ref().expect("native package marker");
        assert!(native.cargo_manifest.ends_with("Cargo.toml"));
        assert_eq!(host.providers.len(), 1);
        assert_eq!(host.providers[0].descriptor.provider.name, "native");
        assert_ne!(host.providers[0].support.cargo.manifest_key, "host_alias");
        assert_eq!(host.providers[0].support.package, package_lang_id(&host.id));
    }

    #[test]
    fn source_native_dependency_gets_provider_attachment() {
        let fixture = PackageFixture::default();
        fixture.write_package("game", &[("colors", "../colors")]);
        fixture.write_source_native_package("colors", &[]);

        let graph = load_package_graph(&fixture.manifest("game")).unwrap();
        let colors = graph
            .packages()
            .iter()
            .find(|package| package.id.manifest_path().ends_with("colors/anvyx.toml"))
            .expect("source native dependency package");

        assert!(colors.source.is_some());
        assert!(colors.native.is_some());
        assert_eq!(colors.providers.len(), 1);
        assert_eq!(
            colors.providers[0].support.package,
            package_lang_id(&colors.id)
        );
    }

    #[test]
    fn root_native_package_gets_provider_attachment() {
        let fixture = PackageFixture::default();
        fixture.write_source_native_package("game", &[]);

        let graph = load_package_graph(&fixture.manifest("game")).unwrap();
        let root = graph.root();

        assert!(root.source.is_some());
        assert!(root.native.is_some());
        assert_eq!(root.providers.len(), 1);
        assert_eq!(root.providers[0].support.package, package_lang_id(&root.id));
    }

    #[test]
    fn invalid_native_provider_reports_package_context() {
        let fixture = PackageFixture::default();
        fixture.write_package("game", &[("host", "../host")]);
        fixture.write_invalid_native_package("host");

        let error = load_package_graph(&fixture.manifest("game")).unwrap_err();

        assert!(error.contains("native provider package"));
        assert!(error.contains("invalid provider descriptor"));
    }

    #[test]
    fn graph_provider_helpers_group_descriptors_and_supports_by_package() {
        let root = synthetic_package_id(PathBuf::from("/tmp/game/anvyx.toml"));
        let left = loaded_provider("left", "left-provider");
        let right = loaded_provider("right", "right-provider");
        let graph = PackageGraph {
            root: root.clone(),
            packages: vec![
                PackageNode {
                    id: root,
                    source: None,
                    native: None,
                    providers: vec![],
                    dependencies: HashMap::new(),
                },
                PackageNode {
                    id: synthetic_package_id(PathBuf::from("/tmp/left/anvyx.toml")),
                    source: None,
                    native: None,
                    providers: vec![left],
                    dependencies: HashMap::new(),
                },
                PackageNode {
                    id: synthetic_package_id(PathBuf::from("/tmp/right/anvyx.toml")),
                    source: None,
                    native: None,
                    providers: vec![right],
                    dependencies: HashMap::new(),
                },
            ],
        };

        let externs = graph.package_externs();
        let supports = graph.rust_provider_supports();

        assert_eq!(externs.len(), 2);
        assert_eq!(supports.len(), 2);
        for (package, descriptors) in externs {
            assert_eq!(descriptors.len(), 1);
            assert!(
                supports
                    .iter()
                    .any(|support| support.package == package.to_string())
            );
        }
    }

    #[test]
    fn source_only_graph_has_no_native_provider_helpers() {
        let fixture = PackageFixture::default();
        fixture.write_package("game", &[("math", "../math")]);
        fixture.write_package("math", &[]);

        let graph = load_package_graph(&fixture.manifest("game")).unwrap();

        assert!(graph.package_externs().is_empty());
        assert!(graph.rust_provider_supports().is_empty());
    }

    #[test]
    fn package_without_entry_or_native_marker_is_rejected() {
        let fixture = PackageFixture::default();
        fixture.write_raw_manifest(
            "host",
            r#"
            [project]
            name = "host"
            "#,
        );

        validate_err(fixture.manifest("host"), "no project.entry");
    }

    #[test]
    fn package_dependency_cycles_are_rejected() {
        let fixture = PackageFixture::default();
        fixture.write_package("a", &[("b", "../b")]);
        fixture.write_package("b", &[("a", "../a")]);

        validate_err(fixture.manifest("a"), "cycle");
    }

    #[test]
    fn same_canonical_package_cannot_be_declared_twice_in_one_manifest() {
        let fixture = PackageFixture::default();
        fixture.write_package("game", &[("math", "../math"), ("m", "../math")]);
        fixture.write_package("math", &[]);

        validate_err(fixture.manifest("game"), "same package");
    }

    #[test]
    fn same_canonical_native_dependency_is_loaded_once() {
        let fixture = PackageFixture::default();
        fixture.write_package("game", &[("ui", "../ui"), ("audio", "../audio")]);
        fixture.write_package("ui", &[("math", "../math")]);
        fixture.write_package("audio", &[("m", "../math")]);
        fixture.write_native_package("math", &[]);

        let graph = load_package_graph(&fixture.manifest("game")).unwrap();
        let math = graph
            .packages()
            .iter()
            .filter(|package| package.id.manifest_path().ends_with("math/anvyx.toml"))
            .collect::<Vec<_>>();

        assert_eq!(math.len(), 1);
        assert_eq!(math[0].providers.len(), 1);
    }

    #[test]
    fn duplicate_project_names_are_allowed_for_different_packages() {
        let fixture = PackageFixture::default();
        fixture.write_package("game", &[("left", "../left"), ("right", "../right")]);
        fixture.write_named_package("left", Some("math"), &[]);
        fixture.write_named_package("right", Some("math"), &[]);

        validate_ok(fixture.manifest("game"));
    }

    #[test]
    fn invalid_dependency_alias_is_rejected() {
        let fixture = PackageFixture::default();
        fixture.write_raw_manifest(
            "game",
            r#"
            [project]
            entry = "src/main.anv"

            [dependencies]
            "bad-name" = { path = "../bad" }
            "#,
        );
        fixture.write_package("bad", &[]);

        validate_err(fixture.manifest("game"), "invalid dependency alias");
    }

    #[test]
    fn keyword_dependency_alias_is_rejected() {
        let fixture = PackageFixture::default();
        fixture.write_raw_manifest(
            "game",
            r#"
            [project]
            entry = "src/main.anv"

            [dependencies]
            fn = { path = "../bad" }
            "#,
        );
        fixture.write_package("bad", &[]);

        validate_err(fixture.manifest("game"), "invalid dependency alias");
    }

    #[test]
    fn std_dependency_alias_is_allowed() {
        let fixture = PackageFixture::default();
        fixture.write_package("game", &[("std", "../fake_std")]);
        fixture.write_package("fake_std", &[]);

        validate_ok(fixture.manifest("game"));
    }

    #[test]
    fn core_dependency_alias_is_allowed() {
        let fixture = PackageFixture::default();
        fixture.write_package("game", &[("core", "../engine_core")]);
        fixture.write_package("engine_core", &[]);

        validate_ok(fixture.manifest("game"));
    }
}
