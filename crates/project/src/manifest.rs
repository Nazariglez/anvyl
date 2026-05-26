use std::{
    collections::{BTreeMap, HashMap, hash_map::DefaultHasher},
    fs,
    hash::{Hash, Hasher},
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

const PROVIDER_PROBE_TIMEOUT: Duration = Duration::from_secs(30);

pub type ManifestLint = BTreeMap<String, String>;

#[derive(Debug, Deserialize)]
pub struct Manifest {
    pub project: Project,
    #[serde(default)]
    pub dependencies: HashMap<String, DependencyEntry>,
    #[serde(default)]
    pub externs: HashMap<String, ExternEntry>,
    #[serde(default)]
    pub lint: ManifestLint,
}

impl Manifest {
    pub fn has_externs(&self) -> bool {
        !self.externs.is_empty()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NativeProviderLoad {
    pub providers: Vec<LoadedExternProvider>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LoadedExternProvider {
    pub name: String,
    pub crate_root: PathBuf,
    pub cargo_manifest: PathBuf,
    pub cargo_package: String,
    pub cargo_alias: String,
    pub descriptor: ProviderDescriptor,
    pub supports: Vec<RustModuleSupport>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct ValidatedExternProvider {
    name: String,
    crate_root: PathBuf,
    cargo_manifest: PathBuf,
    cargo_package: String,
    cargo_alias: String,
}

pub fn load_native_providers(
    manifest_path: &Path,
    manifest: &Manifest,
) -> Result<NativeProviderLoad, String> {
    let providers = validate_extern_providers(manifest_path, manifest)?
        .into_iter()
        .map(load_extern_provider)
        .collect::<Result<Vec<_>, _>>()?;
    Ok(NativeProviderLoad { providers })
}

fn validate_extern_providers(
    manifest_path: &Path,
    manifest: &Manifest,
) -> Result<Vec<ValidatedExternProvider>, String> {
    let root = manifest_path
        .parent()
        .ok_or_else(|| format!("manifest path has no parent: {}", manifest_path.display()))?;
    let mut providers = manifest.externs.iter().collect::<Vec<_>>();
    providers.sort_unstable_by_key(|(name, _)| name.as_str());
    providers
        .into_iter()
        .map(|(name, ext)| validate_extern_provider(root, name, ext))
        .collect()
}

fn validate_extern_provider(
    root: &Path,
    name: &str,
    ext: &ExternEntry,
) -> Result<ValidatedExternProvider, String> {
    anvyx_lang2::validate_dependency_alias(name)
        .map_err(|_| format!("invalid extern provider name `{name}`"))?;
    let path = root.join(&ext.path);
    if !path.exists() {
        return Err(format!(
            "extern provider `{name}` path does not exist: {}",
            path.display()
        ));
    }
    let cargo_manifest = path.join("Cargo.toml");
    if !cargo_manifest.is_file() {
        return Err(format!(
            "extern provider `{name}` must point to a Rust crate with Cargo.toml: {}",
            path.display()
        ));
    }
    let crate_root = path.canonicalize().map_err(|error| {
        format!(
            "failed to canonicalize extern provider `{name}` path {}: {error}",
            path.display()
        )
    })?;
    let cargo_manifest = crate_root.join("Cargo.toml");
    let cargo = parse_provider_cargo(&cargo_manifest, name)?;
    Ok(ValidatedExternProvider {
        name: name.to_string(),
        crate_root,
        cargo_manifest,
        cargo_package: cargo.package.name,
        cargo_alias: provider_cargo_alias(name),
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

fn parse_provider_cargo(path: &Path, name: &str) -> Result<ProviderCargoManifest, String> {
    let text = fs::read_to_string(path).map_err(|error| {
        format!(
            "failed to read extern provider `{name}` Cargo.toml {}: {error}",
            path.display()
        )
    })?;
    toml::from_str(&text).map_err(|error| {
        format!(
            "failed to parse extern provider `{name}` Cargo.toml {}: {error}",
            path.display()
        )
    })
}

fn load_extern_provider(provider: ValidatedExternProvider) -> Result<LoadedExternProvider, String> {
    let output = run_provider_probe(&provider)?;
    if output.descriptor.provider.name != provider.name {
        return Err(format!(
            "extern provider `{}` crate reports provider `{}`",
            provider.name, output.descriptor.provider.name
        ));
    }
    let support = RustProviderSupport {
        package: provider.name.clone(),
        provider: output.descriptor.provider.clone(),
        cargo: RustProviderCargo {
            manifest_key: provider.cargo_alias.clone(),
            package: Some(provider.cargo_package.clone()),
            path: Some(provider.crate_root.clone()),
            features: vec![],
            default_features: true,
        },
        modules: output.supports.clone(),
    };
    validate_rust_provider_support(std::slice::from_ref(&output.descriptor), &[support]).map_err(
        |error| {
            format!(
                "extern provider `{}` has invalid native support: {error}",
                provider.name
            )
        },
    )?;
    Ok(LoadedExternProvider {
        name: provider.name,
        crate_root: provider.crate_root,
        cargo_manifest: provider.cargo_manifest,
        cargo_package: provider.cargo_package,
        cargo_alias: provider.cargo_alias,
        descriptor: output.descriptor,
        supports: output.supports,
    })
}

fn run_provider_probe(provider: &ValidatedExternProvider) -> Result<ProviderProbeOutput, String> {
    let dir = provider_probe_dir(provider);
    fs::create_dir_all(dir.join("src")).map_err(|error| {
        format!(
            "failed to create extern provider `{}` probe at {}: {error}",
            provider.name,
            dir.display()
        )
    })?;
    fs::write(dir.join("Cargo.toml"), provider_probe_manifest(provider)).map_err(|error| {
        format!(
            "failed to write extern provider `{}` probe manifest: {error}",
            provider.name
        )
    })?;
    fs::write(dir.join("src/main.rs"), provider_probe_main(provider)).map_err(|error| {
        format!(
            "failed to write extern provider `{}` probe main: {error}",
            provider.name
        )
    })?;

    let stdout_path = dir.join("stdout.json");
    let stderr_path = dir.join("stderr.txt");
    let stdout_file = fs::File::create(&stdout_path).map_err(|error| {
        format!(
            "failed to create extern provider `{}` probe stdout file: {error}",
            provider.name
        )
    })?;
    let stderr_file = fs::File::create(&stderr_path).map_err(|error| {
        format!(
            "failed to create extern provider `{}` probe stderr file: {error}",
            provider.name
        )
    })?;
    let mut child = Command::new("cargo")
        .args(["run", "--quiet"])
        .current_dir(&dir)
        .stdin(Stdio::null())
        .stdout(Stdio::from(stdout_file))
        .stderr(Stdio::from(stderr_file))
        .spawn()
        .map_err(|error| {
            format!(
                "failed to run extern provider `{}` probe: {error}",
                provider.name
            )
        })?;
    let Some(status) = child
        .wait_timeout(PROVIDER_PROBE_TIMEOUT)
        .map_err(|error| {
            format!(
                "failed to wait for extern provider `{}` probe: {error}",
                provider.name
            )
        })?
    else {
        let _ = child.kill();
        let _ = child.wait();
        let _ = fs::remove_dir_all(&dir);
        return Err(format!(
            "extern provider `{}` probe timed out",
            provider.name
        ));
    };
    let stdout = fs::read(&stdout_path).unwrap_or_default();
    let stderr = fs::read(&stderr_path).unwrap_or_default();
    let _ = fs::remove_dir_all(&dir);
    if !status.success() {
        return Err(format!(
            "extern provider `{}` probe failed\n{}",
            provider.name,
            String::from_utf8_lossy(&stderr)
        ));
    }
    serde_json::from_slice(&stdout).map_err(|error| {
        format!(
            "extern provider `{}` probe emitted invalid metadata: {error}",
            provider.name
        )
    })
}

fn provider_probe_dir(provider: &ValidatedExternProvider) -> PathBuf {
    let stamp = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("system time is after unix epoch")
        .as_nanos();
    let mut hash = DefaultHasher::new();
    provider.crate_root.hash(&mut hash);
    let hash = hash.finish();
    std::env::temp_dir().join(format!(
        "anvyx-provider-{}-{}-{hash:x}-{stamp}",
        std::process::id(),
        provider.name
    ))
}

fn provider_probe_manifest(provider: &ValidatedExternProvider) -> String {
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

fn provider_probe_main(provider: &ValidatedExternProvider) -> String {
    render_probe_template(
        include_str!("templates/provider_probe_main.rs.in"),
        &[("provider_crate", provider.cargo_alias.clone())],
    )
}

fn provider_cargo_alias(name: &str) -> String {
    format!("anvyx_provider_{name}")
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

#[derive(Debug, Deserialize)]
pub struct ExternEntry {
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
        validate_extern_providers(id.manifest_path(), &manifest).map_err(|error| {
            format!("failed to validate extern providers for package {id}: {error}")
        })?;
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
        self.packages.push(PackageNode {
            id: id.clone(),
            source,
            native,
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

    #[test]
    fn parse_manifest_no_externs() {
        let manifest = parse(
            r#"
            [project]
            entry = "src/main.anv"
            "#,
        )
        .unwrap();

        assert!(manifest.externs.is_empty());
        assert!(!manifest.has_externs());
        assert_eq!(manifest.project.entry.as_deref(), Some("src/main.anv"));
    }

    #[test]
    fn parse_manifest_with_one_extern() {
        let manifest = parse(
            r#"
            [project]
            entry = "src/main.anv"

            [externs.engine]
            path = "my_externs/engine"
            "#,
        )
        .unwrap();

        assert!(manifest.has_externs());
        assert_eq!(manifest.externs.len(), 1);
        assert_eq!(manifest.externs["engine"].path, "my_externs/engine");
    }

    #[test]
    fn parse_manifest_with_multiple_externs() {
        let manifest = parse(
            r#"
            [project]
            entry = "src/main.anv"

            [externs.engine]
            path = "my_externs/engine"

            [externs.audio]
            path = "my_externs/audio"
            "#,
        )
        .unwrap();

        assert!(manifest.has_externs());
        assert_eq!(manifest.externs.len(), 2);
        assert_eq!(manifest.externs["engine"].path, "my_externs/engine");
        assert_eq!(manifest.externs["audio"].path, "my_externs/audio");
    }

    #[test]
    fn clean_frontend_validates_extern_provider_paths() {
        let root = tempfile::tempdir().unwrap();
        let manifest_path = root.path().join("anvyx.toml");
        let manifest = parse(
            r#"
            [project]
            entry = "src/main.anv"

            [externs.engine]
            path = "my_externs/engine"
            "#,
        )
        .unwrap();

        let error = load_native_providers(&manifest_path, &manifest).unwrap_err();
        assert!(error.contains("extern provider `engine` path does not exist"));

        let provider = root.path().join("my_externs/engine");
        fs::create_dir_all(&provider).unwrap();
        fs::write(
            provider.join("Cargo.toml"),
            "[package]\nname = \"engine\"\nversion = \"0.1.0\"\nedition = \"2024\"\n",
        )
        .unwrap();
        let error = load_native_providers(&manifest_path, &manifest).unwrap_err();
        assert!(error.contains("extern provider `engine` probe failed"));
    }

    #[test]
    fn clean_frontend_rejects_invalid_extern_provider_names() {
        let root = tempfile::tempdir().unwrap();
        let manifest_path = root.path().join("anvyx.toml");
        let manifest = parse(
            r#"
            [project]
            entry = "src/main.anv"

            [externs.BadName]
            path = "provider"
            "#,
        )
        .unwrap();

        let error = load_native_providers(&manifest_path, &manifest).unwrap_err();
        assert_eq!(error, "invalid extern provider name `BadName`");
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

    fn provider_fixture(name: &str, cargo_package: &str) -> ValidatedExternProvider {
        let root = PathBuf::from(format!("/tmp/{cargo_package}"));
        ValidatedExternProvider {
            name: name.to_string(),
            cargo_manifest: root.join("Cargo.toml"),
            crate_root: root,
            cargo_package: cargo_package.to_string(),
            cargo_alias: provider_cargo_alias(name),
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
            let mut manifest = "[project]\nname = \"native\"\n".to_string();
            if !deps.is_empty() {
                manifest.push_str("\n[dependencies]\n");
                for (alias, path) in deps {
                    writeln!(manifest, "{alias} = {{ path = \"{path}\" }}").unwrap();
                }
            }
            self.write_raw_manifest(package, &manifest);
            fs::write(
                self.root.path().join(package).join("Cargo.toml"),
                "[package]\nname = \"native\"\nversion = \"0.1.0\"\nedition = \"2024\"\n",
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

        assert!(!manifest.has_externs());
        assert_eq!(manifest.dependencies.len(), 2);
        assert_eq!(manifest.dependencies["math"].path, "../math");
        assert_eq!(manifest.dependencies["physics"].path, "../physics");
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
    fn native_only_dependency_with_cargo_marker_is_allowed() {
        let fixture = PackageFixture::default();
        fixture.write_package("game", &[("host", "../host")]);
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
    fn same_canonical_dependency_reached_from_different_parents_is_allowed() {
        let fixture = PackageFixture::default();
        fixture.write_package("game", &[("ui", "../ui"), ("audio", "../audio")]);
        fixture.write_package("ui", &[("math", "../math")]);
        fixture.write_package("audio", &[("m", "../math")]);
        fixture.write_package("math", &[]);

        validate_ok(fixture.manifest("game"));
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
