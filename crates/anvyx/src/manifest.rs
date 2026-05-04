use std::{
    collections::HashMap,
    fs,
    path::{Path, PathBuf},
};

use anvyx_lang::LintConfig;
use serde::Deserialize;

#[derive(Debug, Deserialize)]
pub struct Manifest {
    pub project: Project,
    #[serde(default)]
    pub dependencies: HashMap<String, DependencyEntry>,
    #[serde(default)]
    pub externs: HashMap<String, ExternEntry>,
    #[serde(default)]
    pub lint: LintConfig,
}

impl Manifest {
    pub fn has_externs(&self) -> bool {
        !self.externs.is_empty()
    }
}

#[derive(Debug, Deserialize)]
pub struct Project {
    pub name: Option<String>,
    pub entry: String,
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
    toml::from_str(&contents).map_err(|e| format!("Failed to parse {}: {e}", path.display()))
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
pub struct PackageNode {
    pub id: PackageId,
    pub entry: PathBuf,
    pub source_root: PathBuf,
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
        let entry = dir.join(&manifest.project.entry);
        let source_root = entry
            .parent()
            .filter(|parent| !parent.as_os_str().is_empty())
            .unwrap_or_else(|| Path::new("."))
            .to_path_buf();
        self.packages.push(PackageNode {
            id: id.clone(),
            entry,
            source_root,
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
        toml::from_str(toml).map_err(|e| format!("Failed to parse: {e}"))
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
        assert_eq!(manifest.project.entry, "src/main.anv");
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
    fn parse_manifest_lint_error() {
        use anvyx_lang::LintLevel;
        let manifest = parse(
            r#"
            [project]
            entry = "src/main.anv"

            [lint]
            internal_access = "error"
            "#,
        )
        .unwrap();

        assert_eq!(manifest.lint.internal_access, LintLevel::Error);
    }

    #[test]
    fn parse_manifest_lint_allow() {
        use anvyx_lang::LintLevel;
        let manifest = parse(
            r#"
            [project]
            entry = "src/main.anv"

            [lint]
            internal_access = "allow"
            "#,
        )
        .unwrap();

        assert_eq!(manifest.lint.internal_access, LintLevel::Allow);
    }

    #[test]
    fn parse_manifest_lint_default() {
        use anvyx_lang::LintLevel;
        let manifest = parse(
            r#"
            [project]
            entry = "src/main.anv"
            "#,
        )
        .unwrap();

        assert_eq!(manifest.lint.internal_access, LintLevel::Warn);
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
