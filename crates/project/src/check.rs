use std::{
    collections::HashMap,
    path::{Path, PathBuf},
};

use anvyx_lang2::{
    AirBuildError, AirBuildOutput, CheckFileInput, CheckOutput, CheckPackageInput, FrontendConfig,
    PackageSource, SourceOverride,
};

use crate::{
    manifest::{Manifest, PackageGraph, package_frontend_id},
    source_bundle,
};

pub(crate) fn build_air_path_typed(
    file: &Path,
    config: FrontendConfig,
) -> Result<AirBuildOutput, AirBuildError<anvyx_lang2::CheckError>> {
    build_air_path_with_source_overrides_typed(file, vec![], config)
}

pub(crate) fn build_air_path_with_graph_typed(
    file: &Path,
    config: FrontendConfig,
    graph: &PackageGraph,
) -> Result<AirBuildOutput, AirBuildError<anvyx_lang2::CheckError>> {
    let input = package_check_input_with_overrides(graph, file, vec![])
        .map_err(|error| AirBuildError::Fatal(anvyx_lang2::CheckError::InvalidInput(error)))?
        .with_package_externs(graph.package_externs())
        .with_config(config);
    anvyx_lang2::build_air_package(input)
}

fn build_air_path_with_source_overrides_typed(
    file: &Path,
    source_overrides: Vec<SourceOverride>,
    config: FrontendConfig,
) -> Result<AirBuildOutput, AirBuildError<anvyx_lang2::CheckError>> {
    let manifest = load_nearest_manifest(file)
        .map_err(|error| AirBuildError::Fatal(anvyx_lang2::CheckError::InvalidInput(error)))?;
    build_air_loaded_path_typed(file, source_overrides, config, manifest)
}

pub fn check_path(file: &Path, config: FrontendConfig) -> Result<CheckOutput, String> {
    check_path_with_source_overrides(file, vec![], config)
}

pub fn check_path_with_source_overrides(
    file: &Path,
    source_overrides: Vec<SourceOverride>,
    config: FrontendConfig,
) -> Result<CheckOutput, String> {
    check_loaded_path(file, source_overrides, config, load_nearest_manifest(file)?)
}

pub fn check_path_with_manifest_lints(
    file: &Path,
    source_overrides: Vec<SourceOverride>,
) -> Result<CheckOutput, String> {
    let manifest = load_nearest_manifest(file)?;
    let lint = match &manifest {
        Some((_, manifest)) => crate::manifest::lint_config(Some(manifest), &[] as &[String])?,
        None => anvyx_lang2::LintConfig::default(),
    };
    check_loaded_path(
        file,
        source_overrides,
        FrontendConfig {
            lint,
            ..FrontendConfig::default()
        },
        manifest,
    )
}

fn load_nearest_manifest(file: &Path) -> Result<Option<(PathBuf, Manifest)>, String> {
    let Some(path) = crate::manifest::find_nearest_manifest(file)? else {
        return Ok(None);
    };
    let manifest = crate::manifest::parse_manifest_file(&path)?;
    Ok(Some((path, manifest)))
}

fn build_air_loaded_path_typed(
    file: &Path,
    source_overrides: Vec<SourceOverride>,
    config: FrontendConfig,
    manifest: Option<(PathBuf, Manifest)>,
) -> Result<AirBuildOutput, AirBuildError<anvyx_lang2::CheckError>> {
    let Some((path, _manifest)) = manifest else {
        let input = standalone_check_input_with_overrides(file, source_overrides)
            .map_err(|error| AirBuildError::Fatal(anvyx_lang2::CheckError::InvalidInput(error)))?
            .with_config(config);
        return anvyx_lang2::build_air_file(input);
    };
    let graph = crate::manifest::load_package_graph(&path)
        .map_err(|error| AirBuildError::Fatal(anvyx_lang2::CheckError::InvalidInput(error)))?;
    let input = package_check_input_with_overrides(&graph, file, source_overrides)
        .map_err(|error| AirBuildError::Fatal(anvyx_lang2::CheckError::InvalidInput(error)))?
        .with_package_externs(graph.package_externs())
        .with_config(config);
    anvyx_lang2::build_air_package(input)
}

pub(crate) fn air_error_ref(error: &AirBuildError<anvyx_lang2::CheckError>) -> String {
    match error {
        AirBuildError::Diagnostic(output) => output.summary().to_string(),
        AirBuildError::Lower(message) => format!("failed to lower AIR: {message}"),
        AirBuildError::Fatal(error) => error.to_string(),
    }
}

fn check_loaded_path(
    file: &Path,
    source_overrides: Vec<SourceOverride>,
    config: FrontendConfig,
    manifest: Option<(PathBuf, Manifest)>,
) -> Result<CheckOutput, String> {
    let Some((path, _manifest)) = manifest else {
        let input =
            standalone_check_input_with_overrides(file, source_overrides)?.with_config(config);
        return anvyx_lang2::check_file(input).map_err(|error| error.to_string());
    };
    let graph = crate::manifest::load_package_graph(&path)?;
    let input = package_check_input_with_overrides(&graph, file, source_overrides)?
        .with_package_externs(graph.package_externs())
        .with_config(config);
    anvyx_lang2::check_package(input).map_err(|error| error.to_string())
}

pub fn standalone_check_input(file: &Path) -> Result<CheckFileInput, String> {
    CheckFileInput::new(file.to_path_buf(), source_bundle()?).map_err(|error| error.to_string())
}

pub fn standalone_check_input_with_overrides(
    file: &Path,
    source_overrides: Vec<SourceOverride>,
) -> Result<CheckFileInput, String> {
    Ok(standalone_check_input(file)?.with_source_overrides(source_overrides))
}

pub fn package_check_input(graph: &PackageGraph, file: &Path) -> Result<CheckPackageInput, String> {
    CheckPackageInput::new(
        package_frontend_id(&graph.root().id),
        file.to_path_buf(),
        package_sources(graph)?,
        source_bundle()?,
    )
    .map_err(|error| error.to_string())
}

pub fn package_check_input_with_overrides(
    graph: &PackageGraph,
    file: &Path,
    source_overrides: Vec<SourceOverride>,
) -> Result<CheckPackageInput, String> {
    Ok(package_check_input(graph, file)?.with_source_overrides(source_overrides))
}

fn package_sources(graph: &PackageGraph) -> Result<Vec<PackageSource>, String> {
    graph
        .packages()
        .iter()
        .map(|package| {
            let dependencies = package
                .dependencies
                .iter()
                .map(|(alias, id)| (alias.clone(), package_frontend_id(id)))
                .collect::<HashMap<_, _>>();
            match &package.source {
                Some(source) => PackageSource::new(
                    package_frontend_id(&package.id),
                    source.entry.clone(),
                    source.source_root.clone(),
                    dependencies,
                )
                .map_err(|error| error.to_string()),
                None => Ok(PackageSource::native_only(
                    package_frontend_id(&package.id),
                    dependencies,
                )),
            }
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use std::{fmt::Write, fs, path::Path};

    use anvyx_lang2::{CheckPhase, CheckStatus};

    use super::*;

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
            self.write_source_package(package, "fn main() {}\n", deps);
        }

        fn write_source_package(&self, package: &str, source: &str, deps: &[(&str, &str)]) {
            let dir = self.root.path().join(package);
            fs::create_dir_all(dir.join("src")).unwrap();
            fs::write(
                dir.join("anvyx.toml"),
                source_manifest(Some("src/main.anv"), deps),
            )
            .unwrap();
            fs::write(dir.join("src/main.anv"), source).unwrap();
        }

        fn write_native_package(&self, package: &str, module: &str, source: Option<&str>) {
            let dir = self.root.path().join(package);
            fs::create_dir_all(dir.join("src")).unwrap();
            fs::write(
                dir.join("anvyx.toml"),
                source_manifest(source.map(|_| "src/main.anv"), &[]),
            )
            .unwrap();
            if let Some(source) = source {
                fs::write(dir.join("src/main.anv"), source).unwrap();
            }
            write_provider_crate(&dir, module);
        }

        fn manifest(&self, package: &str) -> PathBuf {
            self.root.path().join(package).join("anvyx.toml")
        }

        fn entry(&self, package: &str) -> PathBuf {
            self.root.path().join(package).join("src/main.anv")
        }
    }

    fn source_manifest(entry: Option<&str>, deps: &[(&str, &str)]) -> String {
        let mut manifest = "[project]\n".to_string();
        if let Some(entry) = entry {
            writeln!(manifest, "entry = \"{entry}\"").unwrap();
        }
        if !deps.is_empty() {
            manifest.push_str("\n[dependencies]\n");
            for (alias, path) in deps {
                writeln!(manifest, "{alias} = {{ path = \"{path}\" }}").unwrap();
            }
        }
        manifest
    }

    fn quoted_path(path: &Path) -> String {
        format!("\"{}\"", path.display())
    }

    fn check_succeeds(file: &Path) {
        let output = check_path(file, FrontendConfig::default()).unwrap();
        assert_eq!(output.status, CheckStatus::Passed, "{}", output.summary());
    }

    fn check_fails(file: &Path, phase: CheckPhase, message: &str) {
        let output = check_path(file, FrontendConfig::default()).unwrap();
        assert_eq!(
            output.status,
            CheckStatus::Failed { phase },
            "{}",
            output.summary()
        );
        let messages = output
            .report
            .diagnostics()
            .iter()
            .map(anvyx_lang2::Diagnostic::message)
            .collect::<Vec<_>>();
        assert!(
            messages
                .iter()
                .any(|diagnostic| diagnostic.contains(message)),
            "expected diagnostic containing {message:?}, got {messages:?}"
        );
    }

    fn write_provider_crate(dir: &Path, module: &str) {
        fs::write(
            dir.join("Cargo.toml"),
            format!(
                "[package]\nname = \"native\"\nversion = \"0.1.0\"\nedition = \"2024\"\n\n[dependencies]\nanvyx-runtime = {{ path = {} }}\n",
                quoted_path(&Path::new(env!("CARGO_MANIFEST_DIR")).parent().unwrap().join("runtime"))
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

    #[test]
    fn root_source_native_package_can_import_own_provider() {
        let fixture = PackageFixture::default();
        fixture.write_native_package(
            "game",
            "host",
            Some("import ext:host { ping }; fn main() { let x: int = ping(); }"),
        );

        check_succeeds(&fixture.entry("game"));
    }

    #[test]
    fn native_only_dependency_imports_provider_through_pkg_module() {
        let fixture = PackageFixture::default();
        fixture.write_source_package(
            "game",
            "import pkg:host.host { ping }; fn main() { let x: int = ping(); }",
            &[("host", "../host")],
        );
        fixture.write_native_package("host", "host", None);

        check_succeeds(&fixture.entry("game"));
    }

    #[test]
    fn native_only_dependency_root_import_fails() {
        let fixture = PackageFixture::default();
        fixture.write_source_package(
            "game",
            "import pkg:host; fn main() {}",
            &[("host", "../host")],
        );
        fixture.write_native_package("host", "host", None);

        check_fails(
            &fixture.entry("game"),
            CheckPhase::Resolve,
            "has no source root",
        );
    }

    #[test]
    fn source_native_dependency_uses_internal_ext_and_exports_source_api() {
        let fixture = PackageFixture::default();
        fixture.write_source_package(
            "game",
            "import pkg:colors { mix }; fn main() { let x: int = mix(); }",
            &[("colors", "../colors")],
        );
        fixture.write_native_package(
            "colors",
            "host",
            Some("import ext:host { ping }; pub fn mix() -> int { ping() }"),
        );

        check_succeeds(&fixture.entry("game"));
    }

    #[test]
    fn source_native_dependency_provider_is_hidden_from_consumer() {
        let fixture = PackageFixture::default();
        fixture.write_source_package(
            "game",
            "import pkg:colors.host { ping }; fn main() { let x: int = ping(); }",
            &[("colors", "../colors")],
        );
        fixture.write_native_package("colors", "host", Some("pub fn mix() -> int { 1 }"));

        check_fails(
            &fixture.entry("game"),
            CheckPhase::Type,
            "Unknown member 'host'",
        );
    }

    #[test]
    fn check_input_preserves_root_dependencies_and_sources() {
        let fixture = PackageFixture::default();
        fixture.write_package("game", &[("math", "../math")]);
        fixture.write_package("math", &[]);
        let graph = crate::manifest::load_package_graph(&fixture.manifest("game")).unwrap();

        let input = package_check_input(&graph, &fixture.entry("game")).unwrap();

        let entry = fixture.entry("game").canonicalize().unwrap();
        let source_root = fixture.root.path().join("game/src").canonicalize().unwrap();

        assert_eq!(input.root_file(), fixture.entry("game"));
        assert!(input.sources().core().is_some());
        assert!(input.sources().std().is_some());
        let root = input
            .packages()
            .iter()
            .find(|package| package.id() == input.root_package())
            .unwrap();
        assert_eq!(root.entry(), Some(entry.as_path()));
        assert_eq!(root.source_root(), Some(source_root.as_path()));
        assert_eq!(root.dependencies().len(), 1);
    }

    #[test]
    fn check_input_keeps_package_source_overrides() {
        let fixture = PackageFixture::default();
        fixture.write_package("game", &[]);
        let graph = crate::manifest::load_package_graph(&fixture.manifest("game")).unwrap();
        let override_path = fixture.entry("game");
        let source_overrides = vec![SourceOverride::new(&override_path, "fn main() {}\n").unwrap()];

        let input =
            package_check_input_with_overrides(&graph, &override_path, source_overrides).unwrap();

        assert_eq!(input.source_overrides()[0].path(), override_path.as_path());
    }

    #[test]
    fn check_input_keeps_standalone_source_overrides() {
        let fixture = PackageFixture::default();
        fixture.write_package("game", &[]);
        let override_path = fixture.entry("game");
        let source_overrides = vec![SourceOverride::new(&override_path, "fn main() {}\n").unwrap()];

        let input =
            standalone_check_input_with_overrides(&override_path, source_overrides).unwrap();

        assert_eq!(input.file(), override_path.as_path());
        assert_eq!(input.source_overrides()[0].path(), override_path.as_path());
    }
}
