use std::{
    collections::HashMap,
    path::{Path, PathBuf},
};

use anvyx_lang2::{
    CheckFileInput, CheckPackageInput, CheckResult, FrontendConfig, PackageId as FrontendPackageId,
    PackageSource, SourceOverride,
};

use crate::{
    manifest::{Manifest, PackageGraph, PackageId},
    source_bundle,
};

pub fn check_path(file: &Path, config: FrontendConfig) -> Result<CheckResult, String> {
    check_path_with_source_overrides(file, vec![], config)
}

pub fn check_path_with_source_overrides(
    file: &Path,
    source_overrides: Vec<SourceOverride>,
    config: FrontendConfig,
) -> Result<CheckResult, String> {
    check_loaded_path(file, source_overrides, config, load_nearest_manifest(file)?)
}

pub fn check_path_with_manifest_lints(
    file: &Path,
    source_overrides: Vec<SourceOverride>,
) -> Result<CheckResult, String> {
    let manifest = load_nearest_manifest(file)?;
    let lint = match &manifest {
        Some((_, manifest)) => crate::manifest::lint_config(Some(manifest), &[] as &[String])?,
        None => Default::default(),
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

fn check_loaded_path(
    file: &Path,
    source_overrides: Vec<SourceOverride>,
    config: FrontendConfig,
    manifest: Option<(PathBuf, Manifest)>,
) -> Result<CheckResult, String> {
    let Some((path, manifest)) = manifest else {
        let input =
            standalone_check_input_with_overrides(file, source_overrides)?.with_config(config);
        return Ok(anvyx_lang2::check_file(input));
    };
    crate::manifest::reject_clean_frontend_inputs(Some(&manifest))?;
    let graph = crate::manifest::load_package_graph(&path)?;
    let input =
        package_check_input_with_overrides(&graph, file, source_overrides)?.with_config(config);
    Ok(anvyx_lang2::check_package(input))
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
        frontend_package_id(&graph.root().id),
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
                .map(|(alias, id)| (alias.clone(), frontend_package_id(id)))
                .collect::<HashMap<_, _>>();
            match &package.source {
                Some(source) => PackageSource::new(
                    frontend_package_id(&package.id),
                    source.entry.clone(),
                    source.source_root.clone(),
                    dependencies,
                )
                .map_err(|error| error.to_string()),
                None => Ok(PackageSource::native_only(
                    frontend_package_id(&package.id),
                    dependencies,
                )),
            }
        })
        .collect()
}

fn frontend_package_id(id: &PackageId) -> FrontendPackageId {
    FrontendPackageId::new(id.manifest_path().display().to_string())
}

#[cfg(test)]
mod tests {
    use std::fs;

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
            let dir = self.root.path().join(package);
            fs::create_dir_all(dir.join("src")).unwrap();
            let mut manifest = "[project]\nentry = \"src/main.anv\"\n".to_string();
            if !deps.is_empty() {
                manifest.push_str("\n[dependencies]\n");
                for (alias, path) in deps {
                    manifest.push_str(&format!("{alias} = {{ path = \"{path}\" }}\n"));
                }
            }
            fs::write(dir.join("anvyx.toml"), manifest).unwrap();
            fs::write(dir.join("src/main.anv"), "fn main() {}\n").unwrap();
        }

        fn manifest(&self, package: &str) -> PathBuf {
            self.root.path().join(package).join("anvyx.toml")
        }

        fn entry(&self, package: &str) -> PathBuf {
            self.root.path().join(package).join("src/main.anv")
        }
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
