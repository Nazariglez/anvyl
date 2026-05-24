use std::{
    collections::HashMap,
    path::{Path, PathBuf},
};

use anvyx_lang2::{
    AirBuildError, AirBuildOutput, CheckFileInput, CheckOutput, CheckPackageInput, FrontendConfig,
    PackageId as FrontendPackageId, PackageSource, SourceOverride,
};

use crate::{
    manifest::{Manifest, PackageGraph, PackageId},
    source_bundle,
};

#[derive(Debug)]
pub enum CleanRustRunError {
    Air(AirBuildError<anvyx_lang2::CheckError>),
    Plan(anvyx_backend::rust::RustPlanError),
    SourceJob(anvyx_backend::rust::source_job::RustSourceJobError),
    Run(anvyx_backend::rust::source_job::RustSourceJobOutput),
}

impl std::fmt::Display for CleanRustRunError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Air(error) => write!(f, "{}", air_error_ref(error)),
            Self::Plan(error) => write!(f, "{error}"),
            Self::SourceJob(error) => write!(f, "{error}"),
            Self::Run(output) => {
                write!(f, "generated Rust job failed: {:?}", output.status)?;
                if !output.stderr.is_empty() {
                    write!(f, "\n{}", output.stderr)?;
                }
                Ok(())
            }
        }
    }
}

impl std::error::Error for CleanRustRunError {}

pub struct CleanRustRunOutput {
    pub stdout: String,
    pub stderr: String,
}

pub fn run_clean_rust_path(
    file: &Path,
    config: FrontendConfig,
) -> Result<CleanRustRunOutput, CleanRustRunError> {
    let output = build_air_path_typed(file, config).map_err(CleanRustRunError::Air)?;
    let plan = anvyx_backend::rust::plan(
        &output.air.as_verified(),
        anvyx_backend::rust::RustPlanConfig::default(),
    )
    .map_err(CleanRustRunError::Plan)?;
    let source = anvyx_backend::rust::emit::emit(&plan.verified());
    let output = anvyx_backend::rust::source_job::compile_and_run(
        &anvyx_backend::rust::source_job::RustSourceJob {
            source,
            work_dir: None,
        },
    )
    .map_err(CleanRustRunError::SourceJob)?;
    if output.status != anvyx_backend::rust::source_job::SourceJobStatus::Success {
        return Err(CleanRustRunError::Run(output));
    }
    Ok(CleanRustRunOutput {
        stdout: output.stdout,
        stderr: output.stderr,
    })
}

fn build_air_path_typed(
    file: &Path,
    config: FrontendConfig,
) -> Result<AirBuildOutput, AirBuildError<anvyx_lang2::CheckError>> {
    build_air_path_with_source_overrides_typed(file, vec![], config)
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
    let Some((path, manifest)) = manifest else {
        let input = standalone_check_input_with_overrides(file, source_overrides)
            .map_err(|error| AirBuildError::Fatal(anvyx_lang2::CheckError::InvalidInput(error)))?
            .with_config(config);
        return anvyx_lang2::build_air_file(input);
    };
    crate::manifest::reject_clean_frontend_inputs(Some(&manifest))
        .map_err(|error| AirBuildError::Fatal(anvyx_lang2::CheckError::InvalidInput(error)))?;
    let graph = crate::manifest::load_package_graph(&path)
        .map_err(|error| AirBuildError::Fatal(anvyx_lang2::CheckError::InvalidInput(error)))?;
    let input = package_check_input_with_overrides(&graph, file, source_overrides)
        .map_err(|error| AirBuildError::Fatal(anvyx_lang2::CheckError::InvalidInput(error)))?
        .with_config(config);
    anvyx_lang2::build_air_package(input)
}

fn air_error_ref(error: &AirBuildError<anvyx_lang2::CheckError>) -> String {
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
    let Some((path, manifest)) = manifest else {
        let input =
            standalone_check_input_with_overrides(file, source_overrides)?.with_config(config);
        return anvyx_lang2::check_file(input).map_err(|error| error.to_string());
    };
    crate::manifest::reject_clean_frontend_inputs(Some(&manifest))?;
    let graph = crate::manifest::load_package_graph(&path)?;
    let input =
        package_check_input_with_overrides(&graph, file, source_overrides)?.with_config(config);
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
    use std::{fmt::Write, fs};

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
                    writeln!(manifest, "{alias} = {{ path = \"{path}\" }}").unwrap();
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
