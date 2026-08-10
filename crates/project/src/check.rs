use std::{
    collections::HashMap,
    path::{Path, PathBuf},
};

use anvyx_lang::{
    AirBuildError, CheckFileInput, CheckOutput, CheckPackageInput, FrontendConfig, PackageSource,
    SourceOverride,
};

use crate::{
    manifest::{Manifest, PackageGraph, package_frontend_id},
    source_bundle,
};

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
        None => anvyx_lang::LintConfig::default(),
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

pub(crate) fn air_error_ref(error: &AirBuildError<anvyx_lang::CheckError>) -> String {
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
        let input = standalone_check_input(file)?
            .with_source_overrides(source_overrides)
            .with_config(config);
        let world = crate::manifest::system_provider_world()?;
        return anvyx_lang::check_file(input, &world.catalog).map_err(|error| error.to_string());
    };
    let graph = crate::manifest::load_package_graph(&path)?;
    let input = package_check_input(&graph, file)?
        .with_source_overrides(source_overrides)
        .with_config(config);
    anvyx_lang::check_package(input, &graph.provider_world().catalog)
        .map_err(|error| error.to_string())
}

pub fn standalone_check_input(file: &Path) -> Result<CheckFileInput, String> {
    CheckFileInput::new(file.to_path_buf(), source_bundle()?).map_err(|error| error.to_string())
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
