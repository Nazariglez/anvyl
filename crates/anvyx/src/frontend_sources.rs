use anvyx_lang2::{ModuleSource, SourceBundle, SourceText, SystemPackageSource};

pub(crate) fn source_bundle() -> Result<SourceBundle, String> {
    Ok(SourceBundle::new(
        Some(core_package()?),
        Some(std_package()?),
    ))
}

fn core_package() -> Result<SystemPackageSource, String> {
    SystemPackageSource::with_providers(
        core_source_text(&anvyx_core2::ROOT)?,
        anvyx_core2::MODULES
            .iter()
            .map(core_module_source)
            .collect::<Result<Vec<_>, _>>()?,
        anvyx_core2::provider_descriptors(),
    )
    .map_err(|error| error.to_string())
}

fn std_package() -> Result<SystemPackageSource, String> {
    SystemPackageSource::with_providers(
        std_source_text(&anvyx_stdlib2::ROOT)?,
        anvyx_stdlib2::MODULES
            .iter()
            .map(std_module_source)
            .collect::<Result<Vec<_>, _>>()?,
        anvyx_stdlib2::provider_descriptors(),
    )
    .map_err(|error| error.to_string())
}

fn core_source_text(source: &anvyx_core2::SourceFile) -> Result<SourceText, String> {
    SourceText::new(source.code, source.label).map_err(|error| error.to_string())
}

fn std_source_text(source: &anvyx_stdlib2::SourceFile) -> Result<SourceText, String> {
    SourceText::new(source.code, source.label).map_err(|error| error.to_string())
}

fn core_module_source(source: &anvyx_core2::SourceFile) -> Result<ModuleSource, String> {
    ModuleSource::new(path(source.path), source.code, source.label)
        .map_err(|error| error.to_string())
}

fn std_module_source(source: &anvyx_stdlib2::SourceFile) -> Result<ModuleSource, String> {
    ModuleSource::new(path(source.path), source.code, source.label)
        .map_err(|error| error.to_string())
}

fn path(path: &[&str]) -> Vec<String> {
    path.iter().map(|segment| (*segment).to_string()).collect()
}
