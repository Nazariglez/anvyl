mod cache;
mod rust_deps;

pub mod check;
pub mod manifest;
pub mod rust;

use anvyx_lang::{ModuleSource, SourceBundle, SourceText, SystemPackageSource};

pub fn source_bundle() -> Result<SourceBundle, String> {
    Ok(SourceBundle::new(
        Some(core_package()?),
        Some(std_package()?),
    ))
}

fn core_package() -> Result<SystemPackageSource, String> {
    SystemPackageSource::new(
        source_text(anvyx_core::ROOT.code, anvyx_core::ROOT.label)?,
        anvyx_core::MODULES
            .iter()
            .map(|source| module_source(source.path, source.code, source.label))
            .collect::<Result<Vec<_>, _>>()?,
    )
    .map_err(|error| error.to_string())
}

fn std_package() -> Result<SystemPackageSource, String> {
    SystemPackageSource::new(
        source_text(anvyx_stdlib::ROOT.code, anvyx_stdlib::ROOT.label)?,
        anvyx_stdlib::MODULES
            .iter()
            .map(|source| module_source(source.path, source.code, source.label))
            .collect::<Result<Vec<_>, _>>()?,
    )
    .map_err(|error| error.to_string())
}

fn source_text(code: &str, label: &str) -> Result<SourceText, String> {
    SourceText::new(code, label).map_err(|error| error.to_string())
}

fn module_source(path: &[&str], code: &str, label: &str) -> Result<ModuleSource, String> {
    ModuleSource::new(
        path.iter().map(|segment| (*segment).to_string()).collect(),
        code,
        label,
    )
    .map_err(|error| error.to_string())
}
