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
    SystemPackageSource::with_providers(
        source_text(anvyx_core::ROOT.code, anvyx_core::ROOT.label)?,
        anvyx_core::MODULES
            .iter()
            .map(|source| module_source(source.path, source.code, source.label))
            .collect::<Result<Vec<_>, _>>()?,
        anvyx_core::provider_descriptors(),
    )
    .map_err(|error| error.to_string())
}

fn std_package() -> Result<SystemPackageSource, String> {
    SystemPackageSource::with_providers(
        source_text(anvyx_stdlib::ROOT.code, anvyx_stdlib::ROOT.label)?,
        anvyx_stdlib::MODULES
            .iter()
            .map(|source| module_source(source.path, source.code, source.label))
            .collect::<Result<Vec<_>, _>>()?,
        anvyx_stdlib::provider_descriptors(),
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

#[cfg(test)]
mod tests {
    use super::*;

    fn path(segments: &[&str]) -> Vec<String> {
        segments
            .iter()
            .map(|segment| (*segment).to_string())
            .collect()
    }

    #[test]
    fn source_bundle_surface_is_explicit() {
        let bundle = source_bundle().unwrap();
        let core = bundle.core().unwrap();
        let core_paths = core
            .modules()
            .iter()
            .map(|module| module.path().to_vec())
            .collect::<Vec<_>>();
        let std_paths = bundle
            .std()
            .unwrap()
            .modules()
            .iter()
            .map(|module| module.path().to_vec())
            .collect::<Vec<_>>();

        assert_eq!(core.root().label(), "crates/core/src/lib.anv");
        assert_eq!(
            core_paths,
            [
                path(&["option"]),
                path(&["result"]),
                path(&["range"]),
                path(&["collections"]),
                path(&["runtime"]),
                path(&["core_int"]),
                path(&["core_float"]),
                path(&["core_string"]),
                path(&["core_char"]),
            ]
        );
        assert_eq!(std_paths, [path(&["mem"])]);
        assert_eq!(
            bundle
                .std()
                .unwrap()
                .module(&path(&["mem"]))
                .unwrap()
                .label(),
            "crates/stdlib/src/mem.anv"
        );
    }
}
