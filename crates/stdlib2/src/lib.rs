pub mod mem;

#[doc(hidden)]
pub mod __anvyx_native {
    pub mod mem {
        pub use crate::mem::__anvyx_native::*;
    }
}

pub struct SourceFile {
    pub path: &'static [&'static str],
    pub label: &'static str,
    pub code: &'static str,
}

pub const ROOT: SourceFile = SourceFile {
    path: &[],
    label: "crates/stdlib2/src/lib.anv",
    code: include_str!("lib.anv"),
};

pub const MODULES: &[SourceFile] = &[SourceFile {
    path: &["mem"],
    label: "crates/stdlib2/src/mem.anv",
    code: include_str!("mem.anv"),
}];

pub fn provider_descriptors() -> Vec<anvyx_externs::ProviderDescriptor> {
    vec![mem::provider_descriptor()]
}

pub fn rust_provider_supports() -> Vec<anvyx_runtime::RustProviderSupport> {
    vec![provider_support(mem::rust_module_support())]
}

fn provider_support(
    mut module: anvyx_runtime::RustModuleSupport,
) -> anvyx_runtime::RustProviderSupport {
    let native_prefix = ["mem".to_string()];
    for binding in &mut module.bindings {
        binding.path.crate_name = "anvyx_stdlib2".to_string();
        binding.path.prefix_native(&native_prefix);
    }
    anvyx_runtime::RustProviderSupport {
        package: "<std>".to_string(),
        provider: anvyx_runtime::ProviderId {
            name: "stdlib_mem".to_string(),
        },
        cargo: anvyx_runtime::RustProviderCargo {
            manifest_key: "anvyx_stdlib2".to_string(),
            package: Some("anvyx-stdlib2".to_string()),
            ..Default::default()
        },
        modules: vec![module],
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn root_metadata() {
        assert_eq!(ROOT.path, &[] as &[&str]);
        assert_eq!(ROOT.label, "crates/stdlib2/src/lib.anv");
        assert_eq!(ROOT.code.trim(), "pub import mem;");
    }

    #[test]
    fn module_metadata() {
        assert_eq!(MODULES.len(), 1);
        let module = &MODULES[0];

        assert_eq!(module.path, &["mem"]);
        assert_eq!(module.label, "crates/stdlib2/src/mem.anv");
        assert!(module.label.starts_with("crates/stdlib2/src/"));
        assert_eq!(
            std::path::Path::new(module.label)
                .extension()
                .and_then(std::ffi::OsStr::to_str),
            Some("anv")
        );
    }

    #[test]
    fn mem_source_reexports_private_provider_members() {
        let code = MODULES[0].code;

        assert!(code.contains("pub import ext:mem"));
        assert!(!code.contains("extern fn "));
        assert!(!code.contains("pub fn "));
    }

    #[test]
    fn provider_metadata() {
        let providers = provider_descriptors();
        let module = &providers[0].modules[0];

        assert_eq!(module.path.segments, ["mem"]);
        assert_eq!(module.functions.len(), 3);
    }

    #[test]
    fn module_paths_are_package_local() {
        assert!(
            !MODULES
                .iter()
                .any(|module| module.path.starts_with(&["std"]))
        );
        assert!(!ROOT.code.contains("std:"));
    }

    #[test]
    fn excludes_unported_legacy_std_modules() {
        let code = MODULES[0].code;

        assert!(!code.contains("math"));
        assert!(!code.contains("maps"));
        assert!(!code.contains("time"));
        assert!(!code.contains("linalg"));
    }
}
