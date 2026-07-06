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
    label: "crates/stdlib/src/lib.anv",
    code: include_str!("lib.anv"),
};

pub const MODULES: &[SourceFile] = &[SourceFile {
    path: &["mem"],
    label: "crates/stdlib/src/mem.anv",
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
        binding.path.crate_name = "anvyx_stdlib".to_string();
        binding.path.prefix_native(&native_prefix);
    }
    anvyx_runtime::RustProviderSupport {
        package: "<std>".to_string(),
        provider: anvyx_runtime::ProviderId {
            name: "stdlib_mem".to_string(),
        },
        cargo: anvyx_runtime::RustProviderCargo {
            manifest_key: "anvyx_stdlib".to_string(),
            package: Some("anvyx-stdlib".to_string()),
            ..Default::default()
        },
        modules: vec![module],
    }
}
