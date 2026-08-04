pub mod core_char;
pub mod core_float;
pub mod core_int;
pub mod core_runtime;
pub mod core_string;

#[doc(hidden)]
pub mod __anvyx_native {
    pub mod core_char {
        pub use crate::core_char::__anvyx_native::*;
    }
    pub mod core_float {
        pub use crate::core_float::__anvyx_native::*;
    }
    pub mod core_int {
        pub use crate::core_int::__anvyx_native::*;
    }
    pub mod core_runtime {
        pub use crate::core_runtime::__anvyx_native::*;
    }
    pub mod core_string {
        pub use crate::core_string::__anvyx_native::*;
    }
}

pub struct SourceFile {
    pub path: &'static [&'static str],
    pub label: &'static str,
    pub code: &'static str,
}

pub const ROOT: SourceFile = SourceFile {
    path: &[],
    label: "crates/core/src/lib.anv",
    code: include_str!("lib.anv"),
};

pub const MODULES: &[SourceFile] = &[
    SourceFile {
        path: &["option"],
        label: "crates/core/src/option.anv",
        code: include_str!("option.anv"),
    },
    SourceFile {
        path: &["result"],
        label: "crates/core/src/result.anv",
        code: include_str!("result.anv"),
    },
    SourceFile {
        path: &["range"],
        label: "crates/core/src/range.anv",
        code: include_str!("range.anv"),
    },
    SourceFile {
        path: &["collections"],
        label: "crates/core/src/collections.anv",
        code: include_str!("collections.anv"),
    },
    SourceFile {
        path: &["flags"],
        label: "crates/core/src/flags.anv",
        code: include_str!("flags.anv"),
    },
    SourceFile {
        path: &["runtime"],
        label: "crates/core/src/runtime.anv",
        code: include_str!("runtime.anv"),
    },
    SourceFile {
        path: &["core_int"],
        label: "crates/core/src/core_int.anv",
        code: include_str!("core_int.anv"),
    },
    SourceFile {
        path: &["core_float"],
        label: "crates/core/src/core_float.anv",
        code: include_str!("core_float.anv"),
    },
    SourceFile {
        path: &["core_string"],
        label: "crates/core/src/core_string.anv",
        code: include_str!("core_string.anv"),
    },
    SourceFile {
        path: &["core_char"],
        label: "crates/core/src/core_char.anv",
        code: include_str!("core_char.anv"),
    },
];

pub fn provider_descriptors() -> Vec<anvyx_externs::ProviderDescriptor> {
    vec![
        core_int::provider_descriptor(),
        core_float::provider_descriptor(),
        core_string::provider_descriptor(),
        core_char::provider_descriptor(),
        core_runtime::provider_descriptor(),
    ]
}

pub fn rust_provider_supports() -> Vec<anvyx_runtime::RustProviderSupport> {
    vec![
        provider_support("core_int", core_int::rust_module_support()),
        provider_support("core_float", core_float::rust_module_support()),
        provider_support("core_string", core_string::rust_module_support()),
        provider_support("core_char", core_char::rust_module_support()),
        provider_support("core_runtime", core_runtime::rust_module_support()),
    ]
}

fn provider_support(
    provider: &str,
    mut module: anvyx_runtime::RustModuleSupport,
) -> anvyx_runtime::RustProviderSupport {
    for ty in &mut module.types {
        ty.retarget_crate("anvyx_core");
    }
    let native_prefix = [provider.to_string()];
    for binding in &mut module.bindings {
        binding.path.crate_name = "anvyx_core".to_string();
        binding.path.prefix_native(&native_prefix);
    }
    anvyx_runtime::RustProviderSupport {
        package: "<core>".to_string(),
        provider: anvyx_runtime::ProviderId {
            name: provider.to_string(),
        },
        cargo: anvyx_runtime::RustProviderCargo {
            manifest_key: "anvyx_core".to_string(),
            package: Some("anvyx-core".to_string()),
            ..Default::default()
        },
        modules: vec![module],
    }
}
