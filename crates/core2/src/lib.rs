pub mod core_float;
pub mod core_int;
pub mod core_runtime;
pub mod core_string;

#[doc(hidden)]
pub mod __anvyx_native {
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
    label: "crates/core2/src/lib.anv",
    code: include_str!("lib.anv"),
};

pub const MODULES: &[SourceFile] = &[
    SourceFile {
        path: &["option"],
        label: "crates/core2/src/option.anv",
        code: include_str!("option.anv"),
    },
    SourceFile {
        path: &["result"],
        label: "crates/core2/src/result.anv",
        code: include_str!("result.anv"),
    },
    SourceFile {
        path: &["range"],
        label: "crates/core2/src/range.anv",
        code: include_str!("range.anv"),
    },
    SourceFile {
        path: &["collections"],
        label: "crates/core2/src/collections.anv",
        code: include_str!("collections.anv"),
    },
    SourceFile {
        path: &["runtime"],
        label: "crates/core2/src/runtime.anv",
        code: include_str!("runtime.anv"),
    },
    SourceFile {
        path: &["core_int"],
        label: "crates/core2/src/core_int.anv",
        code: include_str!("core_int.anv"),
    },
    SourceFile {
        path: &["core_float"],
        label: "crates/core2/src/core_float.anv",
        code: include_str!("core_float.anv"),
    },
    SourceFile {
        path: &["core_string"],
        label: "crates/core2/src/core_string.anv",
        code: include_str!("core_string.anv"),
    },
];

pub fn provider_descriptors() -> Vec<anvyx_externs::ProviderDescriptor> {
    vec![
        core_int::provider_descriptor(),
        core_float::provider_descriptor(),
        core_string::provider_descriptor(),
        core_runtime::provider_descriptor(),
    ]
}

pub fn rust_provider_supports() -> Vec<anvyx_runtime::RustProviderSupport> {
    vec![
        provider_support("core_int", core_int::rust_module_support()),
        provider_support("core_float", core_float::rust_module_support()),
        provider_support("core_string", core_string::rust_module_support()),
        provider_support("core_runtime", core_runtime::rust_module_support()),
    ]
}

fn provider_support(
    provider: &str,
    mut module: anvyx_runtime::RustModuleSupport,
) -> anvyx_runtime::RustProviderSupport {
    for ty in &mut module.types {
        ty.path.crate_name = "anvyx_core2".to_string();
    }
    let native_prefix = [provider.to_string()];
    for binding in &mut module.bindings {
        binding.path.crate_name = "anvyx_core2".to_string();
        binding.path.prefix_native(&native_prefix);
    }
    anvyx_runtime::RustProviderSupport {
        package: "<core>".to_string(),
        provider: anvyx_runtime::ProviderId {
            name: provider.to_string(),
        },
        cargo: anvyx_runtime::RustProviderCargo {
            manifest_key: "anvyx_core2".to_string(),
            package: Some("anvyx-core2".to_string()),
            ..Default::default()
        },
        modules: vec![module],
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn paths(items: &[SourceFile]) -> Vec<&'static [&'static str]> {
        items.iter().map(|item| item.path).collect()
    }

    #[test]
    fn root_metadata() {
        assert_eq!(ROOT.path, &[] as &[&str]);
        assert_eq!(ROOT.label, "crates/core2/src/lib.anv");
        assert!(ROOT.code.contains("pub import core_int { * };"));
        assert!(ROOT.code.contains("pub import core_float { * };"));
        assert!(ROOT.code.contains("pub import core_string { * };"));
        assert!(ROOT.code.contains("pub import runtime { * };"));
        assert!(ROOT.code.contains("pub import option { * };"));
        assert!(ROOT.code.contains("pub import result { * };"));
        assert!(ROOT.code.contains("pub import range { * };"));
        assert!(ROOT.code.contains("pub import collections { * };"));
        assert!(!ROOT.code.contains("pub import core_int;"));
        assert!(!ROOT.code.contains("pub import core_float;"));
        assert!(!ROOT.code.contains("pub import core_string;"));
    }

    #[test]
    fn module_metadata() {
        assert_eq!(
            paths(MODULES),
            vec![
                &["option"][..],
                &["result"],
                &["range"],
                &["collections"],
                &["runtime"],
                &["core_int"],
                &["core_float"],
                &["core_string"]
            ]
        );
        for module in MODULES {
            assert!(module.label.starts_with("crates/core2/src/"));
            assert_eq!(
                std::path::Path::new(module.label)
                    .extension()
                    .and_then(std::ffi::OsStr::to_str),
                Some("anv")
            );
            assert!(!module.path.is_empty());
        }
    }

    #[test]
    fn extension_helpers_are_not_module_reexported_by_root() {
        assert!(!ROOT.code.contains("pub import core_int;"));
        assert!(!ROOT.code.contains("pub import core_float;"));
        assert!(!ROOT.code.contains("pub import core_string;"));
    }

    #[test]
    fn extension_modules_use_private_provider_imports() {
        let code = MODULES
            .iter()
            .filter(|module| {
                matches!(
                    module.path,
                    ["core_int" | "core_float" | "core_string" | "runtime"]
                )
            })
            .map(|module| module.code)
            .collect::<Vec<_>>()
            .join("\n");

        assert!(code.contains("import ext:core_int"));
        assert!(code.contains("import ext:core_float"));
        assert!(code.contains("import ext:core_string"));
        assert!(code.contains("import ext:core_runtime"));
        assert!(!code.contains("import ext:int"));
        assert!(!code.contains("import ext:float"));
        assert!(!code.contains("import ext:string"));
        assert!(!code.contains(&["extern fn ", "int_"].concat()));
        assert!(!code.contains(&["extern fn ", "float_"].concat()));
        assert!(!code.contains(&["extern fn ", "str_"].concat()));
    }

    #[test]
    fn provider_metadata() {
        let providers = provider_descriptors();
        let modules = providers
            .iter()
            .map(|provider| provider.modules[0].path.segments.as_slice())
            .collect::<Vec<_>>();

        assert_eq!(
            modules,
            [
                &["core_int"][..],
                &["core_float"],
                &["core_string"],
                &["core_runtime"]
            ]
        );
        assert_eq!(providers[0].modules[0].functions[0].name, "int_abs");
        let runtime = &providers[3].modules[0].functions;
        assert_eq!(runtime[0].name, "_println");
        assert_eq!(
            runtime[0].signature.params[0].ty,
            anvyx_externs::ExternTypeExpr::String
        );
        assert_eq!(
            runtime[0].signature.ret,
            anvyx_externs::ExternTypeExpr::Void
        );
        assert_eq!(
            runtime[0].signature.params[0].flow,
            anvyx_externs::ParamFlow::Borrow
        );
        assert!(!runtime[0].effects.fallible);

        assert_eq!(runtime[1].name, "_assert");
        assert_eq!(
            runtime[1].signature.params[0].ty,
            anvyx_externs::ExternTypeExpr::Bool
        );
        assert_eq!(
            runtime[1].signature.params[1].ty,
            anvyx_externs::ExternTypeExpr::String
        );
        assert_eq!(
            runtime[1].signature.params[0].flow,
            anvyx_externs::ParamFlow::Value
        );
        assert_eq!(
            runtime[1].signature.params[1].flow,
            anvyx_externs::ParamFlow::Borrow
        );
        assert_eq!(
            runtime[1].signature.ret,
            anvyx_externs::ExternTypeExpr::Void
        );
        assert!(!runtime[1].effects.fallible);
    }

    #[test]
    fn scalar_provider_metadata() {
        let providers = provider_descriptors();
        let int = &providers[0].modules[0].functions;
        assert_eq!(
            int.iter()
                .map(|function| function.name.as_str())
                .collect::<Vec<_>>(),
            ["int_abs", "int_min", "int_max", "int_clamp"]
        );
        for function in int {
            assert_eq!(function.signature.ret, anvyx_externs::ExternTypeExpr::Int);
            assert!(
                function
                    .signature
                    .params
                    .iter()
                    .all(|param| param.ty == anvyx_externs::ExternTypeExpr::Int)
            );
        }

        let float = &providers[1].modules[0].functions;
        assert!(float.iter().any(|function| function.name == "float_lerp"));
        for function in float {
            assert_eq!(function.signature.ret, anvyx_externs::ExternTypeExpr::Float);
            assert!(
                function
                    .signature
                    .params
                    .iter()
                    .all(|param| param.ty == anvyx_externs::ExternTypeExpr::Float)
            );
        }
    }

    #[test]
    fn string_provider_metadata_and_support() {
        let providers = provider_descriptors();
        let string = &providers[2].modules[0].functions;
        assert_eq!(string.len(), 14);
        assert!(
            string
                .iter()
                .flat_map(|function| &function.signature.params)
                .filter(|param| param.ty == anvyx_externs::ExternTypeExpr::String)
                .all(|param| param.flow == anvyx_externs::ParamFlow::Borrow)
        );

        let support = rust_provider_supports();
        assert_eq!(support.len(), 4);
        assert_eq!(support[0].cargo.manifest_key, "anvyx_core2");
        assert_eq!(support[0].cargo.package.as_deref(), Some("anvyx-core2"));
        let string_support = &support[2].modules[0];
        assert!(
            string_support
                .bindings
                .iter()
                .any(|binding| binding.abi.support == anvyx_runtime::RustAbiSupport::Direct)
        );
        assert!(
            string_support
                .bindings
                .iter()
                .any(|binding| binding.abi.support == anvyx_runtime::RustAbiSupport::Unsupported)
        );
    }

    #[test]
    fn providers_validate() {
        for provider in provider_descriptors() {
            assert_eq!(anvyx_externs::validate(&provider), Ok(()));
        }
    }
}
