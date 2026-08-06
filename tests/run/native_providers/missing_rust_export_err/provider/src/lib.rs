use anvyx_runtime::RustModuleSupport;

mod descriptor {
    use anvyx_runtime::function;

    #[function]
    pub fn missing_export() {}

    anvyx_runtime::builtin_module! {
        name: "host",
        source: "",
        exports: [missing_export],
    }
}

pub use descriptor::{missing_export, provider_descriptors};

pub fn rust_module_supports() -> Vec<RustModuleSupport> {
    let mut supports = descriptor::rust_module_supports();
    for support in &mut supports {
        support.bindings.clear();
    }
    supports
}
