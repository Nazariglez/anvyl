mod descriptor {
    use anvyx_runtime::{function, AnvyxInline};

    #[derive(Clone, Copy, AnvyxInline)]
    #[anvyx(name = "WindowConfig")]
    pub struct RustWindowConfig {
        width: i64,
    }

    #[function(ret = "WindowConfig")]
    pub fn make_config(width: i64) -> RustWindowConfig {
        RustWindowConfig { width }
    }

    #[function(params(config = "WindowConfig"))]
    pub fn config_width(config: RustWindowConfig) -> i64 {
        config.width
    }

    anvyx_runtime::builtin_module! {
        name: "host",
        exports: [make_config, config_width, RustWindowConfig],
    }
}

pub use descriptor::{config_width, make_config, RustWindowConfig};

pub fn rust_providers() -> anvyx_runtime::RawProviderPackage {
    let mut wire = serde_json::to_value(descriptor::rust_providers()).unwrap();
    wire["exports"][0]["Rust"]["modules"][0]["types"] = serde_json::json!([]);
    serde_json::from_value(wire).unwrap()
}
