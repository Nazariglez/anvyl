mod descriptor {
    use anvyx_runtime::function;

    #[function]
    pub fn missing_export() {}

    anvyx_runtime::builtin_module! {
        name: "host",
        exports: [missing_export],
    }
}

pub use descriptor::missing_export;

pub fn rust_providers() -> anvyx_runtime::RawProviderPackage {
    let mut wire = serde_json::to_value(descriptor::rust_providers()).unwrap();
    wire["exports"][0]["Rust"]["modules"][0]["bindings"] = serde_json::json!([]);
    serde_json::from_value(wire).unwrap()
}
