mod descriptor {
    use anvyx_runtime::function;

    #[function]
    pub fn ping() {}

    anvyx_runtime::builtin_module! {
        name: "host",
        exports: [ping],
    }
}

pub use descriptor::ping;

pub fn rust_providers() -> anvyx_runtime::RawProviderPackage {
    let mut wire = serde_json::to_value(descriptor::rust_providers()).unwrap();
    wire["exports"][0]["Rust"]["modules"][0]["bindings"][0]["path"] =
        serde_json::json!(["bad-segment"]);
    serde_json::from_value(wire).unwrap()
}
