mod descriptor {
    use anvyx_runtime::function;

    #[function]
    pub fn value() -> i64 { 1 }

    anvyx_runtime::builtin_module! {
        name: "host",
        exports: [value],
    }
}

pub fn rust_providers() -> anvyx_runtime::RawProviderPackage {
    let mut wire = serde_json::to_value(descriptor::rust_providers()).unwrap();
    let duplicate = wire["exports"][0].clone();
    wire["exports"].as_array_mut().unwrap().push(duplicate);
    serde_json::from_value(wire).unwrap()
}
