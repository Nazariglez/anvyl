mod descriptor {
    use anvyx_runtime::function;

    #[function]
    pub fn value() -> i64 { 1 }

    anvyx_runtime::builtin_module! {
        name: "first",
        exports: [value],
    }
}

pub fn rust_providers() -> anvyx_runtime::RawProviderPackage {
    let wire = serde_json::to_value(descriptor::rust_providers()).unwrap();
    let first = wire["exports"][0].clone();
    let mut middle = first.clone();
    middle["Rust"]["provider"]["name"] = serde_json::json!("middle");
    let mut fresh = middle["Rust"]["modules"][0].clone();
    fresh["descriptor"]["path"]["segments"][0] = serde_json::json!("fresh");
    middle["Rust"]["modules"] = serde_json::json!([fresh, first["Rust"]["modules"][0].clone()]);
    let mut later = first.clone();
    later["Rust"]["provider"]["name"] = serde_json::json!("later");
    later["Rust"]["modules"][0]["descriptor"]["path"]["segments"][0] = serde_json::json!("fresh");
    serde_json::from_value(serde_json::json!({"exports": [first, middle, later]})).unwrap()
}
