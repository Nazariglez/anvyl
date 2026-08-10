pub mod calls;
pub mod types;

pub fn rust_providers() -> anvyx_runtime::RawProviderPackage {
    let mut package = anvyx_runtime::RawProviderPackage::default();
    calls::__anvyx_provider_export(
        &mut package,
        vec!["calls".to_string(), "__anvyx_native".to_string()],
        Some(vec!["calls".to_string()]),
    );
    types::__anvyx_provider_export(
        &mut package,
        vec!["types".to_string(), "__anvyx_native".to_string()],
        Some(vec!["types".to_string()]),
    );
    let mut wire = serde_json::to_value(package).unwrap();
    wire["exports"][0]["Rust"]["modules"][0]["descriptor"]["functions"][0]["signature"]
        ["params"][0]["ty"]["Named"]["module"] = serde_json::json!({ "segments": ["types"] });
    serde_json::from_value(wire).unwrap()
}
