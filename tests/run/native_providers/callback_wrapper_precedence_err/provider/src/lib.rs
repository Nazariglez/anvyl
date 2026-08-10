use anvyx_runtime::{
    AnvyxRef, EscapingLambda, methods,
};

mod descriptor {
    use super::{AnvyxRef, EscapingLambda, methods};

    #[derive(AnvyxRef)]
    #[anvyx(name = "Surface")]
    pub struct Surface {
        id: i64,
    }

    #[methods]
    impl Surface {
        pub fn visit(&self, _callback: EscapingLambda<(), ()>) {}
    }

    anvyx_runtime::builtin_module! {
        name: "host",
        exports: [Surface],
    }
}

pub fn rust_providers() -> anvyx_runtime::RawProviderPackage {
    let mut wire = serde_json::to_value(descriptor::rust_providers()).unwrap();
    let binding = &mut wire["exports"][0]["Rust"]["modules"][0]["bindings"][0]["abi"];
    binding["params"][0] = serde_json::json!("Value");
    binding["ret"] = serde_json::json!("Value");
    serde_json::from_value(wire).unwrap()
}
