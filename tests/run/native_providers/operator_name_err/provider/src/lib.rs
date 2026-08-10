mod descriptor {
    use anvyx_runtime::{methods, AnvyxInline};

    #[derive(Clone, Copy, AnvyxInline)]
    pub struct Vec2 {
        #[anvyx(field)]
        pub x: f64,
        #[anvyx(field)]
        pub y: f64,
    }

    #[methods]
    impl Vec2 {
        #[anvyx(init)]
        pub fn new(x: f64, y: f64) -> Self {
            Self { x, y }
        }

        #[anvyx(op(Self + Self))]
        pub fn add(&self, rhs: Vec2) -> Vec2 {
            Vec2 {
                x: self.x + rhs.x,
                y: self.y + rhs.y,
            }
        }
    }

    anvyx_runtime::builtin_module! {
        name: "host",
        exports: [Vec2],
    }
}

pub use descriptor::Vec2;

pub fn rust_providers() -> anvyx_runtime::RawProviderPackage {
    let mut wire = serde_json::to_value(descriptor::rust_providers()).unwrap();
    wire["exports"][0]["Rust"]["modules"][0]["bindings"]
        .as_array_mut()
        .unwrap()
        .retain(|binding| binding["target"]["Member"]["selector"].get("Operator").is_none());
    serde_json::from_value(wire).unwrap()
}
