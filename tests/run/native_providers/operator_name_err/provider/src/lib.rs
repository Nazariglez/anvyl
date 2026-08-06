use anvyx_runtime::{
    ExternBindingTarget::Member, ExternMemberKey, ExternMemberSelector::Operator,
    RustModuleSupport,
};

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
        source: "",
        exports: [Vec2],
    }
}

pub use descriptor::{Vec2, provider_descriptors};

pub fn rust_module_supports() -> Vec<RustModuleSupport> {
    let mut supports = descriptor::rust_module_supports();
    for support in &mut supports {
        support.bindings.retain(|binding| {
            !matches!(
                &binding.key.target,
                Member(ExternMemberKey {
                    selector: Operator(_),
                    ..
                })
            )
        });
    }
    supports
}
