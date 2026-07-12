#![allow(dead_code)]

use anvyx_runtime::{
    AnvyxInline, AnvyxRef, Ctx, ExternBindingOp, ExternBindingTarget, ExternMemberSelector, Heap,
    function, methods,
};

mod root_module {
    use super::{AnvyxInline, function};

    #[derive(Clone, Copy, AnvyxInline)]
    pub struct Point {
        #[anvyx(field)]
        pub x: i64,
    }

    #[function]
    pub fn ping(value: i64) -> i64 {
        value + 1
    }

    anvyx_runtime::module!(root: game, exports: [ping, Point]);
}

mod tree_module {
    use super::function;

    pub mod child {
        use super::function;

        #[function]
        pub fn pong(value: i64) -> i64 {
            value * 2
        }

        anvyx_runtime::module!(exports: [pong]);
    }

    #[function]
    pub fn ping(value: i64) -> i64 {
        value + 1
    }

    anvyx_runtime::module!(root: game, modules: [child], exports: [ping]);
}

mod builtin {
    use super::{AnvyxInline, function};

    #[derive(Clone, Copy, AnvyxInline)]
    pub struct Point {
        #[anvyx(field)]
        pub x: i64,
    }

    #[function(name = "int_abs")]
    pub fn abs(value: i64) -> i64 {
        value.abs()
    }

    anvyx_runtime::builtin_module! {
        name: "core_int",
        source: "",
        exports: [abs, Point],
    }
}

mod package {
    use super::{AnvyxInline, function, methods};

    pub mod window {
        use super::{AnvyxInline, function, methods};

        #[derive(Clone, Copy, AnvyxInline)]
        pub struct WindowConfig {
            #[anvyx(field)]
            pub width: i64,
        }

        #[methods]
        impl WindowConfig {
            pub fn doubled(&mut self) -> i64 {
                self.width *= 2;
                self.width
            }
        }

        #[function]
        pub fn open_window() -> i64 {
            11
        }

        anvyx_runtime::builtin_module! {
            name: "window",
            source: "",
            exports: [open_window, WindowConfig],
        }
    }

    pub mod gpu {
        use super::function;

        #[function]
        pub fn create_device() -> i64 {
            29
        }

        anvyx_runtime::builtin_module! {
            name: "gpu",
            source: "",
            exports: [create_device],
        }
    }

    pub mod platform {
        pub mod input {
            use super::super::function;

            #[function]
            pub fn poll_input() -> i64 {
                3
            }

            anvyx_runtime::builtin_module! {
                name: "input",
                source: "",
                exports: [poll_input],
            }
        }
    }

    anvyx_runtime::provider_package! { modules: [window, gpu, platform::input] }
}

mod private_resource_package {
    use super::{AnvyxRef, function, methods};

    mod win {
        use super::{AnvyxRef, function, methods};

        #[derive(AnvyxRef)]
        pub struct WWin {
            #[anvyx(field)]
            pub value: i64,
        }

        #[methods]
        impl WWin {
            #[anvyx(init)]
            pub fn new(value: i64) -> WWin {
                WWin { value }
            }

            pub fn duplicate(&self) -> WWin {
                WWin { value: self.value }
            }
        }

        #[function]
        pub fn make_win(value: i64) -> WWin {
            WWin { value }
        }

        anvyx_runtime::builtin_module! {
            name: "win",
            source: "",
            exports: [WWin, make_win],
        }
    }

    anvyx_runtime::provider_package! { modules: [win] }
}

#[test]
fn module_assembles_root_provider_and_native_tree() {
    let provider = root_module::provider_descriptor();

    assert_eq!(provider.provider.name, "game");
    assert_eq!(provider.modules[0].path.segments, ["game"]);
    assert_eq!(provider.modules[0].functions[0].name, "ping");
    assert_eq!(provider.modules[0].types[0].name, "Point");
    Heap::scope(|heap| {
        let mut ctx = Ctx::new(heap);
        assert_eq!(root_module::__anvyx_native::ping(&mut ctx, 41), 42);
    });

    let descriptors = root_module::provider_descriptors();
    assert_eq!(descriptors, vec![provider]);

    let support = root_module::rust_module_support();
    assert_eq!(support.module.segments, ["game"]);
    assert_eq!(support.bindings[0].key.operation, ExternBindingOp::Call);
}

#[test]
fn module_assembles_child_descriptors_and_support() {
    let provider = tree_module::provider_descriptor();

    assert_eq!(provider.modules.len(), 2);
    assert_eq!(provider.modules[0].path.segments, ["game"]);
    assert_eq!(provider.modules[1].path.segments, ["game", "child"]);
    assert_eq!(provider.modules[1].functions[0].name, "pong");
    assert_eq!(tree_module::provider_descriptors(), vec![provider.clone()]);
    Heap::scope(|heap| {
        let mut ctx = Ctx::new(heap);
        assert_eq!(tree_module::__anvyx_native::child::pong(&mut ctx, 21), 42);
    });

    let supports = tree_module::rust_module_supports();
    assert_eq!(supports.len(), 2);
    assert_eq!(supports[0].module.segments, ["game"]);
    assert_eq!(supports[1].module.segments, ["game", "child"]);
    assert_eq!(
        supports[1].bindings[0].path.segments,
        ["__anvyx_native", "child", "pong"]
    );
}

#[test]
fn builtin_assembles_descriptor_and_native_support() {
    let provider = builtin::provider_descriptor();
    let support = builtin::rust_module_support();

    assert_eq!(provider.provider.name, "core_int");
    assert_eq!(provider.modules[0].path.segments, ["core_int"]);
    assert_eq!(provider.modules[0].functions[0].name, "int_abs");
    assert_eq!(builtin::provider_descriptors(), vec![provider.clone()]);
    Heap::scope(|heap| {
        let mut ctx = Ctx::new(heap);
        assert_eq!(builtin::__anvyx_native::int_abs(&mut ctx, -3), 3);
    });
    assert_eq!(support.module.segments, ["core_int"]);
    assert_eq!(support.types.len(), 1);
    assert_eq!(support.types[0].key.name, "Point");
    assert_eq!(support.types[0].key.module.segments, ["core_int"]);
    assert_eq!(support.types[0].path.crate_name, "crate");
    assert_eq!(support.types[0].path.segments, ["builtin", "Point"]);
    assert_eq!(
        support.bindings[0].path.segments,
        ["__anvyx_native", "int_abs"]
    );
    assert_eq!(support.bindings[0].key.operation, ExternBindingOp::Call);
    let ExternBindingTarget::Function(function) = &support.bindings[0].key.target else {
        panic!("expected function binding");
    };
    assert_eq!(function.module.segments, ["core_int"]);
    assert_eq!(function.name, "int_abs");
}

#[test]
fn provider_package_aggregates_child_descriptors_and_prefixes_native_paths() {
    let descriptors = package::provider_descriptors();

    assert_eq!(descriptors.len(), 3);
    assert_eq!(descriptors[0].provider.name, "window");
    assert_eq!(descriptors[1].provider.name, "gpu");
    assert_eq!(descriptors[2].provider.name, "input");
    assert_eq!(descriptors[0].modules[0].path.segments, ["window"]);
    assert_eq!(descriptors[1].modules[0].path.segments, ["gpu"]);
    assert_eq!(descriptors[2].modules[0].path.segments, ["input"]);

    let supports = package::rust_module_supports();
    assert_eq!(supports.len(), 3);
    assert_eq!(supports[0].module.segments, ["window"]);
    assert_eq!(supports[1].module.segments, ["gpu"]);
    assert_eq!(supports[2].module.segments, ["input"]);
    assert_eq!(supports[0].bindings[0].path.crate_name, "crate");
    assert_eq!(
        supports[0].bindings[0].path.segments,
        [
            "package",
            "__anvyx_native_package",
            "window",
            "__anvyx_native",
            "open_window",
        ]
    );
    assert_eq!(
        supports[1].bindings[0].path.segments,
        [
            "package",
            "__anvyx_native_package",
            "gpu",
            "__anvyx_native",
            "create_device",
        ]
    );
    assert_eq!(
        supports[2].bindings[0].path.segments,
        [
            "package",
            "__anvyx_native_package",
            "platform",
            "input",
            "__anvyx_native",
            "poll_input",
        ]
    );
    assert_eq!(supports[0].types[0].path.crate_name, "crate");
    assert_eq!(
        supports[0].types[0].path.segments,
        [
            "package",
            "__anvyx_native_package",
            "window",
            "WindowConfig",
        ]
    );
    assert!(supports[0].bindings.iter().any(|binding| {
        binding.path.segments
            == [
                "package",
                "__anvyx_native_package",
                "window",
                "__anvyx_methods_native_windowconfig",
                "doubled",
            ]
    }));
}

#[test]
fn provider_package_retargets_private_resource_paths() {
    let supports = private_resource_package::rust_module_supports();
    assert_eq!(supports.len(), 1);
    let support = &supports[0];
    assert_eq!(support.types[0].path.crate_name, "crate");
    assert_eq!(
        support.types[0].path.segments,
        [
            "private_resource_package",
            "__anvyx_native_package",
            "win",
            "WWin",
        ]
    );
    assert!(support.bindings.iter().any(|binding| {
        binding.path.segments
            == [
                "private_resource_package",
                "__anvyx_native_package",
                "win",
                "__anvyx_native",
                "make_win",
            ]
    }));
    assert!(support.bindings.iter().any(|binding| {
        binding.path.segments
            == [
                "private_resource_package",
                "__anvyx_native_package",
                "win",
                "__anvyx_methods_native_wwin",
                "duplicate",
            ]
    }));

    let init = support
        .bindings
        .iter()
        .find(|binding| {
            matches!(
                binding.key.target,
                ExternBindingTarget::Member(ref member)
                    if member.selector == ExternMemberSelector::Init
            )
        })
        .expect("init binding");
    assert_eq!(
        init.abi.ret,
        anvyx_runtime::RustReturnAbi::OwnedNamed(anvyx_runtime::ExternTypeExpr::Named {
            module: None,
            name: "WWin".to_string(),
            args: vec![],
        })
    );
}
