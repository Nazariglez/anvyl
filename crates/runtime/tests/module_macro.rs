#![allow(dead_code)]

use anvyx_runtime::{AnvyxInline, AnvyxRef, Ctx, Heap, function, methods};

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
            exports: [WWin, make_win],
        }
    }

    anvyx_runtime::provider_package! { modules: [win] }
}

#[test]
fn module_macros_expose_native_exports() {
    Heap::scope(|heap| {
        let mut ctx = Ctx::new(heap);
        assert_eq!(root_module::__anvyx_native::ping(&mut ctx, 41), 42);
        assert_eq!(tree_module::__anvyx_native::child::pong(&mut ctx, 21), 42);
    });
}
