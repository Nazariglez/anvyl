use anvyx_runtime::{AnvyxInline, function, methods};

#[derive(Clone, Copy, AnvyxInline)]
pub struct WindowConfig {
    #[anvyx(field)]
    pub width: i64,
}

#[methods]
impl WindowConfig {
    #[anvyx(init)]
    pub fn new() -> Self {
        Self { width: 7 }
    }
}

#[function]
pub fn open_window() -> i64 {
    11
}

#[function]
pub fn window_width(config: WindowConfig) -> i64 {
    config.width
}

anvyx_runtime::builtin_module! {
    name: "window",
    root: false,
    exports: [open_window, window_width, WindowConfig],}
