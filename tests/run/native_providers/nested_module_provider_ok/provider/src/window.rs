use anvyx_runtime::function;

#[function]
pub fn open_window() -> i64 { 11 }

anvyx_runtime::builtin_module! {
    name: "window",
    source: "",
    exports: [open_window],
}
