use anvyx_runtime::function;

unsafe extern "C" {
    fn unavailable() -> i64;
}

#[function]
pub fn broken() -> i64 {
    unsafe { unavailable() }
}

anvyx_runtime::builtin_module! {
    name: "broken",
    exports: [broken],
}
