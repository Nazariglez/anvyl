use anvyx_runtime::function;

#[function]
pub fn create_device() -> i64 {
    29
}

anvyx_runtime::builtin_module! {
    name: "gpu",
    root: false,
    exports: [create_device],}
