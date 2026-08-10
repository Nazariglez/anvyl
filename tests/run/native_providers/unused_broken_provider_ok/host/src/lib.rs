use anvyx_runtime::function;

#[function]
pub fn value() -> i64 {
    7
}

anvyx_runtime::builtin_module! {
    name: "host",
    exports: [value],
}
