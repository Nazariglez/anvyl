use anvyx_runtime::function;

#[function]
pub fn left_value() -> i64 {
    1
}

anvyx_runtime::builtin_module! {
    name: "shared",
    exports: [left_value],
}
