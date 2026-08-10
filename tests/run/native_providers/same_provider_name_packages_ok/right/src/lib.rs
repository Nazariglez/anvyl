use anvyx_runtime::function;

#[function]
pub fn right_value() -> i64 {
    2
}

anvyx_runtime::builtin_module! {
    name: "shared",
    exports: [right_value],
}
