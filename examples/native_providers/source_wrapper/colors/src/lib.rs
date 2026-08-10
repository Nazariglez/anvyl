use anvyx_runtime::function;

#[function]
pub fn ping() -> i64 {
    7
}

anvyx_runtime::builtin_module! {
    name: "colors_native",
    exports: [ping],
}
