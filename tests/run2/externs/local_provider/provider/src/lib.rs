use anvyx_runtime::function;

#[function]
pub fn host_add(a: i64, b: i64) -> i64 {
    a + b
}

#[function]
pub fn host_len(text: &str) -> i64 {
    text.len() as i64
}

anvyx_runtime::builtin_module! {
    name: "host",
    source: "",
    exports: [host_add, host_len],
}
