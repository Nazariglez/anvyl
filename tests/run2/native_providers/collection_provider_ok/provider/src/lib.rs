use anvyx_runtime::function;

#[function]
pub fn host_first(xs: Vec<i64>) -> i64 {
    xs[0]
}

#[function]
pub fn host_make() -> Vec<i64> {
    vec![5, 7]
}

#[function]
pub fn host_join(xs: Vec<String>) -> String {
    xs.join("|")
}

anvyx_runtime::builtin_module! {
    name: "host",
    source: "",
    exports: [host_first, host_make, host_join],
}
