use anvyx_runtime::function;

#[function]
pub fn ping() -> i64 { 7 }

anvyx_runtime::builtin_module! {
    name: "host",
    source: "",
    exports: [ping],
}
