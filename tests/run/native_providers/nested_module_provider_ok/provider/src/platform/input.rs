use anvyx_runtime::function;

#[function]
pub fn read_input() -> i64 { 29 }

anvyx_runtime::builtin_module! {
    name: "input",
    source: "",
    exports: [read_input],
}
