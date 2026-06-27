use anvyx_runtime::function;

#[function]
pub fn pair() -> [i64; 3] {
    [2, 3, 5]
}

#[function]
pub fn sum(values: [i64; 3]) -> i64 {
    values.into_iter().sum()
}

#[function]
pub fn maybe(flag: bool) -> [Option<i64>; 2] {
    [Some(7), flag.then_some(11)]
}

#[function]
pub fn add_present(values: [Option<i64>; 2]) -> i64 {
    values[0].unwrap_or(0) + values[1].unwrap_or(0)
}

anvyx_runtime::builtin_module! {
    name: "host",
    source: "",
    exports: [pair, sum, maybe, add_present],
}
