use anvyx_runtime::function;

#[function]
fn bad(value: &i64) -> i64 { *value }

fn main() {}
