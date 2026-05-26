use anvyx_runtime::function;

#[function]
fn bad<T>(value: i64) -> i64 { value }

fn main() {}
