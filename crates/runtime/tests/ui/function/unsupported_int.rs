use anvyx_runtime::function;

#[function]
fn bad(value: usize) -> i64 { value as i64 }

fn main() {}
