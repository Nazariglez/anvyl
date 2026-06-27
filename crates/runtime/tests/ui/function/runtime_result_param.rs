use anvyx_runtime::{function, RuntimeResult};

#[function]
fn bad(value: RuntimeResult<i64>) -> i64 { value.unwrap_or_default() }

fn main() {}
