use anvyx_runtime::{function, RuntimeError};

#[function]
fn bad(value: i64) -> Result<i64, RuntimeError> { Ok(value) }

fn main() {}
