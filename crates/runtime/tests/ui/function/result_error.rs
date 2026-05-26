use anvyx_runtime::function;

#[function]
fn bad(value: i64) -> Result<i64, String> { Ok(value) }

fn main() {}
