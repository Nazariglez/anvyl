use anvyx_runtime::function;

#[function(foo = "bar")]
fn bad(value: i64) -> i64 { value }

fn main() {}
