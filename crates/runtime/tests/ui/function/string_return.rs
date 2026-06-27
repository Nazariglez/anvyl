use anvyx_runtime::function;

#[function]
fn bad() -> String { String::new() }

fn main() {}
