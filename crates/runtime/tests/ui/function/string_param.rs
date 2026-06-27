use anvyx_runtime::function;

#[function]
fn bad(value: String) { drop(value); }

fn main() {}
