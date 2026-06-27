use anvyx_runtime::function;

#[function]
fn bad(value: Option<String>) { drop(value); }

fn main() {}
