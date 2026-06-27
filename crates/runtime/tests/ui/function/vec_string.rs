use anvyx_runtime::function;

#[function]
fn bad(values: Vec<String>) { drop(values); }

fn main() {}
