use anvyx_runtime::function;

#[function]
fn bad(values: Vec<i64>) { drop(values); }

fn main() {}
