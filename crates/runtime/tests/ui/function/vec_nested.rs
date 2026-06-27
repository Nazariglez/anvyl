use anvyx_runtime::function;

#[function]
fn bad(values: Option<Vec<i64>>) { drop(values); }

fn main() {}
