use std::collections::HashMap;

use anvyx_runtime::function;

#[function]
fn bad(values: HashMap<i64, i64>) { drop(values); }

fn main() {}
