use std::collections::BTreeMap;

use anvyx_runtime::function;

#[function]
fn bad(values: BTreeMap<i64, i64>) { drop(values); }

fn main() {}
