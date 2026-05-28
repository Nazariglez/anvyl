use anvyx_runtime::function;

#[function(ctx)]
fn bad(value: i64) -> i64 {
    value
}

fn main() {}
