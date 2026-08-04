use anvyx_runtime::function;

#[function(params(value = "fn(int, int, int, int, int, int, int, int, int)"))]
fn bad(value: i64) {
    let _ = value;
}

fn main() {}
