use anvyx_runtime::{function, EscapingLambda};

#[function]
fn bad(callback: EscapingLambda<(String,), ()>) {
    drop(callback);
}

fn main() {}
