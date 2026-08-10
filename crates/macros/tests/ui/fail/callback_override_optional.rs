use anvyx_runtime::{function, ScopedLambda};

#[function(params(callback = "fn(int)?"))]
fn optional_callback(callback: ScopedLambda<'_, '_, (i64,), ()>) {
    let _ = callback;
}

fn main() {}
