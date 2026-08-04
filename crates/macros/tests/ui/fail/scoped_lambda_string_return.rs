use anvyx_runtime::{function, ScopedLambda};

#[function]
fn bad(callback: ScopedLambda<'_, '_, (i64,), String>) {
    drop(callback);
}

fn main() {}
