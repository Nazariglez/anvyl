use anvyx_runtime::{function, ScopedLambda};

#[function]
fn bad(callback: ScopedLambda<'_, '_, (String,), ()>) {
    drop(callback);
}

fn main() {}
