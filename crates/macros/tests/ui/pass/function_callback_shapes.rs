use anvyx_runtime::{function, RuntimeResult, ScopedLambda};

#[function(params(f = "fn(int) -> ()"))]
fn unit_callback(f: ScopedLambda<'_, '_, (i64,), ()>) -> RuntimeResult<()> {
    f.call(1)
}

#[function]
fn char_and_unit(value: char, callback: ScopedLambda<'_, '_, (char,), char>, unit: Option<()>) {
    let _ = (value, callback, unit);
}

fn main() {}
