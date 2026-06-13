use anvyx_runtime::{function, RuntimeError, ScopedLambda};

#[function]
fn bad(name: &str, f: ScopedLambda<'_, '_, (i64,), ()>) -> Result<(), RuntimeError> {
    let _ = name;
    f.call(1)
}

fn main() {}
