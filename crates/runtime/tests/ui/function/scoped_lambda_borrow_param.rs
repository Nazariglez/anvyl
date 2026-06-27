use anvyx_runtime::{function, RuntimeResult, ScopedLambda};

#[function]
fn bad(name: &str, f: ScopedLambda<'_, '_, (i64,), ()>) -> RuntimeResult<()> {
    let _ = name;
    f.call(1)
}

fn main() {}
