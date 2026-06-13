use anvyx_runtime::{function, RuntimeError, ScopedLambda};

#[function]
fn bad(f: ScopedLambda<'static, '_, (i64,), ()>) -> Result<(), RuntimeError> {
    f.call(1)
}

fn main() {}
