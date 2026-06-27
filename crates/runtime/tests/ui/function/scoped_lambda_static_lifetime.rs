use anvyx_runtime::{function, RuntimeResult, ScopedLambda};

#[function]
fn bad(f: ScopedLambda<'static, '_, (i64,), ()>) -> RuntimeResult<()> {
    f.call(1)
}

fn main() {}
