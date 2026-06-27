use anvyx_runtime::{function, RuntimeResult, ScopedLambda};

#[function(params(f = "fn(float) -> void"))]
fn bad(f: ScopedLambda<'_, '_, (i64,), ()>) -> RuntimeResult<()> {
    f.call(1)
}

fn main() {}
