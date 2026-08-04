use anvyx_runtime::{function, RuntimeResult, ScopedLambda};

#[function(params(callback = "fn(string) -> void"))]
fn bad(callback: ScopedLambda<'_, '_, (i64,), ()>) -> RuntimeResult<()> {
    callback.call(1)
}

fn main() {}
