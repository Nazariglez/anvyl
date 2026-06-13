use anvyx_runtime::{function, RuntimeError, ScopedLambda};

#[function(params(f = "fn(float) -> void"))]
fn bad(f: ScopedLambda<'_, '_, (i64,), ()>) -> Result<(), RuntimeError> {
    f.call(1)
}

fn main() {}
