use anvyx_runtime::{function, RuntimeError, ScopedLambda};

#[function]
fn bad(
    f: ScopedLambda<'_, '_, (i64, i64, i64, i64, i64, i64, i64, i64, i64), ()>,
) -> Result<(), RuntimeError> {
    let _ = f;
    Ok(())
}

fn main() {}
