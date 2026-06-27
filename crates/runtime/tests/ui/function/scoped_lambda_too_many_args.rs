use anvyx_runtime::{function, RuntimeResult, ScopedLambda};

#[function]
fn bad(
    f: ScopedLambda<'_, '_, (i64, i64, i64, i64, i64, i64, i64, i64, i64), ()>,
) -> RuntimeResult<()> {
    let _ = f;
    Ok(())
}

fn main() {}
