use anvyx_runtime::{function, RuntimeResult, ScopedLambda};

#[function]
fn bad(f: ScopedLambda<'_, '_, (Option<i64>,), ()>) -> RuntimeResult<()> {
    f.call(Some(1))
}

fn main() {}
