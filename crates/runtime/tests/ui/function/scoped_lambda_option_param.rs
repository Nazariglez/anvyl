use anvyx_runtime::{function, RuntimeError, ScopedLambda};

#[function]
fn bad(f: ScopedLambda<'_, '_, (Option<i64>,), ()>) -> Result<(), RuntimeError> {
    f.call(Some(1))
}

fn main() {}
