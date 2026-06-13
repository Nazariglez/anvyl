use anvyx_runtime::{function, Ctx, RuntimeError, ScopedLambda};

#[function(ctx)]
fn bad<'cx>(ctx: &mut Ctx<'cx, '_>, f: ScopedLambda<'_, '_, (i64,), ()>) -> Result<(), RuntimeError> {
    let _ = ctx.heap();
    f.call(1)
}

fn main() {}
