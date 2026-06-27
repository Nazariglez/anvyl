use anvyx_runtime::{function, Ctx, RuntimeResult, ScopedLambda};

#[function(ctx)]
fn bad<'cx>(ctx: &mut Ctx<'cx, '_>, f: ScopedLambda<'_, '_, (i64,), ()>) -> RuntimeResult<()> {
    let _ = ctx.heap();
    f.call(1)
}

fn main() {}
