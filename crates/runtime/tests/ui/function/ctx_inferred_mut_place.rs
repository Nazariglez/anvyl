use anvyx_runtime::{function, Ctx, MutPlace};

#[function(ctx)]
fn bad<'cx>(ctx: &mut Ctx<'_, '_>, value: MutPlace<'_, 'cx, i64>) {
    let _ = (ctx, value);
}

fn main() {}
