use anvyx_runtime::{function, Ctx, MutPlace};

#[function(ctx)]
fn bad<'cx>(ctx: &mut Ctx<'cx, '_>, value: MutPlace<'_, 'cx, String>) {
    let _ = (ctx, value);
}

fn main() {}
