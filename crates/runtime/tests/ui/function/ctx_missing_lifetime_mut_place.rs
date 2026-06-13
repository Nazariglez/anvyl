use anvyx_runtime::{function, Ctx, MutPlace};

#[function(ctx)]
fn bad(ctx: &mut Ctx<'cx, '_>, value: MutPlace<'_, 'cx, i64>) {
    let _ = (ctx, value);
}

fn main() {}
