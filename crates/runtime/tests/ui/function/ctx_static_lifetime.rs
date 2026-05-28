use anvyx_runtime::{function, Ctx};

#[function(ctx)]
fn bad(ctx: &mut Ctx<'static, '_>) -> i64 {
    let _ = ctx.heap();
    0
}

fn main() {}
