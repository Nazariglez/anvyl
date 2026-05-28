use anvyx_runtime::{function, Ctx};

#[function(ctx)]
fn bad(value: i64, ctx: &mut Ctx<'_, '_>) -> i64 {
    let _ = ctx.heap();
    value
}

fn main() {}
