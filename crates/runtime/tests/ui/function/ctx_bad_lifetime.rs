use anvyx_runtime::{function, Ctx};

#[function(ctx)]
fn bad<'heap>(ctx: &mut Ctx<'heap, '_>, value: i64) -> i64 {
    let _ = ctx.heap();
    value
}

fn main() {}
