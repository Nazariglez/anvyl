use anvyx_runtime::{function, AnvRef, AnvyxRefExport, Ctx};

struct Counter;

unsafe impl AnvyxRefExport for Counter {}

#[function(ctx)]
fn bad(ctx: &mut Ctx<'_, '_>, value: AnvRef<'_, Counter>) {
    let _ = (ctx, value);
}

fn main() {}
