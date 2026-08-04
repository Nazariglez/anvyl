use anvyx_runtime::{function, AnvRef, AnvyxRefExport, Ctx};

struct Counter;

unsafe impl AnvyxRefExport for Box<Counter> {}

#[function(ctx)]
fn bad<'cx>(ctx: &mut Ctx<'cx, '_>, value: AnvRef<'cx, Box<Counter>>) {
    let _ = (ctx, value);
}

fn main() {}
