use anvyx_runtime::{methods, AnvyxInline};

#[derive(Clone, Copy, AnvyxInline)]
pub struct Counter {
    value: i64,
}

#[methods]
impl Counter {
    #[anvyx(place, ctx)]
    pub fn bump<'cx>(
        ctx: &mut anvyx_runtime::Ctx<'cx, '_>,
        receiver: anvyx_runtime::MutPlace<'_, 'cx, Counter>,
    ) {
        let _ = (ctx, receiver);
    }
}

fn main() {}
