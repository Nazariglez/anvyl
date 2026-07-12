use anvyx_runtime::{methods, AnvyxInline};

#[derive(Clone, Copy, AnvyxInline)]
pub struct Counter {
    value: i64,
}

#[methods]
impl Counter {
    #[anvyx(place, ctx)]
    pub fn apply<'cx>(
        ctx: &mut anvyx_runtime::Ctx<'cx, '_>,
        place: anvyx_runtime::MutPlace<'_, 'cx, Self>,
        callback: anvyx_runtime::ScopedLambda<'_, '_, (), ()>,
    ) -> anvyx_runtime::RuntimeResult<()> {
        let _ = (ctx, place, callback);
        unreachable!()
    }
}

fn main() {}
