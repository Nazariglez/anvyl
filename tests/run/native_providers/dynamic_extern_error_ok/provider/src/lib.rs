use anvyx_runtime::{
    function, methods, AnvRef, AnvRefType, AnvyxRef, Ctx, RuntimeError, RuntimeResult,
};

#[derive(AnvyxRef)]
#[anvyx(name = "Counter")]
pub struct Counter {
    _value: i64,
}

#[methods]
impl Counter {
    pub fn fail(&self) -> RuntimeResult<i64> {
        Err(RuntimeError::new("dynamic provider failure"))
    }
}

#[function(ctx)]
pub fn make_counter<'cx>(ctx: &mut Ctx<'cx, '_>) -> AnvRef<'cx, Counter> {
    AnvRefType::<Counter>::register_untracked_in(ctx).alloc_in(ctx, Counter { _value: 0 })
}

anvyx_runtime::builtin_module! {
    name: "host",
    source: "",
    exports: [Counter, make_counter],
}
