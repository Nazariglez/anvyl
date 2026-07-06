use anvyx_runtime::{function, Ctx, MutPlace, RuntimeResult, ScopedLambda};

#[function]
pub fn host_add(a: i64, b: i64) -> i64 {
    a + b
}

#[function]
pub fn host_len(text: &str) -> i64 {
    text.len() as i64
}

#[function(ctx)]
pub fn host_bump<'cx>(
    ctx: &mut Ctx<'cx, '_>,
    mut value: MutPlace<'_, 'cx, i64>,
) -> RuntimeResult<()> {
    value.update_copy(ctx, |n| n + 1)
}

#[function]
pub fn host_apply(f: ScopedLambda<'_, '_, (i64,), i64>) -> RuntimeResult<i64> {
    f.call(41)
}

#[function]
pub fn host_call0(f: ScopedLambda<'_, '_, (), i64>) -> RuntimeResult<i64> {
    f.call()
}

#[function]
pub fn host_each(f: ScopedLambda<'_, '_, (i64,), ()>) -> RuntimeResult<()> {
    f.call(1)?;
    f.call(2)
}

anvyx_runtime::builtin_module! {
    name: "host",
    source: "",
    exports: [host_add, host_len, host_bump, host_apply, host_call0, host_each],
}
