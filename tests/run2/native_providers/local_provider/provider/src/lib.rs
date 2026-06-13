use anvyx_runtime::{function, Ctx, MutPlace, RuntimeError};

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
) -> Result<(), RuntimeError> {
    value.update_copy(ctx, |n| n + 1)
}

anvyx_runtime::builtin_module! {
    name: "host",
    source: "",
    exports: [host_add, host_len, host_bump],
}
