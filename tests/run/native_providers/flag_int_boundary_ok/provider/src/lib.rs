use anvyx_runtime::{function, Ctx, MutPlace, RuntimeResult};

#[function]
pub fn echo_bits(bits: i64) -> i64 {
    bits
}

#[function(ctx)]
pub fn clear_bits<'cx>(
    ctx: &mut Ctx<'cx, '_>,
    mut bits: MutPlace<'_, 'cx, i64>,
) -> RuntimeResult<()> {
    bits.update_copy(ctx, |_| 0)
}

anvyx_runtime::builtin_module! {
    name: "host",
    exports: [echo_bits, clear_bits],
}
