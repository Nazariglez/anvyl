use anvyx_runtime::{Ctx, RuntimeError, function};

#[function(ctx, trap)]
pub fn collect_cycles(ctx: &mut Ctx<'_, '_>) -> Result<(), RuntimeError> {
    ctx.collect_all()?;
    Ok(())
}

#[function(ctx)]
pub fn auto_collect(ctx: &mut Ctx<'_, '_>, enabled: bool) {
    ctx.set_collection_enabled(enabled);
}

#[function(ctx)]
pub fn managed_count(ctx: &mut Ctx<'_, '_>) -> i64 {
    ctx.stats().live as i64
}

anvyx_runtime::builtin_module! {
    provider: "stdlib_mem",
    name: "mem",
    source: include_str!("mem.anv"),
    exports: [collect_cycles, auto_collect, managed_count],
}
