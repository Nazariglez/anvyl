use anvyx_runtime::{AnvCallback, AnvRef, AnvRefType, AnvyxRef, Ctx, RuntimeResult, Trace, function};

#[derive(AnvyxRef)]
#[anvyx(name = "Counter")]
pub struct Counter {
    value: i64,
}

#[derive(AnvyxRef, Trace)]
#[trace(crate = anvyx_runtime, ctx = 'cx)]
#[anvyx(name = "Slot")]
pub struct Slot<'cx> {
    callback: AnvCallback<'cx, (), ()>,
    counter: AnvRef<'cx, Counter>,
}

#[function(ctx)]
pub fn make_counter<'cx>(ctx: &mut Ctx<'cx, '_>, value: i64) -> AnvRef<'cx, Counter> {
    AnvRefType::<Counter>::register_untracked_in(ctx).alloc_in(ctx, Counter { value })
}

#[function(ctx)]
pub fn counter_value<'cx>(ctx: &mut Ctx<'cx, '_>, counter: AnvRef<'cx, Counter>) -> i64 {
    counter.with_in(ctx, |counter| counter.value).unwrap()
}

#[function(ctx)]
pub fn make_slot<'cx>(
    ctx: &mut Ctx<'cx, '_>,
    callback: AnvCallback<'cx, (), ()>,
    counter: AnvRef<'cx, Counter>,
) -> AnvRef<'cx, Slot<'cx>> {
    AnvRefType::<Slot<'cx>>::register_tracked_in(ctx).alloc_in(ctx, Slot { callback, counter })
}

#[function(ctx)]
pub fn collect_all<'cx>(ctx: &mut Ctx<'cx, '_>) -> RuntimeResult<()> {
    ctx.collect_all()?;
    Ok(())
}

#[function(ctx)]
pub fn fire_slot<'cx>(ctx: &mut Ctx<'cx, '_>, slot: AnvRef<'cx, Slot<'cx>>) -> RuntimeResult<()> {
    let callback = slot.with_in(ctx, |slot| slot.callback.clone()).unwrap();
    callback.call()
}

anvyx_runtime::builtin_module! {
    name: "host",
    source: "",
    exports: [Counter, Slot, make_counter, counter_value, make_slot, collect_all, fire_slot],
}
