use std::cell::RefCell;

use anvyx_runtime::{
    AnvRef, AnvRefType, AnvyxRef, Ctx, EscapingLambda, RuntimeError, RuntimeResult, function,
    methods,
};

thread_local! {
    static CALLBACK: RefCell<Option<EscapingLambda<(), ()>>> = const { RefCell::new(None) };
}

#[derive(AnvyxRef)]
#[anvyx(name = "Counter")]
pub struct Counter {
    value: i64,
}

#[methods]
impl Counter {
    #[anvyx(init)]
    pub fn new(value: i64) -> Self {
        Self { value }
    }

    #[anvyx(getter)]
    pub fn value(&self) -> i64 {
        self.value
    }

    #[anvyx(setter)]
    pub fn set_value(&mut self, value: i64) {
        self.value = value;
    }

    pub fn bump(&mut self, delta: i64) {
        self.value += delta;
    }
}

#[function(ctx)]
pub fn make_counter<'cx>(ctx: &mut Ctx<'cx, '_>, value: i64) -> AnvRef<'cx, Counter> {
    AnvRefType::<Counter>::register_untracked_in(ctx).alloc_in(ctx, Counter { value })
}

#[function(ctx)]
pub fn counter_value<'cx>(ctx: &mut Ctx<'cx, '_>, counter: AnvRef<'cx, Counter>) -> i64 {
    counter.with_in(ctx, |counter| counter.value).unwrap()
}

#[function]
pub fn retain(cb: EscapingLambda<(), ()>) {
    CALLBACK.with(|slot| *slot.borrow_mut() = Some(cb));
}

#[function(ctx)]
pub fn fire<'cx>(ctx: &mut Ctx<'cx, '_>) -> RuntimeResult<()> {
    ctx.collect(0)?;
    let callback = CALLBACK.with(|slot| {
        slot.borrow_mut()
            .take()
            .ok_or_else(|| RuntimeError::new("missing retained callback"))
    })?;
    callback.call()?;
    CALLBACK.with(|slot| *slot.borrow_mut() = Some(callback));
    Ok(())
}

#[function(ctx)]
pub fn close<'cx>(ctx: &mut Ctx<'cx, '_>) -> RuntimeResult<bool> {
    ctx.collect(0)?;
    CALLBACK.with(|slot| {
        let Some(mut callback) = slot.borrow_mut().take() else {
            return Ok(false);
        };
        callback.close()
    })
}

anvyx_runtime::builtin_module! {
    name: "host",
    source: "",
    exports: [Counter, make_counter, counter_value, retain, fire, close],
}
