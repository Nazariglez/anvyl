use std::cell::RefCell;

use anvyx_runtime::{
    AnvSlice, AnvString, Ctx, EscapingLambda, RuntimeError, RuntimeResult, ScopedLambda, function,
};

#[derive(Default)]
struct Slot {
    cb: Option<EscapingLambda<(), ()>>,
    active: usize,
    close_requested: bool,
}

impl Slot {
    fn retain(&mut self, cb: EscapingLambda<(), ()>) -> RuntimeResult<()> {
        if self.active > 0 {
            return Err(RuntimeError::new("cannot replace active retained callback"));
        }
        self.cb = Some(cb);
        self.close_requested = false;
        Ok(())
    }

    fn fire(&mut self) -> RuntimeResult<*const EscapingLambda<(), ()>> {
        if self.close_requested {
            return Err(RuntimeError::new("missing retained callback"));
        }
        let cb = self
            .cb
            .as_ref()
            .ok_or_else(|| RuntimeError::new("missing retained callback"))?;
        self.active += 1;
        Ok(cb as *const _)
    }

    fn finish(&mut self) -> Option<EscapingLambda<(), ()>> {
        self.active -= 1;
        if self.active == 0 && self.close_requested {
            self.cb.take()
        } else {
            None
        }
    }

    fn close(&mut self) -> RuntimeResult<bool> {
        if self.active > 0 {
            let closed = self.cb.is_some() && !self.close_requested;
            self.close_requested |= closed;
            return Ok(closed);
        }
        let Some(mut cb) = self.cb.take() else {
            return Ok(false);
        };
        cb.close()
    }
}

thread_local! {
    static PRIMARY: RefCell<Slot> = RefCell::new(Slot::default());
    static SECONDARY: RefCell<Slot> = RefCell::new(Slot::default());
}

fn fire_slot(slot: &'static std::thread::LocalKey<RefCell<Slot>>) -> RuntimeResult<()> {
    let cb = slot.with(|slot| slot.borrow_mut().fire())?;
    let result = unsafe { (&*cb).call() };
    let close = slot.with(|slot| slot.borrow_mut().finish());
    if let Some(mut cb) = close {
        cb.close()?;
    }
    result
}

#[function]
pub fn retain(cb: EscapingLambda<(), ()>) -> RuntimeResult<()> {
    PRIMARY.with(|slot| slot.borrow_mut().retain(cb))
}

#[function]
pub fn retain_second(cb: EscapingLambda<(), ()>) -> RuntimeResult<()> {
    SECONDARY.with(|slot| slot.borrow_mut().retain(cb))
}

#[function]
pub fn fire() -> RuntimeResult<()> {
    fire_slot(&PRIMARY)
}

#[function]
pub fn fire_second() -> RuntimeResult<()> {
    fire_slot(&SECONDARY)
}

#[function]
pub fn close() -> RuntimeResult<bool> {
    PRIMARY.with(|slot| slot.borrow_mut().close())
}

#[function]
pub fn close_second() -> RuntimeResult<bool> {
    SECONDARY.with(|slot| slot.borrow_mut().close())
}

#[function(ctx)]
pub fn fire_with_heap_borrow<'cx>(ctx: &mut Ctx<'cx, '_>) -> RuntimeResult<()> {
    let heap = ctx.heap_ref();
    let result = fire_slot(&PRIMARY);
    drop(heap);
    result
}

#[function(ctx)]
pub fn close_with_heap_borrow<'cx>(ctx: &mut Ctx<'cx, '_>) -> RuntimeResult<bool> {
    let heap = ctx.heap_ref();
    let result = PRIMARY.with(|slot| slot.borrow_mut().close());
    drop(heap);
    result
}

#[function]
pub fn host_log(msg: &str) {
    println!("{msg}");
}

#[function]
pub fn host_add(a: i64, b: i64) -> i64 {
    a + b
}

#[function]
pub fn host_name() -> AnvString {
    AnvString::from("goblin")
}

#[function(ctx)]
pub fn collect_now<'cx>(ctx: &mut Ctx<'cx, '_>) -> RuntimeResult<()> {
    ctx.collect(0)?;
    Ok(())
}

#[function(ctx)]
pub fn host_slice_first<'cx>(
    ctx: &mut Ctx<'cx, '_>,
    values: AnvSlice<'cx, i64>,
) -> RuntimeResult<i64> {
    values.elem_at_shared(ctx, 0)
}

#[function]
pub fn each(cb: ScopedLambda<'_, '_, (i64,), ()>) -> RuntimeResult<()> {
    cb.call(1)?;
    cb.call(2)
}

anvyx_runtime::builtin_module! {
    name: "host",
    source: "",
    exports: [
        retain, retain_second, fire, fire_second, close, close_second,
        fire_with_heap_borrow, close_with_heap_borrow,
        host_log, host_add, host_name, collect_now, host_slice_first, each,
    ],
}
