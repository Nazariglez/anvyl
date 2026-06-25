use std::cell::RefCell;

use anvyx_runtime::{Ctx, EscapingLambda, RuntimeError, function};

thread_local! {
    static CALLBACK: RefCell<Option<EscapingLambda<(i64,), i64>>> = const { RefCell::new(None) };
}

#[function]
pub fn retain(cb: EscapingLambda<(i64,), i64>) {
    CALLBACK.with(|slot| {
        drop(slot.replace(Some(cb)));
    });
}

#[function(ctx)]
pub fn fire<'cx>(ctx: &mut Ctx<'cx, '_>) -> Result<(), RuntimeError> {
    ctx.collect(0)?;
    let callback = CALLBACK.with(|slot| {
        slot.borrow_mut()
            .take()
            .ok_or_else(|| RuntimeError::new("missing retained callback"))
    })?;
    let value = callback.call(41);
    CALLBACK.with(|slot| {
        let mut slot = slot.borrow_mut();
        if slot.is_none() {
            *slot = Some(callback);
        }
    });
    println!("{}", value?);
    Ok(())
}

#[function(ctx)]
pub fn close<'cx>(ctx: &mut Ctx<'cx, '_>) -> Result<bool, RuntimeError> {
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
    exports: [retain, fire, close],
}
