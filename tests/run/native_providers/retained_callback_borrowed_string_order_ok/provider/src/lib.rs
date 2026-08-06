use std::cell::RefCell;

use anvyx_runtime::{EscapingLambda, RuntimeError, RuntimeResult, function};

thread_local! {
    static CALLBACK: RefCell<Option<EscapingLambda<(), ()>>> = const { RefCell::new(None) };
}

#[function]
pub fn log_and_fire(msg: &str) -> RuntimeResult<()> {
    println!("{msg}");
    let callback = CALLBACK.with(|slot| {
        slot.borrow_mut()
            .take()
            .ok_or_else(|| RuntimeError::new("missing retained callback"))
    })?;
    let result = callback.call();
    CALLBACK.with(|slot| {
        let mut slot = slot.borrow_mut();
        if slot.is_none() {
            *slot = Some(callback);
        }
    });
    result
}

#[function]
pub fn retain(callback: EscapingLambda<(), ()>) {
    CALLBACK.with(|slot| *slot.borrow_mut() = Some(callback));
}

#[function]
pub fn close() -> RuntimeResult<()> {
    let callback = CALLBACK.with(|slot| slot.borrow_mut().take());
    if let Some(mut callback) = callback {
        callback.close()?;
    }
    Ok(())
}

anvyx_runtime::builtin_module! {
    name: "host",
    source: "",
    exports: [log_and_fire, retain, close],
}
