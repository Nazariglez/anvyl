use std::cell::RefCell;

use anvyx_runtime::{
    AnvRef, AnvRefType, AnvyxRef, Ctx, EscapingLambda, RuntimeError, RuntimeResult, function,
    methods,
};

thread_local! {
    static DRAW: RefCell<Option<EscapingLambda<(), ()>>> = const { RefCell::new(None) };
}

#[derive(AnvyxRef)]
#[anvyx(name = "Surface")]
pub struct Surface {
    id: i64,
}

#[methods]
impl Surface {
    pub fn on_draw(&self, callback: EscapingLambda<(), ()>) {
        DRAW.with(|slot| *slot.borrow_mut() = Some(callback));
    }

    pub fn id(&self) -> i64 {
        self.id
    }
}

#[function(ctx)]
pub fn make_surface<'cx>(ctx: &mut Ctx<'cx, '_>, id: i64) -> AnvRef<'cx, Surface> {
    AnvRefType::<Surface>::register_untracked_in(ctx).alloc_in(ctx, Surface { id })
}

#[function(ctx)]
pub fn trigger_draw<'cx>(ctx: &mut Ctx<'cx, '_>) -> RuntimeResult<()> {
    ctx.collect(0)?;
    let callback = DRAW.with(|slot| {
        slot.borrow_mut()
            .take()
            .ok_or_else(|| RuntimeError::new("missing draw callback"))
    })?;
    callback.call()?;
    DRAW.with(|slot| *slot.borrow_mut() = Some(callback));
    Ok(())
}

#[function(ctx)]
pub fn close_draw<'cx>(ctx: &mut Ctx<'cx, '_>) -> RuntimeResult<bool> {
    ctx.collect(0)?;
    DRAW.with(|slot| {
        let Some(mut callback) = slot.borrow_mut().take() else {
            return Ok(false);
        };
        callback.close()
    })
}

anvyx_runtime::builtin_module! {
    name: "host",
    exports: [Surface, make_surface, trigger_draw, close_draw],
}
