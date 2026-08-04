use std::marker::PhantomData;

use crate::{ErasedHandle, RuntimeError, RuntimeOwnerHandle, Trace, TraceDriver, Visitor};

pub type AnvCallbackCall<'cx, Args, Ret> =
    unsafe fn(&RuntimeOwnerHandle, &ErasedHandle<'cx>, Args) -> Result<Ret, RuntimeError>;

pub struct AnvCallback<'cx, Args, Ret> {
    owner: RuntimeOwnerHandle,
    handle: ErasedHandle<'cx>,
    call: AnvCallbackCall<'cx, Args, Ret>,
    _marker: PhantomData<fn(Args) -> Ret>,
}

impl<'cx, Args, Ret> AnvCallback<'cx, Args, Ret> {
    /// # Safety
    ///
    /// `owner`, `handle`, and `call` must describe one generated heap-owned callback record with
    /// matching `Args` and `Ret`. The thunk must validate owner liveness before touching generated
    /// runtime state and must not retain borrowed argument state beyond the call.
    #[doc(hidden)]
    pub unsafe fn __anvyx_new(
        owner: RuntimeOwnerHandle,
        handle: ErasedHandle<'cx>,
        call: AnvCallbackCall<'cx, Args, Ret>,
    ) -> Self {
        Self {
            owner,
            handle,
            call,
            _marker: PhantomData,
        }
    }

    pub fn handle(&self) -> &ErasedHandle<'cx> {
        &self.handle
    }

    fn call_tuple(&self, args: Args) -> Result<Ret, RuntimeError> {
        unsafe { (self.call)(&self.owner, &self.handle, args) }
    }
}

impl<Args, Ret> Clone for AnvCallback<'_, Args, Ret> {
    fn clone(&self) -> Self {
        Self {
            owner: self.owner.clone(),
            handle: self.handle.clone(),
            call: self.call,
            _marker: PhantomData,
        }
    }
}

unsafe impl<'cx, Args: 'cx, Ret: 'cx> Trace<'cx> for AnvCallback<'cx, Args, Ret> {
    fn trace<D: TraceDriver<'cx>>(&self, visitor: &mut Visitor<'cx, '_, D>) {
        self.handle.trace(visitor);
    }
}

impl<Ret> AnvCallback<'_, (), Ret> {
    pub fn call(&self) -> Result<Ret, RuntimeError> {
        self.call_tuple(())
    }
}

macro_rules! anv_callback_call {
    ($($arg:ident : $var:ident),+ $(,)?) => {
        impl<Ret, $($arg,)+> AnvCallback<'_, ($($arg,)+), Ret> {
            pub fn call(&self, $($var: $arg),+) -> Result<Ret, RuntimeError> {
                self.call_tuple(($($var,)+))
            }
        }
    };
}

anv_callback_call!(A0: a0);
anv_callback_call!(A0: a0, A1: a1);
anv_callback_call!(A0: a0, A1: a1, A2: a2);
anv_callback_call!(A0: a0, A1: a1, A2: a2, A3: a3);
anv_callback_call!(A0: a0, A1: a1, A2: a2, A3: a3, A4: a4);
anv_callback_call!(A0: a0, A1: a1, A2: a2, A3: a3, A4: a4, A5: a5);
anv_callback_call!(A0: a0, A1: a1, A2: a2, A3: a3, A4: a4, A5: a5, A6: a6);
anv_callback_call!(A0: a0, A1: a1, A2: a2, A3: a3, A4: a4, A5: a5, A6: a6, A7: a7);
