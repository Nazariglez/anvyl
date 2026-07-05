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

#[cfg(test)]
mod tests {
    use std::{cell::Cell, rc::Rc};

    use super::*;
    use crate::{Heap, RuntimeOwnerHandle};

    struct Node {
        traces: Rc<Cell<usize>>,
    }

    unsafe impl<'cx> Trace<'cx> for Node {
        fn trace<D: TraceDriver<'cx>>(&self, _visitor: &mut Visitor<'cx, '_, D>) {
            self.traces.set(self.traces.get() + 1);
        }
    }

    unsafe fn unit(
        _: &RuntimeOwnerHandle,
        handle: &ErasedHandle<'_>,
        _: (),
    ) -> Result<(), RuntimeError> {
        if handle.heap_type_id().index() == usize::MAX {
            Err(RuntimeError::new("invalid callback"))
        } else {
            Ok(())
        }
    }

    #[test]
    fn traces_owned_callback_record_handle() {
        Heap::scope(|heap| {
            let traces = Rc::new(Cell::new(0));
            let ty = heap.register_tracked::<Node>();
            let handle = heap.alloc(
                ty,
                Node {
                    traces: traces.clone(),
                },
            );
            let erased = heap.erase(&handle).unwrap();
            let callback = unsafe {
                AnvCallback::__anvyx_new(RuntimeOwnerHandle::new().unwrap(), erased, unit)
            };
            drop(handle);

            let outcome = heap.collect_all_with_external_roots(&callback);

            assert_eq!(outcome.collected, 0);
            assert!(traces.get() > 0);
            assert!(heap.try_with_erased(callback.handle(), ty, |_| ()).is_ok());
        });
    }

    #[test]
    fn calls_typed_thunk() {
        unsafe fn index(
            _: &RuntimeOwnerHandle,
            handle: &ErasedHandle<'_>,
            arg: (i64,),
        ) -> Result<i64, RuntimeError> {
            let index = i64::try_from(handle.heap_type_id().index())
                .map_err(|_| RuntimeError::new("callback index overflow"))?;
            index
                .checked_add(arg.0)
                .ok_or_else(|| RuntimeError::new("callback index overflow"))
        }

        Heap::scope(|heap| {
            let ty = heap.register_untracked::<()>();
            let handle = heap.alloc(ty, ());
            let erased = heap.erase(&handle).unwrap();
            let callback = unsafe {
                AnvCallback::__anvyx_new(RuntimeOwnerHandle::new().unwrap(), erased, index)
            };

            assert_eq!(callback.call(3).unwrap(), 3);
        });
    }
}
