use std::{marker::PhantomData, ptr::NonNull, rc::Rc};

use crate::RuntimeError;

type ScopedLambdaMarker<'call, 'cx, Args, Ret> =
    (&'call mut (), &'cx mut (), fn(Args) -> Ret, Rc<()>);

pub struct ScopedLambda<'call, 'cx, Args, Ret> {
    state: NonNull<()>,
    thunk: unsafe fn(NonNull<()>, Args) -> Result<Ret, RuntimeError>,
    _marker: PhantomData<ScopedLambdaMarker<'call, 'cx, Args, Ret>>,
}

impl<'call, Args, Ret> ScopedLambda<'call, '_, Args, Ret> {
    /// # Safety
    ///
    /// `state` must be generated call state that remains valid and same-thread for the full
    /// native call. `thunk` must match `Args`, `Ret`, and the descriptor signature for that state.
    /// The caller must not invoke the lambda after the native call returns or while any
    /// provider-visible mutable runtime context aliases the generated context reachable from
    /// `state`.
    #[doc(hidden)]
    pub unsafe fn __anvyx_from_raw<State>(
        state: &'call mut State,
        thunk: unsafe fn(NonNull<()>, Args) -> Result<Ret, RuntimeError>,
    ) -> Self {
        Self {
            state: NonNull::from(state).cast(),
            thunk,
            _marker: PhantomData,
        }
    }

    fn call_tuple(&self, args: Args) -> Result<Ret, RuntimeError> {
        unsafe { (self.thunk)(self.state, args) }
    }
}

impl<Ret> ScopedLambda<'_, '_, (), Ret> {
    pub fn call(&self) -> Result<Ret, RuntimeError> {
        self.call_tuple(())
    }
}

macro_rules! scoped_lambda_call {
    ($($arg:ident : $var:ident),+ $(,)?) => {
        impl<Ret, $($arg,)+> ScopedLambda<'_, '_, ($($arg,)+), Ret> {
            pub fn call(&self, $($var: $arg),+) -> Result<Ret, RuntimeError> {
                self.call_tuple(($($var,)+))
            }
        }
    };
}

scoped_lambda_call!(A0: a0);
scoped_lambda_call!(A0: a0, A1: a1);
scoped_lambda_call!(A0: a0, A1: a1, A2: a2);
scoped_lambda_call!(A0: a0, A1: a1, A2: a2, A3: a3);
scoped_lambda_call!(A0: a0, A1: a1, A2: a2, A3: a3, A4: a4);
scoped_lambda_call!(A0: a0, A1: a1, A2: a2, A3: a3, A4: a4, A5: a5);
scoped_lambda_call!(A0: a0, A1: a1, A2: a2, A3: a3, A4: a4, A5: a5, A6: a6);
scoped_lambda_call!(A0: a0, A1: a1, A2: a2, A3: a3, A4: a4, A5: a5, A6: a6, A7: a7);

#[cfg(test)]
mod tests {
    use super::*;

    unsafe fn add(state: NonNull<()>, args: (i64, i64)) -> Result<i64, RuntimeError> {
        let base = unsafe { *(state.as_ptr().cast::<i64>()) };
        Ok(base + args.0 + args.1)
    }

    unsafe fn fail(_: NonNull<()>, _: ()) -> Result<(), RuntimeError> {
        Err(RuntimeError::new("callback failed"))
    }

    #[test]
    fn calls_typed_thunk() {
        let mut base: i64 = 10;
        let f =
            unsafe { ScopedLambda::<'_, '_, (i64, i64), i64>::__anvyx_from_raw(&mut base, add) };

        assert_eq!(f.call(1, 2).unwrap(), 13);
    }

    #[test]
    fn propagates_runtime_error() {
        let mut state = ();
        let f = unsafe { ScopedLambda::<'_, '_, (), ()>::__anvyx_from_raw(&mut state, fail) };

        assert_eq!(f.call().unwrap_err().message(), "callback failed");
    }
}
