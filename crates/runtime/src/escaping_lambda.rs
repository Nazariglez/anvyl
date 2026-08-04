use std::{
    marker::PhantomData,
    num::NonZeroU64,
    panic::{AssertUnwindSafe, catch_unwind},
};

use crate::{RuntimeError, RuntimeOwnerHandle};

pub type EscapingLambdaCall<Args, Ret> =
    unsafe fn(&RuntimeOwnerHandle, CallbackKey, Args) -> Result<Ret, RuntimeError>;

pub type EscapingLambdaClose =
    unsafe fn(&RuntimeOwnerHandle, CallbackKey) -> Result<bool, RuntimeError>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CallbackKey {
    owner_id: NonZeroU64,
    shutdown_generation: NonZeroU64,
    table_id: NonZeroU64,
    signature_id: NonZeroU64,
    index: usize,
    slot_generation: NonZeroU64,
}

pub struct EscapingLambda<Args, Ret> {
    owner: RuntimeOwnerHandle,
    key: CallbackKey,
    call: EscapingLambdaCall<Args, Ret>,
    close: EscapingLambdaClose,
    closed: bool,
    _marker: PhantomData<fn(Args) -> Ret>,
}

impl CallbackKey {
    pub fn new(
        owner_id: NonZeroU64,
        shutdown_generation: NonZeroU64,
        table_id: NonZeroU64,
        signature_id: NonZeroU64,
        index: usize,
        slot_generation: NonZeroU64,
    ) -> Self {
        Self {
            owner_id,
            shutdown_generation,
            table_id,
            signature_id,
            index,
            slot_generation,
        }
    }

    pub fn owner_id(self) -> NonZeroU64 {
        self.owner_id
    }

    pub fn shutdown_generation(self) -> NonZeroU64 {
        self.shutdown_generation
    }

    pub fn table_id(self) -> NonZeroU64 {
        self.table_id
    }

    pub fn signature_id(self) -> NonZeroU64 {
        self.signature_id
    }

    pub fn index(self) -> usize {
        self.index
    }

    pub fn slot_generation(self) -> NonZeroU64 {
        self.slot_generation
    }

    #[doc(hidden)]
    pub fn __anvyx_check_identity(
        self,
        table_id: NonZeroU64,
        signature_id: NonZeroU64,
    ) -> Result<(), RuntimeError> {
        if self.table_id != table_id {
            return Err(RuntimeError::new("callback table mismatch"));
        }
        if self.signature_id != signature_id {
            return Err(RuntimeError::new("callback signature mismatch"));
        }
        Ok(())
    }
}

impl<Args, Ret> EscapingLambda<Args, Ret> {
    /// # Safety
    ///
    /// `owner`, `key`, `call`, and `close` must describe one generated callback slot with
    /// matching `Args` and `Ret`. The thunks must validate owner/key liveness before touching
    /// generated runtime state and must not retain borrowed argument state beyond the call.
    #[doc(hidden)]
    pub unsafe fn __anvyx_new(
        owner: RuntimeOwnerHandle,
        key: CallbackKey,
        call: EscapingLambdaCall<Args, Ret>,
        close: EscapingLambdaClose,
    ) -> Self {
        Self {
            owner,
            key,
            call,
            close,
            closed: false,
            _marker: PhantomData,
        }
    }

    pub fn close(&mut self) -> Result<bool, RuntimeError> {
        if self.closed {
            return Ok(false);
        }
        let closed = unsafe { (self.close)(&self.owner, self.key)? };
        self.closed = true;
        Ok(closed)
    }

    pub fn is_closed(&self) -> bool {
        self.closed
    }

    fn call_tuple(&self, args: Args) -> Result<Ret, RuntimeError> {
        if self.closed {
            return Err(RuntimeError::new("escaping Lambda is closed"));
        }
        unsafe { (self.call)(&self.owner, self.key, args) }
    }
}

impl<Args, Ret> Drop for EscapingLambda<Args, Ret> {
    fn drop(&mut self) {
        let _ = catch_unwind(AssertUnwindSafe(|| {
            let _ = self.close();
        }));
    }
}

impl<Ret> EscapingLambda<(), Ret> {
    pub fn call(&self) -> Result<Ret, RuntimeError> {
        self.call_tuple(())
    }
}

macro_rules! escaping_lambda_call {
    ($($arg:ident : $var:ident),+ $(,)?) => {
        impl<Ret, $($arg,)+> EscapingLambda<($($arg,)+), Ret> {
            pub fn call(&self, $($var: $arg),+) -> Result<Ret, RuntimeError> {
                self.call_tuple(($($var,)+))
            }
        }
    };
}

escaping_lambda_call!(A0: a0);
escaping_lambda_call!(A0: a0, A1: a1);
escaping_lambda_call!(A0: a0, A1: a1, A2: a2);
escaping_lambda_call!(A0: a0, A1: a1, A2: a2, A3: a3);
escaping_lambda_call!(A0: a0, A1: a1, A2: a2, A3: a3, A4: a4);
escaping_lambda_call!(A0: a0, A1: a1, A2: a2, A3: a3, A4: a4, A5: a5);
escaping_lambda_call!(A0: a0, A1: a1, A2: a2, A3: a3, A4: a4, A5: a5, A6: a6);
escaping_lambda_call!(A0: a0, A1: a1, A2: a2, A3: a3, A4: a4, A5: a5, A6: a6, A7: a7);
