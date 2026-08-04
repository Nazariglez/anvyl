use std::{
    num::NonZeroU64,
    panic::{AssertUnwindSafe, catch_unwind},
    ptr::NonNull,
};

use crate::{RuntimeError, escaping_lambda::CallbackKey, runtime_owner::RuntimeOwnerHandle};

#[doc(hidden)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CallbackSlotState {
    Vacant,
    Open,
    Closing,
    Closed,
    Retired,
}

#[doc(hidden)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct CallbackCloseResult {
    pub closed: bool,
}

#[doc(hidden)]
#[derive(Debug)]
pub struct CallbackSlot<Handle> {
    inner: Box<CallbackSlotInner<Handle>>,
}

#[derive(Debug)]
struct CallbackSlotInner<Handle> {
    handle: Option<Handle>,
    generation: NonZeroU64,
    state: CallbackSlotState,
    active_invocations: usize,
}

#[doc(hidden)]
#[derive(Debug)]
pub struct CallbackInvocationGuard<Handle> {
    owner: RuntimeOwnerHandle,
    key: CallbackKey,
    slot: NonNull<CallbackSlotInner<Handle>>,
    active: bool,
}

impl<Handle> Default for CallbackSlot<Handle> {
    fn default() -> Self {
        Self {
            inner: Box::default(),
        }
    }
}

impl<Handle> CallbackSlot<Handle> {
    pub fn is_free(&self) -> bool {
        self.inner.handle.is_none()
            && matches!(
                self.inner.state(),
                CallbackSlotState::Vacant | CallbackSlotState::Closed
            )
    }

    pub fn generation(&self) -> NonZeroU64 {
        self.inner.generation()
    }

    pub fn state(&self) -> CallbackSlotState {
        self.inner.state()
    }

    pub fn insert(&mut self, handle: Handle) -> Result<NonZeroU64, RuntimeError> {
        self.inner.insert(handle)
    }

    pub fn close(&mut self, key: CallbackKey) -> CallbackCloseResult {
        self.inner.close(key)
    }
}

impl<Handle: Clone> CallbackSlot<Handle> {
    /// # Safety
    ///
    /// The owning callback slot allocation must outlive the returned guard.
    pub unsafe fn begin_invocation(
        &mut self,
        owner: &RuntimeOwnerHandle,
        key: CallbackKey,
    ) -> Result<(Handle, CallbackInvocationGuard<Handle>), RuntimeError> {
        let handle = self.inner.begin_invocation(owner, key)?;
        Ok((
            handle,
            CallbackInvocationGuard {
                owner: owner.clone(),
                key,
                slot: NonNull::from(self.inner.as_mut()),
                active: true,
            },
        ))
    }
}

impl<Handle> Default for CallbackSlotInner<Handle> {
    fn default() -> Self {
        Self {
            handle: None,
            generation: nonzero(1),
            state: CallbackSlotState::Vacant,
            active_invocations: 0,
        }
    }
}

impl<Handle> CallbackSlotInner<Handle> {
    fn generation(&self) -> NonZeroU64 {
        self.generation
    }

    fn state(&self) -> CallbackSlotState {
        self.state
    }

    fn insert(&mut self, handle: Handle) -> Result<NonZeroU64, RuntimeError> {
        if self.handle.is_some() {
            return Err(RuntimeError::new("callback slot is already open"));
        }
        let generation = self.open()?;
        self.handle = Some(handle);
        Ok(generation)
    }

    fn open(&mut self) -> Result<NonZeroU64, RuntimeError> {
        match self.state {
            CallbackSlotState::Vacant | CallbackSlotState::Closed => {}
            CallbackSlotState::Open | CallbackSlotState::Closing => {
                return Err(RuntimeError::new("callback slot is already open"));
            }
            CallbackSlotState::Retired => {
                return Err(RuntimeError::new("callback slot is retired"));
            }
        }
        self.generation = match next_generation(self.generation) {
            Ok(generation) => generation,
            Err(error) => {
                self.state = CallbackSlotState::Retired;
                return Err(error);
            }
        };
        self.state = CallbackSlotState::Open;
        self.active_invocations = 0;
        Ok(self.generation)
    }

    fn close(&mut self, key: CallbackKey) -> CallbackCloseResult {
        if self.generation != key.slot_generation() {
            return CallbackCloseResult { closed: false };
        }
        let closed = match self.state {
            CallbackSlotState::Open if self.active_invocations == 0 => {
                self.state = CallbackSlotState::Closed;
                self.handle.take();
                true
            }
            CallbackSlotState::Open => {
                self.state = CallbackSlotState::Closing;
                true
            }
            CallbackSlotState::Closed => {
                self.handle.take();
                false
            }
            CallbackSlotState::Vacant | CallbackSlotState::Closing | CallbackSlotState::Retired => {
                false
            }
        };
        CallbackCloseResult { closed }
    }

    fn finish_invocation(&mut self, key: CallbackKey) -> Result<(), RuntimeError> {
        self.check_live_key(key)?;
        if self.active_invocations == 0 {
            return Err(RuntimeError::new("callback slot invocation underflow"));
        }
        self.active_invocations -= 1;
        if self.state == CallbackSlotState::Closing && self.active_invocations == 0 {
            self.state = CallbackSlotState::Closed;
            self.handle.take();
        }
        Ok(())
    }

    fn check_live_key(&self, key: CallbackKey) -> Result<(), RuntimeError> {
        if self.generation == key.slot_generation() {
            Ok(())
        } else {
            Err(RuntimeError::new("callback slot generation mismatch"))
        }
    }
}

impl<Handle: Clone> CallbackSlotInner<Handle> {
    fn begin_invocation(
        &mut self,
        owner: &RuntimeOwnerHandle,
        key: CallbackKey,
    ) -> Result<Handle, RuntimeError> {
        self.check_live_key(key)?;
        if self.state != CallbackSlotState::Open {
            return Err(RuntimeError::new("callback slot is not open"));
        }
        let next_invocations = self
            .active_invocations
            .checked_add(1)
            .ok_or_else(|| RuntimeError::new("callback slot invocation count overflow"))?;
        let handle = self
            .handle
            .as_ref()
            .ok_or_else(|| RuntimeError::new("callback slot is closed"))?
            .clone();
        owner.__anvyx_increment_active_invocations()?;
        self.active_invocations = next_invocations;
        Ok(handle)
    }
}

impl<Handle> CallbackInvocationGuard<Handle> {
    pub fn finish(mut self) -> Result<(), RuntimeError> {
        unsafe { self.slot.as_mut().finish_invocation(self.key) }?;
        self.owner.__anvyx_decrement_active_invocations();
        self.active = false;
        Ok(())
    }
}

impl<Handle> Drop for CallbackInvocationGuard<Handle> {
    fn drop(&mut self) {
        if !self.active {
            return;
        }
        let _ = catch_unwind(AssertUnwindSafe(|| unsafe {
            self.slot.as_mut().finish_invocation(self.key)
        }));
        self.owner.__anvyx_decrement_active_invocations();
        self.active = false;
    }
}

fn next_generation(generation: NonZeroU64) -> Result<NonZeroU64, RuntimeError> {
    NonZeroU64::new(
        generation
            .get()
            .checked_add(1)
            .ok_or_else(|| RuntimeError::new("callback slot generation overflow"))?,
    )
    .ok_or_else(|| RuntimeError::new("callback slot generation overflow"))
}

fn nonzero(value: u64) -> NonZeroU64 {
    NonZeroU64::new(value).expect("value must be non-zero")
}
