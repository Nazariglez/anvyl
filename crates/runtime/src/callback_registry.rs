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

#[cfg(test)]
mod tests {
    use std::ptr::NonNull;

    use super::*;

    fn key(slot_generation: NonZeroU64) -> CallbackKey {
        let owner = RuntimeOwnerHandle::new().unwrap();
        CallbackKey::new(
            owner.owner_id(),
            owner.shutdown_generation(),
            nonzero(2),
            nonzero(3),
            0,
            slot_generation,
        )
    }

    fn owner() -> (RuntimeOwnerHandle, Box<i64>) {
        let owner = RuntimeOwnerHandle::new().unwrap();
        let mut runtime = Box::new(1_i64);
        unsafe {
            owner
                .__anvyx_attach_owner_ptr(NonNull::from(runtime.as_mut()).cast())
                .unwrap();
        };
        (owner, runtime)
    }

    #[test]
    fn close_is_idempotent_for_closed_slot() {
        let mut slot = CallbackSlotInner::<()>::default();
        let key = key(slot.insert(()).unwrap());

        assert!(slot.close(key).closed);
        assert_eq!(slot.handle, None);
        assert!(!slot.close(key).closed);
    }

    #[test]
    fn stale_generation_close_is_false() {
        let mut slot = CallbackSlotInner::<()>::default();
        let stale = key(slot.generation());
        slot.insert(()).unwrap();

        assert!(!slot.close(stale).closed);
    }

    #[test]
    fn open_rejects_live_or_closing_slot() {
        let (owner, _runtime) = owner();
        let mut slot = CallbackSlot::default();
        let key = key(slot.insert(7).unwrap());

        assert_eq!(
            slot.insert(8).unwrap_err().message(),
            "callback slot is already open"
        );
        let (_, guard) = unsafe { slot.begin_invocation(&owner, key) }.unwrap();
        drop(guard);
        assert!(slot.close(key).closed);
    }

    #[test]
    fn begin_invocation_overflow_does_not_increment_owner() {
        let (owner, _runtime) = owner();
        let mut slot = CallbackSlotInner {
            handle: Some(7),
            generation: nonzero(2),
            state: CallbackSlotState::Open,
            active_invocations: usize::MAX,
        };
        let key = key(slot.generation());

        assert_eq!(
            slot.begin_invocation(&owner, key).unwrap_err().message(),
            "callback slot invocation count overflow"
        );
        assert!(owner.__anvyx_begin_shutdown().is_ok());
    }

    #[test]
    fn generation_overflow_retires_slot() {
        let mut slot = CallbackSlotInner::<()> {
            handle: None,
            generation: nonzero(u64::MAX),
            state: CallbackSlotState::Vacant,
            active_invocations: 0,
        };

        assert_eq!(
            slot.open().unwrap_err().message(),
            "callback slot generation overflow"
        );
        assert_eq!(slot.state(), CallbackSlotState::Retired);
    }

    #[test]
    fn active_close_defers_handle_removal_until_finish() {
        let (owner, _runtime) = owner();
        let mut slot = CallbackSlot::default();
        let key = key(slot.insert(7).unwrap());
        let (_, guard) = unsafe { slot.begin_invocation(&owner, key) }.unwrap();

        let close = slot.close(key);
        assert!(close.closed);
        assert_eq!(slot.inner.handle, Some(7));
        assert_eq!(slot.state(), CallbackSlotState::Closing);

        guard.finish().unwrap();
        assert_eq!(slot.inner.handle, None);
        assert_eq!(slot.state(), CallbackSlotState::Closed);
    }

    #[test]
    fn invocation_guard_blocks_shutdown_until_drop() {
        let (owner, _runtime) = owner();
        let mut slot = CallbackSlot::default();
        let key = key(slot.insert(7).unwrap());
        let (_, guard) = unsafe { slot.begin_invocation(&owner, key) }.unwrap();

        assert_eq!(
            owner.__anvyx_begin_shutdown().unwrap_err().message(),
            "runtime callback invocation is active"
        );
        drop(guard);
        assert!(owner.__anvyx_begin_shutdown().is_ok());
    }

    #[test]
    fn guard_survives_vec_growth() {
        let (owner, _runtime) = owner();
        let mut slots = vec![CallbackSlot::default()];
        let key = key(slots[0].insert(7).unwrap());
        let (_, guard) = unsafe { slots[0].begin_invocation(&owner, key) }.unwrap();
        let inner = NonNull::from(slots[0].inner.as_mut());

        for _ in 0..128 {
            slots.push(CallbackSlot::default());
        }

        assert_eq!(inner, NonNull::from(slots[0].inner.as_mut()));
        assert!(slots[0].close(key).closed);
        guard.finish().unwrap();
        assert!(slots[0].is_free());
    }

    #[test]
    fn slot_reuse_keeps_generation_moving() {
        let (owner, _runtime) = owner();
        let mut slot = CallbackSlot::default();
        let first_generation = slot.insert("first").unwrap();
        let first_key = key(first_generation);

        let close = slot.close(first_key);
        assert!(close.closed);
        assert!(slot.is_free());

        let second_generation = slot.insert("second").unwrap();
        assert_ne!(first_generation, second_generation);
        assert!(unsafe { slot.begin_invocation(&owner, first_key) }.is_err());
    }

    #[test]
    fn slot_close_during_invocation_defers_handle_removal() {
        let (owner, _runtime) = owner();
        let mut slot = CallbackSlot::default();
        let key = key(slot.insert(7).unwrap());
        let (handle, guard) = unsafe { slot.begin_invocation(&owner, key) }.unwrap();

        let close = slot.close(key);
        assert!(close.closed);
        assert_eq!(handle, 7);
        assert_eq!(slot.state(), CallbackSlotState::Closing);

        guard.finish().unwrap();
        assert!(slot.is_free());
    }
}
