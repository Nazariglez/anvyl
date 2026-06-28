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
pub enum CallbackCloseAction {
    None,
    RemoveRoot,
}

#[doc(hidden)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct CallbackCloseResult {
    pub closed: bool,
    pub action: CallbackCloseAction,
}

#[doc(hidden)]
#[derive(Debug)]
pub struct CallbackSlot<Root> {
    root: Option<Root>,
    meta: Box<CallbackSlotMeta>,
}

#[doc(hidden)]
#[derive(Debug)]
pub struct CallbackSlotMeta {
    generation: NonZeroU64,
    state: CallbackSlotState,
    active_invocations: usize,
}

#[doc(hidden)]
#[derive(Debug)]
pub struct CallbackInvocationGuard {
    owner: RuntimeOwnerHandle,
    key: CallbackKey,
    slot: NonNull<CallbackSlotMeta>,
    active: bool,
}

impl<Root> Default for CallbackSlot<Root> {
    fn default() -> Self {
        Self {
            root: None,
            meta: Box::default(),
        }
    }
}

impl<Root> CallbackSlot<Root> {
    pub fn is_free(&self) -> bool {
        self.root.is_none()
            && matches!(
                self.meta.state(),
                CallbackSlotState::Vacant | CallbackSlotState::Closed
            )
    }

    pub fn generation(&self) -> NonZeroU64 {
        self.meta.generation()
    }

    pub fn state(&self) -> CallbackSlotState {
        self.meta.state()
    }

    pub fn insert(&mut self, root: Root) -> Result<NonZeroU64, RuntimeError> {
        if self.root.is_some() {
            return Err(RuntimeError::new("callback slot is already open"));
        }
        let generation = self.meta.open()?;
        self.root = Some(root);
        Ok(generation)
    }

    pub fn begin_invocation(
        &mut self,
        owner: &RuntimeOwnerHandle,
        key: CallbackKey,
    ) -> Result<(&Root, CallbackInvocationGuard), RuntimeError> {
        let root = self
            .root
            .as_ref()
            .ok_or_else(|| RuntimeError::new("callback slot is closed"))?;
        let guard = self.meta.begin_invocation(owner, key)?;
        Ok((root, guard))
    }

    pub fn close(&mut self, key: CallbackKey) -> (CallbackCloseResult, Option<Root>) {
        let close = self.meta.close(key);
        let should_remove_root =
            close.action == CallbackCloseAction::RemoveRoot || self.meta.closed_for(key);
        let root = should_remove_root.then(|| self.root.take()).flatten();
        (close, root)
    }

    pub fn take_closed_root(&mut self, key: CallbackKey) -> Option<Root> {
        self.meta
            .closed_for(key)
            .then(|| self.root.take())
            .flatten()
    }
}

impl Default for CallbackSlotMeta {
    fn default() -> Self {
        Self {
            generation: nonzero(1),
            state: CallbackSlotState::Vacant,
            active_invocations: 0,
        }
    }
}

impl CallbackSlotMeta {
    pub fn generation(&self) -> NonZeroU64 {
        self.generation
    }

    pub fn state(&self) -> CallbackSlotState {
        self.state
    }

    pub fn open(&mut self) -> Result<NonZeroU64, RuntimeError> {
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

    pub fn begin_invocation(
        &mut self,
        owner: &RuntimeOwnerHandle,
        key: CallbackKey,
    ) -> Result<CallbackInvocationGuard, RuntimeError> {
        self.check_live_key(key)?;
        if self.state != CallbackSlotState::Open {
            return Err(RuntimeError::new("callback slot is not open"));
        }
        let next_invocations = self
            .active_invocations
            .checked_add(1)
            .ok_or_else(|| RuntimeError::new("callback slot invocation count overflow"))?;
        owner.__anvyx_increment_active_invocations()?;
        self.active_invocations = next_invocations;
        Ok(CallbackInvocationGuard {
            owner: owner.clone(),
            key,
            slot: NonNull::from(self),
            active: true,
        })
    }

    pub fn close(&mut self, key: CallbackKey) -> CallbackCloseResult {
        if self.generation != key.slot_generation() {
            return CallbackCloseResult::none();
        }
        match self.state {
            CallbackSlotState::Open if self.active_invocations == 0 => {
                self.state = CallbackSlotState::Closed;
                CallbackCloseResult::remove_root()
            }
            CallbackSlotState::Open => {
                self.state = CallbackSlotState::Closing;
                CallbackCloseResult::closed()
            }
            CallbackSlotState::Vacant
            | CallbackSlotState::Closing
            | CallbackSlotState::Closed
            | CallbackSlotState::Retired => CallbackCloseResult::none(),
        }
    }

    pub fn finish_invocation(
        &mut self,
        key: CallbackKey,
    ) -> Result<CallbackCloseAction, RuntimeError> {
        self.check_live_key(key)?;
        if self.active_invocations == 0 {
            return Err(RuntimeError::new("callback slot invocation underflow"));
        }
        self.active_invocations -= 1;
        if self.state == CallbackSlotState::Closing && self.active_invocations == 0 {
            self.state = CallbackSlotState::Closed;
            Ok(CallbackCloseAction::RemoveRoot)
        } else {
            Ok(CallbackCloseAction::None)
        }
    }

    fn check_live_key(&self, key: CallbackKey) -> Result<(), RuntimeError> {
        if self.generation == key.slot_generation() {
            Ok(())
        } else {
            Err(RuntimeError::new("callback slot generation mismatch"))
        }
    }

    fn closed_for(&self, key: CallbackKey) -> bool {
        self.generation == key.slot_generation() && self.state == CallbackSlotState::Closed
    }
}

impl CallbackCloseResult {
    fn none() -> Self {
        Self {
            closed: false,
            action: CallbackCloseAction::None,
        }
    }

    fn remove_root() -> Self {
        Self {
            closed: true,
            action: CallbackCloseAction::RemoveRoot,
        }
    }

    fn closed() -> Self {
        Self {
            closed: true,
            action: CallbackCloseAction::None,
        }
    }
}

impl CallbackInvocationGuard {
    pub fn finish(mut self) -> Result<CallbackCloseAction, RuntimeError> {
        let action = unsafe { self.slot.as_mut().finish_invocation(self.key) };
        self.owner.__anvyx_decrement_active_invocations();
        self.active = false;
        action
    }
}

impl Drop for CallbackInvocationGuard {
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
        let mut slot = CallbackSlotMeta::default();
        let key = key(slot.open().unwrap());

        assert_eq!(
            slot.close(key),
            CallbackCloseResult {
                closed: true,
                action: CallbackCloseAction::RemoveRoot,
            }
        );
        assert_eq!(slot.close(key), CallbackCloseResult::none());
    }

    #[test]
    fn stale_generation_close_is_false() {
        let mut slot = CallbackSlotMeta::default();
        let stale = key(slot.generation());
        slot.open().unwrap();

        assert_eq!(slot.close(stale), CallbackCloseResult::none());
    }

    #[test]
    fn open_rejects_live_or_closing_slot() {
        let (owner, _runtime) = owner();
        let mut slot = CallbackSlotMeta::default();
        let key = key(slot.open().unwrap());

        assert_eq!(
            slot.open().unwrap_err().message(),
            "callback slot is already open"
        );
        let guard = slot.begin_invocation(&owner, key).unwrap();
        assert_eq!(slot.close(key).action, CallbackCloseAction::None);
        assert_eq!(
            slot.open().unwrap_err().message(),
            "callback slot is already open"
        );
        drop(guard);
    }

    #[test]
    fn begin_invocation_overflow_does_not_increment_owner() {
        let (owner, _runtime) = owner();
        let mut slot = CallbackSlotMeta {
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
        let mut slot = CallbackSlotMeta {
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
    fn active_close_defers_root_removal_until_finish() {
        let (owner, _runtime) = owner();
        let mut slot = CallbackSlotMeta::default();
        let key = key(slot.open().unwrap());
        let guard = slot.begin_invocation(&owner, key).unwrap();

        assert_eq!(
            slot.close(key),
            CallbackCloseResult {
                closed: true,
                action: CallbackCloseAction::None,
            }
        );
        assert_eq!(guard.finish().unwrap(), CallbackCloseAction::RemoveRoot);
        assert_eq!(slot.state(), CallbackSlotState::Closed);
    }

    #[test]
    fn invocation_guard_blocks_shutdown_until_drop() {
        let (owner, _runtime) = owner();
        let mut slot = CallbackSlotMeta::default();
        let key = key(slot.open().unwrap());
        let guard = slot.begin_invocation(&owner, key).unwrap();

        assert_eq!(
            owner.__anvyx_begin_shutdown().unwrap_err().message(),
            "runtime callback invocation is active"
        );
        drop(guard);
        assert!(owner.__anvyx_begin_shutdown().is_ok());
    }

    #[test]
    fn slot_reuse_keeps_generation_moving() {
        let (owner, _runtime) = owner();
        let mut slot = CallbackSlot::default();
        let first_generation = slot.insert("first").unwrap();
        let first_key = key(first_generation);

        let (close, root) = slot.close(first_key);
        assert!(close.closed);
        assert_eq!(root, Some("first"));

        let second_generation = slot.insert("second").unwrap();
        assert_ne!(first_generation, second_generation);
        assert!(slot.begin_invocation(&owner, first_key).is_err());
    }

    #[test]
    fn slot_close_during_invocation_defers_root_removal() {
        let (owner, _runtime) = owner();
        let mut slot = CallbackSlot::default();
        let key = key(slot.insert(7).unwrap());
        let (_, guard) = slot.begin_invocation(&owner, key).unwrap();

        let (close, root) = slot.close(key);
        assert_eq!(close.action, CallbackCloseAction::None);
        assert_eq!(root, None);

        assert_eq!(guard.finish().unwrap(), CallbackCloseAction::RemoveRoot);
        assert_eq!(slot.take_closed_root(key), Some(7));
        assert_eq!(slot.take_closed_root(key), None);
    }
}
