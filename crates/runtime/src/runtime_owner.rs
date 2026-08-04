use std::{
    cell::Cell,
    num::NonZeroU64,
    ptr::NonNull,
    rc::Rc,
    sync::atomic::{AtomicU64, Ordering},
    thread::{self, ThreadId},
};

use crate::RuntimeError;

static NEXT_OWNER_ID: AtomicU64 = AtomicU64::new(1);

#[derive(Debug, Clone)]
pub struct RuntimeOwnerHandle {
    token: Rc<RuntimeOwnerToken>,
    core: Rc<RuntimeOwnerCore>,
}

#[derive(Debug)]
pub struct RuntimeOwnerToken {
    owner_id: NonZeroU64,
    owner_thread: ThreadId,
    shutdown_generation: Cell<NonZeroU64>,
    owner_ptr: Cell<Option<NonNull<()>>>,
    attached_once: Cell<bool>,
}

#[derive(Debug)]
pub struct RuntimeOwnerCore {
    entry_active: Cell<bool>,
    shutdown_in_progress: Cell<bool>,
    active_invocations_total: Cell<usize>,
}

#[derive(Debug)]
pub struct RuntimeOwnerEntry {
    owner_ptr: NonNull<()>,
    core: Rc<RuntimeOwnerCore>,
}

#[derive(Debug)]
pub struct RuntimeOwnerShutdownGuard {
    handle: RuntimeOwnerHandle,
}

#[derive(Debug)]
pub struct RuntimeOwnerProviderGuard {
    core: Rc<RuntimeOwnerCore>,
}

impl RuntimeOwnerHandle {
    pub fn new() -> Result<Self, RuntimeError> {
        let owner_id = next_owner_id()?;
        Ok(Self {
            token: Rc::new(RuntimeOwnerToken {
                owner_id,
                owner_thread: thread::current().id(),
                shutdown_generation: Cell::new(nonzero(1)),
                owner_ptr: Cell::new(None),
                attached_once: Cell::new(false),
            }),
            core: Rc::new(RuntimeOwnerCore {
                entry_active: Cell::new(false),
                shutdown_in_progress: Cell::new(false),
                active_invocations_total: Cell::new(0),
            }),
        })
    }

    pub fn owner_id(&self) -> NonZeroU64 {
        self.token.owner_id
    }

    pub fn shutdown_generation(&self) -> NonZeroU64 {
        self.token.shutdown_generation.get()
    }

    #[doc(hidden)]
    pub unsafe fn __anvyx_attach_owner_ptr(
        &self,
        owner_ptr: NonNull<()>,
    ) -> Result<(), RuntimeError> {
        self.check_thread()?;
        if self.core.shutdown_in_progress.get() {
            return Err(RuntimeError::new("runtime owner shutdown is in progress"));
        }
        if self.core.entry_active.get() {
            return Err(RuntimeError::new("runtime owner entry is active"));
        }
        if self.token.owner_ptr.get().is_some() {
            return Err(RuntimeError::new(
                "runtime owner pointer is already attached",
            ));
        }
        if self.token.attached_once.get() {
            return Err(RuntimeError::new(
                "runtime owner pointer cannot be reattached",
            ));
        }
        self.token.owner_ptr.set(Some(owner_ptr));
        self.token.attached_once.set(true);
        Ok(())
    }

    #[doc(hidden)]
    pub fn __anvyx_detach_owner_ptr(&self) -> Result<(), RuntimeError> {
        self.check_thread()?;
        if self.core.entry_active.get() {
            return Err(RuntimeError::new("runtime owner entry is active"));
        }
        if self.core.active_invocations_total.get() != 0 {
            return Err(RuntimeError::new("runtime callback invocation is active"));
        }
        self.token.owner_ptr.set(None);
        Ok(())
    }

    #[doc(hidden)]
    pub fn __anvyx_enter_current(&self) -> Result<RuntimeOwnerEntry, RuntimeError> {
        self.__anvyx_enter(self.owner_id(), self.shutdown_generation())
    }

    #[doc(hidden)]
    pub fn __anvyx_enter(
        &self,
        owner_id: NonZeroU64,
        shutdown_generation: NonZeroU64,
    ) -> Result<RuntimeOwnerEntry, RuntimeError> {
        self.check_thread()?;
        if owner_id != self.token.owner_id {
            return Err(RuntimeError::new("runtime owner mismatch"));
        }
        if self.core.shutdown_in_progress.get() {
            return Err(RuntimeError::new("runtime owner shutdown is in progress"));
        }
        if self.core.entry_active.get() {
            return Err(RuntimeError::new("runtime owner entry is active"));
        }

        if shutdown_generation != self.token.shutdown_generation.get() {
            return Err(RuntimeError::new(
                "runtime owner shutdown generation mismatch",
            ));
        }
        let Some(owner_ptr) = self.token.owner_ptr.get() else {
            return Err(RuntimeError::new("runtime owner pointer is detached"));
        };

        self.core.entry_active.set(true);
        Ok(RuntimeOwnerEntry {
            owner_ptr,
            core: Rc::clone(&self.core),
        })
    }

    #[doc(hidden)]
    pub fn __anvyx_suspend_entry_for_provider(
        &self,
    ) -> Result<RuntimeOwnerProviderGuard, RuntimeError> {
        self.check_thread()?;
        if !self.core.entry_active.get() {
            return Err(RuntimeError::new("runtime owner entry is not active"));
        }
        self.core.entry_active.set(false);
        Ok(RuntimeOwnerProviderGuard {
            core: Rc::clone(&self.core),
        })
    }

    #[doc(hidden)]
    pub fn __anvyx_begin_shutdown(&self) -> Result<RuntimeOwnerShutdownGuard, RuntimeError> {
        self.check_thread()?;
        if self.core.entry_active.get() {
            return Err(RuntimeError::new("runtime owner entry is active"));
        }
        if self.core.active_invocations_total.get() != 0 {
            return Err(RuntimeError::new("runtime callback invocation is active"));
        }
        if self.core.shutdown_in_progress.get() {
            return Err(RuntimeError::new(
                "runtime owner shutdown is already in progress",
            ));
        }
        if self.token.owner_ptr.get().is_none() {
            return Err(RuntimeError::new("runtime owner pointer is detached"));
        }

        let next_generation = next_generation(self.token.shutdown_generation.get())?;
        self.core.shutdown_in_progress.set(true);
        self.token.shutdown_generation.set(next_generation);
        self.token.owner_ptr.set(None);
        Ok(RuntimeOwnerShutdownGuard {
            handle: self.clone(),
        })
    }

    #[doc(hidden)]
    pub fn __anvyx_increment_active_invocations(&self) -> Result<(), RuntimeError> {
        let next = self
            .core
            .active_invocations_total
            .get()
            .checked_add(1)
            .ok_or_else(|| RuntimeError::new("runtime callback invocation count overflow"))?;
        self.core.active_invocations_total.set(next);
        Ok(())
    }

    #[doc(hidden)]
    pub fn __anvyx_decrement_active_invocations(&self) {
        let current = self.core.active_invocations_total.get();
        debug_assert!(current > 0);
        self.core
            .active_invocations_total
            .set(current.saturating_sub(1));
    }

    fn check_thread(&self) -> Result<(), RuntimeError> {
        if thread::current().id() == self.token.owner_thread {
            Ok(())
        } else {
            Err(RuntimeError::new(
                "runtime owner used from the wrong thread",
            ))
        }
    }
}

impl RuntimeOwnerEntry {
    pub fn owner_ptr(&self) -> NonNull<()> {
        self.owner_ptr
    }
}

impl Drop for RuntimeOwnerEntry {
    fn drop(&mut self) {
        debug_assert!(self.core.entry_active.get());
        self.core.entry_active.set(false);
    }
}

impl Drop for RuntimeOwnerShutdownGuard {
    fn drop(&mut self) {
        debug_assert!(self.handle.core.shutdown_in_progress.get());
        self.handle.core.shutdown_in_progress.set(false);
    }
}

impl Drop for RuntimeOwnerProviderGuard {
    fn drop(&mut self) {
        debug_assert!(!self.core.entry_active.get());
        self.core.entry_active.set(true);
    }
}

fn next_owner_id() -> Result<NonZeroU64, RuntimeError> {
    let id = NEXT_OWNER_ID
        .fetch_update(Ordering::Relaxed, Ordering::Relaxed, |id| id.checked_add(1))
        .map_err(|_| RuntimeError::new("runtime owner id overflow"))?;
    NonZeroU64::new(id).ok_or_else(|| RuntimeError::new("runtime owner id overflow"))
}

fn next_generation(generation: NonZeroU64) -> Result<NonZeroU64, RuntimeError> {
    NonZeroU64::new(
        generation
            .get()
            .checked_add(1)
            .ok_or_else(|| RuntimeError::new("runtime owner shutdown generation overflow"))?,
    )
    .ok_or_else(|| RuntimeError::new("runtime owner shutdown generation overflow"))
}

fn nonzero(value: u64) -> NonZeroU64 {
    NonZeroU64::new(value).expect("value must be non-zero")
}

#[cfg(test)]
mod tests {
    use super::*;

    fn attached_owner() -> (RuntimeOwnerHandle, Box<i64>) {
        let owner = RuntimeOwnerHandle::new().unwrap();
        let mut runtime = Box::new(1_i64);
        unsafe {
            owner
                .__anvyx_attach_owner_ptr(NonNull::from(runtime.as_mut()).cast())
                .unwrap();
        };
        (owner, runtime)
    }

    fn cast_after_entry(owner: &RuntimeOwnerHandle) -> Result<i64, RuntimeError> {
        let entry = owner.__anvyx_enter_current()?;
        let runtime = unsafe { entry.owner_ptr().cast::<i64>().as_ref() };
        Ok(*runtime)
    }

    #[test]
    fn active_entry_blocks_reentry_before_cast() {
        let (owner, _runtime) = attached_owner();
        let _entry = owner.__anvyx_enter_current().unwrap();

        assert_eq!(
            cast_after_entry(&owner).unwrap_err().message(),
            "runtime owner entry is active"
        );
    }

    #[test]
    fn owner_pointer_cannot_be_reattached_after_detach_or_shutdown() {
        let (owner, _runtime) = attached_owner();
        owner.__anvyx_detach_owner_ptr().unwrap();
        let mut replacement = 2_i64;

        assert_eq!(
            unsafe { owner.__anvyx_attach_owner_ptr(NonNull::from(&mut replacement).cast()) }
                .unwrap_err()
                .message(),
            "runtime owner pointer cannot be reattached"
        );

        let (owner, _runtime) = attached_owner();
        let shutdown = owner.__anvyx_begin_shutdown().unwrap();
        drop(shutdown);
        assert_eq!(
            unsafe { owner.__anvyx_attach_owner_ptr(NonNull::from(&mut replacement).cast()) }
                .unwrap_err()
                .message(),
            "runtime owner pointer cannot be reattached"
        );
    }

    #[test]
    fn shutdown_rejects_active_entry_and_invocation() {
        let (owner, _runtime) = attached_owner();
        let entry = owner.__anvyx_enter_current().unwrap();
        assert_eq!(
            owner.__anvyx_begin_shutdown().unwrap_err().message(),
            "runtime owner entry is active"
        );
        drop(entry);

        owner.__anvyx_increment_active_invocations().unwrap();
        assert_eq!(
            owner.__anvyx_begin_shutdown().unwrap_err().message(),
            "runtime callback invocation is active"
        );
        owner.__anvyx_decrement_active_invocations();
        assert!(owner.__anvyx_begin_shutdown().is_ok());
    }
}
