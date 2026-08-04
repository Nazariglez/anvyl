use std::{
    cell::{Cell, Ref, RefCell, RefMut},
    marker::PhantomData,
    ops::{Deref, DerefMut},
    rc::Rc,
};

use crate::{
    RuntimeError, SafepointGuard, SafepointGuardKind, SafepointState, Trace, TraceDriver, Visitor,
    collection::ACTIVE_COLLECTION_LOAN_ERROR,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GlobalSlotState {
    Uninit,
    Initializing,
    Ready,
    Failed,
}

pub struct GlobalRef<'a, T> {
    value: Ref<'a, T>,
    _safepoint_guard: SafepointGuard,
}

impl<T> Deref for GlobalRef<'_, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

pub struct GlobalRefMut<'a, T> {
    value: RefMut<'a, T>,
    _safepoint_guard: SafepointGuard,
}

pub struct GlobalProjectedLoanGuard<'a, T> {
    slot: &'a GlobalSlot<T>,
    _safepoint_guard: SafepointGuard,
}

impl<T> Drop for GlobalProjectedLoanGuard<'_, T> {
    fn drop(&mut self) {
        let active = self.slot.projected_loans.get();
        debug_assert!(active > 0);
        self.slot.projected_loans.set(active.saturating_sub(1));
    }
}

impl<T> Deref for GlobalRefMut<'_, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<T> DerefMut for GlobalRefMut<'_, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.value
    }
}

enum LazyState<T> {
    Uninit,
    Initializing,
    Ready(T),
    Failed(RuntimeError),
}

pub struct GlobalSlot<T> {
    name: &'static str,
    state: RefCell<LazyState<T>>,
    projected_loans: Cell<usize>,
    safepoint: SafepointState,
    _not_send_sync: PhantomData<Rc<()>>,
}

impl<T> std::fmt::Debug for GlobalSlot<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("GlobalSlot")
            .field("name", &self.name)
            .field("state", &self.state())
            .finish_non_exhaustive()
    }
}

// SAFETY: a ready slot owns exactly one payload and delegates tracing to it once. Non-ready
// states own no initialized payload. The runtime must only trace globals at safepoints without
// active global borrows or projected loans.
unsafe impl<'cx, T: Trace<'cx>> Trace<'cx> for GlobalSlot<T> {
    fn trace<D: TraceDriver<'cx>>(&self, visitor: &mut Visitor<'cx, '_, D>) {
        self.validate_trace()
            .expect("global slot traced outside a safepoint");
        let state = self.state.borrow();
        if let LazyState::Ready(value) = &*state {
            value.trace(visitor);
        }
    }
}

impl<T> GlobalSlot<T> {
    pub fn new(name: &'static str) -> Self {
        Self::new_with_safepoint(name, SafepointState::default())
    }

    pub fn new_with_safepoint(name: &'static str, safepoint: SafepointState) -> Self {
        Self {
            name,
            state: RefCell::new(LazyState::Uninit),
            projected_loans: Cell::new(0),
            safepoint,
            _not_send_sync: PhantomData,
        }
    }

    pub fn state(&self) -> GlobalSlotState {
        let Ok(state) = self.state.try_borrow() else {
            return GlobalSlotState::Ready;
        };
        match &*state {
            LazyState::Uninit => GlobalSlotState::Uninit,
            LazyState::Initializing => GlobalSlotState::Initializing,
            LazyState::Ready(_) => GlobalSlotState::Ready,
            LazyState::Failed(_) => GlobalSlotState::Failed,
        }
    }

    pub fn validate_trace(&self) -> Result<(), RuntimeError> {
        if self.projected_loans.get() != 0 {
            return Err(RuntimeError::new(format!(
                "cannot collect while global '{}' has an active projected loan",
                self.name
            )));
        }
        if self.state.try_borrow_mut().is_err() {
            return Err(RuntimeError::new(format!(
                "cannot collect while global '{}' has an active borrow",
                self.name
            )));
        }
        Ok(())
    }

    pub fn ensure(
        &self,
        init: impl FnOnce() -> Result<T, RuntimeError>,
    ) -> Result<(), RuntimeError> {
        {
            let state = self
                .state
                .try_borrow()
                .map_err(|_| self.borrow_conflict())?;
            match &*state {
                LazyState::Ready(_) => return Ok(()),
                LazyState::Failed(error) => return Err(error.clone()),
                LazyState::Initializing => return Err(self.cycle_error()),
                LazyState::Uninit => {}
            }
        }

        let mut state = self
            .state
            .try_borrow_mut()
            .map_err(|_| self.borrow_conflict())?;
        match &*state {
            LazyState::Ready(_) => return Ok(()),
            LazyState::Failed(error) => return Err(error.clone()),
            LazyState::Initializing => return Err(self.cycle_error()),
            LazyState::Uninit => *state = LazyState::Initializing,
        }
        drop(state);

        let result = init();
        let mut state = self
            .state
            .try_borrow_mut()
            .map_err(|_| self.borrow_conflict())?;
        match result {
            Ok(value) => {
                *state = LazyState::Ready(value);
                Ok(())
            }
            Err(error) => {
                let error = self.poisoned_error(&error);
                *state = LazyState::Failed(error.clone());
                Err(error)
            }
        }
    }

    pub fn read(
        &self,
        init: impl FnOnce() -> Result<T, RuntimeError>,
    ) -> Result<GlobalRef<'_, T>, RuntimeError> {
        self.ensure(init)?;
        let state = self
            .state
            .try_borrow()
            .map_err(|_| self.borrow_conflict())?;
        let guard = self.safepoint.enter(SafepointGuardKind::Global)?;
        Ref::filter_map(state, |state| match state {
            LazyState::Ready(value) => Some(value),
            LazyState::Uninit | LazyState::Initializing | LazyState::Failed(_) => None,
        })
        .map(|value| GlobalRef {
            value,
            _safepoint_guard: guard,
        })
        .map_err(|_| self.internal_error("successful global read found no ready value"))
    }

    pub fn write(
        &self,
        init: impl FnOnce() -> Result<T, RuntimeError>,
    ) -> Result<GlobalRefMut<'_, T>, RuntimeError> {
        self.ensure(init)?;
        self.check_projected_loan_assignment()?;
        let state = self
            .state
            .try_borrow_mut()
            .map_err(|_| self.borrow_conflict())?;
        let guard = self.safepoint.enter(SafepointGuardKind::Global)?;
        RefMut::filter_map(state, |state| match state {
            LazyState::Ready(value) => Some(value),
            LazyState::Uninit | LazyState::Initializing | LazyState::Failed(_) => None,
        })
        .map(|value| GlobalRefMut {
            value,
            _safepoint_guard: guard,
        })
        .map_err(|_| self.internal_error("successful global write found no ready value"))
    }

    pub fn begin_projected_loan(&self) -> Result<GlobalProjectedLoanGuard<'_, T>, RuntimeError> {
        let active = self
            .projected_loans
            .get()
            .checked_add(1)
            .ok_or_else(|| RuntimeError::new("too many active global projected loans"))?;
        let guard = self.safepoint.enter(SafepointGuardKind::Global)?;
        self.projected_loans.set(active);
        Ok(GlobalProjectedLoanGuard {
            slot: self,
            _safepoint_guard: guard,
        })
    }

    pub fn set_without_init(&self, value: T) -> Result<(), RuntimeError> {
        self.set_without_init_or_replace(value, |slot, value| {
            *slot = value;
            Ok(())
        })
    }

    pub fn set_without_init_or_replace(
        &self,
        value: T,
        replace: impl FnOnce(&mut T, T) -> Result<(), RuntimeError>,
    ) -> Result<(), RuntimeError> {
        self.check_projected_loan_assignment()?;
        let mut state = self
            .state
            .try_borrow_mut()
            .map_err(|_| self.borrow_conflict())?;
        match &mut *state {
            LazyState::Initializing => Err(self.initializing_assignment_error()),
            LazyState::Ready(slot) => replace(slot, value),
            LazyState::Uninit | LazyState::Failed(_) => {
                *state = LazyState::Ready(value);
                Ok(())
            }
        }
    }

    fn cycle_error(&self) -> RuntimeError {
        RuntimeError::new(format!(
            "lazy global '{}' is already initializing",
            self.name
        ))
    }

    fn borrow_conflict(&self) -> RuntimeError {
        RuntimeError::new(format!("lazy global '{}' has an active borrow", self.name))
    }

    fn poisoned_error(&self, error: &RuntimeError) -> RuntimeError {
        RuntimeError::new(format!("poisoned lazy global '{}': {error}", self.name))
    }

    fn check_projected_loan_assignment(&self) -> Result<(), RuntimeError> {
        if self.projected_loans.get() == 0 {
            Ok(())
        } else {
            Err(self.projected_loan_assignment_error())
        }
    }

    fn initializing_assignment_error(&self) -> RuntimeError {
        RuntimeError::new(format!(
            "cannot assign lazy global '{}' while it is initializing",
            self.name
        ))
    }

    fn projected_loan_assignment_error(&self) -> RuntimeError {
        RuntimeError::new(format!(
            "cannot assign lazy global '{}' because {}",
            self.name, ACTIVE_COLLECTION_LOAN_ERROR
        ))
    }

    fn internal_error(&self, message: &'static str) -> RuntimeError {
        RuntimeError::new(format!(
            "internal lazy global '{}' error: {message}",
            self.name
        ))
    }
}
