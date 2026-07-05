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

#[cfg(test)]
mod tests {
    use std::{cell::Cell, rc::Rc};

    use super::LazyState;
    use crate::{
        GlobalProjectedLoanGuard, GlobalRef, GlobalRefMut, GlobalSlot, GlobalSlotState,
        SafepointState, collection::ACTIVE_COLLECTION_LOAN_ERROR,
    };

    #[test]
    fn new_slot_starts_uninit() {
        let slot = GlobalSlot::<i64>::new("score");

        assert_eq!(slot.state(), GlobalSlotState::Uninit);
        assert_eq!(slot.name, "score");
    }

    #[test]
    fn state_reports_all_internal_states() {
        let slot = GlobalSlot::<i64>::new("score");

        *slot.state.borrow_mut() = LazyState::Initializing;
        assert_eq!(slot.state(), GlobalSlotState::Initializing);

        *slot.state.borrow_mut() = LazyState::Ready(1);
        assert_eq!(slot.state(), GlobalSlotState::Ready);

        *slot.state.borrow_mut() = LazyState::Failed(crate::RuntimeError::new("failed"));
        assert_eq!(slot.state(), GlobalSlotState::Failed);
    }

    #[test]
    fn ensure_initializes_once() {
        let slot = GlobalSlot::<i64>::new("score");
        let mut calls = 0;

        slot.ensure(|| {
            calls += 1;
            Ok(7)
        })
        .unwrap();
        slot.ensure(|| {
            calls += 1;
            Ok(9)
        })
        .unwrap();

        assert_eq!(calls, 1);
        assert_eq!(slot.state(), GlobalSlotState::Ready);
        assert_eq!(*slot.read(|| unreachable!()).unwrap(), 7);
    }

    #[test]
    fn read_initializes_and_returns_ready_value() {
        let slot = GlobalSlot::<i64>::new("score");

        let value = slot.read(|| Ok(7)).unwrap();

        assert_eq!(*value, 7);
        assert_eq!(slot.state(), GlobalSlotState::Ready);
    }

    #[test]
    fn read_registers_safepoint_blocker() {
        let safepoint = SafepointState::default();
        let slot = GlobalSlot::new_with_safepoint("score", safepoint.clone());
        slot.ensure(|| Ok(7)).unwrap();

        let guard = slot.read(|| unreachable!()).unwrap();
        assert_eq!(
            safepoint.validate_collect().unwrap_err().message(),
            "cannot collect while global guard is active"
        );

        drop(guard);
        assert!(safepoint.validate_collect().is_ok());
    }

    #[test]
    fn failed_initializer_is_poisoned() {
        let slot = GlobalSlot::<i64>::new("score");
        let first = slot
            .ensure(|| Err(crate::RuntimeError::new("boom")))
            .unwrap_err();
        let second = slot.ensure(|| Ok(7)).unwrap_err();

        assert_eq!(first.message(), "poisoned lazy global 'score': boom");
        assert_eq!(second.message(), "poisoned lazy global 'score': boom");
        assert_eq!(slot.state(), GlobalSlotState::Failed);
    }

    #[test]
    fn initializing_cycle_returns_error_instead_of_borrow_panic() {
        let slot = GlobalSlot::<i64>::new("score");
        let error = slot
            .ensure(|| {
                let cycle = slot.ensure(|| Ok(1)).unwrap_err();
                Err(cycle)
            })
            .unwrap_err();

        assert!(error.message().contains("score"));
        assert!(error.message().contains("initializing"));
        assert_eq!(slot.state(), GlobalSlotState::Failed);
    }

    #[test]
    fn write_initializes_and_mutates_ready_value() {
        let slot = GlobalSlot::<i64>::new("score");

        *slot.write(|| Ok(7)).unwrap() = 9;

        assert_eq!(*slot.read(|| unreachable!()).unwrap(), 9);
    }

    #[test]
    fn set_without_init_replaces_uninit_ready_and_failed() {
        let slot = GlobalSlot::<i64>::new("score");

        slot.set_without_init(1).unwrap();
        assert_eq!(*slot.read(|| unreachable!()).unwrap(), 1);

        slot.set_without_init(2).unwrap();
        assert_eq!(*slot.read(|| unreachable!()).unwrap(), 2);

        *slot.state.borrow_mut() = LazyState::Failed(crate::RuntimeError::new("boom"));
        slot.set_without_init(3).unwrap();
        assert_eq!(*slot.read(|| unreachable!()).unwrap(), 3);
    }

    #[test]
    fn set_without_init_or_replace_uses_ready_replacer() {
        let slot = GlobalSlot::<i64>::new("score");
        slot.set_without_init(1).unwrap();

        slot.set_without_init_or_replace(2, |slot, value| {
            *slot += value;
            Ok(())
        })
        .unwrap();

        assert_eq!(*slot.read(|| unreachable!()).unwrap(), 3);
    }

    #[test]
    fn set_without_init_or_replace_sets_uninit_without_replacer() {
        let slot = GlobalSlot::<i64>::new("score");

        slot.set_without_init_or_replace(2, |_, _| unreachable!())
            .unwrap();

        assert_eq!(*slot.read(|| unreachable!()).unwrap(), 2);
    }

    #[test]
    fn set_without_init_rejects_initializing() {
        let slot = GlobalSlot::<i64>::new("score");
        let error = slot
            .ensure(|| {
                let error = slot.set_without_init(1).unwrap_err();
                Err(error)
            })
            .unwrap_err();

        assert!(error.message().contains("score"));
        assert!(error.message().contains("initializing"));
    }

    #[test]
    fn multiple_shared_reads_can_coexist() {
        let slot = GlobalSlot::<i64>::new("score");

        let first = slot.read(|| Ok(7)).unwrap();
        let second = slot.read(|| unreachable!()).unwrap();

        assert_eq!(*first, 7);
        assert_eq!(*second, 7);
    }

    #[test]
    fn active_read_blocks_write_and_root_replacement() {
        let slot = GlobalSlot::<i64>::new("score");
        let read = slot.read(|| Ok(7)).unwrap();

        let write = slot.write(|| unreachable!()).err().unwrap();
        let set = slot.set_without_init(9).unwrap_err();

        assert_eq!(*read, 7);
        assert!(write.message().contains("active borrow"));
        assert!(set.message().contains("active borrow"));
    }

    #[test]
    fn active_write_blocks_reads_writes_and_root_replacement() {
        let slot = GlobalSlot::<i64>::new("score");
        let mut write = slot.write(|| Ok(7)).unwrap();
        *write = 8;

        let read_error = slot.read(|| unreachable!()).err().unwrap();
        let write_error = slot.write(|| unreachable!()).err().unwrap();
        let set_error = slot.set_without_init(9).unwrap_err();

        assert_eq!(*write, 8);
        assert_eq!(slot.state(), GlobalSlotState::Ready);
        assert!(read_error.message().contains("active borrow"));
        assert!(write_error.message().contains("active borrow"));
        assert!(set_error.message().contains("active borrow"));
    }

    #[test]
    fn replacement_drops_ready_payload_once() {
        #[derive(Debug)]
        struct CountDrop(Rc<Cell<usize>>);

        impl Drop for CountDrop {
            fn drop(&mut self) {
                self.0.set(self.0.get() + 1);
            }
        }

        let drops = Rc::new(Cell::new(0));
        let slot = GlobalSlot::new("score");
        slot.set_without_init(CountDrop(drops.clone())).unwrap();
        assert_eq!(drops.get(), 0);

        slot.set_without_init(CountDrop(drops.clone())).unwrap();
        assert_eq!(drops.get(), 1);

        *slot.state.borrow_mut() = LazyState::Failed(crate::RuntimeError::new("boom"));
        assert_eq!(drops.get(), 2);

        slot.set_without_init(CountDrop(drops.clone())).unwrap();
        assert_eq!(drops.get(), 2);

        drop(slot);
        assert_eq!(drops.get(), 3);
    }

    #[test]
    fn validate_trace_rejects_active_borrow() {
        let slot = GlobalSlot::<i64>::new("score");
        let read = slot.read(|| Ok(7)).unwrap();

        let error = slot.validate_trace().unwrap_err();

        assert_eq!(*read, 7);
        assert!(error.message().contains("active borrow"));
        assert!(error.message().contains("score"));
    }

    #[test]
    fn validate_trace_rejects_projected_loan() {
        let slot = GlobalSlot::<i64>::new("score");
        slot.set_without_init(7).unwrap();
        let loan = slot.begin_projected_loan().unwrap();

        let error = slot.validate_trace().unwrap_err();

        assert!(error.message().contains("active projected loan"));
        assert!(error.message().contains("score"));
        drop(loan);
        slot.validate_trace().unwrap();
    }

    #[test]
    fn projected_loan_blocks_writes_and_replacement() {
        let slot = GlobalSlot::<i64>::new("state");
        slot.set_without_init(1).unwrap();
        let loan = slot.begin_projected_loan().unwrap();

        let write = slot.write(|| unreachable!()).err().unwrap();
        let set = slot.set_without_init(2).unwrap_err();

        assert!(write.message().contains(ACTIVE_COLLECTION_LOAN_ERROR));
        assert!(set.message().contains(ACTIVE_COLLECTION_LOAN_ERROR));
        drop(loan);
        slot.set_without_init(3).unwrap();
        assert_eq!(*slot.read(|| unreachable!()).unwrap(), 3);
    }

    #[test]
    fn exports_are_visible() {
        fn assert_exported<T>(_: T) {}

        assert_exported::<GlobalSlot<i64>>(GlobalSlot::new("score"));
        assert_eq!(GlobalSlotState::Uninit, GlobalSlotState::Uninit);
        let _: Option<GlobalRef<'_, i64>> = None;
        let _: Option<GlobalRefMut<'_, i64>> = None;
        let _: Option<GlobalProjectedLoanGuard<'_, i64>> = None;
    }
}
