use std::{cell::Cell, rc::Rc};

use crate::{RuntimeError, SafepointGuard, SafepointGuardKind, SafepointState};

pub(crate) const ACTIVE_COLLECTION_LOAN_ERROR: &str =
    "cannot structurally mutate collection during active iteration or slice view";
const ERR_STALE_VIEW: &str = "collection view is stale after structural mutation";
const ERR_LOAN_OVERFLOW: &str = "too many nested collection loans";

#[derive(Clone, Default)]
pub struct CollectionLoanState {
    inner: Rc<CollectionLoanStateInner>,
}

#[derive(Default)]
struct CollectionLoanStateInner {
    active_shape_loans: Cell<u32>,
    active_value_loan: Cell<Option<u64>>,
    next_value_loan: Cell<u64>,
    structural_version: Cell<u64>,
    safepoint: SafepointState,
}

pub struct ShapeLoanGuard {
    state: CollectionLoanState,
    expected_version: u64,
    _safepoint_guard: SafepointGuard,
}

pub struct ValueLoanGuard {
    state: CollectionLoanState,
    shape: ShapeLoanGuard,
    id: u64,
}

impl Clone for ShapeLoanGuard {
    fn clone(&self) -> Self {
        let active = self
            .state
            .inner
            .active_shape_loans
            .get()
            .checked_add(1)
            .expect(ERR_LOAN_OVERFLOW);
        let safepoint_guard = self
            .state
            .inner
            .safepoint
            .enter(SafepointGuardKind::Collection)
            .expect(ERR_LOAN_OVERFLOW);
        self.state.inner.active_shape_loans.set(active);
        Self {
            state: self.state.clone(),
            expected_version: self.expected_version,
            _safepoint_guard: safepoint_guard,
        }
    }
}

impl CollectionLoanState {
    pub fn with_safepoint(safepoint: SafepointState) -> Self {
        Self {
            inner: Rc::new(CollectionLoanStateInner {
                safepoint,
                ..CollectionLoanStateInner::default()
            }),
        }
    }

    #[must_use]
    pub fn fresh_with_same_safepoint(&self) -> Self {
        Self::with_safepoint(self.inner.safepoint.clone())
    }

    #[must_use]
    pub fn fresh_for_projection(&self) -> Self {
        let state = self.fresh_with_same_safepoint();
        state.inner.structural_version.set(self.current_version());
        state
    }

    pub fn begin_shape_loan(&self) -> Result<ShapeLoanGuard, RuntimeError> {
        let safepoint_guard = self.inner.safepoint.enter(SafepointGuardKind::Collection)?;
        let active = self
            .inner
            .active_shape_loans
            .get()
            .checked_add(1)
            .ok_or_else(|| RuntimeError::new(ERR_LOAN_OVERFLOW))?;
        self.inner.active_shape_loans.set(active);
        Ok(ShapeLoanGuard {
            state: self.clone(),
            expected_version: self.current_version(),
            _safepoint_guard: safepoint_guard,
        })
    }

    pub fn begin_value_loan(&self) -> Result<ValueLoanGuard, RuntimeError> {
        if self.inner.active_value_loan.get().is_some() {
            return Err(RuntimeError::new(ACTIVE_COLLECTION_LOAN_ERROR));
        }
        let id = self.inner.next_value_loan.get();
        let next_id = id
            .checked_add(1)
            .ok_or_else(|| RuntimeError::new(ERR_LOAN_OVERFLOW))?;
        let shape = self.begin_shape_loan()?;
        self.inner.next_value_loan.set(next_id);
        self.inner.active_value_loan.set(Some(id));
        Ok(ValueLoanGuard {
            state: self.clone(),
            shape,
            id,
        })
    }

    pub fn active_shape_loans(&self) -> u32 {
        self.inner.active_shape_loans.get()
    }

    pub fn current_version(&self) -> u64 {
        self.inner.structural_version.get()
    }

    pub fn check_stable(&self, expected: u64) -> Result<(), RuntimeError> {
        if self.current_version() == expected {
            Ok(())
        } else {
            Err(RuntimeError::new(ERR_STALE_VIEW))
        }
    }

    pub fn before_structural_mutation(&self) -> Result<(), RuntimeError> {
        if self.active_shape_loans() == 0 {
            Ok(())
        } else {
            Err(RuntimeError::new(ACTIVE_COLLECTION_LOAN_ERROR))
        }
    }

    pub fn before_unloaned_storage_access(&self) -> Result<(), RuntimeError> {
        if self.inner.active_value_loan.get().is_none() {
            Ok(())
        } else {
            Err(RuntimeError::new(ACTIVE_COLLECTION_LOAN_ERROR))
        }
    }

    pub fn check_value_loan(&self, loan_id: u64) -> Result<(), RuntimeError> {
        if self.inner.active_value_loan.get() == Some(loan_id) {
            Ok(())
        } else {
            Err(RuntimeError::new(ACTIVE_COLLECTION_LOAN_ERROR))
        }
    }

    pub fn finish_structural_mutation(&self) {
        let next = self
            .current_version()
            .checked_add(1)
            .expect("collection structural version overflow");
        self.inner.structural_version.set(next);
    }

    pub fn structural_mutation_result<R>(
        &self,
        mutate: impl FnOnce() -> Result<R, RuntimeError>,
    ) -> Result<R, RuntimeError> {
        self.before_structural_mutation()?;
        let result = mutate()?;
        self.finish_structural_mutation();
        Ok(result)
    }
}

impl ShapeLoanGuard {
    pub fn version(&self) -> u64 {
        self.expected_version
    }

    pub fn check_stable(&self) -> Result<(), RuntimeError> {
        self.state.check_stable(self.expected_version)
    }
}

impl ValueLoanGuard {
    pub fn id(&self) -> u64 {
        self.id
    }

    pub fn version(&self) -> u64 {
        self.shape.version()
    }

    pub fn check_stable(&self) -> Result<(), RuntimeError> {
        self.shape.check_stable()
    }
}

impl Drop for ValueLoanGuard {
    fn drop(&mut self) {
        let active = self.state.inner.active_value_loan.get();
        debug_assert_eq!(active, Some(self.id));
        if active == Some(self.id) {
            self.state.inner.active_value_loan.set(None);
        }
    }
}

impl Drop for ShapeLoanGuard {
    fn drop(&mut self) {
        let active = self.state.inner.active_shape_loans.get();
        debug_assert!(active > 0, "collection loan depth underflow");
        if active > 0 {
            self.state.inner.active_shape_loans.set(active - 1);
        }
    }
}
