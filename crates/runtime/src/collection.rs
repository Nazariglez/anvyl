use std::{cell::Cell, rc::Rc};

use crate::RuntimeError;

const ERR_ACTIVE_LOAN: &str =
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
    structural_version: Cell<u64>,
}

pub struct ShapeLoanGuard {
    state: CollectionLoanState,
    expected_version: u64,
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
        self.state.inner.active_shape_loans.set(active);
        Self {
            state: self.state.clone(),
            expected_version: self.expected_version,
        }
    }
}

impl CollectionLoanState {
    pub fn begin_shape_loan(&self) -> Result<ShapeLoanGuard, RuntimeError> {
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
            Err(RuntimeError::new(ERR_ACTIVE_LOAN))
        }
    }

    pub fn finish_structural_mutation(&self) {
        let next = self
            .current_version()
            .checked_add(1)
            .expect("collection structural version overflow");
        self.inner.structural_version.set(next);
    }

    pub fn structural_mutation<R>(&self, mutate: impl FnOnce() -> R) -> Result<R, RuntimeError> {
        self.before_structural_mutation()?;
        let result = mutate();
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

impl Drop for ShapeLoanGuard {
    fn drop(&mut self) {
        let active = self.state.inner.active_shape_loans.get();
        debug_assert!(active > 0, "collection loan depth underflow");
        if active > 0 {
            self.state.inner.active_shape_loans.set(active - 1);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::CollectionLoanState;

    #[test]
    fn guard_owns_state_handle() {
        let state = CollectionLoanState::default();
        let guard = state.begin_shape_loan().unwrap();

        assert_eq!(state.active_shape_loans(), 1);
        assert!(state.before_structural_mutation().is_err());
        assert_eq!(guard.version(), 0);

        drop(guard);
        assert_eq!(state.active_shape_loans(), 0);
    }

    #[test]
    fn nested_guards_restore_depth() {
        let state = CollectionLoanState::default();
        let outer = state.begin_shape_loan().unwrap();
        let inner = state.begin_shape_loan().unwrap();

        assert_eq!(state.active_shape_loans(), 2);
        drop(inner);
        assert_eq!(state.active_shape_loans(), 1);
        assert!(state.before_structural_mutation().is_err());
        drop(outer);
        assert_eq!(state.active_shape_loans(), 0);
        assert!(state.before_structural_mutation().is_ok());
    }

    #[test]
    fn cloned_guard_extends_depth() {
        let state = CollectionLoanState::default();
        let guard = state.begin_shape_loan().unwrap();
        let clone = guard.clone();

        assert_eq!(state.active_shape_loans(), 2);
        drop(guard);
        assert_eq!(state.active_shape_loans(), 1);
        assert!(state.before_structural_mutation().is_err());
        drop(clone);
        assert_eq!(state.active_shape_loans(), 0);
    }

    #[test]
    fn structural_mutation_increments_version() {
        let state = CollectionLoanState::default();

        assert_eq!(state.current_version(), 0);
        state.before_structural_mutation().unwrap();
        state.finish_structural_mutation();
        assert_eq!(state.current_version(), 1);
        assert!(state.check_stable(0).is_err());
        assert!(state.check_stable(1).is_ok());
    }

    #[test]
    fn guard_drops_after_error_path() {
        let state = CollectionLoanState::default();
        let err = {
            let _guard = state.begin_shape_loan().unwrap();
            Err::<(), _>("early")
        };

        assert_eq!(err, Err("early"));
        assert_eq!(state.active_shape_loans(), 0);
    }
}
