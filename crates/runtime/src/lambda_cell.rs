use std::{
    cell::{Cell, UnsafeCell},
    marker::PhantomData,
    rc::Rc,
};

use crate::RuntimeError;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum CellBorrowState {
    Unborrowed,
    Shared(usize),
    Mutable,
}

pub(crate) struct CellBorrowFlag {
    state: Cell<CellBorrowState>,
}

impl Default for CellBorrowFlag {
    fn default() -> Self {
        Self {
            state: Cell::new(CellBorrowState::Unborrowed),
        }
    }
}

impl CellBorrowFlag {
    pub(crate) fn shared_guard(&self) -> Result<SharedCellGuard<'_>, RuntimeError> {
        match self.state.get() {
            CellBorrowState::Unborrowed => self.state.set(CellBorrowState::Shared(1)),
            CellBorrowState::Shared(count) => self.state.set(CellBorrowState::Shared(count + 1)),
            CellBorrowState::Mutable => return Err(cell_borrow_error()),
        }
        Ok(SharedCellGuard { flag: self })
    }

    pub(crate) fn mutable_guard(&self) -> Result<MutableCellGuard<'_>, RuntimeError> {
        match self.state.get() {
            CellBorrowState::Unborrowed => self.state.set(CellBorrowState::Mutable),
            CellBorrowState::Shared(_) | CellBorrowState::Mutable => {
                return Err(cell_borrow_error());
            }
        }
        Ok(MutableCellGuard { flag: self })
    }
}

pub struct StackLambdaCell<T> {
    value: UnsafeCell<T>,
    borrow: CellBorrowFlag,
    _not_send_sync: PhantomData<Rc<()>>,
}

impl<T> StackLambdaCell<T> {
    pub fn new(value: T) -> Self {
        Self {
            value: UnsafeCell::new(value),
            borrow: CellBorrowFlag::default(),
            _not_send_sync: PhantomData,
        }
    }

    pub fn access<R>(
        &self,
        f: impl FnOnce(&T) -> Result<R, RuntimeError>,
    ) -> Result<R, RuntimeError> {
        let _guard = self.borrow.shared_guard()?;
        f(unsafe { &*self.value.get() })
    }

    pub fn mutate<R>(
        &self,
        f: impl FnOnce(&mut T) -> Result<R, RuntimeError>,
    ) -> Result<R, RuntimeError> {
        let _guard = self.borrow.mutable_guard()?;
        f(unsafe { &mut *self.value.get() })
    }

    pub fn replace(&self, value: T) -> Result<T, RuntimeError> {
        self.mutate(|slot| Ok(std::mem::replace(slot, value)))
    }

    pub fn set(&self, value: T) -> Result<(), RuntimeError> {
        self.mutate(|slot| {
            *slot = value;
            Ok(())
        })
    }
}

impl<T: Copy> StackLambdaCell<T> {
    pub fn get_copy(&self) -> Result<T, RuntimeError> {
        self.access(|value| Ok(*value))
    }
}

fn cell_borrow_error() -> RuntimeError {
    RuntimeError::new("conflicting mutable cell access")
}

pub(crate) struct SharedCellGuard<'a> {
    flag: &'a CellBorrowFlag,
}

impl Drop for SharedCellGuard<'_> {
    fn drop(&mut self) {
        match self.flag.state.get() {
            CellBorrowState::Shared(1) => self.flag.state.set(CellBorrowState::Unborrowed),
            CellBorrowState::Shared(count) => {
                self.flag.state.set(CellBorrowState::Shared(count - 1));
            }
            CellBorrowState::Unborrowed | CellBorrowState::Mutable => {
                debug_assert!(false, "invalid shared cell borrow state");
            }
        }
    }
}

pub(crate) struct MutableCellGuard<'a> {
    flag: &'a CellBorrowFlag,
}

impl Drop for MutableCellGuard<'_> {
    fn drop(&mut self) {
        debug_assert_eq!(self.flag.state.get(), CellBorrowState::Mutable);
        self.flag.state.set(CellBorrowState::Unborrowed);
    }
}

#[cfg(test)]
mod tests {
    use std::{cell::Cell, rc::Rc};

    use super::StackLambdaCell;
    use crate::RuntimeError;

    struct CountDrop(Rc<Cell<usize>>);

    impl Drop for CountDrop {
        fn drop(&mut self) {
            self.0.set(self.0.get() + 1);
        }
    }

    #[test]
    fn scalar_access_mutate() {
        let cell = StackLambdaCell::new(1);

        assert_eq!(cell.get_copy().unwrap(), 1);
        cell.mutate(|value| {
            *value += 1;
            Ok(())
        })
        .unwrap();

        assert_eq!(cell.get_copy().unwrap(), 2);
    }

    #[test]
    fn nested_shared_access_succeeds() {
        let cell = StackLambdaCell::new(1);

        let sum = cell
            .access(|outer| cell.access(|inner| Ok(*outer + *inner)))
            .unwrap();

        assert_eq!(sum, 2);
    }

    #[test]
    fn nested_mutable_access_fails_safely() {
        let cell = StackLambdaCell::new(1);

        let err = cell
            .mutate(|_| {
                cell.mutate(|value| {
                    *value += 1;
                    Ok(())
                })
            })
            .unwrap_err();

        assert_eq!(err.message(), "conflicting mutable cell access");
        cell.set(3).unwrap();
        assert_eq!(cell.get_copy().unwrap(), 3);
    }

    #[test]
    fn shared_access_during_mutable_access_fails_safely() {
        let cell = StackLambdaCell::new(1);

        let err = cell
            .mutate(|_| cell.access(|value| Ok(*value)))
            .unwrap_err();

        assert_eq!(err.message(), "conflicting mutable cell access");
        assert_eq!(cell.get_copy().unwrap(), 1);
    }

    #[test]
    fn mutable_access_during_shared_access_fails_safely() {
        let cell = StackLambdaCell::new(1);

        let err = cell
            .access(|_| {
                cell.mutate(|value| {
                    *value += 1;
                    Ok(())
                })
            })
            .unwrap_err();

        assert_eq!(err.message(), "conflicting mutable cell access");
        assert_eq!(cell.get_copy().unwrap(), 1);
    }

    #[test]
    fn replace_drops_old_value_once() {
        let drops = Rc::new(Cell::new(0));
        let cell = StackLambdaCell::new(CountDrop(Rc::clone(&drops)));

        let old = cell.replace(CountDrop(Rc::clone(&drops))).unwrap();
        assert_eq!(drops.get(), 0);

        drop(old);
        assert_eq!(drops.get(), 1);
        drop(cell);
        assert_eq!(drops.get(), 2);
    }

    #[test]
    fn set_drops_old_value_once() {
        let drops = Rc::new(Cell::new(0));
        let cell = StackLambdaCell::new(CountDrop(Rc::clone(&drops)));

        cell.set(CountDrop(Rc::clone(&drops))).unwrap();
        assert_eq!(drops.get(), 1);

        drop(cell);
        assert_eq!(drops.get(), 2);
    }

    #[test]
    fn guard_state_restored_after_result_error() {
        let cell = StackLambdaCell::new(1);
        let err = cell
            .mutate(|_| Err::<(), _>(RuntimeError::new("early")))
            .unwrap_err();

        assert_eq!(err.message(), "early");
        cell.set(2).unwrap();
        assert_eq!(cell.get_copy().unwrap(), 2);
    }
}
