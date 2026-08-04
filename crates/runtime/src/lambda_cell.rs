use std::{
    cell::{Cell, UnsafeCell},
    marker::PhantomData,
    rc::Rc,
};

use anvyx_heap::{Trace, TraceDriver, Visitor};

use crate::{RuntimeError, SafepointGuardKind, SafepointState};

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
            CellBorrowState::Shared(count) => {
                self.state
                    .set(CellBorrowState::Shared(count.checked_add(1).ok_or_else(
                        || RuntimeError::new("too many shared mutable cell borrows"),
                    )?));
            }
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

    fn is_unborrowed(&self) -> bool {
        self.state.get() == CellBorrowState::Unborrowed
    }
}

struct LambdaCellCore<T> {
    value: UnsafeCell<T>,
    borrow: CellBorrowFlag,
    safepoint: SafepointState,
    _not_send_sync: PhantomData<Rc<()>>,
}

impl<T> LambdaCellCore<T> {
    fn new(value: T) -> Self {
        Self::new_with_safepoint(value, SafepointState::default())
    }

    fn new_with_safepoint(value: T, safepoint: SafepointState) -> Self {
        Self {
            value: UnsafeCell::new(value),
            borrow: CellBorrowFlag::default(),
            safepoint,
            _not_send_sync: PhantomData,
        }
    }

    fn access<R>(&self, f: impl FnOnce(&T) -> Result<R, RuntimeError>) -> Result<R, RuntimeError> {
        let _safepoint = self.safepoint.enter(SafepointGuardKind::LambdaCell)?;
        let _guard = self.borrow.shared_guard()?;
        f(unsafe { &*self.value.get() })
    }

    fn mutate<R>(
        &self,
        f: impl FnOnce(&mut T) -> Result<R, RuntimeError>,
    ) -> Result<R, RuntimeError> {
        let _safepoint = self.safepoint.enter(SafepointGuardKind::LambdaCell)?;
        let _guard = self.borrow.mutable_guard()?;
        f(unsafe { &mut *self.value.get() })
    }

    fn replace(&self, value: T) -> Result<T, RuntimeError> {
        self.mutate(|slot| Ok(std::mem::replace(slot, value)))
    }

    fn set(&self, value: T) -> Result<(), RuntimeError> {
        self.mutate(|slot| {
            *slot = value;
            Ok(())
        })
    }

    fn validate_trace(&self) -> Result<(), RuntimeError> {
        if self.borrow.is_unborrowed() {
            Ok(())
        } else {
            Err(RuntimeError::new(
                "cannot collect while lambda cell has an active borrow",
            ))
        }
    }

    fn trace_value<'cx, D: TraceDriver<'cx>>(&self, visitor: &mut Visitor<'cx, '_, D>)
    where
        T: Trace<'cx>,
    {
        self.validate_trace()
            .expect("lambda cell traced outside a safepoint");
        unsafe { &*self.value.get() }.trace(visitor);
    }
}

impl<T: Copy> LambdaCellCore<T> {
    fn get_copy(&self) -> Result<T, RuntimeError> {
        self.access(|value| Ok(*value))
    }
}

pub struct StackLambdaCell<T> {
    core: LambdaCellCore<T>,
}

impl<T> StackLambdaCell<T> {
    pub fn new(value: T) -> Self {
        Self {
            core: LambdaCellCore::new(value),
        }
    }

    pub fn new_with_safepoint(value: T, safepoint: SafepointState) -> Self {
        Self {
            core: LambdaCellCore::new_with_safepoint(value, safepoint),
        }
    }

    pub fn access<R>(
        &self,
        f: impl FnOnce(&T) -> Result<R, RuntimeError>,
    ) -> Result<R, RuntimeError> {
        self.core.access(f)
    }

    pub fn mutate<R>(
        &self,
        f: impl FnOnce(&mut T) -> Result<R, RuntimeError>,
    ) -> Result<R, RuntimeError> {
        self.core.mutate(f)
    }

    pub fn replace(&self, value: T) -> Result<T, RuntimeError> {
        self.core.replace(value)
    }

    pub fn set(&self, value: T) -> Result<(), RuntimeError> {
        self.core.set(value)
    }
}

impl<T: Copy> StackLambdaCell<T> {
    pub fn get_copy(&self) -> Result<T, RuntimeError> {
        self.core.get_copy()
    }
}

pub struct LambdaCell<T> {
    core: LambdaCellCore<T>,
}

impl<T> LambdaCell<T> {
    pub fn new(value: T) -> Self {
        Self {
            core: LambdaCellCore::new(value),
        }
    }

    pub fn new_with_safepoint(value: T, safepoint: SafepointState) -> Self {
        Self {
            core: LambdaCellCore::new_with_safepoint(value, safepoint),
        }
    }

    pub fn access<R>(
        &self,
        f: impl FnOnce(&T) -> Result<R, RuntimeError>,
    ) -> Result<R, RuntimeError> {
        self.core.access(f)
    }

    pub fn mutate<R>(
        &self,
        f: impl FnOnce(&mut T) -> Result<R, RuntimeError>,
    ) -> Result<R, RuntimeError> {
        self.core.mutate(f)
    }

    pub fn replace(&self, value: T) -> Result<T, RuntimeError> {
        self.core.replace(value)
    }

    pub fn set(&self, value: T) -> Result<(), RuntimeError> {
        self.core.set(value)
    }
}

impl<T: Copy> LambdaCell<T> {
    pub fn get_copy(&self) -> Result<T, RuntimeError> {
        self.core.get_copy()
    }
}

// SAFETY: `LambdaCellCore::trace_value` reports the contained payload exactly once without
// cloning, dropping, or mutating it. Heap collection reaches this only at safepoints where no
// cell access guard is active.
unsafe impl<'cx, T: Trace<'cx>> Trace<'cx> for LambdaCell<T> {
    fn trace<D: TraceDriver<'cx>>(&self, visitor: &mut Visitor<'cx, '_, D>) {
        self.core.trace_value(visitor);
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
    use super::{LambdaCell, StackLambdaCell};

    #[test]
    fn stack_cell_nested_mutable_access_fails_safely() {
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
    fn heap_cell_shared_access_during_mutable_access_fails_safely() {
        let cell = LambdaCell::new(1);

        let err = cell
            .mutate(|_| cell.access(|value| Ok(*value)))
            .unwrap_err();

        assert_eq!(err.message(), "conflicting mutable cell access");
        assert_eq!(cell.get_copy().unwrap(), 1);
    }

    #[test]
    fn heap_cell_mutable_access_during_shared_access_fails_safely() {
        let cell = LambdaCell::new(1);

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
}
