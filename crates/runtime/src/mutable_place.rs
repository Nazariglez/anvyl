use std::{marker::PhantomData, rc::Rc};

use crate::{RuntimeError, StackLambdaCell, lambda_cell::CellBorrowFlag};

pub enum MutPlace<'place, 'cx, T> {
    Local(&'place mut T, PhantomData<&'cx ()>),
    StackCell(&'place StackLambdaCell<T>, PhantomData<&'cx ()>),
    ScopedCell(&'place ScopedMutPlaceCell<'place, 'cx, T>),
}

pub struct ScopedMutPlaceCell<'source, 'cx, T> {
    root: ScopedMutPlaceRoot<'source, 'cx, T>,
    borrow: CellBorrowFlag,
    _not_send_sync: PhantomData<Rc<()>>,
}

enum ScopedMutPlaceRoot<'source, 'cx, T> {
    Local(*mut T, PhantomData<&'source mut T>, PhantomData<&'cx ()>),
    StackCell(&'source StackLambdaCell<T>, PhantomData<&'cx ()>),
    ScopedCell(&'source ScopedMutPlaceCell<'source, 'cx, T>),
}

impl<'place, 'cx, T> MutPlace<'place, 'cx, T> {
    pub fn local(value: &'place mut T) -> Self {
        Self::Local(value, PhantomData)
    }

    pub fn stack_cell(cell: &'place StackLambdaCell<T>) -> Self {
        Self::StackCell(cell, PhantomData)
    }

    pub fn scoped_cell(cell: &'place ScopedMutPlaceCell<'place, 'cx, T>) -> Self {
        Self::ScopedCell(cell)
    }

    pub fn reborrow(&mut self) -> MutPlace<'_, 'cx, T> {
        match self {
            Self::Local(value, _) => MutPlace::local(&mut **value),
            Self::StackCell(cell, _) => MutPlace::stack_cell(cell),
            Self::ScopedCell(cell) => MutPlace::scoped_cell(cell),
        }
    }

    pub fn access<R>(
        &self,
        f: impl FnOnce(&T) -> Result<R, RuntimeError>,
    ) -> Result<R, RuntimeError> {
        match self {
            Self::Local(value, _) => f(value),
            Self::StackCell(cell, _) => cell.access(f),
            Self::ScopedCell(cell) => cell.access(f),
        }
    }

    pub fn mutate<R>(
        &mut self,
        f: impl FnOnce(&mut T) -> Result<R, RuntimeError>,
    ) -> Result<R, RuntimeError> {
        match self {
            Self::Local(value, _) => f(*value),
            Self::StackCell(cell, _) => cell.mutate(f),
            Self::ScopedCell(cell) => cell.mutate(f),
        }
    }

    pub fn set(&mut self, value: T) -> Result<(), RuntimeError> {
        self.mutate(|slot| {
            *slot = value;
            Ok(())
        })
    }

    pub fn replace(&mut self, value: T) -> Result<T, RuntimeError> {
        self.mutate(|slot| Ok(std::mem::replace(slot, value)))
    }
}

impl<T: Copy> MutPlace<'_, '_, T> {
    pub fn get_copy(&self) -> Result<T, RuntimeError> {
        self.access(|value| Ok(*value))
    }

    pub fn update_copy(&mut self, f: impl FnOnce(T) -> T) -> Result<(), RuntimeError> {
        self.mutate(|value| {
            *value = f(*value);
            Ok(())
        })
    }
}

impl<'source, 'cx, T> ScopedMutPlaceCell<'source, 'cx, T> {
    pub fn new(place: MutPlace<'source, 'cx, T>) -> Self {
        Self {
            root: ScopedMutPlaceRoot::from_place(place),
            borrow: CellBorrowFlag::default(),
            _not_send_sync: PhantomData,
        }
    }

    pub fn access<R>(
        &self,
        f: impl FnOnce(&T) -> Result<R, RuntimeError>,
    ) -> Result<R, RuntimeError> {
        let _guard = self.borrow.shared_guard()?;
        match self.root {
            ScopedMutPlaceRoot::Local(value, _, _) => f(unsafe { &*value }),
            ScopedMutPlaceRoot::StackCell(cell, _) => cell.access(f),
            ScopedMutPlaceRoot::ScopedCell(cell) => cell.access(f),
        }
    }

    pub fn mutate<R>(
        &self,
        f: impl FnOnce(&mut T) -> Result<R, RuntimeError>,
    ) -> Result<R, RuntimeError> {
        let _guard = self.borrow.mutable_guard()?;
        match self.root {
            ScopedMutPlaceRoot::Local(value, _, _) => f(unsafe { &mut *value }),
            ScopedMutPlaceRoot::StackCell(cell, _) => cell.mutate(f),
            ScopedMutPlaceRoot::ScopedCell(cell) => cell.mutate(f),
        }
    }

    pub fn set(&self, value: T) -> Result<(), RuntimeError> {
        self.mutate(|slot| {
            *slot = value;
            Ok(())
        })
    }

    pub fn replace(&self, value: T) -> Result<T, RuntimeError> {
        self.mutate(|slot| Ok(std::mem::replace(slot, value)))
    }
}

impl<'source, 'cx, T> ScopedMutPlaceRoot<'source, 'cx, T> {
    fn from_place(place: MutPlace<'source, 'cx, T>) -> Self {
        match place {
            MutPlace::Local(value, _) => Self::Local(value, PhantomData, PhantomData),
            MutPlace::StackCell(cell, _) => Self::StackCell(cell, PhantomData),
            MutPlace::ScopedCell(cell) => Self::ScopedCell(cell),
        }
    }
}

impl<T: Copy> ScopedMutPlaceCell<'_, '_, T> {
    pub fn get_copy(&self) -> Result<T, RuntimeError> {
        self.access(|value| Ok(*value))
    }
}

#[cfg(test)]
mod tests {
    use crate::{AnvList, MutPlace, RuntimeError, ScopedMutPlaceCell, StackLambdaCell};

    #[test]
    fn local_access_mutate_and_copy_update() {
        let mut value = 1;
        let mut place = MutPlace::local(&mut value);

        assert_eq!(place.get_copy().unwrap(), 1);
        place.update_copy(|value| value + 1).unwrap();
        assert_eq!(place.get_copy().unwrap(), 2);
        place.set(5).unwrap();
        assert_eq!(place.replace(8).unwrap(), 5);
        assert_eq!(place.get_copy().unwrap(), 8);
    }

    #[test]
    fn stack_cell_routes_through_guarded_cell() {
        let cell = StackLambdaCell::new(1);
        let mut place = MutPlace::stack_cell(&cell);

        place.update_copy(|value| value + 1).unwrap();
        assert_eq!(place.get_copy().unwrap(), 2);
        place.set(4).unwrap();
        assert_eq!(cell.get_copy().unwrap(), 4);
    }

    #[test]
    fn reborrow_preserves_local_identity() {
        let mut value = 1;
        let mut place = MutPlace::local(&mut value);
        {
            let mut forwarded = place.reborrow();
            forwarded.update_copy(|value| value + 1).unwrap();
        }
        place.update_copy(|value| value + 1).unwrap();

        assert_eq!(value, 3);
    }

    #[test]
    fn reborrow_preserves_stack_cell_identity() {
        let cell = StackLambdaCell::new(1);
        let mut place = MutPlace::stack_cell(&cell);
        {
            let mut forwarded = place.reborrow();
            forwarded.update_copy(|value| value + 1).unwrap();
        }
        place.update_copy(|value| value + 1).unwrap();

        assert_eq!(cell.get_copy().unwrap(), 3);
    }

    #[test]
    fn set_and_replace_do_not_require_clone() {
        struct NonClone(i64);

        let mut value = NonClone(1);
        let mut place = MutPlace::local(&mut value);

        let old = place.replace(NonClone(2)).unwrap();
        assert_eq!(old.0, 1);
        place.set(NonClone(3)).unwrap();
        assert_eq!(value.0, 3);
    }

    #[test]
    fn local_list_mutation_uses_short_region() {
        let mut list = AnvList::from_elems([1_i64]);
        let mut place = MutPlace::local(&mut list);

        place
            .mutate(|list| {
                list.push(2);
                Ok(())
            })
            .unwrap();

        assert_eq!(place.access(|list| Ok(list.len())).unwrap(), 2);
    }

    #[test]
    fn stack_cell_conflict_is_returned_not_panicked() {
        let cell = StackLambdaCell::new(1);
        let mut place = MutPlace::stack_cell(&cell);
        let err = cell
            .access(|_| place.update_copy(|value| value + 1))
            .unwrap_err();

        assert_eq!(err.message(), "conflicting mutable cell access");
    }

    #[test]
    fn local_region_restores_after_error() {
        let mut value = 1;
        let mut place = MutPlace::local(&mut value);
        let err = place
            .mutate(|_| Err::<(), _>(RuntimeError::new("early")))
            .unwrap_err();

        assert_eq!(err.message(), "early");
        place.set(2).unwrap();
        assert_eq!(value, 2);
    }

    #[test]
    fn scoped_cell_mutates_local_place() {
        let mut value = 1;
        let cell = ScopedMutPlaceCell::new(MutPlace::local(&mut value));

        cell.set(2).unwrap();
        assert_eq!(cell.get_copy().unwrap(), 2);
        let mut forwarded = MutPlace::scoped_cell(&cell);
        forwarded.update_copy(|value| value + 1).unwrap();

        assert_eq!(cell.get_copy().unwrap(), 3);
    }

    #[test]
    fn scoped_cell_mutates_stack_cell_place() {
        let source = StackLambdaCell::new(1);
        let cell = ScopedMutPlaceCell::new(MutPlace::stack_cell(&source));

        cell.set(2).unwrap();
        let mut forwarded = MutPlace::scoped_cell(&cell);
        forwarded.update_copy(|value| value + 1).unwrap();

        assert_eq!(source.get_copy().unwrap(), 3);
    }

    #[test]
    fn scoped_cell_reborrow_preserves_identity() {
        let mut value = 1;
        let cell = ScopedMutPlaceCell::new(MutPlace::local(&mut value));
        let mut place = MutPlace::scoped_cell(&cell);
        {
            let mut forwarded = place.reborrow();
            forwarded.update_copy(|value| value + 1).unwrap();
        }
        place.update_copy(|value| value + 1).unwrap();

        assert_eq!(cell.get_copy().unwrap(), 3);
    }

    #[test]
    fn scoped_cell_can_wrap_forwarded_scoped_place() {
        let mut value = 1;
        let outer = ScopedMutPlaceCell::new(MutPlace::local(&mut value));
        let inner = ScopedMutPlaceCell::new(MutPlace::scoped_cell(&outer));

        inner.set(2).unwrap();
        let mut forwarded = MutPlace::scoped_cell(&inner);
        forwarded.update_copy(|value| value + 1).unwrap();

        assert_eq!(outer.get_copy().unwrap(), 3);
    }

    #[test]
    fn scoped_cell_conflict_is_returned_not_panicked() {
        let mut value = 1;
        let cell = ScopedMutPlaceCell::new(MutPlace::local(&mut value));
        let err = cell.access(|_| cell.set(2)).unwrap_err();

        assert_eq!(err.message(), "conflicting mutable cell access");
        cell.set(3).unwrap();
        assert_eq!(cell.get_copy().unwrap(), 3);
    }

    #[test]
    fn scoped_cell_guard_restores_after_error() {
        let mut value = 1;
        let cell = ScopedMutPlaceCell::new(MutPlace::local(&mut value));
        let err = cell
            .mutate(|_| Err::<(), _>(RuntimeError::new("early")))
            .unwrap_err();

        assert_eq!(err.message(), "early");
        cell.set(2).unwrap();
        assert_eq!(cell.get_copy().unwrap(), 2);
    }
}
