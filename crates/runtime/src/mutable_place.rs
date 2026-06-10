use std::{marker::PhantomData, rc::Rc};

use crate::{
    AccessError, Ctx, Handle, LambdaCell, RuntimeError, StackLambdaCell,
    lambda_cell::CellBorrowFlag,
};

pub enum MutPlace<'place, 'cx, T> {
    Local(&'place mut T, PhantomData<&'cx ()>),
    StackCell(&'place StackLambdaCell<T>, PhantomData<&'cx ()>),
    HeapCell(Handle<'cx, LambdaCell<T>>),
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
    HeapCell(Handle<'cx, LambdaCell<T>>),
    ScopedCell(&'source ScopedMutPlaceCell<'source, 'cx, T>),
}

fn heap_access<'cx, 'rt, T: 'cx, R>(
    ctx: &mut Ctx<'cx, 'rt>,
    cell: &Handle<'cx, LambdaCell<T>>,
    f: impl FnOnce(&LambdaCell<T>) -> Result<R, RuntimeError>,
) -> Result<R, RuntimeError> {
    ctx.heap().try_with(cell, f).map_err(heap_access_error)?
}

fn heap_access_error(error: AccessError) -> RuntimeError {
    let message = match error {
        AccessError::WrongHeap => "heap cell belongs to a different heap",
        AccessError::DeadHandle => "heap cell handle is no longer live",
    };
    RuntimeError::new(message)
}

impl<'place, 'cx, T: 'cx> MutPlace<'place, 'cx, T> {
    pub fn local(value: &'place mut T) -> Self {
        Self::Local(value, PhantomData)
    }

    pub fn stack_cell(cell: &'place StackLambdaCell<T>) -> Self {
        Self::StackCell(cell, PhantomData)
    }

    pub fn heap_cell(cell: Handle<'cx, LambdaCell<T>>) -> Self {
        Self::HeapCell(cell)
    }

    pub fn scoped_cell(cell: &'place ScopedMutPlaceCell<'place, 'cx, T>) -> Self {
        Self::ScopedCell(cell)
    }

    pub fn reborrow(&mut self) -> MutPlace<'_, 'cx, T> {
        match self {
            Self::Local(value, _) => MutPlace::local(&mut **value),
            Self::StackCell(cell, _) => MutPlace::stack_cell(cell),
            Self::HeapCell(cell) => MutPlace::heap_cell(cell.clone()),
            Self::ScopedCell(cell) => MutPlace::scoped_cell(cell),
        }
    }

    pub fn access<'rt, R>(
        &self,
        ctx: &mut Ctx<'cx, 'rt>,
        f: impl FnOnce(&T) -> Result<R, RuntimeError>,
    ) -> Result<R, RuntimeError> {
        match self {
            Self::Local(value, _) => f(value),
            Self::StackCell(cell, _) => cell.access(f),
            Self::HeapCell(cell) => heap_access(ctx, cell, |cell| cell.access(f)),
            Self::ScopedCell(cell) => cell.access(ctx, f),
        }
    }

    pub fn mutate<'rt, R>(
        &mut self,
        ctx: &mut Ctx<'cx, 'rt>,
        f: impl FnOnce(&mut T) -> Result<R, RuntimeError>,
    ) -> Result<R, RuntimeError> {
        match self {
            Self::Local(value, _) => f(*value),
            Self::StackCell(cell, _) => cell.mutate(f),
            Self::HeapCell(cell) => heap_access(ctx, cell, |cell| cell.mutate(f)),
            Self::ScopedCell(cell) => cell.mutate(ctx, f),
        }
    }

    pub fn set<'rt>(&mut self, ctx: &mut Ctx<'cx, 'rt>, value: T) -> Result<(), RuntimeError> {
        self.mutate(ctx, |slot| {
            *slot = value;
            Ok(())
        })
    }

    pub fn replace<'rt>(&mut self, ctx: &mut Ctx<'cx, 'rt>, value: T) -> Result<T, RuntimeError> {
        self.mutate(ctx, |slot| Ok(std::mem::replace(slot, value)))
    }
}

impl<'cx, T: Copy + 'cx> MutPlace<'_, 'cx, T> {
    pub fn get_copy<'rt>(&self, ctx: &mut Ctx<'cx, 'rt>) -> Result<T, RuntimeError> {
        self.access(ctx, |value| Ok(*value))
    }

    pub fn update_copy<'rt>(
        &mut self,
        ctx: &mut Ctx<'cx, 'rt>,
        f: impl FnOnce(T) -> T,
    ) -> Result<(), RuntimeError> {
        self.mutate(ctx, |value| {
            *value = f(*value);
            Ok(())
        })
    }
}

impl<'source, 'cx, T: 'cx> ScopedMutPlaceCell<'source, 'cx, T> {
    pub fn new(place: MutPlace<'source, 'cx, T>) -> Self {
        Self {
            root: ScopedMutPlaceRoot::from_place(place),
            borrow: CellBorrowFlag::default(),
            _not_send_sync: PhantomData,
        }
    }

    pub fn access<'rt, R>(
        &self,
        ctx: &mut Ctx<'cx, 'rt>,
        f: impl FnOnce(&T) -> Result<R, RuntimeError>,
    ) -> Result<R, RuntimeError> {
        let _guard = self.borrow.shared_guard()?;
        match self.root {
            ScopedMutPlaceRoot::Local(value, _, _) => f(unsafe { &*value }),
            ScopedMutPlaceRoot::StackCell(cell, _) => cell.access(f),
            ScopedMutPlaceRoot::HeapCell(ref cell) => heap_access(ctx, cell, |cell| cell.access(f)),
            ScopedMutPlaceRoot::ScopedCell(cell) => cell.access(ctx, f),
        }
    }

    pub fn mutate<'rt, R>(
        &self,
        ctx: &mut Ctx<'cx, 'rt>,
        f: impl FnOnce(&mut T) -> Result<R, RuntimeError>,
    ) -> Result<R, RuntimeError> {
        let _guard = self.borrow.mutable_guard()?;
        match self.root {
            ScopedMutPlaceRoot::Local(value, _, _) => f(unsafe { &mut *value }),
            ScopedMutPlaceRoot::StackCell(cell, _) => cell.mutate(f),
            ScopedMutPlaceRoot::HeapCell(ref cell) => heap_access(ctx, cell, |cell| cell.mutate(f)),
            ScopedMutPlaceRoot::ScopedCell(cell) => cell.mutate(ctx, f),
        }
    }

    pub fn set<'rt>(&self, ctx: &mut Ctx<'cx, 'rt>, value: T) -> Result<(), RuntimeError> {
        self.mutate(ctx, |slot| {
            *slot = value;
            Ok(())
        })
    }

    pub fn replace<'rt>(&self, ctx: &mut Ctx<'cx, 'rt>, value: T) -> Result<T, RuntimeError> {
        self.mutate(ctx, |slot| Ok(std::mem::replace(slot, value)))
    }
}

impl<'source, 'cx, T: 'cx> ScopedMutPlaceRoot<'source, 'cx, T> {
    fn from_place(place: MutPlace<'source, 'cx, T>) -> Self {
        match place {
            MutPlace::Local(value, _) => Self::Local(value, PhantomData, PhantomData),
            MutPlace::StackCell(cell, _) => Self::StackCell(cell, PhantomData),
            MutPlace::HeapCell(cell) => Self::HeapCell(cell),
            MutPlace::ScopedCell(cell) => Self::ScopedCell(cell),
        }
    }
}

impl<'cx, T: Copy + 'cx> ScopedMutPlaceCell<'_, 'cx, T> {
    pub fn get_copy<'rt>(&self, ctx: &mut Ctx<'cx, 'rt>) -> Result<T, RuntimeError> {
        self.access(ctx, |value| Ok(*value))
    }
}

#[cfg(test)]
mod tests {
    use crate::{AnvList, LambdaCell, MutPlace, RuntimeError, ScopedMutPlaceCell, StackLambdaCell};

    macro_rules! with_ctx {
        ($ctx:ident; $($body:tt)*) => {
            crate::Heap::scope(|heap| {
                let mut $ctx = crate::Ctx::new(heap);
                $($body)*
            })
        };
    }

    #[test]
    fn local_access_mutate_and_copy_update() {
        with_ctx!(ctx;
        let mut value = 1;
        let mut place = MutPlace::local(&mut value);

        assert_eq!(place.get_copy(&mut ctx).unwrap(), 1);
        place.update_copy(&mut ctx, |value| value + 1).unwrap();
        assert_eq!(place.get_copy(&mut ctx).unwrap(), 2);
        place.set(&mut ctx, 5).unwrap();
        assert_eq!(place.replace(&mut ctx, 8).unwrap(), 5);
        assert_eq!(place.get_copy(&mut ctx).unwrap(), 8);
            );
    }

    #[test]
    fn stack_cell_routes_through_guarded_cell() {
        with_ctx!(ctx;
        let cell = StackLambdaCell::new(1);
        let mut place = MutPlace::stack_cell(&cell);

        place.update_copy(&mut ctx, |value| value + 1).unwrap();
        assert_eq!(place.get_copy(&mut ctx).unwrap(), 2);
        place.set(&mut ctx, 4).unwrap();
        assert_eq!(cell.get_copy().unwrap(), 4);
            );
    }

    #[test]
    fn reborrow_preserves_local_identity() {
        with_ctx!(ctx;
        let mut value = 1;
        let mut place = MutPlace::local(&mut value);
        {
            let mut forwarded = place.reborrow();
            forwarded.update_copy(&mut ctx, |value| value + 1).unwrap();
        }
        place.update_copy(&mut ctx, |value| value + 1).unwrap();

        assert_eq!(value, 3);
            );
    }

    #[test]
    fn reborrow_preserves_stack_cell_identity() {
        with_ctx!(ctx;
        let cell = StackLambdaCell::new(1);
        let mut place = MutPlace::stack_cell(&cell);
        {
            let mut forwarded = place.reborrow();
            forwarded.update_copy(&mut ctx, |value| value + 1).unwrap();
        }
        place.update_copy(&mut ctx, |value| value + 1).unwrap();

        assert_eq!(cell.get_copy().unwrap(), 3);
            );
    }

    #[test]
    fn set_and_replace_do_not_require_clone() {
        with_ctx!(ctx;
        struct NonClone(i64);

        let mut value = NonClone(1);
        let mut place = MutPlace::local(&mut value);

        let old = place.replace(&mut ctx, NonClone(2)).unwrap();
        assert_eq!(old.0, 1);
        place.set(&mut ctx, NonClone(3)).unwrap();
        assert_eq!(value.0, 3);
            );
    }

    #[test]
    fn local_list_mutation_uses_short_region() {
        with_ctx!(ctx;
        let mut list = AnvList::from_elems([1_i64]);
        let mut place = MutPlace::local(&mut list);

        place
            .mutate(&mut ctx, |list| {
                list.push(2);
                Ok(())
            })
            .unwrap();

        assert_eq!(place.access(&mut ctx, |list| Ok(list.len())).unwrap(), 2);
            );
    }

    #[test]
    fn stack_cell_conflict_is_returned_not_panicked() {
        with_ctx!(ctx;
        let cell = StackLambdaCell::new(1);
        let mut place = MutPlace::stack_cell(&cell);
        let err = cell
            .access(|_| place.update_copy(&mut ctx, |value| value + 1))
            .unwrap_err();

        assert_eq!(err.message(), "conflicting mutable cell access");
            );
    }

    #[test]
    fn local_region_restores_after_error() {
        with_ctx!(ctx;
        let mut value = 1;
        let mut place = MutPlace::local(&mut value);
        let err = place
            .mutate(&mut ctx, |_| Err::<(), _>(RuntimeError::new("early")))
            .unwrap_err();

        assert_eq!(err.message(), "early");
        place.set(&mut ctx, 2).unwrap();
        assert_eq!(value, 2);
            );
    }

    #[test]
    fn scoped_cell_mutates_local_place() {
        with_ctx!(ctx;
        let mut value = 1;
        let cell = ScopedMutPlaceCell::new(MutPlace::local(&mut value));

        cell.set(&mut ctx, 2).unwrap();
        assert_eq!(cell.get_copy(&mut ctx).unwrap(), 2);
        let mut forwarded = MutPlace::scoped_cell(&cell);
        forwarded.update_copy(&mut ctx, |value| value + 1).unwrap();

        assert_eq!(cell.get_copy(&mut ctx).unwrap(), 3);
            );
    }

    #[test]
    fn scoped_cell_mutates_stack_cell_place() {
        with_ctx!(ctx;
        let source = StackLambdaCell::new(1);
        let cell = ScopedMutPlaceCell::new(MutPlace::stack_cell(&source));

        cell.set(&mut ctx, 2).unwrap();
        let mut forwarded = MutPlace::scoped_cell(&cell);
        forwarded.update_copy(&mut ctx, |value| value + 1).unwrap();

        assert_eq!(source.get_copy().unwrap(), 3);
            );
    }

    #[test]
    fn scoped_cell_reborrow_preserves_identity() {
        with_ctx!(ctx;
        let mut value = 1;
        let cell = ScopedMutPlaceCell::new(MutPlace::local(&mut value));
        let mut place = MutPlace::scoped_cell(&cell);
        {
            let mut forwarded = place.reborrow();
            forwarded.update_copy(&mut ctx, |value| value + 1).unwrap();
        }
        place.update_copy(&mut ctx, |value| value + 1).unwrap();

        assert_eq!(cell.get_copy(&mut ctx).unwrap(), 3);
            );
    }

    #[test]
    fn scoped_cell_can_wrap_forwarded_scoped_place() {
        with_ctx!(ctx;
        let mut value = 1;
        let outer = ScopedMutPlaceCell::new(MutPlace::local(&mut value));
        let inner = ScopedMutPlaceCell::new(MutPlace::scoped_cell(&outer));

        inner.set(&mut ctx, 2).unwrap();
        let mut forwarded = MutPlace::scoped_cell(&inner);
        forwarded.update_copy(&mut ctx, |value| value + 1).unwrap();

        assert_eq!(outer.get_copy(&mut ctx).unwrap(), 3);
            );
    }

    #[test]
    fn scoped_cell_conflict_is_returned_not_panicked() {
        with_ctx!(ctx;
        let mut value = 1;
        let cell = ScopedMutPlaceCell::new(MutPlace::local(&mut value));
        let guard = cell.borrow.shared_guard().unwrap();
        let err = cell.set(&mut ctx, 2).unwrap_err();

        assert_eq!(err.message(), "conflicting mutable cell access");
        drop(guard);
        cell.set(&mut ctx, 3).unwrap();
        assert_eq!(cell.get_copy(&mut ctx).unwrap(), 3);
            );
    }

    #[test]
    fn heap_cell_routes_through_lambda_cell() {
        with_ctx!(ctx;
        let cell_ty = ctx.heap().register_untracked::<LambdaCell<i64>>();
        let cell = ctx.heap().alloc(cell_ty, LambdaCell::new(1));
        let mut place = MutPlace::heap_cell(cell.clone());

        place.update_copy(&mut ctx, |value| value + 1).unwrap();
        assert_eq!(place.get_copy(&mut ctx).unwrap(), 2);
        place.set(&mut ctx, 4).unwrap();
        assert_eq!(ctx.heap().with(&cell, LambdaCell::get_copy).unwrap(), 4);
            );
    }

    #[test]
    fn reborrow_preserves_heap_cell_identity() {
        with_ctx!(ctx;
        let cell_ty = ctx.heap().register_untracked::<LambdaCell<i64>>();
        let cell = ctx.heap().alloc(cell_ty, LambdaCell::new(1));
        let mut place = MutPlace::heap_cell(cell.clone());
        {
            let mut forwarded = place.reborrow();
            forwarded.update_copy(&mut ctx, |value| value + 1).unwrap();
        }
        place.update_copy(&mut ctx, |value| value + 1).unwrap();

        assert_eq!(ctx.heap().with(&cell, LambdaCell::get_copy).unwrap(), 3);
            );
    }

    #[test]
    fn scoped_cell_mutates_heap_cell_place() {
        with_ctx!(ctx;
        let cell_ty = ctx.heap().register_untracked::<LambdaCell<i64>>();
        let source = ctx.heap().alloc(cell_ty, LambdaCell::new(1));
        let cell = ScopedMutPlaceCell::new(MutPlace::heap_cell(source.clone()));

        cell.set(&mut ctx, 2).unwrap();
        let mut forwarded = MutPlace::scoped_cell(&cell);
        forwarded.update_copy(&mut ctx, |value| value + 1).unwrap();

        assert_eq!(ctx.heap().with(&source, LambdaCell::get_copy).unwrap(), 3);
            );
    }

    #[test]
    fn heap_cell_guard_restores_after_error() {
        with_ctx!(ctx;
        let cell_ty = ctx.heap().register_untracked::<LambdaCell<i64>>();
        let cell = ctx.heap().alloc(cell_ty, LambdaCell::new(1));
        let mut place = MutPlace::heap_cell(cell.clone());
        let err = place
            .mutate(&mut ctx, |_| Err::<(), _>(RuntimeError::new("early")))
            .unwrap_err();

        assert_eq!(err.message(), "early");
        place.set(&mut ctx, 2).unwrap();
        assert_eq!(ctx.heap().with(&cell, LambdaCell::get_copy).unwrap(), 2);
            );
    }

    #[test]
    fn heap_cell_set_and_replace_do_not_require_clone() {
        with_ctx!(ctx;
        struct NonClone(i64);

        let cell_ty = ctx.heap().register_untracked::<LambdaCell<NonClone>>();
        let cell = ctx.heap().alloc(cell_ty, LambdaCell::new(NonClone(1)));
        let mut place = MutPlace::heap_cell(cell.clone());

        let old = place.replace(&mut ctx, NonClone(2)).unwrap();
        assert_eq!(old.0, 1);
        place.set(&mut ctx, NonClone(3)).unwrap();
        assert_eq!(ctx.heap().with(&cell, |cell| cell.access(|value| Ok(value.0))).unwrap(), 3);
            );
    }

    #[test]
    fn scoped_cell_guard_restores_after_error() {
        with_ctx!(ctx;
        let mut value = 1;
        let cell = ScopedMutPlaceCell::new(MutPlace::local(&mut value));
        let err = cell
            .mutate(&mut ctx, |_| Err::<(), _>(RuntimeError::new("early")))
            .unwrap_err();

        assert_eq!(err.message(), "early");
        cell.set(&mut ctx, 2).unwrap();
        assert_eq!(cell.get_copy(&mut ctx).unwrap(), 2);
            );
    }
}
