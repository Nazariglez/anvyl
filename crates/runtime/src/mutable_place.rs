use std::{marker::PhantomData, rc::Rc};

use crate::{
    AccessError, AnvList, AnvSlice, Ctx, ErasedHandle, Handle, LambdaCell, RuntimeError,
    StackLambdaCell, lambda_cell::CellBorrowFlag,
};

pub enum MutPlace<'place, 'cx, T> {
    Local(&'place mut T, PhantomData<&'cx ()>),
    StackCell(&'place StackLambdaCell<T>, PhantomData<&'cx ()>),
    HeapCell(Handle<'cx, LambdaCell<T>>),
    DataRef(DataRefPlace<'place, 'cx, T>),
    ScopedCell(&'place ScopedMutPlaceCell<'place, 'cx, T>),
}

pub struct DataRefPlace<'ops, 'cx, T> {
    object: ErasedHandle<'cx>,
    ops: &'ops dyn DataRefPlaceOps<'cx, T>,
    _not_send_sync: PhantomData<Rc<()>>,
}

pub trait DataRefPlaceOps<'cx, T> {
    fn access(
        &self,
        ctx: &mut Ctx<'cx, '_>,
        object: &ErasedHandle<'cx>,
        f: &mut dyn FnMut(&T) -> Result<(), RuntimeError>,
    ) -> Result<(), RuntimeError>;

    fn mutate(
        &self,
        ctx: &mut Ctx<'cx, '_>,
        object: &ErasedHandle<'cx>,
        f: &mut dyn FnMut(&mut T) -> Result<(), RuntimeError>,
    ) -> Result<(), RuntimeError>;
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
    DataRef(DataRefPlace<'source, 'cx, T>),
    ScopedCell(&'source ScopedMutPlaceCell<'source, 'cx, T>),
}

fn heap_access<'cx, 'rt, T: 'cx, R>(
    ctx: &mut Ctx<'cx, 'rt>,
    cell: &Handle<'cx, LambdaCell<T>>,
    f: impl FnOnce(&LambdaCell<T>) -> Result<R, RuntimeError>,
) -> Result<R, RuntimeError> {
    ctx.heap().try_with(cell, f).map_err(heap_access_error)?
}

pub fn heap_access_error(error: AccessError) -> RuntimeError {
    let message = match error {
        AccessError::WrongHeap => "heap object belongs to a different heap",
        AccessError::DeadHandle => "heap object handle is no longer live",
    };
    RuntimeError::new(message)
}

impl<'ops, 'cx, T: 'cx> DataRefPlace<'ops, 'cx, T> {
    pub fn new(object: ErasedHandle<'cx>, ops: &'ops dyn DataRefPlaceOps<'cx, T>) -> Self {
        Self {
            object,
            ops,
            _not_send_sync: PhantomData,
        }
    }

    #[must_use]
    pub fn reborrow(&self) -> DataRefPlace<'ops, 'cx, T> {
        Self::new(self.object.clone(), self.ops)
    }

    fn access<'rt, R>(
        &self,
        ctx: &mut Ctx<'cx, 'rt>,
        f: impl FnOnce(&T) -> Result<R, RuntimeError>,
    ) -> Result<R, RuntimeError> {
        let mut f = Some(f);
        let mut out = None;
        self.ops.access(ctx, &self.object, &mut |slot| {
            let f = f.take().expect("dataref place access invoked twice");
            out = Some(f(slot)?);
            Ok(())
        })?;
        Ok(out.expect("dataref place access did not invoke callback"))
    }

    fn mutate<'rt, R>(
        &self,
        ctx: &mut Ctx<'cx, 'rt>,
        f: impl FnOnce(&mut T) -> Result<R, RuntimeError>,
    ) -> Result<R, RuntimeError> {
        let mut f = Some(f);
        let mut out = None;
        self.ops.mutate(ctx, &self.object, &mut |slot| {
            let f = f.take().expect("dataref place mutation invoked twice");
            out = Some(f(slot)?);
            Ok(())
        })?;
        Ok(out.expect("dataref place mutation did not invoke callback"))
    }
}

impl<'cx, T: 'cx> MutPlace<'_, 'cx, AnvList<T>> {
    pub fn slice_view(
        &self,
        start: i64,
        end: i64,
        inclusive: bool,
    ) -> Result<AnvSlice<T>, RuntimeError> {
        match self {
            Self::Local(list, _) => {
                let range = crate::checked_range(start, end, inclusive, list.len());
                let guard = list.begin_shape_loan()?;
                Ok(unsafe {
                    AnvSlice::from_list(
                        std::ptr::from_ref(&**list),
                        range.start,
                        range.len(),
                        guard,
                    )
                })
            }
            Self::StackCell(..) | Self::HeapCell(_) | Self::DataRef(_) | Self::ScopedCell(_) => {
                Err(non_local_slice_view_error())
            }
        }
    }

    pub fn slice_view_mut(
        &mut self,
        start: i64,
        end: i64,
        inclusive: bool,
    ) -> Result<AnvSlice<T>, RuntimeError> {
        match self {
            Self::Local(list, _) => {
                let range = crate::checked_range(start, end, inclusive, list.len());
                let guard = list.begin_shape_loan()?;
                Ok(unsafe {
                    AnvSlice::from_list_mut(
                        std::ptr::from_mut(&mut **list),
                        range.start,
                        range.len(),
                        guard,
                    )
                })
            }
            Self::StackCell(..) | Self::HeapCell(_) | Self::DataRef(_) | Self::ScopedCell(_) => {
                Err(non_local_slice_view_error())
            }
        }
    }
}

impl<'cx, T: 'cx, const N: usize> MutPlace<'_, 'cx, [T; N]> {
    pub fn slice_view(
        &self,
        start: i64,
        end: i64,
        inclusive: bool,
    ) -> Result<AnvSlice<T>, RuntimeError> {
        match self {
            Self::Local(array, _) => {
                let range = crate::checked_range(start, end, inclusive, N);
                Ok(
                    unsafe {
                        AnvSlice::from_raw_parts(array.as_ptr(), N, range.start, range.len())
                    },
                )
            }
            Self::StackCell(..) | Self::HeapCell(_) | Self::DataRef(_) | Self::ScopedCell(_) => {
                Err(non_local_slice_view_error())
            }
        }
    }

    pub fn slice_view_mut(
        &mut self,
        start: i64,
        end: i64,
        inclusive: bool,
    ) -> Result<AnvSlice<T>, RuntimeError> {
        match self {
            Self::Local(array, _) => {
                let range = crate::checked_range(start, end, inclusive, N);
                Ok(unsafe {
                    AnvSlice::from_raw_parts_mut(array.as_mut_ptr(), N, range.start, range.len())
                })
            }
            Self::StackCell(..) | Self::HeapCell(_) | Self::DataRef(_) | Self::ScopedCell(_) => {
                Err(non_local_slice_view_error())
            }
        }
    }
}

fn non_local_slice_view_error() -> RuntimeError {
    RuntimeError::new("slice view over non-local mutable collection parameter is unsupported")
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

    pub fn dataref(object: ErasedHandle<'cx>, ops: &'place dyn DataRefPlaceOps<'cx, T>) -> Self {
        Self::DataRef(DataRefPlace::new(object, ops))
    }

    pub fn scoped_cell(cell: &'place ScopedMutPlaceCell<'place, 'cx, T>) -> Self {
        Self::ScopedCell(cell)
    }

    pub fn reborrow(&mut self) -> MutPlace<'_, 'cx, T> {
        match self {
            Self::Local(value, _) => MutPlace::local(&mut **value),
            Self::StackCell(cell, _) => MutPlace::stack_cell(cell),
            Self::HeapCell(cell) => MutPlace::heap_cell(cell.clone()),
            Self::DataRef(place) => MutPlace::DataRef(place.reborrow()),
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
            Self::DataRef(place) => place.access(ctx, f),
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
            Self::DataRef(place) => place.mutate(ctx, f),
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
            ScopedMutPlaceRoot::DataRef(ref place) => place.access(ctx, f),
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
            ScopedMutPlaceRoot::DataRef(ref place) => place.mutate(ctx, f),
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
            MutPlace::DataRef(place) => Self::DataRef(place),
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
    use std::mem::ManuallyDrop;

    use crate::{
        AnvList, AnvSlice, Ctx, DataRefPlaceOps, ErasedHandle, HeapType, LambdaCell, MutPlace,
        RuntimeError, ScopedMutPlaceCell, StackLambdaCell, heap_access_error,
    };

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
                list.push(2)?;
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

    struct Storage<T> {
        field: T,
    }

    struct FieldOps<'cx, T> {
        ty: HeapType<'cx, Storage<T>>,
    }

    impl<'cx, T: 'cx> DataRefPlaceOps<'cx, T> for FieldOps<'cx, T> {
        fn access(
            &self,
            ctx: &mut Ctx<'cx, '_>,
            object: &ErasedHandle<'cx>,
            f: &mut dyn FnMut(&T) -> Result<(), RuntimeError>,
        ) -> Result<(), RuntimeError> {
            ctx.heap()
                .try_with_erased(object, self.ty, |storage| f(&storage.field))
                .map_err(heap_access_error)?
        }

        fn mutate(
            &self,
            ctx: &mut Ctx<'cx, '_>,
            object: &ErasedHandle<'cx>,
            f: &mut dyn FnMut(&mut T) -> Result<(), RuntimeError>,
        ) -> Result<(), RuntimeError> {
            ctx.heap()
                .try_with_erased_mut(object, self.ty, |storage| f(&mut storage.field))
                .map_err(heap_access_error)?
        }
    }

    fn manual_copy<T>(value: &T) -> ManuallyDrop<T> {
        // SAFETY: these tests need a bitwise duplicate whose destructor is manually controlled.
        unsafe { ManuallyDrop::new(std::ptr::read(value)) }
    }

    unsafe fn copy_erased_to_current_heap<'cx>(erased: &ErasedHandle<'_>) -> ErasedHandle<'cx> {
        // SAFETY: this is only for wrong-heap diagnostics. The copied handle is dropped exactly once.
        unsafe { std::mem::transmute_copy(erased) }
    }

    #[test]
    fn dataref_place_routes_through_descriptor() {
        with_ctx!(ctx;
        let ty = ctx.heap().register_untracked::<Storage<i64>>();
        let object = ctx.heap().alloc(ty, Storage { field: 1 });
        let erased = ctx.heap().erase(&object).unwrap();
        let ops = FieldOps { ty };
        let mut place = MutPlace::dataref(erased, &ops);

        assert_eq!(place.get_copy(&mut ctx).unwrap(), 1);
        place.update_copy(&mut ctx, |value| value + 1).unwrap();
        assert_eq!(place.get_copy(&mut ctx).unwrap(), 2);
        place.set(&mut ctx, 5).unwrap();
        assert_eq!(place.replace(&mut ctx, 8).unwrap(), 5);
        assert_eq!(ctx.heap().with(&object, |storage| storage.field), 8);
            );
    }

    #[test]
    fn dataref_place_set_and_replace_do_not_require_clone() {
        with_ctx!(ctx;
        struct NonClone(i64);

        let ty = ctx.heap().register_untracked::<Storage<NonClone>>();
        let object = ctx.heap().alloc(ty, Storage { field: NonClone(1) });
        let erased = ctx.heap().erase(&object).unwrap();
        let ops = FieldOps { ty };
        let mut place = MutPlace::dataref(erased, &ops);

        let old = place.replace(&mut ctx, NonClone(2)).unwrap();
        assert_eq!(old.0, 1);
        place.set(&mut ctx, NonClone(3)).unwrap();
        assert_eq!(ctx.heap().with(&object, |storage| storage.field.0), 3);
            );
    }

    #[test]
    fn dataref_place_reborrow_preserves_identity() {
        with_ctx!(ctx;
        let ty = ctx.heap().register_untracked::<Storage<i64>>();
        let object = ctx.heap().alloc(ty, Storage { field: 1 });
        let erased = ctx.heap().erase(&object).unwrap();
        let ops = FieldOps { ty };
        let mut place = MutPlace::dataref(erased, &ops);
        {
            let mut forwarded = place.reborrow();
            forwarded.update_copy(&mut ctx, |value| value + 1).unwrap();
        }
        place.update_copy(&mut ctx, |value| value + 1).unwrap();

        assert_eq!(ctx.heap().with(&object, |storage| storage.field), 3);
            );
    }

    #[test]
    fn dataref_place_keeps_object_live() {
        with_ctx!(ctx;
        let ty = ctx.heap().register_untracked::<Storage<i64>>();
        let object = ctx.heap().alloc(ty, Storage { field: 1 });
        let erased = ctx.heap().erase(&object).unwrap();
        let ops = FieldOps { ty };
        let mut place = MutPlace::dataref(erased, &ops);
        drop(object);
        ctx.heap().collect(0);

        place.update_copy(&mut ctx, |value| value + 1).unwrap();
        assert_eq!(place.get_copy(&mut ctx).unwrap(), 2);
            );
    }

    #[test]
    fn dataref_place_errors_for_wrong_heap_type_and_dead_handle() {
        with_ctx!(ctx;
        let int_ty = ctx.heap().register_untracked::<Storage<i64>>();
        let bool_ty = ctx.heap().register_untracked::<Storage<bool>>();
        let object = ctx.heap().alloc(int_ty, Storage { field: 1 });
        let erased = ctx.heap().erase(&object).unwrap();
        let wrong_ops = FieldOps { ty: bool_ty };
        let wrong = MutPlace::dataref(erased, &wrong_ops);
        assert_eq!(
            wrong.get_copy(&mut ctx).unwrap_err().message(),
            "heap object handle is no longer live"
        );
        drop(wrong);

        let foreign = ManuallyDrop::new(ctx.heap().erase(&object).unwrap());
        crate::Heap::scope(|heap| {
            let mut ctx = Ctx::new(heap);
            let ty = ctx.heap().register_untracked::<Storage<i64>>();
            let ops = FieldOps { ty };
            let foreign = unsafe { copy_erased_to_current_heap(&foreign) };
            let wrong = MutPlace::dataref(foreign, &ops);
            assert_eq!(
                wrong.get_copy(&mut ctx).unwrap_err().message(),
                "heap object belongs to a different heap"
            );
        });

        let erased = ctx.heap().erase(&object).unwrap();
        let dead = manual_copy(&erased);
        drop(object);
        drop(erased);
        ctx.heap().collect(0);
        let ops = FieldOps { ty: int_ty };
        let dead = MutPlace::dataref(ManuallyDrop::into_inner(dead), &ops);
        assert_eq!(
            dead.get_copy(&mut ctx).unwrap_err().message(),
            "heap object handle is no longer live"
        );
            );
    }

    #[test]
    fn dataref_place_region_restores_after_error() {
        with_ctx!(ctx;
        let ty = ctx.heap().register_untracked::<Storage<i64>>();
        let object = ctx.heap().alloc(ty, Storage { field: 1 });
        let erased = ctx.heap().erase(&object).unwrap();
        let ops = FieldOps { ty };
        let mut place = MutPlace::dataref(erased, &ops);
        let err = place
            .mutate(&mut ctx, |_| Err::<(), _>(RuntimeError::new("early")))
            .unwrap_err();

        assert_eq!(err.message(), "early");
        place.set(&mut ctx, 2).unwrap();
        assert_eq!(ctx.heap().with(&object, |storage| storage.field), 2);
            );
    }

    #[test]
    fn scoped_cell_mutates_dataref_place() {
        with_ctx!(ctx;
        let ty = ctx.heap().register_untracked::<Storage<i64>>();
        let object = ctx.heap().alloc(ty, Storage { field: 1 });
        let erased = ctx.heap().erase(&object).unwrap();
        let ops = FieldOps { ty };
        let cell = ScopedMutPlaceCell::new(MutPlace::dataref(erased, &ops));

        cell.set(&mut ctx, 2).unwrap();
        let mut forwarded = MutPlace::scoped_cell(&cell);
        forwarded.update_copy(&mut ctx, |value| value + 1).unwrap();

        assert_eq!(ctx.heap().with(&object, |storage| storage.field), 3);
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

    macro_rules! assert_unsupported_slice_views {
        ($place:expr) => {{
            let mut place = $place;
            assert_unsupported_slice_view(place.slice_view(0, 2, false));
            assert_unsupported_slice_view(place.slice_view_mut(0, 2, false));
        }};
    }

    fn assert_unsupported_slice_view<T>(result: Result<AnvSlice<T>, RuntimeError>) {
        let Err(err) = result else {
            panic!("expected unsupported slice view");
        };
        assert_eq!(
            err.message(),
            "slice view over non-local mutable collection parameter is unsupported"
        );
    }

    fn set_slice_second(slice: &mut AnvSlice<i64>) {
        slice
            .with_elem_mut_short(1, |value| {
                *value = 9;
                Ok(())
            })
            .unwrap();
    }

    #[test]
    fn local_slice_views_succeed() {
        let mut list = AnvList::from_elems([1_i64, 2, 3]);
        let mut place = MutPlace::local(&mut list);

        let slice = place.slice_view(1, 3, false).unwrap();
        assert_eq!(slice.elem_at_shared(0).unwrap(), 2);
        assert_eq!(slice.elem_at_shared(1).unwrap(), 3);
        set_slice_second(&mut place.slice_view_mut(0, 2, false).unwrap());
        assert_eq!(*list.get(1).unwrap(), 9);

        let mut array = [1_i64, 2, 3];
        let mut place = MutPlace::local(&mut array);

        let slice = place.slice_view(1, 3, false).unwrap();
        assert_eq!(slice.elem_at_shared(0).unwrap(), 2);
        assert_eq!(slice.elem_at_shared(1).unwrap(), 3);
        set_slice_second(&mut place.slice_view_mut(0, 2, false).unwrap());
        assert_eq!(array[1], 9);
    }

    #[test]
    fn stack_cell_slice_views_are_unsupported() {
        let list_cell = StackLambdaCell::new(AnvList::from_elems([1_i64, 2, 3]));
        assert_unsupported_slice_views!(MutPlace::stack_cell(&list_cell));

        let array_cell = StackLambdaCell::new([1_i64, 2, 3]);
        assert_unsupported_slice_views!(MutPlace::stack_cell(&array_cell));
    }

    #[test]
    fn heap_cell_slice_views_are_unsupported() {
        with_ctx!(ctx;
        let list_ty = ctx.heap().register_untracked::<LambdaCell<AnvList<i64>>>();
        let list_cell = ctx
            .heap()
            .alloc(list_ty, LambdaCell::new(AnvList::from_elems([1_i64, 2, 3])));
        assert_unsupported_slice_views!(MutPlace::heap_cell(list_cell));

        let array_ty = ctx.heap().register_untracked::<LambdaCell<[i64; 3]>>();
        let array_cell = ctx.heap().alloc(array_ty, LambdaCell::new([1_i64, 2, 3]));
        assert_unsupported_slice_views!(MutPlace::heap_cell(array_cell));
            );
    }

    #[test]
    fn scoped_cell_slice_views_are_unsupported() {
        let mut list = AnvList::from_elems([1_i64, 2, 3]);
        let list_cell = ScopedMutPlaceCell::new(MutPlace::local(&mut list));
        assert_unsupported_slice_views!(MutPlace::scoped_cell(&list_cell));

        let mut array = [1_i64, 2, 3];
        let array_cell = ScopedMutPlaceCell::new(MutPlace::local(&mut array));
        assert_unsupported_slice_views!(MutPlace::scoped_cell(&array_cell));
    }

    #[test]
    fn dataref_slice_views_are_unsupported() {
        with_ctx!(ctx;
        let list_ty = ctx.heap().register_untracked::<Storage<AnvList<i64>>>();
        let list_object = ctx.heap().alloc(
            list_ty,
            Storage {
                field: AnvList::from_elems([1_i64, 2, 3]),
            },
        );
        let list_erased = ctx.heap().erase(&list_object).unwrap();
        let list_ops = FieldOps { ty: list_ty };
        assert_unsupported_slice_views!(MutPlace::dataref(list_erased, &list_ops));

        let array_ty = ctx.heap().register_untracked::<Storage<[i64; 3]>>();
        let array_object = ctx.heap().alloc(
            array_ty,
            Storage {
                field: [1_i64, 2, 3],
            },
        );
        let array_erased = ctx.heap().erase(&array_object).unwrap();
        let array_ops = FieldOps { ty: array_ty };
        assert_unsupported_slice_views!(MutPlace::dataref(array_erased, &array_ops));
            );
    }
}
