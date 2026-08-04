use std::{cell::UnsafeCell, hash::Hash, marker::PhantomData, rc::Rc};

use crate::{
    AnvList, AnvMap, AnvSlice, Ctx, ErasedHandle, GlobalSlot, Handle, LambdaCell, RuntimeError,
    SafepointGuardKind, SafepointState, ShapeLoanGuard, StackLambdaCell, ValueLoanGuard,
    heap_access_error, lambda_cell::CellBorrowFlag,
};

pub enum MutPlace<'place, 'cx, T> {
    Local(*mut T, PhantomData<(&'place mut T, &'cx ())>),
    StackCell(&'place StackLambdaCell<T>, PhantomData<&'cx ()>),
    HeapCell(Handle<'cx, LambdaCell<T>>),
    Global(GlobalPlace<'place, 'cx, T>),
    Projected(Box<dyn ProjectedPlaceObject<'cx, T> + 'place>),
    ProjectedBorrow(&'place dyn ProjectedPlaceObject<'cx, T>),
    ScopedCell(&'place ScopedMutPlaceCell<'place, 'cx, T>),
}

type GlobalInit<'place, 'cx, T> =
    dyn for<'rt> Fn(&mut Ctx<'cx, 'rt>) -> Result<T, RuntimeError> + 'place;

pub struct GlobalPlace<'place, 'cx, T> {
    slot: &'place GlobalSlot<T>,
    init: &'place GlobalInit<'place, 'cx, T>,
}

pub struct DataRefPlace<'ops, 'cx, T> {
    object: ErasedHandle<'cx>,
    ops: &'ops dyn DataRefPlaceOps<'cx, T>,
    _not_send_sync: PhantomData<Rc<()>>,
}

pub struct ProjectedPlace<'place, 'cx, R, T> {
    root: UnsafeCell<MutPlace<'place, 'cx, R>>,
    ops: &'place dyn ProjectionOps<'cx, R, T>,
    _not_send_sync: PhantomData<Rc<()>>,
}

/// # Safety
///
/// Implementations must call the leaf callback at most once, exactly once on success, and must not
/// reenter Anvyx while a projected reference is live.
pub unsafe trait ProjectedPlaceObject<'cx, T> {
    fn access(
        &self,
        ctx: &mut Ctx<'cx, '_>,
        f: &mut dyn FnMut(&T) -> Result<(), RuntimeError>,
    ) -> Result<(), RuntimeError>;

    fn mutate(
        &self,
        ctx: &mut Ctx<'cx, '_>,
        f: &mut dyn FnMut(&mut T) -> Result<(), RuntimeError>,
    ) -> Result<(), RuntimeError>;
}

/// # Safety
///
/// Implementations must call the leaf callback at most once, exactly once on success, propagate its
/// failure, and must not reenter Anvyx while `root` or a projected reference is live.
pub unsafe trait ProjectionOps<'cx, R, T> {
    fn access(
        &self,
        ctx: &mut Ctx<'cx, '_>,
        root: &R,
        f: &mut dyn FnMut(&T) -> Result<(), RuntimeError>,
    ) -> Result<(), RuntimeError>;

    fn mutate(
        &self,
        ctx: &mut Ctx<'cx, '_>,
        root: &mut R,
        f: &mut dyn FnMut(&mut T) -> Result<(), RuntimeError>,
    ) -> Result<(), RuntimeError>;
}

#[derive(Default)]
pub struct OptionalPayloadOps<T>(PhantomData<T>);

pub struct MapValueOps<K> {
    key: K,
    expected_version: u64,
    value_loan: u64,
}

impl<K> MapValueOps<K> {
    pub fn new(key: K, loan: &ValueLoanGuard) -> Self {
        Self {
            key,
            expected_version: loan.version(),
            value_loan: loan.id(),
        }
    }
}

/// # Safety
///
/// Implementations must call the leaf callback at most once, exactly once on success, propagate its
/// failure, and must not reenter Anvyx while a projected reference is live.
pub unsafe trait DataRefPlaceOps<'cx, T> {
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
    safepoint: SafepointState,
    _not_send_sync: PhantomData<Rc<()>>,
}

enum ScopedMutPlaceRoot<'source, 'cx, T> {
    Local(*mut T, PhantomData<&'source mut T>, PhantomData<&'cx ()>),
    StackCell(&'source StackLambdaCell<T>, PhantomData<&'cx ()>),
    HeapCell(Handle<'cx, LambdaCell<T>>),
    Global(GlobalPlace<'source, 'cx, T>),
    Projected(Box<dyn ProjectedPlaceObject<'cx, T> + 'source>),
    ProjectedBorrow(&'source dyn ProjectedPlaceObject<'cx, T>),
    ScopedCell(&'source ScopedMutPlaceCell<'source, 'cx, T>),
}

fn heap_access<'cx, 'rt, T: 'cx, R>(
    ctx: &mut Ctx<'cx, 'rt>,
    cell: &Handle<'cx, LambdaCell<T>>,
    f: impl FnOnce(&LambdaCell<T>) -> Result<R, RuntimeError>,
) -> Result<R, RuntimeError> {
    let cell = ctx
        .heap_ref()
        .try_with(cell, std::ptr::from_ref::<LambdaCell<T>>)
        .map_err(heap_access_error)?;
    f(unsafe { &*cell })
}

impl<'place, 'cx, R: 'cx, T: 'cx> ProjectedPlace<'place, 'cx, R, T> {
    pub fn new(root: MutPlace<'place, 'cx, R>, ops: &'place dyn ProjectionOps<'cx, R, T>) -> Self {
        Self {
            root: UnsafeCell::new(root),
            ops,
            _not_send_sync: PhantomData,
        }
    }
}

unsafe impl<'cx, R: 'cx, T: 'cx> ProjectedPlaceObject<'cx, T> for ProjectedPlace<'_, 'cx, R, T> {
    fn access(
        &self,
        ctx: &mut Ctx<'cx, '_>,
        f: &mut dyn FnMut(&T) -> Result<(), RuntimeError>,
    ) -> Result<(), RuntimeError> {
        unsafe {
            (&*self.root.get()).access_with_ctx(ctx, |ctx, root| self.ops.access(ctx, root, f))
        }
    }

    fn mutate(
        &self,
        ctx: &mut Ctx<'cx, '_>,
        f: &mut dyn FnMut(&mut T) -> Result<(), RuntimeError>,
    ) -> Result<(), RuntimeError> {
        unsafe {
            (&mut *self.root.get()).mutate_with_ctx(ctx, |ctx, root| self.ops.mutate(ctx, root, f))
        }
    }
}

fn projected_access<'cx, T: 'cx, R>(
    place: &dyn ProjectedPlaceObject<'cx, T>,
    ctx: &mut Ctx<'cx, '_>,
    f: impl FnOnce(&T) -> Result<R, RuntimeError>,
) -> Result<R, RuntimeError> {
    let mut f = Some(f);
    let mut out = None;
    place.access(ctx, &mut |slot| {
        let f = f.take().expect("projected place access invoked twice");
        out = Some(f(slot)?);
        Ok(())
    })?;
    Ok(out.expect("projected place access did not invoke callback"))
}

fn projected_mutate<'cx, T: 'cx, R>(
    place: &dyn ProjectedPlaceObject<'cx, T>,
    ctx: &mut Ctx<'cx, '_>,
    f: impl FnOnce(&mut T) -> Result<R, RuntimeError>,
) -> Result<R, RuntimeError> {
    let mut f = Some(f);
    let mut out = None;
    place.mutate(ctx, &mut |slot| {
        let f = f.take().expect("projected place mutation invoked twice");
        out = Some(f(slot)?);
        Ok(())
    })?;
    Ok(out.expect("projected place mutation did not invoke callback"))
}

unsafe impl<'cx, T: 'cx> ProjectionOps<'cx, Option<T>, T> for OptionalPayloadOps<T> {
    fn access(
        &self,
        _ctx: &mut Ctx<'cx, '_>,
        root: &Option<T>,
        f: &mut dyn FnMut(&T) -> Result<(), RuntimeError>,
    ) -> Result<(), RuntimeError> {
        let Some(payload) = root.as_ref() else {
            return Err(RuntimeError::new("optional payload is nil"));
        };
        f(payload)
    }

    fn mutate(
        &self,
        _ctx: &mut Ctx<'cx, '_>,
        root: &mut Option<T>,
        f: &mut dyn FnMut(&mut T) -> Result<(), RuntimeError>,
    ) -> Result<(), RuntimeError> {
        let Some(payload) = root.as_mut() else {
            return Err(RuntimeError::new("optional payload is nil"));
        };
        f(payload)
    }
}

unsafe impl<'cx, K, V> ProjectionOps<'cx, AnvMap<'cx, K, V>, V> for MapValueOps<K>
where
    K: Eq + Hash + 'cx,
    V: 'cx,
{
    fn access(
        &self,
        ctx: &mut Ctx<'cx, '_>,
        root: &AnvMap<'cx, K, V>,
        f: &mut dyn FnMut(&V) -> Result<(), RuntimeError>,
    ) -> Result<(), RuntimeError> {
        root.with_value_shared_by_key(ctx, &self.key, self.expected_version, self.value_loan, f)
    }

    fn mutate(
        &self,
        ctx: &mut Ctx<'cx, '_>,
        root: &mut AnvMap<'cx, K, V>,
        f: &mut dyn FnMut(&mut V) -> Result<(), RuntimeError>,
    ) -> Result<(), RuntimeError> {
        root.with_value_mut_by_key(ctx, &self.key, self.expected_version, self.value_loan, f)
    }
}

impl<'ops, 'cx, T: 'cx> DataRefPlace<'ops, 'cx, T> {
    pub fn new(object: ErasedHandle<'cx>, ops: &'ops dyn DataRefPlaceOps<'cx, T>) -> Self {
        Self {
            object,
            ops,
            _not_send_sync: PhantomData,
        }
    }
}

unsafe impl<'cx, T: 'cx> ProjectedPlaceObject<'cx, T> for DataRefPlace<'_, 'cx, T> {
    fn access(
        &self,
        ctx: &mut Ctx<'cx, '_>,
        f: &mut dyn FnMut(&T) -> Result<(), RuntimeError>,
    ) -> Result<(), RuntimeError> {
        self.ops.access(ctx, &self.object, f)
    }

    fn mutate(
        &self,
        ctx: &mut Ctx<'cx, '_>,
        f: &mut dyn FnMut(&mut T) -> Result<(), RuntimeError>,
    ) -> Result<(), RuntimeError> {
        self.ops.mutate(ctx, &self.object, f)
    }
}

impl<'cx, T: 'cx> MutPlace<'_, 'cx, AnvList<'cx, T>> {
    /// # Safety
    ///
    /// The place must remain valid while the returned loan is active.
    pub unsafe fn begin_shape_loan_with_ctx<'rt>(
        &mut self,
        ctx: &mut Ctx<'cx, 'rt>,
    ) -> Result<ShapeLoanGuard, RuntimeError> {
        let owner =
            unsafe { self.access_with_ctx(ctx, |_ctx, list| Ok(list.__anvyx_staged_owner())) }?;
        let loan = owner.begin_shape_loan()?;
        unsafe {
            self.mutate(ctx, |list| {
                *list = owner.__anvyx_commit_staged_owner();
                Ok(())
            })?;
        }
        Ok(loan)
    }

    /// # Safety
    ///
    /// The place and its source storage must outlive the returned slice, and the callback region
    /// must not reenter Anvyx.
    pub unsafe fn slice_view_with_ctx<'rt>(
        &self,
        ctx: &mut Ctx<'cx, 'rt>,
        start: i64,
        end: i64,
        inclusive: bool,
    ) -> Result<AnvSlice<'cx, T>, RuntimeError> {
        unsafe {
            self.access_with_ctx(ctx, |_ctx, list| {
                let range = crate::checked_range(start, end, inclusive, list.len())?;
                AnvSlice::from_list(list, range.start, range.len())
            })
        }
    }

    /// # Safety
    ///
    /// The materializer must not reenter Anvyx or access the list while storage is reconstructed.
    /// The place and its source storage must outlive the returned slice.
    pub unsafe fn slice_view_mut_with<'rt>(
        &mut self,
        ctx: &mut Ctx<'cx, 'rt>,
        start: i64,
        end: i64,
        inclusive: bool,
        materialize: impl FnMut(&T) -> T,
    ) -> Result<AnvSlice<'cx, T>, RuntimeError> {
        let mut owner =
            unsafe { self.access_with_ctx(ctx, |_ctx, list| Ok(list.__anvyx_staged_owner())) }?;
        let range = crate::checked_range(start, end, inclusive, owner.len())?;
        let slice = unsafe {
            AnvSlice::from_list_mut_with(ctx, &mut owner, range.start, range.len(), materialize)?
        };
        unsafe {
            self.mutate(ctx, |list| {
                *list = owner.__anvyx_commit_staged_owner();
                Ok(())
            })?;
        }
        Ok(slice)
    }
}

impl<'cx, K: Eq + Hash + 'cx, V: 'cx> MutPlace<'_, 'cx, AnvMap<'cx, K, V>> {
    /// # Safety
    ///
    /// The place must remain valid while the returned loan is active.
    pub unsafe fn begin_shape_loan_with_ctx<'rt>(
        &mut self,
        ctx: &mut Ctx<'cx, 'rt>,
    ) -> Result<ShapeLoanGuard, RuntimeError> {
        let owner =
            unsafe { self.access_with_ctx(ctx, |_ctx, map| Ok(map.__anvyx_staged_owner())) }?;
        let loan = owner.begin_shape_loan()?;
        unsafe {
            self.mutate(ctx, |map| {
                *map = owner.__anvyx_commit_staged_owner();
                Ok(())
            })?;
        }
        Ok(loan)
    }

    /// # Safety
    ///
    /// The materializers must not reenter Anvyx or access the map while storage is reconstructed.
    /// Key materialization must preserve equality and hash identity. The place must outlive the
    /// returned loan.
    pub unsafe fn map_value_loan_with<'rt>(
        &mut self,
        ctx: &mut Ctx<'cx, 'rt>,
        key: &K,
        materialize_key: impl FnMut(&K) -> K,
        materialize_value: impl FnMut(&V) -> V,
    ) -> Result<ValueLoanGuard, RuntimeError> {
        let mut owner =
            unsafe { self.access_with_ctx(ctx, |_ctx, map| Ok(map.__anvyx_staged_owner())) }?;
        let loan = unsafe {
            owner.begin_value_loan_by_key_with(ctx, key, materialize_key, materialize_value)?
        };
        unsafe {
            self.mutate(ctx, |map| {
                *map = owner.__anvyx_commit_staged_owner();
                Ok(())
            })?;
        }
        Ok(loan)
    }
}

impl<'cx, T: 'cx, const N: usize> MutPlace<'_, 'cx, [T; N]> {
    /// # Safety
    ///
    /// The returned raw slice descriptor must not outlive this place, and the source array must not
    /// be moved or invalidated while the descriptor is used.
    pub unsafe fn slice_view_with_ctx<'rt>(
        &self,
        ctx: &mut Ctx<'cx, 'rt>,
        start: i64,
        end: i64,
        inclusive: bool,
    ) -> Result<AnvSlice<'cx, T>, RuntimeError> {
        unsafe {
            self.access_with_ctx(ctx, |_ctx, array| {
                let range = crate::checked_range(start, end, inclusive, N)?;
                Ok(AnvSlice::from_raw_parts(
                    array.as_ptr(),
                    N,
                    range.start,
                    range.len(),
                ))
            })
        }
    }

    /// # Safety
    ///
    /// The returned raw slice descriptor must not outlive this place, and no other access may move
    /// or invalidate the source array while the descriptor is used.
    pub unsafe fn slice_view_mut<'rt>(
        &mut self,
        ctx: &mut Ctx<'cx, 'rt>,
        start: i64,
        end: i64,
        inclusive: bool,
    ) -> Result<AnvSlice<'cx, T>, RuntimeError> {
        unsafe {
            self.mutate_with_ctx(ctx, |_ctx, array| {
                let range = crate::checked_range(start, end, inclusive, N)?;
                Ok(AnvSlice::from_raw_parts_mut(
                    array.as_mut_ptr(),
                    N,
                    range.start,
                    range.len(),
                ))
            })
        }
    }
}

impl<T> Copy for GlobalPlace<'_, '_, T> {}

impl<T> Clone for GlobalPlace<'_, '_, T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<'cx, T: 'cx> GlobalPlace<'_, 'cx, T> {
    fn access<'rt, R>(
        &self,
        ctx: &mut Ctx<'cx, 'rt>,
        f: impl FnOnce(&T) -> Result<R, RuntimeError>,
    ) -> Result<R, RuntimeError> {
        let value = self.slot.read(|| (self.init)(ctx))?;
        f(&value)
    }

    fn mutate<'rt, R>(
        &self,
        ctx: &mut Ctx<'cx, 'rt>,
        f: impl FnOnce(&mut T) -> Result<R, RuntimeError>,
    ) -> Result<R, RuntimeError> {
        let mut value = self.slot.write(|| (self.init)(ctx))?;
        f(&mut value)
    }
}

impl<'place, 'cx, T: 'cx> MutPlace<'place, 'cx, T> {
    pub fn local(value: &'place mut T) -> Self {
        Self::Local(std::ptr::from_mut(value), PhantomData)
    }

    /// # Safety
    ///
    /// `value` must stay valid for the returned place's lifetime, and all access to it through
    /// this place must obey Rust's aliasing rules at runtime.
    pub unsafe fn local_raw(value: *mut T) -> Self {
        Self::Local(value, PhantomData)
    }

    pub fn stack_cell(cell: &'place StackLambdaCell<T>) -> Self {
        Self::StackCell(cell, PhantomData)
    }

    pub fn heap_cell(cell: Handle<'cx, LambdaCell<T>>) -> Self {
        Self::HeapCell(cell)
    }

    pub fn global(slot: &'place GlobalSlot<T>, init: &'place GlobalInit<'place, 'cx, T>) -> Self {
        Self::Global(GlobalPlace { slot, init })
    }

    pub fn dataref(object: ErasedHandle<'cx>, ops: &'place dyn DataRefPlaceOps<'cx, T>) -> Self {
        Self::Projected(Box::new(DataRefPlace::new(object, ops)))
    }

    pub fn projected<R: 'cx>(
        root: MutPlace<'place, 'cx, R>,
        ops: &'place dyn ProjectionOps<'cx, R, T>,
    ) -> Self {
        Self::Projected(Box::new(ProjectedPlace::new(root, ops)))
    }

    pub fn scoped_cell(cell: &'place ScopedMutPlaceCell<'place, 'cx, T>) -> Self {
        Self::ScopedCell(cell)
    }

    pub fn reborrow(&mut self) -> MutPlace<'_, 'cx, T> {
        match self {
            Self::Local(value, _) => MutPlace::Local(*value, PhantomData),
            Self::StackCell(cell, _) => MutPlace::stack_cell(cell),
            Self::HeapCell(cell) => MutPlace::heap_cell(cell.clone()),
            Self::Global(global) => MutPlace::Global(*global),
            Self::Projected(place) => MutPlace::ProjectedBorrow(&**place),
            Self::ProjectedBorrow(place) => MutPlace::ProjectedBorrow(*place),
            Self::ScopedCell(cell) => MutPlace::scoped_cell(cell),
        }
    }

    /// # Safety
    ///
    /// The callback must not reenter Anvyx while the place reference is live.
    pub unsafe fn access<'rt, R>(
        &self,
        ctx: &mut Ctx<'cx, 'rt>,
        f: impl FnOnce(&T) -> Result<R, RuntimeError>,
    ) -> Result<R, RuntimeError> {
        match self {
            Self::Local(value, _) => f(unsafe { &**value }),
            Self::StackCell(cell, _) => cell.access(f),
            Self::HeapCell(cell) => heap_access(ctx, cell, |cell| cell.access(f)),
            Self::Global(global) => global.access(ctx, f),
            Self::Projected(place) => projected_access(&**place, ctx, f),
            Self::ProjectedBorrow(place) => projected_access(*place, ctx, f),
            Self::ScopedCell(cell) => cell.access(ctx, f),
        }
    }

    /// # Safety
    ///
    /// The callback must not reenter Anvyx while the place reference is live.
    pub unsafe fn mutate<'rt, R>(
        &mut self,
        ctx: &mut Ctx<'cx, 'rt>,
        f: impl FnOnce(&mut T) -> Result<R, RuntimeError>,
    ) -> Result<R, RuntimeError> {
        match self {
            Self::Local(value, _) => f(unsafe { &mut **value }),
            Self::StackCell(cell, _) => cell.mutate(f),
            Self::HeapCell(cell) => heap_access(ctx, cell, |cell| cell.mutate(f)),
            Self::Global(global) => global.mutate(ctx, f),
            Self::Projected(place) => projected_mutate(&**place, ctx, f),
            Self::ProjectedBorrow(place) => projected_mutate(*place, ctx, f),
            Self::ScopedCell(cell) => cell.mutate(ctx, f),
        }
    }

    /// # Safety
    ///
    /// The callback may use the context only for leaf operations that cannot reenter Anvyx.
    pub unsafe fn access_with_ctx<'rt, R>(
        &self,
        ctx: &mut Ctx<'cx, 'rt>,
        f: impl FnOnce(&mut Ctx<'cx, 'rt>, &T) -> Result<R, RuntimeError>,
    ) -> Result<R, RuntimeError> {
        let ctx_ptr = std::ptr::from_mut::<Ctx<'cx, 'rt>>(ctx);
        unsafe { self.access(ctx, |value| f(&mut *ctx_ptr, value)) }
    }

    /// # Safety
    ///
    /// The callback may use the context only for leaf operations that cannot reenter Anvyx.
    pub unsafe fn mutate_with_ctx<'rt, R>(
        &mut self,
        ctx: &mut Ctx<'cx, 'rt>,
        f: impl FnOnce(&mut Ctx<'cx, 'rt>, &mut T) -> Result<R, RuntimeError>,
    ) -> Result<R, RuntimeError> {
        let ctx_ptr = std::ptr::from_mut::<Ctx<'cx, 'rt>>(ctx);
        unsafe { self.mutate(ctx, |value| f(&mut *ctx_ptr, value)) }
    }

    pub fn set<'rt>(&mut self, ctx: &mut Ctx<'cx, 'rt>, value: T) -> Result<(), RuntimeError> {
        unsafe {
            self.mutate(ctx, |slot| {
                *slot = value;
                Ok(())
            })
        }
    }

    pub fn replace<'rt>(&mut self, ctx: &mut Ctx<'cx, 'rt>, value: T) -> Result<T, RuntimeError> {
        unsafe { self.mutate(ctx, |slot| Ok(std::mem::replace(slot, value))) }
    }
}

impl<'cx, T: Copy + 'cx> MutPlace<'_, 'cx, T> {
    pub fn get_copy<'rt>(&self, ctx: &mut Ctx<'cx, 'rt>) -> Result<T, RuntimeError> {
        unsafe { self.access(ctx, |value| Ok(*value)) }
    }

    pub fn update_copy<'rt>(
        &mut self,
        ctx: &mut Ctx<'cx, 'rt>,
        f: impl FnOnce(T) -> T,
    ) -> Result<(), RuntimeError> {
        unsafe {
            self.mutate(ctx, |value| {
                *value = f(*value);
                Ok(())
            })
        }
    }
}

impl<'source, 'cx, T: 'cx> ScopedMutPlaceCell<'source, 'cx, T> {
    pub fn new(place: MutPlace<'source, 'cx, T>) -> Self {
        Self::new_with_safepoint(place, SafepointState::default())
    }

    pub fn new_with_safepoint(place: MutPlace<'source, 'cx, T>, safepoint: SafepointState) -> Self {
        Self {
            root: ScopedMutPlaceRoot::from_place(place),
            borrow: CellBorrowFlag::default(),
            safepoint,
            _not_send_sync: PhantomData,
        }
    }

    pub fn access<'rt, R>(
        &self,
        ctx: &mut Ctx<'cx, 'rt>,
        f: impl FnOnce(&T) -> Result<R, RuntimeError>,
    ) -> Result<R, RuntimeError> {
        let _safepoint = self.safepoint.enter(SafepointGuardKind::MutPlace)?;
        let _guard = self.borrow.shared_guard()?;
        match self.root {
            ScopedMutPlaceRoot::Local(value, _, _) => f(unsafe { &*value }),
            ScopedMutPlaceRoot::StackCell(cell, _) => cell.access(f),
            ScopedMutPlaceRoot::HeapCell(ref cell) => heap_access(ctx, cell, |cell| cell.access(f)),
            ScopedMutPlaceRoot::Global(global) => global.access(ctx, f),
            ScopedMutPlaceRoot::Projected(ref place) => projected_access(&**place, ctx, f),
            ScopedMutPlaceRoot::ProjectedBorrow(place) => projected_access(place, ctx, f),
            ScopedMutPlaceRoot::ScopedCell(cell) => cell.access(ctx, f),
        }
    }

    pub fn mutate<'rt, R>(
        &self,
        ctx: &mut Ctx<'cx, 'rt>,
        f: impl FnOnce(&mut T) -> Result<R, RuntimeError>,
    ) -> Result<R, RuntimeError> {
        let _safepoint = self.safepoint.enter(SafepointGuardKind::MutPlace)?;
        let _guard = self.borrow.mutable_guard()?;
        match self.root {
            ScopedMutPlaceRoot::Local(value, _, _) => f(unsafe { &mut *value }),
            ScopedMutPlaceRoot::StackCell(cell, _) => cell.mutate(f),
            ScopedMutPlaceRoot::HeapCell(ref cell) => heap_access(ctx, cell, |cell| cell.mutate(f)),
            ScopedMutPlaceRoot::Global(global) => global.mutate(ctx, f),
            ScopedMutPlaceRoot::Projected(ref place) => projected_mutate(&**place, ctx, f),
            ScopedMutPlaceRoot::ProjectedBorrow(place) => projected_mutate(place, ctx, f),
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
            MutPlace::Global(global) => Self::Global(global),
            MutPlace::Projected(place) => Self::Projected(place),
            MutPlace::ProjectedBorrow(place) => Self::ProjectedBorrow(place),
            MutPlace::ScopedCell(cell) => Self::ScopedCell(cell),
        }
    }
}

impl<'cx, T: Copy + 'cx> ScopedMutPlaceCell<'_, 'cx, T> {
    pub fn get_copy<'rt>(&self, ctx: &mut Ctx<'cx, 'rt>) -> Result<T, RuntimeError> {
        self.access(ctx, |value| Ok(*value))
    }
}
