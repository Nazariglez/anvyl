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

#[cfg(test)]
mod tests {
    use std::mem::ManuallyDrop;

    use crate::{
        AnvList, AnvMap, Ctx, DataRefPlaceOps, ErasedHandle, GlobalSlot, HeapType, LambdaCell,
        ListStorage, MapStorage, MapValueOps, MutPlace, OptionalPayloadOps, ProjectionOps,
        RuntimeError, SafepointState, ScopedMutPlaceCell, StackLambdaCell, heap_access_error,
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
    fn scoped_cell_registers_safepoint_blocker() {
        crate::Heap::scope(|heap| {
            let safepoint = SafepointState::default();
            let mut ctx = Ctx::new_with_safepoint(heap, &safepoint);
            let mut value = 1_i64;
            let cell = ScopedMutPlaceCell::new_with_safepoint(
                MutPlace::local(&mut value),
                safepoint.clone(),
            );

            cell.access(&mut ctx, |_| {
                assert_eq!(
                    safepoint.validate_collect().unwrap_err().message(),
                    "cannot collect while mutable place guard is active"
                );
                Ok(())
            })
            .unwrap();
            assert!(safepoint.validate_collect().is_ok());
        });
    }

    #[test]
    fn optional_payload_ops_reject_nil_payload() {
        with_ctx!(ctx;
        let mut source: Option<i64> = None;
        let ops = OptionalPayloadOps::<i64>::default();
        let place = MutPlace::projected(MutPlace::local(&mut source), &ops);

        let err = place.get_copy(&mut ctx).unwrap_err();

        assert_eq!(err.message(), "optional payload is nil");
            );
    }

    #[test]
    fn global_place_reports_active_borrow_conflict() {
        with_ctx!(ctx;
            let slot = GlobalSlot::new("score");
            let init = |_: &mut Ctx<'_, '_>| Ok(7);
            let mut place = MutPlace::global(&slot, &init);
            let guard = slot.read(|| Ok(1)).unwrap();

            let err = unsafe {
                place.mutate(&mut ctx, |value| {
                    *value = 2;
                    Ok(())
                })
            }
            .expect_err("active read guard should block mutation");
            assert!(err.to_string().contains("active borrow"));
            drop(guard);
        );
    }

    fn list_storage_ty<'cx>(ctx: &mut Ctx<'cx, '_>) -> HeapType<'cx, ListStorage<'cx, i64>> {
        ctx.heap().register_untracked::<ListStorage<'_, i64>>()
    }

    fn list<'cx>(
        ctx: &mut Ctx<'cx, '_>,
        elems: impl IntoIterator<Item = i64>,
    ) -> AnvList<'cx, i64> {
        let ty = list_storage_ty(ctx);
        AnvList::from_elems(ctx, ty, elems)
    }

    fn map_storage_ty<'cx, V: 'cx>(
        ctx: &mut Ctx<'cx, '_>,
    ) -> HeapType<'cx, MapStorage<'cx, &'static str, V>> {
        ctx.heap()
            .register_untracked::<MapStorage<'_, &'static str, V>>()
    }

    fn map<'cx, V: 'cx>(
        ctx: &mut Ctx<'cx, '_>,
        entries: impl IntoIterator<Item = (&'static str, V)>,
    ) -> AnvMap<'cx, &'static str, V> {
        let ty = map_storage_ty(ctx);
        AnvMap::from_entries(ctx, ty, entries)
    }

    struct ListElemOps {
        index: i64,
        version: u64,
    }

    unsafe impl<'cx> ProjectionOps<'cx, AnvList<'cx, i64>, i64> for ListElemOps {
        fn access(
            &self,
            ctx: &mut Ctx<'cx, '_>,
            root: &AnvList<'cx, i64>,
            f: &mut dyn FnMut(&i64) -> Result<(), RuntimeError>,
        ) -> Result<(), RuntimeError> {
            let index = crate::checked_index_result(self.index, root.len(), "list")?;
            unsafe { root.with_elem_shared_short(ctx, index, self.version, f) }
        }

        fn mutate(
            &self,
            ctx: &mut Ctx<'cx, '_>,
            root: &mut AnvList<'cx, i64>,
            f: &mut dyn FnMut(&mut i64) -> Result<(), RuntimeError>,
        ) -> Result<(), RuntimeError> {
            let index = crate::checked_index_result(self.index, root.len(), "list")?;
            // SAFETY: Projection callbacks are leaf operations and cannot reenter the list.
            unsafe { root.with_elem_mut_leaf(ctx, index, self.version, |value| *value, f) }
        }
    }

    #[test]
    fn projected_list_stale_version_rejects_access_and_mutate() {
        with_ctx!(ctx;
        let mut list = list(&mut ctx, [1_i64, 2]);
        let ops = ListElemOps {
            index: 0,
            version: list.structural_version(),
        };
        unsafe { list.push_with(&mut ctx, 3, |value| *value) }.unwrap();
        let mut place = MutPlace::projected(MutPlace::local(&mut list), &ops);

        assert!(place.get_copy(&mut ctx).is_err());
        assert!(place.set(&mut ctx, 4).is_err());
            );
    }

    #[test]
    fn projected_list_callback_error_restores_access() {
        with_ctx!(ctx;
        let mut list = list(&mut ctx, [1_i64, 2]);
        let ops = ListElemOps {
            index: 1,
            version: list.structural_version(),
        };
        let mut place = MutPlace::projected(MutPlace::local(&mut list), &ops);

        let err = unsafe {
            place.mutate(&mut ctx, |value| {
                *value = 4;
                Err::<(), _>(RuntimeError::new("early"))
            })
        }
        .unwrap_err();
        assert_eq!(err.message(), "early");
        assert_eq!(place.get_copy(&mut ctx).unwrap(), 4);
        place.set(&mut ctx, 5).unwrap();
        drop(place);
        assert_eq!(
            unsafe {
                list.elem_at_shared_with(&ctx, 1, list.structural_version(), |value| *value)
            }
            .unwrap(),
            5
        );
            );
    }

    #[test]
    fn projected_map_value_rejects_missing_and_inactive_loans() {
        with_ctx!(ctx;
        let mut map = map(&mut ctx, [("a", 1_i64)]);
        let guard = unsafe {
            map.begin_value_loan_by_key_with(&mut ctx, &"a", |key| *key, |value| *value)
        }
        .unwrap();
        let missing_ops = MapValueOps::new("missing", &guard);
        {
            let mut place = MutPlace::projected(MutPlace::local(&mut map), &missing_ops);

            assert_eq!(place.get_copy(&mut ctx).unwrap_err().message(), "map entry key is missing");
            assert_eq!(place.set(&mut ctx, 3).unwrap_err().message(), "map entry key is missing");
        }

        let ops = MapValueOps::new("a", &guard);
        drop(guard);
        let mut place = MutPlace::projected(MutPlace::local(&mut map), &ops);

        assert!(place.get_copy(&mut ctx).is_err());
        assert!(place.set(&mut ctx, 4).is_err());
            );
    }

    #[test]
    fn projected_map_callback_error_preserves_mutation() {
        with_ctx!(ctx;
        let mut map = map(&mut ctx, [("a", 1_i64)]);
        let guard = unsafe {
            map.begin_value_loan_by_key_with(&mut ctx, &"a", |key| *key, |value| *value)
        }
        .unwrap();
        let ops = MapValueOps::new("a", &guard);
        {
            let mut place = MutPlace::projected(MutPlace::local(&mut map), &ops);
            let err = unsafe {
                place.mutate(&mut ctx, |value| {
                    *value = 4;
                    Err::<(), _>(RuntimeError::new("early"))
                })
            }
            .unwrap_err();

            assert_eq!(err.message(), "early");
            assert_eq!(place.get_copy(&mut ctx).unwrap(), 4);
            place.set(&mut ctx, 5).unwrap();
        }
        drop(guard);
        assert_eq!(unsafe { map.get_with(&ctx, &"a", |value| *value) }.unwrap(), Some(5));
            );
    }

    #[test]
    fn scoped_cell_wraps_projected_map_value() {
        with_ctx!(ctx;
        let mut map = map(&mut ctx, [("a", 1_i64)]);
        let guard = unsafe {
            map.begin_value_loan_by_key_with(&mut ctx, &"a", |key| *key, |value| *value)
        }
        .unwrap();
        let ops = MapValueOps::new("a", &guard);
        {
            let place = MutPlace::projected(MutPlace::local(&mut map), &ops);
            let cell = ScopedMutPlaceCell::new(place);

            assert_eq!(cell.get_copy(&mut ctx).unwrap(), 1);
            cell.set(&mut ctx, 5).unwrap();
        }

        drop(guard);
        assert_eq!(unsafe { map.get_with(&ctx, &"a", |value| *value) }.unwrap(), Some(5));
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
        drop(place);
        assert_eq!(value.0, 3);
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
        let err = unsafe {
            place.mutate(&mut ctx, |_| Err::<(), _>(RuntimeError::new("early")))
        }
        .unwrap_err();

        assert_eq!(err.message(), "early");
        place.set(&mut ctx, 2).unwrap();
        drop(place);
        assert_eq!(value, 2);
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

    unsafe impl<'cx, T: 'cx> DataRefPlaceOps<'cx, T> for FieldOps<'cx, T> {
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

    struct StagedListOps<'cx> {
        ty: HeapType<'cx, Storage<AnvList<'cx, i64>>>,
    }

    unsafe impl<'cx> DataRefPlaceOps<'cx, AnvList<'cx, i64>> for StagedListOps<'cx> {
        fn access(
            &self,
            ctx: &mut Ctx<'cx, '_>,
            object: &ErasedHandle<'cx>,
            f: &mut dyn FnMut(&AnvList<'cx, i64>) -> Result<(), RuntimeError>,
        ) -> Result<(), RuntimeError> {
            let value = ctx
                .heap_ref()
                .try_with_erased(object, self.ty, |storage| {
                    Ok(storage.field.__anvyx_staged_owner())
                })
                .map_err(heap_access_error)??;
            f(&value)
        }

        fn mutate(
            &self,
            ctx: &mut Ctx<'cx, '_>,
            object: &ErasedHandle<'cx>,
            f: &mut dyn FnMut(&mut AnvList<'cx, i64>) -> Result<(), RuntimeError>,
        ) -> Result<(), RuntimeError> {
            let mut value = ctx
                .heap_ref()
                .try_with_erased(object, self.ty, |storage| {
                    Ok(storage.field.__anvyx_staged_owner())
                })
                .map_err(heap_access_error)??;
            let result = f(&mut value);
            let writeback = ctx
                .heap()
                .try_with_erased_mut(object, self.ty, |storage| {
                    storage.field = value.__anvyx_commit_staged_owner();
                    Ok(())
                })
                .map_err(heap_access_error)?;
            match (result, writeback) {
                (Ok(()), Ok(())) => Ok(()),
                (Err(error), _) | (_, Err(error)) => Err(error),
            }
        }
    }

    struct ListPayload<'cx> {
        values: AnvList<'cx, i64>,
    }

    struct ListPayloadOps;

    unsafe impl<'cx> ProjectionOps<'cx, ListPayload<'cx>, AnvList<'cx, i64>> for ListPayloadOps {
        fn access(
            &self,
            _ctx: &mut Ctx<'cx, '_>,
            root: &ListPayload<'cx>,
            f: &mut dyn FnMut(&AnvList<'cx, i64>) -> Result<(), RuntimeError>,
        ) -> Result<(), RuntimeError> {
            f(&root.values)
        }

        fn mutate(
            &self,
            _ctx: &mut Ctx<'cx, '_>,
            root: &mut ListPayload<'cx>,
            f: &mut dyn FnMut(&mut AnvList<'cx, i64>) -> Result<(), RuntimeError>,
        ) -> Result<(), RuntimeError> {
            f(&mut root.values)
        }
    }

    struct StagedPayloadOps<'cx> {
        ty: HeapType<'cx, Storage<ListPayload<'cx>>>,
    }

    unsafe impl<'cx> DataRefPlaceOps<'cx, ListPayload<'cx>> for StagedPayloadOps<'cx> {
        fn access(
            &self,
            ctx: &mut Ctx<'cx, '_>,
            object: &ErasedHandle<'cx>,
            f: &mut dyn FnMut(&ListPayload<'cx>) -> Result<(), RuntimeError>,
        ) -> Result<(), RuntimeError> {
            let value = ctx
                .heap_ref()
                .try_with_erased(object, self.ty, |storage| {
                    Ok(ListPayload {
                        values: storage.field.values.__anvyx_staged_owner(),
                    })
                })
                .map_err(heap_access_error)??;
            f(&value)
        }

        fn mutate(
            &self,
            ctx: &mut Ctx<'cx, '_>,
            object: &ErasedHandle<'cx>,
            f: &mut dyn FnMut(&mut ListPayload<'cx>) -> Result<(), RuntimeError>,
        ) -> Result<(), RuntimeError> {
            let mut value = ctx
                .heap_ref()
                .try_with_erased(object, self.ty, |storage| {
                    Ok(ListPayload {
                        values: storage.field.values.__anvyx_staged_owner(),
                    })
                })
                .map_err(heap_access_error)??;
            let result = f(&mut value);
            let writeback = ctx
                .heap()
                .try_with_erased_mut(object, self.ty, |storage| {
                    storage.field = ListPayload {
                        values: value.values.__anvyx_commit_staged_owner(),
                    };
                    Ok(())
                })
                .map_err(heap_access_error)?;
            match (result, writeback) {
                (Ok(()), Ok(())) => Ok(()),
                (Err(error), _) | (_, Err(error)) => Err(error),
            }
        }
    }

    #[test]
    fn dataref_collection_stage_preserves_version_and_reentry_gate() {
        with_ctx!(ctx;
        let list_ty = ctx.heap().register_untracked::<ListStorage<'_, i64>>();
        let list = AnvList::from_elems(&mut ctx, list_ty, [1]);
        let storage_ty = ctx
            .heap()
            .register_untracked::<Storage<AnvList<'_, i64>>>();
        let object = ctx.heap().alloc(storage_ty, Storage { field: list });
        let ops = StagedListOps { ty: storage_ty };
        let erased = ctx.heap().erase(&object).unwrap();
        let mut place = MutPlace::dataref(erased, &ops);

        unsafe {
            place
                .mutate_with_ctx(&mut ctx, |ctx, list| {
                    list.push_with(ctx, 2, |value| *value)
                })
                .unwrap();
        }
        let version = unsafe {
            place
                .access_with_ctx(&mut ctx, |_ctx, list| Ok(list.structural_version()))
                .unwrap()
        };
        assert_eq!(version, 1);

        let loan = unsafe { place.begin_shape_loan_with_ctx(&mut ctx) }.unwrap();
        let err = unsafe {
            place.mutate_with_ctx(&mut ctx, |ctx, list| {
                list.push_with(ctx, 3, |value| *value)
            })
        }
        .unwrap_err();
        assert_eq!(
            err.message(),
            "cannot structurally mutate collection during active iteration or slice view"
        );
        drop(loan);
            );
    }

    #[test]
    fn aggregate_dataref_stage_preserves_nested_collection_gate() {
        with_ctx!(ctx;
        let list_ty = ctx.heap().register_untracked::<ListStorage<'_, i64>>();
        let list = AnvList::from_elems(&mut ctx, list_ty, [1]);
        let storage_ty = ctx
            .heap()
            .register_untracked::<Storage<ListPayload<'_>>>();
        let object = ctx.heap().alloc(
            storage_ty,
            Storage {
                field: ListPayload { values: list },
            },
        );
        let ops = StagedPayloadOps { ty: storage_ty };
        let fields = ListPayloadOps;
        let first = ctx.heap().erase(&object).unwrap();
        let second = ctx.heap().erase(&object).unwrap();
        let mut first = MutPlace::dataref(first, &ops);
        let mut second = MutPlace::dataref(second, &ops);

        unsafe {
            first
                .mutate_with_ctx(&mut ctx, |ctx, payload| {
                    payload.values.push_with(ctx, 2, |value| *value)
                })
                .unwrap();
        }
        let version = unsafe {
            first
                .access_with_ctx(&mut ctx, |_ctx, payload| {
                    Ok(payload.values.structural_version())
                })
                .unwrap()
        };
        assert_eq!(version, 1);

        let mut nested = MutPlace::projected(first.reborrow(), &fields);
        let loan = unsafe { nested.begin_shape_loan_with_ctx(&mut ctx) }.unwrap();
        let err = unsafe {
            second.mutate_with_ctx(&mut ctx, |ctx, payload| {
                payload.values.push_with(ctx, 3, |value| *value)
            })
        }
        .unwrap_err();
        assert_eq!(
            err.message(),
            "cannot structurally mutate collection during active iteration or slice view"
        );
        drop(loan);
            );
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
    fn dataref_descriptor_observes_intervening_alias_write() {
        with_ctx!(ctx;
        let ty = ctx.heap().register_untracked::<Storage<i64>>();
        let object = ctx.heap().alloc(ty, Storage { field: 1 });
        let erased = ctx.heap().erase(&object).unwrap();
        let ops = FieldOps { ty };
        let mut place = MutPlace::dataref(erased, &ops);

        assert_eq!(place.get_copy(&mut ctx).unwrap(), 1);
        ctx.heap().with_mut(&object, |storage| storage.field = 4);
        assert_eq!(place.get_copy(&mut ctx).unwrap(), 4);
        place.set(&mut ctx, 5).unwrap();
        assert_eq!(ctx.heap().with(&object, |storage| storage.field), 5);
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
    fn dataref_place_keeps_object_live() {
        with_ctx!(ctx;
        let ty = ctx.heap().register_untracked::<Storage<i64>>();
        let object = ctx.heap().alloc(ty, Storage { field: 1 });
        let erased = ctx.heap().erase(&object).unwrap();
        let ops = FieldOps { ty };
        let mut place = MutPlace::dataref(erased, &ops);
        drop(object);
        ctx.collect(0).unwrap();

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
        ctx.collect(0).unwrap();
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
        let err = unsafe {
            place.mutate(&mut ctx, |_| Err::<(), _>(RuntimeError::new("early")))
        }
        .unwrap_err();

        assert_eq!(err.message(), "early");
        place.set(&mut ctx, 2).unwrap();
        assert_eq!(ctx.heap().with(&object, |storage| storage.field), 2);
            );
    }

    #[test]
    fn heap_cell_guard_restores_after_error() {
        with_ctx!(ctx;
        let cell_ty = ctx.heap().register_untracked::<LambdaCell<i64>>();
        let cell = ctx.heap().alloc(cell_ty, LambdaCell::new(1));
        let mut place = MutPlace::heap_cell(cell.clone());
        let err = unsafe {
            place.mutate(&mut ctx, |_| Err::<(), _>(RuntimeError::new("early")))
        }
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

    macro_rules! assert_list_slice_views {
        ($ctx:expr, $place:expr) => {{
            let mut place = $place;
            let view = unsafe { place.slice_view_with_ctx($ctx, 0, 2, false) }.unwrap();
            assert_eq!(view.len(), 2);
            drop(view);
            let view =
                unsafe { place.slice_view_mut_with($ctx, 0, 2, false, |value| *value) }.unwrap();
            assert_eq!(view.len(), 2);
        }};
    }

    macro_rules! assert_array_slice_views {
        ($ctx:expr, $place:expr) => {{
            let mut place = $place;
            let view = unsafe { place.slice_view_with_ctx($ctx, 0, 2, false) }.unwrap();
            assert_eq!(view.len(), 2);
            drop(view);
            let view = unsafe { place.slice_view_mut($ctx, 0, 2, false) }.unwrap();
            assert_eq!(view.len(), 2);
        }};
    }

    #[test]
    fn stack_cell_slice_views_work() {
        with_ctx!(ctx;
        let list_cell = StackLambdaCell::new(list(&mut ctx, [1_i64, 2, 3]));
        assert_list_slice_views!(&mut ctx, MutPlace::stack_cell(&list_cell));

        let array_cell = StackLambdaCell::new([1_i64, 2, 3]);
        assert_array_slice_views!(&mut ctx, MutPlace::stack_cell(&array_cell));
            );
    }

    #[test]
    fn heap_cell_slice_views_work() {
        with_ctx!(ctx;
        let list_ty = ctx.heap().register_untracked::<LambdaCell<AnvList<'_, i64>>>();
        let list_value = list(&mut ctx, [1_i64, 2, 3]);
        let list_cell = ctx.heap().alloc(list_ty, LambdaCell::new(list_value));
        assert_list_slice_views!(&mut ctx, MutPlace::heap_cell(list_cell));

        let array_ty = ctx.heap().register_untracked::<LambdaCell<[i64; 3]>>();
        let array_cell = ctx.heap().alloc(array_ty, LambdaCell::new([1_i64, 2, 3]));
        assert_array_slice_views!(&mut ctx, MutPlace::heap_cell(array_cell));
            );
    }

    #[test]
    fn scoped_cell_slice_views_work() {
        with_ctx!(ctx;
        let mut list = list(&mut ctx, [1_i64, 2, 3]);
        let list_cell = ScopedMutPlaceCell::new(MutPlace::local(&mut list));
        assert_list_slice_views!(&mut ctx, MutPlace::scoped_cell(&list_cell));

        let mut array = [1_i64, 2, 3];
        let array_cell = ScopedMutPlaceCell::new(MutPlace::local(&mut array));
        assert_array_slice_views!(&mut ctx, MutPlace::scoped_cell(&array_cell));
            );
    }

    #[test]
    fn dataref_slice_views_work() {
        with_ctx!(ctx;
        let list_ty = ctx.heap().register_untracked::<Storage<AnvList<'_, i64>>>();
        let list_value = list(&mut ctx, [1_i64, 2, 3]);
        let list_object = ctx.heap().alloc(list_ty, Storage { field: list_value });
        let list_erased = ctx.heap().erase(&list_object).unwrap();
        let list_ops = FieldOps { ty: list_ty };
        assert_list_slice_views!(&mut ctx, MutPlace::dataref(list_erased, &list_ops));

        let array_ty = ctx.heap().register_untracked::<Storage<[i64; 3]>>();
        let array_object = ctx.heap().alloc(
            array_ty,
            Storage {
                field: [1_i64, 2, 3],
            },
        );
        let array_erased = ctx.heap().erase(&array_object).unwrap();
        let array_ops = FieldOps { ty: array_ty };
        assert_array_slice_views!(&mut ctx, MutPlace::dataref(array_erased, &array_ops));
            );
    }
}
