use std::{cell::UnsafeCell, hash::Hash, marker::PhantomData, rc::Rc};

use crate::{
    AnvList, AnvMap, AnvSlice, Ctx, ErasedHandle, GlobalSlot, Handle, LambdaCell, RuntimeError,
    StackLambdaCell, ValueLoanGuard, heap_access_error, lambda_cell::CellBorrowFlag,
};

pub enum MutPlace<'place, 'cx, T> {
    Local(*mut T, PhantomData<(&'place mut T, &'cx ())>),
    StackCell(&'place StackLambdaCell<T>, PhantomData<&'cx ()>),
    HeapCell(Handle<'cx, LambdaCell<T>>),
    Global(GlobalPlace<'place, 'cx, T>),
    Projected(Box<dyn ProjectedPlaceObject<'cx, T> + 'place>),
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

pub trait ProjectedPlaceObject<'cx, T> {
    fn reborrow<'a>(&'a self) -> Box<dyn ProjectedPlaceObject<'cx, T> + 'a>;

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

pub trait ProjectionOps<'cx, R, T> {
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
    Global(GlobalPlace<'source, 'cx, T>),
    Projected(Box<dyn ProjectedPlaceObject<'cx, T> + 'source>),
    ScopedCell(&'source ScopedMutPlaceCell<'source, 'cx, T>),
}

fn heap_access<'cx, 'rt, T: 'cx, R>(
    ctx: &mut Ctx<'cx, 'rt>,
    cell: &Handle<'cx, LambdaCell<T>>,
    f: impl FnOnce(&LambdaCell<T>) -> Result<R, RuntimeError>,
) -> Result<R, RuntimeError> {
    ctx.heap().try_with(cell, f).map_err(heap_access_error)?
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

impl<'cx, R: 'cx, T: 'cx> ProjectedPlaceObject<'cx, T> for ProjectedPlace<'_, 'cx, R, T> {
    fn reborrow<'a>(&'a self) -> Box<dyn ProjectedPlaceObject<'cx, T> + 'a> {
        let root = unsafe { &mut *self.root.get() }.reborrow();
        Box::new(ProjectedPlace::new(root, self.ops))
    }

    fn access(
        &self,
        ctx: &mut Ctx<'cx, '_>,
        f: &mut dyn FnMut(&T) -> Result<(), RuntimeError>,
    ) -> Result<(), RuntimeError> {
        unsafe { &*self.root.get() }.access_with_ctx(ctx, |ctx, root| self.ops.access(ctx, root, f))
    }

    fn mutate(
        &self,
        ctx: &mut Ctx<'cx, '_>,
        f: &mut dyn FnMut(&mut T) -> Result<(), RuntimeError>,
    ) -> Result<(), RuntimeError> {
        unsafe { &mut *self.root.get() }
            .mutate_with_ctx(ctx, |ctx, root| self.ops.mutate(ctx, root, f))
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

impl<'cx, T: 'cx> ProjectionOps<'cx, Option<T>, T> for OptionalPayloadOps<T> {
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

impl<'cx, K, V> ProjectionOps<'cx, AnvMap<'cx, K, V>, V> for MapValueOps<K>
where
    K: Eq + Hash + Clone + 'cx,
    V: Clone + 'cx,
{
    fn access(
        &self,
        ctx: &mut Ctx<'cx, '_>,
        root: &AnvMap<'cx, K, V>,
        f: &mut dyn FnMut(&V) -> Result<(), RuntimeError>,
    ) -> Result<(), RuntimeError> {
        root.with_value_shared_by_key(
            ctx,
            &self.key,
            self.expected_version,
            self.value_loan,
            |value| f(value),
        )
    }

    fn mutate(
        &self,
        ctx: &mut Ctx<'cx, '_>,
        root: &mut AnvMap<'cx, K, V>,
        f: &mut dyn FnMut(&mut V) -> Result<(), RuntimeError>,
    ) -> Result<(), RuntimeError> {
        root.with_value_owned_mut_short_by_key(
            ctx,
            &self.key,
            self.expected_version,
            self.value_loan,
            |value| f(value),
        )
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

impl<'cx, T: 'cx> ProjectedPlaceObject<'cx, T> for DataRefPlace<'_, 'cx, T> {
    fn reborrow<'a>(&'a self) -> Box<dyn ProjectedPlaceObject<'cx, T> + 'a> {
        Box::new(Self::new(self.object.clone(), self.ops))
    }

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
    pub fn slice_view(
        &self,
        start: i64,
        end: i64,
        inclusive: bool,
    ) -> Result<AnvSlice<'cx, T>, RuntimeError> {
        match self {
            Self::Local(list, _) => {
                let list = unsafe { &**list };
                let range = crate::checked_range(start, end, inclusive, list.len());
                AnvSlice::from_list(list, range.start, range.len())
            }
            Self::StackCell(..)
            | Self::HeapCell(_)
            | Self::Global(_)
            | Self::Projected(_)
            | Self::ScopedCell(_) => Err(non_local_slice_view_error()),
        }
    }

    pub fn slice_view_mut<'rt>(
        &mut self,
        ctx: &mut Ctx<'cx, 'rt>,
        start: i64,
        end: i64,
        inclusive: bool,
    ) -> Result<AnvSlice<'cx, T>, RuntimeError>
    where
        T: Clone,
    {
        match self {
            Self::Local(list, _) => {
                let list = unsafe { &mut **list };
                let range = crate::checked_range(start, end, inclusive, list.len());
                AnvSlice::from_list_mut(ctx, list, range.start, range.len())
            }
            Self::StackCell(..)
            | Self::HeapCell(_)
            | Self::Global(_)
            | Self::Projected(_)
            | Self::ScopedCell(_) => Err(non_local_slice_view_error()),
        }
    }
}

impl<'cx, T: 'cx, const N: usize> MutPlace<'_, 'cx, [T; N]> {
    /// # Safety
    ///
    /// The returned raw slice descriptor must not outlive this place, and the source array must not
    /// be moved or invalidated while the descriptor is used.
    pub unsafe fn slice_view(
        &self,
        start: i64,
        end: i64,
        inclusive: bool,
    ) -> Result<AnvSlice<'cx, T>, RuntimeError> {
        match self {
            Self::Local(array, _) => {
                let array = unsafe { &**array };
                let range = crate::checked_range(start, end, inclusive, N);
                Ok(
                    unsafe {
                        AnvSlice::from_raw_parts(array.as_ptr(), N, range.start, range.len())
                    },
                )
            }
            Self::StackCell(..)
            | Self::HeapCell(_)
            | Self::Global(_)
            | Self::Projected(_)
            | Self::ScopedCell(_) => Err(non_local_slice_view_error()),
        }
    }

    /// # Safety
    ///
    /// The returned raw slice descriptor must not outlive this place, and no other access may move
    /// or invalidate the source array while the descriptor is used.
    pub unsafe fn slice_view_mut<'rt>(
        &mut self,
        _ctx: &mut Ctx<'cx, 'rt>,
        start: i64,
        end: i64,
        inclusive: bool,
    ) -> Result<AnvSlice<'cx, T>, RuntimeError> {
        match self {
            Self::Local(array, _) => {
                let array = unsafe { &mut **array };
                let range = crate::checked_range(start, end, inclusive, N);
                Ok(unsafe {
                    AnvSlice::from_raw_parts_mut(array.as_mut_ptr(), N, range.start, range.len())
                })
            }
            Self::StackCell(..)
            | Self::HeapCell(_)
            | Self::Global(_)
            | Self::Projected(_)
            | Self::ScopedCell(_) => Err(non_local_slice_view_error()),
        }
    }
}

fn non_local_slice_view_error() -> RuntimeError {
    RuntimeError::new("slice view over non-local mutable collection parameter is unsupported")
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

    fn access_with_ctx<'rt, R>(
        &self,
        ctx: &mut Ctx<'cx, 'rt>,
        f: impl FnOnce(&mut Ctx<'cx, 'rt>, &T) -> Result<R, RuntimeError>,
    ) -> Result<R, RuntimeError> {
        let ctx_ptr = std::ptr::from_mut::<Ctx<'cx, 'rt>>(ctx);
        self.access(ctx, |value| f(unsafe { &mut *ctx_ptr }, value))
    }

    fn mutate_with_ctx<'rt, R>(
        &self,
        ctx: &mut Ctx<'cx, 'rt>,
        f: impl FnOnce(&mut Ctx<'cx, 'rt>, &mut T) -> Result<R, RuntimeError>,
    ) -> Result<R, RuntimeError> {
        let ctx_ptr = std::ptr::from_mut::<Ctx<'cx, 'rt>>(ctx);
        self.mutate(ctx, |value| f(unsafe { &mut *ctx_ptr }, value))
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
            Self::Projected(place) => MutPlace::Projected(place.reborrow()),
            Self::ScopedCell(cell) => MutPlace::scoped_cell(cell),
        }
    }

    pub fn access<'rt, R>(
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
            Self::ScopedCell(cell) => cell.access(ctx, f),
        }
    }

    pub fn mutate<'rt, R>(
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
            Self::ScopedCell(cell) => cell.mutate(ctx, f),
        }
    }

    pub fn access_with_ctx<'rt, R>(
        &self,
        ctx: &mut Ctx<'cx, 'rt>,
        f: impl FnOnce(&mut Ctx<'cx, 'rt>, &T) -> Result<R, RuntimeError>,
    ) -> Result<R, RuntimeError> {
        let ctx_ptr = std::ptr::from_mut::<Ctx<'cx, 'rt>>(ctx);
        match self {
            Self::Local(value, _) => f(ctx, unsafe { &**value }),
            Self::StackCell(cell, _) => cell.access(|value| f(ctx, value)),
            Self::HeapCell(cell) => heap_access(ctx, cell, |cell| {
                cell.access(|value| f(unsafe { &mut *ctx_ptr }, value))
            }),
            Self::Global(global) => global.access_with_ctx(ctx, f),
            Self::Projected(place) => {
                projected_access(&**place, ctx, |value| f(unsafe { &mut *ctx_ptr }, value))
            }
            Self::ScopedCell(cell) => cell.access(ctx, |value| f(unsafe { &mut *ctx_ptr }, value)),
        }
    }

    pub fn mutate_with_ctx<'rt, R>(
        &mut self,
        ctx: &mut Ctx<'cx, 'rt>,
        f: impl FnOnce(&mut Ctx<'cx, 'rt>, &mut T) -> Result<R, RuntimeError>,
    ) -> Result<R, RuntimeError> {
        let ctx_ptr = std::ptr::from_mut::<Ctx<'cx, 'rt>>(ctx);
        match self {
            Self::Local(value, _) => f(ctx, unsafe { &mut **value }),
            Self::StackCell(cell, _) => cell.mutate(|value| f(ctx, value)),
            Self::HeapCell(cell) => heap_access(ctx, cell, |cell| {
                cell.mutate(|value| f(unsafe { &mut *ctx_ptr }, value))
            }),
            Self::Global(global) => global.mutate_with_ctx(ctx, f),
            Self::Projected(place) => {
                projected_mutate(&**place, ctx, |value| f(unsafe { &mut *ctx_ptr }, value))
            }
            Self::ScopedCell(cell) => cell.mutate(ctx, |value| f(unsafe { &mut *ctx_ptr }, value)),
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
            ScopedMutPlaceRoot::Global(global) => global.access(ctx, f),
            ScopedMutPlaceRoot::Projected(ref place) => projected_access(&**place, ctx, f),
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
            ScopedMutPlaceRoot::Global(global) => global.mutate(ctx, f),
            ScopedMutPlaceRoot::Projected(ref place) => projected_mutate(&**place, ctx, f),
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
    use std::{cell::Cell, mem::ManuallyDrop};

    use crate::{
        AnvList, AnvMap, AnvSlice, Ctx, DataRefPlaceOps, ErasedHandle, GlobalSlot, HeapType,
        LambdaCell, ListStorage, MapStorage, MapValueOps, MutPlace, OptionalPayloadOps,
        ProjectionOps, RuntimeError, ScopedMutPlaceCell, StackLambdaCell, heap_access_error,
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
    fn optional_payload_ops_project_some_payload() {
        with_ctx!(ctx;
        let mut source = Some(1);
        let ops = OptionalPayloadOps::<i64>::default();
        let mut place = MutPlace::projected(MutPlace::local(&mut source), &ops);

        place.update_copy(&mut ctx, |value| value + 1).unwrap();
        drop(place);

        assert_eq!(source, Some(2));
            );
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
    fn global_place_initializes_and_reopens_short_guards() {
        with_ctx!(ctx;
            let calls = Cell::new(0);
            let slot = GlobalSlot::new("score");
            let init = |_: &mut Ctx<'_, '_>| {
                calls.set(calls.get() + 1);
                Ok(7)
            };
            let mut place = MutPlace::global(&slot, &init);

            assert_eq!(place.access(&mut ctx, |value| Ok(*value)).unwrap(), 7);
            place.mutate(&mut ctx, |value| {
                *value += 1;
                Ok(())
            }).unwrap();
            assert_eq!(place.access(&mut ctx, |value| Ok(*value)).unwrap(), 8);
            assert_eq!(calls.get(), 1);
        );
    }

    #[test]
    fn global_place_reports_active_borrow_conflict() {
        with_ctx!(ctx;
            let slot = GlobalSlot::new("score");
            let init = |_: &mut Ctx<'_, '_>| Ok(7);
            let mut place = MutPlace::global(&slot, &init);
            let guard = slot.read(|| Ok(1)).unwrap();

            let err = place.mutate(&mut ctx, |value| {
                *value = 2;
                Ok(())
            }).expect_err("active read guard should block mutation");
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

    struct Pair {
        x: i64,
        y: i64,
    }

    struct PairYOps {
        access_calls: Cell<usize>,
        mutate_calls: Cell<usize>,
    }

    struct ListElemOps {
        index: i64,
        version: u64,
    }

    struct SliceElemOps {
        index: i64,
    }

    impl ProjectionOps<'_, Pair, i64> for PairYOps {
        fn access(
            &self,
            _ctx: &mut Ctx<'_, '_>,
            root: &Pair,
            f: &mut dyn FnMut(&i64) -> Result<(), RuntimeError>,
        ) -> Result<(), RuntimeError> {
            self.access_calls.set(self.access_calls.get() + 1);
            f(&root.y)
        }

        fn mutate(
            &self,
            _ctx: &mut Ctx<'_, '_>,
            root: &mut Pair,
            f: &mut dyn FnMut(&mut i64) -> Result<(), RuntimeError>,
        ) -> Result<(), RuntimeError> {
            self.mutate_calls.set(self.mutate_calls.get() + 1);
            f(&mut root.y)
        }
    }

    impl<'cx> ProjectionOps<'cx, AnvList<'cx, i64>, i64> for ListElemOps {
        fn access(
            &self,
            ctx: &mut Ctx<'cx, '_>,
            root: &AnvList<'cx, i64>,
            f: &mut dyn FnMut(&i64) -> Result<(), RuntimeError>,
        ) -> Result<(), RuntimeError> {
            let index = crate::checked_index_result(self.index, root.len(), "list")?;
            root.with_elem_shared_short(ctx, index, self.version, f)
        }

        fn mutate(
            &self,
            ctx: &mut Ctx<'cx, '_>,
            root: &mut AnvList<'cx, i64>,
            f: &mut dyn FnMut(&mut i64) -> Result<(), RuntimeError>,
        ) -> Result<(), RuntimeError> {
            let index = crate::checked_index_result(self.index, root.len(), "list")?;
            root.with_elem_owned_mut_ctx_short(ctx, index, self.version, |_, value| f(value))
        }
    }

    impl<'cx> ProjectionOps<'cx, AnvSlice<'cx, i64>, i64> for SliceElemOps {
        fn access(
            &self,
            ctx: &mut Ctx<'cx, '_>,
            root: &AnvSlice<'cx, i64>,
            f: &mut dyn FnMut(&i64) -> Result<(), RuntimeError>,
        ) -> Result<(), RuntimeError> {
            let value = root.elem_at_shared(ctx, self.index)?;
            f(&value)
        }

        fn mutate(
            &self,
            ctx: &mut Ctx<'cx, '_>,
            root: &mut AnvSlice<'cx, i64>,
            f: &mut dyn FnMut(&mut i64) -> Result<(), RuntimeError>,
        ) -> Result<(), RuntimeError> {
            root.with_elem_owned_mut_ctx_short(ctx, self.index, |_, value| f(value))
        }
    }

    #[test]
    fn projected_field_access_mutate_and_reborrow() {
        with_ctx!(ctx;
        let mut pair = Pair { x: 1, y: 2 };
        let ops = PairYOps {
            access_calls: Cell::new(0),
            mutate_calls: Cell::new(0),
        };
        {
            let mut place = MutPlace::projected(MutPlace::local(&mut pair), &ops);

            assert_eq!(place.get_copy(&mut ctx).unwrap(), 2);
            place.update_copy(&mut ctx, |value| value + 10).unwrap();
            {
                let mut reborrowed = place.reborrow();
                reborrowed.set(&mut ctx, 5).unwrap();
            }
        }

        assert_eq!(pair.x, 1);
        assert_eq!(pair.y, 5);
        assert_eq!(ops.access_calls.get(), 1);
        assert_eq!(ops.mutate_calls.get(), 2);
            );
    }

    #[test]
    fn projected_list_stale_version_rejects_access_and_mutate() {
        with_ctx!(ctx;
        let mut list = list(&mut ctx, [1_i64, 2]);
        let ops = ListElemOps {
            index: 0,
            version: list.structural_version(),
        };
        list.push(&mut ctx, 3).unwrap();
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

        let err = place
            .mutate(&mut ctx, |_| Err::<(), _>(RuntimeError::new("early")))
            .unwrap_err();
        assert_eq!(err.message(), "early");
        place.set(&mut ctx, 5).unwrap();
        drop(place);
        assert_eq!(list.elem_at_shared(&ctx, 1, list.structural_version()).unwrap(), 5);
            );
    }

    #[test]
    fn projected_map_value_updates_existing_entry() {
        with_ctx!(ctx;
        let mut map = map(&mut ctx, [("a", 1_i64)]);
        let guard = map.begin_value_loan_by_key(&mut ctx, &"a").unwrap();
        let ops = MapValueOps::new("a", &guard);
        {
            let mut place = MutPlace::projected(MutPlace::local(&mut map), &ops);

            assert_eq!(place.get_copy(&mut ctx).unwrap(), 1);
            place.update_copy(&mut ctx, |value| value + 1).unwrap();
            let err = place
                .mutate(&mut ctx, |value| {
                    *value = 9;
                    Err::<(), _>(RuntimeError::new("early"))
                })
                .unwrap_err();
            assert_eq!(err.message(), "early");
        }

        assert_eq!(map.get(&ctx, &"a").unwrap(), Some(2));
        assert_eq!(map.structural_version(), 0);
            );
    }

    #[test]
    fn projected_map_value_rejects_missing_and_inactive_loans() {
        with_ctx!(ctx;
        let mut map = map(&mut ctx, [("a", 1_i64)]);
        let guard = map.begin_value_loan_by_key(&mut ctx, &"a").unwrap();
        let missing_ops = MapValueOps::new("missing", &guard);
        {
            let mut place = MutPlace::projected(MutPlace::local(&mut map), &missing_ops);

            assert_eq!(place.get_copy(&mut ctx).unwrap_err().message(), "map entry key is missing");
            assert_eq!(place.set(&mut ctx, 3).unwrap_err().message(), "map entry key is missing");
        }
        assert_eq!(map.get(&ctx, &"missing").unwrap(), None);

        let ops = MapValueOps::new("a", &guard);
        drop(guard);
        let mut place = MutPlace::projected(MutPlace::local(&mut map), &ops);

        assert!(place.get_copy(&mut ctx).is_err());
        assert!(place.set(&mut ctx, 4).is_err());
            );
    }

    #[test]
    fn projected_map_value_detaches_shared_outer_map() {
        with_ctx!(ctx;
        let mut map = map(&mut ctx, [("a", 1_i64)]);
        let shared = map.share();
        let guard = map.begin_value_loan_by_key(&mut ctx, &"a").unwrap();
        let ops = MapValueOps::new("a", &guard);
        {
            let mut place = MutPlace::projected(MutPlace::local(&mut map), &ops);
            place.set(&mut ctx, 2).unwrap();
        }

        assert_eq!(map.get(&ctx, &"a").unwrap(), Some(2));
        assert_eq!(shared.get(&ctx, &"a").unwrap(), Some(1));
            );
    }

    #[test]
    fn projected_map_optional_value_writes_back_present_nil() {
        with_ctx!(ctx;
        let mut map = map(&mut ctx, [("a", Some(1_i64))]);
        let guard = map.begin_value_loan_by_key(&mut ctx, &"a").unwrap();
        let ops = MapValueOps::new("a", &guard);
        {
            let mut place = MutPlace::projected(MutPlace::local(&mut map), &ops);
            place.set(&mut ctx, None).unwrap();
        }
        drop(guard);

        assert_eq!(map.get(&ctx, &"a").unwrap(), Some(None));
        assert_eq!(map.remove(&mut ctx, &"a").unwrap(), Some(None));
            );
    }

    #[test]
    fn scoped_cell_wraps_projected_map_value() {
        with_ctx!(ctx;
        let mut map = map(&mut ctx, [("a", 1_i64)]);
        let guard = map.begin_value_loan_by_key(&mut ctx, &"a").unwrap();
        let ops = MapValueOps::new("a", &guard);
        {
            let place = MutPlace::projected(MutPlace::local(&mut map), &ops);
            let cell = ScopedMutPlaceCell::new(place);

            assert_eq!(cell.get_copy(&mut ctx).unwrap(), 1);
            cell.set(&mut ctx, 5).unwrap();
        }

        assert_eq!(map.get(&ctx, &"a").unwrap(), Some(5));
            );
    }

    #[test]
    fn projected_slice_bounds_checked_per_operation() {
        with_ctx!(ctx;
        let mut list = list(&mut ctx, [1_i64, 2]);
        let mut root = MutPlace::local(&mut list)
            .slice_view_mut(&mut ctx, 0, 2, false)
            .unwrap();
        let ops = SliceElemOps { index: 2 };
        let mut place = MutPlace::projected(MutPlace::local(&mut root), &ops);

        assert!(place.get_copy(&mut ctx).is_err());
        assert!(place.set(&mut ctx, 3).is_err());
            );
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
        drop(place);

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
        drop(place);
        assert_eq!(value.0, 3);
            );
    }

    #[test]
    fn local_list_mutation_uses_short_region() {
        with_ctx!(ctx;
        let mut list = list(&mut ctx, [1_i64]);
        let place = MutPlace::local(&mut list);

        assert_eq!(place.access(&mut ctx, |list| Ok(list.len())).unwrap(), 1);
        drop(place);
        list.push(&mut ctx, 2).unwrap();
        assert_eq!(list.len(), 2);
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
        drop(place);
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
        ($ctx:expr, $place:expr) => {{
            let mut place = $place;
            assert_unsupported_slice_view(place.slice_view(0, 2, false));
            assert_unsupported_slice_view(place.slice_view_mut($ctx, 0, 2, false));
        }};
    }

    macro_rules! assert_unsupported_raw_slice_views {
        ($ctx:expr, $place:expr) => {{
            let mut place = $place;
            assert_unsupported_slice_view(unsafe { place.slice_view(0, 2, false) });
            assert_unsupported_slice_view(unsafe { place.slice_view_mut($ctx, 0, 2, false) });
        }};
    }

    fn assert_unsupported_slice_view<T>(result: Result<AnvSlice<'_, T>, RuntimeError>) {
        let Err(err) = result else {
            panic!("expected unsupported slice view");
        };
        assert_eq!(
            err.message(),
            "slice view over non-local mutable collection parameter is unsupported"
        );
    }

    fn set_slice_second<'cx>(ctx: &mut Ctx<'cx, '_>, slice: &mut AnvSlice<'cx, i64>) {
        slice
            .with_elem_owned_mut_ctx_short(ctx, 1, |_, value| {
                *value = 9;
                Ok(())
            })
            .unwrap();
    }

    #[test]
    fn local_slice_views_succeed() {
        with_ctx!(ctx;
        let mut list = list(&mut ctx, [1_i64, 2, 3]);
        let mut place = MutPlace::local(&mut list);

        let slice = place.slice_view(1, 3, false).unwrap();
        assert_eq!(slice.elem_at_shared(&ctx, 0).unwrap(), 2);
        assert_eq!(slice.elem_at_shared(&ctx, 1).unwrap(), 3);
        let mut slice = place.slice_view_mut(&mut ctx, 0, 2, false).unwrap();
        set_slice_second(&mut ctx, &mut slice);
        drop(place);
        assert_eq!(list.checked_index(&ctx, 1).unwrap(), 9);

        let mut array = [1_i64, 2, 3];
        let mut place = MutPlace::local(&mut array);

        let slice = unsafe { place.slice_view(1, 3, false) }.unwrap();
        assert_eq!(slice.elem_at_shared(&ctx, 0).unwrap(), 2);
        assert_eq!(slice.elem_at_shared(&ctx, 1).unwrap(), 3);
        let mut slice = unsafe { place.slice_view_mut(&mut ctx, 0, 2, false) }.unwrap();
        set_slice_second(&mut ctx, &mut slice);
        drop(place);
        assert_eq!(array[1], 9);
            );
    }

    #[test]
    fn stack_cell_slice_views_are_unsupported() {
        with_ctx!(ctx;
        let list_cell = StackLambdaCell::new(list(&mut ctx, [1_i64, 2, 3]));
        assert_unsupported_slice_views!(&mut ctx, MutPlace::stack_cell(&list_cell));

        let array_cell = StackLambdaCell::new([1_i64, 2, 3]);
        assert_unsupported_raw_slice_views!(&mut ctx, MutPlace::stack_cell(&array_cell));
            );
    }

    #[test]
    fn heap_cell_slice_views_are_unsupported() {
        with_ctx!(ctx;
        let list_ty = ctx.heap().register_untracked::<LambdaCell<AnvList<'_, i64>>>();
        let list_value = list(&mut ctx, [1_i64, 2, 3]);
        let list_cell = ctx.heap().alloc(list_ty, LambdaCell::new(list_value));
        assert_unsupported_slice_views!(&mut ctx, MutPlace::heap_cell(list_cell));

        let array_ty = ctx.heap().register_untracked::<LambdaCell<[i64; 3]>>();
        let array_cell = ctx.heap().alloc(array_ty, LambdaCell::new([1_i64, 2, 3]));
        assert_unsupported_raw_slice_views!(&mut ctx, MutPlace::heap_cell(array_cell));
            );
    }

    #[test]
    fn scoped_cell_slice_views_are_unsupported() {
        with_ctx!(ctx;
        let mut list = list(&mut ctx, [1_i64, 2, 3]);
        let list_cell = ScopedMutPlaceCell::new(MutPlace::local(&mut list));
        assert_unsupported_slice_views!(&mut ctx, MutPlace::scoped_cell(&list_cell));

        let mut array = [1_i64, 2, 3];
        let array_cell = ScopedMutPlaceCell::new(MutPlace::local(&mut array));
        assert_unsupported_raw_slice_views!(&mut ctx, MutPlace::scoped_cell(&array_cell));
            );
    }

    #[test]
    fn dataref_slice_views_are_unsupported() {
        with_ctx!(ctx;
        let list_ty = ctx.heap().register_untracked::<Storage<AnvList<'_, i64>>>();
        let list_value = list(&mut ctx, [1_i64, 2, 3]);
        let list_object = ctx.heap().alloc(list_ty, Storage { field: list_value });
        let list_erased = ctx.heap().erase(&list_object).unwrap();
        let list_ops = FieldOps { ty: list_ty };
        assert_unsupported_slice_views!(&mut ctx, MutPlace::dataref(list_erased, &list_ops));

        let array_ty = ctx.heap().register_untracked::<Storage<[i64; 3]>>();
        let array_object = ctx.heap().alloc(
            array_ty,
            Storage {
                field: [1_i64, 2, 3],
            },
        );
        let array_erased = ctx.heap().erase(&array_object).unwrap();
        let array_ops = FieldOps { ty: array_ty };
        assert_unsupported_raw_slice_views!(&mut ctx, MutPlace::dataref(array_erased, &array_ops));
            );
    }
}
