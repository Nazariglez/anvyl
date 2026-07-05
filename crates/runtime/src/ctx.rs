use std::{marker::PhantomData, ptr::NonNull};

use crate::{
    AccessError, CollectOutcome, ErasedHandle, Handle, Heap, HeapStats, HeapType, RuntimeError,
    SafepointGuard, SafepointGuardKind, SafepointState, Trace,
};

pub trait TraceRootSet<'cx>: Trace<'cx> {
    fn validate_trace_roots(&self) -> Result<(), RuntimeError>;
}

pub struct Ctx<'cx, 'rt> {
    heap: NonNull<Heap<'cx>>,
    roots: Option<Roots<'rt, 'cx>>,
    safepoint: SafepointState,
    marker: PhantomData<&'rt ()>,
}

pub struct HeapBorrowMut<'heap, 'cx> {
    heap: &'heap mut Heap<'cx>,
    _guard: SafepointGuard,
}

pub struct HeapBorrowRef<'heap, 'cx> {
    heap: &'heap Heap<'cx>,
    _guard: SafepointGuard,
}

#[derive(Copy, Clone)]
struct Roots<'rt, 'cx> {
    ptr: NonNull<()>,
    validate: unsafe fn(NonNull<()>) -> Result<(), RuntimeError>,
    collect: unsafe fn(&mut Heap<'cx>, NonNull<()>, usize) -> CollectOutcome,
    collect_all: unsafe fn(&mut Heap<'cx>, NonNull<()>) -> CollectOutcome,
    marker: PhantomData<&'rt ()>,
}

impl<'cx, 'rt> Ctx<'cx, 'rt> {
    pub fn new(heap: &'rt mut Heap<'cx>) -> Self {
        unsafe { Self::__anvyx_from_raw(NonNull::from(heap)) }
    }

    pub fn new_with_safepoint(heap: &'rt mut Heap<'cx>, safepoint: &SafepointState) -> Self {
        unsafe { Self::__anvyx_from_raw_with_safepoint(NonNull::from(heap), safepoint) }
    }

    pub fn new_with_trace_roots<T: TraceRootSet<'cx>>(
        heap: &'rt mut Heap<'cx>,
        roots: &'rt T,
    ) -> Self {
        unsafe { Self::__anvyx_from_raw_with_trace_roots(NonNull::from(heap), roots) }
    }

    pub fn new_with_trace_roots_and_safepoint<T: TraceRootSet<'cx>>(
        heap: &'rt mut Heap<'cx>,
        roots: &'rt T,
        safepoint: &SafepointState,
    ) -> Self {
        unsafe {
            Self::__anvyx_from_raw_with_trace_roots_and_safepoint(
                NonNull::from(heap),
                roots,
                safepoint,
            )
        }
    }

    #[doc(hidden)]
    pub unsafe fn __anvyx_from_raw(heap: NonNull<Heap<'cx>>) -> Self {
        Self {
            heap,
            roots: None,
            safepoint: SafepointState::default(),
            marker: PhantomData,
        }
    }

    #[doc(hidden)]
    pub unsafe fn __anvyx_from_raw_with_safepoint(
        heap: NonNull<Heap<'cx>>,
        safepoint: &SafepointState,
    ) -> Self {
        Self {
            heap,
            roots: None,
            safepoint: safepoint.clone(),
            marker: PhantomData,
        }
    }

    #[doc(hidden)]
    pub unsafe fn __anvyx_from_raw_with_trace_roots<T: TraceRootSet<'cx>>(
        heap: NonNull<Heap<'cx>>,
        roots: &'rt T,
    ) -> Self {
        unsafe {
            Self::__anvyx_from_raw_with_trace_roots_and_safepoint(
                heap,
                roots,
                &SafepointState::default(),
            )
        }
    }

    #[doc(hidden)]
    pub unsafe fn __anvyx_from_raw_with_trace_roots_and_safepoint<T: TraceRootSet<'cx>>(
        heap: NonNull<Heap<'cx>>,
        roots: &'rt T,
        safepoint: &SafepointState,
    ) -> Self {
        Self {
            heap,
            roots: Some(Roots {
                ptr: NonNull::from(roots).cast(),
                validate: validate_trace_roots::<T>,
                collect: collect_with_trace_roots::<T>,
                collect_all: collect_all_with_trace_roots::<T>,
                marker: PhantomData,
            }),
            safepoint: safepoint.clone(),
            marker: PhantomData,
        }
    }

    pub fn heap(&mut self) -> HeapBorrowMut<'_, 'cx> {
        self.safepoint
            .validate_heap_access()
            .expect("nested runtime heap access");
        let guard = self
            .safepoint
            .enter(SafepointGuardKind::HeapBorrow)
            .expect("too many active runtime safepoint guards");
        HeapBorrowMut {
            heap: unsafe { self.heap.as_mut() },
            _guard: guard,
        }
    }

    pub fn heap_ref(&self) -> HeapBorrowRef<'_, 'cx> {
        let guard = self
            .safepoint
            .enter(SafepointGuardKind::HeapBorrow)
            .expect("too many active runtime safepoint guards");
        HeapBorrowRef {
            heap: unsafe { self.heap.as_ref() },
            _guard: guard,
        }
    }

    fn raw_heap(&self) -> &Heap<'cx> {
        unsafe { self.heap.as_ref() }
    }

    fn raw_heap_mut(&mut self) -> &mut Heap<'cx> {
        unsafe { self.heap.as_mut() }
    }

    pub fn collect(&mut self, cycle_work_hint: usize) -> Result<CollectOutcome, RuntimeError> {
        self.safepoint.validate_collect()?;
        let _collecting = self.safepoint.enter(SafepointGuardKind::Collecting)?;
        match self.roots {
            Some(roots) => unsafe {
                (roots.validate)(roots.ptr)?;
                Ok((roots.collect)(
                    self.raw_heap_mut(),
                    roots.ptr,
                    cycle_work_hint,
                ))
            },
            None => Ok(self.raw_heap_mut().collect(cycle_work_hint)),
        }
    }

    pub fn collect_all(&mut self) -> Result<CollectOutcome, RuntimeError> {
        self.safepoint.validate_collect()?;
        let _collecting = self.safepoint.enter(SafepointGuardKind::Collecting)?;
        match self.roots {
            Some(roots) => unsafe {
                (roots.validate)(roots.ptr)?;
                Ok((roots.collect_all)(self.raw_heap_mut(), roots.ptr))
            },
            None => Ok(self.raw_heap_mut().collect_all()),
        }
    }

    pub fn set_collection_enabled(&mut self, enabled: bool) {
        self.safepoint
            .validate_heap_access()
            .expect("nested runtime heap access");
        self.raw_heap_mut().set_collection_enabled(enabled);
    }

    #[doc(hidden)]
    pub fn __anvyx_validate_reentry(&self) -> Result<(), RuntimeError> {
        self.safepoint.validate_reentry()
    }

    pub fn stats(&self) -> HeapStats {
        self.safepoint
            .validate_heap_access()
            .expect("nested runtime heap access");
        self.raw_heap().stats()
    }

    #[doc(hidden)]
    pub fn __anvyx_safepoint_state(&self) -> SafepointState {
        self.safepoint.clone()
    }
}

impl<'cx> HeapBorrowRef<'_, 'cx> {
    pub fn with<T: 'cx, R>(&self, handle: &Handle<'cx, T>, f: impl FnOnce(&T) -> R) -> R {
        self.heap.with(handle, f)
    }

    pub fn try_with<T: 'cx, R>(
        &self,
        handle: &Handle<'cx, T>,
        f: impl FnOnce(&T) -> R,
    ) -> Result<R, AccessError> {
        self.heap.try_with(handle, f)
    }

    pub fn erase<T: 'cx>(&self, handle: &Handle<'cx, T>) -> Result<ErasedHandle<'cx>, AccessError> {
        self.heap.erase(handle)
    }

    pub fn try_with_erased<T: 'cx, R>(
        &self,
        handle: &ErasedHandle<'cx>,
        heap_type: HeapType<'cx, T>,
        f: impl FnOnce(&T) -> R,
    ) -> Result<R, AccessError> {
        self.heap.try_with_erased(handle, heap_type, f)
    }

    pub fn stats(&self) -> HeapStats {
        self.heap.stats()
    }
}

impl<'cx> HeapBorrowMut<'_, 'cx> {
    pub fn register_untracked<T: 'cx>(&mut self) -> HeapType<'cx, T> {
        self.heap.register_untracked::<T>()
    }

    pub fn register_tracked<T: Trace<'cx> + 'cx>(&mut self) -> HeapType<'cx, T> {
        self.heap.register_tracked::<T>()
    }

    pub fn alloc<T: 'cx>(&mut self, heap_type: HeapType<'cx, T>, value: T) -> Handle<'cx, T> {
        self.heap.alloc(heap_type, value)
    }

    pub fn with<T: 'cx, R>(&self, handle: &Handle<'cx, T>, f: impl FnOnce(&T) -> R) -> R {
        self.heap.with(handle, f)
    }

    pub fn with_mut<T: 'cx, R>(
        &mut self,
        handle: &Handle<'cx, T>,
        f: impl FnOnce(&mut T) -> R,
    ) -> R {
        self.heap.with_mut(handle, f)
    }

    pub fn try_with<T: 'cx, R>(
        &self,
        handle: &Handle<'cx, T>,
        f: impl FnOnce(&T) -> R,
    ) -> Result<R, AccessError> {
        self.heap.try_with(handle, f)
    }

    pub fn try_with_mut<T: 'cx, R>(
        &mut self,
        handle: &Handle<'cx, T>,
        f: impl FnOnce(&mut T) -> R,
    ) -> Result<R, AccessError> {
        self.heap.try_with_mut(handle, f)
    }

    pub fn erase<T: 'cx>(&self, handle: &Handle<'cx, T>) -> Result<ErasedHandle<'cx>, AccessError> {
        self.heap.erase(handle)
    }

    pub fn try_with_erased<T: 'cx, R>(
        &self,
        handle: &ErasedHandle<'cx>,
        heap_type: HeapType<'cx, T>,
        f: impl FnOnce(&T) -> R,
    ) -> Result<R, AccessError> {
        self.heap.try_with_erased(handle, heap_type, f)
    }

    pub fn try_with_erased_mut<T: 'cx, R>(
        &mut self,
        handle: &ErasedHandle<'cx>,
        heap_type: HeapType<'cx, T>,
        f: impl FnOnce(&mut T) -> R,
    ) -> Result<R, AccessError> {
        self.heap.try_with_erased_mut(handle, heap_type, f)
    }

    pub fn reset_stats(&mut self) {
        self.heap.reset_stats();
    }

    pub fn stats(&self) -> HeapStats {
        self.heap.stats()
    }
}

unsafe fn validate_trace_roots<'cx, T: TraceRootSet<'cx>>(
    roots: NonNull<()>,
) -> Result<(), RuntimeError> {
    unsafe { roots.cast::<T>().as_ref() }.validate_trace_roots()
}

unsafe fn collect_with_trace_roots<'cx, T: Trace<'cx>>(
    heap: &mut Heap<'cx>,
    roots: NonNull<()>,
    cycle_work_hint: usize,
) -> CollectOutcome {
    heap.collect_with_external_roots(cycle_work_hint, unsafe { roots.cast::<T>().as_ref() })
}

unsafe fn collect_all_with_trace_roots<'cx, T: Trace<'cx>>(
    heap: &mut Heap<'cx>,
    roots: NonNull<()>,
) -> CollectOutcome {
    heap.collect_all_with_external_roots(unsafe { roots.cast::<T>().as_ref() })
}

#[cfg(test)]
mod tests {
    use std::cell::Cell;

    use super::*;
    use crate::{SafepointGuardKind, TraceDriver, Visitor};

    #[test]
    fn heap_borrow_blocks_collect_and_reentry() {
        Heap::scope(|heap| {
            let mut ctx = Ctx::new(heap);
            let state = ctx.__anvyx_safepoint_state();
            let borrow = ctx.heap();

            assert_eq!(
                state.validate_collect().unwrap_err().message(),
                "cannot collect while heap borrow guard is active"
            );
            assert_eq!(
                state.validate_reentry().unwrap_err().message(),
                "cannot reenter runtime while heap borrow guard is active"
            );

            drop(borrow);
            assert!(ctx.collect_all().is_ok());
        });
    }

    #[test]
    fn heap_ref_borrow_blocks_collect_and_reentry() {
        Heap::scope(|heap| {
            let ctx = Ctx::new(heap);
            let borrow = ctx.heap_ref();
            let state = ctx.__anvyx_safepoint_state();

            assert!(state.validate_collect().is_err());
            assert!(state.validate_reentry().is_err());

            drop(borrow);
            assert!(state.validate_collect().is_ok());
            assert!(state.validate_reentry().is_ok());
        });
    }

    #[test]
    #[should_panic(expected = "nested runtime heap access")]
    fn heap_ref_borrow_rejects_nested_heap_access_before_borrowing() {
        Heap::scope(|heap| {
            let ctx = Ctx::new(heap);
            let _borrow = ctx.heap_ref();

            let _ = ctx.stats();
        });
    }

    #[test]
    fn collect_checks_safepoint_state() {
        Heap::scope(|heap| {
            let mut ctx = Ctx::new(heap);
            let state = ctx.__anvyx_safepoint_state();
            let guard = state.enter(SafepointGuardKind::HeapBorrow).unwrap();

            assert_eq!(
                ctx.collect_all().unwrap_err().message(),
                "cannot collect while heap borrow guard is active"
            );

            drop(guard);
            assert!(ctx.collect_all().is_ok());
        });
    }

    #[test]
    fn collect_blocks_reentry_while_roots_are_validated() {
        struct Roots {
            state: SafepointState,
            validated: Cell<bool>,
        }

        unsafe impl<'cx> Trace<'cx> for Roots {
            fn trace<D: TraceDriver<'cx>>(&self, _visitor: &mut Visitor<'cx, '_, D>) {}
        }

        impl TraceRootSet<'_> for Roots {
            fn validate_trace_roots(&self) -> Result<(), RuntimeError> {
                self.validated.set(true);
                assert_eq!(
                    self.state.validate_reentry().unwrap_err().message(),
                    "cannot reenter runtime while collection guard is active"
                );
                Ok(())
            }
        }

        Heap::scope(|heap| {
            let state = SafepointState::default();
            let roots = Roots {
                state: state.clone(),
                validated: Cell::new(false),
            };
            let mut ctx = Ctx::new_with_trace_roots_and_safepoint(heap, &roots, &state);

            ctx.collect_all().unwrap();

            assert!(roots.validated.get());
            assert!(state.validate_reentry().is_ok());
        });
    }
}
