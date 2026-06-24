use std::{marker::PhantomData, ptr::NonNull};

use crate::{CollectOutcome, Heap, HeapStats, RuntimeError, Trace};

pub trait CtxRoots<'cx>: Trace<'cx> {
    fn validate_roots(&self) -> Result<(), RuntimeError>;
}

pub struct Ctx<'cx, 'rt> {
    heap: NonNull<Heap<'cx>>,
    roots: Option<Roots<'rt, 'cx>>,
    marker: PhantomData<&'rt ()>,
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

    pub fn new_with_roots<T: CtxRoots<'cx>>(heap: &'rt mut Heap<'cx>, roots: &'rt T) -> Self {
        unsafe { Self::__anvyx_from_raw_with_roots(NonNull::from(heap), roots) }
    }

    #[doc(hidden)]
    pub unsafe fn __anvyx_from_raw(heap: NonNull<Heap<'cx>>) -> Self {
        Self {
            heap,
            roots: None,
            marker: PhantomData,
        }
    }

    #[doc(hidden)]
    pub unsafe fn __anvyx_from_raw_with_roots<T: CtxRoots<'cx>>(
        heap: NonNull<Heap<'cx>>,
        roots: &'rt T,
    ) -> Self {
        Self {
            heap,
            roots: Some(Roots {
                ptr: NonNull::from(roots).cast(),
                validate: validate_roots::<T>,
                collect: collect_with_roots::<T>,
                collect_all: collect_all_with_roots::<T>,
                marker: PhantomData,
            }),
            marker: PhantomData,
        }
    }

    pub fn heap(&mut self) -> &mut Heap<'cx> {
        unsafe { self.heap.as_mut() }
    }

    pub fn heap_ref(&self) -> &Heap<'cx> {
        unsafe { self.heap.as_ref() }
    }

    pub fn collect(&mut self, cycle_work_hint: usize) -> Result<CollectOutcome, RuntimeError> {
        match self.roots {
            Some(roots) => unsafe {
                (roots.validate)(roots.ptr)?;
                Ok((roots.collect)(self.heap(), roots.ptr, cycle_work_hint))
            },
            None => Ok(self.heap().collect(cycle_work_hint)),
        }
    }

    pub fn collect_all(&mut self) -> Result<CollectOutcome, RuntimeError> {
        match self.roots {
            Some(roots) => unsafe {
                (roots.validate)(roots.ptr)?;
                Ok((roots.collect_all)(self.heap(), roots.ptr))
            },
            None => Ok(self.heap().collect_all()),
        }
    }

    pub fn set_collection_enabled(&mut self, enabled: bool) {
        self.heap().set_collection_enabled(enabled);
    }

    pub fn stats(&self) -> HeapStats {
        self.heap_ref().stats()
    }
}

unsafe fn validate_roots<'cx, T: CtxRoots<'cx>>(roots: NonNull<()>) -> Result<(), RuntimeError> {
    unsafe { roots.cast::<T>().as_ref() }.validate_roots()
}

unsafe fn collect_with_roots<'cx, T: Trace<'cx>>(
    heap: &mut Heap<'cx>,
    roots: NonNull<()>,
    cycle_work_hint: usize,
) -> CollectOutcome {
    heap.collect_with_roots(cycle_work_hint, unsafe { roots.cast::<T>().as_ref() })
}

unsafe fn collect_all_with_roots<'cx, T: Trace<'cx>>(
    heap: &mut Heap<'cx>,
    roots: NonNull<()>,
) -> CollectOutcome {
    heap.collect_all_with_roots(unsafe { roots.cast::<T>().as_ref() })
}
