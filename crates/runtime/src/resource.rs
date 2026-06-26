use std::fmt;

use crate::{
    AccessError, AnvyxRefExport, Ctx, ErasedHandle, Handle, Heap, HeapType, Trace, TraceDriver,
    TraceMode, Visitor,
};

pub struct AnvRefType<'cx, T: AnvyxRefExport + 'cx> {
    heap_type: HeapType<'cx, T>,
}

impl<T: AnvyxRefExport> Copy for AnvRefType<'_, T> {}

impl<T: AnvyxRefExport> Clone for AnvRefType<'_, T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<'cx, T: AnvyxRefExport + 'cx> AnvRefType<'cx, T> {
    pub fn register_untracked(heap: &mut Heap<'cx>) -> Self {
        Self {
            heap_type: heap.register_untracked::<T>(),
        }
    }

    pub fn register_tracked(heap: &mut Heap<'cx>) -> Self
    where
        T: Trace<'cx>,
    {
        Self {
            heap_type: heap.register_tracked::<T>(),
        }
    }

    pub fn heap_type(self) -> HeapType<'cx, T> {
        self.heap_type
    }

    pub fn trace_mode(self) -> TraceMode {
        self.heap_type.trace_mode()
    }

    pub fn alloc(self, heap: &mut Heap<'cx>, value: T) -> AnvRef<'cx, T> {
        AnvRef {
            handle: heap.alloc(self.heap_type, value),
        }
    }

    pub fn alloc_in<'rt>(self, ctx: &mut Ctx<'cx, 'rt>, value: T) -> AnvRef<'cx, T> {
        self.alloc(ctx.heap(), value)
    }

    pub fn with_erased<R>(
        self,
        heap: &Heap<'cx>,
        handle: &ErasedHandle<'cx>,
        f: impl FnOnce(&T) -> R,
    ) -> Result<R, AccessError> {
        heap.try_with_erased(handle, self.heap_type, f)
    }

    pub fn with_erased_mut<R>(
        self,
        heap: &mut Heap<'cx>,
        handle: &ErasedHandle<'cx>,
        f: impl FnOnce(&mut T) -> R,
    ) -> Result<R, AccessError> {
        heap.try_with_erased_mut(handle, self.heap_type, f)
    }
}

pub struct AnvRef<'cx, T: AnvyxRefExport + 'cx> {
    handle: Handle<'cx, T>,
}

impl<'cx, T: AnvyxRefExport + 'cx> AnvRef<'cx, T> {
    pub fn from_handle(handle: Handle<'cx, T>) -> Self {
        Self { handle }
    }

    pub fn as_handle(&self) -> &Handle<'cx, T> {
        &self.handle
    }

    pub fn into_handle(self) -> Handle<'cx, T> {
        self.handle
    }

    pub fn ptr_eq(&self, other: &Self) -> bool {
        self.handle.ptr_eq(&other.handle)
    }

    pub fn is_alive(&self) -> bool {
        self.handle.is_alive()
    }

    pub fn erase(&self, heap: &Heap<'cx>) -> Result<ErasedHandle<'cx>, AccessError> {
        heap.erase(&self.handle)
    }

    pub fn with<R>(&self, heap: &Heap<'cx>, f: impl FnOnce(&T) -> R) -> Result<R, AccessError> {
        heap.try_with(&self.handle, f)
    }

    pub fn with_mut<R>(
        &self,
        heap: &mut Heap<'cx>,
        f: impl FnOnce(&mut T) -> R,
    ) -> Result<R, AccessError> {
        heap.try_with_mut(&self.handle, f)
    }
}

impl<T: AnvyxRefExport> Clone for AnvRef<'_, T> {
    fn clone(&self) -> Self {
        Self {
            handle: self.handle.clone(),
        }
    }
}

impl<T: AnvyxRefExport> fmt::Debug for AnvRef<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("AnvRef")
            .field("alive", &self.is_alive())
            .finish_non_exhaustive()
    }
}

unsafe impl<'cx, T: AnvyxRefExport + 'cx> Trace<'cx> for AnvRef<'cx, T> {
    fn trace<D: TraceDriver<'cx>>(&self, visitor: &mut Visitor<'cx, '_, D>) {
        self.handle.trace(visitor);
    }
}
