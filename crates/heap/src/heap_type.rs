use std::{
    cell::Cell,
    marker::PhantomData,
    sync::atomic::{AtomicU64, Ordering},
};

static NEXT_HEAP_ID: AtomicU64 = AtomicU64::new(1);

type Invariant<'cx> = PhantomData<Cell<&'cx ()>>;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub(crate) struct HeapId(u64);

impl HeapId {
    #[inline]
    pub(crate) fn fresh() -> Self {
        Self(NEXT_HEAP_ID.fetch_add(1, Ordering::Relaxed))
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct HeapTypeId(pub(crate) usize);

impl HeapTypeId {
    #[inline]
    pub fn index(self) -> usize {
        self.0
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum TraceMode {
    Untracked,
    Tracked,
}

pub struct HeapType<'cx, T> {
    pub(crate) heap_id: HeapId,
    pub(crate) id: HeapTypeId,
    pub(crate) trace_mode: TraceMode,
    marker: PhantomData<fn(T) -> T>,
    brand: Invariant<'cx>,
}

impl<T> HeapType<'_, T> {
    #[inline]
    pub(crate) fn new(heap_id: HeapId, id: HeapTypeId, trace_mode: TraceMode) -> Self {
        Self {
            heap_id,
            id,
            trace_mode,
            marker: PhantomData,
            brand: PhantomData,
        }
    }

    #[inline]
    pub fn id(self) -> HeapTypeId {
        self.id
    }

    #[inline]
    pub fn trace_mode(self) -> TraceMode {
        self.trace_mode
    }
}

impl<T> Copy for HeapType<'_, T> {}

impl<T> Clone for HeapType<'_, T> {
    fn clone(&self) -> Self {
        *self
    }
}
