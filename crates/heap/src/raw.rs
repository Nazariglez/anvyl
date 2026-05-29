use std::{
    alloc::{Layout, alloc, dealloc, handle_alloc_error},
    cell::{Cell, UnsafeCell},
    marker::PhantomData,
    mem::{MaybeUninit, size_of},
    ptr::NonNull,
};

use crate::{
    heap_type::{HeapTypeId, TraceMode},
    trace::{BlackTrace, GrayTrace, Trace, TraceDriver, Visitor},
};

type Invariant<'cx> = PhantomData<Cell<&'cx ()>>;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub(crate) enum Color {
    Black,
    Purple,
    Gray,
    White,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub(crate) enum SlotState {
    Live,
    Dropping,
    Dropped,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct ObjPtr<'cx> {
    ptr: NonNull<Header<'cx>>,
    brand: Invariant<'cx>,
}

impl<'cx> ObjPtr<'cx> {
    #[inline]
    pub(crate) fn new(ptr: NonNull<Header<'cx>>) -> Self {
        Self {
            ptr,
            brand: PhantomData,
        }
    }

    #[inline]
    pub(crate) fn as_non_null(self) -> NonNull<Header<'cx>> {
        self.ptr
    }

    #[inline]
    pub(crate) fn header(self) -> &'cx Header<'cx> {
        // SAFETY: `ObjPtr` is only built from heap slot headers that outlive `'cx`.
        unsafe { self.ptr.as_ref() }
    }
}

#[derive(Debug)]
pub(crate) struct StatePtr<'cx, T> {
    ptr: NonNull<T>,
    brand: Invariant<'cx>,
}

impl<T> Copy for StatePtr<'_, T> {}

impl<T> Clone for StatePtr<'_, T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Eq for StatePtr<'_, T> {}

impl<T> PartialEq for StatePtr<'_, T> {
    fn eq(&self, other: &Self) -> bool {
        self.ptr == other.ptr
    }
}

impl<'cx, T> StatePtr<'cx, T> {
    #[inline]
    pub(crate) fn new(value: &T) -> Self {
        Self {
            ptr: NonNull::from(value),
            brand: PhantomData,
        }
    }

    #[inline]
    pub(crate) fn get(self) -> &'cx T {
        // SAFETY: `StatePtr` is only built from heap-owned state that outlives `'cx`.
        unsafe { self.ptr.as_ref() }
    }
}

#[repr(C)]
pub(crate) struct Header<'cx> {
    pub strong: Cell<u32>,
    pub color: Cell<Color>,
    pub buffered: Cell<bool>,
    state: Cell<SlotState>,
    pub generation: Cell<u64>,
    pub trial: Cell<u32>,
    pub visit_epoch: Cell<u64>,
    pub heap_type_id: Cell<HeapTypeId>,
    pub trace_mode: TraceMode,
    vtable: NonNull<VTable<'cx>>,
    marker: PhantomData<&'cx ()>,
}

impl<'cx> Header<'cx> {
    pub(crate) fn live(
        generation: u64,
        heap_type_id: HeapTypeId,
        trace_mode: TraceMode,
        vtable: NonNull<VTable<'cx>>,
    ) -> Self {
        Self {
            strong: Cell::new(1),
            color: Cell::new(Color::Black),
            buffered: Cell::new(false),
            state: Cell::new(SlotState::Live),
            generation: Cell::new(generation),
            trial: Cell::new(0),
            visit_epoch: Cell::new(0),
            heap_type_id: Cell::new(heap_type_id),
            trace_mode,
            vtable,
            marker: PhantomData,
        }
    }

    pub(crate) fn reset(
        &self,
        generation: u64,
        heap_type_id: HeapTypeId,
        trace_mode: TraceMode,
        vtable: NonNull<VTable<'cx>>,
    ) {
        self.strong.set(1);
        self.color.set(Color::Black);
        self.buffered.set(false);
        self.state.set(SlotState::Live);
        self.generation.set(generation);
        self.trial.set(0);
        self.visit_epoch.set(0);
        self.heap_type_id.set(heap_type_id);
        debug_assert_eq!(self.trace_mode, trace_mode);
        debug_assert_eq!(self.vtable, vtable);
    }

    #[inline]
    pub(crate) fn vtable(&self) -> &VTable<'cx> {
        // SAFETY: vtables are stored in heap type state for the whole heap scope.
        unsafe { self.vtable.as_ref() }
    }

    #[inline]
    pub(crate) fn is_live(&self) -> bool {
        self.state.get() == SlotState::Live
    }

    #[inline]
    pub(crate) fn begin_drop(&self) -> bool {
        self.state.replace(SlotState::Dropping) == SlotState::Live
    }

    #[inline]
    pub(crate) fn finish_drop(&self) {
        self.state.set(SlotState::Dropped);
    }
}

#[repr(C)]
pub(crate) struct Slot<'cx, T> {
    pub header: Header<'cx>,
    pub value: UnsafeCell<MaybeUninit<T>>,
}

impl<'cx, T> Slot<'cx, T> {
    pub(crate) unsafe fn write_header(ptr: ObjPtr<'cx>, header: Header<'cx>) {
        let slot = ptr.as_non_null().cast::<Self>();
        // SAFETY: caller guarantees `ptr` names writable `Slot<'cx, T>` storage.
        unsafe { std::ptr::addr_of_mut!((*slot.as_ptr()).header).write(header) };
    }

    #[inline]
    pub(crate) unsafe fn write_payload_unchecked(ptr: ObjPtr<'cx>, value: T) {
        let slot = ptr.as_non_null().cast::<Self>();
        // SAFETY: caller guarantees `ptr` names a dead/reused `Slot<'cx, T>` payload.
        unsafe { (*slot.as_ref().value.get()).write(value) };
    }

    #[inline]
    pub(crate) unsafe fn payload_ref_unchecked(ptr: ObjPtr<'cx>) -> &'cx T {
        let slot = ptr.as_non_null().cast::<Self>();
        // SAFETY: caller guarantees `ptr` names a live initialized `Slot<'cx, T>`.
        unsafe { (*slot.as_ref().value.get()).assume_init_ref() }
    }

    #[inline]
    pub(crate) unsafe fn payload_mut_unchecked(ptr: ObjPtr<'cx>) -> &'cx mut T {
        let slot = ptr.as_non_null().cast::<Self>();
        // SAFETY: caller guarantees a live `Slot<'cx, T>` and exclusive heap access.
        unsafe { (*slot.as_ref().value.get()).assume_init_mut() }
    }

    #[inline]
    pub(crate) unsafe fn drop_payload_unchecked(ptr: ObjPtr<'cx>) {
        let slot = ptr.as_non_null().cast::<Self>();
        // SAFETY: caller guarantees the `Slot<'cx, T>` payload is initialized and undropped.
        unsafe { (*slot.as_ref().value.get()).assume_init_drop() };
    }
}

pub(crate) struct SlotPage<'cx> {
    data: NonNull<u8>,
    layout: Layout,
    slot_size: usize,
    slots: usize,
    initialized: usize,
    marker: Invariant<'cx>,
}

impl<'cx> SlotPage<'cx> {
    pub(crate) fn new<T: 'cx>(slots: usize) -> Self {
        debug_assert!(slots > 0);
        let layout = Layout::array::<Slot<'cx, T>>(slots).expect("heap slot page layout overflow");
        // SAFETY: slot pages are created with at least one slot; null is handled below.
        let data = unsafe { alloc(layout) };
        let Some(data) = NonNull::new(data) else {
            handle_alloc_error(layout);
        };
        Self {
            data,
            layout,
            slot_size: size_of::<Slot<'cx, T>>(),
            slots,
            initialized: 0,
            marker: PhantomData,
        }
    }

    pub(crate) fn is_full(&self) -> bool {
        self.initialized == self.slots
    }

    pub(crate) unsafe fn next_slot<T: 'cx>(&mut self) -> Option<ObjPtr<'cx>> {
        debug_assert_eq!(self.slot_size, size_of::<Slot<'cx, T>>());
        if self.is_full() {
            return None;
        }
        let index = self.initialized;
        self.initialized += 1;
        // SAFETY: `index < self.slots` and this page stores `Slot<'cx, T>`.
        let slot = unsafe { self.data.as_ptr().cast::<Slot<'cx, T>>().add(index) };
        // SAFETY: `slot` is derived from a non-null page allocation.
        Some(unsafe { ObjPtr::new(NonNull::new_unchecked(slot.cast::<Header<'cx>>())) })
    }

    pub(crate) fn for_each_initialized(&self, mut f: impl FnMut(ObjPtr<'cx>)) {
        debug_assert!(self.initialized <= self.slots);
        for index in 0..self.initialized {
            // SAFETY: `index < initialized <= slots`; initialized slots have headers.
            let ptr =
                unsafe { NonNull::new_unchecked(self.data.as_ptr().add(index * self.slot_size)) }
                    .cast::<Header<'cx>>();
            f(ObjPtr::new(ptr));
        }
    }
}

impl Drop for SlotPage<'_> {
    fn drop(&mut self) {
        // SAFETY: `data` was allocated with this exact `layout`.
        unsafe { dealloc(self.data.as_ptr(), self.layout) };
    }
}

type TraceFn<'cx, D> = for<'a> unsafe fn(ObjPtr<'cx>, &mut Visitor<'cx, 'a, D>);

pub(crate) struct VTable<'cx> {
    pub type_name: &'static str,
    trace_gray: TraceFn<'cx, GrayTrace>,
    trace_black: TraceFn<'cx, BlackTrace>,
    drop_payload: unsafe fn(ObjPtr<'cx>),
}

impl<'cx> VTable<'cx> {
    #[inline]
    pub(crate) fn new_untracked<T: 'cx>() -> Self {
        Self {
            type_name: std::any::type_name::<T>(),
            trace_gray: trace_noop::<T, GrayTrace>,
            trace_black: trace_noop::<T, BlackTrace>,
            drop_payload: drop_payload::<T>,
        }
    }

    #[inline]
    pub(crate) fn new_tracked<T: Trace<'cx> + 'cx>() -> Self {
        Self {
            type_name: std::any::type_name::<T>(),
            trace_gray: trace_impl::<T, GrayTrace>,
            trace_black: trace_impl::<T, BlackTrace>,
            drop_payload: drop_payload::<T>,
        }
    }

    #[inline]
    pub(crate) fn trace_gray(&self, ptr: ObjPtr<'cx>, visitor: &mut Visitor<'cx, '_, GrayTrace>) {
        // SAFETY: the vtable was registered for `ptr`'s payload type.
        unsafe { (self.trace_gray)(ptr, visitor) };
    }

    #[inline]
    pub(crate) fn trace_black(&self, ptr: ObjPtr<'cx>, visitor: &mut Visitor<'cx, '_, BlackTrace>) {
        // SAFETY: the vtable was registered for `ptr`'s payload type.
        unsafe { (self.trace_black)(ptr, visitor) };
    }

    #[inline]
    pub(crate) fn drop_payload(&self, ptr: ObjPtr<'cx>) {
        // SAFETY: the vtable was registered for `ptr`'s payload type.
        unsafe { (self.drop_payload)(ptr) };
    }
}

// SAFETY: this vtable function does not touch the payload.
unsafe fn trace_noop<'cx, T, D: TraceDriver<'cx>>(
    _ptr: ObjPtr<'cx>,
    _visitor: &mut Visitor<'cx, '_, D>,
) {
}

#[inline]
// SAFETY: this is called through a vtable registered for `T`.
unsafe fn trace_impl<'cx, T, D>(ptr: ObjPtr<'cx>, visitor: &mut Visitor<'cx, '_, D>)
where
    T: Trace<'cx> + 'cx,
    D: TraceDriver<'cx>,
{
    // SAFETY: the vtable was registered for `T`, so `ptr` points at `Slot<'cx, T>`.
    unsafe { Slot::<'cx, T>::payload_ref_unchecked(ptr) }.trace(visitor);
}

#[inline]
// SAFETY: this is called through a vtable registered for `T`.
unsafe fn drop_payload<'cx, T: 'cx>(ptr: ObjPtr<'cx>) {
    // SAFETY: caller guarantees the payload is initialized and undropped.
    unsafe { Slot::<'cx, T>::drop_payload_unchecked(ptr) };
}
