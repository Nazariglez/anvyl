use std::{
    any::Any,
    cell::{Cell, RefCell},
    collections::BTreeMap,
    marker::PhantomData,
    mem::size_of,
    panic::{self, AssertUnwindSafe},
    ptr::NonNull,
    rc::Rc,
};

use crate::{
    handle::Handle,
    heap_type::{HeapId, HeapType, HeapTypeId, TraceMode},
    metrics::{HeapStats, MetricsCells},
    queue::{WorkItem, WorkQueue},
    raw::{Color, Header, ObjPtr, Slot, SlotPage, StatePtr, VTable},
    trace::{BlackTrace, GrayTrace, Trace, TraceDriver, Visitor, sealed},
};

const MAX_ROOT_BATCH: usize = 4096;
const ESTIMATED_ROOT_COST: usize = 6;
const TARGET_SLOT_PAGE_BYTES: usize = 32 * 1024;
const MAX_SLOT_PAGE_SLOTS: usize = 4096;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum AccessError {
    WrongHeap,
    DeadHandle,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum CycleStatus {
    Disabled,
    Idle,
    BelowThreshold,
    Complete,
    BudgetExhausted,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct CollectOutcome {
    pub cycle_status: CycleStatus,
    pub collected: usize,
    pub cycle_roots: usize,
    pub pending_cycles: usize,
}

impl CollectOutcome {
    fn new(cycle_status: CycleStatus, pending_cycles: usize) -> Self {
        Self {
            cycle_status,
            collected: 0,
            cycle_roots: 0,
            pending_cycles,
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
struct CollectionControl {
    enabled: bool,
    cycle_threshold: usize,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub(crate) enum ReleaseOwner {
    Handle,
    Root,
    Erased,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct RootId<'cx, T> {
    index: u32,
    generation: u32,
    heap_id: HeapId,
    heap_type_id: HeapTypeId,
    marker: PhantomData<fn(T) -> T>,
    brand: PhantomData<Cell<&'cx ()>>,
}

pub struct ErasedHandle<'cx> {
    ptr: ObjPtr<'cx>,
    state: StatePtr<'cx, SharedState<'cx>>,
    generation: u64,
    heap_type_id: HeapTypeId,
    brand: PhantomData<Cell<&'cx ()>>,
    not_send_sync: PhantomData<Rc<()>>,
}

impl ErasedHandle<'_> {
    #[inline]
    pub fn heap_type_id(&self) -> HeapTypeId {
        self.heap_type_id
    }
}

impl Clone for ErasedHandle<'_> {
    fn clone(&self) -> Self {
        let state = self.state.get();
        state
            .retain_strong(self.ptr, self.generation)
            .expect("cloned dead or stale heap handle");
        state.metrics.clones.set(state.metrics.clones.get() + 1);
        Self {
            ptr: self.ptr,
            state: self.state,
            generation: self.generation,
            heap_type_id: self.heap_type_id,
            brand: PhantomData,
            not_send_sync: PhantomData,
        }
    }
}

impl Drop for ErasedHandle<'_> {
    fn drop(&mut self) {
        self.state
            .get()
            .release_strong(self.ptr, self.generation, ReleaseOwner::Erased);
    }
}

impl<T> RootId<'_, T> {
    #[inline]
    pub fn index(&self) -> u32 {
        self.index
    }

    #[inline]
    pub fn generation(&self) -> u32 {
        self.generation
    }
}

struct RootEntry<'cx> {
    ptr: ObjPtr<'cx>,
    handle_generation: u64,
    root_generation: u32,
    heap_type_id: HeapTypeId,
    occupied: bool,
    next_free: Option<u32>,
}

#[derive(Default)]
struct RootTable<'cx> {
    entries: Vec<RootEntry<'cx>>,
    free_head: Option<u32>,
}

impl<'cx> RootTable<'cx> {
    fn insert<T>(
        &mut self,
        heap_id: HeapId,
        ptr: ObjPtr<'cx>,
        handle_generation: u64,
        heap_type_id: HeapTypeId,
    ) -> RootId<'cx, T> {
        let index = match self.free_head {
            Some(index) => {
                let entry = &mut self.entries[index as usize];
                debug_assert!(!entry.occupied);
                self.free_head = entry.next_free;
                entry.next_free = None;
                index
            }
            None => {
                let index = u32::try_from(self.entries.len()).expect("heap root table overflow");
                self.entries.push(RootEntry {
                    ptr,
                    handle_generation,
                    root_generation: 0,
                    heap_type_id,
                    occupied: false,
                    next_free: None,
                });
                index
            }
        };
        let entry = &mut self.entries[index as usize];
        entry.ptr = ptr;
        entry.handle_generation = handle_generation;
        entry.heap_type_id = heap_type_id;
        entry.root_generation = entry
            .root_generation
            .checked_add(1)
            .expect("heap root generation overflow");
        entry.occupied = true;
        entry.next_free = None;
        RootId {
            index,
            generation: entry.root_generation,
            heap_id,
            heap_type_id,
            marker: PhantomData,
            brand: PhantomData,
        }
    }

    fn resolve<T>(&self, heap_id: HeapId, root: &RootId<'cx, T>) -> Option<(ObjPtr<'cx>, u64)> {
        let entry = self.entries.get(root.index as usize)?;
        if root.heap_id != heap_id
            || root.heap_type_id != entry.heap_type_id
            || !entry.occupied
            || entry.root_generation != root.generation
        {
            return None;
        }
        let header = entry.ptr.header();
        if header.generation.get() != entry.handle_generation || !header.is_live() {
            return None;
        }
        Some((entry.ptr, entry.handle_generation))
    }

    fn remove<T>(&mut self, heap_id: HeapId, root: &RootId<'cx, T>) -> Option<(ObjPtr<'cx>, u64)> {
        let entry = self.entries.get_mut(root.index as usize)?;
        if root.heap_id != heap_id
            || root.heap_type_id != entry.heap_type_id
            || !entry.occupied
            || entry.root_generation != root.generation
        {
            return None;
        }
        entry.occupied = false;
        entry.next_free = self.free_head;
        self.free_head = Some(root.index);
        Some((entry.ptr, entry.handle_generation))
    }

    fn drain_occupied(&mut self) -> Vec<(ObjPtr<'cx>, u64)> {
        self.free_head = None;
        self.entries
            .drain(..)
            .filter(|entry| entry.occupied)
            .map(|entry| (entry.ptr, entry.handle_generation))
            .collect()
    }
}

#[derive(Debug, Copy, Clone)]
pub struct HeapConfig {
    pub collection_enabled: bool,
    pub cycle_threshold: usize,
}

impl Default for HeapConfig {
    fn default() -> Self {
        Self {
            collection_enabled: true,
            cycle_threshold: 0,
        }
    }
}

pub(crate) struct HeapTypeState<'cx> {
    vtable: Box<VTable<'cx>>,
    trace_mode: TraceMode,
    free_list: Vec<ObjPtr<'cx>>,
    pages: Vec<SlotPage<'cx>>,
}

pub(crate) struct SharedState<'cx> {
    pub(crate) heap_id: HeapId,
    pub(crate) suspects: RefCell<WorkQueue<'cx>>,
    pub(crate) zeroes: RefCell<WorkQueue<'cx>>,
    pub(crate) metrics: MetricsCells,
    type_states: RefCell<Vec<HeapTypeState<'cx>>>,
    roots: RefCell<RootTable<'cx>>,
    poisoned: Cell<bool>,
    next_generation: Cell<u64>,
    next_epoch: Cell<u64>,
}

impl<'cx> SharedState<'cx> {
    #[inline]
    fn new() -> Self {
        Self {
            heap_id: HeapId::fresh(),
            suspects: RefCell::default(),
            zeroes: RefCell::default(),
            metrics: MetricsCells::default(),
            type_states: RefCell::default(),
            roots: RefCell::default(),
            poisoned: Cell::new(false),
            next_generation: Cell::new(1),
            next_epoch: Cell::new(1),
        }
    }

    #[inline]
    fn assert_not_poisoned(&self) {
        assert!(!self.poisoned.get(), "heap poisoned by payload drop panic");
    }

    #[inline]
    fn poison(&self) {
        self.poisoned.set(true);
    }

    #[inline]
    fn next_generation(&self) -> u64 {
        let generation = self.next_generation.get();
        self.next_generation
            .set(generation.checked_add(1).expect("heap generation overflow"));
        generation
    }

    #[inline]
    pub(crate) fn retain_strong(&self, ptr: ObjPtr<'cx>, generation: u64) -> Option<()> {
        self.assert_not_poisoned();
        let header = ptr.header();
        if header.generation.get() != generation || !header.is_live() {
            return None;
        }
        header.strong.set(
            header
                .strong
                .get()
                .checked_add(1)
                .expect("heap handle count overflow"),
        );
        Some(())
    }

    pub(crate) fn release_strong(&self, ptr: ObjPtr<'cx>, generation: u64, owner: ReleaseOwner) {
        let header = ptr.header();
        if header.generation.get() != generation || !header.is_live() {
            return;
        }
        let strong = header.strong.get();
        debug_assert!(strong > 0, "heap handle underflow");
        let next = strong - 1;
        header.strong.set(next);
        if owner == ReleaseOwner::Handle {
            self.metrics.drops.set(self.metrics.drops.get() + 1);
        }
        if next == 0 {
            let pending = {
                let mut zeroes = self.zeroes.borrow_mut();
                zeroes.push(ptr);
                zeroes.pending_len()
            };
            self.metrics
                .zeroes_peak
                .set(self.metrics.zeroes_peak.get().max(pending));
        } else if header.trace_mode == TraceMode::Tracked {
            self.buffer_suspect(ptr);
        }
    }

    pub(crate) fn buffer_suspect(&self, ptr: ObjPtr<'cx>) {
        let header = ptr.header();
        debug_assert_eq!(header.trace_mode, TraceMode::Tracked);
        if header.buffered.replace(true) {
            return;
        }

        header.color.set(Color::Purple);
        let pending = {
            let mut suspects = self.suspects.borrow_mut();
            suspects.push(ptr);
            suspects.pending_len()
        };
        self.metrics.suspects.set(self.metrics.suspects.get() + 1);
        self.metrics
            .suspects_peak
            .set(self.metrics.suspects_peak.get().max(pending));
    }

    fn for_each_initialized_slot(&self, mut f: impl FnMut(ObjPtr<'cx>)) {
        for state in &*self.type_states.borrow() {
            for page in &state.pages {
                page.for_each_initialized(&mut f);
            }
        }
    }
}

type DropPayload = Box<dyn Any + Send + 'static>;

#[cfg(panic = "unwind")]
#[inline]
fn drop_payload_result(ptr: ObjPtr<'_>) -> Result<(), DropPayload> {
    let header = ptr.header();
    panic::catch_unwind(AssertUnwindSafe(|| {
        header.vtable().drop_payload(ptr);
    }))
}

#[cfg(panic = "abort")]
#[inline]
fn drop_payload_result(ptr: ObjPtr<'_>) -> Result<(), DropPayload> {
    ptr.header().vtable().drop_payload(ptr);
    Ok(())
}

impl Drop for SharedState<'_> {
    fn drop(&mut self) {
        let roots = self.roots.get_mut().drain_occupied();
        for (ptr, generation) in roots {
            self.release_strong(ptr, generation, ReleaseOwner::Root);
        }

        let mut live = vec![];
        self.for_each_initialized_slot(|ptr| {
            if ptr.header().begin_drop() {
                live.push(ptr);
            }
        });

        let mut panic_payload = None;
        for ptr in live {
            let header = ptr.header();
            match drop_payload_result(ptr) {
                Ok(()) => header.finish_drop(),
                Err(payload) => {
                    header.finish_drop();
                    self.poison();
                    panic_payload.get_or_insert(payload);
                }
            }
        }

        if let Some(payload) = panic_payload {
            panic::resume_unwind(payload);
        }
    }
}

#[derive(Default)]
struct Scratch<'cx> {
    work_items: Vec<WorkItem<'cx>>,
    candidate: Vec<ObjPtr<'cx>>,
    stack: Vec<ObjPtr<'cx>>,
}

#[derive(Debug, Default, Copy, Clone)]
struct CycleBatchStats {
    roots: usize,
    stale_roots: usize,
    candidates: usize,
    edge_visits: usize,
    internal_edges: usize,
    blackened: usize,
    candidate_strong_sum: usize,
}

impl CycleBatchStats {
    fn cost(self) -> usize {
        (self.candidates + self.edge_visits).max(self.roots).max(1)
    }
}

pub struct GrayTraceState<'a, 'cx> {
    scratch: &'a mut Scratch<'cx>,
    epoch: u64,
    stats: &'a mut CycleBatchStats,
}

pub struct BlackTraceState<'a, 'cx> {
    scratch: &'a mut Scratch<'cx>,
    epoch: u64,
    stats: &'a mut CycleBatchStats,
}

impl<'cx> sealed::Driver<'cx> for GrayTrace {
    type State<'a>
        = GrayTraceState<'a, 'cx>
    where
        'cx: 'a;

    #[inline]
    fn edge(state: &mut Self::State<'_>, child: ObjPtr<'cx>) {
        let child_header = child.header();
        if child_header.visit_epoch.get() != state.epoch {
            mark_candidate_in(state.scratch, child, state.epoch, state.stats);
        }
        if child_header.visit_epoch.get() == state.epoch {
            child_header
                .trial
                .set(child_header.trial.get().saturating_sub(1));
            state.stats.internal_edges += 1;
        }
        state.stats.edge_visits += 1;
    }
}

impl TraceDriver<'_> for GrayTrace {}

impl<'cx> sealed::Driver<'cx> for BlackTrace {
    type State<'a>
        = BlackTraceState<'a, 'cx>
    where
        'cx: 'a;

    fn edge(state: &mut Self::State<'_>, child: ObjPtr<'cx>) {
        let child_header = child.header();
        if child_header.visit_epoch.get() == state.epoch && child_header.color.get() != Color::Black
        {
            state.scratch.stack.push(child);
        }
        state.stats.edge_visits += 1;
    }
}

impl TraceDriver<'_> for BlackTrace {}

fn mark_candidate_in<'cx>(
    scratch: &mut Scratch<'cx>,
    ptr: ObjPtr<'cx>,
    epoch: u64,
    stats: &mut CycleBatchStats,
) {
    let header = ptr.header();
    if !header.is_live() || header.visit_epoch.get() == epoch {
        return;
    }

    header.visit_epoch.set(epoch);
    header.color.set(Color::Gray);
    let strong = header.strong.get();
    header.trial.set(strong);
    stats.candidate_strong_sum += strong as usize;
    scratch.candidate.push(ptr);
    scratch.stack.push(ptr);
}

pub struct Heap<'cx> {
    pub(crate) state: Box<SharedState<'cx>>,
    scratch: Scratch<'cx>,
    collection: CollectionControl,
    brand: PhantomData<Cell<&'cx ()>>,
    not_send_sync: PhantomData<Rc<()>>,
}

impl Heap<'_> {
    #[inline]
    pub fn scope<R>(f: impl for<'cx> FnOnce(&mut Heap<'cx>) -> R) -> R {
        Self::scope_with_config(HeapConfig::default(), f)
    }

    #[inline]
    pub fn scope_owned<R>(f: impl for<'cx> FnOnce(Heap<'cx>) -> R) -> R {
        Self::scope_owned_with_config(HeapConfig::default(), f)
    }

    #[inline]
    pub fn scope_with_config<R>(
        config: HeapConfig,
        f: impl for<'cx> FnOnce(&mut Heap<'cx>) -> R,
    ) -> R {
        let mut heap = Heap::new(config);
        f(&mut heap)
    }

    #[inline]
    pub fn scope_owned_with_config<R>(
        config: HeapConfig,
        f: impl for<'cx> FnOnce(Heap<'cx>) -> R,
    ) -> R {
        f(Heap::new(config))
    }
}

impl<'cx> Heap<'cx> {
    #[inline]
    fn new(config: HeapConfig) -> Self {
        Self {
            state: Box::new(SharedState::new()),
            scratch: Scratch::default(),
            collection: CollectionControl {
                enabled: config.collection_enabled,
                cycle_threshold: config.cycle_threshold,
            },
            brand: PhantomData,
            not_send_sync: PhantomData,
        }
    }

    #[inline]
    pub fn register_untracked<T: 'cx>(&mut self) -> HeapType<'cx, T> {
        self.register_type(VTable::new_untracked::<T>(), TraceMode::Untracked)
    }

    #[inline]
    pub fn register_tracked<T: Trace<'cx> + 'cx>(&mut self) -> HeapType<'cx, T> {
        self.register_type(VTable::new_tracked::<T>(), TraceMode::Tracked)
    }

    #[inline]
    fn register_type<T: 'cx>(
        &mut self,
        vtable: VTable<'cx>,
        trace_mode: TraceMode,
    ) -> HeapType<'cx, T> {
        let type_states = self.state.type_states.get_mut();
        let id = HeapTypeId(type_states.len());
        type_states.push(HeapTypeState {
            vtable: Box::new(vtable),
            trace_mode,
            free_list: vec![],
            pages: vec![],
        });
        HeapType::new(self.state.heap_id, id, trace_mode)
    }

    pub fn alloc<T: 'cx>(&mut self, heap_type: HeapType<'cx, T>, value: T) -> Handle<'cx, T> {
        self.state.assert_not_poisoned();
        assert_eq!(
            heap_type.heap_id, self.state.heap_id,
            "heap type belongs to another heap"
        );
        let generation = self.state.next_generation();
        let (ptr, vtable, initialized) = self.pop_free(heap_type);
        if initialized {
            ptr.header()
                .reset(generation, heap_type.id, heap_type.trace_mode, vtable);
        } else {
            // SAFETY: this fresh slot came from a page allocated for `T`.
            unsafe {
                Slot::<T>::write_header(
                    ptr,
                    Header::live(generation, heap_type.id, heap_type.trace_mode, vtable),
                );
            }
        }
        // SAFETY: the old payload is gone, and the header now matches `T`.
        unsafe { Slot::write_payload_unchecked(ptr, value) };
        self.state
            .metrics
            .allocs
            .set(self.state.metrics.allocs.get() + 1);
        let live = self.state.metrics.live.get() + 1;
        self.state.metrics.live.set(live);
        self.state
            .metrics
            .live_peak
            .set(self.state.metrics.live_peak.get().max(live));
        Handle::new(ptr, &self.state)
    }

    #[inline]
    pub fn root<T: 'cx>(&mut self, handle: &Handle<'cx, T>) -> RootId<'cx, T> {
        let heap_type_id = self
            .check_handle(handle)
            .expect("invalid heap handle")
            .heap_type_id
            .get();
        self.state
            .retain_strong(handle.ptr, handle.generation)
            .expect("invalid heap handle");
        self.state.roots.get_mut().insert(
            self.state.heap_id,
            handle.ptr,
            handle.generation,
            heap_type_id,
        )
    }

    #[inline]
    pub fn resolve_root<T: 'cx>(&self, root: &RootId<'cx, T>) -> Option<Handle<'cx, T>> {
        let (ptr, generation) = self.root_ptr(root)?;
        self.state.retain_strong(ptr, generation)?;
        Some(Handle::from_raw(ptr, &self.state, generation))
    }

    #[inline]
    pub fn with_root<T: 'cx, R>(
        &self,
        root: &RootId<'cx, T>,
        f: impl FnOnce(&T) -> R,
    ) -> Option<R> {
        let (ptr, _) = self.root_ptr(root)?;
        // SAFETY: `root_ptr` validated the live root for `T`.
        Some(f(unsafe { Slot::payload_ref_unchecked(ptr) }))
    }

    #[inline]
    pub fn with_root_mut<T: 'cx, R>(
        &mut self,
        root: &RootId<'cx, T>,
        f: impl FnOnce(&mut T) -> R,
    ) -> Option<R> {
        let (ptr, _) = self.root_ptr(root)?;
        // SAFETY: `root_ptr` validated the live root; `&mut Heap` is exclusive access.
        Some(f(unsafe { Slot::payload_mut_unchecked(ptr) }))
    }

    #[inline]
    pub fn remove_root<T>(&mut self, root: &RootId<'cx, T>) -> bool {
        self.state.assert_not_poisoned();
        let removed = self.state.roots.get_mut().remove(self.state.heap_id, root);
        let Some((ptr, generation)) = removed else {
            return false;
        };
        self.state
            .release_strong(ptr, generation, ReleaseOwner::Root);
        true
    }

    #[inline]
    fn root_ptr<T>(&self, root: &RootId<'cx, T>) -> Option<(ObjPtr<'cx>, u64)> {
        self.state.assert_not_poisoned();
        self.state.roots.borrow().resolve(self.state.heap_id, root)
    }

    #[inline]
    fn pop_free<T: 'cx>(
        &mut self,
        heap_type: HeapType<'cx, T>,
    ) -> (ObjPtr<'cx>, NonNull<VTable<'cx>>, bool) {
        let type_states = self.state.type_states.get_mut();
        let state = &mut type_states[heap_type.id.0];
        debug_assert_eq!(state.trace_mode, heap_type.trace_mode);
        let vtable = NonNull::from(state.vtable.as_ref());
        if let Some(ptr) = state.free_list.pop() {
            return (ptr, vtable, true);
        }

        if state.pages.last().is_none_or(SlotPage::is_full) {
            state
                .pages
                .push(SlotPage::new::<T>(Self::slots_per_page::<T>()));
        }
        // SAFETY: `heap_type` indexes the type state whose pages store `Slot<'cx, T>`.
        let ptr = unsafe {
            state
                .pages
                .last_mut()
                .and_then(|page| page.next_slot::<T>())
                .expect("heap slot page created no slots")
        };
        (ptr, vtable, false)
    }

    #[inline]
    fn slots_per_page<T: 'cx>() -> usize {
        let slot_size = size_of::<Slot<'cx, T>>().max(1);
        (TARGET_SLOT_PAGE_BYTES / slot_size).clamp(1, MAX_SLOT_PAGE_SLOTS)
    }

    #[inline]
    pub fn with<T: 'cx, R>(&self, handle: &Handle<'cx, T>, f: impl FnOnce(&T) -> R) -> R {
        self.try_with(handle, f).expect("invalid heap handle")
    }

    #[inline]
    pub fn with_mut<T: 'cx, R>(
        &mut self,
        handle: &Handle<'cx, T>,
        f: impl FnOnce(&mut T) -> R,
    ) -> R {
        self.try_with_mut(handle, f).expect("invalid heap handle")
    }

    #[inline]
    pub fn try_with<T: 'cx, R>(
        &self,
        handle: &Handle<'cx, T>,
        f: impl FnOnce(&T) -> R,
    ) -> Result<R, AccessError> {
        self.check_handle(handle)?;
        // SAFETY: `check_handle` validated the live handle for `T`.
        let value = unsafe { Slot::payload_ref_unchecked(handle.ptr) };
        Ok(f(value))
    }

    #[inline]
    pub fn try_with_mut<T: 'cx, R>(
        &mut self,
        handle: &Handle<'cx, T>,
        f: impl FnOnce(&mut T) -> R,
    ) -> Result<R, AccessError> {
        self.check_handle(handle)?;
        // SAFETY: `check_handle` validated the handle; `&mut Heap` is exclusive access.
        let value = unsafe { Slot::payload_mut_unchecked(handle.ptr) };
        Ok(f(value))
    }

    #[inline]
    pub fn erase<T: 'cx>(&self, handle: &Handle<'cx, T>) -> Result<ErasedHandle<'cx>, AccessError> {
        let header = self.check_handle(handle)?;
        self.state
            .retain_strong(handle.ptr, handle.generation)
            .ok_or(AccessError::DeadHandle)?;
        Ok(ErasedHandle {
            ptr: handle.ptr,
            state: handle.state,
            generation: handle.generation,
            heap_type_id: header.heap_type_id.get(),
            brand: PhantomData,
            not_send_sync: PhantomData,
        })
    }

    #[inline]
    pub fn try_with_erased<T: 'cx, R>(
        &self,
        handle: &ErasedHandle<'cx>,
        heap_type: HeapType<'cx, T>,
        f: impl FnOnce(&T) -> R,
    ) -> Result<R, AccessError> {
        self.check_erased(handle, heap_type)?;
        // SAFETY: `check_erased` validated the live handle and type descriptor.
        Ok(f(unsafe { Slot::payload_ref_unchecked(handle.ptr) }))
    }

    #[inline]
    pub fn try_with_erased_mut<T: 'cx, R>(
        &mut self,
        handle: &ErasedHandle<'cx>,
        heap_type: HeapType<'cx, T>,
        f: impl FnOnce(&mut T) -> R,
    ) -> Result<R, AccessError> {
        self.check_erased(handle, heap_type)?;
        // SAFETY: `check_erased` validated the handle; `&mut Heap` is exclusive access.
        Ok(f(unsafe { Slot::payload_mut_unchecked(handle.ptr) }))
    }

    #[inline]
    fn check_handle<T: 'cx>(&self, handle: &Handle<'cx, T>) -> Result<&Header<'cx>, AccessError> {
        self.state.assert_not_poisoned();
        if StatePtr::new(self.state.as_ref()) != handle.state {
            return Err(AccessError::WrongHeap);
        }
        let header = handle.ptr.header();
        if header.generation.get() != handle.generation || !header.is_live() {
            return Err(AccessError::DeadHandle);
        }
        Ok(header)
    }

    #[inline]
    fn check_erased<T: 'cx>(
        &self,
        handle: &ErasedHandle<'cx>,
        heap_type: HeapType<'cx, T>,
    ) -> Result<(), AccessError> {
        self.state.assert_not_poisoned();
        if StatePtr::new(self.state.as_ref()) != handle.state
            || heap_type.heap_id != self.state.heap_id
        {
            return Err(AccessError::WrongHeap);
        }
        let header = handle.ptr.header();
        if handle.heap_type_id != heap_type.id
            || header.heap_type_id.get() != heap_type.id
            || header.generation.get() != handle.generation
            || !header.is_live()
        {
            return Err(AccessError::DeadHandle);
        }
        Ok(())
    }

    #[inline]
    pub fn set_collection_enabled(&mut self, enabled: bool) {
        self.collection.enabled = enabled;
    }

    #[inline]
    pub fn collection_enabled(&self) -> bool {
        self.collection.enabled
    }

    #[inline]
    pub fn set_cycle_threshold(&mut self, pending_cycle_candidates: usize) {
        self.collection.cycle_threshold = pending_cycle_candidates;
    }

    #[inline]
    pub fn cycle_threshold(&self) -> usize {
        self.collection.cycle_threshold
    }

    #[inline]
    pub fn collect(&mut self, cycle_work_hint: usize) -> CollectOutcome {
        self.collect_impl::<()>(Some(cycle_work_hint), true, true, None)
    }

    #[inline]
    pub fn collect_with_roots<T: Trace<'cx>>(
        &mut self,
        cycle_work_hint: usize,
        roots: &T,
    ) -> CollectOutcome {
        self.collect_impl(Some(cycle_work_hint), true, true, Some(roots))
    }

    #[inline]
    pub fn collect_all(&mut self) -> CollectOutcome {
        self.collect_impl::<()>(None, false, true, None)
    }

    #[inline]
    pub fn collect_all_with_roots<T: Trace<'cx>>(&mut self, roots: &T) -> CollectOutcome {
        self.collect_impl(None, false, true, Some(roots))
    }

    fn collect_impl<T: Trace<'cx>>(
        &mut self,
        cycle_work_hint: Option<usize>,
        apply_threshold: bool,
        respect_enabled: bool,
        external_roots: Option<&T>,
    ) -> CollectOutcome {
        self.state.assert_not_poisoned();
        if respect_enabled && !self.collection.enabled {
            return CollectOutcome::new(
                CycleStatus::Disabled,
                self.pending_cycle_candidates_internal(),
            );
        }

        let collected_before = self.state.metrics.collected.get();
        self.collect_zeroes();

        let pending = self.pending_cycle_candidates_internal();
        let mut outcome = CollectOutcome::new(CycleStatus::Idle, pending);
        if pending == 0 {
            outcome.collected = self.state.metrics.collected.get() - collected_before;
            return outcome;
        }
        if apply_threshold
            && self.collection.cycle_threshold > 0
            && pending < self.collection.cycle_threshold
        {
            outcome.cycle_status = CycleStatus::BelowThreshold;
            outcome.collected = self.state.metrics.collected.get() - collected_before;
            return outcome;
        }

        let Some(remaining_work) =
            cycle_work_hint.map(|hint| Self::effective_work_budget(hint, pending))
        else {
            outcome.cycle_roots = self.collect_all_pending(external_roots);
            outcome.pending_cycles = self.pending_cycle_candidates_internal();
            outcome.collected = self.state.metrics.collected.get() - collected_before;
            outcome.cycle_status = CycleStatus::Complete;
            return outcome;
        };

        if remaining_work == 0 {
            outcome.cycle_status = CycleStatus::BudgetExhausted;
            outcome.collected = self.state.metrics.collected.get() - collected_before;
            return outcome;
        }

        if self.pending_cycle_candidates_internal() > 0 {
            self.record_budget_debt(remaining_work);
            let root_limit = (remaining_work / ESTIMATED_ROOT_COST).clamp(1, MAX_ROOT_BATCH);
            self.take_suspects_up_to(root_limit);
            if !self.scratch.work_items.is_empty() {
                let stats = self.collect_suspect_roots(remaining_work, external_roots);
                outcome.cycle_roots += stats.roots;
            }
        }

        self.collect_zeroes();
        outcome.pending_cycles = self.pending_cycle_candidates_internal();
        outcome.collected = self.state.metrics.collected.get() - collected_before;
        outcome.cycle_status = if outcome.pending_cycles == 0 {
            CycleStatus::Complete
        } else {
            CycleStatus::BudgetExhausted
        };
        outcome
    }

    fn collect_all_pending<T: Trace<'cx>>(&mut self, external_roots: Option<&T>) -> usize {
        let mut roots = 0;
        loop {
            self.collect_zeroes();
            if self.pending_cycle_candidates_internal() == 0 {
                return roots;
            }
            self.take_suspects_up_to(usize::MAX);
            if !self.scratch.work_items.is_empty() {
                roots += self.collect_suspect_roots(usize::MAX, external_roots).roots;
            }
        }
    }

    #[inline]
    fn effective_work_budget(hint: usize, pending: usize) -> usize {
        if hint == 0 || hint == usize::MAX {
            return hint;
        }
        let needed = pending.saturating_mul(ESTIMATED_ROOT_COST);
        if needed <= hint {
            hint
        } else {
            needed.min(hint.saturating_mul(4))
        }
    }

    #[inline]
    fn collect_zeroes(&mut self) {
        loop {
            self.take_zeroes();
            if self.scratch.work_items.is_empty() {
                return;
            }
            let mut index = 0;
            while index < self.scratch.work_items.len() {
                let item = self.scratch.work_items[index];
                index += 1;
                let Some(ptr) = item.live_ptr() else {
                    continue;
                };
                let header = ptr.header();
                if header.strong.get() == 0 {
                    self.free_slot(ptr);
                }
            }
        }
    }

    #[inline]
    fn take_suspects_up_to(&mut self, limit: usize) {
        self.state
            .suspects
            .get_mut()
            .take_up_to_into(&mut self.scratch.work_items, limit);
    }

    #[inline]
    fn take_zeroes(&mut self) {
        self.state
            .zeroes
            .get_mut()
            .take_all_into(&mut self.scratch.work_items);
    }

    fn collect_suspect_roots<T: Trace<'cx>>(
        &mut self,
        remaining_budget: usize,
        external_roots: Option<&T>,
    ) -> CycleBatchStats {
        let result = panic::catch_unwind(AssertUnwindSafe(|| {
            let mut stats = self.mark_gray_and_subtract_from_roots();
            self.state
                .metrics
                .suspects_processed
                .set(self.state.metrics.suspects_processed.get() + stats.roots);
            self.state
                .metrics
                .cycle_roots
                .set(self.state.metrics.cycle_roots.get() + stats.roots);
            self.state
                .metrics
                .stale_roots
                .set(self.state.metrics.stale_roots.get() + stats.stale_roots);
            if !self.scratch.candidate.is_empty() {
                if let Some(roots) = external_roots {
                    self.scan_external_roots(roots, &mut stats);
                }
                if stats.candidate_strong_sum != stats.internal_edges {
                    self.scan_live_candidates(&mut stats);
                }
                self.collect_white_candidates();
            }
            self.record_batch_stats(&stats, remaining_budget);
            stats
        }));
        match result {
            Ok(stats) => stats,
            Err(payload) => {
                self.reset_after_panic();
                panic::resume_unwind(payload);
            }
        }
    }

    fn mark_gray_and_subtract_from_roots(&mut self) -> CycleBatchStats {
        let mut stats = CycleBatchStats::default();
        self.scratch.candidate.clear();
        self.scratch.stack.clear();
        let epoch = self.next_epoch();

        let mut root_index = 0;
        while root_index < self.scratch.work_items.len() {
            let root = self.scratch.work_items[root_index];
            root_index += 1;
            stats.roots += 1;
            let Some(ptr) = root.live_ptr() else {
                stats.stale_roots += 1;
                continue;
            };
            let header = ptr.header();
            if header.buffered.get() && header.trace_mode == TraceMode::Tracked {
                mark_candidate_in(&mut self.scratch, ptr, epoch, &mut stats);
            } else {
                stats.stale_roots += 1;
            }
        }

        while let Some(ptr) = self.scratch.stack.pop() {
            let header = ptr.header();
            if header.is_live() {
                let mut state = GrayTraceState {
                    scratch: &mut self.scratch,
                    epoch,
                    stats: &mut stats,
                };
                let mut visitor = Visitor::<GrayTrace>::new(self.state.heap_id, &mut state);
                header.vtable().trace_gray(ptr, &mut visitor);
            }
        }

        stats.candidates = self.scratch.candidate.len();
        stats
    }

    #[inline]
    fn scan_live_candidates(&mut self, stats: &mut CycleBatchStats) {
        let epoch = self.current_epoch();
        let mut index = 0;
        while index < self.scratch.candidate.len() {
            let ptr = self.scratch.candidate[index];
            index += 1;
            let header = ptr.header();
            if header.is_live() && header.color.get() == Color::Gray && header.trial.get() > 0 {
                self.scan_black(ptr, epoch, stats);
            }
        }
    }

    #[inline]
    fn scan_external_roots<T: Trace<'cx>>(&mut self, roots: &T, stats: &mut CycleBatchStats) {
        let epoch = self.current_epoch();
        self.scratch.stack.clear();
        let mut state = BlackTraceState {
            scratch: &mut self.scratch,
            epoch,
            stats,
        };
        let mut visitor = Visitor::<BlackTrace>::new(self.state.heap_id, &mut state);
        roots.trace(&mut visitor);
        self.blacken_stack(epoch, stats);
    }

    #[inline]
    fn scan_black(&mut self, root: ObjPtr<'cx>, epoch: u64, stats: &mut CycleBatchStats) {
        self.scratch.stack.clear();
        self.scratch.stack.push(root);
        self.blacken_stack(epoch, stats);
    }

    fn blacken_stack(&mut self, epoch: u64, stats: &mut CycleBatchStats) {
        while let Some(ptr) = self.scratch.stack.pop() {
            let header = ptr.header();
            if !header.is_live()
                || header.visit_epoch.get() != epoch
                || header.color.get() == Color::Black
            {
                continue;
            }
            header.color.set(Color::Black);
            header.buffered.set(false);
            header.trial.set(0);
            stats.blackened += 1;
            let mut state = BlackTraceState {
                scratch: &mut self.scratch,
                epoch,
                stats,
            };
            let mut visitor = Visitor::<BlackTrace>::new(self.state.heap_id, &mut state);
            header.vtable().trace_black(ptr, &mut visitor);
        }
    }

    fn collect_white_candidates(&mut self) {
        self.scratch.stack.clear();
        let mut index = 0;
        while index < self.scratch.candidate.len() {
            let ptr = self.scratch.candidate[index];
            index += 1;
            let header = ptr.header();
            if !header.is_live() {
                continue;
            }
            header.buffered.set(false);
            header.trial.set(0);
            if header.color.get() == Color::Gray {
                header.color.set(Color::White);
                self.scratch.stack.push(ptr);
            } else {
                header.color.set(Color::Black);
            }
        }
        for ptr in &self.scratch.stack {
            ptr.header().begin_drop();
        }
        self.drop_begun_slots_from_stack();
    }

    #[inline]
    fn reset_after_panic(&mut self) {
        for ptr in &self.scratch.candidate {
            let header = ptr.header();
            if header.is_live() {
                header.color.set(Color::Black);
                header.trial.set(0);
                if header.trace_mode == TraceMode::Tracked {
                    header.buffered.set(true);
                    self.state.suspects.borrow_mut().push(*ptr);
                } else {
                    header.buffered.set(false);
                }
            }
        }
    }

    #[inline]
    fn record_batch_stats(&self, stats: &CycleBatchStats, remaining_budget: usize) {
        let metrics = &self.state.metrics;
        let cost = stats.cost();
        metrics
            .cycle_batches
            .set(metrics.cycle_batches.get() + usize::from(stats.roots > 0));
        metrics
            .candidate_nodes
            .set(metrics.candidate_nodes.get() + stats.candidates);
        metrics
            .edge_visits
            .set(metrics.edge_visits.get() + stats.edge_visits);
        metrics
            .internal_edges
            .set(metrics.internal_edges.get() + stats.internal_edges);
        metrics
            .blackened
            .set(metrics.blackened.get() + stats.blackened);
        metrics
            .max_candidate
            .set(metrics.max_candidate.get().max(stats.candidates));
        metrics
            .max_batch_cost
            .set(metrics.max_batch_cost.get().max(cost));
        if cost > remaining_budget {
            metrics
                .budget_overshoots
                .set(metrics.budget_overshoots.get() + 1);
        }
    }

    #[inline]
    fn record_budget_debt(&self, budget: usize) {
        let pending = self.pending_cycle_candidates_internal();
        self.state.metrics.budget_debt_peak.set(
            self.state
                .metrics
                .budget_debt_peak
                .get()
                .max(pending.saturating_sub(budget)),
        );
    }

    #[inline]
    fn next_epoch(&self) -> u64 {
        let next = self
            .state
            .next_epoch
            .get()
            .checked_add(1)
            .expect("heap collection epoch overflow");
        self.state.next_epoch.set(next);
        next
    }

    #[inline]
    fn current_epoch(&self) -> u64 {
        self.state.next_epoch.get()
    }

    fn free_slot(&mut self, ptr: ObjPtr<'cx>) {
        if !ptr.header().begin_drop() {
            return;
        }
        match self.drop_slot_payload(ptr) {
            Ok(()) => {
                Self::finish_dropped_slot(ptr);
                self.scratch.stack.clear();
                self.scratch.stack.push(ptr);
                self.finish_free_batch(None);
            }
            Err(payload) => {
                ptr.header().finish_drop();
                panic::resume_unwind(payload);
            }
        }
    }

    fn drop_begun_slots_from_stack(&mut self) {
        let dropping = self.scratch.stack.len();
        let mut freed = 0;
        let mut panic_payload = None;
        for index in 0..dropping {
            let ptr = self.scratch.stack[index];
            match self.drop_slot_payload(ptr) {
                Ok(()) => {
                    Self::finish_dropped_slot(ptr);
                    self.scratch.stack[freed] = ptr;
                    freed += 1;
                }
                Err(payload) => {
                    ptr.header().finish_drop();
                    panic_payload.get_or_insert(payload);
                }
            }
        }

        self.scratch.stack.truncate(freed);
        self.finish_free_batch(panic_payload);
    }

    fn finish_free_batch(&mut self, panic_payload: Option<DropPayload>) {
        let freed = self.scratch.stack.len();
        if freed > 0 {
            self.state
                .metrics
                .collected
                .set(self.state.metrics.collected.get() + freed);
            self.state
                .metrics
                .live
                .set(self.state.metrics.live.get().saturating_sub(freed));
            self.return_free_slots_from_stack(freed);
        }
        self.scratch.stack.clear();

        if let Some(payload) = panic_payload {
            panic::resume_unwind(payload);
        }
    }

    #[inline]
    fn drop_slot_payload(&mut self, ptr: ObjPtr<'cx>) -> Result<(), DropPayload> {
        if let Err(payload) = drop_payload_result(ptr) {
            self.state.poison();
            return Err(payload);
        }
        Ok(())
    }

    #[inline]
    fn finish_dropped_slot(ptr: ObjPtr<'cx>) {
        let header = ptr.header();
        header.finish_drop();
        header.buffered.set(false);
        header.color.set(Color::Black);
        header.trial.set(0);
    }

    fn return_free_slots_from_stack(&mut self, count: usize) {
        let type_states = self.state.type_states.get_mut();
        for index in 0..count {
            let ptr = self.scratch.stack[index];
            let heap_type_id = ptr.header().heap_type_id.get().0;
            type_states[heap_type_id].free_list.push(ptr);
        }
    }

    #[inline]
    pub fn stats(&self) -> HeapStats {
        self.state.metrics.snapshot()
    }

    #[inline]
    pub fn reset_stats(&mut self) {
        self.state.metrics.reset();
    }

    #[inline]
    fn pending_cycle_candidates_internal(&self) -> usize {
        self.state.suspects.borrow().pending_len()
    }

    pub fn leak_report(&self) -> LeakReport {
        let mut by_type = BTreeMap::<usize, LeakTypeReport>::new();
        let type_states = self.state.type_states.borrow();
        for state in &*type_states {
            for page in &state.pages {
                page.for_each_initialized(|ptr| {
                    let header = ptr.header();
                    if !header.is_live() {
                        return;
                    }
                    let heap_type_id = header.heap_type_id.get();
                    let entry = by_type.entry(heap_type_id.0).or_insert(LeakTypeReport {
                        heap_type_id,
                        type_name: type_states[heap_type_id.0].vtable.type_name,
                        live: 0,
                        suspects: 0,
                    });
                    entry.live += 1;
                });
            }
        }
        for item in self.state.suspects.borrow().live_items() {
            if let Some(ptr) = item.live_ptr() {
                let header = ptr.header();
                let heap_type_id = header.heap_type_id.get();
                let entry = by_type.entry(heap_type_id.0).or_insert(LeakTypeReport {
                    heap_type_id,
                    type_name: type_states[heap_type_id.0].vtable.type_name,
                    live: 0,
                    suspects: 0,
                });
                entry.suspects += 1;
            }
        }
        LeakReport {
            live: self.stats().live,
            by_type: by_type.into_values().collect(),
        }
    }

    pub fn finalize(&mut self) -> Result<(), LeakReport> {
        self.state.assert_not_poisoned();
        self.collect_impl::<()>(None, false, false, None);
        let report = self.leak_report();
        if report.live == 0 {
            Ok(())
        } else {
            Err(report)
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct LeakReport {
    pub live: usize,
    pub by_type: Vec<LeakTypeReport>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct LeakTypeReport {
    pub heap_type_id: HeapTypeId,
    pub type_name: &'static str,
    pub live: usize,
    pub suspects: usize,
}
