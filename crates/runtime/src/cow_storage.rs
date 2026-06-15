use std::{cell::Cell, rc::Rc};

use crate::{AccessError, Handle, Heap, HeapType, Trace, TraceDriver, Visitor};

const ERR_OWNER_OVERFLOW: &str = "too many logical collection storage owners";

pub struct CowStorageOwner<'cx, S> {
    handle: Handle<'cx, S>,
    owners: Rc<Cell<u32>>,
}

pub struct CowStorageView<'cx, S> {
    handle: Handle<'cx, S>,
}

impl<S> Clone for CowStorageView<'_, S> {
    fn clone(&self) -> Self {
        Self {
            handle: self.handle.clone(),
        }
    }
}

impl<'cx, S> CowStorageOwner<'cx, S> {
    fn new(handle: Handle<'cx, S>) -> Self {
        Self {
            handle,
            owners: Rc::new(Cell::new(1)),
        }
    }

    pub fn alloc(heap: &mut Heap<'cx>, storage_ty: HeapType<'cx, S>, storage: S) -> Self
    where
        S: 'cx,
    {
        Self::new(heap.alloc(storage_ty, storage))
    }

    #[must_use]
    pub fn share(&self) -> Self {
        let handle = self.handle.clone();
        let owners = self
            .logical_owners()
            .checked_add(1)
            .expect(ERR_OWNER_OVERFLOW);
        self.owners.set(owners);
        Self {
            handle,
            owners: Rc::clone(&self.owners),
        }
    }

    #[must_use]
    pub fn view(&self) -> CowStorageView<'cx, S> {
        CowStorageView {
            handle: self.handle.clone(),
        }
    }

    pub fn logical_owners(&self) -> u32 {
        self.owners.get()
    }

    pub fn is_unique(&self) -> bool {
        self.logical_owners() == 1
    }

    pub(crate) fn handle(&self) -> &Handle<'cx, S> {
        &self.handle
    }

    pub(crate) fn make_unique(
        &mut self,
        heap: &mut Heap<'cx>,
        storage_ty: HeapType<'cx, S>,
    ) -> Result<(), AccessError>
    where
        S: Clone + 'cx,
    {
        if self.is_unique() {
            return Ok(());
        }
        let storage = heap.try_with(self.handle(), Clone::clone)?;
        let handle = heap.alloc(storage_ty, storage);
        self.replace_with_fresh(handle);
        Ok(())
    }

    fn replace_with_fresh(&mut self, handle: Handle<'cx, S>) {
        *self = Self::new(handle);
    }
}

impl<S> Drop for CowStorageOwner<'_, S> {
    fn drop(&mut self) {
        let owners = self.owners.get();
        debug_assert!(owners > 0, "collection storage owner count underflow");
        if owners > 0 {
            self.owners.set(owners - 1);
        }
    }
}

impl<'cx, S> CowStorageView<'cx, S> {
    pub(crate) fn handle(&self) -> &Handle<'cx, S> {
        &self.handle
    }
}

// SAFETY: logical collection values own exactly one strong edge to their storage object.
unsafe impl<'cx, S> Trace<'cx> for CowStorageOwner<'cx, S> {
    fn trace<D: TraceDriver<'cx>>(&self, visitor: &mut Visitor<'cx, '_, D>) {
        self.handle().trace(visitor);
    }
}

// SAFETY: views keep storage alive by owning exactly one strong edge, but do not logically own COW storage.
unsafe impl<'cx, S> Trace<'cx> for CowStorageView<'cx, S> {
    fn trace<D: TraceDriver<'cx>>(&self, visitor: &mut Visitor<'cx, '_, D>) {
        self.handle().trace(visitor);
    }
}

#[cfg(test)]
mod tests {
    use std::{cell::Cell, rc::Rc};

    use super::CowStorageOwner;
    use crate::{Handle, Heap, Trace, TraceDriver, Visitor};

    #[derive(Default)]
    struct TraceCounts {
        root: Rc<Cell<u32>>,
        storage: Rc<Cell<u32>>,
        child: Rc<Cell<u32>>,
    }

    struct TestChild {
        traces: Rc<Cell<u32>>,
    }

    struct TestStorage<'cx> {
        child: Handle<'cx, TestChild>,
        traces: Rc<Cell<u32>>,
    }

    struct TestRoot<'cx> {
        owner: CowStorageOwner<'cx, TestStorage<'cx>>,
        self_ref: Option<Handle<'cx, TestRoot<'cx>>>,
        traces: Rc<Cell<u32>>,
    }

    struct TestViewRoot<'cx> {
        view: super::CowStorageView<'cx, TestStorage<'cx>>,
        self_ref: Option<Handle<'cx, TestViewRoot<'cx>>>,
        traces: Rc<Cell<u32>>,
    }

    // SAFETY: test child owns no heap edges.
    unsafe impl<'cx> Trace<'cx> for TestChild {
        fn trace<D: TraceDriver<'cx>>(&self, _visitor: &mut Visitor<'cx, '_, D>) {
            self.traces.set(self.traces.get() + 1);
        }
    }

    // SAFETY: test storage owns exactly one child handle edge.
    unsafe impl<'cx> Trace<'cx> for TestStorage<'cx> {
        fn trace<D: TraceDriver<'cx>>(&self, visitor: &mut Visitor<'cx, '_, D>) {
            self.traces.set(self.traces.get() + 1);
            self.child.trace(visitor);
        }
    }

    // SAFETY: test root owns its COW storage owner edge and an optional self-cycle edge.
    unsafe impl<'cx> Trace<'cx> for TestRoot<'cx> {
        fn trace<D: TraceDriver<'cx>>(&self, visitor: &mut Visitor<'cx, '_, D>) {
            self.traces.set(self.traces.get() + 1);
            self.owner.trace(visitor);
            self.self_ref.trace(visitor);
        }
    }

    // SAFETY: test root owns its COW storage view edge and an optional self-cycle edge.
    unsafe impl<'cx> Trace<'cx> for TestViewRoot<'cx> {
        fn trace<D: TraceDriver<'cx>>(&self, visitor: &mut Visitor<'cx, '_, D>) {
            self.traces.set(self.traces.get() + 1);
            self.view.trace(visitor);
            self.self_ref.trace(visitor);
        }
    }

    fn storage_owner<'cx>(
        heap: &mut Heap<'cx>,
        counts: &TraceCounts,
    ) -> CowStorageOwner<'cx, TestStorage<'cx>> {
        let child_ty = heap.register_tracked::<TestChild>();
        let storage_ty = heap.register_tracked::<TestStorage<'_>>();
        let child = heap.alloc(
            child_ty,
            TestChild {
                traces: Rc::clone(&counts.child),
            },
        );
        CowStorageOwner::alloc(
            heap,
            storage_ty,
            TestStorage {
                child,
                traces: Rc::clone(&counts.storage),
            },
        )
    }

    fn cyclic_root<'cx>(
        heap: &mut Heap<'cx>,
        counts: &TraceCounts,
        owner: CowStorageOwner<'cx, TestStorage<'cx>>,
    ) -> Handle<'cx, TestRoot<'cx>> {
        let root_ty = heap.register_tracked::<TestRoot<'_>>();
        let root = heap.alloc(
            root_ty,
            TestRoot {
                owner,
                self_ref: None,
                traces: Rc::clone(&counts.root),
            },
        );
        heap.with_mut(&root, |root_data| root_data.self_ref = Some(root.clone()));
        root
    }

    fn cyclic_view_root<'cx>(
        heap: &mut Heap<'cx>,
        counts: &TraceCounts,
        view: super::CowStorageView<'cx, TestStorage<'cx>>,
    ) -> Handle<'cx, TestViewRoot<'cx>> {
        let root_ty = heap.register_tracked::<TestViewRoot<'_>>();
        let root = heap.alloc(
            root_ty,
            TestViewRoot {
                view,
                self_ref: None,
                traces: Rc::clone(&counts.root),
            },
        );
        heap.with_mut(&root, |root_data| root_data.self_ref = Some(root.clone()));
        root
    }

    #[test]
    fn share_increments_logical_owners() {
        Heap::scope(|heap| {
            let ty = heap.register_untracked::<i64>();
            let owner = CowStorageOwner::alloc(heap, ty, 1);

            let shared = owner.share();

            assert_eq!(owner.logical_owners(), 2);
            assert_eq!(shared.logical_owners(), 2);
            assert!(!owner.is_unique());
            assert!(!shared.is_unique());
        });
    }

    #[test]
    fn view_does_not_increment_logical_owners() {
        Heap::scope(|heap| {
            let ty = heap.register_untracked::<i64>();
            let owner = CowStorageOwner::alloc(heap, ty, 1);

            let view = owner.view();

            assert_eq!(owner.logical_owners(), 1);
            assert!(owner.is_unique());
            assert_eq!(view.handle().strong_count(), 2);
        });
    }

    #[test]
    fn dropping_owner_decrements_logical_count() {
        Heap::scope(|heap| {
            let ty = heap.register_untracked::<i64>();
            let owner = CowStorageOwner::alloc(heap, ty, 1);
            let shared = owner.share();

            drop(shared);

            assert_eq!(owner.logical_owners(), 1);
            assert!(owner.is_unique());
        });
    }

    #[test]
    fn owner_traces_storage_edge_and_storage_traces_child_once() {
        Heap::scope(|heap| {
            let counts = TraceCounts::default();
            let owner = storage_owner(heap, &counts);
            let root = cyclic_root(heap, &counts, owner);

            drop(root);
            heap.reset_stats();
            let outcome = heap.collect_all();

            assert_eq!(outcome.collected, 3);
            assert_eq!(counts.root.get(), 1);
            assert_eq!(counts.storage.get(), 1);
            assert_eq!(counts.child.get(), 1);
            assert_eq!(heap.stats().edge_visits, 3);
            assert_eq!(heap.stats().internal_edges, 3);
        });
    }

    #[test]
    fn shared_owners_do_not_duplicate_storage_child_edges() {
        Heap::scope(|heap| {
            let counts = TraceCounts::default();
            let owner = storage_owner(heap, &counts);
            let shared = owner.share();
            let root = cyclic_root(heap, &counts, owner);
            let shared_root = cyclic_root(heap, &counts, shared);

            drop(root);
            drop(shared_root);
            heap.reset_stats();
            let outcome = heap.collect_all();

            assert_eq!(outcome.collected, 4);
            assert_eq!(counts.root.get(), 2);
            assert_eq!(counts.storage.get(), 1);
            assert_eq!(counts.child.get(), 1);
            assert_eq!(heap.stats().edge_visits, 5);
            assert_eq!(heap.stats().internal_edges, 5);
        });
    }

    #[test]
    fn view_traces_storage_edge_after_owner_drops() {
        Heap::scope(|heap| {
            let counts = TraceCounts::default();
            let owner = storage_owner(heap, &counts);
            let view = owner.view();
            drop(owner);
            let root = cyclic_view_root(heap, &counts, view);

            drop(root);
            heap.reset_stats();
            let outcome = heap.collect_all();

            assert_eq!(outcome.collected, 3);
            assert_eq!(counts.root.get(), 1);
            assert_eq!(counts.storage.get(), 1);
            assert_eq!(counts.child.get(), 1);
            assert_eq!(heap.stats().edge_visits, 3);
            assert_eq!(heap.stats().internal_edges, 3);
        });
    }

    #[test]
    fn owner_replaces_with_fresh_storage() {
        Heap::scope(|heap| {
            let ty = heap.register_untracked::<i64>();
            let mut owner = CowStorageOwner::alloc(heap, ty, 1);

            let handle = heap.alloc(ty, 2);
            owner.replace_with_fresh(handle);

            assert_eq!(heap.with(owner.handle(), |value| *value), 2);
            assert!(owner.is_unique());
        });
    }

    #[test]
    fn fresh_replacement_detaches_from_shared_owner_count() {
        Heap::scope(|heap| {
            let ty = heap.register_untracked::<i64>();
            let mut owner = CowStorageOwner::alloc(heap, ty, 1);
            let shared = owner.share();

            let handle = heap.alloc(ty, 2);
            owner.replace_with_fresh(handle);

            assert!(owner.is_unique());
            assert!(shared.is_unique());
            assert_eq!(heap.with(owner.handle(), |value| *value), 2);
            assert_eq!(heap.with(shared.handle(), |value| *value), 1);
        });
    }

    #[test]
    fn heap_strong_count_and_logical_owners_diverge_for_views() {
        Heap::scope(|heap| {
            let ty = heap.register_untracked::<i64>();
            let owner = CowStorageOwner::alloc(heap, ty, 1);
            let view = owner.view();
            let shared = owner.share();

            assert_eq!(owner.logical_owners(), 2);
            assert_eq!(shared.logical_owners(), 2);
            assert_eq!(owner.handle().strong_count(), 3);
            assert_eq!(view.handle().strong_count(), 3);
        });
    }
}
