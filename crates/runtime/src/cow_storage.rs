use std::{cell::Cell, rc::Rc};

use crate::{AccessError, Ctx, Handle, Heap, HeapType, Trace, TraceDriver, Visitor};

const ERR_OWNER_OVERFLOW: &str = "too many logical collection storage owners";

pub struct CowStorageOwner<'cx, S> {
    handle: Handle<'cx, S>,
    owners: Rc<Cell<u32>>,
    counts_owner: bool,
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
            counts_owner: true,
        }
    }

    pub fn alloc(heap: &mut Heap<'cx>, storage_ty: HeapType<'cx, S>, storage: S) -> Self
    where
        S: 'cx,
    {
        Self::new(heap.alloc(storage_ty, storage))
    }

    pub fn alloc_in<'rt>(ctx: &mut Ctx<'cx, 'rt>, storage_ty: HeapType<'cx, S>, storage: S) -> Self
    where
        S: 'cx,
    {
        Self::new(ctx.heap().alloc(storage_ty, storage))
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
            counts_owner: true,
        }
    }

    #[must_use]
    pub(crate) fn stage(&self) -> Self {
        Self {
            handle: self.handle.clone(),
            owners: Rc::clone(&self.owners),
            counts_owner: false,
        }
    }

    pub(crate) fn commit_stage(mut self) -> Self {
        if !self.counts_owner {
            let owners = self
                .logical_owners()
                .checked_add(1)
                .expect(ERR_OWNER_OVERFLOW);
            self.owners.set(owners);
            self.counts_owner = true;
        }
        self
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

    pub(crate) fn reconstruct_if_shared_in<'rt>(
        &mut self,
        ctx: &mut Ctx<'cx, 'rt>,
        storage_ty: HeapType<'cx, S>,
        reconstruct: impl FnOnce(&S) -> S,
    ) -> Result<(), AccessError>
    where
        S: 'cx,
    {
        if self.is_unique() {
            return Ok(());
        }
        let mut heap = ctx.heap();
        let storage = heap.try_with(self.handle(), reconstruct)?;
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
        if !self.counts_owner {
            return;
        }
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
