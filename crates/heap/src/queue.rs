use std::marker::PhantomData;

use crate::raw::ObjPtr;

#[derive(Debug, Copy, Clone)]
pub(crate) struct WorkItem<'cx> {
    ptr: ObjPtr<'cx>,
    generation: u64,
}

impl<'cx> WorkItem<'cx> {
    #[inline]
    pub(crate) fn new(ptr: ObjPtr<'cx>) -> Self {
        Self {
            ptr,
            generation: ptr.header().generation.get(),
        }
    }

    #[inline]
    pub(crate) fn live_ptr(self) -> Option<ObjPtr<'cx>> {
        let header = self.ptr.header();
        (header.generation.get() == self.generation && header.is_live()).then_some(self.ptr)
    }
}

#[derive(Debug, Default)]
pub(crate) struct WorkQueue<'cx> {
    items: Vec<WorkItem<'cx>>,
    head: usize,
    marker: PhantomData<&'cx ()>,
}

impl<'cx> WorkQueue<'cx> {
    #[inline]
    pub(crate) fn push(&mut self, ptr: ObjPtr<'cx>) {
        self.items.push(WorkItem::new(ptr));
    }

    #[inline]
    pub(crate) fn pending_len(&self) -> usize {
        self.items.len().saturating_sub(self.head)
    }

    #[inline]
    pub(crate) fn take_all_into(&mut self, out: &mut Vec<WorkItem<'cx>>) {
        out.clear();
        if self.head == 0 {
            std::mem::swap(out, &mut self.items);
        } else {
            out.extend(self.items.drain(self.head..));
            self.items.clear();
        }
        self.head = 0;
    }

    #[inline]
    pub(crate) fn take_up_to_into(&mut self, out: &mut Vec<WorkItem<'cx>>, limit: usize) {
        if limit >= self.pending_len() {
            self.take_all_into(out);
            return;
        }
        out.clear();
        let end = self.items.len().min(self.head.saturating_add(limit));
        out.extend_from_slice(&self.items[self.head..end]);
        self.head = end;
        self.compact_if_needed();
    }

    #[inline]
    pub(crate) fn live_items(&self) -> impl Iterator<Item = WorkItem<'cx>> + '_ {
        self.items[self.head..].iter().copied()
    }

    #[inline]
    fn compact_if_needed(&mut self) {
        if self.head > 0 && self.head >= self.items.len().div_ceil(2) {
            self.items.drain(..self.head);
            self.head = 0;
        }
    }
}
