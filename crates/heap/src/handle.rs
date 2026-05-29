use std::{cell::Cell, marker::PhantomData, rc::Rc};

use crate::{
    heap::{ReleaseOwner, SharedState},
    raw::{Header, ObjPtr, StatePtr},
};

type Invariant<'cx> = PhantomData<Cell<&'cx ()>>;

pub struct Handle<'cx, T> {
    pub(crate) ptr: ObjPtr<'cx>,
    pub(crate) state: StatePtr<'cx, SharedState<'cx>>,
    pub(crate) generation: u64,
    marker: PhantomData<fn(T) -> T>,
    brand: Invariant<'cx>,
    not_send_sync: PhantomData<Rc<()>>,
}

impl<'cx, T> Handle<'cx, T> {
    #[inline]
    pub(crate) fn new(ptr: ObjPtr<'cx>, state: &SharedState<'cx>) -> Self {
        Self::from_raw(ptr, state, ptr.header().generation.get())
    }

    #[inline]
    pub(crate) fn from_raw(ptr: ObjPtr<'cx>, state: &SharedState<'cx>, generation: u64) -> Self {
        Self {
            ptr,
            state: StatePtr::new(state),
            generation,
            marker: PhantomData,
            brand: PhantomData,
            not_send_sync: PhantomData,
        }
    }

    #[inline]
    pub(crate) fn state(&self) -> &SharedState<'cx> {
        self.state.get()
    }

    #[inline]
    pub(crate) fn header(&self) -> &Header<'cx> {
        let header = self.ptr.header();
        assert_eq!(
            header.generation.get(),
            self.generation,
            "stale heap handle"
        );
        header
    }

    #[inline]
    pub fn ptr_eq(&self, other: &Self) -> bool {
        self.ptr == other.ptr && self.generation == other.generation
    }

    #[inline]
    pub fn strong_count(&self) -> usize {
        usize::try_from(self.header().strong.get()).unwrap()
    }

    #[inline]
    pub fn is_alive(&self) -> bool {
        let header = self.ptr.header();
        header.generation.get() == self.generation && header.is_live()
    }
}

impl<T> Clone for Handle<'_, T> {
    fn clone(&self) -> Self {
        let state = self.state();
        state
            .retain_strong(self.ptr, self.generation)
            .expect("cloned dead or stale heap handle");
        state.metrics.clones.set(state.metrics.clones.get() + 1);
        Self {
            ptr: self.ptr,
            state: self.state,
            generation: self.generation,
            marker: PhantomData,
            brand: PhantomData,
            not_send_sync: PhantomData,
        }
    }
}

impl<T> Drop for Handle<'_, T> {
    fn drop(&mut self) {
        self.state()
            .release_strong(self.ptr, self.generation, ReleaseOwner::Handle);
    }
}
