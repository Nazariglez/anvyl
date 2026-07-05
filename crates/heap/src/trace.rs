use std::marker::PhantomData;

use crate::{ErasedHandle, handle::Handle, heap_type::HeapId};

pub trait TraceDriver<'cx>: sealed::Driver<'cx> {}

pub(crate) struct GrayTrace;
pub(crate) struct BlackTrace;

#[doc(hidden)]
pub mod sealed {
    use crate::raw::ObjPtr;

    pub trait Driver<'cx> {
        type State<'a>
        where
            'cx: 'a;

        fn edge(state: &mut Self::State<'_>, ptr: ObjPtr<'cx>);
    }
}

/// # Safety
/// Implementors must report every owned strong heap edge exactly once per owning field, report
/// only same-context live handles, and must not clone/drop handles or mutate heap ownership while
/// tracing. A derive macro is provided to automatically implement this trait.
pub unsafe trait Trace<'cx> {
    fn trace<D: TraceDriver<'cx>>(&self, visitor: &mut Visitor<'cx, '_, D>);
}

pub struct Visitor<'cx, 'a, D: TraceDriver<'cx>>
where
    'cx: 'a,
{
    heap_id: HeapId,
    state: &'a mut <D as sealed::Driver<'cx>>::State<'a>,
}

impl<'cx, 'a, D: TraceDriver<'cx>> Visitor<'cx, 'a, D>
where
    'cx: 'a,
{
    #[inline]
    pub(crate) fn new(
        heap_id: HeapId,
        state: &'a mut <D as sealed::Driver<'cx>>::State<'a>,
    ) -> Self {
        Self { heap_id, state }
    }

    #[inline]
    pub fn edge<T>(&mut self, handle: &Handle<'cx, T>) {
        assert_eq!(
            handle.state().heap_id,
            self.heap_id,
            "trace reported an edge from another heap"
        );
        let header = handle.ptr.header();
        assert!(
            header.is_live() && header.generation.get() == handle.generation,
            "trace reported a dead or stale edge"
        );
        D::edge(self.state, handle.ptr);
    }

    #[inline]
    pub fn edge_erased(&mut self, handle: &ErasedHandle<'cx>) {
        assert_eq!(
            handle.heap_id(),
            self.heap_id,
            "trace reported an edge from another heap"
        );
        let ptr = handle.ptr();
        let header = ptr.header();
        assert!(
            header.is_live() && header.generation.get() == handle.generation(),
            "trace reported a dead or stale edge"
        );
        D::edge(self.state, ptr);
    }

    #[inline]
    pub fn edge_opt<T>(&mut self, handle: &Option<Handle<'cx, T>>) {
        if let Some(handle) = handle {
            self.edge(handle);
        }
    }

    #[inline]
    pub fn edges<T>(&mut self, handles: &[Handle<'cx, T>]) {
        for handle in handles {
            self.edge(handle);
        }
    }
}

macro_rules! trace_no_edge {
    ($($ty:ty),* $(,)?) => {
        $(
            // SAFETY: these types own no heap handles.
            unsafe impl<'cx> Trace<'cx> for $ty {
                #[inline]
                fn trace<D: TraceDriver<'cx>>(&self, _visitor: &mut Visitor<'cx, '_, D>) {}
            }
        )*
    };
}

trace_no_edge!(
    (),
    bool,
    char,
    String,
    u8,
    u16,
    u32,
    u64,
    u128,
    usize,
    i8,
    i16,
    i32,
    i64,
    i128,
    isize,
    f32,
    f64,
);

// SAFETY: `PhantomData` owns no value and no heap edge.
unsafe impl<'cx, T> Trace<'cx> for PhantomData<T> {
    #[inline]
    fn trace<D: TraceDriver<'cx>>(&self, _visitor: &mut Visitor<'cx, '_, D>) {}
}

// SAFETY: traces the contained value once when present.
unsafe impl<'cx, T: Trace<'cx>> Trace<'cx> for Option<T> {
    #[inline]
    fn trace<D: TraceDriver<'cx>>(&self, visitor: &mut Visitor<'cx, '_, D>) {
        if let Some(value) = self {
            value.trace(visitor);
        }
    }
}

// SAFETY: traces each owned element once.
unsafe impl<'cx, T: Trace<'cx>> Trace<'cx> for Vec<T> {
    #[inline]
    fn trace<D: TraceDriver<'cx>>(&self, visitor: &mut Visitor<'cx, '_, D>) {
        for value in self {
            value.trace(visitor);
        }
    }
}

// SAFETY: traces each array element once.
unsafe impl<'cx, T: Trace<'cx>, const N: usize> Trace<'cx> for [T; N] {
    #[inline]
    fn trace<D: TraceDriver<'cx>>(&self, visitor: &mut Visitor<'cx, '_, D>) {
        for value in self {
            value.trace(visitor);
        }
    }
}

// SAFETY: traces the owned boxed value once.
unsafe impl<'cx, T: Trace<'cx>> Trace<'cx> for Box<T> {
    #[inline]
    fn trace<D: TraceDriver<'cx>>(&self, visitor: &mut Visitor<'cx, '_, D>) {
        self.as_ref().trace(visitor);
    }
}

// SAFETY: traces exactly the active variant payload.
unsafe impl<'cx, T: Trace<'cx>, E: Trace<'cx>> Trace<'cx> for Result<T, E> {
    #[inline]
    fn trace<D: TraceDriver<'cx>>(&self, visitor: &mut Visitor<'cx, '_, D>) {
        match self {
            Ok(value) => value.trace(visitor),
            Err(error) => error.trace(visitor),
        }
    }
}

macro_rules! trace_tuple {
    ($($ty:ident $field:tt),+ $(,)?) => {
        // SAFETY: traces each tuple field once.
        unsafe impl<'cx, $($ty: Trace<'cx>),+> Trace<'cx> for ($($ty,)+) {
            #[inline]
            fn trace<Driver: TraceDriver<'cx>>(&self, visitor: &mut Visitor<'cx, '_, Driver>) {
                $(self.$field.trace(visitor);)+
            }
        }
    };
}

trace_tuple!(A 0);
trace_tuple!(A 0, B 1);
trace_tuple!(A 0, B 1, C 2);
trace_tuple!(A 0, B 1, C 2, D 3);
trace_tuple!(A 0, B 1, C 2, D 3, E 4);
trace_tuple!(A 0, B 1, C 2, D 3, E 4, F 5);
trace_tuple!(A 0, B 1, C 2, D 3, E 4, F 5, G 6);
trace_tuple!(A 0, B 1, C 2, D 3, E 4, F 5, G 6, H 7);
trace_tuple!(A 0, B 1, C 2, D 3, E 4, F 5, G 6, H 7, I 8);
trace_tuple!(A 0, B 1, C 2, D 3, E 4, F 5, G 6, H 7, I 8, J 9);
trace_tuple!(A 0, B 1, C 2, D 3, E 4, F 5, G 6, H 7, I 8, J 9, K 10);
trace_tuple!(A 0, B 1, C 2, D 3, E 4, F 5, G 6, H 7, I 8, J 9, K 10, L 11);

// SAFETY: one erased handle is one strong heap edge.
unsafe impl<'cx> Trace<'cx> for ErasedHandle<'cx> {
    #[inline]
    fn trace<D: TraceDriver<'cx>>(&self, visitor: &mut Visitor<'cx, '_, D>) {
        visitor.edge_erased(self);
    }
}

// SAFETY: one `Handle` is one strong heap edge.
unsafe impl<'cx, T> Trace<'cx> for Handle<'cx, T> {
    #[inline]
    fn trace<D: TraceDriver<'cx>>(&self, visitor: &mut Visitor<'cx, '_, D>) {
        visitor.edge(self);
    }
}
