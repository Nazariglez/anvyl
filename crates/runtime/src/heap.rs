use std::{marker::PhantomData, rc::Rc};

type HeapBrand<'cx> = (fn(&'cx ()) -> &'cx (), Rc<()>);

pub struct Heap<'cx> {
    _brand: PhantomData<HeapBrand<'cx>>,
}

impl Heap<'_> {
    pub fn scope<R>(f: impl for<'cx> FnOnce(&mut Heap<'cx>) -> R) -> R {
        let mut heap = Heap {
            _brand: PhantomData,
        };
        f(&mut heap)
    }
}
