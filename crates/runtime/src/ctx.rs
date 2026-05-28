use crate::Heap;

pub struct Ctx<'cx, 'rt> {
    heap: &'rt mut Heap<'cx>,
}

impl<'cx, 'rt> Ctx<'cx, 'rt> {
    pub fn new(heap: &'rt mut Heap<'cx>) -> Self {
        Self { heap }
    }

    pub fn heap(&mut self) -> &mut Heap<'cx> {
        self.heap
    }
}
