use std::{cell::UnsafeCell, marker::PhantomData, ptr, rc::Rc};

pub struct StackLambdaCell<T> {
    value: UnsafeCell<T>,
    _not_send_sync: PhantomData<Rc<()>>,
}

impl<T> StackLambdaCell<T> {
    pub fn new(value: T) -> Self {
        Self {
            value: UnsafeCell::new(value),
            _not_send_sync: PhantomData,
        }
    }

    pub fn replace(&self, value: T) -> T {
        unsafe { ptr::replace(self.value.get(), value) }
    }

    pub fn set(&self, value: T) {
        self.replace(value);
    }
}

impl<T: Copy> StackLambdaCell<T> {
    pub fn get_copy(&self) -> T {
        unsafe { *self.value.get() }
    }
}

#[cfg(test)]
mod tests {
    use std::{cell::Cell, rc::Rc};

    use super::StackLambdaCell;

    struct CountDrop(Rc<Cell<usize>>);

    impl Drop for CountDrop {
        fn drop(&mut self) {
            self.0.set(self.0.get() + 1);
        }
    }

    #[test]
    fn scalar_get_set() {
        let cell = StackLambdaCell::new(1);
        assert_eq!(cell.get_copy(), 1);

        cell.set(2);
        assert_eq!(cell.get_copy(), 2);
    }

    #[test]
    fn replace_returns_old_value() {
        let cell = StackLambdaCell::new(1);

        assert_eq!(cell.replace(2), 1);
        assert_eq!(cell.get_copy(), 2);
    }

    #[test]
    fn set_drops_old_value_once() {
        let drops = Rc::new(Cell::new(0));
        let cell = StackLambdaCell::new(CountDrop(Rc::clone(&drops)));

        cell.set(CountDrop(Rc::clone(&drops)));
        assert_eq!(drops.get(), 1);

        drop(cell);
        assert_eq!(drops.get(), 2);
    }

    #[test]
    fn replace_preserves_old_non_copy_value() {
        let drops = Rc::new(Cell::new(0));
        let cell = StackLambdaCell::new(CountDrop(Rc::clone(&drops)));

        let old = cell.replace(CountDrop(Rc::clone(&drops)));
        assert_eq!(drops.get(), 0);

        drop(old);
        assert_eq!(drops.get(), 1);
        drop(cell);
        assert_eq!(drops.get(), 2);
    }
}
