use std::marker::PhantomData;

use crate::{Ctx, RuntimeError, StackLambdaCell};

pub enum MutPlace<'place, 'cx, T> {
    Local(&'place mut T, PhantomData<&'cx ()>),
    StackCell(&'place StackLambdaCell<T>, PhantomData<&'cx ()>),
}

impl<'place, 'cx, T> MutPlace<'place, 'cx, T> {
    pub fn local(value: &'place mut T) -> Self {
        Self::Local(value, PhantomData)
    }

    pub fn stack_cell(cell: &'place StackLambdaCell<T>) -> Self {
        Self::StackCell(cell, PhantomData)
    }

    pub fn reborrow(&mut self) -> MutPlace<'_, 'cx, T> {
        match self {
            Self::Local(value, _) => MutPlace::local(&mut **value),
            Self::StackCell(cell, _) => MutPlace::stack_cell(cell),
        }
    }

    pub fn access<'rt, R>(
        &self,
        _ctx: &mut Ctx<'cx, 'rt>,
        f: impl FnOnce(&T) -> Result<R, RuntimeError>,
    ) -> Result<R, RuntimeError> {
        match self {
            Self::Local(value, _) => f(value),
            Self::StackCell(cell, _) => cell.access(f),
        }
    }

    pub fn mutate<'rt, R>(
        &mut self,
        _ctx: &mut Ctx<'cx, 'rt>,
        f: impl FnOnce(&mut T) -> Result<R, RuntimeError>,
    ) -> Result<R, RuntimeError> {
        match self {
            Self::Local(value, _) => f(*value),
            Self::StackCell(cell, _) => cell.mutate(f),
        }
    }

    pub fn set<'rt>(&mut self, ctx: &mut Ctx<'cx, 'rt>, value: T) -> Result<(), RuntimeError> {
        self.mutate(ctx, |slot| {
            *slot = value;
            Ok(())
        })
    }

    pub fn replace<'rt>(&mut self, ctx: &mut Ctx<'cx, 'rt>, value: T) -> Result<T, RuntimeError> {
        self.mutate(ctx, |slot| Ok(std::mem::replace(slot, value)))
    }
}

impl<'cx, T: Copy> MutPlace<'_, 'cx, T> {
    pub fn get_copy<'rt>(&self, ctx: &mut Ctx<'cx, 'rt>) -> Result<T, RuntimeError> {
        self.access(ctx, |value| Ok(*value))
    }

    pub fn update_copy<'rt>(
        &mut self,
        ctx: &mut Ctx<'cx, 'rt>,
        f: impl FnOnce(T) -> T,
    ) -> Result<(), RuntimeError> {
        self.mutate(ctx, |value| {
            *value = f(*value);
            Ok(())
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::{AnvList, Ctx, Heap, MutPlace, RuntimeError, StackLambdaCell};

    fn with_ctx<R>(f: impl for<'cx, 'rt> FnOnce(&mut Ctx<'cx, 'rt>) -> R) -> R {
        Heap::scope(|heap| f(&mut Ctx::new(heap)))
    }

    #[test]
    fn local_access_mutate_and_copy_update() {
        with_ctx(|ctx| {
            let mut value = 1;
            let mut place = MutPlace::local(&mut value);

            assert_eq!(place.get_copy(ctx).unwrap(), 1);
            place.update_copy(ctx, |value| value + 1).unwrap();
            assert_eq!(place.get_copy(ctx).unwrap(), 2);
            place.set(ctx, 5).unwrap();
            assert_eq!(place.replace(ctx, 8).unwrap(), 5);
            assert_eq!(place.get_copy(ctx).unwrap(), 8);
        });
    }

    #[test]
    fn stack_cell_routes_through_guarded_cell() {
        with_ctx(|ctx| {
            let cell = StackLambdaCell::new(1);
            let mut place = MutPlace::stack_cell(&cell);

            place.update_copy(ctx, |value| value + 1).unwrap();
            assert_eq!(place.get_copy(ctx).unwrap(), 2);
            place.set(ctx, 4).unwrap();
            assert_eq!(cell.get_copy().unwrap(), 4);
        });
    }

    #[test]
    fn reborrow_preserves_local_identity() {
        with_ctx(|ctx| {
            let mut value = 1;
            let mut place = MutPlace::local(&mut value);
            {
                let mut forwarded = place.reborrow();
                forwarded.update_copy(ctx, |value| value + 1).unwrap();
            }
            place.update_copy(ctx, |value| value + 1).unwrap();

            assert_eq!(value, 3);
        });
    }

    #[test]
    fn reborrow_preserves_stack_cell_identity() {
        with_ctx(|ctx| {
            let cell = StackLambdaCell::new(1);
            let mut place = MutPlace::stack_cell(&cell);
            {
                let mut forwarded = place.reborrow();
                forwarded.update_copy(ctx, |value| value + 1).unwrap();
            }
            place.update_copy(ctx, |value| value + 1).unwrap();

            assert_eq!(cell.get_copy().unwrap(), 3);
        });
    }

    #[test]
    fn set_and_replace_do_not_require_clone() {
        struct NonClone(i64);

        with_ctx(|ctx| {
            let mut value = NonClone(1);
            let mut place = MutPlace::local(&mut value);

            let old = place.replace(ctx, NonClone(2)).unwrap();
            assert_eq!(old.0, 1);
            place.set(ctx, NonClone(3)).unwrap();
            assert_eq!(value.0, 3);
        });
    }

    #[test]
    fn local_list_mutation_uses_short_region() {
        with_ctx(|ctx| {
            let mut list = AnvList::from_elems([1_i64]);
            let mut place = MutPlace::local(&mut list);

            place
                .mutate(ctx, |list| {
                    list.push(2);
                    Ok(())
                })
                .unwrap();

            assert_eq!(place.access(ctx, |list| Ok(list.len())).unwrap(), 2);
        });
    }

    #[test]
    fn stack_cell_conflict_is_returned_not_panicked() {
        with_ctx(|ctx| {
            let cell = StackLambdaCell::new(1);
            let mut place = MutPlace::stack_cell(&cell);
            let err = cell
                .access(|_| place.update_copy(ctx, |value| value + 1))
                .unwrap_err();

            assert_eq!(err.message(), "conflicting mutable cell access");
        });
    }

    #[test]
    fn local_region_restores_after_error() {
        with_ctx(|ctx| {
            let mut value = 1;
            let mut place = MutPlace::local(&mut value);
            let err = place
                .mutate(ctx, |_| Err::<(), _>(RuntimeError::new("early")))
                .unwrap_err();

            assert_eq!(err.message(), "early");
            place.set(ctx, 2).unwrap();
            assert_eq!(value, 2);
        });
    }
}
