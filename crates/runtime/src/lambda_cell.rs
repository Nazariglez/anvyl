use std::{
    cell::{Cell, UnsafeCell},
    marker::PhantomData,
    rc::Rc,
};

use anvyx_heap::{Trace, TraceDriver, Visitor};

use crate::{RuntimeError, SafepointGuardKind, SafepointState};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum CellBorrowState {
    Unborrowed,
    Shared(usize),
    Mutable,
}

pub(crate) struct CellBorrowFlag {
    state: Cell<CellBorrowState>,
}

impl Default for CellBorrowFlag {
    fn default() -> Self {
        Self {
            state: Cell::new(CellBorrowState::Unborrowed),
        }
    }
}

impl CellBorrowFlag {
    pub(crate) fn shared_guard(&self) -> Result<SharedCellGuard<'_>, RuntimeError> {
        match self.state.get() {
            CellBorrowState::Unborrowed => self.state.set(CellBorrowState::Shared(1)),
            CellBorrowState::Shared(count) => {
                self.state
                    .set(CellBorrowState::Shared(count.checked_add(1).ok_or_else(
                        || RuntimeError::new("too many shared mutable cell borrows"),
                    )?));
            }
            CellBorrowState::Mutable => return Err(cell_borrow_error()),
        }
        Ok(SharedCellGuard { flag: self })
    }

    pub(crate) fn mutable_guard(&self) -> Result<MutableCellGuard<'_>, RuntimeError> {
        match self.state.get() {
            CellBorrowState::Unborrowed => self.state.set(CellBorrowState::Mutable),
            CellBorrowState::Shared(_) | CellBorrowState::Mutable => {
                return Err(cell_borrow_error());
            }
        }
        Ok(MutableCellGuard { flag: self })
    }

    fn is_unborrowed(&self) -> bool {
        self.state.get() == CellBorrowState::Unborrowed
    }
}

struct LambdaCellCore<T> {
    value: UnsafeCell<T>,
    borrow: CellBorrowFlag,
    safepoint: SafepointState,
    _not_send_sync: PhantomData<Rc<()>>,
}

impl<T> LambdaCellCore<T> {
    fn new(value: T) -> Self {
        Self::new_with_safepoint(value, SafepointState::default())
    }

    fn new_with_safepoint(value: T, safepoint: SafepointState) -> Self {
        Self {
            value: UnsafeCell::new(value),
            borrow: CellBorrowFlag::default(),
            safepoint,
            _not_send_sync: PhantomData,
        }
    }

    fn access<R>(&self, f: impl FnOnce(&T) -> Result<R, RuntimeError>) -> Result<R, RuntimeError> {
        let _safepoint = self.safepoint.enter(SafepointGuardKind::LambdaCell)?;
        let _guard = self.borrow.shared_guard()?;
        f(unsafe { &*self.value.get() })
    }

    fn mutate<R>(
        &self,
        f: impl FnOnce(&mut T) -> Result<R, RuntimeError>,
    ) -> Result<R, RuntimeError> {
        let _safepoint = self.safepoint.enter(SafepointGuardKind::LambdaCell)?;
        let _guard = self.borrow.mutable_guard()?;
        f(unsafe { &mut *self.value.get() })
    }

    fn replace(&self, value: T) -> Result<T, RuntimeError> {
        self.mutate(|slot| Ok(std::mem::replace(slot, value)))
    }

    fn set(&self, value: T) -> Result<(), RuntimeError> {
        self.mutate(|slot| {
            *slot = value;
            Ok(())
        })
    }

    fn validate_trace(&self) -> Result<(), RuntimeError> {
        if self.borrow.is_unborrowed() {
            Ok(())
        } else {
            Err(RuntimeError::new(
                "cannot collect while lambda cell has an active borrow",
            ))
        }
    }

    fn trace_value<'cx, D: TraceDriver<'cx>>(&self, visitor: &mut Visitor<'cx, '_, D>)
    where
        T: Trace<'cx>,
    {
        self.validate_trace()
            .expect("lambda cell traced outside a safepoint");
        unsafe { &*self.value.get() }.trace(visitor);
    }
}

impl<T: Copy> LambdaCellCore<T> {
    fn get_copy(&self) -> Result<T, RuntimeError> {
        self.access(|value| Ok(*value))
    }
}

pub struct StackLambdaCell<T> {
    core: LambdaCellCore<T>,
}

impl<T> StackLambdaCell<T> {
    pub fn new(value: T) -> Self {
        Self {
            core: LambdaCellCore::new(value),
        }
    }

    pub fn new_with_safepoint(value: T, safepoint: SafepointState) -> Self {
        Self {
            core: LambdaCellCore::new_with_safepoint(value, safepoint),
        }
    }

    pub fn access<R>(
        &self,
        f: impl FnOnce(&T) -> Result<R, RuntimeError>,
    ) -> Result<R, RuntimeError> {
        self.core.access(f)
    }

    pub fn mutate<R>(
        &self,
        f: impl FnOnce(&mut T) -> Result<R, RuntimeError>,
    ) -> Result<R, RuntimeError> {
        self.core.mutate(f)
    }

    pub fn replace(&self, value: T) -> Result<T, RuntimeError> {
        self.core.replace(value)
    }

    pub fn set(&self, value: T) -> Result<(), RuntimeError> {
        self.core.set(value)
    }
}

impl<T: Copy> StackLambdaCell<T> {
    pub fn get_copy(&self) -> Result<T, RuntimeError> {
        self.core.get_copy()
    }
}

pub struct LambdaCell<T> {
    core: LambdaCellCore<T>,
}

impl<T> LambdaCell<T> {
    pub fn new(value: T) -> Self {
        Self {
            core: LambdaCellCore::new(value),
        }
    }

    pub fn new_with_safepoint(value: T, safepoint: SafepointState) -> Self {
        Self {
            core: LambdaCellCore::new_with_safepoint(value, safepoint),
        }
    }

    pub fn access<R>(
        &self,
        f: impl FnOnce(&T) -> Result<R, RuntimeError>,
    ) -> Result<R, RuntimeError> {
        self.core.access(f)
    }

    pub fn mutate<R>(
        &self,
        f: impl FnOnce(&mut T) -> Result<R, RuntimeError>,
    ) -> Result<R, RuntimeError> {
        self.core.mutate(f)
    }

    pub fn replace(&self, value: T) -> Result<T, RuntimeError> {
        self.core.replace(value)
    }

    pub fn set(&self, value: T) -> Result<(), RuntimeError> {
        self.core.set(value)
    }
}

impl<T: Copy> LambdaCell<T> {
    pub fn get_copy(&self) -> Result<T, RuntimeError> {
        self.core.get_copy()
    }
}

// SAFETY: `LambdaCellCore::trace_value` reports the contained payload exactly once without
// cloning, dropping, or mutating it. Heap collection reaches this only at safepoints where no
// cell access guard is active.
unsafe impl<'cx, T: Trace<'cx>> Trace<'cx> for LambdaCell<T> {
    fn trace<D: TraceDriver<'cx>>(&self, visitor: &mut Visitor<'cx, '_, D>) {
        self.core.trace_value(visitor);
    }
}

fn cell_borrow_error() -> RuntimeError {
    RuntimeError::new("conflicting mutable cell access")
}

pub(crate) struct SharedCellGuard<'a> {
    flag: &'a CellBorrowFlag,
}

impl Drop for SharedCellGuard<'_> {
    fn drop(&mut self) {
        match self.flag.state.get() {
            CellBorrowState::Shared(1) => self.flag.state.set(CellBorrowState::Unborrowed),
            CellBorrowState::Shared(count) => {
                self.flag.state.set(CellBorrowState::Shared(count - 1));
            }
            CellBorrowState::Unborrowed | CellBorrowState::Mutable => {
                debug_assert!(false, "invalid shared cell borrow state");
            }
        }
    }
}

pub(crate) struct MutableCellGuard<'a> {
    flag: &'a CellBorrowFlag,
}

impl Drop for MutableCellGuard<'_> {
    fn drop(&mut self) {
        debug_assert_eq!(self.flag.state.get(), CellBorrowState::Mutable);
        self.flag.state.set(CellBorrowState::Unborrowed);
    }
}

#[cfg(test)]
mod tests {
    use std::{cell::Cell, rc::Rc};

    use anvyx_heap::{Trace, TraceDriver, Visitor};

    use super::{LambdaCell, LambdaCellCore, StackLambdaCell};
    use crate::{Ctx, Handle, Heap, RuntimeError, SafepointState};

    struct CountDrop(Rc<Cell<usize>>);

    impl Drop for CountDrop {
        fn drop(&mut self) {
            self.0.set(self.0.get() + 1);
        }
    }

    struct TraceProbe<'cx> {
        stats: Rc<Cell<usize>>,
        edge: Handle<'cx, TraceNode<'cx>>,
    }

    struct TraceNode<'cx> {
        stats: Rc<Cell<usize>>,
        cell: Option<Handle<'cx, LambdaCell<TraceProbe<'cx>>>>,
    }

    struct TraceLambda<'cx> {
        stats: Rc<Cell<usize>>,
        env: Handle<'cx, TraceLambdaEnv<'cx>>,
    }

    struct TraceLambdaEnv<'cx> {
        stats: Rc<Cell<usize>>,
        cell: Handle<'cx, LambdaCell<TraceLambdaPayload<'cx>>>,
    }

    struct TraceLambdaPayload<'cx> {
        stats: Rc<Cell<usize>>,
        lambdas: Vec<Handle<'cx, TraceLambda<'cx>>>,
    }

    // SAFETY: `edge` is the only heap edge; `stats` owns none.
    unsafe impl<'cx> Trace<'cx> for TraceProbe<'cx> {
        fn trace<D: TraceDriver<'cx>>(&self, visitor: &mut Visitor<'cx, '_, D>) {
            self.stats.set(self.stats.get() + 1);
            visitor.edge(&self.edge);
        }
    }

    // SAFETY: `cell` is the only heap edge; `stats` owns none.
    unsafe impl<'cx> Trace<'cx> for TraceNode<'cx> {
        fn trace<D: TraceDriver<'cx>>(&self, visitor: &mut Visitor<'cx, '_, D>) {
            self.stats.set(self.stats.get() + 1);
            visitor.edge_opt(&self.cell);
        }
    }

    // SAFETY: `env` is the only heap edge; `stats` owns none.
    unsafe impl<'cx> Trace<'cx> for TraceLambda<'cx> {
        fn trace<D: TraceDriver<'cx>>(&self, visitor: &mut Visitor<'cx, '_, D>) {
            self.stats.set(self.stats.get() + 1);
            visitor.edge(&self.env);
        }
    }

    // SAFETY: `cell` is the only heap edge; `stats` owns none.
    unsafe impl<'cx> Trace<'cx> for TraceLambdaEnv<'cx> {
        fn trace<D: TraceDriver<'cx>>(&self, visitor: &mut Visitor<'cx, '_, D>) {
            self.stats.set(self.stats.get() + 1);
            visitor.edge(&self.cell);
        }
    }

    // SAFETY: `lambdas` contains every heap edge; `stats` owns none.
    unsafe impl<'cx> Trace<'cx> for TraceLambdaPayload<'cx> {
        fn trace<D: TraceDriver<'cx>>(&self, visitor: &mut Visitor<'cx, '_, D>) {
            self.stats.set(self.stats.get() + 1);
            for lambda in &self.lambdas {
                visitor.edge(lambda);
            }
        }
    }

    #[test]
    fn stack_cell_nested_shared_access_succeeds() {
        let cell = StackLambdaCell::new(1);

        let sum = cell
            .access(|outer| cell.access(|inner| Ok(*outer + *inner)))
            .unwrap();

        assert_eq!(sum, 2);
    }

    #[test]
    fn stack_cell_nested_mutable_access_fails_safely() {
        let cell = StackLambdaCell::new(1);

        let err = cell
            .mutate(|_| {
                cell.mutate(|value| {
                    *value += 1;
                    Ok(())
                })
            })
            .unwrap_err();

        assert_eq!(err.message(), "conflicting mutable cell access");
        cell.set(3).unwrap();
        assert_eq!(cell.get_copy().unwrap(), 3);
    }

    #[test]
    fn stack_cell_replace_drops_old_value_once() {
        let drops = Rc::new(Cell::new(0));
        let cell = StackLambdaCell::new(CountDrop(Rc::clone(&drops)));

        let old = cell.replace(CountDrop(Rc::clone(&drops))).unwrap();
        assert_eq!(drops.get(), 0);

        drop(old);
        assert_eq!(drops.get(), 1);
        drop(cell);
        assert_eq!(drops.get(), 2);
    }

    #[test]
    fn heap_cell_replace_and_set_drop_old_values_once() {
        let drops = Rc::new(Cell::new(0));
        let cell = LambdaCell::new(CountDrop(Rc::clone(&drops)));

        let old = cell.replace(CountDrop(Rc::clone(&drops))).unwrap();
        assert_eq!(drops.get(), 0);
        drop(old);
        assert_eq!(drops.get(), 1);

        cell.set(CountDrop(Rc::clone(&drops))).unwrap();
        assert_eq!(drops.get(), 2);
        drop(cell);
        assert_eq!(drops.get(), 3);
    }

    #[test]
    fn heap_cell_nested_shared_access_succeeds() {
        let cell = LambdaCell::new(1);

        let sum = cell
            .access(|outer| cell.access(|inner| Ok(*outer + *inner)))
            .unwrap();

        assert_eq!(sum, 2);
    }

    #[test]
    fn heap_cell_nested_mutable_access_fails_safely() {
        let cell = LambdaCell::new(1);

        let err = cell
            .mutate(|_| {
                cell.mutate(|value| {
                    *value += 1;
                    Ok(())
                })
            })
            .unwrap_err();

        assert_eq!(err.message(), "conflicting mutable cell access");
        cell.set(3).unwrap();
        assert_eq!(cell.get_copy().unwrap(), 3);
    }

    #[test]
    fn heap_cell_shared_access_during_mutable_access_fails_safely() {
        let cell = LambdaCell::new(1);

        let err = cell
            .mutate(|_| cell.access(|value| Ok(*value)))
            .unwrap_err();

        assert_eq!(err.message(), "conflicting mutable cell access");
        assert_eq!(cell.get_copy().unwrap(), 1);
    }

    #[test]
    fn heap_cell_mutable_access_during_shared_access_fails_safely() {
        let cell = LambdaCell::new(1);

        let err = cell
            .access(|_| {
                cell.mutate(|value| {
                    *value += 1;
                    Ok(())
                })
            })
            .unwrap_err();

        assert_eq!(err.message(), "conflicting mutable cell access");
        assert_eq!(cell.get_copy().unwrap(), 1);
    }

    #[test]
    fn heap_cell_guard_state_restored_after_result_error() {
        let cell = LambdaCell::new(1);
        let err = cell
            .mutate(|_| Err::<(), _>(RuntimeError::new("early")))
            .unwrap_err();

        assert_eq!(err.message(), "early");
        cell.set(2).unwrap();
        assert_eq!(cell.get_copy().unwrap(), 2);
    }

    #[test]
    fn heap_cell_trace_reports_payload_edge_in_cycle() {
        Heap::scope(|heap| {
            let stats = Rc::new(Cell::new(0));
            let node_ty = heap.register_tracked::<TraceNode<'_>>();
            let cell_ty = heap.register_tracked::<LambdaCell<TraceProbe<'_>>>();
            let node = heap.alloc(
                node_ty,
                TraceNode {
                    stats: Rc::clone(&stats),
                    cell: None,
                },
            );
            let cell = heap.alloc(
                cell_ty,
                LambdaCell::new(TraceProbe {
                    stats: Rc::clone(&stats),
                    edge: node.clone(),
                }),
            );
            heap.with_mut(&node, |node| node.cell = Some(cell.clone()));

            drop(node);
            drop(cell);
            heap.reset_stats();
            let outcome = heap.collect_all();

            assert_eq!(outcome.collected, 2);
            assert_eq!(heap.stats().internal_edges, 2);
            assert_eq!(stats.get(), 2);
        });
    }

    #[test]
    fn lambda_env_cell_cycle_with_runtime_cell_is_collectible() {
        Heap::scope(|heap| {
            let lambda_stats = Rc::new(Cell::new(0));
            let env_stats = Rc::new(Cell::new(0));
            let payload_stats = Rc::new(Cell::new(0));
            let lambda_ty = heap.register_tracked::<TraceLambda<'_>>();
            let env_ty = heap.register_tracked::<TraceLambdaEnv<'_>>();
            let cell_ty = heap.register_tracked::<LambdaCell<TraceLambdaPayload<'_>>>();
            let cell = heap.alloc(
                cell_ty,
                LambdaCell::new(TraceLambdaPayload {
                    stats: Rc::clone(&payload_stats),
                    lambdas: vec![],
                }),
            );
            let env = heap.alloc(
                env_ty,
                TraceLambdaEnv {
                    stats: Rc::clone(&env_stats),
                    cell: cell.clone(),
                },
            );
            let lambda = heap.alloc(
                lambda_ty,
                TraceLambda {
                    stats: Rc::clone(&lambda_stats),
                    env: env.clone(),
                },
            );
            heap.with(&cell, |cell| {
                cell.set(TraceLambdaPayload {
                    stats: Rc::clone(&payload_stats),
                    lambdas: vec![lambda.clone()],
                })
                .unwrap();
            });

            drop(lambda);
            drop(env);
            drop(cell);
            heap.reset_stats();
            let outcome = heap.collect_all();

            assert_eq!(outcome.collected, 3);
            assert_eq!(heap.stats().live, 0);
            assert_eq!(heap.stats().internal_edges, 3);
            assert_eq!(lambda_stats.get(), 1);
            assert_eq!(env_stats.get(), 1);
            assert_eq!(payload_stats.get(), 1);
        });
    }

    #[test]
    fn retained_lambda_keeps_runtime_cell_graph_alive_until_dropped() {
        Heap::scope(|heap| {
            let stats = Rc::new(Cell::new(0));
            let lambda_ty = heap.register_tracked::<TraceLambda<'_>>();
            let env_ty = heap.register_tracked::<TraceLambdaEnv<'_>>();
            let cell_ty = heap.register_tracked::<LambdaCell<TraceLambdaPayload<'_>>>();
            let cell = heap.alloc(
                cell_ty,
                LambdaCell::new(TraceLambdaPayload {
                    stats: Rc::clone(&stats),
                    lambdas: vec![],
                }),
            );
            let env = heap.alloc(
                env_ty,
                TraceLambdaEnv {
                    stats: Rc::clone(&stats),
                    cell: cell.clone(),
                },
            );
            let lambda = heap.alloc(
                lambda_ty,
                TraceLambda {
                    stats: Rc::clone(&stats),
                    env: env.clone(),
                },
            );
            heap.with(&cell, |cell| {
                cell.set(TraceLambdaPayload {
                    stats: Rc::clone(&stats),
                    lambdas: vec![lambda.clone()],
                })
                .unwrap();
            });
            let retained = lambda.clone();

            drop(lambda);
            drop(env);
            drop(cell);
            heap.collect_all();
            assert_eq!(heap.stats().live, 3);
            assert!(heap.try_with(&retained, |_| ()).is_ok());

            drop(retained);
            let outcome = heap.collect_all();
            assert_eq!(outcome.collected, 3);
            assert_eq!(heap.stats().live, 0);
        });
    }

    #[test]
    fn collection_rejects_active_cell_borrow() {
        Heap::scope(|heap| {
            let safepoint = SafepointState::default();
            let mut ctx = Ctx::new_with_safepoint(heap, &safepoint);
            let cell = StackLambdaCell::new_with_safepoint(1_i64, safepoint);

            cell.access(|_| {
                assert_eq!(
                    ctx.collect_all().unwrap_err().message(),
                    "cannot collect while lambda cell guard is active"
                );
                Ok(())
            })
            .unwrap();
            assert!(ctx.collect_all().is_ok());
        });
    }

    #[test]
    fn trace_validation_rejects_active_cell_borrow() {
        let cell = LambdaCellCore::new(1_i64);

        cell.access(|_| {
            assert_eq!(
                cell.validate_trace().unwrap_err().message(),
                "cannot collect while lambda cell has an active borrow"
            );
            Ok(())
        })
        .unwrap();
        assert!(cell.validate_trace().is_ok());
    }

    #[test]
    fn multiple_envs_share_one_runtime_cell_payload_trace() {
        Heap::scope(|heap| {
            let lambda_stats = Rc::new(Cell::new(0));
            let env_stats = Rc::new(Cell::new(0));
            let payload_stats = Rc::new(Cell::new(0));
            let lambda_ty = heap.register_tracked::<TraceLambda<'_>>();
            let env_ty = heap.register_tracked::<TraceLambdaEnv<'_>>();
            let cell_ty = heap.register_tracked::<LambdaCell<TraceLambdaPayload<'_>>>();
            let cell = heap.alloc(
                cell_ty,
                LambdaCell::new(TraceLambdaPayload {
                    stats: Rc::clone(&payload_stats),
                    lambdas: vec![],
                }),
            );
            let env0 = heap.alloc(
                env_ty,
                TraceLambdaEnv {
                    stats: Rc::clone(&env_stats),
                    cell: cell.clone(),
                },
            );
            let env1 = heap.alloc(
                env_ty,
                TraceLambdaEnv {
                    stats: Rc::clone(&env_stats),
                    cell: cell.clone(),
                },
            );
            let lambda0 = heap.alloc(
                lambda_ty,
                TraceLambda {
                    stats: Rc::clone(&lambda_stats),
                    env: env0.clone(),
                },
            );
            let lambda1 = heap.alloc(
                lambda_ty,
                TraceLambda {
                    stats: Rc::clone(&lambda_stats),
                    env: env1.clone(),
                },
            );
            heap.with(&cell, |cell| {
                cell.set(TraceLambdaPayload {
                    stats: Rc::clone(&payload_stats),
                    lambdas: vec![lambda0.clone(), lambda1.clone()],
                })
                .unwrap();
            });

            drop(lambda0);
            drop(lambda1);
            drop(env0);
            drop(env1);
            drop(cell);
            heap.reset_stats();
            let outcome = heap.collect_all();

            assert_eq!(outcome.collected, 5);
            assert_eq!(heap.stats().live, 0);
            assert_eq!(heap.stats().internal_edges, 6);
            assert_eq!(lambda_stats.get(), 2);
            assert_eq!(env_stats.get(), 2);
            assert_eq!(payload_stats.get(), 1);
        });
    }

    #[test]
    fn heap_cell_replacement_updates_traced_edge() {
        Heap::scope(|heap| {
            let stats = Rc::new(Cell::new(0));
            let node_ty = heap.register_tracked::<TraceNode<'_>>();
            let cell_ty = heap.register_tracked::<LambdaCell<TraceProbe<'_>>>();
            let old_node = heap.alloc(
                node_ty,
                TraceNode {
                    stats: Rc::clone(&stats),
                    cell: None,
                },
            );
            let new_node = heap.alloc(
                node_ty,
                TraceNode {
                    stats: Rc::clone(&stats),
                    cell: None,
                },
            );
            let cell = heap.alloc(
                cell_ty,
                LambdaCell::new(TraceProbe {
                    stats: Rc::clone(&stats),
                    edge: old_node.clone(),
                }),
            );
            let retained = cell.clone();

            heap.with(&cell, |cell| {
                let replaced = cell
                    .replace(TraceProbe {
                        stats: Rc::clone(&stats),
                        edge: new_node.clone(),
                    })
                    .unwrap();
                drop(replaced);
            });
            heap.with_mut(&new_node, |node| node.cell = Some(cell.clone()));
            drop(old_node);
            drop(new_node);
            heap.reset_stats();

            let outcome = heap.collect_all();
            assert_eq!(outcome.collected, 1);
            assert_eq!(heap.stats().live, 2);
            assert!(stats.get() >= 2);

            drop(retained);
            drop(cell);
            assert_eq!(heap.collect_all().collected, 2);
        });
    }

    #[test]
    fn untracked_scalar_heap_cell_is_allowed() {
        Heap::scope(|heap| {
            let cell_ty = heap.register_untracked::<LambdaCell<i64>>();
            let cell = heap.alloc(cell_ty, LambdaCell::new(7));

            assert_eq!(heap.with(&cell, |cell| cell.get_copy().unwrap()), 7);
            drop(cell);
            assert_eq!(heap.collect_all().collected, 1);
        });
    }
}
