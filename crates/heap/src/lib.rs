mod handle;
mod heap;
mod heap_type;
mod metrics;
mod queue;
mod raw;
mod trace;

pub use anvyx_heap_derive::Trace;
pub use handle::Handle;
pub use heap::{
    AccessError, CollectOutcome, CycleStatus, ErasedHandle, Heap, HeapConfig, LeakReport,
    LeakTypeReport,
};
pub use heap_type::{HeapType, HeapTypeId, TraceMode};
pub use metrics::HeapStats;
pub use trace::{Trace, TraceDriver, Visitor};

#[cfg(test)]
mod tests {
    use std::{
        cell::{Cell, RefCell},
        mem::ManuallyDrop,
        rc::Rc,
    };

    use super::*;

    fn pending_cycles(heap: &Heap<'_>) -> usize {
        heap.state.suspects.borrow().pending_len()
    }

    fn manual_copy<T>(value: &T) -> ManuallyDrop<T> {
        // SAFETY: these tests need a bitwise duplicate whose destructor is manually controlled.
        ManuallyDrop::new(unsafe { std::ptr::read(value) })
    }

    #[derive(Trace)]
    struct Node<'cx> {
        next: Option<Handle<'cx, Node<'cx>>>,
        value: i32,
    }

    struct ExternalRoot<'cx> {
        visits: Rc<Cell<usize>>,
        handle: Handle<'cx, Node<'cx>>,
    }

    // SAFETY: `handle` is the only heap edge; `visits` owns none.
    unsafe impl<'cx> Trace<'cx> for ExternalRoot<'cx> {
        fn trace<D: TraceDriver<'cx>>(&self, visitor: &mut Visitor<'cx, '_, D>) {
            self.visits.set(self.visits.get() + 1);
            visitor.edge(&self.handle);
        }
    }

    struct CountedNode<'cx> {
        stats: Rc<Cell<usize>>,
        callback: Option<Handle<'cx, CountedCallback<'cx>>>,
    }

    struct CountedCallback<'cx> {
        stats: Rc<Cell<usize>>,
        env: Handle<'cx, CountedNode<'cx>>,
    }

    struct TestLambda<'cx> {
        stats: Rc<Cell<usize>>,
        env: Handle<'cx, TestLambdaEnv<'cx>>,
    }

    struct TestLambdaEnv<'cx> {
        stats: Rc<Cell<usize>>,
        cell: Handle<'cx, TestLambdaCell<'cx>>,
    }

    struct TestLambdaCell<'cx> {
        stats: Rc<Cell<usize>>,
        payload: Option<Handle<'cx, TestLambda<'cx>>>,
    }

    // SAFETY: `callback` is the only heap edge; `stats` owns none.
    unsafe impl<'cx> Trace<'cx> for CountedNode<'cx> {
        fn trace<D: TraceDriver<'cx>>(&self, visitor: &mut Visitor<'cx, '_, D>) {
            self.stats.set(self.stats.get() + 1);
            visitor.edge_opt(&self.callback);
        }
    }

    // SAFETY: `env` is the only heap edge; `stats` owns none.
    unsafe impl<'cx> Trace<'cx> for CountedCallback<'cx> {
        fn trace<D: TraceDriver<'cx>>(&self, visitor: &mut Visitor<'cx, '_, D>) {
            self.stats.set(self.stats.get() + 1);
            visitor.edge(&self.env);
        }
    }

    // SAFETY: `env` is the only heap edge; `stats` owns none.
    unsafe impl<'cx> Trace<'cx> for TestLambda<'cx> {
        fn trace<D: TraceDriver<'cx>>(&self, visitor: &mut Visitor<'cx, '_, D>) {
            self.stats.set(self.stats.get() + 1);
            visitor.edge(&self.env);
        }
    }

    // SAFETY: `cell` is the only heap edge; `stats` owns none.
    unsafe impl<'cx> Trace<'cx> for TestLambdaEnv<'cx> {
        fn trace<D: TraceDriver<'cx>>(&self, visitor: &mut Visitor<'cx, '_, D>) {
            self.stats.set(self.stats.get() + 1);
            visitor.edge(&self.cell);
        }
    }

    // SAFETY: `payload` is the only heap edge; `stats` owns none.
    unsafe impl<'cx> Trace<'cx> for TestLambdaCell<'cx> {
        fn trace<D: TraceDriver<'cx>>(&self, visitor: &mut Visitor<'cx, '_, D>) {
            self.stats.set(self.stats.get() + 1);
            visitor.edge_opt(&self.payload);
        }
    }

    #[derive(Trace)]
    struct ModelNode<'cx> {
        id: usize,
        edges: Vec<Handle<'cx, ModelNode<'cx>>>,
    }

    struct ModelGraph<'cx> {
        ty: HeapType<'cx, ModelNode<'cx>>,
        handles: Vec<Option<Handle<'cx, ModelNode<'cx>>>>,
        retained: Vec<Option<Handle<'cx, ModelNode<'cx>>>>,
        edges: Vec<Vec<usize>>,
        external: Vec<bool>,
        retained_live: Vec<bool>,
    }

    impl<'cx> ModelGraph<'cx> {
        fn new(heap: &mut Heap<'cx>) -> Self {
            Self {
                ty: heap.register_tracked::<ModelNode<'_>>(),
                handles: vec![],
                retained: vec![],
                edges: vec![],
                external: vec![],
                retained_live: vec![],
            }
        }

        fn alloc(&mut self, heap: &mut Heap<'cx>) -> usize {
            let id = self.handles.len();
            let handle = heap.alloc(self.ty, ModelNode { id, edges: vec![] });
            self.handles.push(Some(handle));
            self.retained.push(None);
            self.edges.push(vec![]);
            self.external.push(true);
            self.retained_live.push(false);
            id
        }

        fn add_edge(&mut self, heap: &mut Heap<'cx>, from: usize, to: usize) {
            let target = self.handle(to);
            heap.with_mut(&self.handle(from), |node| node.edges.push(target));
            self.edges[from].push(to);
        }

        fn drop_external(&mut self, id: usize) {
            self.handles[id] = None;
            self.external[id] = false;
        }

        fn retain(&mut self, id: usize) {
            if self.retained[id].is_none() {
                self.retained[id] = Some(self.handle(id));
                self.retained_live[id] = true;
            }
        }

        fn release(&mut self, id: usize) {
            if self.retained[id].take().is_some() {
                self.retained_live[id] = false;
            }
        }

        fn handle(&self, id: usize) -> Handle<'cx, ModelNode<'cx>> {
            self.handles[id]
                .as_ref()
                .or(self.retained[id].as_ref())
                .expect("node handle")
                .clone()
        }

        fn expected_live(&self) -> Vec<bool> {
            let mut live = vec![false; self.handles.len()];
            let mut stack = vec![];
            for id in 0..self.handles.len() {
                if self.external[id] || self.retained_live[id] {
                    stack.push(id);
                }
            }
            while let Some(id) = stack.pop() {
                if live[id] {
                    continue;
                }
                live[id] = true;
                stack.extend(self.edges[id].iter().copied());
            }
            live
        }

        fn assert_matches_model(&self, heap: &mut Heap<'cx>) {
            let expected = self.expected_live();
            assert_eq!(
                heap.stats().live,
                expected.iter().filter(|live| **live).count()
            );
            for (id, handle) in self.handles.iter().enumerate() {
                if let Some(handle) = handle {
                    assert!(expected[id]);
                    assert_eq!(heap.with(handle, |node| node.id), id);
                }
            }
            for (id, handle) in self.retained.iter().enumerate() {
                if let Some(handle) = handle {
                    assert!(expected[id]);
                    assert_eq!(heap.try_with(handle, |node| node.id), Ok(id));
                }
            }
        }
    }

    #[derive(Clone)]
    struct TinyRng(u64);

    impl TinyRng {
        fn new(seed: u64) -> Self {
            Self(seed)
        }

        fn next(&mut self, limit: usize) -> usize {
            self.0 = self
                .0
                .wrapping_mul(6_364_136_223_846_793_005)
                .wrapping_add(1);
            ((self.0 >> 32) as usize) % limit
        }
    }

    fn run_model_random(heap: &mut Heap<'_>, seed: u64, ops: usize) {
        let mut graph = ModelGraph::new(heap);
        let mut rng = TinyRng::new(seed);
        for _ in 0..4 {
            graph.alloc(heap);
        }
        for _ in 0..ops {
            match rng.next(6) {
                0 => {
                    graph.alloc(heap);
                }
                1 => {
                    let from = rng.next(graph.handles.len());
                    let to = rng.next(graph.handles.len());
                    if (graph.external[from] || graph.retained_live[from])
                        && (graph.external[to] || graph.retained_live[to])
                    {
                        graph.add_edge(heap, from, to);
                    }
                }
                2 => {
                    let id = rng.next(graph.handles.len());
                    if graph.external[id] && !graph.retained_live[id] {
                        graph.drop_external(id);
                    }
                }
                3 => {
                    let id = rng.next(graph.handles.len());
                    if graph.external[id] || graph.retained_live[id] {
                        graph.retain(id);
                    }
                }
                4 => {
                    let id = rng.next(graph.handles.len());
                    graph.release(id);
                }
                _ => {
                    heap.collect_all();
                    graph.assert_matches_model(heap);
                }
            }
        }
        heap.collect_all();
        graph.assert_matches_model(heap);
    }

    #[test]
    fn external_trace_root_preserves_cycle_until_dropped() {
        Heap::scope(|heap| {
            let ty = heap.register_tracked::<Node<'_>>();
            let node = heap.alloc(
                ty,
                Node {
                    next: None,
                    value: 1,
                },
            );
            let next = node.clone();
            heap.with_mut(&node, |node| node.next = Some(next));
            let visits = Rc::new(Cell::new(0));
            let root = ExternalRoot {
                visits: visits.clone(),
                handle: node.clone(),
            };
            drop(node);

            heap.collect_all_with_external_roots(&root);

            assert!(visits.get() > 0);
            assert_eq!(heap.stats().live, 1);
            drop(root);
            heap.collect_all();
            assert_eq!(heap.stats().live, 0);
        });
    }

    #[test]
    fn model_graph_rooted_cycle_survives() {
        fn run(heap: &mut Heap<'_>) {
            let mut graph = ModelGraph::new(heap);
            let a = graph.alloc(heap);
            let b = graph.alloc(heap);
            graph.add_edge(heap, a, b);
            graph.add_edge(heap, b, a);
            graph.retain(a);
            graph.drop_external(a);
            graph.drop_external(b);
            heap.collect_all();
            graph.assert_matches_model(heap);
            graph.release(a);
            heap.collect_all();
            graph.assert_matches_model(heap);
        }

        Heap::scope(run);
    }

    #[test]
    fn model_graph_random_1000_ops_seed_2() {
        Heap::scope(|heap| run_model_random(heap, 2, 1000));
    }

    #[test]
    fn model_graph_budgeted_then_full_matches_full_collect() {
        fn run_scenario(budgeted_first: bool) -> (usize, usize, bool, Option<usize>) {
            Heap::scope(|heap| {
                let mut graph = ModelGraph::new(heap);
                let a = graph.alloc(heap);
                let b = graph.alloc(heap);
                let c = graph.alloc(heap);
                graph.add_edge(heap, a, b);
                graph.add_edge(heap, b, a);
                graph.add_edge(heap, c, a);
                graph.retain(c);
                graph.drop_external(a);
                graph.drop_external(b);
                graph.drop_external(c);
                if budgeted_first {
                    heap.collect(1);
                }
                heap.collect_all();
                graph.assert_matches_model(heap);
                let resolved = graph.retained[c].as_ref();
                let resolved_id = resolved.map(|handle| heap.with(handle, |node| node.id));
                (
                    heap.stats().live,
                    pending_cycles(heap),
                    resolved.is_some(),
                    resolved_id,
                )
            })
        }

        assert_eq!(run_scenario(true), run_scenario(false));
    }

    #[derive(Trace)]
    struct GeneratedNode;

    #[derive(Trace)]
    struct GeneratedCallback<'cx> {
        env: Handle<'cx, GeneratedNode>,
        add: i32,
    }

    struct GeneratedContext<'cx, 'heap> {
        heap: &'heap mut Heap<'cx>,
        node: HeapType<'cx, GeneratedNode>,
        callback: HeapType<'cx, GeneratedCallback<'cx>>,
    }

    impl<'cx, 'heap> GeneratedContext<'cx, 'heap> {
        fn new(heap: &'heap mut Heap<'cx>) -> Self {
            Self {
                node: heap.register_tracked::<GeneratedNode>(),
                callback: heap.register_tracked::<GeneratedCallback<'_>>(),
                heap,
            }
        }

        fn alloc_node(&mut self) -> Handle<'cx, GeneratedNode> {
            self.heap.alloc(self.node, GeneratedNode)
        }

        fn alloc_callback(
            &mut self,
            env: &Handle<'cx, GeneratedNode>,
            add: i32,
        ) -> Handle<'cx, GeneratedCallback<'cx>> {
            self.heap.alloc(
                self.callback,
                GeneratedCallback {
                    env: env.clone(),
                    add,
                },
            )
        }
    }
    #[test]
    fn retained_handle_lifecycle_counts_are_balanced() {
        fn run(heap: &mut Heap<'_>) {
            let mut ctx = GeneratedContext::new(heap);
            let node = ctx.alloc_node();
            let callback = ctx.alloc_callback(&node, 2);
            ctx.heap.reset_stats();
            let retained = callback.clone();
            drop(retained);
            let stats = ctx.heap.stats();
            assert_eq!(stats.clones, 1);
            assert_eq!(stats.drops, 1);
            assert_eq!(pending_cycles(ctx.heap), 1);
            assert_eq!(ctx.heap.with(&callback, |callback| callback.add), 2);
        }

        Heap::scope(run);
    }

    #[test]
    fn untracked_zero_cleanup_reuses_slot() {
        fn run(heap: &mut Heap<'_>) {
            let node_type = heap.register_untracked::<i32>();
            let first = heap.alloc(node_type, 1);
            drop(first);
            let outcome = heap.collect(0);
            assert_eq!(outcome.collected, 1);
            assert_eq!(heap.stats().live, 0);
            let second = heap.alloc(node_type, 2);
            assert_eq!(heap.with(&second, |value| *value), 2);
        }

        Heap::scope(run);
    }

    #[test]
    fn tracked_nonzero_drop_buffers_suspect_once() {
        fn run(heap: &mut Heap<'_>) {
            let node_type = heap.register_tracked::<Node<'_>>();
            let node = heap.alloc(
                node_type,
                Node {
                    next: None,
                    value: 1,
                },
            );
            let alias = node.clone();

            assert_eq!(pending_cycles(heap), 0);
            drop(alias);
            assert_eq!(pending_cycles(heap), 1);

            let alias = node.clone();
            drop(alias);
            assert_eq!(pending_cycles(heap), 1);
        }

        Heap::scope(run);
    }

    #[test]
    fn value_only_mutation_does_not_buffer_suspects() {
        fn run(heap: &mut Heap<'_>) {
            let node_type = heap.register_tracked::<Node<'_>>();
            let node = heap.alloc(
                node_type,
                Node {
                    next: None,
                    value: 0,
                },
            );
            heap.reset_stats();

            for _ in 0..100 {
                heap.with_mut(&node, |node| node.value += 1);
            }

            assert_eq!(heap.stats().suspects, 0);
            assert_eq!(pending_cycles(heap), 0);
        }

        Heap::scope(run);
    }

    #[test]
    fn dead_cycle_traces_each_candidate_once() {
        fn run(heap: &mut Heap<'_>) {
            let stats = Rc::new(Cell::new(0));
            let node_type = heap.register_tracked::<CountedNode<'_>>();
            let callback_type = heap.register_tracked::<CountedCallback<'_>>();
            let node = heap.alloc(
                node_type,
                CountedNode {
                    stats: Rc::clone(&stats),
                    callback: None,
                },
            );
            let callback = heap.alloc(
                callback_type,
                CountedCallback {
                    stats: Rc::clone(&stats),
                    env: node.clone(),
                },
            );
            heap.with_mut(&node, |node| node.callback = Some(callback.clone()));
            drop(node);
            drop(callback);
            heap.reset_stats();
            let outcome = heap.collect_all();
            let heap_stats = heap.stats();
            assert_eq!(heap_stats.live, 0);
            assert_eq!(outcome.collected, 2);
            assert_eq!(heap_stats.candidate_nodes, 2);
            assert_eq!(heap_stats.internal_edges, 2);
            assert_eq!(heap_stats.edge_visits, 2);
            assert_eq!(heap_stats.blackened, 0);
            assert_eq!(stats.get(), 2);
        }

        Heap::scope(run);
    }

    #[test]
    fn lambda_env_cell_cycle_is_collectible() {
        fn run(heap: &mut Heap<'_>) {
            let lambda_stats = Rc::new(Cell::new(0));
            let env_stats = Rc::new(Cell::new(0));
            let cell_stats = Rc::new(Cell::new(0));
            let lambda_ty = heap.register_tracked::<TestLambda<'_>>();
            let env_ty = heap.register_tracked::<TestLambdaEnv<'_>>();
            let cell_ty = heap.register_tracked::<TestLambdaCell<'_>>();
            let cell = heap.alloc(
                cell_ty,
                TestLambdaCell {
                    stats: Rc::clone(&cell_stats),
                    payload: None,
                },
            );
            let env = heap.alloc(
                env_ty,
                TestLambdaEnv {
                    stats: Rc::clone(&env_stats),
                    cell: cell.clone(),
                },
            );
            let lambda = heap.alloc(
                lambda_ty,
                TestLambda {
                    stats: Rc::clone(&lambda_stats),
                    env: env.clone(),
                },
            );
            heap.with_mut(&cell, |cell| cell.payload = Some(lambda.clone()));

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
            assert_eq!(cell_stats.get(), 1);
        }

        Heap::scope(run);
    }

    #[test]
    fn retained_lambda_keeps_env_cell_graph_alive_until_dropped() {
        fn run(heap: &mut Heap<'_>) {
            let stats = Rc::new(Cell::new(0));
            let lambda_ty = heap.register_tracked::<TestLambda<'_>>();
            let env_ty = heap.register_tracked::<TestLambdaEnv<'_>>();
            let cell_ty = heap.register_tracked::<TestLambdaCell<'_>>();
            let cell = heap.alloc(
                cell_ty,
                TestLambdaCell {
                    stats: Rc::clone(&stats),
                    payload: None,
                },
            );
            let env = heap.alloc(
                env_ty,
                TestLambdaEnv {
                    stats: Rc::clone(&stats),
                    cell: cell.clone(),
                },
            );
            let lambda = heap.alloc(
                lambda_ty,
                TestLambda {
                    stats: Rc::clone(&stats),
                    env: env.clone(),
                },
            );
            heap.with_mut(&cell, |cell| cell.payload = Some(lambda.clone()));
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
        }

        Heap::scope(run);
    }

    #[test]
    fn cycle_threshold_controls_collect() {
        fn run(heap: &mut Heap<'_>) {
            heap.set_cycle_threshold(2);
            let node_type = heap.register_tracked::<Node<'_>>();
            let a = heap.alloc(
                node_type,
                Node {
                    next: None,
                    value: 1,
                },
            );
            let b = heap.alloc(
                node_type,
                Node {
                    next: Some(a.clone()),
                    value: 2,
                },
            );
            heap.with_mut(&a, |node| node.next = Some(b.clone()));
            drop(a);
            let outcome = heap.collect(100);
            assert_eq!(outcome.cycle_status, CycleStatus::BelowThreshold);
            drop(b);
            let outcome = heap.collect(100);
            assert_eq!(outcome.cycle_status, CycleStatus::Complete);
            assert_eq!(heap.stats().live, 0);
        }

        Heap::scope(run);
    }

    #[test]
    fn collection_disabled_makes_public_collect_noop() {
        fn run(heap: &mut Heap<'_>) {
            heap.set_collection_enabled(false);
            let value_type = heap.register_untracked::<i32>();
            let value = heap.alloc(value_type, 1);
            drop(value);
            let outcome = heap.collect(8);
            assert_eq!(outcome.cycle_status, CycleStatus::Disabled);
            assert_eq!(heap.stats().live, 1);
            let outcome = heap.collect_all();
            assert_eq!(outcome.cycle_status, CycleStatus::Disabled);
            assert_eq!(heap.stats().live, 1);
            heap.set_collection_enabled(true);
            assert_eq!(heap.collect(0).collected, 1);
        }

        Heap::scope(run);
    }

    #[test]
    fn finalize_collects_cycles_even_when_collection_is_disabled() {
        fn run(heap: &mut Heap<'_>) {
            heap.set_collection_enabled(false);
            let node_type = heap.register_tracked::<Node<'_>>();
            let a = heap.alloc(
                node_type,
                Node {
                    next: None,
                    value: 1,
                },
            );
            let b = heap.alloc(
                node_type,
                Node {
                    next: Some(a.clone()),
                    value: 2,
                },
            );
            heap.with_mut(&a, |node| node.next = Some(b.clone()));
            drop(a);
            drop(b);
            assert!(heap.finalize().is_ok());
            assert!(!heap.collection_enabled());
        }

        Heap::scope(run);
    }

    struct PanicTrace<'cx> {
        child: Option<Handle<'cx, PanicTrace<'cx>>>,
        panic: Rc<Cell<bool>>,
    }

    // SAFETY: when tracing continues, `child` is the only edge and is reported once.
    unsafe impl<'cx> Trace<'cx> for PanicTrace<'cx> {
        fn trace<D: TraceDriver<'cx>>(&self, visitor: &mut Visitor<'cx, '_, D>) {
            assert!(!self.panic.get(), "trace panic");
            visitor.edge_opt(&self.child);
        }
    }

    #[test]
    fn panic_during_trace_does_not_poison_later_collection() {
        fn run(heap: &mut Heap<'_>) {
            let panic = Rc::new(Cell::new(false));
            let ty = heap.register_tracked::<PanicTrace<'_>>();
            let a = heap.alloc(
                ty,
                PanicTrace {
                    child: None,
                    panic: Rc::clone(&panic),
                },
            );
            let b = heap.alloc(
                ty,
                PanicTrace {
                    child: Some(a.clone()),
                    panic: Rc::clone(&panic),
                },
            );
            heap.with_mut(&a, |node| node.child = Some(b.clone()));
            drop(a);
            drop(b);
            panic.set(true);
            assert!(
                std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                    heap.collect_all();
                }))
                .is_err()
            );
            panic.set(false);
            heap.collect_all();
            assert_eq!(heap.stats().live, 0);
        }

        Heap::scope(run);
    }

    #[test]
    fn erased_access_uses_heap_type_descriptor_checks() {
        fn run(heap: &mut Heap<'_>) {
            let int_ty = heap.register_untracked::<i32>();
            let string_ty = heap.register_untracked::<String>();
            let value = heap.alloc(int_ty, 9);
            let erased = heap.erase(&value).expect("erase live handle");
            assert_eq!(erased.heap_type_id(), int_ty.id());
            assert_eq!(heap.try_with_erased(&erased, int_ty, |value| *value), Ok(9));
            assert!(
                heap.try_with_erased(&erased, string_ty, String::len)
                    .is_err()
            );
            drop(value);
            heap.collect(0);
            assert_eq!(heap.try_with_erased(&erased, int_ty, |value| *value), Ok(9));
            drop(erased);
            heap.collect(0);
            assert_eq!(heap.stats().live, 0);
        }

        Heap::scope(run);
    }

    #[test]
    fn erased_clone_retains_and_reuses_type_checks() {
        fn run(heap: &mut Heap<'_>) {
            let int_ty = heap.register_untracked::<i32>();
            let string_ty = heap.register_untracked::<String>();
            let value = heap.alloc(int_ty, 9);
            let erased = heap.erase(&value).expect("erase live handle");
            assert_eq!(value.strong_count(), 2);
            let cloned = erased.clone();
            assert_eq!(heap.stats().clones, 1);
            assert_eq!(value.strong_count(), 3);
            drop(value);
            drop(erased);
            heap.collect(0);
            assert_eq!(heap.try_with_erased(&cloned, int_ty, |value| *value), Ok(9));
            assert_eq!(
                heap.try_with_erased(&cloned, string_ty, String::len),
                Err(AccessError::DeadHandle)
            );
            drop(cloned);
            heap.collect(0);
            assert_eq!(heap.stats().live, 0);
        }

        Heap::scope(run);
    }

    #[test]
    fn dead_erased_clone_panics() {
        fn run(heap: &mut Heap<'_>) {
            let ty = heap.register_untracked::<i32>();
            let value = heap.alloc(ty, 1);
            let erased = heap.erase(&value).expect("erase live handle");
            let dead = manual_copy(&erased);
            drop(value);
            drop(erased);
            heap.collect(0);
            assert!(
                std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                    let _ = dead.clone();
                }))
                .is_err()
            );
            drop(ManuallyDrop::into_inner(dead));
        }

        Heap::scope(run);
    }

    #[test]
    fn stale_erased_clone_after_slot_reuse_panics_before_mutation() {
        fn run(heap: &mut Heap<'_>) {
            let ty = heap.register_untracked::<i32>();
            let first = heap.alloc(ty, 1);
            let erased = heap.erase(&first).expect("erase live handle");
            let stale = manual_copy(&erased);
            drop(first);
            drop(erased);
            heap.collect(0);
            let second = heap.alloc(ty, 2);
            assert!(
                std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                    let _ = stale.clone();
                }))
                .is_err()
            );
            assert_eq!(second.strong_count(), 1);
            assert_eq!(heap.with(&second, |value| *value), 2);
            drop(ManuallyDrop::into_inner(stale));
        }

        Heap::scope(run);
    }

    #[test]
    fn heap_teardown_drops_payload_owned_handles_before_deallocating_storage() {
        struct HoldsHandle<'cx> {
            child: Option<Handle<'cx, i32>>,
        }

        fn run(heap: &mut Heap<'_>) {
            let value_ty = heap.register_untracked::<i32>();
            let holder_ty = heap.register_untracked::<HoldsHandle<'_>>();

            let earlier_child = heap.alloc(value_ty, 1);
            let later_holder = heap.alloc(
                holder_ty,
                HoldsHandle {
                    child: Some(earlier_child.clone()),
                },
            );
            drop(earlier_child);
            drop(later_holder);

            let earlier_holder = heap.alloc(holder_ty, HoldsHandle { child: None });
            let later_child = heap.alloc(value_ty, 2);
            heap.with_mut(&earlier_holder, |holder| {
                holder.child = Some(later_child.clone());
            });
            drop(earlier_holder);
            drop(later_child);
        }

        Heap::scope(run);
    }

    #[test]
    fn payload_owned_erased_handle_teardown_does_not_alias_heap_state() {
        struct HoldsErased<'cx> {
            child: Option<ErasedHandle<'cx>>,
        }

        impl Drop for HoldsErased<'_> {
            fn drop(&mut self) {
                drop(self.child.take());
            }
        }

        fn run(heap: &mut Heap<'_>) {
            let value_ty = heap.register_untracked::<i32>();
            let holder_ty = heap.register_untracked::<HoldsErased<'_>>();
            let child = heap.alloc(value_ty, 1);
            let erased = heap.erase(&child).expect("erase child");
            let holder = heap.alloc(
                holder_ty,
                HoldsErased {
                    child: Some(erased),
                },
            );
            drop(child);
            drop(holder);
        }

        Heap::scope(run);
    }

    #[test]
    fn heap_teardown_marks_all_live_dropping_before_payload_drop() {
        struct Target;
        struct Cloner<'cx> {
            target: Option<Handle<'cx, Target>>,
            cloned: Rc<Cell<bool>>,
        }

        impl Drop for Cloner<'_> {
            fn drop(&mut self) {
                let _clone = self.target.as_ref().expect("target").clone();
                self.cloned.set(true);
            }
        }

        let cloned = Rc::new(Cell::new(false));
        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe({
            let cloned = Rc::clone(&cloned);
            move || {
                Heap::scope(|heap| {
                    let target_ty = heap.register_untracked::<Target>();
                    let cloner_ty = heap.register_untracked::<Cloner<'_>>();
                    let cloner = heap.alloc(
                        cloner_ty,
                        Cloner {
                            target: None,
                            cloned,
                        },
                    );
                    let target = heap.alloc(target_ty, Target);
                    heap.with_mut(&cloner, |cloner| cloner.target = Some(target.clone()));
                    drop(target);
                    drop(cloner);
                });
            }
        }));
        assert!(result.is_err());
        assert!(!cloned.get());
    }

    #[test]
    fn heap_teardown_payload_panic_deallocates_storage_once() {
        struct PanicOnDrop(Rc<Cell<usize>>);
        impl Drop for PanicOnDrop {
            fn drop(&mut self) {
                self.0.set(self.0.get() + 1);
                panic!("payload drop panic");
            }
        }

        let drops = Rc::new(Cell::new(0));
        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe({
            let drops = Rc::clone(&drops);
            move || {
                Heap::scope(|heap| {
                    let ty = heap.register_untracked::<PanicOnDrop>();
                    let _handle = heap.alloc(ty, PanicOnDrop(drops));
                });
            }
        }));
        assert!(result.is_err());
        assert_eq!(drops.get(), 1);
    }

    #[test]
    fn heap_teardown_objects_drop_payloads_after_handles() {
        struct Child(Rc<Cell<usize>>);
        impl Drop for Child {
            fn drop(&mut self) {
                self.0.set(self.0.get() + 1);
            }
        }
        struct Parent<'cx> {
            child: Option<Handle<'cx, Child>>,
            drops: Rc<Cell<usize>>,
        }
        impl Drop for Parent<'_> {
            fn drop(&mut self) {
                self.drops.set(self.drops.get() + 1);
                drop(self.child.take());
            }
        }

        let child_drops = Rc::new(Cell::new(0));
        let parent_drops = Rc::new(Cell::new(0));
        Heap::scope(|heap| {
            let child_ty = heap.register_untracked::<Child>();
            let parent_ty = heap.register_untracked::<Parent<'_>>();
            let child = heap.alloc(child_ty, Child(Rc::clone(&child_drops)));
            let _retained = child.clone();
            let _parent = heap.alloc(
                parent_ty,
                Parent {
                    child: Some(child),
                    drops: Rc::clone(&parent_drops),
                },
            );
        });
        assert_eq!(parent_drops.get(), 1);
        assert_eq!(child_drops.get(), 1);
    }

    #[test]
    fn cycle_payload_drop_resurrection_poisons_without_stale_handle() {
        #[derive(Trace)]
        struct ResurrectOnDrop<'cx> {
            edge: Option<Handle<'cx, ResurrectOnDrop<'cx>>>,
            #[trace(skip_unchecked)]
            stash: Rc<RefCell<Option<Handle<'cx, ResurrectOnDrop<'cx>>>>>,
            resurrect: bool,
        }

        impl Drop for ResurrectOnDrop<'_> {
            fn drop(&mut self) {
                if self.resurrect {
                    let edge = self.edge.as_ref().expect("edge to resurrect");
                    self.stash.replace(Some(edge.clone()));
                }
            }
        }

        fn run(heap: &mut Heap<'_>) {
            let stash = Rc::new(RefCell::new(None));
            let ty = heap.register_tracked::<ResurrectOnDrop<'_>>();
            let a = heap.alloc(
                ty,
                ResurrectOnDrop {
                    edge: None,
                    stash: Rc::clone(&stash),
                    resurrect: true,
                },
            );
            let b = heap.alloc(
                ty,
                ResurrectOnDrop {
                    edge: Some(a.clone()),
                    stash: Rc::clone(&stash),
                    resurrect: false,
                },
            );
            heap.with_mut(&a, |node| node.edge = Some(b.clone()));
            drop(a);
            drop(b);
            assert!(
                std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                    heap.collect_all();
                }))
                .is_err()
            );
            assert!(stash.borrow().is_none());
        }

        Heap::scope(run);
    }

    #[test]
    fn collect_all_rechecks_suspects_created_by_final_zero_drops() {
        #[derive(Trace)]
        struct Inner<'cx> {
            next: Option<Handle<'cx, Inner<'cx>>>,
        }

        struct Holder<'cx> {
            left: Handle<'cx, Inner<'cx>>,
            right: Handle<'cx, Inner<'cx>>,
        }

        impl Drop for Holder<'_> {
            fn drop(&mut self) {
                assert!(self.left.is_alive());
                assert!(self.right.is_alive());
            }
        }

        #[derive(Trace)]
        struct Dropper<'cx> {
            next: Option<Handle<'cx, Dropper<'cx>>>,
            #[trace(skip_unchecked)]
            holder: Option<Handle<'cx, Holder<'cx>>>,
        }

        impl Drop for Dropper<'_> {
            fn drop(&mut self) {
                drop(self.holder.take());
            }
        }

        fn run(heap: &mut Heap<'_>) {
            let inner_ty = heap.register_tracked::<Inner<'_>>();
            let holder_ty = heap.register_untracked::<Holder<'_>>();
            let dropper_ty = heap.register_tracked::<Dropper<'_>>();

            let inner_left = heap.alloc(inner_ty, Inner { next: None });
            let inner_right = heap.alloc(inner_ty, Inner { next: None });
            heap.with_mut(&inner_left, |inner| {
                inner.next = Some(inner_right.clone());
            });
            heap.with_mut(&inner_right, |inner| {
                inner.next = Some(inner_left.clone());
            });
            let holder = heap.alloc(
                holder_ty,
                Holder {
                    left: inner_left,
                    right: inner_right,
                },
            );

            let left = heap.alloc(
                dropper_ty,
                Dropper {
                    next: None,
                    holder: Some(holder),
                },
            );
            let right = heap.alloc(
                dropper_ty,
                Dropper {
                    next: Some(left.clone()),
                    holder: None,
                },
            );
            heap.with_mut(&left, |dropper| {
                dropper.next = Some(right.clone());
            });
            drop(left);
            drop(right);

            let outcome = heap.collect_all();
            assert_eq!(outcome.cycle_status, CycleStatus::Complete);
            assert_eq!(heap.stats().live, 0);
            assert_eq!(pending_cycles(heap), 0);
        }

        Heap::scope(run);
    }

    #[test]
    fn stale_generation_trace_edge_panics_before_collector_mutation() {
        struct StaleEdgeNode<'cx> {
            edge: ManuallyDrop<Handle<'cx, i32>>,
        }

        // SAFETY: invalid fixture confined to `catch_unwind`; `Visitor::edge` rejects it before mutation.
        unsafe impl<'cx> Trace<'cx> for StaleEdgeNode<'cx> {
            fn trace<D: TraceDriver<'cx>>(&self, visitor: &mut Visitor<'cx, '_, D>) {
                visitor.edge(&self.edge);
            }
        }

        fn run(heap: &mut Heap<'_>) {
            let value_ty = heap.register_untracked::<i32>();
            let stale = heap.alloc(value_ty, 1);
            let stale_copy = manual_copy(&stale);
            drop(stale);
            heap.collect(0);
            let _reused = heap.alloc(value_ty, 2);

            let node_ty = heap.register_tracked::<StaleEdgeNode<'_>>();
            let node = heap.alloc(node_ty, StaleEdgeNode { edge: stale_copy });
            let alias = node.clone();
            drop(node);
            let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                heap.collect_all();
            }));
            assert!(result.is_err());
            drop(alias);
            heap.collect(0);
        }

        Heap::scope(run);
    }

    #[test]
    fn dead_trace_edge_panics_before_collector_mutation() {
        struct DeadEdgeNode<'cx> {
            edge: ManuallyDrop<Handle<'cx, i32>>,
        }

        // SAFETY: invalid fixture confined to `catch_unwind`; `Visitor::edge` rejects it before mutation.
        unsafe impl<'cx> Trace<'cx> for DeadEdgeNode<'cx> {
            fn trace<D: TraceDriver<'cx>>(&self, visitor: &mut Visitor<'cx, '_, D>) {
                visitor.edge(&self.edge);
            }
        }

        fn run(heap: &mut Heap<'_>) {
            let value_ty = heap.register_untracked::<i32>();
            let dead = heap.alloc(value_ty, 1);
            let dead_copy = manual_copy(&dead);
            drop(dead);
            heap.collect(0);

            let node_ty = heap.register_tracked::<DeadEdgeNode<'_>>();
            let node = heap.alloc(node_ty, DeadEdgeNode { edge: dead_copy });
            let alias = node.clone();
            drop(node);
            let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                heap.collect_all();
            }));
            assert!(result.is_err());
            assert_eq!(heap.stats().candidate_nodes, 0);
            drop(alias);
            heap.collect(0);
        }

        Heap::scope(run);
    }

    #[test]
    fn cycle_payload_drop_can_drop_internal_edges_without_double_drop() {
        #[derive(Trace)]
        struct DropNode<'cx> {
            next: Option<Handle<'cx, DropNode<'cx>>>,
            #[trace(skip_unchecked)]
            drops: Rc<Cell<usize>>,
        }

        impl Drop for DropNode<'_> {
            fn drop(&mut self) {
                self.drops.set(self.drops.get() + 1);
                drop(self.next.take());
            }
        }

        fn run(heap: &mut Heap<'_>, drops: Rc<Cell<usize>>) {
            let ty = heap.register_tracked::<DropNode<'_>>();
            let a = heap.alloc(
                ty,
                DropNode {
                    next: None,
                    drops: Rc::clone(&drops),
                },
            );
            let b = heap.alloc(
                ty,
                DropNode {
                    next: Some(a.clone()),
                    drops,
                },
            );
            heap.with_mut(&a, |node| node.next = Some(b.clone()));
            drop(a);
            drop(b);
            heap.collect_all();
            assert_eq!(heap.stats().live, 0);
        }

        let drops = Rc::new(Cell::new(0));
        Heap::scope(|heap| run(heap, Rc::clone(&drops)));
        assert_eq!(drops.get(), 2);
    }

    #[test]
    fn cycle_payload_drop_panic_continues_other_white_drops() {
        #[derive(Trace)]
        struct PanicNode<'cx> {
            next: Option<Handle<'cx, PanicNode<'cx>>>,
            panic: bool,
            #[trace(skip_unchecked)]
            drops: Rc<Cell<usize>>,
        }

        impl Drop for PanicNode<'_> {
            fn drop(&mut self) {
                self.drops.set(self.drops.get() + 1);
                assert!(!self.panic, "payload drop panic");
            }
        }

        fn run(heap: &mut Heap<'_>, drops: Rc<Cell<usize>>) {
            let ty = heap.register_tracked::<PanicNode<'_>>();
            let a = heap.alloc(
                ty,
                PanicNode {
                    next: None,
                    panic: true,
                    drops: Rc::clone(&drops),
                },
            );
            let b = heap.alloc(
                ty,
                PanicNode {
                    next: Some(a.clone()),
                    panic: false,
                    drops,
                },
            );
            heap.with_mut(&a, |node| node.next = Some(b.clone()));
            drop(a);
            drop(b);
            assert!(
                std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                    heap.collect_all();
                }))
                .is_err()
            );
        }

        let drops = Rc::new(Cell::new(0));
        Heap::scope(|heap| run(heap, Rc::clone(&drops)));
        assert_eq!(drops.get(), 2);
    }

    #[test]
    fn payload_drop_panic_during_cycle_collection_poisons_heap() {
        #[derive(Trace)]
        struct PanicOnDrop<'cx> {
            next: Option<Handle<'cx, PanicOnDrop<'cx>>>,
            #[trace(skip_unchecked)]
            panicked: Rc<Cell<bool>>,
            #[trace(skip_unchecked)]
            drops: Rc<Cell<usize>>,
        }

        impl Drop for PanicOnDrop<'_> {
            fn drop(&mut self) {
                self.drops.set(self.drops.get() + 1);
                assert!(self.panicked.replace(true), "payload drop panic");
            }
        }

        fn run(heap: &mut Heap<'_>, panicked: Rc<Cell<bool>>, drops: Rc<Cell<usize>>) {
            let ty = heap.register_tracked::<PanicOnDrop<'_>>();
            let a = heap.alloc(
                ty,
                PanicOnDrop {
                    next: None,
                    panicked: Rc::clone(&panicked),
                    drops: Rc::clone(&drops),
                },
            );
            let b = heap.alloc(
                ty,
                PanicOnDrop {
                    next: Some(a.clone()),
                    panicked,
                    drops,
                },
            );
            heap.with_mut(&a, |node| node.next = Some(b.clone()));
            drop(a);
            drop(b);
            assert!(
                std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                    heap.collect_all();
                }))
                .is_err()
            );
            let poisoned = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                heap.collect_all();
            }));
            assert!(poisoned.is_err());
            let ty = heap.register_untracked::<i32>();
            let alloc_after_poison = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                heap.alloc(ty, 1);
            }));
            assert!(alloc_after_poison.is_err());
        }

        let panicked = Rc::new(Cell::new(false));
        let drops = Rc::new(Cell::new(0));
        Heap::scope(|heap| run(heap, Rc::clone(&panicked), Rc::clone(&drops)));
        assert_eq!(drops.get(), 2);
    }

    #[test]
    fn stale_handle_drop_after_slot_reuse_does_not_touch_new_object() {
        fn run(heap: &mut Heap<'_>) {
            let ty = heap.register_untracked::<i32>();
            let first = heap.alloc(ty, 1);
            let stale = manual_copy(&first);
            drop(first);
            heap.collect(0);
            let second = heap.alloc(ty, 2);
            assert_eq!(second.strong_count(), 1);
            drop(ManuallyDrop::into_inner(stale));
            assert_eq!(second.strong_count(), 1);
            assert_eq!(heap.with(&second, |value| *value), 2);
        }

        Heap::scope(run);
    }

    #[test]
    fn stale_handle_clone_after_slot_reuse_panics_before_mutation() {
        fn run(heap: &mut Heap<'_>) {
            let ty = heap.register_untracked::<i32>();
            let first = heap.alloc(ty, 1);
            let stale = manual_copy(&first);
            drop(first);
            heap.collect(0);
            let second = heap.alloc(ty, 2);
            assert!(
                std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                    let _ = stale.clone();
                }))
                .is_err()
            );
            assert_eq!(second.strong_count(), 1);
            assert_eq!(heap.with(&second, |value| *value), 2);
            drop(ManuallyDrop::into_inner(stale));
        }

        Heap::scope(run);
    }

    #[test]
    fn dead_handle_drop_is_noop() {
        fn run(heap: &mut Heap<'_>) {
            let ty = heap.register_untracked::<i32>();
            let handle = heap.alloc(ty, 1);
            let dead = manual_copy(&handle);
            drop(handle);
            heap.collect(0);
            drop(ManuallyDrop::into_inner(dead));
            assert_eq!(heap.stats().live, 0);
        }

        Heap::scope(run);
    }

    #[test]
    fn dead_handle_clone_panics() {
        fn run(heap: &mut Heap<'_>) {
            let ty = heap.register_untracked::<i32>();
            let handle = heap.alloc(ty, 1);
            let dead = manual_copy(&handle);
            drop(handle);
            heap.collect(0);
            assert!(
                std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                    let _ = dead.clone();
                }))
                .is_err()
            );
            drop(ManuallyDrop::into_inner(dead));
        }

        Heap::scope(run);
    }

    #[test]
    fn zero_drop_payload_can_drop_child_handle() {
        struct Child(Rc<Cell<usize>>);
        impl Drop for Child {
            fn drop(&mut self) {
                self.0.set(self.0.get() + 1);
            }
        }
        struct Parent<'cx> {
            child: Option<Handle<'cx, Child>>,
            drops: Rc<Cell<usize>>,
        }
        impl Drop for Parent<'_> {
            fn drop(&mut self) {
                self.drops.set(self.drops.get() + 1);
                drop(self.child.take());
            }
        }

        fn run(heap: &mut Heap<'_>, parent_drops: Rc<Cell<usize>>, child_drops: Rc<Cell<usize>>) {
            let child_ty = heap.register_untracked::<Child>();
            let parent_ty = heap.register_untracked::<Parent<'_>>();
            let child = heap.alloc(child_ty, Child(child_drops));
            let parent = heap.alloc(
                parent_ty,
                Parent {
                    child: Some(child.clone()),
                    drops: parent_drops,
                },
            );
            drop(child);
            drop(parent);
            heap.collect(0);
            assert_eq!(heap.stats().live, 0);
        }

        let parent_drops = Rc::new(Cell::new(0));
        let child_drops = Rc::new(Cell::new(0));
        Heap::scope(|heap| run(heap, Rc::clone(&parent_drops), Rc::clone(&child_drops)));
        assert_eq!(parent_drops.get(), 1);
        assert_eq!(child_drops.get(), 1);
    }

    #[test]
    fn zero_drop_payload_can_clone_live_child_without_corruption() {
        struct Child;
        struct Parent<'cx> {
            child: Option<Handle<'cx, Child>>,
            stash: Rc<RefCell<Option<Handle<'cx, Child>>>>,
        }
        impl Drop for Parent<'_> {
            fn drop(&mut self) {
                self.stash
                    .replace(Some(self.child.as_ref().expect("child").clone()));
            }
        }

        fn run<'cx>(heap: &mut Heap<'cx>, stash: &Rc<RefCell<Option<Handle<'cx, Child>>>>) {
            let child_ty = heap.register_untracked::<Child>();
            let parent_ty = heap.register_untracked::<Parent<'_>>();
            let child = heap.alloc(child_ty, Child);
            let parent = heap.alloc(
                parent_ty,
                Parent {
                    child: Some(child.clone()),
                    stash: Rc::clone(stash),
                },
            );
            drop(child);
            drop(parent);
            heap.collect(0);
            assert_eq!(heap.stats().live, 1);
            drop(stash.borrow_mut().take());
            heap.collect(0);
            assert_eq!(heap.stats().live, 0);
        }

        Heap::scope(|heap| {
            let stash = Rc::new(RefCell::new(None));
            run(heap, &stash);
        });
    }

    #[test]
    fn zero_drop_payload_panic_poisons_and_prevents_reuse() {
        struct PanicOnDrop;
        impl Drop for PanicOnDrop {
            fn drop(&mut self) {
                panic!("payload drop panic");
            }
        }

        fn run(heap: &mut Heap<'_>) {
            let ty = heap.register_untracked::<PanicOnDrop>();
            let handle = heap.alloc(ty, PanicOnDrop);
            drop(handle);
            assert!(
                std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| heap.collect(0))).is_err()
            );
            assert!(
                std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| heap.collect(0))).is_err()
            );
            let int_ty = heap.register_untracked::<i32>();
            assert!(
                std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                    heap.alloc(int_ty, 1);
                }))
                .is_err()
            );
        }

        Heap::scope(run);
    }

    #[test]
    fn zero_drop_payload_panic_then_heap_teardown_no_double_drop() {
        struct PanicOnDrop(Rc<Cell<usize>>);
        impl Drop for PanicOnDrop {
            fn drop(&mut self) {
                self.0.set(self.0.get() + 1);
                panic!("payload drop panic");
            }
        }

        let drops = Rc::new(Cell::new(0));
        Heap::scope(|heap| {
            let ty = heap.register_untracked::<PanicOnDrop>();
            let handle = heap.alloc(ty, PanicOnDrop(Rc::clone(&drops)));
            drop(handle);
            assert!(
                std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| heap.collect(0))).is_err()
            );
        });
        assert_eq!(drops.get(), 1);
    }

    #[test]
    fn finalize_reports_retained_handle_kept_object() {
        fn run(heap: &mut Heap<'_>) {
            let ty = heap.register_untracked::<i32>();
            let handle = heap.alloc(ty, 1);
            let retained = handle.clone();
            drop(handle);
            heap.collect(0);
            let report = heap.finalize().unwrap_err();
            assert_eq!(report.live, 1);
            assert_eq!(report.by_type[0].live, 1);
            drop(retained);
            assert!(heap.finalize().is_ok());
        }

        Heap::scope(run);
    }

    #[test]
    fn finalize_reports_erased_handle_kept_object() {
        fn run(heap: &mut Heap<'_>) {
            let ty = heap.register_untracked::<i32>();
            let handle = heap.alloc(ty, 1);
            let erased = heap.erase(&handle).unwrap();
            drop(handle);
            heap.collect(0);
            let report = heap.finalize().unwrap_err();
            assert_eq!(report.live, 1);
            drop(erased);
            assert!(heap.finalize().is_ok());
        }

        Heap::scope(run);
    }

    #[test]
    fn finalize_reports_external_live_handle_by_type() {
        fn run(heap: &mut Heap<'_>) {
            let ty = heap.register_untracked::<i32>();
            let handle = heap.alloc(ty, 1);
            let report = heap.finalize().unwrap_err();
            assert_eq!(report.live, 1);
            assert!(report.by_type.iter().any(|entry| {
                entry.heap_type_id == ty.id() && entry.type_name == "i32" && entry.live == 1
            }));
            drop(handle);
        }

        Heap::scope(run);
    }

    #[test]
    fn finalize_reports_reachable_graph_from_external_handle() {
        fn run(heap: &mut Heap<'_>) {
            let ty = heap.register_tracked::<Node<'_>>();
            let a = heap.alloc(
                ty,
                Node {
                    next: None,
                    value: 1,
                },
            );
            let b = heap.alloc(
                ty,
                Node {
                    next: None,
                    value: 2,
                },
            );
            heap.with_mut(&a, |node| node.next = Some(b.clone()));
            drop(b);
            let report = heap.finalize().unwrap_err();
            assert_eq!(report.live, 2);
            assert_eq!(report.by_type[0].heap_type_id, ty.id());
            assert_eq!(report.by_type[0].live, 2);
            assert_eq!(heap.with(&a, |node| node.value), 1);
            drop(a);
        }

        Heap::scope(run);
    }

    #[test]
    fn finalize_leak_report_ignores_stale_suspect_items() {
        fn run(heap: &mut Heap<'_>) {
            let ty = heap.register_tracked::<Node<'_>>();
            let first = heap.alloc(
                ty,
                Node {
                    next: None,
                    value: 1,
                },
            );
            let alias = first.clone();
            drop(alias);
            drop(first);
            heap.collect(0);
            let second = heap.alloc(
                ty,
                Node {
                    next: None,
                    value: 2,
                },
            );
            let report = heap.leak_report();
            assert_eq!(report.live, 1);
            assert_eq!(report.by_type[0].live, 1);
            assert_eq!(report.by_type[0].suspects, 0);
            let report = heap.finalize().unwrap_err();
            assert_eq!(report.live, 1);
            assert_eq!(report.by_type[0].live, 1);
            assert_eq!(report.by_type[0].suspects, 0);
            assert_eq!(heap.with(&second, |node| node.value), 2);
        }

        Heap::scope(run);
    }

    #[test]
    fn finalize_on_poisoned_heap_panics() {
        struct PanicOnDrop;
        impl Drop for PanicOnDrop {
            fn drop(&mut self) {
                panic!("payload drop panic");
            }
        }

        fn run(heap: &mut Heap<'_>) {
            let ty = heap.register_untracked::<PanicOnDrop>();
            let handle = heap.alloc(ty, PanicOnDrop);
            drop(handle);
            assert!(
                std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| heap.collect(0))).is_err()
            );
            assert!(
                std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| heap.finalize())).is_err()
            );
        }

        Heap::scope(run);
    }
    #[test]
    fn trace_helpers_preserve_edges() {
        #[derive(Trace)]
        struct Child<'cx> {
            back: Option<Handle<'cx, Parent<'cx>>>,
        }
        #[derive(Trace)]
        struct Nested<'cx>(Handle<'cx, Child<'cx>>);
        #[derive(Trace)]
        struct Parent<'cx> {
            opt: Option<Handle<'cx, Child<'cx>>>,
            vec: Vec<Handle<'cx, Child<'cx>>>,
            array: [Handle<'cx, Child<'cx>>; 1],
            boxed: Box<Nested<'cx>>,
            result: Result<Handle<'cx, Child<'cx>>, i32>,
            tuple: (Handle<'cx, Child<'cx>>, i32),
        }

        fn run(heap: &mut Heap<'_>) {
            let parent_ty = heap.register_tracked::<Parent<'_>>();
            let child_ty = heap.register_tracked::<Child<'_>>();
            let child = heap.alloc(child_ty, Child { back: None });
            let parent = heap.alloc(
                parent_ty,
                Parent {
                    opt: Some(child.clone()),
                    vec: vec![child.clone(), child.clone()],
                    array: [child.clone()],
                    boxed: Box::new(Nested(child.clone())),
                    result: Ok(child.clone()),
                    tuple: (child.clone(), 1),
                },
            );
            heap.with_mut(&child, |child| child.back = Some(parent.clone()));
            drop(parent);
            drop(child);
            let outcome = heap.collect_all();
            assert_eq!(outcome.collected, 2);
            assert_eq!(heap.stats().internal_edges, 8);
            assert_eq!(heap.stats().live, 0);
        }

        Heap::scope(run);
    }

    #[test]
    fn budgeted_collection_leaves_no_gray_state_after_completed_batch() {
        fn run(heap: &mut Heap<'_>) {
            let node_ty = heap.register_tracked::<Node<'_>>();
            let mut nodes = Vec::new();
            for value in 0..70 {
                let node = heap.alloc(node_ty, Node { next: None, value });
                let alias = node.clone();
                drop(alias);
                nodes.push(node);
            }

            while pending_cycles(heap) > 0 {
                heap.collect(1);
                for (expected, node) in nodes.iter().enumerate() {
                    assert_eq!(heap.with(node, |node| node.value), expected as i32);
                }
            }
            assert_eq!(heap.stats().live, 70);
            assert_eq!(heap.stats().stale_roots, 0);
        }

        Heap::scope(run);
    }

    #[test]
    fn tiny_hint_collection_records_pending_cycles() {
        fn run(heap: &mut Heap<'_>) {
            let node_ty = heap.register_tracked::<Node<'_>>();
            let a = heap.alloc(
                node_ty,
                Node {
                    next: None,
                    value: 1,
                },
            );
            let b = heap.alloc(
                node_ty,
                Node {
                    next: Some(a.clone()),
                    value: 2,
                },
            );
            heap.with_mut(&a, |node| node.next = Some(b.clone()));
            drop(a);
            drop(b);
            let outcome = heap.collect(1);
            assert_eq!(outcome.cycle_roots, 1);
            assert_eq!(outcome.pending_cycles, pending_cycles(heap));
            heap.collect_all();
            assert_eq!(heap.stats().live, 0);
        }

        Heap::scope(run);
    }

    #[test]
    fn budgeted_collection_zero_budget_skips_without_mutation() {
        fn run(heap: &mut Heap<'_>) {
            let node_ty = heap.register_tracked::<Node<'_>>();
            let node = heap.alloc(
                node_ty,
                Node {
                    next: None,
                    value: 1,
                },
            );
            let alias = node.clone();
            drop(alias);
            assert_eq!(pending_cycles(heap), 1);
            let outcome = heap.collect(0);
            assert_eq!(outcome.cycle_status, CycleStatus::BudgetExhausted);
            assert_eq!(pending_cycles(heap), 1);
            assert_eq!(heap.with(&node, |node| node.value), 1);
        }

        Heap::scope(run);
    }

    #[test]
    fn stale_suspect_item_after_slot_reuse_does_not_touch_new_object() {
        fn run(heap: &mut Heap<'_>) {
            let node_ty = heap.register_tracked::<Node<'_>>();
            let first = heap.alloc(
                node_ty,
                Node {
                    next: None,
                    value: 1,
                },
            );
            let alias = first.clone();
            drop(alias);
            assert_eq!(pending_cycles(heap), 1);
            drop(first);
            heap.collect(0);
            let second = heap.alloc(
                node_ty,
                Node {
                    next: None,
                    value: 2,
                },
            );
            assert_eq!(pending_cycles(heap), 1);
            let outcome = heap.collect_all();
            assert_eq!(outcome.cycle_roots, 1);
            assert_eq!(heap.stats().stale_roots, 1);
            assert_eq!(heap.with(&second, |node| node.value), 2);
            assert_eq!(second.strong_count(), 1);
        }

        Heap::scope(run);
    }

    #[test]
    fn stale_erased_handle_drop_after_slot_reuse_does_not_touch_new_object() {
        fn run(heap: &mut Heap<'_>) {
            let ty = heap.register_untracked::<i32>();
            let first = heap.alloc(ty, 1);
            let erased = heap.erase(&first).unwrap();
            let stale = manual_copy(&erased);
            drop(first);
            drop(erased);
            heap.collect(0);
            let second = heap.alloc(ty, 2);
            assert_eq!(second.strong_count(), 1);
            drop(ManuallyDrop::into_inner(stale));
            assert_eq!(second.strong_count(), 1);
            assert_eq!(heap.with(&second, |value| *value), 2);
        }

        Heap::scope(run);
    }

    #[test]
    fn budgeted_suspect_queue_preserves_pending_suffix() {
        fn run(heap: &mut Heap<'_>) {
            let node_ty = heap.register_tracked::<Node<'_>>();
            let mut nodes = Vec::new();
            for value in 0..70 {
                let node = heap.alloc(node_ty, Node { next: None, value });
                let alias = node.clone();
                drop(alias);
                nodes.push(node);
            }
            assert_eq!(pending_cycles(heap), 70);

            let first = heap.collect(1);
            assert_eq!(first.cycle_roots, 1);
            assert_eq!(first.pending_cycles, 69);
            assert_eq!(pending_cycles(heap), 69);

            let second = heap.collect_all();
            assert_eq!(second.cycle_roots, 69);
            assert_eq!(pending_cycles(heap), 0);
            assert_eq!(heap.stats().cycle_roots, 70);
            assert_eq!(heap.stats().stale_roots, 0);
            assert_eq!(heap.stats().live, 70);
            for (expected, node) in nodes.iter().enumerate() {
                assert_eq!(heap.with(node, |node| node.value), expected as i32);
            }

            let tail_alias = nodes.last().expect("tail node").clone();
            drop(tail_alias);
            assert_eq!(pending_cycles(heap), 1);
        }

        Heap::scope(run);
    }

    #[test]
    fn provider_traced_extern_reports_internal_handles() {
        struct ExternObject<'cx> {
            provided_edge: Option<Handle<'cx, ProviderChild<'cx>>>,
            trace_count: Rc<Cell<usize>>,
        }

        #[derive(Trace)]
        struct ProviderChild<'cx> {
            owner: Option<Handle<'cx, ExternObject<'cx>>>,
        }

        // SAFETY: `provided_edge` is the only heap edge; `trace_count` owns none.
        unsafe impl<'cx> Trace<'cx> for ExternObject<'cx> {
            fn trace<D: TraceDriver<'cx>>(&self, visitor: &mut Visitor<'cx, '_, D>) {
                self.trace_count.set(self.trace_count.get() + 1);
                visitor.edge_opt(&self.provided_edge);
            }
        }

        fn run(heap: &mut Heap<'_>) {
            let trace_count = Rc::new(Cell::new(0));
            let extern_ty = heap.register_tracked::<ExternObject<'_>>();
            let child_ty = heap.register_tracked::<ProviderChild<'_>>();
            let child = heap.alloc(child_ty, ProviderChild { owner: None });
            let extern_object = heap.alloc(
                extern_ty,
                ExternObject {
                    provided_edge: Some(child.clone()),
                    trace_count: Rc::clone(&trace_count),
                },
            );
            heap.with_mut(&child, |child| child.owner = Some(extern_object.clone()));
            drop(extern_object);
            drop(child);
            heap.collect_all();
            assert!(trace_count.get() > 0);
            assert_eq!(heap.stats().live, 0);
        }

        Heap::scope(run);
    }
}
