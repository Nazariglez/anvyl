use anvyx_runtime::{Handle, Heap, Trace};

#[derive(Trace)]
struct Node<'cx> {
    next: Option<Handle<'cx, Node<'cx>>>,
}

#[test]
fn runtime_trace_collects_cycle() {
    Heap::scope(|heap| {
        let node_ty = heap.register_tracked::<Node<'_>>();
        let a = heap.alloc(node_ty, Node { next: None });
        let b = heap.alloc(
            node_ty,
            Node {
                next: Some(a.clone()),
            },
        );
        heap.with_mut(&a, |node| node.next = Some(b.clone()));

        drop(a);
        drop(b);
        heap.collect_all();

        assert_eq!(heap.stats().live, 0);
    });
}
