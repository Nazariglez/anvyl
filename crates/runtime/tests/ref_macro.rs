#![allow(dead_code)]

use std::{cell::Cell, marker::PhantomData, rc::Rc};

use anvyx_runtime::{
    AnvRef, AnvRefType, AnvyxRef, AnvyxRefExport, ErasedHandle, ExternRep, ExternTypeExpr, Heap,
    Trace, TraceDriver, Visitor,
};

#[derive(AnvyxRef)]
#[anvyx(name = "Counter")]
struct HostCounter {
    #[anvyx(field)]
    count: i64,
}

struct HostNode<'cx> {
    child: Option<AnvRef<'cx, HostNode<'cx>>>,
    traces: Rc<Cell<usize>>,
}

#[derive(AnvyxRef)]
struct EdgeHolder {
    edge: Option<ErasedHandle<'static>>,
}

struct HiddenEdge<'cx>(AnvRef<'cx, HostCounter>);

#[derive(AnvyxRef)]
struct WrappedEdge<'cx> {
    edge: Option<HiddenEdge<'cx>>,
}

#[derive(AnvyxRef)]
struct BrandedResource<'cx> {
    _brand: PhantomData<AnvRef<'cx, HostCounter>>,
}

unsafe impl AnvyxRefExport for HostNode<'_> {
    const OWNS_ANVYX_HEAP_EDGES: bool = true;
}

unsafe impl<'cx> Trace<'cx> for HostNode<'cx> {
    fn trace<D: TraceDriver<'cx>>(&self, visitor: &mut Visitor<'cx, '_, D>) {
        self.traces.set(self.traces.get() + 1);
        self.child.trace(visitor);
    }
}

fn assert_ref<T: AnvyxRefExport>() {}

#[test]
fn ref_descriptor_contains_exported_fields_and_name() {
    assert_ref::<HostCounter>();

    let export = __anvyx_export_hostcounter();

    assert_eq!(export.descriptor.name, "Counter");
    assert_eq!(export.descriptor.rep, ExternRep::Shared);
    assert_eq!(export.descriptor.fields.len(), 1);
    assert_eq!(export.descriptor.fields[0].name, "count");
    assert_eq!(export.descriptor.fields[0].ty, ExternTypeExpr::Int);
    assert!(export.bindings.is_empty());
}

#[test]
fn derived_ref_marks_heap_edge_fields() {
    assert!(std::hint::black_box(EdgeHolder::OWNS_ANVYX_HEAP_EDGES));
    assert!(std::hint::black_box(WrappedEdge::OWNS_ANVYX_HEAP_EDGES));
}

#[test]
fn lifetime_brand_does_not_make_resource_tracked() {
    assert!(!std::hint::black_box(
        BrandedResource::OWNS_ANVYX_HEAP_EDGES
    ));
    Heap::scope(|heap| {
        let _ = AnvRefType::<BrandedResource<'_>>::register_untracked(heap);
    });
}

#[test]
#[should_panic(expected = "resource type owns Anvyx heap edges and must be registered as tracked")]
fn untracked_registration_rejects_derived_heap_edge_resource() {
    Heap::scope(|heap| {
        let _ = AnvRefType::<EdgeHolder>::register_untracked(heap);
    });
}

#[test]
fn ref_value_allocates_and_borrows_managed_resource() {
    Heap::scope(|heap| {
        let counter_ty = AnvRefType::<HostCounter>::register_untracked(heap);
        let counter = counter_ty.alloc(heap, HostCounter { count: 1 });
        let alias = counter.clone();

        assert!(counter.ptr_eq(&alias));
        assert_eq!(counter.with(heap, |counter| counter.count).unwrap(), 1);
        counter
            .with_mut(heap, |counter| counter.count += 1)
            .unwrap();
        assert_eq!(alias.with(heap, |counter| counter.count).unwrap(), 2);

        let erased = counter.erase(heap).unwrap();
        assert_eq!(
            counter_ty
                .with_erased(heap, &erased, |counter| counter.count)
                .unwrap(),
            2
        );
        counter_ty
            .with_erased_mut(heap, &erased, |counter| counter.count += 3)
            .unwrap();
        assert_eq!(counter.with(heap, |counter| counter.count).unwrap(), 5);
    });
}

#[test]
fn ref_value_traces_native_resource_edges() {
    Heap::scope(|heap| {
        let traces = Rc::new(Cell::new(0));
        let node_ty = AnvRefType::<HostNode<'_>>::register_tracked(heap);
        let child = node_ty.alloc(
            heap,
            HostNode {
                child: None,
                traces: Rc::clone(&traces),
            },
        );
        let parent = node_ty.alloc(
            heap,
            HostNode {
                child: Some(child.clone()),
                traces: Rc::clone(&traces),
            },
        );
        child
            .with_mut(heap, |child| child.child = Some(parent.clone()))
            .unwrap();

        drop(child);
        drop(parent);
        heap.collect_all();

        assert!(traces.get() > 0);
        assert_eq!(heap.stats().live, 0);
    });
}
