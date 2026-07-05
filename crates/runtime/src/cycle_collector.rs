use std::ptr::NonNull;

pub use crate::suspect_buffer::{reset_collect_threshold, set_auto_collect};
use crate::{
    managed_rc::{CycleColor, RcHeader},
    suspect_buffer::SUSPECT_BUFFER,
};

pub struct CollectStats {
    pub suspects: usize,
    pub freed: usize,
}

pub fn collect_cycles() -> CollectStats {
    SUSPECT_BUFFER.with(|buf| {
        let mut suspects = buf.borrow_mut();
        if suspects.is_empty() {
            return CollectStats {
                suspects: 0,
                freed: 0,
            };
        }
        let num_suspects = suspects.len();

        // trial-delete from purple suspects
        for entry in suspects.iter() {
            let header = unsafe { entry.ptr.as_ref() };
            if header.strong() == 0 {
                continue;
            }
            if header.color() == CycleColor::Purple {
                mark_gray(entry.ptr);
            } else {
                header.set_buffered(false);
            }
        }

        // scan to confirm garbage vs reachable
        for entry in suspects.iter() {
            let header = unsafe { entry.ptr.as_ref() };
            if header.color() == CycleColor::Gray {
                scan(entry.ptr);
            }
        }

        // collect white garbage
        // drain first so cascading drops can push new suspects without a borrow conflict
        let entries = suspects.drain(..).collect::<Vec<_>>();
        drop(suspects);

        // handle non-white entries (alive or dead suspects)
        for entry in &entries {
            let header = unsafe { entry.ptr.as_ref() };
            if header.color() != CycleColor::White {
                header.set_buffered(false);
                if header.strong() == 0 {
                    // Dead suspect: strong hit 0 while buffered. Free now.
                    if let Some(vt) = header.cycle_vtable() {
                        (vt.dropper)(entry.ptr);
                    }
                } else {
                    header.set_color(CycleColor::Black);
                }
            }
        }

        // gather all white garbage (suspects + their reachable white children)
        let mut garbage = vec![];
        for entry in &entries {
            collect_white(entry.ptr, &mut garbage);
        }
        let freed_count = garbage.len();

        // clear cycle fields in all garbage (severs references before any frees)
        for &ptr in &garbage {
            let vt = unsafe { ptr.as_ref() }
                .cycle_vtable()
                .expect("garbage node must have vtable");
            (vt.clear_cycle_fields)(ptr);
        }

        for &ptr in &garbage {
            let header = unsafe { ptr.as_ref() };
            header.set_buffered(false);
            let vt = header
                .cycle_vtable()
                .expect("garbage node must have vtable");
            (vt.dropper)(ptr);
        }

        CollectStats {
            suspects: num_suspects,
            freed: freed_count,
        }
    })
}

fn mark_gray(ptr: NonNull<RcHeader>) {
    let header = unsafe { ptr.as_ref() };
    if header.color() == CycleColor::Gray {
        return;
    }
    header.set_color(CycleColor::Gray);
    if let Some(vt) = header.cycle_vtable() {
        (vt.children)(ptr, &mut |child_ptr| {
            let child_hdr = unsafe { child_ptr.as_ref() };
            child_hdr.decrement_strong();
            mark_gray(child_ptr);
        });
    }
}

fn scan(ptr: NonNull<RcHeader>) {
    let header = unsafe { ptr.as_ref() };
    if header.color() != CycleColor::Gray {
        return;
    }
    if header.strong() > 0 {
        scan_black(ptr);
    } else {
        header.set_color(CycleColor::White);
        if let Some(vt) = header.cycle_vtable() {
            (vt.children)(ptr, &mut |child_ptr| {
                scan(child_ptr);
            });
        }
    }
}

fn scan_black(ptr: NonNull<RcHeader>) {
    let header = unsafe { ptr.as_ref() };
    header.set_color(CycleColor::Black);
    if let Some(vt) = header.cycle_vtable() {
        (vt.children)(ptr, &mut |child_ptr| {
            let child_hdr = unsafe { child_ptr.as_ref() };
            child_hdr.increment_strong();
            if child_hdr.color() != CycleColor::Black {
                scan_black(child_ptr);
            }
        });
    }
}

fn collect_white(ptr: NonNull<RcHeader>, garbage: &mut Vec<NonNull<RcHeader>>) {
    let header = unsafe { ptr.as_ref() };
    if header.color() != CycleColor::White {
        return;
    }
    header.set_color(CycleColor::Black); // prevent re-visit
    if let Some(vt) = header.cycle_vtable() {
        (vt.children)(ptr, &mut |child_ptr| {
            collect_white(child_ptr, garbage);
        });
    }
    garbage.push(ptr);
}

pub fn suspect_count() -> usize {
    crate::suspect_buffer::suspect_count()
}

pub fn clear_suspects() {
    crate::suspect_buffer::clear_suspects();
}

#[cfg(test)]
mod tests {
    use std::cell::RefCell;

    use super::*;
    use crate::{
        managed_rc::{CycleVtable, ManagedRc, ManagedRcInner, typed_dropper},
        suspect_buffer::{
            MAX_THRESHOLD, MIN_THRESHOLD, get_collect_threshold, reset_collect_threshold,
            set_collect_threshold,
        },
    };

    struct Node {
        children: RefCell<Vec<ManagedRc<Node>>>,
    }

    fn node_children(ptr: NonNull<RcHeader>, f: &mut dyn FnMut(NonNull<RcHeader>)) {
        let data = unsafe { &(*ptr.cast::<ManagedRcInner<Node>>().as_ptr()).data };
        for child in data.children.borrow().iter() {
            f(child.header_ptr());
        }
    }

    fn node_clear(ptr: NonNull<RcHeader>) {
        let data = unsafe { &mut (*ptr.cast::<ManagedRcInner<Node>>().as_ptr()).data };
        let children = std::mem::take(&mut *data.children.borrow_mut());
        for child in children {
            std::mem::forget(child);
        }
    }

    static TEST_VTABLE: CycleVtable = CycleVtable {
        type_name: "Node",
        children: node_children,
        clear_cycle_fields: node_clear,
        dropper: typed_dropper::<Node>,
        buffer_on_decrement: true,
    };

    fn make_node(children: Vec<ManagedRc<Node>>) -> ManagedRc<Node> {
        ManagedRc::new_with_vtable(
            Node {
                children: RefCell::new(children),
            },
            &TEST_VTABLE,
        )
    }

    #[test]
    fn collect_simple_cycle() {
        clear_suspects();
        reset_collect_threshold();

        let a = make_node(vec![]);
        let b = make_node(vec![]);

        a.children.borrow_mut().push(b.clone());
        b.children.borrow_mut().push(a.clone());

        drop(a);
        drop(b);

        assert_eq!(suspect_count(), 2);
        collect_cycles();
        assert_eq!(suspect_count(), 0);
    }

    #[test]
    fn no_collect_reachable_node() {
        clear_suspects();
        reset_collect_threshold();

        let a = make_node(vec![]);
        let a2 = a.clone();
        drop(a); // a: strong 2->1, Purple, buffered

        assert_eq!(suspect_count(), 1);
        collect_cycles();

        // a is still reachable via a2 — scan_black restores it
        assert_eq!(a2.strong_count(), 1);
        assert_eq!(suspect_count(), 0);

        clear_suspects();
    }

    #[test]
    fn auto_trigger_fires() {
        clear_suspects();
        reset_collect_threshold();

        let mut handles = vec![];
        for _ in 0..256 {
            let a = make_node(vec![]);
            let b = a.clone();
            handles.push(b); // keep allocation alive
            drop(a);
        }

        // After auto-trigger all suspects are processed (reachable via handles), buffer empty.
        assert_eq!(suspect_count(), 0);
    }

    #[test]
    fn collect_non_suspect_white_node() {
        clear_suspects();
        reset_collect_threshold();

        // Cycle: a -> inner -> a. inner is never in the suspect buffer.
        let a = make_node(vec![]);

        // Create inner pointing back to a. a.strong: 1->2.
        let inner = ManagedRc::new_with_vtable(
            Node {
                children: RefCell::new(vec![a.clone()]),
            },
            &TEST_VTABLE,
        );

        // Move inner into a's children. inner.strong stays 1 (moved, not cloned).
        a.children.borrow_mut().push(inner);
        // a.strong=2 (local + inner.children[0]), inner.strong=1 (a.children[0])

        // Drop only external handle. a.strong: 2->1, buffered.
        // inner was never at strong > 1 when any Drop ran, so NOT buffered.
        drop(a);

        assert_eq!(suspect_count(), 1); // only a is a suspect
        collect_cycles();
        assert_eq!(suspect_count(), 0);
        // Both a AND inner freed. Before the fix, inner would have leaked.
    }

    #[test]
    fn threshold_grows_on_ineffective_collection() {
        clear_suspects();
        reset_collect_threshold();

        // 256 reachable suspects: clone kept alive, original dropped
        let mut handles = vec![];
        for _ in 0..256 {
            let a = make_node(vec![]);
            let b = a.clone();
            handles.push(b);
            drop(a); // strong 2->1, buffered
        }
        // 256th push triggers collection; all suspects reachable -> freed = 0
        // effectiveness = 0.0 < 0.1 -> threshold grows: 256 * 3/2 = 384
        assert_eq!(suspect_count(), 0);
        assert_eq!(get_collect_threshold(), 384);

        reset_collect_threshold();
    }

    #[test]
    fn threshold_shrinks_on_effective_collection() {
        clear_suspects();
        // Start above MIN so shrinking is observable
        set_collect_threshold(512);

        // 256 garbage cycles = 512 suspects, all White -> freed = 512
        for _ in 0..256 {
            let a = make_node(vec![]);
            let b = make_node(vec![]);
            a.children.borrow_mut().push(b.clone());
            b.children.borrow_mut().push(a.clone());
            drop(a); // strong 2->1, buffered
            drop(b); // strong 2->1, buffered
        }
        // 512th push triggers collection; effectiveness = 1.0 >= 0.5
        // threshold: max(512 * 2/3, 256) = max(341, 256) = 341
        assert_eq!(suspect_count(), 0);
        assert_eq!(get_collect_threshold(), 341);

        reset_collect_threshold();
    }

    #[test]
    fn threshold_respects_bounds() {
        // Lower bound: effective collection at MIN_THRESHOLD must not shrink below it
        clear_suspects();
        reset_collect_threshold(); // starts at MIN_THRESHOLD (256)

        for _ in 0..128 {
            let a = make_node(vec![]);
            let b = make_node(vec![]);
            a.children.borrow_mut().push(b.clone());
            b.children.borrow_mut().push(a.clone());
            drop(a);
            drop(b);
        }
        // 128 cycles = 256 suspects, all garbage, effectiveness = 1.0
        // 256 * 2/3 = 170 -> clamped to max(170, MIN_THRESHOLD) = 256
        assert_eq!(get_collect_threshold(), MIN_THRESHOLD);

        // Upper bound: ineffective collection that would exceed MAX_THRESHOLD is clamped
        // 2732 reachable suspects -> threshold: min(2732 * 3/2, 4096) = min(4098, 4096) = 4096
        set_collect_threshold(2732);
        let mut handles = vec![];
        for _ in 0..2732 {
            let a = make_node(vec![]);
            let b = a.clone();
            handles.push(b);
            drop(a);
        }
        assert_eq!(get_collect_threshold(), MAX_THRESHOLD);

        reset_collect_threshold();
    }
}
