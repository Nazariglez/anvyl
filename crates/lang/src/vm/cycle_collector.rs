use std::ptr::NonNull;

use anvyx_runtime::legacy_gc::{CycleVtable, ManagedRcInner, RcHeader, typed_dropper};
#[cfg(test)]
pub use anvyx_runtime::legacy_gc::{clear_suspects, reset_collect_threshold, suspect_count};
pub use anvyx_runtime::legacy_gc::{collect_cycles, set_auto_collect};

use super::value::{StructData, Value};

pub fn make_dataref_vtable(type_name: &str, cycle_capable: bool) -> &'static CycleVtable {
    Box::leak(Box::new(CycleVtable {
        type_name: Box::leak(type_name.to_owned().into_boxed_str()),
        children: struct_data_children,
        clear_cycle_fields: struct_data_clear_cycle_fields,
        dropper: typed_dropper::<StructData>,
        buffer_on_decrement: cycle_capable,
    }))
}

fn struct_data_children(ptr: NonNull<RcHeader>, f: &mut dyn FnMut(NonNull<RcHeader>)) {
    let data = unsafe { &(*ptr.cast::<ManagedRcInner<StructData>>().as_ptr()).data };
    for field in &data.fields {
        visit_value_for_datarefs(field, f);
    }
}

fn struct_data_clear_cycle_fields(ptr: NonNull<RcHeader>) {
    let data = unsafe { &mut (*ptr.cast::<ManagedRcInner<StructData>>().as_ptr()).data };
    for field in &mut data.fields {
        neutralize_datarefs(field);
    }
}

fn neutralize_datarefs(val: &mut Value) {
    match val {
        Value::DataRef(_) => {
            let old = std::mem::replace(val, Value::Nil);
            if let Value::DataRef(rc) = old {
                std::mem::forget(rc);
            }
        }
        Value::List(list) | Value::Array(list) | Value::Tuple(list) => {
            for item in list.force_mut().iter_mut() {
                neutralize_datarefs(item);
            }
        }
        Value::Enum(e) => {
            for field in &mut e.force_mut().fields {
                neutralize_datarefs(field);
            }
        }
        Value::Map(map) => {
            for (mut k, mut v) in map.force_mut().drain() {
                neutralize_datarefs(&mut k);
                neutralize_datarefs(&mut v);
            }
        }
        Value::Struct(s) => {
            for field in &mut s.force_mut().fields {
                neutralize_datarefs(field);
            }
        }
        Value::Closure(c) => {
            for capture in &mut c.force_mut().captures {
                neutralize_datarefs(capture);
            }
        }
        _ => {}
    }
}

fn visit_value_for_datarefs(val: &Value, f: &mut dyn FnMut(NonNull<RcHeader>)) {
    match val {
        Value::DataRef(rc) => f(rc.header_ptr()),
        Value::List(list) | Value::Array(list) | Value::Tuple(list) => {
            for item in list.iter() {
                visit_value_for_datarefs(item, f);
            }
        }
        Value::Enum(e) => {
            for field in &e.fields {
                visit_value_for_datarefs(field, f);
            }
        }
        Value::Map(map) => {
            for (k, v) in map.iter() {
                visit_value_for_datarefs(k, f);
                visit_value_for_datarefs(v, f);
            }
        }
        Value::Struct(s) => {
            for field in &s.fields {
                visit_value_for_datarefs(field, f);
            }
        }
        Value::Closure(c) => {
            for capture in &c.captures {
                visit_value_for_datarefs(capture, f);
            }
        }
        _ => {}
    }
}

#[cfg(test)]
mod tests {
    use super::{super::managed_rc::ManagedRc, *};

    fn test_vtable() -> &'static CycleVtable {
        make_dataref_vtable("TestNode", true)
    }

    fn make_data(type_id: u32, fields: Vec<Value>) -> ManagedRc<StructData> {
        ManagedRc::new_with_vtable(StructData { type_id, fields }, test_vtable())
    }

    #[test]
    fn collect_simple_cycle() {
        clear_suspects();
        reset_collect_threshold();

        let a = make_data(0, vec![Value::Nil]);
        let b = make_data(0, vec![Value::Nil]);

        let mut a_mut = a.clone();
        a_mut.force_mut().fields[0] = Value::DataRef(b.clone());
        drop(a_mut);

        let mut b_mut = b.clone();
        b_mut.force_mut().fields[0] = Value::DataRef(a.clone());
        drop(b_mut);

        // a.strong=2, b.strong=2 — normal drop triggers buffering via vtable
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

        let a = make_data(0, vec![]);
        let a2 = a.clone(); // a.strong=2
        drop(a); // strong 2 -> 1, buffered

        assert_eq!(suspect_count(), 1);
        collect_cycles();

        assert_eq!(a2.strong_count(), 1);
        assert_eq!(suspect_count(), 0);
    }

    #[test]
    fn auto_trigger_fires() {
        clear_suspects();
        reset_collect_threshold();

        let mut handles = vec![];
        for _ in 0..256 {
            let a = make_data(0, vec![]);
            let b = a.clone();
            handles.push(b);
            drop(a); // strong 2 -> 1, buffered
        }

        assert_eq!(suspect_count(), 0);
    }

    #[test]
    fn collect_non_suspect_white_node() {
        clear_suspects();
        reset_collect_threshold();

        // cycle: a -> inner -> a. inner is never in the suspect buffer.
        let a = make_data(0, vec![Value::Nil]);

        // create inner pointing back to a. a.strong: 1 -> 2.
        let inner = ManagedRc::new_with_vtable(
            StructData {
                type_id: 0,
                fields: vec![Value::DataRef(a.clone())],
            },
            test_vtable(),
        );

        // move inner into a's field (no clone, inner.strong stays 1).
        {
            let mut a_mut = a.clone(); // a.strong: 2 -> 3
            a_mut.force_mut().fields[0] = Value::DataRef(inner);
            drop(a_mut); // a.strong: 3 -> 2 (not buffered: already Purple? no, first decrement from 3 -> 2)
        }
        // a.strong=2 (local + inner.fields[0]), inner.strong=1 (a.fields[0])

        drop(a);
        // a.strong: 2 -> 1, buffered. inner never at strong>1 -> NOT buffered.
        assert_eq!(suspect_count(), 1);

        collect_cycles();
        assert_eq!(suspect_count(), 0);
        // both a and inner freed via collect_white.
    }

    #[test]
    fn collect_cycle_through_list() {
        clear_suspects();
        reset_collect_threshold();

        let a = make_data(0, vec![Value::Nil]);
        let b = make_data(0, vec![Value::Nil]);

        {
            let mut a_mut = a.clone();
            a_mut.force_mut().fields[0] =
                Value::List(ManagedRc::new(vec![Value::DataRef(b.clone())]));
            drop(a_mut);
        }
        {
            let mut b_mut = b.clone();
            b_mut.force_mut().fields[0] =
                Value::List(ManagedRc::new(vec![Value::DataRef(a.clone())]));
            drop(b_mut);
        }

        drop(a);
        drop(b);

        assert_eq!(suspect_count(), 2);
        collect_cycles();
        assert_eq!(suspect_count(), 0);
    }

    #[test]
    fn collect_cycle_through_map() {
        use super::super::value::MapStorage;

        clear_suspects();
        reset_collect_threshold();

        let a = make_data(0, vec![Value::Nil]);
        let b = make_data(0, vec![Value::Nil]);

        {
            let mut map = MapStorage::new_unordered();
            map.insert(Value::Int(0), Value::DataRef(b.clone()));
            let mut a_mut = a.clone();
            a_mut.force_mut().fields[0] = Value::Map(ManagedRc::new(map));
            drop(a_mut);
        }
        {
            let mut map = MapStorage::new_unordered();
            map.insert(Value::Int(0), Value::DataRef(a.clone()));
            let mut b_mut = b.clone();
            b_mut.force_mut().fields[0] = Value::Map(ManagedRc::new(map));
            drop(b_mut);
        }

        drop(a);
        drop(b);

        assert_eq!(suspect_count(), 2);
        collect_cycles();
        assert_eq!(suspect_count(), 0);
    }
}
