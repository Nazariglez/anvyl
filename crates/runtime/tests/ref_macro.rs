#![allow(dead_code)]

use anvyx_runtime::{AnvyxRef, ExternRep, ExternTypeExpr};

#[derive(AnvyxRef)]
#[anvyx(name = "Counter")]
struct HostCounter {
    #[anvyx(field)]
    count: i64,
}

fn assert_ref<T: anvyx_runtime::AnvyxRefExport>() {}

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
