#![allow(dead_code)]

use anvyx_runtime::{AnvyxInline, ExternRep, ExternTypeExpr};

/// Two-dimensional point.
#[derive(AnvyxInline)]
#[anvyx(name = "Point")]
struct Vec2 {
    /// Horizontal component.
    #[anvyx(field)]
    x: f64,
    #[anvyx(field)]
    y: f64,
    hidden: i64,
}

fn assert_inline<T: anvyx_runtime::AnvyxInlineExport>() {}

#[test]
fn inline_descriptor_contains_exported_fields_docs_and_name() {
    assert_inline::<Vec2>();

    let export = __anvyx_export_vec2();

    assert_eq!(export.descriptor.name, "Point");
    assert_eq!(
        export.descriptor.doc.as_deref(),
        Some("Two-dimensional point.")
    );
    assert_eq!(export.descriptor.rep, ExternRep::Inline);
    assert_eq!(export.descriptor.fields.len(), 2);
    assert_eq!(export.descriptor.fields[0].name, "x");
    assert_eq!(export.descriptor.fields[0].ty, ExternTypeExpr::Float);
    assert_eq!(
        export.descriptor.fields[0].doc.as_deref(),
        Some("Horizontal component.")
    );
    assert_eq!(export.descriptor.fields[1].name, "y");
    assert!(export.bindings.is_empty());
}
