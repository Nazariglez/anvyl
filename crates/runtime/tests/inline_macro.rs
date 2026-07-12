#![allow(dead_code)]

use anvyx_runtime::{
    AnvyxEnum, AnvyxInline, ExternLayout, ExternMaterialization, ExternRep, ExternTypeExpr,
};

/// Two-dimensional point.
#[derive(Clone, Copy, AnvyxInline)]
#[anvyx(name = "Point")]
pub struct Vec2 {
    /// Horizontal component.
    #[anvyx(field)]
    pub x: f64,
    #[anvyx(field)]
    pub y: f64,
    hidden: i64,
}

#[derive(Clone, PartialEq, Eq, Hash, AnvyxEnum)]
#[anvyx(name = "LoadError")]
pub enum HostLoadError {
    /// Missing asset path.
    Missing(anvyx_runtime::AnvString),
    Decode {
        message: anvyx_runtime::AnvString,
    },
}

fn assert_inline<T: anvyx_runtime::AnvyxInlineExport>() {}

fn assert_enum<T: anvyx_runtime::AnvyxEnumExport>() {}

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
    assert_eq!(
        export.descriptor.layout,
        Some(ExternLayout {
            size: size_of::<Vec2>() as u64,
            align: align_of::<Vec2>() as u64,
        })
    );
    assert_eq!(
        export.descriptor.materialization,
        Some(ExternMaterialization::Copy)
    );
    assert_eq!(export.descriptor.owns_heap_edges, Some(false));
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

#[test]
fn enum_descriptor_contains_name_and_marker() {
    assert_enum::<HostLoadError>();

    let export = __anvyx_export_hostloaderror();

    assert_eq!(export.descriptor.name, "LoadError");
    assert_eq!(export.descriptor.rep, ExternRep::Inline);
    assert_eq!(
        export.descriptor.layout,
        Some(ExternLayout {
            size: size_of::<HostLoadError>() as u64,
            align: align_of::<HostLoadError>() as u64,
        })
    );
    assert_eq!(
        export.descriptor.materialization,
        Some(ExternMaterialization::Clone)
    );
    assert_eq!(export.descriptor.owns_heap_edges, Some(false));
    assert!(export.descriptor.fields.is_empty());
    assert_eq!(export.descriptor.variants.len(), 2);
    assert_eq!(export.descriptor.variants[0].name, "Missing");
    assert_eq!(
        export.descriptor.variants[0].fields[0].ty,
        ExternTypeExpr::String
    );
    assert_eq!(
        export.descriptor.variants[0].doc.as_deref(),
        Some("Missing asset path.")
    );
    assert_eq!(export.descriptor.variants[1].name, "Decode");
    assert_eq!(
        export.descriptor.variants[1].fields[0].name.as_deref(),
        Some("message")
    );
    assert!(export.bindings.is_empty());
}
