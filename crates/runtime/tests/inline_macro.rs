#![allow(dead_code)]

mod support;

use anvyx_runtime::{
    AnvyxEnum, AnvyxInline, ExternLayout, ExternMaterialization, ExternRep, ExternTypeExpr,
};
use support::provider_package::TestCatalog;

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

#[derive(Clone, AnvyxEnum)]
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

struct ManualEnum(i64);

unsafe impl anvyx_runtime::AnvyxEnumExport for ManualEnum {
    const OWNS_ANVYX_HEAP_EDGES: bool = false;
    const __ANVYX_MATERIALIZER: fn(&Self) -> Self = |value| Self(value.0);
}

#[test]
fn enum_export_does_not_require_clone() {
    assert_enum::<ManualEnum>();
    let materialized =
        <ManualEnum as anvyx_runtime::AnvyxEnumExport>::__ANVYX_MATERIALIZER(&ManualEnum(7));
    assert_eq!(materialized.0, 7);
}

#[test]
fn inline_descriptor_contains_exported_fields_docs_and_name() {
    assert_inline::<Vec2>();

    let package = TestCatalog::from_export(__anvyx_export_vec2());
    let (_, export) = package.ty("Point");

    assert_eq!(export.name, "Point");
    assert_eq!(export.doc.as_deref(), Some("Two-dimensional point."));
    assert_eq!(export.rep, ExternRep::Inline);
    assert_eq!(
        export.layout,
        Some(ExternLayout {
            size: size_of::<Vec2>() as u64,
            align: align_of::<Vec2>() as u64,
        })
    );
    assert_eq!(export.materialization, Some(ExternMaterialization::Copy));
    assert_eq!(export.owns_heap_edges, Some(false));
    let copied = __anvyx_native_export_vec2::__anvyx_materialize(&Vec2 {
        x: 1.0,
        y: 2.0,
        hidden: 3,
    });
    assert_eq!((copied.x, copied.y, copied.hidden), (1.0, 2.0, 3));
    assert_eq!(export.fields.len(), 2);
    assert_eq!(export.fields[0].name, "x");
    assert_eq!(export.fields[0].ty, ExternTypeExpr::Float);
    assert_eq!(
        export.fields[0].doc.as_deref(),
        Some("Horizontal component.")
    );
    assert_eq!(export.fields[1].name, "y");
}

#[test]
fn enum_descriptor_contains_name_and_marker() {
    assert_enum::<HostLoadError>();

    let package = TestCatalog::from_export(__anvyx_export_hostloaderror());
    let (_, export) = package.ty("LoadError");

    assert_eq!(export.name, "LoadError");
    assert_eq!(export.rep, ExternRep::Inline);
    assert_eq!(
        export.layout,
        Some(ExternLayout {
            size: size_of::<HostLoadError>() as u64,
            align: align_of::<HostLoadError>() as u64,
        })
    );
    assert_eq!(
        export.materialization,
        Some(ExternMaterialization::Materialize)
    );
    assert_eq!(export.owns_heap_edges, Some(false));
    let value = HostLoadError::Missing("missing".into());
    let materialized = __anvyx_native_export_hostloaderror::__anvyx_materialize(&value);
    assert!(matches!(materialized, HostLoadError::Missing(_)));
    assert!(export.fields.is_empty());
    assert_eq!(export.variants.len(), 2);
    assert_eq!(export.variants[0].name, "Missing");
    assert_eq!(export.variants[0].fields[0].ty, ExternTypeExpr::String);
    assert_eq!(
        export.variants[0].doc.as_deref(),
        Some("Missing asset path.")
    );
    assert_eq!(export.variants[1].name, "Decode");
    assert_eq!(
        export.variants[1].fields[0].name.as_deref(),
        Some("message")
    );
}
