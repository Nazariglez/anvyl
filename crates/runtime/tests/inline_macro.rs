#![allow(dead_code)]

use std::any::type_name;

use anvyx_runtime::{
    AnvyxEnum, AnvyxInline, ExternLayout, ExternMaterialization, ExternRep, ExternTypeExpr,
    ModuleExportItem, ModulePath, RustTypeBinding, TypeExport,
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

fn type_binding(export: TypeExport) -> RustTypeBinding {
    ModuleExportItem::rust_type_bindings(
        export,
        ModulePath {
            segments: vec!["host".to_string()],
        },
        "provider",
    )
    .pop()
    .unwrap()
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
    let materialization = export.inline_materialization().unwrap();
    assert_eq!(materialization.mode(), ExternMaterialization::Copy);
    assert_eq!(materialization.rust_type_path(), type_name::<Vec2>());
    assert_eq!(materialization.rust_type_path(), export.rust_type_path());
    assert_eq!(materialization.module(), "__anvyx_native_export_vec2");
    assert_eq!(materialization.symbol(), "__anvyx_materialize");
    let binding = type_binding(export.clone());
    let serialized = binding.materializer.unwrap();
    assert_eq!(serialized.mode, ExternMaterialization::Copy);
    assert_eq!(serialized.path.crate_name, "provider");
    assert_eq!(
        serialized.path.segments,
        ["__anvyx_native_export_vec2", "__anvyx_materialize"]
    );
    let copied = __anvyx_native_export_vec2::__anvyx_materialize(&Vec2 {
        x: 1.0,
        y: 2.0,
        hidden: 3,
    });
    assert_eq!((copied.x, copied.y, copied.hidden), (1.0, 2.0, 3));
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
#[should_panic(expected = "unexpected inline materialization evidence")]
fn serialization_revalidates_type_export() {
    let mut export = __anvyx_export_vec2();
    export.descriptor.materialization = None;

    let _ = type_binding(export);
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
        Some(ExternMaterialization::Materialize)
    );
    assert_eq!(export.descriptor.owns_heap_edges, Some(false));
    let materialization = export.inline_materialization().unwrap();
    assert_eq!(materialization.mode(), ExternMaterialization::Materialize);
    assert_eq!(
        materialization.rust_type_path(),
        type_name::<HostLoadError>()
    );
    assert_eq!(materialization.rust_type_path(), export.rust_type_path());
    assert_eq!(
        materialization.module(),
        "__anvyx_native_export_hostloaderror"
    );
    assert_eq!(materialization.symbol(), "__anvyx_materialize");
    let binding = type_binding(export.clone());
    let serialized = binding.materializer.unwrap();
    assert_eq!(serialized.mode, ExternMaterialization::Materialize);
    assert_eq!(
        serialized.path.segments,
        ["__anvyx_native_export_hostloaderror", "__anvyx_materialize",]
    );
    let value = HostLoadError::Missing("missing".into());
    let materialized = __anvyx_native_export_hostloaderror::__anvyx_materialize(&value);
    assert!(matches!(materialized, HostLoadError::Missing(_)));
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
