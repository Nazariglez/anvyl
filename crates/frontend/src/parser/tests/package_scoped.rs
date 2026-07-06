use super::helpers::{assert_named_path, first_import, parse_program_err};
use crate::ast::{self, ImportRoot};

#[test]
fn parses_native_provider_import() {
    let imp = first_import("pub import ext:core { __assert, __print as print_impl };");

    assert_eq!(imp.visibility, ast::Visibility::Public);
    assert_eq!(imp.target.root, ImportRoot::NativeProvider);
    assert_named_path(&imp.target.path, &["core"]);
    assert!(matches!(imp.kind, ast::ImportKind::Selective(_)));
}

#[test]
fn parses_native_provider_primitive_module() {
    let imp = first_import("import ext:core_int { int_abs };");

    assert_eq!(imp.target.root, ImportRoot::NativeProvider);
    assert_named_path(&imp.target.path, &["core_int"]);
}

#[test]
fn rejects_native_provider_keyword_module() {
    parse_program_err("import ext:int { int_abs };");
}

#[test]
fn parses_native_provider_wildcard_reexport() {
    let imp = first_import("pub import ext:core { * };");

    assert_eq!(imp.visibility, ast::Visibility::Public);
    assert_eq!(imp.target.root, ImportRoot::NativeProvider);
    assert_named_path(&imp.target.path, &["core"]);
    assert_eq!(imp.kind, ast::ImportKind::Wildcard);
}
