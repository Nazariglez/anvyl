use super::helpers::{assert_named_path, first_import, parse_program_err};
use crate::ast::{self, ImportRoot, PackageModulePath};

#[test]
fn parses_dependency_root_import() {
    let imp = first_import("import dep:math;");

    assert!(matches!(&imp.target.root, ImportRoot::Dependency(alias) if alias.as_str() == "math"));
    assert_eq!(imp.target.path, PackageModulePath::Root);
    assert_eq!(imp.kind, ast::ImportKind::Module);
}

#[test]
fn parses_dependency_member_import() {
    let imp = first_import("import dep:math.vec { Vec2 as V, self as vec };");

    assert!(matches!(&imp.target.root, ImportRoot::Dependency(alias) if alias.as_str() == "math"));
    assert_named_path(&imp.target.path, &["vec"]);
    let ast::ImportKind::Selective(items) = imp.kind else {
        panic!("expected selective import");
    };
    assert_eq!(items.len(), 2);
}

#[test]
fn parses_public_dependency_selective_import() {
    let imp = first_import("pub import dep:math { self as m, Vec2 as V };");

    assert_eq!(imp.visibility, ast::Visibility::Public);
    assert!(matches!(&imp.target.root, ImportRoot::Dependency(alias) if alias.as_str() == "math"));
    assert_eq!(imp.target.path, PackageModulePath::Root);
    let ast::ImportKind::Selective(items) = imp.kind else {
        panic!("expected selective import");
    };
    assert_eq!(items.len(), 2);
}

#[test]
fn parses_native_provider_import() {
    let imp = first_import("pub import ext:core { __assert, __print as print_impl };");

    assert_eq!(imp.visibility, ast::Visibility::Public);
    assert_eq!(imp.target.root, ImportRoot::NativeProvider);
    assert_named_path(&imp.target.path, &["core"]);
    assert!(matches!(imp.kind, ast::ImportKind::Selective(_)));
}

#[test]
fn parses_native_provider_wildcard_reexport() {
    let imp = first_import("pub import ext:core { * };");

    assert_eq!(imp.visibility, ast::Visibility::Public);
    assert_eq!(imp.target.root, ImportRoot::NativeProvider);
    assert_named_path(&imp.target.path, &["core"]);
    assert_eq!(imp.kind, ast::ImportKind::Wildcard);
}

#[test]
fn parses_std_root_import() {
    let imp = first_import("import std;");

    assert_eq!(imp.target.root, ImportRoot::Std);
    assert_eq!(imp.target.path, PackageModulePath::Root);
    assert_eq!(imp.kind, ast::ImportKind::Module);
}

#[test]
fn parses_std_module_import() {
    let imp = first_import("import std.math { PI };");

    assert_eq!(imp.target.root, ImportRoot::Std);
    assert_named_path(&imp.target.path, &["math"]);
    assert!(matches!(imp.kind, ast::ImportKind::Selective(_)));
}

#[test]
fn rejects_empty_dependency_import_root() {
    parse_program_err("import dep: { Vec2 };");
}

#[test]
fn rejects_empty_native_provider_import_root() {
    parse_program_err("import ext: { __assert };");
}

#[test]
fn rejects_empty_native_provider_import() {
    parse_program_err("import ext:;");
}

#[test]
fn rejects_accidental_colon_after_local_root() {
    parse_program_err("import foo:bar;");
}

#[test]
fn rejects_package_root_in_type_paths() {
    parse_program_err("fn take_vec(v: dep:math.Vec2) {}");
}

#[test]
fn rejects_package_root_in_expression_paths() {
    parse_program_err("fn main() { dep:math.make_vec(); }");
}
