use super::helpers::{assert_named_path, first_import, parse_program_err};
use crate::ast::{self, ImportRoot, PackageModulePath};

#[test]
fn parses_package_root_import() {
    let imp = first_import("import pkg:math;");

    assert!(matches!(&imp.target.root, ImportRoot::Package(alias) if alias.as_str() == "math"));
    assert_eq!(imp.target.path, PackageModulePath::Root);
    assert_eq!(imp.kind, ast::ImportKind::Module);
}

#[test]
fn parses_package_member_import() {
    let imp = first_import("import pkg:math.vec { Vec2 as V, self as vec };");

    assert!(matches!(&imp.target.root, ImportRoot::Package(alias) if alias.as_str() == "math"));
    assert_named_path(&imp.target.path, &["vec"]);
    let ast::ImportKind::Selective(items) = imp.kind else {
        panic!("expected selective import");
    };
    assert_eq!(items.len(), 2);
}

#[test]
fn public_pkg_selective() {
    let imp = first_import("pub import pkg:math { self as m, Vec2 as V };");

    assert_eq!(imp.visibility, ast::Visibility::Public);
    assert!(matches!(&imp.target.root, ImportRoot::Package(alias) if alias.as_str() == "math"));
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
fn parses_std_module_import() {
    let imp = first_import("import std:math { PI };");

    assert_eq!(imp.target.root, ImportRoot::Std);
    assert_named_path(&imp.target.path, &["math"]);
    assert!(matches!(imp.kind, ast::ImportKind::Selective(_)));
}

#[test]
fn parses_std_nested_import() {
    let imp = first_import("import std:math.vec { * };");

    assert_eq!(imp.target.root, ImportRoot::Std);
    assert_named_path(&imp.target.path, &["math", "vec"]);
    assert_eq!(imp.kind, ast::ImportKind::Wildcard);
}

#[test]
fn empty_colon_roots() {
    parse_program_err("import pkg:;");
    parse_program_err("import std:;");
    parse_program_err("import ext:;");
}

#[test]
fn empty_colon_selective() {
    parse_program_err("import pkg: { Vec2 };");
    parse_program_err("import std: { PI };");
    parse_program_err("import ext: { __assert };");
}

#[test]
fn old_dep_root() {
    parse_program_err("import dep:math;");
}

#[test]
fn rejects_accidental_colon_after_local_root() {
    parse_program_err("import foo:bar;");
}

#[test]
fn rejects_package_root_in_type_paths() {
    parse_program_err("fn take_vec(v: pkg:math.Vec2) {}");
}

#[test]
fn rejects_package_root_in_expression_paths() {
    parse_program_err("fn main() { pkg:math.make_vec(); }");
}
