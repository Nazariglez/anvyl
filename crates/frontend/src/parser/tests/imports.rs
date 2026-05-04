use super::helpers::{assert_local_import, first_import, parse_program, parse_program_err};
use crate::ast::{self, ImportItemKind, ImportKind};

fn ident_str(ident: &ast::Ident) -> &str {
    ident.0.as_ref()
}

fn selective_items(src: &str) -> Vec<ast::ImportItem> {
    let imp = first_import(src);
    let ImportKind::Selective(items) = imp.kind else {
        panic!("expected Selective, found {:?}", imp.kind);
    };
    items
}

#[test]
fn single_seg() {
    let imp = first_import("import foo;");
    assert_local_import(&imp, 0, &["foo"]);
    assert_eq!(imp.kind, ImportKind::Module);
}

#[test]
fn explicit_current_folder() {
    let imp = first_import("import .helper;");
    assert_local_import(&imp, 0, &["helper"]);
    assert_eq!(imp.kind, ImportKind::Module);
}

#[test]
fn parent_folder() {
    let imp = first_import("import ..common;");
    assert_local_import(&imp, 1, &["common"]);
    assert_eq!(imp.kind, ImportKind::Module);
}

#[test]
fn grandparent_selective() {
    let imp = first_import("import ...shared.types { Id };");
    assert_local_import(&imp, 2, &["shared", "types"]);
    assert!(matches!(imp.kind, ImportKind::Selective(_)));
}

#[test]
fn deeper_parent_folder() {
    let imp = first_import("import ....too.high;");
    assert_local_import(&imp, 3, &["too", "high"]);
    assert_eq!(imp.kind, ImportKind::Module);
}

#[test]
fn two_seg() {
    let imp = first_import("import foo.bar;");
    assert_local_import(&imp, 0, &["foo", "bar"]);
    assert_eq!(imp.kind, ImportKind::Module);
}

#[test]
fn three_seg() {
    let imp = first_import("import foo.bar.baz;");
    assert_local_import(&imp, 0, &["foo", "bar", "baz"]);
    assert_eq!(imp.kind, ImportKind::Module);
}

#[test]
fn single_seg_alias() {
    let imp = first_import("import foo as f;");
    assert_local_import(&imp, 0, &["foo"]);
    let ImportKind::ModuleAs(alias) = &imp.kind else {
        panic!("expected ModuleAs, found {:?}", imp.kind);
    };
    assert_eq!(ident_str(alias), "f");
}

#[test]
fn multi_seg_alias() {
    let imp = first_import("import foo.bar as b;");
    assert_local_import(&imp, 0, &["foo", "bar"]);
    let ImportKind::ModuleAs(alias) = &imp.kind else {
        panic!("expected ModuleAs, found {:?}", imp.kind);
    };
    assert_eq!(ident_str(alias), "b");
}

#[test]
fn selective_single() {
    let items = selective_items("import foo { bar };");
    assert_eq!(items.len(), 1);
    let ImportItemKind::Name(name) = &items[0].kind else {
        panic!("expected Name, found {:?}", items[0].kind);
    };
    assert_eq!(ident_str(name), "bar");
    assert!(items[0].alias.is_none());
}

#[test]
fn selective_multi() {
    let items = selective_items("import foo { bar, baz };");
    assert_eq!(items.len(), 2);
    assert!(matches!(&items[0].kind, ImportItemKind::Name(n) if ident_str(n) == "bar"));
    assert!(matches!(&items[1].kind, ImportItemKind::Name(n) if ident_str(n) == "baz"));
}

#[test]
fn selective_trailing() {
    let items = selective_items("import foo { bar, baz, };");
    assert_eq!(items.len(), 2);
}

#[test]
fn selective_alias() {
    let items = selective_items("import foo { bar as b };");
    assert_eq!(items.len(), 1);
    let ImportItemKind::Name(name) = &items[0].kind else {
        panic!("expected Name, found {:?}", items[0].kind);
    };
    assert_eq!(ident_str(name), "bar");
    let alias = items[0].alias.as_ref().expect("expected alias");
    assert_eq!(ident_str(alias), "b");
}

#[test]
fn selective_mixed() {
    let items = selective_items("import foo { bar, baz as z };");
    assert_eq!(items.len(), 2);
    assert!(matches!(&items[0].kind, ImportItemKind::Name(n) if ident_str(n) == "bar"));
    assert!(items[0].alias.is_none());
    assert!(matches!(&items[1].kind, ImportItemKind::Name(n) if ident_str(n) == "baz"));
    assert_eq!(ident_str(items[1].alias.as_ref().unwrap()), "z");
}

#[test]
fn selective_self() {
    let items = selective_items("import foo { self };");
    assert_eq!(items.len(), 1);
    assert!(matches!(&items[0].kind, ImportItemKind::SelfModule));
    assert!(items[0].alias.is_none());
}

#[test]
fn selective_self_alias() {
    let items = selective_items("import foo { self as bar };");
    assert_eq!(items.len(), 1);
    assert!(matches!(&items[0].kind, ImportItemKind::SelfModule));
    let alias = items[0].alias.as_ref().expect("expected alias");
    assert_eq!(ident_str(alias), "bar");
}

#[test]
fn selective_self_with_member() {
    let items = selective_items("import foo { self, bar };");
    assert_eq!(items.len(), 2);
    assert!(matches!(&items[0].kind, ImportItemKind::SelfModule));
    assert!(matches!(&items[1].kind, ImportItemKind::Name(n) if ident_str(n) == "bar"));
}

#[test]
fn selective_self_trailing() {
    let items = selective_items("import foo { self, bar, };");
    assert_eq!(items.len(), 2);
}

#[test]
fn wildcard() {
    let imp = first_import("import foo { * };");
    assert_local_import(&imp, 0, &["foo"]);
    assert_eq!(imp.kind, ImportKind::Wildcard);
}

#[test]
fn multi_import() {
    let prog = parse_program("import foo;\nimport bar.baz;\nimport qux { * };\nfn main() {}");
    assert_eq!(prog.stmts.len(), 4);
    assert!(matches!(prog.stmts[0].node, ast::Stmt::Import(_)));
    assert!(matches!(prog.stmts[1].node, ast::Stmt::Import(_)));
    assert!(matches!(prog.stmts[2].node, ast::Stmt::Import(_)));
    assert!(matches!(prog.stmts[3].node, ast::Stmt::Func(_)));
}

#[test]
fn err_missing_path() {
    parse_program_err("import;");
}

#[test]
fn err_trailing_dot() {
    parse_program_err("import foo.;");
}

#[test]
fn err_empty_braces() {
    parse_program_err("import foo { };");
}

#[test]
fn err_interior_range() {
    parse_program_err("import my..mod;");
    parse_program_err("import my.mod..file;");
}
