use super::helpers::{parse_program, parse_program_err};
use crate::ast::{self, Mutability, Type, Visibility};

fn first_global(src: &str) -> ast::GlobalDeclNode {
    let prog = parse_program(src);
    let ast::Stmt::Global(global) = &prog.stmts[0].node else {
        panic!("expected Global, found {:?}", prog.stmts[0].node);
    };
    global.clone()
}

#[test]
fn parses_lazy_let() {
    let global = first_global("lazy let Config: int = load_config();");

    assert_eq!(global.node.name.as_str(), "Config");
    assert_eq!(global.node.visibility, Visibility::Private);
    assert_eq!(global.node.mutability, Mutability::Immutable);
    assert_eq!(global.node.ty, Some(Type::Int));
}

#[test]
fn parses_pub_lazy_var_with_metadata() {
    let global = first_global(
        "@deprecated(\"use Audio2\")\n/// Runtime audio.\npub lazy var Audio = AudioManager.new();",
    );

    assert_eq!(global.node.name.as_str(), "Audio");
    assert_eq!(global.node.visibility, Visibility::Public);
    assert_eq!(global.node.mutability, Mutability::Mutable);
    assert_eq!(global.node.doc.as_deref(), Some("Runtime audio."));
    assert_eq!(global.node.annotations.len(), 1);
    assert_eq!(global.node.annotations[0].node.name.as_str(), "deprecated");
    assert!(global.node.ty.is_none());
}

#[test]
fn rejects_wrong_lazy_shapes() {
    parse_program_err("lazy pub let Bad = init();");
    parse_program_err("lazy const Bad = 1;");
    parse_program_err("fn f() { lazy let Bad = init(); }");
    parse_program_err("let Bad = init();");
    parse_program_err("var Bad = init();");
}
