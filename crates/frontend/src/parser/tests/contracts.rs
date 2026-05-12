use super::helpers::{parse_program, parse_program_err};
use crate::ast;

fn first_contract(src: &str) -> ast::ContractDecl {
    let prog = parse_program(src);
    let ast::Stmt::Contract(node) = &prog.stmts[0].node else {
        panic!(
            "expected Contract statement, found {:?}",
            prog.stmts[0].node
        );
    };
    node.node.clone()
}

#[test]
fn contract_decl() {
    let contract = first_contract("contract Updatable { fn update(var self, dt: float); }");
    assert_eq!(contract.name.as_str(), "Updatable");
    assert_eq!(contract.visibility, ast::Visibility::Private);
    assert_eq!(contract.requirements.len(), 1);
    let sig = &contract.requirements[0].node.sig;
    assert_eq!(sig.name.as_str(), "update");
    assert_eq!(sig.receiver, Some(ast::MethodReceiver::Var));
    assert_eq!(sig.params.len(), 1);
    assert_eq!(sig.params[0].name.as_str(), "dt");
    assert_eq!(sig.params[0].ty, ast::Type::Float);
    assert_eq!(sig.ret, ast::Type::Void);
}

#[test]
fn pub_contract_with_doc() {
    let contract = first_contract("/// Draw surface\npub contract Drawable { fn draw(self); }");
    assert_eq!(contract.doc.as_deref(), Some("Draw surface"));
    assert_eq!(contract.visibility, ast::Visibility::Public);
}

#[test]
fn deprecated_contract_annotation() {
    let contract = first_contract("@deprecated(\"use New\") contract Old { fn f(self); }");
    assert_eq!(contract.annotations.len(), 1);
    let annotation = &contract.annotations[0].node;
    assert_eq!(annotation.name.as_str(), "deprecated");
    assert!(
        matches!(annotation.args, ast::AnnotationArgs::Positional(ast::Lit::String(ref reason)) if reason == "use New")
    );
}

#[test]
fn contract_annotation_with_doc_and_pub() {
    let contract =
        first_contract("@deprecated\n/// Draw surface\npub contract Drawable { fn draw(self); }");
    assert_eq!(contract.annotations.len(), 1);
    assert_eq!(contract.doc.as_deref(), Some("Draw surface"));
    assert_eq!(contract.visibility, ast::Visibility::Public);
}

#[test]
fn contract_requirement_return_type() {
    let contract = first_contract("contract Named { fn name(self) -> string; }");
    let sig = &contract.requirements[0].node.sig;
    assert_eq!(sig.ret, ast::Type::String);
}

#[test]
fn contract_inclusions() {
    let contract = first_contract("contract Actor { Updatable; render.Drawable; fn draw(self); }");
    assert_eq!(contract.includes.len(), 2);
    assert_eq!(contract.requirements.len(), 1);
    assert!(matches!(
        &contract.includes[1].node,
        ast::ContractRef::Named { qualifier: Some(q), name, .. }
            if q.as_str() == "render" && name.as_str() == "Drawable"
    ));
}

#[test]
fn generic_contract_bounds() {
    let prog = parse_program("fn run<T: Updatable + Drawable, U, N: int>() {}");
    let ast::Stmt::Func(func) = &prog.stmts[0].node else {
        panic!("expected function");
    };
    assert_eq!(func.node.type_params.len(), 2);
    assert_eq!(func.node.type_params[0].bounds.len(), 2);
    assert!(func.node.type_params[1].bounds.is_empty());
    assert_eq!(func.node.const_params.len(), 1);
}

#[test]
fn contract_rejects_field() {
    parse_program_err("contract HasPosition { position: Vec2; }");
}

#[test]
fn contract_rejects_method_body() {
    parse_program_err("contract Updatable { fn update(self) {} }");
}

#[test]
fn contract_rejects_missing_receiver() {
    parse_program_err("contract Factory { fn make() -> int; }");
}

#[test]
fn contract_rejects_generic_requirement() {
    parse_program_err("contract Mapper { fn map<T>(self, x: T) -> T; }");
}

#[test]
fn contract_rejects_default_param() {
    parse_program_err("contract C { fn f(self, x: int = 0); }");
}

#[test]
fn contract_rejects_empty_body() {
    parse_program_err("contract Empty {} ");
}

#[test]
fn contract_rejects_body_annotations() {
    parse_program_err("contract C { @deprecated fn f(self); }");
}
