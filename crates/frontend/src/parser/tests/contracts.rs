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
fn contract_requirement_return_type() {
    let contract = first_contract("contract Named { fn name(self) -> string; }");
    let sig = &contract.requirements[0].node.sig;
    assert_eq!(sig.ret, ast::Type::String);
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
fn contract_rejects_annotations() {
    parse_program_err("@deprecated contract C { fn f(self); }");
}
