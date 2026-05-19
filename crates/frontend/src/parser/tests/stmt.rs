use super::helpers::{parse_expr, parse_program, parse_program_err};
use crate::ast::{
    self, BinaryOp, ExternReceiverMode, ExternTypeRep, MethodReceiver, Mutability, NominalKind,
    Type,
};

#[test]
fn while_let() {
    let prog = parse_program("fn main() { while let Option.Some(x) = get() {} }");
    assert_eq!(prog.stmts.len(), 1);
    let ast::Stmt::Func(func_node) = &prog.stmts[0].node else {
        panic!("expected Func");
    };
    let body_stmts = &func_node.node.body.node.stmts;
    assert_eq!(body_stmts.len(), 1);
    let ast::Stmt::WhileLet(while_let_node) = &body_stmts[0].node else {
        panic!("expected WhileLet");
    };
    assert!(matches!(while_let_node.node.head, ast::PatternHead::Let));
    assert!(matches!(
        &while_let_node.node.pattern.node,
        ast::Pattern::EnumTuple { .. }
    ));
}

#[test]
fn while_var() {
    let prog = parse_program("fn main() { while var x? = opt {} }");
    let ast::Stmt::Func(func_node) = &prog.stmts[0].node else {
        panic!("expected Func");
    };
    let ast::Stmt::WhileLet(while_let_node) = &func_node.node.body.node.stmts[0].node else {
        panic!("expected WhileLet");
    };
    assert!(matches!(while_let_node.node.head, ast::PatternHead::Var));
}

#[test]
fn let_else_var() {
    let prog = parse_program("fn main() { var x? = opt else { return; } }");
    let ast::Stmt::Func(func_node) = &prog.stmts[0].node else {
        panic!("expected Func");
    };
    let ast::Stmt::LetElse(let_else_node) = &func_node.node.body.node.stmts[0].node else {
        panic!("expected LetElse");
    };
    assert!(matches!(let_else_node.node.head, ast::PatternHead::Var));
}

#[test]
fn func_var_return() {
    let prog = parse_program("fn id(var x: int) -> var int { x }");
    let ast::Stmt::Func(func_node) = &prog.stmts[0].node else {
        panic!("expected Func");
    };
    assert_eq!(func_node.node.ret.access, ast::ReturnAccess::Place);
    assert_eq!(func_node.node.ret.ty, Type::Int);
}

#[test]
fn if_var() {
    let expr = parse_expr("if var x? = opt {}");
    let ast::ExprKind::IfLet(if_let_node) = &expr.node.kind else {
        panic!("expected IfLet");
    };
    assert!(matches!(if_let_node.node.head, ast::PatternHead::Var));
}

#[test]
fn match_var() {
    let expr = parse_expr("match var opt { Option.Some(x) => x, Option.None => 0 }");
    let ast::ExprKind::Match(match_node) = &expr.node.kind else {
        panic!("expected Match");
    };
    assert!(matches!(match_node.node.head, ast::PatternHead::Var));
}

#[test]
fn match_let_head_rejected() {
    parse_program_err("fn main() { let _ = match let opt { _ => 0 }; }");
}

fn first_for(src: &str) -> ast::ForNode {
    let prog = parse_program(src);
    let ast::Stmt::Func(func) = &prog.stmts[0].node else {
        panic!("expected Func");
    };
    let ast::Stmt::For(for_node) = &func.node.body.node.stmts[0].node else {
        panic!("expected For stmt");
    };
    (**for_node).clone()
}

fn ident_bindings(for_node: &ast::ForNode) -> Vec<(bool, &str)> {
    for_node
        .node
        .bindings
        .iter()
        .map(|binding| {
            let ast::Pattern::Ident(ident) = &binding.pattern.node else {
                panic!("expected Ident pattern");
            };
            (binding.mutable, ident.as_str())
        })
        .collect()
}

#[test]
fn while_after_let() {
    let prog = parse_program("fn main() { while true {} }");
    assert_eq!(prog.stmts.len(), 1);
    let ast::Stmt::Func(func_node) = &prog.stmts[0].node else {
        panic!("expected Func");
    };
    let body_stmts = &func_node.node.body.node.stmts;
    assert_eq!(body_stmts.len(), 1);
    assert!(matches!(&body_stmts[0].node, ast::Stmt::While(_)));
}

#[test]
fn while_binary_cond() {
    let prog = parse_program("fn main() { while x < 3 {} }");
    assert_eq!(prog.stmts.len(), 1);
    let ast::Stmt::Func(func_node) = &prog.stmts[0].node else {
        panic!("expected Func");
    };
    let body_stmts = &func_node.node.body.node.stmts;
    assert_eq!(body_stmts.len(), 1);
    let ast::Stmt::While(while_node) = &body_stmts[0].node else {
        panic!("expected While");
    };
    let cond = &while_node.node.cond;
    match &cond.node.kind {
        ast::ExprKind::Binary(bin) => {
            assert_eq!(bin.node.op, BinaryOp::LessThan);
        }
        other => panic!("expected Binary cond, found {other:?}"),
    }
}

#[test]
fn while_ident_cond() {
    let prog = parse_program("fn main() { while x {} }");
    assert_eq!(prog.stmts.len(), 1);
    let ast::Stmt::Func(func_node) = &prog.stmts[0].node else {
        panic!("expected Func");
    };
    let body_stmts = &func_node.node.body.node.stmts;
    assert_eq!(body_stmts.len(), 1);
    let ast::Stmt::While(while_node) = &body_stmts[0].node else {
        panic!("expected While");
    };
    let cond = &while_node.node.cond;
    match &cond.node.kind {
        ast::ExprKind::Ident(ident) => {
            assert_eq!(ident.0.as_ref(), "x");
        }
        other => panic!("expected Ident cond, found {other:?}"),
    }
}

#[test]
fn if_ident_cond() {
    let prog = parse_program("fn main() { if x {} }");
    assert_eq!(prog.stmts.len(), 1);
    let ast::Stmt::Func(func_node) = &prog.stmts[0].node else {
        panic!("expected Func");
    };
    let body = &func_node.node.body.node;
    assert_eq!(body.stmts.len(), 0);
    let Some(expr_node) = &body.tail else {
        panic!("expected tail expr");
    };
    let ast::ExprKind::If(if_node) = &expr_node.node.kind else {
        panic!("expected If expr");
    };
    let cond = &if_node.node.cond;
    match &cond.node.kind {
        ast::ExprKind::Ident(ident) => {
            assert_eq!(ident.0.as_ref(), "x");
        }
        other => panic!("expected Ident cond, found {other:?}"),
    }
}

#[test]
fn while_break_assign() {
    let src = r"
        fn main() {
            var i: int = 0;
            while true {
                if i == 3 {
                    break;
                }
                i = i + 1;
            }
        }
    ";
    let prog = parse_program(src);
    assert_eq!(prog.stmts.len(), 1);
    let ast::Stmt::Func(func_node) = &prog.stmts[0].node else {
        panic!("expected Func");
    };
    let body_stmts = &func_node.node.body.node.stmts;
    assert_eq!(body_stmts.len(), 2);

    let ast::Stmt::Binding(_) = &body_stmts[0].node else {
        panic!("expected Binding stmt");
    };

    let ast::Stmt::While(while_node) = &body_stmts[1].node else {
        panic!("expected While stmt");
    };
    let while_body = &while_node.node.body.node.stmts;
    assert_eq!(while_body.len(), 2);

    let ast::Stmt::Expr(if_expr_node) = &while_body[0].node else {
        panic!("expected Expr stmt for if");
    };
    assert!(matches!(&if_expr_node.node.kind, ast::ExprKind::If(_)));

    let ast::Stmt::Expr(assign_expr_node) = &while_body[1].node else {
        panic!("expected Expr stmt for assignment");
    };
    assert!(matches!(
        &assign_expr_node.node.kind,
        ast::ExprKind::Assign(_)
    ));
}

#[test]
fn for_range() {
    let prog = parse_program("fn main() { for n in 0..10 {} }");
    assert_eq!(prog.stmts.len(), 1);
    let ast::Stmt::Func(func_node) = &prog.stmts[0].node else {
        panic!("expected Func");
    };
    let body_stmts = &func_node.node.body.node.stmts;
    assert_eq!(body_stmts.len(), 1);

    let ast::Stmt::For(for_node) = &body_stmts[0].node else {
        panic!("expected For stmt");
    };
    let for_inner = &for_node.node;
    assert_eq!(ident_bindings(for_node), [(false, "n")]);
    assert!(!for_inner.reversed);
    assert!(for_inner.step.is_none());

    let ast::ExprKind::Range(range_node) = &for_inner.iterable.node.kind else {
        panic!("expected Range iterable");
    };
    let ast::Range::Bounded { inclusive, .. } = range_node.node() else {
        panic!("expected bounded range");
    };
    assert!(!inclusive);
}

#[test]
fn for_var_item() {
    let for_node = first_for("fn main() { for var x in xs {} }");
    assert_eq!(ident_bindings(&for_node), [(true, "x")]);
}

#[test]
fn for_var_tuple_pattern() {
    let for_node = first_for("fn main() { for var (a, b) in xs {} }");
    let [binding] = for_node.node.bindings.as_slice() else {
        panic!("expected one for binding");
    };
    assert!(binding.mutable);
    assert!(matches!(binding.pattern.node, ast::Pattern::Tuple(_)));
}

#[test]
fn for_indexed_var_segment() {
    let for_node = first_for("fn main() { for i, var x in xs {} }");
    assert_eq!(ident_bindings(&for_node), [(false, "i"), (true, "x")]);
}

#[test]
fn for_let_rejected() {
    parse_program_err("fn main() { for let x in xs {} }");
}

#[test]
fn for_rev_step() {
    let prog = parse_program("fn main() { for n in rev 0..10 step 2 {} }");
    assert_eq!(prog.stmts.len(), 1);
    let ast::Stmt::Func(func_node) = &prog.stmts[0].node else {
        panic!("expected Func");
    };
    let body_stmts = &func_node.node.body.node.stmts;
    assert_eq!(body_stmts.len(), 1);

    let ast::Stmt::For(for_node) = &body_stmts[0].node else {
        panic!("expected For stmt");
    };
    let for_inner = &for_node.node;

    assert!(for_inner.reversed);
    assert!(for_inner.step.is_some());

    let step_expr = for_inner.step.as_ref().unwrap();
    let ast::ExprKind::Lit(ast::Lit::Int(2)) = &step_expr.node.kind else {
        panic!("expected Int(2) step");
    };
}

#[test]
fn for_range_inclusive() {
    let prog = parse_program("fn main() { for n in 0..=10 {} }");
    assert_eq!(prog.stmts.len(), 1);
    let ast::Stmt::Func(func_node) = &prog.stmts[0].node else {
        panic!("expected Func");
    };
    let body_stmts = &func_node.node.body.node.stmts;
    assert_eq!(body_stmts.len(), 1);

    let ast::Stmt::For(for_node) = &body_stmts[0].node else {
        panic!("expected For stmt");
    };
    let for_inner = &for_node.node;

    let ast::ExprKind::Range(range_node) = &for_inner.iterable.node.kind else {
        panic!("expected Range iterable");
    };
    let ast::Range::Bounded { inclusive, .. } = range_node.node() else {
        panic!("expected bounded range");
    };
    assert!(inclusive);
}

#[test]
fn for_ident_empty_body() {
    let prog = parse_program("fn main() { for x in xs {} }");
    assert_eq!(prog.stmts.len(), 1);
    let ast::Stmt::Func(func_node) = &prog.stmts[0].node else {
        panic!("expected Func");
    };
    let body_stmts = &func_node.node.body.node.stmts;
    assert_eq!(body_stmts.len(), 1);

    let ast::Stmt::For(for_node) = &body_stmts[0].node else {
        panic!("expected For stmt");
    };
    let for_inner = &for_node.node;

    let ast::ExprKind::Ident(ident) = &for_inner.iterable.node.kind else {
        panic!(
            "expected Ident iterable, found {:?}",
            for_inner.iterable.node.kind
        );
    };
    assert_eq!(ident.0.as_ref(), "xs");
    assert!(for_inner.body.node.stmts.is_empty());
    assert!(for_inner.body.node.tail.is_none());
}

#[test]
fn for_paren_struct_iter() {
    let prog = parse_program("fn main() { for x in (Foo {}) {} }");
    assert_eq!(prog.stmts.len(), 1);
    let ast::Stmt::Func(func_node) = &prog.stmts[0].node else {
        panic!("expected Func");
    };
    let body_stmts = &func_node.node.body.node.stmts;
    assert_eq!(body_stmts.len(), 1);

    let ast::Stmt::For(for_node) = &body_stmts[0].node else {
        panic!("expected For stmt");
    };
    let for_inner = &for_node.node;

    let ast::ExprKind::StructLiteral(_) = &for_inner.iterable.node.kind else {
        panic!(
            "expected StructLiteral iterable, found {:?}",
            for_inner.iterable.node.kind
        );
    };
    assert!(for_inner.body.node.stmts.is_empty());
}

#[test]
fn for_unparen_struct_iter_err() {
    parse_program_err("fn main() { for x in Foo { x: 1 } {} }");
}

#[test]
fn for_paren_struct_step() {
    let prog = parse_program("fn main() { for x in xs step (Foo {}) {} }");
    assert_eq!(prog.stmts.len(), 1);
    let ast::Stmt::Func(func_node) = &prog.stmts[0].node else {
        panic!("expected Func");
    };
    let body_stmts = &func_node.node.body.node.stmts;
    assert_eq!(body_stmts.len(), 1);

    let ast::Stmt::For(for_node) = &body_stmts[0].node else {
        panic!("expected For stmt");
    };
    let for_inner = &for_node.node;

    let step = for_inner.step.as_ref().expect("expected step");
    let ast::ExprKind::StructLiteral(_) = &step.node.kind else {
        panic!("expected StructLiteral step, found {:?}", step.node.kind);
    };
    assert!(for_inner.body.node.stmts.is_empty());
}

#[test]
fn for_unparen_struct_step_err() {
    parse_program_err("fn main() { for x in xs step Foo { x: 1 } {} }");
}

#[test]
fn var_param() {
    let prog = parse_program("fn f(var x: int) {}");
    assert_eq!(prog.stmts.len(), 1);
    let ast::Stmt::Func(func_node) = &prog.stmts[0].node else {
        panic!("expected Func");
    };
    let params = &func_node.node.params;
    assert_eq!(params.len(), 1);
    assert_eq!(params[0].mutability, Mutability::Mutable);
    assert_eq!(params[0].name.0.as_ref(), "x");
}

#[test]
fn mixed_params() {
    let prog = parse_program("fn f(a: int, var b: int) {}");
    assert_eq!(prog.stmts.len(), 1);
    let ast::Stmt::Func(func_node) = &prog.stmts[0].node else {
        panic!("expected Func");
    };
    let params = &func_node.node.params;
    assert_eq!(params.len(), 2);
    assert_eq!(params[0].mutability, Mutability::Immutable);
    assert_eq!(params[0].name.0.as_ref(), "a");
    assert_eq!(params[1].mutability, Mutability::Mutable);
    assert_eq!(params[1].name.0.as_ref(), "b");
}

#[test]
fn trailing_comma_params() {
    let prog = parse_program("fn f(a: int, var b: int,) {}");
    assert_eq!(prog.stmts.len(), 1);
    let ast::Stmt::Func(func_node) = &prog.stmts[0].node else {
        panic!("expected Func");
    };
    let params = &func_node.node.params;
    assert_eq!(params.len(), 2);
    assert_eq!(params[0].name.0.as_ref(), "a");
    assert_eq!(params[1].name.0.as_ref(), "b");
    assert_eq!(params[1].mutability, Mutability::Mutable);
}

fn parse_aggregate(source: &str) -> ast::StructDecl {
    let mut prog = parse_program(source);
    assert_eq!(prog.stmts.len(), 1);
    let ast::Stmt::Aggregate(node) = prog.stmts.pop().unwrap().node else {
        panic!("expected Aggregate");
    };
    node.node
}

fn parse_aggregate_method(source: &str) -> ast::Method {
    let aggregate = parse_aggregate(source);
    assert_eq!(aggregate.methods.len(), 1);
    aggregate.methods.into_iter().next().unwrap()
}

fn parse_extend_method(source: &str) -> ast::ExtendMethod {
    let mut prog = parse_program(source);
    assert_eq!(prog.stmts.len(), 1);
    let ast::Stmt::Extend(extend_node) = prog.stmts.pop().unwrap().node else {
        panic!("expected Extend");
    };
    assert_eq!(extend_node.node.methods.len(), 1);
    extend_node.node.methods.into_iter().next().unwrap().node
}

fn parse_extend(source: &str) -> ast::ExtendDecl {
    let mut prog = parse_program(source);
    assert_eq!(prog.stmts.len(), 1);
    let ast::Stmt::Extend(extend_node) = prog.stmts.pop().unwrap().node else {
        panic!("expected Extend");
    };
    extend_node.node
}

#[test]
fn embed_field() {
    let aggregate = parse_aggregate("struct S { embed transform: Transform }");
    let field = &aggregate.fields[0];
    assert_eq!(field.name.0.as_ref(), "transform");
    assert!(matches!(
        field.embed,
        Some(ast::EmbedSpec { selector: None })
    ));
}

#[test]
fn embed_selector_and_default() {
    let aggregate = parse_aggregate(
        "struct S { embed health: Health { hp, max_hp as max, fn damage as hit } = Health {} }",
    );
    let field = &aggregate.fields[0];
    let selector = field
        .embed
        .as_ref()
        .and_then(|embed| embed.selector.as_ref())
        .expect("missing selector");
    assert_eq!(selector.items.len(), 3);
    assert_eq!(selector.items[0].kind, ast::EmbedSelectorKind::Field);
    assert_eq!(selector.items[0].name.0.as_ref(), "hp");
    assert_eq!(selector.items[1].alias.unwrap().0.as_ref(), "max");
    assert_eq!(selector.items[2].kind, ast::EmbedSelectorKind::Method);
    assert_eq!(selector.items[2].alias.unwrap().0.as_ref(), "hit");
    assert!(field.default.is_some());
}

#[test]
fn dataref_embed_field() {
    let aggregate = parse_aggregate("dataref S { embed body: Body }");
    assert_eq!(aggregate.kind, ast::AggregateKind::DataRef);
    assert!(aggregate.fields[0].embed.is_some());
}

#[test]
fn enum_payload_rejects_embed() {
    parse_program_err("enum E { V { embed body: Body } }");
}

#[test]
fn empty_embed_selector_fails() {
    parse_program_err("struct S { embed body: Body {} }");
}

#[test]
fn selector_without_embed_fails() {
    parse_program_err("struct S { body: Body { hp } }");
}

#[test]
fn embed_is_identifier_outside_field_prefix() {
    parse_program("fn main() { let embed = 1; embed; }");
}

#[test]
fn as_annotation_parses() {
    let aggregate = parse_aggregate("struct S { @as\n embed body: Body }");
    let annotation = &aggregate.fields[0].annotations[0].node;
    assert_eq!(annotation.name.0.as_ref(), "as");
    assert_eq!(annotation.args, ast::AnnotationArgs::None);
}

#[test]
fn non_as_keyword_annotation_fails() {
    parse_program_err("struct S { @fn field: int }");
}

#[test]
fn as_parameter_syntax_parses() {
    let prog = parse_program("fn f(x: as int) {}");
    let ast::Stmt::Func(func) = &prog.stmts[0].node else {
        panic!("expected Func");
    };
    assert!(func.node.params[0].cast_accept);
}

#[test]
fn value_self() {
    let method = parse_aggregate_method("struct S { fn m(self) {} }");
    assert_eq!(method.sig.receiver, Some(MethodReceiver::Value));
    assert_eq!(method.sig.name.0.as_ref(), "m");
}

#[test]
fn var_self() {
    let method = parse_aggregate_method("struct S { fn m(var self) {} }");
    assert_eq!(method.sig.receiver, Some(MethodReceiver::Var));
    assert_eq!(method.sig.name.0.as_ref(), "m");
}

#[test]
fn aggregate_static_method() {
    let method = parse_aggregate_method("struct S { fn make() -> S { S {} } }");
    assert_eq!(method.sig.receiver, None);
    assert!(method.sig.params.is_empty());
}

#[test]
fn method_trailing_comma() {
    let prog = parse_program("struct S { fn m(var self, x: int,) {} }");
    assert_eq!(prog.stmts.len(), 1);
    let ast::Stmt::Aggregate(struct_node) = &prog.stmts[0].node else {
        panic!("expected Aggregate");
    };
    let methods = &struct_node.node.methods;
    assert_eq!(methods.len(), 1);
    assert_eq!(methods[0].sig.receiver, Some(MethodReceiver::Var));
    assert_eq!(methods[0].sig.params.len(), 1);
    assert_eq!(methods[0].sig.params[0].name.0.as_ref(), "x");
    assert_eq!(methods[0].sig.params[0].ty, Type::Int);
}

#[test]
fn method_self_trailing_comma() {
    let prog = parse_program("struct S { fn m(self,) {} }");
    assert_eq!(prog.stmts.len(), 1);
    let ast::Stmt::Aggregate(struct_node) = &prog.stmts[0].node else {
        panic!("expected Aggregate");
    };
    let methods = &struct_node.node.methods;
    assert_eq!(methods.len(), 1);
    assert_eq!(methods[0].sig.receiver, Some(MethodReceiver::Value));
    assert!(methods[0].sig.params.is_empty());
}

#[test]
fn aggregate_invalid_receivers_fail() {
    for source in [
        "struct S { fn m(self: Self) {} }",
        "struct S { fn m(var self: Self) {} }",
        "struct S { fn m(shared self) {} }",
        "struct S { fn m(x: int, self) {} }",
        "struct S { fn m(x: int, var self) {} }",
        "struct S { fn m(x: int, self: Self) {} }",
        "struct S { fn m(self: int) {} }",
    ] {
        parse_program_err(source);
    }
}

#[test]
fn extend_value_self() {
    let method = parse_extend_method("extend int { fn abs(self) -> int { self } }");
    assert_eq!(method.sig.receiver, Some(MethodReceiver::Value));
    assert!(method.sig.params.is_empty());
}

#[test]
fn extend_var_self() {
    let method = parse_extend_method("extend int { fn reset(var self) {} }");
    assert_eq!(method.sig.receiver, Some(MethodReceiver::Var));
    assert!(method.sig.params.is_empty());
}

#[test]
fn extend_static_method() {
    let method = parse_extend_method("extend Point { fn make() -> int { 1 } }");
    assert_eq!(method.sig.receiver, None);
    assert!(method.sig.params.is_empty());
}

#[test]
fn extend_slice_target() {
    let extend = parse_extend("extend<T> slice[T] { fn keep(self, value: T) -> T { value } }");
    assert_eq!(extend.type_params.len(), 1);
    let Type::Slice { elem } = extend.ty else {
        panic!("expected slice target");
    };
    assert!(matches!(*elem, Type::UnresolvedNominal { .. }));
}

#[test]
fn extend_invalid_receivers_fail() {
    for source in [
        "extend int { fn m(self: int) {} }",
        "extend int { fn m(var self: int) {} }",
        "extend int { fn m(shared self) {} }",
        "extend int { fn m(x: int, self) {} }",
    ] {
        parse_program_err(source);
    }
}

mod externs {
    use super::*;

    fn parse_extern_func(src: &str) -> ast::ExternFuncNode {
        let mut prog = parse_program(src);
        assert_eq!(prog.stmts.len(), 1);
        let ast::Stmt::ExternFunc(node) = prog.stmts.pop().unwrap().node else {
            panic!("expected ExternFunc");
        };
        node
    }

    fn parse_extern_type(src: &str) -> ast::ExternTypeNode {
        let mut prog = parse_program(src);
        assert_eq!(prog.stmts.len(), 1);
        let ast::Stmt::ExternType(node) = prog.stmts.pop().unwrap().node else {
            panic!("expected ExternType");
        };
        node
    }

    #[test]
    fn fn_no_params() {
        let node = parse_extern_func("extern fn tick() -> void;");
        assert_eq!(node.node.name.0.as_ref(), "tick");
        assert_eq!(node.node.params.len(), 0);
        assert_eq!(node.node.ret.ty, Type::Void);
    }

    #[test]
    fn fn_params() {
        let node = parse_extern_func("extern fn add(a: int, b: int) -> int;");
        let ef = &node.node;
        assert_eq!(ef.name.0.as_ref(), "add");
        assert_eq!(ef.params.len(), 2);
        assert_eq!(ef.params[0].name.0.as_ref(), "a");
        assert_eq!(ef.params[0].ty, Type::Int);
        assert_eq!(ef.params[1].name.0.as_ref(), "b");
        assert_eq!(ef.params[1].ty, Type::Int);
        assert_eq!(ef.ret.ty, Type::Int);
    }

    #[test]
    fn fn_void_default() {
        let node = parse_extern_func("extern fn fire();");
        assert_eq!(node.node.ret.ty, Type::Void);
    }

    #[test]
    fn opaque_type() {
        let node = parse_extern_type("extern type Sprite;");
        assert_eq!(node.node.name.0.as_ref(), "Sprite");
        assert!(node.node.members.is_empty());
    }

    #[test]
    fn rep_shared() {
        let node = parse_extern_type("extern type Texture rep shared;");
        assert_eq!(node.node.rep, ExternTypeRep::Shared);
    }

    #[test]
    fn rep_inline() {
        let node = parse_extern_type("extern type Vec2 rep inline { x: float; }");
        assert_eq!(node.node.rep, ExternTypeRep::Inline);
    }

    #[test]
    fn rep_defaults_shared() {
        let node = parse_extern_type("extern type Sprite;");
        assert_eq!(node.node.rep, ExternTypeRep::Shared);
    }

    #[test]
    fn bad_rep() {
        parse_program_err("extern type T rep value;");
    }

    #[test]
    fn init_params() {
        let node = parse_extern_type("extern type Point { init(x: float, y: float); }");
        let init = node.node.init.as_ref().expect("expected init");
        assert_eq!(init.params.len(), 2);
        assert_eq!(init.params[0].name.0.as_ref(), "x");
        assert_eq!(init.params[0].ty, Type::Float);
        assert_eq!(init.params[1].name.0.as_ref(), "y");
        assert_eq!(init.params[1].ty, Type::Float);
    }

    #[test]
    fn init_empty_forms() {
        for src in ["extern type T { init; }", "extern type T { init(); }"] {
            let node = parse_extern_type(src);
            assert!(
                node.node
                    .init
                    .as_ref()
                    .is_some_and(|init| init.params.is_empty())
            );
        }
    }

    #[test]
    fn duplicate_init_fails() {
        parse_program_err("extern type T { init; init(); }");
    }

    #[test]
    fn fields() {
        let node = parse_extern_type(
            r"
        extern type Point {
            x: float;
            y: float;
        }
    ",
        );
        assert_eq!(node.node.name.0.as_ref(), "Point");
        assert_eq!(node.node.members.len(), 2);
        let ast::ExternTypeMember::Field { name, ty, .. } = &node.node.members[0] else {
            panic!("expected Field");
        };
        assert_eq!(name.0.as_ref(), "x");
        assert_eq!(*ty, Type::Float);
        let ast::ExternTypeMember::Field { name, ty, .. } = &node.node.members[1] else {
            panic!("expected Field");
        };
        assert_eq!(name.0.as_ref(), "y");
        assert_eq!(*ty, Type::Float);
    }

    #[test]
    fn field_forms() {
        let node = parse_extern_type(
            r"
        extern type T {
            plain: int;
            computed cached: int;
        }
    ",
        );

        let fields = node
            .node
            .members
            .iter()
            .map(|member| {
                let ast::ExternTypeMember::Field { name, computed, .. } = member else {
                    panic!("expected Field");
                };
                (name.0.as_ref().as_str(), *computed)
            })
            .collect::<Vec<_>>();

        assert_eq!(fields, [("plain", false), ("cached", true)]);
    }

    #[test]
    fn invalid_field_forms_fail() {
        for source in [
            "extern type T { let x: int; }",
            "extern type T { var x: int; }",
            "extern type T { computed var x: int; }",
        ] {
            parse_program_err(source);
        }
    }

    #[test]
    fn static_method() {
        let node = parse_extern_type(
            r"
        extern type Point {
            fn new(x: float, y: float) -> Point;
        }
    ",
        );
        assert_eq!(node.node.members.len(), 1);
        let ast::ExternTypeMember::StaticMethod {
            name, params, ret, ..
        } = &node.node.members[0]
        else {
            panic!("expected StaticMethod");
        };
        assert_eq!(name.0.as_ref(), "new");
        assert_eq!(params.len(), 2);
        let Type::UnresolvedNominal {
            qualifier: None,
            name,
            generic_args,
        } = &ret.ty
        else {
            panic!("expected Point nominal, found {ret:?}");
        };
        let is_point = name.0.as_ref() == "Point" && generic_args.is_empty();
        assert!(is_point);
    }

    #[test]
    fn methods() {
        let node = parse_extern_type(
            r"
        extern type Point {
            fn get_x(self) -> float;
            fn move_by(var self, dx: float, dy: float);
        }
    ",
        );
        assert_eq!(node.node.members.len(), 2);

        let ast::ExternTypeMember::Method {
            name,
            receiver,
            params,
            ret,
            ..
        } = &node.node.members[0]
        else {
            panic!("expected Method");
        };
        assert_eq!(name.0.as_ref(), "get_x");
        assert_eq!(*receiver, ExternReceiverMode::Value);
        assert!(params.is_empty());
        assert_eq!(ret.ty, Type::Float);

        let ast::ExternTypeMember::Method { name, receiver, .. } = &node.node.members[1] else {
            panic!("expected Method");
        };
        assert_eq!(name.0.as_ref(), "move_by");
        assert_eq!(*receiver, ExternReceiverMode::Mutable);
    }

    #[test]
    fn shared_receiver() {
        let node = parse_extern_type("extern type Vec2 { fn length(shared self) -> float; }");
        let ast::ExternTypeMember::Method { receiver, ret, .. } = &node.node.members[0] else {
            panic!("expected Method");
        };
        assert_eq!(*receiver, ExternReceiverMode::Shared);
        assert_eq!(ret.ty, Type::Float);
    }

    #[test]
    fn shared_self_is_extern_only() {
        parse_program_err("struct Vec2 { fn length(shared self) -> float {} }");
    }

    #[test]
    fn typed_self_receiver_fails() {
        for source in [
            "extern type T { fn f(self: int) -> void; }",
            "extern type T { fn f(var self: int) -> void; }",
            "extern type T { fn f(self, self: int) -> void; }",
        ] {
            parse_program_err(source);
        }
    }

    #[test]
    fn unsupported_operator_fails() {
        parse_program_err("extern type T { op Self && Self -> bool; }");
    }

    #[test]
    fn self_return() {
        let node = parse_extern_type(
            r"
        extern type Point {
            fn new(x: float, y: float) -> Self;
        }
    ",
        );
        let ast::ExternTypeMember::StaticMethod { ret, .. } = &node.node.members[0] else {
            panic!("expected StaticMethod");
        };
        assert_eq!(
            ret.ty,
            Type::nominal(
                NominalKind::Extern,
                ast::Ident(internment::Intern::new("Point".to_string())),
                vec![],
                vec![],
                None,
            )
        );
    }

    #[test]
    fn comparison_operators() {
        let node = parse_extern_type(
            r"
        extern type T {
            op Self != Self -> bool;
            op Self < Self -> bool;
            op Self > Self -> bool;
            op Self <= Self -> bool;
            op Self >= Self -> bool;
        }
    ",
        );
        let ops = node
            .node
            .members
            .iter()
            .map(|member| {
                let ast::ExternTypeMember::Operator { op, .. } = member else {
                    panic!("expected Operator");
                };
                *op
            })
            .collect::<Vec<_>>();
        assert_eq!(
            ops,
            [
                BinaryOp::NotEq,
                BinaryOp::LessThan,
                BinaryOp::GreaterThan,
                BinaryOp::LessThanEq,
                BinaryOp::GreaterThanEq,
            ]
        );
    }

    #[test]
    fn mixed_members() {
        let node = parse_extern_type(
            r"
        extern type Point {
            x: float;
            y: float;
            fn new(x: float, y: float) -> Self;
            fn move_by(var self, dx: float, dy: float);
            fn distance_to(self, other: Point) -> float;
        }
    ",
        );
        assert_eq!(node.node.members.len(), 5);
        assert!(matches!(
            &node.node.members[0],
            ast::ExternTypeMember::Field { .. }
        ));
        assert!(matches!(
            &node.node.members[1],
            ast::ExternTypeMember::Field { .. }
        ));
        assert!(matches!(
            &node.node.members[2],
            ast::ExternTypeMember::StaticMethod { .. }
        ));
        assert!(matches!(
            &node.node.members[3],
            ast::ExternTypeMember::Method { .. }
        ));
        assert!(matches!(
            &node.node.members[4],
            ast::ExternTypeMember::Method { .. }
        ));
    }

    #[test]
    fn empty_type() {
        let node = parse_extern_type("extern type Foo {}");
        assert_eq!(node.node.name.0.as_ref(), "Foo");
        assert!(node.node.members.is_empty());
    }

    #[test]
    fn type_and_fn() {
        let prog = parse_program("extern type Sprite;\nextern fn create() -> Sprite;");
        assert_eq!(prog.stmts.len(), 2);
        assert!(matches!(prog.stmts[0].node, ast::Stmt::ExternType(_)));
        assert!(matches!(prog.stmts[1].node, ast::Stmt::ExternFunc(_)));
    }
}

#[test]
fn index_assign() {
    use super::helpers::{expect_ident, expect_index, expect_int};
    let prog = parse_program("fn main() { var a = [1, 2, 3]; a[0] = 5; }");
    let ast::Stmt::Func(func_node) = &prog.stmts[0].node else {
        panic!("expected Func");
    };
    let body = &func_node.node.body.node.stmts;
    assert_eq!(body.len(), 2);

    let ast::Stmt::Expr(assign_expr) = &body[1].node else {
        panic!("expected Expr stmt for assignment");
    };
    let ast::ExprKind::Assign(assign_node) = &assign_expr.node.kind else {
        panic!("expected Assign expr");
    };
    let (target, index) = expect_index(&assign_node.node.target, false);
    expect_ident(target, "a");
    expect_int(index, 0);
}

#[test]
fn field_idx_assign() {
    use super::helpers::{expect_field, expect_index, expect_int};
    let prog = parse_program("fn main() { a.x[0] = 5; }");
    let ast::Stmt::Func(func_node) = &prog.stmts[0].node else {
        panic!("expected Func");
    };
    let body = &func_node.node.body.node.stmts;
    assert_eq!(body.len(), 1);

    let ast::Stmt::Expr(assign_expr) = &body[0].node else {
        panic!("expected Expr stmt for assignment");
    };
    let ast::ExprKind::Assign(assign_node) = &assign_expr.node.kind else {
        panic!("expected Assign expr");
    };
    let (field_target, index) = expect_index(&assign_node.node.target, false);
    expect_int(index, 0);
    let base = expect_field(field_target, "x", false);
    match &base.node.kind {
        ast::ExprKind::Ident(ident) => assert_eq!(ident.0.as_ref(), "a"),
        other => panic!("expected Ident 'a', got {other:?}"),
    }
}

#[test]
fn tuple_idx_assign() {
    use super::helpers::{expect_ident, expect_tuple_index};

    let prog = parse_program("fn main() { pair.0 = 5; }");
    let ast::Stmt::Func(func_node) = &prog.stmts[0].node else {
        panic!("expected Func");
    };
    let body = &func_node.node.body.node.stmts;
    assert_eq!(body.len(), 1);

    let ast::Stmt::Expr(assign_expr) = &body[0].node else {
        panic!("expected Expr stmt for assignment");
    };
    let ast::ExprKind::Assign(assign_node) = &assign_expr.node.kind else {
        panic!("expected Assign expr");
    };
    let target = expect_tuple_index(&assign_node.node.target, 0);
    expect_ident(target, "pair");
}

#[test]
fn tuple_idx_compound() {
    use super::helpers::{expect_ident, expect_tuple_index};

    let prog = parse_program("fn main() { pair.1 += 5; }");
    let ast::Stmt::Func(func_node) = &prog.stmts[0].node else {
        panic!("expected Func");
    };
    let body = &func_node.node.body.node.stmts;
    assert_eq!(body.len(), 1);

    let ast::Stmt::Expr(assign_expr) = &body[0].node else {
        panic!("expected Expr stmt for assignment");
    };
    let ast::ExprKind::Assign(assign_node) = &assign_expr.node.kind else {
        panic!("expected Assign expr");
    };
    assert!(matches!(assign_node.node.op, ast::AssignOp::AddAssign));
    let target = expect_tuple_index(&assign_node.node.target, 1);
    expect_ident(target, "pair");
}

#[test]
fn rejects_labelled_tuple_pattern() {
    parse_program_err("fn f(p: (int, int)) { let (a: x, b: y) = p; }");
}

#[test]
fn rejects_single_labelled_tuple_pattern() {
    parse_program_err("fn f(p: int) { let (a: x) = p; }");
}
