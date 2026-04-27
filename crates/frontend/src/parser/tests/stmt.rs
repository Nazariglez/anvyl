use super::helpers::{parse_program, parse_program_err};
use crate::ast::{self, MethodReceiver, Mutability, NominalKind, Type};

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
    assert!(matches!(
        &while_let_node.node.pattern.node,
        ast::Pattern::EnumTuple { .. }
    ));
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
            assert_eq!(bin.node.op, ast::BinaryOp::LessThan);
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

    let ast::Pattern::Ident(ident) = &for_inner.pattern.node else {
        panic!("expected Ident pattern");
    };
    assert_eq!(ident.0.as_ref(), "n");

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

#[test]
fn var_self() {
    let prog = parse_program("struct S { fn m(var self) {} }");
    assert_eq!(prog.stmts.len(), 1);
    let ast::Stmt::Aggregate(struct_node) = &prog.stmts[0].node else {
        panic!("expected Aggregate");
    };
    let methods = &struct_node.node.methods;
    assert_eq!(methods.len(), 1);
    assert_eq!(methods[0].receiver, Some(MethodReceiver::Var));
    assert_eq!(methods[0].name.0.as_ref(), "m");
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
    assert_eq!(methods[0].receiver, Some(MethodReceiver::Var));
    assert_eq!(methods[0].params.len(), 1);
    assert_eq!(methods[0].params[0].name.0.as_ref(), "x");
    assert_eq!(methods[0].params[0].ty, Type::Int);
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
    assert_eq!(methods[0].receiver, Some(MethodReceiver::Value));
    assert!(methods[0].params.is_empty());
}

#[test]
fn extern_fn_no_params() {
    let prog = parse_program("extern fn tick() -> void;");
    assert_eq!(prog.stmts.len(), 1);
    let ast::Stmt::ExternFunc(node) = &prog.stmts[0].node else {
        panic!("expected ExternFunc");
    };
    assert_eq!(node.node.name.0.as_ref(), "tick");
    assert_eq!(node.node.params.len(), 0);
    assert_eq!(node.node.ret, Type::Void);
}

#[test]
fn extern_fn_params() {
    let prog = parse_program("extern fn add(a: int, b: int) -> int;");
    assert_eq!(prog.stmts.len(), 1);
    let ast::Stmt::ExternFunc(node) = &prog.stmts[0].node else {
        panic!("expected ExternFunc");
    };
    let ef = &node.node;
    assert_eq!(ef.name.0.as_ref(), "add");
    assert_eq!(ef.params.len(), 2);
    assert_eq!(ef.params[0].name.0.as_ref(), "a");
    assert_eq!(ef.params[0].ty, Type::Int);
    assert_eq!(ef.params[1].name.0.as_ref(), "b");
    assert_eq!(ef.params[1].ty, Type::Int);
    assert_eq!(ef.ret, Type::Int);
}

#[test]
fn extern_fn_void_default() {
    let prog = parse_program("extern fn fire();");
    assert_eq!(prog.stmts.len(), 1);
    let ast::Stmt::ExternFunc(node) = &prog.stmts[0].node else {
        panic!("expected ExternFunc");
    };
    assert_eq!(node.node.ret, Type::Void);
}

#[test]
fn extern_type() {
    let prog = parse_program("extern type Sprite;");
    assert_eq!(prog.stmts.len(), 1);
    let ast::Stmt::ExternType(node) = &prog.stmts[0].node else {
        panic!("expected ExternType");
    };
    assert_eq!(node.node.name.0.as_ref(), "Sprite");
    assert!(node.node.members.is_empty());
}

#[test]
fn extern_type_fields() {
    let prog = parse_program(
        r"
        extern type Point {
            x: float;
            y: float;
        }
    ",
    );
    assert_eq!(prog.stmts.len(), 1);
    let ast::Stmt::ExternType(node) = &prog.stmts[0].node else {
        panic!("expected ExternType");
    };
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
fn extern_type_static() {
    let prog = parse_program(
        r"
        extern type Point {
            fn new(x: float, y: float) -> Point;
        }
    ",
    );
    let ast::Stmt::ExternType(node) = &prog.stmts[0].node else {
        panic!("expected ExternType");
    };
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
    } = ret
    else {
        panic!("expected Point nominal, found {ret:?}");
    };
    let is_point = name.0.as_ref() == "Point" && generic_args.is_empty();
    assert!(is_point);
}

#[test]
fn extern_type_methods() {
    let prog = parse_program(
        r"
        extern type Point {
            fn get_x(self) -> float;
            fn move_by(var self, dx: float, dy: float);
        }
    ",
    );
    let ast::Stmt::ExternType(node) = &prog.stmts[0].node else {
        panic!("expected ExternType");
    };
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
    assert_eq!(*receiver, MethodReceiver::Value);
    assert!(params.is_empty());
    assert_eq!(*ret, Type::Float);

    let ast::ExternTypeMember::Method { name, receiver, .. } = &node.node.members[1] else {
        panic!("expected Method");
    };
    assert_eq!(name.0.as_ref(), "move_by");
    assert_eq!(*receiver, MethodReceiver::Var);
}

#[test]
fn extern_type_self_return() {
    let prog = parse_program(
        r"
        extern type Point {
            fn new(x: float, y: float) -> Self;
        }
    ",
    );
    let ast::Stmt::ExternType(node) = &prog.stmts[0].node else {
        panic!("expected ExternType");
    };
    let ast::ExternTypeMember::StaticMethod { ret, .. } = &node.node.members[0] else {
        panic!("expected StaticMethod");
    };
    assert_eq!(
        *ret,
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
fn extern_type_full() {
    let prog = parse_program(
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
    let ast::Stmt::ExternType(node) = &prog.stmts[0].node else {
        panic!("expected ExternType");
    };
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
fn extern_type_empty() {
    let prog = parse_program("extern type Foo {}");
    let ast::Stmt::ExternType(node) = &prog.stmts[0].node else {
        panic!("expected ExternType");
    };
    assert_eq!(node.node.name.0.as_ref(), "Foo");
    assert!(node.node.members.is_empty());
}

#[test]
fn extern_fn_post_refactor() {
    let prog = parse_program("extern fn add(a: int, b: int) -> int;");
    assert_eq!(prog.stmts.len(), 1);
    let ast::Stmt::ExternFunc(node) = &prog.stmts[0].node else {
        panic!("expected ExternFunc");
    };
    let ef = &node.node;
    assert_eq!(ef.name.0.as_ref(), "add");
    assert_eq!(ef.params.len(), 2);
    assert_eq!(ef.ret, Type::Int);
}

#[test]
fn extern_type_fn_mixed() {
    let prog = parse_program("extern type Sprite;\nextern fn create() -> Sprite;");
    assert_eq!(prog.stmts.len(), 2);
    assert!(matches!(prog.stmts[0].node, ast::Stmt::ExternType(_)));
    assert!(matches!(prog.stmts[1].node, ast::Stmt::ExternFunc(_)));
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
