use super::*;
use crate::{
    ast::Type,
    builtin::Builtin,
    hir,
    hir::{ExprKind, LocalId, StmtKind},
    test_helpers::TestCtx,
};

fn lower_ok(source: &str) -> hir::Program {
    TestCtx::lower_ok(source)
}

fn lower_err(source: &str) -> LowerError {
    TestCtx::lower_err(source)
}

fn find_main(prog: &hir::Program) -> &hir::Func {
    prog.funcs
        .iter()
        .find(|f| f.name.to_string() == "main")
        .expect("main function not found")
}

#[test]
fn empty_main() {
    let prog = lower_ok("fn main() {}");
    let main = find_main(&prog);
    assert_eq!(main.params_len, 0);
    assert_eq!(main.locals.len(), 0);
    assert_eq!(main.body.stmts.len(), 0);
    assert_eq!(main.ret, Type::Void);
}

#[test]
fn let_binding_int() {
    let prog = lower_ok("fn main() { let x = 42; }");
    let main = find_main(&prog);
    assert_eq!(main.locals.len(), 1);
    assert_eq!(main.locals[0].name.unwrap().to_string(), "x");
    assert_eq!(main.locals[0].ty, Type::Int);
    assert_eq!(main.body.stmts.len(), 1);
    let StmtKind::Let {
        local: LocalId(0),
        init,
    } = &main.body.stmts[0].kind
    else {
        panic!("expected Let stmt")
    };
    assert!(matches!(init.kind, ExprKind::Int(42)));
}

#[test]
fn annotated_optional_binding_uses_effective_binding_type() {
    let prog = lower_ok(
        "fn main() { let x: int? = 42; match x { Option.Some(v) => {}, Option.None => {}, } }",
    );
    let main = find_main(&prog);

    assert_eq!(main.locals[0].ty, Type::option_of(Type::Int));

    let StmtKind::Let { init, .. } = &main.body.stmts[0].kind else {
        panic!("expected Let stmt")
    };
    let ExprKind::EnumLiteral {
        type_id,
        variant,
        fields,
    } = &init.kind
    else {
        panic!("expected coerced EnumLiteral, got {:?}", init.kind)
    };
    assert_eq!(*type_id, crate::prelude_enums::OPTION_TYPE_ID);
    assert_eq!(*variant, 1);
    assert_eq!(fields.len(), 1);
    assert!(matches!(fields[0].kind, ExprKind::Int(42)));

    let StmtKind::Match { scrutinee, .. } = &main.body.stmts[1].kind else {
        panic!("expected Match stmt")
    };
    assert_eq!(
        main.locals[scrutinee.0 as usize].ty,
        Type::option_of(Type::Int)
    );
}

#[test]
fn annotated_array_infer_binding_uses_resolved_binding_type() {
    let prog = lower_ok("fn main() { let xs: [int?; _] = [nil, 1]; }");
    let main = find_main(&prog);

    assert_eq!(
        main.locals[0].ty,
        Type::Array {
            elem: Type::option_of(Type::Int).boxed(),
            len: ast::ArrayLen::Fixed(2),
        }
    );
}

#[test]
fn let_binding_binary() {
    let prog = lower_ok("fn main() { let x = 1 + 2; }");
    let main = find_main(&prog);
    let StmtKind::Let { init, .. } = &main.body.stmts[0].kind else {
        panic!("expected Let stmt")
    };
    assert!(matches!(
        init.kind,
        ExprKind::Binary {
            op: ast::BinaryOp::Add,
            ..
        }
    ));
}

#[test]
fn let_binding_unary() {
    let prog = lower_ok("fn main() { let x = -1; }");
    let main = find_main(&prog);
    let StmtKind::Let { init, .. } = &main.body.stmts[0].kind else {
        panic!("expected Let stmt")
    };
    assert!(matches!(
        init.kind,
        ExprKind::Unary {
            op: ast::UnaryOp::Neg,
            ..
        }
    ));
}

#[test]
fn explicit_return_with_value() {
    let prog = lower_ok("fn main() -> int { return 1; }");
    let main = find_main(&prog);
    assert!(matches!(main.body.stmts[0].kind, StmtKind::Return(Some(_))));
}

#[test]
fn explicit_return_void() {
    let prog = lower_ok("fn main() { return; }");
    let main = find_main(&prog);
    assert!(matches!(main.body.stmts[0].kind, StmtKind::Return(None)));
}

#[test]
fn implicit_return_from_if_expr() {
    let prog = lower_ok("fn foo() -> int { if true { 1 } else { 2 } }");
    let foo = prog
        .funcs
        .iter()
        .find(|f| f.name.to_string() == "foo")
        .unwrap();
    let StmtKind::If {
        then_block,
        else_block,
        ..
    } = &foo.body.stmts[0].kind
    else {
        panic!("expected If stmt")
    };
    assert!(matches!(
        then_block.stmts[0].kind,
        StmtKind::Return(Some(_))
    ));
    let else_stmts = &else_block.as_ref().unwrap().stmts;
    assert!(matches!(else_stmts[0].kind, StmtKind::Return(Some(_))));
}

#[test]
fn implicit_return_from_nested_if_expr() {
    let prog = lower_ok("fn foo() -> int { if true { if false { 1 } else { 2 } } else { 3 } }");
    let foo = prog
        .funcs
        .iter()
        .find(|f| f.name.to_string() == "foo")
        .unwrap();
    let StmtKind::If {
        then_block,
        else_block,
        ..
    } = &foo.body.stmts[0].kind
    else {
        panic!("expected outer If")
    };
    let else_stmts = &else_block.as_ref().unwrap().stmts;
    assert!(matches!(else_stmts[0].kind, StmtKind::Return(Some(_))));
    let StmtKind::If {
        then_block: inner_then,
        else_block: inner_else,
        ..
    } = &then_block.stmts[0].kind
    else {
        panic!("expected inner If")
    };
    assert!(matches!(
        inner_then.stmts[0].kind,
        StmtKind::Return(Some(_))
    ));
    let inner_else_stmts = &inner_else.as_ref().unwrap().stmts;
    assert!(matches!(
        inner_else_stmts[0].kind,
        StmtKind::Return(Some(_))
    ));
}

#[test]
fn if_without_else() {
    let prog = lower_ok("fn main() { let x = true; if x {} }");
    let main = find_main(&prog);
    assert_eq!(main.body.stmts.len(), 2);
    assert!(matches!(
        main.body.stmts[1].kind,
        StmtKind::If {
            else_block: None,
            ..
        }
    ));
}

#[test]
fn if_with_else() {
    let prog = lower_ok("fn main() { let x = true; if x {} else {} }");
    let main = find_main(&prog);
    assert_eq!(main.body.stmts.len(), 2);
    assert!(matches!(
        main.body.stmts[1].kind,
        StmtKind::If {
            else_block: Some(_),
            ..
        }
    ));
}

#[test]
fn if_cond_uses_local() {
    let prog = lower_ok("fn main() { let x = true; if x {} }");
    let main = find_main(&prog);
    let StmtKind::If { cond, .. } = &main.body.stmts[1].kind else {
        panic!("expected If stmt")
    };
    assert!(matches!(cond.kind, ExprKind::Local(LocalId(0))));
}

#[test]
fn while_with_break() {
    let prog = lower_ok("fn main() { while true { break; } }");
    let main = find_main(&prog);
    let StmtKind::While { body, .. } = &main.body.stmts[0].kind else {
        panic!("expected While stmt")
    };
    assert!(matches!(body.stmts[0].kind, StmtKind::Break));
}

#[test]
fn while_with_continue() {
    let prog = lower_ok("fn main() { while true { continue; } }");
    let main = find_main(&prog);
    let StmtKind::While { body, .. } = &main.body.stmts[0].kind else {
        panic!("expected While stmt")
    };
    assert!(matches!(body.stmts[0].kind, StmtKind::Continue));
}

#[test]
fn while_cond_is_bool_literal() {
    let prog = lower_ok("fn main() { while true { break; } }");
    let main = find_main(&prog);
    let StmtKind::While { cond, .. } = &main.body.stmts[0].kind else {
        panic!("expected While stmt")
    };
    assert!(matches!(cond.kind, ExprKind::Bool(true)));
}

#[test]
fn direct_function_call() {
    let prog = lower_ok("fn foo() {} fn main() { foo(); }");
    let foo = prog
        .funcs
        .iter()
        .find(|f| f.name.to_string() == "foo")
        .expect("foo");
    let main = find_main(&prog);
    let foo_id = foo.id;
    let StmtKind::Expr(call_expr) = &main.body.stmts[0].kind else {
        panic!("expected Expr stmt")
    };
    assert!(matches!(call_expr.kind, ExprKind::Call { func, .. } if func == foo_id));
}

#[test]
fn function_call_args_are_lowered() {
    let prog = lower_ok("fn add(a: int, b: int) -> int { return a + b; } fn main() { add(1, 2); }");
    let add = prog
        .funcs
        .iter()
        .find(|f| f.name.to_string() == "add")
        .expect("add");
    let main = find_main(&prog);

    assert_eq!(add.params_len, 2);
    assert_eq!(add.locals[0].name.unwrap().to_string(), "a");
    assert_eq!(add.locals[1].name.unwrap().to_string(), "b");

    let StmtKind::Expr(call_expr) = &main.body.stmts[0].kind else {
        panic!("expected Expr stmt")
    };
    let ExprKind::Call { args, .. } = &call_expr.kind else {
        panic!("expected Call expr")
    };
    assert_eq!(args.len(), 2);
    assert!(matches!(args[0].kind, ExprKind::Int(1)));
    assert!(matches!(args[1].kind, ExprKind::Int(2)));
}

#[test]
fn call_builtin_println() {
    let prog = lower_ok(r#"fn main() { println("hi"); }"#);
    let main = find_main(&prog);
    let StmtKind::Expr(expr) = &main.body.stmts[0].kind else {
        panic!("expected Expr stmt")
    };
    assert!(matches!(
        expr.kind,
        ExprKind::CallBuiltin {
            builtin: Builtin::Println,
            ..
        }
    ));
}

#[test]
fn call_builtin_assert() {
    let prog = lower_ok("fn main() { assert(true); }");
    let main = find_main(&prog);
    let StmtKind::Expr(expr) = &main.body.stmts[0].kind else {
        panic!("expected Expr stmt")
    };
    assert!(matches!(
        expr.kind,
        ExprKind::CallBuiltin {
            builtin: Builtin::Assert,
            ..
        }
    ));
}

#[test]
fn call_builtin_assert_msg() {
    let prog = lower_ok(r#"fn main() { assert_msg(true, "ok"); }"#);
    let main = find_main(&prog);
    let StmtKind::Expr(expr) = &main.body.stmts[0].kind else {
        panic!("expected Expr stmt")
    };
    assert!(matches!(
        expr.kind,
        ExprKind::CallBuiltin {
            builtin: Builtin::AssertMsg,
            ..
        }
    ));
}

#[test]
fn variable_reference_resolves_to_local_id() {
    let prog = lower_ok("fn main() { let x = 1; let y = x; }");
    let main = find_main(&prog);
    // x is LocalId(0), y is LocalId(1)
    let StmtKind::Let { init, .. } = &main.body.stmts[1].kind else {
        panic!("expected Let stmt")
    };
    assert!(matches!(init.kind, ExprKind::Local(LocalId(0))));
}

#[test]
fn assignment_emits_assign_stmt() {
    let prog = lower_ok("fn main() { var x = 1; x = 2; }");
    let main = find_main(&prog);
    assert!(matches!(
        main.body.stmts[1].kind,
        StmtKind::Assign {
            local: LocalId(0),
            ..
        }
    ));
}

#[test]
fn multiple_functions_have_distinct_ids() {
    let prog = lower_ok("fn foo() {} fn bar() {} fn main() {}");
    assert!(prog.funcs.len() >= 3);
    let foo = prog
        .funcs
        .iter()
        .find(|f| f.name.to_string() == "foo")
        .expect("foo");
    let bar = prog
        .funcs
        .iter()
        .find(|f| f.name.to_string() == "bar")
        .expect("bar");
    let main = find_main(&prog);
    assert_ne!(foo.id, bar.id);
    assert_ne!(bar.id, main.id);
    assert_ne!(foo.id, main.id);
}

#[test]
fn cross_function_call_resolves_id() {
    let prog = lower_ok("fn foo() {} fn bar() { foo(); } fn main() { bar(); }");
    let foo = prog
        .funcs
        .iter()
        .find(|f| f.name.to_string() == "foo")
        .expect("foo");
    let bar = prog
        .funcs
        .iter()
        .find(|f| f.name.to_string() == "bar")
        .expect("bar");
    let main = find_main(&prog);
    let foo_id = foo.id;
    let bar_id = bar.id;

    let StmtKind::Expr(bar_call) = &bar.body.stmts[0].kind else {
        panic!()
    };
    assert!(matches!(bar_call.kind, ExprKind::Call { func, .. } if func == foo_id));

    let StmtKind::Expr(main_call) = &main.body.stmts[0].kind else {
        panic!()
    };
    assert!(matches!(main_call.kind, ExprKind::Call { func, .. } if func == bar_id));
}

#[test]
fn tail_expr_becomes_implicit_return() {
    let prog = lower_ok("fn answer() -> int { 42 } fn main() { answer(); }");
    let answer = prog
        .funcs
        .iter()
        .find(|f| f.name.to_string() == "answer")
        .expect("answer");
    assert_eq!(answer.body.stmts.len(), 1);
    let StmtKind::Return(Some(expr)) = &answer.body.stmts[0].kind else {
        panic!("expected Return stmt")
    };
    assert!(matches!(expr.kind, ExprKind::Int(42)));
}

#[test]
fn void_tail_expr_becomes_expr_stmt() {
    // A tail expression in a void function becomes Expr, not Return
    let prog = lower_ok(r#"fn main() { println("hi") }"#);
    let main = find_main(&prog);
    assert_eq!(main.body.stmts.len(), 1);
    assert!(matches!(main.body.stmts[0].kind, StmtKind::Expr(_)));
}

#[test]
fn params_have_correct_locals() {
    let prog =
        lower_ok("fn greet(name: string, count: int) -> bool { return count > 0; } fn main() {}");
    let greet = prog
        .funcs
        .iter()
        .find(|f| f.name.to_string() == "greet")
        .expect("greet");
    assert_eq!(greet.params_len, 2);
    assert_eq!(greet.locals.len(), 2);
    assert_eq!(greet.locals[0].name.unwrap().to_string(), "name");
    assert_eq!(greet.locals[0].ty, Type::String);
    assert_eq!(greet.locals[1].name.unwrap().to_string(), "count");
    assert_eq!(greet.locals[1].ty, Type::Int);
}

#[test]
fn inner_block_locals_do_not_leak_to_outer_scope() {
    let prog = lower_ok("fn main() { while true { let inner = 1; break; } let outer = 2; }");
    let main = find_main(&prog);
    assert_eq!(main.locals.len(), 2);
    assert_eq!(main.locals[0].name.unwrap().to_string(), "inner");
    assert_eq!(main.locals[1].name.unwrap().to_string(), "outer");
}

#[test]
fn if_in_stmts_position_is_promoted() {
    let prog = lower_ok("fn main() { if true {} let x = 1; }");
    let main = find_main(&prog);
    assert!(matches!(main.body.stmts[0].kind, StmtKind::If { .. }));
}

#[test]
fn lowers_for_range_to_while() {
    let prog = lower_ok("fn main() { for n in 0..10 {} }");
    let main = find_main(&prog);
    assert_eq!(main.body.stmts.len(), 4);
    assert!(matches!(main.body.stmts[0].kind, StmtKind::Let { .. }));
    assert!(matches!(main.body.stmts[1].kind, StmtKind::Let { .. }));
    assert!(matches!(main.body.stmts[2].kind, StmtKind::Let { .. }));
    assert!(matches!(main.body.stmts[3].kind, StmtKind::While { .. }));
}

#[test]
fn lowers_array_literal() {
    let prog = lower_ok("fn main() { let x = [1, 2, 3]; }");
    let main = find_main(&prog);
    let StmtKind::Let { init, .. } = &main.body.stmts[0].kind else {
        panic!("expected Let stmt")
    };
    assert!(
        matches!(init.kind, ExprKind::ArrayLiteral { .. }),
        "expected ArrayLiteral, got {:?}",
        init.kind
    );
}

#[test]
fn lowers_list_literal() {
    let prog = lower_ok("fn main() { let x: [int] = [1, 2, 3]; }");
    let main = find_main(&prog);
    let StmtKind::Let { init, .. } = &main.body.stmts[0].kind else {
        panic!("expected Let stmt")
    };
    assert!(
        matches!(init.kind, ExprKind::ListLiteral { .. }),
        "expected ListLiteral, got {:?}",
        init.kind
    );
    if let ExprKind::ListLiteral { elements } = &init.kind {
        assert_eq!(elements.len(), 3);
    }
}

#[test]
fn lowers_array_fill() {
    let prog = lower_ok("fn main() { let x = [0; 3]; }");
    let main = find_main(&prog);
    let StmtKind::Let { init, .. } = &main.body.stmts[0].kind else {
        panic!("expected Let stmt")
    };
    assert!(
        matches!(init.kind, ExprKind::ArrayFill { len: 3, .. }),
        "expected ArrayFill {{ len: 3 }}, got {:?}",
        init.kind
    );
}

#[test]
fn lowers_list_fill() {
    let prog = lower_ok("fn main() { let x: [int] = [0; 3]; }");
    let main = find_main(&prog);
    let StmtKind::Let { init, .. } = &main.body.stmts[0].kind else {
        panic!("expected Let stmt")
    };
    assert!(
        matches!(init.kind, ExprKind::ListFill { len: 3, .. }),
        "expected ListFill {{ len: 3 }}, got {:?}",
        init.kind
    );
}

#[test]
fn lowers_index_get() {
    let prog = lower_ok("fn main() { let a = [1, 2]; let x = a[0]; }");
    let main = find_main(&prog);
    let StmtKind::Let { init, .. } = &main.body.stmts[1].kind else {
        panic!("expected Let stmt at index 1")
    };
    assert!(
        matches!(init.kind, ExprKind::IndexGet { .. }),
        "expected IndexGet, got {:?}",
        init.kind
    );
}

#[test]
fn lowers_index_set() {
    let prog = lower_ok("fn main() { var a = [1, 2, 3]; a[0] = 99; }");
    let main = find_main(&prog);
    let second_stmt = &main.body.stmts[1];
    assert!(
        matches!(
            second_stmt.kind,
            StmtKind::SetIndex {
                object: LocalId(0),
                ..
            }
        ),
        "expected SetIndex {{ object: LocalId(0) }}, got {:?}",
        second_stmt.kind
    );
}

#[test]
fn lowers_map_literal() {
    let prog = lower_ok(r#"fn main() { let x = ["a": 1]; }"#);
    let main = find_main(&prog);
    let init = &main.body.stmts[0];
    match &init.kind {
        StmtKind::Let { init, .. } => {
            assert!(
                matches!(init.kind, ExprKind::MapLiteral { .. }),
                "expected MapLiteral, got {:?}",
                init.kind
            );
        }
        other => panic!("expected Let, got {other:?}"),
    }
}

#[test]
fn lowers_empty_map_literal() {
    let prog = lower_ok(r"fn main() { let x: [string: int] = [:]; }");
    let main = find_main(&prog);
    let init = &main.body.stmts[0];
    match &init.kind {
        StmtKind::Let { init, .. } => match &init.kind {
            ExprKind::MapLiteral { entries } => {
                assert!(entries.is_empty(), "expected empty entries");
            }
            other => panic!("expected MapLiteral, got {other:?}"),
        },
        other => panic!("expected Let, got {other:?}"),
    }
}

#[test]
fn tuple_literal_lowers_to_hir() {
    let prog = lower_ok("fn main() { let t = (1, 2); }");
    let main = find_main(&prog);
    let StmtKind::Let { init, .. } = &main.body.stmts[0].kind else {
        panic!("expected Let stmt")
    };
    assert!(
        matches!(init.kind, ExprKind::TupleLiteral { .. }),
        "expected TupleLiteral, got {:?}",
        init.kind
    );
}

#[test]
fn tuple_index_lowers_to_hir() {
    let prog = lower_ok("fn main() { let t = (10, 20); let v = t.0; }");
    let main = find_main(&prog);
    let StmtKind::Let { init, .. } = &main.body.stmts[1].kind else {
        panic!("expected Let stmt")
    };
    assert!(
        matches!(init.kind, ExprKind::TupleIndex { index: 0, .. }),
        "expected TupleIndex(0), got {:?}",
        init.kind
    );
}

#[test]
fn lowers_range_to_struct_literal() {
    let prog = lower_ok("fn main() { let x = 0..10; }");
    let main = find_main(&prog);
    let StmtKind::Let { init, .. } = &main.body.stmts[0].kind else {
        panic!("expected Let stmt")
    };
    assert!(
        matches!(init.kind, ExprKind::StructLiteral { .. }),
        "expected StructLiteral, got {:?}",
        init.kind
    );
}

#[test]
fn string_interp_with_var() {
    let prog = lower_ok(r#"fn main() { let n = 1; let s = f"n = {n}"; }"#);
    let main = find_main(&prog);
    // s = "n = " + n  → Binary(Add, String("n = "), Local(n))
    let StmtKind::Let { init, .. } = &main.body.stmts[1].kind else {
        panic!("expected Let stmt for s")
    };
    assert!(matches!(
        init.kind,
        ExprKind::Binary {
            op: ast::BinaryOp::Add,
            ..
        }
    ));
}

#[test]
fn string_interp_single_expr_only() {
    let prog = lower_ok(r#"fn main() { let x = "hi"; let s = f"{x}"; }"#);
    let main = find_main(&prog);
    // single Expr part -> just the local, no wrapper
    let StmtKind::Let { init, .. } = &main.body.stmts[1].kind else {
        panic!("expected Let stmt for s")
    };
    assert!(matches!(init.kind, ExprKind::Local(_)));
}

#[test]
fn string_interp_multiple_parts() {
    let prog = lower_ok(r#"fn main() { let x = 1; let y = 2; let s = f"a {x} b {y}"; }"#);
    let main = find_main(&prog);
    // "a {x} b {y}" → (("a " + x) + " b ") + y
    let StmtKind::Let { init, .. } = &main.body.stmts[2].kind else {
        panic!("expected Let stmt for s")
    };
    // outermost node is Add
    assert!(matches!(
        init.kind,
        ExprKind::Binary {
            op: ast::BinaryOp::Add,
            ..
        }
    ));
}

#[test]
fn struct_literal_lowers_to_hir() {
    let prog =
        lower_ok("struct Point { x: int, y: int } fn main() { let p = Point { x: 1, y: 2 }; }");
    let main = find_main(&prog);
    let StmtKind::Let { init, .. } = &main.body.stmts[0].kind else {
        panic!("expected Let stmt")
    };
    assert!(
        matches!(init.kind, ExprKind::StructLiteral { .. }),
        "expected StructLiteral, got {:?}",
        init.kind
    );
}

#[test]
fn struct_literal_fields_in_declaration_order() {
    // fields provided in reversed order, lowering must reorder to declaration order
    let prog =
        lower_ok("struct Pair { a: int, b: int } fn main() { let p = Pair { b: 2, a: 1 }; }");
    let main = find_main(&prog);
    let StmtKind::Let { init, .. } = &main.body.stmts[0].kind else {
        panic!("expected Let stmt")
    };
    let ExprKind::StructLiteral { fields, .. } = &init.kind else {
        panic!("expected StructLiteral")
    };
    // declaration order: a=0, b=1; provided b=2,a=1 -> fields[0]=Int(1), fields[1]=Int(2)
    assert!(matches!(fields[0].kind, ExprKind::Int(1)));
    assert!(matches!(fields[1].kind, ExprKind::Int(2)));
}

#[test]
fn struct_literal_has_correct_type_id() {
    let prog = lower_ok(
        "struct A { x: int } struct B { y: int } fn main() { let a = A { x: 1 }; let b = B { y: 2 }; }",
    );
    let main = find_main(&prog);
    let StmtKind::Let { init: init_a, .. } = &main.body.stmts[0].kind else {
        panic!()
    };
    let StmtKind::Let { init: init_b, .. } = &main.body.stmts[1].kind else {
        panic!()
    };
    let ExprKind::StructLiteral { type_id: id_a, .. } = &init_a.kind else {
        panic!()
    };
    let ExprKind::StructLiteral { type_id: id_b, .. } = &init_b.kind else {
        panic!()
    };
    assert_ne!(id_a, id_b, "different structs must have different type_ids");
}

#[test]
fn field_get_lowers_to_hir() {
    let prog = lower_ok(
        "struct Point { x: int, y: int } fn main() { let p = Point { x: 5, y: 10 }; let v = p.x; }",
    );
    let main = find_main(&prog);
    let StmtKind::Let { init, .. } = &main.body.stmts[1].kind else {
        panic!("expected Let stmt")
    };
    assert!(
        matches!(init.kind, ExprKind::FieldGet { index: 0, .. }),
        "expected FieldGet(index=0), got {:?}",
        init.kind
    );
}

#[test]
fn set_field_lowers_to_hir() {
    let prog = lower_ok(
        "struct Point { x: int, y: int } fn main() { var p = Point { x: 1, y: 2 }; p.x = 99; }",
    );
    let main = find_main(&prog);
    assert!(
        matches!(
            main.body.stmts[1].kind,
            StmtKind::SetField { field_index: 0, .. }
        ),
        "expected SetField, got {:?}",
        main.body.stmts[1].kind
    );
}

#[test]
fn coalesce_lowers_to_match() {
    let prog = lower_ok("fn main() { var x: int? = nil; let y = x ?? 0; }");
    let main = find_main(&prog);
    assert!(matches!(main.body.stmts[1].kind, StmtKind::Match { .. }));
}

#[test]
fn lowers_compound_assignment() {
    let prog = lower_ok("fn main() { var x = 1; x += 1; }");
    let main = find_main(&prog);
    assert_eq!(main.body.stmts.len(), 2);
    assert!(matches!(main.body.stmts[0].kind, StmtKind::Let { .. }));
    assert!(
        matches!(
            &main.body.stmts[1].kind,
            StmtKind::Assign {
                value: hir::Expr {
                    kind: ExprKind::Binary {
                        op: ast::BinaryOp::Add,
                        ..
                    },
                    ..
                },
                ..
            }
        ),
        "expected compound assign to desugar to Assign(Binary(Add, ...))"
    );
}

#[test]
fn rejects_non_ident_pattern_in_let() {
    // Tuple destructuring is not supported in HIR v1
    let err = lower_err("fn main() { let (a, b) = (1, 2); }");
    assert!(matches!(err, LowerError::UnsupportedPattern { .. }));
}

#[test]
fn lowers_match_expr() {
    let prog = lower_ok(
        "fn main() { var x: int? = nil; match x { Option.Some(v) => {}, Option.None => {}, } }",
    );
    let main = find_main(&prog);
    // let x, then the match stmt
    assert_eq!(main.body.stmts.len(), 2);
    assert!(matches!(main.body.stmts[1].kind, StmtKind::Match { .. }));
}

// ---- extern fn lowering tests ----

#[test]
fn extern_fn_emits_call_extern_node() {
    let prog = lower_ok("extern fn add(a: int, b: int) -> int;\nfn main() { let x = add(1, 2); }");
    let main = find_main(&prog);
    let StmtKind::Let { init, .. } = &main.body.stmts[0].kind else {
        panic!("expected Let stmt");
    };
    assert!(
        matches!(init.kind, ExprKind::CallExtern { .. }),
        "expected CallExtern, got {:?}",
        init.kind
    );
}

#[test]
fn extern_fn_decl_is_in_hir_program() {
    let prog = lower_ok("extern fn tick();\nextern fn add(a: int, b: int) -> int;\nfn main() {}");
    let tick = prog
        .externs
        .iter()
        .find(|e| e.name.to_string() == "tick")
        .expect("tick extern");
    let add = prog
        .externs
        .iter()
        .find(|e| e.name.to_string() == "add")
        .expect("add extern");
    assert_eq!(tick.params, vec![]);
    assert_eq!(add.params, vec![Type::Int, Type::Int]);
    assert_eq!(add.ret, Type::Int);
}

#[test]
fn extern_fn_call_extern_has_correct_id() {
    let prog = lower_ok("extern fn add(a: int, b: int) -> int;\nfn main() { let x = add(1, 2); }");
    let add_decl = prog
        .externs
        .iter()
        .find(|e| e.name.to_string() == "add")
        .expect("add extern");
    let main = find_main(&prog);
    let StmtKind::Let { init, .. } = &main.body.stmts[0].kind else {
        panic!("expected Let");
    };
    let ExprKind::CallExtern { extern_id, args } = &init.kind else {
        panic!("expected CallExtern");
    };
    assert_eq!(*extern_id, add_decl.id);
    assert_eq!(args.len(), 2);
}

#[test]
fn extern_type_flows_through_hir() {
    let prog = lower_ok(
        "extern type Sprite;\nextern fn create() -> Sprite;\nfn main() { let s = create(); }",
    );
    let main = find_main(&prog);
    let StmtKind::Let { init, .. } = &main.body.stmts[0].kind else {
        panic!("expected Let stmt");
    };
    let ExprKind::CallExtern { .. } = &init.kind else {
        panic!("expected CallExtern, got {:?}", init.kind);
    };
    let Type::Extern { name, .. } = &init.ty else {
        panic!("expected Type::Extern, got {:?}", init.ty);
    };
    assert_eq!(name.to_string(), "Sprite");
}

// ---- enum lowering tests ----

#[test]
fn lowers_unit_enum_variant() {
    let prog = lower_ok("enum Color { Red, Green, Blue } fn main() { let c = Color.Red; }");
    let main = find_main(&prog);
    let StmtKind::Let { init, .. } = &main.body.stmts[0].kind else {
        panic!("expected Let");
    };
    assert!(
        matches!(init.kind, ExprKind::EnumLiteral { variant: 0, .. }),
        "expected EnumLiteral variant=0, got {:?}",
        init.kind
    );
}

#[test]
fn lowers_tuple_enum_variant() {
    let prog =
        lower_ok("enum Msg { Ping(int), Move(int, int) } fn main() { let m = Msg.Ping(42); }");
    let main = find_main(&prog);
    let StmtKind::Let { init, .. } = &main.body.stmts[0].kind else {
        panic!("expected Let");
    };
    let ExprKind::EnumLiteral {
        variant, fields, ..
    } = &init.kind
    else {
        panic!("expected EnumLiteral, got {:?}", init.kind);
    };
    assert_eq!(*variant, 0);
    assert_eq!(fields.len(), 1);
}

#[test]
fn lowers_struct_enum_variant() {
    let prog = lower_ok(
        "enum Ev { Move { dx: int, dy: int } } fn main() { let e = Ev.Move { dx: 5, dy: 10 }; }",
    );
    let main = find_main(&prog);
    let StmtKind::Let { init, .. } = &main.body.stmts[0].kind else {
        panic!("expected Let");
    };
    let ExprKind::EnumLiteral {
        variant, fields, ..
    } = &init.kind
    else {
        panic!("expected EnumLiteral, got {:?}", init.kind);
    };
    assert_eq!(*variant, 0);
    assert_eq!(fields.len(), 2);
}

#[test]
fn match_arms_have_correct_variant_indices() {
    let prog = lower_ok(
        "enum Color { Red, Green, Blue } fn main() { let c = Color.Green; match c { Color.Red => {}, Color.Green => {}, Color.Blue => {}, } }",
    );
    let main = find_main(&prog);
    let StmtKind::Match { arms, .. } = &main.body.stmts[1].kind else {
        panic!("expected Match stmt");
    };
    assert_eq!(arms[0].variant, 0); // Red
    assert_eq!(arms[1].variant, 1); // Green
    assert_eq!(arms[2].variant, 2); // Blue
}

#[test]
fn match_wildcard_becomes_else_body() {
    let prog = lower_ok(
        "enum Color { Red, Green } fn main() { let c = Color.Red; match c { Color.Red => {}, _ => {}, } }",
    );
    let main = find_main(&prog);
    let StmtKind::Match {
        arms, else_body, ..
    } = &main.body.stmts[1].kind
    else {
        panic!("expected Match stmt");
    };
    assert_eq!(arms.len(), 1);
    assert!(else_body.is_some());
    assert!(else_body.as_ref().unwrap().binding.is_none());
}

#[test]
fn match_tuple_arm_has_bindings() {
    let prog = lower_ok(
        "enum Msg { Ping(int) } fn main() { let m = Msg.Ping(42); match m { Msg.Ping(v) => {}, } }",
    );
    let main = find_main(&prog);
    let StmtKind::Match { arms, .. } = &main.body.stmts[1].kind else {
        panic!("expected Match stmt");
    };
    assert_eq!(arms[0].bindings.len(), 1);
    assert_eq!(arms[0].bindings[0].field_index, 0);
}

#[test]
fn coalesce_desugars_to_match() {
    let prog = lower_ok("fn main() { let a: int? = nil; let x = a ?? 0; }");
    let main = find_main(&prog);
    assert!(matches!(main.body.stmts[1].kind, StmtKind::Match { .. }));
    let StmtKind::Let { init, .. } = &main.body.stmts[2].kind else {
        panic!("expected Let stmt for x");
    };
    assert!(matches!(init.kind, ExprKind::Local(_)));
}

#[test]
fn option_enum_has_well_known_type_id() {
    let prog = lower_ok("fn main() { let x = Option.Some(42); }");
    let main = find_main(&prog);
    let StmtKind::Let { init, .. } = &main.body.stmts[0].kind else {
        panic!("expected Let");
    };
    let ExprKind::EnumLiteral {
        type_id,
        variant,
        fields,
        ..
    } = &init.kind
    else {
        panic!("expected EnumLiteral, got {:?}", init.kind);
    };
    assert_eq!(
        *type_id,
        crate::prelude_enums::OPTION_TYPE_ID,
        "Option must have the well-known OPTION_TYPE_ID"
    );
    assert_eq!(*variant, 1, "Some must be variant 1");
    assert_eq!(fields.len(), 1, "Some must have 1 field");
}

#[test]
fn option_none_has_well_known_type_id() {
    let prog = lower_ok("fn main() { let x: int? = Option.None; }");
    let main = find_main(&prog);
    let StmtKind::Let { init, .. } = &main.body.stmts[0].kind else {
        panic!("expected Let");
    };
    let ExprKind::EnumLiteral {
        type_id, variant, ..
    } = &init.kind
    else {
        panic!("expected EnumLiteral, got {:?}", init.kind);
    };
    assert_eq!(
        *type_id,
        crate::prelude_enums::OPTION_TYPE_ID,
        "Option.None must have the well-known OPTION_TYPE_ID"
    );
    assert_eq!(*variant, 0, "None must be variant 0");
}
