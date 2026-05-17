use super::helpers::*;
use crate::ast;

fn expect_ternary(expr: &ast::ExprNode) -> (&ast::ExprNode, &ast::ExprNode, &ast::ExprNode) {
    match &expr.node.kind {
        ast::ExprKind::Ternary(node) => (
            node.node.cond.as_ref(),
            node.node.then_expr.as_ref(),
            node.node.else_expr.as_ref(),
        ),
        other => panic!("expected ternary, found {other:?}"),
    }
}

fn expect_if_let_downcast_head(source: &str, head: ast::PatternHead) {
    let program = parse_program(source);
    let ast::Stmt::Func(func) = &program.stmts[0].node else {
        panic!("expected function");
    };
    let Some(expr) = &func.node.body.node.tail else {
        panic!("expected if-let tail expression");
    };
    let ast::ExprKind::IfLet(if_let) = &expr.node.kind else {
        panic!("expected if-let");
    };
    assert_eq!(if_let.node.head, head);
    expect_exact_downcast(&if_let.node.value);
}

fn expect_match(source: &str) -> ast::MatchNode {
    let expr = parse_expr(source);
    match expr.node.kind {
        ast::ExprKind::Match(node) => node,
        other => panic!("expected match, found {other:?}"),
    }
}

fn expect_dyn_downcast(head: &ast::MatchArmHead) -> &ast::DynDowncastArmNode {
    match head {
        ast::MatchArmHead::DynDowncast(arm) => arm,
        other => panic!("expected dynamic downcast arm, found {other:?}"),
    }
}

fn expect_dyn_else(head: &ast::MatchArmHead) -> &ast::DynElseArmNode {
    match head {
        ast::MatchArmHead::DynElse(arm) => arm,
        other => panic!("expected dynamic else arm, found {other:?}"),
    }
}

fn assert_dyn_binding(binding: &ast::DynArmBinding, expected: ast::DynArmBinding) {
    assert_eq!(binding, &expected);
}

#[test]
fn lambda_var_return() {
    let expr = parse_expr("|var x: int| -> var int { x }");
    let ast::ExprKind::Lambda(lambda) = &expr.node.kind else {
        panic!("expected lambda");
    };
    let ret = lambda.node.ret_type.as_ref().expect("return type");
    assert_eq!(ret.access, ast::ReturnAccess::Place);
    assert_eq!(ret.ty, ast::Type::Int);
}

#[test]
fn mul_add_prec() {
    let expr = parse_expr("1 + 2 * 3");
    let (left, right) = expect_binary(&expr, ast::BinaryOp::Add);
    expect_int(left, 1);
    let (mul_left, mul_right) = expect_binary(right, ast::BinaryOp::Mul);
    expect_int(mul_left, 2);
    expect_int(mul_right, 3);
}

#[test]
fn sub_left_assoc() {
    let expr = parse_expr("a - b - c");
    let (left, right) = expect_binary(&expr, ast::BinaryOp::Sub);
    let (first_left, first_right) = expect_binary(left, ast::BinaryOp::Sub);
    expect_ident(first_left, "a");
    expect_ident(first_right, "b");
    expect_ident(right, "c");
}

#[test]
fn cmp_arith_prec() {
    let expr = parse_expr("a + b < c * d");
    let (left, right) = expect_binary(&expr, ast::BinaryOp::LessThan);
    let (add_left, add_right) = expect_binary(left, ast::BinaryOp::Add);
    expect_ident(add_left, "a");
    expect_ident(add_right, "b");
    let (mul_left, mul_right) = expect_binary(right, ast::BinaryOp::Mul);
    expect_ident(mul_left, "c");
    expect_ident(mul_right, "d");
}

#[test]
fn eq_mul_prec() {
    let expr = parse_expr("a * b == c + d");
    let (left, right) = expect_binary(&expr, ast::BinaryOp::Eq);
    let (mul_left, mul_right) = expect_binary(left, ast::BinaryOp::Mul);
    expect_ident(mul_left, "a");
    expect_ident(mul_right, "b");
    let (add_left, add_right) = expect_binary(right, ast::BinaryOp::Add);
    expect_ident(add_left, "c");
    expect_ident(add_right, "d");

    let chain = parse_expr("x == y == z");
    let (first, tail) = expect_binary(&chain, ast::BinaryOp::Eq);
    let (lhs, rhs) = expect_binary(first, ast::BinaryOp::Eq);
    expect_ident(lhs, "x");
    expect_ident(rhs, "y");
    expect_ident(tail, "z");
}

#[test]
fn and_or_prec() {
    let expr = parse_expr("a && b || c");
    let (left, right) = expect_binary(&expr, ast::BinaryOp::Or);
    let (and_left, and_right) = expect_binary(left, ast::BinaryOp::And);
    expect_ident(and_left, "a");
    expect_ident(and_right, "b");
    expect_ident(right, "c");
}

#[test]
fn ternary_shape() {
    let expr = parse_expr("cond ? a : b");
    let (cond, then_expr, else_expr) = expect_ternary(&expr);
    expect_ident(cond, "cond");
    expect_ident(then_expr, "a");
    expect_ident(else_expr, "b");
}

#[test]
fn try_precedence() {
    let expr = parse_expr("try a + b");
    let (left, right) = expect_binary(&expr, ast::BinaryOp::Add);
    expect_ident(expect_try(left), "a");
    expect_ident(right, "b");

    let expr = parse_expr("try f().g()");
    let target = expect_try(&expr);
    let (method, method_args) = expect_call(target, false);
    assert!(method_args.is_empty());
    let call_target = expect_field(method, "g", false);
    let (func, args) = expect_call(call_target, false);
    expect_ident(func, "f");
    assert!(args.is_empty());

    let expr = parse_expr("try -a");
    expect_ident(expect_unary(expect_try(&expr), ast::UnaryOp::Neg), "a");

    let expr = parse_expr("-try a");
    expect_ident(expect_try(expect_unary(&expr, ast::UnaryOp::Neg)), "a");

    let expr = parse_expr("try try nested()");
    let inner = expect_try(expect_try(&expr));
    let (func, args) = expect_call(inner, false);
    expect_ident(func, "nested");
    assert!(args.is_empty());
}

#[test]
fn ternary_precedence() {
    let expr = parse_expr("a || b ? c + d : e * f");
    let (cond, then_expr, else_expr) = expect_ternary(&expr);
    expect_binary(cond, ast::BinaryOp::Or);
    expect_binary(then_expr, ast::BinaryOp::Add);
    expect_binary(else_expr, ast::BinaryOp::Mul);
}

#[test]
fn nested_ternary() {
    let expr = parse_expr("a ? b : c ? d : e");
    let (cond, then_expr, else_expr) = expect_ternary(&expr);
    expect_ident(cond, "a");
    expect_ident(then_expr, "b");
    let (nested_cond, nested_then, nested_else) = expect_ternary(else_expr);
    expect_ident(nested_cond, "c");
    expect_ident(nested_then, "d");
    expect_ident(nested_else, "e");
}

#[test]
fn ternary_missing_colon_fails() {
    parse_expr_err("a ? b");
}

#[test]
fn coalesce_and_or_prec() {
    let expr = parse_expr("a ?? b || c");
    let (left, right) = expect_binary(&expr, ast::BinaryOp::Or);
    let (coal_left, coal_right) = expect_binary(left, ast::BinaryOp::Coalesce);
    expect_ident(coal_left, "a");
    expect_ident(coal_right, "b");
    expect_ident(right, "c");

    let expr = parse_expr("a && b ?? c");
    let (coal_left, coal_right) = expect_binary(&expr, ast::BinaryOp::Coalesce);
    let (and_left, and_right) = expect_binary(coal_left, ast::BinaryOp::And);
    expect_ident(and_left, "a");
    expect_ident(and_right, "b");
    expect_ident(coal_right, "c");
}

#[test]
fn coalesce_mixed() {
    let expr = parse_expr("a ?? b && c");
    let (left, right) = expect_binary(&expr, ast::BinaryOp::Coalesce);
    expect_ident(left, "a");
    let (and_left, and_right) = expect_binary(right, ast::BinaryOp::And);
    expect_ident(and_left, "b");
    expect_ident(and_right, "c");

    let expr = parse_expr("a ?? b ?? c");
    let (first, tail) = expect_binary(&expr, ast::BinaryOp::Coalesce);
    let (left_left, left_right) = expect_binary(first, ast::BinaryOp::Coalesce);
    expect_ident(left_left, "a");
    expect_ident(left_right, "b");
    expect_ident(tail, "c");
}

#[test]
fn coalesce_eq_prec() {
    let expr = parse_expr("x == y ?? z");
    let (left, right) = expect_binary(&expr, ast::BinaryOp::Coalesce);
    let (eq_left, eq_right) = expect_binary(left, ast::BinaryOp::Eq);
    expect_ident(eq_left, "x");
    expect_ident(eq_right, "y");
    expect_ident(right, "z");

    let expr = parse_expr("x ?? y == z");
    let (left, right) = expect_binary(&expr, ast::BinaryOp::Coalesce);
    expect_ident(left, "x");
    let (eq_left, eq_right) = expect_binary(right, ast::BinaryOp::Eq);
    expect_ident(eq_left, "y");
    expect_ident(eq_right, "z");
}

#[test]
fn full_prec_chain() {
    let expr = parse_expr("a + b ?? c * d && e || f");
    let (left, right) = expect_binary(&expr, ast::BinaryOp::Or);
    expect_ident(right, "f");

    let (coal_left, coal_right) = expect_binary(left, ast::BinaryOp::Coalesce);
    let (add_left, add_right) = expect_binary(coal_left, ast::BinaryOp::Add);
    expect_ident(add_left, "a");
    expect_ident(add_right, "b");

    let (and_left, and_right) = expect_binary(coal_right, ast::BinaryOp::And);
    let (mul_left, mul_right) = expect_binary(and_left, ast::BinaryOp::Mul);
    expect_ident(mul_left, "c");
    expect_ident(mul_right, "d");
    expect_ident(and_right, "e");
}

#[test]
fn range_add_prec() {
    let expr = parse_expr("1 + 2 .. 3");
    let (start, end) = expect_range(&expr, false);
    let (add_left, add_right) = expect_binary(start, ast::BinaryOp::Add);
    expect_int(add_left, 1);
    expect_int(add_right, 2);
    expect_int(end, 3);
}

#[test]
fn range_cmp_prec() {
    let expr = parse_expr("a..b < c");
    let (left, right) = expect_binary(&expr, ast::BinaryOp::LessThan);
    let (start, end) = expect_range(left, false);
    expect_ident(start, "a");
    expect_ident(end, "b");
    expect_ident(right, "c");
}

#[test]
fn inclusive_range() {
    let expr = parse_expr("0..=10");
    let (start, end) = expect_range(&expr, true);
    expect_int(start, 0);
    expect_int(end, 10);
}

#[test]
fn range_mixed() {
    let expr = parse_expr("a + b .. c < d && e ?? f || g");
    let (left, right) = expect_binary(&expr, ast::BinaryOp::Or);
    expect_ident(right, "g");

    let (coal_left, coal_right) = expect_binary(left, ast::BinaryOp::Coalesce);
    expect_ident(coal_right, "f");

    let (and_left, and_right) = expect_binary(coal_left, ast::BinaryOp::And);
    expect_ident(and_right, "e");

    let (cmp_left, cmp_right) = expect_binary(and_left, ast::BinaryOp::LessThan);
    expect_ident(cmp_right, "d");

    let (range_start, range_end) = expect_range(cmp_left, false);
    let (add_left, add_right) = expect_binary(range_start, ast::BinaryOp::Add);
    expect_ident(add_left, "a");
    expect_ident(add_right, "b");
    expect_ident(range_end, "c");
}

#[test]
fn opt_field_flag() {
    let expr = parse_expr("foo?.bar");
    let target = expect_field(&expr, "bar", true);
    expect_ident(target, "foo");
}

#[test]
fn opt_field_chain() {
    let expr = parse_expr("foo?.bar.baz");
    let first = expect_field(&expr, "baz", false);
    let base = expect_field(first, "bar", true);
    expect_ident(base, "foo");

    let expr = parse_expr("foo.bar?.baz");
    let first = expect_field(&expr, "baz", true);
    let base = expect_field(first, "bar", false);
    expect_ident(base, "foo");
}

#[test]
fn opt_idx_field() {
    let expr = parse_expr("arr?[0]");
    let (target, index_expr) = expect_index(&expr, true);
    expect_ident(target, "arr");
    expect_int(index_expr, 0);

    let expr = parse_expr("arr?[i].field");
    let field_target = expect_field(&expr, "field", false);
    let (target, index_expr) = expect_index(field_target, true);
    expect_ident(target, "arr");
    expect_ident(index_expr, "i");
}

#[test]
fn opt_map_idx() {
    let expr = parse_expr(r#"map?["key"]"#);
    let (target, index_expr) = expect_index(&expr, true);
    expect_ident(target, "map");
    match &index_expr.node.kind {
        ast::ExprKind::Lit(ast::Lit::String(s)) => assert_eq!(s, "key"),
        ast::ExprKind::Lit(other) => panic!("expected string literal key, found {other:?}"),
        other => panic!("expected string literal key, found {other:?}"),
    }
}

#[test]
fn opt_call() {
    let expr = parse_expr("foo?()");
    let (target, args) = expect_call(&expr, true);
    expect_ident(target, "foo");
    assert!(args.is_empty());
}

#[test]
fn call_const_arg() {
    let expr = parse_expr("foo<int, 3>(1)");
    let (_, _, generics) = expect_generic_call(&expr, false);
    assert_eq!(
        generics,
        [
            ast::GenericArg::Type(ast::Type::Int),
            ast::GenericArg::Const(ast::ConstArg::Value(ast::ConstValue::Int(3))),
        ]
    );
}

#[test]
fn opt_chain_coalesce() {
    let expr = parse_expr("foo?.bar ?? default");
    let (left, right) = expect_binary(&expr, ast::BinaryOp::Coalesce);
    expect_ident(right, "default");
    let field_target = expect_field(left, "bar", true);
    expect_ident(field_target, "foo");

    let expr = parse_expr("foo?.bar?.baz ?? y");
    let (left, right) = expect_binary(&expr, ast::BinaryOp::Coalesce);
    expect_ident(right, "y");
    let baz_target = expect_field(left, "baz", true);
    let bar_target = expect_field(baz_target, "bar", true);
    expect_ident(bar_target, "foo");
}

#[test]
fn array_basic() {
    let expr = parse_expr("[1, 2, 3]");
    let elements = expect_array_literal(&expr);
    assert_eq!(elements.len(), 3);
    expect_int(&elements[0], 1);
    expect_int(&elements[1], 2);
    expect_int(&elements[2], 3);
}

#[test]
fn array_trailing_comma() {
    let expr = parse_expr("[1, 2, 3,]");
    let elements = expect_array_literal(&expr);
    assert_eq!(elements.len(), 3);
    expect_int(&elements[0], 1);
    expect_int(&elements[1], 2);
    expect_int(&elements[2], 3);
}

#[test]
fn array_empty() {
    let expr = parse_expr("[]");
    let elements = expect_array_literal(&expr);
    assert_eq!(elements.len(), 0);
}

#[test]
fn array_nested() {
    let expr = parse_expr("[[1, 2], [3, 4]]");
    let outer_elements = expect_array_literal(&expr);
    assert_eq!(outer_elements.len(), 2);

    let inner1 = expect_array_literal(&outer_elements[0]);
    assert_eq!(inner1.len(), 2);
    expect_int(&inner1[0], 1);
    expect_int(&inner1[1], 2);

    let inner2 = expect_array_literal(&outer_elements[1]);
    assert_eq!(inner2.len(), 2);
    expect_int(&inner2[0], 3);
    expect_int(&inner2[1], 4);
}

#[test]
fn array_fill_basic() {
    let expr = parse_expr("[0; 3]");
    let (value, len) = expect_array_fill(&expr);
    expect_int(value, 0);
    expect_int(len, 3);
}

#[test]
fn array_fill_expr_len() {
    let expr = parse_expr("[x + 1; n]");
    let (value, len) = expect_array_fill(&expr);

    let (left, right) = expect_binary(value, ast::BinaryOp::Add);
    expect_ident(left, "x");
    expect_int(right, 1);

    expect_ident(len, "n");
}

#[test]
fn map_empty() {
    let expr = parse_expr("[:]");
    let entries = expect_map_literal(&expr);
    assert!(entries.is_empty());
}

#[test]
fn map_single() {
    let expr = parse_expr(r#"["hp": 100]"#);
    let entries = expect_map_literal(&expr);
    assert_eq!(entries.len(), 1);
    expect_string(&entries[0].0, "hp");
    expect_int(&entries[0].1, 100);
}

#[test]
fn map_multi() {
    let expr = parse_expr(r#"["hp": 100, "mp": 50]"#);
    let entries = expect_map_literal(&expr);
    assert_eq!(entries.len(), 2);
    expect_string(&entries[0].0, "hp");
    expect_int(&entries[0].1, 100);
    expect_string(&entries[1].0, "mp");
    expect_int(&entries[1].1, 50);
}

#[test]
fn map_trailing_comma() {
    let expr = parse_expr(r#"["a": 1,]"#);
    let entries = expect_map_literal(&expr);
    assert_eq!(entries.len(), 1);
    expect_string(&entries[0].0, "a");
    expect_int(&entries[0].1, 1);
}

#[test]
fn map_int_keys() {
    let expr = parse_expr(r#"[1: "one", 2: "two"]"#);
    let entries = expect_map_literal(&expr);
    assert_eq!(entries.len(), 2);
    expect_int(&entries[0].0, 1);
    expect_string(&entries[0].1, "one");
    expect_int(&entries[1].0, 2);
    expect_string(&entries[1].1, "two");
}

#[test]
fn map_nested() {
    let expr = parse_expr(r#"["outer": ["inner": 1]]"#);
    let outer = expect_map_literal(&expr);
    assert_eq!(outer.len(), 1);
    expect_string(&outer[0].0, "outer");
    let inner = expect_map_literal(&outer[0].1);
    assert_eq!(inner.len(), 1);
    expect_string(&inner[0].0, "inner");
    expect_int(&inner[0].1, 1);
}

#[test]
fn map_expr_values() {
    let expr = parse_expr(r#"["sum": 1 + 2]"#);
    let entries = expect_map_literal(&expr);
    assert_eq!(entries.len(), 1);
    expect_string(&entries[0].0, "sum");
    let (left, right) = expect_binary(&entries[0].1, ast::BinaryOp::Add);
    expect_int(left, 1);
    expect_int(right, 2);
}

#[test]
fn interp_single_var() {
    let expr = parse_expr(r#"f"HP: {hp}""#);
    let parts = expect_string_interp(&expr);
    assert_eq!(parts.len(), 2);
    match &parts[0] {
        ast::StringPart::Text(s) => assert_eq!(s, "HP: "),
        ast::StringPart::Expr(other, _) => panic!("expected Text, found Expr({other:?}, _)"),
    }
    match &parts[1] {
        ast::StringPart::Expr(e, _) => expect_ident(e, "hp"),
        ast::StringPart::Text(other) => panic!("expected Expr, found Text({other:?})"),
    }
}

#[test]
fn interp_expr() {
    let expr = parse_expr(r#"f"a {x + y} b""#);
    let parts = expect_string_interp(&expr);
    assert_eq!(parts.len(), 3);
    match &parts[0] {
        ast::StringPart::Text(s) => assert_eq!(s, "a "),
        ast::StringPart::Expr(other, _) => panic!("expected Text, found Expr({other:?}, _)"),
    }
    match &parts[1] {
        ast::StringPart::Expr(e, _) => {
            let (left, right) = expect_binary(e, ast::BinaryOp::Add);
            expect_ident(left, "x");
            expect_ident(right, "y");
        }
        ast::StringPart::Text(other) => panic!("expected Expr, found Text({other:?})"),
    }
    match &parts[2] {
        ast::StringPart::Text(s) => assert_eq!(s, " b"),
        ast::StringPart::Expr(other, _) => panic!("expected Text, found Expr({other:?}, _)"),
    }
}

#[test]
fn interp_multi_expr() {
    let expr = parse_expr(r#"f"{a} and {b}""#);
    let parts = expect_string_interp(&expr);
    assert_eq!(parts.len(), 3);
    match &parts[0] {
        ast::StringPart::Expr(e, _) => expect_ident(e, "a"),
        ast::StringPart::Text(other) => panic!("expected Expr, found Text({other:?})"),
    }
    match &parts[1] {
        ast::StringPart::Text(s) => assert_eq!(s, " and "),
        ast::StringPart::Expr(other, _) => panic!("expected Text, found Expr({other:?}, _)"),
    }
    match &parts[2] {
        ast::StringPart::Expr(e, _) => expect_ident(e, "b"),
        ast::StringPart::Text(other) => panic!("expected Expr, found Text({other:?})"),
    }
}

#[test]
fn interp_only_expr() {
    let expr = parse_expr(r#"f"{x}""#);
    let parts = expect_string_interp(&expr);
    assert_eq!(parts.len(), 1);
    match &parts[0] {
        ast::StringPart::Expr(e, _) => expect_ident(e, "x"),
        ast::StringPart::Text(other) => panic!("expected Expr, found Text({other:?})"),
    }
}

#[test]
fn interp_plain_str() {
    let expr = parse_expr(r#""just text""#);
    expect_string(&expr, "just text");
}

#[test]
fn interp_escaped_brace() {
    let expr = parse_expr(r#"f"\{not_interp}""#);
    expect_string(&expr, "{not_interp}");
}

#[test]
fn interp_no_fmt() {
    let expr = parse_expr(r#"f"{x}""#);
    let parts = expect_string_interp(&expr);
    assert_eq!(parts.len(), 1);
    match &parts[0] {
        ast::StringPart::Expr(_, None) => {}
        ast::StringPart::Text(other) => {
            panic!("expected Expr without FormatSpec, found Text({other:?})")
        }
        ast::StringPart::Expr(_, Some(spec)) => {
            panic!("expected Expr without FormatSpec, found Expr(_, Some({spec:?}))")
        }
    }
}

#[test]
fn interp_fmt_width() {
    let expr = parse_expr(r#"f"{x:04}""#);
    let parts = expect_string_interp(&expr);
    assert_eq!(parts.len(), 1);
    match &parts[0] {
        ast::StringPart::Expr(_, Some(spec)) => {
            assert!(spec.node.zero_pad);
            assert_eq!(spec.node.width, Some(4));
            assert_eq!(spec.node.fill, '0');
            assert_eq!(spec.node.align, Some(ast::FormatAlign::Right));
            assert_eq!(spec.node.kind, ast::FormatKind::Default);
        }
        ast::StringPart::Text(other) => {
            panic!("expected Expr with FormatSpec, found Text({other:?})")
        }
        ast::StringPart::Expr(_, None) => {
            panic!("expected Expr with FormatSpec, found Expr(_, None)")
        }
    }
}

#[test]
fn interp_fmt_precision() {
    let expr = parse_expr(r#"f"{x:.2}""#);
    let parts = expect_string_interp(&expr);
    assert_eq!(parts.len(), 1);
    match &parts[0] {
        ast::StringPart::Expr(_, Some(spec)) => {
            assert_eq!(spec.node.precision, Some(2));
            assert_eq!(spec.node.kind, ast::FormatKind::Default);
        }
        ast::StringPart::Text(other) => {
            panic!("expected Expr with FormatSpec, found Text({other:?})")
        }
        ast::StringPart::Expr(_, None) => {
            panic!("expected Expr with FormatSpec, found Expr(_, None)")
        }
    }
}

#[test]
fn interp_fmt_align_width() {
    let expr = parse_expr(r#"f"{x:>10}""#);
    let parts = expect_string_interp(&expr);
    assert_eq!(parts.len(), 1);
    match &parts[0] {
        ast::StringPart::Expr(_, Some(spec)) => {
            assert_eq!(spec.node.align, Some(ast::FormatAlign::Right));
            assert_eq!(spec.node.width, Some(10));
        }
        ast::StringPart::Text(other) => {
            panic!("expected Expr with FormatSpec, found Text({other:?})")
        }
        ast::StringPart::Expr(_, None) => {
            panic!("expected Expr with FormatSpec, found Expr(_, None)")
        }
    }
}

#[test]
fn interp_fmt_fill_align() {
    let expr = parse_expr(r#"f"{x:*>10}""#);
    let parts = expect_string_interp(&expr);
    assert_eq!(parts.len(), 1);
    match &parts[0] {
        ast::StringPart::Expr(_, Some(spec)) => {
            assert_eq!(spec.node.fill, '*');
            assert_eq!(spec.node.align, Some(ast::FormatAlign::Right));
            assert_eq!(spec.node.width, Some(10));
        }
        ast::StringPart::Text(other) => {
            panic!("expected Expr with FormatSpec, found Text({other:?})")
        }
        ast::StringPart::Expr(_, None) => {
            panic!("expected Expr with FormatSpec, found Expr(_, None)")
        }
    }
}

#[test]
fn interp_fmt_hex() {
    let expr = parse_expr(r#"f"{x:08x}""#);
    let parts = expect_string_interp(&expr);
    assert_eq!(parts.len(), 1);
    match &parts[0] {
        ast::StringPart::Expr(_, Some(spec)) => {
            assert!(spec.node.zero_pad);
            assert_eq!(spec.node.width, Some(8));
            assert_eq!(spec.node.kind, ast::FormatKind::Hex);
        }
        ast::StringPart::Text(other) => {
            panic!("expected Expr with FormatSpec, found Text({other:?})")
        }
        ast::StringPart::Expr(_, None) => {
            panic!("expected Expr with FormatSpec, found Expr(_, None)")
        }
    }
}

#[test]
fn interp_fmt_sign_prec() {
    let expr = parse_expr(r#"f"{x:+.2}""#);
    let parts = expect_string_interp(&expr);
    assert_eq!(parts.len(), 1);
    match &parts[0] {
        ast::StringPart::Expr(_, Some(spec)) => {
            assert_eq!(spec.node.sign, ast::FormatSign::Always);
            assert_eq!(spec.node.precision, Some(2));
        }
        ast::StringPart::Text(other) => {
            panic!("expected Expr with FormatSpec, found Text({other:?})")
        }
        ast::StringPart::Expr(_, None) => {
            panic!("expected Expr with FormatSpec, found Expr(_, None)")
        }
    }
}

#[test]
fn interp_fmt_composed() {
    let expr = parse_expr(r#"f"{x:0>+8x}""#);
    let parts = expect_string_interp(&expr);
    assert_eq!(parts.len(), 1);
    match &parts[0] {
        ast::StringPart::Expr(_, Some(spec)) => {
            assert_eq!(spec.node.fill, '0');
            assert_eq!(spec.node.align, Some(ast::FormatAlign::Right));
            assert_eq!(spec.node.sign, ast::FormatSign::Always);
            assert_eq!(spec.node.width, Some(8));
            assert_eq!(spec.node.kind, ast::FormatKind::Hex);
        }
        ast::StringPart::Text(other) => {
            panic!("expected Expr with FormatSpec, found Text({other:?})")
        }
        ast::StringPart::Expr(_, None) => {
            panic!("expected Expr with FormatSpec, found Expr(_, None)")
        }
    }
}

#[test]
fn err_interp_bad_fmt_type() {
    parse_program_err(r#"fn main() { let s = f"{x:q}"; }"#);
}

#[test]
fn err_interp_empty_fmt() {
    parse_program_err(r#"fn main() { let s = f"{x:}"; }"#);
}

#[test]
fn cast_int_float() {
    let expr = parse_expr("42 as float");
    let (inner, target) = expect_cast(&expr);
    expect_int(inner, 42);
    assert_eq!(*target, ast::Type::Float);
}

#[test]
fn cast_float_int() {
    let expr = parse_expr("1.25 as int");
    let (inner, target) = expect_cast(&expr);
    expect_float(inner, 1.25);
    assert_eq!(*target, ast::Type::Int);
}

#[test]
fn cast_vs_binary() {
    let expr = parse_expr("1 + x as float");
    let (left, right) = expect_binary(&expr, ast::BinaryOp::Add);
    expect_int(left, 1);
    let (inner, target) = expect_cast(right);
    expect_ident(inner, "x");
    assert_eq!(*target, ast::Type::Float);
}

#[test]
fn cast_vs_unary() {
    let expr = parse_expr("-x as float");
    let (inner, target) = expect_cast(&expr);
    let operand = expect_unary(inner, ast::UnaryOp::Neg);
    expect_ident(operand, "x");
    assert_eq!(*target, ast::Type::Float);
}

#[test]
fn cast_chained() {
    let expr = parse_expr("x as float as int");
    let (outer_inner, outer_target) = expect_cast(&expr);
    assert_eq!(*outer_target, ast::Type::Int);
    let (inner_inner, inner_target) = expect_cast(outer_inner);
    expect_ident(inner_inner, "x");
    assert_eq!(*inner_target, ast::Type::Float);
}

#[test]
fn exact_downcast_basic() {
    let expr = parse_expr("actor as? Enemy");
    let (inner, target) = expect_exact_downcast(&expr);
    expect_ident(inner, "actor");
    expect_nominal(target, "Enemy");
}

#[test]
fn exact_downcast_chains_with_casts() {
    let expr = parse_expr("x as? Enemy as Actor");
    let (outer_inner, outer_target) = expect_cast(&expr);
    expect_nominal(outer_target, "Actor");
    let (_, inner_target) = expect_exact_downcast(outer_inner);
    expect_nominal(inner_target, "Enemy");

    let expr = parse_expr("x as Enemy as? Actor");
    let (outer_inner, outer_target) = expect_exact_downcast(&expr);
    expect_nominal(outer_target, "Actor");
    let (_, inner_target) = expect_cast(outer_inner);
    expect_nominal(inner_target, "Enemy");
}

#[test]
fn exact_downcast_if_var_scrutinee() {
    expect_if_let_downcast_head(
        "fn main() { if var enemy = actor as? Enemy {} }",
        ast::PatternHead::Var,
    );
}

#[test]
fn exact_downcast_if_let_scrutinee() {
    expect_if_let_downcast_head(
        "fn main() { if let enemy = actor as? Enemy {} }",
        ast::PatternHead::Let,
    );
}

#[test]
fn exact_downcast_if_let_var_rejected() {
    parse_program_err("fn main() { if let var enemy = actor as? Enemy {} }");
}

#[test]
fn dynamic_match_arm_heads() {
    let match_node = expect_match("match actor { as Enemy(enemy) => enemy, else(other) => other }");
    assert_eq!(match_node.node.arms.len(), 2);

    let downcast = expect_dyn_downcast(&match_node.node.arms[0].node.head);
    expect_nominal(&downcast.node.target, "Enemy");
    assert_dyn_binding(
        &downcast.node.binding,
        ast::DynArmBinding::Named(ast::Ident::new("enemy")),
    );

    let else_arm = expect_dyn_else(&match_node.node.arms[1].node.head);
    assert_dyn_binding(
        &else_arm.node.binding,
        ast::DynArmBinding::Named(ast::Ident::new("other")),
    );
}

#[test]
fn dynamic_match_downcast_arm_ids_are_distinct() {
    let match_node = expect_match(
        "match actor { as Enemy(enemy) => enemy, as Bullet(bullet) => bullet, else(other) => other }",
    );
    let first = expect_dyn_downcast(&match_node.node.arms[0].node.head);
    let second = expect_dyn_downcast(&match_node.node.arms[1].node.head);
    assert_ne!(first.node.id, second.node.id);
}

#[test]
fn dynamic_match_wildcard_arm_heads() {
    let match_node = expect_match("match var actor { as Enemy(_) => actor, else(_) => actor }");
    assert_eq!(match_node.node.head, ast::PatternHead::Var);
    assert_dyn_binding(
        &expect_dyn_downcast(&match_node.node.arms[0].node.head)
            .node
            .binding,
        ast::DynArmBinding::Wildcard,
    );
    assert_dyn_binding(
        &expect_dyn_else(&match_node.node.arms[1].node.head)
            .node
            .binding,
        ast::DynArmBinding::Wildcard,
    );
}

#[test]
fn struct_args() {
    let expr = parse_expr("FixedBuf<int, 3> { data: xs }");
    let ast::ExprKind::StructLiteral(lit) = &expr.node.kind else {
        panic!("expected struct literal, found {:?}", expr.node.kind);
    };
    assert_eq!(lit.node.name.0.as_ref(), "FixedBuf");
    assert_eq!(lit.node.generic_args.len(), 2);
    assert_eq!(
        lit.node.generic_args[0],
        ast::GenericArg::Type(ast::Type::Int)
    );
    assert_eq!(
        lit.node.generic_args[1],
        ast::GenericArg::Const(ast::ConstArg::Value(ast::ConstValue::Int(3)))
    );
}

#[test]
fn intrinsic_basic() {
    let expr = parse_expr("#profile(debug)");
    let args = expect_intrinsic_call(&expr, "profile");
    assert_eq!(args.len(), 1);
    expect_ident(&args[0], "debug");
}

#[test]
fn intrinsic_no_args() {
    let expr = parse_expr("#file()");
    let args = expect_intrinsic_call(&expr, "file");
    assert!(args.is_empty());
}

#[test]
fn intrinsic_unknown() {
    let expr = parse_expr("#unknown(x)");
    let args = expect_intrinsic_call(&expr, "unknown");
    assert_eq!(args.len(), 1);
}

#[test]
fn rejects_labelled_tuple_literal() {
    parse_expr_err("(a: 1, b: 2)");
}

#[test]
fn rejects_single_labelled_tuple_literal() {
    parse_expr_err("(a: 1)");
}
