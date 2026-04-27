use super::support::{assert_err, assert_err_count, assert_type};
use crate::ast::Type;

mod binding_patterns {
    use super::*;

    #[test]
    fn let_tuple_ok() {
        assert_type("fn main() { let (x, y) = (1, true); x; }", Type::Int);
    }

    #[test]
    fn let_tuple_yields_bool() {
        assert_type("fn main() { let (x, y) = (1, true); y; }", Type::Bool);
    }

    #[test]
    fn let_tuple_arity_err() {
        assert_err("fn main() { let (x, y) = (1, 2, 3); }");
    }

    #[test]
    fn let_tuple_non_tuple_err() {
        assert_err("fn main() { let (x, y) = 1; }");
    }

    #[test]
    fn let_literal_int_ok() {
        assert_err_count("fn main() { let 1 = 1; }", 0);
    }

    #[test]
    fn let_literal_bool_ok() {
        assert_err_count("fn main() { let true = true; }", 0);
    }

    #[test]
    fn let_literal_string_ok() {
        assert_err_count("fn main() { let \"hi\" = \"hi\"; }", 0);
    }
}

mod for_patterns {
    use super::*;

    #[test]
    fn for_wildcard() {
        assert_err_count("fn main(xs: [int]) { for _ in xs {} }", 0);
    }

    #[test]
    fn for_ident() {
        assert_err_count("fn main(xs: [int]) { for x in xs {} }", 0);
    }

    #[test]
    fn for_tuple_non_tuple_err() {
        assert_err("fn main(x: int) { for (a, b) in x {} }");
    }
}

mod while_let {
    use super::*;

    #[test]
    fn bool_match() {
        assert_err_count("fn main() { while let true = true {} }", 0);
    }

    #[test]
    fn ident_binds() {
        assert_err_count("fn main() { while let x = true { let y = x; } }", 0);
    }

    #[test]
    fn scope() {
        assert_err("fn main() { while let x = true {} x; }");
    }

    #[test]
    fn break_ok() {
        assert_err_count("fn main() { while let true = true { break; } }", 0);
    }

    #[test]
    fn continue_ok() {
        assert_err_count("fn main() { while let true = true { continue; } }", 0);
    }

    #[test]
    fn nested_break() {
        assert_err_count(
            "fn main() { while true { while let true = true { break; } } }",
            0,
        );
    }
}

mod if_let {
    use super::*;

    #[test]
    fn bool_match() {
        assert_err_count("fn main() { if let true = true {} }", 0);
    }

    #[test]
    fn ident_binds() {
        assert_type(
            "fn main() { if let x = true { x } else { false }; }",
            Type::Bool,
        );
    }

    #[test]
    fn scope() {
        assert_err("fn main() { if let x = true {} x; }");
    }

    #[test]
    fn else_scope() {
        assert_err("fn main() { if let x = true { true } else { x }; }");
    }

    #[test]
    fn tuple_ok() {
        assert_type(
            "fn main() { if let (a, b) = (1, true) { a } else { 0 }; }",
            Type::Int,
        );
    }

    #[test]
    fn tuple_arity_err() {
        assert_err("fn main() { if let (a, b) = (1, 2, 3) {} }");
    }

    #[test]
    fn non_tuple_err() {
        assert_err("fn main() { if let (a, b) = 1 {} }");
    }

    #[test]
    fn branch_match() {
        assert_type(
            "fn main() { if let true = true { 1 } else { 2 }; }",
            Type::Int,
        );
    }

    #[test]
    fn branch_mismatch() {
        assert_err("fn main() { if let true = true { 1 } else { \"hi\" }; }");
    }

    #[test]
    fn no_else_void() {
        assert_type("fn main() { if let true = true { 1 }; }", Type::Void);
    }
}

mod match_stmt {
    use super::*;

    #[test]
    fn lit_int() {
        assert_type(
            "fn main() { let _ = match 1 { 1 => 2, _ => 3 }; }",
            Type::Int,
        );
    }

    #[test]
    fn lit_mismatch() {
        assert_err("fn main() { let _ = match 1 { 1 => 2, _ => \"hi\" }; }");
    }

    #[test]
    fn wildcard() {
        assert_type("fn main() { let _ = match 1 { _ => 42 }; }", Type::Int);
    }

    #[test]
    fn ident_binds() {
        assert_type("fn main() { let _ = match 1 { x => x }; }", Type::Int);
    }

    #[test]
    fn ident_bool_condition() {
        assert_type(
            "fn main() { let _ = match true { x => if x { 1 } else { 2 } }; }",
            Type::Int,
        );
    }

    #[test]
    fn tuple_ok() {
        assert_type(
            "fn main() { let _ = match (1, true) { (a, b) => a }; }",
            Type::Int,
        );
    }

    #[test]
    fn tuple_arity_err() {
        assert_err("fn main() { let _ = match (1, 2, 3) { (a, b) => a }; }");
    }

    #[test]
    fn non_tuple_pattern() {
        assert_err("fn main() { let _ = match 1 { (a, b) => a }; }");
    }

    #[test]
    fn arm_scope() {
        assert_err("fn main() { let _ = match 1 { x => 1 }; x; }");
    }

    #[test]
    fn arm_type_mismatch() {
        assert_err("fn main() { let _ = match 1 { 1 => 1, 2 => \"hi\" }; }");
    }

    #[test]
    fn or_unsupported() {
        assert_err("fn main() { let _ = match 1 { 1 | 2 => 1 }; }");
    }

    #[test]
    fn range_unsupported() {
        assert_err("fn main() { let _ = match 1 { 1..5 => 1 }; }");
    }
}

mod unsupported_forms {
    use super::*;

    #[test]
    fn struct_pattern() {
        assert_err("fn main() { let _ = match 1 { Point { x, y } => 1 }; }");
    }

    #[test]
    fn var_ident() {
        assert_err("fn main() { let _ = match 1 { var x => 1 }; }");
    }
}
