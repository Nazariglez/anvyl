use super::support::{assert_err, assert_err_count, assert_single_error, assert_ty, errors};
use crate::{ast::Type, typecheck::TypeError};

mod binding_patterns {
    use super::*;

    #[test]
    fn let_literal_int_ok() {
        assert_err_count("fn main() { let 1 = 1; }", 0);
    }

    #[test]
    fn let_literal_bool_ok() {
        assert_err_count("fn main() { let true = true; }", 0);
    }

    #[test]
    fn let_literal_false_ok() {
        assert_err_count("fn main() { let false = false; }", 0);
    }

    #[test]
    fn let_literal_string_ok() {
        assert_err_count("fn main() { let \"hi\" = \"hi\"; }", 0);
    }
}

mod while_let {
    use super::*;

    #[test]
    fn bool_match() {
        assert_err_count("fn main() { while let true = true {} }", 0);
    }

    #[test]
    fn scope() {
        assert_err("fn main() { while let x = true {} x; }");
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
        assert_ty(
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
    fn non_tuple_err() {
        assert_err("fn main() { if let (a, b) = 1 {} }");
    }
}

mod match_stmt {
    use super::*;

    #[test]
    fn lit_int() {
        assert_ty(
            "fn main() { let _ = match 1 { 1 => 2, _ => 3 }; }",
            Type::Int,
        );
    }

    #[test]
    fn lit_mismatch() {
        assert_err("fn main() { let _ = match 1 { 1 => 2, _ => \"hi\" }; }");
    }

    #[test]
    fn ident_binds() {
        assert_ty("fn main() { let _ = match 1 { x => x }; }", Type::Int);
    }

    #[test]
    fn ident_bool_condition() {
        assert_ty(
            "fn main() { let _ = match true { x => if x { 1 } else { 2 } }; }",
            Type::Int,
        );
    }

    #[test]
    fn tuple_false_ok() {
        assert_ty(
            "fn main() { let _ = match (1, false) { (a, false) => a }; }",
            Type::Int,
        );
    }

    #[test]
    fn string_lit_not_catch_all() {
        assert_single_error("fn main() { match \"x\" { \"x\" => {} } }", |err| {
            matches!(err, TypeError::NonExhaustiveMatch { .. })
        });
    }

    #[test]
    fn float_lit_not_catch_all() {
        assert_single_error("fn main() { match 1.0 { 1.0 => {} } }", |err| {
            matches!(err, TypeError::NonExhaustiveMatch { .. })
        });
    }

    #[test]
    fn if_let_bare_optional() {
        assert_single_error("fn main() { let x: int? = nil; if let v = x {} }", |err| {
            matches!(err, TypeError::RequiresUnwrappingPattern { .. })
        });
    }

    #[test]
    fn let_else_tail_block_diverges() {
        assert_err_count(
            "fn main() { let x: int? = 1; let v? = x else { { return; } } }",
            0,
        );
    }

    #[test]
    fn let_else_return_isolated() {
        let errors =
            errors("fn f(opt: int?) -> int { let Option.Some(x) = opt else { return 0; } }");
        assert!(
            errors
                .iter()
                .any(|err| matches!(err, TypeError::MissingReturn { .. }))
        );
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
    fn arm_expected_context() {
        let errors = errors(
            "enum State { A, B } fn main() { let s = State.A; let _: int = match s { State.A => 1, State.B => \"hi\" }; }",
        );
        assert!(
            errors
                .iter()
                .any(|err| matches!(err, TypeError::MatchArmTypeMismatch { .. }))
        );
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

mod enum_unit_patterns {
    use super::*;

    #[test]
    fn if_let() {
        assert_ty(
            "enum State { A, B } fn main() { let s = State.A; if let State.A = s { 1 } else { 2 }; }",
            Type::Int,
        );
    }

    #[test]
    fn while_let() {
        assert_err_count(
            "enum State { A, B } fn main() { let s = State.A; while let State.A = s { break; } }",
            0,
        );
    }

    #[test]
    fn inferred_needs_expected() {
        assert_single_error("fn main() { let .A = 1; }", |err| {
            matches!(err, TypeError::CannotInferEnum { .. })
        });
    }
}

mod enum_payload_patterns {
    use super::*;

    #[test]
    fn tuple_arity() {
        assert_single_error(
            "enum Message { Data(int) } fn main() { let m = Message.Data(1); match m { Message.Data(a, b) => a }; }",
            |err| {
                matches!(
                    err,
                    TypeError::WrongArgCount {
                        expected: 1,
                        found: 2,
                        ..
                    }
                )
            },
        );
    }

    #[test]
    fn struct_pattern_explicit_binding() {
        assert_ty(
            "enum Event { Move { dx: int, dy: int }, Stop } fn main() { let e = Event.Move { dx: 1, dy: 2 }; match e { Event.Move { dx: x, dy: y } => x + y, Event.Stop => 0 }; }",
            Type::Int,
        );
    }
}

mod unsupported_forms {
    use super::*;

    #[test]
    fn struct_pattern() {
        assert_err("fn main() { let _ = match 1 { Point { x, y } => 1 }; }");
    }
}
