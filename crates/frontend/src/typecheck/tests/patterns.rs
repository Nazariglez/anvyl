use super::support::{assert_err, assert_err_count, assert_single_error, assert_ty, errors};
use crate::{
    ast::{Ident, Type},
    typecheck::{TypeError, VariantShape},
};

mod binding_patterns {
    use super::*;

    #[test]
    fn let_tuple_ok() {
        assert_ty("fn main() { let (x, y) = (1, true); x; }", Type::Int);
    }

    #[test]
    fn let_tuple_yields_bool() {
        assert_ty("fn main() { let (x, y) = (1, true); y; }", Type::Bool);
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
    fn let_literal_false_ok() {
        assert_err_count("fn main() { let false = false; }", 0);
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
    fn tuple_ok() {
        assert_ty(
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
        assert_ty(
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
        assert_ty("fn main() { if let true = true { 1 }; }", Type::Void);
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
    fn wildcard() {
        assert_ty("fn main() { let _ = match 1 { _ => 42 }; }", Type::Int);
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
    fn tuple_ok() {
        assert_ty(
            "fn main() { let _ = match (1, true) { (a, b) => a }; }",
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
    fn bool_exhaustive() {
        assert_ty(
            "fn main() { let _ = match true { true => 1, false => 0 }; }",
            Type::Int,
        );
    }

    #[test]
    fn bool_non_exhaustive() {
        assert_single_error("fn main() { match true { true => {} } }", |err| {
            matches!(err, TypeError::NonExhaustiveMatch { .. })
        });
    }

    #[test]
    fn int_requires_catch_all() {
        assert_single_error("fn main() { match 1 { 1 => {} } }", |err| {
            matches!(err, TypeError::NonExhaustiveMatch { .. })
        });
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
    fn struct_scrutinee_rejected() {
        assert_single_error(
            "struct Point { x: int } fn main() { let p = Point { x: 1 }; match p { _ => {} } }",
            |err| matches!(err, TypeError::UnsupportedMatchScrutinee { .. }),
        );
    }

    #[test]
    fn invalid_literal_pattern() {
        assert_single_error("fn main() { match 1 { true => {}, _ => {} } }", |err| {
            matches!(err, TypeError::InvalidLiteralPattern { .. })
        });
    }

    #[test]
    fn optional_on_plain_type() {
        assert_single_error("fn main() { match 1 { x? => {}, _ => {} } }", |err| {
            matches!(err, TypeError::OptionalPatternOnNonOptional { .. })
        });
    }

    #[test]
    fn if_let_bare_optional() {
        assert_single_error("fn main() { let x: int? = nil; if let v = x {} }", |err| {
            matches!(err, TypeError::RequiresUnwrappingPattern { .. })
        });
    }

    #[test]
    fn let_else_binds() {
        assert_ty(
            "enum Option<T> { Some(T), None } fn main() { let opt: int? = 1; let Option.Some(x) = opt else { return; } x; }",
            Type::Int,
        );
    }

    #[test]
    fn let_else_tail_block_diverges() {
        assert_err_count(
            "fn main() { let x: int? = 1; let v? = x else { { return; } } }",
            0,
        );
    }

    #[test]
    fn let_else_irrefutable() {
        assert_single_error("fn main() { let x = 1 else { return; } }", |err| {
            matches!(err, TypeError::IrrefutableLetElse { .. })
        });
    }

    #[test]
    fn let_else_return_isolated() {
        let errors = errors(
            "enum Option<T> { Some(T), None } fn f(opt: int?) -> int { let Option.Some(x) = opt else { return 0; } }",
        );
        assert!(
            errors
                .iter()
                .any(|err| matches!(err, TypeError::MissingReturn { .. }))
        );
    }

    #[test]
    fn nested_optional() {
        let errors = errors("fn main() { let x: int? = nil; match x { (v?)? => {} } }");
        assert!(
            errors
                .iter()
                .any(|err| matches!(err, TypeError::NestedOptionalPattern { .. }))
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
    fn arm_expected_context() {
        let errors = errors(
            "enum State { A, B } fn main() { let s = State.A; let _: int = match s { State.A => 1, State.B => \"hi\" }; }",
        );
        assert!(
            errors
                .iter()
                .any(|err| { matches!(err, TypeError::MatchArmTypeMismatch { .. }) })
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

    #[test]
    fn enum_unit() {
        assert_ty(
            "enum State { A, B } fn main() { let s = State.A; match s { State.A => 1, State.B => 2 }; }",
            Type::Int,
        );
    }

    #[test]
    fn inferred_enum_unit() {
        assert_ty(
            "enum State { A, B } fn main() { let s = State.A; match s { .A => 1, .B => 2 }; }",
            Type::Int,
        );
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
            matches!(err, TypeError::CannotInferType { .. })
        });
    }

    #[test]
    fn wrong_owner() {
        assert_single_error(
            "enum A { X } enum B { X } fn main() { let b = B.X; match b { A.X => 1 }; }",
            |err| matches!(err, TypeError::EnumPatternTypeMismatch { .. }),
        );
    }

    #[test]
    fn unknown_variant() {
        assert_single_error(
            "enum State { A } fn main() { let s = State.A; match s { State.B => 1 }; }",
            |err| matches!(err, TypeError::UnknownEnumVariant { variant, .. } if *variant == Ident::new("B")),
        );
    }
}

mod enum_payload_patterns {
    use super::*;

    #[test]
    fn tuple_binds_payload() {
        assert_ty(
            "enum Message { Data(int), Empty } fn main() { let m = Message.Data(1); match m { Message.Data(x) => x, Message.Empty => 0 }; }",
            Type::Int,
        );
    }

    #[test]
    fn generic_tuple_payload() {
        assert_ty(
            "enum Box<T> { Value(T), Empty } fn main() { let b: Box<string> = Box.Value(\"x\"); match b { Box.Value(x) => x, Box.Empty => \"\" }; }",
            Type::String,
        );
    }

    #[test]
    fn nested_tuple() {
        assert_ty(
            "enum Option<T> { Some(T), None } enum Wrap { Value(Option<int>) } fn main() { let w = Wrap.Value(Option.Some(1)); match w { Wrap.Value(Option.Some(x)) => x, Wrap.Value(Option.None) => 0 }; }",
            Type::Int,
        );
    }

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
    fn tuple_on_unit() {
        assert_single_error(
            "enum Message { Empty } fn main() { let m = Message.Empty; match m { Message.Empty(x) => x }; }",
            |err| {
                matches!(
                    err,
                    TypeError::EnumVariantShapeMismatch {
                        expected: VariantShape::Tuple,
                        ..
                    }
                )
            },
        );
    }

    #[test]
    fn struct_pattern_binds_fields() {
        assert_ty(
            "enum Event { Move { dx: int, dy: int }, Stop } fn main() { let e = Event.Move { dx: 1, dy: 2 }; match e { Event.Move { dx, dy } => dx + dy, Event.Stop => 0 }; }",
            Type::Int,
        );
    }

    #[test]
    fn struct_pattern_explicit_binding() {
        assert_ty(
            "enum Event { Move { dx: int, dy: int }, Stop } fn main() { let e = Event.Move { dx: 1, dy: 2 }; match e { Event.Move { dx: x, dy: y } => x + y, Event.Stop => 0 }; }",
            Type::Int,
        );
    }

    #[test]
    fn struct_pattern_rest() {
        assert_ty(
            "enum Event { Move { dx: int, dy: int }, Stop } fn main() { let e = Event.Move { dx: 1, dy: 2 }; match e { Event.Move { dx, .. } => dx, Event.Stop => 0 }; }",
            Type::Int,
        );
    }

    #[test]
    fn struct_missing_field() {
        assert_single_error(
            "enum Event { Move { dx: int, dy: int } } fn main() { let e = Event.Move { dx: 1, dy: 2 }; match e { Event.Move { dx } => dx }; }",
            |err| matches!(err, TypeError::MissingVariantField { field, .. } if *field == Ident::new("dy")),
        );
    }

    #[test]
    fn enum_non_exhaustive() {
        assert_single_error(
            "enum Color { Red, Green } fn main() { let c = Color.Red; match c { Color.Red => {} } }",
            |err| matches!(err, TypeError::NonExhaustiveMatch { .. }),
        );
    }

    #[test]
    fn option_none_non_exhaustive() {
        assert_single_error(
            "enum Option<T> { Some(T), None } fn main() { let x: int? = nil; match x { Option.None => {} } }",
            |err| matches!(err, TypeError::NonExhaustiveMatch { .. }),
        );
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
