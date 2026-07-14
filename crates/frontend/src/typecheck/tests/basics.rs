use super::support::{assert_typecheck_closed, check, errors};
use crate::{
    ast::{ArrayLen, EscapeMode, FuncParam, GenericArg, Ident, NominalKind, ReturnSpec, Type},
    span::SourceSpan,
    typecheck::{TypeError, type_closure_facts},
};

mod constraints {
    use super::*;

    fn assert_error_span(
        source: &str,
        needle: &str,
        error_span: impl FnOnce(&TypeError) -> Option<SourceSpan>,
    ) {
        let errors = errors(source);
        assert_eq!(errors.len(), 1, "unexpected errors: {errors:?}");
        assert_eq!(
            error_span(&errors[0]).map(SourceSpan::start),
            Some(source.find(needle).expect("missing span needle"))
        );
    }

    fn type_mismatch_span(error: &TypeError) -> Option<SourceSpan> {
        let TypeError::TypeMismatch { span, .. } = error else {
            return None;
        };
        *span
    }

    #[test]
    fn annotated_binding_mismatch() {
        assert_error_span(
            "fn main() { let x: int = true; }",
            "true",
            type_mismatch_span,
        );
    }

    #[test]
    fn explicit_return_mismatch() {
        assert_error_span("fn f() -> int { return true; }", "true", type_mismatch_span);
    }

    #[test]
    fn assignment_mismatch() {
        assert_error_span(
            "fn main() { var x = 1; x = true; }",
            "true",
            type_mismatch_span,
        );
    }

    #[test]
    fn immutable_assignment_blames_target() {
        assert_error_span("fn main() { let x = 1; x = 2; }", "x = 2", |error| {
            let TypeError::ImmutableAssignment { span, .. } = error else {
                return None;
            };
            *span
        });
    }

    #[test]
    fn compound_assignment_rejects_non_assignable_binary_result() {
        assert_error_span(
            "fn main() { var x = 1; x += \" apples\"; }",
            "\" apples\"",
            type_mismatch_span,
        );
    }

    #[test]
    fn call_argument_mismatch_blames_argument() {
        assert_error_span(
            "fn takes(x: int) {} fn main() { takes(true); }",
            "true",
            type_mismatch_span,
        );
    }

    #[test]
    fn field_initializer_mismatch_blames_value() {
        assert_error_span(
            "struct Point { x: int } fn main() { Point { x: true }; }",
            "true",
            type_mismatch_span,
        );
    }

    #[test]
    fn index_type_mismatch_blames_index() {
        assert_error_span("fn main() { let xs = [1]; xs[true]; }", "true", |error| {
            let TypeError::IndexNotInt { span, .. } = error else {
                return None;
            };
            *span
        });
    }

    #[test]
    fn no_infer_leak() {
        let result = check("fn main(cond: bool) { let x = if cond { 1 } else { 2 }; x; }")
            .expect("typecheck failed");
        assert_typecheck_closed(&result);
    }

    #[test]
    fn array_len_infer_is_closed() {
        let ty = Type::Array {
            elem: Box::new(Type::Int),
            len: ArrayLen::Infer,
        };
        assert!(!type_closure_facts(&ty).infer.contains_type);
    }

    #[test]
    fn nested_func_ret_infer_guard() {
        let ty = Type::Func {
            params: vec![FuncParam::new(
                Type::Int,
                false,
                false,
                EscapeMode::NonEscaping,
            )],
            ret: Box::new(ReturnSpec::value(Type::Tuple(vec![Type::Infer]))),
        };
        assert!(type_closure_facts(&ty).infer.contains_type);
    }

    #[test]
    fn unresolved_arg_infer_guard() {
        let ty = Type::UnresolvedNominal {
            qualifier: None,
            name: Ident::new("Box"),
            generic_args: vec![GenericArg::Type(Type::Infer)],
        };
        assert!(type_closure_facts(&ty).infer.contains_type);
    }

    #[test]
    fn nominal_type_arg_infer_guard() {
        let ty = crate::test_support::test_nominal_type(
            crate::test_support::nominal_test_source_id(),
            60,
            NominalKind::Struct,
            Ident::new("Box"),
            vec![Type::Infer],
            vec![],
            None,
        );
        assert!(type_closure_facts(&ty).infer.contains_type);
    }
}
