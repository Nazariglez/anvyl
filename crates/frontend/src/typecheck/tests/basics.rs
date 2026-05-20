use super::support::{
    assert_err, assert_err_count, assert_single_error, assert_ty, assert_ty_mods, assert_ty_named,
    assert_typecheck_closed, check, check_mods, check_named, core_option, errors,
};
use crate::{
    ast::{
        ArrayLen, ConstArg, ConstValue, EscapeMode, FuncParam, GenericArg, Ident, NominalKind,
        ReturnSpec, Type,
    },
    span::SourceSpan,
    typecheck::{ArityError, DeclError, TypeError, type_closure_facts},
};

mod storage {
    use crate::{
        ast::{Ident, Program, Type},
        test_support::empty_resolved,
        typecheck::{DeclarationIndex, SemanticLocalId, TypeChecker, TypecheckConfig},
    };

    trait TypeCheckerTestExt {
        fn set_local_type(&mut self, id: SemanticLocalId, ty: Type);
    }

    impl TypeCheckerTestExt for TypeChecker {
        fn set_local_type(&mut self, id: SemanticLocalId, ty: Type) {
            self.solver.set_local_type_from_type(id, &ty);
        }
    }

    #[test]
    fn local_type_cell_update() {
        let program = Program { stmts: vec![] };
        let resolved = empty_resolved();
        let decls = DeclarationIndex::from_root_and_modules(
            &program,
            &resolved,
            &crate::externs::RawExterns::default(),
        );
        let mut tc = TypeChecker::new(
            decls,
            crate::externs::catalog::ExternCatalog::default(),
            TypecheckConfig::default(),
        );
        let name = Ident::new("x");
        tc.push_scope();
        tc.define(name, Type::Int, false);
        let type_id = tc.lookup(name).expect("local binding").type_id;

        tc.set_local_type(type_id, Type::String);

        let info = tc.lookup(name).expect("local binding");
        assert_eq!(info.type_id, type_id);
        assert_eq!(tc.solver.local_type_to_type(info.type_id), Type::String);
    }
}

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
    fn implicit_return_mismatch() {
        assert_err_count("fn f() -> int { true }", 1);
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
    fn compound_assignment_uses_binary_operator_type() {
        check("fn main() { var x = \"score: \"; x += 1; }").expect("typecheck failed");
    }

    #[test]
    fn compound_assignment_rejects_invalid_binary_operand() {
        assert_single_error("fn main() { var x = 1; x += true; }", |error| {
            matches!(error, TypeError::InvalidOperand { .. })
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
    fn branch_join_same_type() {
        assert_ty(
            "fn main(cond: bool) { if cond { 1 } else { 2 }; }",
            Type::Int,
        );
    }

    #[test]
    fn branch_join_mismatch() {
        assert_err_count("fn main(cond: bool) { if cond { 1 } else { \"x\" }; }", 1);
    }

    #[test]
    fn ternary_tail_expression() {
        assert_ty("fn f(cond: bool) -> int { cond ? 1 : 2 }", Type::Int);
    }

    #[test]
    fn ternary_nil_uses_binding_context() {
        assert_ty(
            "fn main(cond: bool) { let x: int? = cond ? nil : 1; x; }",
            core_option(Type::Int),
        );
    }

    #[test]
    fn annotated_if_without_else_value() {
        let errors = errors("fn main() { let x: int = if true { 1 }; }");
        assert!(
            errors
                .iter()
                .any(|err| matches!(err, TypeError::IfWithoutElseValue { .. }))
        );
    }

    #[test]
    fn nil_shared_once() {
        assert_single_error("fn main() { let x = nil; x; }", |err| {
            matches!(err, TypeError::CannotInferType { .. })
        });
    }

    #[test]
    fn nil_branch_uses_return_context() {
        assert_ty(
            "fn main(cond: bool) -> int? { if cond { nil } else { 1 } }",
            core_option(Type::Int),
        );
    }

    #[test]
    fn nil_branch_uses_binding_context() {
        assert_ty(
            "fn main(cond: bool) { let x: int? = if cond { nil } else { 1 }; x; }",
            core_option(Type::Int),
        );
    }

    #[test]
    fn nil_branch_plain_join_err() {
        assert_err_count(
            "fn main(cond: bool) { let x = if cond { nil } else { 1 }; }",
            1,
        );
    }

    #[test]
    fn undefined_no_infer_cascade() {
        let Err(errors) = check("fn main() { missing; }") else {
            panic!("expected undefined variable");
        };
        assert_eq!(errors.len(), 1);
        assert!(matches!(errors[0], TypeError::UndefinedVariable { .. }));
    }

    #[test]
    fn undefined_binding() {
        assert_single_error("fn main() { let x = missing; x; }", |err| {
            matches!(err, TypeError::UndefinedVariable { .. })
        });
    }

    #[test]
    fn branch_child_error() {
        assert_single_error(
            "fn main(cond: bool) { if cond { missing } else { nil }; }",
            |err| matches!(err, TypeError::UndefinedVariable { .. }),
        );
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
        let ty = Type::nominal(
            NominalKind::Struct,
            Ident::new("Box"),
            vec![Type::Infer],
            vec![],
            None,
        );
        assert!(type_closure_facts(&ty).infer.contains_type);
    }
}

mod consts {
    use super::*;

    #[test]
    fn evaluated() {
        check("const X = 1 + 2; fn main() { let xs: [int; X] = [1, 2, 3]; }")
            .expect("typecheck failed");
    }

    #[test]
    fn forward_ref() {
        assert_ty("const A = B + 1; const B = 2; fn main() { A; }", Type::Int);
    }

    #[test]
    fn local() {
        assert_ty("fn main() { const X = 1 + 2; X; }", Type::Int);
    }

    #[test]
    fn local_shadow() {
        assert_ty(
            "const X = 1; fn main() { const X = \"x\"; X; }",
            Type::String,
        );
    }

    #[test]
    fn imported() {
        assert_ty_mods(
            "import gamekit { SIZE }; fn main() { SIZE; }",
            "pub const SIZE = 4;",
            Type::Int,
        );
    }

    #[test]
    fn imported_forward_ref() {
        assert_ty_mods(
            "import gamekit { SIZE }; fn main() { SIZE; }",
            "pub const SIZE = BASE + 1; const BASE = 3;",
            Type::Int,
        );
    }

    #[test]
    fn module_private_err() {
        let result = check_mods(
            "import gamekit as gk; const X = gk.SECRET; fn main() { X; }",
            "const SECRET = 1;",
        );
        assert!(result.is_err(), "expected private module const error");
    }

    #[test]
    fn bool_short_circuit() {
        check(
            "const X = false && (1 / 0 == 0); const N = X ? 1 : 2; fn main() { let xs: [int; N] = [1, 2]; }",
        )
        .expect("typecheck failed");
    }
}

mod nominals {
    use super::*;

    fn array(elem: Type, len: usize) -> Type {
        Type::Array {
            elem: Box::new(elem),
            len: ArrayLen::Fixed(len),
        }
    }

    fn arg(value: i64) -> ConstArg {
        ConstArg::Value(ConstValue::Int(value))
    }

    fn nominal(
        kind: NominalKind,
        name: &str,
        type_args: Vec<Type>,
        const_args: Vec<ConstArg>,
    ) -> Type {
        Type::nominal(kind, Ident::new(name), type_args, const_args, None)
    }

    fn fixed(len: i64) -> Type {
        nominal(
            NominalKind::Struct,
            "FixedBuf",
            vec![Type::Int],
            vec![arg(len)],
        )
    }

    fn dataref(len: i64) -> Type {
        nominal(
            NominalKind::DataRef,
            "FixedBuf",
            vec![Type::Int],
            vec![arg(len)],
        )
    }

    fn packet(len: i64) -> Type {
        nominal(NominalKind::Enum, "Packet", vec![Type::Int], vec![arg(len)])
    }

    fn maybe(inner: Type) -> Type {
        nominal(NominalKind::Enum, "Maybe", vec![inner], vec![])
    }

    fn struct_ty(name: &str, type_args: Vec<Type>, const_args: Vec<ConstArg>) -> Type {
        nominal(NominalKind::Struct, name, type_args, const_args)
    }

    fn dataref_ty(name: &str, type_args: Vec<Type>) -> Type {
        nominal(NominalKind::DataRef, name, type_args, vec![])
    }

    fn box_struct(inner: Type) -> Type {
        struct_ty("Box", vec![inner], vec![])
    }

    fn buf(src: &str) -> String {
        format!("struct FixedBuf<T, N: int> {{ data: [T; N] }} {src}")
    }

    fn ref_buf(src: &str) -> String {
        format!("dataref FixedBuf<T, N: int> {{ data: [T; N] }} {src}")
    }

    fn packets(src: &str) -> String {
        format!("enum Packet<T, N: int> {{ Inline([T; N]) }} {src}")
    }

    #[test]
    fn imported() {
        assert_ty_named(
            "import bufs; fn main(x: bufs.FixedBuf<int, 3>) { x.data; }",
            &[("bufs", "pub struct FixedBuf<T, N: int> { data: [T; N] }")],
            array(Type::Int, 3),
        );
    }

    #[test]
    fn dataref_arg() {
        assert_ty(
            &ref_buf("fn main(x: FixedBuf<int, 3>) -> FixedBuf<int, 3> { x }"),
            dataref(3),
        );
    }

    #[test]
    fn enum_arg() {
        assert_ty(
            &packets("fn main(x: Packet<int, 3>) -> Packet<int, 3> { x }"),
            packet(3),
        );
    }

    #[test]
    fn order_err() {
        assert_single_error(&buf("fn f(x: FixedBuf<3, int>) {} fn main() {}"), |err| {
            matches!(
                err,
                TypeError::GenericArgKindMismatch {
                    expected: "type",
                    ..
                }
            )
        });
    }

    #[test]
    fn arity_err() {
        let Err(errors) = check(&buf("fn f(x: FixedBuf<int>) {} fn main() {}")) else {
            panic!("expected arity error");
        };
        assert!(matches!(
            errors.first(),
            Some(TypeError::GenericArity(ArityError::TypeArgs {
                expected: 2,
                found: 1,
            }))
        ));
    }

    #[test]
    fn too_many_args_err() {
        assert_single_error(
            &buf("fn f(x: FixedBuf<int, 3, 4>) {} fn main() {}"),
            |err| {
                matches!(
                    err,
                    TypeError::GenericArity(ArityError::TypeArgs {
                        expected: 2,
                        found: 3,
                    })
                )
            },
        );
    }

    #[test]
    fn non_bare_type_const_arg_kind_err() {
        assert_single_error(
            &buf("fn f(x: FixedBuf<int, [int]>) {} fn main() {}"),
            |err| {
                matches!(
                    err,
                    TypeError::GenericArgKindMismatch {
                        expected: "const",
                        ..
                    }
                )
            },
        );
    }

    #[test]
    fn unknown_const_arg_err() {
        assert_single_error(
            &buf("fn f(x: FixedBuf<int, N>) {} fn main() {}"),
            |err| matches!(err, TypeError::UnknownConst { name, .. } if *name == Ident::new("N")),
        );
    }

    #[test]
    fn kind_err() {
        let Err(errors) = check(&buf("fn f(x: FixedBuf<int, true>) {} fn main() {}")) else {
            panic!("expected const kind error");
        };
        assert!(matches!(
            errors.first(),
            Some(TypeError::ExpectedIntConst {
                found: Type::Bool,
                ..
            })
        ));
    }

    #[test]
    fn bool_const_arg_err() {
        assert_err("struct Flag<N: int> {} fn main(x: Flag<true>) -> Flag<true> { x }");
    }

    #[test]
    fn dataref_arity_err() {
        assert_err(&ref_buf("fn f(x: FixedBuf<int>) {} fn main() {}"));
    }

    #[test]
    fn enum_arity_err() {
        assert_err(&packets("fn f(x: Packet<int>) {} fn main() {}"));
    }

    #[test]
    fn const_name() {
        assert_ty(
            "const CAP = 3; struct FixedBuf<T, N: int> { data: [T; N] } fn main(x: FixedBuf<int, CAP>) -> FixedBuf<int, 3> { x }",
            fixed(3),
        );
    }

    #[test]
    fn const_param() {
        assert_ty(
            &buf(
                "fn f<T, N: int>(x: FixedBuf<T, N>) -> int { N } fn main(x: FixedBuf<int, 3>) -> int { f(x) }",
            ),
            Type::Int,
        );
    }

    #[test]
    fn same() {
        assert_ty(
            &buf("fn main(x: FixedBuf<int, 3>) { let y: FixedBuf<int, 3> = x; y; }"),
            fixed(3),
        );
    }

    #[test]
    fn const_diff() {
        assert_err(&buf(
            "fn main(x: FixedBuf<int, 3>) { let y: FixedBuf<int, 4> = x; y; }",
        ));
    }

    #[test]
    fn type_diff() {
        assert_err(&buf(
            "fn main(x: FixedBuf<int, 3>) { let y: FixedBuf<float, 3> = x; y; }",
        ));
    }

    #[test]
    fn first() {
        assert_ty(
            "struct FixedBuf<T, N: int> { head: T, data: [T; N] } fn first<T, N: int>(buf: FixedBuf<T, N>) -> T { buf.head } fn main() { first(FixedBuf { head: 1, data: [1, 2, 3] }); }",
            Type::Int,
        );
    }

    #[test]
    fn literal_args() {
        assert_ty(
            &buf("fn main() { FixedBuf<int, 3> { data: [1, 2, 3] }; }"),
            fixed(3),
        );
    }

    #[test]
    fn literal_hint() {
        assert_ty(
            &buf("fn main() { let buf: FixedBuf<int, 3> = FixedBuf { data: [1, 2, 3] }; buf; }"),
            fixed(3),
        );
    }

    #[test]
    fn literal_infers_args() {
        assert_ty(
            &buf("fn main() { FixedBuf { data: [1, 2, 3] }; }"),
            fixed(3),
        );
    }

    #[test]
    fn dataref_infer() {
        assert_ty(
            &ref_buf("fn main() { FixedBuf { data: [1, 2, 3] }; }"),
            dataref(3),
        );
    }

    #[test]
    fn field() {
        assert_ty(
            &buf("fn main() { let buf = FixedBuf { data: [1, 2, 3] }; buf.data; }"),
            array(Type::Int, 3),
        );
    }

    #[test]
    fn method() {
        assert_ty(
            "struct FixedBuf<T, N: int> { data: [T; N], fn get(self) -> [T; N] { self.data } } fn main() { let buf = FixedBuf { data: [1, 2, 3] }; buf.get(); }",
            array(Type::Int, 3),
        );
    }

    #[test]
    fn enum_payload() {
        assert_ty(
            &packets("fn main() { Packet.Inline([1, 2, 3]); }"),
            packet(3),
        );
    }

    #[test]
    fn enum_explicit() {
        assert_ty(
            &packets("fn main() { Packet.Inline<int, 3>([1, 2, 3]); }"),
            packet(3),
        );
    }

    #[test]
    fn literal_expected_optional() {
        assert_ty(
            "struct Box<T> { value: T } fn main() { let x: Box<int?> = Box { value: nil }; x; }",
            box_struct(core_option(Type::Int)),
        );
    }

    #[test]
    fn explicit_optional_arg() {
        assert_ty(
            "struct Box<T> { value: T } fn main() { let x = Box<int?> { value: nil }; x; }",
            box_struct(core_option(Type::Int)),
        );
    }

    #[test]
    fn nested_literal_optional() {
        assert_ty(
            "struct Inner<T> { value: T } struct Outer<T> { inner: T } fn main() { let x: Outer<Inner<int?>> = Outer { inner: Inner { value: nil } }; x; }",
            struct_ty(
                "Outer",
                vec![struct_ty("Inner", vec![core_option(Type::Int)], vec![])],
                vec![],
            ),
        );
    }

    #[test]
    fn literal_const_array_hint() {
        assert_ty(
            &buf("fn main() { let x: FixedBuf<int, 3> = FixedBuf { data: [1, 2, 3] }; x; }"),
            fixed(3),
        );
    }

    #[test]
    fn literal_optional_const_array_hint() {
        assert_ty(
            &buf("fn main() { let x: FixedBuf<int?, 2> = FixedBuf { data: [nil, 1] }; x; }"),
            struct_ty("FixedBuf", vec![core_option(Type::Int)], vec![arg(2)]),
        );
    }

    #[test]
    fn dataref_expected_optional() {
        assert_ty(
            "dataref Box<T> { value: T } fn main() { let x: Box<int?> = Box { value: nil }; x; }",
            dataref_ty("Box", vec![core_option(Type::Int)]),
        );
    }

    #[test]
    fn literal_plain_nil_err() {
        assert_err_count(
            "struct Box<T> { value: T } fn main() { let x: Box<int> = Box { value: nil }; }",
            1,
        );
    }

    #[test]
    fn literal_short_array_err() {
        assert_err_count(
            &buf("fn main() { let x: FixedBuf<int, 3> = FixedBuf { data: [1, 2] }; }"),
            1,
        );
    }

    #[test]
    fn explicit_arg_conflict() {
        assert_err_count(
            "struct Box<T> { value: T } fn main() { let x: Box<int> = Box<string> { value: \"x\" }; }",
            1,
        );
    }

    #[test]
    fn tuple_expected_optional() {
        assert_ty(
            "enum Maybe<T> { Some(T), None } fn main() { let x: Maybe<int?> = .Some(nil); x; }",
            maybe(core_option(Type::Int)),
        );
    }

    #[test]
    fn struct_expected_optional() {
        assert_ty(
            "enum Maybe<T> { Pair { value: T }, None } fn main() { let x: Maybe<int?> = .Pair { value: nil }; x; }",
            maybe(core_option(Type::Int)),
        );
    }

    #[test]
    fn unit_expected_enum() {
        assert_ty(
            "enum Maybe<T> { None } fn main() { let x: Maybe<int> = .None; x; }",
            maybe(Type::Int),
        );
    }

    #[test]
    fn return_expected_optional() {
        assert_ty(
            "enum Maybe<T> { Some(T) } fn main() -> Maybe<int?> { .Some(nil) }",
            maybe(core_option(Type::Int)),
        );
    }

    #[test]
    fn array_element_expected_optional() {
        assert_ty(
            "enum Maybe<T> { Some(T) } fn main() { let xs: [Maybe<int?>; 1] = [.Some(nil)]; xs; }",
            array(maybe(core_option(Type::Int)), 1),
        );
    }

    #[test]
    fn plain_nil_payload_err() {
        assert_err_count(
            "enum Maybe<T> { Some(T) } fn main() { let x: Maybe<int> = .Some(nil); }",
            1,
        );
    }

    #[test]
    fn without_context_err() {
        assert_err_count(
            "enum Maybe<T> { Some(T) } fn main() { let x = .Some(1); }",
            1,
        );
    }

    #[test]
    fn unknown_variant_err() {
        assert_err_count(
            "enum Maybe<T> { Some(T) } fn main() { let x: Maybe<int> = .Missing(1); }",
            1,
        );
    }

    #[test]
    fn struct_payload_mismatch_err() {
        assert_err_count(
            "enum Maybe<T> { Pair { value: T } } fn main() { let x: Maybe<int> = .Pair { value: \"x\" }; }",
            1,
        );
    }
}

mod arrays {
    use super::*;

    fn array(elem: Type, len: usize) -> Type {
        Type::Array {
            elem: Box::new(elem),
            len: ArrayLen::Fixed(len),
        }
    }

    #[test]
    fn unknown_const_len_err() {
        assert_single_error(
            "fn main() { let xs: [int; N] = []; }",
            |err| matches!(err, TypeError::UnknownConst { name, .. } if *name == Ident::new("N")),
        );
    }

    #[test]
    fn bool_const_len_err() {
        assert_single_error(
            "const B = true; fn main() { let xs: [int; B] = []; }",
            |err| {
                matches!(
                    err,
                    TypeError::ExpectedIntConst {
                        found: Type::Bool,
                        ..
                    }
                )
            },
        );
    }

    #[test]
    fn negative_const_len_err() {
        assert_single_error(
            "const N = -1; fn main() { let xs: [int; N] = []; }",
            |err| matches!(err, TypeError::NegativeArrayLength { value: -1, .. }),
        );
    }

    #[test]
    fn nominal_const_arg_name_normalizes() {
        assert_ty(
            "const N = 3; struct Buf<N: int> {} fn main(x: Buf<N>) -> Buf<3> { x }",
            Type::nominal(
                NominalKind::Struct,
                Ident::new("Buf"),
                vec![],
                vec![ConstArg::Value(ConstValue::Int(3))],
                None,
            ),
        );
    }

    #[test]
    fn empty_context() {
        assert_ty(
            "fn main() { let xs: [int; 0] = []; xs; }",
            array(Type::Int, 0),
        );
    }

    #[test]
    fn nested_optional_context() {
        assert_ty(
            "fn main() { let xs: [[int?; 1]; 1] = [[nil]]; xs; }",
            array(array(core_option(Type::Int), 1), 1),
        );
    }

    #[test]
    fn empty_no_context_err() {
        assert_single_error("fn main() { let xs = []; }", |err| {
            matches!(err, TypeError::CannotInferType { .. })
        });
    }

    #[test]
    fn nil_plain_array_element_err() {
        assert_err_count("fn main() { let xs: [int; 1] = [nil]; }", 1);
    }

    #[test]
    fn nested_imported_const_len() {
        assert_ty_mods(
            "import gamekit { W, H }; fn main() { let grid: [[int; W]; H] = [[0; W]; H]; grid; }",
            "pub const W = 4; pub const H = 3;",
            array(array(Type::Int, 4), 3),
        );
    }

    #[test]
    fn fill_non_int_len_err() {
        assert_err("const N = \"x\"; fn main() { [0; N]; }");
    }

    #[test]
    fn fill_negative_len_err() {
        assert_err("fn main() { [0; -1]; }");
    }
}

mod tuples {
    use super::*;

    #[test]
    fn optional_context() {
        assert_ty(
            "fn main() { let x: (int, string?) = (1, nil); x; }",
            Type::Tuple(vec![Type::Int, core_option(Type::String)]),
        );
    }

    #[test]
    fn nested_optional_context() {
        assert_ty(
            "fn main() { let x: (int?, (string, bool?)) = (nil, (\"s\", nil)); x; }",
            Type::Tuple(vec![
                core_option(Type::Int),
                Type::Tuple(vec![Type::String, core_option(Type::Bool)]),
            ]),
        );
    }

    #[test]
    fn branch_context() {
        assert_ty(
            "fn main(cond: bool) { let x: (int, int?) = if cond { (1, nil) } else { (2, 3) }; x; }",
            Type::Tuple(vec![Type::Int, core_option(Type::Int)]),
        );
    }

    #[test]
    fn tuple_mismatch_err() {
        assert_err_count("fn main() { let x: (int, string) = (1, 2); }", 1);
    }

    #[test]
    fn tuple_arity_err() {
        assert_err_count("fn main() { let x: (int, int?) = (1, 2, 3); }", 1);
    }

    #[test]
    fn nil_no_context_err() {
        assert_err("fn main() { let x = (nil, nil); }");
    }
}

mod bindings {
    use super::*;

    #[test]
    fn let_unknown_annotation_err() {
        assert_single_error(
            "fn main() { let x: Missing = 1; }",
            |err| matches!(err, TypeError::UnknownType { qualifier: None, name, .. } if *name == Ident::new("Missing")),
        );
    }

    #[test]
    fn let_mismatch() {
        assert_err("fn main() { let x: int = \"hi\"; }");
    }

    #[test]
    fn var_reassign() {
        assert_err("fn main() { var x = 1; x = \"hi\"; }");
    }

    #[test]
    fn undefined_var() {
        assert_err("fn main() { x; }");
    }
}

mod functions {
    use super::*;

    #[test]
    fn fn_unknown_param_type_err() {
        assert_single_error(
            "fn f(x: Missing) {}",
            |err| matches!(err, TypeError::Decl(DeclError::UnknownType { qualifier: None, name, .. }) if *name == Ident::new("Missing")),
        );
    }

    #[test]
    fn struct_unknown_field_type_err() {
        assert_single_error(
            "struct Box { value: Missing }",
            |err| matches!(err, TypeError::Decl(DeclError::UnknownType { qualifier: None, name, .. }) if *name == Ident::new("Missing")),
        );
    }

    #[test]
    fn enum_unknown_payload_type_err() {
        assert_single_error(
            "enum E { A(Missing) }",
            |err| matches!(err, TypeError::Decl(DeclError::UnknownType { qualifier: None, name, .. }) if *name == Ident::new("Missing")),
        );
    }

    #[test]
    fn extern_unknown_param_type_err() {
        assert_single_error(
            "extern fn host(x: Missing);",
            |err| matches!(err, TypeError::ExternCatalog(crate::externs::catalog::ExternCatalogError::UnknownType { name, .. }) if *name == Ident::new("Missing")),
        );
    }

    #[test]
    fn generic_param_remains_valid() {
        assert_ty("fn id<T>(x: T) -> T { x } fn main() { id(1); }", Type::Int);
    }

    #[test]
    fn duplicate_function_decl_err() {
        assert_err("fn f() {} fn f() {}");
    }

    #[test]
    fn duplicate_function_const_err() {
        assert_err("fn dup() {} const dup = 1;");
    }

    #[test]
    fn duplicate_extern_function_err() {
        assert_err("extern fn host(); fn host() {}");
    }

    #[test]
    fn duplicate_params() {
        for source in [
            "fn f<T, T>(x: T) {}",
            "fn f<N: int, N: int>(x: int) {}",
            "fn f<T, T: int>(x: int) {}",
            "struct Box<T, T> { value: T }",
            "enum Option<T, T> { Some(T), None }",
            "extend<T, T> T { fn id(self) -> T { self } }",
        ] {
            assert_err(source);
        }
    }

    #[test]
    fn wrong_nominal_arity_in_decl() {
        assert_single_error(
            "struct Box<T> { value: T } struct Holder { x: Box<int, int> }",
            |err| {
                matches!(
                    err,
                    TypeError::GenericArity(ArityError::TypeArgs {
                        expected: 1,
                        found: 2,
                    })
                )
            },
        );
    }

    #[test]
    fn wrong_nominal_arity_in_signature() {
        assert_single_error(
            "struct Box<T> { value: T } fn f<U>(x: Box<int, int>) {}",
            |err| {
                matches!(
                    err,
                    TypeError::GenericArity(ArityError::TypeArgs {
                        expected: 1,
                        found: 2,
                    })
                )
            },
        );
    }

    #[test]
    fn field_no_export_fallback() {
        let result = check_named(
            "struct Holder { x: Item }",
            &[("alpha", "pub struct Item { value: int }")],
        );
        let Err(errors) = result else {
            panic!("expected unknown type error");
        };
        assert!(errors.iter().any(|err| matches!(
            err,
            TypeError::Decl(DeclError::UnknownType { qualifier: None, name, .. })
                if *name == Ident::new("Item")
        )));
    }

    #[test]
    fn unknown_type_arg_in_payload() {
        assert_single_error(
            "struct Box<T> { value: T } enum E { A(Box<Missing>) }",
            |err| matches!(err, TypeError::Decl(DeclError::UnknownType { qualifier: None, name, .. }) if *name == Ident::new("Missing")),
        );
    }

    #[test]
    fn body_annotation_type_param() {
        assert_ty(
            "fn id<T>(x: T) -> T { let y: T = x; y } fn main() { id(1); }",
            Type::Int,
        );
    }

    #[test]
    fn const_param_as_type_err() {
        assert_single_error(
            "fn f<N: int>(x: N) {}",
            |err| matches!(err, TypeError::Decl(DeclError::UnknownType { qualifier: None, name, .. }) if *name == Ident::new("N")),
        );
    }

    #[test]
    fn type_param_rejected_as_const_arg() {
        assert_single_error(
            "struct FixedBuf<T, N: int> { data: [T; N] } fn f<T>(x: FixedBuf<int, T>) {}",
            |err| matches!(err, TypeError::Decl(DeclError::UnknownType { qualifier: None, name, .. }) if *name == Ident::new("T")),
        );
    }

    #[test]
    fn unknown_array_len_in_field() {
        assert_single_error(
            "struct Holder { x: [int; Missing] }",
            |err| matches!(err, TypeError::UnknownConst { name, .. } if *name == Ident::new("Missing")),
        );
    }

    #[test]
    fn unknown_const_arg_in_field() {
        assert_single_error(
            "struct FixedBuf<T, N: int> { data: [T; N] } struct Holder { x: FixedBuf<int, Missing> }",
            |err| matches!(err, TypeError::UnknownConst { name, .. } if *name == Ident::new("Missing")),
        );
    }

    #[test]
    fn bool_const_arg_in_field() {
        assert_single_error(
            "struct FixedBuf<T, N: int> { data: [T; N] } struct Holder { x: FixedBuf<int, true> }",
            |err| {
                matches!(
                    err,
                    TypeError::ExpectedIntConst {
                        found: Type::Bool,
                        ..
                    }
                )
            },
        );
    }

    #[test]
    fn unknown_const_arg_in_signature() {
        assert_single_error(
            "struct FixedBuf<T, N: int> { data: [T; N] } fn f<U>(x: FixedBuf<int, Missing>) {}",
            |err| matches!(err, TypeError::UnknownConst { name, .. } if *name == Ident::new("Missing")),
        );
    }

    #[test]
    fn type_param_const_arg_in_field() {
        assert_single_error(
            "struct FixedBuf<T, N: int> { data: [T; N] } struct Holder<T> { x: FixedBuf<int, T> }",
            |err| matches!(err, TypeError::Decl(DeclError::UnknownType { qualifier: None, name, .. }) if *name == Ident::new("T")),
        );
    }

    #[test]
    fn const_param_valid_in_array_len() {
        assert_err_count("fn f<N: int>(x: [int; N]) {}", 0);
    }

    #[test]
    fn nominal_const_arg_uses_const_param() {
        assert_err_count(
            "struct FixedBuf<T, N: int> { data: [T; N] } fn f<N: int>(x: FixedBuf<int, N>) {}",
            0,
        );
    }
}

mod blocks {
    use super::*;

    #[test]
    fn block_tail() {
        assert_ty("fn main() { { 1 }; }", Type::Int);
    }

    #[test]
    fn block_void() {
        assert_ty("fn main() { { }; }", Type::Void);
    }
}
