use super::support::{
    assert_err, assert_err_count, assert_no_infer_vars_in_result, assert_single_error, assert_type,
    assert_type_with_modules, assert_type_with_named_modules, typecheck, typecheck_with_modules,
};
use crate::{
    ast::{ArrayLen, ConstArg, ConstValue, FuncParam, GenericArg, Ident, NominalKind, Type},
    typecheck::{ArityError, ModuleScope, TypeError, type_contains_infer},
};

mod storage {
    use crate::{
        ast::{Ident, Program, Type},
        typecheck::{DeclarationIndex, TypeChecker},
    };

    #[test]
    fn local_type_cell_update() {
        let decls = DeclarationIndex::from_root(&Program { stmts: vec![] });
        let mut tc = TypeChecker::new(decls);
        let name = Ident::new("x");
        tc.push_scope();
        tc.define(name, Type::Int, false);
        let type_id = tc.lookup(name).expect("local binding").type_id;

        tc.set_local_type(type_id, Type::String);

        let info = tc.lookup(name).expect("local binding");
        assert_eq!(info.type_id, type_id);
        assert_eq!(tc.lookup_type(name), Some(Type::String));
    }
}

mod constraints {
    use super::*;
    #[test]
    fn annotated_binding_mismatch() {
        assert_err_count("fn main() { let x: int = true; }", 1);
    }

    #[test]
    fn explicit_return_mismatch() {
        assert_err_count("fn f() -> int { return true; }", 1);
    }

    #[test]
    fn implicit_return_mismatch() {
        assert_err_count("fn f() -> int { true }", 1);
    }

    #[test]
    fn assignment_mismatch() {
        assert_err_count("fn main() { var x = 1; x = true; }", 1);
    }

    #[test]
    fn branch_join_same_type() {
        assert_type(
            "fn main(cond: bool) { if cond { 1 } else { 2 }; }",
            Type::Int,
        );
    }

    #[test]
    fn branch_join_mismatch() {
        assert_err_count("fn main(cond: bool) { if cond { 1 } else { \"x\" }; }", 1);
    }

    #[test]
    fn nil_unresolved_once() {
        let Err(errors) = typecheck("fn main() { let x = nil; }") else {
            panic!("expected cannot infer");
        };
        assert_eq!(errors.len(), 1);
        assert!(matches!(errors[0], TypeError::CannotInferType { .. }));
    }

    #[test]
    fn nil_shared_once() {
        assert_single_error("fn main() { let x = nil; x; }", |err| {
            matches!(err, TypeError::CannotInferType { .. })
        });
    }

    #[test]
    fn nil_contextual_optional() {
        assert_type(
            "fn main() { let x: int? = nil; x; }",
            Type::option_of(Type::Int),
        );
    }

    #[test]
    fn nil_return_optional() {
        assert_type("fn f() -> int? { nil }", Type::option_of(Type::Int));
    }

    #[test]
    fn nil_rejects_plain_target() {
        assert_err_count("fn main() { let x: int = nil; }", 1);
        assert_err_count("fn f() -> int { nil }", 1);
    }

    #[test]
    fn nil_branch_uses_return_context() {
        assert_type(
            "fn main(cond: bool) -> int? { if cond { nil } else { 1 } }",
            Type::option_of(Type::Int),
        );
    }

    #[test]
    fn nil_branch_uses_binding_context() {
        assert_type(
            "fn main(cond: bool) { let x: int? = if cond { nil } else { 1 }; x; }",
            Type::option_of(Type::Int),
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
        let Err(errors) = typecheck("fn main() { missing; }") else {
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
        let result = typecheck("fn main(cond: bool) { let x = if cond { 1 } else { 2 }; x; }")
            .expect("typecheck failed");
        assert_no_infer_vars_in_result(&result);
    }

    #[test]
    fn array_len_infer_guard() {
        let ty = Type::Array {
            elem: Box::new(Type::Int),
            len: ArrayLen::Infer,
        };
        assert!(type_contains_infer(&ty));
    }

    #[test]
    fn nested_func_ret_infer_guard() {
        let ty = Type::Func {
            params: vec![FuncParam::new(Type::Int, false)],
            ret: Box::new(Type::Tuple(vec![Type::Infer])),
        };
        assert!(type_contains_infer(&ty));
    }

    #[test]
    fn unresolved_nominal_type_arg_infer_guard() {
        let ty = Type::UnresolvedNominal {
            qualifier: None,
            name: Ident::new("Box"),
            generic_args: vec![GenericArg::Type(Type::Infer)],
        };
        assert!(type_contains_infer(&ty));
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
        assert!(type_contains_infer(&ty));
    }
}

mod literals {
    use super::*;

    #[test]
    fn int_literal() {
        assert_type("fn main() { 1; }", Type::Int);
    }

    #[test]
    fn float_literal() {
        assert_type("fn main() { 1.0; }", Type::Float);
    }

    #[test]
    fn bool_true() {
        assert_type("fn main() { true; }", Type::Bool);
    }

    #[test]
    fn bool_false() {
        assert_type("fn main() { false; }", Type::Bool);
    }

    #[test]
    fn string_literal() {
        assert_type("fn main() { \"hi\"; }", Type::String);
    }
}

mod consts {
    use super::*;

    #[test]
    fn evaluated() {
        let result = typecheck("const X = 1 + 2; fn main() { X; }").expect("typecheck failed");
        assert_eq!(
            result.consts().get(&(ModuleScope::Root, Ident::new("X"))),
            Some(&ConstValue::Int(3))
        );
    }

    #[test]
    fn forward_ref() {
        assert_type("const A = B + 1; const B = 2; fn main() { A; }", Type::Int);
    }

    #[test]
    fn local() {
        assert_type("fn main() { const X = 1 + 2; X; }", Type::Int);
    }

    #[test]
    fn local_shadow() {
        assert_type(
            "const X = 1; fn main() { const X = \"x\"; X; }",
            Type::String,
        );
    }

    #[test]
    fn imported() {
        assert_type_with_modules(
            "import gamekit { SIZE }; fn main() { SIZE; }",
            "pub const SIZE = 4;",
            Type::Int,
        );
    }

    #[test]
    fn imported_forward_ref() {
        assert_type_with_modules(
            "import gamekit { SIZE }; fn main() { SIZE; }",
            "pub const SIZE = BASE + 1; const BASE = 3;",
            Type::Int,
        );
    }

    #[test]
    fn module_private_err() {
        let result = typecheck_with_modules(
            "import gamekit as gk; const X = gk.SECRET; fn main() { X; }",
            "const SECRET = 1;",
        );
        assert!(result.is_err(), "expected private module const error");
    }

    #[test]
    fn cycle_err() {
        assert_err("const A = B; const B = A; fn main() { A; }");
    }

    #[test]
    fn div_zero_err() {
        assert_err("const X = 1 / 0; fn main() { X; }");
    }

    #[test]
    fn bool_short_circuit() {
        let result = typecheck("const X = false && (1 / 0 == 0); fn main() { X; }")
            .expect("typecheck failed");
        assert_eq!(
            result.consts().get(&(ModuleScope::Root, Ident::new("X"))),
            Some(&ConstValue::Bool(false))
        );
    }

    #[test]
    fn type_mismatch_err() {
        assert_err("const X: int = \"x\"; fn main() { X; }");
    }

    #[test]
    fn duplicate_err() {
        assert_err("const X = 1; const X = 2; fn main() {}");
    }

    #[test]
    fn non_const_initializer_err() {
        assert_err("fn f() -> int { 1 } const X = f(); fn main() { X; }");
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
    fn literal() {
        assert_type(
            &buf("fn main(x: FixedBuf<int, 3>) -> FixedBuf<int, 3> { x }"),
            fixed(3),
        );
    }

    #[test]
    fn imported() {
        assert_type_with_named_modules(
            "import bufs; fn main(x: bufs.FixedBuf<int, 3>) { x.data; }",
            &[("bufs", "pub struct FixedBuf<T, N: int> { data: [T; N] }")],
            array(Type::Int, 3),
        );
    }

    #[test]
    fn dataref_arg() {
        assert_type(
            &ref_buf("fn main(x: FixedBuf<int, 3>) -> FixedBuf<int, 3> { x }"),
            dataref(3),
        );
    }

    #[test]
    fn enum_arg() {
        assert_type(
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
        let Err(errors) = typecheck(&buf("fn f(x: FixedBuf<int>) {} fn main() {}")) else {
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
        let Err(errors) = typecheck(&buf("fn f(x: FixedBuf<int, true>) {} fn main() {}")) else {
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
    fn bool_const_arg_ok_when_not_array_len() {
        assert_type(
            "struct Flag<N: int> {} fn main(x: Flag<true>) -> Flag<true> { x }",
            Type::nominal(
                NominalKind::Struct,
                Ident::new("Flag"),
                vec![],
                vec![ConstArg::Value(ConstValue::Bool(true))],
                None,
            ),
        );
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
        assert_type(
            "const CAP = 3; struct FixedBuf<T, N: int> { data: [T; N] } fn main(x: FixedBuf<int, CAP>) -> FixedBuf<int, 3> { x }",
            fixed(3),
        );
    }

    #[test]
    fn const_param() {
        assert_type(
            &buf(
                "fn f<T, N: int>(x: FixedBuf<T, N>) -> int { N } fn main(x: FixedBuf<int, 3>) -> int { f(x) }",
            ),
            Type::Int,
        );
    }

    #[test]
    fn same() {
        assert_type(
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
        assert_type(
            "struct FixedBuf<T, N: int> { head: T, data: [T; N] } fn first<T, N: int>(buf: FixedBuf<T, N>) -> T { buf.head } fn main() { first(FixedBuf { head: 1, data: [1, 2, 3] }); }",
            Type::Int,
        );
    }

    #[test]
    fn literal_args() {
        assert_type(
            &buf("fn main() { FixedBuf<int, 3> { data: [1, 2, 3] }; }"),
            fixed(3),
        );
    }

    #[test]
    fn literal_hint() {
        assert_type(
            &buf("fn main() { let buf: FixedBuf<int, 3> = FixedBuf { data: [1, 2, 3] }; buf; }"),
            fixed(3),
        );
    }

    #[test]
    fn literal_infers_args() {
        assert_type(
            &buf("fn main() { FixedBuf { data: [1, 2, 3] }; }"),
            fixed(3),
        );
    }

    #[test]
    fn dataref_infer() {
        assert_type(
            &ref_buf("fn main() { FixedBuf { data: [1, 2, 3] }; }"),
            dataref(3),
        );
    }

    #[test]
    fn field() {
        assert_type(
            &buf("fn main() { let buf = FixedBuf { data: [1, 2, 3] }; buf.data; }"),
            array(Type::Int, 3),
        );
    }

    #[test]
    fn method() {
        assert_type(
            "struct FixedBuf<T, N: int> { data: [T; N], fn get(self) -> [T; N] { self.data } } fn main() { let buf = FixedBuf { data: [1, 2, 3] }; buf.get(); }",
            array(Type::Int, 3),
        );
    }

    #[test]
    fn enum_payload() {
        assert_type(
            &packets("fn main() { Packet.Inline([1, 2, 3]); }"),
            packet(3),
        );
    }

    #[test]
    fn enum_explicit() {
        assert_type(
            &packets("fn main() { Packet.Inline<int, 3>([1, 2, 3]); }"),
            packet(3),
        );
    }

    #[test]
    fn literal_expected_optional() {
        assert_type(
            "struct Box<T> { value: T } fn main() { let x: Box<int?> = Box { value: nil }; x; }",
            box_struct(Type::option_of(Type::Int)),
        );
    }

    #[test]
    fn explicit_optional_arg() {
        assert_type(
            "struct Box<T> { value: T } fn main() { let x = Box<int?> { value: nil }; x; }",
            box_struct(Type::option_of(Type::Int)),
        );
    }

    #[test]
    fn nested_literal_optional() {
        assert_type(
            "struct Inner<T> { value: T } struct Outer<T> { inner: T } fn main() { let x: Outer<Inner<int?>> = Outer { inner: Inner { value: nil } }; x; }",
            struct_ty(
                "Outer",
                vec![struct_ty("Inner", vec![Type::option_of(Type::Int)], vec![])],
                vec![],
            ),
        );
    }

    #[test]
    fn literal_const_array_hint() {
        assert_type(
            &buf("fn main() { let x: FixedBuf<int, 3> = FixedBuf { data: [1, 2, 3] }; x; }"),
            fixed(3),
        );
    }

    #[test]
    fn literal_optional_const_array_hint() {
        assert_type(
            &buf("fn main() { let x: FixedBuf<int?, 2> = FixedBuf { data: [nil, 1] }; x; }"),
            struct_ty("FixedBuf", vec![Type::option_of(Type::Int)], vec![arg(2)]),
        );
    }

    #[test]
    fn dataref_expected_optional() {
        assert_type(
            "dataref Box<T> { value: T } fn main() { let x: Box<int?> = Box { value: nil }; x; }",
            dataref_ty("Box", vec![Type::option_of(Type::Int)]),
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
        assert_type(
            "enum Maybe<T> { Some(T), None } fn main() { let x: Maybe<int?> = .Some(nil); x; }",
            maybe(Type::option_of(Type::Int)),
        );
    }

    #[test]
    fn struct_expected_optional() {
        assert_type(
            "enum Maybe<T> { Pair { value: T }, None } fn main() { let x: Maybe<int?> = .Pair { value: nil }; x; }",
            maybe(Type::option_of(Type::Int)),
        );
    }

    #[test]
    fn unit_expected_enum() {
        assert_type(
            "enum Maybe<T> { None } fn main() { let x: Maybe<int> = .None; x; }",
            maybe(Type::Int),
        );
    }

    #[test]
    fn return_expected_optional() {
        assert_type(
            "enum Maybe<T> { Some(T) } fn main() -> Maybe<int?> { .Some(nil) }",
            maybe(Type::option_of(Type::Int)),
        );
    }

    #[test]
    fn array_element_expected_optional() {
        assert_type(
            "enum Maybe<T> { Some(T) } fn main() { let xs: [Maybe<int?>; 1] = [.Some(nil)]; xs; }",
            array(maybe(Type::option_of(Type::Int)), 1),
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
    fn literal() {
        assert_type("fn main() { [1, 2, 3]; }", array(Type::Int, 3));
    }

    #[test]
    fn fill_const_len() {
        assert_type("const N = 3; fn main() { [0; N]; }", array(Type::Int, 3));
    }

    #[test]
    fn annotation_const_len() {
        assert_type(
            "const N = 3; fn main() { let xs: [int; N] = [1, 2, 3]; xs; }",
            array(Type::Int, 3),
        );
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
        assert_type(
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
        assert_type(
            "fn main() { let xs: [int; 0] = []; xs; }",
            array(Type::Int, 0),
        );
    }

    #[test]
    fn optional_elements_context() {
        assert_type(
            "fn main() { let xs: [int?; 2] = [nil, 1]; xs; }",
            array(Type::option_of(Type::Int), 2),
        );
    }

    #[test]
    fn optional_fill_context() {
        assert_type(
            "fn main() { let xs: [int?; 2] = [nil; 2]; xs; }",
            array(Type::option_of(Type::Int), 2),
        );
    }

    #[test]
    fn list_context() {
        assert_type(
            "fn main() { let xs: [int?] = [nil, 1]; xs; }",
            Type::List {
                elem: Box::new(Type::option_of(Type::Int)),
            },
        );
    }

    #[test]
    fn nested_optional_context() {
        assert_type(
            "fn main() { let xs: [[int?; 1]; 1] = [[nil]]; xs; }",
            array(array(Type::option_of(Type::Int), 1), 1),
        );
    }

    #[test]
    fn empty_no_context_err() {
        assert_single_error("fn main() { let xs = []; }", |err| {
            matches!(err, TypeError::CannotInferType { .. })
        });
    }

    #[test]
    fn nil_no_context_err() {
        assert_err("fn main() { let xs = [nil, nil]; }");
    }

    #[test]
    fn literal_element_mismatch_err() {
        assert_err_count("fn main() { let xs: [int; 2] = [1, \"x\"]; }", 1);
    }

    #[test]
    fn literal_len_mismatch_err() {
        assert_err_count("fn main() { let xs: [int; 3] = [1, 2]; }", 1);
    }

    #[test]
    fn nil_plain_array_element_err() {
        assert_err_count("fn main() { let xs: [int; 1] = [nil]; }", 1);
    }

    #[test]
    fn nested_imported_const_len() {
        assert_type_with_modules(
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
        assert_type(
            "fn main() { let x: (int, string?) = (1, nil); x; }",
            Type::Tuple(vec![Type::Int, Type::option_of(Type::String)]),
        );
    }

    #[test]
    fn nested_optional_context() {
        assert_type(
            "fn main() { let x: (int?, (string, bool?)) = (nil, (\"s\", nil)); x; }",
            Type::Tuple(vec![
                Type::option_of(Type::Int),
                Type::Tuple(vec![Type::String, Type::option_of(Type::Bool)]),
            ]),
        );
    }

    #[test]
    fn branch_context() {
        assert_type(
            "fn main(cond: bool) { let x: (int, int?) = if cond { (1, nil) } else { (2, 3) }; x; }",
            Type::Tuple(vec![Type::Int, Type::option_of(Type::Int)]),
        );
    }

    #[test]
    fn named_context() {
        assert_type(
            "fn main() { let x: (a: int, b: int?) = (a: 1, b: nil); x; }",
            Type::NamedTuple(vec![
                (Ident::new("a"), Type::Int),
                (Ident::new("b"), Type::option_of(Type::Int)),
            ]),
        );
    }

    #[test]
    fn named_branch_context() {
        assert_type(
            "fn main(cond: bool) { let x: (a: int, b: int?) = if cond { (a: 1, b: nil) } else { (a: 2, b: 3) }; x; }",
            Type::NamedTuple(vec![
                (Ident::new("a"), Type::Int),
                (Ident::new("b"), Type::option_of(Type::Int)),
            ]),
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
    fn named_tuple_label_err() {
        assert_err_count("fn main() { let x: (a: int, b: int) = (a: 1, c: 2); }", 1);
    }

    #[test]
    fn nil_no_context_err() {
        assert_err("fn main() { let x = (nil, nil); }");
    }
}

mod bindings {
    use super::*;

    #[test]
    fn let_infer() {
        assert_type("fn main() { let x = 1; x; }", Type::Int);
    }

    #[test]
    fn let_annotated() {
        assert_type("fn main() { let x: int = 1; x; }", Type::Int);
    }

    #[test]
    fn let_mismatch() {
        assert_err("fn main() { let x: int = \"hi\"; }");
    }

    #[test]
    fn var_binding() {
        assert_type("fn main() { var x = 1; x = 2; x; }", Type::Int);
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

mod binary_ops {
    use super::*;

    #[test]
    fn add_ints() {
        assert_type("fn main() { 1 + 2; }", Type::Int);
    }

    #[test]
    fn add_floats() {
        assert_type("fn main() { 1.0 + 2.0; }", Type::Float);
    }

    #[test]
    fn add_strings() {
        assert_type("fn main() { \"a\" + \"b\"; }", Type::String);
    }

    #[test]
    fn add_string_int() {
        assert_type("fn main() { \"hi\" + 1; }", Type::String);
    }

    #[test]
    fn add_mixed_num() {
        assert_err("fn main() { 1 + 2.0; }");
    }

    #[test]
    fn compare_ints() {
        assert_type("fn main() { 1 < 2; }", Type::Bool);
    }

    #[test]
    fn eq_ints() {
        assert_type("fn main() { 1 == 2; }", Type::Bool);
    }

    #[test]
    fn and_bools() {
        assert_type("fn main() { true && false; }", Type::Bool);
    }

    #[test]
    fn or_bools() {
        assert_type("fn main() { true || false; }", Type::Bool);
    }

    #[test]
    fn bitor_ints() {
        assert_type("fn main() { 1 | 2; }", Type::Int);
    }

    #[test]
    fn bitand_ints() {
        assert_type("fn main() { 1 & 2; }", Type::Int);
    }

    #[test]
    fn eq_mismatch() {
        assert_err("fn main() { 1 == \"hi\"; }");
    }
}

mod unary_ops {
    use super::*;

    #[test]
    fn neg_int() {
        assert_type("fn main() { -1; }", Type::Int);
    }

    #[test]
    fn neg_float() {
        assert_type("fn main() { -1.0; }", Type::Float);
    }

    #[test]
    fn not_bool() {
        assert_type("fn main() { !true; }", Type::Bool);
    }

    #[test]
    fn bitnot_int() {
        assert_type("fn main() { ~1; }", Type::Int);
    }

    #[test]
    fn neg_type_mismatch() {
        assert_err("fn main() { -\"hi\"; }");
    }
}

mod functions {
    use super::*;

    #[test]
    fn fn_no_params() {
        assert_err_count("fn foo() -> int { return 1; } fn main() { foo(); }", 0);
    }

    #[test]
    fn fn_with_params() {
        assert_err_count(
            "fn add(a: int, b: int) -> int { return a + b; } fn main() { add(1, 2); }",
            0,
        );
    }

    #[test]
    fn fn_wrong_ret() {
        assert_err("fn foo() -> int { return \"hi\"; }");
    }

    #[test]
    fn fn_wrong_arg_count() {
        assert_err("fn foo(a: int) -> int { return a; } fn main() { foo(1, 2); }");
    }

    #[test]
    fn fn_wrong_arg_type() {
        assert_err("fn foo(a: int) -> int { return a; } fn main() { foo(\"hi\"); }");
    }

    #[test]
    fn fn_void_ret() {
        assert_err_count("fn foo() {} fn main() { foo(); }", 0);
    }

    #[test]
    fn fn_missing_return() {
        assert_err("fn foo() -> int {} ");
    }
}

mod if_expr {
    use super::*;

    #[test]
    fn if_both_int() {
        assert_type("fn main() { if true { 1 } else { 2 }; }", Type::Int);
    }

    #[test]
    fn if_mismatch() {
        assert_err("fn main() { if true { 1 } else { \"hi\" }; }");
    }

    #[test]
    fn if_no_else() {
        assert_type("fn main() { if true { 1 }; }", Type::Void);
    }

    #[test]
    fn if_condition_not_bool() {
        assert_err("fn main() { if 1 { } }");
    }
}

mod blocks {
    use super::*;

    #[test]
    fn block_tail() {
        assert_type("fn main() { { 1 }; }", Type::Int);
    }

    #[test]
    fn block_void() {
        assert_type("fn main() { { }; }", Type::Void);
    }
}

mod while_stmt {
    use super::*;

    #[test]
    fn while_bool() {
        assert_err_count("fn main() { while true {} }", 0);
    }

    #[test]
    fn while_non_bool() {
        assert_err("fn main() { while 1 {} }");
    }

    #[test]
    fn while_inner_break() {
        assert_err_count("fn main() { while true { break; } }", 0);
    }

    #[test]
    fn while_inner_continue() {
        assert_err_count("fn main() { while true { continue; } }", 0);
    }

    #[test]
    fn nested_break() {
        assert_err_count("fn main() { while true { while true { break; } } }", 0);
    }

    #[test]
    fn nested_continue() {
        assert_err_count("fn main() { while true { while true { continue; } } }", 0);
    }
}

mod loop_control {
    use super::*;

    #[test]
    fn break_outside() {
        assert_err("fn main() { break; }");
    }

    #[test]
    fn continue_outside() {
        assert_err("fn main() { continue; }");
    }
}

mod for_stmt {
    use super::*;

    #[test]
    fn for_list_param() {
        assert_err_count("fn main(xs: [int]) { for x in xs {} }", 0);
    }

    #[test]
    fn for_array_param() {
        assert_err_count("fn main(xs: [int; 3]) { for x in xs {} }", 0);
    }

    #[test]
    fn for_non_iterable() {
        assert_err("fn main(x: int) { for y in x {} }");
    }

    #[test]
    fn for_binding_scope() {
        assert_err("fn main(xs: [int]) { for x in xs {} x; }");
    }

    #[test]
    fn for_wildcard() {
        assert_err_count("fn main(xs: [int]) { for _ in xs {} }", 0);
    }

    #[test]
    fn for_step_expr_checked() {
        assert_err("fn main(xs: [int]) { for x in xs step missing {} }");
    }

    #[test]
    fn for_item_type() {
        assert_type("fn main(xs: [int]) { for x in xs { x; } }", Type::Int);
    }
}
