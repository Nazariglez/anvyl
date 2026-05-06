use super::support::{
    assert_calls, assert_err, assert_err_count, assert_single_error, assert_ty, assert_ty_mods,
    assert_ty_named, assert_typecheck_closed, check, check_named, errors,
};
use crate::{
    ast::{ArrayLen, Ident, NominalKind, Type},
    typecheck::{
        CallTarget, GenericArgs, MemberAccessKind, TypeError, TypecheckResult, VariantShape,
        decls::{
            CallableId, DeclError, ExtendId, MethodKey, MethodSurface, ModuleScope, NominalKey,
            VariantSchema,
        },
    },
};

fn root_key(kind: NominalKind, name: &str) -> NominalKey {
    NominalKey {
        module: ModuleScope::Root,
        kind,
        name: Ident::new(name),
    }
}

fn nominal(
    kind: NominalKind,
    name: &str,
    type_args: Vec<Type>,
    origin: Option<std::rc::Rc<[String]>>,
) -> Type {
    Type::nominal(kind, Ident::new(name), type_args, vec![], origin)
}

mod field_access {
    use super::*;

    #[test]
    fn field_access() {
        assert_ty(
            "struct Point { 
                x: int, 
                y: int 
            } 
            fn main() { 
                let p = Point { x: 1, y: 2 };
                p.x; 
            }",
            Type::Int,
        );
    }

    #[test]
    fn nested_field_access() {
        assert_ty(
            "struct Pos { x: int } 
            struct Player { pos: Pos } 
            fn main() { 
                let p = Player { pos: Pos { x: 1 } }; 
                p.pos.x; 
            }",
            Type::Int,
        );
    }

    #[test]
    fn unknown_field() {
        assert_single_error(
            "struct Point { x: int } 
            fn main() { 
                let p = Point { x: 1 }; 
                p.z; 
            }",
            |err| {
                matches!(
                    err,
                    TypeError::UnknownMember {
                        member,
                        kind: MemberAccessKind::Field,
                        ..
                    } if *member == Ident::new("z")
                )
            },
        );
    }

    #[test]
    fn field_on_int() {
        assert_single_error("fn main() { let x = 1; x.y; }", |err| {
            matches!(
                err,
                TypeError::MemberAccessOnNonAggregate {
                    ty: Type::Int,
                    member,
                    kind: MemberAccessKind::Field,
                    ..
                } if *member == Ident::new("y")
            )
        });
    }

    #[test]
    fn imported_field_access() {
        assert_ty_mods(
            "import gamekit { Point }; 
            fn main() { 
                let p = Point { x: 1, y: 2 }; 
                p.x; 
            }",
            "pub struct Point { x: int, y: int }",
            Type::Int,
        );
    }

    #[test]
    fn imported_field_origin() {
        assert_ty_named(
            "import alpha { Item };
            fn main() {
                let item = Item { value: 1 };
                item.value;
            }",
            &[
                ("alpha", "pub struct Item { value: int }"),
                ("beta", "pub struct Item { label: string }"),
            ],
            Type::Int,
        );
    }

    #[test]
    fn qualified_method_origin() {
        assert_ty_named(
            "import shapes;
            fn main() {
                let p = shapes.Point { x: 1 };
                p.x();
            }",
            &[
                (
                    "shapes",
                    "pub struct Point { x: int } pub extend Point { fn x(self) -> int { self.x } }",
                ),
                (
                    "other",
                    "pub struct Point { y: string } pub extend Point { fn x(self) -> string { self.y } }",
                ),
            ],
            Type::Int,
        );
    }

    #[test]
    fn originless_receiver_no_guess() {
        let result = check_named(
            "fn main() {
                let p: Point = 1;
                p.value;
            }",
            &[("alpha", "pub struct Point { value: int }")],
        );

        assert!(result.is_err());
    }

    #[test]
    fn receiver_args() {
        assert_ty(
            "struct Wrapper<T> { value: T } fn main() { let w = Wrapper { value: 42 }; w.value; }",
            Type::Int,
        );
    }

    #[test]
    fn dataref_receiver_args() {
        assert_ty(
            "dataref Box<T> { value: T } fn main() { let b = Box { value: true }; b.value; }",
            Type::Bool,
        );
    }

    #[test]
    fn nested_receiver_args() {
        assert_ty(
            "struct Box<T> { value: T } 
            struct Wrapper<T> { value: T } 
            fn main() { 
                let w = Wrapper { 
                    value: Box { value: 42 } 
                }; 
                w.value.value; 
            }",
            Type::Int,
        );
    }

    #[test]
    fn imported_receiver_args() {
        assert_ty_mods(
            "import gamekit { Wrapper }; fn main() { let w = Wrapper { value: 42 }; w.value; }",
            "pub struct Wrapper<T> { value: T }",
            Type::Int,
        );
    }
}

mod struct_literal {
    use super::*;

    #[test]
    fn local_struct_literal() {
        assert_ty(
            "struct Point { x: int, y: int } fn main() { Point { x: 1, y: 2 }; }",
            nominal(NominalKind::Struct, "Point", vec![], None),
        );
    }

    #[test]
    fn struct_literal_missing_field() {
        assert_err_count(
            "struct Point { x: int, y: int } fn main() { Point { x: 1 }; }",
            1,
        );
    }

    #[test]
    fn struct_literal_unknown_field() {
        assert_err_count(
            "struct Point { x: int } fn main() { Point { x: 1, z: 3 }; }",
            1,
        );
    }

    #[test]
    fn struct_literal_duplicate_field() {
        assert_err_count(
            "struct Point { x: int } fn main() { Point { x: 1, x: 2 }; }",
            1,
        );
    }

    #[test]
    fn struct_literal_type_mismatch() {
        assert_err_count(
            "struct Point { x: int } fn main() { Point { x: true }; }",
            1,
        );
    }

    #[test]
    fn module_struct_literal() {
        assert_ty_mods(
            "import gamekit { Point }; fn main() { Point { x: 1, y: 2 }; }",
            "pub struct Point { x: int, y: int }",
            nominal(
                NominalKind::Struct,
                "Point",
                vec![],
                Some(std::rc::Rc::from(
                    vec![String::from("gamekit")].into_boxed_slice(),
                )),
            ),
        );
    }

    #[test]
    fn struct_infer() {
        assert_ty(
            "struct Wrapper<T> { value: T } fn main() { Wrapper { value: 42 }; }",
            nominal(NominalKind::Struct, "Wrapper", vec![Type::Int], None),
        );
    }

    #[test]
    fn dataref_infer() {
        assert_ty(
            "dataref Box<T> { value: T } fn main() { Box { value: \"hi\" }; }",
            nominal(NominalKind::DataRef, "Box", vec![Type::String], None),
        );
    }

    #[test]
    fn annotation_unconstrained() {
        assert_ty(
            "struct Token<T> {} fn main() { let value: Token<int> = Token {}; value; }",
            nominal(NominalKind::Struct, "Token", vec![Type::Int], None),
        );
    }

    #[test]
    fn nested_annotation() {
        assert_ty(
            "struct Token<T> {}
             struct Wrapper<T> { value: Token<T> } 
             fn main() { 
                let w: Wrapper<int> = Wrapper { value: Token {} }; 
                w.value; 
            }",
            nominal(NominalKind::Struct, "Token", vec![Type::Int], None),
        );
    }

    #[test]
    fn unbound_params() {
        assert_err_count("struct Token<T> {} fn main() { Token {}; }", 1);
    }
}

mod aggregate_schemas {
    use super::*;

    #[test]
    fn struct_schema() {
        let result = check("struct Wrapper<T> { value: T } fn main() {}").unwrap();
        let key = root_key(NominalKind::Struct, "Wrapper");
        let agg = result.decls().aggregate(&key).expect("no aggregate");
        let field = agg.fields.get(&Ident::new("value")).expect("no field");
        let t = agg.generics.type_params[0].id;

        assert_eq!(agg.generics.type_params.len(), 1);
        assert!(agg.generics.const_params.is_empty());
        assert_eq!(field.ty, Type::Var(t));
    }

    #[test]
    fn dataref_schema() {
        let result = check("dataref Buffer<T, N: int> { data: [T; N] } fn main() {}").unwrap();
        let key = root_key(NominalKind::DataRef, "Buffer");
        let agg = result.decls().aggregate(&key).expect("no aggregate");
        let field = agg.fields.get(&Ident::new("data")).expect("no field");
        let t = agg.generics.type_params[0].id;
        let n = agg.generics.const_params[0].id;

        assert_eq!(agg.generics.type_params.len(), 1);
        assert_eq!(agg.generics.const_params.len(), 1);
        assert_eq!(
            field.ty,
            Type::Array {
                elem: Box::new(Type::Var(t)),
                len: ArrayLen::Param(n),
            }
        );
    }
}

mod enum_schemas {
    use super::*;

    #[test]
    fn enum_schema() {
        let result = check("enum Option<T> { Some(T), None } fn main() {}").unwrap();
        let key = root_key(NominalKind::Enum, "Option");
        let enm = result.decls().enum_schema(&key).expect("no enum");
        let t = enm.generics.type_params[0].id;

        assert_eq!(enm.generics.type_params.len(), 1);
        assert!(enm.generics.const_params.is_empty());
        assert!(matches!(
            enm.variants.get(&Ident::new("Some")),
            Some(VariantSchema::Tuple(types)) if types.as_slice() == [Type::Var(t)]
        ));
        assert!(matches!(
            enm.variants.get(&Ident::new("None")),
            Some(VariantSchema::Unit)
        ));
    }
}

mod method_schemas {
    use super::*;

    #[test]
    fn collects_instance_method() {
        let result = check(
            "struct Point { 
                x: int, 
                fn len(self) -> int { 0 }
            } 
            fn main() {}
        ",
        )
        .unwrap();
        let agg = result
            .decls()
            .aggregate(&root_key(NominalKind::Struct, "Point"))
            .expect("no aggregate");
        let method = agg
            .methods
            .get(&MethodKey::instance(Ident::new("len")))
            .expect("no method");
        assert_eq!(method.mode.surface(), MethodSurface::Instance);
        assert_eq!(method.params.len(), 0);
        assert_eq!(method.ret, Type::Int);
    }

    #[test]
    fn collects_static_method() {
        let result = check(
            "struct Point { x: int, fn origin() -> Point { Point { x: 0 } } } 
            fn main() {}
        ",
        )
        .unwrap();
        let agg = result
            .decls()
            .aggregate(&root_key(NominalKind::Struct, "Point"))
            .expect("no aggregate");
        let method = agg
            .methods
            .get(&MethodKey::static_(Ident::new("origin")))
            .expect("no method");
        assert_eq!(method.mode.surface(), MethodSurface::Static);
        assert_eq!(method.params.len(), 0);
    }

    #[test]
    fn instance_method_with_params() {
        let result =
            check("struct Point { x: int, fn add(self, v: int) -> int { 0 } } fn main() {}")
                .unwrap();
        let agg = result
            .decls()
            .aggregate(&root_key(NominalKind::Struct, "Point"))
            .expect("no aggregate");
        let method = agg
            .methods
            .get(&MethodKey::instance(Ident::new("add")))
            .expect("no method");
        assert_eq!(method.mode.surface(), MethodSurface::Instance);
        assert_eq!(method.params.len(), 1);
        assert_eq!(method.params[0].ty, Type::Int);
        assert_eq!(method.ret, Type::Int);
    }

    #[test]
    fn collects_generic_instance_method() {
        let result =
            check("struct Point { fn map<T>(self, f: T) -> T { f } } fn main() {}").unwrap();
        let agg = result
            .decls()
            .aggregate(&root_key(NominalKind::Struct, "Point"))
            .expect("no aggregate");
        let method = agg
            .methods
            .get(&MethodKey::instance(Ident::new("map")))
            .expect("no method");
        let t = method.generics.type_params[0].id;

        assert_eq!(method.mode.surface(), MethodSurface::Instance);
        assert_eq!(method.generics.type_params.len(), 1);
        assert!(method.generics.const_params.is_empty());
        assert_eq!(method.params[0].ty, Type::Var(t));
        assert_eq!(method.ret, Type::Var(t));
    }

    #[test]
    fn collects_generic_static_method() {
        let result =
            check("struct Point { fn wrap<T, N: int>(x: [T; N]) -> [T; N] { x } } fn main() {}")
                .unwrap();
        let agg = result
            .decls()
            .aggregate(&root_key(NominalKind::Struct, "Point"))
            .expect("no aggregate");
        let method = agg
            .methods
            .get(&MethodKey::static_(Ident::new("wrap")))
            .expect("no method");
        let t = method.generics.type_params[0].id;
        let n = method.generics.const_params[0].id;
        let array_ty = Type::Array {
            elem: Box::new(Type::Var(t)),
            len: ArrayLen::Param(n),
        };

        assert_eq!(method.mode.surface(), MethodSurface::Static);
        assert_eq!(method.generics.type_params.len(), 1);
        assert_eq!(method.generics.const_params.len(), 1);
        assert_eq!(method.params[0].ty, array_ty.clone());
        assert_eq!(method.ret, array_ty);
    }
}

mod method_calls {
    use super::*;

    fn assert_method_target(
        result: &TypecheckResult,
        owner: &str,
        name: &str,
        is_instance: bool,
        type_args: Vec<Type>,
    ) {
        let target = result.calls().values().next().expect("missing call target");
        assert_eq!(
            target,
            &CallTarget::new(
                CallableId::aggregate_method(
                    root_key(NominalKind::Struct, owner),
                    Ident::new(name),
                    if is_instance {
                        MethodSurface::Instance
                    } else {
                        MethodSurface::Static
                    },
                ),
                GenericArgs {
                    type_args,
                    const_args: vec![],
                }
            )
        );
    }

    #[test]
    fn method_call() {
        assert_ty(
            "struct Point { x: int, fn len(self) -> int { 0 } } fn main() -> int { let p = Point { x: 1 }; p.len() }",
            Type::Int,
        );
    }

    #[test]
    fn method_call_with_args() {
        assert_ty(
            "struct Point { x: int, fn add(self, v: int) -> int { 0 } } fn main() -> int { let p = Point { x: 1 }; p.add(2) }",
            Type::Int,
        );
    }

    #[test]
    fn unknown_method_on_aggregate() {
        assert_single_error(
            "struct Point { x: int } fn main() { let p = Point { x: 1 }; p.move(); }",
            |err| {
                matches!(
                    err,
                    TypeError::UnknownMember {
                        member,
                        kind: MemberAccessKind::Method,
                        ..
                    } if *member == Ident::new("move")
                )
            },
        );
    }

    #[test]
    fn unknown_method_on_primitive() {
        assert_single_error("fn main() { 1.foo(); }", |err| {
            matches!(
                err,
                TypeError::MemberAccessOnNonAggregate {
                    ty: Type::Int,
                    member,
                    kind: MemberAccessKind::Method,
                    ..
                } if *member == Ident::new("foo")
            )
        });
    }

    #[test]
    fn collection_pop_not_builtin() {
        assert_single_error("fn main() { let xs = [1]; xs.pop(); }", |err| {
            matches!(
                err,
                TypeError::MemberAccessOnNonAggregate {
                    member,
                    kind: MemberAccessKind::Method,
                    ..
                } if *member == Ident::new("pop")
            )
        });
    }

    #[test]
    fn unknown_enum_variant_call() {
        assert_single_error("enum Color { Red } fn main() { Color.Yellow(); }", |err| {
            matches!(
                err,
                TypeError::UnknownEnumVariant {
                    enum_name,
                    variant,
                    ..
                } if *enum_name == Ident::new("Color") && *variant == Ident::new("Yellow")
            )
        });
    }

    #[test]
    fn enum_struct_literal() {
        assert_ty(
            "enum Event { Move { dx: int, dy: int } } fn main() { let e = Event.Move { dx: 1, dy: 2 }; e; }",
            nominal(NominalKind::Enum, "Event", vec![], None),
        );
    }

    #[test]
    fn enum_struct_unknown_field() {
        let errors = errors("enum Event { Move { dx: int } } fn main() { Event.Move { dz: 1 }; }");
        assert!(errors.iter().any(|err| {
            matches!(err, TypeError::UnknownVariantField { field, .. } if *field == Ident::new("dz"))
        }));
    }

    #[test]
    fn enum_struct_missing_field() {
        assert_single_error(
            "enum Event { Move { dx: int, dy: int } } fn main() { Event.Move { dx: 1 }; }",
            |err| matches!(err, TypeError::MissingVariantField { field, .. } if *field == Ident::new("dy")),
        );
    }

    #[test]
    fn enum_struct_on_tuple() {
        assert_single_error(
            "enum Event { Move(int) } fn main() { Event.Move { dx: 1 }; }",
            |err| {
                matches!(
                    err,
                    TypeError::EnumVariantShapeMismatch {
                        expected: VariantShape::Struct,
                        ..
                    }
                )
            },
        );
    }

    #[test]
    fn field_called_as_method() {
        assert_single_error(
            "struct Point { x: int } fn main() { let p = Point { x: 1 }; p.x(); }",
            |err| {
                matches!(
                    err,
                    TypeError::UnknownMember {
                        member,
                        kind: MemberAccessKind::Method,
                        ..
                    } if *member == Ident::new("x")
                )
            },
        );
    }

    #[test]
    fn error_callee_checks_args() {
        let errors = errors("enum Color { Red } fn main() { Color.Yellow(missing); }");
        assert_eq!(errors.len(), 2, "unexpected errors: {errors:?}");
        assert!(errors.iter().any(|err| {
            matches!(
                err,
                TypeError::UnknownEnumVariant { enum_name, variant, .. }
                    if *enum_name == Ident::new("Color") && *variant == Ident::new("Yellow")
            )
        }));
        assert!(errors.iter().any(|err| {
            matches!(
                err,
                TypeError::UndefinedVariable { name, .. } if *name == Ident::new("missing")
            )
        }));
        assert!(
            !errors
                .iter()
                .any(|err| matches!(err, TypeError::NotCallable { .. }))
        );
    }

    #[test]
    fn field_and_method_share_name() {
        assert_ty(
            "struct Point { x: int, fn x(self) -> bool { true } } fn main() { let p = Point { x: 1 }; p.x; }",
            Type::Int,
        );
        assert_ty(
            "struct Point { x: int, fn x(self) -> bool { true } } fn main() { let p = Point { x: 1 }; p.x(); }",
            Type::Bool,
        );
    }

    #[test]
    fn method_wrong_arg_count() {
        assert_err_count(
            "struct Point { x: int, fn add(self, v: int) -> int { 0 } } fn main() { let p = Point { x: 1 }; p.add(); }",
            1,
        );
    }

    #[test]
    fn method_wrong_arg_type() {
        assert_err_count(
            "struct Point { x: int, fn add(self, v: int) -> int { 0 } } fn main() { let p = Point { x: 1 }; p.add(true); }",
            1,
        );
    }

    #[test]
    fn call_target_method() {
        let result = check(
            "struct Point { x: int, fn len(self) -> int { 0 } } fn main() { let p = Point { x: 1 }; p.len(); }",
        )
        .unwrap();
        assert_method_target(&result, "Point", "len", true, vec![]);
    }

    #[test]
    fn type_name_is_not_receiver() {
        assert_err("struct Point { x: int, fn len(self) -> int { 0 } } fn main() { Point.len(); }");
    }

    #[test]
    fn static_explicit_args() {
        assert_ty(
            "struct Foo { fn make<T>(x: T) -> T { x } } fn main() -> int { Foo.make<int>(42) }",
            Type::Int,
        );
    }

    #[test]
    fn static_infer_args() {
        assert_ty(
            "struct Foo { fn make<T>(x: T) -> T { x } } fn main() -> string { Foo.make(\"hi\") }",
            Type::String,
        );
    }

    #[test]
    fn instance_explicit_args() {
        assert_ty(
            "struct Foo { fn pair<A, B>(self, a: A, b: B) -> B { b } } fn main() -> string { let f = Foo {}; f.pair<int, string>(1, \"ok\") }",
            Type::String,
        );
    }

    #[test]
    fn instance_infer_args() {
        assert_ty(
            "struct Foo { fn pair<A, B>(self, a: A, b: B) -> B { b } } fn main() -> int { let f = Foo {}; f.pair(\"x\", 7) }",
            Type::Int,
        );
    }

    #[test]
    fn owner_instance_receiver_args() {
        assert_ty(
            "struct Wrapper<T> { value: T, fn get(self) -> T { self.value } } fn main() -> int { let w = Wrapper { value: 42 }; w.get() }",
            Type::Int,
        );
    }

    #[test]
    fn owner_static_infer_args() {
        assert_ty(
            "struct Wrapper<T> { value: T, fn new(value: T) -> Self { Wrapper { value: value } } } fn main() -> int { Wrapper.new(42).value }",
            Type::Int,
        );
    }

    #[test]
    fn static_expected_nil() {
        assert_ty(
            "struct Foo { fn id<T>(x: T) -> T { x } } fn main() { let x: int? = Foo.id(nil); x; }",
            Type::option_of(Type::Int),
        );
    }

    #[test]
    fn static_nil_no_leak() {
        let checked = check(
            "struct Foo { fn id<T>(x: T) -> T { x } } fn main() { let x: int? = Foo.id(nil); x; }",
        )
        .expect("typecheck failed");
        assert_typecheck_closed(&checked);
    }

    #[test]
    fn instance_expected_nil() {
        assert_ty(
            "struct Foo { fn id<T>(self, x: T) -> T { x } } fn main() { let f = Foo {}; let x: int? = f.id(nil); x; }",
            Type::option_of(Type::Int),
        );
    }

    #[test]
    fn static_target_args() {
        let result =
            check("struct Foo { fn make<T>(x: T) -> T { x } } fn main() { Foo.make<int>(42); }")
                .unwrap();
        assert_method_target(&result, "Foo", "make", false, vec![Type::Int]);
    }

    #[test]
    fn owner_static_target_args() {
        let result = check(
            "struct Wrapper<T> { value: T, fn new(value: T) -> Self { Wrapper { value: value } } } fn main() { Wrapper.new(42); }",
        )
        .unwrap();
        assert_method_target(&result, "Wrapper", "new", false, vec![Type::Int]);
    }

    #[test]
    fn static_body_err() {
        assert_err_count(
            "fn mul2(x: int) -> int { x * 2 } struct Foo { fn duplicate<T>(x: T) -> T { mul2(x) } } fn main() { Foo.duplicate<string>(\"x\"); }",
            2,
        );
    }

    #[test]
    fn instance_body_err() {
        assert_err_count(
            "struct Foo { fn bad<T>(self, x: T) -> T { 1 } } fn main() { let f = Foo {}; f.bad(\"x\"); }",
            1,
        );
    }

    #[test]
    fn owner_instance_self_err() {
        assert_err_count(
            "struct Wrapper<T> { value: T, fn get(self) -> T { true } } fn main() { let w = Wrapper { value: 42 }; w.get(); }",
            1,
        );
    }

    #[test]
    fn owner_static_body_err() {
        assert_err_count(
            "struct Wrapper<T> { value: T, fn new(value: T) -> Self { Wrapper { value: true } } } fn main() { Wrapper.new(42); }",
            1,
        );
    }

    #[test]
    fn generic_parity() {
        let result = check(
            "struct Foo {
                fn make<T>(x: T) -> T { x }
                fn pair<A, B>(self, a: A, b: B) -> B { b }
            }
            fn main() {
                let f = Foo {};
                let a: int = Foo.make<int>(42);
                let b: string = Foo.make(\"hi\");
                let c: string = f.pair<int, string>(1, \"ok\");
                let d: int = f.pair(\"x\", 7);
            }",
        )
        .unwrap();

        assert_eq!(result.calls().len(), 4);
    }
}

mod extend_schemas {
    use super::*;

    #[test]
    fn collects_extend_on_primitive() {
        let result =
            check("extend int { fn double(self) -> int { self * 2 } } fn main() {}").unwrap();
        let mut found = false;
        for ext in result
            .decls()
            .extends()
            .filter(|ext| ext.target == Type::Int)
        {
            if ext
                .methods
                .contains_key(&MethodKey::instance(Ident::new("double")))
            {
                found = true;
                break;
            }
        }
        assert!(found, "extend method 'double' not found on int");
    }

    #[test]
    fn extend_method_params_skip_self() {
        let result =
            check("extend int { fn add(self, v: int) -> int { self + v } } fn main() {}").unwrap();
        let ext = result
            .decls()
            .extends()
            .find(|ext| ext.target == Type::Int)
            .expect("no extend");
        let method = ext
            .methods
            .get(&MethodKey::instance(Ident::new("add")))
            .expect("no method");
        assert_eq!(method.params.len(), 1);
        assert_eq!(method.params[0].ty, Type::Int);
        assert_eq!(method.ret, Type::Int);
    }

    #[test]
    fn generic_extend() {
        let result = check(
            "struct Box<T> { value: T } extend<T> Box<T> { fn get(self) -> T { self.value } } fn main() {}",
        )
        .unwrap();
        let ext = result.decls().extends().next().expect("no extend");
        let method = ext
            .methods
            .get(&MethodKey::instance(Ident::new("get")))
            .expect("no method");

        assert_eq!(ext.generics.type_params.len(), 1);
        let type_param = ext.generics.type_params[0].id;
        assert_eq!(
            ext.target,
            nominal(
                NominalKind::Struct,
                "Box",
                vec![Type::Var(type_param)],
                None
            )
        );
        assert!(ext.generics.const_params.is_empty());
        assert!(method.generics.is_empty());
        assert!(method.params.is_empty());
        assert_eq!(method.ret, Type::Var(type_param));
    }

    #[test]
    fn exact_extend_match() {
        let result = check("extend<T> T { fn id(self) -> T { self } } fn main() {}").unwrap();
        assert_eq!(
            result
                .decls()
                .extends()
                .filter(|ext| ext.target == Type::Int)
                .count(),
            0
        );
    }

    #[test]
    fn collects_extend_on_struct() {
        let result = check(
            "struct Point { x: int } extend Point { fn len(self) -> int { 0 } } fn main() {}",
        )
        .unwrap();
        let ty = nominal(NominalKind::Struct, "Point", vec![], None);
        let ext = result
            .decls()
            .extends()
            .find(|ext| ext.target == ty)
            .expect("no extend");
        assert!(
            ext.methods
                .contains_key(&MethodKey::instance(Ident::new("len")))
        );
    }
}

mod extend_calls {
    use super::*;

    fn assert_extend_target(
        result: &TypecheckResult,
        index: usize,
        name: &str,
        surface: MethodSurface,
        type_args: Vec<Type>,
    ) {
        let target = result.calls().values().next().expect("missing call target");
        assert_eq!(
            target,
            &CallTarget::new(
                CallableId::extend_method(
                    ExtendId {
                        module: ModuleScope::Root,
                        index,
                    },
                    Ident::new(name),
                    surface,
                ),
                GenericArgs {
                    type_args,
                    const_args: vec![],
                }
            )
        );
    }

    fn buf(src: &str) -> String {
        format!("struct FixedBuf<T, N: int> {{ data: [T; N] }} {src}")
    }

    fn assert_buf_type(src: &str, ty: Type) {
        assert_ty(&buf(src), ty);
    }

    fn assert_buf_err(src: &str) {
        assert_err(&buf(src));
    }

    #[test]
    fn call_extend_on_primitive() {
        assert_ty(
            "extend int { fn double(self) -> int { self * 2 } } fn main() -> int { 5.double() }",
            Type::Int,
        );
    }

    #[test]
    fn call_extend_with_args() {
        assert_ty(
            "extend int { fn add(self, v: int) -> int { self + v } } fn main() -> int { 1.add(2) }",
            Type::Int,
        );
    }

    #[test]
    fn extend_wrong_arg_count() {
        assert_err_count(
            "extend int { fn add(self, v: int) -> int { self + v } } fn main() { 1.add(); }",
            1,
        );
    }

    #[test]
    fn extend_wrong_arg_type() {
        assert_err_count(
            "extend int { fn add(self, v: int) -> int { self + v } } fn main() { 1.add(true); }",
            1,
        );
    }

    #[test]
    fn call_target_extend() {
        let result =
            check("extend int { fn double(self) -> int { self * 2 } } fn main() { 5.double(); }")
                .unwrap();
        assert_extend_target(&result, 0, "double", MethodSurface::Instance, vec![]);
    }

    #[test]
    fn call_target_static_extend() {
        let result = check(
            "struct Point { x: int } extend Point { fn zero() -> int { 0 } } fn main() { Point.zero(); }",
        )
        .unwrap();
        assert_extend_target(&result, 0, "zero", MethodSurface::Static, vec![]);
    }

    #[test]
    fn call_extend_on_struct() {
        assert_ty(
            "struct Point { x: int } extend Point { fn len(self) -> int { 0 } } fn main() -> int { let p = Point { x: 1 }; p.len() }",
            Type::Int,
        );
    }

    #[test]
    fn extend_via_variable() {
        assert_ty(
            "extend int { fn double(self) -> int { self * 2 } } fn main() -> int { let x = 3; x.double() }",
            Type::Int,
        );
    }

    #[test]
    fn concrete_extend_body_err() {
        assert_err_count(
            "extend int { fn bad(self) -> int { true } } fn main() { 1.bad(); }",
            1,
        );
    }

    #[test]
    fn generic_primitive() {
        assert_ty(
            "extend<T> T { fn tag(self) -> int { 0 } } fn main() -> int { 1.tag() }",
            Type::Int,
        );
    }

    #[test]
    fn generic_unbound_owner_arg() {
        let errors = errors("extend<T, U> T { fn bad(self) -> int { 0 } } fn main() { 1.bad(); }");
        assert!(errors.iter().any(|err| matches!(
            err,
            TypeError::Decl(DeclError::UnusedExtendTypeParam { name, .. }) if *name == Ident::new("U")
        )));
    }

    #[test]
    fn receiver_mismatch_not_unbound() {
        let errors = errors(
            "struct Box<T> { value: T } extend<T> Box<T> { fn get(self) -> T { self.value } } fn main() { 1.get(); }",
        );
        assert!(
            errors
                .iter()
                .any(|err| matches!(err, TypeError::MemberAccessOnNonAggregate { .. }))
        );
        assert!(
            !errors
                .iter()
                .any(|err| matches!(err, TypeError::UnboundGenericParam { .. }))
        );
    }

    #[test]
    fn generic_struct() {
        assert_ty(
            "struct Box<T> { value: T } extend<T> Box<T> { fn tag(self) -> int { 0 } } fn main() -> int { let b = Box { value: 1 }; b.tag() }",
            Type::Int,
        );
    }

    #[test]
    fn generic_return() {
        assert_ty(
            "extend<T> T { fn id(self) -> T { self } } fn main() -> int { 1.id() }",
            Type::Int,
        );
    }

    #[test]
    fn generic_call_target() {
        let result =
            check("extend<T> T { fn id(self) -> T { self } } fn main() { 1.id(); }").unwrap();
        assert_extend_target(&result, 0, "id", MethodSurface::Instance, vec![Type::Int]);
    }

    #[test]
    fn generic_param() {
        assert_ty(
            "extend<T> T { fn pick(self, x: T) -> T { x } } fn main() -> int { 1.pick(2) }",
            Type::Int,
        );
    }

    #[test]
    fn generic_receiver_args() {
        assert_ty(
            "struct Box<T> { value: T } extend<T> Box<T> { fn get(self) -> T { self.value } } fn main() -> int { let b = Box { value: 1 }; b.get() }",
            Type::Int,
        );
    }

    #[test]
    fn generic_dataref() {
        assert_ty(
            "dataref Box<T> { value: T } extend<T> Box<T> { fn get(self) -> T { self.value } } fn main() -> bool { let b = Box { value: true }; b.get() }",
            Type::Bool,
        );
    }

    #[test]
    fn generic_enum() {
        assert_ty(
            "enum Option<T> { Some(T), None } extend<T> Option<T> { fn keep(self, other: Option<T>) -> Option<T> { other } } fn main() -> int { let _: Option<int> = Option.Some(1).keep(Option.Some(2)); 0 }",
            Type::Int,
        );
    }

    #[test]
    fn generic_extend_body_err() {
        assert_err_count(
            "fn mul2(x: int) -> int { x * 2 } extend<T> T { fn duplicate(self) -> T { mul2(self) } } fn main() { true.duplicate(); }",
            2,
        );
    }

    #[test]
    fn public_extend_private_slots() {
        let result = check_named(
            "import ext { * }; fn main() { 1.bad(); }",
            &[(
                "ext",
                "extend bool { fn keep(self) -> bool { self } } pub extend string { fn keep(self) -> string { self } } pub extend<T> T { fn bad(self) -> T { true } }",
            )],
        );
        let Err(errors) = result else {
            panic!("expected generic extend body error");
        };

        assert_eq!(errors.len(), 1);
    }

    #[test]
    fn body_once_per_spec() {
        assert_err_count(
            "fn mul2(x: int) -> int { x * 2 } extend<T> T { fn duplicate(self) -> T { mul2(self) } } fn main() { true.duplicate(); true.duplicate(); }",
            2,
        );
    }

    #[test]
    fn generic_extend_self_body_err() {
        assert_err_count(
            "struct Box<T> { value: T } extend<T> Box<T> { fn get(self) -> T { true } } fn main() { let b = Box { value: 1 }; b.get(); }",
            1,
        );
    }

    #[test]
    fn exact_beats_generic() {
        assert_ty(
            "extend<T> T { fn tag(self) -> bool { true } } extend int { fn tag(self) -> int { self } } fn main() -> int { 1.tag() }",
            Type::Int,
        );
    }

    #[test]
    fn exact_const_target() {
        assert_buf_type(
            r#"
                extend FixedBuf<int, 3> { fn tag(self) -> string { "exact" } }
                fn main() { FixedBuf { data: [1, 2, 3] }.tag(); }
            "#,
            Type::String,
        );
    }

    #[test]
    fn partial_type() {
        assert_buf_type(
            r#"
                extend<T> FixedBuf<T, 5> { fn tag(self) -> string { "cap" } }
                fn main() { FixedBuf { data: [1, 2, 3, 4, 5] }.tag(); }
            "#,
            Type::String,
        );
    }

    #[test]
    fn partial_const() {
        assert_buf_type(
            r#"
                extend<N: int> FixedBuf<float, N> { fn tag(self) -> string { "float" } }
                fn main() { FixedBuf { data: [1.0, 2.0] }.tag(); }
            "#,
            Type::String,
        );
    }

    #[test]
    fn generic_const() {
        assert_buf_type(
            r#"
                extend<T, N: int> FixedBuf<T, N> { fn tag(self) -> string { "any" } }
                fn main() { FixedBuf { data: [1, 2] }.tag(); }
            "#,
            Type::String,
        );
    }

    #[test]
    fn body_const() {
        assert_buf_type(
            "
                extend<T, N: int> FixedBuf<T, N> { fn len(self) -> int { N } }
                fn main() -> int { FixedBuf { data: [1, 2, 3] }.len() }
            ",
            Type::Int,
        );
    }

    #[test]
    fn partial_over_generic() {
        assert_buf_type(
            "
                extend<T, N: int> FixedBuf<T, N> { fn tag(self) -> bool { true } }
                extend<T> FixedBuf<T, 5> { fn tag(self) -> int { 5 } }
                fn main() -> int { FixedBuf { data: [1, 2, 3, 4, 5] }.tag() }
            ",
            Type::Int,
        );
    }

    #[test]
    fn exact_array_len() {
        assert_ty(
            "
                extend<T, N: int> [T; N] { fn tag(self) -> bool { true } }
                extend<T> [T; 3] { fn tag(self) -> int { 3 } }
                fn main() -> int { [1, 2, 3].tag() }
            ",
            Type::Int,
        );
    }

    #[test]
    fn nested() {
        assert_ty(
            "extend<T, N: int> [T; N] { fn tag(self) -> bool { true } } extend<T, N: int, M: int> [[T; N]; M] { fn tag(self) -> int { 0 } } fn main() -> int { [[1]].tag() }",
            Type::Int,
        );
    }

    #[test]
    fn tuple_ambig() {
        assert_err(
            "extend<T> (T, int) { fn tag(self) -> int { 0 } } extend<T> (string, T) { fn tag(self) -> int { 1 } } fn main() { (\"x\", 1).tag(); }",
        );
    }

    #[test]
    fn exact_target() {
        assert_buf_type(
            "
                extend<T> FixedBuf<T, 5> { fn tag(self) -> bool { true } }
                extend<N: int> FixedBuf<int, N> { fn tag(self) -> bool { true } }
                extend FixedBuf<int, 5> { fn tag(self) -> int { 5 } }
                fn main() -> int { FixedBuf { data: [1, 2, 3, 4, 5] }.tag() }
            ",
            Type::Int,
        );
    }

    #[test]
    fn partial_ambiguity() {
        assert_buf_err(
            r#"
                extend<T> FixedBuf<T, 5> { fn tag(self) -> string { "cap5" } }
                extend<N: int> FixedBuf<int, N> { fn tag(self) -> string { "intN" } }
                fn main() { FixedBuf { data: [1, 2, 3, 4, 5] }.tag(); }
            "#,
        );
    }

    #[test]
    fn ambiguous_generic() {
        assert_err(
            "extend<T> T { fn tag(self) -> int { 0 } } extend<U> U { fn tag(self) -> int { 1 } } fn main() { 1.tag(); }",
        );
    }

    #[test]
    fn ambiguous_exact() {
        assert_err(
            "extend int { fn tag(self) -> int { 0 } } extend int { fn tag(self) -> int { 1 } } fn main() { 1.tag(); }",
        );
    }

    #[test]
    fn method_beats_extend() {
        let errors = errors(
            "struct Point { 
                x: int,

                fn len(self) -> int { 0 } 
            } 
            
            extend Point { 
                fn len(self) -> bool { true } 
            } 
            
            fn main() -> int { 
                let p = Point { x: 1 }; 
                p.len()
            }",
        );
        assert!(errors.iter().any(|err| matches!(
            err,
            TypeError::Decl(DeclError::ExtendMethodConflict { name, .. }) if *name == Ident::new("len")
        )));
    }
}

mod enum_variants {
    use super::*;

    fn color_type() -> Type {
        nominal(NominalKind::Enum, "Color", vec![], None)
    }

    fn option_type(inner: Type) -> Type {
        nominal(NominalKind::Enum, "Option", vec![inner], None)
    }

    #[test]
    fn enum_unit_variant() {
        assert_ty(
            "enum Color { Red, Blue } fn main() { Color.Red; }",
            color_type(),
        );
    }

    #[test]
    fn enum_tuple_variant() {
        assert_ty(
            "enum Color { Rgb(int, int, int) } fn main() { Color.Rgb(1, 2, 3); }",
            color_type(),
        );
    }

    #[test]
    fn enum_variant_call_target() {
        assert_calls(
            "enum Color { Rgb(int, int, int) } fn main() { Color.Rgb(1, 2, 3); }",
            1,
        );
    }

    #[test]
    fn unknown_enum_variant() {
        assert_err("enum Color { Red } fn main() { Color.Blue; }");
    }

    #[test]
    fn variant_not_on_value() {
        assert_err("enum Color { Red, Blue } fn main() { let c = Color.Red; c.Blue; }");
    }

    #[test]
    fn tuple_variant_wrong_args() {
        assert_err("enum Color { Rgb(int, int, int) } fn main() { Color.Rgb(1, 2); }");
    }

    #[test]
    fn tuple_variant_type_mismatch() {
        assert_err("enum Color { Rgb(int, int, int) } fn main() { Color.Rgb(1, 2, true); }");
    }

    #[test]
    fn unit_variant_no_args() {
        assert_ty(
            "enum Color { Red } fn main() { Color.Red(); }",
            color_type(),
        );
    }

    #[test]
    fn unit_variant_args_err() {
        assert_err("enum Color { Red } fn main() { Color.Red(42); }");
    }

    #[test]
    fn unit_variant_via_variable() {
        assert_ty(
            "enum Color { Red, Blue } fn main() { let c = Color.Red; c; }",
            color_type(),
        );
    }

    #[test]
    fn tuple_infer() {
        assert_ty(
            "enum Option<T> { Some(T), None } fn main() { Option.Some(42); }",
            option_type(Type::Int),
        );
    }

    #[test]
    fn tuple_explicit_args() {
        assert_ty(
            "enum Option<T> { Some(T), None } fn main() { Option.Some<int>(42); }",
            option_type(Type::Int),
        );
    }

    #[test]
    fn tuple_payload_mismatch() {
        assert_err_count(
            "enum Option<T> { Some(T), None } fn main() { Option.Some<int>(true); }",
            1,
        );
    }

    #[test]
    fn unit_unconstrained_err() {
        assert_err_count(
            "enum Option<T> { Some(T), None } fn main() { Option.None(); }",
            1,
        );
    }

    #[test]
    fn unit_explicit_args() {
        assert_ty(
            "enum Option<T> { Some(T), None } fn main() { Option.None<int>(); }",
            option_type(Type::Int),
        );
    }

    #[test]
    fn tuple_optional_nil() {
        assert_ty(
            "enum Option<T> { Some(T), None } fn main() { let x: Option<int?> = Option.Some(nil); x; }",
            option_type(Type::option_of(Type::Int)),
        );
    }

    #[test]
    fn tuple_nil_no_leak() {
        let checked = check(
            "enum Option<T> { Some(T), None } fn main() { let x: Option<int?> = Option.Some(nil); x; }",
        )
        .expect("typecheck failed");
        assert_typecheck_closed(&checked);
    }

    #[test]
    fn unit_expected_return() {
        assert_ty(
            "enum Option<T> { Some(T), None } fn main() -> Option<int> { Option.None() }",
            option_type(Type::Int),
        );
    }

    #[test]
    fn unit_expected_binding() {
        assert_ty(
            "enum Option<T> { Some(T), None } fn main() -> Option<int> { let x: Option<int> = Option.None; x }",
            option_type(Type::Int),
        );
    }
}
