use super::support::{
    assert_calls, assert_err, assert_err_count, assert_no_infer_vars_in_result, assert_type,
    assert_type_with_modules, typecheck,
};
use crate::{
    ast::{ArrayLen, Ident, Type},
    typecheck::{
        CallTarget, GenericArgs, TypecheckResult,
        decls::{
            CallableId, CallableKind, CallableParent, ExtendId, ModuleScope, NominalKey,
            NominalKind, VariantSchema,
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

mod field_access {
    use super::*;

    #[test]
    fn field_access() {
        assert_type(
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
        assert_type(
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
        assert_err_count(
            "struct Point { x: int } 
            fn main() { 
                let p = Point { x: 1 }; 
                p.z; 
            }",
            1,
        );
    }

    #[test]
    fn field_on_int() {
        assert_err_count("fn main() { let x = 1; x.y; }", 1);
    }

    #[test]
    fn imported_field_access() {
        assert_type_with_modules(
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
    fn receiver_args() {
        assert_type(
            "struct Wrapper<T> { value: T } fn main() { let w = Wrapper { value: 42 }; w.value; }",
            Type::Int,
        );
    }

    #[test]
    fn dataref_receiver_args() {
        assert_type(
            "dataref Box<T> { value: T } fn main() { let b = Box { value: true }; b.value; }",
            Type::Bool,
        );
    }

    #[test]
    fn nested_receiver_args() {
        assert_type(
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
        assert_type_with_modules(
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
        assert_type(
            "struct Point { x: int, y: int } fn main() { Point { x: 1, y: 2 }; }",
            Type::Struct {
                name: Ident::new("Point"),
                type_args: vec![],
                const_args: vec![],
                origin: None,
            },
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
        assert_type_with_modules(
            "import gamekit { Point }; fn main() { Point { x: 1, y: 2 }; }",
            "pub struct Point { x: int, y: int }",
            Type::Struct {
                name: Ident::new("Point"),
                type_args: vec![],
                const_args: vec![],
                origin: None,
            },
        );
    }

    #[test]
    fn struct_infer() {
        assert_type(
            "struct Wrapper<T> { value: T } fn main() { Wrapper { value: 42 }; }",
            Type::Struct {
                name: Ident::new("Wrapper"),
                type_args: vec![Type::Int],
                const_args: vec![],
                origin: None,
            },
        );
    }

    #[test]
    fn dataref_infer() {
        assert_type(
            "dataref Box<T> { value: T } fn main() { Box { value: \"hi\" }; }",
            Type::DataRef {
                name: Ident::new("Box"),
                type_args: vec![Type::String],
                const_args: vec![],
                origin: None,
            },
        );
    }

    #[test]
    fn annotation_unconstrained() {
        assert_type(
            "struct Token<T> {} fn main() { let value: Token<int> = Token {}; value; }",
            Type::Struct {
                name: Ident::new("Token"),
                type_args: vec![Type::Int],
                const_args: vec![],
                origin: None,
            },
        );
    }

    #[test]
    fn nested_annotation() {
        assert_type(
            "struct Token<T> {}
             struct Wrapper<T> { value: Token<T> } 
             fn main() { 
                let w: Wrapper<int> = Wrapper { value: Token {} }; 
                w.value; 
            }",
            Type::Struct {
                name: Ident::new("Token"),
                type_args: vec![Type::Int],
                const_args: vec![],
                origin: None,
            },
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
        let result = typecheck("struct Wrapper<T> { value: T } fn main() {}").unwrap();
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
        let result = typecheck("dataref Buffer<T, N: int> { data: [T; N] } fn main() {}").unwrap();
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
        let result = typecheck("enum Option<T> { Some(T), None } fn main() {}").unwrap();
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
        let result = typecheck(
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
        let method = agg.methods.get(&Ident::new("len")).expect("no method");
        assert!(method.receiver.is_some());
        assert_eq!(method.params.len(), 0);
        assert_eq!(method.ret, Type::Int);
    }

    #[test]
    fn collects_static_method() {
        let result = typecheck(
            "struct Point { x: int, fn origin() -> Point { Point { x: 0 } } } 
            fn main() {}
        ",
        )
        .unwrap();
        let agg = result
            .decls()
            .aggregate(&root_key(NominalKind::Struct, "Point"))
            .expect("no aggregate");
        let method = agg.methods.get(&Ident::new("origin")).expect("no method");
        assert!(method.receiver.is_none());
        assert_eq!(method.params.len(), 0);
    }

    #[test]
    fn instance_method_with_params() {
        let result =
            typecheck("struct Point { x: int, fn add(self, v: int) -> int { 0 } } fn main() {}")
                .unwrap();
        let agg = result
            .decls()
            .aggregate(&root_key(NominalKind::Struct, "Point"))
            .expect("no aggregate");
        let method = agg.methods.get(&Ident::new("add")).expect("no method");
        assert!(method.receiver.is_some());
        assert_eq!(method.params.len(), 1);
        assert_eq!(method.params[0].ty, Type::Int);
        assert_eq!(method.ret, Type::Int);
    }

    #[test]
    fn collects_generic_instance_method() {
        let result =
            typecheck("struct Point { fn map<T>(self, f: T) -> T { f } } fn main() {}").unwrap();
        let agg = result
            .decls()
            .aggregate(&root_key(NominalKind::Struct, "Point"))
            .expect("no aggregate");
        let method = agg.methods.get(&Ident::new("map")).expect("no method");
        let t = method.generics.type_params[0].id;

        assert!(method.receiver.is_some());
        assert_eq!(method.generics.type_params.len(), 1);
        assert!(method.generics.const_params.is_empty());
        assert_eq!(method.params[0].ty, Type::Var(t));
        assert_eq!(method.ret, Type::Var(t));
    }

    #[test]
    fn collects_generic_static_method() {
        let result = typecheck(
            "struct Point { fn wrap<T, N: int>(x: [T; N]) -> [T; N] { x } } fn main() {}",
        )
        .unwrap();
        let agg = result
            .decls()
            .aggregate(&root_key(NominalKind::Struct, "Point"))
            .expect("no aggregate");
        let method = agg.methods.get(&Ident::new("wrap")).expect("no method");
        let t = method.generics.type_params[0].id;
        let n = method.generics.const_params[0].id;
        let array_ty = Type::Array {
            elem: Box::new(Type::Var(t)),
            len: ArrayLen::Param(n),
        };

        assert!(method.receiver.is_none());
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
        type_args: Vec<Type>,
    ) {
        let target = result.calls().values().next().expect("missing call target");
        assert_eq!(
            target,
            &CallTarget::Method {
                owner: root_key(NominalKind::Struct, owner),
                name: Ident::new(name),
                type_args,
                const_args: vec![],
            }
        );
    }

    #[test]
    fn method_call() {
        assert_type(
            "struct Point { x: int, fn len(self) -> int { 0 } } fn main() -> int { let p = Point { x: 1 }; p.len() }",
            Type::Int,
        );
    }

    #[test]
    fn method_call_with_args() {
        assert_type(
            "struct Point { x: int, fn add(self, v: int) -> int { 0 } } fn main() -> int { let p = Point { x: 1 }; p.add(2) }",
            Type::Int,
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
        let result = typecheck(
            "struct Point { x: int, fn len(self) -> int { 0 } } fn main() { let p = Point { x: 1 }; p.len(); }",
        )
        .unwrap();
        assert_method_target(&result, "Point", "len", vec![]);
    }

    #[test]
    fn type_name_is_not_receiver() {
        assert_err("struct Point { x: int, fn len(self) -> int { 0 } } fn main() { Point.len(); }");
    }

    #[test]
    fn static_explicit_args() {
        assert_type(
            "struct Foo { fn make<T>(x: T) -> T { x } } fn main() -> int { Foo.make<int>(42) }",
            Type::Int,
        );
    }

    #[test]
    fn static_infer_args() {
        assert_type(
            "struct Foo { fn make<T>(x: T) -> T { x } } fn main() -> string { Foo.make(\"hi\") }",
            Type::String,
        );
    }

    #[test]
    fn instance_explicit_args() {
        assert_type(
            "struct Foo { fn pair<A, B>(self, a: A, b: B) -> B { b } } fn main() -> string { let f = Foo {}; f.pair<int, string>(1, \"ok\") }",
            Type::String,
        );
    }

    #[test]
    fn instance_infer_args() {
        assert_type(
            "struct Foo { fn pair<A, B>(self, a: A, b: B) -> B { b } } fn main() -> int { let f = Foo {}; f.pair(\"x\", 7) }",
            Type::Int,
        );
    }

    #[test]
    fn owner_instance_receiver_args() {
        assert_type(
            "struct Wrapper<T> { value: T, fn get(self) -> T { self.value } } fn main() -> int { let w = Wrapper { value: 42 }; w.get() }",
            Type::Int,
        );
    }

    #[test]
    fn owner_static_infer_args() {
        assert_type(
            "struct Wrapper<T> { value: T, fn new(value: T) -> Self { Wrapper { value: value } } } fn main() -> int { Wrapper.new(42).value }",
            Type::Int,
        );
    }

    #[test]
    fn static_expected_nil() {
        assert_type(
            "struct Foo { fn id<T>(x: T) -> T { x } } fn main() { let x: int? = Foo.id(nil); x; }",
            Type::option_of(Type::Int),
        );
    }

    #[test]
    fn static_nil_no_leak() {
        let checked = typecheck(
            "struct Foo { fn id<T>(x: T) -> T { x } } fn main() { let x: int? = Foo.id(nil); x; }",
        )
        .expect("typecheck failed");
        assert_no_infer_vars_in_result(&checked);
    }

    #[test]
    fn instance_expected_nil() {
        assert_type(
            "struct Foo { fn id<T>(self, x: T) -> T { x } } fn main() { let f = Foo {}; let x: int? = f.id(nil); x; }",
            Type::option_of(Type::Int),
        );
    }

    #[test]
    fn static_target_args() {
        let result = typecheck(
            "struct Foo { fn make<T>(x: T) -> T { x } } fn main() { Foo.make<int>(42); }",
        )
        .unwrap();
        assert_method_target(&result, "Foo", "make", vec![Type::Int]);
    }

    #[test]
    fn owner_static_target_args() {
        let result = typecheck(
            "struct Wrapper<T> { value: T, fn new(value: T) -> Self { Wrapper { value: value } } } fn main() { Wrapper.new(42); }",
        )
        .unwrap();
        assert_method_target(&result, "Wrapper", "new", vec![Type::Int]);
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
        let result = typecheck(
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
            typecheck("extend int { fn double(self) -> int { self * 2 } } fn main() {}").unwrap();
        let mut found = false;
        for ext in result.decls().extends_for(&Type::Int) {
            if ext.methods.get(&Ident::new("double")).is_some() {
                found = true;
                break;
            }
        }
        assert!(found, "extend method 'double' not found on int");
    }

    #[test]
    fn extend_method_params_skip_self() {
        let result =
            typecheck("extend int { fn add(self, v: int) -> int { self + v } } fn main() {}")
                .unwrap();
        let ext = result
            .decls()
            .extends_for(&Type::Int)
            .next()
            .expect("no extend");
        let method = ext.methods.get(&Ident::new("add")).expect("no method");
        assert_eq!(method.params.len(), 1);
        assert_eq!(method.params[0].ty, Type::Int);
        assert_eq!(method.ret, Type::Int);
    }

    #[test]
    fn generic_extend() {
        let result = typecheck(
            "struct Box<T> { value: T } extend<T> Box<T> { fn get(self) -> T { self.value } } fn main() {}",
        )
        .unwrap();
        let ext = result.decls().extends().next().expect("no extend");
        let method = ext.methods.get(&Ident::new("get")).expect("no method");

        assert_eq!(
            ext.target,
            Type::Struct {
                name: Ident::new("Box"),
                type_args: vec![Type::UnresolvedNominal {
                    qualifier: None,
                    name: Ident::new("T"),
                    generic_args: vec![],
                }],
                const_args: vec![],
                origin: None,
            }
        );
        assert_eq!(ext.generics.type_params.len(), 1);
        assert!(ext.generics.const_params.is_empty());
        assert!(method.generics.is_empty());
        assert!(method.params.is_empty());
        assert_eq!(
            method.ret,
            Type::UnresolvedNominal {
                qualifier: None,
                name: Ident::new("T"),
                generic_args: vec![],
            }
        );
    }

    #[test]
    fn extends_for_exact() {
        let result = typecheck("extend<T> T { fn id(self) -> T { self } } fn main() {}").unwrap();
        assert_eq!(result.decls().extends_for(&Type::Int).count(), 0);
    }

    #[test]
    fn collects_extend_on_struct() {
        let result = typecheck(
            "struct Point { x: int } extend Point { fn len(self) -> int { 0 } } fn main() {}",
        )
        .unwrap();
        let ty = Type::Struct {
            name: Ident::new("Point"),
            type_args: vec![],
            const_args: vec![],
            origin: None,
        };
        let ext = result.decls().extends_for(&ty).next().expect("no extend");
        assert!(ext.methods.contains_key(&Ident::new("len")));
    }
}

mod extend_calls {
    use super::*;

    fn assert_extend_target(
        result: &TypecheckResult,
        index: usize,
        name: &str,
        receiver: Type,
        type_args: Vec<Type>,
    ) {
        let target = result.calls().values().next().expect("missing call target");
        assert_eq!(
            target,
            &CallTarget::Extend {
                target: CallableId {
                    module: ModuleScope::Root,
                    parent: Some(CallableParent::Extend(ExtendId {
                        module: ModuleScope::Root,
                        index,
                    })),
                    kind: CallableKind::ExtendMethod,
                    name: Ident::new(name),
                },
                receiver,
                args: GenericArgs {
                    type_args,
                    const_args: vec![],
                },
            }
        );
    }

    fn buf(src: &str) -> String {
        format!("struct FixedBuf<T, N: int> {{ data: [T; N] }} {src}")
    }

    fn assert_buf_type(src: &str, ty: Type) {
        assert_type(&buf(src), ty);
    }

    fn assert_buf_err(src: &str) {
        assert_err(&buf(src));
    }

    #[test]
    fn call_extend_on_primitive() {
        assert_type(
            "extend int { fn double(self) -> int { self * 2 } } fn main() -> int { 5.double() }",
            Type::Int,
        );
    }

    #[test]
    fn call_extend_with_args() {
        assert_type(
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
        let result = typecheck(
            "extend int { fn double(self) -> int { self * 2 } } fn main() { 5.double(); }",
        )
        .unwrap();
        assert_extend_target(&result, 0, "double", Type::Int, vec![]);
    }

    #[test]
    fn call_extend_on_struct() {
        assert_type(
            "struct Point { x: int } extend Point { fn len(self) -> int { 0 } } fn main() -> int { let p = Point { x: 1 }; p.len() }",
            Type::Int,
        );
    }

    #[test]
    fn extend_via_variable() {
        assert_type(
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
        assert_type(
            "extend<T> T { fn tag(self) -> int { 0 } } fn main() -> int { 1.tag() }",
            Type::Int,
        );
    }

    #[test]
    fn generic_struct() {
        assert_type(
            "struct Box<T> { value: T } extend<T> Box<T> { fn tag(self) -> int { 0 } } fn main() -> int { let b = Box { value: 1 }; b.tag() }",
            Type::Int,
        );
    }

    #[test]
    fn generic_return() {
        assert_type(
            "extend<T> T { fn id(self) -> T { self } } fn main() -> int { 1.id() }",
            Type::Int,
        );
    }

    #[test]
    fn generic_call_target() {
        let result =
            typecheck("extend<T> T { fn id(self) -> T { self } } fn main() { 1.id(); }").unwrap();
        assert_extend_target(&result, 0, "id", Type::Int, vec![Type::Int]);
    }

    #[test]
    fn generic_param() {
        assert_type(
            "extend<T> T { fn pick(self, x: T) -> T { x } } fn main() -> int { 1.pick(2) }",
            Type::Int,
        );
    }

    #[test]
    fn generic_receiver_args() {
        assert_type(
            "struct Box<T> { value: T } extend<T> Box<T> { fn get(self) -> T { self.value } } fn main() -> int { let b = Box { value: 1 }; b.get() }",
            Type::Int,
        );
    }

    #[test]
    fn generic_dataref() {
        assert_type(
            "dataref Box<T> { value: T } extend<T> Box<T> { fn get(self) -> T { self.value } } fn main() -> bool { let b = Box { value: true }; b.get() }",
            Type::Bool,
        );
    }

    #[test]
    fn generic_enum() {
        assert_type(
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
        assert_type(
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
        assert_type(
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
        assert_type(
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
        assert_type(
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
            Type::Int,
        );
    }
}

mod enum_variants {
    use super::*;

    fn color_type() -> Type {
        Type::Enum {
            name: Ident::new("Color"),
            type_args: vec![],
            const_args: vec![],
            origin: None,
        }
    }

    fn option_type(inner: Type) -> Type {
        Type::Enum {
            name: Ident::new("Option"),
            type_args: vec![inner],
            const_args: vec![],
            origin: None,
        }
    }

    #[test]
    fn enum_unit_variant() {
        assert_type(
            "enum Color { Red, Blue } fn main() { Color.Red; }",
            color_type(),
        );
    }

    #[test]
    fn enum_tuple_variant() {
        assert_type(
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
    fn enum_tuple_variant_wrong_args() {
        assert_err("enum Color { Rgb(int, int, int) } fn main() { Color.Rgb(1, 2); }");
    }

    #[test]
    fn enum_tuple_variant_type_mismatch() {
        assert_err("enum Color { Rgb(int, int, int) } fn main() { Color.Rgb(1, 2, true); }");
    }

    #[test]
    fn enum_unit_variant_no_args() {
        assert_type(
            "enum Color { Red } fn main() { Color.Red(); }",
            color_type(),
        );
    }

    #[test]
    fn unit_variant_args_err() {
        assert_err("enum Color { Red } fn main() { Color.Red(42); }");
    }

    #[test]
    fn enum_unit_variant_via_variable() {
        assert_type(
            "enum Color { Red, Blue } fn main() { let c = Color.Red; c; }",
            color_type(),
        );
    }

    #[test]
    fn tuple_infer() {
        assert_type(
            "enum Option<T> { Some(T), None } fn main() { Option.Some(42); }",
            option_type(Type::Int),
        );
    }

    #[test]
    fn tuple_explicit_args() {
        assert_type(
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
        assert_type(
            "enum Option<T> { Some(T), None } fn main() { Option.None<int>(); }",
            option_type(Type::Int),
        );
    }

    #[test]
    fn tuple_optional_nil() {
        assert_type(
            "enum Option<T> { Some(T), None } fn main() { let x: Option<int?> = Option.Some(nil); x; }",
            option_type(Type::option_of(Type::Int)),
        );
    }

    #[test]
    fn tuple_nil_no_leak() {
        let checked = typecheck(
            "enum Option<T> { Some(T), None } fn main() { let x: Option<int?> = Option.Some(nil); x; }",
        )
        .expect("typecheck failed");
        assert_no_infer_vars_in_result(&checked);
    }

    #[test]
    fn unit_expected_return() {
        assert_type(
            "enum Option<T> { Some(T), None } fn main() -> Option<int> { Option.None() }",
            option_type(Type::Int),
        );
    }
}
