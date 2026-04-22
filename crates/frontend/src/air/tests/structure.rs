use super::*;
use crate::ast::Ident;

#[test]
fn empty_program() {
    let program = Program::default();
    assert!(program.entry().is_none());
    assert!(program.modules.is_empty());
    assert!(program.functions.is_empty());
    assert!(program.externs.is_empty());
    assert!(program.extern_types.is_empty());
    assert!(program.aggregates.is_empty());
    assert!(program.enums.is_empty());
    assert!(program.type_arena.is_empty());
    assert!(program.const_arena.is_empty());
}

#[test]
fn type_arena_all_variants() {
    let mut arena = TypeArena::default();
    let int_ty = arena.alloc(TypeData::Int);
    let bool_ty = arena.alloc(TypeData::Bool);
    let void_ty = arena.alloc(TypeData::Void);
    let tuple_ty = arena.alloc(TypeData::Tuple(vec![int_ty, bool_ty]));
    let list_ty = arena.alloc(TypeData::List(int_ty));
    let array_ty = arena.alloc(TypeData::Array {
        elem: int_ty,
        len: 10,
    });
    let sig = SignatureType::new(vec![int_ty, bool_ty], void_ty);
    let fn_ty = arena.alloc(TypeData::Function(sig));

    assert_eq!(arena.data(int_ty), &TypeData::Int);
    assert_eq!(arena.data(bool_ty), &TypeData::Bool);
    assert_eq!(arena.data(void_ty), &TypeData::Void);
    assert_eq!(
        arena.data(tuple_ty),
        &TypeData::Tuple(vec![int_ty, bool_ty])
    );
    assert_eq!(arena.data(list_ty), &TypeData::List(int_ty));
    assert_eq!(
        arena.data(array_ty),
        &TypeData::Array {
            elem: int_ty,
            len: 10
        }
    );
    assert!(matches!(arena.data(fn_ty), TypeData::Function(_)));
    assert_eq!(arena.len(), 7);
}

#[test]
fn const_arena() {
    let mut arena = ConstArena::default();
    let int_ty = TypeId::from_index(0);
    let bool_ty = TypeId::from_index(1);

    let c1 = arena.alloc(ConstData {
        ty: int_ty,
        value: ConstValue::Int(42),
    });
    let c2 = arena.alloc(ConstData {
        ty: bool_ty,
        value: ConstValue::Bool(true),
    });
    let c3 = arena.alloc(ConstData {
        ty: int_ty,
        value: ConstValue::Nil,
    });

    assert_eq!(arena.get(c1).value, ConstValue::Int(42));
    assert_eq!(arena.get(c2).value, ConstValue::Bool(true));
    assert_eq!(arena.get(c3).value, ConstValue::Nil);
    assert_eq!(arena.len(), 3);
}

#[test]
fn aggregate_field_order() {
    let mut builder = ProgramBuilder::new();
    let int_ty = builder.alloc_type(TypeData::Int);
    let bool_ty = builder.alloc_type(TypeData::Bool);

    let agg_id = builder.alloc_aggregate(AggregateDecl {
        name: Ident::new("Point"),
        module: ModuleId::from_index(0),
        kind: AggregateKind::Struct,
        fields: vec![
            FieldDecl {
                name: Ident::new("x"),
                ty: int_ty,
            },
            FieldDecl {
                name: Ident::new("y"),
                ty: int_ty,
            },
            FieldDecl {
                name: Ident::new("flag"),
                ty: bool_ty,
            },
        ],
        cycle_capable: false,
    });

    let program = builder.finish();
    let fields = program.aggregate(agg_id).fields.clone();
    assert_eq!(fields.len(), 3);
    assert_eq!(fields[0].name.as_str(), "x");
    assert_eq!(fields[1].name.as_str(), "y");
    assert_eq!(fields[2].name.as_str(), "flag");
}

#[test]
fn enum_variant_order() {
    let mut builder = ProgramBuilder::new();
    let int_ty = builder.alloc_type(TypeData::Int);
    let bool_ty = builder.alloc_type(TypeData::Bool);

    let enum_id = builder.alloc_enum(EnumDecl {
        name: Ident::new("Shape"),
        module: ModuleId::from_index(0),
        variants: vec![
            VariantDecl {
                name: Ident::new("Circle"),
                shape: VariantShape::Unit,
            },
            VariantDecl {
                name: Ident::new("Rect"),
                shape: VariantShape::Tuple(vec![int_ty, int_ty]),
            },
            VariantDecl {
                name: Ident::new("Flagged"),
                shape: VariantShape::Struct(vec![
                    FieldDecl {
                        name: Ident::new("value"),
                        ty: int_ty,
                    },
                    FieldDecl {
                        name: Ident::new("ok"),
                        ty: bool_ty,
                    },
                ]),
            },
        ],
    });

    let program = builder.finish();
    let variants = program.enum_decl(enum_id).variants.clone();
    assert_eq!(variants.len(), 3);
    assert!(matches!(variants[0].shape, VariantShape::Unit));
    assert!(matches!(variants[1].shape, VariantShape::Tuple(ref ts) if ts.len() == 2));
    assert!(matches!(variants[2].shape, VariantShape::Struct(ref fs) if fs.len() == 2));
}

#[test]
fn function_stable_ids() {
    let mut builder = ProgramBuilder::new();
    let void_ty = builder.alloc_type(TypeData::Void);

    let module = test_module(&mut builder);

    let mut fb = FunctionBuilder::new("test_fn", module, FunctionKind::Normal, void_ty);
    let param_a = fb.push_param("a", TypeId::from_index(1), ParamRole::Normal);
    let local_b = fb.push_local(
        Some("b"),
        TypeId::from_index(2),
        Mutability::Mutable,
        LocalKind::User,
    );
    let bb0 = fb.push_block(term_return_void());
    let bb1 = fb.push_block(term_unreachable());
    let func_id = builder.alloc_function(fb.finish());

    let program = builder.finish();
    let func = program.function(func_id);
    assert_eq!(func.locals.len(), 2);
    assert_eq!(func.body.len(), 2);
    assert_eq!(param_a.index(), 0);
    assert_eq!(local_b.index(), 1);
    let _ = (bb0, bb1);
}

#[test]
fn module_refs_stable() {
    let mut builder = ProgramBuilder::new();

    let m0 = builder.alloc_module(Module {
        path: vec![Ident::new("a")],
        functions: vec![],
        aggregates: vec![],
        enums: vec![],
        extern_types: vec![],
        externs: vec![],
    });
    let m1 = builder.alloc_module(Module {
        path: vec![Ident::new("b")],
        functions: vec![],
        aggregates: vec![],
        enums: vec![],
        extern_types: vec![],
        externs: vec![],
    });

    assert_eq!(m0.index(), 0);
    assert_eq!(m1.index(), 1);

    let program = builder.finish();
    assert_eq!(program.module(m0).path[0].as_str(), "a");
    assert_eq!(program.module(m1).path[0].as_str(), "b");
}

#[test]
fn program_accessors() {
    let mut builder = ProgramBuilder::new();
    let int_ty = builder.alloc_type(TypeData::Int);
    let void_ty = builder.alloc_type(TypeData::Void);

    let module = test_module(&mut builder);

    let agg_id = builder.alloc_aggregate(AggregateDecl {
        name: Ident::new("MyStruct"),
        module,
        kind: AggregateKind::Struct,
        fields: vec![FieldDecl {
            name: Ident::new("f"),
            ty: int_ty,
        }],
        cycle_capable: false,
    });

    let enum_id = builder.alloc_enum(EnumDecl {
        name: Ident::new("MyEnum"),
        module,
        variants: vec![VariantDecl {
            name: Ident::new("V"),
            shape: VariantShape::Unit,
        }],
    });

    let ext_ty_id = builder.alloc_extern_type(ExternTypeDecl {
        name: Ident::new("Ext"),
        module,
        rep: ExternRep::Shared,
        has_init: false,
        fields: vec![],
        methods: vec![],
        statics: vec![],
        operators: vec![],
    });

    let ext_id = builder.alloc_extern(ExternDecl {
        name: Ident::new("ext_fn"),
        module,
        member: ExternMember::FreeFunction,
        params: vec![int_ty],
        return_type: int_ty,
    });

    let mut fb = FunctionBuilder::new("my_fn", module, FunctionKind::Normal, void_ty);
    fb.push_block(term_return_void());
    let func_id = builder.alloc_function(fb.finish());

    builder.set_entry(func_id);
    let program = builder.finish();

    assert_eq!(program.aggregate(agg_id).name.as_str(), "MyStruct");
    assert_eq!(program.enum_decl(enum_id).name.as_str(), "MyEnum");
    assert_eq!(program.extern_type(ext_ty_id).name.as_str(), "Ext");
    assert_eq!(program.extern_decl(ext_id).name.as_str(), "ext_fn");
    assert_eq!(program.function(func_id).name.as_str(), "my_fn");
    assert_eq!(program.entry(), Some(func_id));
}

#[test]
fn id_roundtrip() {
    let mut builder = ProgramBuilder::new();
    let void_ty = builder.alloc_type(TypeData::Void);

    let module = test_module(&mut builder);

    let mut fb = FunctionBuilder::new("f", module, FunctionKind::Normal, void_ty);
    let bid = fb.push_block(term_return_void());
    let func_id = builder.alloc_function(fb.finish());
    let _program = builder.finish();

    for i in 0..10 {
        assert_eq!(TypeId::from_index(i).index(), i);
        assert_eq!(ConstId::from_index(i).index(), i);
        assert_eq!(LocalId::from_index(i).index(), i);
    }

    assert_eq!(BlockId::from_index(0), bid);
    assert_eq!(FunctionId::from_index(0), func_id);
    assert_eq!(ModuleId::from_index(0), module);
    let _ = void_ty;
}
