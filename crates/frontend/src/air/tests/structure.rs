use super::*;
use crate::{air::DynContractData, ast::Ident};

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
fn primitive_builder_helpers_cache() {
    let mut builder = ProgramBuilder::default();
    assert_eq!(builder.string_ty(), builder.string_ty());
    assert_eq!(builder.any_ty(), builder.any_ty());
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
    let slice_ty = arena.alloc(TypeData::Slice(int_ty));
    let dyn_ty = arena.alloc(TypeData::Dyn(DynContractData {
        display_name: "Drawable".to_string(),
        method_table_key: "named::Drawable".to_string(),
        concrete_printer: None,
    }));
    let sig = SignatureType::new(
        vec![
            ParamType {
                ty: int_ty,
                mode: ParamMode::Value,
                escape: ParamEscape::NonEscaping,
            },
            ParamType {
                ty: bool_ty,
                mode: ParamMode::Value,
                escape: ParamEscape::NonEscaping,
            },
        ],
        ReturnMode::Value(void_ty),
    );
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
    assert_eq!(arena.data(slice_ty), &TypeData::Slice(int_ty));
    assert!(matches!(arena.data(dyn_ty), TypeData::Dyn(_)));
    assert!(matches!(arena.data(fn_ty), TypeData::Function(_)));
    assert_eq!(arena.len(), 9);
}

#[test]
fn type_name_renderers_are_stable() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let string_ty = builder.string_ty();
    let list_ty = builder.alloc_type(TypeData::List(int_ty));
    let map_ty = builder.alloc_type(TypeData::Map {
        key: string_ty,
        value: list_ty,
        order: MapOrder::Insertion,
    });
    let module = test_module(&mut builder);
    let aggregate = builder.alloc_aggregate(AggregateDecl {
        name: Ident::new("Point"),
        module,
        kind: AggregateKind::Struct,
        type_args: vec![],
        const_args: vec![],
        fields: vec![],
        cycle_capable: false,
        stringify_override: None,
    });
    let point_ty = builder.alloc_type(TypeData::Aggregate(aggregate));

    let program = builder.finish();

    assert_eq!(program.type_display_name(map_ty), "[string: [int]]");
    assert_eq!(program.type_helper_key(map_ty), "map_6_string10_list_3_int");
    assert!(matches!(
        program.type_data(map_ty),
        TypeData::Map {
            order: MapOrder::Insertion,
            ..
        }
    ));
    assert_eq!(program.type_display_name(point_ty), "test::Point");
    assert_eq!(program.type_helper_key(point_ty), "74657374_506f696e74");
}

#[test]
fn nominal_type_renderers_include_concrete_args() {
    let mut builder = ProgramBuilder::default();
    let module = test_module(&mut builder);
    let int_ty = builder.int_ty();
    let string_ty = builder.string_ty();
    let box_int = builder.alloc_aggregate(AggregateDecl {
        name: Ident::new("Box"),
        module,
        kind: AggregateKind::Struct,
        type_args: vec![int_ty],
        const_args: vec![],
        fields: vec![],
        cycle_capable: false,
        stringify_override: None,
    });
    let box_string = builder.alloc_aggregate(AggregateDecl {
        name: Ident::new("Box"),
        module,
        kind: AggregateKind::Struct,
        type_args: vec![string_ty],
        const_args: vec![],
        fields: vec![],
        cycle_capable: false,
        stringify_override: None,
    });
    let box_int_ty = builder.alloc_type(TypeData::Aggregate(box_int));
    let box_string_ty = builder.alloc_type(TypeData::Aggregate(box_string));
    let program = builder.finish();

    assert_eq!(program.type_display_name(box_int_ty), "test::Box<int>");
    assert_eq!(
        program.type_display_name(box_string_ty),
        "test::Box<string>"
    );
    assert_ne!(
        program.type_helper_key(box_int_ty),
        program.type_helper_key(box_string_ty)
    );
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
fn function_stable_ids() {
    let mut builder = ProgramBuilder::default();
    let void_ty = builder.void_ty();

    let module = test_module(&mut builder);

    let mut fb = FunctionBuilder::new("test_fn", module, FunctionKind::Normal, void_ty);
    let param_a = fb.push_param("a", TypeId::from_index(1), ParamRole::Normal);
    let local_b = fb.push_local(
        Some("b"),
        TypeId::from_index(2),
        Mutability::Mutable,
        LocalKind::User,
    );
    fb.push_block(term_return_void());
    let func_id = builder.alloc_function(fb.finish());

    let program = builder.finish();
    let func = program.function(func_id);
    assert_eq!(func.locals.len(), 2);
    assert_eq!(func.body.block.stmts.len(), 0);
    assert_eq!(param_a.index(), 0);
    assert_eq!(local_b.index(), 1);
}

#[test]
fn aggregate_stringify_override_metadata() {
    let mut builder = ProgramBuilder::default();
    let module = test_module(&mut builder);
    let string_ty = builder.string_ty();
    let aggregate = builder.alloc_aggregate(AggregateDecl {
        name: Ident::new("S"),
        module,
        kind: AggregateKind::Struct,
        type_args: vec![],
        const_args: vec![],
        fields: vec![],
        cycle_capable: false,
        stringify_override: None,
    });
    let aggregate_ty = builder.alloc_type(TypeData::Aggregate(aggregate));
    let mut fb = FunctionBuilder::new("to_string", module, FunctionKind::Method, string_ty);
    fb.push_param("self", aggregate_ty, ParamRole::Receiver);
    let string_const = builder.alloc_const(ConstData {
        ty: string_ty,
        value: ConstValue::String("S".into()),
    });
    fb.push_block(term_return(op_const(string_const)));
    let override_id = builder.alloc_function(fb.finish());
    let mut program = builder.finish();
    program.aggregate_mut(aggregate).stringify_override = Some(override_id);

    assert_eq!(
        program.aggregate(aggregate).stringify_override,
        Some(override_id)
    );
    verify(&program).expect("override metadata should verify");
}

#[test]
fn module_refs_stable() {
    let mut builder = ProgramBuilder::default();

    let m0 = builder.alloc_module(empty_module("a"));
    let m1 = builder.alloc_module(empty_module("b"));

    assert_eq!(m0.index(), 0);
    assert_eq!(m1.index(), 1);

    let program = builder.finish();
    assert_eq!(program.module(m0).path[0].as_str(), "a");
    assert_eq!(program.module(m1).path[0].as_str(), "b");
}

#[test]
fn program_accessors() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let void_ty = builder.void_ty();

    let module = test_module(&mut builder);

    let agg_id = builder.alloc_aggregate(AggregateDecl {
        name: Ident::new("MyStruct"),
        module,
        kind: AggregateKind::Struct,
        type_args: vec![],
        const_args: vec![],
        fields: vec![FieldDecl {
            name: Ident::new("f"),
            ty: int_ty,
        }],
        cycle_capable: false,
        stringify_override: None,
    });

    let enum_id = builder.alloc_enum(EnumDecl {
        name: Ident::new("MyEnum"),
        module,
        core: None,
        repr: crate::air::EnumRepr::Adt,
        raw_type: None,
        type_args: vec![],
        const_args: vec![],
        variants: vec![VariantDecl {
            name: Ident::new("V"),
            shape: VariantShape::Unit,
            raw_value: None,
        }],
    });

    let ext_ty_id = builder.alloc_extern_type(ExternTypeDecl {
        name: Ident::new("Ext"),
        module,
        binding: None,
        type_args: vec![],
        const_args: vec![],
        rep: ExternRep::Shared,
        has_init: false,
        init_fields: vec![],
        fields: vec![],
        methods: vec![],
        statics: vec![],
        operators: vec![],
    });

    let ext_id = builder.alloc_extern(ExternDecl {
        name: Ident::new("ext_fn"),
        module,
        member: ExternMember::FreeFunction,
        params: vec![ExternParamDecl {
            ty: int_ty,
            mode: ParamMode::Value,
            escape: ParamEscape::NonEscaping,
        }],
        return_type: int_ty,
        binding: None,
        effects: anvyx_externs::ExternEffects::default(),
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
    let mut builder = ProgramBuilder::default();
    let void_ty = builder.void_ty();

    let module = test_module(&mut builder);

    let mut fb = FunctionBuilder::new("f", module, FunctionKind::Normal, void_ty);
    let bid = fb.push_block(term_return_void());
    let func_id = builder.alloc_function(fb.finish());
    for i in 0..10 {
        assert_eq!(TypeId::from_index(i).index(), i);
        assert_eq!(ConstId::from_index(i).index(), i);
        assert_eq!(LocalId::from_index(i).index(), i);
    }

    assert_eq!(BlockId::from_index(0), bid);
    assert_eq!(FunctionId::from_index(0), func_id);
    assert_eq!(ModuleId::from_index(0), module);
}
