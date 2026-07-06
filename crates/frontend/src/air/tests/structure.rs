use super::*;
use crate::ast::Ident;
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
fn collection_loan_for_each_rvalue_visits_nested_body() {
    let body = AirBody {
        block: AirBlock {
            stmts: vec![AirStmt::CollectionLoan(AirCollectionLoan {
                root: place(LocalId::from_index(0), TypeId::from_index(0)),
                root_kind: AirCollectionRootKind::List,
                mode: AirCollectionLoanMode::ReadonlySequence,
                body: AirBlock {
                    stmts: vec![stmt_eval(RValue::Use(op_const(ConstId::from_index(0))))],
                    tail: AirTail::None,
                },
            })],
            tail: AirTail::None,
        },
    };

    let mut count = 0;
    body.for_each_rvalue(&mut |_| count += 1);
    assert_eq!(count, 1);
}
