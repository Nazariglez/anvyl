use air::AirStmt as Statement;
use anvyx_frontend::{
    air::{
        self, AggregateCtor, AirBody, AirOptionalMatch, BindingId, CallArg, Callee,
        CaptureCellDecl, CaptureLocalSource, ConstData, ConstValue, EnumDecl, ExternBindingDecl,
        ExternDecl, ExternMember, ExternParamDecl, ExternRep, ExternTypeDecl, FieldDecl, Function,
        FunctionId, FunctionKind, FunctionSpecialization, GlobalDecl, LambdaDecl, LambdaEscape,
        Local, LocalKind, Mutability, Operand, Param, ParamEscape, ParamMode, ParamRole, Place,
        PlaceRoot, Program, Projection, RValue, RawEnumValue, Signature, TypeData, VariantDecl,
        VariantId, VariantShape,
    },
    ast::{BinaryOp, ExprId, FormatAlign, FormatKind, FormatSign, FormatSpec, Ident},
};

use super::{
    RustPlanConfig, RustPlanError, RustTargetGapKind, cargo_job,
    dataref_mut_place::{DataRefMutPlaceSupport, classify as classify_dataref_mut_place},
    dataref_place::DataRefPlaceDescriptors,
    emit, plan,
    profile::{ProfileErrorKind, ProfileSite, RustBackendProfile, RustBackendProfileError},
    rep_policy::{RustRepPolicy, RustTracePlan},
    rir::{
        self, RirCallArg, RirCallTarget, RirCellDecl, RirCellId, RirCellRef, RirCellStorage,
        RirConst, RirConstId, RirConstValue, RirCoreEnumKind, RirDataRef, RirDataRefId, RirEnum,
        RirEnumId, RirEnumMatch, RirEnumMatchArm, RirExtern, RirExternId, RirExternKind,
        RirExternParam, RirField, RirFieldId, RirFormatKind, RirFormatSpec, RirFunction,
        RirFunctionId, RirIf, RirLambda, RirLambdaCapture, RirLambdaCaptureArg,
        RirLambdaCaptureKind, RirLambdaEnvField, RirLambdaEnvFieldKind, RirLambdaEnvId,
        RirLambdaEnvLayout, RirLambdaEscape, RirLambdaId, RirLambdaParam, RirLambdaSig,
        RirLambdaSigId, RirLambdaSource, RirLambdaStorage, RirLocal, RirLocalId, RirLoop,
        RirLoopId, RirMutPlaceArg, RirOperand, RirOptionMatch, RirParam, RirParamAbi,
        RirParamEscape, RirParamSemantic, RirPlace, RirProgram, RirProjection, RirRValue,
        RirReturn, RirScopedPlaceCellDecl, RirScopedPlaceCellId, RirScopedPlaceCellRef, RirStmt,
        RirStringifyHelper, RirStringifyHelperId, RirStringifyReq, RirStringifyReqId,
        RirStringifyReqKind, RirStruct, RirStructId, RirStructuredBlock, RirSymbol, RirTerm,
        RirTuple, RirTupleId, RirType, RirTypeId, RirVariant, RirVariantId, RirVariantKind,
        RirVerifyErrorKind, RirVerifySite,
    },
    source_job::{self, SourceJobStatus},
};
use crate::test_support::{
    immutable_local as local, mutable_local as mut_local, param, place, root_module,
    structured_body,
};

#[test]
fn profile_accepts_empty_air() {
    check(Program::default());
}

#[test]
fn profile_accepts_scalar_arithmetic_shape() {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let module = program.alloc_module(root_module());
    let one = program.const_arena.alloc(ConstData {
        ty: int,
        value: ConstValue::Int(1),
    });
    let two = program.const_arena.alloc(ConstData {
        ty: int,
        value: ConstValue::Int(2),
    });
    let tmp = air::LocalId::from_index(0);
    let func = program.alloc_function(Function {
        name: Ident::new("main"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], int),
        locals: vec![local(int, LocalKind::Temp)],
        body: structured_body(
            vec![Statement::Init {
                local: tmp,
                value: RValue::Binary {
                    op: BinaryOp::Add,
                    lhs: Operand::Const(one),
                    rhs: Operand::Const(two),
                    ty: int,
                },
            }],
            air::AirTail::Return(Some(Operand::Place(place(tmp, int)))),
        ),
    });
    program.module_mut(module).functions.push(func);
    program.set_entry(func);

    check(program);
}

#[test]
fn profile_accepts_numeric_cast_shape() {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let float = program.alloc_type(TypeData::Float);
    let module = program.alloc_module(root_module());
    let one = program.const_arena.alloc(ConstData {
        ty: int,
        value: ConstValue::Int(1),
    });
    let tmp = air::LocalId::from_index(0);
    let func = program.alloc_function(Function {
        name: Ident::new("main"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], float),
        locals: vec![local(float, LocalKind::Temp)],
        body: structured_body(
            vec![Statement::Init {
                local: tmp,
                value: RValue::Cast {
                    value: Operand::Const(one),
                    target: float,
                },
            }],
            air::AirTail::Return(Some(Operand::Place(place(tmp, float)))),
        ),
    });
    program.module_mut(module).functions.push(func);

    check(program);
}

#[test]
fn profile_accepts_direct_function_call_shape() {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let module = program.alloc_module(root_module());
    let one = program.const_arena.alloc(ConstData {
        ty: int,
        value: ConstValue::Int(1),
    });
    let helper = program.alloc_function(Function {
        name: Ident::new("helper"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], int),
        locals: vec![],
        body: structured_body(vec![], air::AirTail::Return(Some(Operand::Const(one)))),
    });
    let tmp = air::LocalId::from_index(0);
    let main = program.alloc_function(Function {
        name: Ident::new("main"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], int),
        locals: vec![local(int, LocalKind::Temp)],
        body: structured_body(
            vec![Statement::Init {
                local: tmp,
                value: RValue::Call {
                    callee: Callee::Function(helper),
                    args: vec![],
                },
            }],
            air::AirTail::Return(Some(Operand::Place(place(tmp, int)))),
        ),
    });
    program.module_mut(module).functions.extend([helper, main]);

    check(program);
}

#[test]
fn profile_accepts_core_println_extern_call_shape() {
    let mut program = Program::default();
    let string = program.alloc_type(TypeData::String);
    let void = program.alloc_type(TypeData::Void);
    let extern_id = runtime_extern(
        &mut program,
        "_println",
        vec![(string, ParamMode::SharedBorrow)],
        void,
    );
    let message = program.const_arena.alloc(ConstData {
        ty: string,
        value: ConstValue::String("ok".into()),
    });
    let module = program.alloc_module(root_module());
    let message_local = air::LocalId::from_index(0);
    let main = program.alloc_function(Function {
        name: Ident::new("main"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], void),
        locals: vec![local(string, LocalKind::Temp)],
        body: structured_body(
            vec![
                Statement::Init {
                    local: message_local,
                    value: RValue::Use(Operand::Const(message)),
                },
                Statement::Eval(RValue::Call {
                    callee: Callee::Extern(extern_id),
                    args: vec![CallArg::SharedBorrow(place(message_local, string))],
                }),
            ],
            air::AirTail::Return(None),
        ),
    });
    program.module_mut(module).functions.push(main);

    check(program);
}

#[test]
fn profile_accepts_core_assert_extern_call_shape() {
    let mut program = Program::default();
    let bool_ty = program.alloc_type(TypeData::Bool);
    let string = program.alloc_type(TypeData::String);
    let void = program.alloc_type(TypeData::Void);
    let extern_id = runtime_extern(
        &mut program,
        "_assert",
        vec![
            (bool_ty, ParamMode::Value),
            (string, ParamMode::SharedBorrow),
        ],
        void,
    );
    let condition = program.const_arena.alloc(ConstData {
        ty: bool_ty,
        value: ConstValue::Bool(true),
    });
    let message = program.const_arena.alloc(ConstData {
        ty: string,
        value: ConstValue::String("ok".into()),
    });
    let module = program.alloc_module(root_module());
    let message_local = air::LocalId::from_index(0);
    let main = program.alloc_function(Function {
        name: Ident::new("main"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], void),
        locals: vec![local(string, LocalKind::Temp)],
        body: structured_body(
            vec![
                Statement::Init {
                    local: message_local,
                    value: RValue::Use(Operand::Const(message)),
                },
                Statement::Eval(RValue::Call {
                    callee: Callee::Extern(extern_id),
                    args: vec![
                        CallArg::Value(Operand::Const(condition)),
                        CallArg::SharedBorrow(place(message_local, string)),
                    ],
                }),
            ],
            air::AirTail::Return(None),
        ),
    });
    program.module_mut(module).functions.push(main);

    check(program);
}

#[test]
fn profile_accepts_string_concat_without_value_copying_string_places() {
    check(string_concat_program());
}

#[test]
fn profile_accepts_supported_format_rvalues() {
    check(format_program());
}

#[test]
fn profile_rejects_dataref_list_payload_as_explicit_target_gap() {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let list = program.alloc_type(TypeData::List(int));
    let module = program.alloc_module(root_module());
    let aggregate = program.alloc_aggregate(air::AggregateDecl {
        name: Ident::new("Node"),
        module,
        kind: air::AggregateKind::DataRef,
        type_args: vec![],
        const_args: vec![],
        fields: vec![FieldDecl {
            name: Ident::new("items"),
            ty: list,
        }],
        cycle_capable: true,
        stringify_override: None,
    });
    program.module_mut(module).aggregates.push(aggregate);

    expect_reject(program, ProfileErrorKind::UnsupportedModuleItem);
}

#[test]
fn profile_accepts_dataref_tuple_payload() {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let tuple = program.alloc_type(TypeData::Tuple(vec![int]));
    let module = program.alloc_module(root_module());
    let aggregate = program.alloc_aggregate(air::AggregateDecl {
        name: Ident::new("Node"),
        module,
        kind: air::AggregateKind::DataRef,
        type_args: vec![],
        const_args: vec![],
        fields: vec![FieldDecl {
            name: Ident::new("field"),
            ty: tuple,
        }],
        cycle_capable: true,
        stringify_override: None,
    });
    program.module_mut(module).aggregates.push(aggregate);

    check(program);
}

#[test]
fn plan_marks_tuple_with_dataref_field_noncopy() {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let module = program.alloc_module(root_module());
    let child = program.alloc_aggregate(air::AggregateDecl {
        name: Ident::new("Child"),
        module,
        kind: air::AggregateKind::DataRef,
        type_args: vec![],
        const_args: vec![],
        fields: vec![],
        cycle_capable: false,
        stringify_override: None,
    });
    let child_ty = program.alloc_type(TypeData::DataRef(child));
    let tuple_ty = program.alloc_type(TypeData::Tuple(vec![child_ty, int]));
    let payload = program.alloc_aggregate(air::AggregateDecl {
        name: Ident::new("Payload"),
        module,
        kind: air::AggregateKind::Struct,
        type_args: vec![],
        const_args: vec![],
        fields: vec![FieldDecl {
            name: Ident::new("pair"),
            ty: tuple_ty,
        }],
        cycle_capable: false,
        stringify_override: None,
    });
    let payload_ty = program.alloc_type(TypeData::Aggregate(payload));
    program
        .module_mut(module)
        .aggregates
        .extend([child, payload]);

    let verified = air::verify(&program).expect("AIR verify failed");
    let plan = plan(&verified, rust_plan_config()).expect("plan failed");
    let RirType::Tuple(tuple) = plan.program().types[tuple_ty.index()] else {
        panic!("tuple type not planned as tuple")
    };
    let RirType::Struct(payload) = plan.program().types[payload_ty.index()] else {
        panic!("payload type not planned as struct")
    };

    assert!(!plan.program().tuples[tuple.index()].copyable);
    assert!(!plan.program().structs[payload.index()].copyable);
}

#[test]
fn profile_rejects_dataref_tuple_payload_with_unsupported_element() {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let array = program.alloc_type(TypeData::Array { elem: int, len: 1 });
    let tuple = program.alloc_type(TypeData::Tuple(vec![array, int]));
    let module = program.alloc_module(root_module());
    let aggregate = program.alloc_aggregate(air::AggregateDecl {
        name: Ident::new("Node"),
        module,
        kind: air::AggregateKind::DataRef,
        type_args: vec![],
        const_args: vec![],
        fields: vec![FieldDecl {
            name: Ident::new("bad"),
            ty: tuple,
        }],
        cycle_capable: true,
        stringify_override: None,
    });
    program.module_mut(module).aggregates.push(aggregate);

    expect_reject(program, ProfileErrorKind::UnsupportedModuleItem);
}

#[test]
fn emit_derives_trace_for_struct_payload_containing_tuple() {
    let program = tracked_struct_tuple_payload_program();
    check(program.clone());

    let source = plan_source(program).into_string();

    assert!(source.contains("#[derive(Clone, anvyx_runtime::Trace)]\n#[trace(crate = anvyx_runtime)]\nstruct anvT2_Payload"));
    assert!(source.contains("#[derive(Clone, anvyx_runtime::Trace)]\n#[trace(crate = anvyx_runtime)]\nstruct anvT1_Tuple"));
}

#[test]
fn profile_accepts_tuple_construction() {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let tuple = program.alloc_type(TypeData::Tuple(vec![int]));
    let one = program.const_arena.alloc(ConstData {
        ty: int,
        value: ConstValue::Int(1),
    });
    let module = program.alloc_module(root_module());
    let out = air::LocalId::from_index(0);
    let func = program.alloc_function(Function {
        name: Ident::new("main"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], tuple),
        locals: vec![local(tuple, LocalKind::Temp)],
        body: structured_body(
            vec![Statement::Init {
                local: out,
                value: RValue::Aggregate {
                    kind: AggregateCtor::Tuple,
                    fields: vec![Operand::Const(one)],
                    ty: tuple,
                },
            }],
            air::AirTail::Return(Some(Operand::Place(place(out, tuple)))),
        ),
    });
    program.module_mut(module).functions.push(func);

    check(program);
}

#[test]
fn plan_preserves_tuple_construction_and_projection() {
    let program = tuple_projection_program();
    let verified = air::verify(&program).expect("AIR verify failed");
    let plan = plan(&verified, rust_plan_config()).expect("plan failed");
    let function = &plan.program().functions[0];

    assert!(matches!(
        function.body.stmts[0],
        RirStmt::Init {
            value: RirRValue::Tuple { .. },
            ..
        }
    ));
    assert!(matches!(
        &function.body.term,
        RirTerm::Return(Some(RirOperand::Place(RirPlace { projections, .. })))
            if projections == &[RirProjection::TupleField(RirFieldId::from_index(0))]
    ));
}

#[test]
fn emit_renders_tuple_declaration_construction_and_projection() {
    let source = plan_source(tuple_projection_program()).into_string();

    assert!(source.contains("struct anvT1_Tuple"));
    assert!(source.contains("_0: i64"));
    assert!(source.contains("anvT1_Tuple { _0: 1 }"));
    assert!(source.contains("return v0._0;"));
}

#[test]
fn profile_rejects_unsupported_format_source() {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let tuple = program.alloc_type(TypeData::Tuple(vec![int]));
    let string = program.alloc_type(TypeData::String);
    let module = program.alloc_module(root_module());
    let arg = air::LocalId::from_index(0);
    let text = air::LocalId::from_index(1);
    let func = program.alloc_function(Function {
        name: Ident::new("main"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![param("x", tuple, ParamMode::Value, arg)], string),
        locals: vec![local(tuple, LocalKind::Arg), local(string, LocalKind::Temp)],
        body: structured_body(
            vec![Statement::Init {
                local: text,
                value: RValue::Format {
                    value: Operand::Place(place(arg, tuple)),
                    spec: FormatSpec::default(),
                },
            }],
            air::AirTail::Return(Some(Operand::Place(place(text, string)))),
        ),
    });
    program.module_mut(module).functions.push(func);

    let errors = profile_errors(program);
    assert!(has_error(&errors, ProfileErrorKind::UnsupportedRValue));
}

#[test]
fn profile_rejects_invalid_format_spec_for_source_type() {
    let mut program = Program::default();
    let bool_ty = program.alloc_type(TypeData::Bool);
    let string = program.alloc_type(TypeData::String);
    let module = program.alloc_module(root_module());
    let flag = program.const_arena.alloc(ConstData {
        ty: bool_ty,
        value: ConstValue::Bool(true),
    });
    let text = air::LocalId::from_index(0);
    let func = program.alloc_function(Function {
        name: Ident::new("main"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], string),
        locals: vec![local(string, LocalKind::Temp)],
        body: structured_body(
            vec![Statement::Init {
                local: text,
                value: RValue::Format {
                    value: Operand::Const(flag),
                    spec: FormatSpec {
                        sign: FormatSign::Always,
                        ..FormatSpec::default()
                    },
                },
            }],
            air::AirTail::Return(Some(Operand::Place(place(text, string)))),
        ),
    });
    program.module_mut(module).functions.push(func);

    expect_reject(program, ProfileErrorKind::UnsupportedRValue);
}

#[test]
fn profile_accepts_unreachable_terminator() {
    let mut program = Program::default();
    let void = program.alloc_type(TypeData::Void);
    let module = program.alloc_module(root_module());
    let func = program.alloc_function(Function {
        name: Ident::new("main"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], void),
        locals: vec![],
        body: structured_body(vec![], air::AirTail::Unreachable),
    });
    program.module_mut(module).functions.push(func);

    check(program);
}

#[test]
fn profile_accepts_plain_struct_declarations() {
    check(struct_decl_program(false));
}

#[test]
fn profile_accepts_trace_safe_dataref_declarations() {
    check(struct_decl_program(true));
}

#[test]
fn profile_rejects_unsupported_dataref_payloads() {
    expect_reject(
        unsupported_dataref_payload_program(),
        ProfileErrorKind::UnsupportedModuleItem,
    );
}

#[test]
fn profile_accepts_dataref_field_projections() {
    check(dataref_field_projection_program());
}

#[test]
fn profile_rejects_non_local_place_roots() {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let module = program.alloc_module(root_module());
    let global = program.alloc_global(GlobalDecl {
        name: Ident::new("g"),
        module,
        ty: int,
        mutability: Mutability::Mutable,
    });
    let function = Function {
        name: Ident::new("f"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], int),
        locals: vec![],
        body: structured_body(
            vec![],
            air::AirTail::Return(Some(Operand::Place(Place {
                root: PlaceRoot::Global(global),
                projection: vec![],
                ty: int,
            }))),
        ),
    };
    let id = program.alloc_function(function);
    program.module_mut(module).functions.push(id);
    program.set_entry(id);

    expect_reject(program, ProfileErrorKind::UnsupportedPlaceRoot);
}

#[test]
fn profile_accepts_whole_copyable_capture_cell_roots() {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let module = program.alloc_module(root_module());
    let cell = program.alloc_capture_cell(CaptureCellDecl {
        binding: BindingId::from_index(0),
        owner: FunctionId::from_index(0),
        source_local: air::LocalId::from_index(0),
        ty: int,
    });
    let init = program.alloc_const(ConstData {
        ty: int,
        value: ConstValue::Int(0),
    });
    let mut source = mut_local(int, LocalKind::User);
    source.binding = Some(BindingId::from_index(0));
    let function = Function {
        name: Ident::new("f"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], int),
        locals: vec![source],
        body: structured_body(
            vec![Statement::Assign {
                dst: Place {
                    root: PlaceRoot::CaptureCell(cell),
                    projection: vec![],
                    ty: int,
                },
                value: RValue::Use(Operand::Const(init)),
            }],
            air::AirTail::Return(Some(Operand::Place(Place {
                root: PlaceRoot::CaptureCell(cell),
                projection: vec![],
                ty: int,
            }))),
        ),
    };
    let id = program.alloc_function(function);
    program.module_mut(module).functions.push(id);
    program.set_entry(id);

    check(program);
}

#[test]
fn source_job_compiles_hand_built_air_capture_cell_lambdas() {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let void = program.alloc_type(TypeData::Void);
    let writer_sig = air::SignatureType::new(vec![], air::ReturnMode::Value(void));
    let reader_sig = air::SignatureType::new(vec![], air::ReturnMode::Value(int));
    let writer_ty = program.alloc_type(TypeData::Function(writer_sig.clone()));
    let reader_ty = program.alloc_type(TypeData::Function(reader_sig.clone()));
    let module = program.alloc_module(root_module());
    let binding = BindingId::from_index(0);
    let owner = FunctionId::from_index(2);
    let source_local = air::LocalId::from_index(0);
    let writer_lambda = air::LambdaId::from_index(0);
    let reader_lambda = air::LambdaId::from_index(1);
    let writer_body = FunctionId::from_index(0);
    let reader_body = FunctionId::from_index(1);
    let cell = program.alloc_capture_cell(CaptureCellDecl {
        binding,
        owner,
        source_local,
        ty: int,
    });
    assert_eq!(
        program.alloc_lambda(LambdaDecl {
            source: ExprId(0),
            module,
            owner,
            body: writer_body,
            signature: writer_sig,
            escape: LambdaEscape::NonEscaping,
            captures: vec![air::LambdaCaptureDecl::CaptureCell {
                binding,
                cell,
                ty: int,
            }],
        }),
        writer_lambda
    );
    assert_eq!(
        program.alloc_lambda(LambdaDecl {
            source: ExprId(1),
            module,
            owner,
            body: reader_body,
            signature: reader_sig,
            escape: LambdaEscape::NonEscaping,
            captures: vec![air::LambdaCaptureDecl::CaptureCell {
                binding,
                cell,
                ty: int,
            }],
        }),
        reader_lambda
    );
    let zero = program.alloc_const(ConstData {
        ty: int,
        value: ConstValue::Int(0),
    });
    let one = program.alloc_const(ConstData {
        ty: int,
        value: ConstValue::Int(1),
    });
    let mut writer = Function {
        name: Ident::new("writer"),
        module,
        kind: FunctionKind::Lambda(writer_lambda),
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], void),
        locals: vec![],
        body: structured_body(
            vec![Statement::Assign {
                dst: Place {
                    root: PlaceRoot::LambdaCapture(air::LambdaCaptureSlotId::from_index(0)),
                    projection: vec![],
                    ty: int,
                },
                value: RValue::Use(Operand::Const(one)),
            }],
            air::AirTail::Return(None),
        ),
    };
    writer.signature.return_mode = air::ReturnMode::Value(void);
    let reader = Function {
        name: Ident::new("reader"),
        module,
        kind: FunctionKind::Lambda(reader_lambda),
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], int),
        locals: vec![],
        body: structured_body(
            vec![],
            air::AirTail::Return(Some(Operand::Place(Place {
                root: PlaceRoot::LambdaCapture(air::LambdaCaptureSlotId::from_index(0)),
                projection: vec![],
                ty: int,
            }))),
        ),
    };
    let mut source = mut_local(int, LocalKind::User);
    source.binding = Some(binding);
    let writer_local = local(writer_ty, LocalKind::User);
    let reader_local = local(reader_ty, LocalKind::User);
    let result_local = local(int, LocalKind::Temp);
    let owner_fn = Function {
        name: Ident::new("main"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], int),
        locals: vec![source, writer_local, reader_local, result_local],
        body: structured_body(
            vec![
                Statement::Assign {
                    dst: Place {
                        root: PlaceRoot::CaptureCell(cell),
                        projection: vec![],
                        ty: int,
                    },
                    value: RValue::Use(Operand::Const(zero)),
                },
                Statement::Init {
                    local: air::LocalId::from_index(1),
                    value: RValue::MakeLambda {
                        lambda: writer_lambda,
                        captures: vec![air::LambdaCaptureArg::CaptureCell { cell }],
                        ty: writer_ty,
                    },
                },
                Statement::Init {
                    local: air::LocalId::from_index(2),
                    value: RValue::MakeLambda {
                        lambda: reader_lambda,
                        captures: vec![air::LambdaCaptureArg::CaptureCell { cell }],
                        ty: reader_ty,
                    },
                },
                Statement::Eval(RValue::Call {
                    callee: Callee::Lambda(Operand::Place(Place {
                        root: PlaceRoot::Local(air::LocalId::from_index(1)),
                        projection: vec![],
                        ty: writer_ty,
                    })),
                    args: vec![],
                }),
                Statement::Init {
                    local: air::LocalId::from_index(3),
                    value: RValue::Call {
                        callee: Callee::Lambda(Operand::Place(Place {
                            root: PlaceRoot::Local(air::LocalId::from_index(2)),
                            projection: vec![],
                            ty: reader_ty,
                        })),
                        args: vec![],
                    },
                },
            ],
            air::AirTail::Return(Some(Operand::Place(Place {
                root: PlaceRoot::Local(air::LocalId::from_index(3)),
                projection: vec![],
                ty: int,
            }))),
        ),
    };
    assert_eq!(program.alloc_function(writer), writer_body);
    assert_eq!(program.alloc_function(reader), reader_body);
    assert_eq!(program.alloc_function(owner_fn), owner);
    program
        .module_mut(module)
        .functions
        .extend([writer_body, reader_body, owner]);
    program.set_entry(owner);

    let source = plan_source(program);
    let text = source.as_str();
    assert!(text.contains("StackLambdaCell<i64>"));
    assert!(text.contains("#[derive(Clone, Copy)]\nenum LambdaSig0<'env>"));
    assert!(text.contains("#[derive(Clone, Copy)]\nenum LambdaSig1<'env>"));
    assert!(text.contains("c0: &'env anvyx_runtime::StackLambdaCell<i64>"));
    assert!(text.contains("fn call<'cx, 'rt>(&self, ctx: &mut AnvCtx<'cx, 'rt>)"));
    assert!(text.contains(".set(1)?;"));
    assert!(text.contains(".get_copy()?"));
    assert!(text.contains("c0: &__cell0"));
    assert!(!text.contains("&'env mut"));
    assert!(!text.contains("&mut v0"));
    assert!(!text.contains("fn call<'cx, 'rt>(&mut self"));
    assert!(!text.contains("LambdaEnv"));
    assert!(!text.contains("anvyx_runtime::LambdaCell"));
    assert!(!text.contains("Box<dyn"));
    assert!(!text.contains("Rc<"));
    assert!(!text.contains("RefCell"));
    assert!(!text.contains("erased::Value"));

    let output = run_source(source);
    assert_eq!(output.status, SourceJobStatus::Success, "{}", output.stderr);
}

#[test]
fn profile_rejects_capture_cell_in_unsupported_rvalue() {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let list = program.alloc_type(TypeData::List(int));
    let optional_int = program.alloc_type(TypeData::Optional(int));
    let void = program.alloc_type(TypeData::Void);
    let module = program.alloc_module(root_module());
    let cell = program.alloc_capture_cell(CaptureCellDecl {
        binding: BindingId::from_index(0),
        owner: FunctionId::from_index(0),
        source_local: air::LocalId::from_index(0),
        ty: list,
    });
    let mut source = mut_local(list, LocalKind::User);
    source.binding = Some(BindingId::from_index(0));
    let init = RValue::Aggregate {
        kind: AggregateCtor::List,
        fields: vec![],
        ty: list,
    };
    let function = Function {
        name: Ident::new("f"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], void),
        locals: vec![source],
        body: structured_body(
            vec![
                Statement::Assign {
                    dst: Place {
                        root: PlaceRoot::CaptureCell(cell),
                        projection: vec![],
                        ty: list,
                    },
                    value: init,
                },
                Statement::Eval(RValue::ListPop {
                    list: Place {
                        root: PlaceRoot::CaptureCell(cell),
                        projection: vec![],
                        ty: list,
                    },
                    ty: optional_int,
                }),
            ],
            air::AirTail::Return(None),
        ),
    };
    let id = program.alloc_function(function);
    program.module_mut(module).functions.push(id);
    program.set_entry(id);

    let errors = profile_errors(program);
    assert!(errors.iter().any(|error| {
        error.kind == ProfileErrorKind::UnsupportedRValue
            && error.site == ProfileSite::Statement(id, 1)
    }));
}

#[test]
fn profile_rejects_payload_ref_through_dataref_projection() {
    expect_reject(
        dataref_optional_payload_ref_program(),
        ProfileErrorKind::UnsupportedPlaceProjection,
    );
}

#[test]
fn profile_rejects_optional_payload_copy_from_void_ref() {
    expect_reject(
        optional_void_payload_copy_program(),
        ProfileErrorKind::NonCopyValueRequired,
    );
}

#[test]
fn plan_lowers_dataref_field_places_to_heap_ops() {
    let program = dataref_field_projection_program();
    let verified = air::verify(&program).expect("AIR verify failed");
    let plan = plan(&verified, rust_plan_config()).expect("plan failed");
    let stmts = &plan.program().functions[0].body.stmts;

    assert!(matches!(stmts[0], RirStmt::DataRefSet { .. }));
    assert!(matches!(
        stmts[1],
        RirStmt::Init {
            value: RirRValue::DataRefGet { .. },
            ..
        }
    ));
    assert!(matches!(stmts[2], RirStmt::Init { .. }));
}

#[test]
fn plan_lowers_nested_dataref_field_read_to_separate_heap_ops() {
    let program = nested_dataref_read_program();
    let verified = air::verify(&program).expect("AIR verify failed");
    let plan = plan(&verified, rust_plan_config()).expect("plan failed");
    let stmts = &plan.program().functions[0].body.stmts;

    assert_eq!(
        stmts
            .iter()
            .filter(|stmt| matches!(
                stmt,
                RirStmt::Init {
                    value: RirRValue::DataRefGet { .. },
                    ..
                }
            ))
            .count(),
        2
    );
}

#[test]
fn plan_lowers_dataref_field_len_and_shared_borrow_call_args() {
    let program = dataref_string_field_consumers_program();
    let verified = air::verify(&program).expect("AIR verify failed");
    let plan = plan(&verified, rust_plan_config()).expect("plan failed");
    let stmts = &plan.program().functions[1].body.stmts;

    assert!(matches!(
        stmts[0],
        RirStmt::Init {
            value: RirRValue::DataRefGet { .. },
            ..
        }
    ));
    assert!(matches!(
        stmts[1],
        RirStmt::Init {
            value: RirRValue::Len { .. },
            ..
        }
    ));
    assert!(matches!(
        stmts[2],
        RirStmt::Init {
            value: RirRValue::DataRefGet { .. },
            ..
        }
    ));
    assert!(matches!(stmts[3], RirStmt::Eval(RirRValue::Call { .. })));
}

#[test]
fn emit_dataref_get_uses_short_heap_borrow() {
    let source = plan_source(nested_dataref_read_program()).into_string();

    assert!(source.contains("ctx.heap().with(&v0, |storage| storage.child.clone())"));
    assert!(source.contains("ctx.heap().with(&v2, |storage| storage.value)"));
    assert!(!source.contains("|storage| anv_f"));
}

#[test]
fn emit_dataref_set_uses_short_mut_heap_borrow() {
    let source = plan_source(dataref_field_projection_program()).into_string();

    assert!(source.contains("ctx.heap().with_mut(&v0, |storage| { storage.value = 1; })"));
    assert!(!source.contains("with_mut(&v0, |storage| { anv"));
}

#[test]
fn plan_lowers_projected_source_mut_call_arg_to_dataref_place() {
    let source = plan_source(projected_mut_call_arg_program()).into_string();

    assert!(source.contains("let __anv_dataref_place_object_0 = ctx.heap().erase(&v0).map_err(anvyx_runtime::heap_access_error)?;"));
    assert!(source.contains(
        "anvyx_runtime::MutPlace::dataref(__anv_dataref_place_object_0, &__anv_dataref_place_ops_0)"
    ));
    assert!(source.contains("try_with_erased(object, self.heap_type"));
    assert!(!source.contains("ctx.heap().with_mut(&v0, |storage| { storage.value ="));
}

#[test]
fn plan_lowers_multiple_projected_source_mut_call_args_without_copyback() {
    let source = plan_source(multi_projected_mut_call_arg_program()).into_string();

    assert_eq!(
        source.matches("anvyx_runtime::MutPlace::dataref").count(),
        2
    );
    assert!(source.contains("__anv_dataref_place_object_0"));
    assert!(source.contains("__anv_dataref_place_object_1"));
    assert!(!source.contains("ctx.heap().with_mut(&v0, |storage| { storage.value ="));
    assert!(!source.contains("ctx.heap().with_mut(&v1, |storage| { storage.value ="));
}

#[test]
fn emit_dataref_mut_borrow_root_rebinds_handle() {
    let source = plan_source(dataref_root_rebind_program()).into_string();

    assert!(source.contains(
        "mut v0: anvyx_runtime::MutPlace<'_, 'cx, anvT2_Node<'cx>>, v1: anvT2_Node<'cx>"
    ));
    assert!(source.contains("v0.set(ctx.runtime(), v1.clone())?;"));
}

#[test]
fn source_job_compiles_struct_stringify_helper() {
    let mut program = struct_field_read_program();
    let point = program.functions[0].locals[0].ty;
    if let air::AirStmt::Init { value, .. } = &mut program.functions[0].body.block.stmts[1] {
        *value = RValue::Stringify {
            value: Operand::Place(place(air::LocalId::from_index(0), point)),
            source_ty: point,
        };
    }
    let source = plan_source(program);

    assert!(source.as_str().contains("use std::fmt::Write;"));
    assert!(source.as_str().contains("fn anvstringify_t3_point"));
    assert!(
        source
            .as_str()
            .contains("let _ = write!(out, \"{}\", value.x);")
    );
    assert!(!source.as_str().contains("format!(\"{}\", value.x)"));
    assert!(!source.as_str().contains("{:?}"));
    assert!(!source.as_str().contains(".clone()"));
    assert!(!source.as_str().contains(".to_owned()"));

    let output = run_source(source);

    assert_eq!(output.status, SourceJobStatus::Success);
    assert_eq!(output.stdout, "Point(x: 7)\n");
}

#[test]
fn stringify_override_value_receiver_uses_copy_reconstruction() {
    let mut program = struct_field_read_program();
    let module = air::ModuleId::from_index(0);
    let point = program.functions[0].locals[0].ty;
    let string = program.functions[0].locals[1].ty;
    let aggregate = match program.type_arena.data(point) {
        TypeData::Aggregate(aggregate) => *aggregate,
        _ => panic!("point type missing"),
    };
    let recv = air::LocalId::from_index(0);
    let ok = program.const_arena.alloc(ConstData {
        ty: string,
        value: ConstValue::String("ok".into()),
    });
    let method = program.alloc_function(Function {
        name: Ident::new("to_string"),
        module,
        kind: FunctionKind::Method,
        owner: None,
        specialization: None,
        signature: Signature::new(
            vec![Param {
                name: Some(Ident::new("self")),
                ty: point,
                mode: ParamMode::Value,
                escape: ParamEscape::NonEscaping,
                role: ParamRole::Receiver,
                local_id: recv,
            }],
            string,
        ),
        locals: vec![local(point, LocalKind::Arg)],
        body: structured_body(vec![], air::AirTail::Return(Some(Operand::Const(ok)))),
    });
    program.aggregate_mut(aggregate).stringify_override = Some(method);
    program.module_mut(module).functions.insert(0, method);
    if let air::AirStmt::Init { value, .. } = &mut program.functions[0].body.block.stmts[1] {
        *value = RValue::Stringify {
            value: Operand::Place(place(air::LocalId::from_index(0), point)),
            source_ty: point,
        };
    }

    let source = plan_source(program);
    let text = source.as_str();

    assert!(text.contains("anv_f1_Point_to_string(ctx, anvT3_Point { x: v0.x })"));
    assert!(!text.contains(".clone()"));

    let output = run_source(source);

    assert_eq!(output.status, SourceJobStatus::Success);
    assert_eq!(output.stdout, "ok\n");
}

#[test]
fn stringify_override_propagates_fallible_receiver_function() {
    let mut program = struct_field_read_program();
    let module = air::ModuleId::from_index(0);
    let point = program.functions[0].locals[0].ty;
    let string = program.functions[0].locals[1].ty;
    let aggregate = match program.type_arena.data(point) {
        TypeData::Aggregate(aggregate) => *aggregate,
        _ => panic!("point type missing"),
    };
    let int = program.aggregate(aggregate).fields[0].ty;
    let fallible = fallible_extern(&mut program, int);
    let recv = air::LocalId::from_index(0);
    let tmp = air::LocalId::from_index(1);
    let arg = int_const(&mut program, int, 41);
    let ok = program.const_arena.alloc(ConstData {
        ty: string,
        value: ConstValue::String("ok".into()),
    });
    let method = program.alloc_function(Function {
        name: Ident::new("to_string"),
        module,
        kind: FunctionKind::Method,
        owner: None,
        specialization: None,
        signature: Signature::new(
            vec![Param {
                name: Some(Ident::new("self")),
                ty: point,
                mode: ParamMode::SharedBorrow,
                escape: ParamEscape::NonEscaping,
                role: ParamRole::Receiver,
                local_id: recv,
            }],
            string,
        ),
        locals: vec![local(point, LocalKind::Arg), local(int, LocalKind::Temp)],
        body: structured_body(
            vec![Statement::Init {
                local: tmp,
                value: RValue::Call {
                    callee: Callee::Extern(fallible),
                    args: vec![CallArg::Value(Operand::Const(arg))],
                },
            }],
            air::AirTail::Return(Some(Operand::Const(ok))),
        ),
    });
    program.aggregate_mut(aggregate).stringify_override = Some(method);
    program.module_mut(module).functions.insert(0, method);
    if let air::AirStmt::Init { value, .. } = &mut program.functions[0].body.block.stmts[1] {
        *value = RValue::Stringify {
            value: Operand::Place(place(air::LocalId::from_index(0), point)),
            source_ty: point,
        };
    }

    let source = plan_source(program);
    let text = source.as_str();

    assert!(
        text.contains("Point_to_string<'cx, 'rt>(ctx: &mut AnvCtx<'cx, 'rt>, v0: &anvT3_Point)")
    );
    assert!(text.contains("-> Result<anvyx_runtime::AnvString, anvyx_runtime::RuntimeError>"));
    assert!(text.contains("host::fallible(ctx.runtime(), 41)?;"));
    assert!(text.contains("Point_to_string(ctx, &v0)?"));
    assert!(text.contains("fn anv_f0_main<'cx, 'rt>(ctx: &mut AnvCtx<'cx, 'rt>)"));
    assert!(text.contains("-> Result<(), anvyx_runtime::RuntimeError>"));

    let output = run_source(with_fallible_host(source));

    assert_eq!(output.status, SourceJobStatus::Success);
    assert_eq!(output.stdout, "ok\n");
}

#[test]
fn stringify_override_noncopy_value_receiver_is_target_gap() {
    let mut program = Program::default();
    let void = program.alloc_type(TypeData::Void);
    let string = program.alloc_type(TypeData::String);
    let module = program.alloc_module(root_module());
    let aggregate = program.alloc_aggregate(air::AggregateDecl {
        name: Ident::new("Named"),
        module,
        kind: air::AggregateKind::Struct,
        type_args: vec![],
        const_args: vec![],
        fields: vec![FieldDecl {
            name: Ident::new("name"),
            ty: string,
        }],
        cycle_capable: false,
        stringify_override: None,
    });
    program.module_mut(module).aggregates.push(aggregate);
    let named = program.alloc_type(TypeData::Aggregate(aggregate));
    let recv = air::LocalId::from_index(0);
    let text_const = program.const_arena.alloc(ConstData {
        ty: string,
        value: ConstValue::String("x".into()),
    });
    let method = program.alloc_function(Function {
        name: Ident::new("to_string"),
        module,
        kind: FunctionKind::Method,
        owner: None,
        specialization: None,
        signature: Signature::new(
            vec![Param {
                name: Some(Ident::new("self")),
                ty: named,
                mode: ParamMode::Value,
                escape: ParamEscape::NonEscaping,
                role: ParamRole::Receiver,
                local_id: recv,
            }],
            string,
        ),
        locals: vec![local(named, LocalKind::Arg)],
        body: structured_body(
            vec![],
            air::AirTail::Return(Some(Operand::Const(text_const))),
        ),
    });
    program.aggregate_mut(aggregate).stringify_override = Some(method);
    program.module_mut(module).functions.push(method);
    let name_const = program.const_arena.alloc(ConstData {
        ty: string,
        value: ConstValue::String("n".into()),
    });
    let value = air::LocalId::from_index(0);
    let text = air::LocalId::from_index(1);
    let main = program.alloc_function(Function {
        name: Ident::new("main"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], void),
        locals: vec![
            local(named, LocalKind::Temp),
            local(string, LocalKind::Temp),
        ],
        body: structured_body(
            vec![
                Statement::Init {
                    local: value,
                    value: RValue::Aggregate {
                        kind: AggregateCtor::Struct(aggregate),
                        fields: vec![Operand::Const(name_const)],
                        ty: named,
                    },
                },
                Statement::Init {
                    local: text,
                    value: RValue::Stringify {
                        value: Operand::Place(place(value, named)),
                        source_ty: named,
                    },
                },
            ],
            air::AirTail::Return(None),
        ),
    });
    program.module_mut(module).functions.push(main);
    program.set_entry(main);

    assert_plan_gap(
        program,
        RustPlanConfig::default(),
        RustTargetGapKind::NonCopyValueRequired,
    );
}

#[test]
fn stringify_planning_dedupes_struct_requirements() {
    let mut program = struct_field_read_program();
    let point = program.functions[0].locals[0].ty;
    let text = air::LocalId::from_index(1);
    let string = program.functions[0].locals[1].ty;
    program.functions[0].locals[1].mutability = Mutability::Mutable;
    if let air::AirStmt::Init { value, .. } = &mut program.functions[0].body.block.stmts[1] {
        *value = RValue::Stringify {
            value: Operand::Place(place(air::LocalId::from_index(0), point)),
            source_ty: point,
        };
    }
    program.functions[0].body.block.stmts.insert(
        2,
        air::AirStmt::Assign {
            dst: place(text, string),
            value: RValue::Stringify {
                value: Operand::Place(place(air::LocalId::from_index(0), point)),
                source_ty: point,
            },
        },
    );

    let verified = air::verify(&program).expect("AIR verify failed");
    let plan = plan(&verified, rust_plan_config()).expect("plan failed");

    assert_eq!(plan.program().stringify_helpers.len(), 1);
}

#[test]
fn source_job_suffixes_generic_function_specializations() {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let float = program.alloc_type(TypeData::Float);
    let bool_ty = program.alloc_type(TypeData::Bool);
    let void = program.alloc_type(TypeData::Void);
    let module = program.alloc_module(root_module());
    for ty in [int, float, bool_ty] {
        let func = program.alloc_function(Function {
            name: Ident::new("println"),
            module,
            kind: FunctionKind::Normal,
            owner: None,
            specialization: Some(FunctionSpecialization {
                type_args: vec![ty],
                const_args: vec![],
            }),
            signature: Signature::new(vec![], void),
            locals: vec![],
            body: structured_body(vec![], air::AirTail::Return(None)),
        });
        program.module_mut(module).functions.push(func);
    }

    let source = plan_source(program).into_string();

    assert!(source.contains("fn anv_f0_println_int"));
    assert!(source.contains("fn anv_f1_println_float"));
    assert!(source.contains("fn anv_f2_println_bool"));
}

#[test]
fn source_job_suffixes_nested_generic_function_specialization() {
    let mut program = Program::default();
    let string = program.alloc_type(TypeData::String);
    let option_enum = program.alloc_enum(EnumDecl {
        name: Ident::new("Option"),
        module: air::ModuleId::from_index(0),
        type_args: vec![string],
        const_args: vec![],
        core: Some(air::CoreEnumKind::Option),
        repr: air::EnumRepr::Adt,
        raw_type: None,
        variants: vec![
            VariantDecl {
                name: Ident::new("None"),
                shape: VariantShape::Unit,
                raw_value: None,
            },
            VariantDecl {
                name: Ident::new("Some"),
                shape: VariantShape::Tuple(vec![string]),
                raw_value: None,
            },
        ],
    });
    let option = program.alloc_type(TypeData::Enum(option_enum));
    let list = program.alloc_type(TypeData::List(option));
    let void = program.alloc_type(TypeData::Void);
    let module = program.alloc_module(root_module());
    program.enum_decl_mut(option_enum).module = module;
    let func = program.alloc_function(Function {
        name: Ident::new("show"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: Some(FunctionSpecialization {
            type_args: vec![list],
            const_args: vec![],
        }),
        signature: Signature::new(vec![], void),
        locals: vec![],
        body: structured_body(vec![], air::AirTail::Return(None)),
    });
    program.module_mut(module).enums.push(option_enum);
    program.module_mut(module).functions.push(func);

    let source = plan_source(program).into_string();

    assert!(source.contains("fn anv_f0_show_list_option_string"));
}

#[test]
fn source_job_suffixes_negative_const_specialization() {
    let mut program = Program::default();
    let void = program.alloc_type(TypeData::Void);
    let module = program.alloc_module(root_module());
    let func = program.alloc_function(Function {
        name: Ident::new("show"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: Some(FunctionSpecialization {
            type_args: vec![],
            const_args: vec![ConstValue::Int(-1)],
        }),
        signature: Signature::new(vec![], void),
        locals: vec![],
        body: structured_body(vec![], air::AirTail::Return(None)),
    });
    program.module_mut(module).functions.push(func);

    let source = plan_source(program).into_string();

    assert!(source.contains("fn anv_f0_show_n_neg_1"));
}

#[test]
fn source_job_compiles_methods_as_free_functions() {
    let program = struct_method_program();
    let source = plan_source(program);
    let text = source.as_str();

    assert!(text.contains(
        "fn anv_f0_Point_value<'cx, 'rt>(_ctx: &mut AnvCtx<'cx, 'rt>, v0: anvT3_Point) -> i64"
    ));
    assert!(text.contains("anv_f0_Point_value(ctx, anvT3_Point { x: v0.x })"));
    assert!(!text.contains("impl "));
    assert!(!text.contains("trait "));
    assert!(!text.contains(".clone()"));

    let output = run_source(source);

    assert_eq!(output.status, SourceJobStatus::Success);
    assert_eq!(output.stdout, "7\n");
}

#[test]
fn profile_accepts_struct_construction_and_field_projection() {
    check(struct_field_read_program());
}

#[test]
fn source_job_compiles_and_runs_struct_copy_reconstruction() {
    let mut program = struct_field_read_program();
    let function = &mut program.functions[0];
    let point = function.locals[0].ty;
    let copied = air::LocalId::from_index(2);
    function.locals.push(local(point, LocalKind::Temp));
    function.body.block.stmts.insert(
        1,
        air::AirStmt::Init {
            local: copied,
            value: RValue::Use(Operand::Place(place(air::LocalId::from_index(0), point))),
        },
    );
    if let air::AirStmt::Init {
        value:
            RValue::Stringify {
                value: Operand::Place(place),
                ..
            },
        ..
    } = &mut function.body.block.stmts[2]
    {
        place.root = PlaceRoot::Local(copied);
    }
    let source = plan_source(program);
    let text = source.as_str();

    assert!(text.contains("let v2: anvT3_Point = anvT3_Point { x: v0.x };"));
    assert!(!text.contains(".clone()"));

    let output = run_source(source);

    assert_eq!(output.status, SourceJobStatus::Success);
    assert_eq!(output.stdout, "7\n");
}

#[test]
fn source_job_compiles_structured_control_struct_locals_without_defaults() {
    let mut program = struct_field_read_program();
    let bool_ty = program.alloc_type(TypeData::Bool);
    let cond = program.const_arena.alloc(ConstData {
        ty: bool_ty,
        value: ConstValue::Bool(true),
    });
    let then_block = program.functions[0].body.block.clone();
    program.functions[0].body = AirBody {
        block: air::AirBlock {
            stmts: vec![air::AirStmt::If(air::AirIf {
                cond: Operand::Const(cond),
                then_block,
                else_block: None,
            })],
            tail: air::AirTail::Return(None),
        },
    };

    let source = plan_source(program);
    assert!(!source.as_str().contains("Default::default()"));
    let output = run_source(source);
    assert_eq!(output.status, SourceJobStatus::Success, "{}", output.stderr);
}

#[test]
fn source_job_compiles_and_runs_struct_construction_and_field_read() {
    let program = struct_field_read_program();
    let source = plan_source(program);
    let text = source.as_str();

    assert!(text.contains("anvT3_Point { x: 7 }"));
    assert!(text.contains("v0.x"));
    assert!(!text.contains(".clone()"));
    assert!(text.contains("#[derive(Clone)]"));
    assert!(!text.contains("derive(Copy"));

    let output = run_source(source);

    assert_eq!(output.status, SourceJobStatus::Success);
    assert_eq!(output.stdout, "7\n");
}

#[test]
fn emit_renders_dataref_storage_and_handle_alias() {
    let source =
        emit::emit(&rir::verify(&dataref_metadata_rir()).expect("RIR verify failed")).into_string();

    assert!(source.contains("struct NodeStorage {"));
    assert!(source.contains("value: i64"));
    assert!(source.contains("type Node<'cx> = anvyx_runtime::Handle<'cx, NodeStorage>;"));
    assert!(source.contains("NodeHeapType: anvyx_runtime::HeapType<'cx, NodeStorage>"));
    assert!(source.contains("NodeHeapType: heap.register_tracked::<NodeStorage>()"));
    assert!(source.contains("unsafe impl<'cx> anvyx_runtime::Trace<'cx> for NodeStorage"));
    assert!(!source.contains("#[derive(anvyx_runtime::Trace)]"));
    assert!(!source.contains("NodeStorage<'cx>"));
}

#[test]
fn emit_renders_tracked_dataref_storage_with_context_lifetime() {
    let mut program = dataref_metadata_rir();
    let node = RirTypeId::from_index(1);
    program.datarefs[0].fields.push(RirField {
        id: RirFieldId::from_index(1),
        symbol: RirSymbol::new("child"),
        ty: node,
    });

    let source = emit::emit(&rir::verify(&program).expect("RIR verify failed")).into_string();

    assert!(source.contains("#[derive(anvyx_runtime::Trace)]"));
    assert!(source.contains("#[trace(crate = anvyx_runtime, ctx = 'cx)]"));
    assert!(source.contains("struct NodeStorage<'cx>"));
    assert!(source.contains("type Node<'cx> = anvyx_runtime::Handle<'cx, NodeStorage<'cx>>;"));
    assert!(source.contains("NodeHeapType: anvyx_runtime::HeapType<'cx, NodeStorage<'cx>>"));
    assert!(source.contains("NodeHeapType: heap.register_tracked::<NodeStorage<'cx>>()"));
}

#[test]
fn trace_plan_marks_generated_payloads_from_tracked_storage() {
    let mut program = dataref_metadata_rir();
    let payload_ty = add_payload_struct(&mut program);
    program.datarefs[0].fields.push(RirField {
        id: RirFieldId::from_index(1),
        symbol: RirSymbol::new("payload"),
        ty: payload_ty,
    });

    let plan = RustTracePlan::build(&program);

    assert!(plan.needs_struct_trace(RirStructId::from_index(0)));
}

#[test]
fn trace_plan_marks_generated_fields_of_cx_dependent_roots() {
    let mut program = dataref_metadata_rir();
    let node = RirTypeId::from_index(1);
    let payload = add_payload_struct(&mut program);
    let tuple = RirTupleId::from_index(program.tuples.len());
    program.types.push(RirType::Tuple(tuple));
    program.tuples.push(RirTuple {
        id: tuple,
        symbol: RirSymbol::new("Tuple"),
        display: RirSymbol::new("Tuple"),
        copyable: false,
        fields: vec![
            RirField {
                id: RirFieldId::from_index(0),
                symbol: RirSymbol::new("node"),
                ty: node,
            },
            RirField {
                id: RirFieldId::from_index(1),
                symbol: RirSymbol::new("payload"),
                ty: payload,
            },
        ],
    });

    let plan = RustTracePlan::build(&program);

    assert!(plan.needs_tuple_trace(tuple));
    assert!(plan.needs_struct_trace(RirStructId::from_index(0)));
}

#[test]
fn emit_derives_trace_for_generated_payloads_from_tracked_storage() {
    let mut program = dataref_metadata_rir();
    let payload_ty = add_payload_struct(&mut program);
    program.datarefs[0].fields.push(RirField {
        id: RirFieldId::from_index(1),
        symbol: RirSymbol::new("payload"),
        ty: payload_ty,
    });

    let source = emit::emit(&rir::verify(&program).expect("RIR verify failed")).into_string();

    assert!(source.contains(
        "#[derive(Clone, anvyx_runtime::Trace)]\n#[trace(crate = anvyx_runtime)]\nstruct Payload"
    ));
}

#[test]
fn emit_renders_plain_struct_declarations_without_impls() {
    let program = struct_decl_program(false);
    let source = plan_source(program).into_string();

    assert!(source.contains("struct anvT2_Point"));
    assert!(source.contains("x: i64"));
    assert!(source.contains("name: anvyx_runtime::AnvString"));
    assert!(source.contains("#[derive(Clone)]"));
    assert!(!source.contains("impl "));
    assert!(!source.contains("trait "));
    assert!(!source.contains("derive(Copy"));
    assert!(!source.contains("derive(Debug"));
    assert!(!source.contains(".clone()"));
    assert!(!source.contains(".to_owned()"));
}

#[test]
fn plan_mangles_same_name_structs_by_type_id() {
    let program = duplicate_struct_name_program();
    let verified = air::verify(&program).expect("AIR verify failed");
    let plan = plan(&verified, RustPlanConfig::default()).expect("plan failed");

    assert_eq!(plan.program().structs.len(), 2);
    assert_ne!(
        plan.program().structs[0].symbol,
        plan.program().structs[1].symbol
    );
}

#[test]
fn source_job_compiles_rust_keyword_member_symbols() {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let module = program.alloc_module(root_module());
    let aggregate = program.alloc_aggregate(air::AggregateDecl {
        name: Ident::new("KeywordRecord"),
        module,
        kind: air::AggregateKind::Struct,
        type_args: vec![],
        const_args: vec![],
        fields: vec![
            FieldDecl {
                name: Ident::new("crate"),
                ty: int,
            },
            FieldDecl {
                name: Ident::new("move"),
                ty: int,
            },
        ],
        cycle_capable: false,
        stringify_override: None,
    });
    program.module_mut(module).aggregates.push(aggregate);
    let record = program.alloc_type(TypeData::Aggregate(aggregate));
    let enum_id = program.alloc_enum(EnumDecl {
        name: Ident::new("KeywordEnum"),
        module,
        type_args: vec![],
        const_args: vec![],
        core: None,
        repr: air::EnumRepr::Adt,
        raw_type: None,
        variants: vec![VariantDecl {
            name: Ident::new("dyn"),
            shape: VariantShape::Unit,
            raw_value: None,
        }],
    });
    program.module_mut(module).enums.push(enum_id);
    let enum_ty = program.alloc_type(TypeData::Enum(enum_id));
    let one = program.const_arena.alloc(ConstData {
        ty: int,
        value: ConstValue::Int(1),
    });
    let two = program.const_arena.alloc(ConstData {
        ty: int,
        value: ConstValue::Int(2),
    });
    let record_local = air::LocalId::from_index(0);
    let enum_local = air::LocalId::from_index(1);
    let mut crate_field = place(record_local, int);
    crate_field
        .projection
        .push(Projection::Field(air::FieldId::from_index(0)));
    let main = program.alloc_function(Function {
        name: Ident::new("main"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], int),
        locals: vec![
            local(record, LocalKind::Temp),
            local(enum_ty, LocalKind::Temp),
        ],
        body: structured_body(
            vec![
                Statement::Init {
                    local: record_local,
                    value: RValue::Aggregate {
                        kind: AggregateCtor::Struct(aggregate),
                        fields: vec![Operand::Const(one), Operand::Const(two)],
                        ty: record,
                    },
                },
                Statement::Init {
                    local: enum_local,
                    value: RValue::Aggregate {
                        kind: AggregateCtor::EnumVariant {
                            enum_id,
                            variant: VariantId::from_index(0),
                        },
                        fields: vec![],
                        ty: enum_ty,
                    },
                },
            ],
            air::AirTail::Return(Some(Operand::Place(crate_field))),
        ),
    });
    program.module_mut(module).functions.push(main);
    program.set_entry(main);

    let source = plan_source(program);
    assert!(source.as_str().contains("r_crate: i64"));
    assert!(source.as_str().contains("r_move: i64"));
    assert!(source.as_str().contains("r_dyn"));

    let output = run_source(source);
    assert_eq!(output.status, SourceJobStatus::Success);
}

#[test]
fn plan_handles_non_topological_type_arena_refs() {
    let mut program = Program::default();
    let array = program.alloc_type(TypeData::Array {
        elem: air::TypeId::from_index(1),
        len: 1,
    });
    let int = program.alloc_type(TypeData::Int);
    assert_eq!(int.index(), 1);
    let one = program.const_arena.alloc(ConstData {
        ty: int,
        value: ConstValue::Int(1),
    });
    let module = program.alloc_module(root_module());
    let out = air::LocalId::from_index(0);
    let main = program.alloc_function(Function {
        name: Ident::new("main"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], array),
        locals: vec![local(array, LocalKind::Temp)],
        body: structured_body(
            vec![Statement::Init {
                local: out,
                value: RValue::Aggregate {
                    kind: AggregateCtor::Array,
                    fields: vec![Operand::Const(one)],
                    ty: array,
                },
            }],
            air::AirTail::Return(Some(Operand::Place(place(out, array)))),
        ),
    });
    program.module_mut(module).functions.push(main);
    program.set_entry(main);

    let verified = air::verify(&program).expect("AIR verify failed");
    let plan = plan(&verified, RustPlanConfig::default()).expect("plan failed");
    assert_eq!(
        plan.program().types[array.index()],
        RirType::Array {
            elem: RirTypeId::from_index(int.index()),
            len: 1,
        }
    );
}

#[test]
fn rir_verify_accepts_tuple_declaration_and_construction() {
    let program = tuple_rir_program(RirRValue::Tuple {
        ty: RirTypeId::from_index(3),
        fields: vec![
            RirOperand::Const(RirConstId::from_index(0)),
            RirOperand::Const(RirConstId::from_index(1)),
        ],
    });

    rir::verify(&program).expect("RIR verify failed");
}

#[test]
fn rir_verify_rejects_bad_tuple_type_id() {
    let mut program = empty_rir_function(RirType::Tuple(RirTupleId::from_index(99)));
    program.types.push(RirType::Void);
    program.functions[0].ret.ty = RirTypeId::from_index(1);

    assert_rir_error(program, RirVerifyErrorKind::BadId);
}

#[test]
fn rir_verify_rejects_tuple_rvalue_with_non_tuple_type() {
    let program = tuple_rir_program(RirRValue::Tuple {
        ty: RirTypeId::from_index(0),
        fields: vec![
            RirOperand::Const(RirConstId::from_index(0)),
            RirOperand::Const(RirConstId::from_index(1)),
        ],
    });

    assert_rir_error(program, RirVerifyErrorKind::UnsupportedRValueType);
}

#[test]
fn rir_verify_rejects_tuple_rvalue_arity_mismatch() {
    let program = tuple_rir_program(RirRValue::Tuple {
        ty: RirTypeId::from_index(3),
        fields: vec![RirOperand::Const(RirConstId::from_index(0))],
    });

    assert_rir_error(
        program,
        RirVerifyErrorKind::FieldCount {
            expected: 2,
            found: 1,
        },
    );
}

#[test]
fn rir_verify_rejects_tuple_rvalue_element_type_mismatch() {
    let program = tuple_rir_program(RirRValue::Tuple {
        ty: RirTypeId::from_index(3),
        fields: vec![
            RirOperand::Const(RirConstId::from_index(1)),
            RirOperand::Const(RirConstId::from_index(1)),
        ],
    });

    assert_rir_type_error(program);
}

#[test]
fn rir_verify_rejects_bad_tuple_projection() {
    let mut program = tuple_rir_program(valid_tuple_rvalue());
    program.functions[0]
        .body
        .stmts
        .push(RirStmt::Eval(RirRValue::Use(RirOperand::Place(RirPlace {
            local: RirLocalId::from_index(0),
            projections: vec![RirProjection::TupleField(RirFieldId::from_index(2))],
            ty: RirTypeId::from_index(0),
        }))));

    assert_rir_error(program, RirVerifyErrorKind::BadId);
}

#[test]
fn rir_verify_rejects_tuple_projection_on_non_tuple_place() {
    let mut program = tuple_rir_program(valid_tuple_rvalue());
    program.functions[0].locals.push(RirLocal {
        id: RirLocalId::from_index(1),
        ty: RirTypeId::from_index(0),
        mutable: false,
        symbol: RirSymbol::new("i"),
        initialized: true,
        payload_ref: false,
    });
    program.functions[0]
        .body
        .stmts
        .push(RirStmt::Eval(RirRValue::Use(RirOperand::Place(RirPlace {
            local: RirLocalId::from_index(1),
            projections: vec![RirProjection::TupleField(RirFieldId::from_index(0))],
            ty: RirTypeId::from_index(0),
        }))));

    assert_rir_error(program, RirVerifyErrorKind::UnsupportedRValueType);
}

#[test]
fn rir_verify_rejects_copyable_tuple_with_noncopy_field() {
    let mut program = tuple_rir_program(valid_tuple_rvalue());
    program.tuples[0].fields[0].ty = RirTypeId::from_index(4);
    program.tuples[0].copyable = true;
    program.types.push(RirType::String);

    assert_rir_error(program, RirVerifyErrorKind::NonCopyValueRequired);
}

#[test]
fn rir_verify_rejects_copyable_tuple_with_falsely_copyable_struct_field() {
    let int = RirTypeId::from_index(0);
    let strukt = RirTypeId::from_index(1);
    let program = RirProgram {
        types: vec![
            RirType::Int,
            RirType::Struct(RirStructId::from_index(0)),
            RirType::Tuple(RirTupleId::from_index(0)),
        ],
        structs: vec![RirStruct {
            id: RirStructId::from_index(0),
            air_id: None,
            symbol: RirSymbol::new("Box"),
            display: RirSymbol::new("Box"),
            native_path: None,
            native_key: None,
            copyable: false,
            fields: vec![RirField {
                id: RirFieldId::from_index(0),
                symbol: RirSymbol::new("x"),
                ty: int,
            }],
        }],
        tuples: vec![RirTuple {
            id: RirTupleId::from_index(0),
            symbol: RirSymbol::new("Tuple"),
            display: RirSymbol::new("(Box)"),
            copyable: true,
            fields: vec![RirField {
                id: RirFieldId::from_index(0),
                symbol: RirSymbol::new("_0"),
                ty: strukt,
            }],
        }],
        ..RirProgram::default()
    };

    assert_rir_error(program, RirVerifyErrorKind::NonCopyValueRequired);
}

#[test]
fn rir_verify_rejects_noncopy_value_call_arg() {
    let int = RirTypeId::from_index(0);
    let void = RirTypeId::from_index(1);
    let slice = RirTypeId::from_index(2);
    let strukt = RirTypeId::from_index(3);
    let program = RirProgram {
        types: vec![
            RirType::Int,
            RirType::Void,
            RirType::Slice(int),
            RirType::Struct(RirStructId::from_index(0)),
        ],
        structs: vec![RirStruct {
            id: RirStructId::from_index(0),
            air_id: None,
            symbol: RirSymbol::new("Box"),
            display: RirSymbol::new("Box"),
            native_path: None,
            native_key: None,
            copyable: false,
            fields: vec![RirField {
                id: RirFieldId::from_index(0),
                symbol: RirSymbol::new("x"),
                ty: slice,
            }],
        }],
        functions: vec![
            RirFunction {
                id: RirFunctionId::from_index(0),
                air_id: None,
                symbol: RirSymbol::new("callee"),
                params: vec![RirParam {
                    local: RirLocalId::from_index(0),
                    ty: strukt,
                    semantic: RirParamSemantic::Value,
                    abi: RirParamAbi::Value,
                    escape: RirParamEscape::NonEscaping,
                }],
                ret: RirReturn { ty: void },
                locals: vec![RirLocal {
                    id: RirLocalId::from_index(0),
                    ty: strukt,
                    mutable: false,
                    symbol: RirSymbol::new("xs"),
                    initialized: true,
                    payload_ref: false,
                }],
                body: RirStructuredBlock {
                    stmts: vec![],
                    term: RirTerm::Return(None),
                },
            },
            RirFunction {
                id: RirFunctionId::from_index(1),
                air_id: None,
                symbol: RirSymbol::new("caller"),
                params: vec![],
                ret: RirReturn { ty: void },
                locals: vec![RirLocal {
                    id: RirLocalId::from_index(0),
                    ty: strukt,
                    mutable: false,
                    symbol: RirSymbol::new("xs"),
                    initialized: true,
                    payload_ref: false,
                }],
                body: RirStructuredBlock {
                    stmts: vec![RirStmt::Eval(RirRValue::Call {
                        callee: RirCallTarget::Function(RirFunctionId::from_index(0)),
                        args: vec![RirCallArg::Value(RirOperand::Place(RirPlace {
                            local: RirLocalId::from_index(0),
                            projections: vec![],
                            ty: strukt,
                        }))],
                        ty: void,
                    })],
                    term: RirTerm::Return(None),
                },
            },
        ],
        entry: Some(RirFunctionId::from_index(1)),
        ..RirProgram::default()
    };

    assert_rir_error(program, RirVerifyErrorKind::NonCopyValueRequired);
}

#[test]
fn rir_verify_rejects_bad_struct_declarations() {
    let mut program = empty_rir_function(RirType::Struct(RirStructId::from_index(0)));
    program.structs.push(RirStruct {
        id: RirStructId::from_index(0),
        air_id: None,
        symbol: RirSymbol::new("S"),
        display: RirSymbol::new("S"),
        native_path: None,
        native_key: None,
        copyable: true,
        fields: vec![
            RirField {
                id: RirFieldId::from_index(0),
                symbol: RirSymbol::new("x"),
                ty: RirTypeId::from_index(9),
            },
            RirField {
                id: RirFieldId::from_index(1),
                symbol: RirSymbol::new("x"),
                ty: RirTypeId::from_index(0),
            },
        ],
    });

    let errors = rir::verify(&program).expect_err("verified invalid struct");
    assert!(
        errors
            .iter()
            .any(|error| error.kind == RirVerifyErrorKind::BadId)
    );
    assert!(
        errors
            .iter()
            .any(|error| error.kind == RirVerifyErrorKind::DuplicateSymbol)
    );
}

#[test]
fn rir_verify_rejects_bad_struct_construction_and_projection() {
    let mut program = empty_rir_function(RirType::Struct(RirStructId::from_index(0)));
    program.types.push(RirType::Int);
    program.structs.push(RirStruct {
        id: RirStructId::from_index(0),
        air_id: None,
        symbol: RirSymbol::new("S"),
        display: RirSymbol::new("S"),
        native_path: None,
        native_key: None,
        copyable: true,
        fields: vec![RirField {
            id: RirFieldId::from_index(0),
            symbol: RirSymbol::new("x"),
            ty: RirTypeId::from_index(1),
        }],
    });
    program.functions[0].locals.push(RirLocal {
        id: RirLocalId::from_index(0),
        symbol: RirSymbol::new("s"),
        ty: RirTypeId::from_index(0),
        mutable: true,
        initialized: false,
        payload_ref: false,
    });
    program.functions[0].locals.push(RirLocal {
        id: RirLocalId::from_index(1),
        symbol: RirSymbol::new("i"),
        ty: RirTypeId::from_index(1),
        mutable: true,
        initialized: false,
        payload_ref: false,
    });
    program.functions[0].body.stmts = vec![
        RirStmt::Init {
            local: RirLocalId::from_index(0),
            value: RirRValue::Struct {
                ty: RirTypeId::from_index(0),
                fields: vec![],
            },
        },
        RirStmt::Assign {
            dst: RirPlace {
                local: RirLocalId::from_index(1),
                projections: vec![RirProjection::Field(RirFieldId::from_index(0))],
                ty: RirTypeId::from_index(1),
            },
            value: RirRValue::Use(RirOperand::Const(RirConstId::from_index(0))),
        },
    ];

    let errors = rir::verify(&program).expect_err("verified invalid struct operations");
    assert!(errors.iter().any(|error| error.kind
        == RirVerifyErrorKind::FieldCount {
            expected: 1,
            found: 0
        }));
    assert!(
        errors
            .iter()
            .any(|error| error.kind == RirVerifyErrorKind::UnsupportedRValueType)
    );
}

#[test]
fn profile_accepts_function_types() {
    let mut program = Program::default();
    let void = program.alloc_type(TypeData::Void);
    program.alloc_type(TypeData::Function(air::SignatureType::new(
        vec![],
        air::ReturnMode::Value(void),
    )));

    check(program);
}

#[test]
fn profile_rejects_function_value_containers() {
    let mut program = Program::default();
    let void = program.alloc_type(TypeData::Void);
    let function = program.alloc_type(TypeData::Function(air::SignatureType::new(
        vec![],
        air::ReturnMode::Value(void),
    )));
    program.alloc_type(TypeData::Array {
        elem: function,
        len: 1,
    });

    expect_reject(program, ProfileErrorKind::UnsupportedLambdaValue);
}

#[test]
fn plan_interns_function_type_signatures_with_escape_modes() {
    let mut program = Program::default();
    let void = program.alloc_type(TypeData::Void);
    let inner = program.alloc_type(TypeData::Function(air::SignatureType::new(
        vec![],
        air::ReturnMode::Value(void),
    )));
    let non = program.alloc_type(TypeData::Function(air::SignatureType::new(
        vec![air::ParamType {
            ty: inner,
            mode: ParamMode::Value,
            escape: ParamEscape::NonEscaping,
        }],
        air::ReturnMode::Value(void),
    )));
    let esc = program.alloc_type(TypeData::Function(air::SignatureType::new(
        vec![air::ParamType {
            ty: inner,
            mode: ParamMode::Value,
            escape: ParamEscape::Escaping,
        }],
        air::ReturnMode::Value(void),
    )));

    let verified = air::verify(&program).expect("AIR verify failed");
    let plan = plan(&verified, RustPlanConfig::default()).expect("plan failed");

    let RirType::Lambda(non_sig) = plan.program().types[non.index()] else {
        panic!("non-escaping function type did not become RIR Lambda type");
    };
    let RirType::Lambda(esc_sig) = plan.program().types[esc.index()] else {
        panic!("escaping function type did not become RIR Lambda type");
    };
    assert_ne!(non_sig, esc_sig);
    assert_eq!(
        plan.program().lambda_sigs[non_sig.index()].params[0].escape,
        RirParamEscape::NonEscaping
    );
    assert_eq!(
        plan.program().lambda_sigs[esc_sig.index()].params[0].escape,
        RirParamEscape::Escaping
    );
}

#[test]
fn rir_accepts_stack_cell_declaration() {
    rir::verify(&stack_cell_rir(valid_stack_cell_decl())).expect("RIR verify failed");
}

#[test]
fn rir_rejects_stack_cell_bad_id() {
    assert_rir_error(
        stack_cell_rir_with(|cell| cell.id = RirCellId::from_index(1)),
        RirVerifyErrorKind::BadId,
    );
}

#[test]
fn rir_rejects_stack_cell_bad_owner() {
    assert_rir_error(
        stack_cell_rir_with(|cell| cell.owner = RirFunctionId::from_index(1)),
        RirVerifyErrorKind::BadId,
    );
}

#[test]
fn rir_rejects_stack_cell_bad_source_local() {
    assert_rir_error(
        stack_cell_rir_with(|cell| cell.source_local = RirLocalId::from_index(1)),
        RirVerifyErrorKind::BadId,
    );
}

#[test]
fn rir_rejects_stack_cell_payload_type_mismatch() {
    assert_rir_type_error(stack_cell_rir_with(|cell| {
        cell.payload_ty = RirTypeId::from_index(0);
    }));
}

#[test]
fn rir_rejects_stack_cell_immutable_source_local() {
    let mut program = stack_cell_rir(valid_stack_cell_decl());
    program.functions[0].locals[0].mutable = false;

    assert_rir_error(
        program,
        RirVerifyErrorKind::ImmutableCellSourceLocal(RirLocalId::from_index(0)),
    );
}

#[test]
fn rir_rejects_stack_cell_payload_ref_source_local() {
    let mut program = stack_cell_rir(valid_stack_cell_decl());
    program.functions[0].locals[0].payload_ref = true;

    assert_rir_error(
        program,
        RirVerifyErrorKind::CellSourceLocalPayloadRef(RirLocalId::from_index(0)),
    );
}

#[test]
fn rir_rejects_duplicate_stack_cells_for_source_local() {
    let mut program = stack_cell_rir(valid_stack_cell_decl());
    let mut duplicate = valid_stack_cell_decl();
    duplicate.id = RirCellId::from_index(1);
    program.cells.push(duplicate);

    assert_rir_error(
        program,
        RirVerifyErrorKind::DuplicateCell {
            owner: RirFunctionId::from_index(0),
            source_local: RirLocalId::from_index(0),
            first: RirCellId::from_index(0),
            second: RirCellId::from_index(1),
        },
    );
}

#[test]
fn rir_accepts_stack_cell_init_get_and_set() {
    let mut program = stack_cell_rir(valid_stack_cell_decl());
    let int = RirTypeId::from_index(1);
    let one = push_int_const(&mut program, 1);
    let two = push_int_const(&mut program, 2);
    program.functions[0].body.stmts = vec![
        RirStmt::CellInit {
            cell: owner_cell_ref(),
            value: RirRValue::Use(RirOperand::Const(one)),
        },
        RirStmt::Eval(RirRValue::CellGetCopy {
            cell: owner_cell_ref(),
            ty: int,
        }),
        RirStmt::CellSet {
            cell: owner_cell_ref(),
            value: RirRValue::Use(RirOperand::Const(two)),
        },
    ];

    rir::verify(&program).expect("RIR verify failed");
}

#[test]
fn rir_rejects_stack_cell_read_before_init() {
    let mut program = stack_cell_rir(valid_stack_cell_decl());
    program.functions[0].body.stmts = vec![RirStmt::Eval(RirRValue::CellGetCopy {
        cell: owner_cell_ref(),
        ty: RirTypeId::from_index(1),
    })];

    assert_rir_error(
        program,
        RirVerifyErrorKind::UninitializedCell(RirCellId::from_index(0)),
    );
}

#[test]
fn rir_rejects_stack_cell_set_before_init() {
    let mut program = stack_cell_rir(valid_stack_cell_decl());
    let one = push_int_const(&mut program, 1);
    program.functions[0].body.stmts = vec![RirStmt::CellSet {
        cell: owner_cell_ref(),
        value: RirRValue::Use(RirOperand::Const(one)),
    }];

    assert_rir_error(
        program,
        RirVerifyErrorKind::UninitializedCell(RirCellId::from_index(0)),
    );
}

#[test]
fn rir_rejects_stack_cell_double_init() {
    let mut program = stack_cell_rir(valid_stack_cell_decl());
    let one = push_int_const(&mut program, 1);
    let two = push_int_const(&mut program, 2);
    program.functions[0].body.stmts = vec![
        RirStmt::CellInit {
            cell: owner_cell_ref(),
            value: RirRValue::Use(RirOperand::Const(one)),
        },
        RirStmt::CellInit {
            cell: owner_cell_ref(),
            value: RirRValue::Use(RirOperand::Const(two)),
        },
    ];

    assert_rir_error(
        program,
        RirVerifyErrorKind::InitCellTwice(RirCellId::from_index(0)),
    );
}

#[test]
fn rir_rejects_stack_cell_get_type_mismatch() {
    let mut program = stack_cell_rir(valid_stack_cell_decl());
    let one = push_int_const(&mut program, 1);
    program.functions[0].body.stmts = vec![
        RirStmt::CellInit {
            cell: owner_cell_ref(),
            value: RirRValue::Use(RirOperand::Const(one)),
        },
        RirStmt::Eval(RirRValue::CellGetCopy {
            cell: owner_cell_ref(),
            ty: RirTypeId::from_index(0),
        }),
    ];

    assert_rir_type_error(program);
}

#[test]
fn rir_rejects_nonshareable_stack_cell_get_copy() {
    let mut program = stack_cell_rir_with(|cell| cell.payload_ty = RirTypeId::from_index(2));
    program.types.push(RirType::Slice(RirTypeId::from_index(1)));
    program.functions[0].locals[0].ty = RirTypeId::from_index(2);
    program.functions[0].body.stmts = vec![RirStmt::Eval(RirRValue::CellGetCopy {
        cell: owner_cell_ref(),
        ty: RirTypeId::from_index(2),
    })];

    assert_rir_error(program, RirVerifyErrorKind::NonCopyValueRequired);
}

#[test]
fn rir_rejects_cell_ops_with_capture_ref_until_hidden_capture_validation() {
    let mut program = stack_cell_rir(valid_stack_cell_decl());
    program.functions[0].body.stmts = vec![RirStmt::Eval(RirRValue::CellGetCopy {
        cell: RirCellRef::Capture {
            cell: RirCellId::from_index(0),
            local: RirLocalId::from_index(99),
        },
        ty: RirTypeId::from_index(1),
    })];

    assert_rir_error(program, RirVerifyErrorKind::UnsupportedLambdaCapture);
}

#[test]
fn emit_stack_cell_ops_use_runtime_cell() {
    let mut program = stack_cell_rir(valid_stack_cell_decl());
    program.entry = Some(RirFunctionId::from_index(0));
    let one = push_int_const(&mut program, 1);
    let two = push_int_const(&mut program, 2);
    program.functions[0].body.stmts = vec![
        RirStmt::CellInit {
            cell: owner_cell_ref(),
            value: RirRValue::Use(RirOperand::Const(one)),
        },
        RirStmt::Eval(RirRValue::CellGetCopy {
            cell: owner_cell_ref(),
            ty: RirTypeId::from_index(1),
        }),
        RirStmt::CellSet {
            cell: owner_cell_ref(),
            value: RirRValue::Use(RirOperand::Const(two)),
        },
    ];

    let source = emit::emit(&rir::verify(&program).expect("RIR verify failed"));
    assert!(
        source.as_str().contains(
            "let __cell0: anvyx_runtime::StackLambdaCell<i64> = anvyx_runtime::StackLambdaCell::<i64>::new(1);"
        ),
        "{}",
        source.as_str()
    );
    assert!(source.as_str().contains("__cell0.get_copy()?;"));
    assert!(source.as_str().contains("__cell0.set(2)?;"));
    assert!(!source.as_str().contains("RefCell"));
    assert!(!source.as_str().contains("Rc<"));

    let output = run_source(source);
    assert_eq!(output.status, SourceJobStatus::Success, "{}", output.stderr);
}

#[test]
fn emit_stack_cell_init_is_control_flow_safe() {
    let mut program = stack_cell_rir(valid_stack_cell_decl());
    program.entry = Some(RirFunctionId::from_index(0));
    let bool_ty = RirTypeId::from_index(2);
    program.types.push(RirType::Bool);
    let cond = RirConstId::from_index(program.consts.len());
    program.consts.push(RirConst {
        id: cond,
        ty: bool_ty,
        value: RirConstValue::Bool(true),
    });
    let one = push_int_const(&mut program, 1);
    let two = push_int_const(&mut program, 2);
    let cell = owner_cell_ref();
    program.functions[0].body.stmts = vec![
        RirStmt::If(RirIf {
            cond: RirOperand::Const(cond),
            then_block: RirStructuredBlock {
                stmts: vec![RirStmt::CellInit {
                    cell,
                    value: RirRValue::Use(RirOperand::Const(one)),
                }],
                term: RirTerm::None,
            },
            else_block: Some(RirStructuredBlock {
                stmts: vec![RirStmt::CellInit {
                    cell,
                    value: RirRValue::Use(RirOperand::Const(two)),
                }],
                term: RirTerm::None,
            }),
        }),
        RirStmt::Eval(RirRValue::CellGetCopy {
            cell,
            ty: RirTypeId::from_index(1),
        }),
    ];

    let source = emit::emit(&rir::verify(&program).expect("RIR verify failed"));
    let text = source.as_str();
    assert!(text.contains("let __cell0: anvyx_runtime::StackLambdaCell<i64>;"));
    assert!(text.contains("__cell0 = anvyx_runtime::StackLambdaCell::<i64>::new(1);"));
    assert!(text.contains("__cell0 = anvyx_runtime::StackLambdaCell::<i64>::new(2);"));
    assert!(!text.contains("let mut source: i64;"));

    let output = run_source(source);
    assert_eq!(output.status, SourceJobStatus::Success, "{}", output.stderr);
}

#[test]
fn rir_rejects_stack_cell_init_in_loop() {
    let mut program = stack_cell_rir(valid_stack_cell_decl());
    let one = push_int_const(&mut program, 1);
    program.functions[0].body.stmts = vec![RirStmt::Loop(RirLoop {
        id: RirLoopId::from_index(0),
        body: RirStructuredBlock {
            stmts: vec![RirStmt::CellInit {
                cell: owner_cell_ref(),
                value: RirRValue::Use(RirOperand::Const(one)),
            }],
            term: RirTerm::None,
        },
    })];

    assert_rir_error(program, RirVerifyErrorKind::UnsupportedLambdaCell);
}

#[test]
fn rir_accepts_heap_env_lambda_capture_descriptor_and_value() {
    let program = valid_heap_env_lambda_rir();
    rir::verify(&program).expect("RIR verify failed");
}

#[test]
fn rir_accepts_zero_env_escaping_lambda_without_env() {
    let program = zero_env_escaping_lambda_rir();
    rir::verify(&program).expect("RIR verify failed");
}

#[test]
fn rir_rejects_heap_env_on_non_escaping_lambda() {
    let mut program = valid_heap_env_lambda_rir();
    program.lambdas[0].escape = RirLambdaEscape::NonEscaping;

    assert_rir_error(program, RirVerifyErrorKind::UnsupportedLambdaCapture);
}

#[test]
fn rir_rejects_heap_env_with_mut_borrow_capture() {
    let mut program = valid_heap_env_lambda_rir();
    program.lambdas[0].captures[0].semantic = RirParamSemantic::MutBorrow;
    program.lambdas[0].captures[0].abi = RirParamAbi::MutBorrow;
    program.functions[0].params[0].semantic = RirParamSemantic::MutBorrow;
    program.functions[0].params[0].abi = RirParamAbi::MutBorrow;

    assert_rir_error(program, RirVerifyErrorKind::UnsupportedLambdaCapture);
}

#[test]
fn rir_rejects_heap_env_with_scoped_cell_capture() {
    let mut program = valid_heap_env_lambda_rir();
    program.lambdas[0].captures[0].semantic = RirParamSemantic::ScopedPlaceCell;
    program.lambdas[0].captures[0].abi = RirParamAbi::ScopedPlaceCell;
    program.lambdas[0].captures[0].kind = RirLambdaCaptureKind::ScopedPlaceCell {
        cell: RirScopedPlaceCellId::from_index(0),
    };

    assert_rir_error(program, RirVerifyErrorKind::UnsupportedLambdaCapture);
}

#[test]
fn rir_rejects_shared_heap_env_layout_between_lambdas() {
    let mut program = valid_heap_env_lambda_rir();
    let mut lambda = program.lambdas[0].clone();
    lambda.id = RirLambdaId::from_index(1);
    program.lambdas.push(lambda);

    assert_rir_error(program, RirVerifyErrorKind::UnsupportedLambdaCapture);
}

#[test]
fn rir_rejects_unknown_function_value_captured_by_heap_env() {
    let mut program = valid_heap_env_lambda_rir();
    let lambda_ty = RirTypeId::from_index(2);
    program.lambdas[0].captures[0].ty = lambda_ty;
    program.lambda_envs[0].fields[0].ty = lambda_ty;
    program.functions[0].params[0].ty = lambda_ty;
    program.functions[0].locals[0].ty = lambda_ty;
    program.functions[1].locals[0].ty = lambda_ty;
    let RirStmt::Init {
        value: RirRValue::Lambda { captures, .. },
        ..
    } = &mut program.functions[1].body.stmts[0]
    else {
        panic!("missing lambda init");
    };
    captures[0] = RirLambdaCaptureArg::Readonly {
        value: RirOperand::Place(RirPlace {
            local: RirLocalId::from_index(0),
            projections: vec![],
            ty: lambda_ty,
        }),
    };

    assert_rir_error(program, RirVerifyErrorKind::CallArgEscape);
}

#[test]
fn rir_rejects_heap_env_and_borrowed_variants_sharing_signature() {
    let mut program = valid_heap_env_lambda_rir();
    let void = RirTypeId::from_index(0);
    let int = RirTypeId::from_index(1);
    let sig = RirLambdaSigId::from_index(0);
    let lambda = RirLambdaId::from_index(1);
    let target = RirFunctionId::from_index(2);
    let source = RirLocalId::from_index(0);

    program.lambdas.push(RirLambda {
        id: lambda,
        source: RirLambdaSource::Lambda(air::LambdaId::from_index(1)),
        function: target,
        sig,
        escape: RirLambdaEscape::NonEscaping,
        storage: RirLambdaStorage::ScopedCaptures,
        captures: vec![RirLambdaCapture {
            ty: int,
            semantic: RirParamSemantic::SharedBorrow,
            abi: RirParamAbi::SharedBorrow,
            kind: RirLambdaCaptureKind::Param,
        }],
    });
    program.functions.push(RirFunction {
        id: target,
        air_id: None,
        symbol: RirSymbol::new("borrowed_target"),
        params: vec![RirParam {
            local: source,
            ty: int,
            semantic: RirParamSemantic::SharedBorrow,
            abi: RirParamAbi::SharedBorrow,
            escape: RirParamEscape::NonEscaping,
        }],
        ret: RirReturn { ty: void },
        locals: vec![RirLocal {
            id: source,
            ty: int,
            mutable: false,
            symbol: RirSymbol::new("capture"),
            initialized: true,
            payload_ref: false,
        }],
        body: RirStructuredBlock::default(),
    });

    assert_rir_error(program, RirVerifyErrorKind::UnsupportedLambdaCapture);
}

#[test]
fn rir_accepts_returning_heap_env_lambda_local() {
    let mut program = valid_heap_env_lambda_rir();
    let lambda_ty = RirTypeId::from_index(2);
    let f = RirLocalId::from_index(1);
    program.functions[1].ret.ty = lambda_ty;
    program.functions[1].body.term = RirTerm::Return(Some(RirOperand::Place(RirPlace {
        local: f,
        projections: vec![],
        ty: lambda_ty,
    })));

    rir::verify(&program).expect("RIR verify failed");
}

#[test]
fn rir_accepts_heap_env_lambda_local_alias_and_repeated_calls() {
    let mut program = valid_heap_env_lambda_rir();
    let void = RirTypeId::from_index(0);
    let lambda_ty = RirTypeId::from_index(2);
    let f = RirLocalId::from_index(1);
    let g = RirLocalId::from_index(2);
    program.functions[1].locals.push(RirLocal {
        id: g,
        ty: lambda_ty,
        mutable: false,
        symbol: RirSymbol::new("g"),
        initialized: false,
        payload_ref: false,
    });
    program.functions[1].body.stmts.extend([
        RirStmt::Init {
            local: g,
            value: RirRValue::Use(RirOperand::Place(RirPlace {
                local: f,
                projections: vec![],
                ty: lambda_ty,
            })),
        },
        RirStmt::Eval(RirRValue::Call {
            callee: RirCallTarget::LambdaValue {
                callee: RirOperand::Place(RirPlace {
                    local: g,
                    projections: vec![],
                    ty: lambda_ty,
                }),
                sig: RirLambdaSigId::from_index(0),
            },
            args: vec![],
            ty: void,
        }),
        RirStmt::Eval(RirRValue::Call {
            callee: RirCallTarget::LambdaValue {
                callee: RirOperand::Place(RirPlace {
                    local: g,
                    projections: vec![],
                    ty: lambda_ty,
                }),
                sig: RirLambdaSigId::from_index(0),
            },
            args: vec![],
            ty: void,
        }),
    ]);

    rir::verify(&program).expect("RIR verify failed");
}

#[test]
fn rir_accepts_heap_env_lambda_passed_to_escaping_param() {
    let mut program = valid_heap_env_lambda_rir();
    let void = RirTypeId::from_index(0);
    let lambda_ty = RirTypeId::from_index(2);
    let f = RirLocalId::from_index(1);
    let consumer = RirFunctionId::from_index(2);
    program.functions.push(RirFunction {
        id: consumer,
        air_id: None,
        symbol: RirSymbol::new("consumer"),
        params: vec![RirParam {
            local: RirLocalId::from_index(0),
            ty: lambda_ty,
            semantic: RirParamSemantic::Value,
            abi: RirParamAbi::Value,
            escape: RirParamEscape::Escaping,
        }],
        ret: RirReturn { ty: void },
        locals: vec![RirLocal {
            id: RirLocalId::from_index(0),
            ty: lambda_ty,
            mutable: false,
            symbol: RirSymbol::new("f"),
            initialized: true,
            payload_ref: false,
        }],
        body: RirStructuredBlock::default(),
    });
    program.functions[1]
        .body
        .stmts
        .push(RirStmt::Eval(RirRValue::Call {
            callee: RirCallTarget::Function(consumer),
            args: vec![RirCallArg::Value(RirOperand::Place(RirPlace {
                local: f,
                projections: vec![],
                ty: lambda_ty,
            }))],
            ty: void,
        }));

    rir::verify(&program).expect("RIR verify failed");
}

#[test]
fn rir_rejects_heap_env_lambda_call_signature_mismatch() {
    let mut program = valid_heap_env_lambda_rir();
    let void = RirTypeId::from_index(0);
    let int = RirTypeId::from_index(1);
    let lambda_ty = RirTypeId::from_index(2);
    let f = RirLocalId::from_index(1);
    program.lambda_sigs.push(RirLambdaSig {
        id: RirLambdaSigId::from_index(1),
        params: vec![RirLambdaParam {
            ty: int,
            semantic: RirParamSemantic::Value,
            abi: RirParamAbi::Value,
            escape: RirParamEscape::NonEscaping,
        }],
        ret: void,
    });
    program.functions[1]
        .body
        .stmts
        .push(RirStmt::Eval(RirRValue::Call {
            callee: RirCallTarget::LambdaValue {
                callee: RirOperand::Place(RirPlace {
                    local: f,
                    projections: vec![],
                    ty: lambda_ty,
                }),
                sig: RirLambdaSigId::from_index(1),
            },
            args: vec![],
            ty: void,
        }));

    assert_rir_error(
        program,
        RirVerifyErrorKind::CallArgCount {
            expected: 1,
            found: 0,
        },
    );
}

#[test]
fn heap_env_lambda_signatures_need_context_and_are_not_copy() {
    let program = valid_heap_env_lambda_rir();
    let policy = RustRepPolicy::new(&program);
    let sig = RirLambdaSigId::from_index(0);

    assert!(policy.lambda_sig_has_heap_env(sig));
    assert!(policy.lambda_sig_needs_ctx_lifetime(sig));
    assert!(!policy.lambda_sig_copyable(sig));
}

#[test]
fn rir_rejects_heap_env_field_capture_count_mismatch() {
    let mut program = valid_heap_env_lambda_rir();
    program.lambda_envs[0].fields.clear();

    assert_rir_error(program, RirVerifyErrorKind::UnsupportedLambdaCapture);
}

#[test]
fn rir_accepts_stack_cell_lambda_capture_descriptor_and_value() {
    rir::verify(&valid_stack_cell_lambda_rir()).expect("RIR verify failed");
}

#[test]
fn rir_accepts_scoped_place_cell_declaration() {
    rir::verify(&scoped_place_cell_rir(valid_scoped_place_cell_decl())).expect("RIR verify failed");
}

#[test]
fn rir_accepts_scoped_place_cell_lambda_capture_descriptor_and_value() {
    rir::verify(&valid_scoped_place_cell_lambda_rir()).expect("RIR verify failed");
}

#[test]
fn rir_accepts_scoped_place_cell_get() {
    let mut program = scoped_place_cell_rir(valid_scoped_place_cell_decl());
    program.functions[0]
        .body
        .stmts
        .push(RirStmt::Eval(RirRValue::ScopedPlaceCellGet {
            cell: RirScopedPlaceCellRef::Owner(RirScopedPlaceCellId::from_index(0)),
            ty: RirTypeId::from_index(1),
        }));

    rir::verify(&program).expect("RIR verify failed");
}

#[test]
fn rir_accepts_scoped_place_cell_get_shareable_noncopy_payload() {
    let mut program = scoped_place_cell_rir(valid_scoped_place_cell_decl());
    let string = RirTypeId::from_index(program.types.len());
    program.types.push(RirType::String);
    program.scoped_place_cells[0].payload_ty = string;
    program.functions[0].params[0].ty = string;
    program.functions[0].locals[0].ty = string;
    program.functions[0]
        .body
        .stmts
        .push(RirStmt::Eval(RirRValue::ScopedPlaceCellGet {
            cell: RirScopedPlaceCellRef::Owner(RirScopedPlaceCellId::from_index(0)),
            ty: string,
        }));

    rir::verify(&program).expect("RIR verify failed");
}

#[test]
fn rir_accepts_scoped_place_cell_set() {
    let mut program = scoped_place_cell_rir(valid_scoped_place_cell_decl());
    let value = push_int_const(&mut program, 1);
    program.functions[0]
        .body
        .stmts
        .push(RirStmt::ScopedPlaceCellSet {
            cell: RirScopedPlaceCellRef::Owner(RirScopedPlaceCellId::from_index(0)),
            value: RirRValue::Use(RirOperand::Const(value)),
        });

    rir::verify(&program).expect("RIR verify failed");
}

#[test]
fn rir_accepts_scoped_place_cell_mut_place_arg() {
    let mut program = scoped_place_cell_rir(valid_scoped_place_cell_decl());
    program
        .functions
        .push(mut_place_sink_function(RirFunctionId::from_index(1)));
    program.functions[0]
        .body
        .stmts
        .push(RirStmt::Eval(RirRValue::Call {
            callee: RirCallTarget::Function(RirFunctionId::from_index(1)),
            args: vec![RirCallArg::MutPlace(RirMutPlaceArg::ScopedPlaceCell {
                cell: RirScopedPlaceCellRef::Owner(RirScopedPlaceCellId::from_index(0)),
                ty: RirTypeId::from_index(1),
            })],
            ty: RirTypeId::from_index(0),
        }));

    rir::verify(&program).expect("RIR verify failed");
}

#[test]
fn rir_rejects_promoted_scoped_place_source_local_use() {
    let mut program = scoped_place_cell_rir(valid_scoped_place_cell_decl());
    program.functions[0]
        .body
        .stmts
        .push(RirStmt::Eval(RirRValue::Use(RirOperand::Place(RirPlace {
            local: RirLocalId::from_index(0),
            projections: vec![],
            ty: RirTypeId::from_index(1),
        }))));

    assert_rir_error(program, RirVerifyErrorKind::UnsupportedLambdaCapture);
}

#[test]
fn rir_rejects_promoted_scoped_place_source_param_forwarding() {
    let mut program = scoped_place_cell_rir(valid_scoped_place_cell_decl());
    program
        .functions
        .push(mut_place_sink_function(RirFunctionId::from_index(1)));
    program.functions[0]
        .body
        .stmts
        .push(RirStmt::Eval(RirRValue::Call {
            callee: RirCallTarget::Function(RirFunctionId::from_index(1)),
            args: vec![RirCallArg::MutPlace(RirMutPlaceArg::Param {
                local: RirLocalId::from_index(0),
                ty: RirTypeId::from_index(1),
            })],
            ty: RirTypeId::from_index(0),
        }));

    assert_rir_error(program, RirVerifyErrorKind::CallArgMode);
}

#[test]
fn rir_rejects_scoped_place_cell_mut_place_arg_to_native_mut_borrow() {
    let mut program = scoped_place_cell_rir(valid_scoped_place_cell_decl());
    program.externs.push(RirExtern {
        id: RirExternId::from_index(0),
        symbol: RirSymbol::new("native_touch"),
        kind: RirExternKind::Native(rir::RirNativeExtern {
            path: vec!["host".to_string(), "touch".to_string()],
            abi: anvyx_runtime::RustExternAbi {
                params: vec![anvyx_runtime::RustParamAbi::MutBorrow(
                    anvyx_runtime::ExternTypeExpr::Int,
                )],
                ret: anvyx_runtime::RustReturnAbi::Value(anvyx_runtime::ExternTypeExpr::Void),
                fallible: false,
                support: anvyx_runtime::RustAbiSupport::Direct,
            },
        }),
        params: vec![RirExternParam {
            ty: RirTypeId::from_index(1),
            semantic: RirParamSemantic::MutBorrow,
            abi: RirParamAbi::MutBorrow,
        }],
        ret: RirTypeId::from_index(0),
    });
    program.functions[0]
        .body
        .stmts
        .push(RirStmt::Eval(RirRValue::Call {
            callee: RirCallTarget::Extern(RirExternId::from_index(0)),
            args: vec![RirCallArg::MutPlace(RirMutPlaceArg::ScopedPlaceCell {
                cell: RirScopedPlaceCellRef::Owner(RirScopedPlaceCellId::from_index(0)),
                ty: RirTypeId::from_index(1),
            })],
            ty: RirTypeId::from_index(0),
        }));

    assert_rir_error(program, RirVerifyErrorKind::CallArgMode);
}

#[test]
fn rir_rejects_scoped_place_cell_get_payload_type_mismatch() {
    let mut program = scoped_place_cell_rir(valid_scoped_place_cell_decl());
    program.functions[0]
        .body
        .stmts
        .push(RirStmt::Eval(RirRValue::ScopedPlaceCellGet {
            cell: RirScopedPlaceCellRef::Owner(RirScopedPlaceCellId::from_index(0)),
            ty: RirTypeId::from_index(0),
        }));

    assert_rir_type_error(program);
}

#[test]
fn rir_rejects_scoped_place_cell_mut_place_arg_payload_type_mismatch() {
    let mut program = scoped_place_cell_rir(valid_scoped_place_cell_decl());
    program
        .functions
        .push(mut_place_sink_function(RirFunctionId::from_index(1)));
    program.functions[0]
        .body
        .stmts
        .push(RirStmt::Eval(RirRValue::Call {
            callee: RirCallTarget::Function(RirFunctionId::from_index(1)),
            args: vec![RirCallArg::MutPlace(RirMutPlaceArg::ScopedPlaceCell {
                cell: RirScopedPlaceCellRef::Owner(RirScopedPlaceCellId::from_index(0)),
                ty: RirTypeId::from_index(0),
            })],
            ty: RirTypeId::from_index(0),
        }));

    assert_rir_type_error(program);
}

#[test]
fn rir_accepts_nested_scoped_place_cell_capture_ref() {
    rir::verify(&valid_nested_scoped_place_cell_lambda_rir()).expect("RIR verify failed");
}

#[test]
fn rir_rejects_scoped_place_cell_bad_id() {
    assert_rir_error(
        scoped_place_cell_rir_with(|cell| cell.id = RirScopedPlaceCellId::from_index(1)),
        RirVerifyErrorKind::BadId,
    );
}

#[test]
fn rir_rejects_scoped_place_cell_bad_owner() {
    assert_rir_error(
        scoped_place_cell_rir_with(|cell| cell.owner = RirFunctionId::from_index(1)),
        RirVerifyErrorKind::BadId,
    );
}

#[test]
fn rir_rejects_scoped_place_cell_bad_source_local() {
    assert_rir_error(
        scoped_place_cell_rir_with(|cell| cell.source_local = RirLocalId::from_index(1)),
        RirVerifyErrorKind::BadId,
    );
}

#[test]
fn rir_rejects_scoped_place_cell_payload_type_mismatch() {
    assert_rir_type_error(scoped_place_cell_rir_with(|cell| {
        cell.payload_ty = RirTypeId::from_index(0);
    }));
}

#[test]
fn rir_rejects_scoped_place_cell_non_mut_place_source_local() {
    let mut program = scoped_place_cell_rir(valid_scoped_place_cell_decl());
    program.functions[0].params[0].semantic = RirParamSemantic::Value;
    program.functions[0].params[0].abi = RirParamAbi::Value;

    assert_rir_error(program, RirVerifyErrorKind::CallArgMode);
}

#[test]
fn rir_rejects_duplicate_scoped_place_cells_for_source_local() {
    let mut program = scoped_place_cell_rir(valid_scoped_place_cell_decl());
    let mut duplicate = valid_scoped_place_cell_decl();
    duplicate.id = RirScopedPlaceCellId::from_index(1);
    program.scoped_place_cells.push(duplicate);

    assert_rir_error(
        program,
        RirVerifyErrorKind::DuplicateScopedPlaceCell {
            owner: RirFunctionId::from_index(0),
            source_local: RirLocalId::from_index(0),
            first: RirScopedPlaceCellId::from_index(0),
            second: RirScopedPlaceCellId::from_index(1),
        },
    );
}

#[test]
fn rir_accepts_heap_cell_lambda_capture_and_env_field() {
    let program = valid_heap_cell_lambda_rir();
    rir::verify(&program).expect("RIR rejected heap cell lambda");
    let env = &program.lambda_envs[0];

    assert_eq!(program.cells[0].storage, RirCellStorage::Heap);
    assert!(matches!(
        program.lambdas[0].captures[0].kind,
        RirLambdaCaptureKind::HeapCell { cell: RirCellId(0) }
    ));
    assert!(matches!(
        env.fields[0].kind,
        RirLambdaEnvFieldKind::HeapCell { cell: RirCellId(0) }
    ));
}

#[test]
fn policy_tracks_heap_cell_payload_lambda_with_heap_cell_capture() {
    let mut program = valid_heap_cell_lambda_rir();
    program.lambdas[0].escape = RirLambdaEscape::NonEscaping;
    program.lambdas[0].storage = RirLambdaStorage::ScopedCaptures;
    program.lambda_envs.clear();
    program.cells.push(RirCellDecl {
        id: RirCellId::from_index(1),
        owner: RirFunctionId::from_index(0),
        source_local: RirLocalId::from_index(1),
        payload_ty: RirTypeId::from_index(2),
        storage: RirCellStorage::Heap,
        symbol: RirSymbol::new("__cell1"),
    });
    let policy = RustRepPolicy::new(&program);
    let trace = RustTracePlan::build(&program);

    let sig = RirLambdaSigId::from_index(0);
    let lambda_ty = RirTypeId::from_index(2);

    assert!(policy.cell_storage_tracked(&program.cells[1]));
    assert!(policy.lambda_sig_needs_ctx_lifetime(sig));
    assert!(policy.type_cx_dependent(lambda_ty));
    assert!(trace.needs_lambda_sig_trace(sig));
}

#[test]
fn policy_heap_cell_env_field_uses_lambda_cell_handle_type() {
    let program = valid_heap_cell_lambda_rir();
    rir::verify(&program).expect("RIR rejected heap cell lambda");
    let policy = RustRepPolicy::new(&program);

    assert_eq!(
        policy.lambda_env_field_ty(&program.lambda_envs[0].fields[0]),
        "anvyx_runtime::Handle<'cx, anvyx_runtime::LambdaCell<i64>>"
    );
}

#[test]
fn emit_heap_cell_owner_read_write_use_heap_handle() {
    let mut program = valid_heap_cell_lambda_rir();
    program.entry = Some(RirFunctionId::from_index(0));
    let two = RirConstId::from_index(program.consts.len());
    program.consts.push(RirConst {
        id: two,
        ty: RirTypeId::from_index(1),
        value: RirConstValue::Int(2),
    });
    let tmp = RirLocalId::from_index(2);
    program.functions[0].locals.push(RirLocal {
        id: tmp,
        ty: RirTypeId::from_index(1),
        mutable: false,
        symbol: RirSymbol::new("tmp"),
        initialized: false,
        payload_ref: false,
    });
    program.functions[0].body.stmts.extend([
        RirStmt::CellSet {
            cell: RirCellRef::Owner(RirCellId::from_index(0)),
            value: RirRValue::Use(RirOperand::Const(two)),
        },
        RirStmt::Init {
            local: tmp,
            value: RirRValue::CellGetCopy {
                cell: RirCellRef::Owner(RirCellId::from_index(0)),
                ty: RirTypeId::from_index(1),
            },
        },
    ]);
    let source = emit::emit(&rir::verify(&program).expect("RIR verify failed"));
    let text = source.as_str();

    assert!(text.contains("ctx.heap().with(&__cell0, |cell| cell.set(2))?;"));
    assert!(text.contains("let tmp: i64 = ctx.heap().with(&__cell0, |cell| cell.get_copy())?;"));
    let output = run_source(source);
    assert_eq!(output.status, SourceJobStatus::Success, "{}", output.stderr);
}

#[test]
fn emit_heap_cell_init_hoists_ctx_using_value_before_alloc() {
    let mut program = valid_heap_cell_lambda_rir();
    program.entry = Some(RirFunctionId::from_index(0));
    let callee = RirFunctionId::from_index(program.functions.len());
    let one = RirConstId::from_index(0);
    program.functions.push(RirFunction {
        id: callee,
        air_id: None,
        symbol: RirSymbol::new("seed"),
        params: vec![],
        ret: RirReturn {
            ty: RirTypeId::from_index(1),
        },
        locals: vec![],
        body: RirStructuredBlock {
            stmts: vec![],
            term: RirTerm::Return(Some(RirOperand::Const(one))),
        },
    });
    let RirStmt::CellInit { value, .. } = &mut program.functions[0].body.stmts[0] else {
        panic!("missing cell init");
    };
    *value = RirRValue::Call {
        callee: RirCallTarget::Function(callee),
        args: vec![],
        ty: RirTypeId::from_index(1),
    };
    let source = emit::emit(&rir::verify(&program).expect("RIR verify failed"));
    let text = source.as_str();

    assert!(text.contains("let value = seed(ctx);"));
    assert!(
        text.contains("ctx.heap().alloc(heap_type, anvyx_runtime::LambdaCell::<i64>::new(value))")
    );
    let output = run_source(source);
    assert_eq!(output.status, SourceJobStatus::Success, "{}", output.stderr);
}

#[test]
fn emit_heap_cell_owner_allocation_compiles() {
    let mut program = valid_heap_cell_lambda_rir();
    program.entry = Some(RirFunctionId::from_index(0));
    let source = emit::emit(&rir::verify(&program).expect("RIR verify failed"));
    let output = run_source(source);

    assert_eq!(output.status, SourceJobStatus::Success, "{}", output.stderr);
}

#[test]
fn emit_heap_cell_alloc_env_and_access_use_handles() {
    let program = valid_heap_cell_lambda_rir();
    let verified = rir::verify(&program).expect("RIR rejected heap cell lambda");
    let source = emit::emit(&verified).into_string();

    assert!(
        source
            .contains("lambda_cell0: heap.register_untracked::<anvyx_runtime::LambdaCell<i64>>()")
    );
    assert!(
        source
            .contains("let __cell0: anvyx_runtime::Handle<'cx, anvyx_runtime::LambdaCell<i64>> =")
    );
    assert!(source.contains(
        "= { let value = 1; let heap_type = ctx._types.lambda_cell0; ctx.heap().alloc(heap_type, anvyx_runtime::LambdaCell::<i64>::new(value)) };"
    ));
    assert!(source.contains("c0: anvyx_runtime::Handle<'cx, anvyx_runtime::LambdaCell<i64>>,"));
    assert!(source.contains("c0: __cell0.clone()"));
    assert!(source.contains("let c0 = ctx.heap().with(env, |env| env.c0.clone());"));
    assert!(source.contains("target(ctx, c0)"));
    assert!(source.contains("ctx.heap().with(&cell, |cell| cell.get_copy())?"));
    assert!(!source.contains("StackLambdaCell"));
    assert!(!source.contains("RefCell"));
    assert!(!source.contains("Rc<"));
    assert!(!source.contains("Box<dyn Fn"));
}

#[test]
fn rir_accepts_non_escaping_heap_cell_capture() {
    let mut program = valid_heap_cell_lambda_rir();
    program.lambdas[0].escape = RirLambdaEscape::NonEscaping;
    program.lambdas[0].storage = RirLambdaStorage::ScopedCaptures;
    program.lambda_envs.clear();

    rir::verify(&program).expect("RIR rejected non-escaping heap cell capture");
}

#[test]
fn rir_rejects_heap_cell_capture_with_stack_storage() {
    let mut program = valid_heap_cell_lambda_rir();
    program.cells[0].storage = RirCellStorage::StackScoped;

    assert_rir_error(program, RirVerifyErrorKind::UnsupportedLambdaCapture);
}

#[test]
fn rir_rejects_heap_cell_capture_payload_type_mismatch() {
    let mut program = valid_heap_cell_lambda_rir();
    program.lambdas[0].captures[0].ty = RirTypeId::from_index(0);

    assert_rir_type_error(program);
}

#[test]
fn rir_rejects_heap_cell_env_field_wrong_kind() {
    let mut program = valid_heap_cell_lambda_rir();
    program.lambda_envs[0].fields[0].kind = RirLambdaEnvFieldKind::Value;

    assert_rir_error(program, RirVerifyErrorKind::UnsupportedLambdaCapture);
}

#[test]
fn rir_rejects_heap_cell_env_field_mismatched_cell() {
    let mut program = valid_heap_cell_lambda_rir();
    let mut duplicate = valid_heap_cell_decl();
    duplicate.id = RirCellId::from_index(1);
    duplicate.symbol = RirSymbol::new("__cell1");
    program.cells.push(duplicate);
    program.lambda_envs[0].fields[0].kind = RirLambdaEnvFieldKind::HeapCell {
        cell: RirCellId::from_index(1),
    };

    assert_rir_error(program, RirVerifyErrorKind::UnsupportedLambdaCapture);
}

#[test]
fn rir_rejects_duplicate_heap_cells_for_source_local() {
    let mut program = stack_cell_rir(valid_heap_cell_decl());
    let mut duplicate = valid_heap_cell_decl();
    duplicate.id = RirCellId::from_index(1);
    duplicate.symbol = RirSymbol::new("__cell1");
    program.cells.push(duplicate);

    assert_rir_error(
        program,
        RirVerifyErrorKind::DuplicateCell {
            owner: RirFunctionId::from_index(0),
            source_local: RirLocalId::from_index(0),
            first: RirCellId::from_index(0),
            second: RirCellId::from_index(1),
        },
    );
}

#[test]
fn rir_accepts_stack_cell_read_compute_write() {
    rir::verify(&read_compute_write_cell_rir(valid_stack_cell_decl()))
        .expect("RIR rejected stack cell read-compute-write");
}

#[test]
fn rir_accepts_heap_cell_read_compute_write() {
    rir::verify(&read_compute_write_cell_rir(valid_heap_cell_decl()))
        .expect("RIR rejected heap cell read-compute-write");
}

#[test]
fn rir_rejects_cell_set_value_call() {
    assert_rir_error(
        cell_set_call_rir(valid_stack_cell_decl()),
        RirVerifyErrorKind::UnsupportedLambdaCell,
    );
}

#[test]
fn rir_rejects_cell_set_value_binary() {
    assert_rir_error(
        cell_set_binary_rir(valid_stack_cell_decl()),
        RirVerifyErrorKind::UnsupportedLambdaCell,
    );
}

#[test]
fn rir_rejects_heap_cell_set_value_call() {
    assert_rir_error(
        cell_set_call_rir(valid_heap_cell_decl()),
        RirVerifyErrorKind::UnsupportedLambdaCell,
    );
}

#[test]
fn rir_rejects_heap_cell_set_value_mut_place_param() {
    assert_rir_error(
        heap_cell_set_mut_place_param_rir(),
        RirVerifyErrorKind::UnsupportedLambdaCell,
    );
}

#[test]
fn rir_accepts_heap_cell_mut_place_arg() {
    rir::verify(&cell_mut_place_call_rir(
        valid_heap_cell_decl(),
        RirMutPlaceArg::HeapCell {
            cell: RirCellRef::Owner(RirCellId::from_index(0)),
            ty: RirTypeId::from_index(1),
        },
    ))
    .expect("RIR rejected heap cell mut-place arg");
}

#[test]
fn rir_rejects_heap_cell_as_stack_mut_place_arg() {
    assert_rir_error(
        cell_mut_place_call_rir(
            valid_heap_cell_decl(),
            RirMutPlaceArg::StackCell {
                cell: RirCellRef::Owner(RirCellId::from_index(0)),
                ty: RirTypeId::from_index(1),
            },
        ),
        RirVerifyErrorKind::CallArgMode,
    );
}

#[test]
fn rir_rejects_stack_cell_as_heap_mut_place_arg() {
    assert_rir_error(
        cell_mut_place_call_rir(
            valid_stack_cell_decl(),
            RirMutPlaceArg::HeapCell {
                cell: RirCellRef::Owner(RirCellId::from_index(0)),
                ty: RirTypeId::from_index(1),
            },
        ),
        RirVerifyErrorKind::CallArgMode,
    );
}

#[test]
fn rir_rejects_heap_cell_mut_place_arg_payload_mismatch() {
    assert_rir_error(
        cell_mut_place_call_rir(
            valid_heap_cell_decl(),
            RirMutPlaceArg::HeapCell {
                cell: RirCellRef::Owner(RirCellId::from_index(0)),
                ty: RirTypeId::from_index(0),
            },
        ),
        RirVerifyErrorKind::TypeMismatch {
            expected: RirTypeId::from_index(1),
            found: RirTypeId::from_index(0),
        },
    );
}

#[test]
fn rir_rejects_uninitialized_heap_cell_mut_place_arg() {
    let mut program = cell_mut_place_call_rir(
        valid_heap_cell_decl(),
        RirMutPlaceArg::HeapCell {
            cell: RirCellRef::Owner(RirCellId::from_index(0)),
            ty: RirTypeId::from_index(1),
        },
    );
    program.functions[0].body.stmts.remove(0);

    assert_rir_error(
        program,
        RirVerifyErrorKind::UninitializedCell(RirCellId::from_index(0)),
    );
}

#[test]
fn rir_accepts_capture_heap_cell_mut_place_arg() {
    let mut program = valid_heap_cell_lambda_rir();
    program
        .functions
        .push(mut_place_sink_function(RirFunctionId::from_index(2)));
    program.functions[1].body.stmts = vec![RirStmt::Eval(RirRValue::Call {
        callee: RirCallTarget::Function(RirFunctionId::from_index(2)),
        args: vec![RirCallArg::MutPlace(RirMutPlaceArg::HeapCell {
            cell: RirCellRef::Capture {
                cell: RirCellId::from_index(0),
                local: RirLocalId::from_index(0),
            },
            ty: RirTypeId::from_index(1),
        })],
        ty: RirTypeId::from_index(0),
    })];

    rir::verify(&program).expect("RIR rejected capture heap cell mut-place arg");
}

#[test]
fn rir_rejects_heap_cell_capture_bad_id() {
    let mut program = valid_heap_cell_lambda_rir();
    let owner = &mut program.functions[0];
    let RirStmt::Init {
        value: RirRValue::Lambda { captures, .. },
        ..
    } = &mut owner.body.stmts[1]
    else {
        panic!("missing lambda init");
    };
    captures[0] = RirLambdaCaptureArg::HeapCell {
        cell: RirCellRef::Owner(RirCellId::from_index(99)),
    };

    assert_rir_error(program, RirVerifyErrorKind::BadId);
}

#[test]
fn rir_rejects_stack_and_scoped_place_cell_symbol_collision() {
    let mut program = scoped_place_cell_rir(valid_scoped_place_cell_decl());
    program.cells.push(RirCellDecl {
        id: RirCellId::from_index(0),
        owner: RirFunctionId::from_index(0),
        source_local: RirLocalId::from_index(0),
        payload_ty: RirTypeId::from_index(1),
        storage: RirCellStorage::StackScoped,
        symbol: RirSymbol::new("__scoped0"),
    });

    assert_rir_error(program, RirVerifyErrorKind::DuplicateSymbol);
}

#[test]
fn rir_rejects_escaping_scoped_place_cell_lambda_capture() {
    let mut program = valid_scoped_place_cell_lambda_rir();
    program.lambdas[0].escape = RirLambdaEscape::Escaping;

    assert_rir_error(program, RirVerifyErrorKind::UnsupportedLambdaCapture);
}

#[test]
fn rir_rejects_scoped_place_cell_hidden_abi_in_source_signature() {
    let mut program = scoped_place_cell_rir(valid_scoped_place_cell_decl());
    program.functions[0].params[0].semantic = RirParamSemantic::ScopedPlaceCell;
    program.functions[0].params[0].abi = RirParamAbi::ScopedPlaceCell;

    assert_rir_error(program, RirVerifyErrorKind::UnsupportedAbi);
}

#[test]
fn rir_rejects_scoped_place_cell_wrong_capture_kind() {
    let mut program = valid_scoped_place_cell_lambda_rir();
    program.lambdas[0].captures[0].kind = RirLambdaCaptureKind::Param;

    assert_rir_error(program, RirVerifyErrorKind::UnsupportedLambdaCapture);
}

#[test]
fn rir_rejects_scoped_place_cell_invalid_capture_ref() {
    let mut program = valid_scoped_place_cell_lambda_rir();
    let owner = &mut program.functions[0];
    let RirStmt::Init {
        value: RirRValue::Lambda { captures, .. },
        ..
    } = &mut owner.body.stmts[0]
    else {
        panic!("missing lambda init");
    };
    captures[0] = RirLambdaCaptureArg::ScopedPlaceCell {
        cell: RirScopedPlaceCellRef::Capture {
            cell: RirScopedPlaceCellId::from_index(0),
            local: RirLocalId::from_index(99),
        },
    };

    assert_rir_error(program, RirVerifyErrorKind::UnsupportedLambdaCapture);
}

#[test]
fn rir_rejects_nested_scoped_place_cell_mismatched_capture_ref() {
    let mut program = valid_nested_scoped_place_cell_lambda_rir();
    program.scoped_place_cells.push(RirScopedPlaceCellDecl {
        id: RirScopedPlaceCellId::from_index(1),
        owner: RirFunctionId::from_index(0),
        source_local: RirLocalId::from_index(0),
        payload_ty: RirTypeId::from_index(1),
        symbol: RirSymbol::new("__scoped1"),
    });
    let RirStmt::Init {
        value: RirRValue::Lambda { captures, .. },
        ..
    } = &mut program.functions[1].body.stmts[0]
    else {
        panic!("missing nested lambda init");
    };
    captures[0] = RirLambdaCaptureArg::ScopedPlaceCell {
        cell: RirScopedPlaceCellRef::Capture {
            cell: RirScopedPlaceCellId::from_index(1),
            local: RirLocalId::from_index(0),
        },
    };

    assert_rir_error(program, RirVerifyErrorKind::UnsupportedLambdaCapture);
}

#[test]
fn emit_stack_cell_lambda_values_are_copyable_shared_refs() {
    let mut program = valid_stack_cell_lambda_rir();
    program.entry = Some(RirFunctionId::from_index(0));
    let lambda_ty = RirTypeId::from_index(2);
    let f = RirLocalId::from_index(1);
    let g = RirLocalId::from_index(2);
    program.functions[0].locals.push(RirLocal {
        id: g,
        ty: lambda_ty,
        mutable: false,
        symbol: RirSymbol::new("g"),
        initialized: false,
        payload_ref: false,
    });
    program.functions[0].body.stmts.extend([
        RirStmt::Init {
            local: g,
            value: RirRValue::Use(RirOperand::Place(RirPlace {
                local: f,
                projections: vec![],
                ty: lambda_ty,
            })),
        },
        RirStmt::Eval(RirRValue::Call {
            callee: RirCallTarget::LambdaValue {
                callee: RirOperand::Place(RirPlace {
                    local: f,
                    projections: vec![],
                    ty: lambda_ty,
                }),
                sig: RirLambdaSigId::from_index(0),
            },
            args: vec![],
            ty: RirTypeId::from_index(0),
        }),
        RirStmt::Eval(RirRValue::Call {
            callee: RirCallTarget::LambdaValue {
                callee: RirOperand::Place(RirPlace {
                    local: g,
                    projections: vec![],
                    ty: lambda_ty,
                }),
                sig: RirLambdaSigId::from_index(0),
            },
            args: vec![],
            ty: RirTypeId::from_index(0),
        }),
    ]);

    let source = emit::emit(&rir::verify(&program).expect("RIR verify failed"));
    let text = source.as_str();
    assert!(text.contains("#[derive(Clone, Copy)]\nenum LambdaSig0<'env>"));
    assert!(text.contains("L0 { c0: &'env anvyx_runtime::StackLambdaCell<i64> }"));
    assert!(text.contains("fn call<'cx, 'rt>(&self, ctx: &mut AnvCtx<'cx, 'rt>)"));
    assert!(text.contains("c0: &__cell0"));
    assert!(text.contains("target(ctx, *c0)"));
    assert!(!text.contains("&'env mut"));
    assert!(!text.contains("fn call<'cx, 'rt>(&mut self"));
    assert!(!text.contains("&mut source"));
    assert!(!text.contains("let mut f"));
    assert!(!text.contains("RefCell"));
    assert!(!text.contains("Rc<"));

    let output = run_source(source);
    assert_eq!(output.status, SourceJobStatus::Success, "{}", output.stderr);
}

#[test]
fn rir_rejects_ordinary_place_use_of_stack_cell_capture_param() {
    let mut program = valid_stack_cell_lambda_rir();
    program.functions[1].body.stmts =
        vec![RirStmt::Eval(RirRValue::Use(RirOperand::Place(RirPlace {
            local: RirLocalId::from_index(0),
            projections: vec![],
            ty: RirTypeId::from_index(1),
        })))];

    assert_rir_error(program, RirVerifyErrorKind::UnsupportedLambdaCapture);
}

#[test]
fn rir_rejects_ordinary_place_use_of_scoped_place_cell_capture_param() {
    let mut program = valid_scoped_place_cell_lambda_rir();
    program.functions[1].body.stmts =
        vec![RirStmt::Eval(RirRValue::Use(RirOperand::Place(RirPlace {
            local: RirLocalId::from_index(0),
            projections: vec![],
            ty: RirTypeId::from_index(1),
        })))];

    assert_rir_error(program, RirVerifyErrorKind::UnsupportedLambdaCapture);
}

#[test]
fn rir_rejects_mut_borrow_call_arg_use_of_scoped_place_cell_capture_param() {
    let mut program = valid_scoped_place_cell_lambda_rir();
    program
        .functions
        .push(mut_borrow_sink_function(RirFunctionId::from_index(2)));
    program.functions[1].body.stmts = vec![RirStmt::Eval(RirRValue::Call {
        callee: RirCallTarget::Function(RirFunctionId::from_index(2)),
        args: vec![RirCallArg::MutBorrow(RirPlace {
            local: RirLocalId::from_index(0),
            projections: vec![],
            ty: RirTypeId::from_index(1),
        })],
        ty: RirTypeId::from_index(0),
    })];

    assert_rir_error(program, RirVerifyErrorKind::UnsupportedLambdaCapture);
}

#[test]
fn rir_rejects_stack_cell_and_mut_borrow_variants_sharing_signature() {
    let mut program = valid_stack_cell_lambda_rir();
    let mut_function = RirFunctionId::from_index(2);
    program.lambdas.push(RirLambda {
        id: RirLambdaId::from_index(1),
        source: RirLambdaSource::Lambda(air::LambdaId::from_index(1)),
        function: mut_function,
        sig: RirLambdaSigId::from_index(0),
        escape: RirLambdaEscape::NonEscaping,
        storage: RirLambdaStorage::ScopedCaptures,
        captures: vec![RirLambdaCapture {
            ty: RirTypeId::from_index(1),
            semantic: RirParamSemantic::MutBorrow,
            abi: RirParamAbi::MutBorrow,
            kind: RirLambdaCaptureKind::Param,
        }],
    });
    program.functions.push(RirFunction {
        id: mut_function,
        air_id: None,
        symbol: RirSymbol::new("mut_target"),
        params: vec![RirParam {
            local: RirLocalId::from_index(0),
            ty: RirTypeId::from_index(1),
            semantic: RirParamSemantic::MutBorrow,
            abi: RirParamAbi::MutBorrow,
            escape: RirParamEscape::NonEscaping,
        }],
        ret: RirReturn {
            ty: RirTypeId::from_index(0),
        },
        locals: vec![RirLocal {
            id: RirLocalId::from_index(0),
            ty: RirTypeId::from_index(1),
            mutable: true,
            symbol: RirSymbol::new("capture"),
            initialized: true,
            payload_ref: false,
        }],
        body: RirStructuredBlock::default(),
    });

    assert_rir_error(program, RirVerifyErrorKind::UnsupportedLambdaCapture);
}

#[test]
fn rir_rejects_escaping_stack_cell_lambda_capture() {
    assert_rir_error(
        stack_cell_lambda_rir(
            RirLambdaEscape::Escaping,
            RirCellId::from_index(0),
            RirTypeId::from_index(1),
            RirParamSemantic::StackCell,
            RirParamAbi::StackCell,
            valid_stack_cell_arg(),
        ),
        RirVerifyErrorKind::UnsupportedLambdaCapture,
    );
}

#[test]
fn rir_rejects_stack_cell_lambda_capture_wrong_cell() {
    let mut program = stack_cell_lambda_rir(
        RirLambdaEscape::NonEscaping,
        RirCellId::from_index(0),
        RirTypeId::from_index(1),
        RirParamSemantic::StackCell,
        RirParamAbi::StackCell,
        RirLambdaCaptureArg::StackCell {
            cell: RirCellRef::Owner(RirCellId::from_index(1)),
        },
    );
    program.cells.push(RirCellDecl {
        id: RirCellId::from_index(1),
        owner: RirFunctionId::from_index(0),
        source_local: RirLocalId::from_index(2),
        payload_ty: RirTypeId::from_index(1),
        storage: RirCellStorage::StackScoped,
        symbol: RirSymbol::new("__cell0"),
    });
    program.functions[0].locals.push(RirLocal {
        id: RirLocalId::from_index(2),
        ty: RirTypeId::from_index(1),
        mutable: true,
        symbol: RirSymbol::new("other"),
        initialized: true,
        payload_ref: false,
    });

    assert_rir_error(program, RirVerifyErrorKind::UnsupportedLambdaCapture);
}

#[test]
fn rir_rejects_stack_cell_lambda_capture_wrong_payload_type() {
    assert_rir_type_error(stack_cell_lambda_rir(
        RirLambdaEscape::NonEscaping,
        RirCellId::from_index(0),
        RirTypeId::from_index(0),
        RirParamSemantic::StackCell,
        RirParamAbi::StackCell,
        valid_stack_cell_arg(),
    ));
}

#[test]
fn rir_rejects_stack_cell_lambda_capture_wrong_hidden_param() {
    assert_rir_error(
        stack_cell_lambda_rir(
            RirLambdaEscape::NonEscaping,
            RirCellId::from_index(0),
            RirTypeId::from_index(1),
            RirParamSemantic::Value,
            RirParamAbi::Value,
            valid_stack_cell_arg(),
        ),
        RirVerifyErrorKind::CallArgMode,
    );
}

#[test]
fn rir_rejects_stack_cell_lambda_capture_before_cell_init() {
    let mut program = valid_stack_cell_lambda_rir();
    program.functions[0].body.stmts.remove(0);

    assert_rir_error(
        program,
        RirVerifyErrorKind::UninitializedCell(RirCellId::from_index(0)),
    );
}

#[test]
fn rir_rejects_stack_cell_source_visible_lambda_sig_param() {
    let mut program = valid_stack_cell_lambda_rir();
    program.lambda_sigs[0].params.push(RirLambdaParam {
        ty: RirTypeId::from_index(1),
        semantic: RirParamSemantic::StackCell,
        abi: RirParamAbi::StackCell,
        escape: RirParamEscape::NonEscaping,
    });

    assert_rir_error(program, RirVerifyErrorKind::UnsupportedAbi);
}

#[test]
fn rir_rejects_stack_cell_ordinary_function_param() {
    let mut program = stack_cell_rir(valid_stack_cell_decl());
    program.functions[0].params.push(RirParam {
        local: RirLocalId::from_index(0),
        ty: RirTypeId::from_index(1),
        semantic: RirParamSemantic::StackCell,
        abi: RirParamAbi::StackCell,
        escape: RirParamEscape::NonEscaping,
    });

    assert_rir_error(program, RirVerifyErrorKind::UnsupportedAbi);
}

#[test]
fn rir_rejects_inconsistent_stack_cell_lambda_descriptors_for_shared_body() {
    let mut program = valid_stack_cell_lambda_rir();
    program.cells.push(RirCellDecl {
        id: RirCellId::from_index(1),
        owner: RirFunctionId::from_index(0),
        source_local: RirLocalId::from_index(2),
        payload_ty: RirTypeId::from_index(1),
        storage: RirCellStorage::StackScoped,
        symbol: RirSymbol::new("__cell0"),
    });
    program.functions[0].locals.push(RirLocal {
        id: RirLocalId::from_index(2),
        ty: RirTypeId::from_index(1),
        mutable: true,
        symbol: RirSymbol::new("other"),
        initialized: true,
        payload_ref: false,
    });
    let mut lambda = program.lambdas[0].clone();
    lambda.id = RirLambdaId::from_index(1);
    lambda.captures[0].kind = RirLambdaCaptureKind::StackCell {
        cell: RirCellId::from_index(1),
    };
    program.lambdas.push(lambda);

    assert_rir_error(program, RirVerifyErrorKind::UnsupportedLambdaCapture);
}

#[test]
fn rir_verifies_lambda_signature_ids() {
    let void = RirTypeId::from_index(0);
    let lambda_ty = RirTypeId::from_index(1);
    let program = RirProgram {
        types: vec![
            RirType::Void,
            RirType::Lambda(RirLambdaSigId::from_index(0)),
        ],
        lambda_sigs: vec![RirLambdaSig {
            id: RirLambdaSigId::from_index(0),
            params: vec![RirLambdaParam {
                ty: lambda_ty,
                semantic: RirParamSemantic::Value,
                abi: RirParamAbi::Value,
                escape: RirParamEscape::Escaping,
            }],
            ret: void,
        }],
        ..RirProgram::default()
    };

    rir::verify(&program).expect("RIR verify failed");
}

#[test]
fn rir_rejects_lambda_container_types() {
    let void = RirTypeId::from_index(0);
    let lambda_ty = RirTypeId::from_index(1);
    let array_ty = RirTypeId::from_index(2);
    let sig = RirLambdaSigId::from_index(0);
    let program = RirProgram {
        types: vec![
            RirType::Void,
            RirType::Lambda(sig),
            RirType::Array {
                elem: lambda_ty,
                len: 1,
            },
        ],
        lambda_sigs: vec![RirLambdaSig {
            id: sig,
            params: vec![],
            ret: void,
        }],
        ..RirProgram::default()
    };

    let errors = rir::verify(&program).expect_err("verified lambda array type");
    assert!(errors.iter().any(|error| {
        error.site == RirVerifySite::Type(array_ty)
            && error.kind == RirVerifyErrorKind::UnsupportedRValueType
    }));
}

#[test]
fn rir_rejects_non_escaping_lambda_arg_to_escaping_param() {
    let void = RirTypeId::from_index(0);
    let lambda_ty = RirTypeId::from_index(1);
    let sig = RirLambdaSigId::from_index(0);
    let lambda = RirLambdaId::from_index(0);
    let target = RirFunctionId::from_index(0);
    let callee = RirFunctionId::from_index(1);
    let caller = RirFunctionId::from_index(2);
    let f = RirLocalId::from_index(0);
    let program = RirProgram {
        types: vec![RirType::Void, RirType::Lambda(sig)],
        lambda_sigs: vec![RirLambdaSig {
            id: sig,
            params: vec![],
            ret: void,
        }],
        lambdas: vec![RirLambda {
            id: lambda,
            source: RirLambdaSource::Function(FunctionId::from_index(0)),
            function: target,
            sig,
            escape: RirLambdaEscape::NonEscaping,
            storage: RirLambdaStorage::ZeroEnv,
            captures: vec![],
        }],
        functions: vec![
            RirFunction {
                id: target,
                air_id: None,
                symbol: RirSymbol::new("target"),
                params: vec![],
                ret: RirReturn { ty: void },
                locals: vec![],
                body: RirStructuredBlock::default(),
            },
            RirFunction {
                id: callee,
                air_id: None,
                symbol: RirSymbol::new("callee"),
                params: vec![RirParam {
                    local: f,
                    ty: lambda_ty,
                    semantic: RirParamSemantic::Value,
                    abi: RirParamAbi::Value,
                    escape: RirParamEscape::Escaping,
                }],
                ret: RirReturn { ty: void },
                locals: vec![RirLocal {
                    id: f,
                    ty: lambda_ty,
                    mutable: false,
                    symbol: RirSymbol::new("f"),
                    initialized: true,
                    payload_ref: false,
                }],
                body: RirStructuredBlock::default(),
            },
            RirFunction {
                id: caller,
                air_id: None,
                symbol: RirSymbol::new("caller"),
                params: vec![],
                ret: RirReturn { ty: void },
                locals: vec![RirLocal {
                    id: f,
                    ty: lambda_ty,
                    mutable: false,
                    symbol: RirSymbol::new("f"),
                    initialized: false,
                    payload_ref: false,
                }],
                body: RirStructuredBlock {
                    stmts: vec![
                        RirStmt::Init {
                            local: f,
                            value: RirRValue::Lambda {
                                lambda,
                                captures: vec![],
                                ty: lambda_ty,
                            },
                        },
                        RirStmt::Eval(RirRValue::Call {
                            callee: RirCallTarget::Function(callee),
                            args: vec![RirCallArg::Value(RirOperand::Place(RirPlace {
                                local: f,
                                projections: vec![],
                                ty: lambda_ty,
                            }))],
                            ty: void,
                        }),
                    ],
                    term: RirTerm::Return(None),
                },
            },
        ],
        ..RirProgram::default()
    };

    let errors = rir::verify(&program).expect_err("verified non-escaping lambda escape");
    assert!(
        errors
            .iter()
            .any(|error| error.kind == RirVerifyErrorKind::CallArgEscape)
    );
}

#[test]
fn rir_accepts_mut_place_local_call_arg() {
    let int = RirTypeId::from_index(1);
    let local = RirLocalId::from_index(0);
    let program = mut_place_call_rir(
        vec![rir_local(local, int, true, "n")],
        vec![],
        RirCallArg::MutPlace(RirMutPlaceArg::Local(rir_place(local, int))),
    );

    rir::verify(&program).expect("RIR verify failed");
}

#[test]
fn rir_rejects_mut_borrow_for_mut_place_param() {
    let int = RirTypeId::from_index(1);
    let local = RirLocalId::from_index(0);
    let program = mut_place_call_rir(
        vec![rir_local(local, int, true, "n")],
        vec![],
        RirCallArg::MutBorrow(rir_place(local, int)),
    );

    assert_rir_error(program, RirVerifyErrorKind::CallArgMode);
}

#[test]
fn rir_rejects_mut_place_local_arg_from_mut_place_param() {
    let int = RirTypeId::from_index(1);
    let local = RirLocalId::from_index(0);
    let program = mut_place_call_rir(
        vec![rir_local(local, int, true, "x")],
        vec![rir_param(
            local,
            int,
            RirParamSemantic::MutPlace,
            RirParamAbi::MutPlace,
        )],
        RirCallArg::MutPlace(RirMutPlaceArg::Local(rir_place(local, int))),
    );

    assert_rir_error(program, RirVerifyErrorKind::CallArgMode);
}

#[test]
fn rir_rejects_mut_place_param_as_mut_borrow_arg() {
    let void = RirTypeId::from_index(0);
    let int = RirTypeId::from_index(1);
    let local = RirLocalId::from_index(0);
    let callee = RirFunctionId::from_index(0);
    let caller = RirFunctionId::from_index(1);
    let program = RirProgram {
        types: vec![RirType::Void, RirType::Int],
        functions: vec![
            rir_function(
                callee,
                void,
                vec![rir_param(
                    local,
                    int,
                    RirParamSemantic::MutBorrow,
                    RirParamAbi::MutBorrow,
                )],
                vec![rir_local(local, int, true, "x")],
                vec![],
            ),
            rir_function(
                caller,
                void,
                vec![rir_param(
                    local,
                    int,
                    RirParamSemantic::MutPlace,
                    RirParamAbi::MutPlace,
                )],
                vec![rir_local(local, int, true, "x")],
                vec![RirStmt::Eval(RirRValue::Call {
                    callee: RirCallTarget::Function(callee),
                    args: vec![RirCallArg::MutBorrow(rir_place(local, int))],
                    ty: void,
                })],
            ),
        ],
        ..RirProgram::default()
    };

    assert_rir_error(program, RirVerifyErrorKind::CallArgMode);
}

#[test]
fn rir_rejects_projected_mut_place_param() {
    let void = RirTypeId::from_index(0);
    let int = RirTypeId::from_index(1);
    let tuple_ty = RirTypeId::from_index(2);
    let tuple = RirTupleId::from_index(0);
    let field = RirFieldId::from_index(0);
    let local = RirLocalId::from_index(0);
    let mut place = rir_place(local, int);
    place.projections.push(RirProjection::TupleField(field));
    let program = RirProgram {
        types: vec![RirType::Void, RirType::Int, RirType::Tuple(tuple)],
        tuples: vec![RirTuple {
            id: tuple,
            symbol: RirSymbol::new("Pair"),
            display: RirSymbol::new("Pair"),
            copyable: true,
            fields: vec![RirField {
                id: field,
                symbol: RirSymbol::new("_0"),
                ty: int,
            }],
        }],
        functions: vec![rir_function(
            RirFunctionId::from_index(0),
            void,
            vec![rir_param(
                local,
                tuple_ty,
                RirParamSemantic::MutPlace,
                RirParamAbi::MutPlace,
            )],
            vec![rir_local(local, tuple_ty, true, "x")],
            vec![RirStmt::Eval(RirRValue::Use(RirOperand::Place(place)))],
        )],
        ..RirProgram::default()
    };

    assert_rir_error(program, RirVerifyErrorKind::UnsupportedRValueType);
}

#[test]
fn rir_rejects_mut_place_operand_inside_short_region() {
    let void = RirTypeId::from_index(0);
    let int = RirTypeId::from_index(1);
    let list = RirTypeId::from_index(2);
    let xs = RirLocalId::from_index(0);
    let value = RirLocalId::from_index(1);
    let program = RirProgram {
        types: vec![RirType::Void, RirType::Int, RirType::List(int)],
        functions: vec![rir_function(
            RirFunctionId::from_index(0),
            void,
            vec![
                rir_param(xs, list, RirParamSemantic::MutPlace, RirParamAbi::MutPlace),
                rir_param(
                    value,
                    int,
                    RirParamSemantic::MutPlace,
                    RirParamAbi::MutPlace,
                ),
            ],
            vec![
                rir_local(xs, list, true, "xs"),
                rir_local(value, int, true, "value"),
            ],
            vec![RirStmt::Eval(RirRValue::ListPush {
                list: rir_place(xs, list),
                value: RirOperand::Place(rir_place(value, int)),
            })],
        )],
        ..RirProgram::default()
    };

    assert_rir_error(program, RirVerifyErrorKind::UnsupportedRValueType);
}

#[test]
fn rir_rejects_lambda_descriptor_signature_mismatch() {
    let void = RirTypeId::from_index(0);
    let int = RirTypeId::from_index(1);
    let sig = RirLambdaSigId::from_index(0);
    let function = RirFunctionId::from_index(0);
    let program = RirProgram {
        types: vec![RirType::Void, RirType::Int],
        lambda_sigs: vec![RirLambdaSig {
            id: sig,
            params: vec![],
            ret: int,
        }],
        lambdas: vec![RirLambda {
            id: RirLambdaId::from_index(0),
            source: RirLambdaSource::Function(FunctionId::from_index(0)),
            function,
            sig,
            escape: RirLambdaEscape::Escaping,
            storage: RirLambdaStorage::ZeroEnv,
            captures: vec![],
        }],
        functions: vec![RirFunction {
            id: function,
            air_id: None,
            symbol: RirSymbol::new("target"),
            params: vec![],
            ret: RirReturn { ty: void },
            locals: vec![],
            body: RirStructuredBlock::default(),
        }],
        ..RirProgram::default()
    };

    let errors = rir::verify(&program).expect_err("verified mismatched lambda descriptor");
    assert!(errors.iter().any(|error| {
        error.kind
            == RirVerifyErrorKind::TypeMismatch {
                expected: int,
                found: void,
            }
    }));
}

#[test]
fn rir_accepts_scoped_capture_lambda_descriptor_and_value() {
    let int = RirTypeId::from_index(1);
    let program = scoped_capture_rir(
        vec![RirType::Void, RirType::Int],
        vec![],
        RirLambdaCapture {
            ty: int,
            semantic: RirParamSemantic::Value,
            abi: RirParamAbi::Value,
            kind: RirLambdaCaptureKind::Param,
        },
        int,
        false,
        RirLambdaCaptureArg::Readonly {
            value: RirOperand::Place(RirPlace {
                local: RirLocalId::from_index(0),
                projections: vec![],
                ty: int,
            }),
        },
    );

    rir::verify(&program).expect("RIR verify failed");
}

#[test]
fn rir_rejects_shared_readonly_capture_from_const() {
    let int = RirTypeId::from_index(1);
    let mut program = scoped_capture_rir(
        vec![RirType::Void, RirType::Int],
        vec![],
        RirLambdaCapture {
            ty: int,
            semantic: RirParamSemantic::SharedBorrow,
            abi: RirParamAbi::SharedBorrow,
            kind: RirLambdaCaptureKind::Param,
        },
        int,
        false,
        RirLambdaCaptureArg::Readonly {
            value: RirOperand::Const(RirConstId::from_index(0)),
        },
    );
    program.consts.push(RirConst {
        id: RirConstId::from_index(0),
        ty: int,
        value: RirConstValue::Int(1),
    });

    assert_rir_error(program, RirVerifyErrorKind::CallArgMode);
}

#[test]
fn rir_rejects_noncopy_value_capture() {
    let string = RirTypeId::from_index(1);
    let program = scoped_capture_rir(
        vec![RirType::Void, RirType::String],
        vec![],
        RirLambdaCapture {
            ty: string,
            semantic: RirParamSemantic::Value,
            abi: RirParamAbi::Value,
            kind: RirLambdaCaptureKind::Param,
        },
        string,
        false,
        RirLambdaCaptureArg::Readonly {
            value: RirOperand::Place(RirPlace {
                local: RirLocalId::from_index(0),
                projections: vec![],
                ty: string,
            }),
        },
    );

    assert_rir_error(program, RirVerifyErrorKind::NonCopyValueRequired);
}

#[test]
fn rir_rejects_projected_scoped_capture() {
    let int = RirTypeId::from_index(1);
    let tuple_ty = RirTypeId::from_index(2);
    let tuple = RirTupleId::from_index(0);
    let field = RirFieldId::from_index(0);
    let program = scoped_capture_rir(
        vec![RirType::Void, RirType::Int, RirType::Tuple(tuple)],
        vec![RirTuple {
            id: tuple,
            symbol: RirSymbol::new("Pair"),
            display: RirSymbol::new("Pair"),
            copyable: true,
            fields: vec![RirField {
                id: field,
                symbol: RirSymbol::new("_0"),
                ty: int,
            }],
        }],
        RirLambdaCapture {
            ty: int,
            semantic: RirParamSemantic::MutBorrow,
            abi: RirParamAbi::MutBorrow,
            kind: RirLambdaCaptureKind::Param,
        },
        tuple_ty,
        true,
        RirLambdaCaptureArg::Scoped {
            place: RirPlace {
                local: RirLocalId::from_index(0),
                projections: vec![RirProjection::TupleField(field)],
                ty: int,
            },
        },
    );

    assert_rir_error(program, RirVerifyErrorKind::UnsupportedLambdaCapture);
}

#[test]
fn rir_accepts_copying_stack_cell_lambda_value() {
    let mut program = valid_stack_cell_lambda_rir();
    let g = RirLocalId::from_index(2);
    let lambda_ty = RirTypeId::from_index(2);
    program.functions[0].locals.push(RirLocal {
        id: g,
        ty: lambda_ty,
        mutable: false,
        symbol: RirSymbol::new("g"),
        initialized: false,
        payload_ref: false,
    });
    program.functions[0].body.stmts.push(RirStmt::Init {
        local: g,
        value: RirRValue::Use(RirOperand::Place(RirPlace {
            local: RirLocalId::from_index(1),
            projections: vec![],
            ty: lambda_ty,
        })),
    });

    rir::verify(&program).expect("RIR verify failed");
}

#[test]
fn mut_borrow_lambda_signature_remains_noncopyable() {
    let program = mut_borrow_lambda_rir();

    assert!(!RustRepPolicy::new(&program).lambda_sig_copyable(RirLambdaSigId::from_index(0)));
}

#[test]
fn rir_rejects_copying_mut_borrow_lambda_value() {
    let mut program = mut_borrow_lambda_rir();
    let lambda_ty = RirTypeId::from_index(2);
    let g = RirLocalId::from_index(2);
    program.functions[1].locals.push(RirLocal {
        id: g,
        ty: lambda_ty,
        mutable: false,
        symbol: RirSymbol::new("g"),
        initialized: false,
        payload_ref: false,
    });
    program.functions[1].body.stmts.push(RirStmt::Init {
        local: g,
        value: RirRValue::Use(RirOperand::Place(RirPlace {
            local: RirLocalId::from_index(1),
            projections: vec![],
            ty: lambda_ty,
        })),
    });

    assert_rir_error(program, RirVerifyErrorKind::NonCopyValueRequired);
}

#[test]
fn rir_rejects_passing_mut_borrow_lambda_value() {
    let mut program = mut_borrow_lambda_rir();
    let lambda_ty = RirTypeId::from_index(2);
    let callee = RirFunctionId::from_index(2);
    program.functions.push(RirFunction {
        id: callee,
        air_id: None,
        symbol: RirSymbol::new("callee"),
        params: vec![RirParam {
            local: RirLocalId::from_index(0),
            ty: lambda_ty,
            semantic: RirParamSemantic::Value,
            abi: RirParamAbi::Value,
            escape: RirParamEscape::NonEscaping,
        }],
        ret: RirReturn {
            ty: RirTypeId::from_index(0),
        },
        locals: vec![RirLocal {
            id: RirLocalId::from_index(0),
            ty: lambda_ty,
            mutable: false,
            symbol: RirSymbol::new("f"),
            initialized: true,
            payload_ref: false,
        }],
        body: RirStructuredBlock::default(),
    });
    program.functions[1]
        .body
        .stmts
        .push(RirStmt::Eval(RirRValue::Call {
            callee: RirCallTarget::Function(callee),
            args: vec![RirCallArg::Value(RirOperand::Place(RirPlace {
                local: RirLocalId::from_index(1),
                projections: vec![],
                ty: lambda_ty,
            }))],
            ty: RirTypeId::from_index(0),
        }));

    assert_rir_error(program, RirVerifyErrorKind::NonCopyValueRequired);
}

#[test]
fn lambda_sig_copyable_rejects_value_capture_cycles() {
    let void = RirTypeId::from_index(0);
    let lambda_ty = RirTypeId::from_index(1);
    let sig = RirLambdaSigId::from_index(0);
    let program = RirProgram {
        types: vec![RirType::Void, RirType::Lambda(sig)],
        lambda_sigs: vec![RirLambdaSig {
            id: sig,
            params: vec![],
            ret: void,
        }],
        lambdas: vec![RirLambda {
            id: RirLambdaId::from_index(0),
            source: RirLambdaSource::Lambda(air::LambdaId::from_index(0)),
            function: RirFunctionId::from_index(0),
            sig,
            escape: RirLambdaEscape::NonEscaping,
            storage: RirLambdaStorage::ScopedCaptures,
            captures: vec![RirLambdaCapture {
                ty: lambda_ty,
                semantic: RirParamSemantic::Value,
                abi: RirParamAbi::Value,
                kind: RirLambdaCaptureKind::Param,
            }],
        }],
        ..RirProgram::default()
    };

    assert!(!RustRepPolicy::new(&program).lambda_sig_copyable(sig));
}

#[test]
fn rir_rejects_returning_non_escaping_lambda() {
    let void = RirTypeId::from_index(0);
    let lambda_ty = RirTypeId::from_index(1);
    let sig = RirLambdaSigId::from_index(0);
    let lambda = RirLambdaId::from_index(0);
    let target = RirFunctionId::from_index(0);
    let maker = RirFunctionId::from_index(1);
    let f = RirLocalId::from_index(0);
    let program = RirProgram {
        types: vec![RirType::Void, RirType::Lambda(sig)],
        lambda_sigs: vec![RirLambdaSig {
            id: sig,
            params: vec![],
            ret: void,
        }],
        lambdas: vec![RirLambda {
            id: lambda,
            source: RirLambdaSource::Lambda(air::LambdaId::from_index(0)),
            function: target,
            sig,
            escape: RirLambdaEscape::NonEscaping,
            storage: RirLambdaStorage::ZeroEnv,
            captures: vec![],
        }],
        functions: vec![
            RirFunction {
                id: target,
                air_id: None,
                symbol: RirSymbol::new("target"),
                params: vec![],
                ret: RirReturn { ty: void },
                locals: vec![],
                body: RirStructuredBlock::default(),
            },
            RirFunction {
                id: maker,
                air_id: None,
                symbol: RirSymbol::new("maker"),
                params: vec![],
                ret: RirReturn { ty: lambda_ty },
                locals: vec![RirLocal {
                    id: f,
                    ty: lambda_ty,
                    mutable: false,
                    symbol: RirSymbol::new("f"),
                    initialized: false,
                    payload_ref: false,
                }],
                body: RirStructuredBlock {
                    stmts: vec![RirStmt::Init {
                        local: f,
                        value: RirRValue::Lambda {
                            lambda,
                            captures: vec![],
                            ty: lambda_ty,
                        },
                    }],
                    term: RirTerm::Return(Some(RirOperand::Place(RirPlace {
                        local: f,
                        projections: vec![],
                        ty: lambda_ty,
                    }))),
                },
            },
        ],
        ..RirProgram::default()
    };

    assert_rir_error(program, RirVerifyErrorKind::CallArgEscape);
}

#[test]
fn emit_zero_env_lambda_values_without_heap_envs() {
    let void = RirTypeId::from_index(0);
    let lambda_ty = RirTypeId::from_index(1);
    let sig = RirLambdaSigId::from_index(0);
    let lambda = RirLambdaId::from_index(0);
    let local = RirLocalId::from_index(0);
    let mut program = RirProgram {
        types: vec![RirType::Void, RirType::Lambda(sig)],
        lambda_sigs: vec![RirLambdaSig {
            id: sig,
            params: vec![],
            ret: void,
        }],
        lambdas: vec![RirLambda {
            id: lambda,
            source: RirLambdaSource::Function(FunctionId::from_index(0)),
            function: RirFunctionId::from_index(0),
            sig,
            escape: RirLambdaEscape::Escaping,
            storage: RirLambdaStorage::ZeroEnv,
            captures: vec![],
        }],
        entry: Some(RirFunctionId::from_index(1)),
        ..RirProgram::default()
    };
    program.functions.push(RirFunction {
        id: RirFunctionId::from_index(0),
        air_id: None,
        symbol: RirSymbol::new("target"),
        params: vec![],
        ret: RirReturn { ty: void },
        locals: vec![],
        body: RirStructuredBlock {
            stmts: vec![],
            term: RirTerm::Return(None),
        },
    });
    program.functions.push(RirFunction {
        id: RirFunctionId::from_index(1),
        air_id: None,
        symbol: RirSymbol::new("main_fn"),
        params: vec![],
        ret: RirReturn { ty: void },
        locals: vec![RirLocal {
            id: local,
            ty: lambda_ty,
            mutable: false,
            symbol: RirSymbol::new("f"),
            initialized: false,
            payload_ref: false,
        }],
        body: RirStructuredBlock {
            stmts: vec![
                RirStmt::Init {
                    local,
                    value: RirRValue::Lambda {
                        lambda,
                        captures: vec![],
                        ty: lambda_ty,
                    },
                },
                RirStmt::Eval(RirRValue::Call {
                    callee: RirCallTarget::LambdaValue {
                        callee: RirOperand::Place(RirPlace {
                            local,
                            projections: vec![],
                            ty: lambda_ty,
                        }),
                        sig,
                    },
                    args: vec![],
                    ty: void,
                }),
            ],
            term: RirTerm::Return(None),
        },
    });

    let source = emit::emit(&rir::verify(&program).expect("RIR verify failed"));
    let text = source.as_str();
    assert!(text.contains("enum LambdaSig0"));
    assert!(text.contains("fn call<'cx, 'rt>(self, ctx: &mut AnvCtx<'cx, 'rt>)"));
    assert!(text.contains("LambdaSig0::L0"));
    assert!(!text.contains("Box<dyn"));
    assert!(!text.contains("LambdaEnv"));
    assert!(!text.contains("Vec<"));

    let output = run_source(source);
    assert_eq!(output.status, SourceJobStatus::Success, "{}", output.stderr);
}

#[test]
fn rir_rejects_internal_direct_mut_borrow_lambda_capture() {
    let void = RirTypeId::from_index(0);
    let int = RirTypeId::from_index(1);
    let lambda_ty = RirTypeId::from_index(2);
    let sig = RirLambdaSigId::from_index(0);
    let lambda = RirLambdaId::from_index(0);
    let capture = RirLocalId::from_index(0);
    let f = RirLocalId::from_index(1);
    let hidden = RirLocalId::from_index(0);
    let mut program = RirProgram {
        types: vec![RirType::Void, RirType::Int, RirType::Lambda(sig)],
        consts: vec![
            RirConst {
                id: RirConstId::from_index(0),
                ty: int,
                value: RirConstValue::Int(0),
            },
            RirConst {
                id: RirConstId::from_index(1),
                ty: int,
                value: RirConstValue::Int(1),
            },
        ],
        lambda_sigs: vec![RirLambdaSig {
            id: sig,
            params: vec![],
            ret: void,
        }],
        lambdas: vec![RirLambda {
            id: lambda,
            source: RirLambdaSource::Lambda(air::LambdaId::from_index(0)),
            function: RirFunctionId::from_index(0),
            sig,
            escape: RirLambdaEscape::NonEscaping,
            storage: RirLambdaStorage::ScopedCaptures,
            captures: vec![RirLambdaCapture {
                ty: int,
                semantic: RirParamSemantic::MutBorrow,
                abi: RirParamAbi::MutBorrow,
                kind: RirLambdaCaptureKind::Param,
            }],
        }],
        entry: Some(RirFunctionId::from_index(1)),
        ..RirProgram::default()
    };
    program.functions.push(RirFunction {
        id: RirFunctionId::from_index(0),
        air_id: None,
        symbol: RirSymbol::new("target"),
        params: vec![RirParam {
            local: hidden,
            ty: int,
            semantic: RirParamSemantic::MutBorrow,
            abi: RirParamAbi::MutBorrow,
            escape: RirParamEscape::NonEscaping,
        }],
        ret: RirReturn { ty: void },
        locals: vec![RirLocal {
            id: hidden,
            ty: int,
            mutable: true,
            symbol: RirSymbol::new("captured"),
            initialized: true,
            payload_ref: false,
        }],
        body: RirStructuredBlock {
            stmts: vec![RirStmt::Assign {
                dst: RirPlace {
                    local: hidden,
                    projections: vec![],
                    ty: int,
                },
                value: RirRValue::Use(RirOperand::Const(RirConstId::from_index(1))),
            }],
            term: RirTerm::Return(None),
        },
    });
    program.functions.push(RirFunction {
        id: RirFunctionId::from_index(1),
        air_id: None,
        symbol: RirSymbol::new("main_fn"),
        params: vec![],
        ret: RirReturn { ty: void },
        locals: vec![
            RirLocal {
                id: capture,
                ty: int,
                mutable: true,
                symbol: RirSymbol::new("count"),
                initialized: false,
                payload_ref: false,
            },
            RirLocal {
                id: f,
                ty: lambda_ty,
                mutable: false,
                symbol: RirSymbol::new("f"),
                initialized: false,
                payload_ref: false,
            },
        ],
        body: RirStructuredBlock {
            stmts: vec![
                RirStmt::Init {
                    local: capture,
                    value: RirRValue::Use(RirOperand::Const(RirConstId::from_index(0))),
                },
                RirStmt::Init {
                    local: f,
                    value: RirRValue::Lambda {
                        lambda,
                        captures: vec![RirLambdaCaptureArg::Scoped {
                            place: RirPlace {
                                local: capture,
                                projections: vec![],
                                ty: int,
                            },
                        }],
                        ty: lambda_ty,
                    },
                },
                RirStmt::Eval(RirRValue::Call {
                    callee: RirCallTarget::LambdaValue {
                        callee: RirOperand::Place(RirPlace {
                            local: f,
                            projections: vec![],
                            ty: lambda_ty,
                        }),
                        sig,
                    },
                    args: vec![],
                    ty: void,
                }),
            ],
            term: RirTerm::Return(None),
        },
    });

    assert_rir_error(program, RirVerifyErrorKind::UnsupportedLambdaCapture);
}

#[test]
fn profile_accepts_string_value_params() {
    let mut program = Program::default();
    let string = program.alloc_type(TypeData::String);
    let void = program.alloc_type(TypeData::Void);
    let module = program.alloc_module(root_module());
    let local_id = air::LocalId::from_index(0);
    let func = program.alloc_function(Function {
        name: Ident::new("takes_string"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![param("x", string, ParamMode::Value, local_id)], void),
        locals: vec![local(string, LocalKind::Arg)],
        body: structured_body(vec![], air::AirTail::Return(None)),
    });
    program.module_mut(module).functions.push(func);

    let verified = air::verify(&program).expect("AIR verify failed");
    RustBackendProfile::check(&verified).expect("profile rejected string value param");
}

#[test]
fn profile_accepts_mut_borrow_string_param() {
    let mut program = Program::default();
    let string = program.alloc_type(TypeData::String);
    let void = program.alloc_type(TypeData::Void);
    let module = program.alloc_module(root_module());
    let local_id = air::LocalId::from_index(0);
    let func = program.alloc_function(Function {
        name: Ident::new("takes_mut"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(
            vec![param("x", string, ParamMode::MutBorrow, local_id)],
            void,
        ),
        locals: vec![Local {
            name: None,
            binding: None,
            ty: string,
            mutability: Mutability::Mutable,
            kind: LocalKind::Arg,
        }],
        body: structured_body(vec![], air::AirTail::Return(None)),
    });
    program.module_mut(module).functions.push(func);

    check(program);
}

#[test]
fn profile_rejects_deferred_function_kinds_and_param_roles() {
    let mut program = Program::default();
    let void = program.alloc_type(TypeData::Void);
    let module = program.alloc_module(root_module());
    let self_local = air::LocalId::from_index(0);
    let func = program.alloc_function(Function {
        name: Ident::new("method"),
        module,
        kind: FunctionKind::ExtendMethod,
        owner: None,
        specialization: None,
        signature: Signature::new(
            vec![Param {
                role: ParamRole::Receiver,
                ..param("self", void, ParamMode::Value, self_local)
            }],
            void,
        ),
        locals: vec![local(void, LocalKind::Arg)],
        body: structured_body(vec![], air::AirTail::Return(None)),
    });
    program.module_mut(module).functions.push(func);

    let errors = profile_errors(program);
    assert!(has_error(
        &errors,
        ProfileErrorKind::UnsupportedFunctionKind
    ));
    assert!(has_error(&errors, ProfileErrorKind::UnsupportedParamRole));
}

#[test]
fn profile_rejects_hidden_scoped_local_lambda_capture() {
    let mut program = Program::default();
    let void = program.alloc_type(TypeData::Void);
    let int = program.alloc_type(TypeData::Int);
    let lambda_ty = program.alloc_type(TypeData::Function(air::SignatureType::new(
        vec![],
        air::ReturnMode::Value(void),
    )));
    let module = program.alloc_module(root_module());
    let body = program.alloc_function(Function {
        name: Ident::new("lambda"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], void),
        locals: vec![],
        body: structured_body(vec![], air::AirTail::Return(None)),
    });
    let owner = FunctionId::from_index(1);
    let captured = air::LocalId::from_index(0);
    let lambda = program.alloc_lambda(LambdaDecl {
        source: ExprId(0),
        module,
        body,
        owner,
        signature: air::SignatureType::new(vec![], air::ReturnMode::Value(void)),
        escape: LambdaEscape::NonEscaping,
        captures: vec![air::LambdaCaptureDecl::ScopedLocal {
            binding: BindingId::from_index(0),
            source: CaptureLocalSource {
                owner,
                local: captured,
            },
            ty: int,
            mutability: Mutability::Mutable,
        }],
    });
    program.function_mut(body).kind = FunctionKind::Lambda(lambda);
    let one = program.alloc_const(ConstData {
        ty: int,
        value: ConstValue::Int(1),
    });
    let func = program.alloc_function(Function {
        name: Ident::new("main"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], void),
        locals: vec![
            Local {
                name: None,
                binding: Some(BindingId::from_index(0)),
                ty: int,
                mutability: Mutability::Mutable,
                kind: LocalKind::User,
            },
            local(void, LocalKind::PatternBinding),
            local(void, LocalKind::Return),
        ],
        body: structured_body(
            vec![
                Statement::Init {
                    local: captured,
                    value: RValue::Use(Operand::Const(one)),
                },
                Statement::Eval(RValue::MakeLambda {
                    lambda,
                    captures: vec![air::LambdaCaptureArg::ScopedLocal {
                        place: Place {
                            root: PlaceRoot::Local(captured),
                            projection: vec![],
                            ty: int,
                        },
                    }],
                    ty: lambda_ty,
                }),
            ],
            air::AirTail::Return(None),
        ),
    });
    debug_assert_eq!(func, owner);
    program.module_mut(module).functions.extend([body, func]);

    let errors = profile_errors(program);
    assert!(has_error(
        &errors,
        ProfileErrorKind::UnsupportedLambdaCapture
    ));
    assert!(has_error(&errors, ProfileErrorKind::UnsupportedLocalKind));
}

#[test]
fn plan_rejects_hidden_scoped_local_lambda_capture() {
    let mut program = Program::default();
    let void = program.alloc_type(TypeData::Void);
    let int = program.alloc_type(TypeData::Int);
    let lambda_ty = program.alloc_type(TypeData::Function(air::SignatureType::new(
        vec![],
        air::ReturnMode::Value(void),
    )));
    let module = program.alloc_module(root_module());
    let owner = FunctionId::from_index(1);
    let captured = air::LocalId::from_index(0);
    let body = program.alloc_function(Function {
        name: Ident::new("lambda"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], void),
        locals: vec![],
        body: structured_body(vec![], air::AirTail::Return(None)),
    });
    let lambda = program.alloc_lambda(LambdaDecl {
        source: ExprId(0),
        module,
        owner,
        body,
        signature: air::SignatureType::new(vec![], air::ReturnMode::Value(void)),
        escape: LambdaEscape::NonEscaping,
        captures: vec![air::LambdaCaptureDecl::ScopedLocal {
            binding: BindingId::from_index(0),
            source: CaptureLocalSource {
                owner,
                local: captured,
            },
            ty: int,
            mutability: Mutability::Mutable,
        }],
    });
    program.function_mut(body).kind = FunctionKind::Lambda(lambda);
    let one = program.alloc_const(ConstData {
        ty: int,
        value: ConstValue::Int(1),
    });
    let func = program.alloc_function(Function {
        name: Ident::new("main"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], void),
        locals: vec![Local {
            name: None,
            binding: Some(BindingId::from_index(0)),
            ty: int,
            mutability: Mutability::Mutable,
            kind: LocalKind::User,
        }],
        body: structured_body(
            vec![
                Statement::Init {
                    local: captured,
                    value: RValue::Use(Operand::Const(one)),
                },
                Statement::Eval(RValue::MakeLambda {
                    lambda,
                    captures: vec![air::LambdaCaptureArg::ScopedLocal {
                        place: Place {
                            root: PlaceRoot::Local(captured),
                            projection: vec![],
                            ty: int,
                        },
                    }],
                    ty: lambda_ty,
                }),
            ],
            air::AirTail::Return(None),
        ),
    });
    debug_assert_eq!(func, owner);
    program.module_mut(module).functions.extend([body, func]);
    let verified = air::verify(&program).expect("AIR verify failed");

    let Err(err) = plan(&verified, rust_plan_config()) else {
        panic!("plan accepted scoped-local capture");
    };
    assert!(matches!(
        err,
        RustPlanError::TargetGaps(gaps)
            if gaps.iter().any(|gap| gap.kind == RustTargetGapKind::UnsupportedLambdaCapture)
    ));
}

#[test]
fn emit_traces_tracked_lambda_env_and_lambda_value_fields() {
    let mut program = valid_heap_env_lambda_rir();
    let lambda_ty = RirTypeId::from_index(2);
    let zero = RirLambdaId::from_index(1);
    let zero_fn = RirFunctionId::from_index(2);
    program.lambdas[0].captures[0].ty = lambda_ty;
    program.lambda_envs[0].fields[0].ty = lambda_ty;
    program.functions[0].params[0].ty = lambda_ty;
    program.functions[0].locals[0].ty = lambda_ty;
    program.functions[1].locals[0].ty = lambda_ty;
    program.functions[1].locals[0].initialized = false;
    program.lambdas.push(RirLambda {
        id: zero,
        source: RirLambdaSource::Lambda(air::LambdaId::from_index(1)),
        function: zero_fn,
        sig: RirLambdaSigId::from_index(0),
        escape: RirLambdaEscape::Escaping,
        storage: RirLambdaStorage::ZeroEnv,
        captures: vec![],
    });
    program.functions.push(RirFunction {
        id: zero_fn,
        air_id: None,
        symbol: RirSymbol::new("zero"),
        params: vec![],
        ret: RirReturn {
            ty: RirTypeId::from_index(0),
        },
        locals: vec![],
        body: RirStructuredBlock::default(),
    });
    program.functions[1].body.stmts.insert(
        0,
        RirStmt::Init {
            local: RirLocalId::from_index(0),
            value: RirRValue::Lambda {
                lambda: zero,
                captures: vec![],
                ty: lambda_ty,
            },
        },
    );
    let RirStmt::Init {
        value: RirRValue::Lambda { captures, .. },
        ..
    } = &mut program.functions[1].body.stmts[1]
    else {
        panic!("missing lambda init");
    };
    captures[0] = RirLambdaCaptureArg::Readonly {
        value: RirOperand::Place(RirPlace {
            local: RirLocalId::from_index(0),
            projections: vec![],
            ty: lambda_ty,
        }),
    };

    let verified = rir::verify(&program).expect("RIR verify failed");
    let source = emit::emit(&verified).into_string();

    assert!(source.contains("lambda_env0: heap.register_tracked::<LambdaEnv0"));
    assert!(source.contains("#[derive(Clone, anvyx_runtime::Trace)]\n#[trace(crate = anvyx_runtime, ctx = 'cx)]\nstruct LambdaEnv0<'cx>"));
    assert!(source.contains("c0: LambdaSig0<'cx>,"));
    assert!(source.contains("#[derive(Clone, anvyx_runtime::Trace)]\n#[trace(crate = anvyx_runtime, ctx = 'cx)]\nenum LambdaSig0<'cx>"));
}

#[test]
fn profile_accepts_direct_escaping_capture_cell() {
    check(capture_cell_lambda_program(LambdaEscape::Escaping));
}

#[test]
fn plan_maps_capture_cell_storage_from_lambda_escape() {
    let heap_program = capture_cell_lambda_program(LambdaEscape::Escaping);
    let heap_air = air::verify(&heap_program).expect("AIR verify failed");
    let heap_plan = plan(&heap_air, rust_plan_config()).expect("plan failed");
    let heap_rir = heap_plan.program();

    assert_eq!(heap_rir.cells.len(), 1);
    assert_eq!(heap_rir.cells[0].storage, RirCellStorage::Heap);
    assert!(matches!(
        heap_rir.lambdas[0].storage,
        RirLambdaStorage::HeapEnv {
            env: RirLambdaEnvId(0)
        }
    ));
    assert!(matches!(
        heap_rir.lambdas[0].captures[0].kind,
        RirLambdaCaptureKind::HeapCell { cell: RirCellId(0) }
    ));
    assert!(matches!(
        heap_rir.lambda_envs[0].fields[0].kind,
        RirLambdaEnvFieldKind::HeapCell { cell: RirCellId(0) }
    ));

    let stack_program = capture_cell_lambda_program(LambdaEscape::NonEscaping);
    let stack_air = air::verify(&stack_program).expect("AIR verify failed");
    let stack_plan = plan(&stack_air, rust_plan_config()).expect("plan failed");
    let stack_rir = stack_plan.program();

    assert_eq!(stack_rir.cells.len(), 1);
    assert_eq!(stack_rir.cells[0].storage, RirCellStorage::StackScoped);
    assert!(stack_rir.lambda_envs.is_empty());
    assert!(matches!(
        stack_rir.lambdas[0].storage,
        RirLambdaStorage::ScopedCaptures
    ));
    assert!(matches!(
        stack_rir.lambdas[0].captures[0].kind,
        RirLambdaCaptureKind::StackCell { cell: RirCellId(0) }
    ));
}

#[test]
fn plan_lowers_escaping_readonly_capture_to_heap_env() {
    let mut program = Program::default();
    let void = program.alloc_type(TypeData::Void);
    let int = program.alloc_type(TypeData::Int);
    let lambda_ty = program.alloc_type(TypeData::Function(air::SignatureType::new(
        vec![],
        air::ReturnMode::Value(void),
    )));
    let module = program.alloc_module(root_module());
    let owner = FunctionId::from_index(1);
    let consumer = FunctionId::from_index(2);
    let captured = air::LocalId::from_index(0);
    let f = air::LocalId::from_index(1);
    let g = air::LocalId::from_index(2);
    let body = program.alloc_function(Function {
        name: Ident::new("lambda"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], void),
        locals: vec![],
        body: structured_body(vec![], air::AirTail::Return(None)),
    });
    let lambda = program.alloc_lambda(LambdaDecl {
        source: ExprId(0),
        module,
        owner,
        body,
        signature: air::SignatureType::new(vec![], air::ReturnMode::Value(void)),
        escape: LambdaEscape::Escaping,
        captures: vec![air::LambdaCaptureDecl::ReadonlyLocal {
            binding: BindingId::from_index(0),
            source: CaptureLocalSource {
                owner,
                local: captured,
            },
            ty: int,
        }],
    });
    program.function_mut(body).kind = FunctionKind::Lambda(lambda);
    let one = program.alloc_const(ConstData {
        ty: int,
        value: ConstValue::Int(1),
    });
    let func = program.alloc_function(Function {
        name: Ident::new("main"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], void),
        locals: vec![
            Local {
                name: None,
                binding: Some(BindingId::from_index(0)),
                ty: int,
                mutability: Mutability::Immutable,
                kind: LocalKind::User,
            },
            local(lambda_ty, LocalKind::User),
            local(lambda_ty, LocalKind::User),
        ],
        body: structured_body(
            vec![
                Statement::Init {
                    local: captured,
                    value: RValue::Use(Operand::Const(one)),
                },
                Statement::Init {
                    local: f,
                    value: RValue::MakeLambda {
                        lambda,
                        captures: vec![air::LambdaCaptureArg::ReadonlyLocal {
                            value: Operand::Place(Place {
                                root: PlaceRoot::Local(captured),
                                projection: vec![],
                                ty: int,
                            }),
                        }],
                        ty: lambda_ty,
                    },
                },
                Statement::Init {
                    local: g,
                    value: RValue::Use(Operand::Place(Place {
                        root: PlaceRoot::Local(f),
                        projection: vec![],
                        ty: lambda_ty,
                    })),
                },
                Statement::Eval(RValue::Call {
                    callee: Callee::Function(consumer),
                    args: vec![CallArg::Value(Operand::Place(Place {
                        root: PlaceRoot::Local(g),
                        projection: vec![],
                        ty: lambda_ty,
                    }))],
                }),
            ],
            air::AirTail::Return(None),
        ),
    });
    debug_assert_eq!(func, owner);
    let consumer_func = program.alloc_function(Function {
        name: Ident::new("consumer"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(
            vec![Param {
                name: Some(Ident::new("f")),
                ty: lambda_ty,
                mode: ParamMode::Value,
                escape: ParamEscape::Escaping,
                role: ParamRole::Normal,
                local_id: air::LocalId::from_index(0),
            }],
            void,
        ),
        locals: vec![local(lambda_ty, LocalKind::Arg)],
        body: structured_body(vec![], air::AirTail::Return(None)),
    });
    debug_assert_eq!(consumer_func, consumer);
    program
        .module_mut(module)
        .functions
        .extend([body, func, consumer_func]);

    let verified = air::verify(&program).expect("AIR verify failed");
    let plan = plan(&verified, rust_plan_config()).expect("plan failed");
    let rir = plan.program();

    assert_eq!(rir.lambda_envs.len(), 1);
    assert_eq!(rir.lambda_envs[0].lambda, RirLambdaId::from_index(0));
    assert_eq!(rir.lambda_envs[0].fields.len(), 1);
    let int = rir
        .types
        .iter()
        .position(|ty| *ty == RirType::Int)
        .map(RirTypeId::from_index)
        .unwrap();
    assert_eq!(rir.lambda_envs[0].fields[0].ty, int);
    assert!(matches!(
        rir.lambdas[0].storage,
        RirLambdaStorage::HeapEnv {
            env: RirLambdaEnvId(_)
        }
    ));

    let source = emit::emit(&plan.verified()).into_string();
    assert!(source.contains("lambda_env0: anvyx_runtime::HeapType<'cx, LambdaEnv0>"));
    assert!(source.contains("lambda_env0: heap.register_untracked::<LambdaEnv0>()"));
    assert!(source.contains("struct LambdaEnv0"));
    assert!(source.contains("c0: i64,"));
    assert!(source.contains("ctx.heap().alloc(heap_type, LambdaEnv0 { c0:"));
    assert!(source.contains("L0 { env: anvyx_runtime::Handle<'cx, LambdaEnv0> }"));
    let materialize = source
        .find("let c0 = ctx.heap().with(env, |env|")
        .expect("missing env field materialization");
    let call = source[materialize..]
        .find("(ctx, c0)")
        .expect("missing materialized body call");
    assert!(call > 0);
    for forbidden in [
        "Box<dyn Fn",
        "Vec<Value",
        "static ",
        "thread_local!",
        "OnceLock",
    ] {
        assert!(
            !source.contains(forbidden),
            "generated source contains {forbidden}"
        );
    }
}

#[test]
fn profile_accepts_zero_env_lambda_callees() {
    let mut program = Program::default();
    let void = program.alloc_type(TypeData::Void);
    let lambda_ty = program.alloc_type(TypeData::Function(air::SignatureType::new(
        vec![],
        air::ReturnMode::Value(void),
    )));
    let module = program.alloc_module(root_module());
    let lambda_local = air::LocalId::from_index(0);
    let func = program.alloc_function(Function {
        name: Ident::new("main"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(
            vec![param("f", lambda_ty, ParamMode::Value, lambda_local)],
            void,
        ),
        locals: vec![local(lambda_ty, LocalKind::Arg)],
        body: structured_body(
            vec![Statement::Eval(RValue::Call {
                callee: Callee::Lambda(Operand::Place(place(lambda_local, lambda_ty))),
                args: vec![],
            })],
            air::AirTail::Return(None),
        ),
    });
    program.module_mut(module).functions.push(func);

    let verified = air::verify(&program).expect("AIR verify failed");
    RustBackendProfile::check(&verified).expect("profile rejected zero-env lambda callee");
}

#[test]
fn profile_rejects_lambda_extern_boundaries() {
    let mut program = Program::default();
    let void = program.alloc_type(TypeData::Void);
    let function = program.alloc_type(TypeData::Function(air::SignatureType::new(
        vec![],
        air::ReturnMode::Value(void),
    )));
    let ext = extern_in_module(
        &mut program,
        &["host"],
        "retain",
        vec![(function, ParamMode::Value)],
        void,
        ExternMember::FreeFunction,
    );
    program.externs[ext.index()].binding = Some(provider_binding("host", "retain"));

    let errors = profile_errors(program);
    assert!(has_error(
        &errors,
        ProfileErrorKind::UnsupportedLambdaExternBoundary
    ));
}

#[test]
fn profile_rejects_collection_rvalues() {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let optional = program.alloc_type(TypeData::Optional(int));
    let list = program.alloc_type(TypeData::List(int));
    let module = program.alloc_module(root_module());
    let list_local = air::LocalId::from_index(0);
    let one = program.const_arena.alloc(ConstData {
        ty: int,
        value: ConstValue::Int(1),
    });
    let func = program.alloc_function(Function {
        name: Ident::new("main"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], int),
        locals: vec![local(list, LocalKind::Temp)],
        body: structured_body(
            vec![
                Statement::Init {
                    local: list_local,
                    value: RValue::Aggregate {
                        kind: AggregateCtor::List,
                        fields: vec![Operand::Const(one)],
                        ty: list,
                    },
                },
                Statement::Eval(RValue::ListPop {
                    list: place(list_local, list),
                    ty: optional,
                }),
            ],
            air::AirTail::Return(Some(Operand::Const(one))),
        ),
    });
    program.module_mut(module).functions.push(func);

    expect_reject(program, ProfileErrorKind::UnsupportedRValue);
}

#[test]
fn profile_accepts_provider_bound_non_runtime_extern_call() {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let ext = extern_in_module(
        &mut program,
        &["core_int"],
        "int_abs",
        vec![(int, ParamMode::Value)],
        int,
        ExternMember::FreeFunction,
    );
    program.externs[ext.index()].binding = Some(ExternBindingDecl {
        package: anvyx_frontend::resolve::PackageId::core(),
        provider: anvyx_runtime::ProviderId {
            name: "core_int".to_string(),
        },
        key: anvyx_runtime::ExternBindingKey {
            target: anvyx_runtime::ExternBindingTarget::Function(
                anvyx_runtime::ExternFunctionKey {
                    module: anvyx_runtime::ModulePath {
                        segments: vec!["core_int".to_string()],
                    },
                    name: "int_abs".to_string(),
                },
            ),
            operation: anvyx_runtime::ExternBindingOp::Call,
        },
    });
    let module = program.alloc_module(root_module());
    let tmp = air::LocalId::from_index(0);
    let arg = program.const_arena.alloc(ConstData {
        ty: int,
        value: ConstValue::Int(-7),
    });
    let main = program.alloc_function(Function {
        name: Ident::new("main"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], int),
        locals: vec![local(int, LocalKind::Temp)],
        body: structured_body(
            vec![Statement::Init {
                local: tmp,
                value: RValue::Call {
                    callee: Callee::Extern(ext),
                    args: vec![CallArg::Value(Operand::Const(arg))],
                },
            }],
            air::AirTail::Return(Some(Operand::Place(place(tmp, int)))),
        ),
    });
    program.module_mut(module).functions.push(main);

    check(program);
}

#[test]
fn plan_accepts_core_runtime_native_binding() {
    let program = scalar_print_program();
    let verified = air::verify(&program).expect("AIR verify failed");
    let plan = plan(&verified, rust_plan_config()).expect("plan failed");

    assert!(matches!(
        plan.program().externs[0].kind,
        RirExternKind::Native(_)
    ));
}

#[test]
fn plan_rejects_missing_native_binding() {
    let program = scalar_print_program();
    let verified = air::verify(&program).expect("AIR verify failed");
    let Err(err) = plan(&verified, RustPlanConfig::default()) else {
        panic!("plan should reject missing native binding");
    };

    assert!(
        matches!(err, RustPlanError::TargetGaps(gaps) if gaps.iter().any(|gap| gap.kind == RustTargetGapKind::UnsupportedExtern))
    );
}

#[test]
fn plan_rejects_unsupported_native_abi() {
    let program = scalar_print_program();
    let verified = air::verify(&program).expect("AIR verify failed");
    let mut config = rust_plan_config();
    let binding = &mut config.native_providers[0].modules[0].bindings[0];
    binding.abi.support = anvyx_runtime::RustAbiSupport::Unsupported;
    let Err(err) = plan(&verified, config) else {
        panic!("plan should reject unsupported native ABI");
    };

    assert!(
        matches!(err, RustPlanError::TargetGaps(gaps) if gaps.iter().any(|gap| gap.kind == RustTargetGapKind::UnsupportedRustAbi))
    );
}

#[test]
fn profile_rejects_source_only_extern() {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let void = program.alloc_type(TypeData::Void);
    extern_in_module(
        &mut program,
        &["core_runtime"],
        "_println",
        vec![(int, ParamMode::Value)],
        void,
        ExternMember::FreeFunction,
    );

    expect_reject(program, ProfileErrorKind::UnsupportedExtern);
}

#[test]
fn profile_rejects_runtime_named_extern_in_wrong_module() {
    let mut program = Program::default();
    let string = program.alloc_type(TypeData::String);
    let void = program.alloc_type(TypeData::Void);
    extern_in_module(
        &mut program,
        &["host"],
        "_println",
        vec![(string, ParamMode::Value)],
        void,
        ExternMember::FreeFunction,
    );

    expect_reject(program, ProfileErrorKind::UnsupportedExtern);
}

#[test]
fn profile_accepts_bound_extern_members_but_rejects_missing_binding() {
    let mut program = Program::default();
    let void = program.alloc_type(TypeData::Void);
    let module = program.alloc_module(runtime_module());
    let owner = program.alloc_extern_type(ExternTypeDecl {
        name: Ident::new("Host"),
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
    program.module_mut(module).extern_types.push(owner);
    let id = program.alloc_extern(ExternDecl {
        name: Ident::new("_println"),
        module,
        member: ExternMember::StaticMethod { owner },
        params: vec![],
        return_type: void,
        binding: None,
        effects: anvyx_runtime::ExternEffects::default(),
    });
    program.module_mut(module).externs.push(id);

    let errors = profile_errors(program);
    assert!(has_error(&errors, ProfileErrorKind::UnsupportedExtern));
}

#[test]
fn plan_maps_structured_air_branch_to_structured_rir() {
    let program = scalar_branch_program();
    let verified = air::verify(&program).expect("AIR verify failed");
    let plan = plan(&verified, RustPlanConfig::default()).expect("plan failed");
    let body = &plan.program().functions[0].body;

    assert!(matches!(body.stmts[0], RirStmt::If(_)));
    assert!(matches!(body.term, RirTerm::Return(Some(_))));
}

#[test]
fn plan_signatures_uses_stable_ids_and_context_first_shape() {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let module = program.alloc_module(root_module());
    let arg = air::LocalId::from_index(0);
    let first = program.alloc_function(Function {
        name: Ident::new("same"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![param("x", int, ParamMode::Value, arg)], int),
        locals: vec![local(int, LocalKind::Arg)],
        body: structured_body(
            vec![],
            air::AirTail::Return(Some(Operand::Place(place(arg, int)))),
        ),
    });
    let one = program.const_arena.alloc(ConstData {
        ty: int,
        value: ConstValue::Int(1),
    });
    let second = program.alloc_function(Function {
        name: Ident::new("same"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], int),
        locals: vec![],
        body: structured_body(vec![], air::AirTail::Return(Some(Operand::Const(one)))),
    });
    program.module_mut(module).functions.extend([first, second]);
    let verified = air::verify(&program).expect("AIR verify failed");

    let plan = plan(&verified, RustPlanConfig::default()).expect("plan failed");
    let functions = &plan.program().functions;

    assert_ne!(functions[0].symbol, functions[1].symbol);
    assert_eq!(functions[0].params.len(), 1);
    assert_eq!(functions[0].params[0].semantic, RirParamSemantic::Value);
    assert_eq!(functions[0].params[0].abi, RirParamAbi::Value);
}

#[test]
fn plan_preserves_semantic_param_and_call_modes() {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let string = program.alloc_type(TypeData::String);
    let void = program.alloc_type(TypeData::Void);
    let array = program.alloc_type(TypeData::Array { elem: int, len: 1 });
    let list = program.alloc_type(TypeData::List(int));
    let module = program.alloc_module(root_module());
    let aggregate = program.alloc_aggregate(air::AggregateDecl {
        name: Ident::new("Pair"),
        module,
        kind: air::AggregateKind::Struct,
        type_args: vec![],
        const_args: vec![],
        fields: vec![FieldDecl {
            name: Ident::new("x"),
            ty: int,
        }],
        cycle_capable: false,
        stringify_override: None,
    });
    program.module_mut(module).aggregates.push(aggregate);
    let strukt = program.alloc_type(TypeData::Aggregate(aggregate));
    let zero = program.const_arena.alloc(ConstData {
        ty: int,
        value: ConstValue::Int(0),
    });
    let one = program.const_arena.alloc(ConstData {
        ty: int,
        value: ConstValue::Int(1),
    });
    let text = program.const_arena.alloc(ConstData {
        ty: string,
        value: ConstValue::String("ok".into()),
    });
    let scalar =
        returning_int_param_function(&mut program, module, "scalar", int, ParamMode::Value, zero);
    let string_borrow = returning_int_param_function(
        &mut program,
        module,
        "string_borrow",
        string,
        ParamMode::SharedBorrow,
        zero,
    );
    let array_value = returning_int_param_function(
        &mut program,
        module,
        "array_value",
        array,
        ParamMode::Value,
        zero,
    );
    let array_shared = returning_int_param_function(
        &mut program,
        module,
        "array_shared",
        array,
        ParamMode::SharedBorrow,
        zero,
    );
    let struct_shared = returning_int_param_function(
        &mut program,
        module,
        "struct_shared",
        strukt,
        ParamMode::SharedBorrow,
        zero,
    );
    let list_value = returning_int_param_function(
        &mut program,
        module,
        "list_value",
        list,
        ParamMode::Value,
        zero,
    );
    let list_shared = returning_int_param_function(
        &mut program,
        module,
        "list_shared",
        list,
        ParamMode::SharedBorrow,
        zero,
    );

    let array_local = air::LocalId::from_index(0);
    let list_local = air::LocalId::from_index(1);
    let entry = program.alloc_function(Function {
        name: Ident::new("main"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], void),
        locals: vec![local(array, LocalKind::Temp), local(list, LocalKind::Temp)],
        body: structured_body(
            vec![
                Statement::Init {
                    local: array_local,
                    value: RValue::Aggregate {
                        kind: AggregateCtor::Array,
                        fields: vec![Operand::Const(one)],
                        ty: array,
                    },
                },
                Statement::Init {
                    local: list_local,
                    value: RValue::Aggregate {
                        kind: AggregateCtor::List,
                        fields: vec![Operand::Const(one)],
                        ty: list,
                    },
                },
                Statement::Eval(RValue::Call {
                    callee: Callee::Function(scalar),
                    args: vec![CallArg::Value(Operand::Const(one))],
                }),
                Statement::Eval(RValue::Call {
                    callee: Callee::Function(string_borrow),
                    args: vec![CallArg::SharedStringConst(text)],
                }),
                Statement::Eval(RValue::Call {
                    callee: Callee::Function(array_value),
                    args: vec![CallArg::Value(Operand::Place(place(array_local, array)))],
                }),
                Statement::Eval(RValue::Call {
                    callee: Callee::Function(array_shared),
                    args: vec![CallArg::SharedBorrow(place(array_local, array))],
                }),
                Statement::Eval(RValue::Call {
                    callee: Callee::Function(list_shared),
                    args: vec![CallArg::SharedBorrow(place(list_local, list))],
                }),
            ],
            air::AirTail::Return(None),
        ),
    });
    program.module_mut(module).functions.extend([
        scalar,
        string_borrow,
        array_value,
        array_shared,
        struct_shared,
        list_value,
        list_shared,
        entry,
    ]);
    program.entry = Some(entry);
    let verified = air::verify(&program).expect("AIR verify failed");

    let plan = plan(&verified, RustPlanConfig::default()).expect("plan failed");
    let functions = &plan.program().functions;
    for (function, semantic, abi) in [
        (scalar, RirParamSemantic::Value, RirParamAbi::Value),
        (
            string_borrow,
            RirParamSemantic::SharedBorrow,
            RirParamAbi::SharedBorrow,
        ),
        (array_value, RirParamSemantic::Value, RirParamAbi::Value),
        (
            array_shared,
            RirParamSemantic::SharedBorrow,
            RirParamAbi::SharedBorrow,
        ),
        (
            struct_shared,
            RirParamSemantic::SharedBorrow,
            RirParamAbi::SharedBorrow,
        ),
        (list_value, RirParamSemantic::Value, RirParamAbi::Value),
        (
            list_shared,
            RirParamSemantic::SharedBorrow,
            RirParamAbi::SharedBorrow,
        ),
    ] {
        let param = functions[function.index()].params[0];
        assert_eq!((param.semantic, param.abi), (semantic, abi));
    }

    let call_args = functions[entry.index()].body.stmts[2..]
        .iter()
        .map(|stmt| match stmt {
            RirStmt::Eval(RirRValue::Call { args, .. }) => args[0].semantic(),
            other => panic!("expected call eval, got {other:?}"),
        })
        .collect::<Vec<_>>();
    assert_eq!(
        call_args,
        vec![
            RirParamSemantic::Value,
            RirParamSemantic::SharedBorrow,
            RirParamSemantic::Value,
            RirParamSemantic::SharedBorrow,
            RirParamSemantic::SharedBorrow,
        ]
    );
}

#[test]
fn profile_accepts_mut_borrow_call() {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let void = program.alloc_type(TypeData::Void);
    let module = program.alloc_module(root_module());
    let arg = air::LocalId::from_index(0);
    let callee = program.alloc_function(Function {
        name: Ident::new("mutate"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![param("x", int, ParamMode::MutBorrow, arg)], void),
        locals: vec![Local {
            mutability: Mutability::Mutable,
            ..local(int, LocalKind::Arg)
        }],
        body: structured_body(vec![], air::AirTail::Return(None)),
    });
    let local_id = air::LocalId::from_index(0);
    let one = program.const_arena.alloc(ConstData {
        ty: int,
        value: ConstValue::Int(1),
    });
    let caller = program.alloc_function(Function {
        name: Ident::new("main"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], void),
        locals: vec![Local {
            mutability: Mutability::Mutable,
            ..local(int, LocalKind::Temp)
        }],
        body: structured_body(
            vec![
                Statement::Init {
                    local: local_id,
                    value: RValue::Use(Operand::Const(one)),
                },
                Statement::Eval(RValue::Call {
                    callee: Callee::Function(callee),
                    args: vec![CallArg::MutBorrow(place(local_id, int))],
                }),
            ],
            air::AirTail::Return(None),
        ),
    });
    program
        .module_mut(module)
        .functions
        .extend([callee, caller]);
    program.entry = Some(caller);

    check(program);
}

#[test]
fn profile_accepts_lambda_callee_capture_cell_source_var_arg() {
    let mut program = owner_capture_cell_source_var_arg_program();
    let callee = FunctionId::from_index(0);
    let caller = FunctionId::from_index(1);
    let int = program.function(callee).signature.params[0].ty;
    let void = program.function(callee).signature.return_mode.ty();
    let lambda_ty = program.alloc_type(TypeData::Function(air::SignatureType::new(
        vec![air::ParamType {
            ty: int,
            mode: ParamMode::MutBorrow,
            escape: ParamEscape::NonEscaping,
        }],
        air::ReturnMode::Value(void),
    )));
    let callee_local = air::LocalId::from_index(program.function(caller).locals.len());
    let function = program.function_mut(caller);
    function.locals.push(local(lambda_ty, LocalKind::Temp));
    function.body.block.stmts.insert(
        1,
        Statement::Init {
            local: callee_local,
            value: RValue::FunctionRef {
                function: callee,
                ty: lambda_ty,
            },
        },
    );
    let Statement::Eval(RValue::Call { callee, .. }) = &mut function.body.block.stmts[2] else {
        unreachable!("test helper should end with the source var call")
    };
    *callee = Callee::Lambda(Operand::Place(place(callee_local, lambda_ty)));

    check(program);
}

#[test]
fn profile_accepts_scoped_borrowed_param_capture() {
    let mut program = scoped_borrow_lambda_program();
    program
        .function_mut(FunctionId::from_index(1))
        .body
        .block
        .stmts
        .clear();
    check(program);
}

#[test]
fn profile_accepts_nested_scoped_borrowed_param_capture() {
    check(nested_scoped_borrow_lambda_program());
}

#[test]
fn profile_accepts_scoped_borrowed_param_forwarding() {
    check(scoped_borrow_lambda_program());
}

#[test]
fn profile_rejects_scoped_borrowed_param_to_native_mut_borrow() {
    let mut program = scoped_borrow_lambda_program();
    let body = FunctionId::from_index(1);
    let int = program.function(FunctionId::from_index(0)).signature.params[0].ty;
    let void = program.function(body).signature.return_mode.ty();
    let ext = extern_in_module(
        &mut program,
        &["host"],
        "touch",
        vec![(int, ParamMode::MutBorrow)],
        void,
        ExternMember::FreeFunction,
    );
    program.externs[ext.index()].binding = Some(provider_binding("host", "touch"));
    let Statement::Eval(RValue::Call { callee, .. }) =
        &mut program.function_mut(body).body.block.stmts[0]
    else {
        unreachable!("test helper should start with source var call")
    };
    *callee = Callee::Extern(ext);

    expect_reject(
        program,
        ProfileErrorKind::UnsupportedMutablePlaceNativeBoundary,
    );
}

#[test]
fn classifier_accepts_direct_scalar_dataref_source_var_arg() {
    let program = projected_mut_call_arg_program();
    let function = FunctionId::from_index(1);
    let Statement::Eval(RValue::Call { args, .. }) =
        &program.function(function).body.block.stmts[0]
    else {
        unreachable!();
    };
    let CallArg::MutBorrow(place) = &args[0] else {
        unreachable!();
    };
    let root = place.root.local().unwrap();
    let root_ty = program.function(function).locals[root.index()].ty;

    assert!(matches!(
        classify_dataref_mut_place(&program, root_ty, place),
        DataRefMutPlaceSupport::Supported(_)
    ));
}

#[test]
fn profile_accepts_dataref_source_var_arg() {
    check(projected_mut_call_arg_program());
}

#[test]
fn profile_accepts_capture_cell_dataref_source_var_arg() {
    check(capture_cell_dataref_source_var_arg_program());
}

#[test]
fn profile_rejects_capture_cell_to_native_mut_borrow() {
    let mut program = lambda_capture_cell_source_var_arg_program();
    let body = FunctionId::from_index(1);
    let int = program.function(FunctionId::from_index(0)).signature.params[0].ty;
    let void = program.function(body).signature.return_mode.ty();
    let ext = extern_in_module(
        &mut program,
        &["host"],
        "touch",
        vec![(int, ParamMode::MutBorrow)],
        void,
        ExternMember::FreeFunction,
    );
    program.externs[ext.index()].binding = Some(provider_binding("host", "touch"));
    let Statement::Eval(RValue::Call { callee, .. }) =
        &mut program.function_mut(body).body.block.stmts[0]
    else {
        unreachable!("test helper should start with the source var call")
    };
    *callee = Callee::Extern(ext);

    expect_reject(
        program,
        ProfileErrorKind::UnsupportedMutablePlaceNativeBoundary,
    );
}

#[test]
fn plan_temps_capture_cell_call_value_before_cell_set() {
    let program = owner_capture_cell_call_set_program();
    let verified = air::verify(&program).expect("AIR verify failed");
    let plan = plan(&verified, rust_plan_config()).expect("plan failed");
    let owner = rir_function_for_air(plan.program(), FunctionId::from_index(1));

    let cell_set = owner
        .body
        .stmts
        .iter()
        .position(|stmt| matches!(stmt, RirStmt::CellSet { .. }))
        .expect("missing cell set");
    assert!(owner.body.stmts[..cell_set].iter().any(|stmt| {
        matches!(
            stmt,
            RirStmt::Init {
                value: RirRValue::Call { .. },
                ..
            }
        )
    }));
    assert!(matches!(
        &owner.body.stmts[cell_set],
        RirStmt::CellSet {
            value: RirRValue::Use(RirOperand::Place(_)),
            ..
        }
    ));
}

#[test]
fn plan_lowers_owner_heap_capture_cell_source_var_arg_to_heap_cell() {
    let program = heap_capture_cell_source_var_arg_program();
    let verified = air::verify(&program).expect("AIR verify failed");
    let plan = plan(&verified, rust_plan_config()).expect("plan failed");
    let caller = rir_function_for_air(plan.program(), FunctionId::from_index(2));
    let arg = only_call_arg(caller);

    assert!(matches!(
        arg,
        RirCallArg::MutPlace(RirMutPlaceArg::HeapCell {
            cell: RirCellRef::Owner(id),
            ..
        }) if *id == RirCellId::from_index(0)
    ));
}

#[test]
fn plan_lowers_lambda_heap_capture_cell_source_var_arg_to_heap_cell() {
    let program = escaping_lambda_capture_cell_source_var_arg_program();
    let verified = air::verify(&program).expect("AIR verify failed");
    let plan = plan(&verified, rust_plan_config()).expect("plan failed");
    let lambda_body = rir_function_for_air(plan.program(), FunctionId::from_index(1));
    let arg = only_call_arg(lambda_body);

    assert!(matches!(
        arg,
        RirCallArg::MutPlace(RirMutPlaceArg::HeapCell {
            cell: RirCellRef::Capture { cell, .. },
            ..
        }) if *cell == RirCellId::from_index(0)
    ));
}

#[test]
fn plan_lowers_owner_capture_cell_source_var_arg_to_stack_cell() {
    let program = owner_capture_cell_source_var_arg_program();
    let verified = air::verify(&program).expect("AIR verify failed");
    let plan = plan(&verified, rust_plan_config()).expect("plan failed");
    let caller = rir_function_for_air(plan.program(), FunctionId::from_index(1));
    let arg = only_call_arg(caller);

    assert!(matches!(
        arg,
        RirCallArg::MutPlace(RirMutPlaceArg::StackCell {
            cell: RirCellRef::Owner(id),
            ..
        }) if *id == RirCellId::from_index(0)
    ));
}

#[test]
fn plan_lowers_lambda_capture_cell_source_var_arg_to_stack_cell() {
    let program = lambda_capture_cell_source_var_arg_program();
    let verified = air::verify(&program).expect("AIR verify failed");
    let plan = plan(&verified, rust_plan_config()).expect("plan failed");
    let lambda_body = rir_function_for_air(plan.program(), FunctionId::from_index(1));
    let arg = only_call_arg(lambda_body);

    assert!(matches!(
        arg,
        RirCallArg::MutPlace(RirMutPlaceArg::StackCell {
            cell: RirCellRef::Capture { cell, .. },
            ..
        }) if *cell == RirCellId::from_index(0)
    ));
}

#[test]
fn air_rejects_scoped_borrowed_param_projection_before_profile() {
    let mut program = scoped_borrow_lambda_program();
    let int = program.function(FunctionId::from_index(0)).signature.params[0].ty;
    let tuple = program.alloc_type(TypeData::Tuple(vec![int]));
    program.scoped_borrows[0].ty = tuple;
    let owner = program.function_mut(FunctionId::from_index(2));
    owner.signature.params[0].ty = tuple;
    owner.locals[0].ty = tuple;
    let lambda_body = program.function_mut(FunctionId::from_index(1));
    let Statement::Eval(RValue::Call { args, .. }) = &mut lambda_body.body.block.stmts[0] else {
        unreachable!("test helper should start with source var call")
    };
    args[0] = CallArg::MutBorrow(Place {
        root: PlaceRoot::LambdaCapture(air::LambdaCaptureSlotId::from_index(0)),
        projection: vec![Projection::TupleField(0)],
        ty: int,
    });

    air::verify(&program).expect_err("AIR accepted scoped borrow projection");
}

#[test]
fn air_rejects_escaping_scoped_borrowed_param_capture_before_profile() {
    let mut program = scoped_borrow_lambda_program();
    program.lambdas[0].escape = LambdaEscape::Escaping;

    air::verify(&program).expect_err("AIR accepted escaping scoped borrow capture");
}

#[test]
fn plan_lowers_scoped_borrow_to_scoped_place_cell() {
    let program = scoped_borrow_lambda_program();
    let verified = air::verify(&program).expect("AIR verify failed");
    let plan = plan(&verified, rust_plan_config()).expect("plan failed");
    let rir = plan.program();

    assert!(rir.cells.is_empty());
    assert_eq!(rir.scoped_place_cells.len(), 1);
    assert_eq!(
        rir.scoped_place_cells[0].owner,
        RirFunctionId::from_index(2)
    );
    assert_eq!(
        rir.scoped_place_cells[0].source_local,
        RirLocalId::from_index(0)
    );
    assert!(matches!(
        rir.lambdas[0].captures[0].kind,
        RirLambdaCaptureKind::ScopedPlaceCell {
            cell: RirScopedPlaceCellId(0)
        }
    ));
}

#[test]
fn plan_lowers_scoped_borrow_capture_arg_to_owner_ref() {
    let program = scoped_borrow_lambda_program();
    let verified = air::verify(&program).expect("AIR verify failed");
    let plan = plan(&verified, rust_plan_config()).expect("plan failed");
    let owner = rir_function_for_air(plan.program(), FunctionId::from_index(2));

    let RirStmt::Init {
        value: RirRValue::Lambda { captures, .. },
        ..
    } = &owner.body.stmts[0]
    else {
        panic!("expected lambda init")
    };
    assert!(matches!(
        captures.as_slice(),
        [RirLambdaCaptureArg::ScopedPlaceCell {
            cell: RirScopedPlaceCellRef::Owner(RirScopedPlaceCellId(0))
        }]
    ));
}

#[test]
fn plan_lowers_scoped_borrow_forwarding_to_scoped_place_cell_arg() {
    let program = scoped_borrow_lambda_program();
    let verified = air::verify(&program).expect("AIR verify failed");
    let plan = plan(&verified, rust_plan_config()).expect("plan failed");
    let lambda_body = rir_function_for_air(plan.program(), FunctionId::from_index(1));
    let arg = only_call_arg(lambda_body);

    assert!(matches!(
        arg,
        RirCallArg::MutPlace(RirMutPlaceArg::ScopedPlaceCell {
            cell: RirScopedPlaceCellRef::Capture {
                cell: RirScopedPlaceCellId(0),
                ..
            },
            ..
        })
    ));
}

#[test]
fn plan_lowers_nested_scoped_borrow_recapture_to_same_cell() {
    let program = nested_scoped_borrow_lambda_program();
    let verified = air::verify(&program).expect("AIR verify failed");
    let plan = plan(&verified, rust_plan_config()).expect("plan failed");
    let outer_body = rir_function_for_air(plan.program(), FunctionId::from_index(1));
    let inner_body = rir_function_for_air(plan.program(), FunctionId::from_index(2));

    let RirStmt::Init {
        value: RirRValue::Lambda { captures, .. },
        ..
    } = &outer_body.body.stmts[0]
    else {
        panic!("expected nested lambda init")
    };
    assert!(matches!(
        captures.as_slice(),
        [RirLambdaCaptureArg::ScopedPlaceCell {
            cell: RirScopedPlaceCellRef::Capture {
                cell: RirScopedPlaceCellId(0),
                local: RirLocalId(1),
            }
        }]
    ));
    assert!(matches!(
        only_call_arg(inner_body),
        RirCallArg::MutPlace(RirMutPlaceArg::ScopedPlaceCell {
            cell: RirScopedPlaceCellRef::Capture {
                cell: RirScopedPlaceCellId(0),
                local: RirLocalId(0),
            },
            ..
        })
    ));
}

#[test]
fn plan_lowers_owner_scoped_borrow_write_to_scoped_place_cell_set() {
    let mut program = scoped_borrow_lambda_program();
    let int = program.function(FunctionId::from_index(0)).signature.params[0].ty;
    let one = int_const(&mut program, int, 1);
    program
        .function_mut(FunctionId::from_index(2))
        .body
        .block
        .stmts
        .push(Statement::Assign {
            dst: root_place(
                PlaceRoot::ScopedBorrow(air::ScopedBorrowId::from_index(0)),
                int,
            ),
            value: RValue::Use(Operand::Const(one)),
        });
    let verified = air::verify(&program).expect("AIR verify failed");
    let plan = plan(&verified, rust_plan_config()).expect("plan failed");
    let owner = rir_function_for_air(plan.program(), FunctionId::from_index(2));

    assert!(
        owner
            .body
            .stmts
            .iter()
            .any(|stmt| matches!(stmt, RirStmt::ScopedPlaceCellSet { .. }))
    );
}

#[test]
fn plan_lowers_lambda_scoped_borrow_read_to_scoped_place_cell_get() {
    let mut program = scoped_borrow_lambda_program();
    let int = program.function(FunctionId::from_index(0)).signature.params[0].ty;
    let body = program.function_mut(FunctionId::from_index(1));
    body.locals.push(local(int, LocalKind::Temp));
    body.body.block.stmts.insert(
        0,
        Statement::Init {
            local: air::LocalId::from_index(0),
            value: RValue::Use(Operand::Place(root_place(
                PlaceRoot::LambdaCapture(air::LambdaCaptureSlotId::from_index(0)),
                int,
            ))),
        },
    );
    let verified = air::verify(&program).expect("AIR verify failed");
    let plan = plan(&verified, rust_plan_config()).expect("plan failed");
    let lambda_body = rir_function_for_air(plan.program(), FunctionId::from_index(1));

    assert!(lambda_body.body.stmts.iter().any(|stmt| matches!(
        stmt,
        RirStmt::Init {
            value: RirRValue::ScopedPlaceCellGet { .. },
            ..
        }
    )));
}

#[test]
fn plan_lowers_owner_scoped_borrow_read_to_scoped_place_cell_get() {
    let mut program = scoped_borrow_lambda_program();
    let int = program.function(FunctionId::from_index(0)).signature.params[0].ty;
    let owner = program.function_mut(FunctionId::from_index(2));
    let temp = air::LocalId::from_index(owner.locals.len());
    owner.locals.push(local(int, LocalKind::Temp));
    owner.body.block.stmts.push(Statement::Init {
        local: temp,
        value: RValue::Use(Operand::Place(root_place(
            PlaceRoot::ScopedBorrow(air::ScopedBorrowId::from_index(0)),
            int,
        ))),
    });
    let verified = air::verify(&program).expect("AIR verify failed");
    let plan = plan(&verified, rust_plan_config()).expect("plan failed");
    let owner = rir_function_for_air(plan.program(), FunctionId::from_index(2));

    assert!(owner.body.stmts.iter().any(|stmt| matches!(
        stmt,
        RirStmt::Init {
            value: RirRValue::ScopedPlaceCellGet {
                cell: RirScopedPlaceCellRef::Owner(RirScopedPlaceCellId(0)),
                ..
            },
            ..
        }
    )));
}

#[test]
fn plan_lowers_owner_scoped_borrow_forwarding_to_scoped_place_cell_arg() {
    let mut program = scoped_borrow_lambda_program();
    let int = program.function(FunctionId::from_index(0)).signature.params[0].ty;
    let callee = FunctionId::from_index(0);
    program
        .function_mut(FunctionId::from_index(2))
        .body
        .block
        .stmts
        .push(source_var_call(
            callee,
            PlaceRoot::ScopedBorrow(air::ScopedBorrowId::from_index(0)),
            int,
        ));
    let verified = air::verify(&program).expect("AIR verify failed");
    let plan = plan(&verified, rust_plan_config()).expect("plan failed");
    let owner = rir_function_for_air(plan.program(), FunctionId::from_index(2));

    assert!(matches!(
        only_call_arg(owner),
        RirCallArg::MutPlace(RirMutPlaceArg::ScopedPlaceCell {
            cell: RirScopedPlaceCellRef::Owner(RirScopedPlaceCellId(0)),
            ..
        })
    ));
}

#[test]
fn plan_lowers_lambda_scoped_borrow_write_to_scoped_place_cell_set() {
    let mut program = scoped_borrow_lambda_program();
    let int = program.function(FunctionId::from_index(0)).signature.params[0].ty;
    let one = int_const(&mut program, int, 1);
    program
        .function_mut(FunctionId::from_index(1))
        .body
        .block
        .stmts
        .push(Statement::Assign {
            dst: root_place(
                PlaceRoot::LambdaCapture(air::LambdaCaptureSlotId::from_index(0)),
                int,
            ),
            value: RValue::Use(Operand::Const(one)),
        });
    let verified = air::verify(&program).expect("AIR verify failed");
    let plan = plan(&verified, rust_plan_config()).expect("plan failed");
    let body = rir_function_for_air(plan.program(), FunctionId::from_index(1));

    assert!(body.body.stmts.iter().any(|stmt| matches!(
        stmt,
        RirStmt::ScopedPlaceCellSet {
            cell: RirScopedPlaceCellRef::Capture {
                cell: RirScopedPlaceCellId(0),
                local: RirLocalId(0),
            },
            ..
        }
    )));
}

#[test]
fn emit_passes_capture_cell_var_arg_as_stack_cell_mut_place() {
    let source = plan_source(lambda_capture_cell_source_var_arg_program()).into_string();

    assert!(source.contains("MutPlace::stack_cell(&"));
    assert!(!source.contains("MutPlace::local(&mut"));
    assert!(!source.contains(".get_copy()?"));
}

#[test]
fn emit_passes_owner_heap_cell_var_arg_as_heap_cell_mut_place() {
    let source = plan_source(heap_capture_cell_source_var_arg_program()).into_string();

    assert!(source.contains("MutPlace::heap_cell(__cell0.clone())"));
    assert!(!source.contains("MutPlace::local(&mut"));
    assert!(!source.contains("__cell0.get_copy"));
    assert!(!source.contains("__cell0.set"));
    assert!(!source.contains("ctx.heap().with(&__cell0, |cell| cell.get_copy())?"));
    assert!(!source.contains("ctx.heap().with(&__cell0, |cell| cell.set"));
    assert!(!source.contains("ctx.heap().with(&__cell0, |cell| bump"));
    assert!(!source.contains("StackLambdaCell"));
}

#[test]
fn emit_passes_capture_heap_cell_var_arg_as_heap_cell_mut_place() {
    let source = plan_source(escaping_lambda_capture_cell_source_var_arg_program()).into_string();

    assert!(source.contains("MutPlace::heap_cell(v0.clone())"));
    assert!(!source.contains("MutPlace::local(&mut"));
    assert!(!source.contains("v0.get_copy"));
    assert!(!source.contains("v0.set"));
    assert!(!source.contains("ctx.heap().with(&v0, |cell| cell.get_copy())?"));
    assert!(!source.contains("ctx.heap().with(&v0, |cell| cell.set"));
    assert!(!source.contains("ctx.heap().with(&v0, |cell| bump"));
    assert!(!source.contains("StackLambdaCell"));
}

#[test]
fn emit_reentrant_heap_cell_var_arg_call_is_not_wrapped_in_cell_borrow() {
    let source = plan_source(heap_capture_cell_reentrant_source_var_arg_program()).into_string();

    assert!(source.contains("apply(ctx, anvyx_runtime::MutPlace::heap_cell("));
    assert!(!source.contains("ctx.heap().with(&v0, |cell| apply"));
    assert!(!source.contains("cell.mutate(|value| apply"));
    assert!(!source.contains("ctx.heap().with(&v0, |cell| cell.get_copy())?"));
    assert!(!source.contains("ctx.heap().with(&v0, |cell| cell.set"));
}

#[test]
fn emit_temps_mut_place_read_before_heap_cell_set() {
    let source = plan_source(heap_cell_set_from_mut_place_param_program()).into_string();

    assert!(source.contains(".get_copy(ctx.runtime())?;"));
    assert!(source.contains("ctx.heap().with(&__cell0, |cell| cell.set(v"));
    assert!(
        !source.contains(
            "ctx.heap().with(&__cell0, |cell| cell.set(v0_x.get_copy(ctx.runtime())?))?;"
        )
    );
}

#[test]
fn emit_scoped_borrowed_param_uses_scoped_mut_place_cell() {
    let source = plan_source(scoped_borrow_lambda_program()).into_string();

    assert!(source.contains("let __scoped0 = anvyx_runtime::ScopedMutPlaceCell::new(v0);"));
    assert!(source.contains("c0: &'env anvyx_runtime::ScopedMutPlaceCell<'env, 'cx, i64>"));
    assert!(source.contains("c0: &__scoped0"));
    assert!(source.contains("anvyx_runtime::MutPlace::scoped_cell(v0)"));
    assert!(!source.contains("&mut **c0"));
    assert!(!source.contains("MutPlace::local(&mut v0)"));
    assert!(!source.contains("MutPlace::stack_cell"));
}

#[test]
fn emit_owner_scoped_borrow_forwarding_uses_scoped_cell_mut_place() {
    let mut program = scoped_borrow_lambda_program();
    let int = program.function(FunctionId::from_index(0)).signature.params[0].ty;
    program
        .function_mut(FunctionId::from_index(2))
        .body
        .block
        .stmts
        .push(source_var_call(
            FunctionId::from_index(0),
            PlaceRoot::ScopedBorrow(air::ScopedBorrowId::from_index(0)),
            int,
        ));
    let source = plan_source(program).into_string();

    assert!(source.contains("anvyx_runtime::MutPlace::scoped_cell(&__scoped0)"));
    assert!(!source.contains("MutPlace::local(&mut v0)"));
    assert!(!source.contains("v0.reborrow()"));
}

#[test]
fn profile_rejects_source_var_param_to_native_mut_borrow() {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let void = program.alloc_type(TypeData::Void);
    let ext = extern_in_module(
        &mut program,
        &["host"],
        "touch",
        vec![(int, ParamMode::MutBorrow)],
        void,
        ExternMember::FreeFunction,
    );
    program.externs[ext.index()].binding = Some(provider_binding("host", "touch"));
    let module = program.alloc_module(root_module());
    let x = air::LocalId::from_index(0);
    let caller = program.alloc_function(Function {
        name: Ident::new("caller"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![param("x", int, ParamMode::MutBorrow, x)], void),
        locals: vec![mut_local(int, LocalKind::Arg)],
        body: structured_body(
            vec![Statement::Eval(RValue::Call {
                callee: Callee::Extern(ext),
                args: vec![CallArg::MutBorrow(place(x, int))],
            })],
            air::AirTail::Return(None),
        ),
    });
    program.module_mut(module).functions.push(caller);

    expect_reject(
        program,
        ProfileErrorKind::UnsupportedMutablePlaceNativeBoundary,
    );
}

#[test]
fn profile_rejects_dataref_field_to_native_mut_borrow() {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let void = program.alloc_type(TypeData::Void);
    let ext = extern_in_module(
        &mut program,
        &["host"],
        "touch",
        vec![(int, ParamMode::MutBorrow)],
        void,
        ExternMember::FreeFunction,
    );
    program.externs[ext.index()].binding = Some(provider_binding("host", "touch"));
    let module = program.alloc_module(root_module());
    let aggregate = dataref_decl(&mut program, module, int);
    let node = program.alloc_type(TypeData::DataRef(aggregate));
    let node_local = air::LocalId::from_index(0);
    let caller = program.alloc_function(Function {
        name: Ident::new("caller"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(
            vec![param("node", node, ParamMode::Value, node_local)],
            void,
        ),
        locals: vec![local(node, LocalKind::Arg)],
        body: structured_body(
            vec![Statement::Eval(RValue::Call {
                callee: Callee::Extern(ext),
                args: vec![CallArg::MutBorrow(Place {
                    root: PlaceRoot::Local(node_local),
                    projection: vec![Projection::Field(air::FieldId::from_index(0))],
                    ty: int,
                })],
            })],
            air::AirTail::Return(None),
        ),
    });
    program.module_mut(module).functions.push(caller);

    expect_reject(
        program,
        ProfileErrorKind::UnsupportedMutablePlaceNativeBoundary,
    );
}

#[test]
fn plan_method_symbols_include_owner_and_keep_free_function_calls() {
    let mut program = Program::default();
    let void = program.alloc_type(TypeData::Void);
    let int = program.alloc_type(TypeData::Int);
    let module = program.alloc_module(root_module());
    let first_aggregate = program.alloc_aggregate(air::AggregateDecl {
        name: Ident::new("First"),
        module,
        kind: air::AggregateKind::Struct,
        type_args: vec![],
        const_args: vec![],
        fields: vec![],
        cycle_capable: false,
        stringify_override: None,
    });
    let second_aggregate = program.alloc_aggregate(air::AggregateDecl {
        name: Ident::new("Second"),
        module,
        kind: air::AggregateKind::Struct,
        type_args: vec![],
        const_args: vec![],
        fields: vec![],
        cycle_capable: false,
        stringify_override: None,
    });
    program
        .module_mut(module)
        .aggregates
        .extend([first_aggregate, second_aggregate]);
    let first_ty = program.alloc_type(TypeData::Aggregate(first_aggregate));
    let second_ty = program.alloc_type(TypeData::Aggregate(second_aggregate));
    let one = program.const_arena.alloc(ConstData {
        ty: int,
        value: ConstValue::Int(1),
    });
    let two = program.const_arena.alloc(ConstData {
        ty: int,
        value: ConstValue::Int(2),
    });
    let recv = air::LocalId::from_index(0);
    let first_method = program.alloc_function(Function {
        name: Ident::new("value"),
        module,
        kind: FunctionKind::Method,
        owner: None,
        specialization: None,
        signature: Signature::new(
            vec![Param {
                role: ParamRole::Receiver,
                ..param("self", first_ty, ParamMode::Value, recv)
            }],
            int,
        ),
        locals: vec![local(first_ty, LocalKind::Arg)],
        body: structured_body(vec![], air::AirTail::Return(Some(Operand::Const(one)))),
    });
    let second_method = program.alloc_function(Function {
        name: Ident::new("value"),
        module,
        kind: FunctionKind::Method,
        owner: None,
        specialization: None,
        signature: Signature::new(
            vec![Param {
                role: ParamRole::Receiver,
                ..param("self", second_ty, ParamMode::Value, recv)
            }],
            int,
        ),
        locals: vec![local(second_ty, LocalKind::Arg)],
        body: structured_body(vec![], air::AirTail::Return(Some(Operand::Const(two)))),
    });
    let static_method = program.alloc_function(Function {
        name: Ident::new("value"),
        module,
        kind: FunctionKind::Method,
        owner: Some(air::FunctionOwner {
            name: Ident::new("First"),
        }),
        specialization: None,
        signature: Signature::new(vec![], int),
        locals: vec![],
        body: structured_body(vec![], air::AirTail::Return(Some(Operand::Const(one)))),
    });
    let free = program.alloc_function(Function {
        name: Ident::new("value"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], int),
        locals: vec![],
        body: structured_body(vec![], air::AirTail::Return(Some(Operand::Const(one)))),
    });
    let first_local = air::LocalId::from_index(0);
    let second_local = air::LocalId::from_index(1);
    let main = program.alloc_function(Function {
        name: Ident::new("main"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], void),
        locals: vec![
            local(first_ty, LocalKind::Temp),
            local(second_ty, LocalKind::Temp),
        ],
        body: structured_body(
            vec![
                Statement::Init {
                    local: first_local,
                    value: RValue::Aggregate {
                        kind: AggregateCtor::Struct(first_aggregate),
                        fields: vec![],
                        ty: first_ty,
                    },
                },
                Statement::Init {
                    local: second_local,
                    value: RValue::Aggregate {
                        kind: AggregateCtor::Struct(second_aggregate),
                        fields: vec![],
                        ty: second_ty,
                    },
                },
                Statement::Eval(RValue::Call {
                    callee: Callee::Function(first_method),
                    args: vec![CallArg::Value(Operand::Place(place(first_local, first_ty)))],
                }),
                Statement::Eval(RValue::Call {
                    callee: Callee::Function(second_method),
                    args: vec![CallArg::Value(Operand::Place(place(
                        second_local,
                        second_ty,
                    )))],
                }),
                Statement::Eval(RValue::Call {
                    callee: Callee::Function(static_method),
                    args: vec![],
                }),
                Statement::Eval(RValue::Call {
                    callee: Callee::Function(free),
                    args: vec![],
                }),
            ],
            air::AirTail::Return(None),
        ),
    });
    program.module_mut(module).functions.extend([
        first_method,
        second_method,
        static_method,
        free,
        main,
    ]);
    program.set_entry(main);

    let source = plan_source(program).into_string();

    assert!(source.contains("fn anv_f0_First_value"));
    assert!(source.contains("fn anv_f1_Second_value"));
    assert!(source.contains("fn anv_f2_First_value"));
    assert!(source.contains("fn anv_f3_value"));
    assert!(source.contains("anv_f0_First_value(ctx,"));
    assert!(source.contains("anv_f1_Second_value(ctx,"));
    assert!(source.contains("anv_f2_First_value(ctx)"));
    assert!(source.contains("anv_f3_value(ctx)"));
    assert!(!source.contains("impl "));
    assert!(!source.contains("trait "));
}

#[test]
fn emit_renders_format_with_central_specs_and_borrowed_strings() {
    let program = format_program();
    let source = plan_source(program).into_string();

    assert!(source.contains("anvyx_runtime::AnvString::from(format!(\"{:04}\", 7))"));
    assert!(source.contains("anvyx_runtime::AnvString::from(format!(\"{:*>5}\", v1.as_str()))"));
    assert!(source.contains("anvyx_runtime::AnvString::from(format!(\"{:.2}\", 1.25))"));
    assert!(source.contains("anvyx_runtime::AnvString::from(format!(\"{:X}\", 255))"));
    assert!(source.contains("anvyx_runtime::AnvString::from(format!(\"{:b}\", 5))"));
    assert!(source.contains("anvyx_runtime::AnvString::from(format!(\"{:e}\", 1.0))"));
    assert!(source.contains("anvyx_runtime::AnvString::from(format!(\"{:E}\", 1.0))"));
    assert!(!source.contains(".clone()"));
    assert!(!source.contains(".to_owned()"));
}

#[test]
fn emit_renders_string_concat_as_anv_string_without_clone_or_to_owned() {
    let program = string_concat_program();
    let source = plan_source(program).into_string();

    assert!(source.contains("let mut out = String::new();"));
    assert!(source.contains("out.push_str(\"a\");"));
    assert!(source.contains("out.push_str(v0.as_str());"));
    assert!(source.contains("v0 = { let mut out = String::new(); out.push_str(v0.as_str());"));
    assert!(source.contains("anvyx_runtime::AnvString::from(out)"));
    assert!(!source.contains(".clone()"));
    assert!(!source.contains(".to_owned()"));
}

#[test]
fn emit_renders_context_first_free_functions_without_clone_or_traits() {
    let program = scalar_print_program();
    let source = plan_source(program).into_string();

    assert!(source.contains("struct AnvTypes<'cx>"));
    assert!(source.contains("fn register(heap: &mut anvyx_runtime::Heap<'cx>) -> Self"));
    assert!(source.contains("struct AnvCtx<'cx, 'rt>"));
    assert!(source.contains("fn runtime(&mut self) -> &mut anvyx_runtime::Ctx<'cx, 'rt>"));
    assert!(source.contains("let types = AnvTypes::register(heap);"));
    assert!(source.contains("let rt = anvyx_runtime::Ctx::new(heap);"));
    assert!(source.contains("let mut ctx = AnvCtx::new(rt, types);"));
    assert!(source.contains("fn anv_f0_main<'cx, 'rt>(ctx: &mut AnvCtx<'cx, 'rt>)"));
    assert!(source.contains(
        "anvyx_core2::__anvyx_native::core_runtime::_println(ctx.runtime(), v0.as_str())"
    ));
    assert!(!source.contains("type AnvCtx"));
    assert_eq!(source.matches("anvyx_runtime::Ctx::new").count(), 1);
    assert!(!source.contains("fn anv_extern__println"));
    assert!(!source.contains("trait "));
    assert!(!source.contains(".clone()"));
    assert!(!source.contains(".to_owned()"));
}

#[test]
fn emit_uses_underscored_context_only_for_leaf_bodies() {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let module = program.alloc_module(root_module());
    let one = program.const_arena.alloc(ConstData {
        ty: int,
        value: ConstValue::Int(1),
    });
    let leaf = program.alloc_function(Function {
        name: Ident::new("leaf"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], int),
        locals: vec![],
        body: structured_body(vec![], air::AirTail::Return(Some(Operand::Const(one)))),
    });
    let caller = program.alloc_function(Function {
        name: Ident::new("caller"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], int),
        locals: vec![local(int, LocalKind::Temp)],
        body: structured_body(
            vec![Statement::Init {
                local: air::LocalId::from_index(0),
                value: RValue::Call {
                    callee: Callee::Function(leaf),
                    args: vec![],
                },
            }],
            air::AirTail::Return(Some(Operand::Place(place(
                air::LocalId::from_index(0),
                int,
            )))),
        ),
    });
    program.module_mut(module).functions.extend([leaf, caller]);

    let source = plan_source(program).into_string();

    assert!(source.contains("fn anv_f0_leaf<'cx, 'rt>(_ctx: &mut AnvCtx<'cx, 'rt>) -> i64"));
    assert!(source.contains("fn anv_f1_caller<'cx, 'rt>(ctx: &mut AnvCtx<'cx, 'rt>) -> i64"));
    assert!(source.contains("anv_f0_leaf(ctx)"));
}

#[test]
fn emit_uses_underscored_context_for_primitive_struct_stringify_helper() {
    let mut program = struct_field_read_program();
    let point = program.functions[0].locals[0].ty;
    if let air::AirStmt::Init { value, .. } = &mut program.functions[0].body.block.stmts[1] {
        *value = RValue::Stringify {
            value: Operand::Place(place(air::LocalId::from_index(0), point)),
            source_ty: point,
        };
    }
    let source = plan_source(program).into_string();

    assert!(source.contains(
        "fn anvstringify_t3_point<'cx, 'rt>(_ctx: &mut AnvCtx<'cx, 'rt>, value: &anvT3_Point)"
    ));
}

#[test]
fn emit_keeps_context_name_for_nested_struct_stringify_helper() {
    let int = RirTypeId::from_index(0);
    let inner = RirTypeId::from_index(1);
    let outer = RirTypeId::from_index(2);
    let program = RirProgram {
        types: vec![
            RirType::Int,
            RirType::Struct(RirStructId::from_index(0)),
            RirType::Struct(RirStructId::from_index(1)),
        ],
        structs: vec![
            RirStruct {
                id: RirStructId::from_index(0),
                air_id: None,
                symbol: RirSymbol::new("Inner"),
                display: RirSymbol::new("Inner"),
                native_path: None,
                native_key: None,
                copyable: true,
                fields: vec![RirField {
                    id: RirFieldId::from_index(0),
                    symbol: RirSymbol::new("x"),
                    ty: int,
                }],
            },
            RirStruct {
                id: RirStructId::from_index(1),
                air_id: None,
                symbol: RirSymbol::new("Outer"),
                display: RirSymbol::new("Outer"),
                native_path: None,
                native_key: None,
                copyable: true,
                fields: vec![RirField {
                    id: RirFieldId::from_index(0),
                    symbol: RirSymbol::new("inner"),
                    ty: inner,
                }],
            },
        ],
        stringify_helpers: vec![
            RirStringifyHelper {
                id: RirStringifyHelperId::from_index(0),
                ty: inner,
                symbol: RirSymbol::new("stringify_inner"),
            },
            RirStringifyHelper {
                id: RirStringifyHelperId::from_index(1),
                ty: outer,
                symbol: RirSymbol::new("stringify_outer"),
            },
        ],
        stringify_reqs: vec![
            RirStringifyReq {
                id: RirStringifyReqId::from_index(0),
                ty: inner,
                kind: RirStringifyReqKind::Structural(RirStringifyHelperId::from_index(0)),
            },
            RirStringifyReq {
                id: RirStringifyReqId::from_index(1),
                ty: outer,
                kind: RirStringifyReqKind::Structural(RirStringifyHelperId::from_index(1)),
            },
        ],
        ..RirProgram::default()
    };
    let verified = rir::verify(&program).expect("RIR verify failed");
    let source = emit::emit(&verified).into_string();

    assert!(
        source.contains("fn stringify_inner<'cx, 'rt>(_ctx: &mut AnvCtx<'cx, 'rt>, value: &Inner)")
    );
    assert!(
        source.contains("fn stringify_outer<'cx, 'rt>(ctx: &mut AnvCtx<'cx, 'rt>, value: &Outer)")
    );
    assert!(source.contains("stringify_inner(ctx, &value.inner)"));
}

#[test]
fn source_job_compiles_and_runs_format_program() {
    let program = format_program();
    let source = plan_source(program);

    let output = run_source(source);

    assert_eq!(output.status, SourceJobStatus::Success);
    assert_eq!(output.stdout, "0007\n");
}

#[test]
fn source_job_compiles_and_runs_string_concat_program() {
    let program = string_concat_program();
    let source = plan_source(program);

    let output = run_source(source);

    assert_eq!(output.status, SourceJobStatus::Success);
    assert_eq!(output.stdout, "ab\nabc\n");
}

#[test]
fn emit_wraps_native_string_returns() {
    let source =
        emit::emit(&rir::verify(&native_string_return_program()).expect("RIR verify failed"))
            .into_string();

    assert!(source.contains("anvyx_runtime::AnvString::from(host::string(ctx.runtime()))"));
}

#[test]
fn emit_wraps_native_option_string_returns() {
    let mut program = native_option_return_program(Some(RirCoreEnumKind::Option));
    let option = RirTypeId::from_index(1);
    program.functions.push(RirFunction {
        id: RirFunctionId::from_index(0),
        air_id: None,
        symbol: RirSymbol::new("f"),
        params: vec![],
        ret: RirReturn { ty: option },
        locals: vec![RirLocal {
            id: RirLocalId::from_index(0),
            ty: option,
            mutable: false,
            symbol: RirSymbol::new("v0"),
            initialized: false,
            payload_ref: false,
        }],
        body: RirStructuredBlock {
            stmts: vec![RirStmt::Init {
                local: RirLocalId::from_index(0),
                value: RirRValue::Call {
                    callee: RirCallTarget::Extern(RirExternId::from_index(0)),
                    args: vec![],
                    ty: option,
                },
            }],
            term: RirTerm::Return(Some(RirOperand::Place(RirPlace {
                local: RirLocalId::from_index(0),
                projections: vec![],
                ty: option,
            }))),
        },
    });

    program.entry = Some(RirFunctionId::from_index(0));
    let source = emit::emit(&rir::verify(&program).expect("RIR verify failed"));
    let text = source.as_str();

    assert!(text.contains("match host::substring(ctx.runtime())"));
    assert!(text.contains("Some(value) => Some(anvyx_runtime::AnvString::from(value))"));

    let source = emit::RustSource::new(format!(
        "mod host {{ pub fn substring<'cx, 'rt>(_ctx: &mut anvyx_runtime::Ctx<'cx, 'rt>) -> Option<String> {{ Some(\"ok\".to_string()) }} }}\n{}",
        source.into_string()
    ));
    let output = run_source(source);

    assert_eq!(output.status, SourceJobStatus::Success, "{}", output.stderr);
}

#[test]
fn emit_propagates_fallible_native_calls() {
    let source = plan_source(fallible_call_program(false, false)).into_string();

    assert!(source.contains(
        "fn anv_f0_main<'cx, 'rt>(ctx: &mut AnvCtx<'cx, 'rt>) -> Result<(), anvyx_runtime::RuntimeError>"
    ));
    assert!(source.contains("host::fallible(ctx.runtime(), 41)?;"));
    assert!(source.contains("fn main() -> Result<(), anvyx_runtime::RuntimeError>"));
    assert!(source.contains("let _ = anv_f0_main(&mut ctx)?;"));
    assert!(!source.contains(".unwrap()"));
}

#[test]
fn emit_propagates_generated_fallibility_transitively() {
    let source = plan_source(fallible_call_program(true, false)).into_string();

    assert!(source.contains(
        "fn anv_f0_leaf<'cx, 'rt>(ctx: &mut AnvCtx<'cx, 'rt>) -> Result<(), anvyx_runtime::RuntimeError>"
    ));
    assert!(source.contains(
        "fn anv_f1_main<'cx, 'rt>(ctx: &mut AnvCtx<'cx, 'rt>) -> Result<(), anvyx_runtime::RuntimeError>"
    ));
    assert!(source.contains("anv_f0_leaf(ctx)?;"));
}

#[test]
fn source_job_compiles_fallible_non_void_entry() {
    let source = with_fallible_host(plan_source(fallible_call_program(false, true)));
    let output = run_source(source);

    assert_eq!(output.status, SourceJobStatus::Success);
}

#[test]
fn emit_borrows_string_literal_call_arg_without_owned_temp() {
    let source = plan_source(borrow_string_literal_program()).into_string();

    assert!(source.contains(
        "anvyx_core2::__anvyx_native::core_runtime::_println(ctx.runtime(), \"ready\");"
    ));
    assert!(!source.contains("String::from"));
    assert!(!source.contains("to_string()"));
}

#[test]
fn emit_forwards_borrowed_string_param_as_str_without_double_borrow() {
    let source = plan_source(shared_string_forward_program()).into_string();

    assert!(source.contains(": &str"));
    assert!(
        source.contains("anvyx_core2::__anvyx_native::core_runtime::_println(ctx.runtime(), v0);")
    );
    assert!(
        !source
            .contains("anvyx_core2::__anvyx_native::core_runtime::_println(ctx.runtime(), &v0);")
    );
    assert!(!source.contains(
        "anvyx_core2::__anvyx_native::core_runtime::_println(ctx.runtime(), v0.as_str());"
    ));
}

#[test]
fn emit_borrows_string_constant_for_native_string_param() {
    let source = plan_source(native_str_len_const_program()).into_string();

    assert!(
        source
            .contains("anvyx_core2::__anvyx_native::core_string::str_len(ctx.runtime(), \"abc\")")
    );
    assert!(!source.contains("String::from"));
    assert!(!source.contains("to_string()"));
}

#[test]
fn emit_borrows_string_local_for_native_string_param() {
    let source = plan_source(native_str_len_local_program()).into_string();

    assert!(
        source.contains(
            "anvyx_core2::__anvyx_native::core_string::str_len(ctx.runtime(), v0.as_str())"
        )
    );
    assert!(
        !source.contains("anvyx_core2::__anvyx_native::core_string::str_len(ctx.runtime(), &v0)")
    );
}

#[test]
fn emit_formats_borrowed_string_param_without_owned_temp() {
    let source = plan_source(format_borrowed_string_program()).into_string();

    assert!(source.contains("v0: &str"));
    assert!(source.contains("format!(\"{:*>5}\", v0)"));
    assert!(!source.contains("v0.as_str()"));
    assert!(!source.contains(".clone()"));
    assert!(!source.contains(".to_owned()"));
}

#[test]
fn source_job_compiles_and_runs_scalar_print_program() {
    let program = scalar_print_program();
    let source = plan_source(program);

    let output = run_source(source);

    assert_eq!(output.status, SourceJobStatus::Success);
    assert_eq!(output.stdout, "1\n");
}

#[test]
fn rir_verify_rejects_return_from_uninitialized_local() {
    let mut program = empty_rir_function(RirType::Int);
    program.functions[0].locals.push(RirLocal {
        id: RirLocalId::from_index(0),
        ty: RirTypeId::from_index(0),
        mutable: false,
        symbol: RirSymbol::new("v0"),
        initialized: false,
        payload_ref: false,
    });
    program.functions[0].body.term = RirTerm::Return(Some(RirOperand::Place(RirPlace {
        local: RirLocalId::from_index(0),
        projections: vec![],
        ty: RirTypeId::from_index(0),
    })));

    let errors = rir::verify(&program).expect_err("verified uninitialized local");
    assert!(
        errors
            .iter()
            .any(|error| matches!(error.kind, RirVerifyErrorKind::UninitializedLocal(_)))
    );
}

#[test]
fn source_job_compiles_structured_branch_without_dispatcher() {
    let source = plan_source(scalar_branch_program());
    let text = source.as_str();

    assert!(text.contains("if true {"));
    assert!(text.contains("return v0;"));
    assert!(!text.contains("let mut bb = 0usize;"));
    assert!(!text.contains("match bb"));
    let output = run_source(source);
    assert_eq!(output.status, SourceJobStatus::Success, "{}", output.stderr);
}

#[test]
fn source_job_compiles_unreachable_terminator() {
    let mut program = empty_rir_function(RirType::Int);
    program.functions[0].body.term = RirTerm::Unreachable;
    program.entry = Some(RirFunctionId::from_index(0));
    let verified = rir::verify(&program).expect("RIR verify failed");
    let source = emit::emit(&verified);

    let output = run_source(source);

    assert!(matches!(output.status, SourceJobStatus::RunFailed(_)));
}

#[test]
fn emit_preserves_nested_unreachable_terms() {
    let mut program = empty_rir_function(RirType::Int);
    let bool_ty = RirTypeId::from_index(1);
    program.types.push(RirType::Bool);
    program.consts.push(RirConst {
        id: RirConstId::from_index(1),
        ty: bool_ty,
        value: RirConstValue::Bool(true),
    });
    program.functions[0].body = RirStructuredBlock {
        stmts: vec![RirStmt::If(RirIf {
            cond: RirOperand::Const(RirConstId::from_index(1)),
            then_block: RirStructuredBlock {
                stmts: vec![],
                term: RirTerm::Unreachable,
            },
            else_block: Some(RirStructuredBlock {
                stmts: vec![],
                term: RirTerm::Return(Some(RirOperand::Const(RirConstId::from_index(0)))),
            }),
        })],
        term: RirTerm::None,
    };

    let verified = rir::verify(&program).expect("RIR verify failed");
    let source = emit::emit(&verified);

    assert!(source.as_str().contains("unreachable!();"));
}

#[test]
fn rir_verify_rejects_falling_root_none_for_nonvoid_function() {
    let mut program = empty_rir_function(RirType::Int);
    program.functions[0].body.term = RirTerm::None;

    assert_rir_error(program, RirVerifyErrorKind::ReturnValueRequired);
}

#[test]
fn rir_verify_accepts_root_none_after_nonfallthrough_if() {
    let mut program = empty_rir_function(RirType::Int);
    let bool_ty = RirTypeId::from_index(1);
    program.types.push(RirType::Bool);
    program.consts.push(RirConst {
        id: RirConstId::from_index(1),
        ty: bool_ty,
        value: RirConstValue::Bool(true),
    });
    program.functions[0].body = RirStructuredBlock {
        stmts: vec![RirStmt::If(RirIf {
            cond: RirOperand::Const(RirConstId::from_index(1)),
            then_block: RirStructuredBlock {
                stmts: vec![],
                term: RirTerm::Return(Some(RirOperand::Const(RirConstId::from_index(0)))),
            },
            else_block: Some(RirStructuredBlock {
                stmts: vec![],
                term: RirTerm::Return(Some(RirOperand::Const(RirConstId::from_index(0)))),
            }),
        })],
        term: RirTerm::None,
    };

    rir::verify(&program).expect("RIR verify failed");
}

#[test]
fn rir_verify_rejects_duplicate_enum_match_arms() {
    let program = enum_match_rir(
        vec![
            RirEnumMatchArm {
                variant: RirVariantId::from_index(0),
                block: RirStructuredBlock {
                    stmts: vec![],
                    term: RirTerm::None,
                },
            },
            RirEnumMatchArm {
                variant: RirVariantId::from_index(0),
                block: RirStructuredBlock {
                    stmts: vec![],
                    term: RirTerm::None,
                },
            },
        ],
        Some(RirStructuredBlock {
            stmts: vec![],
            term: RirTerm::None,
        }),
    );

    assert_rir_error(program, RirVerifyErrorKind::DuplicateMatchArm);
}

#[test]
fn rir_verify_rejects_nonexhaustive_enum_match() {
    let program = enum_match_rir(
        vec![RirEnumMatchArm {
            variant: RirVariantId::from_index(0),
            block: RirStructuredBlock {
                stmts: vec![],
                term: RirTerm::None,
            },
        }],
        None,
    );

    assert_rir_error(program, RirVerifyErrorKind::MatchNotExhaustive);
}

#[test]
fn rir_verify_rejects_invalid_format_spec_for_source_type() {
    let mut program = empty_rir_function(RirType::String);
    program.types.push(RirType::Float);
    program.consts.push(RirConst {
        id: RirConstId::from_index(1),
        ty: RirTypeId::from_index(1),
        value: RirConstValue::Float(1.0),
    });
    program.functions[0]
        .body
        .stmts
        .push(RirStmt::Eval(RirRValue::Format {
            value: RirOperand::Const(RirConstId::from_index(1)),
            source_ty: RirTypeId::from_index(1),
            spec: RirFormatSpec {
                kind: RirFormatKind::Hex,
                ..RirFormatSpec::default()
            },
        }));

    assert_rir_error(program, RirVerifyErrorKind::UnsupportedRValueType);
}

#[test]
fn rir_verify_rejects_format_type_mismatch() {
    let mut program = empty_rir_function(RirType::String);
    program.types.push(RirType::Int);
    program.consts.push(RirConst {
        id: RirConstId::from_index(1),
        ty: RirTypeId::from_index(1),
        value: RirConstValue::Int(1),
    });
    program.functions[0]
        .body
        .stmts
        .push(RirStmt::Eval(RirRValue::Format {
            value: RirOperand::Const(RirConstId::from_index(1)),
            source_ty: RirTypeId::from_index(0),
            spec: RirFormatSpec::default(),
        }));

    assert_rir_error(program, RirVerifyErrorKind::UnsupportedRValueType);
}

#[test]
fn rir_verify_rejects_non_string_concat_part() {
    let mut program = empty_rir_function(RirType::String);
    program.types.push(RirType::Int);
    program.consts.push(RirConst {
        id: RirConstId::from_index(1),
        ty: RirTypeId::from_index(1),
        value: RirConstValue::Int(1),
    });
    program.functions[0]
        .body
        .stmts
        .push(RirStmt::Eval(RirRValue::StringConcat {
            parts: vec![RirOperand::Const(RirConstId::from_index(1))],
        }));

    assert_rir_error(program, RirVerifyErrorKind::UnsupportedRValueType);
}

#[test]
fn rir_verify_rejects_bad_call_arg_mode() {
    let int = RirTypeId::from_index(0);
    let mut program = empty_rir_function(RirType::Int);
    program.functions.push(RirFunction {
        id: RirFunctionId::from_index(1),
        air_id: None,
        symbol: RirSymbol::new("callee"),
        params: vec![RirParam {
            local: RirLocalId::from_index(0),
            ty: int,
            semantic: RirParamSemantic::Value,
            abi: RirParamAbi::Value,
            escape: RirParamEscape::NonEscaping,
        }],
        ret: RirReturn { ty: int },
        locals: vec![RirLocal {
            id: RirLocalId::from_index(0),
            ty: int,
            mutable: false,
            symbol: RirSymbol::new("arg"),
            initialized: true,
            payload_ref: false,
        }],
        body: RirStructuredBlock {
            stmts: vec![],
            term: RirTerm::Return(Some(RirOperand::Place(RirPlace {
                local: RirLocalId::from_index(0),
                projections: vec![],
                ty: int,
            }))),
        },
    });
    program.functions[0]
        .body
        .stmts
        .push(RirStmt::Eval(RirRValue::Call {
            callee: RirCallTarget::Function(RirFunctionId::from_index(1)),
            args: vec![RirCallArg::SharedBorrow(RirPlace {
                local: RirLocalId::from_index(0),
                projections: vec![],
                ty: int,
            })],
            ty: int,
        }));

    assert_rir_error(program, RirVerifyErrorKind::CallArgMode);
}

#[test]
fn rir_verify_rejects_bad_ids() {
    let mut program = empty_rir_function(RirType::Int);
    program.entry = Some(RirFunctionId::from_index(9));

    assert_rir_error(program, RirVerifyErrorKind::BadId);
}

#[test]
fn rir_verify_rejects_return_type_mismatch() {
    let mut program = empty_rir_function(RirType::Int);
    let bool_ty = RirTypeId::from_index(1);
    program.types.push(RirType::Bool);
    program.consts.push(RirConst {
        id: RirConstId::from_index(1),
        ty: bool_ty,
        value: RirConstValue::Bool(true),
    });
    program.functions[0].body.term =
        RirTerm::Return(Some(RirOperand::Const(RirConstId::from_index(1))));

    let errors = rir::verify(&program).expect_err("verified return mismatch");
    assert!(
        errors
            .iter()
            .any(|error| matches!(error.kind, RirVerifyErrorKind::TypeMismatch { .. }))
    );
}

#[test]
fn rir_verify_accepts_dataref_type_metadata() {
    let program = dataref_metadata_rir();

    rir::verify(&program).expect("dataref metadata should verify");
}

#[test]
fn rir_verify_rejects_bad_dataref_type_id() {
    let mut program = dataref_metadata_rir();
    program.types[1] = RirType::DataRef(RirDataRefId::from_index(1));

    assert_rir_error(program, RirVerifyErrorKind::BadId);
}

#[test]
fn rir_verify_rejects_bad_dataref_table_id() {
    let mut program = dataref_metadata_rir();
    program.datarefs[0].id = RirDataRefId::from_index(1);

    assert_rir_error(program, RirVerifyErrorKind::BadId);
}

#[test]
fn rir_verify_rejects_duplicate_dataref_symbols() {
    let mut program = dataref_metadata_rir();
    program.datarefs.push(RirDataRef {
        id: RirDataRefId::from_index(1),
        air_id: air::AggregateId::from_index(1),
        symbol: RirSymbol::new("NodeStorage"),
        display: RirSymbol::new("Other"),
        cycle_capable: false,
        fields: vec![],
    });

    assert_rir_error(program, RirVerifyErrorKind::DuplicateSymbol);
}

#[test]
fn rir_verify_rejects_bad_dataref_field_id() {
    let mut program = dataref_metadata_rir();
    program.datarefs[0].fields[0].id = RirFieldId::from_index(1);

    assert_rir_error(program, RirVerifyErrorKind::BadId);
}

#[test]
fn rir_verify_rejects_bad_dataref_field_type() {
    let mut program = dataref_metadata_rir();
    program.datarefs[0].fields[0].ty = RirTypeId::from_index(9);

    assert_rir_error(program, RirVerifyErrorKind::BadId);
}

#[test]
fn dataref_place_descriptor_inventory_dedups_same_projection() {
    let mut program =
        dataref_projection_mut_place_call_rir(valid_dataref_projection_mut_place_arg());
    let stmt = program.functions[0].body.stmts[0].clone();
    program.functions[0].body.stmts.push(stmt);

    let descriptors = DataRefPlaceDescriptors::build(&program);

    assert_eq!(descriptors.all().len(), 1);
    assert_eq!(descriptors.all()[0].symbol, "anvP0_Node_value_place");
}

#[test]
fn dataref_place_descriptor_inventory_separates_different_projections() {
    let mut program =
        dataref_projection_mut_place_call_rir(valid_dataref_projection_mut_place_arg());
    program.functions[0]
        .body
        .stmts
        .push(RirStmt::Eval(RirRValue::Call {
            callee: RirCallTarget::Function(RirFunctionId::from_index(1)),
            args: vec![RirCallArg::MutPlace(
                nested_dataref_projection_mut_place_arg(),
            )],
            ty: RirTypeId::from_index(4),
        }));

    let descriptors = DataRefPlaceDescriptors::build(&program);
    let symbols = descriptors
        .all()
        .iter()
        .map(|descriptor| descriptor.symbol.as_str())
        .collect::<Vec<_>>();

    assert_eq!(
        symbols,
        ["anvP0_Node_value_place", "anvP1_Node_point_x_place"]
    );
}

#[test]
fn dataref_place_descriptor_inventory_finds_call_arg_descriptor() {
    let program = dataref_projection_mut_place_call_rir(valid_dataref_projection_mut_place_arg());
    let descriptors = DataRefPlaceDescriptors::build(&program);
    let RirStmt::Eval(RirRValue::Call { args, .. }) = &program.functions[0].body.stmts[0] else {
        unreachable!();
    };
    let RirCallArg::MutPlace(RirMutPlaceArg::DataRefProjection {
        dataref,
        projections,
        ty,
        ..
    }) = &args[0]
    else {
        unreachable!();
    };

    let descriptor = descriptors
        .find(*dataref, projections, *ty)
        .expect("descriptor missing for dataref arg");

    assert_eq!(descriptor.symbol, "anvP0_Node_value_place");
}

#[test]
fn emit_dataref_projection_mut_place_descriptor_and_call_arg() {
    let source = emit::emit(
        &rir::verify(&dataref_projection_mut_place_call_rir(
            valid_dataref_projection_mut_place_arg(),
        ))
        .expect("RIR verify failed"),
    )
    .into_string();

    for needle in [
        "struct anvP0_Node_value_place<'cx>",
        "heap_type: anvyx_runtime::HeapType<'cx, NodeStorage<'cx>>",
        "impl<'cx> anvyx_runtime::DataRefPlaceOps<'cx, i64> for anvP0_Node_value_place<'cx>",
        "try_with_erased(object, self.heap_type, |storage: &NodeStorage<'cx>| f(&storage.value))",
        "try_with_erased_mut(object, self.heap_type, |storage: &mut NodeStorage<'cx>| f(&mut storage.value))",
        "let __anv_dataref_place_object_0 = ctx.heap().erase(&node).map_err(anvyx_runtime::heap_access_error)?;",
        "let __anv_dataref_place_ops_0 = anvP0_Node_value_place { heap_type: ctx._types.NodeHeapType };",
        "sink(ctx, anvyx_runtime::MutPlace::dataref(__anv_dataref_place_object_0, &__anv_dataref_place_ops_0))",
    ] {
        assert!(source.contains(needle), "missing {needle}");
    }
    assert!(!source.contains("\"value\""));
}

#[test]
fn emit_nested_dataref_projection_mut_place_descriptor_path() {
    let source = emit::emit(
        &rir::verify(&dataref_projection_mut_place_call_rir(
            nested_dataref_projection_mut_place_arg(),
        ))
        .expect("RIR verify failed"),
    )
    .into_string();

    assert!(source.contains("struct anvP0_Node_point_x_place<'cx>"));
    assert!(source.contains("f(&storage.point.x)"));
    assert!(source.contains("f(&mut storage.point.x)"));
}

#[test]
fn rir_verify_accepts_direct_scalar_dataref_projection_mut_place_arg() {
    rir::verify(&dataref_projection_mut_place_call_rir(
        valid_dataref_projection_mut_place_arg(),
    ))
    .expect("RIR rejected dataref projection mut-place arg");
}

#[test]
fn rir_verify_accepts_nested_inline_scalar_dataref_projection_mut_place_arg() {
    rir::verify(&dataref_projection_mut_place_call_rir(
        nested_dataref_projection_mut_place_arg(),
    ))
    .expect("RIR rejected nested dataref projection mut-place arg");
}

#[test]
fn rir_verify_rejects_dataref_projection_mut_place_arg_wrong_object_type() {
    let arg = edit_dataref_projection_mut_place_arg(|object, _, _, _| {
        *object = RirOperand::Const(RirConstId::from_index(0));
    });

    assert_rir_error(
        dataref_projection_mut_place_call_rir(arg),
        RirVerifyErrorKind::UnsupportedRValueType,
    );
}

#[test]
fn rir_verify_rejects_dataref_projection_mut_place_arg_wrong_dataref_id() {
    let arg = edit_dataref_projection_mut_place_arg(|_, dataref, _, _| {
        *dataref = RirDataRefId::from_index(1);
    });

    assert_rir_error(
        dataref_projection_mut_place_call_rir(arg),
        RirVerifyErrorKind::UnsupportedRValueType,
    );
}

#[test]
fn rir_verify_rejects_dataref_projection_mut_place_arg_wrong_payload_type() {
    let arg = edit_dataref_projection_mut_place_arg(|_, _, _, ty| {
        *ty = RirTypeId::from_index(5);
    });

    assert_rir_error(
        dataref_projection_mut_place_call_rir(arg),
        RirVerifyErrorKind::TypeMismatch {
            expected: RirTypeId::from_index(0),
            found: RirTypeId::from_index(5),
        },
    );
}

#[test]
fn rir_verify_rejects_empty_dataref_projection_mut_place_arg() {
    let arg = edit_dataref_projection_mut_place_arg(|_, _, projections, _| projections.clear());

    assert_rir_error(
        dataref_projection_mut_place_call_rir(arg),
        RirVerifyErrorKind::UnsupportedRValueType,
    );
}

#[test]
fn rir_verify_rejects_index_dataref_projection_mut_place_arg() {
    let arg = dataref_projection_mut_place_arg(
        vec![
            RirProjection::Field(RirFieldId::from_index(4)),
            RirProjection::Index(RirLocalId::from_index(1)),
        ],
        RirTypeId::from_index(0),
    );

    assert_rir_error(
        dataref_projection_mut_place_call_rir(arg),
        RirVerifyErrorKind::UnsupportedRValueType,
    );
}

#[test]
fn rir_verify_rejects_nested_dataref_handle_projection_mut_place_arg() {
    let arg = dataref_projection_mut_place_arg(
        vec![
            RirProjection::Field(RirFieldId::from_index(0)),
            RirProjection::Field(RirFieldId::from_index(1)),
        ],
        RirTypeId::from_index(1),
    );

    assert_rir_error(
        dataref_projection_mut_place_call_rir(arg),
        RirVerifyErrorKind::UnsupportedRValueType,
    );
}

#[test]
fn rir_verify_accepts_dataref_handle_projection_mut_place_arg() {
    let arg = dataref_projection_mut_place_arg(
        vec![RirProjection::Field(RirFieldId::from_index(3))],
        RirTypeId::from_index(1),
    );
    let mut program = dataref_projection_mut_place_call_rir(arg);
    program.functions[1].params[0].ty = RirTypeId::from_index(1);
    program.functions[1].locals[0].ty = RirTypeId::from_index(1);

    rir::verify(&program).expect("RIR rejected dataref handle mut-place arg");
}

#[test]
fn rir_verify_rejects_aggregate_dataref_projection_mut_place_arg_payloads() {
    for (field, ty) in [
        (RirFieldId::from_index(0), RirTypeId::from_index(2)),
        (RirFieldId::from_index(4), RirTypeId::from_index(3)),
        (RirFieldId::from_index(5), RirTypeId::from_index(6)),
        (RirFieldId::from_index(6), RirTypeId::from_index(7)),
        (RirFieldId::from_index(7), RirTypeId::from_index(8)),
        (RirFieldId::from_index(8), RirTypeId::from_index(9)),
    ] {
        assert_rir_error(
            dataref_projection_mut_place_call_rir(dataref_projection_mut_place_arg(
                vec![RirProjection::Field(field)],
                ty,
            )),
            RirVerifyErrorKind::UnsupportedRValueType,
        );
    }
}

#[test]
fn rir_verify_rejects_uninitialized_dataref_projection_mut_place_object() {
    let mut program =
        dataref_projection_mut_place_call_rir(valid_dataref_projection_mut_place_arg());
    program.functions[0].locals[0].initialized = false;

    assert_rir_error(
        program,
        RirVerifyErrorKind::UninitializedLocal(RirLocalId::from_index(0)),
    );
}

#[test]
fn rir_verify_rejects_dataref_projection_mut_place_arg_native_boundary() {
    let mut program =
        dataref_projection_mut_place_call_rir(valid_dataref_projection_mut_place_arg());
    program.functions[1].params[0].semantic = RirParamSemantic::MutBorrow;
    program.functions[1].params[0].abi = RirParamAbi::MutBorrow;

    assert_rir_error(program, RirVerifyErrorKind::CallArgMode);
}

#[test]
fn rir_verify_rejects_mut_place_param_dataref_projection_mut_place_object() {
    let mut program =
        dataref_projection_mut_place_call_rir(valid_dataref_projection_mut_place_arg());
    program.functions[0].params.push(RirParam {
        local: RirLocalId::from_index(0),
        ty: RirTypeId::from_index(1),
        semantic: RirParamSemantic::MutPlace,
        abi: RirParamAbi::MutPlace,
        escape: RirParamEscape::NonEscaping,
    });

    assert_rir_error(program, RirVerifyErrorKind::UnsupportedRValueType);
}

#[test]
fn rir_verify_accepts_dataref_get_set_projection_ops() {
    let program = dataref_access_rir(vec![
        RirStmt::Init {
            local: RirLocalId::from_index(2),
            value: RirRValue::DataRefGet {
                object: RirOperand::Place(dataref_access_place(0, 1)),
                dataref: RirDataRefId::from_index(0),
                projections: vec![RirProjection::Field(RirFieldId::from_index(2))],
                ty: RirTypeId::from_index(0),
            },
        },
        RirStmt::DataRefSet {
            object: RirOperand::Place(dataref_access_place(0, 1)),
            dataref: RirDataRefId::from_index(0),
            projections: vec![
                RirProjection::Field(RirFieldId::from_index(0)),
                RirProjection::Field(RirFieldId::from_index(0)),
            ],
            value: RirOperand::Const(RirConstId::from_index(0)),
        },
        RirStmt::DataRefSet {
            object: RirOperand::Place(dataref_access_place(0, 1)),
            dataref: RirDataRefId::from_index(0),
            projections: vec![
                RirProjection::Field(RirFieldId::from_index(4)),
                RirProjection::Index(RirLocalId::from_index(1)),
            ],
            value: RirOperand::Const(RirConstId::from_index(0)),
        },
    ]);

    rir::verify(&program).expect("dataref access ops should verify");
}

#[test]
fn rir_verify_rejects_dataref_get_bad_object_type() {
    let mut program = dataref_access_rir(vec![RirStmt::Init {
        local: RirLocalId::from_index(2),
        value: RirRValue::DataRefGet {
            object: RirOperand::Const(RirConstId::from_index(0)),
            dataref: RirDataRefId::from_index(0),
            projections: vec![RirProjection::Field(RirFieldId::from_index(2))],
            ty: RirTypeId::from_index(0),
        },
    }]);
    program.functions[0].ret.ty = RirTypeId::from_index(0);

    assert_rir_error(program, RirVerifyErrorKind::UnsupportedRValueType);
}

#[test]
fn rir_verify_rejects_dataref_get_bad_dataref_id() {
    let program = dataref_access_rir(vec![RirStmt::Init {
        local: RirLocalId::from_index(2),
        value: RirRValue::DataRefGet {
            object: RirOperand::Place(dataref_access_place(0, 1)),
            dataref: RirDataRefId::from_index(1),
            projections: vec![RirProjection::Field(RirFieldId::from_index(2))],
            ty: RirTypeId::from_index(0),
        },
    }]);

    assert_rir_error(program, RirVerifyErrorKind::BadId);
}

#[test]
fn rir_verify_rejects_dataref_get_bad_field_id() {
    let program = dataref_access_rir(vec![RirStmt::Init {
        local: RirLocalId::from_index(2),
        value: RirRValue::DataRefGet {
            object: RirOperand::Place(dataref_access_place(0, 1)),
            dataref: RirDataRefId::from_index(0),
            projections: vec![RirProjection::Field(RirFieldId::from_index(9))],
            ty: RirTypeId::from_index(0),
        },
    }]);

    assert_rir_error(program, RirVerifyErrorKind::BadId);
}

#[test]
fn rir_verify_rejects_dataref_get_final_type_mismatch() {
    let program = dataref_access_rir(vec![RirStmt::Eval(RirRValue::DataRefGet {
        object: RirOperand::Place(dataref_access_place(0, 1)),
        dataref: RirDataRefId::from_index(0),
        projections: vec![RirProjection::Field(RirFieldId::from_index(2))],
        ty: RirTypeId::from_index(5),
    })]);

    assert_rir_type_error(program);
}

#[test]
fn rir_verify_rejects_dataref_get_nested_dataref_crossing() {
    let program = dataref_access_rir(vec![RirStmt::Init {
        local: RirLocalId::from_index(2),
        value: RirRValue::DataRefGet {
            object: RirOperand::Place(dataref_access_place(0, 1)),
            dataref: RirDataRefId::from_index(0),
            projections: vec![
                RirProjection::Field(RirFieldId::from_index(3)),
                RirProjection::Field(RirFieldId::from_index(2)),
            ],
            ty: RirTypeId::from_index(0),
        },
    }]);

    assert_rir_error(program, RirVerifyErrorKind::UnsupportedRValueType);
}

#[test]
fn rir_verify_rejects_dataref_set_value_mismatch() {
    let program = dataref_access_rir(vec![RirStmt::DataRefSet {
        object: RirOperand::Place(dataref_access_place(0, 1)),
        dataref: RirDataRefId::from_index(0),
        projections: vec![RirProjection::Field(RirFieldId::from_index(2))],
        value: RirOperand::Const(RirConstId::from_index(1)),
    }]);

    assert_rir_type_error(program);
}

#[test]
fn rir_verify_accepts_string_value_abi() {
    let mut program = empty_rir_function(RirType::String);
    program.types.push(RirType::Void);
    program.consts.clear();
    program.functions[0].ret.ty = RirTypeId::from_index(1);
    program.functions[0].locals.push(RirLocal {
        id: RirLocalId::from_index(0),
        ty: RirTypeId::from_index(0),
        mutable: false,
        symbol: RirSymbol::new("s"),
        initialized: true,
        payload_ref: false,
    });
    program.functions[0].params.push(RirParam {
        local: RirLocalId::from_index(0),
        ty: RirTypeId::from_index(0),
        semantic: RirParamSemantic::Value,
        abi: RirParamAbi::Value,
        escape: RirParamEscape::NonEscaping,
    });

    rir::verify(&program).expect("RIR rejected string value ABI");
}

#[test]
fn rir_verify_accepts_loop_break_continue() {
    let mut program = empty_rir_function(RirType::Void);
    program.types.push(RirType::Bool);
    program.consts[0].ty = RirTypeId::from_index(1);
    program.consts[0].value = RirConstValue::Bool(true);
    let loop_id = RirLoopId::from_index(0);
    program.functions[0].body.stmts.push(RirStmt::Loop(RirLoop {
        id: loop_id,
        body: RirStructuredBlock {
            stmts: vec![RirStmt::If(RirIf {
                cond: RirOperand::Const(RirConstId::from_index(0)),
                then_block: RirStructuredBlock {
                    stmts: vec![],
                    term: RirTerm::Continue(loop_id),
                },
                else_block: Some(RirStructuredBlock {
                    stmts: vec![],
                    term: RirTerm::Break(loop_id),
                }),
            })],
            term: RirTerm::None,
        },
    }));

    rir::verify(&program).expect("RIR rejected loop break/continue");
}

#[test]
fn rir_verify_rejects_break_continue_outside_loop() {
    let loop_id = RirLoopId::from_index(0);
    let mut break_program = empty_rir_function(RirType::Void);
    break_program.functions[0].body.term = RirTerm::Break(loop_id);
    assert_rir_error(break_program, RirVerifyErrorKind::BreakOutsideLoop(loop_id));

    let mut continue_program = empty_rir_function(RirType::Void);
    continue_program.functions[0].body.term = RirTerm::Continue(loop_id);
    assert_rir_error(
        continue_program,
        RirVerifyErrorKind::ContinueOutsideLoop(loop_id),
    );
}

#[test]
fn rir_verify_rejects_supported_type_semantic_abi_mismatch() {
    let mut program = empty_rir_function(RirType::Int);
    program.functions[0].locals.push(RirLocal {
        id: RirLocalId::from_index(0),
        ty: RirTypeId::from_index(0),
        mutable: false,
        symbol: RirSymbol::new("x"),
        initialized: true,
        payload_ref: false,
    });
    program.functions[0].params.push(RirParam {
        local: RirLocalId::from_index(0),
        ty: RirTypeId::from_index(0),
        semantic: RirParamSemantic::Value,
        abi: RirParamAbi::SharedBorrow,
        escape: RirParamEscape::NonEscaping,
    });

    assert_rir_error(program, RirVerifyErrorKind::UnsupportedAbi);
}

#[test]
fn rir_verify_rejects_bad_extern_signature() {
    let mut program = empty_rir_function(RirType::Int);
    program.types.push(RirType::String);
    program.types.push(RirType::Void);
    program.externs.push(RirExtern {
        id: RirExternId::from_index(0),
        symbol: RirSymbol::new("bad_println"),
        kind: RirExternKind::Native(rir::RirNativeExtern {
            path: vec!["host".to_string(), "println".to_string()],
            abi: anvyx_runtime::RustExternAbi {
                params: vec![anvyx_runtime::RustParamAbi::Borrow(
                    anvyx_runtime::ExternTypeExpr::String,
                )],
                ret: anvyx_runtime::RustReturnAbi::Void,
                fallible: false,
                support: anvyx_runtime::RustAbiSupport::Direct,
            },
        }),
        params: vec![RirExternParam {
            ty: RirTypeId::from_index(0),
            semantic: RirParamSemantic::Value,
            abi: RirParamAbi::Value,
        }],
        ret: RirTypeId::from_index(2),
    });

    assert_rir_error(program, RirVerifyErrorKind::UnsupportedRValueType);
}

#[test]
fn rir_verify_rejects_native_extern_param_type_mismatch() {
    let mut program = empty_rir_function(RirType::Int);
    program.types.push(RirType::Void);
    program.externs.push(RirExtern {
        id: RirExternId::from_index(0),
        symbol: RirSymbol::new("bad_bool"),
        kind: RirExternKind::Native(rir::RirNativeExtern {
            path: vec!["host".to_string(), "bad_bool".to_string()],
            abi: anvyx_runtime::RustExternAbi {
                params: vec![anvyx_runtime::RustParamAbi::Value(
                    anvyx_runtime::ExternTypeExpr::Bool,
                )],
                ret: anvyx_runtime::RustReturnAbi::Void,
                fallible: false,
                support: anvyx_runtime::RustAbiSupport::Direct,
            },
        }),
        params: vec![RirExternParam {
            ty: RirTypeId::from_index(0),
            semantic: RirParamSemantic::Value,
            abi: RirParamAbi::Value,
        }],
        ret: RirTypeId::from_index(1),
    });

    assert_rir_error(program, RirVerifyErrorKind::UnsupportedRValueType);
}

#[test]
fn rir_verify_accepts_native_core_option_return() {
    let program = native_option_return_program(Some(RirCoreEnumKind::Option));

    rir::verify(&program).expect("core option return should verify");
}

#[test]
fn rir_verify_rejects_native_option_return_for_non_core_option_shape() {
    let program = native_option_return_program(None);

    assert_rir_error(program, RirVerifyErrorKind::UnsupportedRValueType);
}

#[test]
fn rir_verify_rejects_native_extern_return_type_mismatch() {
    let mut program = empty_rir_function(RirType::Int);
    program.externs.push(RirExtern {
        id: RirExternId::from_index(0),
        symbol: RirSymbol::new("bad_return"),
        kind: RirExternKind::Native(rir::RirNativeExtern {
            path: vec!["host".to_string(), "bad_return".to_string()],
            abi: anvyx_runtime::RustExternAbi {
                params: vec![],
                ret: anvyx_runtime::RustReturnAbi::Value(anvyx_runtime::ExternTypeExpr::Bool),
                fallible: false,
                support: anvyx_runtime::RustAbiSupport::Direct,
            },
        }),
        params: vec![],
        ret: RirTypeId::from_index(0),
    });

    assert_rir_error(program, RirVerifyErrorKind::UnsupportedRValueType);
}

#[test]
fn rir_verify_rejects_param_init_and_reinit() {
    let mut program = empty_rir_function(RirType::Int);
    program.functions[0].locals.push(RirLocal {
        id: RirLocalId::from_index(0),
        ty: RirTypeId::from_index(0),
        mutable: false,
        symbol: RirSymbol::new("p"),
        initialized: true,
        payload_ref: false,
    });
    program.functions[0].params.push(RirParam {
        local: RirLocalId::from_index(0),
        ty: RirTypeId::from_index(0),
        semantic: RirParamSemantic::Value,
        abi: RirParamAbi::Value,
        escape: RirParamEscape::NonEscaping,
    });
    program.functions[0].body.stmts.push(RirStmt::Init {
        local: RirLocalId::from_index(0),
        value: RirRValue::Use(RirOperand::Const(RirConstId::from_index(0))),
    });

    assert_rir_error(program, RirVerifyErrorKind::InitParamLocal);
}

#[test]
fn rir_verify_accepts_mut_borrow_abi() {
    let mut program = empty_rir_function(RirType::Void);
    program.consts.clear();
    program.types.push(RirType::Int);
    program.functions[0].locals.push(RirLocal {
        id: RirLocalId::from_index(0),
        ty: RirTypeId::from_index(1),
        mutable: true,
        symbol: RirSymbol::new("p"),
        initialized: true,
        payload_ref: false,
    });
    program.functions[0].params.push(RirParam {
        local: RirLocalId::from_index(0),
        ty: RirTypeId::from_index(1),
        semantic: RirParamSemantic::MutBorrow,
        abi: RirParamAbi::MutBorrow,
        escape: RirParamEscape::NonEscaping,
    });

    rir::verify(&program).expect("RIR rejected mut borrow ABI");
}

fn valid_stack_cell_decl() -> RirCellDecl {
    RirCellDecl {
        id: RirCellId::from_index(0),
        owner: RirFunctionId::from_index(0),
        source_local: RirLocalId::from_index(0),
        payload_ty: RirTypeId::from_index(1),
        storage: RirCellStorage::StackScoped,
        symbol: RirSymbol::new("__cell0"),
    }
}

fn valid_heap_cell_decl() -> RirCellDecl {
    RirCellDecl {
        storage: RirCellStorage::Heap,
        ..valid_stack_cell_decl()
    }
}

fn stack_cell_rir_with(edit: impl FnOnce(&mut RirCellDecl)) -> RirProgram {
    let mut cell = valid_stack_cell_decl();
    edit(&mut cell);
    stack_cell_rir(cell)
}

fn owner_cell_ref() -> RirCellRef {
    RirCellRef::Owner(RirCellId::from_index(0))
}

fn valid_stack_cell_arg() -> RirLambdaCaptureArg {
    RirLambdaCaptureArg::StackCell {
        cell: owner_cell_ref(),
    }
}

fn valid_heap_env_lambda_rir() -> RirProgram {
    let void = RirTypeId::from_index(0);
    let int = RirTypeId::from_index(1);
    let sig = RirLambdaSigId::from_index(0);
    let lambda_ty = RirTypeId::from_index(2);
    let lambda = RirLambdaId::from_index(0);
    let env = RirLambdaEnvId::from_index(0);
    let target = RirFunctionId::from_index(0);
    let maker = RirFunctionId::from_index(1);
    let capture = RirLocalId::from_index(0);
    let source = RirLocalId::from_index(0);
    let f = RirLocalId::from_index(1);

    RirProgram {
        types: vec![RirType::Void, RirType::Int, RirType::Lambda(sig)],
        lambda_sigs: vec![RirLambdaSig {
            id: sig,
            params: vec![],
            ret: void,
        }],
        lambdas: vec![RirLambda {
            id: lambda,
            source: RirLambdaSource::Lambda(air::LambdaId::from_index(0)),
            function: target,
            sig,
            escape: RirLambdaEscape::Escaping,
            storage: RirLambdaStorage::HeapEnv { env },
            captures: vec![RirLambdaCapture {
                ty: int,
                semantic: RirParamSemantic::Value,
                abi: RirParamAbi::Value,
                kind: RirLambdaCaptureKind::Param,
            }],
        }],
        lambda_envs: vec![RirLambdaEnvLayout {
            id: env,
            lambda,
            symbol: RirSymbol::new("LambdaEnv0"),
            fields: vec![RirLambdaEnvField {
                ty: int,
                symbol: RirSymbol::new("c0"),
                kind: RirLambdaEnvFieldKind::Value,
            }],
        }],
        functions: vec![
            RirFunction {
                id: target,
                air_id: None,
                symbol: RirSymbol::new("target"),
                params: vec![RirParam {
                    local: capture,
                    ty: int,
                    semantic: RirParamSemantic::Value,
                    abi: RirParamAbi::Value,
                    escape: RirParamEscape::NonEscaping,
                }],
                ret: RirReturn { ty: void },
                locals: vec![RirLocal {
                    id: capture,
                    ty: int,
                    mutable: false,
                    symbol: RirSymbol::new("capture"),
                    initialized: true,
                    payload_ref: false,
                }],
                body: RirStructuredBlock::default(),
            },
            RirFunction {
                id: maker,
                air_id: None,
                symbol: RirSymbol::new("maker"),
                params: vec![],
                ret: RirReturn { ty: void },
                locals: vec![
                    RirLocal {
                        id: source,
                        ty: int,
                        mutable: false,
                        symbol: RirSymbol::new("source"),
                        initialized: true,
                        payload_ref: false,
                    },
                    RirLocal {
                        id: f,
                        ty: lambda_ty,
                        mutable: false,
                        symbol: RirSymbol::new("f"),
                        initialized: false,
                        payload_ref: false,
                    },
                ],
                body: RirStructuredBlock {
                    stmts: vec![RirStmt::Init {
                        local: f,
                        value: RirRValue::Lambda {
                            lambda,
                            captures: vec![RirLambdaCaptureArg::Readonly {
                                value: RirOperand::Place(RirPlace {
                                    local: source,
                                    projections: vec![],
                                    ty: int,
                                }),
                            }],
                            ty: lambda_ty,
                        },
                    }],
                    term: RirTerm::Return(None),
                },
            },
        ],
        ..RirProgram::default()
    }
}

fn zero_env_escaping_lambda_rir() -> RirProgram {
    let void = RirTypeId::from_index(0);
    let sig = RirLambdaSigId::from_index(0);
    let lambda = RirLambdaId::from_index(0);
    let target = RirFunctionId::from_index(0);

    RirProgram {
        types: vec![RirType::Void, RirType::Lambda(sig)],
        lambda_sigs: vec![RirLambdaSig {
            id: sig,
            params: vec![],
            ret: void,
        }],
        lambdas: vec![RirLambda {
            id: lambda,
            source: RirLambdaSource::Lambda(air::LambdaId::from_index(0)),
            function: target,
            sig,
            escape: RirLambdaEscape::Escaping,
            storage: RirLambdaStorage::ZeroEnv,
            captures: vec![],
        }],
        functions: vec![RirFunction {
            id: target,
            air_id: None,
            symbol: RirSymbol::new("target"),
            params: vec![],
            ret: RirReturn { ty: void },
            locals: vec![],
            body: RirStructuredBlock::default(),
        }],
        ..RirProgram::default()
    }
}

fn valid_stack_cell_lambda_rir() -> RirProgram {
    stack_cell_lambda_rir(
        RirLambdaEscape::NonEscaping,
        RirCellId::from_index(0),
        RirTypeId::from_index(1),
        RirParamSemantic::StackCell,
        RirParamAbi::StackCell,
        valid_stack_cell_arg(),
    )
}

fn valid_heap_cell_lambda_rir() -> RirProgram {
    let void = RirTypeId::from_index(0);
    let int = RirTypeId::from_index(1);
    let sig = RirLambdaSigId::from_index(0);
    let lambda_ty = RirTypeId::from_index(2);
    let lambda = RirLambdaId::from_index(0);
    let env = RirLambdaEnvId::from_index(0);
    let owner = RirFunctionId::from_index(0);
    let target = RirFunctionId::from_index(1);
    let source = RirLocalId::from_index(0);
    let f = RirLocalId::from_index(1);

    RirProgram {
        types: vec![RirType::Void, RirType::Int, RirType::Lambda(sig)],
        cells: vec![valid_heap_cell_decl()],
        lambda_sigs: vec![RirLambdaSig {
            id: sig,
            params: vec![],
            ret: void,
        }],
        lambdas: vec![RirLambda {
            id: lambda,
            source: RirLambdaSource::Lambda(air::LambdaId::from_index(0)),
            function: target,
            sig,
            escape: RirLambdaEscape::Escaping,
            storage: RirLambdaStorage::HeapEnv { env },
            captures: vec![RirLambdaCapture {
                ty: int,
                semantic: RirParamSemantic::HeapCell,
                abi: RirParamAbi::HeapCell,
                kind: RirLambdaCaptureKind::HeapCell {
                    cell: RirCellId::from_index(0),
                },
            }],
        }],
        lambda_envs: vec![RirLambdaEnvLayout {
            id: env,
            lambda,
            symbol: RirSymbol::new("LambdaEnv0"),
            fields: vec![RirLambdaEnvField {
                ty: int,
                symbol: RirSymbol::new("c0"),
                kind: RirLambdaEnvFieldKind::HeapCell {
                    cell: RirCellId::from_index(0),
                },
            }],
        }],
        functions: vec![
            RirFunction {
                id: owner,
                air_id: None,
                symbol: RirSymbol::new("owner"),
                params: vec![],
                ret: RirReturn { ty: void },
                locals: vec![
                    RirLocal {
                        id: source,
                        ty: int,
                        mutable: true,
                        symbol: RirSymbol::new("source"),
                        initialized: true,
                        payload_ref: false,
                    },
                    RirLocal {
                        id: f,
                        ty: lambda_ty,
                        mutable: false,
                        symbol: RirSymbol::new("f"),
                        initialized: false,
                        payload_ref: false,
                    },
                ],
                body: RirStructuredBlock {
                    stmts: vec![
                        RirStmt::CellInit {
                            cell: RirCellRef::Owner(RirCellId::from_index(0)),
                            value: RirRValue::Use(RirOperand::Const(RirConstId::from_index(0))),
                        },
                        RirStmt::Init {
                            local: f,
                            value: RirRValue::Lambda {
                                lambda,
                                captures: vec![RirLambdaCaptureArg::HeapCell {
                                    cell: RirCellRef::Owner(RirCellId::from_index(0)),
                                }],
                                ty: lambda_ty,
                            },
                        },
                    ],
                    term: RirTerm::Return(None),
                },
            },
            RirFunction {
                id: target,
                air_id: None,
                symbol: RirSymbol::new("target"),
                params: vec![RirParam {
                    local: source,
                    ty: int,
                    semantic: RirParamSemantic::HeapCell,
                    abi: RirParamAbi::HeapCell,
                    escape: RirParamEscape::NonEscaping,
                }],
                ret: RirReturn { ty: void },
                locals: vec![RirLocal {
                    id: source,
                    ty: int,
                    mutable: false,
                    symbol: RirSymbol::new("cell"),
                    initialized: true,
                    payload_ref: false,
                }],
                body: RirStructuredBlock {
                    stmts: vec![RirStmt::Eval(RirRValue::CellGetCopy {
                        cell: RirCellRef::Capture {
                            cell: RirCellId::from_index(0),
                            local: source,
                        },
                        ty: int,
                    })],
                    term: RirTerm::Return(None),
                },
            },
        ],
        consts: vec![RirConst {
            id: RirConstId::from_index(0),
            ty: int,
            value: RirConstValue::Int(1),
        }],
        ..RirProgram::default()
    }
}

fn push_int_const(program: &mut RirProgram, value: i64) -> RirConstId {
    let id = RirConstId::from_index(program.consts.len());
    program.consts.push(RirConst {
        id,
        ty: RirTypeId::from_index(1),
        value: RirConstValue::Int(value),
    });
    id
}

fn stack_cell_rir(cell: RirCellDecl) -> RirProgram {
    let void = RirTypeId::from_index(0);
    let int = RirTypeId::from_index(1);
    let owner = RirFunctionId::from_index(0);
    let source = RirLocalId::from_index(0);
    RirProgram {
        types: vec![RirType::Void, RirType::Int],
        cells: vec![cell],
        functions: vec![RirFunction {
            id: owner,
            air_id: None,
            symbol: RirSymbol::new("owner"),
            params: vec![],
            ret: RirReturn { ty: void },
            locals: vec![RirLocal {
                id: source,
                ty: int,
                mutable: true,
                symbol: RirSymbol::new("source"),
                initialized: true,
                payload_ref: false,
            }],
            body: RirStructuredBlock::default(),
        }],
        ..RirProgram::default()
    }
}

fn cell_mut_place_call_rir(cell: RirCellDecl, arg: RirMutPlaceArg) -> RirProgram {
    let mut program = stack_cell_rir(cell);
    program
        .functions
        .push(mut_place_sink_function(RirFunctionId::from_index(1)));
    let value = push_int_const(&mut program, 1);
    program.functions[0].body.stmts = vec![
        RirStmt::CellInit {
            cell: RirCellRef::Owner(RirCellId::from_index(0)),
            value: RirRValue::Use(RirOperand::Const(value)),
        },
        RirStmt::Eval(RirRValue::Call {
            callee: RirCallTarget::Function(RirFunctionId::from_index(1)),
            args: vec![RirCallArg::MutPlace(arg)],
            ty: RirTypeId::from_index(0),
        }),
    ];
    program
}

fn heap_cell_set_mut_place_param_rir() -> RirProgram {
    let mut program = stack_cell_rir(valid_heap_cell_decl());
    let int = RirTypeId::from_index(1);
    let source = RirLocalId::from_index(0);
    program.functions[0].params.push(RirParam {
        local: source,
        ty: int,
        semantic: RirParamSemantic::MutPlace,
        abi: RirParamAbi::MutPlace,
        escape: RirParamEscape::NonEscaping,
    });
    let value = push_int_const(&mut program, 1);
    program.functions[0].body.stmts = vec![
        RirStmt::CellInit {
            cell: RirCellRef::Owner(RirCellId::from_index(0)),
            value: RirRValue::Use(RirOperand::Const(value)),
        },
        RirStmt::CellSet {
            cell: RirCellRef::Owner(RirCellId::from_index(0)),
            value: RirRValue::Use(RirOperand::Place(rir_place(source, int))),
        },
    ];
    program
}

fn read_compute_write_cell_rir(cell: RirCellDecl) -> RirProgram {
    let mut program = stack_cell_rir(cell);
    let int = RirTypeId::from_index(1);
    let tmp = RirLocalId::from_index(1);
    let next = RirLocalId::from_index(2);
    program.functions[0].locals.extend([
        RirLocal {
            id: tmp,
            ty: int,
            mutable: false,
            symbol: RirSymbol::new("tmp"),
            initialized: false,
            payload_ref: false,
        },
        RirLocal {
            id: next,
            ty: int,
            mutable: false,
            symbol: RirSymbol::new("next"),
            initialized: false,
            payload_ref: false,
        },
    ]);
    let one = push_int_const(&mut program, 1);
    program.functions[0].body.stmts = vec![
        RirStmt::CellInit {
            cell: RirCellRef::Owner(RirCellId::from_index(0)),
            value: RirRValue::Use(RirOperand::Const(one)),
        },
        RirStmt::Init {
            local: tmp,
            value: RirRValue::CellGetCopy {
                cell: RirCellRef::Owner(RirCellId::from_index(0)),
                ty: int,
            },
        },
        RirStmt::Init {
            local: next,
            value: RirRValue::Binary {
                op: BinaryOp::Add,
                lhs: RirOperand::Place(RirPlace {
                    local: tmp,
                    projections: vec![],
                    ty: int,
                }),
                rhs: RirOperand::Const(one),
                ty: int,
            },
        },
        RirStmt::CellSet {
            cell: RirCellRef::Owner(RirCellId::from_index(0)),
            value: RirRValue::Use(RirOperand::Place(RirPlace {
                local: next,
                projections: vec![],
                ty: int,
            })),
        },
    ];
    program
}

fn cell_set_binary_rir(cell: RirCellDecl) -> RirProgram {
    let mut program = stack_cell_rir(cell);
    let int = RirTypeId::from_index(1);
    let one = push_int_const(&mut program, 1);
    program.functions[0].body.stmts = vec![
        RirStmt::CellInit {
            cell: RirCellRef::Owner(RirCellId::from_index(0)),
            value: RirRValue::Use(RirOperand::Const(one)),
        },
        RirStmt::CellSet {
            cell: RirCellRef::Owner(RirCellId::from_index(0)),
            value: RirRValue::Binary {
                op: BinaryOp::Add,
                lhs: RirOperand::Const(one),
                rhs: RirOperand::Const(one),
                ty: int,
            },
        },
    ];
    program
}

fn cell_set_call_rir(cell: RirCellDecl) -> RirProgram {
    let mut program = stack_cell_rir(cell);
    let int = RirTypeId::from_index(1);
    let callee = RirFunctionId::from_index(1);
    let one = push_int_const(&mut program, 1);
    program.functions[0].body.stmts = vec![
        RirStmt::CellInit {
            cell: RirCellRef::Owner(RirCellId::from_index(0)),
            value: RirRValue::Use(RirOperand::Const(one)),
        },
        RirStmt::CellSet {
            cell: RirCellRef::Owner(RirCellId::from_index(0)),
            value: RirRValue::Call {
                callee: RirCallTarget::Function(callee),
                args: vec![],
                ty: int,
            },
        },
    ];
    program.functions.push(RirFunction {
        id: callee,
        air_id: None,
        symbol: RirSymbol::new("next_value"),
        params: vec![],
        ret: RirReturn { ty: int },
        locals: vec![],
        body: RirStructuredBlock {
            stmts: vec![],
            term: RirTerm::Return(Some(RirOperand::Const(one))),
        },
    });
    program
}

fn stack_cell_lambda_rir(
    escape: RirLambdaEscape,
    cell: RirCellId,
    payload_ty: RirTypeId,
    hidden_semantic: RirParamSemantic,
    hidden_abi: RirParamAbi,
    arg: RirLambdaCaptureArg,
) -> RirProgram {
    let void = RirTypeId::from_index(0);
    let sig = RirLambdaSigId::from_index(0);
    let lambda_ty = RirTypeId::from_index(2);
    let owner = RirFunctionId::from_index(0);
    let target = RirFunctionId::from_index(1);
    let source = RirLocalId::from_index(0);
    let f = RirLocalId::from_index(1);
    RirProgram {
        types: vec![RirType::Void, RirType::Int, RirType::Lambda(sig)],
        cells: vec![valid_stack_cell_decl()],
        lambda_sigs: vec![RirLambdaSig {
            id: sig,
            params: vec![],
            ret: void,
        }],
        lambdas: vec![RirLambda {
            id: RirLambdaId::from_index(0),
            source: RirLambdaSource::Lambda(air::LambdaId::from_index(0)),
            function: target,
            sig,
            escape,
            storage: RirLambdaStorage::ScopedCaptures,
            captures: vec![RirLambdaCapture {
                ty: payload_ty,
                semantic: RirParamSemantic::StackCell,
                abi: RirParamAbi::StackCell,
                kind: RirLambdaCaptureKind::StackCell { cell },
            }],
        }],
        functions: vec![
            RirFunction {
                id: owner,
                air_id: None,
                symbol: RirSymbol::new("owner"),
                params: vec![],
                ret: RirReturn { ty: void },
                locals: vec![
                    RirLocal {
                        id: source,
                        ty: RirTypeId::from_index(1),
                        mutable: true,
                        symbol: RirSymbol::new("source"),
                        initialized: true,
                        payload_ref: false,
                    },
                    RirLocal {
                        id: f,
                        ty: lambda_ty,
                        mutable: false,
                        symbol: RirSymbol::new("f"),
                        initialized: false,
                        payload_ref: false,
                    },
                ],
                body: RirStructuredBlock {
                    stmts: vec![
                        RirStmt::CellInit {
                            cell: owner_cell_ref(),
                            value: RirRValue::Use(RirOperand::Const(RirConstId::from_index(0))),
                        },
                        RirStmt::Init {
                            local: f,
                            value: RirRValue::Lambda {
                                lambda: RirLambdaId::from_index(0),
                                captures: vec![arg],
                                ty: lambda_ty,
                            },
                        },
                    ],
                    term: RirTerm::Return(None),
                },
            },
            RirFunction {
                id: target,
                air_id: None,
                symbol: RirSymbol::new("target"),
                params: vec![RirParam {
                    local: source,
                    ty: payload_ty,
                    semantic: hidden_semantic,
                    abi: hidden_abi,
                    escape: RirParamEscape::NonEscaping,
                }],
                ret: RirReturn { ty: void },
                locals: vec![RirLocal {
                    id: source,
                    ty: payload_ty,
                    mutable: false,
                    symbol: RirSymbol::new("cell"),
                    initialized: true,
                    payload_ref: false,
                }],
                body: RirStructuredBlock {
                    stmts: vec![RirStmt::Eval(RirRValue::CellGetCopy {
                        cell: RirCellRef::Capture {
                            cell: RirCellId::from_index(0),
                            local: source,
                        },
                        ty: RirTypeId::from_index(1),
                    })],
                    term: RirTerm::Return(None),
                },
            },
        ],
        consts: vec![RirConst {
            id: RirConstId::from_index(0),
            ty: RirTypeId::from_index(1),
            value: RirConstValue::Int(1),
        }],
        ..RirProgram::default()
    }
}

fn valid_scoped_place_cell_decl() -> RirScopedPlaceCellDecl {
    RirScopedPlaceCellDecl {
        id: RirScopedPlaceCellId::from_index(0),
        owner: RirFunctionId::from_index(0),
        source_local: RirLocalId::from_index(0),
        payload_ty: RirTypeId::from_index(1),
        symbol: RirSymbol::new("__scoped0"),
    }
}

fn scoped_place_cell_rir_with(edit: impl FnOnce(&mut RirScopedPlaceCellDecl)) -> RirProgram {
    let mut cell = valid_scoped_place_cell_decl();
    edit(&mut cell);
    scoped_place_cell_rir(cell)
}

fn scoped_place_cell_rir(cell: RirScopedPlaceCellDecl) -> RirProgram {
    let void = RirTypeId::from_index(0);
    let int = RirTypeId::from_index(1);
    let owner = RirFunctionId::from_index(0);
    let source = RirLocalId::from_index(0);
    RirProgram {
        types: vec![RirType::Void, RirType::Int],
        scoped_place_cells: vec![cell],
        functions: vec![RirFunction {
            id: owner,
            air_id: None,
            symbol: RirSymbol::new("owner"),
            params: vec![RirParam {
                local: source,
                ty: int,
                semantic: RirParamSemantic::MutPlace,
                abi: RirParamAbi::MutPlace,
                escape: RirParamEscape::NonEscaping,
            }],
            ret: RirReturn { ty: void },
            locals: vec![RirLocal {
                id: source,
                ty: int,
                mutable: true,
                symbol: RirSymbol::new("source"),
                initialized: true,
                payload_ref: false,
            }],
            body: RirStructuredBlock::default(),
        }],
        ..RirProgram::default()
    }
}

fn valid_scoped_place_cell_lambda_rir() -> RirProgram {
    scoped_place_cell_lambda_rir(false)
}

fn valid_nested_scoped_place_cell_lambda_rir() -> RirProgram {
    scoped_place_cell_lambda_rir(true)
}

fn scoped_place_cell_capture() -> RirLambdaCapture {
    RirLambdaCapture {
        ty: RirTypeId::from_index(1),
        semantic: RirParamSemantic::ScopedPlaceCell,
        abi: RirParamAbi::ScopedPlaceCell,
        kind: RirLambdaCaptureKind::ScopedPlaceCell {
            cell: RirScopedPlaceCellId::from_index(0),
        },
    }
}

fn mut_place_sink_function(id: RirFunctionId) -> RirFunction {
    let source = RirLocalId::from_index(0);
    RirFunction {
        id,
        air_id: None,
        symbol: RirSymbol::new("mut_place_sink"),
        params: vec![RirParam {
            local: source,
            ty: RirTypeId::from_index(1),
            semantic: RirParamSemantic::MutPlace,
            abi: RirParamAbi::MutPlace,
            escape: RirParamEscape::NonEscaping,
        }],
        ret: RirReturn {
            ty: RirTypeId::from_index(0),
        },
        locals: vec![RirLocal {
            id: source,
            ty: RirTypeId::from_index(1),
            mutable: true,
            symbol: RirSymbol::new("p"),
            initialized: true,
            payload_ref: false,
        }],
        body: RirStructuredBlock::default(),
    }
}

fn mut_borrow_sink_function(id: RirFunctionId) -> RirFunction {
    let source = RirLocalId::from_index(0);
    RirFunction {
        id,
        air_id: None,
        symbol: RirSymbol::new("mut_borrow_sink"),
        params: vec![RirParam {
            local: source,
            ty: RirTypeId::from_index(1),
            semantic: RirParamSemantic::MutBorrow,
            abi: RirParamAbi::MutBorrow,
            escape: RirParamEscape::NonEscaping,
        }],
        ret: RirReturn {
            ty: RirTypeId::from_index(0),
        },
        locals: vec![RirLocal {
            id: source,
            ty: RirTypeId::from_index(1),
            mutable: true,
            symbol: RirSymbol::new("p"),
            initialized: true,
            payload_ref: false,
        }],
        body: RirStructuredBlock::default(),
    }
}

fn scoped_place_cell_lambda_rir(nested: bool) -> RirProgram {
    let void = RirTypeId::from_index(0);
    let int = RirTypeId::from_index(1);
    let sig = RirLambdaSigId::from_index(0);
    let lambda_ty = RirTypeId::from_index(2);
    let owner = RirFunctionId::from_index(0);
    let target = RirFunctionId::from_index(1);
    let nested_target = RirFunctionId::from_index(2);
    let source = RirLocalId::from_index(0);
    let f = RirLocalId::from_index(1);
    let mut program = RirProgram {
        types: vec![RirType::Void, RirType::Int, RirType::Lambda(sig)],
        scoped_place_cells: vec![valid_scoped_place_cell_decl()],
        lambda_sigs: vec![RirLambdaSig {
            id: sig,
            params: vec![],
            ret: void,
        }],
        lambdas: vec![RirLambda {
            id: RirLambdaId::from_index(0),
            source: RirLambdaSource::Lambda(air::LambdaId::from_index(0)),
            function: target,
            sig,
            escape: RirLambdaEscape::NonEscaping,
            storage: RirLambdaStorage::ScopedCaptures,
            captures: vec![scoped_place_cell_capture()],
        }],
        functions: vec![
            RirFunction {
                id: owner,
                air_id: None,
                symbol: RirSymbol::new("owner"),
                params: vec![RirParam {
                    local: source,
                    ty: int,
                    semantic: RirParamSemantic::MutPlace,
                    abi: RirParamAbi::MutPlace,
                    escape: RirParamEscape::NonEscaping,
                }],
                ret: RirReturn { ty: void },
                locals: vec![
                    RirLocal {
                        id: source,
                        ty: int,
                        mutable: true,
                        symbol: RirSymbol::new("source"),
                        initialized: true,
                        payload_ref: false,
                    },
                    RirLocal {
                        id: f,
                        ty: lambda_ty,
                        mutable: false,
                        symbol: RirSymbol::new("f"),
                        initialized: false,
                        payload_ref: false,
                    },
                ],
                body: RirStructuredBlock {
                    stmts: vec![RirStmt::Init {
                        local: f,
                        value: RirRValue::Lambda {
                            lambda: RirLambdaId::from_index(0),
                            captures: vec![RirLambdaCaptureArg::ScopedPlaceCell {
                                cell: RirScopedPlaceCellRef::Owner(
                                    RirScopedPlaceCellId::from_index(0),
                                ),
                            }],
                            ty: lambda_ty,
                        },
                    }],
                    term: RirTerm::Return(None),
                },
            },
            RirFunction {
                id: target,
                air_id: None,
                symbol: RirSymbol::new("target"),
                params: vec![RirParam {
                    local: source,
                    ty: int,
                    semantic: RirParamSemantic::ScopedPlaceCell,
                    abi: RirParamAbi::ScopedPlaceCell,
                    escape: RirParamEscape::NonEscaping,
                }],
                ret: RirReturn { ty: void },
                locals: vec![RirLocal {
                    id: source,
                    ty: int,
                    mutable: false,
                    symbol: RirSymbol::new("cell"),
                    initialized: true,
                    payload_ref: false,
                }],
                body: RirStructuredBlock::default(),
            },
        ],
        ..RirProgram::default()
    };
    if nested {
        program.lambdas.push(RirLambda {
            id: RirLambdaId::from_index(1),
            source: RirLambdaSource::Lambda(air::LambdaId::from_index(1)),
            function: nested_target,
            sig,
            escape: RirLambdaEscape::NonEscaping,
            storage: RirLambdaStorage::ScopedCaptures,
            captures: vec![scoped_place_cell_capture()],
        });
        program.functions[1].locals.push(RirLocal {
            id: f,
            ty: lambda_ty,
            mutable: false,
            symbol: RirSymbol::new("inner"),
            initialized: false,
            payload_ref: false,
        });
        program.functions[1].body.stmts.push(RirStmt::Init {
            local: f,
            value: RirRValue::Lambda {
                lambda: RirLambdaId::from_index(1),
                captures: vec![RirLambdaCaptureArg::ScopedPlaceCell {
                    cell: RirScopedPlaceCellRef::Capture {
                        cell: RirScopedPlaceCellId::from_index(0),
                        local: source,
                    },
                }],
                ty: lambda_ty,
            },
        });
        program.functions.push(RirFunction {
            id: nested_target,
            air_id: None,
            symbol: RirSymbol::new("nested_target"),
            params: vec![RirParam {
                local: source,
                ty: int,
                semantic: RirParamSemantic::ScopedPlaceCell,
                abi: RirParamAbi::ScopedPlaceCell,
                escape: RirParamEscape::NonEscaping,
            }],
            ret: RirReturn { ty: void },
            locals: vec![RirLocal {
                id: source,
                ty: int,
                mutable: false,
                symbol: RirSymbol::new("cell"),
                initialized: true,
                payload_ref: false,
            }],
            body: RirStructuredBlock::default(),
        });
    }
    program
}

fn scoped_capture_rir(
    mut types: Vec<RirType>,
    tuples: Vec<RirTuple>,
    capture: RirLambdaCapture,
    source_ty: RirTypeId,
    source_mutable: bool,
    arg: RirLambdaCaptureArg,
) -> RirProgram {
    let void = RirTypeId::from_index(0);
    let sig = RirLambdaSigId::from_index(0);
    let lambda_ty = RirTypeId::from_index(types.len());
    types.push(RirType::Lambda(sig));
    let lambda = RirLambdaId::from_index(0);
    let target = RirFunctionId::from_index(0);
    let maker = RirFunctionId::from_index(1);
    let source = RirLocalId::from_index(0);
    let f = RirLocalId::from_index(1);
    RirProgram {
        types,
        tuples,
        lambda_sigs: vec![RirLambdaSig {
            id: sig,
            params: vec![],
            ret: void,
        }],
        lambdas: vec![RirLambda {
            id: lambda,
            source: RirLambdaSource::Lambda(air::LambdaId::from_index(0)),
            function: target,
            sig,
            escape: RirLambdaEscape::NonEscaping,
            storage: RirLambdaStorage::ScopedCaptures,
            captures: vec![capture],
        }],
        functions: vec![
            RirFunction {
                id: target,
                air_id: None,
                symbol: RirSymbol::new("target"),
                params: vec![RirParam {
                    local: source,
                    ty: capture.ty,
                    semantic: capture.semantic,
                    abi: capture.abi,
                    escape: RirParamEscape::NonEscaping,
                }],
                ret: RirReturn { ty: void },
                locals: vec![RirLocal {
                    id: source,
                    ty: capture.ty,
                    mutable: capture.semantic == RirParamSemantic::MutBorrow,
                    symbol: RirSymbol::new("capture"),
                    initialized: true,
                    payload_ref: false,
                }],
                body: RirStructuredBlock::default(),
            },
            RirFunction {
                id: maker,
                air_id: None,
                symbol: RirSymbol::new("maker"),
                params: vec![],
                ret: RirReturn { ty: void },
                locals: vec![
                    RirLocal {
                        id: source,
                        ty: source_ty,
                        mutable: source_mutable,
                        symbol: RirSymbol::new("source"),
                        initialized: true,
                        payload_ref: false,
                    },
                    RirLocal {
                        id: f,
                        ty: lambda_ty,
                        mutable: false,
                        symbol: RirSymbol::new("f"),
                        initialized: false,
                        payload_ref: false,
                    },
                ],
                body: RirStructuredBlock {
                    stmts: vec![RirStmt::Init {
                        local: f,
                        value: RirRValue::Lambda {
                            lambda,
                            captures: vec![arg],
                            ty: lambda_ty,
                        },
                    }],
                    term: RirTerm::Return(None),
                },
            },
        ],
        ..RirProgram::default()
    }
}

fn mut_borrow_lambda_rir() -> RirProgram {
    let int = RirTypeId::from_index(1);
    scoped_capture_rir(
        vec![RirType::Void, RirType::Int],
        vec![],
        RirLambdaCapture {
            ty: int,
            semantic: RirParamSemantic::MutBorrow,
            abi: RirParamAbi::MutBorrow,
            kind: RirLambdaCaptureKind::Param,
        },
        int,
        true,
        RirLambdaCaptureArg::Scoped {
            place: RirPlace {
                local: RirLocalId::from_index(0),
                projections: vec![],
                ty: int,
            },
        },
    )
}

fn assert_rir_error(program: RirProgram, kind: RirVerifyErrorKind) {
    let errors = rir::verify(&program).expect_err("verified invalid RIR");
    assert!(errors.iter().any(|error| error.kind == kind));
}

fn assert_rir_type_error(program: RirProgram) {
    let errors = rir::verify(&program).expect_err("verified invalid RIR");
    assert!(
        errors
            .iter()
            .any(|error| matches!(error.kind, RirVerifyErrorKind::TypeMismatch { .. })),
        "missing expected type mismatch: {errors:?}"
    );
}

fn tracked_struct_tuple_payload_program() -> Program {
    let mut program = Program::default();
    let bool_ty = program.alloc_type(TypeData::Bool);
    let tuple = program.alloc_type(TypeData::Tuple(vec![bool_ty, bool_ty]));
    let module = program.alloc_module(root_module());
    let payload_id = program.alloc_aggregate(air::AggregateDecl {
        name: Ident::new("Payload"),
        module,
        kind: air::AggregateKind::Struct,
        type_args: vec![],
        const_args: vec![],
        fields: vec![FieldDecl {
            name: Ident::new("flags"),
            ty: tuple,
        }],
        cycle_capable: false,
        stringify_override: None,
    });
    program.module_mut(module).aggregates.push(payload_id);
    let payload = program.alloc_type(TypeData::Aggregate(payload_id));
    let node_id = program.alloc_aggregate(air::AggregateDecl {
        name: Ident::new("Node"),
        module,
        kind: air::AggregateKind::DataRef,
        type_args: vec![],
        const_args: vec![],
        fields: vec![FieldDecl {
            name: Ident::new("payload"),
            ty: payload,
        }],
        cycle_capable: true,
        stringify_override: None,
    });
    program.module_mut(module).aggregates.push(node_id);
    program.alloc_type(TypeData::DataRef(node_id));
    program
}

fn tuple_projection_program() -> Program {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let tuple = program.alloc_type(TypeData::Tuple(vec![int]));
    let one = program.const_arena.alloc(ConstData {
        ty: int,
        value: ConstValue::Int(1),
    });
    let module = program.alloc_module(root_module());
    let out = air::LocalId::from_index(0);
    let mut first = place(out, int);
    first.projection.push(Projection::TupleField(0));
    let func = program.alloc_function(Function {
        name: Ident::new("main"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], int),
        locals: vec![local(tuple, LocalKind::Temp)],
        body: structured_body(
            vec![Statement::Init {
                local: out,
                value: RValue::Aggregate {
                    kind: AggregateCtor::Tuple,
                    fields: vec![Operand::Const(one)],
                    ty: tuple,
                },
            }],
            air::AirTail::Return(Some(Operand::Place(first))),
        ),
    });
    program.module_mut(module).functions.push(func);
    program.set_entry(func);
    program
}

fn add_payload_struct(program: &mut RirProgram) -> RirTypeId {
    let ty = RirTypeId::from_index(program.types.len());
    let id = RirStructId::from_index(program.structs.len());
    program.types.push(RirType::Struct(id));
    program.structs.push(RirStruct {
        id,
        air_id: None,
        symbol: RirSymbol::new("Payload"),
        display: RirSymbol::new("Payload"),
        native_path: None,
        native_key: None,
        copyable: true,
        fields: vec![RirField {
            id: RirFieldId::from_index(0),
            symbol: RirSymbol::new("value"),
            ty: RirTypeId::from_index(0),
        }],
    });
    ty
}

fn dataref_metadata_rir() -> RirProgram {
    let int = RirTypeId::from_index(0);
    let node = RirTypeId::from_index(1);
    let aggregate = air::AggregateId::from_index(0);
    let mut program = RirProgram {
        types: vec![RirType::Int, RirType::DataRef(RirDataRefId::from_index(0))],
        datarefs: vec![RirDataRef {
            id: RirDataRefId::from_index(0),
            air_id: aggregate,
            symbol: RirSymbol::new("Node"),
            display: RirSymbol::new("Node"),
            cycle_capable: true,
            fields: vec![RirField {
                id: RirFieldId::from_index(0),
                symbol: RirSymbol::new("value"),
                ty: int,
            }],
        }],
        ..RirProgram::default()
    };
    program.functions.push(RirFunction {
        id: RirFunctionId::from_index(0),
        air_id: None,
        symbol: RirSymbol::new("main"),
        params: vec![],
        ret: RirReturn { ty: node },
        locals: vec![],
        body: RirStructuredBlock {
            stmts: vec![],
            term: RirTerm::Unreachable,
        },
    });
    program
}

fn dataref_projection_mut_place_arg(
    projections: Vec<RirProjection>,
    ty: RirTypeId,
) -> RirMutPlaceArg {
    RirMutPlaceArg::DataRefProjection {
        object: RirOperand::Place(dataref_access_place(0, 1)),
        dataref: RirDataRefId::from_index(0),
        projections,
        ty,
    }
}

fn valid_dataref_projection_mut_place_arg() -> RirMutPlaceArg {
    dataref_projection_mut_place_arg(
        vec![RirProjection::Field(RirFieldId::from_index(2))],
        RirTypeId::from_index(0),
    )
}

fn nested_dataref_projection_mut_place_arg() -> RirMutPlaceArg {
    dataref_projection_mut_place_arg(
        vec![
            RirProjection::Field(RirFieldId::from_index(0)),
            RirProjection::Field(RirFieldId::from_index(0)),
        ],
        RirTypeId::from_index(0),
    )
}

fn edit_dataref_projection_mut_place_arg(
    edit: impl FnOnce(&mut RirOperand, &mut RirDataRefId, &mut Vec<RirProjection>, &mut RirTypeId),
) -> RirMutPlaceArg {
    let mut arg = valid_dataref_projection_mut_place_arg();
    let RirMutPlaceArg::DataRefProjection {
        object,
        dataref,
        projections,
        ty,
    } = &mut arg
    else {
        unreachable!();
    };
    edit(object, dataref, projections, ty);
    arg
}

fn dataref_projection_mut_place_call_rir(arg: RirMutPlaceArg) -> RirProgram {
    let int = RirTypeId::from_index(0);
    let void = RirTypeId::from_index(4);
    let mut program = dataref_access_rir(vec![]);
    program.functions.push(RirFunction {
        id: RirFunctionId::from_index(1),
        air_id: None,
        symbol: RirSymbol::new("sink"),
        params: vec![RirParam {
            local: RirLocalId::from_index(0),
            ty: int,
            semantic: RirParamSemantic::MutPlace,
            abi: RirParamAbi::MutPlace,
            escape: RirParamEscape::NonEscaping,
        }],
        ret: RirReturn { ty: void },
        locals: vec![RirLocal {
            id: RirLocalId::from_index(0),
            ty: int,
            mutable: true,
            symbol: RirSymbol::new("x"),
            initialized: true,
            payload_ref: false,
        }],
        body: RirStructuredBlock::default(),
    });
    program.functions[0].body.stmts = vec![RirStmt::Eval(RirRValue::Call {
        callee: RirCallTarget::Function(RirFunctionId::from_index(1)),
        args: vec![RirCallArg::MutPlace(arg)],
        ty: void,
    })];
    program
}

fn dataref_access_rir(stmts: Vec<RirStmt>) -> RirProgram {
    let int = RirTypeId::from_index(0);
    let node = RirTypeId::from_index(1);
    let point = RirTypeId::from_index(2);
    let array = RirTypeId::from_index(3);
    let void = RirTypeId::from_index(4);
    let bool_ty = RirTypeId::from_index(5);
    let list = RirTypeId::from_index(6);
    let map = RirTypeId::from_index(7);
    let option = RirTypeId::from_index(8);
    let enm = RirTypeId::from_index(9);
    let mut program = RirProgram {
        types: vec![
            RirType::Int,
            RirType::DataRef(RirDataRefId::from_index(0)),
            RirType::Struct(RirStructId::from_index(0)),
            RirType::Array { elem: int, len: 2 },
            RirType::Void,
            RirType::Bool,
            RirType::List(int),
            RirType::Map {
                key: int,
                value: int,
            },
            RirType::Option(int),
            RirType::Enum(RirEnumId::from_index(0)),
        ],
        structs: vec![RirStruct {
            id: RirStructId::from_index(0),
            air_id: None,
            symbol: RirSymbol::new("Point"),
            display: RirSymbol::new("Point"),
            native_path: None,
            native_key: None,
            copyable: false,
            fields: vec![
                RirField {
                    id: RirFieldId::from_index(0),
                    symbol: RirSymbol::new("x"),
                    ty: int,
                },
                RirField {
                    id: RirFieldId::from_index(1),
                    symbol: RirSymbol::new("node"),
                    ty: node,
                },
            ],
        }],
        enums: vec![RirEnum {
            id: RirEnumId::from_index(0),
            air_id: None,
            core: None,
            repr: rir::RirEnumRepr::Adt,
            raw_type: None,
            symbol: RirSymbol::new("Choice"),
            display: RirSymbol::new("Choice"),
            copyable: true,
            variants: vec![RirVariant {
                id: RirVariantId::from_index(0),
                symbol: RirSymbol::new("Some"),
                display: RirSymbol::new("Some"),
                kind: RirVariantKind::Tuple,
                raw_value: None,
                fields: vec![RirField {
                    id: RirFieldId::from_index(0),
                    symbol: RirSymbol::new("0"),
                    ty: int,
                }],
            }],
        }],
        datarefs: vec![RirDataRef {
            id: RirDataRefId::from_index(0),
            air_id: air::AggregateId::from_index(0),
            symbol: RirSymbol::new("Node"),
            display: RirSymbol::new("Node"),
            cycle_capable: true,
            fields: vec![
                RirField {
                    id: RirFieldId::from_index(0),
                    symbol: RirSymbol::new("point"),
                    ty: point,
                },
                RirField {
                    id: RirFieldId::from_index(1),
                    symbol: RirSymbol::new("items"),
                    ty: array,
                },
                RirField {
                    id: RirFieldId::from_index(2),
                    symbol: RirSymbol::new("value"),
                    ty: int,
                },
                RirField {
                    id: RirFieldId::from_index(3),
                    symbol: RirSymbol::new("child"),
                    ty: node,
                },
                RirField {
                    id: RirFieldId::from_index(4),
                    symbol: RirSymbol::new("more"),
                    ty: array,
                },
                RirField {
                    id: RirFieldId::from_index(5),
                    symbol: RirSymbol::new("list"),
                    ty: list,
                },
                RirField {
                    id: RirFieldId::from_index(6),
                    symbol: RirSymbol::new("map"),
                    ty: map,
                },
                RirField {
                    id: RirFieldId::from_index(7),
                    symbol: RirSymbol::new("option"),
                    ty: option,
                },
                RirField {
                    id: RirFieldId::from_index(8),
                    symbol: RirSymbol::new("enm"),
                    ty: enm,
                },
            ],
        }],
        consts: vec![
            RirConst {
                id: RirConstId::from_index(0),
                ty: int,
                value: RirConstValue::Int(1),
            },
            RirConst {
                id: RirConstId::from_index(1),
                ty: bool_ty,
                value: RirConstValue::Bool(true),
            },
        ],
        ..RirProgram::default()
    };
    program.functions.push(RirFunction {
        id: RirFunctionId::from_index(0),
        air_id: None,
        symbol: RirSymbol::new("main"),
        params: vec![],
        ret: RirReturn { ty: void },
        locals: vec![
            RirLocal {
                id: RirLocalId::from_index(0),
                ty: node,
                mutable: true,
                symbol: RirSymbol::new("node"),
                initialized: true,
                payload_ref: false,
            },
            RirLocal {
                id: RirLocalId::from_index(1),
                ty: int,
                mutable: false,
                symbol: RirSymbol::new("index"),
                initialized: true,
                payload_ref: false,
            },
            RirLocal {
                id: RirLocalId::from_index(2),
                ty: int,
                mutable: false,
                symbol: RirSymbol::new("out"),
                initialized: false,
                payload_ref: false,
            },
        ],
        body: RirStructuredBlock {
            stmts,
            term: RirTerm::Return(None),
        },
    });
    program
}

fn dataref_access_place(local: usize, ty: usize) -> RirPlace {
    RirPlace {
        local: RirLocalId::from_index(local),
        projections: vec![],
        ty: RirTypeId::from_index(ty),
    }
}

fn enum_match_rir(
    arms: Vec<RirEnumMatchArm>,
    else_block: Option<RirStructuredBlock>,
) -> RirProgram {
    let mut program = empty_rir_function(RirType::Enum(RirEnumId::from_index(0)));
    program.consts.clear();
    program.enums.push(RirEnum {
        id: RirEnumId::from_index(0),
        air_id: None,
        core: None,
        repr: rir::RirEnumRepr::Adt,
        raw_type: None,
        symbol: RirSymbol::new("E"),
        display: RirSymbol::new("E"),
        copyable: true,
        variants: vec![
            RirVariant {
                id: RirVariantId::from_index(0),
                symbol: RirSymbol::new("A"),
                display: RirSymbol::new("A"),
                kind: RirVariantKind::Unit,
                raw_value: None,
                fields: vec![],
            },
            RirVariant {
                id: RirVariantId::from_index(1),
                symbol: RirSymbol::new("B"),
                display: RirSymbol::new("B"),
                kind: RirVariantKind::Unit,
                raw_value: None,
                fields: vec![],
            },
        ],
    });
    program.functions[0].locals.push(RirLocal {
        id: RirLocalId::from_index(0),
        ty: RirTypeId::from_index(0),
        mutable: false,
        symbol: RirSymbol::new("v0"),
        initialized: true,
        payload_ref: false,
    });
    program.functions[0].body = RirStructuredBlock {
        stmts: vec![RirStmt::EnumMatch(RirEnumMatch {
            discr: RirPlace {
                local: RirLocalId::from_index(0),
                projections: vec![],
                ty: RirTypeId::from_index(0),
            },
            arms,
            else_block,
        })],
        term: RirTerm::Unreachable,
    };
    program
}

fn valid_tuple_rvalue() -> RirRValue {
    RirRValue::Tuple {
        ty: RirTypeId::from_index(3),
        fields: vec![
            RirOperand::Const(RirConstId::from_index(0)),
            RirOperand::Const(RirConstId::from_index(1)),
        ],
    }
}

fn tuple_rir_program(value: RirRValue) -> RirProgram {
    let int = RirTypeId::from_index(0);
    let bool_ty = RirTypeId::from_index(1);
    let void = RirTypeId::from_index(2);
    let tuple = RirTypeId::from_index(3);
    RirProgram {
        types: vec![
            RirType::Int,
            RirType::Bool,
            RirType::Void,
            RirType::Tuple(RirTupleId::from_index(0)),
        ],
        tuples: vec![RirTuple {
            id: RirTupleId::from_index(0),
            symbol: RirSymbol::new("Tuple0"),
            display: RirSymbol::new("(int, bool)"),
            copyable: true,
            fields: vec![
                RirField {
                    id: RirFieldId::from_index(0),
                    symbol: RirSymbol::new("_0"),
                    ty: int,
                },
                RirField {
                    id: RirFieldId::from_index(1),
                    symbol: RirSymbol::new("_1"),
                    ty: bool_ty,
                },
            ],
        }],
        functions: vec![RirFunction {
            id: RirFunctionId::from_index(0),
            air_id: None,
            symbol: RirSymbol::new("f"),
            params: vec![],
            ret: RirReturn { ty: void },
            locals: vec![RirLocal {
                id: RirLocalId::from_index(0),
                ty: tuple,
                mutable: false,
                symbol: RirSymbol::new("t"),
                initialized: false,
                payload_ref: false,
            }],
            body: RirStructuredBlock {
                stmts: vec![RirStmt::Init {
                    local: RirLocalId::from_index(0),
                    value,
                }],
                term: RirTerm::Return(None),
            },
        }],
        consts: vec![
            RirConst {
                id: RirConstId::from_index(0),
                ty: int,
                value: RirConstValue::Int(1),
            },
            RirConst {
                id: RirConstId::from_index(1),
                ty: bool_ty,
                value: RirConstValue::Bool(true),
            },
        ],
        ..RirProgram::default()
    }
}

fn rir_function(
    id: RirFunctionId,
    ret: RirTypeId,
    params: Vec<RirParam>,
    locals: Vec<RirLocal>,
    stmts: Vec<RirStmt>,
) -> RirFunction {
    RirFunction {
        id,
        air_id: None,
        symbol: RirSymbol::new(format!("f{}", id.index())),
        params,
        ret: RirReturn { ty: ret },
        locals,
        body: RirStructuredBlock {
            stmts,
            term: RirTerm::Return(None),
        },
    }
}

fn rir_param(
    local: RirLocalId,
    ty: RirTypeId,
    semantic: RirParamSemantic,
    abi: RirParamAbi,
) -> RirParam {
    RirParam {
        local,
        ty,
        semantic,
        abi,
        escape: RirParamEscape::NonEscaping,
    }
}

fn rir_local(id: RirLocalId, ty: RirTypeId, mutable: bool, symbol: &str) -> RirLocal {
    RirLocal {
        id,
        ty,
        mutable,
        symbol: RirSymbol::new(symbol),
        initialized: true,
        payload_ref: false,
    }
}

fn rir_place(local: RirLocalId, ty: RirTypeId) -> RirPlace {
    RirPlace {
        local,
        projections: vec![],
        ty,
    }
}

fn mut_place_call_rir(
    caller_locals: Vec<RirLocal>,
    caller_params: Vec<RirParam>,
    arg: RirCallArg,
) -> RirProgram {
    let void = RirTypeId::from_index(0);
    let int = RirTypeId::from_index(1);
    let local = RirLocalId::from_index(0);
    let callee = RirFunctionId::from_index(0);
    RirProgram {
        types: vec![RirType::Void, RirType::Int],
        functions: vec![
            rir_function(
                callee,
                void,
                vec![rir_param(
                    local,
                    int,
                    RirParamSemantic::MutPlace,
                    RirParamAbi::MutPlace,
                )],
                vec![rir_local(local, int, true, "x")],
                vec![],
            ),
            rir_function(
                RirFunctionId::from_index(1),
                void,
                caller_params,
                caller_locals,
                vec![RirStmt::Eval(RirRValue::Call {
                    callee: RirCallTarget::Function(callee),
                    args: vec![arg],
                    ty: void,
                })],
            ),
        ],
        ..RirProgram::default()
    }
}

fn empty_rir_function(ret: RirType) -> RirProgram {
    RirProgram {
        types: vec![ret],
        functions: vec![RirFunction {
            id: RirFunctionId::from_index(0),
            air_id: None,
            symbol: RirSymbol::new("f"),
            params: vec![],
            ret: RirReturn {
                ty: RirTypeId::from_index(0),
            },
            locals: vec![],
            body: RirStructuredBlock {
                stmts: vec![],
                term: RirTerm::Return(None),
            },
        }],
        consts: vec![RirConst {
            id: RirConstId::from_index(0),
            ty: RirTypeId::from_index(0),
            value: RirConstValue::Int(0),
        }],
        ..RirProgram::default()
    }
}

fn struct_decl_program(dataref: bool) -> Program {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let string = program.alloc_type(TypeData::String);
    let module = program.alloc_module(root_module());
    let aggregate = program.alloc_aggregate(air::AggregateDecl {
        name: Ident::new("Point"),
        module,
        kind: if dataref {
            air::AggregateKind::DataRef
        } else {
            air::AggregateKind::Struct
        },
        type_args: vec![],
        const_args: vec![],
        fields: vec![
            FieldDecl {
                name: Ident::new("x"),
                ty: int,
            },
            FieldDecl {
                name: Ident::new("name"),
                ty: string,
            },
        ],
        cycle_capable: dataref,
        stringify_override: None,
    });
    program.module_mut(module).aggregates.push(aggregate);
    program.alloc_type(if dataref {
        TypeData::DataRef(aggregate)
    } else {
        TypeData::Aggregate(aggregate)
    });
    program
}

fn unsupported_dataref_payload_program() -> Program {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let list = program.alloc_type(TypeData::List(int));
    let module = program.alloc_module(root_module());
    let aggregate = program.alloc_aggregate(air::AggregateDecl {
        name: Ident::new("Bad"),
        module,
        kind: air::AggregateKind::DataRef,
        type_args: vec![],
        const_args: vec![],
        fields: vec![FieldDecl {
            name: Ident::new("items"),
            ty: list,
        }],
        cycle_capable: true,
        stringify_override: None,
    });
    program.module_mut(module).aggregates.push(aggregate);
    program.alloc_type(TypeData::DataRef(aggregate));
    program
}

fn dataref_field_projection_program() -> Program {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let module = program.alloc_module(root_module());
    let aggregate = dataref_decl(&mut program, module, int);
    let node = program.alloc_type(TypeData::DataRef(aggregate));
    let one = program.const_arena.alloc(ConstData {
        ty: int,
        value: ConstValue::Int(1),
    });
    let arg = air::LocalId::from_index(0);
    let out = air::LocalId::from_index(1);
    let value = Place {
        root: PlaceRoot::Local(arg),
        projection: vec![Projection::Field(air::FieldId::from_index(0))],
        ty: int,
    };
    let main = program.alloc_function(Function {
        name: Ident::new("update"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![param("node", node, ParamMode::Value, arg)], int),
        locals: vec![
            Local {
                name: None,
                binding: None,
                ty: node,
                mutability: Mutability::Mutable,
                kind: LocalKind::Arg,
            },
            local(int, LocalKind::Temp),
        ],
        body: structured_body(
            vec![
                Statement::Assign {
                    dst: value.clone(),
                    value: RValue::Use(Operand::Const(one)),
                },
                Statement::Init {
                    local: out,
                    value: RValue::Use(Operand::Place(value)),
                },
            ],
            air::AirTail::Return(Some(Operand::Place(place(out, int)))),
        ),
    });
    program.module_mut(module).functions.push(main);
    program
}

fn dataref_optional_payload_ref_program() -> Program {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let option = program.alloc_type(TypeData::Optional(int));
    let void = program.alloc_type(TypeData::Void);
    let module = program.alloc_module(root_module());
    let aggregate = dataref_decl(&mut program, module, option);
    let node = program.alloc_type(TypeData::DataRef(aggregate));
    let arg = air::LocalId::from_index(0);
    let payload = air::LocalId::from_index(1);
    let discr = Place {
        root: PlaceRoot::Local(arg),
        projection: vec![Projection::Field(air::FieldId::from_index(0))],
        ty: option,
    };
    let main = program.alloc_function(Function {
        name: Ident::new("update"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![param("node", node, ParamMode::Value, arg)], void),
        locals: vec![
            Local {
                name: None,
                binding: None,
                ty: node,
                mutability: Mutability::Mutable,
                kind: LocalKind::Arg,
            },
            Local {
                name: None,
                binding: None,
                ty: int,
                mutability: Mutability::Mutable,
                kind: LocalKind::PatternBinding,
            },
        ],
        body: structured_body(
            vec![Statement::OptionalMatch(AirOptionalMatch {
                discr,
                payload: Some(payload),
                payload_ref: true,
                payload_escapes: false,
                some_block: air::AirBlock::default(),
                none_block: air::AirBlock::default(),
            })],
            air::AirTail::Return(None),
        ),
    });
    program.module_mut(module).functions.push(main);
    program
}

fn optional_void_payload_copy_program() -> Program {
    let mut program = Program::default();
    let void = program.alloc_type(TypeData::Void);
    let option = program.alloc_type(TypeData::Optional(void));
    let module = program.alloc_module(root_module());
    let arg = air::LocalId::from_index(0);
    let payload = air::LocalId::from_index(1);
    let main = program.alloc_function(Function {
        name: Ident::new("main"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![param("value", option, ParamMode::Value, arg)], void),
        locals: vec![local(option, LocalKind::Arg), local(void, LocalKind::Temp)],
        body: structured_body(
            vec![Statement::OptionalMatch(AirOptionalMatch {
                discr: place(arg, option),
                payload: Some(payload),
                payload_ref: false,
                payload_escapes: false,
                some_block: air::AirBlock::default(),
                none_block: air::AirBlock::default(),
            })],
            air::AirTail::Return(None),
        ),
    });
    program.module_mut(module).functions.push(main);
    program
}

fn nested_dataref_read_program() -> Program {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let module = program.alloc_module(root_module());
    let aggregate = dataref_decl(&mut program, module, int);
    let node = program.alloc_type(TypeData::DataRef(aggregate));
    program.aggregate_mut(aggregate).fields.push(FieldDecl {
        name: Ident::new("child"),
        ty: node,
    });
    let root = air::LocalId::from_index(0);
    let out = air::LocalId::from_index(1);
    let nested = Place {
        root: PlaceRoot::Local(root),
        projection: vec![
            Projection::Field(air::FieldId::from_index(1)),
            Projection::Field(air::FieldId::from_index(0)),
        ],
        ty: int,
    };
    let main = program.alloc_function(Function {
        name: Ident::new("nested"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![param("node", node, ParamMode::Value, root)], int),
        locals: vec![local(node, LocalKind::Arg), local(int, LocalKind::Temp)],
        body: structured_body(
            vec![Statement::Init {
                local: out,
                value: RValue::Use(Operand::Place(nested)),
            }],
            air::AirTail::Return(Some(Operand::Place(place(out, int)))),
        ),
    });
    program.module_mut(module).functions.push(main);
    program
}

fn dataref_string_field_consumers_program() -> Program {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let string = program.alloc_type(TypeData::String);
    let void = program.alloc_type(TypeData::Void);
    let module = program.alloc_module(root_module());
    let aggregate = program.alloc_aggregate(air::AggregateDecl {
        name: Ident::new("Named"),
        module,
        kind: air::AggregateKind::DataRef,
        type_args: vec![],
        const_args: vec![],
        fields: vec![FieldDecl {
            name: Ident::new("label"),
            ty: string,
        }],
        cycle_capable: true,
        stringify_override: None,
    });
    program.module_mut(module).aggregates.push(aggregate);
    let named = program.alloc_type(TypeData::DataRef(aggregate));
    let arg = air::LocalId::from_index(0);
    let consume = program.alloc_function(Function {
        name: Ident::new("consume"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(
            vec![param("label", string, ParamMode::SharedBorrow, arg)],
            void,
        ),
        locals: vec![local(string, LocalKind::Arg)],
        body: structured_body(vec![], air::AirTail::Return(None)),
    });
    let node = air::LocalId::from_index(0);
    let len = air::LocalId::from_index(1);
    let label = Place {
        root: PlaceRoot::Local(node),
        projection: vec![Projection::Field(air::FieldId::from_index(0))],
        ty: string,
    };
    let main = program.alloc_function(Function {
        name: Ident::new("main"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![param("node", named, ParamMode::Value, node)], int),
        locals: vec![local(named, LocalKind::Arg), local(int, LocalKind::Temp)],
        body: structured_body(
            vec![
                Statement::Init {
                    local: len,
                    value: RValue::Len {
                        source: label.clone(),
                    },
                },
                Statement::Eval(RValue::Call {
                    callee: Callee::Function(consume),
                    args: vec![CallArg::SharedBorrow(label)],
                }),
            ],
            air::AirTail::Return(Some(Operand::Place(place(len, int)))),
        ),
    });
    program.module_mut(module).functions.extend([consume, main]);
    program
}

fn multi_projected_mut_call_arg_program() -> Program {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let void = program.alloc_type(TypeData::Void);
    let module = program.alloc_module(root_module());
    let aggregate = program.alloc_aggregate(air::AggregateDecl {
        name: Ident::new("Pair"),
        module,
        kind: air::AggregateKind::DataRef,
        type_args: vec![],
        const_args: vec![],
        fields: vec![
            FieldDecl {
                name: Ident::new("a"),
                ty: int,
            },
            FieldDecl {
                name: Ident::new("b"),
                ty: int,
            },
        ],
        cycle_capable: true,
        stringify_override: None,
    });
    program.module_mut(module).aggregates.push(aggregate);
    let pair = program.alloc_type(TypeData::DataRef(aggregate));
    let a = air::LocalId::from_index(0);
    let b = air::LocalId::from_index(1);
    let swap = program.alloc_function(Function {
        name: Ident::new("swap"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(
            vec![
                param("a", int, ParamMode::MutBorrow, a),
                param("b", int, ParamMode::MutBorrow, b),
            ],
            void,
        ),
        locals: vec![
            mut_local(int, LocalKind::Arg),
            mut_local(int, LocalKind::Arg),
        ],
        body: structured_body(vec![], air::AirTail::Return(None)),
    });
    let pair_local = air::LocalId::from_index(0);
    let field_a = Place {
        root: PlaceRoot::Local(pair_local),
        projection: vec![Projection::Field(air::FieldId::from_index(0))],
        ty: int,
    };
    let field_b = Place {
        root: PlaceRoot::Local(pair_local),
        projection: vec![Projection::Field(air::FieldId::from_index(1))],
        ty: int,
    };
    let main = program.alloc_function(Function {
        name: Ident::new("main"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(
            vec![param("pair", pair, ParamMode::Value, pair_local)],
            void,
        ),
        locals: vec![local(pair, LocalKind::Arg)],
        body: structured_body(
            vec![Statement::Eval(RValue::Call {
                callee: Callee::Function(swap),
                args: vec![CallArg::MutBorrow(field_a), CallArg::MutBorrow(field_b)],
            })],
            air::AirTail::Return(None),
        ),
    });
    program.module_mut(module).functions.extend([swap, main]);
    program
}

fn dataref_root_rebind_program() -> Program {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let void = program.alloc_type(TypeData::Void);
    let module = program.alloc_module(root_module());
    let aggregate = dataref_decl(&mut program, module, int);
    let node = program.alloc_type(TypeData::DataRef(aggregate));
    let target = air::LocalId::from_index(0);
    let other = air::LocalId::from_index(1);
    let replace = program.alloc_function(Function {
        name: Ident::new("replace"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(
            vec![
                param("target", node, ParamMode::MutBorrow, target),
                param("other", node, ParamMode::Value, other),
            ],
            void,
        ),
        locals: vec![mut_local(node, LocalKind::Arg), local(node, LocalKind::Arg)],
        body: structured_body(
            vec![Statement::Assign {
                dst: place(target, node),
                value: RValue::Use(Operand::Place(place(other, node))),
            }],
            air::AirTail::Return(None),
        ),
    });
    program.module_mut(module).functions.push(replace);
    program
}

fn projected_mut_call_arg_program() -> Program {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let void = program.alloc_type(TypeData::Void);
    let module = program.alloc_module(root_module());
    let aggregate = dataref_decl(&mut program, module, int);
    let node = program.alloc_type(TypeData::DataRef(aggregate));
    let arg = air::LocalId::from_index(0);
    let bump = program.alloc_function(Function {
        name: Ident::new("bump"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![param("value", int, ParamMode::MutBorrow, arg)], void),
        locals: vec![mut_local(int, LocalKind::Arg)],
        body: structured_body(vec![], air::AirTail::Return(None)),
    });
    let node_local = air::LocalId::from_index(0);
    let field = Place {
        root: PlaceRoot::Local(node_local),
        projection: vec![Projection::Field(air::FieldId::from_index(0))],
        ty: int,
    };
    let main = program.alloc_function(Function {
        name: Ident::new("main"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(
            vec![param("node", node, ParamMode::Value, node_local)],
            void,
        ),
        locals: vec![local(node, LocalKind::Arg)],
        body: structured_body(
            vec![Statement::Eval(RValue::Call {
                callee: Callee::Function(bump),
                args: vec![CallArg::MutBorrow(field)],
            })],
            air::AirTail::Return(None),
        ),
    });
    program.module_mut(module).functions.extend([bump, main]);
    program
}

fn capture_cell_dataref_source_var_arg_program() -> Program {
    let mut program = projected_mut_call_arg_program();
    let owner = FunctionId::from_index(1);
    let binding = BindingId::from_index(0);
    let source_local = air::LocalId::from_index(0);
    let node = program.function(owner).locals[source_local.index()].ty;
    let aggregate = match program.type_arena.data(node) {
        TypeData::DataRef(aggregate) => *aggregate,
        _ => unreachable!("test helper should use a dataref local"),
    };
    let int = program.aggregate(aggregate).fields[0].ty;
    let zero = int_const(&mut program, int, 0);
    let cell = program.alloc_capture_cell(CaptureCellDecl {
        binding,
        owner,
        source_local,
        ty: node,
    });

    let function = program.function_mut(owner);
    function.signature.params.clear();
    let source = &mut function.locals[source_local.index()];
    source.kind = LocalKind::User;
    source.mutability = Mutability::Mutable;
    source.binding = Some(binding);
    let Statement::Eval(RValue::Call { args, .. }) = &mut function.body.block.stmts[0] else {
        unreachable!("test helper should start with the projected mut call")
    };
    let [CallArg::MutBorrow(place)] = args.as_mut_slice() else {
        unreachable!("test helper should have one mut call arg")
    };
    place.root = PlaceRoot::CaptureCell(cell);
    function.body.block.stmts.insert(
        0,
        Statement::Assign {
            dst: Place {
                root: PlaceRoot::CaptureCell(cell),
                projection: vec![],
                ty: node,
            },
            value: RValue::Aggregate {
                kind: AggregateCtor::DataRef(aggregate),
                fields: vec![Operand::Const(zero)],
                ty: node,
            },
        },
    );
    program
}

fn dataref_decl(
    program: &mut Program,
    module: air::ModuleId,
    int: air::TypeId,
) -> air::AggregateId {
    let aggregate = program.alloc_aggregate(air::AggregateDecl {
        name: Ident::new("Node"),
        module,
        kind: air::AggregateKind::DataRef,
        type_args: vec![],
        const_args: vec![],
        fields: vec![FieldDecl {
            name: Ident::new("value"),
            ty: int,
        }],
        cycle_capable: true,
        stringify_override: None,
    });
    program.module_mut(module).aggregates.push(aggregate);
    aggregate
}

fn struct_method_program() -> Program {
    let mut program = Program::default();
    let void = program.alloc_type(TypeData::Void);
    let string = program.alloc_type(TypeData::String);
    let int = program.alloc_type(TypeData::Int);
    let module = program.alloc_module(root_module());
    let aggregate = program.alloc_aggregate(air::AggregateDecl {
        name: Ident::new("Point"),
        module,
        kind: air::AggregateKind::Struct,
        type_args: vec![],
        const_args: vec![],
        fields: vec![FieldDecl {
            name: Ident::new("x"),
            ty: int,
        }],
        cycle_capable: false,
        stringify_override: None,
    });
    program.module_mut(module).aggregates.push(aggregate);
    let point = program.alloc_type(TypeData::Aggregate(aggregate));
    let recv = air::LocalId::from_index(0);
    let method = program.alloc_function(Function {
        name: Ident::new("value"),
        module,
        kind: FunctionKind::Method,
        owner: None,
        specialization: None,
        signature: Signature::new(
            vec![Param {
                name: Some(Ident::new("self")),
                ty: point,
                mode: ParamMode::Value,
                escape: ParamEscape::NonEscaping,
                role: ParamRole::Receiver,
                local_id: recv,
            }],
            int,
        ),
        locals: vec![local(point, LocalKind::Arg)],
        body: structured_body(
            vec![],
            air::AirTail::Return(Some(Operand::Place(Place {
                root: PlaceRoot::Local(recv),
                projection: vec![Projection::Field(air::FieldId::from_index(0))],
                ty: int,
            }))),
        ),
    });
    let println = runtime_extern(
        &mut program,
        "_println",
        vec![(string, ParamMode::SharedBorrow)],
        void,
    );
    let seven = program.const_arena.alloc(ConstData {
        ty: int,
        value: ConstValue::Int(7),
    });
    let point_local = air::LocalId::from_index(0);
    let value_local = air::LocalId::from_index(1);
    let text = air::LocalId::from_index(2);
    let main = program.alloc_function(Function {
        name: Ident::new("main"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], void),
        locals: vec![
            local(point, LocalKind::Temp),
            local(int, LocalKind::Temp),
            local(string, LocalKind::Temp),
        ],
        body: structured_body(
            vec![
                Statement::Init {
                    local: point_local,
                    value: RValue::Aggregate {
                        kind: AggregateCtor::Struct(aggregate),
                        fields: vec![Operand::Const(seven)],
                        ty: point,
                    },
                },
                Statement::Init {
                    local: value_local,
                    value: RValue::Call {
                        callee: Callee::Function(method),
                        args: vec![CallArg::Value(Operand::Place(place(point_local, point)))],
                    },
                },
                Statement::Init {
                    local: text,
                    value: RValue::Stringify {
                        value: Operand::Place(place(value_local, int)),
                        source_ty: int,
                    },
                },
                Statement::Eval(RValue::Call {
                    callee: Callee::Extern(println),
                    args: vec![CallArg::SharedBorrow(place(text, string))],
                }),
            ],
            air::AirTail::Return(None),
        ),
    });
    program.module_mut(module).functions.push(method);
    program.module_mut(module).functions.push(main);
    program.set_entry(main);
    program
}

fn struct_field_read_program() -> Program {
    let mut program = Program::default();
    let void = program.alloc_type(TypeData::Void);
    let string = program.alloc_type(TypeData::String);
    let int = program.alloc_type(TypeData::Int);
    let module = program.alloc_module(root_module());
    let aggregate = program.alloc_aggregate(air::AggregateDecl {
        name: Ident::new("Point"),
        module,
        kind: air::AggregateKind::Struct,
        type_args: vec![],
        const_args: vec![],
        fields: vec![FieldDecl {
            name: Ident::new("x"),
            ty: int,
        }],
        cycle_capable: false,
        stringify_override: None,
    });
    program.module_mut(module).aggregates.push(aggregate);
    let point = program.alloc_type(TypeData::Aggregate(aggregate));
    let seven = program.const_arena.alloc(ConstData {
        ty: int,
        value: ConstValue::Int(7),
    });
    let println = runtime_extern(
        &mut program,
        "_println",
        vec![(string, ParamMode::SharedBorrow)],
        void,
    );
    let point_local = air::LocalId::from_index(0);
    let text = air::LocalId::from_index(1);
    let main = program.alloc_function(Function {
        name: Ident::new("main"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], void),
        locals: vec![
            local(point, LocalKind::Temp),
            local(string, LocalKind::Temp),
        ],
        body: structured_body(
            vec![
                Statement::Init {
                    local: point_local,
                    value: RValue::Aggregate {
                        kind: AggregateCtor::Struct(aggregate),
                        fields: vec![Operand::Const(seven)],
                        ty: point,
                    },
                },
                Statement::Init {
                    local: text,
                    value: RValue::Stringify {
                        value: Operand::Place(Place {
                            root: PlaceRoot::Local(point_local),
                            projection: vec![Projection::Field(air::FieldId::from_index(0))],
                            ty: int,
                        }),
                        source_ty: int,
                    },
                },
                Statement::Eval(RValue::Call {
                    callee: Callee::Extern(println),
                    args: vec![CallArg::SharedBorrow(place(text, string))],
                }),
            ],
            air::AirTail::Return(None),
        ),
    });
    program.module_mut(module).functions.push(main);
    program.set_entry(main);
    program
}

fn duplicate_struct_name_program() -> Program {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    for path in [vec!["a"], vec!["b"]] {
        let module = program.alloc_module(air_module(&path));
        let aggregate = program.alloc_aggregate(air::AggregateDecl {
            name: Ident::new("Same"),
            module,
            kind: air::AggregateKind::Struct,
            type_args: vec![],
            const_args: vec![],
            fields: vec![FieldDecl {
                name: Ident::new("x"),
                ty: int,
            }],
            cycle_capable: false,
            stringify_override: None,
        });
        program.module_mut(module).aggregates.push(aggregate);
        program.alloc_type(TypeData::Aggregate(aggregate));
    }
    program
}

fn scalar_branch_program() -> Program {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let bool_ty = program.alloc_type(TypeData::Bool);
    let module = program.alloc_module(root_module());
    let condition = program.const_arena.alloc(ConstData {
        ty: bool_ty,
        value: ConstValue::Bool(true),
    });
    let one = program.const_arena.alloc(ConstData {
        ty: int,
        value: ConstValue::Int(1),
    });
    let two = program.const_arena.alloc(ConstData {
        ty: int,
        value: ConstValue::Int(2),
    });
    let out = air::LocalId::from_index(0);
    let func = program.alloc_function(Function {
        name: Ident::new("main"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], int),
        locals: vec![local(int, LocalKind::Temp)],
        body: AirBody {
            block: air::AirBlock {
                stmts: vec![air::AirStmt::If(air::AirIf {
                    cond: Operand::Const(condition),
                    then_block: air::AirBlock {
                        stmts: vec![air::AirStmt::Init {
                            local: out,
                            value: RValue::Use(Operand::Const(one)),
                        }],
                        tail: air::AirTail::None,
                    },
                    else_block: Some(air::AirBlock {
                        stmts: vec![air::AirStmt::Init {
                            local: out,
                            value: RValue::Use(Operand::Const(two)),
                        }],
                        tail: air::AirTail::None,
                    }),
                })],
                tail: air::AirTail::Return(Some(Operand::Place(place(out, int)))),
            },
        },
    });
    program.module_mut(module).functions.push(func);
    program.entry = Some(func);
    program
}

fn format_program() -> Program {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let float = program.alloc_type(TypeData::Float);
    let bool_ty = program.alloc_type(TypeData::Bool);
    let string = program.alloc_type(TypeData::String);
    let void = program.alloc_type(TypeData::Void);
    let println = runtime_extern(
        &mut program,
        "_println",
        vec![(string, ParamMode::SharedBorrow)],
        void,
    );
    let seven = program.const_arena.alloc(ConstData {
        ty: int,
        value: ConstValue::Int(7),
    });
    let one = program.const_arena.alloc(ConstData {
        ty: float,
        value: ConstValue::Float(1.25),
    });
    let one_exp = program.const_arena.alloc(ConstData {
        ty: float,
        value: ConstValue::Float(1.0),
    });
    let flag = program.const_arena.alloc(ConstData {
        ty: bool_ty,
        value: ConstValue::Bool(true),
    });
    let text_const = program.const_arena.alloc(ConstData {
        ty: string,
        value: ConstValue::String("xy".into()),
    });
    let five = program.const_arena.alloc(ConstData {
        ty: int,
        value: ConstValue::Int(5),
    });
    let hex = program.const_arena.alloc(ConstData {
        ty: int,
        value: ConstValue::Int(255),
    });
    let module = program.alloc_module(root_module());
    let formatted = air::LocalId::from_index(0);
    let text = air::LocalId::from_index(1);
    let main = program.alloc_function(Function {
        name: Ident::new("main"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], void),
        locals: vec![
            Local {
                name: None,
                binding: None,
                ty: string,
                mutability: Mutability::Mutable,
                kind: LocalKind::Temp,
            },
            local(string, LocalKind::Temp),
        ],
        body: structured_body(
            vec![
                Statement::Init {
                    local: formatted,
                    value: RValue::Format {
                        value: Operand::Const(seven),
                        spec: FormatSpec {
                            fill: '0',
                            align: Some(FormatAlign::Right),
                            zero_pad: true,
                            width: Some(4),
                            ..FormatSpec::default()
                        },
                    },
                },
                Statement::Eval(RValue::Call {
                    callee: Callee::Extern(println),
                    args: vec![CallArg::SharedBorrow(place(formatted, string))],
                }),
                Statement::Init {
                    local: text,
                    value: RValue::Use(Operand::Const(text_const)),
                },
                Statement::Assign {
                    dst: place(formatted, string),
                    value: RValue::Format {
                        value: Operand::Place(place(text, string)),
                        spec: FormatSpec {
                            fill: '*',
                            align: Some(FormatAlign::Right),
                            width: Some(5),
                            ..FormatSpec::default()
                        },
                    },
                },
                Statement::Assign {
                    dst: place(formatted, string),
                    value: RValue::Format {
                        value: Operand::Const(one),
                        spec: FormatSpec {
                            precision: Some(2),
                            ..FormatSpec::default()
                        },
                    },
                },
                Statement::Assign {
                    dst: place(formatted, string),
                    value: RValue::Format {
                        value: Operand::Const(flag),
                        spec: FormatSpec {
                            align: Some(FormatAlign::Right),
                            width: Some(5),
                            ..FormatSpec::default()
                        },
                    },
                },
                Statement::Assign {
                    dst: place(formatted, string),
                    value: RValue::Format {
                        value: Operand::Const(hex),
                        spec: FormatSpec {
                            kind: FormatKind::HexUpper,
                            ..FormatSpec::default()
                        },
                    },
                },
                Statement::Assign {
                    dst: place(formatted, string),
                    value: RValue::Format {
                        value: Operand::Const(five),
                        spec: FormatSpec {
                            kind: FormatKind::Binary,
                            ..FormatSpec::default()
                        },
                    },
                },
                Statement::Assign {
                    dst: place(formatted, string),
                    value: RValue::Format {
                        value: Operand::Const(one_exp),
                        spec: FormatSpec {
                            kind: FormatKind::Exp,
                            ..FormatSpec::default()
                        },
                    },
                },
                Statement::Assign {
                    dst: place(formatted, string),
                    value: RValue::Format {
                        value: Operand::Const(one_exp),
                        spec: FormatSpec {
                            kind: FormatKind::ExpUpper,
                            ..FormatSpec::default()
                        },
                    },
                },
            ],
            air::AirTail::Return(None),
        ),
    });
    program.module_mut(module).functions.push(main);
    program.set_entry(main);
    program
}

fn string_concat_program() -> Program {
    let mut program = Program::default();
    let string = program.alloc_type(TypeData::String);
    let void = program.alloc_type(TypeData::Void);
    let println = runtime_extern(
        &mut program,
        "_println",
        vec![(string, ParamMode::SharedBorrow)],
        void,
    );
    let a = program.const_arena.alloc(ConstData {
        ty: string,
        value: ConstValue::String("a".into()),
    });
    let b = program.const_arena.alloc(ConstData {
        ty: string,
        value: ConstValue::String("b".into()),
    });
    let c = program.const_arena.alloc(ConstData {
        ty: string,
        value: ConstValue::String("c".into()),
    });
    let text = air::LocalId::from_index(0);
    let suffix = air::LocalId::from_index(1);
    let out = air::LocalId::from_index(2);
    let module = program.alloc_module(root_module());
    let main = program.alloc_function(Function {
        name: Ident::new("main"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], void),
        locals: vec![
            Local {
                name: None,
                binding: None,
                ty: string,
                mutability: Mutability::Mutable,
                kind: LocalKind::User,
            },
            local(string, LocalKind::Temp),
            local(string, LocalKind::Temp),
        ],
        body: structured_body(
            vec![
                Statement::Init {
                    local: text,
                    value: RValue::StringConcat {
                        parts: vec![Operand::Const(a), Operand::Const(b)],
                    },
                },
                Statement::Eval(RValue::Call {
                    callee: Callee::Extern(println),
                    args: vec![CallArg::SharedBorrow(place(text, string))],
                }),
                Statement::Init {
                    local: suffix,
                    value: RValue::Use(Operand::Const(c)),
                },
                Statement::Init {
                    local: out,
                    value: RValue::StringConcat {
                        parts: vec![
                            Operand::Place(place(text, string)),
                            Operand::Place(place(suffix, string)),
                        ],
                    },
                },
                Statement::Eval(RValue::Call {
                    callee: Callee::Extern(println),
                    args: vec![CallArg::SharedBorrow(place(out, string))],
                }),
                Statement::Assign {
                    dst: place(text, string),
                    value: RValue::StringConcat {
                        parts: vec![Operand::Place(place(text, string)), Operand::Const(c)],
                    },
                },
            ],
            air::AirTail::Return(None),
        ),
    });
    program.module_mut(module).functions.push(main);
    program.set_entry(main);
    program
}

fn fallible_call_program(transitive: bool, returns_value: bool) -> Program {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let void = program.alloc_type(TypeData::Void);
    let fallible = fallible_extern(&mut program, int);
    let value = program.const_arena.alloc(ConstData {
        ty: int,
        value: ConstValue::Int(41),
    });
    let module = program.alloc_module(root_module());
    let tmp = air::LocalId::from_index(0);
    let leaf = program.alloc_function(Function {
        name: Ident::new(if transitive { "leaf" } else { "main" }),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], if returns_value { int } else { void }),
        locals: vec![local(int, LocalKind::Temp)],
        body: structured_body(
            vec![Statement::Init {
                local: tmp,
                value: RValue::Call {
                    callee: Callee::Extern(fallible),
                    args: vec![CallArg::Value(Operand::Const(value))],
                },
            }],
            air::AirTail::Return(returns_value.then_some(Operand::Place(place(tmp, int)))),
        ),
    });
    program.module_mut(module).functions.push(leaf);
    if transitive {
        let main = program.alloc_function(Function {
            name: Ident::new("main"),
            module,
            kind: FunctionKind::Normal,
            owner: None,
            specialization: None,
            signature: Signature::new(vec![], void),
            locals: vec![],
            body: structured_body(
                vec![Statement::Eval(RValue::Call {
                    callee: Callee::Function(leaf),
                    args: vec![],
                })],
                air::AirTail::Return(None),
            ),
        });
        program.module_mut(module).functions.push(main);
        program.entry = Some(main);
    } else {
        program.entry = Some(leaf);
    }
    program
}

fn borrow_string_literal_program() -> Program {
    let mut program = Program::default();
    let string = program.alloc_type(TypeData::String);
    let void = program.alloc_type(TypeData::Void);
    let println = runtime_extern(
        &mut program,
        "_println",
        vec![(string, ParamMode::SharedBorrow)],
        void,
    );
    let message = program.const_arena.alloc(ConstData {
        ty: string,
        value: ConstValue::String("ready".into()),
    });
    let module = program.alloc_module(root_module());
    let main = program.alloc_function(Function {
        name: Ident::new("main"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], void),
        locals: vec![],
        body: structured_body(
            vec![Statement::Eval(RValue::Call {
                callee: Callee::Extern(println),
                args: vec![CallArg::SharedStringConst(message)],
            })],
            air::AirTail::Return(None),
        ),
    });
    program.module_mut(module).functions.push(main);
    program
}

fn shared_string_forward_program() -> Program {
    let mut program = Program::default();
    let string = program.alloc_type(TypeData::String);
    let void = program.alloc_type(TypeData::Void);
    let println = runtime_extern(
        &mut program,
        "_println",
        vec![(string, ParamMode::SharedBorrow)],
        void,
    );
    let module = program.alloc_module(root_module());
    let message = air::LocalId::from_index(0);
    let function = program.alloc_function(Function {
        name: Ident::new("forward"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(
            vec![param("message", string, ParamMode::SharedBorrow, message)],
            void,
        ),
        locals: vec![local(string, LocalKind::Arg)],
        body: structured_body(
            vec![Statement::Eval(RValue::Call {
                callee: Callee::Extern(println),
                args: vec![CallArg::SharedBorrow(place(message, string))],
            })],
            air::AirTail::Return(None),
        ),
    });
    program.module_mut(module).functions.push(function);
    program
}

fn native_str_len_const_program() -> Program {
    let mut program = Program::default();
    let string = program.alloc_type(TypeData::String);
    let int = program.alloc_type(TypeData::Int);
    let str_len = string_extern(
        &mut program,
        "str_len",
        vec![(string, ParamMode::SharedBorrow)],
        int,
    );
    let text = program.const_arena.alloc(ConstData {
        ty: string,
        value: ConstValue::String("abc".into()),
    });
    let out = air::LocalId::from_index(0);
    let module = program.alloc_module(root_module());
    let main = program.alloc_function(Function {
        name: Ident::new("main"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], int),
        locals: vec![local(int, LocalKind::Temp)],
        body: structured_body(
            vec![Statement::Init {
                local: out,
                value: RValue::Call {
                    callee: Callee::Extern(str_len),
                    args: vec![CallArg::SharedStringConst(text)],
                },
            }],
            air::AirTail::Return(Some(Operand::Place(place(out, int)))),
        ),
    });
    program.module_mut(module).functions.push(main);
    program.set_entry(main);
    program
}

fn native_str_len_local_program() -> Program {
    let mut program = Program::default();
    let string = program.alloc_type(TypeData::String);
    let int = program.alloc_type(TypeData::Int);
    let str_len = string_extern(
        &mut program,
        "str_len",
        vec![(string, ParamMode::SharedBorrow)],
        int,
    );
    let text = program.const_arena.alloc(ConstData {
        ty: string,
        value: ConstValue::String("abc".into()),
    });
    let text_local = air::LocalId::from_index(0);
    let out = air::LocalId::from_index(1);
    let module = program.alloc_module(root_module());
    let main = program.alloc_function(Function {
        name: Ident::new("main"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], int),
        locals: vec![local(string, LocalKind::Temp), local(int, LocalKind::Temp)],
        body: structured_body(
            vec![
                Statement::Init {
                    local: text_local,
                    value: RValue::Use(Operand::Const(text)),
                },
                Statement::Init {
                    local: out,
                    value: RValue::Call {
                        callee: Callee::Extern(str_len),
                        args: vec![CallArg::SharedBorrow(place(text_local, string))],
                    },
                },
            ],
            air::AirTail::Return(Some(Operand::Place(place(out, int)))),
        ),
    });
    program.module_mut(module).functions.push(main);
    program.set_entry(main);
    program
}

fn format_borrowed_string_program() -> Program {
    let mut program = Program::default();
    let string = program.alloc_type(TypeData::String);
    let module = program.alloc_module(root_module());
    let input = air::LocalId::from_index(0);
    let out = air::LocalId::from_index(1);
    let function = program.alloc_function(Function {
        name: Ident::new("pad"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(
            vec![param("input", string, ParamMode::SharedBorrow, input)],
            string,
        ),
        locals: vec![
            local(string, LocalKind::Arg),
            local(string, LocalKind::Temp),
        ],
        body: structured_body(
            vec![Statement::Init {
                local: out,
                value: RValue::Format {
                    value: Operand::Place(place(input, string)),
                    spec: FormatSpec {
                        fill: '*',
                        align: Some(FormatAlign::Right),
                        sign: FormatSign::Default,
                        zero_pad: false,
                        width: Some(5),
                        precision: None,
                        kind: FormatKind::Default,
                    },
                },
            }],
            air::AirTail::Return(Some(Operand::Place(place(out, string)))),
        ),
    });
    program.module_mut(module).functions.push(function);
    program
}

fn scalar_print_program() -> Program {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let string = program.alloc_type(TypeData::String);
    let void = program.alloc_type(TypeData::Void);
    let println = runtime_extern(
        &mut program,
        "_println",
        vec![(string, ParamMode::SharedBorrow)],
        void,
    );
    let one = program.const_arena.alloc(ConstData {
        ty: int,
        value: ConstValue::Int(1),
    });
    let text_local = air::LocalId::from_index(0);
    let module = program.alloc_module(root_module());
    let main = program.alloc_function(Function {
        name: Ident::new("main"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], void),
        locals: vec![local(string, LocalKind::Temp)],
        body: structured_body(
            vec![
                Statement::Init {
                    local: text_local,
                    value: RValue::Stringify {
                        value: Operand::Const(one),
                        source_ty: int,
                    },
                },
                Statement::Eval(RValue::Call {
                    callee: Callee::Extern(println),
                    args: vec![CallArg::SharedBorrow(place(text_local, string))],
                }),
            ],
            air::AirTail::Return(None),
        ),
    });
    program.module_mut(module).functions.push(main);
    program.set_entry(main);
    program
}

mod enums {
    use super::*;

    #[test]
    fn constructs_unit_tuple_and_struct_variants() {
        let program = enum_value_program();
        let source = plan_source(program);
        let text = source.as_str();

        assert!(text.contains("enum anvT3_Event"));
        assert!(text.contains("Start,"));
        assert!(text.contains("Hit(i64, bool),"));
        assert!(text.contains("Move { x: i64, y: i64 },"));
        assert!(text.contains("anvT3_Event::Start"));
        assert!(text.contains("anvT3_Event::Hit(7, true)"));
        assert!(text.contains("anvT3_Event::Move { x: 7, y: 9 }"));
        assert!(!text.contains("clone"));
        assert!(!text.contains("impl anvT3_Event"));

        let output = run_source(source);
        assert_eq!(output.status, SourceJobStatus::Success);
    }

    #[test]
    fn passes_and_returns_copyable_enum_values_without_derives_or_clone() {
        let mut program = enum_value_program();
        let module = air::ModuleId::from_index(0);
        let enum_ty = enum_type(&program);
        let value = air::LocalId::from_index(0);
        let id_fn = program.alloc_function(Function {
            name: Ident::new("id"),
            module,
            kind: FunctionKind::Normal,
            owner: None,
            specialization: None,
            signature: Signature::new(
                vec![param("event", enum_ty, ParamMode::Value, value)],
                enum_ty,
            ),
            locals: vec![local(enum_ty, LocalKind::Arg)],
            body: structured_body(
                vec![],
                air::AirTail::Return(Some(Operand::Place(place(value, enum_ty)))),
            ),
        });
        program.module_mut(module).functions.insert(0, id_fn);
        let main = program.entry().expect("entry missing");
        let copied = air::LocalId::from_index(3);
        program
            .function_mut(main)
            .locals
            .push(local(enum_ty, LocalKind::Temp));
        program
            .function_mut(main)
            .body
            .block
            .stmts
            .push(air::AirStmt::Init {
                local: copied,
                value: RValue::Call {
                    callee: Callee::Function(id_fn),
                    args: vec![CallArg::Value(Operand::Place(place(
                        air::LocalId::from_index(0),
                        enum_ty,
                    )))],
                },
            });

        let source = plan_source(program);
        let text = source.as_str();

        assert!(text.contains("match &v0"));
        assert!(!text.contains("clone"));
        assert!(!text.contains("impl anvT3_Event"));

        let output = run_source(source);
        assert_eq!(output.status, SourceJobStatus::Success);
    }

    #[test]
    fn string_payload_uses_policy_representation() {
        let program = enum_with_string_payload_program(false);
        let source = plan_source(program).into_string();

        assert!(source.contains("enum anvT2_Message"));
        assert!(source.contains("Text(anvyx_runtime::AnvString)"));
        assert!(source.contains("anvT2_Message::Text(anvyx_runtime::AnvString::from(\"x\"))"));
        assert!(!source.contains("Text(String)"));
        assert!(!source.contains("Text(String::from"));
        assert!(!source.contains(".to_owned()"));
    }

    #[test]
    fn cow_payload_enum_value_reconstructs_with_shared_payload() {
        let mut program = enum_with_string_payload_program(false);
        let enum_ty = enum_type(&program);
        let value = air::LocalId::from_index(0);
        let copy = air::LocalId::from_index(1);
        let entry = program.entry().expect("entry missing");
        program
            .function_mut(entry)
            .locals
            .push(local(enum_ty, LocalKind::Temp));
        program
            .function_mut(entry)
            .body
            .block
            .stmts
            .push(air::AirStmt::Init {
                local: copy,
                value: RValue::Use(Operand::Place(place(value, enum_ty))),
            });

        let source = plan_source(program);
        let text = source.as_str();

        assert!(text.contains("match &v0"));
        assert!(text.contains("Text(f0) => anvT2_Message::Text((*(f0)).share())"));
        assert!(!text.contains(".clone()"));
        assert!(!text.contains(".to_owned()"));

        let output = run_source(source);
        assert_eq!(output.status, SourceJobStatus::Success, "{}", output.stderr);
    }

    #[test]
    fn enum_stringify_is_explicit_target_gap() {
        let program = enum_with_string_payload_program(true);
        let verified = air::verify(&program).expect("AIR verify failed");
        let Err(err) = plan(&verified, RustPlanConfig::default()) else {
            panic!("plan should reject stringify");
        };
        assert!(
            matches!(err, RustPlanError::TargetGaps(gaps) if gaps.iter().any(|gap| gap.kind == RustTargetGapKind::UnsupportedStructuralStringify))
        );
    }

    #[test]
    fn verifier_rejects_unit_variant_payload_shape() {
        let mut program = RirProgram::default();
        let int = RirTypeId::from_index(0);
        program.types.push(RirType::Int);
        program.types.push(RirType::Enum(RirEnumId::from_index(0)));
        program.enums.push(RirEnum {
            id: RirEnumId::from_index(0),
            air_id: None,
            core: None,
            repr: rir::RirEnumRepr::Adt,
            raw_type: None,
            symbol: RirSymbol::new("E"),
            display: RirSymbol::new("E"),
            copyable: true,
            variants: vec![RirVariant {
                id: RirVariantId::from_index(0),
                symbol: RirSymbol::new("Unit"),
                display: RirSymbol::new("Unit"),
                kind: RirVariantKind::Unit,
                raw_value: None,
                fields: vec![RirField {
                    id: RirFieldId::from_index(0),
                    symbol: RirSymbol::new("f0"),
                    ty: int,
                }],
            }],
        });

        let errors = rir::verify(&program).expect_err("malformed enum should fail");
        assert!(errors.iter().any(|error| matches!(
            error.kind,
            RirVerifyErrorKind::FieldCount {
                expected: 0,
                found: 1
            }
        )));
    }

    #[test]
    fn verifier_rejects_copyable_enum_with_noncopy_payload() {
        let mut program = RirProgram::default();
        let string = RirTypeId::from_index(0);
        program.types.push(RirType::String);
        program.types.push(RirType::Enum(RirEnumId::from_index(0)));
        program.enums.push(RirEnum {
            id: RirEnumId::from_index(0),
            air_id: None,
            core: None,
            repr: rir::RirEnumRepr::Adt,
            raw_type: None,
            symbol: RirSymbol::new("E"),
            display: RirSymbol::new("E"),
            copyable: true,
            variants: vec![RirVariant {
                id: RirVariantId::from_index(0),
                symbol: RirSymbol::new("Text"),
                display: RirSymbol::new("Text"),
                kind: RirVariantKind::Tuple,
                raw_value: None,
                fields: vec![RirField {
                    id: RirFieldId::from_index(0),
                    symbol: RirSymbol::new("f0"),
                    ty: string,
                }],
            }],
        });

        let errors = rir::verify(&program).expect_err("malformed enum should fail");
        assert!(
            errors
                .iter()
                .any(|error| error.kind == RirVerifyErrorKind::NonCopyValueRequired)
        );
    }

    #[test]
    fn verifier_rejects_copyable_enum_with_falsely_copyable_struct_payload() {
        let mut program = RirProgram::default();
        let string = RirTypeId::from_index(0);
        let strukt = RirTypeId::from_index(1);
        program.types.push(RirType::String);
        program
            .types
            .push(RirType::Struct(RirStructId::from_index(0)));
        program.types.push(RirType::Enum(RirEnumId::from_index(0)));
        program.structs.push(RirStruct {
            id: RirStructId::from_index(0),
            air_id: None,
            symbol: RirSymbol::new("S"),
            display: RirSymbol::new("S"),
            native_path: None,
            native_key: None,
            copyable: true,
            fields: vec![RirField {
                id: RirFieldId::from_index(0),
                symbol: RirSymbol::new("text"),
                ty: string,
            }],
        });
        program.enums.push(RirEnum {
            id: RirEnumId::from_index(0),
            air_id: None,
            core: None,
            repr: rir::RirEnumRepr::Adt,
            raw_type: None,
            symbol: RirSymbol::new("E"),
            display: RirSymbol::new("E"),
            copyable: true,
            variants: vec![RirVariant {
                id: RirVariantId::from_index(0),
                symbol: RirSymbol::new("Struct"),
                display: RirSymbol::new("Struct"),
                kind: RirVariantKind::Tuple,
                raw_value: None,
                fields: vec![RirField {
                    id: RirFieldId::from_index(0),
                    symbol: RirSymbol::new("f0"),
                    ty: strukt,
                }],
            }],
        });

        let errors = rir::verify(&program).expect_err("malformed enum should fail");
        assert!(
            errors
                .iter()
                .any(|error| error.kind == RirVerifyErrorKind::NonCopyValueRequired)
        );
    }

    fn enum_value_program() -> Program {
        let mut program = Program::default();
        let void = program.alloc_type(TypeData::Void);
        let int = program.alloc_type(TypeData::Int);
        let bool_ty = program.alloc_type(TypeData::Bool);
        let module = program.alloc_module(root_module());
        let enum_id = program.alloc_enum(EnumDecl {
            name: Ident::new("Event"),
            module,
            type_args: vec![],
            const_args: vec![],
            core: None,
            repr: air::EnumRepr::Adt,
            raw_type: None,
            variants: vec![
                VariantDecl {
                    name: Ident::new("Start"),
                    shape: VariantShape::Unit,
                    raw_value: None,
                },
                VariantDecl {
                    name: Ident::new("Hit"),
                    shape: VariantShape::Tuple(vec![int, bool_ty]),
                    raw_value: None,
                },
                VariantDecl {
                    name: Ident::new("Move"),
                    shape: VariantShape::Struct(vec![
                        FieldDecl {
                            name: Ident::new("x"),
                            ty: int,
                        },
                        FieldDecl {
                            name: Ident::new("y"),
                            ty: int,
                        },
                    ]),
                    raw_value: None,
                },
            ],
        });
        program.module_mut(module).enums.push(enum_id);
        let event = program.alloc_type(TypeData::Enum(enum_id));
        let seven = program.const_arena.alloc(ConstData {
            ty: int,
            value: ConstValue::Int(7),
        });
        let nine = program.const_arena.alloc(ConstData {
            ty: int,
            value: ConstValue::Int(9),
        });
        let yes = program.const_arena.alloc(ConstData {
            ty: bool_ty,
            value: ConstValue::Bool(true),
        });
        let main = program.alloc_function(Function {
            name: Ident::new("main"),
            module,
            kind: FunctionKind::Normal,
            owner: None,
            specialization: None,
            signature: Signature::new(vec![], void),
            locals: vec![
                local(event, LocalKind::Temp),
                local(event, LocalKind::Temp),
                local(event, LocalKind::Temp),
            ],
            body: structured_body(
                vec![
                    enum_init(0, event, enum_id, 0, vec![]),
                    enum_init(
                        1,
                        event,
                        enum_id,
                        1,
                        vec![Operand::Const(seven), Operand::Const(yes)],
                    ),
                    enum_init(
                        2,
                        event,
                        enum_id,
                        2,
                        vec![Operand::Const(seven), Operand::Const(nine)],
                    ),
                ],
                air::AirTail::Return(None),
            ),
        });
        program.module_mut(module).functions.push(main);
        program.set_entry(main);
        program
    }

    fn enum_with_string_payload_program(stringify: bool) -> Program {
        let mut program = Program::default();
        let void = program.alloc_type(TypeData::Void);
        let string = program.alloc_type(TypeData::String);
        let module = program.alloc_module(root_module());
        let enum_id = program.alloc_enum(EnumDecl {
            name: Ident::new("Message"),
            module,
            type_args: vec![],
            const_args: vec![],
            core: None,
            repr: air::EnumRepr::Adt,
            raw_type: None,
            variants: vec![VariantDecl {
                name: Ident::new("Text"),
                shape: VariantShape::Tuple(vec![string]),
                raw_value: None,
            }],
        });
        program.module_mut(module).enums.push(enum_id);
        let message = program.alloc_type(TypeData::Enum(enum_id));
        let text = program.const_arena.alloc(ConstData {
            ty: string,
            value: ConstValue::String("x".into()),
        });
        let value = air::LocalId::from_index(0);
        let mut statements = vec![enum_init(
            0,
            message,
            enum_id,
            0,
            vec![Operand::Const(text)],
        )];
        if stringify {
            statements.push(Statement::Eval(RValue::Stringify {
                value: Operand::Place(place(value, message)),
                source_ty: message,
            }));
        }
        let main = program.alloc_function(Function {
            name: Ident::new("main"),
            module,
            kind: FunctionKind::Normal,
            owner: None,
            specialization: None,
            signature: Signature::new(vec![], void),
            locals: vec![local(message, LocalKind::Temp)],
            body: structured_body(statements, air::AirTail::Return(None)),
        });
        program.module_mut(module).functions.push(main);
        program.set_entry(main);
        program
    }

    fn enum_type(program: &Program) -> air::TypeId {
        program
            .type_arena
            .iter()
            .position(|ty| matches!(ty, TypeData::Enum(_)))
            .map(air::TypeId::from_index)
            .expect("enum type missing")
    }

    fn enum_init(
        local: usize,
        ty: air::TypeId,
        enum_id: air::EnumId,
        variant: usize,
        fields: Vec<Operand>,
    ) -> Statement {
        Statement::Init {
            local: air::LocalId::from_index(local),
            value: RValue::Aggregate {
                kind: AggregateCtor::EnumVariant {
                    enum_id,
                    variant: VariantId::from_index(variant),
                },
                fields,
                ty,
            },
        }
    }
}

#[test]
fn rir_option_match_verifies_and_emits_match_by_ref() {
    let program = option_match_rir();
    let verified = rir::verify(&program).expect("RIR verify failed");
    let source = emit::emit(&verified).into_string();

    assert!(source.contains("match &opt"));
    assert!(source.contains("Some(__anv_option_payload)"));
    assert!(source.contains("payload = *(__anv_option_payload);"));
    assert!(!source.contains("__anv_option_payload = *(__anv_option_payload);"));
    assert!(!source.contains("unwrap()"));
}

#[test]
fn rir_option_match_rejects_bad_discriminant_payload_type_and_mutable_payload() {
    let mut bad_discr = option_match_rir();
    let int = RirTypeId::from_index(0);
    if let RirStmt::OptionMatch(match_) = &mut bad_discr.functions[0].body.stmts[0] {
        match_.discr.ty = int;
    }
    let errors = rir::verify(&bad_discr).expect_err("bad option discr should fail");
    assert!(
        errors
            .iter()
            .any(|error| error.kind == RirVerifyErrorKind::UnsupportedRValueType)
    );

    let mut bad_payload = option_match_rir();
    let option = RirTypeId::from_index(1);
    bad_payload.types.push(RirType::Bool);
    bad_payload.functions[0].locals[1].ty = RirTypeId::from_index(2);
    if let RirStmt::OptionMatch(match_) = &mut bad_payload.functions[0].body.stmts[0] {
        match_.discr.ty = option;
    }
    let errors = rir::verify(&bad_payload).expect_err("bad payload should fail");
    assert!(errors.iter().any(|error| matches!(
        error.kind,
        RirVerifyErrorKind::TypeMismatch { expected, found }
            if expected == int && found == RirTypeId::from_index(2)
    )));

    let mut mutable_payload = option_match_rir();
    mutable_payload.functions[0].locals[1].mutable = true;
    let errors = rir::verify(&mutable_payload).expect_err("mutable payload should fail");
    assert!(
        errors
            .iter()
            .any(|error| error.kind == RirVerifyErrorKind::ImmutableAssign)
    );
}

#[test]
fn rir_optional_some_rejects_non_shareable_place_value() {
    let mut program = option_match_rir();
    let slice = RirTypeId::from_index(2);
    let option = RirTypeId::from_index(1);
    program.types.push(RirType::Slice(RirTypeId::from_index(0)));
    program.types[1] = RirType::Option(slice);
    program.functions[0].params[0].ty = option;
    program.functions[0].locals[0].ty = option;
    program.functions[0].locals[1].ty = slice;
    program.functions[0].locals[1].initialized = true;
    program.functions[0].body = RirStructuredBlock {
        stmts: vec![RirStmt::Eval(RirRValue::OptionalSome {
            value: RirOperand::Place(RirPlace {
                local: RirLocalId::from_index(1),
                projections: vec![],
                ty: slice,
            }),
            ty: option,
        })],
        term: RirTerm::Unreachable,
    };
    let errors = rir::verify(&program).expect_err("slice some should fail");
    assert!(
        errors
            .iter()
            .any(|error| error.kind == RirVerifyErrorKind::NonCopyValueRequired)
    );
}

#[test]
fn rir_option_match_rejects_payload_copy_from_unowned_ref() {
    let mut slice_payload = option_match_rir();
    let slice = RirTypeId::from_index(2);
    slice_payload
        .types
        .push(RirType::Slice(RirTypeId::from_index(0)));
    slice_payload.types[1] = RirType::Option(slice);
    slice_payload.functions[0].params[0].ty = RirTypeId::from_index(1);
    slice_payload.functions[0].locals[0].ty = RirTypeId::from_index(1);
    slice_payload.functions[0].locals[1].ty = slice;
    if let RirStmt::OptionMatch(match_) = &mut slice_payload.functions[0].body.stmts[0] {
        match_.discr.ty = RirTypeId::from_index(1);
        match_.some_block.term = RirTerm::Unreachable;
        match_.none_block.term = RirTerm::Unreachable;
    }
    let errors = rir::verify(&slice_payload).expect_err("slice payload should fail");
    assert!(
        errors
            .iter()
            .any(|error| error.kind == RirVerifyErrorKind::NonCopyValueRequired)
    );

    let mut void_payload = option_match_rir();
    let void = RirTypeId::from_index(2);
    void_payload.types.push(RirType::Void);
    void_payload.types[1] = RirType::Option(void);
    void_payload.functions[0].params[0].ty = RirTypeId::from_index(1);
    void_payload.functions[0].locals[0].ty = RirTypeId::from_index(1);
    void_payload.functions[0].locals[1].ty = void;
    if let RirStmt::OptionMatch(match_) = &mut void_payload.functions[0].body.stmts[0] {
        match_.discr.ty = RirTypeId::from_index(1);
        match_.some_block.term = RirTerm::Unreachable;
        match_.none_block.term = RirTerm::Unreachable;
    }
    let errors = rir::verify(&void_payload).expect_err("void payload should fail");
    assert!(
        errors
            .iter()
            .any(|error| error.kind == RirVerifyErrorKind::NonCopyValueRequired)
    );
}

#[test]
fn rir_option_match_rejects_invalid_escaping_payload() {
    let mut missing_payload = option_match_rir();
    if let RirStmt::OptionMatch(match_) = &mut missing_payload.functions[0].body.stmts[0] {
        match_.payload = None;
        match_.payload_ref = true;
        match_.payload_escapes = true;
    }
    let errors = rir::verify(&missing_payload).expect_err("escaping payload requires payload");
    assert!(
        errors
            .iter()
            .any(|error| error.kind == RirVerifyErrorKind::OptionPayloadEscapeRequiresPayload)
    );

    let mut non_ref = option_match_rir();
    if let RirStmt::OptionMatch(match_) = &mut non_ref.functions[0].body.stmts[0] {
        match_.payload_escapes = true;
    }
    let errors = rir::verify(&non_ref).expect_err("escaping payload requires ref");
    assert!(
        errors
            .iter()
            .any(|error| error.kind == RirVerifyErrorKind::OptionPayloadEscapeRequiresRef)
    );

    let mut falling_none = option_match_rir();
    falling_none.functions[0].locals[0].mutable = true;
    falling_none.functions[0].locals[1].mutable = true;
    falling_none.functions[0].locals[1].payload_ref = true;
    if let RirStmt::OptionMatch(match_) = &mut falling_none.functions[0].body.stmts[0] {
        match_.payload_ref = true;
        match_.payload_escapes = true;
        match_.none_block.term = RirTerm::None;
    }
    let errors = rir::verify(&falling_none).expect_err("escaping none branch must diverge");
    assert!(
        errors
            .iter()
            .any(|error| error.kind == RirVerifyErrorKind::OptionPayloadEscapeNoneMustDiverge)
    );
}

#[test]
fn rir_option_match_rejects_invalid_payload_ref_shape() {
    let mut local_mismatch = option_match_rir();
    local_mismatch.functions[0].locals[0].mutable = true;
    local_mismatch.functions[0].locals[1].mutable = true;
    if let RirStmt::OptionMatch(match_) = &mut local_mismatch.functions[0].body.stmts[0] {
        match_.payload_ref = true;
    }
    let errors = rir::verify(&local_mismatch).expect_err("payload_ref needs payload local flag");
    assert!(
        errors
            .iter()
            .any(|error| error.kind == RirVerifyErrorKind::OptionPayloadRefLocalMismatch)
    );

    let mut immutable_discr = option_match_rir();
    immutable_discr.functions[0].locals[1].mutable = true;
    immutable_discr.functions[0].locals[1].payload_ref = true;
    if let RirStmt::OptionMatch(match_) = &mut immutable_discr.functions[0].body.stmts[0] {
        match_.payload_ref = true;
    }
    let errors = rir::verify(&immutable_discr).expect_err("payload_ref needs mutable discriminant");
    assert!(errors.iter().any(|error| {
        error.kind == RirVerifyErrorKind::OptionPayloadRefDiscriminantMustBeMutable
    }));

    let mut stray_payload_ref = option_match_rir();
    stray_payload_ref.functions[0].locals[1].payload_ref = true;
    let errors = rir::verify(&stray_payload_ref).expect_err("payload_ref needs option match owner");
    assert!(
        errors
            .iter()
            .any(|error| error.kind == RirVerifyErrorKind::OptionPayloadRefWithoutOwner)
    );

    let mut normal_init_payload_ref = option_match_rir();
    normal_init_payload_ref.functions[0].locals[0].mutable = true;
    normal_init_payload_ref.functions[0].locals[1].mutable = true;
    normal_init_payload_ref.functions[0].locals[1].payload_ref = true;
    if let RirStmt::OptionMatch(match_) = &mut normal_init_payload_ref.functions[0].body.stmts[0] {
        match_.payload_ref = true;
    }
    normal_init_payload_ref.functions[0]
        .body
        .stmts
        .push(RirStmt::Init {
            local: RirLocalId::from_index(1),
            value: RirRValue::Use(RirOperand::Const(RirConstId::from_index(0))),
        });
    let errors = rir::verify(&normal_init_payload_ref)
        .expect_err("payload_ref local must be initialized only by option match");
    assert!(
        errors
            .iter()
            .any(|error| error.kind == RirVerifyErrorKind::InitPayloadRefLocal)
    );

    let mut nested_scoped_ref_used_after_block = option_match_rir();
    nested_scoped_ref_used_after_block.types.push(RirType::Bool);
    nested_scoped_ref_used_after_block.consts.push(RirConst {
        id: RirConstId::from_index(1),
        ty: RirTypeId::from_index(2),
        value: RirConstValue::Bool(true),
    });
    nested_scoped_ref_used_after_block.functions[0].locals[0].mutable = true;
    nested_scoped_ref_used_after_block.functions[0].locals[1].mutable = true;
    nested_scoped_ref_used_after_block.functions[0].locals[1].payload_ref = true;
    let mut option_match = nested_scoped_ref_used_after_block.functions[0]
        .body
        .stmts
        .remove(0);
    if let RirStmt::OptionMatch(match_) = &mut option_match {
        match_.payload_ref = true;
        match_.payload_escapes = true;
        match_.some_block.term = RirTerm::None;
        match_.none_block.term = RirTerm::Unreachable;
    }
    nested_scoped_ref_used_after_block.functions[0]
        .body
        .stmts
        .push(RirStmt::If(RirIf {
            cond: RirOperand::Const(RirConstId::from_index(1)),
            then_block: RirStructuredBlock {
                stmts: vec![option_match],
                term: RirTerm::None,
            },
            else_block: None,
        }));
    nested_scoped_ref_used_after_block.functions[0]
        .body
        .stmts
        .push(RirStmt::Eval(RirRValue::Use(RirOperand::Place(RirPlace {
            local: RirLocalId::from_index(1),
            projections: vec![],
            ty: RirTypeId::from_index(0),
        }))));
    let errors = rir::verify(&nested_scoped_ref_used_after_block)
        .expect_err("payload_ref from nested block must not escape block scope");
    assert!(errors.iter().any(|error| {
        error.kind == RirVerifyErrorKind::UninitializedLocal(RirLocalId::from_index(1))
    }));

    let mut scoped_ref_used_after_match = option_match_rir();
    scoped_ref_used_after_match.functions[0].locals[0].mutable = true;
    scoped_ref_used_after_match.functions[0].locals[1].mutable = true;
    scoped_ref_used_after_match.functions[0].locals[1].payload_ref = true;
    if let RirStmt::OptionMatch(match_) =
        &mut scoped_ref_used_after_match.functions[0].body.stmts[0]
    {
        match_.payload_ref = true;
        match_.some_block.term = RirTerm::None;
        match_.none_block.term = RirTerm::Unreachable;
    }
    scoped_ref_used_after_match.functions[0]
        .body
        .stmts
        .push(RirStmt::Eval(RirRValue::Use(RirOperand::Place(RirPlace {
            local: RirLocalId::from_index(1),
            projections: vec![],
            ty: RirTypeId::from_index(0),
        }))));
    let errors = rir::verify(&scoped_ref_used_after_match)
        .expect_err("non-escaping payload_ref must not outlive match arm");
    assert!(errors.iter().any(|error| {
        error.kind == RirVerifyErrorKind::UninitializedLocal(RirLocalId::from_index(1))
    }));
}

fn option_match_rir() -> RirProgram {
    let int = RirTypeId::from_index(0);
    let option = RirTypeId::from_index(1);
    let zero = RirConstId::from_index(0);
    let opt = RirLocalId::from_index(0);
    let payload = RirLocalId::from_index(1);
    RirProgram {
        types: vec![RirType::Int, RirType::Option(int)],
        consts: vec![RirConst {
            id: zero,
            ty: int,
            value: RirConstValue::Int(0),
        }],
        functions: vec![RirFunction {
            id: RirFunctionId::from_index(0),
            air_id: None,
            symbol: RirSymbol::new("main"),
            params: vec![RirParam {
                local: opt,
                ty: option,
                semantic: RirParamSemantic::Value,
                abi: RirParamAbi::Value,
                escape: RirParamEscape::NonEscaping,
            }],
            ret: RirReturn { ty: int },
            locals: vec![
                RirLocal {
                    id: opt,
                    ty: option,
                    mutable: false,
                    symbol: RirSymbol::new("opt"),
                    initialized: true,
                    payload_ref: false,
                },
                RirLocal {
                    id: payload,
                    ty: int,
                    mutable: false,
                    symbol: RirSymbol::new("payload"),
                    initialized: false,
                    payload_ref: false,
                },
            ],
            body: RirStructuredBlock {
                stmts: vec![RirStmt::OptionMatch(RirOptionMatch {
                    discr: RirPlace {
                        local: opt,
                        projections: vec![],
                        ty: option,
                    },
                    payload: Some(payload),
                    payload_ref: false,
                    payload_escapes: false,
                    some_block: RirStructuredBlock {
                        stmts: vec![],
                        term: RirTerm::Return(Some(RirOperand::Place(RirPlace {
                            local: payload,
                            projections: vec![],
                            ty: int,
                        }))),
                    },
                    none_block: RirStructuredBlock {
                        stmts: vec![],
                        term: RirTerm::Return(Some(RirOperand::Const(zero))),
                    },
                })],
                term: RirTerm::Unreachable,
            },
        }],
        entry: Some(RirFunctionId::from_index(0)),
        ..RirProgram::default()
    }
}

mod arrays {
    use super::*;

    #[test]
    fn constructs_indexes_and_gets_len() {
        let mut program = Program::default();
        let int = program.alloc_type(TypeData::Int);
        let array = program.alloc_type(TypeData::Array { elem: int, len: 2 });
        let module = program.alloc_module(root_module());
        let one = int_const(&mut program, int, 1);
        let two = int_const(&mut program, int, 2);
        let idx = int_const(&mut program, int, 1);
        let array_local = air::LocalId::from_index(0);
        let index_local = air::LocalId::from_index(1);
        let value_local = air::LocalId::from_index(2);
        let len_local = air::LocalId::from_index(3);
        let mut indexed = place(array_local, int);
        indexed.projection.push(Projection::Index(index_local));
        let main = program.alloc_function(Function {
            name: Ident::new("main"),
            module,
            kind: FunctionKind::Normal,
            owner: None,
            specialization: None,
            signature: Signature::new(vec![], int),
            locals: vec![
                local(array, LocalKind::Temp),
                local(int, LocalKind::Temp),
                local(int, LocalKind::Temp),
                local(int, LocalKind::Temp),
            ],
            body: structured_body(
                vec![
                    Statement::Init {
                        local: array_local,
                        value: RValue::Aggregate {
                            kind: AggregateCtor::Array,
                            fields: vec![Operand::Const(one), Operand::Const(two)],
                            ty: array,
                        },
                    },
                    Statement::Init {
                        local: index_local,
                        value: RValue::Use(Operand::Const(idx)),
                    },
                    Statement::Init {
                        local: value_local,
                        value: RValue::Use(Operand::Place(indexed)),
                    },
                    Statement::Init {
                        local: len_local,
                        value: RValue::Len {
                            source: place(array_local, array),
                        },
                    },
                ],
                air::AirTail::Return(Some(Operand::Place(place(value_local, int)))),
            ),
        });
        program.module_mut(module).functions.push(main);
        program.set_entry(main);

        let source = plan_source(program);
        let text = source.as_str();
        assert!(text.contains("[1, 2]"));
        assert!(!text.contains("Vec<"));
        assert!(!text.contains("vec!"));
        assert!(text.contains("v0[anvyx_runtime::checked_index(v1, 2)]"));
        assert!(!text.contains("negative index"));
        assert!(!text.contains("index out of bounds"));
        assert!(text.contains(".len() as i64"));
        let output = run_source(source);
        assert_eq!(output.status, SourceJobStatus::Success);
    }

    #[test]
    fn shared_borrow_array_param_uses_shared_borrow_abi() {
        let mut program = Program::default();
        let int = program.alloc_type(TypeData::Int);
        let array = program.alloc_type(TypeData::Array { elem: int, len: 1 });
        let module = program.alloc_module(root_module());
        let value = int_const(&mut program, int, 7);
        let arg = air::LocalId::from_index(0);
        let helper = program.alloc_function(Function {
            name: Ident::new("first"),
            module,
            kind: FunctionKind::Normal,
            owner: None,
            specialization: None,
            signature: Signature::new(
                vec![param("items", array, ParamMode::SharedBorrow, arg)],
                int,
            ),
            locals: vec![local(array, LocalKind::Arg)],
            body: structured_body(vec![], air::AirTail::Return(Some(Operand::Const(value)))),
        });
        program.module_mut(module).functions.push(helper);
        let array_local = air::LocalId::from_index(0);
        let main = program.alloc_function(Function {
            name: Ident::new("main"),
            module,
            kind: FunctionKind::Normal,
            owner: None,
            specialization: None,
            signature: Signature::new(vec![], int),
            locals: vec![local(array, LocalKind::Temp), local(int, LocalKind::Temp)],
            body: structured_body(
                vec![
                    Statement::Init {
                        local: array_local,
                        value: RValue::Aggregate {
                            kind: AggregateCtor::Array,
                            fields: vec![Operand::Const(value)],
                            ty: array,
                        },
                    },
                    Statement::Init {
                        local: air::LocalId::from_index(1),
                        value: RValue::Call {
                            callee: Callee::Function(helper),
                            args: vec![CallArg::SharedBorrow(place(array_local, array))],
                        },
                    },
                ],
                air::AirTail::Return(Some(Operand::Place(place(
                    air::LocalId::from_index(1),
                    int,
                )))),
            ),
        });
        program.module_mut(module).functions.push(main);
        program.set_entry(main);

        let source = plan_source(program);
        assert!(source.as_str().contains("&[i64; 1]"));
    }

    #[test]
    fn copies_array_of_copyable_structs_elementwise() {
        let mut program = Program::default();
        let int = program.alloc_type(TypeData::Int);
        let module = program.alloc_module(root_module());
        let point_id = program.alloc_aggregate(air::AggregateDecl {
            name: Ident::new("Point"),
            module,
            kind: air::AggregateKind::Struct,
            type_args: vec![],
            const_args: vec![],
            fields: vec![FieldDecl {
                name: Ident::new("x"),
                ty: int,
            }],
            cycle_capable: false,
            stringify_override: None,
        });
        program.module_mut(module).aggregates.push(point_id);
        let point = program.alloc_type(TypeData::Aggregate(point_id));
        let array = program.alloc_type(TypeData::Array {
            elem: point,
            len: 1,
        });
        let one = int_const(&mut program, int, 1);
        let point_local = air::LocalId::from_index(0);
        let array_local = air::LocalId::from_index(1);
        let copy_local = air::LocalId::from_index(2);
        let main = program.alloc_function(Function {
            name: Ident::new("main"),
            module,
            kind: FunctionKind::Normal,
            owner: None,
            specialization: None,
            signature: Signature::new(vec![], array),
            locals: vec![
                local(point, LocalKind::Temp),
                local(array, LocalKind::Temp),
                local(array, LocalKind::Temp),
            ],
            body: structured_body(
                vec![
                    Statement::Init {
                        local: point_local,
                        value: RValue::Aggregate {
                            kind: AggregateCtor::Struct(point_id),
                            fields: vec![Operand::Const(one)],
                            ty: point,
                        },
                    },
                    Statement::Init {
                        local: array_local,
                        value: RValue::Aggregate {
                            kind: AggregateCtor::Array,
                            fields: vec![Operand::Place(place(point_local, point))],
                            ty: array,
                        },
                    },
                    Statement::Init {
                        local: copy_local,
                        value: RValue::Use(Operand::Place(place(array_local, array))),
                    },
                ],
                air::AirTail::Return(Some(Operand::Place(place(copy_local, array)))),
            ),
        });
        program.module_mut(module).functions.push(main);
        program.set_entry(main);

        let source = plan_source(program);
        assert!(source.as_str().contains("Point { x: *(&(&v1[0]).x) }]"));
        assert!(!source.as_str().contains("clone"));
        let output = run_source(source);
        assert_eq!(output.status, SourceJobStatus::Success);
    }

    #[test]
    fn copies_array_of_copyable_enums_elementwise() {
        let mut program = Program::default();
        let int = program.alloc_type(TypeData::Int);
        let module = program.alloc_module(root_module());
        let enum_id = program.alloc_enum(EnumDecl {
            name: Ident::new("Choice"),
            module,
            type_args: vec![],
            const_args: vec![],
            core: None,
            repr: air::EnumRepr::Adt,
            raw_type: None,
            variants: vec![VariantDecl {
                name: Ident::new("Some"),
                shape: VariantShape::Tuple(vec![int]),
                raw_value: None,
            }],
        });
        program.module_mut(module).enums.push(enum_id);
        let choice = program.alloc_type(TypeData::Enum(enum_id));
        let array = program.alloc_type(TypeData::Array {
            elem: choice,
            len: 1,
        });
        let one = int_const(&mut program, int, 1);
        let enum_local = air::LocalId::from_index(0);
        let array_local = air::LocalId::from_index(1);
        let copy_local = air::LocalId::from_index(2);
        let main = program.alloc_function(Function {
            name: Ident::new("main"),
            module,
            kind: FunctionKind::Normal,
            owner: None,
            specialization: None,
            signature: Signature::new(vec![], array),
            locals: vec![
                local(choice, LocalKind::Temp),
                local(array, LocalKind::Temp),
                local(array, LocalKind::Temp),
            ],
            body: structured_body(
                vec![
                    Statement::Init {
                        local: enum_local,
                        value: RValue::Aggregate {
                            kind: AggregateCtor::EnumVariant {
                                enum_id,
                                variant: VariantId::from_index(0),
                            },
                            fields: vec![Operand::Const(one)],
                            ty: choice,
                        },
                    },
                    Statement::Init {
                        local: array_local,
                        value: RValue::Aggregate {
                            kind: AggregateCtor::Array,
                            fields: vec![Operand::Place(place(enum_local, choice))],
                            ty: array,
                        },
                    },
                    Statement::Init {
                        local: copy_local,
                        value: RValue::Use(Operand::Place(place(array_local, array))),
                    },
                ],
                air::AirTail::Return(Some(Operand::Place(place(copy_local, array)))),
            ),
        });
        program.module_mut(module).functions.push(main);
        program.set_entry(main);

        let source = plan_source(program);
        assert!(source.as_str().contains("match &v1[0]"));
        assert!(!source.as_str().contains("clone"));
        let output = run_source(source);
        assert_eq!(output.status, SourceJobStatus::Success);
    }

    #[test]
    fn negative_index_panics_without_wrapping() {
        let mut program = Program::default();
        let int = program.alloc_type(TypeData::Int);
        let array = program.alloc_type(TypeData::Array { elem: int, len: 1 });
        let module = program.alloc_module(root_module());
        let value = int_const(&mut program, int, 1);
        let negative = int_const(&mut program, int, -1);
        let array_local = air::LocalId::from_index(0);
        let index_local = air::LocalId::from_index(1);
        let value_local = air::LocalId::from_index(2);
        let mut indexed = place(array_local, int);
        indexed.projection.push(Projection::Index(index_local));
        let main = program.alloc_function(Function {
            name: Ident::new("main"),
            module,
            kind: FunctionKind::Normal,
            owner: None,
            specialization: None,
            signature: Signature::new(vec![], int),
            locals: vec![
                local(array, LocalKind::Temp),
                local(int, LocalKind::Temp),
                local(int, LocalKind::Temp),
            ],
            body: structured_body(
                vec![
                    Statement::Init {
                        local: array_local,
                        value: RValue::Aggregate {
                            kind: AggregateCtor::Array,
                            fields: vec![Operand::Const(value)],
                            ty: array,
                        },
                    },
                    Statement::Init {
                        local: index_local,
                        value: RValue::Use(Operand::Const(negative)),
                    },
                    Statement::Init {
                        local: value_local,
                        value: RValue::Use(Operand::Place(indexed)),
                    },
                ],
                air::AirTail::Return(Some(Operand::Place(place(value_local, int)))),
            ),
        });
        program.module_mut(module).functions.push(main);
        program.set_entry(main);

        let source = plan_source(program);
        let output = run_source(source);
        assert!(matches!(output.status, SourceJobStatus::RunFailed(_)));
        assert!(output.stderr.contains("negative index"));
    }

    #[test]
    fn out_of_bounds_index_panics() {
        let mut program = Program::default();
        let int = program.alloc_type(TypeData::Int);
        let array = program.alloc_type(TypeData::Array { elem: int, len: 1 });
        let module = program.alloc_module(root_module());
        let value = int_const(&mut program, int, 1);
        let too_large = int_const(&mut program, int, 1);
        let array_local = air::LocalId::from_index(0);
        let index_local = air::LocalId::from_index(1);
        let value_local = air::LocalId::from_index(2);
        let mut indexed = place(array_local, int);
        indexed.projection.push(Projection::Index(index_local));
        let main = program.alloc_function(Function {
            name: Ident::new("main"),
            module,
            kind: FunctionKind::Normal,
            owner: None,
            specialization: None,
            signature: Signature::new(vec![], int),
            locals: vec![
                local(array, LocalKind::Temp),
                local(int, LocalKind::Temp),
                local(int, LocalKind::Temp),
            ],
            body: structured_body(
                vec![
                    Statement::Init {
                        local: array_local,
                        value: RValue::Aggregate {
                            kind: AggregateCtor::Array,
                            fields: vec![Operand::Const(value)],
                            ty: array,
                        },
                    },
                    Statement::Init {
                        local: index_local,
                        value: RValue::Use(Operand::Const(too_large)),
                    },
                    Statement::Init {
                        local: value_local,
                        value: RValue::Use(Operand::Place(indexed)),
                    },
                ],
                air::AirTail::Return(Some(Operand::Place(place(value_local, int)))),
            ),
        });
        program.module_mut(module).functions.push(main);
        program.set_entry(main);

        let source = plan_source(program);
        let output = run_source(source);
        assert!(matches!(output.status, SourceJobStatus::RunFailed(_)));
        assert!(output.stderr.contains("index out of bounds"));
    }

    #[test]
    fn noncopy_array_value_copy_is_target_gap() {
        let mut program = Program::default();
        let string = program.alloc_type(TypeData::String);
        let array = program.alloc_type(TypeData::Array {
            elem: string,
            len: 1,
        });
        let module = program.alloc_module(root_module());
        let text = program.const_arena.alloc(ConstData {
            ty: string,
            value: ConstValue::String("x".into()),
        });
        let array_local = air::LocalId::from_index(0);
        let copy_local = air::LocalId::from_index(1);
        let main = program.alloc_function(Function {
            name: Ident::new("main"),
            module,
            kind: FunctionKind::Normal,
            owner: None,
            specialization: None,
            signature: Signature::new(vec![], array),
            locals: vec![local(array, LocalKind::Temp), local(array, LocalKind::Temp)],
            body: structured_body(
                vec![
                    Statement::Init {
                        local: array_local,
                        value: RValue::Aggregate {
                            kind: AggregateCtor::Array,
                            fields: vec![Operand::Const(text)],
                            ty: array,
                        },
                    },
                    Statement::Init {
                        local: copy_local,
                        value: RValue::Use(Operand::Place(place(array_local, array))),
                    },
                ],
                air::AirTail::Return(Some(Operand::Place(place(copy_local, array)))),
            ),
        });
        program.module_mut(module).functions.push(main);
        program.set_entry(main);

        let errors = profile_errors(program);
        assert!(has_error(&errors, ProfileErrorKind::NonCopyValueRequired));
    }

    #[test]
    fn verifier_rejects_index_local_with_non_int_type() {
        let mut program = RirProgram::default();
        let int = RirTypeId::from_index(0);
        let bool_ty = RirTypeId::from_index(1);
        let array = RirTypeId::from_index(2);
        program.types.push(RirType::Int);
        program.types.push(RirType::Bool);
        program.types.push(RirType::Array { elem: int, len: 1 });
        program.functions.push(RirFunction {
            id: RirFunctionId::from_index(0),
            air_id: None,
            symbol: RirSymbol::new("f"),
            params: vec![],
            ret: RirReturn { ty: int },
            locals: vec![
                RirLocal {
                    id: RirLocalId::from_index(0),
                    ty: array,
                    mutable: false,
                    symbol: RirSymbol::new("a"),
                    initialized: true,
                    payload_ref: false,
                },
                RirLocal {
                    id: RirLocalId::from_index(1),
                    ty: bool_ty,
                    mutable: false,
                    symbol: RirSymbol::new("i"),
                    initialized: true,
                    payload_ref: false,
                },
            ],
            body: RirStructuredBlock {
                stmts: vec![],
                term: RirTerm::Return(Some(RirOperand::Place(RirPlace {
                    local: RirLocalId::from_index(0),
                    projections: vec![RirProjection::Index(RirLocalId::from_index(1))],
                    ty: int,
                }))),
            },
        });

        let errors = rir::verify(&program).expect_err("bad index local should fail");
        assert!(
            errors
                .iter()
                .any(|error| error.kind == RirVerifyErrorKind::UnsupportedRValueType)
        );
    }
}

mod lists {
    use super::*;

    #[test]
    fn constructs_pushes_indexes_and_gets_len() {
        let mut program = Program::default();
        let int = program.alloc_type(TypeData::Int);
        program.alloc_type(TypeData::Void);
        let list = program.alloc_type(TypeData::List(int));
        let module = program.alloc_module(root_module());
        let one = int_const(&mut program, int, 1);
        let two = int_const(&mut program, int, 2);
        let idx = int_const(&mut program, int, 1);
        let list_local = air::LocalId::from_index(0);
        let index_local = air::LocalId::from_index(1);
        let value_local = air::LocalId::from_index(2);
        let len_local = air::LocalId::from_index(3);
        let mut indexed = place(list_local, int);
        indexed.projection.push(Projection::Index(index_local));
        let main = program.alloc_function(Function {
            name: Ident::new("main"),
            module,
            kind: FunctionKind::Normal,
            owner: None,
            specialization: None,
            signature: Signature::new(vec![], int),
            locals: vec![
                mut_local(list, LocalKind::Temp),
                local(int, LocalKind::Temp),
                local(int, LocalKind::Temp),
                local(int, LocalKind::Temp),
            ],
            body: structured_body(
                vec![
                    Statement::Init {
                        local: list_local,
                        value: RValue::Aggregate {
                            kind: AggregateCtor::List,
                            fields: vec![Operand::Const(one)],
                            ty: list,
                        },
                    },
                    Statement::Eval(RValue::ListPush {
                        list: place(list_local, list),
                        value: Operand::Const(two),
                    }),
                    Statement::Init {
                        local: index_local,
                        value: RValue::Use(Operand::Const(idx)),
                    },
                    Statement::Init {
                        local: value_local,
                        value: RValue::Use(Operand::Place(indexed)),
                    },
                    Statement::Init {
                        local: len_local,
                        value: RValue::Len {
                            source: place(list_local, list),
                        },
                    },
                ],
                air::AirTail::Return(Some(Operand::Place(place(value_local, int)))),
            ),
        });
        program.module_mut(module).functions.push(main);
        program.set_entry(main);

        let source = plan_source(program);
        let text = source.as_str();
        assert!(text.contains("anvyx_runtime::AnvList::from_elems([1])"));
        assert!(!text.contains("Vec<"));
        assert!(!text.contains("vec!"));
        assert!(text.contains(".push(2)"));
        assert!(text.contains("v0[anvyx_runtime::checked_index(v1, v0.len())]"));
        assert!(!text.contains("negative index"));
        assert!(!text.contains("index out of bounds"));
        assert!(text.contains(".len() as i64"));
        let output = run_source(source);
        assert_eq!(output.status, SourceJobStatus::Success, "{}", output.stderr);
    }

    #[test]
    fn shared_borrow_list_param_uses_shared_borrow_abi() {
        let mut program = Program::default();
        let int = program.alloc_type(TypeData::Int);
        let list = program.alloc_type(TypeData::List(int));
        let module = program.alloc_module(root_module());
        let value = int_const(&mut program, int, 7);
        let arg = air::LocalId::from_index(0);
        let helper = program.alloc_function(Function {
            name: Ident::new("first"),
            module,
            kind: FunctionKind::Normal,
            owner: None,
            specialization: None,
            signature: Signature::new(
                vec![param("items", list, ParamMode::SharedBorrow, arg)],
                int,
            ),
            locals: vec![local(list, LocalKind::Arg)],
            body: structured_body(vec![], air::AirTail::Return(Some(Operand::Const(value)))),
        });
        program.module_mut(module).functions.push(helper);
        let list_local = air::LocalId::from_index(0);
        let main = program.alloc_function(Function {
            name: Ident::new("main"),
            module,
            kind: FunctionKind::Normal,
            owner: None,
            specialization: None,
            signature: Signature::new(vec![], int),
            locals: vec![local(list, LocalKind::Temp), local(int, LocalKind::Temp)],
            body: structured_body(
                vec![
                    Statement::Init {
                        local: list_local,
                        value: RValue::Aggregate {
                            kind: AggregateCtor::List,
                            fields: vec![Operand::Const(value)],
                            ty: list,
                        },
                    },
                    Statement::Init {
                        local: air::LocalId::from_index(1),
                        value: RValue::Call {
                            callee: Callee::Function(helper),
                            args: vec![CallArg::SharedBorrow(place(list_local, list))],
                        },
                    },
                ],
                air::AirTail::Return(Some(Operand::Place(place(
                    air::LocalId::from_index(1),
                    int,
                )))),
            ),
        });
        program.module_mut(module).functions.push(main);
        program.set_entry(main);

        let source = plan_source(program);
        assert!(source.as_str().contains("&anvyx_runtime::AnvList<i64>"));
    }

    #[test]
    fn negative_index_panics_without_wrapping() {
        let mut program = Program::default();
        let int = program.alloc_type(TypeData::Int);
        let list = program.alloc_type(TypeData::List(int));
        let module = program.alloc_module(root_module());
        let value = int_const(&mut program, int, 1);
        let negative = int_const(&mut program, int, -1);
        let list_local = air::LocalId::from_index(0);
        let index_local = air::LocalId::from_index(1);
        let value_local = air::LocalId::from_index(2);
        let mut indexed = place(list_local, int);
        indexed.projection.push(Projection::Index(index_local));
        let main = program.alloc_function(Function {
            name: Ident::new("main"),
            module,
            kind: FunctionKind::Normal,
            owner: None,
            specialization: None,
            signature: Signature::new(vec![], int),
            locals: vec![
                local(list, LocalKind::Temp),
                local(int, LocalKind::Temp),
                local(int, LocalKind::Temp),
            ],
            body: structured_body(
                vec![
                    Statement::Init {
                        local: list_local,
                        value: RValue::Aggregate {
                            kind: AggregateCtor::List,
                            fields: vec![Operand::Const(value)],
                            ty: list,
                        },
                    },
                    Statement::Init {
                        local: index_local,
                        value: RValue::Use(Operand::Const(negative)),
                    },
                    Statement::Init {
                        local: value_local,
                        value: RValue::Use(Operand::Place(indexed)),
                    },
                ],
                air::AirTail::Return(Some(Operand::Place(place(value_local, int)))),
            ),
        });
        program.module_mut(module).functions.push(main);
        program.set_entry(main);

        let source = plan_source(program);
        let output = run_source(source);
        assert!(matches!(output.status, SourceJobStatus::RunFailed(_)));
        assert!(output.stderr.contains("negative index"));
    }

    #[test]
    fn list_value_copy_uses_cow_runtime_value() {
        let mut program = Program::default();
        let int = program.alloc_type(TypeData::Int);
        let list = program.alloc_type(TypeData::List(int));
        let module = program.alloc_module(root_module());
        let one = int_const(&mut program, int, 1);
        let list_local = air::LocalId::from_index(0);
        let copy_local = air::LocalId::from_index(1);
        let main = program.alloc_function(Function {
            name: Ident::new("main"),
            module,
            kind: FunctionKind::Normal,
            owner: None,
            specialization: None,
            signature: Signature::new(vec![], list),
            locals: vec![local(list, LocalKind::Temp), local(list, LocalKind::Temp)],
            body: structured_body(
                vec![
                    Statement::Init {
                        local: list_local,
                        value: RValue::Aggregate {
                            kind: AggregateCtor::List,
                            fields: vec![Operand::Const(one)],
                            ty: list,
                        },
                    },
                    Statement::Init {
                        local: copy_local,
                        value: RValue::Use(Operand::Place(place(list_local, list))),
                    },
                ],
                air::AirTail::Return(Some(Operand::Place(place(copy_local, list)))),
            ),
        });
        program.module_mut(module).functions.push(main);
        program.set_entry(main);

        let source = plan_source(program);
        assert!(source.as_str().contains("anvyx_runtime::AnvList"));
        assert!(source.as_str().contains(".share()"));
        assert!(!source.as_str().contains("Vec<"));
        assert!(!source.as_str().contains("vec!"));
    }

    #[test]
    fn pop_remains_target_gap_until_optional_values_exist() {
        let mut program = Program::default();
        let int = program.alloc_type(TypeData::Int);
        let optional = program.alloc_type(TypeData::Optional(int));
        let list = program.alloc_type(TypeData::List(int));
        let module = program.alloc_module(root_module());
        let one = int_const(&mut program, int, 1);
        let list_local = air::LocalId::from_index(0);
        let main = program.alloc_function(Function {
            name: Ident::new("main"),
            module,
            kind: FunctionKind::Normal,
            owner: None,
            specialization: None,
            signature: Signature::new(vec![], int),
            locals: vec![local(list, LocalKind::Temp)],
            body: structured_body(
                vec![
                    Statement::Init {
                        local: list_local,
                        value: RValue::Aggregate {
                            kind: AggregateCtor::List,
                            fields: vec![Operand::Const(one)],
                            ty: list,
                        },
                    },
                    Statement::Eval(RValue::ListPop {
                        list: place(list_local, list),
                        ty: optional,
                    }),
                ],
                air::AirTail::Return(Some(Operand::Const(one))),
            ),
        });
        program.module_mut(module).functions.push(main);
        program.set_entry(main);

        expect_reject(program, ProfileErrorKind::UnsupportedRValue);
    }
}

mod slices {
    use super::*;

    #[test]
    fn array_slice_view_compiles_with_checked_range() {
        let mut program = Program::default();
        let int = program.alloc_type(TypeData::Int);
        let array = program.alloc_type(TypeData::Array { elem: int, len: 3 });
        let slice = program.alloc_type(TypeData::Slice(int));
        let module = program.alloc_module(root_module());
        let zero = int_const(&mut program, int, 0);
        let two = int_const(&mut program, int, 2);
        let one = int_const(&mut program, int, 1);
        let array_local = air::LocalId::from_index(0);
        let start_local = air::LocalId::from_index(1);
        let end_local = air::LocalId::from_index(2);
        let slice_local = air::LocalId::from_index(3);
        let main = program.alloc_function(Function {
            name: Ident::new("main"),
            module,
            kind: FunctionKind::Normal,
            owner: None,
            specialization: None,
            signature: Signature::new(vec![], int),
            locals: vec![
                local(array, LocalKind::Temp),
                local(int, LocalKind::Temp),
                local(int, LocalKind::Temp),
                local(slice, LocalKind::Temp),
            ],
            body: structured_body(
                vec![
                    Statement::Init {
                        local: array_local,
                        value: RValue::Aggregate {
                            kind: AggregateCtor::Array,
                            fields: vec![
                                Operand::Const(one),
                                Operand::Const(two),
                                Operand::Const(two),
                            ],
                            ty: array,
                        },
                    },
                    Statement::Init {
                        local: start_local,
                        value: RValue::Use(Operand::Const(zero)),
                    },
                    Statement::Init {
                        local: end_local,
                        value: RValue::Use(Operand::Const(two)),
                    },
                    Statement::Init {
                        local: slice_local,
                        value: RValue::SliceView {
                            source: place(array_local, array),
                            start: start_local,
                            end: end_local,
                            inclusive: false,
                            ty: slice,
                        },
                    },
                ],
                air::AirTail::Return(Some(Operand::Const(one))),
            ),
        });
        program.module_mut(module).functions.push(main);
        program.set_entry(main);

        let source = plan_source(program);
        let text = source.as_str();
        assert!(text.contains("&[i64]"));
        assert!(text.contains("&v0[anvyx_runtime::checked_range(v1, v2, false, v0.len())]"));
        assert!(!text.contains("negative range bound"));
        assert!(!text.contains("range out of bounds"));
        let output = run_source(source);
        assert_eq!(output.status, SourceJobStatus::Success, "{}", output.stderr);
    }

    #[test]
    fn list_slice_rebuilds_owned_list_without_clone() {
        let mut program = Program::default();
        let int = program.alloc_type(TypeData::Int);
        let list = program.alloc_type(TypeData::List(int));
        let module = program.alloc_module(root_module());
        let zero = int_const(&mut program, int, 0);
        let two = int_const(&mut program, int, 2);
        let list_local = air::LocalId::from_index(0);
        let start_local = air::LocalId::from_index(1);
        let end_local = air::LocalId::from_index(2);
        let slice_local = air::LocalId::from_index(3);
        let len_local = air::LocalId::from_index(4);
        let main = program.alloc_function(Function {
            name: Ident::new("main"),
            module,
            kind: FunctionKind::Normal,
            owner: None,
            specialization: None,
            signature: Signature::new(vec![], int),
            locals: vec![
                local(list, LocalKind::Temp),
                local(int, LocalKind::Temp),
                local(int, LocalKind::Temp),
                local(list, LocalKind::Temp),
                local(int, LocalKind::Temp),
            ],
            body: structured_body(
                vec![
                    Statement::Init {
                        local: list_local,
                        value: RValue::Aggregate {
                            kind: AggregateCtor::List,
                            fields: vec![
                                Operand::Const(zero),
                                Operand::Const(two),
                                Operand::Const(two),
                            ],
                            ty: list,
                        },
                    },
                    Statement::Init {
                        local: start_local,
                        value: RValue::Use(Operand::Const(zero)),
                    },
                    Statement::Init {
                        local: end_local,
                        value: RValue::Use(Operand::Const(two)),
                    },
                    Statement::Init {
                        local: slice_local,
                        value: RValue::ListSlice {
                            source: place(list_local, list),
                            start: start_local,
                            end: end_local,
                            inclusive: true,
                            ty: list,
                        },
                    },
                    Statement::Init {
                        local: len_local,
                        value: RValue::Len {
                            source: place(slice_local, list),
                        },
                    },
                ],
                air::AirTail::Return(Some(Operand::Place(place(len_local, int)))),
            ),
        });
        program.module_mut(module).functions.push(main);
        program.set_entry(main);

        let source = plan_source(program);
        let text = source.as_str();
        assert!(
            text.contains("for item in &v0[anvyx_runtime::checked_range(v1, v2, true, v0.len())]")
        );
        assert!(!text.contains("negative range bound"));
        assert!(!text.contains("range out of bounds"));
        assert!(!text.contains("clone"));
        let output = run_source(source);
        assert_eq!(output.status, SourceJobStatus::Success, "{}", output.stderr);
    }

    #[test]
    fn inclusive_negative_end_panics_without_wrapping() {
        let mut program = Program::default();
        let int = program.alloc_type(TypeData::Int);
        let array = program.alloc_type(TypeData::Array { elem: int, len: 1 });
        let slice = program.alloc_type(TypeData::Slice(int));
        let module = program.alloc_module(root_module());
        let negative = int_const(&mut program, int, -1);
        let zero = int_const(&mut program, int, 0);
        let array_local = air::LocalId::from_index(0);
        let start_local = air::LocalId::from_index(1);
        let end_local = air::LocalId::from_index(2);
        let slice_local = air::LocalId::from_index(3);
        let main = program.alloc_function(Function {
            name: Ident::new("main"),
            module,
            kind: FunctionKind::Normal,
            owner: None,
            specialization: None,
            signature: Signature::new(vec![], int),
            locals: vec![
                local(array, LocalKind::Temp),
                local(int, LocalKind::Temp),
                local(int, LocalKind::Temp),
                local(slice, LocalKind::Temp),
            ],
            body: structured_body(
                vec![
                    Statement::Init {
                        local: array_local,
                        value: RValue::Aggregate {
                            kind: AggregateCtor::Array,
                            fields: vec![Operand::Const(zero)],
                            ty: array,
                        },
                    },
                    Statement::Init {
                        local: start_local,
                        value: RValue::Use(Operand::Const(zero)),
                    },
                    Statement::Init {
                        local: end_local,
                        value: RValue::Use(Operand::Const(negative)),
                    },
                    Statement::Init {
                        local: slice_local,
                        value: RValue::SliceView {
                            source: place(array_local, array),
                            start: start_local,
                            end: end_local,
                            inclusive: true,
                            ty: slice,
                        },
                    },
                ],
                air::AirTail::Return(Some(Operand::Const(zero))),
            ),
        });
        program.module_mut(module).functions.push(main);
        program.set_entry(main);

        let source = plan_source(program);
        let output = run_source(source);
        assert!(
            matches!(output.status, SourceJobStatus::RunFailed(_)),
            "{:?}\n{}",
            output.status,
            output.stderr
        );
        assert!(output.stderr.contains("negative range bound"));
    }

    #[test]
    fn string_list_slice_is_supported() {
        let mut program = Program::default();
        let string = program.alloc_type(TypeData::String);
        let int = program.alloc_type(TypeData::Int);
        let list = program.alloc_type(TypeData::List(string));
        let module = program.alloc_module(root_module());
        let text = program.const_arena.alloc(ConstData {
            ty: string,
            value: ConstValue::String("x".into()),
        });
        let zero = program.const_arena.alloc(ConstData {
            ty: int,
            value: ConstValue::Int(0),
        });
        let list_local = air::LocalId::from_index(0);
        let start_local = air::LocalId::from_index(1);
        let end_local = air::LocalId::from_index(2);
        let out_local = air::LocalId::from_index(3);
        let main = program.alloc_function(Function {
            name: Ident::new("main"),
            module,
            kind: FunctionKind::Normal,
            owner: None,
            specialization: None,
            signature: Signature::new(vec![], list),
            locals: vec![
                local(list, LocalKind::Temp),
                local(int, LocalKind::Temp),
                local(int, LocalKind::Temp),
                local(list, LocalKind::Temp),
            ],
            body: structured_body(
                vec![
                    Statement::Init {
                        local: list_local,
                        value: RValue::Aggregate {
                            kind: AggregateCtor::List,
                            fields: vec![Operand::Const(text)],
                            ty: list,
                        },
                    },
                    Statement::Init {
                        local: start_local,
                        value: RValue::Use(Operand::Const(zero)),
                    },
                    Statement::Init {
                        local: end_local,
                        value: RValue::Use(Operand::Const(zero)),
                    },
                    Statement::Init {
                        local: out_local,
                        value: RValue::ListSlice {
                            source: place(list_local, list),
                            start: start_local,
                            end: end_local,
                            inclusive: false,
                            ty: list,
                        },
                    },
                ],
                air::AirTail::Return(Some(Operand::Place(place(out_local, list)))),
            ),
        });
        program.module_mut(module).functions.push(main);
        program.set_entry(main);

        let source = plan_source(program).into_string();
        assert!(source.contains("(*(item)).share()"));
    }
}

#[test]
fn rir_verifies_raw_enum_metadata_and_cast() {
    let mut program = RirProgram::default();
    let int = RirTypeId::from_index(0);
    let state = RirTypeId::from_index(1);
    program.types.push(RirType::Int);
    program.types.push(RirType::Enum(RirEnumId::from_index(0)));
    program.enums.push(RirEnum {
        id: RirEnumId::from_index(0),
        air_id: None,
        core: None,
        repr: rir::RirEnumRepr::RawInt,
        raw_type: Some(int),
        symbol: RirSymbol::new("State"),
        display: RirSymbol::new("State"),
        copyable: true,
        variants: vec![RirVariant {
            id: RirVariantId::from_index(0),
            symbol: RirSymbol::new("Idle"),
            display: RirSymbol::new("Idle"),
            kind: RirVariantKind::Unit,
            raw_value: Some(rir::RirRawEnumValue::Int(0)),
            fields: vec![],
        }],
    });
    program.functions.push(RirFunction {
        id: RirFunctionId::from_index(0),
        air_id: None,
        symbol: RirSymbol::new("cast"),
        params: vec![RirParam {
            local: RirLocalId::from_index(0),
            ty: state,
            semantic: RirParamSemantic::Value,
            abi: RirParamAbi::Value,
            escape: RirParamEscape::NonEscaping,
        }],
        ret: RirReturn { ty: int },
        locals: vec![RirLocal {
            id: RirLocalId::from_index(0),
            ty: state,
            mutable: false,
            symbol: RirSymbol::new("state"),
            initialized: true,
            payload_ref: false,
        }],
        body: RirStructuredBlock {
            stmts: vec![RirStmt::Eval(RirRValue::Cast {
                value: RirOperand::Place(RirPlace {
                    local: RirLocalId::from_index(0),
                    projections: vec![],
                    ty: state,
                }),
                target: int,
            })],
            term: RirTerm::Return(Some(RirOperand::Const(RirConstId::from_index(0)))),
        },
    });
    program.consts.push(RirConst {
        id: RirConstId::from_index(0),
        ty: int,
        value: RirConstValue::Int(0),
    });

    assert!(rir::verify(&program).is_ok());
}

#[test]
fn rir_rejects_raw_enum_wrong_value_type() {
    let mut program = RirProgram::default();
    program.types.push(RirType::Int);
    program.types.push(RirType::Enum(RirEnumId::from_index(0)));
    program.enums.push(raw_int_rir_enum(RirVariant {
        id: RirVariantId::from_index(0),
        symbol: RirSymbol::new("Bad"),
        display: RirSymbol::new("Bad"),
        kind: RirVariantKind::Unit,
        raw_value: Some(rir::RirRawEnumValue::String("bad".into())),
        fields: vec![],
    }));

    let errors = rir::verify(&program).expect_err("malformed raw enum verified");
    assert!(
        errors
            .iter()
            .any(|error| matches!(error.kind, RirVerifyErrorKind::RawEnumWrongValue))
    );
}

#[test]
fn rir_rejects_raw_enum_payload() {
    let mut program = RirProgram::default();
    let int = RirTypeId::from_index(0);
    program.types.push(RirType::Int);
    program.types.push(RirType::Enum(RirEnumId::from_index(0)));
    program.enums.push(raw_int_rir_enum(RirVariant {
        id: RirVariantId::from_index(0),
        symbol: RirSymbol::new("Bad"),
        display: RirSymbol::new("Bad"),
        kind: RirVariantKind::Tuple,
        raw_value: Some(rir::RirRawEnumValue::Int(0)),
        fields: vec![RirField {
            id: RirFieldId::from_index(0),
            symbol: RirSymbol::new("f0"),
            ty: int,
        }],
    }));

    let errors = rir::verify(&program).expect_err("malformed raw enum verified");
    assert!(
        errors
            .iter()
            .any(|error| matches!(error.kind, RirVerifyErrorKind::RawEnumPayload))
    );
}

fn raw_int_rir_enum(variant: RirVariant) -> RirEnum {
    RirEnum {
        id: RirEnumId::from_index(0),
        air_id: None,
        core: None,
        repr: rir::RirEnumRepr::RawInt,
        raw_type: Some(RirTypeId::from_index(0)),
        symbol: RirSymbol::new("State"),
        display: RirSymbol::new("State"),
        copyable: true,
        variants: vec![variant],
    }
}

fn raw_air_enum(
    program: &mut Program,
    module: air::ModuleId,
    name: &str,
    repr: air::EnumRepr,
    raw_type: air::TypeId,
    variants: Vec<(&str, RawEnumValue)>,
) -> (air::EnumId, air::TypeId) {
    let enum_id = program.alloc_enum(EnumDecl {
        name: Ident::new(name),
        module,
        type_args: vec![],
        const_args: vec![],
        core: None,
        repr,
        raw_type: Some(raw_type),
        variants: variants
            .into_iter()
            .map(|(name, raw_value)| VariantDecl {
                name: Ident::new(name),
                shape: VariantShape::Unit,
                raw_value: Some(raw_value),
            })
            .collect(),
    });
    program.module_mut(module).enums.push(enum_id);
    (enum_id, program.alloc_type(TypeData::Enum(enum_id)))
}

fn raw_int_air_enum(
    program: &mut Program,
    module: air::ModuleId,
    raw_type: air::TypeId,
    name: &str,
    variants: Vec<(&str, i64)>,
) -> (air::EnumId, air::TypeId) {
    raw_air_enum(
        program,
        module,
        name,
        air::EnumRepr::RawInt,
        raw_type,
        variants
            .into_iter()
            .map(|(name, value)| (name, RawEnumValue::Int(value)))
            .collect(),
    )
}

fn raw_string_air_enum(
    program: &mut Program,
    module: air::ModuleId,
    raw_type: air::TypeId,
    name: &str,
    variants: Vec<(&str, &str)>,
) -> (air::EnumId, air::TypeId) {
    raw_air_enum(
        program,
        module,
        name,
        air::EnumRepr::RawString,
        raw_type,
        variants
            .into_iter()
            .map(|(name, value)| (name, RawEnumValue::String(value.into())))
            .collect(),
    )
}

#[test]
fn raw_int_enum_cast_emits_repr_discriminants_and_cast() {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let module = program.alloc_module(root_module());
    let (enum_id, state) = raw_int_air_enum(&mut program, module, int, "State", vec![("Dead", -1)]);
    let state_local = air::LocalId::from_index(0);
    let raw_local = air::LocalId::from_index(1);
    let main = program.alloc_function(Function {
        name: Ident::new("main"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], int),
        locals: vec![local(state, LocalKind::Temp), local(int, LocalKind::Temp)],
        body: structured_body(
            vec![
                Statement::Init {
                    local: state_local,
                    value: RValue::Aggregate {
                        kind: AggregateCtor::EnumVariant {
                            enum_id,
                            variant: VariantId::from_index(0),
                        },
                        fields: vec![],
                        ty: state,
                    },
                },
                Statement::Init {
                    local: raw_local,
                    value: RValue::Cast {
                        value: Operand::Place(place(state_local, state)),
                        target: int,
                    },
                },
            ],
            air::AirTail::Return(Some(Operand::Place(place(raw_local, int)))),
        ),
    });
    program.module_mut(module).functions.push(main);
    program.set_entry(main);

    let source = plan_source(program);
    let text = source.as_str();
    assert!(text.contains("#[derive(Clone, Copy)]"));
    assert!(text.contains("#[repr(i64)]"));
    assert!(text.contains("Dead = -1"));
    assert!(text.contains("v0 as i64"));
}

#[test]
fn raw_int_enum_cast_does_not_consume_source_place() {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let module = program.alloc_module(root_module());
    let (enum_id, state) = raw_int_air_enum(&mut program, module, int, "State", vec![("Idle", 0)]);
    let state_local = air::LocalId::from_index(0);
    let first = air::LocalId::from_index(1);
    let second = air::LocalId::from_index(2);
    let main = program.alloc_function(Function {
        name: Ident::new("main"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], int),
        locals: vec![
            local(state, LocalKind::Temp),
            local(int, LocalKind::Temp),
            local(int, LocalKind::Temp),
        ],
        body: structured_body(
            vec![
                Statement::Init {
                    local: state_local,
                    value: RValue::Aggregate {
                        kind: AggregateCtor::EnumVariant {
                            enum_id,
                            variant: VariantId::from_index(0),
                        },
                        fields: vec![],
                        ty: state,
                    },
                },
                Statement::Init {
                    local: first,
                    value: RValue::Cast {
                        value: Operand::Place(place(state_local, state)),
                        target: int,
                    },
                },
                Statement::Init {
                    local: second,
                    value: RValue::Cast {
                        value: Operand::Place(place(state_local, state)),
                        target: int,
                    },
                },
            ],
            air::AirTail::Return(Some(Operand::Place(place(second, int)))),
        ),
    });
    program.module_mut(module).functions.push(main);
    program.set_entry(main);

    let output = run_source(plan_source(program));
    assert_eq!(output.status, SourceJobStatus::Success, "{}", output.stderr);
}

#[test]
fn empty_raw_int_enum_cast_emits_impossible_match_without_repr() {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let module = program.alloc_module(root_module());
    let (_, never) = raw_int_air_enum(&mut program, module, int, "Never", vec![]);
    let arg = air::LocalId::from_index(0);
    let raw = air::LocalId::from_index(1);
    let main = program.alloc_function(Function {
        name: Ident::new("f"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(
            vec![Param {
                name: Some(Ident::new("value")),
                ty: never,
                mode: ParamMode::Value,
                escape: ParamEscape::NonEscaping,
                role: ParamRole::Normal,
                local_id: arg,
            }],
            int,
        ),
        locals: vec![local(never, LocalKind::Arg), local(int, LocalKind::Temp)],
        body: structured_body(
            vec![Statement::Init {
                local: raw,
                value: RValue::Cast {
                    value: Operand::Place(place(arg, never)),
                    target: int,
                },
            }],
            air::AirTail::Return(Some(Operand::Place(place(raw, int)))),
        ),
    });
    program.module_mut(module).functions.push(main);
    let zero = int_const(&mut program, int, 0);
    let entry = program.alloc_function(Function {
        name: Ident::new("main"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], int),
        locals: vec![],
        body: structured_body(vec![], air::AirTail::Return(Some(Operand::Const(zero)))),
    });
    program.module_mut(module).functions.push(entry);
    program.set_entry(entry);

    let source = plan_source(program);
    let text = source.as_str();
    assert!(!text.contains("#[repr(i64)]"));
    assert!(text.contains("match &"));
    assert!(text.contains("_ => unreachable!()"));
}

#[test]
fn raw_string_enum_cast_emits_match() {
    let mut program = Program::default();
    let string = program.alloc_type(TypeData::String);
    let module = program.alloc_module(root_module());
    let (enum_id, anim) =
        raw_string_air_enum(&mut program, module, string, "Anim", vec![("Idle", "idle")]);
    let anim_local = air::LocalId::from_index(0);
    let raw_local = air::LocalId::from_index(1);
    let main = program.alloc_function(Function {
        name: Ident::new("main"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], string),
        locals: vec![local(anim, LocalKind::Temp), local(string, LocalKind::Temp)],
        body: structured_body(
            vec![
                Statement::Init {
                    local: anim_local,
                    value: RValue::Aggregate {
                        kind: AggregateCtor::EnumVariant {
                            enum_id,
                            variant: VariantId::from_index(0),
                        },
                        fields: vec![],
                        ty: anim,
                    },
                },
                Statement::Init {
                    local: raw_local,
                    value: RValue::Cast {
                        value: Operand::Place(place(anim_local, anim)),
                        target: string,
                    },
                },
            ],
            air::AirTail::Return(Some(Operand::Place(place(raw_local, string)))),
        ),
    });
    program.module_mut(module).functions.push(main);
    program.set_entry(main);

    let source = plan_source(program);
    let text = source.as_str();
    assert!(text.contains("match &v0"));
    assert!(text.contains("=> anvyx_runtime::AnvString::from(\"idle\")"));
    let output = run_source(source);
    assert_eq!(output.status, SourceJobStatus::Success, "{}", output.stderr);
}

fn native_option_return_program(core: Option<RirCoreEnumKind>) -> RirProgram {
    let mut program = RirProgram::default();
    let string = RirTypeId::from_index(0);
    let option = RirTypeId::from_index(1);
    program.types.push(RirType::String);
    if core.is_some() {
        program.types.push(RirType::Option(string));
    } else {
        program.types.push(RirType::Enum(RirEnumId::from_index(0)));
        program.enums.push(RirEnum {
            id: RirEnumId::from_index(0),
            air_id: None,
            core,
            repr: rir::RirEnumRepr::Adt,
            raw_type: None,
            symbol: RirSymbol::new("OptionString"),
            display: RirSymbol::new("Option"),
            copyable: false,
            variants: vec![
                RirVariant {
                    id: RirVariantId::from_index(0),
                    symbol: RirSymbol::new("None"),
                    display: RirSymbol::new("None"),
                    kind: RirVariantKind::Unit,
                    raw_value: None,
                    fields: vec![],
                },
                RirVariant {
                    id: RirVariantId::from_index(1),
                    symbol: RirSymbol::new("Some"),
                    display: RirSymbol::new("Some"),
                    kind: RirVariantKind::Tuple,
                    raw_value: None,
                    fields: vec![RirField {
                        id: RirFieldId::from_index(0),
                        symbol: RirSymbol::new("f0"),
                        ty: string,
                    }],
                },
            ],
        });
    }
    program.externs.push(RirExtern {
        id: RirExternId::from_index(0),
        symbol: RirSymbol::new("substring"),
        kind: RirExternKind::Native(rir::RirNativeExtern {
            path: vec!["host".to_string(), "substring".to_string()],
            abi: anvyx_runtime::RustExternAbi {
                params: vec![],
                ret: anvyx_runtime::RustReturnAbi::Option(Box::new(
                    anvyx_runtime::RustReturnAbi::Value(anvyx_runtime::ExternTypeExpr::String),
                )),
                fallible: false,
                support: anvyx_runtime::RustAbiSupport::Direct,
            },
        }),
        params: vec![],
        ret: option,
    });
    program
}

fn native_string_return_program() -> RirProgram {
    let mut program = empty_rir_function(RirType::String);
    program.consts.clear();
    program.externs.push(RirExtern {
        id: RirExternId::from_index(0),
        symbol: RirSymbol::new("host_string"),
        kind: RirExternKind::Native(rir::RirNativeExtern {
            path: vec!["host".to_string(), "string".to_string()],
            abi: anvyx_runtime::RustExternAbi {
                params: vec![],
                ret: anvyx_runtime::RustReturnAbi::Value(anvyx_runtime::ExternTypeExpr::String),
                fallible: false,
                support: anvyx_runtime::RustAbiSupport::Direct,
            },
        }),
        params: vec![],
        ret: RirTypeId::from_index(0),
    });
    program.functions[0].body.term = RirTerm::Return(Some(RirOperand::Place(RirPlace {
        local: RirLocalId::from_index(0),
        projections: vec![],
        ty: RirTypeId::from_index(0),
    })));
    program.functions[0].locals.push(RirLocal {
        id: RirLocalId::from_index(0),
        ty: RirTypeId::from_index(0),
        mutable: false,
        symbol: RirSymbol::new("s"),
        initialized: false,
        payload_ref: false,
    });
    program.functions[0].body.stmts.push(RirStmt::Init {
        local: RirLocalId::from_index(0),
        value: RirRValue::Call {
            callee: RirCallTarget::Extern(RirExternId::from_index(0)),
            args: vec![],
            ty: RirTypeId::from_index(0),
        },
    });
    program
}

fn int_const(program: &mut Program, ty: air::TypeId, value: i64) -> air::ConstId {
    program.const_arena.alloc(ConstData {
        ty,
        value: ConstValue::Int(value),
    })
}

fn scoped_borrow_lambda_program() -> Program {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let void = program.alloc_type(TypeData::Void);
    let module = program.alloc_module(root_module());
    let callee = source_var_callee(&mut program, module, int, void);
    let body = FunctionId::from_index(1);
    let owner = FunctionId::from_index(2);
    let source = air::LocalId::from_index(0);
    let binding = BindingId::from_index(0);
    let borrow = scoped_borrow(&mut program, owner, source, binding, int);
    let sig = air::SignatureType::new(vec![], air::ReturnMode::Value(void));
    let lambda_ty = program.alloc_type(TypeData::Function(sig.clone()));
    let lambda = program.alloc_lambda(LambdaDecl {
        source: ExprId(0),
        module,
        owner,
        body,
        signature: sig,
        escape: LambdaEscape::NonEscaping,
        captures: vec![air::LambdaCaptureDecl::ScopedBorrow {
            binding,
            borrow,
            ty: int,
            mutability: Mutability::Mutable,
        }],
    });
    assert_eq!(lambda, air::LambdaId::from_index(0));
    let lambda_body = program.alloc_function(Function {
        name: Ident::new("lambda"),
        module,
        kind: FunctionKind::Lambda(lambda),
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], void),
        locals: vec![],
        body: structured_body(
            vec![source_var_call(
                callee,
                PlaceRoot::LambdaCapture(air::LambdaCaptureSlotId::from_index(0)),
                int,
            )],
            air::AirTail::Return(None),
        ),
    });
    assert_eq!(lambda_body, body);
    let caller = program.alloc_function(Function {
        name: Ident::new("capture"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![param("x", int, ParamMode::MutBorrow, source)], void),
        locals: vec![
            bound_arg_source_local(binding, int),
            local(lambda_ty, LocalKind::Temp),
        ],
        body: structured_body(
            vec![Statement::Init {
                local: air::LocalId::from_index(1),
                value: RValue::MakeLambda {
                    lambda,
                    captures: vec![air::LambdaCaptureArg::ScopedBorrow {
                        place: root_place(PlaceRoot::ScopedBorrow(borrow), int),
                    }],
                    ty: lambda_ty,
                },
            }],
            air::AirTail::Return(None),
        ),
    });
    assert_eq!(caller, owner);
    program
        .module_mut(module)
        .functions
        .extend([callee, lambda_body, caller]);
    program
}

fn nested_scoped_borrow_lambda_program() -> Program {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let void = program.alloc_type(TypeData::Void);
    let module = program.alloc_module(root_module());
    let callee = source_var_callee(&mut program, module, int, void);
    let outer_body = FunctionId::from_index(1);
    let inner_body = FunctionId::from_index(2);
    let owner = FunctionId::from_index(3);
    let source = air::LocalId::from_index(0);
    let binding = BindingId::from_index(0);
    let borrow = scoped_borrow(&mut program, owner, source, binding, int);
    let sig = air::SignatureType::new(vec![], air::ReturnMode::Value(void));
    let lambda_ty = program.alloc_type(TypeData::Function(sig.clone()));
    let outer = program.alloc_lambda(LambdaDecl {
        source: ExprId(0),
        module,
        owner,
        body: outer_body,
        signature: sig.clone(),
        escape: LambdaEscape::NonEscaping,
        captures: vec![air::LambdaCaptureDecl::ScopedBorrow {
            binding,
            borrow,
            ty: int,
            mutability: Mutability::Mutable,
        }],
    });
    let inner = program.alloc_lambda(LambdaDecl {
        source: ExprId(1),
        module,
        owner: outer_body,
        body: inner_body,
        signature: sig,
        escape: LambdaEscape::NonEscaping,
        captures: vec![air::LambdaCaptureDecl::ScopedBorrow {
            binding,
            borrow,
            ty: int,
            mutability: Mutability::Mutable,
        }],
    });
    let outer_fn = program.alloc_function(Function {
        name: Ident::new("outer"),
        module,
        kind: FunctionKind::Lambda(outer),
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], void),
        locals: vec![local(lambda_ty, LocalKind::Temp)],
        body: structured_body(
            vec![Statement::Init {
                local: air::LocalId::from_index(0),
                value: RValue::MakeLambda {
                    lambda: inner,
                    captures: vec![air::LambdaCaptureArg::ScopedBorrow {
                        place: root_place(
                            PlaceRoot::LambdaCapture(air::LambdaCaptureSlotId::from_index(0)),
                            int,
                        ),
                    }],
                    ty: lambda_ty,
                },
            }],
            air::AirTail::Return(None),
        ),
    });
    assert_eq!(outer_fn, outer_body);
    let inner_fn = program.alloc_function(Function {
        name: Ident::new("inner"),
        module,
        kind: FunctionKind::Lambda(inner),
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], void),
        locals: vec![],
        body: structured_body(
            vec![source_var_call(
                callee,
                PlaceRoot::LambdaCapture(air::LambdaCaptureSlotId::from_index(0)),
                int,
            )],
            air::AirTail::Return(None),
        ),
    });
    assert_eq!(inner_fn, inner_body);
    let caller = program.alloc_function(Function {
        name: Ident::new("capture"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![param("x", int, ParamMode::MutBorrow, source)], void),
        locals: vec![
            bound_arg_source_local(binding, int),
            local(lambda_ty, LocalKind::Temp),
        ],
        body: structured_body(
            vec![Statement::Init {
                local: air::LocalId::from_index(1),
                value: RValue::MakeLambda {
                    lambda: outer,
                    captures: vec![air::LambdaCaptureArg::ScopedBorrow {
                        place: root_place(PlaceRoot::ScopedBorrow(borrow), int),
                    }],
                    ty: lambda_ty,
                },
            }],
            air::AirTail::Return(None),
        ),
    });
    assert_eq!(caller, owner);
    program
        .module_mut(module)
        .functions
        .extend([callee, outer_fn, inner_fn, caller]);
    program
}

fn owner_capture_cell_call_set_program() -> Program {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let void = program.alloc_type(TypeData::Void);
    let module = program.alloc_module(root_module());
    let binding = BindingId::from_index(0);
    let source_local = air::LocalId::from_index(0);
    let owner = FunctionId::from_index(1);
    let cell = capture_cell(&mut program, owner, source_local, binding, int);
    let one = int_const(&mut program, int, 1);
    let callee = program.alloc_function(Function {
        name: Ident::new("next"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], int),
        locals: vec![],
        body: structured_body(vec![], air::AirTail::Return(Some(Operand::Const(one)))),
    });
    assert_eq!(callee, FunctionId::from_index(0));
    let init = init_cell(&mut program, cell, int);
    let owner_fn = program.alloc_function(Function {
        name: Ident::new("main"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], void),
        locals: vec![bound_source_local(binding, int)],
        body: structured_body(
            vec![
                init,
                Statement::Assign {
                    dst: root_place(PlaceRoot::CaptureCell(cell), int),
                    value: RValue::Call {
                        callee: Callee::Function(callee),
                        args: vec![],
                    },
                },
            ],
            air::AirTail::Return(None),
        ),
    });
    assert_eq!(owner_fn, owner);
    program
        .module_mut(module)
        .functions
        .extend([callee, owner_fn]);
    program.entry = Some(owner_fn);
    program
}

fn capture_cell_lambda_program(escape: LambdaEscape) -> Program {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let void = program.alloc_type(TypeData::Void);
    let sig = air::SignatureType::new(vec![], air::ReturnMode::Value(void));
    let lambda_ty = program.alloc_type(TypeData::Function(sig.clone()));
    let module = program.alloc_module(root_module());
    let binding = BindingId::from_index(0);
    let source_local = air::LocalId::from_index(0);
    let lambda_local = air::LocalId::from_index(1);
    let body = FunctionId::from_index(0);
    let owner = FunctionId::from_index(1);
    let lambda_id = air::LambdaId::from_index(0);
    let cell = capture_cell(&mut program, owner, source_local, binding, int);

    assert_eq!(
        program.alloc_lambda(LambdaDecl {
            source: ExprId(0),
            module,
            owner,
            body,
            signature: sig,
            escape,
            captures: vec![air::LambdaCaptureDecl::CaptureCell {
                binding,
                cell,
                ty: int,
            }],
        }),
        lambda_id
    );
    let lambda_body = program.alloc_function(Function {
        name: Ident::new("lambda"),
        module,
        kind: FunctionKind::Lambda(lambda_id),
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], void),
        locals: vec![],
        body: structured_body(vec![], air::AirTail::Return(None)),
    });
    assert_eq!(lambda_body, body);

    let returns_lambda = escape == LambdaEscape::Escaping;
    let init = init_cell(&mut program, cell, int);
    let owner_fn = program.alloc_function(Function {
        name: Ident::new("main"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], if returns_lambda { lambda_ty } else { void }),
        locals: vec![
            bound_source_local(binding, int),
            local(lambda_ty, LocalKind::User),
        ],
        body: structured_body(
            vec![
                init,
                Statement::Init {
                    local: lambda_local,
                    value: RValue::MakeLambda {
                        lambda: lambda_id,
                        captures: vec![air::LambdaCaptureArg::CaptureCell { cell }],
                        ty: lambda_ty,
                    },
                },
            ],
            if returns_lambda {
                air::AirTail::Return(Some(Operand::Place(root_place(
                    PlaceRoot::Local(lambda_local),
                    lambda_ty,
                ))))
            } else {
                air::AirTail::Return(None)
            },
        ),
    });
    assert_eq!(owner_fn, owner);
    program
        .module_mut(module)
        .functions
        .extend([lambda_body, owner_fn]);
    program.entry = Some(owner_fn);
    program
}

fn heap_capture_cell_source_var_arg_program() -> Program {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let void = program.alloc_type(TypeData::Void);
    let sig = air::SignatureType::new(vec![], air::ReturnMode::Value(void));
    let lambda_ty = program.alloc_type(TypeData::Function(sig.clone()));
    let module = program.alloc_module(root_module());
    let callee = source_var_callee(&mut program, module, int, void);
    let binding = BindingId::from_index(0);
    let source_local = air::LocalId::from_index(0);
    let lambda_local = air::LocalId::from_index(1);
    let body = FunctionId::from_index(1);
    let owner = FunctionId::from_index(2);
    let lambda = air::LambdaId::from_index(0);
    let cell = capture_cell(&mut program, owner, source_local, binding, int);

    assert_eq!(
        program.alloc_lambda(LambdaDecl {
            source: ExprId(0),
            module,
            owner,
            body,
            signature: sig,
            escape: LambdaEscape::Escaping,
            captures: vec![air::LambdaCaptureDecl::CaptureCell {
                binding,
                cell,
                ty: int,
            }],
        }),
        lambda
    );
    let lambda_body = program.alloc_function(Function {
        name: Ident::new("lambda"),
        module,
        kind: FunctionKind::Lambda(lambda),
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], void),
        locals: vec![],
        body: structured_body(vec![], air::AirTail::Return(None)),
    });
    assert_eq!(lambda_body, body);
    let init = init_cell(&mut program, cell, int);
    let caller = program.alloc_function(Function {
        name: Ident::new("main"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], void),
        locals: vec![
            bound_source_local(binding, int),
            local(lambda_ty, LocalKind::User),
        ],
        body: structured_body(
            vec![
                init,
                Statement::Init {
                    local: lambda_local,
                    value: RValue::MakeLambda {
                        lambda,
                        captures: vec![air::LambdaCaptureArg::CaptureCell { cell }],
                        ty: lambda_ty,
                    },
                },
                source_var_call(callee, PlaceRoot::CaptureCell(cell), int),
            ],
            air::AirTail::Return(None),
        ),
    });
    assert_eq!(caller, owner);
    program
        .module_mut(module)
        .functions
        .extend([callee, lambda_body, caller]);
    program.entry = Some(caller);
    program
}

fn owner_capture_cell_source_var_arg_program() -> Program {
    capture_cell_source_var_arg_program(false, LambdaEscape::NonEscaping)
}

fn lambda_capture_cell_source_var_arg_program() -> Program {
    capture_cell_source_var_arg_program(true, LambdaEscape::NonEscaping)
}

fn escaping_lambda_capture_cell_source_var_arg_program() -> Program {
    capture_cell_source_var_arg_program(true, LambdaEscape::Escaping)
}

fn capture_cell_source_var_arg_program(in_lambda: bool, escape: LambdaEscape) -> Program {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let void = program.alloc_type(TypeData::Void);
    let module = program.alloc_module(root_module());
    let callee = source_var_callee(&mut program, module, int, void);
    let binding = BindingId::from_index(0);
    let source_local = air::LocalId::from_index(0);

    if !in_lambda {
        let owner = FunctionId::from_index(1);
        let cell = capture_cell(&mut program, owner, source_local, binding, int);
        let init = init_cell(&mut program, cell, int);
        let caller = program.alloc_function(Function {
            name: Ident::new("main"),
            module,
            kind: FunctionKind::Normal,
            owner: None,
            specialization: None,
            signature: Signature::new(vec![], void),
            locals: vec![bound_source_local(binding, int)],
            body: structured_body(
                vec![
                    init,
                    source_var_call(callee, PlaceRoot::CaptureCell(cell), int),
                ],
                air::AirTail::Return(None),
            ),
        });
        assert_eq!(caller, owner);
        program
            .module_mut(module)
            .functions
            .extend([callee, caller]);
        program.entry = Some(caller);
        return program;
    }

    let body = FunctionId::from_index(1);
    let owner = FunctionId::from_index(2);
    let lambda = air::LambdaId::from_index(0);
    let cell = capture_cell(&mut program, owner, source_local, binding, int);
    let sig = air::SignatureType::new(vec![], air::ReturnMode::Value(void));
    let lambda_ty = program.alloc_type(TypeData::Function(sig.clone()));
    assert_eq!(
        program.alloc_lambda(LambdaDecl {
            source: ExprId(0),
            module,
            owner,
            body,
            signature: sig,
            escape,
            captures: vec![air::LambdaCaptureDecl::CaptureCell {
                binding,
                cell,
                ty: int,
            }],
        }),
        lambda
    );
    let lambda_body = program.alloc_function(Function {
        name: Ident::new("lambda"),
        module,
        kind: FunctionKind::Lambda(lambda),
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], void),
        locals: vec![],
        body: structured_body(
            vec![source_var_call(
                callee,
                PlaceRoot::LambdaCapture(air::LambdaCaptureSlotId::from_index(0)),
                int,
            )],
            air::AirTail::Return(None),
        ),
    });
    assert_eq!(lambda_body, body);
    let init = init_cell(&mut program, cell, int);
    let caller = program.alloc_function(Function {
        name: Ident::new("main"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], void),
        locals: vec![
            bound_source_local(binding, int),
            local(lambda_ty, LocalKind::User),
        ],
        body: structured_body(
            vec![
                init,
                Statement::Init {
                    local: air::LocalId::from_index(1),
                    value: RValue::MakeLambda {
                        lambda,
                        captures: vec![air::LambdaCaptureArg::CaptureCell { cell }],
                        ty: lambda_ty,
                    },
                },
            ],
            air::AirTail::Return(None),
        ),
    });
    assert_eq!(caller, owner);
    program
        .module_mut(module)
        .functions
        .extend([callee, lambda_body, caller]);
    program.entry = Some(caller);
    program
}

fn heap_capture_cell_reentrant_source_var_arg_program() -> Program {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let void = program.alloc_type(TypeData::Void);
    let cb_sig = air::SignatureType::new(vec![], air::ReturnMode::Value(void));
    let cb_ty = program.alloc_type(TypeData::Function(cb_sig.clone()));
    let outer_sig = air::SignatureType::new(vec![], air::ReturnMode::Value(void));
    let outer_ty = program.alloc_type(TypeData::Function(outer_sig.clone()));
    let module = program.alloc_module(root_module());
    let binding = BindingId::from_index(0);
    let source_local = air::LocalId::from_index(0);
    let apply_x = air::LocalId::from_index(0);
    let apply_cb = air::LocalId::from_index(1);
    let cb_body_id = FunctionId::from_index(1);
    let outer_body_id = FunctionId::from_index(2);
    let owner = FunctionId::from_index(3);
    let cb_lambda = air::LambdaId::from_index(0);
    let outer_lambda = air::LambdaId::from_index(1);
    let cell = capture_cell(&mut program, owner, source_local, binding, int);

    let apply = program.alloc_function(Function {
        name: Ident::new("apply"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(
            vec![
                param("x", int, ParamMode::MutBorrow, apply_x),
                param("cb", cb_ty, ParamMode::Value, apply_cb),
            ],
            void,
        ),
        locals: vec![mut_local(int, LocalKind::Arg), local(cb_ty, LocalKind::Arg)],
        body: structured_body(vec![], air::AirTail::Return(None)),
    });
    assert_eq!(apply, FunctionId::from_index(0));

    assert_eq!(
        program.alloc_lambda(LambdaDecl {
            source: ExprId(0),
            module,
            owner: outer_body_id,
            body: cb_body_id,
            signature: cb_sig,
            escape: LambdaEscape::NonEscaping,
            captures: vec![air::LambdaCaptureDecl::CaptureCell {
                binding,
                cell,
                ty: int,
            }],
        }),
        cb_lambda
    );
    let cb_body = program.alloc_function(Function {
        name: Ident::new("cb"),
        module,
        kind: FunctionKind::Lambda(cb_lambda),
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], void),
        locals: vec![],
        body: structured_body(vec![], air::AirTail::Return(None)),
    });
    assert_eq!(cb_body, cb_body_id);

    assert_eq!(
        program.alloc_lambda(LambdaDecl {
            source: ExprId(1),
            module,
            owner,
            body: outer_body_id,
            signature: outer_sig,
            escape: LambdaEscape::Escaping,
            captures: vec![air::LambdaCaptureDecl::CaptureCell {
                binding,
                cell,
                ty: int,
            }],
        }),
        outer_lambda
    );
    let cb_local = air::LocalId::from_index(0);
    let outer_body = program.alloc_function(Function {
        name: Ident::new("outer"),
        module,
        kind: FunctionKind::Lambda(outer_lambda),
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], void),
        locals: vec![local(cb_ty, LocalKind::Temp)],
        body: structured_body(
            vec![
                Statement::Init {
                    local: cb_local,
                    value: RValue::MakeLambda {
                        lambda: cb_lambda,
                        captures: vec![air::LambdaCaptureArg::CaptureCell { cell }],
                        ty: cb_ty,
                    },
                },
                Statement::Eval(RValue::Call {
                    callee: Callee::Function(apply),
                    args: vec![
                        CallArg::MutBorrow(Place {
                            root: PlaceRoot::LambdaCapture(air::LambdaCaptureSlotId::from_index(0)),
                            projection: vec![],
                            ty: int,
                        }),
                        CallArg::Value(Operand::Place(Place {
                            root: PlaceRoot::Local(cb_local),
                            projection: vec![],
                            ty: cb_ty,
                        })),
                    ],
                }),
            ],
            air::AirTail::Return(None),
        ),
    });
    assert_eq!(outer_body, outer_body_id);

    let init = init_cell(&mut program, cell, int);
    let outer_local = air::LocalId::from_index(1);
    let main = program.alloc_function(Function {
        name: Ident::new("main"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], void),
        locals: vec![
            bound_source_local(binding, int),
            local(outer_ty, LocalKind::User),
        ],
        body: structured_body(
            vec![
                init,
                Statement::Init {
                    local: outer_local,
                    value: RValue::MakeLambda {
                        lambda: outer_lambda,
                        captures: vec![air::LambdaCaptureArg::CaptureCell { cell }],
                        ty: outer_ty,
                    },
                },
            ],
            air::AirTail::Return(None),
        ),
    });
    assert_eq!(main, owner);
    program
        .module_mut(module)
        .functions
        .extend([apply, cb_body, outer_body, main]);
    program.entry = Some(main);
    program
}

fn heap_cell_set_from_mut_place_param_program() -> Program {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let void = program.alloc_type(TypeData::Void);
    let sig = air::SignatureType::new(vec![], air::ReturnMode::Value(void));
    let lambda_ty = program.alloc_type(TypeData::Function(sig.clone()));
    let module = program.alloc_module(root_module());
    let binding = BindingId::from_index(0);
    let arg = air::LocalId::from_index(0);
    let source = air::LocalId::from_index(1);
    let lambda_local = air::LocalId::from_index(2);
    let body = FunctionId::from_index(0);
    let owner = FunctionId::from_index(1);
    let lambda = air::LambdaId::from_index(0);
    let cell = capture_cell(&mut program, owner, source, binding, int);

    assert_eq!(
        program.alloc_lambda(LambdaDecl {
            source: ExprId(0),
            module,
            owner,
            body,
            signature: sig,
            escape: LambdaEscape::Escaping,
            captures: vec![air::LambdaCaptureDecl::CaptureCell {
                binding,
                cell,
                ty: int
            }],
        }),
        lambda
    );
    let lambda_body = program.alloc_function(Function {
        name: Ident::new("lambda"),
        module,
        kind: FunctionKind::Lambda(lambda),
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], void),
        locals: vec![],
        body: structured_body(vec![], air::AirTail::Return(None)),
    });
    assert_eq!(lambda_body, body);
    let init = init_cell(&mut program, cell, int);
    let caller = program.alloc_function(Function {
        name: Ident::new("copy"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![param("x", int, ParamMode::MutBorrow, arg)], void),
        locals: vec![
            mut_local(int, LocalKind::Arg),
            bound_source_local(binding, int),
            local(lambda_ty, LocalKind::User),
        ],
        body: structured_body(
            vec![
                init,
                Statement::Init {
                    local: lambda_local,
                    value: RValue::MakeLambda {
                        lambda,
                        captures: vec![air::LambdaCaptureArg::CaptureCell { cell }],
                        ty: lambda_ty,
                    },
                },
                Statement::Assign {
                    dst: Place {
                        root: PlaceRoot::CaptureCell(cell),
                        projection: vec![],
                        ty: int,
                    },
                    value: RValue::Use(Operand::Place(place(arg, int))),
                },
            ],
            air::AirTail::Return(None),
        ),
    });
    assert_eq!(caller, owner);
    program
        .module_mut(module)
        .functions
        .extend([lambda_body, caller]);
    program
}

fn source_var_callee(
    program: &mut Program,
    module: air::ModuleId,
    int: air::TypeId,
    void: air::TypeId,
) -> FunctionId {
    let arg = air::LocalId::from_index(0);
    program.alloc_function(Function {
        name: Ident::new("bump"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![param("x", int, ParamMode::MutBorrow, arg)], void),
        locals: vec![mut_local(int, LocalKind::Arg)],
        body: structured_body(vec![], air::AirTail::Return(None)),
    })
}

fn capture_cell(
    program: &mut Program,
    owner: FunctionId,
    source_local: air::LocalId,
    binding: BindingId,
    ty: air::TypeId,
) -> air::CaptureCellId {
    program.alloc_capture_cell(CaptureCellDecl {
        binding,
        owner,
        source_local,
        ty,
    })
}

fn scoped_borrow(
    program: &mut Program,
    owner: FunctionId,
    source_local: air::LocalId,
    binding: BindingId,
    ty: air::TypeId,
) -> air::ScopedBorrowId {
    program.alloc_scoped_borrow(air::ScopedBorrowDecl {
        owner,
        binding,
        source: air::ScopedBorrowSource::SourceMutParam {
            local: source_local,
        },
        ty,
        mutability: Mutability::Mutable,
    })
}
fn bound_source_local(binding: BindingId, ty: air::TypeId) -> Local {
    let mut local = mut_local(ty, LocalKind::User);
    local.binding = Some(binding);
    local
}

fn bound_arg_source_local(binding: BindingId, ty: air::TypeId) -> Local {
    let mut local = mut_local(ty, LocalKind::Arg);
    local.binding = Some(binding);
    local
}
fn init_cell(program: &mut Program, cell: air::CaptureCellId, ty: air::TypeId) -> Statement {
    Statement::Assign {
        dst: root_place(PlaceRoot::CaptureCell(cell), ty),
        value: RValue::Use(Operand::Const(int_const(program, ty, 0))),
    }
}

fn source_var_call(callee: FunctionId, root: PlaceRoot, ty: air::TypeId) -> Statement {
    Statement::Eval(RValue::Call {
        callee: Callee::Function(callee),
        args: vec![CallArg::MutBorrow(root_place(root, ty))],
    })
}

fn root_place(root: PlaceRoot, ty: air::TypeId) -> Place {
    Place {
        root,
        projection: vec![],
        ty,
    }
}

fn rir_function_for_air(program: &RirProgram, air_id: FunctionId) -> &RirFunction {
    program
        .functions
        .iter()
        .find(|function| function.air_id == Some(air_id))
        .expect("missing RIR function for AIR function")
}

fn only_call_arg(function: &RirFunction) -> &RirCallArg {
    let calls = function
        .body
        .stmts
        .iter()
        .filter_map(|stmt| match stmt {
            RirStmt::Eval(RirRValue::Call { args, .. }) => Some(args),
            _ => None,
        })
        .collect::<Vec<_>>();
    assert_eq!(calls.len(), 1);
    assert_eq!(calls[0].len(), 1);
    &calls[0][0]
}

fn plan_source(program: Program) -> emit::RustSource {
    let verified = air::verify(&program).expect("AIR verify failed");
    let plan = plan(&verified, rust_plan_config()).expect("plan failed");
    emit::emit(&plan.verified())
}

fn rust_plan_config() -> RustPlanConfig {
    RustPlanConfig {
        symbol_prefix: "anv".into(),
        native_providers: vec![
            core2_runtime_support(),
            core2_string_support(),
            fallible_host_support(),
        ],
    }
}

fn workspace_crate_path(name: &str) -> String {
    std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .join(name)
        .display()
        .to_string()
}

fn with_fallible_host(source: emit::RustSource) -> emit::RustSource {
    emit::RustSource::new(format!(
        "mod host {{ pub fn fallible<'cx, 'rt>(_ctx: &mut anvyx_runtime::Ctx<'cx, 'rt>, value: i64) -> Result<i64, anvyx_runtime::RuntimeError> {{ Ok(value) }} }}\n{}",
        source.into_string()
    ))
}

fn run_source(source: emit::RustSource) -> source_job::RustSourceJobOutput {
    let cache = tempfile::tempdir().expect("temp dir failed");
    let deps = [
        ("anvyx_core2", "anvyx-core2", "core2"),
        ("anvyx_runtime", "anvyx-runtime", "runtime"),
    ]
    .into_iter()
    .map(
        |(name, package, crate_dir)| cargo_job::RustCargoDependency {
            name: cargo_job::RustCargoName::parse(name).unwrap(),
            package: Some(cargo_job::RustCargoPackageName::parse(package).unwrap()),
            source: cargo_job::RustCargoDependencySource::Path(workspace_crate_path(crate_dir)),
            features: vec![],
            default_features: true,
        },
    )
    .collect();
    let job = cargo_job::single_program_job_with_dependencies(
        source,
        cache.path().to_path_buf(),
        cargo_job::RustCargoProfile::Dev,
        cargo_job::RustCargoMode::Run,
        "test",
        deps,
    );
    match cargo_job::execute(&job).expect("Cargo job failed") {
        cargo_job::RustCargoOutput::Success(output) => source_job::RustSourceJobOutput {
            status: SourceJobStatus::Success,
            stdout: output.stdout,
            stderr: output.stderr,
            artifact: output.binary_path,
        },
        cargo_job::RustCargoOutput::CargoFailed(output) => source_job::RustSourceJobOutput {
            status: SourceJobStatus::CompileFailed(output.status),
            stdout: output.stdout,
            stderr: output.stderr,
            artifact: output.target_dir,
        },
        cargo_job::RustCargoOutput::RunFailed(output) => source_job::RustSourceJobOutput {
            status: SourceJobStatus::RunFailed(output.status),
            stdout: output.stdout,
            stderr: output.stderr,
            artifact: output.target_dir,
        },
    }
}

fn check(program: Program) {
    let verified = air::verify(&program).expect("AIR verify failed");
    RustBackendProfile::check(&verified).expect("profile rejected AIR");
}

fn assert_plan_gap(program: Program, config: RustPlanConfig, kind: RustTargetGapKind) {
    let verified = air::verify(&program).expect("AIR verify failed");
    let Err(RustPlanError::TargetGaps(gaps)) = plan(&verified, config) else {
        panic!("expected target gap");
    };
    assert!(gaps.iter().any(|gap| gap.kind == kind));
}

fn expect_reject(program: Program, kind: ProfileErrorKind) {
    let errors = profile_errors(program);
    assert!(has_error(&errors, kind), "missing expected profile error");
}

fn profile_errors(program: Program) -> Vec<RustBackendProfileError> {
    let verified = air::verify(&program).expect("AIR verify failed");
    RustBackendProfile::check(&verified).expect_err("profile accepted invalid AIR")
}

fn has_error(errors: &[RustBackendProfileError], kind: ProfileErrorKind) -> bool {
    errors.iter().any(|error| error.kind == kind)
}

fn core2_runtime_support() -> anvyx_runtime::RustProviderSupport {
    use anvyx_runtime::{ExternTypeExpr, RustParamAbi, RustReturnAbi};

    provider_support(
        "core_runtime",
        vec![
            function_binding(
                "core_runtime",
                "anvyx_core2",
                &["__anvyx_native", "core_runtime", "_println"],
                "_println",
                vec![RustParamAbi::Borrow(ExternTypeExpr::String)],
                RustReturnAbi::Void,
                false,
            ),
            function_binding(
                "core_runtime",
                "anvyx_core2",
                &["__anvyx_native", "core_runtime", "_assert"],
                "_assert",
                vec![
                    RustParamAbi::Value(ExternTypeExpr::Bool),
                    RustParamAbi::Borrow(ExternTypeExpr::String),
                ],
                RustReturnAbi::Void,
                false,
            ),
        ],
    )
}

fn core2_string_support() -> anvyx_runtime::RustProviderSupport {
    use anvyx_runtime::{ExternTypeExpr, RustParamAbi, RustReturnAbi};

    provider_support(
        "core_string",
        vec![function_binding(
            "core_string",
            "anvyx_core2",
            &["__anvyx_native", "core_string", "str_len"],
            "str_len",
            vec![RustParamAbi::Borrow(ExternTypeExpr::String)],
            RustReturnAbi::Value(ExternTypeExpr::Int),
            false,
        )],
    )
}

fn fallible_host_support() -> anvyx_runtime::RustProviderSupport {
    use anvyx_runtime::{ExternTypeExpr, RustParamAbi, RustReturnAbi};

    provider_support(
        "fallible_host",
        vec![function_binding(
            "fallible_host",
            "host",
            &["fallible"],
            "fallible",
            vec![RustParamAbi::Value(ExternTypeExpr::Int)],
            RustReturnAbi::Value(ExternTypeExpr::Int),
            true,
        )],
    )
}

fn provider_support(
    provider: &str,
    bindings: Vec<anvyx_runtime::RustExternBinding>,
) -> anvyx_runtime::RustProviderSupport {
    anvyx_runtime::RustProviderSupport {
        package: "<core>".to_string(),
        provider: anvyx_runtime::ProviderId {
            name: provider.to_string(),
        },
        cargo: anvyx_runtime::RustProviderCargo::default(),
        modules: vec![anvyx_runtime::RustModuleSupport {
            module: anvyx_runtime::ModulePath {
                segments: vec![provider.to_string()],
            },
            types: vec![],
            bindings,
        }],
    }
}

fn function_binding(
    provider: &str,
    crate_name: &str,
    segments: &[&str],
    name: &str,
    params: Vec<anvyx_runtime::RustParamAbi>,
    ret: anvyx_runtime::RustReturnAbi,
    fallible: bool,
) -> anvyx_runtime::RustExternBinding {
    anvyx_runtime::RustExternBinding {
        key: anvyx_runtime::ExternBindingKey {
            target: anvyx_runtime::ExternBindingTarget::Function(
                anvyx_runtime::ExternFunctionKey {
                    module: anvyx_runtime::ModulePath {
                        segments: vec![provider.to_string()],
                    },
                    name: name.to_string(),
                },
            ),
            operation: anvyx_runtime::ExternBindingOp::Call,
        },
        path: anvyx_runtime::RustPath {
            crate_name: crate_name.to_string(),
            segments: segments.iter().map(ToString::to_string).collect(),
        },
        abi: anvyx_runtime::RustExternAbi {
            params,
            ret,
            fallible,
            support: anvyx_runtime::RustAbiSupport::Direct,
        },
    }
}

fn runtime_module() -> air::Module {
    air_module(&["core_runtime"])
}

fn air_module(path: &[&str]) -> air::Module {
    air::Module {
        path: path.iter().map(|segment| Ident::new(*segment)).collect(),
        functions: vec![],
        aggregates: vec![],
        enums: vec![],
        extern_types: vec![],
        externs: vec![],
    }
}

fn returning_int_param_function(
    program: &mut Program,
    module: air::ModuleId,
    name: &str,
    ty: air::TypeId,
    mode: ParamMode,
    ret: air::ConstId,
) -> FunctionId {
    let arg = air::LocalId::from_index(0);
    let ret_ty = program.const_arena.get(ret).ty;
    program.alloc_function(Function {
        name: Ident::new(name),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![param("value", ty, mode, arg)], ret_ty),
        locals: vec![local(ty, LocalKind::Arg)],
        body: structured_body(vec![], air::AirTail::Return(Some(Operand::Const(ret)))),
    })
}

fn string_extern(
    program: &mut Program,
    name: &str,
    params: Vec<(air::TypeId, ParamMode)>,
    return_type: air::TypeId,
) -> air::ExternId {
    let id = extern_in_module(
        program,
        &["core_string"],
        name,
        params,
        return_type,
        ExternMember::FreeFunction,
    );
    program.externs[id.index()].binding = Some(provider_binding("core_string", name));
    id
}

fn runtime_extern(
    program: &mut Program,
    name: &str,
    params: Vec<(air::TypeId, ParamMode)>,
    return_type: air::TypeId,
) -> air::ExternId {
    let id = extern_in_module(
        program,
        &["core_runtime"],
        name,
        params,
        return_type,
        ExternMember::FreeFunction,
    );
    program.externs[id.index()].binding = Some(provider_binding("core_runtime", name));
    id
}

fn fallible_extern(program: &mut Program, int: air::TypeId) -> air::ExternId {
    let id = extern_in_module(
        program,
        &["fallible_host"],
        "fallible",
        vec![(int, ParamMode::Value)],
        int,
        ExternMember::FreeFunction,
    );
    let decl = &mut program.externs[id.index()];
    decl.binding = Some(provider_binding("fallible_host", "fallible"));
    decl.effects.fallible = true;
    id
}

fn provider_binding(provider: &str, name: &str) -> ExternBindingDecl {
    ExternBindingDecl {
        package: anvyx_frontend::resolve::PackageId::core(),
        provider: anvyx_runtime::ProviderId {
            name: provider.to_string(),
        },
        key: anvyx_runtime::ExternBindingKey {
            target: anvyx_runtime::ExternBindingTarget::Function(
                anvyx_runtime::ExternFunctionKey {
                    module: anvyx_runtime::ModulePath {
                        segments: vec![provider.to_string()],
                    },
                    name: name.to_string(),
                },
            ),
            operation: anvyx_runtime::ExternBindingOp::Call,
        },
    }
}

fn extern_in_module(
    program: &mut Program,
    path: &[&str],
    name: &str,
    params: Vec<(air::TypeId, ParamMode)>,
    return_type: air::TypeId,
    member: ExternMember,
) -> air::ExternId {
    let module = program.alloc_module(air_module(path));
    let id = program.alloc_extern(ExternDecl {
        name: Ident::new(name),
        module,
        member,
        params: params
            .into_iter()
            .map(|(ty, mode)| ExternParamDecl {
                ty,
                mode,
                escape: ParamEscape::NonEscaping,
            })
            .collect(),
        return_type,
        binding: None,
        effects: anvyx_runtime::ExternEffects::default(),
    });
    program.module_mut(module).externs.push(id);
    id
}
