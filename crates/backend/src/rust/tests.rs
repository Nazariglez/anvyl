use air::AirStmt as Statement;
use anvyx_frontend::{
    air::{
        self, AggregateCtor, AirBody, CallArg, Callee, ConstData, ConstValue, EnumDecl,
        ExternBindingDecl, ExternDecl, ExternMember, ExternParamDecl, ExternRep, ExternTypeDecl,
        FieldDecl, Function, FunctionKind, FunctionSpecialization, Local, LocalKind, Mutability,
        Operand, Param, ParamMode, ParamRole, Place, Program, Projection, RValue, Signature,
        TypeData, VariantDecl, VariantId, VariantShape,
    },
    ast::{BinaryOp, FormatAlign, FormatKind, FormatSign, FormatSpec, Ident},
};

use super::{
    RustPlanConfig, cargo_job, emit, plan,
    profile::{ProfileErrorKind, RustBackendProfile, RustBackendProfileError},
    rir::{
        self, RirCallArg, RirCallTarget, RirConst, RirConstId, RirConstValue, RirCoreEnumKind,
        RirEnum, RirEnumId, RirEnumMatch, RirEnumMatchArm, RirExtern, RirExternId, RirExternKind,
        RirExternParam, RirField, RirFieldId, RirFormatKind, RirFormatSpec, RirFunction,
        RirFunctionId, RirIf, RirLocal, RirLocalId, RirOperand, RirParam, RirParamAbi,
        RirParamSemantic, RirPlace, RirProgram, RirRValue, RirReturn, RirStmt, RirStringifyHelper,
        RirStringifyHelperId, RirStringifyReq, RirStringifyReqId, RirStringifyReqKind, RirStruct,
        RirStructId, RirStructuredBlock, RirSymbol, RirTerm, RirType, RirTypeId, RirVariant,
        RirVariantId, RirVariantKind, RirVerifyErrorKind,
    },
    source_job::{self, SourceJobStatus},
};

fn structured_body(stmts: Vec<Statement>, tail: air::AirTail) -> AirBody {
    AirBody {
        block: air::AirBlock { stmts, tail },
    }
}

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
fn profile_rejects_tuple_construction_as_explicit_target_gap() {
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

    let errors = profile_errors(program);
    assert!(has_error(&errors, ProfileErrorKind::UnsupportedType));
    assert!(has_error(&errors, ProfileErrorKind::UnsupportedRValue));
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
    assert!(has_error(&errors, ProfileErrorKind::UnsupportedType));
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
fn profile_rejects_dataref_declarations() {
    expect_reject(
        struct_decl_program(true),
        ProfileErrorKind::UnsupportedModuleItem,
    );
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

    let verified = air::verify(&program).expect("AIR verify failed");
    let Err(err) = plan(&verified, RustPlanConfig::default()) else {
        panic!("plan should reject noncopy override");
    };
    assert!(
        matches!(err, super::RustPlanError::TargetGaps(gaps) if gaps.iter().any(|gap| gap.kind == super::RustTargetGapKind::NonCopyValueRequired))
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
        variants: vec![
            VariantDecl {
                name: Ident::new("None"),
                shape: VariantShape::Unit,
            },
            VariantDecl {
                name: Ident::new("Some"),
                shape: VariantShape::Tuple(vec![string]),
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

    assert!(text.contains("fn anv_f0_Point_value(_ctx: &mut AnvCtx, v0: anvT3_Point) -> i64"));
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
        place.root = copied;
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
    assert!(!text.contains("derive(Clone"));
    assert!(!text.contains("derive(Copy"));

    let output = run_source(source);

    assert_eq!(output.status, SourceJobStatus::Success);
    assert_eq!(output.stdout, "7\n");
}

#[test]
fn emit_renders_plain_struct_declarations_without_traits_or_impls() {
    let program = struct_decl_program(false);
    let source = plan_source(program).into_string();

    assert!(source.contains("struct anvT2_Point"));
    assert!(source.contains("x: i64"));
    assert!(source.contains("name: String"));
    assert!(!source.contains("impl "));
    assert!(!source.contains("trait "));
    assert!(!source.contains("derive(Clone"));
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
        variants: vec![VariantDecl {
            name: Ident::new("dyn"),
            shape: VariantShape::Unit,
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
fn rir_verify_rejects_noncopy_value_call_arg() {
    let int = RirTypeId::from_index(0);
    let void = RirTypeId::from_index(1);
    let list = RirTypeId::from_index(2);
    let program = RirProgram {
        types: vec![RirType::Int, RirType::Void, RirType::List(int)],
        functions: vec![
            RirFunction {
                id: RirFunctionId::from_index(0),
                air_id: None,
                symbol: RirSymbol::new("callee"),
                params: vec![RirParam {
                    local: RirLocalId::from_index(0),
                    ty: list,
                    semantic: RirParamSemantic::Value,
                    abi: RirParamAbi::Value,
                }],
                ret: RirReturn { ty: void },
                locals: vec![RirLocal {
                    id: RirLocalId::from_index(0),
                    ty: list,
                    mutable: false,
                    symbol: RirSymbol::new("xs"),
                    initialized: true,
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
                    ty: list,
                    mutable: false,
                    symbol: RirSymbol::new("xs"),
                    initialized: false,
                }],
                body: RirStructuredBlock {
                    stmts: vec![
                        RirStmt::Init {
                            local: RirLocalId::from_index(0),
                            value: RirRValue::List {
                                ty: list,
                                elems: vec![],
                            },
                        },
                        RirStmt::Eval(RirRValue::Call {
                            callee: RirCallTarget::Function(RirFunctionId::from_index(0)),
                            args: vec![RirCallArg::Value(RirOperand::Place(RirPlace {
                                local: RirLocalId::from_index(0),
                                projections: vec![],
                                ty: list,
                            }))],
                            ty: void,
                        }),
                    ],
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
    });
    program.functions[0].locals.push(RirLocal {
        id: RirLocalId::from_index(1),
        symbol: RirSymbol::new("i"),
        ty: RirTypeId::from_index(1),
        mutable: true,
        initialized: false,
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
                projections: vec![rir::RirProjection::Field(RirFieldId::from_index(0))],
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
fn profile_rejects_deferred_types() {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    program.alloc_type(TypeData::Map {
        key: int,
        value: int,
        order: air::MapOrder::Insertion,
    });

    expect_reject(program, ProfileErrorKind::UnsupportedType);
}

#[test]
fn profile_rejects_non_immediate_value_params() {
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

    expect_reject(program, ProfileErrorKind::UnsupportedParamMode);
}

#[test]
fn profile_rejects_unsupported_param_modes() {
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
            ty: string,
            mutability: Mutability::Mutable,
            kind: LocalKind::Arg,
        }],
        body: structured_body(vec![], air::AirTail::Return(None)),
    });
    program.module_mut(module).functions.push(func);

    expect_reject(program, ProfileErrorKind::UnsupportedParamMode);
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
fn profile_rejects_closure_locals_and_rvalues() {
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
        locals: vec![
            local(void, LocalKind::Capture),
            local(void, LocalKind::PatternBinding),
            local(void, LocalKind::Return),
        ],
        body: structured_body(
            vec![Statement::Eval(RValue::MakeClosure {
                func: air::FunctionId::from_index(0),
                captures: vec![],
                ty: void,
            })],
            air::AirTail::Return(None),
        ),
    });
    program.module_mut(module).functions.push(func);

    let errors = profile_errors(program);
    assert!(has_error(&errors, ProfileErrorKind::UnsupportedLocalKind));
    assert!(has_error(&errors, ProfileErrorKind::UnsupportedRValue));
}

#[test]
fn profile_rejects_closure_callees() {
    let mut program = Program::default();
    let void = program.alloc_type(TypeData::Void);
    let closure_ty = program.alloc_type(TypeData::Function(air::SignatureType::new(
        vec![],
        air::ReturnMode::Value(void),
    )));
    let module = program.alloc_module(root_module());
    let closure_local = air::LocalId::from_index(0);
    let func = program.alloc_function(Function {
        name: Ident::new("main"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(
            vec![param("f", closure_ty, ParamMode::Value, closure_local)],
            void,
        ),
        locals: vec![local(closure_ty, LocalKind::Arg)],
        body: structured_body(
            vec![Statement::Eval(RValue::Call {
                callee: Callee::Closure(Operand::Place(place(closure_local, closure_ty))),
                args: vec![],
            })],
            air::AirTail::Return(None),
        ),
    });
    program.module_mut(module).functions.push(func);

    let errors = profile_errors(program);
    assert!(has_error(&errors, ProfileErrorKind::UnsupportedCallee));
    assert!(has_error(&errors, ProfileErrorKind::UnsupportedType));
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
        matches!(err, super::RustPlanError::TargetGaps(gaps) if gaps.iter().any(|gap| gap.kind == super::RustTargetGapKind::UnsupportedExtern))
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
        matches!(err, super::RustPlanError::TargetGaps(gaps) if gaps.iter().any(|gap| gap.kind == super::RustTargetGapKind::UnsupportedRustAbi))
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

    assert!(source.contains("format!(\"{:04}\", 7)"));
    assert!(source.contains("format!(\"{:*>5}\", v1.as_str())"));
    assert!(source.contains("format!(\"{:.2}\", 1.25)"));
    assert!(source.contains("format!(\"{:X}\", 255)"));
    assert!(source.contains("format!(\"{:b}\", 5)"));
    assert!(source.contains("format!(\"{:e}\", 1.0)"));
    assert!(source.contains("format!(\"{:E}\", 1.0)"));
    assert!(!source.contains(".clone()"));
    assert!(!source.contains(".to_owned()"));
}

#[test]
fn emit_renders_string_concat_with_push_str_without_clone_or_to_owned() {
    let program = string_concat_program();
    let source = plan_source(program).into_string();

    assert!(source.contains("let mut out = String::new();"));
    assert!(source.contains("out.push_str(\"a\");"));
    assert!(source.contains("out.push_str(v0.as_str());"));
    assert!(source.contains("v0 = { let mut out = String::new(); out.push_str(v0.as_str());"));
    assert!(!source.contains(".clone()"));
    assert!(!source.contains(".to_owned()"));
}

#[test]
fn emit_renders_context_first_free_functions_without_clone_or_traits() {
    let program = scalar_print_program();
    let source = plan_source(program).into_string();

    assert!(source.contains("fn anv_f0_main(ctx: &mut AnvCtx)"));
    assert!(source.contains("anvyx_core2::__anvyx_native::core_runtime::_println(v0.as_str())"));
    assert!(!source.contains("fn anv_extern__println"));
    assert!(!source.contains("impl "));
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

    assert!(source.contains("fn anv_f0_leaf(_ctx: &mut AnvCtx) -> i64"));
    assert!(source.contains("fn anv_f1_caller(ctx: &mut AnvCtx) -> i64"));
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

    assert!(source.contains("fn anvstringify_t3_point(_ctx: &mut AnvCtx, value: &anvT3_Point)"));
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

    assert!(source.contains("fn stringify_inner(_ctx: &mut AnvCtx, value: &Inner)"));
    assert!(source.contains("fn stringify_outer(ctx: &mut AnvCtx, value: &Outer)"));
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
fn emit_borrows_string_literal_call_arg_without_owned_temp() {
    let source = plan_source(borrow_string_literal_program()).into_string();

    assert!(source.contains("anvyx_core2::__anvyx_native::core_runtime::_println(\"ready\");"));
    assert!(!source.contains("String::from"));
    assert!(!source.contains("to_string()"));
}

#[test]
fn emit_forwards_borrowed_string_param_as_str_without_double_borrow() {
    let source = plan_source(shared_string_forward_program()).into_string();

    assert!(source.contains(": &str"));
    assert!(source.contains("anvyx_core2::__anvyx_native::core_runtime::_println(v0);"));
    assert!(!source.contains("anvyx_core2::__anvyx_native::core_runtime::_println(&v0);"));
    assert!(!source.contains("anvyx_core2::__anvyx_native::core_runtime::_println(v0.as_str());"));
}

#[test]
fn emit_borrows_string_constant_for_native_string_param() {
    let source = plan_source(native_str_len_const_program()).into_string();

    assert!(source.contains("anvyx_core2::__anvyx_native::core_string::str_len(\"abc\")"));
    assert!(!source.contains("String::from"));
    assert!(!source.contains("to_string()"));
}

#[test]
fn emit_borrows_string_local_for_native_string_param() {
    let source = plan_source(native_str_len_local_program()).into_string();

    assert!(source.contains("anvyx_core2::__anvyx_native::core_string::str_len(v0.as_str())"));
    assert!(!source.contains("anvyx_core2::__anvyx_native::core_string::str_len(&v0)"));
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
        }],
        ret: RirReturn { ty: int },
        locals: vec![RirLocal {
            id: RirLocalId::from_index(0),
            ty: int,
            mutable: false,
            symbol: RirSymbol::new("arg"),
            initialized: true,
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
fn rir_verify_rejects_unsupported_abi() {
    let mut program = empty_rir_function(RirType::String);
    program.functions[0].locals.push(RirLocal {
        id: RirLocalId::from_index(0),
        ty: RirTypeId::from_index(0),
        mutable: false,
        symbol: RirSymbol::new("s"),
        initialized: true,
    });
    program.functions[0].params.push(RirParam {
        local: RirLocalId::from_index(0),
        ty: RirTypeId::from_index(0),
        semantic: RirParamSemantic::Value,
        abi: RirParamAbi::Value,
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
                needs_context: false,
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
                needs_context: false,
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
                needs_context: false,
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
    });
    program.functions[0].params.push(RirParam {
        local: RirLocalId::from_index(0),
        ty: RirTypeId::from_index(0),
        semantic: RirParamSemantic::Value,
        abi: RirParamAbi::Value,
    });
    program.functions[0].body.stmts.push(RirStmt::Init {
        local: RirLocalId::from_index(0),
        value: RirRValue::Use(RirOperand::Const(RirConstId::from_index(0))),
    });

    assert_rir_error(program, RirVerifyErrorKind::InitParamLocal);
}

#[test]
fn rir_verify_rejects_mut_borrow_and_place_handle_abi() {
    for abi in [RirParamAbi::MutBorrow, RirParamAbi::PlaceHandle] {
        let mut program = empty_rir_function(RirType::Int);
        program.functions[0].locals.push(RirLocal {
            id: RirLocalId::from_index(0),
            ty: RirTypeId::from_index(0),
            mutable: true,
            symbol: RirSymbol::new("p"),
            initialized: true,
        });
        program.functions[0].params.push(RirParam {
            local: RirLocalId::from_index(0),
            ty: RirTypeId::from_index(0),
            semantic: RirParamSemantic::MutBorrow,
            abi,
        });
        assert_rir_error(program, RirVerifyErrorKind::UnsupportedAbi);
    }
}

fn assert_rir_error(program: RirProgram, kind: RirVerifyErrorKind) {
    let errors = rir::verify(&program).expect_err("verified invalid RIR");
    assert!(errors.iter().any(|error| error.kind == kind));
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
        symbol: RirSymbol::new("E"),
        display: RirSymbol::new("E"),
        copyable: true,
        variants: vec![
            RirVariant {
                id: RirVariantId::from_index(0),
                symbol: RirSymbol::new("A"),
                display: RirSymbol::new("A"),
                kind: RirVariantKind::Unit,
                fields: vec![],
            },
            RirVariant {
                id: RirVariantId::from_index(1),
                symbol: RirSymbol::new("B"),
                display: RirSymbol::new("B"),
                kind: RirVariantKind::Unit,
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
                role: ParamRole::Receiver,
                local_id: recv,
            }],
            int,
        ),
        locals: vec![local(point, LocalKind::Arg)],
        body: structured_body(
            vec![],
            air::AirTail::Return(Some(Operand::Place(Place {
                root: recv,
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
                            root: point_local,
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
    fn noncopy_enum_value_copy_is_target_gap() {
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

        let verified = air::verify(&program).expect("AIR verify failed");
        let Err(err) = plan(&verified, RustPlanConfig::default()) else {
            panic!("plan should reject copy");
        };
        assert!(
            matches!(err, super::super::RustPlanError::TargetGaps(gaps) if gaps.iter().any(|gap| gap.kind == super::super::RustTargetGapKind::NonCopyValueRequired))
        );
    }

    #[test]
    fn enum_stringify_is_explicit_target_gap() {
        let program = enum_with_string_payload_program(true);
        let verified = air::verify(&program).expect("AIR verify failed");
        let Err(err) = plan(&verified, RustPlanConfig::default()) else {
            panic!("plan should reject stringify");
        };
        assert!(
            matches!(err, super::super::RustPlanError::TargetGaps(gaps) if gaps.iter().any(|gap| gap.kind == super::super::RustTargetGapKind::UnsupportedStructuralStringify))
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
            symbol: RirSymbol::new("E"),
            display: RirSymbol::new("E"),
            copyable: true,
            variants: vec![RirVariant {
                id: RirVariantId::from_index(0),
                symbol: RirSymbol::new("Unit"),
                display: RirSymbol::new("Unit"),
                kind: RirVariantKind::Unit,
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
            symbol: RirSymbol::new("E"),
            display: RirSymbol::new("E"),
            copyable: true,
            variants: vec![RirVariant {
                id: RirVariantId::from_index(0),
                symbol: RirSymbol::new("Text"),
                display: RirSymbol::new("Text"),
                kind: RirVariantKind::Tuple,
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
            symbol: RirSymbol::new("E"),
            display: RirSymbol::new("E"),
            copyable: true,
            variants: vec![RirVariant {
                id: RirVariantId::from_index(0),
                symbol: RirSymbol::new("Struct"),
                display: RirSymbol::new("Struct"),
                kind: RirVariantKind::Tuple,
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
            variants: vec![
                VariantDecl {
                    name: Ident::new("Start"),
                    shape: VariantShape::Unit,
                },
                VariantDecl {
                    name: Ident::new("Hit"),
                    shape: VariantShape::Tuple(vec![int, bool_ty]),
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
            variants: vec![VariantDecl {
                name: Ident::new("Text"),
                shape: VariantShape::Tuple(vec![string]),
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
        assert!(source.as_str().contains("[1, 2]"));
        assert!(source.as_str().contains("negative index"));
        assert!(source.as_str().contains(".len() as i64"));
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
        assert!(source.as_str().contains("Point { x: *&v1[0].x }]"));
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
            variants: vec![VariantDecl {
                name: Ident::new("Some"),
                shape: VariantShape::Tuple(vec![int]),
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
        assert!(source.as_str().contains("match &&v1[0]"));
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
                },
                RirLocal {
                    id: RirLocalId::from_index(1),
                    ty: bool_ty,
                    mutable: false,
                    symbol: RirSymbol::new("i"),
                    initialized: true,
                },
            ],
            body: RirStructuredBlock {
                stmts: vec![],
                term: RirTerm::Return(Some(RirOperand::Place(RirPlace {
                    local: RirLocalId::from_index(0),
                    projections: vec![rir::RirProjection::Index(RirLocalId::from_index(1))],
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
        assert!(source.as_str().contains("vec![1]"));
        assert!(source.as_str().contains(".push(2)"));
        assert!(source.as_str().contains("negative index"));
        assert!(source.as_str().contains(".len() as i64"));
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
        assert!(source.as_str().contains("&Vec<i64>"));
    }

    #[test]
    fn immutable_list_push_is_rejected() {
        let mut program = Program::default();
        let int = program.alloc_type(TypeData::Int);
        program.alloc_type(TypeData::Void);
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
                    Statement::Eval(RValue::ListPush {
                        list: place(list_local, list),
                        value: Operand::Const(one),
                    }),
                ],
                air::AirTail::Return(Some(Operand::Const(one))),
            ),
        });
        program.module_mut(module).functions.push(main);
        program.set_entry(main);

        expect_reject(program, ProfileErrorKind::UnsupportedRValue);
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
    fn noncopy_list_value_copy_is_target_gap() {
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

        let errors = profile_errors(program);
        assert!(has_error(&errors, ProfileErrorKind::NonCopyValueRequired));
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
        assert!(source.as_str().contains("&[i64]"));
        assert!(source.as_str().contains("range out of bounds"));
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
        assert!(source.as_str().contains("for item in"));
        assert!(!source.as_str().contains("clone"));
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
    fn noncopy_list_slice_is_target_gap() {
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

        let errors = profile_errors(program);
        assert!(has_error(&errors, ProfileErrorKind::NonCopyValueRequired));
    }
}

fn native_option_return_program(core: Option<RirCoreEnumKind>) -> RirProgram {
    let mut program = RirProgram::default();
    let string = RirTypeId::from_index(0);
    let option = RirTypeId::from_index(1);
    program.types.push(RirType::String);
    program.types.push(RirType::Enum(RirEnumId::from_index(0)));
    program.enums.push(RirEnum {
        id: RirEnumId::from_index(0),
        air_id: None,
        core,
        symbol: RirSymbol::new("OptionString"),
        display: RirSymbol::new("Option"),
        copyable: false,
        variants: vec![
            RirVariant {
                id: RirVariantId::from_index(0),
                symbol: RirSymbol::new("None"),
                display: RirSymbol::new("None"),
                kind: RirVariantKind::Unit,
                fields: vec![],
            },
            RirVariant {
                id: RirVariantId::from_index(1),
                symbol: RirSymbol::new("Some"),
                display: RirSymbol::new("Some"),
                kind: RirVariantKind::Tuple,
                fields: vec![RirField {
                    id: RirFieldId::from_index(0),
                    symbol: RirSymbol::new("f0"),
                    ty: string,
                }],
            },
        ],
    });
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
                needs_context: false,
                fallible: false,
                support: anvyx_runtime::RustAbiSupport::Direct,
            },
        }),
        params: vec![],
        ret: option,
    });
    program
}

fn mut_local(ty: air::TypeId, kind: LocalKind) -> Local {
    Local {
        name: None,
        ty,
        mutability: Mutability::Mutable,
        kind,
    }
}

fn int_const(program: &mut Program, ty: air::TypeId, value: i64) -> air::ConstId {
    program.const_arena.alloc(ConstData {
        ty,
        value: ConstValue::Int(value),
    })
}

fn plan_source(program: Program) -> emit::RustSource {
    let verified = air::verify(&program).expect("AIR verify failed");
    let plan = plan(&verified, rust_plan_config()).expect("plan failed");
    emit::emit(&plan.verified())
}

fn rust_plan_config() -> RustPlanConfig {
    RustPlanConfig {
        symbol_prefix: "anv".into(),
        native_providers: vec![core2_runtime_support(), core2_string_support()],
    }
}

fn run_source(source: emit::RustSource) -> source_job::RustSourceJobOutput {
    let cache = tempfile::tempdir().expect("temp dir failed");
    let dep = cargo_job::RustCargoDependency {
        name: cargo_job::RustCargoName::parse("anvyx_core2").unwrap(),
        package: Some(cargo_job::RustCargoPackageName::parse("anvyx-core2").unwrap()),
        source: cargo_job::RustCargoDependencySource::Path(
            std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
                .parent()
                .unwrap()
                .join("core2")
                .display()
                .to_string(),
        ),
        features: vec![],
        default_features: true,
    };
    let job = cargo_job::single_program_job_with_dependencies(
        source,
        cache.path().to_path_buf(),
        cargo_job::RustCargoProfile::Dev,
        cargo_job::RustCargoMode::Run,
        "test",
        vec![dep],
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
    use anvyx_runtime::{
        ExternBindingKey, ExternBindingOp, ExternBindingTarget, ExternFunctionKey, ExternTypeExpr,
        ModulePath, ProviderId, RustAbiSupport, RustExternAbi, RustExternBinding,
        RustModuleSupport, RustParamAbi, RustPath, RustProviderCargo, RustProviderSupport,
        RustReturnAbi,
    };

    let module = ModulePath {
        segments: vec!["core_runtime".to_string()],
    };
    RustProviderSupport {
        package: "<core>".to_string(),
        provider: ProviderId {
            name: "core_runtime".to_string(),
        },
        cargo: RustProviderCargo::default(),
        modules: vec![RustModuleSupport {
            module: module.clone(),
            types: vec![],
            bindings: vec![
                RustExternBinding {
                    key: ExternBindingKey {
                        target: ExternBindingTarget::Function(ExternFunctionKey {
                            module: module.clone(),
                            name: "_println".to_string(),
                        }),
                        operation: ExternBindingOp::Call,
                    },
                    path: RustPath {
                        crate_name: "anvyx_core2".to_string(),
                        segments: vec![
                            "__anvyx_native".to_string(),
                            "core_runtime".to_string(),
                            "_println".to_string(),
                        ],
                    },
                    abi: RustExternAbi {
                        params: vec![RustParamAbi::Borrow(ExternTypeExpr::String)],
                        ret: RustReturnAbi::Void,
                        needs_context: false,
                        fallible: false,
                        support: RustAbiSupport::Direct,
                    },
                },
                RustExternBinding {
                    key: ExternBindingKey {
                        target: ExternBindingTarget::Function(ExternFunctionKey {
                            module,
                            name: "_assert".to_string(),
                        }),
                        operation: ExternBindingOp::Call,
                    },
                    path: RustPath {
                        crate_name: "anvyx_core2".to_string(),
                        segments: vec![
                            "__anvyx_native".to_string(),
                            "core_runtime".to_string(),
                            "_assert".to_string(),
                        ],
                    },
                    abi: RustExternAbi {
                        params: vec![
                            RustParamAbi::Value(ExternTypeExpr::Bool),
                            RustParamAbi::Borrow(ExternTypeExpr::String),
                        ],
                        ret: RustReturnAbi::Void,
                        needs_context: false,
                        fallible: true,
                        support: RustAbiSupport::Direct,
                    },
                },
            ],
        }],
    }
}

fn core2_string_support() -> anvyx_runtime::RustProviderSupport {
    use anvyx_runtime::{
        ExternBindingKey, ExternBindingOp, ExternBindingTarget, ExternFunctionKey, ExternTypeExpr,
        ModulePath, ProviderId, RustAbiSupport, RustExternAbi, RustExternBinding,
        RustModuleSupport, RustParamAbi, RustPath, RustProviderCargo, RustProviderSupport,
        RustReturnAbi,
    };

    let module = ModulePath {
        segments: vec!["core_string".to_string()],
    };
    RustProviderSupport {
        package: "<core>".to_string(),
        provider: ProviderId {
            name: "core_string".to_string(),
        },
        cargo: RustProviderCargo::default(),
        modules: vec![RustModuleSupport {
            module: module.clone(),
            types: vec![],
            bindings: vec![RustExternBinding {
                key: ExternBindingKey {
                    target: ExternBindingTarget::Function(ExternFunctionKey {
                        module,
                        name: "str_len".to_string(),
                    }),
                    operation: ExternBindingOp::Call,
                },
                path: RustPath {
                    crate_name: "anvyx_core2".to_string(),
                    segments: vec![
                        "__anvyx_native".to_string(),
                        "core_string".to_string(),
                        "str_len".to_string(),
                    ],
                },
                abi: RustExternAbi {
                    params: vec![RustParamAbi::Borrow(ExternTypeExpr::String)],
                    ret: RustReturnAbi::Value(ExternTypeExpr::Int),
                    needs_context: false,
                    fallible: false,
                    support: RustAbiSupport::Direct,
                },
            }],
        }],
    }
}

fn root_module() -> air::Module {
    air_module(&[])
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
            .map(|(ty, mode)| ExternParamDecl { ty, mode })
            .collect(),
        return_type,
        binding: None,
        effects: anvyx_runtime::ExternEffects::default(),
    });
    program.module_mut(module).externs.push(id);
    id
}

fn param(name: &str, ty: air::TypeId, mode: ParamMode, local_id: air::LocalId) -> Param {
    Param {
        name: Some(Ident::new(name)),
        ty,
        mode,
        role: ParamRole::Normal,
        local_id,
    }
}

fn local(ty: air::TypeId, kind: LocalKind) -> Local {
    Local {
        name: None,
        ty,
        mutability: Mutability::Immutable,
        kind,
    }
}

fn place(root: air::LocalId, ty: air::TypeId) -> Place {
    Place {
        root,
        projection: vec![],
        ty,
    }
}
