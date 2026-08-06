use air::AirStmt as Statement;
use anvyx_frontend::{
    air::{
        self, AggregateCtor, CallArg, Callee, ConstData, ConstValue, ExternBindingDecl, ExternDecl,
        ExternMember, ExternParamDecl, FieldDecl, Function, FunctionId, FunctionKind, LocalKind,
        Operand, OwnedValue, Param, ParamEscape, ParamMode, ParamRole, Place, PlaceRoot, Program,
        Projection, RValue, Signature, TypeData,
    },
    ast::Ident,
};

use super::{
    RirPlan, RustPlanConfig, RustPlanError, RustSource, RustTargetGap, RustTargetGapKind,
    RustTargetGapSite, cargo_job, emit, generate, plan,
    rep_policy::{LambdaStorageFamily, RustRecipePosition},
    rir::{
        self, RirCallArg, RirCallTarget, RirCellDecl, RirCellId, RirCellLifetime, RirCellRef,
        RirCellStorage, RirConst, RirConstId, RirConstValue, RirField, RirFieldId, RirFunction,
        RirFunctionId, RirGlobal, RirGlobalId, RirLambda, RirLambdaCapture, RirLambdaCaptureArg,
        RirLambdaCaptureKind, RirLambdaEnvField, RirLambdaEnvFieldKind, RirLambdaEnvId,
        RirLambdaEnvLayout, RirLambdaEscape, RirLambdaId, RirLambdaSig, RirLambdaSigId,
        RirLambdaStorage, RirLocal, RirLocalBinding, RirLocalId, RirMaterializer,
        RirMaterializerAction, RirMutPlaceArg, RirMutPlaceHandle, RirOperand, RirOptionMatch,
        RirOptionSubject, RirOwnedOperand, RirOwnedSource, RirOwnedValue, RirParamEscape,
        RirPassMode, RirPlace, RirPlaceStep, RirPlaceStepKind, RirProgram, RirRValue, RirReturn,
        RirScopedPlaceCellDecl, RirScopedPlaceCellId, RirScopedPlaceSource, RirStmt,
        RirStructuredBlock, RirSymbol, RirTerm, RirTuple, RirTupleId, RirType, RirTypeId,
        RirVerifyErrorKind,
    },
    source_job::{self, SourceJobStatus},
    target,
};
use crate::test_support::{
    immutable_local as local, mutable_local as mut_local, param, place, root_module,
    structured_body,
};

fn owned(value: Operand) -> OwnedValue<Operand> {
    OwnedValue::reusable(value)
}

fn rir_owned(value: RirOperand, materializer: usize) -> RirOwnedValue {
    RirOwnedValue {
        value: RirOwnedOperand::Value(value),
        source: RirOwnedSource::Reuse(rir::RirMaterializerId::from_index(materializer)),
    }
}
struct TestMaterializers<'a> {
    program: &'a mut RirProgram,
}

impl<'a> TestMaterializers<'a> {
    fn new(program: &'a mut RirProgram) -> Self {
        program
            .value_materializers
            .resize(program.types.len(), None);
        Self { program }
    }

    fn declare(
        &mut self,
        ty: RirTypeId,
        position: RustRecipePosition,
        action: RirMaterializerAction,
    ) -> rir::RirMaterializerId {
        let id = rir::RirMaterializerId::from_index(self.program.materializers.len());
        let copy =
            matches!(action, RirMaterializerAction::Copy).then_some(rir::RirCopyEvidence::Leaf);
        self.program.materializers.push(RirMaterializer {
            id,
            ty,
            position,
            action,
            copy,
            support: None,
        });
        if position == RustRecipePosition::Value {
            self.program.value_materializers[ty.index()] = Some(id);
        }
        id
    }

    fn copy(&mut self, ty: RirTypeId, position: RustRecipePosition) -> rir::RirMaterializerId {
        self.declare(ty, position, RirMaterializerAction::Copy)
    }

    fn callable_share(
        &mut self,
        ty: RirTypeId,
        position: RustRecipePosition,
    ) -> rir::RirMaterializerId {
        self.declare(ty, position, RirMaterializerAction::CallableShare)
    }
}
#[test]
fn emit_retained_callback_registry_before_heap() {
    let mut program = native_escaping_lambda_trigger_air();
    air::finalize_materialization(&mut program);
    air::finalize_materialization(&mut program);
    let verified = air::verify(&program).expect("AIR verify failed");
    let source = generate(&verified, host_escaping_lambda_retained_plan_config())
        .expect("generation failed");
    let source = source.as_str();
    let callbacks = source
        .find(&format!(
            "callbacks: {}<'cx>,",
            target::generated_callback_registry_symbol()
        ))
        .expect("retained callback registry field");
    let heap = source
        .find("heap: anvyx_runtime::Heap<'cx>,")
        .expect("runtime heap field");

    assert!(callbacks < heap);
}
#[test]
fn rir_rejects_global_option_match_discriminant() {
    assert_rir_error(
        valid_global_rir(|program| {
            program.functions[1]
                .body
                .stmts
                .push(RirStmt::OptionMatch(RirOptionMatch {
                    subject: RirOptionSubject::Place(rir_global_place()),
                    payload: None,
                    some_block: RirStructuredBlock {
                        stmts: vec![],
                        term: RirTerm::Return(None),
                    },
                    none_block: RirStructuredBlock {
                        stmts: vec![],
                        term: RirTerm::Return(None),
                    },
                }));
        }),
        RirVerifyErrorKind::UnsupportedRValueType,
    );
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
                    args: vec![CallArg::Value(owned(Operand::Const(arg)))],
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

    let output = run_source(with_fallible_host(&plan_source(program)));

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
                        fields: vec![owned(Operand::Const(name_const))],
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
fn planner_rejects_function_place_returns() {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let module = program.alloc_module(root_module());
    let x = air::LocalId::from_index(0);
    for name in ["first", "second"] {
        let function = program.alloc_function(Function {
            name: Ident::new(name),
            module,
            kind: FunctionKind::Normal,
            owner: None,
            specialization: None,
            signature: Signature::with_return_mode(
                vec![param("x", int, ParamMode::MutBorrow, x)],
                air::ReturnMode::Place(int),
            ),
            locals: vec![mut_local(int, LocalKind::Arg)],
            body: structured_body(
                vec![],
                air::AirTail::Return(Some(Operand::Place(place(x, int)))),
            ),
        });
        program.module_mut(module).functions.push(function);
    }

    assert_eq!(
        plan_gaps(program, rust_plan_config()),
        vec![
            RustTargetGap {
                site: RustTargetGapSite::Function(FunctionId::from_index(0)),
                kind: RustTargetGapKind::UnsupportedReturnMode,
            },
            RustTargetGap {
                site: RustTargetGapSite::Function(FunctionId::from_index(1)),
                kind: RustTargetGapKind::UnsupportedReturnMode,
            },
        ]
    );
}

#[test]
fn planner_rejects_function_type_place_returns() {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    program.alloc_type(TypeData::Function(air::SignatureType::new(
        vec![air::ParamType {
            ty: int,
            mode: ParamMode::MutBorrow,
            escape: ParamEscape::NonEscaping,
        }],
        air::ReturnMode::Place(int),
    )));

    assert_default_plan_gap(program, RustTargetGapKind::UnsupportedReturnMode);
}

#[test]
fn rir_accepts_pattern_alias_scoped_place_source_projection() {
    let mut program = scoped_place_cell_rir(valid_scoped_place_cell_decl());
    let tuple = RirTupleId::from_index(program.tuples.len());
    let tuple_ty = RirTypeId::from_index(program.types.len());
    let int = RirTypeId::from_index(1);
    program.types.push(RirType::Tuple(tuple));
    program.tuples.push(RirTuple {
        id: tuple,
        symbol: RirSymbol::new("Pair"),
        display: RirSymbol::new("Pair"),
        fields: vec![RirField {
            id: RirFieldId::from_index(0),
            symbol: RirSymbol::new("0"),
            ty: int,
        }],
    });
    let param = program.functions[0].params[0];
    program.functions[0].locals[param.index()].ty = tuple_ty;
    program.functions[0].locals[0].ty = tuple_ty;
    program.scoped_place_cells[0].source = scoped_source_pattern_alias(
        RirLocalId::from_index(0),
        vec![RirPlaceStep {
            source_ty: tuple_ty,
            target_ty: int,
            kind: RirPlaceStepKind::TupleField(RirFieldId::from_index(0)),
        }],
    );
    let mut materializers = TestMaterializers::new(&mut program);
    let field = materializers.copy(
        int,
        RustRecipePosition::StoredPayload(LambdaStorageFamily::TupleField),
    );
    materializers.declare(
        tuple_ty,
        RustRecipePosition::Value,
        RirMaterializerAction::Tuple {
            fields: vec![field],
        },
    );

    rir::verify(&program).expect("RIR rejected pattern alias scoped-place source");
}

#[test]
fn rir_rejects_pattern_alias_scoped_place_source_local_root() {
    let mut program = scoped_place_cell_rir(valid_scoped_place_cell_decl());
    program.scoped_place_cells[0].source = RirScopedPlaceSource::PatternAlias {
        place: RirMutPlaceArg::from_handle(
            RirMutPlaceHandle::Local {
                local: RirLocalId::from_index(0),
            },
            vec![],
        ),
    };

    assert_rir_error(program, RirVerifyErrorKind::UnsupportedLambdaCapture);
}

#[test]
fn rir_rejects_projected_cell_bad_index_local() {
    let void = RirTypeId::from_index(0);
    let int = RirTypeId::from_index(1);
    let array = RirTypeId::from_index(2);
    let mut program = RirProgram {
        types: vec![
            RirType::Void,
            RirType::Int,
            RirType::Array { elem: int, len: 2 },
        ],
        cells: vec![RirCellDecl {
            payload_ty: array,
            ..valid_stack_cell_decl()
        }],
        functions: vec![rir_function(
            RirFunctionId::from_index(0),
            void,
            vec![],
            vec![rir_local(RirLocalId::from_index(0), array, true, "source")],
            vec![RirStmt::Eval(RirRValue::Call {
                callee: RirCallTarget::Function(RirFunctionId::from_index(1)),
                args: vec![RirCallArg::MutPlace(RirMutPlaceArg::from_handle(
                    RirMutPlaceHandle::StackCell {
                        cell: RirCellRef::Owner(RirCellId::from_index(0)),
                    },
                    vec![RirPlaceStep {
                        source_ty: array,
                        target_ty: int,
                        kind: RirPlaceStepKind::ArrayIndex {
                            index: RirLocalId::from_index(1),
                            len: 2,
                            elem_materializer: rir::RirMaterializerId::from_index(0),
                        },
                    }],
                ))],
                ty: void,
            })],
        )],
        ..RirProgram::default()
    };
    program
        .functions
        .push(mut_place_sink_function(RirFunctionId::from_index(1)));
    let mut materializers = TestMaterializers::new(&mut program);
    let elem = materializers.copy(
        int,
        RustRecipePosition::StoredPayload(LambdaStorageFamily::FixedArrayElement),
    );
    materializers.declare(
        array,
        RustRecipePosition::Value,
        RirMaterializerAction::Array { elem },
    );

    assert_rir_error(program, RirVerifyErrorKind::BadId);
}

#[test]
fn emit_traces_tracked_lambda_env_and_lambda_value_fields() {
    let mut program = valid_heap_env_lambda_rir();
    let lambda_ty = RirTypeId::from_index(2);
    let zero = RirLambdaId::from_index(1);
    let zero_fn = RirFunctionId::from_index(2);
    program.lambdas[0].captures[0].ty = lambda_ty;
    program.lambda_envs[0].fields[0].ty = lambda_ty;
    let param = program.functions[0].params[0];
    program.functions[0].locals[param.index()].ty = lambda_ty;
    program.functions[0].locals[0].ty = lambda_ty;
    program.functions[1].locals[0].ty = lambda_ty;
    program.functions[1].locals[0].binding = RirLocalBinding::Value;
    program.lambdas.push(RirLambda {
        id: zero,
        function: zero_fn,
        sig: RirLambdaSigId::from_index(0),
        escape: RirLambdaEscape::Escaping,
        storage: RirLambdaStorage::ZeroEnv,
        captures: vec![],
    });
    program.functions.push(RirFunction {
        id: zero_fn,
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
    captures[0] = RirLambdaCaptureArg::Owned {
        value: rir_owned(
            RirOperand::Place(RirPlace::local(RirLocalId::from_index(0), vec![])),
            1,
        ),
    };

    rir::verify(&program).expect("RIR verify failed");
    let source = emit::emit(&RirPlan { program });
    let source = source.as_str();

    assert!(source.contains("lambda_env0: heap.register_tracked::<LambdaEnv0"));
    assert!(source.contains("#[derive(anvyx_runtime::Trace)]\n#[trace(crate = anvyx_runtime, ctx = 'cx)]\nstruct LambdaEnv0<'cx>"));
    assert!(source.contains("c0: LambdaSig0<'cx>,"));
    assert!(source.contains("#[derive(Clone)]\nenum LambdaSig0<'cx>"));
    assert!(source.contains("unsafe impl<'cx> anvyx_runtime::Trace<'cx> for LambdaSig0<'cx>"));
}

fn valid_stack_cell_decl() -> RirCellDecl {
    RirCellDecl {
        id: RirCellId::from_index(0),
        owner: RirFunctionId::from_index(0),
        source_local: RirLocalId::from_index(0),
        payload_ty: RirTypeId::from_index(1),
        storage: RirCellStorage::StackScoped,
        lifetime: RirCellLifetime::Function,
        symbol: RirSymbol::new("__cell0"),
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

    let mut program = RirProgram {
        types: vec![RirType::Void, RirType::Int, RirType::Lambda(sig)],
        lambda_sigs: vec![RirLambdaSig {
            id: sig,
            params: vec![],
            ret: void,
        }],
        lambdas: vec![RirLambda {
            id: lambda,
            function: target,
            sig,
            escape: RirLambdaEscape::Escaping,
            storage: RirLambdaStorage::HeapEnv { env },
            captures: vec![RirLambdaCapture {
                ty: int,
                mode: RirPassMode::Value,
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
                symbol: RirSymbol::new("target"),
                params: vec![capture],
                ret: RirReturn { ty: void },
                locals: vec![RirLocal {
                    id: capture,
                    ty: int,
                    mutable: false,
                    symbol: RirSymbol::new("capture"),
                    binding: RirLocalBinding::Parameter {
                        mode: RirPassMode::Value,
                        escape: RirParamEscape::NonEscaping,
                    },
                }],
                body: RirStructuredBlock::default(),
            },
            RirFunction {
                id: maker,
                symbol: RirSymbol::new("maker"),
                params: vec![],
                ret: RirReturn { ty: void },
                locals: vec![
                    RirLocal {
                        id: source,
                        ty: int,
                        mutable: false,
                        symbol: RirSymbol::new("source"),
                        binding: RirLocalBinding::Value,
                    },
                    RirLocal {
                        id: f,
                        ty: lambda_ty,
                        mutable: false,
                        symbol: RirSymbol::new("f"),
                        binding: RirLocalBinding::Value,
                    },
                ],
                body: RirStructuredBlock {
                    stmts: vec![RirStmt::Init {
                        local: f,
                        value: RirRValue::Lambda {
                            lambda,
                            captures: vec![RirLambdaCaptureArg::Owned {
                                value: rir_owned(
                                    RirOperand::Place(RirPlace::local(source, vec![])),
                                    0,
                                ),
                            }],
                            ty: lambda_ty,
                        },
                    }],
                    term: RirTerm::Return(None),
                },
            },
        ],
        ..RirProgram::default()
    };
    let mut materializers = TestMaterializers::new(&mut program);
    materializers.copy(int, RustRecipePosition::Value);
    materializers.callable_share(lambda_ty, RustRecipePosition::Value);
    program
}
fn valid_scoped_place_cell_decl() -> RirScopedPlaceCellDecl {
    RirScopedPlaceCellDecl {
        id: RirScopedPlaceCellId::from_index(0),
        owner: RirFunctionId::from_index(0),
        source: scoped_source_param(RirLocalId::from_index(0)),
        payload_ty: RirTypeId::from_index(1),
        symbol: RirSymbol::new("__scoped0"),
    }
}

fn scoped_source_param(local: RirLocalId) -> RirScopedPlaceSource {
    RirScopedPlaceSource::SourceMutParam {
        place: RirMutPlaceArg::from_handle(RirMutPlaceHandle::Param { local }, vec![]),
    }
}
fn scoped_source_pattern_alias(
    local: RirLocalId,
    projections: Vec<RirPlaceStep>,
) -> RirScopedPlaceSource {
    RirScopedPlaceSource::PatternAlias {
        place: RirMutPlaceArg::from_handle(RirMutPlaceHandle::Param { local }, projections),
    }
}
fn scoped_place_cell_rir(cell: RirScopedPlaceCellDecl) -> RirProgram {
    let void = RirTypeId::from_index(0);
    let int = RirTypeId::from_index(1);
    let owner = RirFunctionId::from_index(0);
    let source = RirLocalId::from_index(0);
    let mut program = RirProgram {
        types: vec![RirType::Void, RirType::Int],
        scoped_place_cells: vec![cell],
        functions: vec![RirFunction {
            id: owner,
            symbol: RirSymbol::new("owner"),
            params: vec![source],
            ret: RirReturn { ty: void },
            locals: vec![RirLocal {
                id: source,
                ty: int,
                mutable: true,
                symbol: RirSymbol::new("source"),
                binding: RirLocalBinding::Parameter {
                    mode: RirPassMode::MutPlace,
                    escape: RirParamEscape::NonEscaping,
                },
            }],
            body: RirStructuredBlock::default(),
        }],
        ..RirProgram::default()
    };
    let mut materializers = TestMaterializers::new(&mut program);
    materializers.copy(int, RustRecipePosition::Value);
    program
}
fn mut_place_sink_function(id: RirFunctionId) -> RirFunction {
    mut_place_sink_function_with_ty(id, RirTypeId::from_index(1))
}

fn mut_place_sink_function_with_ty(id: RirFunctionId, ty: RirTypeId) -> RirFunction {
    let source = RirLocalId::from_index(0);
    RirFunction {
        id,
        symbol: RirSymbol::new("mut_place_sink"),
        params: vec![source],
        ret: RirReturn {
            ty: RirTypeId::from_index(0),
        },
        locals: vec![RirLocal {
            id: source,
            ty,
            mutable: true,
            symbol: RirSymbol::new("p"),
            binding: RirLocalBinding::Parameter {
                mode: RirPassMode::MutPlace,
                escape: RirParamEscape::NonEscaping,
            },
        }],
        body: RirStructuredBlock::default(),
    }
}
fn assert_rir_error(program: RirProgram, kind: RirVerifyErrorKind) {
    let errors = rir::verify(&program).expect_err("verified invalid RIR");
    let found = errors.iter().any(|error| error.kind == kind);
    let kind = std::hint::black_box(kind);
    std::hint::black_box(program);
    assert!(found, "missing {kind:?}: {errors:?}");
}
fn valid_global_rir(edit: impl FnOnce(&mut RirProgram)) -> RirProgram {
    let int = RirTypeId::from_index(0);
    let void = RirTypeId::from_index(1);
    let value = RirConstId::from_index(0);
    let global = RirGlobalId::from_index(0);
    let init = RirFunctionId::from_index(0);
    let main = RirFunctionId::from_index(1);
    let mut program = RirProgram {
        globals: vec![RirGlobal {
            id: global,
            name: RirSymbol::new("game.score"),
            slot_symbol: RirSymbol::new("g0_score"),
            ty: int,
            mutable: true,
            init,
        }],
        types: vec![RirType::Int, RirType::Void],
        consts: vec![RirConst {
            id: value,
            ty: int,
            value: RirConstValue::Int(7),
        }],
        functions: vec![
            RirFunction {
                id: init,
                symbol: RirSymbol::new("ginit0"),
                params: vec![],
                ret: RirReturn { ty: int },
                locals: vec![],
                body: RirStructuredBlock {
                    stmts: vec![],
                    term: RirTerm::Return(Some(RirOperand::Const(value))),
                },
            },
            RirFunction {
                id: main,
                symbol: RirSymbol::new("main"),
                params: vec![],
                ret: RirReturn { ty: void },
                locals: vec![],
                body: RirStructuredBlock {
                    stmts: vec![
                        RirStmt::GlobalEnsure { global },
                        RirStmt::GlobalSetRoot {
                            global,
                            value: RirRValue::Use(RirOperand::Const(value)),
                        },
                    ],
                    term: RirTerm::Return(None),
                },
            },
        ],
        ..RirProgram::default()
    };
    let mut materializers = TestMaterializers::new(&mut program);
    materializers.copy(int, RustRecipePosition::Global);
    materializers.copy(int, RustRecipePosition::Value);
    edit(&mut program);
    program
}
fn rir_function(
    id: RirFunctionId,
    ret: RirTypeId,
    params: Vec<RirLocalId>,
    locals: Vec<RirLocal>,
    stmts: Vec<RirStmt>,
) -> RirFunction {
    RirFunction {
        id,
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
fn rir_local(id: RirLocalId, ty: RirTypeId, mutable: bool, symbol: &str) -> RirLocal {
    RirLocal {
        id,
        ty,
        mutable,
        symbol: RirSymbol::new(symbol),
        binding: RirLocalBinding::Value,
    }
}
fn rir_global_place() -> RirPlace {
    RirPlace::global(RirGlobalId::from_index(0), vec![])
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
                        fields: vec![owned(Operand::Const(seven))],
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
mod enums {}
mod arrays {}

mod lists {}

mod slices {}
fn native_callback_sig() -> anvyx_runtime::ExternCallbackSignature {
    anvyx_runtime::ExternCallbackSignature {
        params: vec![anvyx_runtime::ExternCallbackParam {
            ty: anvyx_runtime::ExternTypeExpr::Int,
            escape: anvyx_runtime::CallbackEscape::NonEscaping,
        }],
        ret: Box::new(anvyx_runtime::ExternTypeExpr::Int),
        policy: anvyx_runtime::CallbackPolicy {
            escape: anvyx_runtime::CallbackEscape::NonEscaping,
            thread: anvyx_runtime::CallbackThread::SameThread,
        },
    }
}

fn host_escaping_lambda_support() -> anvyx_runtime::RustProviderSupport {
    let mut callback = native_callback_sig();
    callback.policy.escape = anvyx_runtime::CallbackEscape::Escaping;
    host_lambda_support_with(anvyx_runtime::RustParamAbi::EscapingLambda(callback))
}

fn host_escaping_lambda_retained_support() -> anvyx_runtime::RustProviderSupport {
    let mut support = host_escaping_lambda_support();
    support.modules[0].bindings.push(function_binding(
        "host_lambda",
        "host",
        &["host", "trigger"],
        "trigger",
        vec![],
        anvyx_runtime::RustReturnAbi::Void,
        false,
    ));
    support
}
fn host_lambda_support_with(
    param: anvyx_runtime::RustParamAbi,
) -> anvyx_runtime::RustProviderSupport {
    let mut binding = function_binding(
        "host_lambda",
        "host",
        &["host", "apply"],
        "apply",
        vec![param],
        anvyx_runtime::RustReturnAbi::Void,
        false,
    );
    binding.abi.support = anvyx_runtime::RustAbiSupport::NeedsWrapperConversion;
    binding.abi.ctx = anvyx_runtime::RustWrapperCtx::None;
    provider_support("host_lambda", vec![binding])
}

fn host_lambda_extern(
    program: &mut Program,
    lambda_ty: air::TypeId,
    void: air::TypeId,
) -> air::ExternId {
    let id = extern_in_module(
        program,
        &["host_lambda"],
        "apply",
        vec![(lambda_ty, ParamMode::Value)],
        void,
        ExternMember::FreeFunction,
    );
    program.externs[id.index()].binding = Some(provider_binding("host_lambda", "apply"));
    id
}
fn host_trigger_extern(program: &mut Program, void: air::TypeId) -> air::ExternId {
    let id = extern_in_module(
        program,
        &["host_lambda"],
        "trigger",
        vec![],
        void,
        ExternMember::FreeFunction,
    );
    program.externs[id.index()].binding = Some(provider_binding("host_lambda", "trigger"));
    id
}

fn native_escaping_lambda_air_with() -> Program {
    let mut program = native_scoped_lambda_air_with();
    set_extern_param_escape(
        &mut program,
        air::ExternId::from_index(0),
        0,
        ParamEscape::Escaping,
    );
    program
}

fn native_escaping_lambda_trigger_air() -> Program {
    let mut program = native_escaping_lambda_air_with();
    let void = program.externs[0].return_type;
    let trigger = host_trigger_extern(&mut program, void);
    let entry = program.entry.expect("entry function");
    program
        .function_mut(entry)
        .body
        .block
        .stmts
        .push(Statement::Eval(RValue::Call {
            callee: Callee::Extern(trigger),
            args: vec![],
        }));
    program
}
fn native_scoped_lambda_air_with() -> Program {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let void = program.alloc_type(TypeData::Void);
    let sig = air::SignatureType::new(
        vec![air::ParamType {
            ty: int,
            mode: ParamMode::Value,
            escape: ParamEscape::NonEscaping,
        }],
        air::ReturnMode::Value(int),
    );
    let lambda_ty = program.alloc_type(TypeData::Function(sig));
    let module = program.alloc_module(root_module());
    let arg = air::LocalId::from_index(0);
    let callback = program.alloc_function(Function {
        name: Ident::new("callback"),
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
    let lambda_body = program.alloc_function(Function {
        name: Ident::new("lambda"),
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
    let ext = host_lambda_extern(&mut program, lambda_ty, void);
    let lambda_local = air::LocalId::from_index(0);
    let locals = vec![local(lambda_ty, LocalKind::Temp)];
    let stmts = vec![
        Statement::Init {
            local: lambda_local,
            value: RValue::FunctionRef {
                function: callback,
                ty: lambda_ty,
            },
        },
        Statement::Eval(RValue::Call {
            callee: Callee::Extern(ext),
            args: vec![CallArg::Value(owned(Operand::Place(place(
                lambda_local,
                lambda_ty,
            ))))],
        }),
    ];
    let main = program.alloc_function(Function {
        name: Ident::new("main"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], void),
        locals,
        body: structured_body(stmts, air::AirTail::Return(None)),
    });
    program
        .module_mut(module)
        .functions
        .extend([callback, lambda_body, main]);
    program.set_entry(main);
    program
}
fn int_const(program: &mut Program, ty: air::TypeId, value: i64) -> air::ConstId {
    program.const_arena.alloc(ConstData {
        ty,
        value: ConstValue::Int(value),
    })
}

fn plan_source(mut program: Program) -> RustSource {
    air::finalize_materialization(&mut program);
    air::finalize_materialization(&mut program);
    let verified = air::verify(&program).expect("AIR verify failed");
    let source = generate(&verified, rust_plan_config()).expect("generation failed");
    std::hint::black_box(program);
    source
}

fn rust_plan_config() -> RustPlanConfig {
    RustPlanConfig {
        symbol_prefix: "anv".into(),
        native_providers: vec![
            core_runtime_support(),
            core_string_support(),
            core_int_support(),
            fallible_host_support(),
            host_mut_support(),
        ],
    }
}

fn host_escaping_lambda_retained_plan_config() -> RustPlanConfig {
    RustPlanConfig {
        symbol_prefix: "anv".into(),
        native_providers: vec![host_escaping_lambda_retained_support()],
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

fn with_fallible_host(source: &RustSource) -> RustSource {
    RustSource::new(format!(
        "mod host {{ pub fn fallible<'cx, 'rt>(_ctx: &mut anvyx_runtime::Ctx<'cx, 'rt>, value: i64) -> Result<i64, anvyx_runtime::RuntimeError> {{ Ok(value) }} }}\n{}",
        source.as_str()
    ))
}

fn run_source(source: RustSource) -> source_job::RustSourceJobOutput {
    let cache = tempfile::tempdir().expect("temp dir failed");
    let deps = [
        ("anvyx_core", "anvyx-core", "core"),
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
fn plan_gaps(mut program: Program, config: RustPlanConfig) -> Vec<RustTargetGap> {
    air::finalize_materialization(&mut program);
    air::finalize_materialization(&mut program);
    let verified = air::verify(&program).expect("AIR verify failed");
    let Err(RustPlanError::TargetGaps(gaps)) = plan(&verified, config) else {
        panic!("expected target gap");
    };
    std::hint::black_box(program);
    gaps.0
}

fn assert_plan_gap(program: Program, config: RustPlanConfig, kind: RustTargetGapKind) {
    assert!(
        plan_gaps(program, config)
            .iter()
            .any(|gap| gap.kind == kind)
    );
}

fn assert_default_plan_gap(program: Program, kind: RustTargetGapKind) {
    assert_plan_gap(program, rust_plan_config(), kind);
}
fn core_runtime_support() -> anvyx_runtime::RustProviderSupport {
    use anvyx_runtime::{ExternTypeExpr, RustParamAbi, RustReturnAbi};

    provider_support(
        "core_runtime",
        vec![
            function_binding(
                "core_runtime",
                "anvyx_core",
                &["__anvyx_native", "core_runtime", "_println"],
                "_println",
                vec![RustParamAbi::Borrow(ExternTypeExpr::String)],
                RustReturnAbi::Void,
                false,
            ),
            function_binding(
                "core_runtime",
                "anvyx_core",
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

fn core_string_support() -> anvyx_runtime::RustProviderSupport {
    use anvyx_runtime::{ExternTypeExpr, RustParamAbi, RustReturnAbi};

    provider_support(
        "core_string",
        vec![function_binding(
            "core_string",
            "anvyx_core",
            &["__anvyx_native", "core_string", "str_len"],
            "str_len",
            vec![RustParamAbi::Borrow(ExternTypeExpr::String)],
            RustReturnAbi::Value(ExternTypeExpr::Int),
            false,
        )],
    )
}

fn core_int_support() -> anvyx_runtime::RustProviderSupport {
    use anvyx_runtime::{ExternTypeExpr, RustParamAbi, RustReturnAbi};

    provider_support(
        "core_int",
        vec![function_binding(
            "core_int",
            "anvyx_core",
            &["__anvyx_native", "core_int", "int_abs"],
            "int_abs",
            vec![RustParamAbi::Value(ExternTypeExpr::Int)],
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

fn host_mut_support() -> anvyx_runtime::RustProviderSupport {
    use anvyx_runtime::{ExternTypeExpr, RustParamAbi, RustReturnAbi};

    provider_support(
        "host",
        vec![
            function_binding(
                "host",
                "host",
                &["host", "touch"],
                "touch",
                vec![RustParamAbi::MutBorrow(ExternTypeExpr::Int)],
                RustReturnAbi::Void,
                false,
            ),
            function_binding(
                "host",
                "host",
                &["host", "touch_place"],
                "touch_place",
                vec![RustParamAbi::MutPlace(ExternTypeExpr::Int)],
                RustReturnAbi::Void,
                false,
            ),
        ],
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
            ctx: anvyx_runtime::RustWrapperCtx::HiddenRuntime,
        },
    }
}
fn air_module(path: &[&str]) -> air::Module {
    air::Module {
        path: path.iter().map(|segment| Ident::new(*segment)).collect(),
        ..air::Module::default()
    }
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
    let abi = test_extern_abi(program, &params, return_type);
    let id = program.alloc_extern(ExternDecl {
        name: Ident::new(name),
        span: None,
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
        abi,
        binding: None,
        effects: anvyx_runtime::ExternEffects::default(),
    });
    program.module_mut(module).externs.push(id);
    id
}

fn set_extern_param_escape(
    program: &mut Program,
    id: air::ExternId,
    index: usize,
    escape: ParamEscape,
) {
    program.externs[id.index()].params[index].escape = escape;
    let escape = match escape {
        ParamEscape::NonEscaping => anvyx_runtime::CallbackEscape::NonEscaping,
        ParamEscape::Escaping => anvyx_runtime::CallbackEscape::Escaping,
    };
    if let Some(anvyx_runtime::ExternTypeExpr::Callback(callback)) =
        program.externs[id.index()].abi.params.get_mut(index)
    {
        callback.policy.escape = escape;
    }
}
fn test_extern_abi(
    program: &Program,
    params: &[(air::TypeId, ParamMode)],
    ret: air::TypeId,
) -> air::ExternAbi {
    air::ExternAbi {
        params: params
            .iter()
            .map(|(ty, _)| test_type_abi(program, *ty))
            .collect(),
        ret: test_type_abi(program, ret),
    }
}

fn test_type_abi(program: &Program, ty: air::TypeId) -> anvyx_runtime::ExternTypeExpr {
    use anvyx_runtime::ExternTypeExpr;
    match program.type_arena.data(ty) {
        TypeData::Void => ExternTypeExpr::Void,
        TypeData::Bool => ExternTypeExpr::Bool,
        TypeData::Int => ExternTypeExpr::Int,
        TypeData::Float => ExternTypeExpr::Float,
        TypeData::String => ExternTypeExpr::String,
        TypeData::Char => ExternTypeExpr::Char,
        TypeData::Optional(inner) => {
            ExternTypeExpr::Option(Box::new(test_type_abi(program, *inner)))
        }
        TypeData::List(inner) => ExternTypeExpr::List(Box::new(test_type_abi(program, *inner))),
        TypeData::Slice(inner) => ExternTypeExpr::Slice(Box::new(test_type_abi(program, *inner))),
        TypeData::Array { elem, len } => ExternTypeExpr::Array {
            elem: Box::new(test_type_abi(program, *elem)),
            len: *len as u64,
        },
        TypeData::Map { key, value, .. } => ExternTypeExpr::Map(
            Box::new(test_type_abi(program, *key)),
            Box::new(test_type_abi(program, *value)),
        ),
        TypeData::Tuple(fields) => ExternTypeExpr::Tuple(
            fields
                .iter()
                .map(|ty| test_type_abi(program, *ty))
                .collect(),
        ),
        TypeData::Function(sig) => {
            ExternTypeExpr::Callback(anvyx_runtime::ExternCallbackSignature {
                params: sig
                    .params
                    .iter()
                    .map(|param| anvyx_runtime::ExternCallbackParam {
                        ty: test_type_abi(program, param.ty),
                        escape: anvyx_runtime::CallbackEscape::NonEscaping,
                    })
                    .collect(),
                ret: Box::new(test_type_abi(program, sig.ret.ty())),
                policy: anvyx_runtime::CallbackPolicy {
                    escape: anvyx_runtime::CallbackEscape::NonEscaping,
                    thread: anvyx_runtime::CallbackThread::SameThread,
                },
            })
        }
        TypeData::Extern(ext) => {
            let ext = program.extern_type(*ext);
            let (module, name) = ext
                .binding
                .as_ref()
                .map_or((None, ext.name.to_string()), |binding| {
                    (Some(binding.key.module.clone()), binding.key.name.clone())
                });
            ExternTypeExpr::Named {
                module,
                name,
                args: vec![],
            }
        }
        TypeData::Any
        | TypeData::Aggregate(_)
        | TypeData::Enum(_)
        | TypeData::Flag(_)
        | TypeData::DataRef(_)
        | TypeData::Dyn(_) => ExternTypeExpr::Any,
    }
}
