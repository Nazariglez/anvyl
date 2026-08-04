use air::AirStmt as Statement;
use anvyx_frontend::{
    air::{
        self, AggregateCtor, BindingId, CallArg, Callee, CaptureCellDecl, CaptureCellLifetime,
        ConstData, ConstValue, ExternBindingDecl, ExternDecl, ExternMember, ExternParamDecl,
        FieldDecl, Function, FunctionId, FunctionKind, LambdaDecl, LambdaEscape, Local, LocalKind,
        Mutability, Operand, OwnedValue, Param, ParamEscape, ParamMode, ParamRole, Place,
        PlaceRoot, Program, Projection, RValue, Signature, TypeData,
    },
    ast::{ExprId, Ident},
};

use super::{
    RustPlanConfig, RustPlanError, RustTargetGapKind, cargo_job, emit, plan,
    profile::{ProfileErrorKind, ProfileSite, RustBackendProfile, RustBackendProfileError},
    rep_policy::{LambdaStorageFamily, RustRecipePosition},
    rir::{
        self, RirCallArg, RirCallTarget, RirCellDecl, RirCellId, RirCellLifetime, RirCellRef,
        RirCellStorage, RirConst, RirConstId, RirConstValue, RirField, RirFieldId, RirFunction,
        RirFunctionId, RirGlobal, RirGlobalId, RirLambda, RirLambdaCapture, RirLambdaCaptureArg,
        RirLambdaCaptureKind, RirLambdaEnvField, RirLambdaEnvFieldKind, RirLambdaEnvId,
        RirLambdaEnvLayout, RirLambdaEscape, RirLambdaId, RirLambdaSig, RirLambdaSigId,
        RirLambdaSource, RirLambdaStorage, RirLocal, RirLocalId, RirMaterializer,
        RirMaterializerAction, RirMutPlaceArg, RirMutPlaceHandle, RirOperand, RirOptionMatch,
        RirOptionSubject, RirOwnedOperand, RirOwnedSource, RirOwnedValue, RirParam, RirParamAbi,
        RirParamEscape, RirParamSemantic, RirPlace, RirProgram, RirProjection, RirRValue,
        RirReturn, RirScopedPlaceCellDecl, RirScopedPlaceCellId, RirScopedPlaceSource, RirStmt,
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
        self.program.materializers.push(RirMaterializer {
            id,
            ty,
            position,
            action,
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
fn profile_rejects_escaping_native_scoped_lambda_param() {
    let mut program = native_scoped_lambda_air();
    set_extern_param_escape(
        &mut program,
        air::ExternId::from_index(0),
        0,
        ParamEscape::Escaping,
    );
    air::finalize_materialization(&mut program);
    let verified = air::verify(&program).expect("AIR verify failed");
    let errors = RustBackendProfile::check_with_native_support(
        &verified,
        &host_lambda_plan_config().native_providers,
    )
    .expect_err("profile accepted escaping native lambda");

    assert!(has_error(
        &errors,
        ProfileErrorKind::UnsupportedLambdaExternBoundary
    ));
}

#[test]
fn profile_rejects_direct_native_scoped_lambda_provider_abi() {
    let mut support = host_lambda_support();
    let binding = &mut support.modules[0].bindings[0];
    binding.abi.support = anvyx_runtime::RustAbiSupport::Direct;
    binding.abi.ctx = anvyx_runtime::RustWrapperCtx::HiddenRuntime;
    let mut program = native_scoped_lambda_air();
    air::finalize_materialization(&mut program);
    let verified = air::verify(&program).expect("AIR verify failed");
    let errors = RustBackendProfile::check_with_native_support(&verified, &[support])
        .expect_err("profile accepted direct scoped native lambda ABI");

    assert!(has_error(&errors, ProfileErrorKind::UnsupportedRustAbi));
}

#[test]
fn native_scoped_lambda_provider_abi_rejects_visible_borrows() {
    let callback = native_callback_sig();
    let abi = anvyx_runtime::RustExternAbi {
        params: vec![
            anvyx_runtime::RustParamAbi::Borrow(anvyx_runtime::ExternTypeExpr::String),
            anvyx_runtime::RustParamAbi::ScopedLambda(callback),
        ],
        ret: anvyx_runtime::RustReturnAbi::Void,
        fallible: false,
        support: anvyx_runtime::RustAbiSupport::NeedsWrapperConversion,
        ctx: anvyx_runtime::RustWrapperCtx::None,
    };

    assert!(!rir::rust_extern_abi_supported_with_receiver(&abi, None));
}

#[test]
fn native_direct_provider_abi_rejects_mutable_collections_and_ctxless_mut_place() {
    use anvyx_runtime::{
        ExternTypeExpr, RustAbiSupport, RustExternAbi, RustParamAbi, RustReturnAbi, RustWrapperCtx,
    };

    let list = ExternTypeExpr::List(Box::new(ExternTypeExpr::Int));

    for (params, ret, ctx) in [
        (
            vec![RustParamAbi::MutBorrow(list.clone())],
            RustReturnAbi::Void,
            RustWrapperCtx::HiddenRuntime,
        ),
        (
            vec![RustParamAbi::MutPlace(list)],
            RustReturnAbi::Void,
            RustWrapperCtx::HiddenRuntime,
        ),
        (
            vec![RustParamAbi::MutPlace(ExternTypeExpr::Int)],
            RustReturnAbi::Void,
            RustWrapperCtx::None,
        ),
    ] {
        let abi = RustExternAbi {
            params,
            ret,
            fallible: false,
            support: RustAbiSupport::Direct,
            ctx,
        };

        assert!(!rir::rust_extern_abi_supported_with_receiver(&abi, None));
    }
}

#[test]
fn emit_retained_callback_registry_before_heap() {
    let mut program = native_escaping_lambda_trigger_air();
    air::finalize_materialization(&mut program);
    air::finalize_materialization(&mut program);
    let verified = air::verify(&program).expect("AIR verify failed");
    let plan = plan(&verified, host_escaping_lambda_retained_plan_config()).expect("plan failed");
    let source = emit::emit(&plan.verified()).into_string();
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
                    subject: RirOptionSubject::Place(rir_global_place(RirTypeId::from_index(0))),
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
        lifetime: CaptureCellLifetime::Function,
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

    let output = run_source(with_fallible_host(plan_source(program)));

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
fn profile_rejects_function_place_returns() {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let module = program.alloc_module(root_module());
    let x = air::LocalId::from_index(0);
    let function = program.alloc_function(Function {
        name: Ident::new("id"),
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

    expect_reject(program, ProfileErrorKind::UnsupportedReturnMode);
}

#[test]
fn profile_rejects_function_type_place_returns() {
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

    expect_reject(program, ProfileErrorKind::UnsupportedReturnMode);
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
        copyable: true,
        fields: vec![RirField {
            id: RirFieldId::from_index(0),
            symbol: RirSymbol::new("0"),
            ty: int,
        }],
    });
    program.functions[0].params[0].ty = tuple_ty;
    program.functions[0].locals[0].ty = tuple_ty;
    program.scoped_place_cells[0].source = scoped_source_pattern_alias(
        RirLocalId::from_index(0),
        tuple_ty,
        vec![RirProjection::TupleField(RirFieldId::from_index(0))],
        int,
    );
    let mut materializers = TestMaterializers::new(&mut program);
    materializers.copy(
        int,
        RustRecipePosition::StoredPayload(LambdaStorageFamily::TupleField),
    );

    rir::verify(&program).expect("RIR rejected pattern alias scoped-place source");
}

#[test]
fn rir_rejects_pattern_alias_scoped_place_source_local_root() {
    let mut program = scoped_place_cell_rir(valid_scoped_place_cell_decl());
    let int = RirTypeId::from_index(1);
    program.scoped_place_cells[0].source = RirScopedPlaceSource::PatternAlias {
        place: RirMutPlaceArg::from_handle(
            RirMutPlaceHandle::Local {
                local: RirLocalId::from_index(0),
                ty: int,
            },
            vec![],
            int,
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
                args: vec![RirCallArg::MutPlace(RirMutPlaceArg::projected(
                    RirMutPlaceHandle::StackCell {
                        cell: RirCellRef::Owner(RirCellId::from_index(0)),
                        ty: array,
                    },
                    vec![RirProjection::Index(RirLocalId::from_index(1))],
                    int,
                ))],
                ty: void,
            })],
        )],
        ..RirProgram::default()
    };
    program
        .functions
        .push(mut_place_sink_function(RirFunctionId::from_index(1)));

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
    captures[0] = RirLambdaCaptureArg::Owned {
        value: rir_owned(
            RirOperand::Place(RirPlace::local(
                RirLocalId::from_index(0),
                vec![],
                lambda_ty,
            )),
            1,
        ),
    };

    let verified = rir::verify(&program).expect("RIR verify failed");
    let source = emit::emit(&verified).into_string();

    assert!(source.contains("lambda_env0: heap.register_tracked::<LambdaEnv0"));
    assert!(source.contains("#[derive(anvyx_runtime::Trace)]\n#[trace(crate = anvyx_runtime, ctx = 'cx)]\nstruct LambdaEnv0<'cx>"));
    assert!(source.contains("c0: LambdaSig0<'cx>,"));
    assert!(source.contains("#[derive(Clone)]\nenum LambdaSig0<'cx>"));
    assert!(source.contains("unsafe impl<'cx> anvyx_runtime::Trace<'cx> for LambdaSig0<'cx>"));
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
        unreachable!("test helper should start with source ref call")
    };
    *callee = Callee::Extern(ext);

    expect_reject(
        program,
        ProfileErrorKind::UnsupportedMutablePlaceNativeBoundary,
    );
}

#[test]
fn profile_rejects_capture_cell_to_native_mut_borrow() {
    let mut program = lambda_capture_cell_source_ref_arg_program();
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
        unreachable!("test helper should start with the source ref call")
    };
    *callee = Callee::Extern(ext);

    expect_reject(
        program,
        ProfileErrorKind::UnsupportedMutablePlaceNativeBoundary,
    );
}
#[test]
fn profile_rejects_source_ref_param_to_native_mut_borrow() {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let void = program.alloc_type(TypeData::Void);
    let ext = host_mut_extern(&mut program, int, void, "touch");
    let module = program.alloc_module(root_module());
    let x = air::LocalId::from_index(0);
    caller_function(
        &mut program,
        module,
        Signature::new(vec![param("x", int, ParamMode::MutBorrow, x)], void),
        vec![mut_local(int, LocalKind::Arg)],
        vec![call_mut_ext(ext, place(x, int))],
    );

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
    let ext = host_mut_extern(&mut program, int, void, "touch");
    let module = program.alloc_module(root_module());
    let aggregate = dataref_decl(&mut program, module, int);
    let node = program.alloc_type(TypeData::DataRef(aggregate));
    let node_local = air::LocalId::from_index(0);
    caller_function(
        &mut program,
        module,
        Signature::new(
            vec![param("node", node, ParamMode::Value, node_local)],
            void,
        ),
        vec![local(node, LocalKind::Arg)],
        vec![call_mut_ext(
            ext,
            Place {
                root: PlaceRoot::Local(node_local),
                projection: vec![Projection::Field(air::FieldId::from_index(0))],
                ty: int,
            },
        )],
    );

    expect_reject(
        program,
        ProfileErrorKind::UnsupportedMutablePlaceNativeBoundary,
    );
}

#[test]
fn profile_rejects_projected_local_to_native_mut_borrow() {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let void = program.alloc_type(TypeData::Void);
    let tuple = program.alloc_type(TypeData::Tuple(vec![int]));
    let ext = host_mut_extern(&mut program, int, void, "touch");
    let module = program.alloc_module(root_module());
    let pair = air::LocalId::from_index(0);
    let one = int_const(&mut program, int, 1);
    caller_function(
        &mut program,
        module,
        Signature::new(vec![], void),
        vec![mut_local(tuple, LocalKind::Temp)],
        vec![
            Statement::Init {
                local: pair,
                value: RValue::Aggregate {
                    kind: AggregateCtor::Tuple,
                    fields: vec![owned(Operand::Const(one))],
                    ty: tuple,
                },
            },
            call_mut_ext(
                ext,
                Place {
                    root: PlaceRoot::Local(pair),
                    projection: vec![Projection::TupleField(0)],
                    ty: int,
                },
            ),
        ],
    );

    expect_reject(
        program,
        ProfileErrorKind::UnsupportedMutablePlaceNativeBoundary,
    );
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
                            captures: vec![RirLambdaCaptureArg::Owned {
                                value: rir_owned(
                                    RirOperand::Place(RirPlace::local(source, vec![], int)),
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
        source: scoped_source_param(RirLocalId::from_index(0), RirTypeId::from_index(1)),
        payload_ty: RirTypeId::from_index(1),
        symbol: RirSymbol::new("__scoped0"),
    }
}

fn scoped_source_param(local: RirLocalId, ty: RirTypeId) -> RirScopedPlaceSource {
    RirScopedPlaceSource::SourceMutParam {
        place: RirMutPlaceArg::from_handle(RirMutPlaceHandle::Param { local, ty }, vec![], ty),
    }
}
fn scoped_source_pattern_alias(
    local: RirLocalId,
    root_ty: RirTypeId,
    projections: Vec<RirProjection>,
    ty: RirTypeId,
) -> RirScopedPlaceSource {
    RirScopedPlaceSource::PatternAlias {
        place: RirMutPlaceArg::from_handle(
            RirMutPlaceHandle::Param { local, ty: root_ty },
            projections,
            ty,
        ),
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
        air_id: None,
        symbol: RirSymbol::new("mut_place_sink"),
        params: vec![RirParam {
            local: source,
            ty,
            semantic: RirParamSemantic::MutPlace,
            abi: RirParamAbi::MutPlace,
            escape: RirParamEscape::NonEscaping,
        }],
        ret: RirReturn {
            ty: RirTypeId::from_index(0),
        },
        locals: vec![RirLocal {
            id: source,
            ty,
            mutable: true,
            symbol: RirSymbol::new("p"),
            initialized: true,
            payload_ref: false,
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
            air_id: air::GlobalId::from_index(0),
            module: air::ModuleId::from_index(0),
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
                air_id: None,
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
                air_id: None,
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
fn rir_global_place(ty: RirTypeId) -> RirPlace {
    RirPlace::global(RirGlobalId::from_index(0), vec![], ty)
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

fn host_lambda_support() -> anvyx_runtime::RustProviderSupport {
    host_lambda_support_with(anvyx_runtime::RustParamAbi::ScopedLambda(
        native_callback_sig(),
    ))
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

fn native_scoped_lambda_air() -> Program {
    native_scoped_lambda_air_with()
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

fn scoped_borrow_lambda_program() -> Program {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let void = program.alloc_type(TypeData::Void);
    let module = program.alloc_module(root_module());
    let callee = source_ref_callee(&mut program, module, int, void);
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
            vec![source_ref_call(
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
fn lambda_capture_cell_source_ref_arg_program() -> Program {
    capture_cell_source_ref_arg_program(true, LambdaEscape::NonEscaping)
}
fn capture_cell_source_ref_arg_program(in_lambda: bool, escape: LambdaEscape) -> Program {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let void = program.alloc_type(TypeData::Void);
    let module = program.alloc_module(root_module());
    let callee = source_ref_callee(&mut program, module, int, void);
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
                    source_ref_call(callee, PlaceRoot::CaptureCell(cell), int),
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
            vec![source_ref_call(
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
fn source_ref_callee(
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
        lifetime: CaptureCellLifetime::Function,
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
fn source_ref_call(callee: FunctionId, root: PlaceRoot, ty: air::TypeId) -> Statement {
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
fn plan_source(mut program: Program) -> emit::RustSource {
    air::finalize_materialization(&mut program);
    air::finalize_materialization(&mut program);
    let verified = air::verify(&program).expect("AIR verify failed");
    let plan = plan(&verified, rust_plan_config()).expect("plan failed");
    let source = emit::emit(&plan.verified());
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

fn host_lambda_plan_config() -> RustPlanConfig {
    RustPlanConfig {
        symbol_prefix: "anv".into(),
        native_providers: vec![host_lambda_support()],
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

fn with_fallible_host(source: emit::RustSource) -> emit::RustSource {
    emit::RustSource::new(format!(
        "mod host {{ pub fn fallible<'cx, 'rt>(_ctx: &mut anvyx_runtime::Ctx<'cx, 'rt>, value: i64) -> Result<i64, anvyx_runtime::RuntimeError> {{ Ok(value) }} }}\n{}",
        source.into_string()
    ))
}

fn run_source(source: emit::RustSource) -> source_job::RustSourceJobOutput {
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
fn assert_plan_gap(mut program: Program, config: RustPlanConfig, kind: RustTargetGapKind) {
    air::finalize_materialization(&mut program);
    air::finalize_materialization(&mut program);
    let verified = air::verify(&program).expect("AIR verify failed");
    let Err(RustPlanError::TargetGaps(gaps)) = plan(&verified, config) else {
        panic!("expected target gap");
    };
    std::hint::black_box(program);
    assert!(gaps.iter().any(|gap| gap.kind == kind));
}

fn expect_reject(program: Program, kind: ProfileErrorKind) {
    let errors = profile_errors(program);
    assert!(has_error(&errors, kind), "missing expected profile error");
}

fn profile_errors(mut program: Program) -> Vec<RustBackendProfileError> {
    air::finalize_materialization(&mut program);
    air::finalize_materialization(&mut program);
    let verified = air::verify(&program).expect("AIR verify failed");
    let config = rust_plan_config();
    let errors = RustBackendProfile::check_with_native_support(&verified, &config.native_providers)
        .expect_err("profile accepted invalid AIR");
    std::hint::black_box(program);
    errors
}

fn has_error(errors: &[RustBackendProfileError], kind: ProfileErrorKind) -> bool {
    errors.iter().any(|error| error.kind == kind)
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

fn host_mut_extern(
    program: &mut Program,
    int: air::TypeId,
    void: air::TypeId,
    name: &str,
) -> air::ExternId {
    let id = extern_in_module(
        program,
        &["host"],
        name,
        vec![(int, ParamMode::MutBorrow)],
        void,
        ExternMember::FreeFunction,
    );
    program.externs[id.index()].binding = Some(provider_binding("host", name));
    id
}

fn caller_function(
    program: &mut Program,
    module: air::ModuleId,
    signature: Signature,
    locals: Vec<Local>,
    stmts: Vec<Statement>,
) -> FunctionId {
    let id = program.alloc_function(Function {
        name: Ident::new("caller"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature,
        locals,
        body: structured_body(stmts, air::AirTail::Return(None)),
    });
    program.module_mut(module).functions.push(id);
    id
}

fn call_mut_ext(ext: air::ExternId, arg: Place) -> Statement {
    Statement::Eval(RValue::Call {
        callee: Callee::Extern(ext),
        args: vec![CallArg::MutBorrow(arg)],
    })
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
