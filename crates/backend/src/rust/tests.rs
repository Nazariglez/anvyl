use air::AirStmt as Statement;
use anvyx_frontend::{
    air::{
        self, AggregateCtor, AirBody, AirOptionalMatch, BindingId, CallArg, Callee,
        CaptureCellDecl, CaptureCellLifetime, CaptureLocalSource, ConstData, ConstValue,
        ContractReceiver, ContractReturnDecl, ContractSlotDecl, ContractSlotId,
        ContractSurfaceDecl, ContractWitnessDecl, ContractWitnessKey, ContractWitnessSlotDecl,
        ContractWitnessTarget, EnumDecl, ExternBindingDecl, ExternDecl, ExternFieldDecl,
        ExternMember, ExternParamDecl, ExternReceiverDecl, ExternRep, ExternTypeBindingDecl,
        ExternTypeDecl, FieldDecl, Function, FunctionId, FunctionKind, FunctionSpecialization,
        LambdaDecl, LambdaEscape, Local, LocalKind, Mutability, Operand, Param, ParamEscape,
        ParamMode, ParamRole, Place, PlaceRoot, Program, Projection, RValue, RawEnumValue,
        Signature, TypeData, TypePassClasses, VariantDecl, VariantId, VariantShape,
    },
    ast::{BinaryOp, ExprId, FormatAlign, FormatKind, FormatSign, FormatSpec, Ident},
};

use super::{
    CollectionAccessOp, PlanCx, RustPlanConfig, RustPlanError, RustTargetGapKind, cargo_job,
    dataref_place::DataRefPlaceDescriptors,
    emit,
    place_access::{PlaceAccessCx, PlaceAccessIntent},
    plan,
    profile::{ProfileErrorKind, ProfileSite, RustBackendProfile, RustBackendProfileError},
    rep_policy::{RirRustRepPolicy, RustTracePlan},
    rir::{
        self, RirCallArg, RirCallTarget, RirCellDecl, RirCellId, RirCellLifetime, RirCellRef,
        RirCellStorage, RirCollectionAccess, RirCollectionLoanMode, RirCollectionLoanScope,
        RirCollectionRootKind, RirCollectionStorage, RirCollectionStorageId,
        RirCollectionStorageKind, RirConst, RirConstId, RirConstValue, RirCoreEnumKind, RirDataRef,
        RirDataRefId, RirEnum, RirEnumId, RirExtern, RirExternId, RirExternKind, RirExternParam,
        RirField, RirFieldId, RirFormatKind, RirFormatSpec, RirFunction, RirFunctionId, RirGlobal,
        RirGlobalId, RirIf, RirLambda, RirLambdaCapture, RirLambdaCaptureArg, RirLambdaCaptureKind,
        RirLambdaEnvField, RirLambdaEnvFieldKind, RirLambdaEnvId, RirLambdaEnvLayout,
        RirLambdaEscape, RirLambdaId, RirLambdaParam, RirLambdaSig, RirLambdaSigId,
        RirLambdaSource, RirLambdaStorage, RirLocal, RirLocalId, RirLoop, RirLoopId,
        RirMapEntryMatch, RirMapWriteKind, RirMutPlaceAccess, RirMutPlaceArg, RirMutPlaceHandle,
        RirOperand, RirOptionMatch, RirOptionSubject, RirParam, RirParamAbi, RirParamEscape,
        RirParamSemantic, RirPatternAlternative, RirPatternArm, RirPatternBinding,
        RirPatternBindingMode, RirPatternMatch, RirPatternPath, RirPatternPathStep, RirPatternTest,
        RirPlace, RirPlaceRoot, RirProgram, RirProjection, RirRValue, RirReturn,
        RirScopedPlaceCellDecl, RirScopedPlaceCellId, RirScopedPlaceCellRef, RirScopedPlaceSource,
        RirStmt, RirStringifyHelper, RirStringifyHelperId, RirStringifyReq, RirStringifyReqId,
        RirStringifyReqKind, RirStruct, RirStructId, RirStructuredBlock, RirSymbol, RirTerm,
        RirTuple, RirTupleId, RirType, RirTypeId, RirVariant, RirVariantId, RirVariantKind,
        RirVerifyErrorKind, RirVerifySite,
    },
    runtime_owner::RuntimeOwnerEmit,
    source_job::{self, SourceJobStatus},
    target,
};
use crate::test_support::{
    global_with_init, immutable_local as local, mutable_local as mut_local, param, place,
    root_module, structured_body,
};

#[test]
fn profile_accepts_native_scoped_lambda_param() {
    let program = native_scoped_lambda_air();
    let verified = air::verify(&program).expect("AIR verify failed");
    let config = host_lambda_plan_config();

    RustBackendProfile::check_with_native_support(&verified, &config.native_providers)
        .expect("profile rejected scoped native lambda");
}

#[test]
fn profile_accepts_native_escaping_lambda_param() {
    let program = native_escaping_lambda_air_with(NativeLambdaArgKind::FunctionRef);
    let verified = air::verify(&program).expect("AIR verify failed");
    let config = host_escaping_lambda_plan_config();

    RustBackendProfile::check_with_native_support(&verified, &config.native_providers)
        .expect("profile rejected escaping native lambda");
}

#[test]
fn profile_rejects_retained_callback_native_borrow_reentry_arg() {
    let (program, support) = retained_callback_borrow_air(
        "borrow",
        TypeData::Int,
        ConstValue::Int(1),
        anvyx_runtime::ExternTypeExpr::Int,
    );
    let verified = air::verify(&program).expect("AIR verify failed");

    let errors = RustBackendProfile::check_with_native_support(&verified, &[support])
        .expect_err("profile accepted retained callback reentry borrow arg");

    assert!(has_error(&errors, ProfileErrorKind::UnsupportedCallArgMode));
}

fn retained_callback_string_borrow_air() -> (Program, anvyx_runtime::RustProviderSupport) {
    retained_callback_borrow_air(
        "borrow_string",
        TypeData::String,
        ConstValue::String("x".into()),
        anvyx_runtime::ExternTypeExpr::String,
    )
}

fn retained_callback_borrow_air(
    name: &str,
    ty: TypeData,
    value: ConstValue,
    rust_ty: anvyx_runtime::ExternTypeExpr,
) -> (Program, anvyx_runtime::RustProviderSupport) {
    let mut program = native_escaping_lambda_air_with(NativeLambdaArgKind::FunctionRef);
    let ty = match ty {
        TypeData::Int => program.function(FunctionId::from_index(0)).signature.params[0].ty,
        ty => program.alloc_type(ty),
    };
    let konst = program.const_arena.alloc(ConstData { ty, value });
    let void = program.externs[0].return_type;
    let borrow = extern_in_module(
        &mut program,
        &["host_lambda"],
        name,
        vec![(ty, ParamMode::SharedBorrow)],
        void,
        ExternMember::FreeFunction,
    );
    program.externs[borrow.index()].binding = Some(provider_binding("host_lambda", name));
    let entry = program.entry.expect("entry function");
    let function = program.function_mut(entry);
    let local_id = air::LocalId::from_index(function.locals.len());
    function.locals.push(local(ty, LocalKind::Temp));
    function.body.block.stmts.extend([
        Statement::Init {
            local: local_id,
            value: RValue::Use(Operand::Const(konst)),
        },
        Statement::Eval(RValue::Call {
            callee: Callee::Extern(borrow),
            args: vec![CallArg::SharedBorrow(place(local_id, ty))],
        }),
    ]);
    let mut support = host_escaping_lambda_support();
    support.modules[0].bindings.push(function_binding(
        "host_lambda",
        "host",
        &["host", name],
        name,
        vec![anvyx_runtime::RustParamAbi::Borrow(rust_ty)],
        anvyx_runtime::RustReturnAbi::Void,
        false,
    ));
    (program, support)
}

#[test]
fn profile_accepts_retained_callback_native_string_borrow_reentry_arg() {
    let (program, support) = retained_callback_string_borrow_air();
    let verified = air::verify(&program).expect("AIR verify failed");

    RustBackendProfile::check_with_native_support(&verified, &[support])
        .expect("profile rejected snapshottable retained callback string borrow");
}

fn retained_callback_string_borrow_rir() -> RirProgram {
    let (program, support) = retained_callback_string_borrow_air();
    let verified = air::verify(&program).expect("AIR verify failed");
    plan(
        &verified,
        RustPlanConfig {
            symbol_prefix: "anv".into(),
            native_providers: vec![support],
        },
    )
    .expect("plan failed")
    .verified()
    .program()
    .clone()
}

fn retained_callback_borrow_extern(program: &RirProgram) -> RirExternId {
    program
        .externs
        .iter()
        .position(|ext| {
            ext.params.len() == 1 && ext.params[0].semantic == RirParamSemantic::SharedBorrow
        })
        .map(RirExternId::from_index)
        .expect("borrow extern")
}

#[test]
fn rir_rejects_forged_retained_callback_live_native_borrow() {
    let mut program = retained_callback_string_borrow_rir();
    let int = program
        .types
        .iter()
        .position(|ty| *ty == RirType::Int)
        .map(RirTypeId::from_index)
        .expect("int type");
    let extern_id = retained_callback_borrow_extern(&program);
    let ext = &mut program.externs[extern_id.index()];
    ext.params[0].ty = int;
    let RirExternKind::Native(native) = &mut ext.kind;
    native.abi.params[0] = anvyx_runtime::RustParamAbi::Borrow(anvyx_runtime::ExternTypeExpr::Int);

    let entry = program.entry.expect("entry function");
    let function = &mut program.functions[entry.index()];
    for stmt in &mut function.body.stmts {
        match stmt {
            RirStmt::Init {
                local,
                value: RirRValue::Use(RirOperand::Const(konst)),
            } => {
                if program.consts[konst.index()].ty != int {
                    program.consts[konst.index()] = RirConst {
                        id: *konst,
                        ty: int,
                        value: RirConstValue::Int(1),
                    };
                    function.locals[local.index()].ty = int;
                }
            }
            RirStmt::Eval(RirRValue::Call { callee, args, .. })
                if *callee == RirCallTarget::Extern(extern_id) =>
            {
                let RirCallArg::SharedBorrow(place) = &mut args[0] else {
                    panic!("shared borrow arg")
                };
                place.ty = int;
            }
            _ => {}
        }
    }

    assert_rir_error(program, RirVerifyErrorKind::CallArgMode);
}

#[test]
fn rir_accepts_retained_callback_native_mut_place_descriptor() {
    let mut program = retained_callback_string_borrow_rir();
    let extern_id = retained_callback_borrow_extern(&program);
    let ext = &mut program.externs[extern_id.index()];
    ext.params[0].semantic = RirParamSemantic::MutPlace;
    ext.params[0].abi = RirParamAbi::MutPlace;
    let RirExternKind::Native(native) = &mut ext.kind;
    native.abi.params[0] =
        anvyx_runtime::RustParamAbi::MutPlace(anvyx_runtime::ExternTypeExpr::String);

    let entry = program.entry.expect("entry function");
    let function = &mut program.functions[entry.index()];
    for stmt in &mut function.body.stmts {
        let RirStmt::Eval(RirRValue::Call { callee, args, .. }) = stmt else {
            continue;
        };
        if *callee != RirCallTarget::Extern(extern_id) {
            continue;
        }
        let RirCallArg::SharedBorrow(place) = &args[0] else {
            panic!("shared borrow arg")
        };
        let RirPlaceRoot::Local(local) = place.root else {
            panic!("local arg")
        };
        function.locals[local.index()].mutable = true;
        args[0] = RirCallArg::MutPlace(RirMutPlaceArg::local(place.clone()));
    }

    rir::verify(&program).expect("mutable-place descriptor should be reentry-safe");
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
    let program = native_scoped_lambda_air();
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
fn native_direct_provider_abi_accepts_canonical_collection_carriers() {
    use anvyx_runtime::{
        ExternTypeExpr, RustAbiSupport, RustExternAbi, RustParamAbi, RustReturnAbi, RustWrapperCtx,
    };

    let list = ExternTypeExpr::List(Box::new(ExternTypeExpr::Int));
    let map = ExternTypeExpr::Map(
        Box::new(ExternTypeExpr::String),
        Box::new(ExternTypeExpr::Int),
    );

    for (params, ret) in [
        (vec![RustParamAbi::Value(list.clone())], RustReturnAbi::Void),
        (
            vec![RustParamAbi::Borrow(list.clone())],
            RustReturnAbi::Void,
        ),
        (vec![RustParamAbi::Value(map.clone())], RustReturnAbi::Void),
        (vec![], RustReturnAbi::Value(list)),
        (vec![], RustReturnAbi::Value(map)),
    ] {
        let abi = RustExternAbi {
            params,
            ret,
            fallible: false,
            support: RustAbiSupport::Direct,
            ctx: RustWrapperCtx::HiddenRuntime,
        };

        assert!(rir::rust_extern_abi_supported_with_receiver(&abi, None));
    }
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
fn plan_lowers_native_scoped_lambda_call_arg() {
    let program = native_scoped_lambda_air();
    let verified = air::verify(&program).expect("AIR verify failed");
    let plan = plan(&verified, host_lambda_plan_config()).expect("plan failed");
    let call_args = plan
        .program()
        .functions
        .iter()
        .flat_map(|function| &function.body.stmts)
        .find_map(|stmt| match stmt {
            RirStmt::Eval(RirRValue::Call { args, .. }) => Some(args.as_slice()),
            _ => None,
        })
        .expect("missing native call");

    assert!(matches!(call_args, [RirCallArg::ScopedLambda { .. }]));
}

#[test]
fn plan_lowers_native_escaping_lambda_call_arg() {
    for kind in [
        NativeLambdaArgKind::FunctionRef,
        NativeLambdaArgKind::EscapingZeroCapture,
        NativeLambdaArgKind::EscapingReadonlyCapture,
    ] {
        let program = native_escaping_lambda_air_with(kind);
        let verified = air::verify(&program).expect("AIR verify failed");
        let plan = plan(&verified, host_escaping_lambda_plan_config()).expect("plan failed");
        let has_escaping_arg = plan
            .program()
            .functions
            .iter()
            .flat_map(|function| &function.body.stmts)
            .any(|stmt| matches!(stmt, RirStmt::Eval(RirRValue::Call { args, .. }) if matches!(args.as_slice(), [RirCallArg::EscapingLambda { .. }])));

        assert!(has_escaping_arg);
    }
}

#[test]
fn plan_rejects_non_escaping_lambda_for_escaping_native_callback() {
    for kind in [
        NativeLambdaArgKind::ReadonlyCapture,
        NativeLambdaArgKind::CaptureCell,
    ] {
        let program = native_escaping_lambda_air_with(kind);

        assert!(air::verify(&program).is_err());
    }
}

#[test]
fn emit_native_escaping_lambda_stays_single_threaded() {
    let program = native_escaping_lambda_air_with(NativeLambdaArgKind::FunctionRef);
    let verified = air::verify(&program).expect("AIR verify failed");
    let plan = plan(&verified, host_escaping_lambda_plan_config()).expect("plan failed");
    let source = emit::emit(&plan.verified()).into_string();

    assert!(source.contains("anvyx_runtime::EscapingLambda::<(i64,), i64>"));
    assert!(!source.contains("Box<dyn Fn"));
    assert!(!source.contains("Arc<Mutex"));
    assert!(!source.contains("thread_local!"));
    assert!(!source.contains("RootId"));
    assert!(!source.contains("remove_root"));
    assert!(!source.contains("with_root"));
}

#[test]
fn emit_retained_callback_reentry_gate_precedes_inner_mut_borrow() {
    let program = native_escaping_lambda_air_with(NativeLambdaArgKind::FunctionRef);
    let verified = air::verify(&program).expect("AIR verify failed");
    let plan = plan(&verified, host_escaping_lambda_retained_plan_config()).expect("plan failed");
    let source = emit::emit(&plan.verified()).into_string();
    let gate = RuntimeOwnerEmit::reentry_gate_line("inner_ptr");

    for thunk in [
        "unsafe fn __anv_callback_call_",
        "unsafe fn __anv_callback_close_",
    ] {
        let start = source.find(thunk).expect("callback thunk emitted");
        let section = &source[start..];
        let gate_pos = section
            .find(&gate)
            .expect("callback thunk has reentry gate");
        let borrow_pos = section
            .find("inner_ptr.as_mut()")
            .expect("callback thunk borrows runtime inner mutably");
        assert!(gate_pos < borrow_pos, "{thunk} gates before mutable borrow");
    }
}

#[test]
fn plan_lowers_native_scoped_lambda_value_forms() {
    for kind in [
        NativeLambdaArgKind::ZeroCapture,
        NativeLambdaArgKind::ReadonlyCapture,
        NativeLambdaArgKind::EscapingReadonlyCapture,
        NativeLambdaArgKind::CaptureCell,
    ] {
        let program = native_scoped_lambda_air_with(kind);
        let verified = air::verify(&program).expect("AIR verify failed");
        let plan = plan(&verified, host_lambda_plan_config()).expect("plan failed");
        let has_scoped_arg = plan
            .program()
            .functions
            .iter()
            .flat_map(|function| &function.body.stmts)
            .any(|stmt| matches!(stmt, RirStmt::Eval(RirRValue::Call { args, .. }) if matches!(args.as_slice(), [RirCallArg::ScopedLambda { .. }])));

        assert!(has_scoped_arg);
    }
}

#[test]
fn emit_native_scoped_lambda_uses_scoped_runtime_state() {
    for kind in [
        NativeLambdaArgKind::FunctionRef,
        NativeLambdaArgKind::ZeroCapture,
    ] {
        let program = native_scoped_lambda_air_with(kind);
        let verified = air::verify(&program).expect("AIR verify failed");
        let plan = plan(&verified, host_lambda_plan_config()).expect("plan failed");
        let source = emit::emit(&plan.verified()).into_string();

        assert!(source.contains("anvyx_runtime::ScopedLambda"));
        assert!(source.contains(target::scoped_lambda_thunk()));
        assert!(source.contains(&format!("{};", target::runtime_validate_reentry("rt"))));
        assert!(!source.contains("Box<dyn Fn"));
        assert!(!source.contains("Rc<dyn Fn"));
        assert!(!source.contains("Vec<Value"));
        assert!(!source.contains("thread_local!"));
        assert!(!source.contains("LambdaEnv"));
    }
}

#[test]
fn rir_rejects_forged_callback_receiver_without_native_ref() {
    let program = native_escaping_lambda_air_with(NativeLambdaArgKind::FunctionRef);
    let verified = air::verify(&program).expect("AIR verify failed");
    let mut program = plan(&verified, host_escaping_lambda_plan_config())
        .expect("plan failed")
        .verified()
        .program()
        .clone();
    let int = program
        .types
        .iter()
        .position(|ty| *ty == RirType::Int)
        .map(RirTypeId::from_index)
        .expect("int type");
    let extern_id = RirExternId::from_index(0);
    let ext = &mut program.externs[extern_id.index()];
    let RirExternKind::Native(native) = &mut ext.kind;
    native.callback_receiver = Some(0);
    native.abi.params.insert(
        0,
        anvyx_runtime::RustParamAbi::Borrow(anvyx_runtime::ExternTypeExpr::Int),
    );
    ext.params.insert(
        0,
        RirExternParam {
            ty: int,
            semantic: RirParamSemantic::SharedBorrow,
            abi: RirParamAbi::SharedBorrow,
            escape: RirParamEscape::NonEscaping,
        },
    );
    ext.abi.params.insert(0, anvyx_runtime::ExternTypeExpr::Int);

    assert_rir_error(program, RirVerifyErrorKind::UnsupportedRValueType);
}

#[test]
fn native_escaping_lambda_coexists_with_scoped_lambda_call() {
    let mut program = native_scoped_lambda_air_with(NativeLambdaArgKind::FunctionRef);
    let lambda_ty = program.externs[0].params[0].ty;
    let int = program.function(FunctionId::from_index(0)).signature.params[0].ty;
    let subscribe = host_subscription_extern(
        &mut program,
        "subscribe",
        vec![(lambda_ty, ParamMode::Value)],
        int,
    );
    set_extern_param_escape(&mut program, subscribe, 0, ParamEscape::Escaping);
    let entry = program.entry.expect("entry function");
    program
        .function_mut(entry)
        .body
        .block
        .stmts
        .push(Statement::Eval(RValue::Call {
            callee: Callee::Extern(subscribe),
            args: vec![CallArg::Value(Operand::Place(place(
                air::LocalId::from_index(0),
                lambda_ty,
            )))],
        }));
    let verified = air::verify(&program).expect("AIR verify failed");
    let plan = plan(&verified, host_scoped_and_subscription_plan_config()).expect("plan failed");
    let call_args = plan
        .program()
        .functions
        .iter()
        .flat_map(|function| &function.body.stmts)
        .filter_map(|stmt| match stmt {
            RirStmt::Eval(RirRValue::Call { args, .. }) => Some(args.as_slice()),
            _ => None,
        })
        .collect::<Vec<_>>();

    assert!(
        call_args
            .iter()
            .any(|args| matches!(args, [RirCallArg::ScopedLambda { .. }]))
    );
    assert!(
        call_args
            .iter()
            .any(|args| matches!(args, [RirCallArg::EscapingLambda { .. }]))
    );
}

#[test]
fn emit_retained_callback_registry_before_heap() {
    let program = native_escaping_lambda_trigger_air(NativeLambdaArgKind::FunctionRef);
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
fn native_escaping_lambda_stored_tuple_origin_without_air_proof_is_rejected() {
    let program = native_escaping_lambda_tuple_air(NativeLambdaArgKind::EscapingCaptureCell);
    let errors = air::verify(&program).expect_err("AIR accepted projected stored callback origin");

    assert!(format!("{errors:?}").contains("ArgEscapeUnknown"));
}

#[test]
fn profile_rejects_native_lambda_return() {
    let mut program = native_scoped_lambda_air();
    let lambda_ty = program.externs[0].params[0].ty;
    program.externs[0].return_type = lambda_ty;
    let verified = air::verify(&program).expect("AIR verify failed");
    let errors = RustBackendProfile::check_with_native_support(
        &verified,
        &host_lambda_plan_config().native_providers,
    )
    .expect_err("profile accepted native lambda return");

    assert!(has_error(
        &errors,
        ProfileErrorKind::UnsupportedLambdaExternBoundary
    ));
}

#[test]
fn analysis_detects_nested_list_indexed_write() {
    let program = nested_index_write_program(RirType::List(RirTypeId::from_index(1)));
    assert!(super::analysis::fallible_functions(&program)[0]);
}

#[test]
fn analysis_detects_nested_slice_indexed_write() {
    let program = nested_index_write_program(RirType::Slice(RirTypeId::from_index(1)));
    assert!(super::analysis::fallible_functions(&program)[0]);
}

#[test]
fn analysis_detects_nested_array_indexed_write() {
    let program = nested_index_write_program(RirType::Array {
        elem: RirTypeId::from_index(1),
        len: 4,
    });
    assert!(super::analysis::fallible_functions(&program)[0]);
}

#[test]
fn analysis_detects_tuple_before_index_write() {
    let program = nested_tuple_index_write_program();
    assert!(super::analysis::fallible_functions(&program)[0]);
}

#[test]
fn analysis_ignores_index_projection_on_map_value() {
    let program = nested_index_write_program(RirType::Map {
        key: RirTypeId::from_index(1),
        value: RirTypeId::from_index(1),
    });
    assert!(!super::analysis::fallible_functions(&program)[0]);
}

#[test]
fn analysis_treats_visible_native_result_as_value() {
    let int = RirTypeId::from_index(0);
    let string = RirTypeId::from_index(1);
    let result = RirTypeId::from_index(2);
    let mut program = native_extern_rir(
        vec![
            RirType::Int,
            RirType::String,
            RirType::Enum(RirEnumId::from_index(0)),
        ],
        vec![],
        result,
        direct_rust_abi(
            vec![],
            anvyx_runtime::RustReturnAbi::Result(
                Box::new(anvyx_runtime::RustReturnAbi::Value(
                    anvyx_runtime::ExternTypeExpr::Int,
                )),
                Box::new(anvyx_runtime::RustReturnAbi::Value(
                    anvyx_runtime::ExternTypeExpr::String,
                )),
            ),
        ),
        rir_abi(
            vec![],
            anvyx_runtime::ExternTypeExpr::Result(
                Box::new(anvyx_runtime::ExternTypeExpr::Int),
                Box::new(anvyx_runtime::ExternTypeExpr::String),
            ),
        ),
    );
    program.enums.push(rir_result_enum(int, string));
    program.functions.push(return_extern_function(result));

    assert!(!super::analysis::fallible_functions(&program)[0]);
}

#[test]
fn analysis_marks_native_slice_arg_preparation_nonfallible() {
    let int = RirTypeId::from_index(1);
    let int_slice =
        anvyx_runtime::ExternTypeExpr::Slice(Box::new(anvyx_runtime::ExternTypeExpr::Int));

    let slice_program = native_arg_analysis_program(
        RirType::Slice(int),
        anvyx_runtime::RustParamAbi::Slice(Box::new(anvyx_runtime::RustParamAbi::Value(
            anvyx_runtime::ExternTypeExpr::Int,
        ))),
        int_slice,
    );
    assert!(!super::analysis::fallible_functions(&slice_program)[0]);
}

fn native_arg_analysis_program(
    arg_ty: RirType,
    rust_abi: anvyx_runtime::RustParamAbi,
    abi: anvyx_runtime::ExternTypeExpr,
) -> RirProgram {
    let void = RirTypeId::from_index(0);
    let arg = RirTypeId::from_index(2);
    let mut program = native_extern_rir(
        vec![RirType::Void, RirType::Int, arg_ty],
        vec![RirExternParam {
            ty: arg,
            semantic: RirParamSemantic::Value,
            abi: RirParamAbi::Value,
            escape: RirParamEscape::NonEscaping,
        }],
        void,
        anvyx_runtime::RustExternAbi {
            params: vec![rust_abi],
            ret: anvyx_runtime::RustReturnAbi::Void,
            fallible: false,
            support: anvyx_runtime::RustAbiSupport::Direct,
            ctx: anvyx_runtime::RustWrapperCtx::HiddenRuntime,
        },
        rir_abi(vec![abi], anvyx_runtime::ExternTypeExpr::Void),
    );
    program.functions.push(call_extern_function(
        void,
        vec![RirCallArg::Value(RirOperand::Place(RirPlace::local(
            RirLocalId::from_index(0),
            vec![],
            arg,
        )))],
    ));
    program.functions[0].params.push(RirParam {
        local: RirLocalId::from_index(0),
        ty: arg,
        semantic: RirParamSemantic::Value,
        abi: RirParamAbi::Value,
        escape: RirParamEscape::NonEscaping,
    });
    program.functions[0].locals.push(RirLocal {
        id: RirLocalId::from_index(0),
        ty: arg,
        mutable: false,
        symbol: RirSymbol::new("v0"),
        initialized: true,
        payload_ref: false,
    });
    program
}

fn nested_index_write_program(indexed_ty: RirType) -> RirProgram {
    let void = RirTypeId::from_index(0);
    let int = RirTypeId::from_index(1);
    let indexed = RirTypeId::from_index(2);
    let root_ty = RirTypeId::from_index(3);
    let strukt = RirStructId::from_index(0);
    let field = RirFieldId::from_index(0);
    let root = RirLocalId::from_index(0);
    let index = RirLocalId::from_index(1);
    let replacement = RirConstId::from_index(0);
    RirProgram {
        types: vec![
            RirType::Void,
            RirType::Int,
            indexed_ty,
            RirType::Struct(strukt),
        ],
        structs: vec![RirStruct {
            id: strukt,
            air_id: None,
            symbol: RirSymbol::new("Box"),
            display: RirSymbol::new("Box"),
            native_path: None,
            native_ref: false,
            native_key: None,
            copyable: false,
            fields: vec![RirField {
                id: field,
                symbol: RirSymbol::new("xs"),
                ty: indexed,
            }],
        }],
        consts: vec![RirConst {
            id: replacement,
            ty: int,
            value: RirConstValue::Int(1),
        }],
        functions: vec![rir_function(
            RirFunctionId::from_index(0),
            void,
            vec![],
            vec![
                rir_local(root, root_ty, true, "box"),
                rir_local(index, int, false, "index"),
            ],
            vec![RirStmt::Assign {
                dst: RirPlace::local(
                    root,
                    vec![RirProjection::Field(field), RirProjection::Index(index)],
                    int,
                ),
                value: RirRValue::Use(RirOperand::Const(replacement)),
            }],
        )],
        ..RirProgram::default()
    }
}

fn nested_tuple_index_write_program() -> RirProgram {
    let void = RirTypeId::from_index(0);
    let int = RirTypeId::from_index(1);
    let list = RirTypeId::from_index(2);
    let tuple_ty = RirTypeId::from_index(3);
    let tuple = RirTupleId::from_index(0);
    let field = RirFieldId::from_index(0);
    let root = RirLocalId::from_index(0);
    let index = RirLocalId::from_index(1);
    let replacement = RirConstId::from_index(0);
    RirProgram {
        types: vec![
            RirType::Void,
            RirType::Int,
            RirType::List(int),
            RirType::Tuple(tuple),
        ],
        tuples: vec![RirTuple {
            id: tuple,
            symbol: RirSymbol::new("Pair"),
            display: RirSymbol::new("Pair"),
            copyable: false,
            fields: vec![RirField {
                id: field,
                symbol: RirSymbol::new("_0"),
                ty: list,
            }],
        }],
        consts: vec![RirConst {
            id: replacement,
            ty: int,
            value: RirConstValue::Int(1),
        }],
        functions: vec![rir_function(
            RirFunctionId::from_index(0),
            void,
            vec![],
            vec![
                rir_local(root, tuple_ty, true, "pair"),
                rir_local(index, int, false, "index"),
            ],
            vec![RirStmt::Assign {
                dst: RirPlace::local(
                    root,
                    vec![
                        RirProjection::TupleField(field),
                        RirProjection::Index(index),
                    ],
                    int,
                ),
                value: RirRValue::Use(RirOperand::Const(replacement)),
            }],
        )],
        ..RirProgram::default()
    }
}

#[test]
fn plan_lowers_local_collection_loan_to_direct_descriptor() {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let list = program.alloc_type(TypeData::List(int));
    let void = program.alloc_type(TypeData::Void);
    let module = program.alloc_module(root_module());
    let xs = air::LocalId::from_index(0);
    let function = program.alloc_function(Function {
        name: Ident::new("main"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![param("xs", list, ParamMode::Value, xs)], void),
        locals: vec![local(list, LocalKind::Arg)],
        body: structured_body(
            vec![collection_loan_stmt(
                place(xs, list),
                air::AirCollectionRootKind::List,
            )],
            air::AirTail::Return(None),
        ),
    });
    program.module_mut(module).functions.push(function);

    let root = planned_collection_loan_root(program, function);
    assert!(matches!(
        root,
        RirCollectionAccess::Direct(RirPlace {
            root: RirPlaceRoot::Local(local),
            projections,
            ..
        }) if local == RirLocalId::from_index(0) && projections.is_empty()
    ));
}

#[test]
fn plan_lowers_source_mut_param_collection_loan_to_mut_place_param() {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let list = program.alloc_type(TypeData::List(int));
    let void = program.alloc_type(TypeData::Void);
    let module = program.alloc_module(root_module());
    let xs = air::LocalId::from_index(0);
    let function = program.alloc_function(Function {
        name: Ident::new("update"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![param("xs", list, ParamMode::MutBorrow, xs)], void),
        locals: vec![mut_local(list, LocalKind::Arg)],
        body: structured_body(
            vec![collection_loan_stmt(
                place(xs, list),
                air::AirCollectionRootKind::List,
            )],
            air::AirTail::Return(None),
        ),
    });
    program.module_mut(module).functions.push(function);

    let root = planned_collection_loan_root(program, function);
    assert!(matches!(
        root,
        RirCollectionAccess::MutPlace(RirMutPlaceArg {
            access: RirMutPlaceAccess::Handle(RirMutPlaceHandle::Param { local, .. }),
            ..
        }) if local == RirLocalId::from_index(0)
    ));
}

#[test]
fn plan_lowers_owner_capture_cell_collection_loan_to_stack_cell() {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let list = program.alloc_type(TypeData::List(int));
    let void = program.alloc_type(TypeData::Void);
    let module = program.alloc_module(root_module());
    let binding = BindingId::from_index(0);
    let source = air::LocalId::from_index(0);
    let owner = FunctionId::from_index(0);
    let cell = capture_cell(&mut program, owner, source, binding, list);
    let init = init_collection_cell(&mut program, cell, list, int);
    let function = program.alloc_function(Function {
        name: Ident::new("main"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], void),
        locals: vec![bound_source_local(binding, list)],
        body: structured_body(
            vec![
                init,
                collection_loan_stmt(
                    root_place(PlaceRoot::CaptureCell(cell), list),
                    air::AirCollectionRootKind::List,
                ),
            ],
            air::AirTail::Return(None),
        ),
    });
    assert_eq!(function, owner);
    program.module_mut(module).functions.push(function);

    let root = planned_collection_loan_root(program, function);
    assert!(matches!(
        root,
        RirCollectionAccess::MutPlace(RirMutPlaceArg {
            access: RirMutPlaceAccess::Handle(RirMutPlaceHandle::StackCell { cell: RirCellRef::Owner(id), .. }),
            ..
        }) if id == RirCellId::from_index(0)
    ));
}

#[test]
fn plan_lowers_lambda_capture_cell_collection_loan_to_heap_cell() {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let map = program.alloc_type(TypeData::Map {
        key: int,
        value: int,
        order: air::MapOrder::Insertion,
    });
    let void = program.alloc_type(TypeData::Void);
    let sig = air::SignatureType::new(vec![], air::ReturnMode::Value(void));
    let lambda_ty = program.alloc_type(TypeData::Function(sig.clone()));
    let module = program.alloc_module(root_module());
    let binding = BindingId::from_index(0);
    let source = air::LocalId::from_index(0);
    let lambda_local = air::LocalId::from_index(1);
    let body = FunctionId::from_index(0);
    let owner = FunctionId::from_index(1);
    let lambda = air::LambdaId::from_index(0);
    let cell = capture_cell(&mut program, owner, source, binding, map);
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
                ty: map,
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
            vec![collection_loan_stmt(
                root_place(
                    PlaceRoot::LambdaCapture(air::LambdaCaptureSlotId::from_index(0)),
                    map,
                ),
                air::AirCollectionRootKind::Map,
            )],
            air::AirTail::Return(None),
        ),
    });
    assert_eq!(lambda_body, body);
    let init = init_collection_cell(&mut program, cell, map, int);
    let owner_fn = program.alloc_function(Function {
        name: Ident::new("main"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], void),
        locals: vec![
            bound_source_local(binding, map),
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
            ],
            air::AirTail::Return(None),
        ),
    });
    assert_eq!(owner_fn, owner);
    program
        .module_mut(module)
        .functions
        .extend([lambda_body, owner_fn]);

    let root = planned_collection_loan_root(program, body);
    assert!(matches!(
        root,
        RirCollectionAccess::MutPlace(RirMutPlaceArg {
            access: RirMutPlaceAccess::Handle(RirMutPlaceHandle::HeapCell { cell: RirCellRef::Capture { cell, .. }, .. }),
            ..
        }) if cell == RirCellId::from_index(0)
    ));
}

#[test]
fn plan_lowers_scoped_collection_loan_to_scoped_place_cell() {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let list = program.alloc_type(TypeData::List(int));
    let void = program.alloc_type(TypeData::Void);
    let module = program.alloc_module(root_module());
    let binding = BindingId::from_index(0);
    let source = air::LocalId::from_index(0);
    let function = FunctionId::from_index(0);
    let borrow = scoped_borrow(&mut program, function, source, binding, list);
    let function = program.alloc_function(Function {
        name: Ident::new("update"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![param("xs", list, ParamMode::MutBorrow, source)], void),
        locals: vec![bound_arg_source_local(binding, list)],
        body: structured_body(
            vec![collection_loan_stmt(
                root_place(PlaceRoot::ScopedBorrow(borrow), list),
                air::AirCollectionRootKind::List,
            )],
            air::AirTail::Return(None),
        ),
    });
    program.module_mut(module).functions.push(function);

    let root = planned_collection_loan_root(program, function);
    assert!(matches!(
        root,
        RirCollectionAccess::MutPlace(RirMutPlaceArg {
            access: RirMutPlaceAccess::Handle(RirMutPlaceHandle::ScopedPlaceCell { cell: RirScopedPlaceCellRef::Owner(id), .. }),
            ..
        }) if id == RirScopedPlaceCellId::from_index(0)
    ));
}

#[test]
fn plan_lowers_list_index_collection_access_to_mut_place() {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let value = program.alloc_type(TypeData::Optional(int));
    let map = program.alloc_type(TypeData::Map {
        key: int,
        value: int,
        order: air::MapOrder::Insertion,
    });
    let list = program.alloc_type(TypeData::List(map));
    let void = program.alloc_type(TypeData::Void);
    let module = program.alloc_module(root_module());
    let index_const = int_const(&mut program, int, 0);
    let key_const = int_const(&mut program, int, 1);
    let value_const = int_const(&mut program, int, 2);
    let maps = air::LocalId::from_index(0);
    let index = air::LocalId::from_index(1);
    let slot = air::LocalId::from_index(2);
    let map_place = Place {
        root: PlaceRoot::Local(maps),
        projection: vec![Projection::Index(index)],
        ty: map,
    };
    let function = program.alloc_function(Function {
        name: Ident::new("main"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![param("maps", list, ParamMode::Value, maps)], void),
        locals: vec![
            mut_local(list, LocalKind::Arg),
            local(int, LocalKind::Temp),
            local(value, LocalKind::Temp),
        ],
        body: structured_body(
            vec![
                Statement::Init {
                    local: index,
                    value: RValue::Use(Operand::Const(index_const)),
                },
                Statement::Init {
                    local: slot,
                    value: RValue::MapGet {
                        map: map_place.clone(),
                        key: Operand::Const(key_const),
                        ty: value,
                    },
                },
                Statement::Eval(RValue::MapInsert {
                    map: map_place,
                    key: Operand::Const(key_const),
                    value: Operand::Const(value_const),
                    kind: air::MapWriteKind::IndexedAssignment,
                }),
            ],
            air::AirTail::Return(None),
        ),
    });
    program.module_mut(module).functions.push(function);

    let verified = air::verify(&program).expect("AIR verify failed");
    let plan = plan(&verified, rust_plan_config()).expect("plan failed");
    let function = rir_function_for_air(plan.program(), function);
    let accesses = function
        .body
        .stmts
        .iter()
        .filter_map(|stmt| match stmt {
            RirStmt::Init {
                value: RirRValue::MapGet { map, .. },
                ..
            }
            | RirStmt::Eval(RirRValue::MapInsert { map, .. }) => Some(map),
            _ => None,
        })
        .collect::<Vec<_>>();
    assert_eq!(accesses.len(), 2);
    assert!(accesses.iter().all(|access| matches!(
        access,
        RirCollectionAccess::MutPlace(RirMutPlaceArg {
            access: RirMutPlaceAccess::Handle(RirMutPlaceHandle::Local { local, .. }),
            projections,
            ..
        }) if *local == RirLocalId::from_index(0) && !projections.is_empty()
    )));
}

#[test]
fn indexed_map_assignment_preserves_unsupported_lambda_capture_gap() {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let map = program.alloc_type(TypeData::Map {
        key: int,
        value: int,
        order: air::MapOrder::Insertion,
    });
    let void = program.alloc_type(TypeData::Void);
    let sig = air::SignatureType::new(vec![], air::ReturnMode::Value(void));
    let module = program.alloc_module(root_module());
    let owner = FunctionId::from_index(1);
    let source = air::LocalId::from_index(0);
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
        signature: sig,
        escape: LambdaEscape::NonEscaping,
        captures: vec![air::LambdaCaptureDecl::ScopedLocal {
            binding: BindingId::from_index(0),
            source: CaptureLocalSource {
                owner,
                local: source,
            },
            ty: map,
            mutability: Mutability::Mutable,
        }],
    });
    program.function_mut(body).kind = FunctionKind::Lambda(lambda);
    let owner_fn = program.alloc_function(Function {
        name: Ident::new("main"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], void),
        locals: vec![bound_source_local(BindingId::from_index(0), map)],
        body: structured_body(vec![], air::AirTail::Return(None)),
    });
    assert_eq!(owner_fn, owner);
    program
        .module_mut(module)
        .functions
        .extend([body, owner_fn]);

    let verified = air::verify(&program).expect("AIR verify failed");
    let mut cx = PlanCx::new(&verified, RustPlanConfig::default());
    cx.plan_types(&mut RirProgram::default())
        .expect("type plan failed");
    let mut locals = vec![];
    let map = root_place(
        PlaceRoot::LambdaCapture(air::LambdaCaptureSlotId::from_index(0)),
        map,
    );

    let Err(RustPlanError::TargetGaps(gaps)) = cx.plan_collection_access(
        body,
        &map,
        CollectionAccessOp::IndexedMapAssign,
        &mut locals,
    ) else {
        panic!("expected indexed map assignment gap");
    };
    assert!(
        gaps.iter()
            .any(|gap| gap.kind == RustTargetGapKind::UnsupportedLambdaCapture)
    );
}

#[test]
fn profile_accepts_dataref_list_payload() {
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

    check(program);
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
    let slice = program.alloc_type(TypeData::Slice(int));
    let tuple = program.alloc_type(TypeData::Tuple(vec![slice, int]));
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

    assert!(source.contains("anvyx_runtime::Trace"));
    assert!(source.contains("struct anvT2_Payload"));
    assert!(source.contains("struct anvT1_Tuple"));
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
fn profile_accepts_dataref_map_index_assignment() {
    check(dataref_map_index_assignment_program());
}

#[test]
fn rir_verifies_valid_global_declaration_and_operations() {
    let program = valid_global_rir(|_| {});

    rir::verify(&program).expect("valid global RIR failed verification");
}

#[test]
fn rir_rejects_bad_global_id() {
    assert_rir_error(
        valid_global_rir(|program| program.globals[0].id = RirGlobalId::from_index(1)),
        RirVerifyErrorKind::BadId,
    );
}

#[test]
fn rir_rejects_bad_global_init_id() {
    assert_rir_error(
        valid_global_rir(|program| program.globals[0].init = RirFunctionId::from_index(99)),
        RirVerifyErrorKind::BadId,
    );
}

#[test]
fn rir_rejects_global_init_return_mismatch() {
    assert_rir_type_error(valid_global_rir(|program| {
        program.functions[0].ret.ty = RirTypeId::from_index(1);
    }));
}

#[test]
fn rir_rejects_global_init_params() {
    let local = RirLocalId::from_index(0);
    assert_rir_error(
        valid_global_rir(|program| {
            program.functions[0].params = vec![rir_param(
                local,
                RirTypeId::from_index(0),
                RirParamSemantic::Value,
                RirParamAbi::Value,
            )];
            program.functions[0].locals =
                vec![rir_local(local, RirTypeId::from_index(0), false, "x")];
        }),
        RirVerifyErrorKind::CallArgCount {
            expected: 0,
            found: 1,
        },
    );
}

#[test]
fn rir_rejects_invalid_global_references() {
    assert_rir_error(
        valid_global_rir(|program| {
            program.functions[1].body.stmts = vec![RirStmt::GlobalEnsure {
                global: RirGlobalId::from_index(99),
            }];
        }),
        RirVerifyErrorKind::BadId,
    );
    assert_rir_error(
        valid_global_rir(|program| {
            program.functions[1].body.stmts = vec![RirStmt::GlobalSetRoot {
                global: RirGlobalId::from_index(99),
                value: RirRValue::Use(RirOperand::Const(RirConstId::from_index(0))),
            }];
        }),
        RirVerifyErrorKind::BadId,
    );
    assert_rir_error(
        valid_global_rir(|program| {
            program.functions[1].ret.ty = RirTypeId::from_index(0);
            program.functions[1].body.term =
                RirTerm::Return(Some(RirOperand::Place(RirPlace::global(
                    RirGlobalId::from_index(99),
                    vec![],
                    RirTypeId::from_index(0),
                ))));
        }),
        RirVerifyErrorKind::BadId,
    );
}

#[test]
fn rir_rejects_invalid_global_payload_type_without_panicking() {
    assert_rir_error(
        valid_global_rir(|program| program.globals[0].ty = RirTypeId::from_index(99)),
        RirVerifyErrorKind::BadId,
    );
}

#[test]
fn rir_rejects_empty_global_symbols() {
    assert_rir_error(
        valid_global_rir(|program| program.globals[0].slot_symbol = RirSymbol::new("")),
        RirVerifyErrorKind::BadId,
    );
    assert_rir_error(
        valid_global_rir(|program| program.globals[0].name = RirSymbol::new("")),
        RirVerifyErrorKind::BadId,
    );
}

#[test]
fn rir_rejects_ordinary_global_assignment() {
    assert_rir_error(
        valid_global_rir(|program| {
            program.functions[1].body.stmts.push(RirStmt::Assign {
                dst: rir_global_place(RirTypeId::from_index(0)),
                value: RirRValue::Use(RirOperand::Const(RirConstId::from_index(0))),
            });
        }),
        RirVerifyErrorKind::UnsupportedRValueType,
    );
}

#[test]
fn rir_rejects_immutable_global_root_set() {
    assert_rir_error(
        valid_global_rir(|program| program.globals[0].mutable = false),
        RirVerifyErrorKind::ImmutableAssign,
    );
}

#[test]
fn rir_accepts_global_root_update_for_supported_payloads() {
    rir::verify(&valid_global_rir(|program| {
        program.functions[1].body.stmts = vec![
            RirStmt::GlobalEnsure {
                global: RirGlobalId::from_index(0),
            },
            RirStmt::GlobalUpdateRoot {
                global: RirGlobalId::from_index(0),
                value: RirRValue::Use(RirOperand::Const(RirConstId::from_index(0))),
            },
        ];
    }))
    .expect("scalar update should verify");

    rir::verify(&valid_global_rir(|program| {
        let (_, value) = set_global_string_payload(program);
        program.functions[1].body.stmts = vec![
            RirStmt::GlobalEnsure {
                global: RirGlobalId::from_index(0),
            },
            RirStmt::GlobalUpdateRoot {
                global: RirGlobalId::from_index(0),
                value: RirRValue::Use(RirOperand::Const(value)),
            },
        ];
    }))
    .expect("string update should verify");
}

#[test]
fn rir_rejects_malformed_global_root_update() {
    assert_rir_error(
        valid_global_rir(|program| {
            program.functions[1].body.stmts = vec![RirStmt::GlobalUpdateRoot {
                global: RirGlobalId::from_index(0),
                value: RirRValue::Use(RirOperand::Const(RirConstId::from_index(0))),
            }];
        }),
        RirVerifyErrorKind::UninitializedGlobal(RirGlobalId::from_index(0)),
    );
    assert_rir_error(
        valid_global_rir(|program| {
            program.functions[1].body.stmts = vec![RirStmt::GlobalUpdateRoot {
                global: RirGlobalId::from_index(99),
                value: RirRValue::Use(RirOperand::Const(RirConstId::from_index(0))),
            }];
        }),
        RirVerifyErrorKind::BadId,
    );
    assert_rir_error(
        valid_global_rir(|program| {
            program.globals[0].mutable = false;
            program.functions[1].body.stmts = vec![RirStmt::GlobalUpdateRoot {
                global: RirGlobalId::from_index(0),
                value: RirRValue::Use(RirOperand::Const(RirConstId::from_index(0))),
            }];
        }),
        RirVerifyErrorKind::ImmutableAssign,
    );
    assert_rir_error(
        valid_global_rir(|program| {
            program.globals[0].ty = RirTypeId::from_index(1);
            program.functions[1].body.stmts = vec![RirStmt::GlobalUpdateRoot {
                global: RirGlobalId::from_index(0),
                value: RirRValue::Use(RirOperand::Const(RirConstId::from_index(0))),
            }];
        }),
        RirVerifyErrorKind::UnsupportedRValueType,
    );
    assert_rir_error(
        valid_global_rir(|program| {
            let bool_ty = RirTypeId::from_index(2);
            let bool_const = RirConstId::from_index(1);
            program.types.push(RirType::Bool);
            program.consts.push(RirConst {
                id: bool_const,
                ty: bool_ty,
                value: RirConstValue::Bool(true),
            });
            program.functions[1].body.stmts = vec![RirStmt::GlobalUpdateRoot {
                global: RirGlobalId::from_index(0),
                value: RirRValue::Use(RirOperand::Const(bool_const)),
            }];
        }),
        RirVerifyErrorKind::TypeMismatch {
            expected: RirTypeId::from_index(0),
            found: RirTypeId::from_index(2),
        },
    );
}

#[test]
fn rir_accepts_global_mut_place_root() {
    rir::verify(&valid_global_rir(|program| {
        let callee = add_global_mut_place_callee(program);
        program.functions[1].body.stmts = vec![
            RirStmt::GlobalEnsure {
                global: RirGlobalId::from_index(0),
            },
            global_mut_place_call(callee, RirGlobalId::from_index(0), RirTypeId::from_index(0)),
        ];
    }))
    .expect("global mut place root should verify");
}

#[test]
fn rir_rejects_malformed_global_mut_place_root() {
    assert_rir_error(
        valid_global_rir(|program| {
            let callee = add_global_mut_place_callee(program);
            program.functions[1].body.stmts = vec![global_mut_place_call(
                callee,
                RirGlobalId::from_index(0),
                RirTypeId::from_index(0),
            )];
        }),
        RirVerifyErrorKind::UninitializedGlobal(RirGlobalId::from_index(0)),
    );
    assert_rir_error(
        valid_global_rir(|program| {
            let callee = add_global_mut_place_callee(program);
            program.globals[0].mutable = false;
            program.functions[1].body.stmts = vec![global_mut_place_call(
                callee,
                RirGlobalId::from_index(0),
                RirTypeId::from_index(0),
            )];
        }),
        RirVerifyErrorKind::ImmutableAssign,
    );
    assert_rir_error(
        valid_global_rir(|program| {
            let callee = add_global_mut_place_callee(program);
            program.functions[1].body.stmts = vec![global_mut_place_call(
                callee,
                RirGlobalId::from_index(99),
                RirTypeId::from_index(0),
            )];
        }),
        RirVerifyErrorKind::BadId,
    );
    assert_rir_error(
        valid_global_rir(|program| {
            let callee = add_global_mut_place_callee(program);
            program.functions[1].body.stmts = vec![global_mut_place_call(
                callee,
                RirGlobalId::from_index(0),
                RirTypeId::from_index(1),
            )];
        }),
        RirVerifyErrorKind::TypeMismatch {
            expected: RirTypeId::from_index(0),
            found: RirTypeId::from_index(1),
        },
    );
}

#[test]
fn rir_rejects_duplicate_global_slot_symbol() {
    assert_rir_error(
        valid_global_rir(|program| {
            let mut second = program.globals[0].clone();
            second.id = RirGlobalId::from_index(1);
            program.globals.push(second);
        }),
        RirVerifyErrorKind::DuplicateSymbol,
    );
}

#[test]
fn rir_rejects_global_projection() {
    assert_rir_error(
        valid_global_rir(|program| {
            program.functions[1].ret.ty = RirTypeId::from_index(0);
            program.functions[1].body.term =
                RirTerm::Return(Some(RirOperand::Place(RirPlace::global(
                    RirGlobalId::from_index(0),
                    vec![RirProjection::TupleField(RirFieldId::from_index(0))],
                    RirTypeId::from_index(0),
                ))));
        }),
        RirVerifyErrorKind::UnsupportedRValueType,
    );
}

#[test]
fn rir_accepts_global_borrow_call_arg_after_ensure() {
    rir::verify(&valid_global_rir(|program| {
        let (string, callee) = add_string_global_borrow_callee(program);
        program.functions[1]
            .body
            .stmts
            .push(RirStmt::Eval(RirRValue::Call {
                callee: RirCallTarget::Function(callee),
                args: vec![RirCallArg::SharedBorrow(rir_global_place(string))],
                ty: RirTypeId::from_index(1),
            }));
    }))
    .expect("global borrow call arg should verify after ensure");
}

#[test]
fn rir_rejects_global_borrow_call_arg_without_ensure() {
    assert_rir_error(
        valid_global_rir(|program| {
            let (string, callee) = add_string_global_borrow_callee(program);
            program.functions[1].body.stmts = vec![RirStmt::Eval(RirRValue::Call {
                callee: RirCallTarget::Function(callee),
                args: vec![RirCallArg::SharedBorrow(rir_global_place(string))],
                ty: RirTypeId::from_index(1),
            })];
        }),
        RirVerifyErrorKind::UninitializedGlobal(RirGlobalId::from_index(0)),
    );
}

#[test]
fn rir_rejects_global_root_type_mismatch() {
    assert_rir_type_error(valid_global_rir(|program| {
        program.functions[1].ret.ty = RirTypeId::from_index(0);
        program.functions[1].body.term = RirTerm::Return(Some(RirOperand::Place(
            rir_global_place(RirTypeId::from_index(1)),
        )));
    }));
}

#[test]
fn rir_rejects_void_global_payload() {
    assert_rir_error(
        valid_global_rir(|program| program.globals[0].ty = RirTypeId::from_index(1)),
        RirVerifyErrorKind::UnsupportedRValueType,
    );
}

#[test]
fn rir_accepts_exact_root_non_scalar_global_payload() {
    rir::verify(&valid_global_rir(|program| {
        let (string, _) = set_global_string_payload(program);
        program.functions[1].body.term =
            RirTerm::Return(Some(RirOperand::Place(rir_global_place(string))));
        program.functions[1].ret.ty = string;
    }))
    .expect("verified exact-root non-scalar global RIR");
}

#[test]
fn rir_rejects_unsupported_global_payloads() {
    assert_rir_error(
        valid_global_rir(|program| program.globals[0].ty = RirTypeId::from_index(1)),
        RirVerifyErrorKind::UnsupportedRValueType,
    );
    assert_rir_error(
        valid_global_rir(|program| {
            let slice = RirTypeId::from_index(program.types.len());
            program.types.push(RirType::Slice(RirTypeId::from_index(0)));
            program.globals[0].ty = slice;
        }),
        RirVerifyErrorKind::UnsupportedRValueType,
    );
    assert_rir_error(
        valid_global_rir(|program| {
            let tuple = RirTypeId::from_index(program.types.len());
            program
                .types
                .push(RirType::Tuple(RirTupleId::from_index(0)));
            let map = RirTypeId::from_index(program.types.len());
            program.types.push(RirType::Map {
                key: tuple,
                value: RirTypeId::from_index(0),
            });
            program.globals[0].ty = map;
        }),
        RirVerifyErrorKind::UnsupportedRValueType,
    );
    assert_rir_error(
        valid_global_rir(|program| {
            let slice = RirTypeId::from_index(program.types.len());
            program.types.push(RirType::Slice(RirTypeId::from_index(0)));
            let maybe_slice = RirTypeId::from_index(program.types.len());
            program.types.push(RirType::Option(slice));
            program.globals[0].ty = maybe_slice;
        }),
        RirVerifyErrorKind::UnsupportedRValueType,
    );
}

#[test]
fn rir_rejects_global_collection_loan_root() {
    assert_rir_error(
        valid_global_rir(|program| {
            program.functions[1]
                .body
                .stmts
                .push(RirStmt::CollectionLoanScope(RirCollectionLoanScope {
                    root: RirCollectionAccess::Direct(rir_global_place(RirTypeId::from_index(0))),
                    root_kind: RirCollectionRootKind::List,
                    mode: RirCollectionLoanMode::MutableSequenceElement,
                    body: RirStructuredBlock {
                        stmts: vec![],
                        term: RirTerm::Return(None),
                    },
                }));
        }),
        RirVerifyErrorKind::UnsupportedRValueType,
    );
}

#[test]
fn emit_direct_rir_global_read_uses_named_context_params() {
    let program = valid_global_rir(|program| {
        let int = RirTypeId::from_index(0);
        let local = RirLocalId::from_index(0);
        program.functions[1].ret.ty = int;
        program.functions[1].locals = vec![RirLocal {
            id: local,
            ty: int,
            mutable: false,
            symbol: RirSymbol::new("value"),
            initialized: false,
            payload_ref: false,
        }];
        program.functions[1].body = RirStructuredBlock {
            stmts: vec![RirStmt::Init {
                local,
                value: RirRValue::Use(RirOperand::Place(rir_global_place(int))),
            }],
            term: RirTerm::Return(Some(RirOperand::Place(rir_place(local, int)))),
        };
    });
    let verified = rir::verify(&program).expect("valid direct global read RIR");
    let source = emit::emit(&verified).into_string();

    assert!(source.contains(
        "fn main<'cx, 'rt>(rt: &mut anvyx_runtime::Ctx<'cx, 'rt>, types: &AnvTypes<'cx>, globals: &AnvGlobals<'cx>)"
    ));
}

#[test]
fn emit_global_call_arg_temps_read_before_runtime_call() {
    let program = valid_global_rir(|program| {
        let int = RirTypeId::from_index(0);
        let void = RirTypeId::from_index(1);
        program.externs.push(RirExtern {
            id: RirExternId::from_index(0),
            air_id: None,
            symbol: RirSymbol::new("sink"),
            kind: RirExternKind::Native(rir::RirNativeExtern::new(
                vec!["host".to_string(), "sink".to_string()],
                anvyx_runtime::RustExternAbi {
                    params: vec![anvyx_runtime::RustParamAbi::Value(
                        anvyx_runtime::ExternTypeExpr::Int,
                    )],
                    ret: anvyx_runtime::RustReturnAbi::Void,
                    fallible: false,
                    support: anvyx_runtime::RustAbiSupport::Direct,
                    ctx: anvyx_runtime::RustWrapperCtx::HiddenRuntime,
                },
            )),
            params: vec![RirExternParam {
                ty: int,
                semantic: RirParamSemantic::Value,
                abi: RirParamAbi::Value,
                escape: RirParamEscape::NonEscaping,
            }],
            ret: void,
            abi: rir_abi(
                vec![anvyx_runtime::ExternTypeExpr::Int],
                anvyx_runtime::ExternTypeExpr::Void,
            ),
        });
        program.functions[1].symbol = RirSymbol::new("entry");
        program.entry = Some(RirFunctionId::from_index(1));
        program.functions[1].body = RirStructuredBlock {
            stmts: vec![RirStmt::Eval(RirRValue::Call {
                callee: RirCallTarget::Extern(RirExternId::from_index(0)),
                args: vec![RirCallArg::Value(RirOperand::Place(rir_global_place(int)))],
                ty: void,
            })],
            term: RirTerm::Return(None),
        };
    });
    let verified = rir::verify(&program).expect("valid global call arg RIR");
    let source = emit::emit(&verified);
    let text = source.as_str();

    assert!(text.contains(
        "let __anv_arg_0 = { let __global = globals.g0_score.read(|| ginit0(rt, types, globals))?; *(&*__global) }; host::sink(rt, __anv_arg_0)"
    ), "{text}");

    let source = emit::RustSource::new(format!(
        "mod host {{ pub fn sink<'cx, 'rt>(_rt: &mut anvyx_runtime::Ctx<'cx, 'rt>, value: i64) {{ assert_eq!(value, 7); }} }}\n{}",
        source.into_string()
    ));
    let output = run_source(source);
    assert_eq!(output.status, SourceJobStatus::Success, "{}", output.stderr);
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
                    payload_ref: false,
                    payload_escapes: false,
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
fn profile_accepts_exact_root_global_payloads() {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let string = program.alloc_type(TypeData::String);
    let list = program.alloc_type(TypeData::List(string));
    let map = program.alloc_type(TypeData::Map {
        key: string,
        value: list,
        order: air::MapOrder::Insertion,
    });
    let module = program.alloc_module(root_module());
    let node_id = program.alloc_aggregate(air::AggregateDecl {
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
    let label_id = program.alloc_aggregate(air::AggregateDecl {
        name: Ident::new("Label"),
        module,
        kind: air::AggregateKind::Struct,
        type_args: vec![],
        const_args: vec![],
        fields: vec![FieldDecl {
            name: Ident::new("text"),
            ty: string,
        }],
        cycle_capable: false,
        stringify_override: None,
    });
    program
        .module_mut(module)
        .aggregates
        .extend([node_id, label_id]);
    let node = program.alloc_type(TypeData::DataRef(node_id));
    let label = program.alloc_type(TypeData::Aggregate(label_id));
    let tuple = program.alloc_type(TypeData::Tuple(vec![label, node]));
    let maybe_node = program.alloc_type(TypeData::Optional(node));
    let labels = program.alloc_type(TypeData::Array {
        elem: label,
        len: 2,
    });
    let label_list = program.alloc_type(TypeData::List(label));
    let label_map = program.alloc_type(TypeData::Map {
        key: string,
        value: label,
        order: air::MapOrder::Insertion,
    });
    let array_map = program.alloc_type(TypeData::Map {
        key: string,
        value: labels,
        order: air::MapOrder::Insertion,
    });

    for (name, ty) in [
        ("int", int),
        ("string", string),
        ("list", list),
        ("map", map),
        ("node", node),
        ("label", label),
        ("tuple", tuple),
        ("maybe_node", maybe_node),
        ("labels", labels),
        ("label_list", label_list),
        ("label_map", label_map),
        ("array_map", array_map),
    ] {
        global_with_init(&mut program, module, name, ty, Mutability::Mutable);
    }

    check(program);
}

#[test]
fn profile_rejects_unsupported_exact_root_global_payloads() {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let slice = program.alloc_type(TypeData::Slice(int));
    let any = program.alloc_type(TypeData::Any);
    let module = program.alloc_module(root_module());
    let ext_id = program.alloc_extern_type(ExternTypeDecl {
        name: Ident::new("HostValue"),
        module,
        binding: None,
        type_args: vec![],
        const_args: vec![],
        rep: ExternRep::Inline,
        layout: None,
        materialization: None,
        owns_heap_edges: None,
        has_init: false,
        init_args: vec![],
        fields: vec![],
        variants: vec![],
        variant_abis: vec![],
        methods: vec![],
        statics: vec![],
        operators: vec![],
    });
    program.module_mut(module).extern_types.push(ext_id);
    let ext = program.alloc_type(TypeData::Extern(ext_id));
    let list_slice = program.alloc_type(TypeData::List(slice));
    let map_slice = program.alloc_type(TypeData::Map {
        key: int,
        value: slice,
        order: air::MapOrder::Insertion,
    });

    let globals = [
        ("slice", slice, ProfileErrorKind::UnsupportedGlobalRooting),
        ("extern", ext, ProfileErrorKind::UnsupportedGlobalRooting),
        ("any", any, ProfileErrorKind::UnsupportedGlobalType),
        (
            "list_slice",
            list_slice,
            ProfileErrorKind::UnsupportedGlobalRooting,
        ),
        (
            "map_slice",
            map_slice,
            ProfileErrorKind::UnsupportedGlobalRooting,
        ),
    ];
    for (name, ty, _) in globals {
        global_with_init(&mut program, module, name, ty, Mutability::Mutable);
    }

    let errors = profile_errors(program);
    for (index, (_, _, kind)) in globals.into_iter().enumerate() {
        assert_profile_error(
            &errors,
            ProfileSite::Global(air::GlobalId::from_index(index)),
            kind,
        );
    }
}

#[test]
fn plan_emits_generated_dynamic_carrier() {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let one = program.alloc_const(ConstData {
        ty: int,
        value: ConstValue::Int(1),
    });
    let string = program.alloc_type(TypeData::String);
    let void = program.alloc_type(TypeData::Void);
    let callback = program.alloc_type(TypeData::Function(air::SignatureType::new(
        vec![],
        air::ReturnMode::Value(void),
    )));
    let large = program.alloc_type(TypeData::Tuple(vec![string, string, string, string]));
    let module = program.alloc_module(root_module());
    let receiver = air::LocalId::from_index(0);
    let mut draw_receiver = param("value", int, ParamMode::Value, receiver);
    draw_receiver.role = ParamRole::Receiver;
    let draw = program.alloc_function(Function {
        name: Ident::new("draw"),
        module,
        kind: FunctionKind::Method,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![draw_receiver], void),
        locals: vec![local(int, LocalKind::Arg)],
        body: structured_body(vec![], air::AirTail::Return(None)),
    });
    program.module_mut(module).functions.push(draw);
    let large_receiver = air::LocalId::from_index(0);
    let mut large_param = param("value", large, ParamMode::Value, large_receiver);
    large_param.role = ParamRole::Receiver;
    let draw_large = program.alloc_function(Function {
        name: Ident::new("draw"),
        module,
        kind: FunctionKind::Method,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![large_param], void),
        locals: vec![local(large, LocalKind::Arg)],
        body: structured_body(vec![], air::AirTail::Return(None)),
    });
    program.module_mut(module).functions.push(draw_large);
    let callback_receiver = air::LocalId::from_index(0);
    let mut callback_param = param("value", callback, ParamMode::Value, callback_receiver);
    callback_param.role = ParamRole::Receiver;
    let draw_callback = program.alloc_function(Function {
        name: Ident::new("draw"),
        module,
        kind: FunctionKind::Method,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![callback_param], void),
        locals: vec![local(callback, LocalKind::Arg)],
        body: structured_body(vec![], air::AirTail::Return(None)),
    });
    program.module_mut(module).functions.push(draw_callback);
    let second_receiver = air::LocalId::from_index(0);
    let mut second_param = param("value", int, ParamMode::Value, second_receiver);
    second_param.role = ParamRole::Receiver;
    let draw_second = program.alloc_function(Function {
        name: Ident::new("draw"),
        module,
        kind: FunctionKind::Method,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![second_param], void),
        locals: vec![local(int, LocalKind::Arg)],
        body: structured_body(vec![], air::AirTail::Return(None)),
    });
    program.module_mut(module).functions.push(draw_second);
    let other_receiver = air::LocalId::from_index(0);
    let mut other_param = param("value", int, ParamMode::Value, other_receiver);
    other_param.role = ParamRole::Receiver;
    let draw_other = program.alloc_function(Function {
        name: Ident::new("draw_other"),
        module,
        kind: FunctionKind::Method,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![other_param], void),
        locals: vec![local(int, LocalKind::Arg)],
        body: structured_body(vec![], air::AirTail::Return(None)),
    });
    program.module_mut(module).functions.push(draw_other);
    let slot = ContractSlotId::from_index(0);
    let surface = program.alloc_contract_surface(ContractSurfaceDecl {
        display_name: "Drawable".into(),
        slots: vec![ContractSlotDecl {
            id: slot,
            name: Ident::new("draw"),
            receiver: ContractReceiver::Value,
            params: vec![],
            ret: ContractReturnDecl::Value(void),
        }],
    });
    let dyn_ty = program.alloc_type(TypeData::Dyn(surface));
    program.alloc_contract_witness(ContractWitnessDecl {
        key: ContractWitnessKey {
            concrete_ty: int,
            surface,
            slots: vec![ContractWitnessSlotDecl {
                slot,
                receiver: ParamMode::Value,
                target: ContractWitnessTarget::Function { function: draw },
            }],
        },
    });
    program.alloc_contract_witness(ContractWitnessDecl {
        key: ContractWitnessKey {
            concrete_ty: large,
            surface,
            slots: vec![ContractWitnessSlotDecl {
                slot,
                receiver: ParamMode::Value,
                target: ContractWitnessTarget::Function {
                    function: draw_large,
                },
            }],
        },
    });
    program.alloc_contract_witness(ContractWitnessDecl {
        key: ContractWitnessKey {
            concrete_ty: callback,
            surface,
            slots: vec![ContractWitnessSlotDecl {
                slot,
                receiver: ParamMode::Value,
                target: ContractWitnessTarget::Function {
                    function: draw_callback,
                },
            }],
        },
    });
    program.alloc_contract_witness(ContractWitnessDecl {
        key: ContractWitnessKey {
            concrete_ty: int,
            surface,
            slots: vec![ContractWitnessSlotDecl {
                slot,
                receiver: ParamMode::Value,
                target: ContractWitnessTarget::Function {
                    function: draw_second,
                },
            }],
        },
    });
    let colliding_surface = program.alloc_contract_surface(ContractSurfaceDecl {
        display_name: "Drawable".into(),
        slots: vec![ContractSlotDecl {
            id: slot,
            name: Ident::new("draw_other"),
            receiver: ContractReceiver::Value,
            params: vec![],
            ret: ContractReturnDecl::Value(void),
        }],
    });
    program.alloc_type(TypeData::Dyn(colliding_surface));
    program.alloc_contract_witness(ContractWitnessDecl {
        key: ContractWitnessKey {
            concrete_ty: int,
            surface: colliding_surface,
            slots: vec![ContractWitnessSlotDecl {
                slot,
                receiver: ParamMode::Value,
                target: ContractWitnessTarget::Function {
                    function: draw_other,
                },
            }],
        },
    });
    let local_id = air::LocalId::from_index(0);
    let packed_id = air::LocalId::from_index(1);
    let function = program.alloc_function(Function {
        name: Ident::new("pack_dyn"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![param("value", int, ParamMode::Value, local_id)], void),
        locals: vec![local(int, LocalKind::Arg), local(dyn_ty, LocalKind::Temp)],
        body: structured_body(
            vec![Statement::Init {
                local: packed_id,
                value: RValue::DynPack {
                    value: Operand::Place(place(local_id, int)),
                    use_: air::DynOwnedUse::ReusableRead,
                    witness: air::ContractWitnessId::from_index(0),
                    ty: dyn_ty,
                },
            }],
            air::AirTail::Return(None),
        ),
    });
    program.module_mut(module).functions.push(function);
    let second_arg = air::LocalId::from_index(0);
    let second_packed = air::LocalId::from_index(1);
    let pack_second = program.alloc_function(Function {
        name: Ident::new("pack_second_dyn"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(
            vec![param("value", int, ParamMode::Value, second_arg)],
            void,
        ),
        locals: vec![local(int, LocalKind::Arg), local(dyn_ty, LocalKind::Temp)],
        body: structured_body(
            vec![Statement::Init {
                local: second_packed,
                value: RValue::DynPack {
                    value: Operand::Place(place(second_arg, int)),
                    use_: air::DynOwnedUse::ReusableRead,
                    witness: air::ContractWitnessId::from_index(3),
                    ty: dyn_ty,
                },
            }],
            air::AirTail::Return(None),
        ),
    });
    program.module_mut(module).functions.push(pack_second);
    let large_arg = air::LocalId::from_index(0);
    let large_packed = air::LocalId::from_index(1);
    let pack_large = program.alloc_function(Function {
        name: Ident::new("pack_large_dyn"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(
            vec![param("value", large, ParamMode::Value, large_arg)],
            void,
        ),
        locals: vec![local(large, LocalKind::Arg), local(dyn_ty, LocalKind::Temp)],
        body: structured_body(
            vec![Statement::Init {
                local: large_packed,
                value: RValue::DynPack {
                    value: Operand::Place(place(large_arg, large)),
                    use_: air::DynOwnedUse::ReusableRead,
                    witness: air::ContractWitnessId::from_index(1),
                    ty: dyn_ty,
                },
            }],
            air::AirTail::Return(None),
        ),
    });
    program.module_mut(module).functions.push(pack_large);
    let callback_arg = air::LocalId::from_index(0);
    let callback_packed = air::LocalId::from_index(1);
    let pack_callback = program.alloc_function(Function {
        name: Ident::new("pack_callback_dyn"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(
            vec![param("value", callback, ParamMode::Value, callback_arg)],
            void,
        ),
        locals: vec![
            local(callback, LocalKind::Arg),
            local(dyn_ty, LocalKind::Temp),
        ],
        body: structured_body(
            vec![Statement::Init {
                local: callback_packed,
                value: RValue::DynPack {
                    value: Operand::Place(place(callback_arg, callback)),
                    use_: air::DynOwnedUse::ReusableRead,
                    witness: air::ContractWitnessId::from_index(2),
                    ty: dyn_ty,
                },
            }],
            air::AirTail::Return(None),
        ),
    });
    program.module_mut(module).functions.push(pack_callback);
    let string_args = (0..4).map(air::LocalId::from_index).collect::<Vec<_>>();
    let tuple_tmp = air::LocalId::from_index(4);
    let moved_dyn = air::LocalId::from_index(5);
    let pack_moved = program.alloc_function(Function {
        name: Ident::new("pack_moved_dyn"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(
            string_args
                .iter()
                .enumerate()
                .map(|(index, local)| {
                    param(&format!("value{index}"), string, ParamMode::Value, *local)
                })
                .collect(),
            void,
        ),
        locals: string_args
            .iter()
            .map(|_| local(string, LocalKind::Arg))
            .chain([
                local(large, LocalKind::Temp),
                local(dyn_ty, LocalKind::Temp),
            ])
            .collect(),
        body: structured_body(
            vec![
                Statement::Init {
                    local: tuple_tmp,
                    value: RValue::Aggregate {
                        kind: AggregateCtor::Tuple,
                        fields: string_args
                            .iter()
                            .map(|local| Operand::Place(place(*local, string)))
                            .collect(),
                        ty: large,
                    },
                },
                Statement::Init {
                    local: moved_dyn,
                    value: RValue::DynPack {
                        value: Operand::Place(place(tuple_tmp, large)),
                        use_: air::DynOwnedUse::ConsumeTemporary,
                        witness: air::ContractWitnessId::from_index(1),
                        ty: dyn_ty,
                    },
                },
            ],
            air::AirTail::Return(None),
        ),
    });
    program.module_mut(module).functions.push(pack_moved);
    let entry_dyn = air::LocalId::from_index(0);
    let main = program.alloc_function(Function {
        name: Ident::new("main"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], void),
        locals: vec![local(dyn_ty, LocalKind::Temp)],
        body: structured_body(
            vec![Statement::Init {
                local: entry_dyn,
                value: RValue::DynPack {
                    value: Operand::Const(one),
                    use_: air::DynOwnedUse::ReusableRead,
                    witness: air::ContractWitnessId::from_index(3),
                    ty: dyn_ty,
                },
            }],
            air::AirTail::Return(None),
        ),
    });
    program.module_mut(module).functions.push(main);
    program.set_entry(main);
    let verified = air::verify(&program).expect("AIR verify failed");

    let plan = plan(&verified, RustPlanConfig::default()).expect("dynamic carrier plan");
    let rir = plan.program();
    assert_eq!(rir.dyn_carriers.len(), 2);
    assert_eq!(rir.dyn_carriers[0].variants.len(), 4);
    assert_eq!(rir.dyn_carriers[1].variants.len(), 1);
    let source = emit::emit(&plan.verified()).into_string();
    assert!(source.contains("enum anvDynDrawable_0"));
    assert!(source.contains("Witness0(i64)"));
    assert!(source.contains("Witness1(Box<"));
    assert!(source.contains("anvDynDrawable_0::Witness0("));
    assert!(source.contains("anvDynDrawable_0::Witness1(Box::new("));
    assert!(source.contains("anvDynDrawable_0::Witness2(Box::new("));
    assert!(source.contains("anvDynDrawable_0::Witness3("));
    assert!(source.contains("#[repr(u32)]\nenum anvDynDrawable_0"));
    assert!(source.contains("#[repr(u32)]\nenum anvDynDrawable_1"));
    let carrier_prefix = source
        .split_once("enum anvDynDrawable_0")
        .expect("dynamic declaration")
        .0;
    let carrier_attrs = carrier_prefix
        .rsplit_once("\n\n")
        .map_or(carrier_prefix, |(_, attrs)| attrs);
    assert!(!carrier_attrs.contains("PartialEq"));
    assert!(!carrier_attrs.contains("Hash"));
    let output = run_source(emit::RustSource::new(source));
    assert_eq!(output.status, SourceJobStatus::Success, "{}", output.stderr);
}

#[test]
fn mutable_dyn_call_emits_projected_payload_descriptor() {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let void = program.alloc_type(TypeData::Void);
    let module = program.alloc_module(root_module());
    let receiver = air::LocalId::from_index(0);
    let mut receiver_param = param("self", int, ParamMode::MutBorrow, receiver);
    receiver_param.role = ParamRole::Receiver;
    let bump = program.alloc_function(Function {
        name: Ident::new("bump"),
        module,
        kind: FunctionKind::Method,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![receiver_param], void),
        locals: vec![mut_local(int, LocalKind::Arg)],
        body: structured_body(vec![], air::AirTail::Return(None)),
    });
    program.module_mut(module).functions.push(bump);
    let slot = ContractSlotId::from_index(0);
    let surface = program.alloc_contract_surface(ContractSurfaceDecl {
        display_name: "Counter".into(),
        slots: vec![ContractSlotDecl {
            id: slot,
            name: Ident::new("bump"),
            receiver: ContractReceiver::Ref,
            params: vec![],
            ret: ContractReturnDecl::Value(void),
        }],
    });
    let dyn_ty = program.alloc_type(TypeData::Dyn(surface));
    program.alloc_contract_witness(ContractWitnessDecl {
        key: ContractWitnessKey {
            concrete_ty: int,
            surface,
            slots: vec![ContractWitnessSlotDecl {
                slot,
                receiver: ParamMode::MutBorrow,
                target: ContractWitnessTarget::Function { function: bump },
            }],
        },
    });
    let packed = air::LocalId::from_index(0);
    let one = int_const(&mut program, int, 1);
    let caller = program.alloc_function(Function {
        name: Ident::new("call_bump"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], void),
        locals: vec![mut_local(dyn_ty, LocalKind::User)],
        body: structured_body(
            vec![
                Statement::Init {
                    local: packed,
                    value: RValue::DynPack {
                        value: Operand::Const(one),
                        use_: air::DynOwnedUse::ReusableRead,
                        witness: air::ContractWitnessId::from_index(0),
                        ty: dyn_ty,
                    },
                },
                Statement::Eval(RValue::DynCall {
                    receiver: air::DynReceiver::MutableOwned(place(packed, dyn_ty)),
                    surface,
                    slot,
                    args: vec![],
                }),
            ],
            air::AirTail::Return(None),
        ),
    });
    program.module_mut(module).functions.push(caller);
    program.set_entry(caller);
    let verified = air::verify(&program).expect("AIR verify failed");
    let plan = plan(&verified, RustPlanConfig::default()).expect("dynamic call plan");
    let source = emit::emit(&plan.verified()).into_string();
    let probe = source
        .find(".access_with_ctx(rt, |rt, value|")
        .expect("short carrier discriminant probe");
    let target = probe + source[probe..].find("bump(").expect("dynamic target call");
    assert!(probe < target);
    assert!(source[probe..target].contains("}) }?;"));
    assert!(source.contains("struct __AnvDynPayloadOps;"));
    assert!(source.contains("MutPlace::projected("));
    assert!(!source.contains(".mutate_with_ctx(rt, |rt, value|"));
    let output = run_source(emit::RustSource::new(source));
    assert_eq!(output.status, SourceJobStatus::Success, "{}", output.stderr);
}

#[test]
fn profile_accepts_non_scalar_direct_root_assignment() {
    let mut program = Program::default();
    let string = program.alloc_type(TypeData::String);
    let value = program.alloc_const(ConstData {
        ty: string,
        value: ConstValue::String("ready".into()),
    });
    let void = program.alloc_type(TypeData::Void);
    let module = program.alloc_module(root_module());
    let global = global_with_init(&mut program, module, "g", string, Mutability::Mutable);
    let function = program.alloc_function(Function {
        name: Ident::new("f"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], void),
        locals: vec![],
        body: structured_body(
            vec![Statement::GlobalSetRoot {
                global,
                value: RValue::Use(Operand::Const(value)),
                init: air::GlobalInitEffect::StoreWithoutInit,
            }],
            air::AirTail::Return(None),
        ),
    });
    program.module_mut(module).functions.push(function);

    check(program);
}

#[test]
fn profile_accepts_scalar_global_declaration_and_initializer() {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let module = program.alloc_module(root_module());
    global_with_init(&mut program, module, "g", int, Mutability::Mutable);

    check(program);
}

#[test]
fn plan_global_slot_symbols_are_id_based() {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let left_module = program.alloc_module(root_module());
    let mut right = root_module();
    right.path = vec![Ident::new("other")];
    let right_module = program.alloc_module(right);
    let left = global_with_init(
        &mut program,
        left_module,
        "score",
        int,
        Mutability::Immutable,
    );
    let right = global_with_init(
        &mut program,
        right_module,
        "score",
        int,
        Mutability::Immutable,
    );
    let verified = air::verify(&program).expect("AIR verify failed");
    let plan = plan(&verified, rust_plan_config()).expect("plan failed");
    let rir = plan.program();

    assert_eq!(rir.globals.len(), 2);
    assert_eq!(rir.globals[0].air_id, left);
    assert_eq!(rir.globals[1].air_id, right);
    assert_eq!(rir.globals[0].slot_symbol.as_str(), "g0_score");
    assert_eq!(rir.globals[1].slot_symbol.as_str(), "g1_score");
}

#[test]
fn plan_lowers_scalar_global_declaration() {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let value = program.alloc_const(ConstData {
        ty: int,
        value: ConstValue::Int(1),
    });
    let module = program.alloc_module(root_module());
    let global = global_with_init(&mut program, module, "score", int, Mutability::Mutable);
    let init = program.globals[global.index()].init;
    program.function_mut(init).body =
        structured_body(vec![], air::AirTail::Return(Some(Operand::Const(value))));
    let verified = air::verify(&program).expect("AIR verify failed");
    let plan = plan(&verified, rust_plan_config()).expect("plan failed");
    let rir = plan.program();

    assert_eq!(rir.globals.len(), 1);
    assert_eq!(rir.globals[0].air_id, global);
    assert_eq!(rir.globals[0].module, module);
    assert_eq!(rir.globals[0].ty, RirTypeId::from_index(int.index()));
    assert!(rir.globals[0].mutable);
    assert_eq!(rir.globals[0].init, RirFunctionId::from_index(init.index()));
    assert_eq!(rir.globals[0].slot_symbol.as_str(), "g0_score");
}

#[test]
fn plan_lowers_scalar_global_read_into_ref_param_assignment() {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let void = program.alloc_type(TypeData::Void);
    let module = program.alloc_module(root_module());
    let global = global_with_init(&mut program, module, "score", int, Mutability::Immutable);
    let target = air::LocalId::from_index(0);
    let callee = program.alloc_function(Function {
        name: Ident::new("set"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![param("x", int, ParamMode::MutBorrow, target)], void),
        locals: vec![mut_local(int, LocalKind::Arg)],
        body: structured_body(
            vec![
                Statement::GlobalEnsure { global },
                Statement::Assign {
                    dst: place(target, int),
                    value: RValue::Use(Operand::Place(root_place(PlaceRoot::Global(global), int))),
                },
            ],
            air::AirTail::Return(None),
        ),
    });
    program.module_mut(module).functions.push(callee);
    let verified = air::verify(&program).expect("AIR verify failed");
    let plan = plan(&verified, rust_plan_config()).expect("plan failed");
    let function = rir_function_for_air(plan.program(), callee);

    assert!(matches!(
        function.body.stmts[0],
        RirStmt::GlobalEnsure { .. }
    ));
    assert!(matches!(
        &function.body.stmts[1],
        RirStmt::Init {
            value: RirRValue::Use(RirOperand::Place(RirPlace {
                root: RirPlaceRoot::Global(_),
                ..
            })),
            ..
        }
    ));
    assert!(matches!(
        &function.body.stmts[2],
        RirStmt::Assign {
            value: RirRValue::Use(RirOperand::Place(RirPlace {
                root: RirPlaceRoot::Local(_),
                ..
            })),
            ..
        }
    ));
}

#[test]
fn plan_temps_global_root_set_rhs_that_uses_generated_context() {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let value = program.alloc_const(ConstData {
        ty: int,
        value: ConstValue::Int(1),
    });
    let void = program.alloc_type(TypeData::Void);
    let module = program.alloc_module(root_module());
    let target = global_with_init(&mut program, module, "target", int, Mutability::Mutable);
    let source = global_with_init(&mut program, module, "source", int, Mutability::Immutable);
    for global in [target, source] {
        let init = program.globals[global.index()].init;
        program.function_mut(init).body =
            structured_body(vec![], air::AirTail::Return(Some(Operand::Const(value))));
    }
    let main = program.alloc_function(Function {
        name: Ident::new("main"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], void),
        locals: vec![],
        body: structured_body(
            vec![
                Statement::GlobalEnsure { global: source },
                Statement::GlobalSetRoot {
                    global: target,
                    value: RValue::Use(Operand::Place(root_place(PlaceRoot::Global(source), int))),
                    init: air::GlobalInitEffect::StoreWithoutInit,
                },
            ],
            air::AirTail::Return(None),
        ),
    });
    program.module_mut(module).functions.push(main);
    let verified = air::verify(&program).expect("AIR verify failed");
    let plan = plan(&verified, rust_plan_config()).expect("plan failed");
    let function = rir_function_for_air(plan.program(), main);

    assert!(matches!(
        function.body.stmts[0],
        RirStmt::GlobalEnsure { .. }
    ));
    assert!(matches!(
        &function.body.stmts[1],
        RirStmt::Init {
            value: RirRValue::Use(RirOperand::Place(RirPlace {
                root: RirPlaceRoot::Global(_),
                ..
            })),
            ..
        }
    ));
    assert!(matches!(
        &function.body.stmts[2],
        RirStmt::GlobalSetRoot {
            value: RirRValue::Use(RirOperand::Place(RirPlace {
                root: RirPlaceRoot::Local(_),
                ..
            })),
            ..
        }
    ));
}

#[test]
fn plan_lowers_global_initializer_body_calls() {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let value = program.alloc_const(ConstData {
        ty: int,
        value: ConstValue::Int(1),
    });
    let module = program.alloc_module(root_module());
    let global = global_with_init(&mut program, module, "score", int, Mutability::Immutable);
    let helper = program.alloc_function(Function {
        name: Ident::new("make_score"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], int),
        locals: vec![],
        body: structured_body(vec![], air::AirTail::Return(Some(Operand::Const(value)))),
    });
    program.module_mut(module).functions.push(helper);
    let init = program.globals[global.index()].init;
    let tmp = air::LocalId::from_index(0);
    program.function_mut(init).locals = vec![local(int, LocalKind::Temp)];
    program.function_mut(init).body = structured_body(
        vec![Statement::Init {
            local: tmp,
            value: RValue::Call {
                callee: Callee::Function(helper),
                args: vec![],
            },
        }],
        air::AirTail::Return(Some(Operand::Place(place(tmp, int)))),
    );
    let verified = air::verify(&program).expect("AIR verify failed");
    let plan = plan(&verified, rust_plan_config()).expect("plan failed");
    let init = rir_function_for_air(plan.program(), init);

    assert!(matches!(
        init.body.stmts.as_slice(),
        [RirStmt::Init {
            value: RirRValue::Call {
                callee: RirCallTarget::Function(_),
                ..
            },
            ..
        }]
    ));
}

#[test]
fn plan_lowers_scalar_global_root_read_and_statements() {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let value = program.alloc_const(ConstData {
        ty: int,
        value: ConstValue::Int(1),
    });
    let module = program.alloc_module(root_module());
    let global = global_with_init(&mut program, module, "score", int, Mutability::Mutable);
    let init = program.globals[global.index()].init;
    program.function_mut(init).body =
        structured_body(vec![], air::AirTail::Return(Some(Operand::Const(value))));
    let main = program.alloc_function(Function {
        name: Ident::new("main"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], int),
        locals: vec![],
        body: structured_body(
            vec![
                Statement::GlobalEnsure { global },
                Statement::GlobalSetRoot {
                    global,
                    value: RValue::Use(Operand::Const(value)),
                    init: air::GlobalInitEffect::StoreWithoutInit,
                },
            ],
            air::AirTail::Return(Some(Operand::Place(root_place(
                PlaceRoot::Global(global),
                int,
            )))),
        ),
    });
    program.module_mut(module).functions.push(main);
    program.set_entry(main);
    let verified = air::verify(&program).expect("AIR verify failed");
    let plan = plan(&verified, rust_plan_config()).expect("plan failed");
    let function = rir_function_for_air(plan.program(), main);

    assert!(matches!(
        function.body.stmts[0],
        RirStmt::GlobalEnsure {
            global: RirGlobalId(0)
        }
    ));
    assert!(matches!(
        function.body.stmts[1],
        RirStmt::GlobalSetRoot {
            global: RirGlobalId(0),
            ..
        }
    ));
    assert!(matches!(
        function.body.stmts[2],
        RirStmt::Init {
            value: RirRValue::Use(RirOperand::Place(RirPlace {
                root: RirPlaceRoot::Global(RirGlobalId(0)),
                ..
            })),
            ..
        }
    ));
    assert!(matches!(
        function.body.term,
        RirTerm::Return(Some(RirOperand::Place(RirPlace {
            root: RirPlaceRoot::Local(_),
            ..
        })))
    ));
}

#[test]
fn plan_lowers_projected_global_assignment() {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let tuple = program.alloc_type(TypeData::Tuple(vec![int]));
    let value = program.alloc_const(ConstData {
        ty: int,
        value: ConstValue::Int(2),
    });
    let void = program.alloc_type(TypeData::Void);
    let module = program.alloc_module(root_module());
    let global = global_with_init(&mut program, module, "g", tuple, Mutability::Mutable);
    let main = program.alloc_function(Function {
        name: Ident::new("main"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], void),
        locals: vec![],
        body: structured_body(
            vec![
                Statement::GlobalEnsure { global },
                Statement::Assign {
                    dst: Place {
                        root: PlaceRoot::Global(global),
                        projection: vec![Projection::TupleField(0)],
                        ty: int,
                    },
                    value: RValue::Use(Operand::Const(value)),
                },
            ],
            air::AirTail::Return(None),
        ),
    });
    program.module_mut(module).functions.push(main);
    let verified = air::verify(&program).expect("AIR verify failed");
    let plan = plan(&verified, rust_plan_config()).expect("plan failed");
    let function = rir_function_for_air(plan.program(), main);

    assert!(matches!(
        function.body.stmts.as_slice(),
        [
            RirStmt::GlobalEnsure { .. },
            RirStmt::MutPlaceSet {
                place: RirMutPlaceArg {
                    access: RirMutPlaceAccess::Handle(RirMutPlaceHandle::Global { .. }),
                    projections,
                    ..
                },
                ..
            }
        ] if matches!(projections.as_slice(), [RirProjection::TupleField(_)])
    ));
}

#[test]
fn plan_lowers_projected_global_ref_arg_to_mut_place() {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let tuple = program.alloc_type(TypeData::Tuple(vec![int]));
    let void = program.alloc_type(TypeData::Void);
    let module = program.alloc_module(root_module());
    let global = global_with_init(&mut program, module, "g", tuple, Mutability::Mutable);
    let param_local = air::LocalId::from_index(0);
    let callee = program.alloc_function(Function {
        name: Ident::new("bump"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(
            vec![param("x", int, ParamMode::MutBorrow, param_local)],
            void,
        ),
        locals: vec![mut_local(int, LocalKind::Arg)],
        body: structured_body(vec![], air::AirTail::Return(None)),
    });
    program.module_mut(module).functions.push(callee);
    let main = program.alloc_function(Function {
        name: Ident::new("main"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], void),
        locals: vec![],
        body: structured_body(
            vec![
                Statement::GlobalEnsure { global },
                Statement::Eval(RValue::Call {
                    callee: Callee::Function(callee),
                    args: vec![CallArg::MutBorrow(Place {
                        root: PlaceRoot::Global(global),
                        projection: vec![Projection::TupleField(0)],
                        ty: int,
                    })],
                }),
            ],
            air::AirTail::Return(None),
        ),
    });
    program.module_mut(module).functions.push(main);
    let verified = air::verify(&program).expect("AIR verify failed");
    let plan = plan(&verified, rust_plan_config()).expect("plan failed");
    let function = rir_function_for_air(plan.program(), main);

    assert!(matches!(
        function.body.stmts.as_slice(),
        [
            RirStmt::GlobalEnsure { .. },
            RirStmt::Eval(RirRValue::Call {
                args,
                ..
            })
        ] if matches!(
            args.as_slice(),
            [RirCallArg::MutPlace(RirMutPlaceArg {
                access: RirMutPlaceAccess::Handle(RirMutPlaceHandle::Global { .. }),
                projections,
                ..
            })] if matches!(projections.as_slice(), [RirProjection::TupleField(_)])
        )
    ));
}

#[test]
fn emit_scalar_global_slots_and_operations() {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let value = program.alloc_const(ConstData {
        ty: int,
        value: ConstValue::Int(1),
    });
    let module = program.alloc_module(root_module());
    let global = global_with_init(&mut program, module, "score", int, Mutability::Mutable);
    let init = program.globals[global.index()].init;
    program.function_mut(init).body =
        structured_body(vec![], air::AirTail::Return(Some(Operand::Const(value))));
    let main = program.alloc_function(Function {
        name: Ident::new("main"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], int),
        locals: vec![],
        body: structured_body(
            vec![
                Statement::GlobalEnsure { global },
                Statement::GlobalSetRoot {
                    global,
                    value: RValue::Use(Operand::Const(value)),
                    init: air::GlobalInitEffect::StoreWithoutInit,
                },
            ],
            air::AirTail::Return(Some(Operand::Place(root_place(
                PlaceRoot::Global(global),
                int,
            )))),
        ),
    });
    program.module_mut(module).functions.push(main);
    program.set_entry(main);
    let source = plan_source(program).into_string();

    assert!(source.contains("g0_score: anvyx_runtime::GlobalSlot<i64>"));
    assert!(source.contains(
        "g0_score: anvyx_runtime::GlobalSlot::new_with_safepoint(\"score\", safepoint.clone())"
    ));
    assert!(source.contains("globals.g0_score.ensure(||"));
    assert!(source.contains("globals.g0_score.set_without_init(1)?;"));
    assert!(source.contains("globals.g0_score.read(||"));
    assert!(!source.contains("static "));
    assert!(!source.contains("OnceLock"));
}

#[test]
fn emit_global_root_update_initializes_then_replaces_root() {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let one = program.alloc_const(ConstData {
        ty: int,
        value: ConstValue::Int(1),
    });
    let two = program.alloc_const(ConstData {
        ty: int,
        value: ConstValue::Int(2),
    });
    let void = program.alloc_type(TypeData::Void);
    let module = program.alloc_module(root_module());
    let global = global_with_init(&mut program, module, "score", int, Mutability::Mutable);
    let init = program.globals[global.index()].init;
    program.function_mut(init).body =
        structured_body(vec![], air::AirTail::Return(Some(Operand::Const(one))));
    let main = program.alloc_function(Function {
        name: Ident::new("main"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], void),
        locals: vec![],
        body: structured_body(
            vec![
                Statement::GlobalEnsure { global },
                Statement::GlobalUpdateRoot {
                    global,
                    value: RValue::Use(Operand::Const(two)),
                },
            ],
            air::AirTail::Return(None),
        ),
    });
    program.module_mut(module).functions.push(main);
    program.set_entry(main);
    let source = plan_source(program).into_string();
    let ensure = source
        .find("globals.g0_score.ensure(||")
        .expect("missing ensure");
    let set = source
        .find("globals.g0_score.set_without_init(2)")
        .expect("missing root update set");

    assert!(ensure < set);
}

#[test]
fn emit_non_scalar_global_read_and_root_set_materializes_owned_values() {
    let mut program = Program::default();
    let string = program.alloc_type(TypeData::String);
    let value = program.alloc_const(ConstData {
        ty: string,
        value: ConstValue::String("ready".into()),
    });
    let module = program.alloc_module(root_module());
    let global = global_with_init(&mut program, module, "title", string, Mutability::Mutable);
    let init = program.globals[global.index()].init;
    program.function_mut(init).body =
        structured_body(vec![], air::AirTail::Return(Some(Operand::Const(value))));
    let main = program.alloc_function(Function {
        name: Ident::new("main"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], string),
        locals: vec![],
        body: structured_body(
            vec![Statement::GlobalSetRoot {
                global,
                value: RValue::Use(Operand::Const(value)),
                init: air::GlobalInitEffect::StoreWithoutInit,
            }],
            air::AirTail::Return(Some(Operand::Place(root_place(
                PlaceRoot::Global(global),
                string,
            )))),
        ),
    });
    program.module_mut(module).functions.push(main);
    program.set_entry(main);
    let source = plan_source(program).into_string();

    assert!(source.contains("g0_title: anvyx_runtime::GlobalSlot<anvyx_runtime::AnvString>"));
    assert!(source.contains(
        "globals.g0_title.set_without_init(anvyx_runtime::AnvString::from(\"ready\"))?;"
    ));
    assert!(source.contains("globals.g0_title.read(||"));
    assert!(source.contains("(*(&*__global)).share()"));
    assert!(!source.contains("static "));
    assert!(!source.contains("OnceLock"));
}

#[test]
fn emit_enum_global_read_materializes_from_deref_guard_ref() {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let module = program.alloc_module(root_module());
    let enum_id = program.alloc_enum(EnumDecl {
        name: Ident::new("Event"),
        module,
        type_args: vec![],
        const_args: vec![],
        core: None,
        repr: air::EnumRepr::Adt,
        raw_type: None,
        variants: vec![VariantDecl {
            name: Ident::new("Hit"),
            shape: VariantShape::Tuple(vec![int]),
            raw_value: None,
        }],
    });
    program.module_mut(module).enums.push(enum_id);
    let event = program.alloc_type(TypeData::Enum(enum_id));
    let one = int_const(&mut program, int, 1);
    let global = global_with_init(&mut program, module, "event", event, Mutability::Immutable);
    let init = program.globals[global.index()].init;
    let tmp = air::LocalId::from_index(0);
    program.function_mut(init).locals = vec![local(event, LocalKind::Temp)];
    program.function_mut(init).body = structured_body(
        vec![Statement::Init {
            local: tmp,
            value: RValue::Aggregate {
                kind: AggregateCtor::EnumVariant {
                    enum_id,
                    variant: VariantId::from_index(0),
                },
                fields: vec![Operand::Const(one)],
                ty: event,
            },
        }],
        air::AirTail::Return(Some(Operand::Place(place(tmp, event)))),
    );
    let main = program.alloc_function(Function {
        name: Ident::new("main"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], event),
        locals: vec![],
        body: structured_body(
            vec![Statement::GlobalEnsure { global }],
            air::AirTail::Return(Some(Operand::Place(root_place(
                PlaceRoot::Global(global),
                event,
            )))),
        ),
    });
    program.module_mut(module).functions.push(main);
    program.set_entry(main);
    let source = plan_source(program);

    assert!(source.as_str().contains("match &*__global"));
    assert!(!source.as_str().contains("match __global"));
    let output = run_source(source);
    assert_eq!(output.status, SourceJobStatus::Success, "{}", output.stderr);
}

#[test]
fn emit_collection_global_reads_share_from_short_guard() {
    let mut program = Program::default();
    let string = program.alloc_type(TypeData::String);
    let list = program.alloc_type(TypeData::List(string));
    let map = program.alloc_type(TypeData::Map {
        key: string,
        value: list,
        order: air::MapOrder::Insertion,
    });
    let key = program.alloc_const(ConstData {
        ty: string,
        value: ConstValue::String("k".into()),
    });
    let value = program.alloc_const(ConstData {
        ty: string,
        value: ConstValue::String("v".into()),
    });
    let module = program.alloc_module(root_module());
    let global = global_with_init(&mut program, module, "table", map, Mutability::Immutable);
    let init = program.globals[global.index()].init;
    let xs = air::LocalId::from_index(0);
    let tmp = air::LocalId::from_index(1);
    program.function_mut(init).locals =
        vec![local(list, LocalKind::Temp), local(map, LocalKind::Temp)];
    program.function_mut(init).body = structured_body(
        vec![
            Statement::Init {
                local: xs,
                value: RValue::Aggregate {
                    kind: AggregateCtor::List,
                    fields: vec![Operand::Const(value)],
                    ty: list,
                },
            },
            Statement::Init {
                local: tmp,
                value: RValue::Aggregate {
                    kind: AggregateCtor::Map,
                    fields: vec![Operand::Const(key), Operand::Place(place(xs, list))],
                    ty: map,
                },
            },
        ],
        air::AirTail::Return(Some(Operand::Place(place(tmp, map)))),
    );
    let main = program.alloc_function(Function {
        name: Ident::new("main"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], map),
        locals: vec![],
        body: structured_body(
            vec![Statement::GlobalEnsure { global }],
            air::AirTail::Return(Some(Operand::Place(root_place(
                PlaceRoot::Global(global),
                map,
            )))),
        ),
    });
    program.module_mut(module).functions.push(main);
    program.set_entry(main);
    let source = plan_source(program);

    assert!(
        source
            .as_str()
            .contains("let __global = globals.g0_table.read")
    );
    assert!(source.as_str().contains("(*(&*__global)).share()"));
    let output = run_source(source);
    assert_eq!(output.status, SourceJobStatus::Success, "{}", output.stderr);
}

#[test]
fn profile_accepts_scalar_global_root_value_reads() {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let module = program.alloc_module(root_module());
    let global = global_with_init(&mut program, module, "g", int, Mutability::Mutable);
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
            air::AirTail::Return(Some(Operand::Place(root_place(
                PlaceRoot::Global(global),
                int,
            )))),
        ),
    };
    let id = program.alloc_function(function);
    program.module_mut(module).functions.push(id);
    program.set_entry(id);

    check(program);
}

#[test]
fn profile_accepts_non_scalar_global_root_value_reads() {
    let mut program = Program::default();
    let string = program.alloc_type(TypeData::String);
    let module = program.alloc_module(root_module());
    let global = global_with_init(&mut program, module, "g", string, Mutability::Mutable);
    let function = Function {
        name: Ident::new("f"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], string),
        locals: vec![],
        body: structured_body(
            vec![],
            air::AirTail::Return(Some(Operand::Place(root_place(
                PlaceRoot::Global(global),
                string,
            )))),
        ),
    };
    let id = program.alloc_function(function);
    program.module_mut(module).functions.push(id);
    program.set_entry(id);

    check(program);
}

#[test]
fn profile_accepts_keyable_tuple_map_keys() {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let void = program.alloc_type(TypeData::Void);
    let tuple = program.alloc_type(TypeData::Tuple(vec![int]));
    let map = program.alloc_type(TypeData::Map {
        key: tuple,
        value: int,
        order: air::MapOrder::Insertion,
    });
    let module = program.alloc_module(root_module());
    let function = Function {
        name: Ident::new("f"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], void),
        locals: vec![local(map, LocalKind::Temp)],
        body: structured_body(vec![], air::AirTail::Return(None)),
    };
    let id = program.alloc_function(function);
    program.module_mut(module).functions.push(id);
    program.set_entry(id);

    check(program);
}

#[test]
fn profile_accepts_supported_projected_global_assignment() {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let tuple = program.alloc_type(TypeData::Tuple(vec![int]));
    let value = program.alloc_const(ConstData {
        ty: int,
        value: ConstValue::Int(1),
    });
    let void = program.alloc_type(TypeData::Void);
    let module = program.alloc_module(root_module());
    let global = global_with_init(&mut program, module, "g", tuple, Mutability::Mutable);
    let function = Function {
        name: Ident::new("f"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], void),
        locals: vec![],
        body: structured_body(
            vec![Statement::Assign {
                dst: Place {
                    root: PlaceRoot::Global(global),
                    projection: vec![Projection::TupleField(0)],
                    ty: int,
                },
                value: RValue::Use(Operand::Const(value)),
            }],
            air::AirTail::Return(None),
        ),
    };
    let id = program.alloc_function(function);
    program.module_mut(module).functions.push(id);
    program.set_entry(id);

    check(program);
}

#[test]
fn profile_accepts_scalar_global_statements() {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let value = program.alloc_const(ConstData {
        ty: int,
        value: ConstValue::Int(1),
    });
    let module = program.alloc_module(root_module());
    let global = global_with_init(&mut program, module, "g", int, Mutability::Mutable);
    let function = Function {
        name: Ident::new("f"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], int),
        locals: vec![],
        body: structured_body(
            vec![
                Statement::GlobalEnsure { global },
                Statement::GlobalSetRoot {
                    global,
                    value: RValue::Use(Operand::Const(value)),
                    init: air::GlobalInitEffect::StoreWithoutInit,
                },
            ],
            air::AirTail::Return(Some(Operand::Const(value))),
        ),
    };
    let id = program.alloc_function(function);
    program.module_mut(module).functions.push(id);
    program.set_entry(id);

    check(program);
}

#[test]
fn profile_accepts_exact_root_global_borrow() {
    let mut program = Program::default();
    let string = program.alloc_type(TypeData::String);
    let void = program.alloc_type(TypeData::Void);
    let module = program.alloc_module(root_module());
    let global = global_with_init(&mut program, module, "g", string, Mutability::Mutable);
    let param_local = air::LocalId::from_index(0);
    let callee = program.alloc_function(Function {
        name: Ident::new("borrow"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(
            vec![param("x", string, ParamMode::SharedBorrow, param_local)],
            void,
        ),
        locals: vec![local(string, LocalKind::Arg)],
        body: structured_body(vec![], air::AirTail::Return(None)),
    });
    let caller = program.alloc_function(Function {
        name: Ident::new("f"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], void),
        locals: vec![],
        body: structured_body(
            vec![Statement::Eval(RValue::Call {
                callee: Callee::Function(callee),
                args: vec![CallArg::SharedBorrow(Place {
                    root: PlaceRoot::Global(global),
                    projection: vec![],
                    ty: string,
                })],
            })],
            air::AirTail::Return(None),
        ),
    });
    program
        .module_mut(module)
        .functions
        .extend([callee, caller]);
    program.set_entry(caller);

    check(program);
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
        lifetime: CaptureCellLifetime::Function,
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
        lifetime: CaptureCellLifetime::Function,
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
    assert!(text.contains("fn call<'cx, 'rt>(&self, rt: &mut anvyx_runtime::Ctx<'cx, 'rt>, types: &AnvTypes<'cx>, globals: &AnvGlobals<'cx>)"));
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

    assert!(source.contains("rt.heap().with(&v0, |storage| storage.child.clone())"));
    assert!(source.contains("rt.heap().with(&v2, |storage| storage.value)"));
    assert!(!source.contains("|storage| anv_f"));
}

#[test]
fn emit_dataref_set_uses_short_mut_heap_borrow() {
    let source = plan_source(dataref_field_projection_program()).into_string();

    assert!(source.contains("rt.heap().with_mut(&v0, |storage| { storage.value = 1; })"));
    assert!(!source.contains("with_mut(&v0, |storage| { anv"));
}

#[test]
fn emit_dataref_map_index_assignment_uses_mut_place() {
    let source = plan_source(dataref_map_index_assignment_program()).into_string();

    assert!(source.contains("anvyx_runtime::MutPlace::dataref("));
    assert!(!source.contains("rt.heap().with_mut(&v0, |storage| { storage.value ="));
}

#[test]
fn plan_lowers_projected_source_mut_call_arg_to_dataref_place() {
    let source = plan_source(projected_mut_call_arg_program()).into_string();

    assert!(source.contains("let __anv_dataref_place_object_0 = rt.heap().erase(&v0).map_err(anvyx_runtime::heap_access_error)?;"));
    assert!(source.contains(&target::mut_place_dataref(
        "__anv_dataref_place_object_0",
        "&__anv_dataref_place_ops_0",
    )));
    assert!(source.contains(&format!(
        "try_with_erased(object, {}",
        target::dataref_place_heap_type_access("self")
    )));
    assert!(!source.contains("rt.heap().with_mut(&v0, |storage| { storage.value ="));
}

#[test]
fn plan_lowers_multiple_projected_source_mut_call_args_without_copyback() {
    let source = plan_source(multi_projected_mut_call_arg_program()).into_string();

    assert!(source.contains(&target::mut_place_dataref(
        "__anv_dataref_place_object_0",
        "&__anv_dataref_place_ops_0",
    )));
    assert!(source.contains(&target::mut_place_dataref(
        "__anv_dataref_place_object_1",
        "&__anv_dataref_place_ops_1",
    )));
    assert!(!source.contains("rt.heap().with_mut(&v0, |storage| { storage.value ="));
    assert!(!source.contains("rt.heap().with_mut(&v1, |storage| { storage.value ="));
}

#[test]
fn emit_dataref_mut_borrow_root_rebinds_handle() {
    let source = plan_source(dataref_root_rebind_program()).into_string();

    assert!(source.contains(
        "mut v0: anvyx_runtime::MutPlace<'_, 'cx, anvT2_Node<'cx>>, v1: anvT2_Node<'cx>"
    ));
    assert!(source.contains("v0.set(rt, v1.clone())?;"));
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

    assert!(!source.as_str().contains("use std::fmt::Write;"));
    assert!(source.as_str().contains("fn anvstringify_t3_point"));
    assert!(
        source.as_str().contains(
            "std::fmt::Write::write_fmt(&mut out, format_args!(\"{}\", value.x)).unwrap();"
        )
    );
    assert!(!source.as_str().contains("format!(\"{}\", value.x)"));
    assert!(!source.as_str().contains("write!(out,"));
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

    assert!(text.contains("anv_f1_Point_to_string(rt, types, globals, anvT3_Point { x: v0.x })"));
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
        text.contains("Point_to_string<'cx, 'rt>(rt: &mut anvyx_runtime::Ctx<'cx, 'rt>, _types: &AnvTypes<'cx>, _globals: &AnvGlobals<'cx>, v0: &anvT3_Point)")
    );
    assert!(text.contains("-> Result<anvyx_runtime::AnvString, anvyx_runtime::RuntimeError>"));
    assert!(text.contains("host::fallible(rt, 41)?;"));
    assert!(text.contains("Point_to_string(rt, types, globals, &v0)?"));
    assert!(text.contains("fn anv_f0_main<'cx, 'rt>(rt: &mut anvyx_runtime::Ctx<'cx, 'rt>, types: &AnvTypes<'cx>, globals: &AnvGlobals<'cx>)"));
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
        "fn anv_f0_Point_value<'cx, 'rt>(_rt: &mut anvyx_runtime::Ctx<'cx, 'rt>, _types: &AnvTypes<'cx>, _globals: &AnvGlobals<'cx>, v0: anvT3_Point) -> i64"
    ));
    assert!(text.contains("anv_f0_Point_value(rt, types, globals, anvT3_Point { x: v0.x })"));
    assert!(!text.contains("impl anvT"));
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

fn heap_edge_global_rir() -> RirProgram {
    valid_global_rir(|program| {
        let int = RirTypeId::from_index(0);
        let list = RirTypeId::from_index(program.types.len());
        program.types.push(RirType::List(int));
        program
            .collection_storages
            .push(rir_list_storage(list, int));
        program.globals[0].ty = list;
        program.functions[0].ret.ty = list;
        program.functions[0].locals.push(RirLocal {
            id: RirLocalId::from_index(0),
            ty: list,
            mutable: true,
            symbol: RirSymbol::new("xs"),
            initialized: false,
            payload_ref: false,
        });
        program.functions[0].body = RirStructuredBlock {
            stmts: vec![RirStmt::Init {
                local: RirLocalId::from_index(0),
                value: RirRValue::List {
                    ty: list,
                    elems: vec![],
                },
            }],
            term: RirTerm::Return(Some(RirOperand::Place(RirPlace::local(
                RirLocalId::from_index(0),
                vec![],
                list,
            )))),
        };
        program.functions[1].body.stmts = vec![RirStmt::GlobalEnsure {
            global: RirGlobalId::from_index(0),
        }];
        program.entry = Some(RirFunctionId::from_index(1));
    })
}

#[test]
fn emit_anv_globals_trace_impl_for_heap_edge_slots() {
    let source =
        emit::emit(&rir::verify(&heap_edge_global_rir()).expect("RIR verify failed")).into_string();

    assert!(source.contains("unsafe impl<'cx> anvyx_runtime::Trace<'cx> for AnvGlobals<'cx>"));
    assert!(source.contains("anvyx_runtime::Trace::trace(&self.g0_score, visitor);"));
    assert!(source.contains("impl<'cx> anvyx_runtime::TraceRootSet<'cx> for AnvGlobals<'cx>"));
    assert!(source.contains("self.g0_score.validate_trace()?;"));
    assert!(source.contains("let globals = AnvGlobals::new(&safepoint);"));
    assert!(source.contains(
        "let mut rt = unsafe { anvyx_runtime::Ctx::__anvyx_from_raw_with_trace_roots_and_safepoint(anv_entry.heap, anv_entry.globals, anv_entry.safepoint) };"
    ));
    for forbidden in ["static ", "thread_local!", "OnceLock"] {
        assert!(
            !source.contains(forbidden),
            "generated source contains {forbidden}"
        );
    }
}

#[test]
fn emit_anv_globals_omits_trace_impl_without_heap_edge_slots() {
    let program = valid_global_rir(|program| {
        program.entry = Some(RirFunctionId::from_index(1));
    });

    let source = emit::emit(&rir::verify(&program).expect("RIR verify failed")).into_string();

    assert!(!source.contains("anvyx_runtime::Trace<'cx> for AnvGlobals<'cx>"));
    assert!(!source.contains("anvyx_runtime::TraceRootSet<'cx> for AnvGlobals<'cx>"));
    assert!(
        source.contains(
            "let mut rt = unsafe { anvyx_runtime::Ctx::__anvyx_from_raw_with_safepoint(anv_entry.heap, anv_entry.safepoint) };"
        )
    );
}

#[test]
fn emit_lambda_sig_trace_impl_for_heap_env_lambda() {
    let mut program = valid_heap_env_lambda_rir();
    let lambda_ty = RirTypeId::from_index(2);
    program.functions[0].locals.push(RirLocal {
        id: RirLocalId::from_index(1),
        ty: lambda_ty,
        mutable: true,
        symbol: RirSymbol::new("nested"),
        initialized: true,
        payload_ref: false,
    });
    program.lambdas.push(RirLambda {
        id: RirLambdaId::from_index(1),
        source: RirLambdaSource::Function(FunctionId::from_index(0)),
        function: RirFunctionId::from_index(1),
        sig: RirLambdaSigId::from_index(0),
        escape: RirLambdaEscape::Escaping,
        storage: RirLambdaStorage::ZeroEnv,
        captures: vec![],
    });
    program.cells.push(RirCellDecl {
        id: RirCellId::from_index(0),
        owner: RirFunctionId::from_index(0),
        source_local: RirLocalId::from_index(1),
        payload_ty: lambda_ty,
        storage: RirCellStorage::Heap,
        lifetime: RirCellLifetime::Function,
        symbol: RirSymbol::new("__cell0"),
    });

    let source = emit::emit(&rir::verify(&program).expect("RIR verify failed")).into_string();

    assert!(source.contains("#[derive(Clone)]\nenum LambdaSig0<'cx>"));
    assert!(!source.contains("#[derive(Clone, anvyx_runtime::Trace)]\nenum LambdaSig0"));
    assert!(source.contains("unsafe impl<'cx> anvyx_runtime::Trace<'cx> for LambdaSig0<'cx>"));
    assert!(source.contains("Self::L0 { env } => anvyx_runtime::Trace::trace(env, visitor)"));
    assert!(source.contains("Self::L1 => {},"));
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

    assert!(source.contains("anvyx_runtime::Trace"));
    assert!(source.contains("struct Payload"));
}

#[test]
fn trace_plan_marks_struct_with_primitive_list_field() {
    let int = RirTypeId::from_index(0);
    let list = RirTypeId::from_index(1);
    let program = RirProgram {
        types: vec![
            RirType::Int,
            RirType::List(int),
            RirType::Struct(RirStructId::from_index(0)),
        ],
        collection_storages: vec![rir_list_storage(list, int)],
        structs: vec![RirStruct {
            id: RirStructId::from_index(0),
            air_id: None,
            symbol: RirSymbol::new("Payload"),
            display: RirSymbol::new("Payload"),
            native_path: None,
            native_ref: false,
            native_key: None,
            copyable: false,
            fields: vec![RirField {
                id: RirFieldId::from_index(0),
                symbol: RirSymbol::new("items"),
                ty: list,
            }],
        }],
        ..RirProgram::default()
    };

    let source = emit::emit(&rir::verify(&program).expect("RIR verify failed")).into_string();

    assert!(RustTracePlan::build(&program).needs_struct_trace(RirStructId::from_index(0)));
    assert!(source.contains(
        "#[derive(Clone, anvyx_runtime::Trace)]\n#[trace(crate = anvyx_runtime, ctx = 'cx)]\nstruct Payload<'cx>"
    ));
    assert!(source.contains(
        "list_storage1: heap.register_untracked::<anvyx_runtime::ListStorage<'cx, i64>>()"
    ));
}

#[test]
fn collection_storage_tracking_follows_payload_edges() {
    let mut program = dataref_metadata_rir();
    let int = RirTypeId::from_index(0);
    let node = RirTypeId::from_index(1);
    let primitive_list = RirTypeId::from_index(program.types.len());
    program.types.push(RirType::List(int));
    let ref_list = RirTypeId::from_index(program.types.len());
    program.types.push(RirType::List(node));
    let map = RirTypeId::from_index(program.types.len());
    program.types.push(RirType::Map {
        key: int,
        value: node,
    });
    program.collection_storages = vec![
        rir_list_storage_id(0, primitive_list, int),
        rir_list_storage_id(1, ref_list, node),
        rir_map_storage(2, map, int, node),
    ];

    let source = emit::emit(&rir::verify(&program).expect("RIR verify failed")).into_string();

    assert!(source.contains(
        "list_storage2: heap.register_untracked::<anvyx_runtime::ListStorage<'cx, i64>>()"
    ));
    assert!(source.contains(
        "list_storage3: heap.register_tracked::<anvyx_runtime::ListStorage<'cx, Node<'cx>>>()"
    ));
    assert!(source.contains(
        "map_storage4: heap.register_tracked::<anvyx_runtime::MapStorage<'cx, i64, Node<'cx>>>()"
    ));
}

#[test]
fn rir_verify_rejects_list_storage_slice_payload() {
    let int = RirTypeId::from_index(1);
    let slice = RirTypeId::from_index(2);
    let list = RirTypeId::from_index(3);
    let program = RirProgram {
        types: vec![
            RirType::Void,
            RirType::Int,
            RirType::Slice(int),
            RirType::List(slice),
        ],
        collection_storages: vec![rir_list_storage(list, slice)],
        ..empty_rir_function(RirType::Void)
    };

    assert_rir_error(program, RirVerifyErrorKind::UnsupportedRValueType);
}

#[test]
fn rir_verify_rejects_map_storage_optional_key() {
    let int = RirTypeId::from_index(1);
    let key = RirTypeId::from_index(2);
    let map = RirTypeId::from_index(3);
    let program = RirProgram {
        types: vec![
            RirType::Void,
            RirType::Int,
            RirType::Option(int),
            RirType::Map { key, value: int },
        ],
        collection_storages: vec![rir_map_storage(0, map, key, int)],
        ..empty_rir_function(RirType::Void)
    };

    assert_rir_error(program, RirVerifyErrorKind::UnsupportedRValueType);
}

#[test]
fn rir_verify_rejects_unsupported_stored_field_payload() {
    let int = RirTypeId::from_index(1);
    let slice = RirTypeId::from_index(2);
    let program = RirProgram {
        types: vec![
            RirType::Void,
            RirType::Int,
            RirType::Slice(int),
            RirType::Struct(RirStructId::from_index(0)),
        ],
        structs: vec![rir_struct(0, "Payload", vec![rir_field(0, "items", slice)])],
        ..empty_rir_function(RirType::Void)
    };

    assert_rir_error(program, RirVerifyErrorKind::UnsupportedRValueType);
}

#[test]
fn emit_renders_plain_struct_declarations_without_impls() {
    let program = struct_decl_program(false);
    let source = plan_source(program).into_string();

    assert!(source.contains("struct anvT2_Point"));
    assert!(source.contains("x: i64"));
    assert!(source.contains("name: anvyx_runtime::AnvString"));
    assert!(source.contains("#[derive("));
    assert!(!source.contains("impl anvT"));
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
        .push(RirStmt::Eval(RirRValue::Use(RirOperand::Place(
            RirPlace::local(
                RirLocalId::from_index(0),
                vec![RirProjection::TupleField(RirFieldId::from_index(2))],
                RirTypeId::from_index(0),
            ),
        ))));

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
        .push(RirStmt::Eval(RirRValue::Use(RirOperand::Place(
            RirPlace::local(
                RirLocalId::from_index(1),
                vec![RirProjection::TupleField(RirFieldId::from_index(0))],
                RirTypeId::from_index(0),
            ),
        ))));

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
            native_ref: false,
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
            native_ref: false,
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
                        args: vec![RirCallArg::Value(RirOperand::Place(RirPlace::local(
                            RirLocalId::from_index(0),
                            vec![],
                            strukt,
                        )))],
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
        native_ref: false,
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
        native_ref: false,
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
            dst: RirPlace::local(
                RirLocalId::from_index(1),
                vec![RirProjection::Field(RirFieldId::from_index(0))],
                RirTypeId::from_index(1),
            ),
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
fn profile_rejects_function_value_map_keys() {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let void = program.alloc_type(TypeData::Void);
    let function = program.alloc_type(TypeData::Function(air::SignatureType::new(
        vec![],
        air::ReturnMode::Value(void),
    )));
    program.alloc_type(TypeData::Map {
        key: function,
        value: int,
        order: air::MapOrder::Insertion,
    });

    expect_reject(program, ProfileErrorKind::UnsupportedMapKey);
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
fn rir_accepts_slice_stack_cell_get_copy() {
    let mut program = stack_cell_rir_with(|cell| cell.payload_ty = RirTypeId::from_index(2));
    program.types.push(RirType::Slice(RirTypeId::from_index(1)));
    let slice = RirTypeId::from_index(2);
    program.functions[0].locals[0].ty = slice;
    program.functions[0].body.stmts = vec![
        RirStmt::CellInit {
            cell: owner_cell_ref(),
            value: RirRValue::Use(RirOperand::Place(rir_place(
                RirLocalId::from_index(0),
                slice,
            ))),
        },
        RirStmt::Eval(RirRValue::CellGetCopy {
            cell: owner_cell_ref(),
            ty: slice,
        }),
    ];

    rir::verify(&program).expect("slice cell get should share descriptor");
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
            "let __cell0: anvyx_runtime::StackLambdaCell<i64> = anvyx_runtime::StackLambdaCell::<i64>::new_with_safepoint(1, rt.__anvyx_safepoint_state());"
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
    assert!(text.contains("__cell0 = anvyx_runtime::StackLambdaCell::<i64>::new_with_safepoint(1, rt.__anvyx_safepoint_state());"));
    assert!(text.contains("__cell0 = anvyx_runtime::StackLambdaCell::<i64>::new_with_safepoint(2, rt.__anvyx_safepoint_state());"));
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
fn rir_accepts_loop_lifetime_stack_cell_init_in_loop() {
    let mut program = stack_cell_rir(valid_stack_cell_decl());
    program.cells[0].lifetime = RirCellLifetime::Loop {
        loop_id: RirLoopId::from_index(0),
    };
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

    let source = emit::emit(&rir::verify(&program).expect("RIR verify failed")).into_string();
    assert!(!source.contains("let __cell0: anvyx_runtime::StackLambdaCell<i64>;"));
    assert!(source.contains("let __cell0: anvyx_runtime::StackLambdaCell<i64> = anvyx_runtime::StackLambdaCell::<i64>::new_with_safepoint(1, rt.__anvyx_safepoint_state());"));
}

#[test]
fn rir_rejects_loop_lifetime_stack_cell_init_outside_loop() {
    let mut program = stack_cell_rir(valid_stack_cell_decl());
    program.cells[0].lifetime = RirCellLifetime::Loop {
        loop_id: RirLoopId::from_index(0),
    };
    let one = push_int_const(&mut program, 1);
    program.functions[0].body.stmts = vec![RirStmt::CellInit {
        cell: owner_cell_ref(),
        value: RirRValue::Use(RirOperand::Const(one)),
    }];

    assert_rir_error(program, RirVerifyErrorKind::UnsupportedLambdaCell);
}

#[test]
fn rir_rejects_loop_lifetime_stack_cell_init_in_nested_loop() {
    let mut program = stack_cell_rir(valid_stack_cell_decl());
    program.cells[0].lifetime = RirCellLifetime::Loop {
        loop_id: RirLoopId::from_index(0),
    };
    let one = push_int_const(&mut program, 1);
    program.functions[0].body.stmts = vec![RirStmt::Loop(RirLoop {
        id: RirLoopId::from_index(0),
        body: RirStructuredBlock {
            stmts: vec![RirStmt::Loop(RirLoop {
                id: RirLoopId::from_index(1),
                body: RirStructuredBlock {
                    stmts: vec![RirStmt::CellInit {
                        cell: owner_cell_ref(),
                        value: RirRValue::Use(RirOperand::Const(one)),
                    }],
                    term: RirTerm::None,
                },
            })],
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
        value: RirOperand::Place(RirPlace::local(
            RirLocalId::from_index(0),
            vec![],
            lambda_ty,
        )),
    };

    assert_rir_error(program, RirVerifyErrorKind::CallArgEscape);
}

#[test]
fn rir_accepts_heap_env_and_borrowed_variants_sharing_signature() {
    let mut program = valid_heap_env_lambda_rir();
    let void = RirTypeId::from_index(0);
    let string = RirTypeId::from_index(3);
    let sig = RirLambdaSigId::from_index(0);
    let lambda = RirLambdaId::from_index(1);
    let target = RirFunctionId::from_index(2);
    let source = RirLocalId::from_index(0);
    program.types.push(RirType::String);

    program.lambdas.push(RirLambda {
        id: lambda,
        source: RirLambdaSource::Lambda(air::LambdaId::from_index(1)),
        function: target,
        sig,
        escape: RirLambdaEscape::NonEscaping,
        storage: RirLambdaStorage::ScopedCaptures,
        captures: vec![RirLambdaCapture {
            ty: string,
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
            ty: string,
            semantic: RirParamSemantic::SharedBorrow,
            abi: RirParamAbi::SharedBorrow,
            escape: RirParamEscape::NonEscaping,
        }],
        ret: RirReturn { ty: void },
        locals: vec![RirLocal {
            id: source,
            ty: string,
            mutable: false,
            symbol: RirSymbol::new("capture"),
            initialized: true,
            payload_ref: false,
        }],
        body: RirStructuredBlock::default(),
    });

    rir::verify(&program).expect("RIR verify failed");
}

#[test]
fn rir_accepts_returning_heap_env_lambda_local() {
    let mut program = valid_heap_env_lambda_rir();
    let lambda_ty = RirTypeId::from_index(2);
    let f = RirLocalId::from_index(1);
    program.functions[1].ret.ty = lambda_ty;
    program.functions[1].body.term = RirTerm::Return(Some(RirOperand::Place(RirPlace::local(
        f,
        vec![],
        lambda_ty,
    ))));

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
            value: RirRValue::Use(RirOperand::Place(RirPlace::local(f, vec![], lambda_ty))),
        },
        RirStmt::Eval(RirRValue::Call {
            callee: RirCallTarget::LambdaValue {
                callee: RirOperand::Place(RirPlace::local(g, vec![], lambda_ty)),
                sig: RirLambdaSigId::from_index(0),
            },
            args: vec![],
            ty: void,
        }),
        RirStmt::Eval(RirRValue::Call {
            callee: RirCallTarget::LambdaValue {
                callee: RirOperand::Place(RirPlace::local(g, vec![], lambda_ty)),
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
            args: vec![RirCallArg::Value(RirOperand::Place(RirPlace::local(
                f,
                vec![],
                lambda_ty,
            )))],
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
                callee: RirOperand::Place(RirPlace::local(f, vec![], lambda_ty)),
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
    let policy = RirRustRepPolicy::new(&program);
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
    program.scoped_place_cells[0].source = scoped_source_param(RirLocalId::from_index(0), string);
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
            args: vec![RirCallArg::MutPlace(RirMutPlaceArg::scoped_place_cell(
                RirScopedPlaceCellRef::Owner(RirScopedPlaceCellId::from_index(0)),
                RirTypeId::from_index(1),
            ))],
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
        .push(RirStmt::Eval(RirRValue::Use(RirOperand::Place(
            RirPlace::local(RirLocalId::from_index(0), vec![], RirTypeId::from_index(1)),
        ))));

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
            args: vec![RirCallArg::MutPlace(RirMutPlaceArg::param(
                RirLocalId::from_index(0),
                RirTypeId::from_index(1),
            ))],
            ty: RirTypeId::from_index(0),
        }));

    assert_rir_error(program, RirVerifyErrorKind::CallArgMode);
}

#[test]
fn rir_rejects_scoped_place_cell_mut_place_arg_to_native_mut_borrow() {
    let mut program = scoped_place_cell_rir(valid_scoped_place_cell_decl());
    program.externs.push(RirExtern {
        id: RirExternId::from_index(0),
        air_id: None,
        symbol: RirSymbol::new("native_touch"),
        kind: RirExternKind::Native(rir::RirNativeExtern::new(
            vec!["host".to_string(), "touch".to_string()],
            anvyx_runtime::RustExternAbi {
                params: vec![anvyx_runtime::RustParamAbi::MutBorrow(
                    anvyx_runtime::ExternTypeExpr::Int,
                )],
                ret: anvyx_runtime::RustReturnAbi::Void,
                fallible: false,
                support: anvyx_runtime::RustAbiSupport::Direct,
                ctx: anvyx_runtime::RustWrapperCtx::HiddenRuntime,
            },
        )),
        params: vec![RirExternParam {
            ty: RirTypeId::from_index(1),
            semantic: RirParamSemantic::MutBorrow,
            abi: RirParamAbi::MutBorrow,
            escape: RirParamEscape::NonEscaping,
        }],
        ret: RirTypeId::from_index(0),
        abi: rir_abi(
            vec![anvyx_runtime::ExternTypeExpr::Int],
            anvyx_runtime::ExternTypeExpr::Void,
        ),
    });
    program.functions[0]
        .body
        .stmts
        .push(RirStmt::Eval(RirRValue::Call {
            callee: RirCallTarget::Extern(RirExternId::from_index(0)),
            args: vec![RirCallArg::MutPlace(RirMutPlaceArg::scoped_place_cell(
                RirScopedPlaceCellRef::Owner(RirScopedPlaceCellId::from_index(0)),
                RirTypeId::from_index(1),
            ))],
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
            args: vec![RirCallArg::MutPlace(RirMutPlaceArg::scoped_place_cell(
                RirScopedPlaceCellRef::Owner(RirScopedPlaceCellId::from_index(0)),
                RirTypeId::from_index(0),
            ))],
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
        scoped_place_cell_rir_with(|cell| {
            cell.source = scoped_source_param(RirLocalId::from_index(1), RirTypeId::from_index(1));
        }),
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
fn rir_accepts_for_ref_alias_scoped_place_source_local_root() {
    let mut program = scoped_place_cell_rir(valid_scoped_place_cell_decl());
    let int = RirTypeId::from_index(1);
    program.scoped_place_cells[0].source = RirScopedPlaceSource::ForRefAlias {
        place: RirMutPlaceArg::from_handle(
            RirMutPlaceHandle::Local {
                local: RirLocalId::from_index(0),
                ty: int,
            },
            vec![],
            int,
        ),
    };

    rir::verify(&program).expect("RIR rejected for-ref alias scoped-place source");
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
fn rir_rejects_duplicate_scoped_place_cells_for_same_local_with_different_sources() {
    let mut program = scoped_place_cell_rir(valid_scoped_place_cell_decl());
    let mut duplicate = valid_scoped_place_cell_decl();
    duplicate.id = RirScopedPlaceCellId::from_index(1);
    duplicate.source = scoped_source_ref_self(RirLocalId::from_index(0), RirTypeId::from_index(1));
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
        lifetime: RirCellLifetime::Function,
        symbol: RirSymbol::new("__cell1"),
    });
    let policy = RirRustRepPolicy::new(&program);
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
    let policy = RirRustRepPolicy::new(&program);

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

    assert!(text.contains("rt.heap().with(&__cell0, |cell| cell.set(2))?;"));
    assert!(text.contains("let tmp: i64 = rt.heap().with(&__cell0, |cell| cell.get_copy())?;"));
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

    assert!(text.contains("let value = seed(rt, types, globals);"));
    assert!(
        text.contains("let safepoint = rt.__anvyx_safepoint_state(); rt.heap().alloc(heap_type, anvyx_runtime::LambdaCell::<i64>::new_with_safepoint(value, safepoint))")
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
        "= { let value = 1; let heap_type = types.lambda_cell0; let safepoint = rt.__anvyx_safepoint_state(); rt.heap().alloc(heap_type, anvyx_runtime::LambdaCell::<i64>::new_with_safepoint(value, safepoint)) };"
    ));
    assert!(source.contains("c0: anvyx_runtime::Handle<'cx, anvyx_runtime::LambdaCell<i64>>,"));
    assert!(source.contains("c0: __cell0.clone()"));
    assert!(source.contains("let c0 = rt.heap().with(env, |env| env.c0.clone());"));
    assert!(source.contains("target(rt, types, globals, c0)"));
    assert!(source.contains("rt.heap().with(&cell, |cell| cell.get_copy())?"));
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
        RirMutPlaceArg::heap_cell(
            RirCellRef::Owner(RirCellId::from_index(0)),
            RirTypeId::from_index(1),
        ),
    ))
    .expect("RIR rejected heap cell mut-place arg");
}

#[test]
fn rir_rejects_heap_cell_as_stack_mut_place_arg() {
    assert_rir_error(
        cell_mut_place_call_rir(
            valid_heap_cell_decl(),
            RirMutPlaceArg::stack_cell(
                RirCellRef::Owner(RirCellId::from_index(0)),
                RirTypeId::from_index(1),
            ),
        ),
        RirVerifyErrorKind::CallArgMode,
    );
}

#[test]
fn rir_rejects_stack_cell_as_heap_mut_place_arg() {
    assert_rir_error(
        cell_mut_place_call_rir(
            valid_stack_cell_decl(),
            RirMutPlaceArg::heap_cell(
                RirCellRef::Owner(RirCellId::from_index(0)),
                RirTypeId::from_index(1),
            ),
        ),
        RirVerifyErrorKind::CallArgMode,
    );
}

#[test]
fn rir_rejects_heap_cell_mut_place_arg_payload_mismatch() {
    assert_rir_error(
        cell_mut_place_call_rir(
            valid_heap_cell_decl(),
            RirMutPlaceArg::heap_cell(
                RirCellRef::Owner(RirCellId::from_index(0)),
                RirTypeId::from_index(0),
            ),
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
        RirMutPlaceArg::heap_cell(
            RirCellRef::Owner(RirCellId::from_index(0)),
            RirTypeId::from_index(1),
        ),
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
        args: vec![RirCallArg::MutPlace(RirMutPlaceArg::heap_cell(
            RirCellRef::Capture {
                cell: RirCellId::from_index(0),
                local: RirLocalId::from_index(0),
            },
            RirTypeId::from_index(1),
        ))],
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
        lifetime: RirCellLifetime::Function,
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
        source: scoped_source_param(RirLocalId::from_index(0), RirTypeId::from_index(1)),
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
            value: RirRValue::Use(RirOperand::Place(RirPlace::local(f, vec![], lambda_ty))),
        },
        RirStmt::Eval(RirRValue::Call {
            callee: RirCallTarget::LambdaValue {
                callee: RirOperand::Place(RirPlace::local(f, vec![], lambda_ty)),
                sig: RirLambdaSigId::from_index(0),
            },
            args: vec![],
            ty: RirTypeId::from_index(0),
        }),
        RirStmt::Eval(RirRValue::Call {
            callee: RirCallTarget::LambdaValue {
                callee: RirOperand::Place(RirPlace::local(g, vec![], lambda_ty)),
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
    assert!(text.contains("fn call<'cx, 'rt>(&self, rt: &mut anvyx_runtime::Ctx<'cx, 'rt>, types: &AnvTypes<'cx>, globals: &AnvGlobals<'cx>)"));
    assert!(text.contains("c0: &__cell0"));
    assert!(text.contains("target(rt, types, globals, *c0)"));
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
    program.functions[1].body.stmts = vec![RirStmt::Eval(RirRValue::Use(RirOperand::Place(
        RirPlace::local(RirLocalId::from_index(0), vec![], RirTypeId::from_index(1)),
    )))];

    assert_rir_error(program, RirVerifyErrorKind::UnsupportedLambdaCapture);
}

#[test]
fn rir_rejects_ordinary_place_use_of_scoped_place_cell_capture_param() {
    let mut program = valid_scoped_place_cell_lambda_rir();
    program.functions[1].body.stmts = vec![RirStmt::Eval(RirRValue::Use(RirOperand::Place(
        RirPlace::local(RirLocalId::from_index(0), vec![], RirTypeId::from_index(1)),
    )))];

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
        args: vec![RirCallArg::MutBorrow(RirPlace::local(
            RirLocalId::from_index(0),
            vec![],
            RirTypeId::from_index(1),
        ))],
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
        lifetime: RirCellLifetime::Function,
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
        lifetime: RirCellLifetime::Function,
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
fn rir_accepts_lambda_struct_and_tuple_fields() {
    let void = RirTypeId::from_index(0);
    let lambda_ty = RirTypeId::from_index(1);
    let sig = RirLambdaSigId::from_index(0);
    let program = RirProgram {
        types: vec![
            RirType::Void,
            RirType::Lambda(sig),
            RirType::Struct(RirStructId::from_index(0)),
            RirType::Tuple(RirTupleId::from_index(0)),
        ],
        lambda_sigs: vec![RirLambdaSig {
            id: sig,
            params: vec![],
            ret: void,
        }],
        structs: vec![RirStruct {
            id: RirStructId::from_index(0),
            air_id: None,
            symbol: RirSymbol::new("Holder"),
            display: RirSymbol::new("Holder"),
            native_path: None,
            native_ref: false,
            native_key: None,
            copyable: false,
            fields: vec![RirField {
                id: RirFieldId::from_index(0),
                symbol: RirSymbol::new("f"),
                ty: lambda_ty,
            }],
        }],
        tuples: vec![RirTuple {
            id: RirTupleId::from_index(0),
            symbol: RirSymbol::new("Tuple0"),
            display: RirSymbol::new("(fn(),)"),
            copyable: false,
            fields: vec![RirField {
                id: RirFieldId::from_index(0),
                symbol: RirSymbol::new("0"),
                ty: lambda_ty,
            }],
        }],
        ..RirProgram::default()
    };

    rir::verify(&program).expect("RIR rejected lambda aggregate fields");
}

#[test]
fn rir_rejects_lambda_map_key_types() {
    let void = RirTypeId::from_index(0);
    let lambda_ty = RirTypeId::from_index(1);
    let map_ty = RirTypeId::from_index(2);
    let sig = RirLambdaSigId::from_index(0);
    let program = RirProgram {
        types: vec![
            RirType::Void,
            RirType::Lambda(sig),
            RirType::Map {
                key: lambda_ty,
                value: void,
            },
        ],
        lambda_sigs: vec![RirLambdaSig {
            id: sig,
            params: vec![],
            ret: void,
        }],
        ..RirProgram::default()
    };

    let errors = rir::verify(&program).expect_err("verified lambda map-key type");
    assert!(errors.iter().any(|error| {
        error.site == RirVerifySite::Type(map_ty)
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
                            args: vec![RirCallArg::Value(RirOperand::Place(RirPlace::local(
                                f,
                                vec![],
                                lambda_ty,
                            )))],
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
        RirCallArg::MutPlace(RirMutPlaceArg::local(rir_place(local, int))),
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
        RirCallArg::MutPlace(RirMutPlaceArg::local(rir_place(local, int))),
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
fn rir_rejects_projected_mut_borrow_arg() {
    let void = RirTypeId::from_index(0);
    let int = RirTypeId::from_index(1);
    let pair_ty = RirTypeId::from_index(2);
    let pair = RirTupleId::from_index(0);
    let field = RirFieldId::from_index(0);
    let local = RirLocalId::from_index(0);
    let callee = RirFunctionId::from_index(0);
    let caller = RirFunctionId::from_index(1);
    let program = RirProgram {
        types: vec![RirType::Void, RirType::Int, RirType::Tuple(pair)],
        tuples: vec![RirTuple {
            id: pair,
            symbol: RirSymbol::new("Pair"),
            display: RirSymbol::new("(int)"),
            copyable: true,
            fields: vec![RirField {
                id: field,
                symbol: RirSymbol::new("_0"),
                ty: int,
            }],
        }],
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
                vec![],
                vec![rir_local(local, pair_ty, true, "pair")],
                vec![RirStmt::Eval(RirRValue::Call {
                    callee: RirCallTarget::Function(callee),
                    args: vec![RirCallArg::MutBorrow(RirPlace::local(
                        local,
                        vec![RirProjection::TupleField(field)],
                        int,
                    ))],
                    ty: void,
                })],
            ),
        ],
        ..RirProgram::default()
    };

    assert_rir_error(program, RirVerifyErrorKind::CallArgMode);
}

#[test]
fn rir_rejects_payload_ref_mut_borrow_arg() {
    let void = RirTypeId::from_index(0);
    let int = RirTypeId::from_index(1);
    let local = RirLocalId::from_index(0);
    let mut payload = rir_local(local, int, true, "payload");
    payload.payload_ref = true;
    let program = RirProgram {
        types: vec![RirType::Void, RirType::Int],
        functions: vec![
            mut_borrow_sink_function(RirFunctionId::from_index(0)),
            rir_function(
                RirFunctionId::from_index(1),
                void,
                vec![],
                vec![payload],
                vec![RirStmt::Eval(RirRValue::Call {
                    callee: RirCallTarget::Function(RirFunctionId::from_index(0)),
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
fn rir_accepts_projected_local_field_mut_place_arg() {
    let void = RirTypeId::from_index(0);
    let int = RirTypeId::from_index(1);
    let pair_ty = RirTypeId::from_index(2);
    let pair = RirTupleId::from_index(0);
    let field = RirFieldId::from_index(0);
    let local = RirLocalId::from_index(0);
    let program = RirProgram {
        types: vec![RirType::Void, RirType::Int, RirType::Tuple(pair)],
        tuples: vec![RirTuple {
            id: pair,
            symbol: RirSymbol::new("Pair"),
            display: RirSymbol::new("Pair"),
            copyable: true,
            fields: vec![RirField {
                id: field,
                symbol: RirSymbol::new("_0"),
                ty: int,
            }],
        }],
        functions: vec![
            rir_function(
                RirFunctionId::from_index(0),
                void,
                vec![],
                vec![rir_local(local, pair_ty, true, "pair")],
                vec![RirStmt::Eval(RirRValue::Call {
                    callee: RirCallTarget::Function(RirFunctionId::from_index(1)),
                    args: vec![RirCallArg::MutPlace(RirMutPlaceArg::projected(
                        RirMutPlaceHandle::Local { local, ty: pair_ty },
                        vec![RirProjection::TupleField(field)],
                        int,
                    ))],
                    ty: void,
                })],
            ),
            mut_place_sink_function(RirFunctionId::from_index(1)),
        ],
        ..RirProgram::default()
    };

    rir::verify(&program).expect("projected local mut-place arg should verify");
}

#[test]
fn rir_accepts_projected_param_mut_place_arg() {
    let void = RirTypeId::from_index(0);
    let int = RirTypeId::from_index(1);
    let pair_ty = RirTypeId::from_index(2);
    let pair = RirTupleId::from_index(0);
    let field = RirFieldId::from_index(0);
    let local = RirLocalId::from_index(0);
    let program = RirProgram {
        types: vec![RirType::Void, RirType::Int, RirType::Tuple(pair)],
        tuples: vec![RirTuple {
            id: pair,
            symbol: RirSymbol::new("Pair"),
            display: RirSymbol::new("Pair"),
            copyable: true,
            fields: vec![RirField {
                id: field,
                symbol: RirSymbol::new("_0"),
                ty: int,
            }],
        }],
        functions: vec![
            rir_function(
                RirFunctionId::from_index(0),
                void,
                vec![rir_param(
                    local,
                    pair_ty,
                    RirParamSemantic::MutPlace,
                    RirParamAbi::MutPlace,
                )],
                vec![rir_local(local, pair_ty, true, "pair")],
                vec![RirStmt::Eval(RirRValue::Call {
                    callee: RirCallTarget::Function(RirFunctionId::from_index(1)),
                    args: vec![RirCallArg::MutPlace(RirMutPlaceArg::projected(
                        RirMutPlaceHandle::Param { local, ty: pair_ty },
                        vec![RirProjection::TupleField(field)],
                        int,
                    ))],
                    ty: void,
                })],
            ),
            mut_place_sink_function(RirFunctionId::from_index(1)),
        ],
        ..RirProgram::default()
    };

    rir::verify(&program).expect("projected param mut-place arg should verify");
}

#[test]
fn rir_accepts_projected_stack_cell_mut_place_arg() {
    let program = projected_cell_mut_place_arg_program(RirMutPlaceHandle::StackCell {
        cell: RirCellRef::Owner(RirCellId::from_index(0)),
        ty: RirTypeId::from_index(2),
    });

    rir::verify(&program).expect("projected stack-cell mut-place arg should verify");
}

#[test]
fn rir_accepts_projected_heap_cell_mut_place_arg() {
    let program = projected_cell_mut_place_arg_program(RirMutPlaceHandle::HeapCell {
        cell: RirCellRef::Owner(RirCellId::from_index(0)),
        ty: RirTypeId::from_index(2),
    });

    rir::verify(&program).expect("projected heap-cell mut-place arg should verify");
}

#[test]
fn rir_accepts_projected_scoped_place_cell_mut_place_arg() {
    let program = projected_scoped_place_cell_mut_place_arg_program();

    rir::verify(&program).expect("projected scoped-place-cell mut-place arg should verify");
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
fn rir_rejects_dynamic_projected_cell_mut_place_arg() {
    let void = RirTypeId::from_index(0);
    let int = RirTypeId::from_index(1);
    let list = RirTypeId::from_index(2);
    let index = RirLocalId::from_index(1);
    let mut program = RirProgram {
        types: vec![RirType::Void, RirType::Int, RirType::List(int)],
        collection_storages: vec![rir_list_storage(list, int)],
        cells: vec![RirCellDecl {
            payload_ty: list,
            ..valid_stack_cell_decl()
        }],
        functions: vec![rir_function(
            RirFunctionId::from_index(0),
            void,
            vec![],
            vec![
                rir_local(RirLocalId::from_index(0), list, true, "source"),
                rir_local(index, int, false, "i"),
            ],
            vec![
                RirStmt::CellInit {
                    cell: RirCellRef::Owner(RirCellId::from_index(0)),
                    value: RirRValue::List {
                        ty: list,
                        elems: vec![],
                    },
                },
                RirStmt::Eval(RirRValue::Call {
                    callee: RirCallTarget::Function(RirFunctionId::from_index(1)),
                    args: vec![RirCallArg::MutPlace(RirMutPlaceArg::projected(
                        RirMutPlaceHandle::StackCell {
                            cell: RirCellRef::Owner(RirCellId::from_index(0)),
                            ty: list,
                        },
                        vec![RirProjection::Index(index)],
                        int,
                    ))],
                    ty: void,
                }),
            ],
        )],
        ..RirProgram::default()
    };
    program
        .functions
        .push(mut_place_sink_function(RirFunctionId::from_index(1)));

    assert_rir_error(program, RirVerifyErrorKind::UnsupportedRValueType);
}

#[test]
fn rir_accepts_single_dynamic_projected_param_mut_place_arg() {
    let void = RirTypeId::from_index(0);
    let int = RirTypeId::from_index(1);
    let list = RirTypeId::from_index(2);
    let xs = RirLocalId::from_index(0);
    let index = RirLocalId::from_index(1);
    let program = RirProgram {
        types: vec![RirType::Void, RirType::Int, RirType::List(int)],
        collection_storages: vec![rir_list_storage(list, int)],
        functions: vec![
            rir_function(
                RirFunctionId::from_index(0),
                void,
                vec![rir_param(
                    xs,
                    list,
                    RirParamSemantic::MutPlace,
                    RirParamAbi::MutPlace,
                )],
                vec![
                    rir_local(xs, list, true, "xs"),
                    rir_local(index, int, false, "i"),
                ],
                vec![RirStmt::Eval(RirRValue::Call {
                    callee: RirCallTarget::Function(RirFunctionId::from_index(1)),
                    args: vec![RirCallArg::MutPlace(RirMutPlaceArg::projected(
                        RirMutPlaceHandle::Param {
                            local: xs,
                            ty: list,
                        },
                        vec![RirProjection::Index(index)],
                        int,
                    ))],
                    ty: void,
                })],
            ),
            mut_place_sink_function(RirFunctionId::from_index(1)),
        ],
        ..RirProgram::default()
    };

    rir::verify(&program).expect("single dynamic projected param should verify");
}

#[test]
fn rir_accepts_multi_dynamic_projected_mut_place_arg() {
    let void = RirTypeId::from_index(0);
    let int = RirTypeId::from_index(1);
    let list = RirTypeId::from_index(2);
    let nested = RirTypeId::from_index(3);
    let xs = RirLocalId::from_index(0);
    let outer = RirLocalId::from_index(1);
    let inner = RirLocalId::from_index(2);
    let program = RirProgram {
        types: vec![
            RirType::Void,
            RirType::Int,
            RirType::List(int),
            RirType::List(list),
        ],
        collection_storages: vec![
            rir_list_storage_id(0, list, int),
            rir_list_storage_id(1, nested, list),
        ],
        functions: vec![
            rir_function(
                RirFunctionId::from_index(0),
                void,
                vec![],
                vec![
                    rir_local(xs, nested, true, "xs"),
                    rir_local(outer, int, false, "i"),
                    rir_local(inner, int, false, "j"),
                ],
                vec![RirStmt::Eval(RirRValue::Call {
                    callee: RirCallTarget::Function(RirFunctionId::from_index(1)),
                    args: vec![RirCallArg::MutPlace(RirMutPlaceArg::projected(
                        RirMutPlaceHandle::Local {
                            local: xs,
                            ty: nested,
                        },
                        vec![RirProjection::Index(outer), RirProjection::Index(inner)],
                        int,
                    ))],
                    ty: void,
                })],
            ),
            mut_place_sink_function(RirFunctionId::from_index(1)),
        ],
        ..RirProgram::default()
    };

    rir::verify(&program).expect("multi-dynamic projected mut-place arg should verify");
}

#[test]
fn rir_accepts_collection_loan_scope_around_loop() {
    let void = RirTypeId::from_index(0);
    let int = RirTypeId::from_index(1);
    let list = RirTypeId::from_index(2);
    let xs = RirLocalId::from_index(0);
    let loop_id = RirLoopId::from_index(0);
    let program = RirProgram {
        types: vec![RirType::Void, RirType::Int, RirType::List(int)],
        collection_storages: vec![rir_list_storage(list, int)],
        functions: vec![rir_function(
            RirFunctionId::from_index(0),
            void,
            vec![rir_param(
                xs,
                list,
                RirParamSemantic::Value,
                RirParamAbi::Value,
            )],
            vec![rir_local(xs, list, true, "xs")],
            vec![RirStmt::CollectionLoanScope(RirCollectionLoanScope {
                root: RirCollectionAccess::Direct(rir_place(xs, list)),
                root_kind: RirCollectionRootKind::List,
                mode: RirCollectionLoanMode::ReadonlySequence,
                body: RirStructuredBlock {
                    stmts: vec![RirStmt::Loop(RirLoop {
                        id: loop_id,
                        body: RirStructuredBlock::default(),
                    })],
                    term: RirTerm::None,
                },
            })],
        )],
        ..RirProgram::default()
    };

    rir::verify(&program).expect("valid collection loan scope rejected");
}

#[test]
fn rir_rejects_direct_mut_place_param_collection_loan() {
    let void = RirTypeId::from_index(0);
    let int = RirTypeId::from_index(1);
    let list = RirTypeId::from_index(2);
    let xs = RirLocalId::from_index(0);
    let program = RirProgram {
        types: vec![RirType::Void, RirType::Int, RirType::List(int)],
        collection_storages: vec![rir_list_storage(list, int)],
        functions: vec![rir_function(
            RirFunctionId::from_index(0),
            void,
            vec![rir_param(
                xs,
                list,
                RirParamSemantic::MutPlace,
                RirParamAbi::MutPlace,
            )],
            vec![rir_local(xs, list, true, "xs")],
            vec![RirStmt::CollectionLoanScope(RirCollectionLoanScope {
                root: RirCollectionAccess::Direct(rir_place(xs, list)),
                root_kind: RirCollectionRootKind::List,
                mode: RirCollectionLoanMode::ReadonlySequence,
                body: RirStructuredBlock::default(),
            })],
        )],
        ..RirProgram::default()
    };

    assert_rir_error(program, RirVerifyErrorKind::UnsupportedRValueType);
}

#[test]
fn rir_rejects_collection_loan_mode_root_mismatch() {
    let void = RirTypeId::from_index(0);
    let int = RirTypeId::from_index(1);
    let list = RirTypeId::from_index(2);
    let xs = RirLocalId::from_index(0);
    let program = RirProgram {
        types: vec![RirType::Void, RirType::Int, RirType::List(int)],
        collection_storages: vec![rir_list_storage(list, int)],
        functions: vec![rir_function(
            RirFunctionId::from_index(0),
            void,
            vec![rir_param(
                xs,
                list,
                RirParamSemantic::Value,
                RirParamAbi::Value,
            )],
            vec![rir_local(xs, list, true, "xs")],
            vec![RirStmt::CollectionLoanScope(RirCollectionLoanScope {
                root: RirCollectionAccess::Direct(rir_place(xs, list)),
                root_kind: RirCollectionRootKind::Map,
                mode: RirCollectionLoanMode::ReadonlyMap,
                body: RirStructuredBlock::default(),
            })],
        )],
        ..RirProgram::default()
    };

    assert_rir_error(program, RirVerifyErrorKind::UnsupportedRValueType);
}

#[test]
fn rir_accepts_map_slot_read_without_active_loan() {
    let void = RirTypeId::from_index(0);
    let int = RirTypeId::from_index(1);
    let map = RirTypeId::from_index(2);
    let xs = RirLocalId::from_index(0);
    let index = RirLocalId::from_index(1);
    let program = RirProgram {
        types: vec![
            RirType::Void,
            RirType::Int,
            RirType::Map {
                key: int,
                value: int,
            },
        ],
        collection_storages: vec![rir_map_storage(0, map, int, int)],
        functions: vec![rir_function(
            RirFunctionId::from_index(0),
            void,
            vec![
                rir_param(xs, map, RirParamSemantic::Value, RirParamAbi::Value),
                rir_param(index, int, RirParamSemantic::Value, RirParamAbi::Value),
            ],
            vec![
                rir_local(xs, map, false, "xs"),
                rir_local(index, int, false, "index"),
            ],
            vec![RirStmt::Eval(RirRValue::MapValueAt {
                map: RirCollectionAccess::Direct(rir_place(xs, map)),
                index,
                ty: int,
            })],
        )],
        ..RirProgram::default()
    };

    rir::verify(&program).expect("RIR verify failed");
}

#[test]
fn rir_rejects_sequence_slot_write_under_readonly_loan() {
    let void = RirTypeId::from_index(0);
    let int = RirTypeId::from_index(1);
    let list = RirTypeId::from_index(2);
    let xs = RirLocalId::from_index(0);
    let index = RirLocalId::from_index(1);
    let value = RirLocalId::from_index(2);
    let program = RirProgram {
        types: vec![RirType::Void, RirType::Int, RirType::List(int)],
        collection_storages: vec![rir_list_storage(list, int)],
        functions: vec![rir_function(
            RirFunctionId::from_index(0),
            void,
            vec![],
            vec![
                rir_local(xs, list, true, "xs"),
                rir_local(index, int, false, "index"),
                rir_local(value, int, false, "value"),
            ],
            vec![RirStmt::CollectionLoanScope(RirCollectionLoanScope {
                root: RirCollectionAccess::Direct(rir_place(xs, list)),
                root_kind: RirCollectionRootKind::List,
                mode: RirCollectionLoanMode::ReadonlySequence,
                body: RirStructuredBlock {
                    stmts: vec![RirStmt::SequenceSlotSet {
                        collection: RirCollectionAccess::Direct(rir_place(xs, list)),
                        index,
                        value: RirOperand::Place(rir_place(value, int)),
                    }],
                    term: RirTerm::None,
                },
            })],
        )],
        ..RirProgram::default()
    };

    assert_rir_error(program, RirVerifyErrorKind::UnsupportedRValueType);
}

#[test]
fn rir_rejects_map_slot_write_under_readonly_loan() {
    let void = RirTypeId::from_index(0);
    let int = RirTypeId::from_index(1);
    let map = RirTypeId::from_index(2);
    let xs = RirLocalId::from_index(0);
    let index = RirLocalId::from_index(1);
    let value = RirLocalId::from_index(2);
    let program = RirProgram {
        types: vec![
            RirType::Void,
            RirType::Int,
            RirType::Map {
                key: int,
                value: int,
            },
        ],
        collection_storages: vec![rir_map_storage(0, map, int, int)],
        functions: vec![rir_function(
            RirFunctionId::from_index(0),
            void,
            vec![],
            vec![
                rir_local(xs, map, true, "xs"),
                rir_local(index, int, false, "index"),
                rir_local(value, int, false, "value"),
            ],
            vec![RirStmt::CollectionLoanScope(RirCollectionLoanScope {
                root: RirCollectionAccess::Direct(rir_place(xs, map)),
                root_kind: RirCollectionRootKind::Map,
                mode: RirCollectionLoanMode::ReadonlyMap,
                body: RirStructuredBlock {
                    stmts: vec![RirStmt::MapValueSet {
                        map: RirCollectionAccess::Direct(rir_place(xs, map)),
                        index,
                        value: RirOperand::Place(rir_place(value, int)),
                    }],
                    term: RirTerm::None,
                },
            })],
        )],
        ..RirProgram::default()
    };

    assert_rir_error(program, RirVerifyErrorKind::UnsupportedRValueType);
}

#[test]
fn rir_rejects_mutable_collection_loan_on_immutable_root() {
    let void = RirTypeId::from_index(0);
    let int = RirTypeId::from_index(1);
    let list = RirTypeId::from_index(2);
    let xs = RirLocalId::from_index(0);
    let program = RirProgram {
        types: vec![RirType::Void, RirType::Int, RirType::List(int)],
        collection_storages: vec![rir_list_storage(list, int)],
        functions: vec![rir_function(
            RirFunctionId::from_index(0),
            void,
            vec![rir_param(
                xs,
                list,
                RirParamSemantic::Value,
                RirParamAbi::Value,
            )],
            vec![rir_local(xs, list, false, "xs")],
            vec![RirStmt::CollectionLoanScope(RirCollectionLoanScope {
                root: RirCollectionAccess::Direct(rir_place(xs, list)),
                root_kind: RirCollectionRootKind::List,
                mode: RirCollectionLoanMode::MutableSequenceElement,
                body: RirStructuredBlock::default(),
            })],
        )],
        ..RirProgram::default()
    };

    assert_rir_error(program, RirVerifyErrorKind::ImmutableAssign);
}

#[test]
fn rir_accepts_mutable_global_collection_loan() {
    let program = global_list_collection_loan_rir(
        RirCollectionLoanMode::MutableSequenceElement,
        vec![],
        vec![],
        RirStructuredBlock::default(),
    );

    rir::verify(&program).expect("mutable global collection loans should verify");
}

#[test]
fn rir_rejects_global_root_replacement_during_active_loan() {
    let list = RirTypeId::from_index(2);
    let global = RirGlobalId::from_index(0);
    let replacement = RirLocalId::from_index(0);
    let mut replacement_local = rir_local(replacement, list, true, "replacement");
    replacement_local.initialized = false;
    let program = global_list_collection_loan_rir(
        RirCollectionLoanMode::ReadonlySequence,
        vec![replacement_local],
        vec![],
        RirStructuredBlock {
            stmts: vec![
                RirStmt::Init {
                    local: replacement,
                    value: RirRValue::List {
                        ty: list,
                        elems: vec![],
                    },
                },
                RirStmt::GlobalSetRoot {
                    global,
                    value: RirRValue::Use(RirOperand::Place(rir_place(replacement, list))),
                },
            ],
            term: RirTerm::None,
        },
    );

    assert_rir_error(program, RirVerifyErrorKind::UnsupportedRValueType);
}

#[test]
fn rir_rejects_global_mut_place_set_during_active_loan() {
    let list = RirTypeId::from_index(2);
    let global = RirGlobalId::from_index(0);
    let replacement = RirLocalId::from_index(0);
    let mut replacement_local = rir_local(replacement, list, true, "replacement");
    replacement_local.initialized = false;
    let program = global_list_collection_loan_rir(
        RirCollectionLoanMode::ReadonlySequence,
        vec![replacement_local],
        vec![],
        RirStructuredBlock {
            stmts: vec![
                RirStmt::Init {
                    local: replacement,
                    value: RirRValue::List {
                        ty: list,
                        elems: vec![],
                    },
                },
                RirStmt::MutPlaceSet {
                    place: RirMutPlaceArg::global(global, list),
                    value: RirRValue::Use(RirOperand::Place(rir_place(replacement, list))),
                },
            ],
            term: RirTerm::None,
        },
    );

    assert_rir_error(program, RirVerifyErrorKind::UnsupportedRValueType);
}

#[test]
fn rir_rejects_dynamic_global_collection_loan() {
    let int = RirTypeId::from_index(0);
    let void = RirTypeId::from_index(1);
    let row = RirTypeId::from_index(2);
    let rows = RirTypeId::from_index(3);
    let global = RirGlobalId::from_index(0);
    let init = RirFunctionId::from_index(0);
    let main = RirFunctionId::from_index(1);
    let xs = RirLocalId::from_index(0);
    let index = RirLocalId::from_index(0);
    let zero = RirConstId::from_index(0);
    let mut init_local = rir_local(xs, rows, true, "xs");
    init_local.initialized = false;
    let mut program = RirProgram {
        types: vec![
            RirType::Int,
            RirType::Void,
            RirType::List(int),
            RirType::List(row),
        ],
        collection_storages: vec![
            rir_list_storage(row, int),
            rir_list_storage_id(1, rows, row),
        ],
        consts: vec![RirConst {
            id: zero,
            ty: int,
            value: RirConstValue::Int(0),
        }],
        globals: vec![RirGlobal {
            id: global,
            air_id: air::GlobalId::from_index(0),
            module: air::ModuleId::from_index(0),
            name: RirSymbol::new("game.rows"),
            slot_symbol: RirSymbol::new("g0_rows"),
            ty: rows,
            mutable: true,
            init,
        }],
        functions: vec![
            rir_function(
                init,
                rows,
                vec![],
                vec![init_local],
                vec![RirStmt::Init {
                    local: xs,
                    value: RirRValue::List {
                        ty: rows,
                        elems: vec![],
                    },
                }],
            ),
            rir_function(
                main,
                void,
                vec![],
                vec![rir_local(index, int, false, "i")],
                vec![
                    RirStmt::GlobalEnsure { global },
                    RirStmt::Init {
                        local: index,
                        value: RirRValue::Use(RirOperand::Const(zero)),
                    },
                    RirStmt::CollectionLoanScope(RirCollectionLoanScope {
                        root: RirCollectionAccess::Direct(RirPlace::global(
                            global,
                            vec![RirProjection::Index(index)],
                            row,
                        )),
                        root_kind: RirCollectionRootKind::List,
                        mode: RirCollectionLoanMode::ReadonlySequence,
                        body: RirStructuredBlock::default(),
                    }),
                ],
            ),
        ],
        ..RirProgram::default()
    };
    program.functions[0].body.term = RirTerm::Return(Some(RirOperand::Place(rir_place(xs, rows))));

    assert_rir_error(program, RirVerifyErrorKind::UnsupportedRValueType);
}

#[test]
fn rir_rejects_active_collection_root_assignment() {
    let void = RirTypeId::from_index(0);
    let int = RirTypeId::from_index(1);
    let list = RirTypeId::from_index(2);
    let xs = RirLocalId::from_index(0);
    let ys = RirLocalId::from_index(1);
    let program = RirProgram {
        types: vec![RirType::Void, RirType::Int, RirType::List(int)],
        collection_storages: vec![rir_list_storage(list, int)],
        functions: vec![rir_function(
            RirFunctionId::from_index(0),
            void,
            vec![],
            vec![
                rir_local(xs, list, true, "xs"),
                rir_local(ys, list, true, "ys"),
            ],
            vec![RirStmt::CollectionLoanScope(RirCollectionLoanScope {
                root: RirCollectionAccess::Direct(rir_place(xs, list)),
                root_kind: RirCollectionRootKind::List,
                mode: RirCollectionLoanMode::ReadonlySequence,
                body: RirStructuredBlock {
                    stmts: vec![RirStmt::Assign {
                        dst: rir_place(xs, list),
                        value: RirRValue::Use(RirOperand::Place(rir_place(ys, list))),
                    }],
                    term: RirTerm::None,
                },
            })],
        )],
        ..RirProgram::default()
    };

    assert_rir_error(program, RirVerifyErrorKind::UnsupportedRValueType);
}

#[test]
fn rir_accepts_slice_view_rvalue() {
    let int = RirTypeId::from_index(0);
    let void = RirTypeId::from_index(1);
    let array = RirTypeId::from_index(2);
    let slice = RirTypeId::from_index(3);
    let source = RirLocalId::from_index(0);
    let start = RirLocalId::from_index(1);
    let end = RirLocalId::from_index(2);
    let program = RirProgram {
        types: vec![
            RirType::Int,
            RirType::Void,
            RirType::Array { elem: int, len: 2 },
            RirType::Slice(int),
        ],
        functions: vec![rir_function(
            RirFunctionId::from_index(0),
            void,
            vec![],
            vec![
                rir_local(source, array, true, "xs"),
                rir_local(start, int, true, "start"),
                rir_local(end, int, true, "end"),
            ],
            vec![RirStmt::Eval(RirRValue::SliceView {
                source: rir_place(source, array),
                start,
                end,
                inclusive: false,
                mutable: false,
                ty: slice,
            })],
        )],
        ..RirProgram::default()
    };

    rir::verify(&program).expect("slice view rvalue should verify");
}

#[test]
fn slice_type_uses_runtime_descriptor_not_raw_slice() {
    let int = RirTypeId::from_index(0);
    let slice = RirTypeId::from_index(1);
    let program = RirProgram {
        types: vec![RirType::Int, RirType::Slice(int)],
        ..RirProgram::default()
    };

    assert_eq!(
        RirRustRepPolicy::new(&program).rust_ty(slice),
        "anvyx_runtime::AnvSlice<'cx, i64>"
    );
}

#[test]
fn rir_rejects_mut_place_operand_inside_short_region() {
    let void = RirTypeId::from_index(0);
    let int = RirTypeId::from_index(1);
    let map = RirTypeId::from_index(2);
    let option = RirTypeId::from_index(3);
    let xs = RirLocalId::from_index(0);
    let key = RirLocalId::from_index(1);
    let program = RirProgram {
        types: vec![
            RirType::Void,
            RirType::Int,
            RirType::Map {
                key: int,
                value: int,
            },
            RirType::Option(int),
        ],
        collection_storages: vec![rir_map_storage(0, map, int, int)],
        functions: vec![rir_function(
            RirFunctionId::from_index(0),
            void,
            vec![rir_param(
                key,
                int,
                RirParamSemantic::MutPlace,
                RirParamAbi::MutPlace,
            )],
            vec![
                rir_local(xs, map, true, "xs"),
                rir_local(key, int, true, "key"),
            ],
            vec![RirStmt::Eval(RirRValue::MapGet {
                map: RirCollectionAccess::MutPlace(RirMutPlaceArg::local(rir_place(xs, map))),
                key: RirOperand::Place(rir_place(key, int)),
                ty: option,
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
            value: RirOperand::Place(RirPlace::local(RirLocalId::from_index(0), vec![], int)),
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
            value: RirOperand::Place(RirPlace::local(RirLocalId::from_index(0), vec![], string)),
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
            place: RirPlace::local(
                RirLocalId::from_index(0),
                vec![RirProjection::TupleField(field)],
                int,
            ),
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
        value: RirRValue::Use(RirOperand::Place(RirPlace::local(
            RirLocalId::from_index(1),
            vec![],
            lambda_ty,
        ))),
    });

    rir::verify(&program).expect("RIR verify failed");
}

#[test]
fn mut_borrow_lambda_signature_remains_noncopyable() {
    let program = mut_borrow_lambda_rir();

    assert!(!RirRustRepPolicy::new(&program).lambda_sig_copyable(RirLambdaSigId::from_index(0)));
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
        value: RirRValue::Use(RirOperand::Place(RirPlace::local(
            RirLocalId::from_index(1),
            vec![],
            lambda_ty,
        ))),
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
            args: vec![RirCallArg::Value(RirOperand::Place(RirPlace::local(
                RirLocalId::from_index(1),
                vec![],
                lambda_ty,
            )))],
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

    assert!(!RirRustRepPolicy::new(&program).lambda_sig_copyable(sig));
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
                    term: RirTerm::Return(Some(RirOperand::Place(RirPlace::local(
                        f,
                        vec![],
                        lambda_ty,
                    )))),
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
                        callee: RirOperand::Place(RirPlace::local(local, vec![], lambda_ty)),
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
    assert!(text.contains("fn call<'cx, 'rt>(self, rt: &mut anvyx_runtime::Ctx<'cx, 'rt>, types: &AnvTypes<'cx>, globals: &AnvGlobals<'cx>)"));
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
                dst: RirPlace::local(hidden, vec![], int),
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
                            place: RirPlace::local(capture, vec![], int),
                        }],
                        ty: lambda_ty,
                    },
                },
                RirStmt::Eval(RirRValue::Call {
                    callee: RirCallTarget::LambdaValue {
                        callee: RirOperand::Place(RirPlace::local(f, vec![], lambda_ty)),
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

    check(program);
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
        value: RirOperand::Place(RirPlace::local(
            RirLocalId::from_index(0),
            vec![],
            lambda_ty,
        )),
    };

    let verified = rir::verify(&program).expect("RIR verify failed");
    let source = emit::emit(&verified).into_string();

    assert!(source.contains("lambda_env0: heap.register_tracked::<LambdaEnv0"));
    assert!(source.contains("#[derive(Clone, anvyx_runtime::Trace)]\n#[trace(crate = anvyx_runtime, ctx = 'cx)]\nstruct LambdaEnv0<'cx>"));
    assert!(source.contains("c0: LambdaSig0<'cx>,"));
    assert!(source.contains("#[derive(Clone)]\nenum LambdaSig0<'cx>"));
    assert!(source.contains("unsafe impl<'cx> anvyx_runtime::Trace<'cx> for LambdaSig0<'cx>"));
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
    assert!(source.contains("rt.heap().alloc(heap_type, LambdaEnv0 { c0:"));
    assert!(source.contains("L0 { env: anvyx_runtime::Handle<'cx, LambdaEnv0> }"));
    let materialize = source
        .find("let c0 = rt.heap().with(env, |env|")
        .expect("missing env field materialization");
    let call = source[materialize..]
        .find("(rt, types, globals, c0)")
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

    check(program);
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
        layout: None,
        materialization: None,
        owns_heap_edges: None,
        has_init: false,
        init_args: vec![],
        fields: vec![],
        variants: vec![],
        variant_abis: vec![],
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
        abi: air::ExternAbi::default(),
        binding: None,
        effects: anvyx_runtime::ExternEffects::default(),
    });
    program.module_mut(module).externs.push(id);

    let errors = profile_errors(program);
    assert!(has_error(&errors, ProfileErrorKind::UnsupportedExtern));
}

#[test]
fn plan_maps_inline_extern_field_ids_to_storage_fields() {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let module = program.alloc_module(air_module(&["host"]));
    let ext_id = air::ExternTypeId::from_index(0);
    let ext_ty = program.alloc_type(TypeData::Extern(ext_id));
    let module_path = anvyx_runtime::ModulePath {
        segments: vec!["host".to_string()],
    };
    let type_key = anvyx_runtime::ExternTypeKey {
        module: module_path.clone(),
        name: "Host".to_string(),
    };
    let receiver = |mode| ExternReceiverDecl { ty: ext_ty, mode };
    let field = |name, computed| ExternFieldDecl {
        name: Ident::new(name),
        ty: int,
        abi: anvyx_runtime::ExternTypeExpr::Int,
        get_receiver: receiver(ParamMode::SharedBorrow),
        set_receiver: receiver(ParamMode::MutBorrow),
        computed,
        readable: true,
        writable: true,
    };

    assert_eq!(
        program.alloc_extern_type(ExternTypeDecl {
            name: Ident::new("Host"),
            module,
            binding: Some(ExternTypeBindingDecl {
                package: anvyx_frontend::resolve::PackageId::core(),
                provider: anvyx_runtime::ProviderId {
                    name: "host".to_string(),
                },
                key: type_key.clone(),
            }),
            type_args: vec![],
            const_args: vec![],
            rep: ExternRep::Inline,
            layout: None,
            materialization: None,
            owns_heap_edges: None,
            has_init: false,
            init_args: vec![],
            fields: vec![field("computed", true), field("direct", false)],
            variants: vec![],
            variant_abis: vec![],
            methods: vec![],
            statics: vec![],
            operators: vec![],
        }),
        ext_id
    );
    program.module_mut(module).extern_types.push(ext_id);

    let arg = air::LocalId::from_index(0);
    let function = program.alloc_function(Function {
        name: Ident::new("read"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![param("value", ext_ty, ParamMode::Value, arg)], int),
        locals: vec![local(ext_ty, LocalKind::Arg)],
        body: structured_body(
            vec![],
            air::AirTail::Return(Some(Operand::Place(Place {
                root: PlaceRoot::Local(arg),
                projection: vec![Projection::Field(air::FieldId::from_index(1))],
                ty: int,
            }))),
        ),
    });
    program.module_mut(module).functions.push(function);

    let verified = air::verify(&program).expect("AIR verify failed");
    let config = RustPlanConfig {
        symbol_prefix: "anv".into(),
        native_providers: vec![anvyx_runtime::RustProviderSupport {
            package: "<core>".to_string(),
            provider: anvyx_runtime::ProviderId {
                name: "host".to_string(),
            },
            cargo: anvyx_runtime::RustProviderCargo::default(),
            modules: vec![anvyx_runtime::RustModuleSupport {
                module: module_path,
                types: vec![anvyx_runtime::RustTypeBinding {
                    key: type_key,
                    path: anvyx_runtime::RustPath {
                        crate_name: "host".to_string(),
                        segments: vec!["host".to_string(), "Host".to_string()],
                    },
                    owns_heap_edges: false,
                }],
                bindings: vec![],
            }],
        }],
    };
    let plan = plan(&verified, config).expect("plan failed");

    let RirTerm::Return(Some(RirOperand::Place(place))) = &plan.program().functions[0].body.term
    else {
        panic!("expected returned field place");
    };
    assert_eq!(
        place.projections,
        [RirProjection::Field(RirFieldId::from_index(0))]
    );
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
fn profile_accepts_lambda_callee_capture_cell_source_ref_arg() {
    let mut program = owner_capture_cell_source_ref_arg_program();
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
        unreachable!("test helper should end with the source ref call")
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
        unreachable!("test helper should start with source ref call")
    };
    *callee = Callee::Extern(ext);

    expect_reject(
        program,
        ProfileErrorKind::UnsupportedMutablePlaceNativeBoundary,
    );
}

#[test]
fn access_plan_accepts_direct_scalar_dataref_source_ref_arg() {
    let program = projected_mut_call_arg_program();
    let classes = TypePassClasses::analyze(&program);
    let function = FunctionId::from_index(1);
    let Statement::Eval(RValue::Call { args, .. }) =
        &program.function(function).body.block.stmts[0]
    else {
        unreachable!();
    };
    let CallArg::MutBorrow(place) = &args[0] else {
        unreachable!();
    };
    let plan = PlaceAccessCx::new(&program, &classes)
        .plan(function, PlaceAccessIntent::MutPlaceArg, place)
        .unwrap();

    assert!(plan.dataref_plan().is_some());
}

#[test]
fn profile_accepts_dataref_source_ref_arg() {
    check(projected_mut_call_arg_program());
}

#[test]
fn profile_accepts_capture_cell_dataref_source_ref_arg() {
    check(capture_cell_dataref_source_ref_arg_program());
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
fn plan_lowers_owner_heap_capture_cell_source_ref_arg_to_heap_cell() {
    let program = heap_capture_cell_source_ref_arg_program();
    let verified = air::verify(&program).expect("AIR verify failed");
    let plan = plan(&verified, rust_plan_config()).expect("plan failed");
    let caller = rir_function_for_air(plan.program(), FunctionId::from_index(2));
    let arg = only_call_arg(caller);

    assert!(matches!(
        arg,
        RirCallArg::MutPlace(RirMutPlaceArg {
            access: RirMutPlaceAccess::Handle(RirMutPlaceHandle::HeapCell { cell: RirCellRef::Owner(id), .. }),
            ..
        }) if *id == RirCellId::from_index(0)
    ));
}

#[test]
fn plan_lowers_lambda_heap_capture_cell_source_ref_arg_to_heap_cell() {
    let program = escaping_lambda_capture_cell_source_ref_arg_program();
    let verified = air::verify(&program).expect("AIR verify failed");
    let plan = plan(&verified, rust_plan_config()).expect("plan failed");
    let lambda_body = rir_function_for_air(plan.program(), FunctionId::from_index(1));
    let arg = only_call_arg(lambda_body);

    assert!(matches!(
        arg,
        RirCallArg::MutPlace(RirMutPlaceArg {
            access: RirMutPlaceAccess::Handle(RirMutPlaceHandle::HeapCell { cell: RirCellRef::Capture { cell, .. }, .. }),
            ..
        }) if *cell == RirCellId::from_index(0)
    ));
}

#[test]
fn plan_lowers_owner_capture_cell_source_ref_arg_to_stack_cell() {
    let program = owner_capture_cell_source_ref_arg_program();
    let verified = air::verify(&program).expect("AIR verify failed");
    let plan = plan(&verified, rust_plan_config()).expect("plan failed");
    let caller = rir_function_for_air(plan.program(), FunctionId::from_index(1));
    let arg = only_call_arg(caller);

    assert!(matches!(
        arg,
        RirCallArg::MutPlace(RirMutPlaceArg {
            access: RirMutPlaceAccess::Handle(RirMutPlaceHandle::StackCell { cell: RirCellRef::Owner(id), .. }),
            ..
        }) if *id == RirCellId::from_index(0)
    ));
}

#[test]
fn plan_lowers_lambda_capture_cell_source_ref_arg_to_stack_cell() {
    let program = lambda_capture_cell_source_ref_arg_program();
    let verified = air::verify(&program).expect("AIR verify failed");
    let plan = plan(&verified, rust_plan_config()).expect("plan failed");
    let lambda_body = rir_function_for_air(plan.program(), FunctionId::from_index(1));
    let arg = only_call_arg(lambda_body);

    assert!(matches!(
        arg,
        RirCallArg::MutPlace(RirMutPlaceArg {
            access: RirMutPlaceAccess::Handle(RirMutPlaceHandle::StackCell { cell: RirCellRef::Capture { cell, .. }, .. }),
            ..
        }) if *cell == RirCellId::from_index(0)
    ));
}

#[test]
fn plan_lowers_projected_owner_capture_cell_assignment_to_mut_place_set() {
    let program = capture_cell_projected_assignment_program(false, LambdaEscape::NonEscaping);
    let verified = air::verify(&program).expect("AIR verify failed");
    let plan = plan(&verified, rust_plan_config()).expect("plan failed");
    let owner = rir_function_for_air(plan.program(), FunctionId::from_index(0));

    assert!(owner.body.stmts.iter().any(|stmt| matches!(
        stmt,
        RirStmt::MutPlaceSet {
            place: RirMutPlaceArg {
                access: RirMutPlaceAccess::Handle(RirMutPlaceHandle::StackCell { cell: RirCellRef::Owner(id), .. }),
                projections,
                ..
            },
            ..
        } if *id == RirCellId::from_index(0) && projections.len() == 1
    )));
}

#[test]
fn plan_lowers_projected_lambda_capture_cell_assignment_to_mut_place_set() {
    let program = capture_cell_projected_assignment_program(true, LambdaEscape::NonEscaping);
    let verified = air::verify(&program).expect("AIR verify failed");
    let plan = plan(&verified, rust_plan_config()).expect("plan failed");
    let lambda_body = rir_function_for_air(plan.program(), FunctionId::from_index(0));

    assert!(lambda_body.body.stmts.iter().any(|stmt| matches!(
        stmt,
        RirStmt::MutPlaceSet {
            place: RirMutPlaceArg {
                access: RirMutPlaceAccess::Handle(RirMutPlaceHandle::StackCell { cell: RirCellRef::Capture { cell, .. }, .. }),
                projections,
                ..
            },
            ..
        } if *cell == RirCellId::from_index(0) && projections.len() == 1
    )));
}

#[test]
fn emit_uses_projected_mut_place_for_capture_cell_assignment_and_read() {
    let source = plan_source(capture_cell_projected_assignment_program(
        false,
        LambdaEscape::NonEscaping,
    ))
    .into_string();

    assert!(
        source.contains("anvyx_runtime::MutPlace::projected(anvyx_runtime::MutPlace::stack_cell")
    );
    assert!(source.contains(".set(rt,"));
    assert!(!source.contains("get_copy()?."));
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
        unreachable!("test helper should start with source ref call")
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
        rir.scoped_place_cells[0].source.root_local(),
        Some(RirLocalId::from_index(0))
    );
    assert!(matches!(
        rir.lambdas[0].captures[0].kind,
        RirLambdaCaptureKind::ScopedPlaceCell {
            cell: RirScopedPlaceCellId(0)
        }
    ));
}

#[test]
fn plan_preserves_ref_self_scoped_borrow_source() {
    let mut program = scoped_borrow_lambda_program();
    program.scoped_borrows[0].source = air::ScopedBorrowSource::RefSelf {
        local: air::LocalId::from_index(0),
    };
    let owner = program.function_mut(FunctionId::from_index(2));
    owner.kind = FunctionKind::Method;
    owner.signature.params[0].role = ParamRole::Receiver;
    let verified = air::verify(&program).expect("AIR verify failed");
    let plan = plan(&verified, rust_plan_config()).expect("plan failed");
    let rir = plan.program();

    assert!(matches!(
        &rir.scoped_place_cells[0].source,
        RirScopedPlaceSource::RefSelf { place }
            if place.root_local() == Some(RirLocalId(0)) && place.projections.is_empty()
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
        RirCallArg::MutPlace(RirMutPlaceArg {
            access: RirMutPlaceAccess::Handle(RirMutPlaceHandle::ScopedPlaceCell {
                cell: RirScopedPlaceCellRef::Capture {
                    cell: RirScopedPlaceCellId(0),
                    ..
                },
                ..
            }),
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
        RirCallArg::MutPlace(RirMutPlaceArg {
            access: RirMutPlaceAccess::Handle(RirMutPlaceHandle::ScopedPlaceCell {
                cell: RirScopedPlaceCellRef::Capture {
                    cell: RirScopedPlaceCellId(0),
                    local: RirLocalId(0),
                },
                ..
            }),
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
        .push(source_ref_call(
            callee,
            PlaceRoot::ScopedBorrow(air::ScopedBorrowId::from_index(0)),
            int,
        ));
    let verified = air::verify(&program).expect("AIR verify failed");
    let plan = plan(&verified, rust_plan_config()).expect("plan failed");
    let owner = rir_function_for_air(plan.program(), FunctionId::from_index(2));

    assert!(matches!(
        only_call_arg(owner),
        RirCallArg::MutPlace(RirMutPlaceArg {
            access: RirMutPlaceAccess::Handle(RirMutPlaceHandle::ScopedPlaceCell {
                cell: RirScopedPlaceCellRef::Owner(RirScopedPlaceCellId(0)),
                ..
            }),
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
fn emit_passes_capture_cell_ref_arg_as_stack_cell_mut_place() {
    let source = plan_source(lambda_capture_cell_source_ref_arg_program()).into_string();

    assert!(source.contains("MutPlace::stack_cell(&"));
    assert!(!source.contains("MutPlace::local(&mut"));
    assert!(!source.contains(".get_copy()?"));
}

#[test]
fn emit_passes_owner_heap_cell_ref_arg_as_heap_cell_mut_place() {
    let source = plan_source(heap_capture_cell_source_ref_arg_program()).into_string();

    assert!(source.contains(&target::mut_place_heap_cell("__cell0")));
    assert!(!source.contains("MutPlace::local(&mut"));
    assert!(!source.contains("__cell0.get_copy"));
    assert!(!source.contains("__cell0.set"));
    assert!(!source.contains("rt.heap().with(&__cell0, |cell| cell.get_copy())?"));
    assert!(!source.contains("rt.heap().with(&__cell0, |cell| cell.set"));
    assert!(!source.contains("rt.heap().with(&__cell0, |cell| bump"));
    assert!(!source.contains("StackLambdaCell"));
}

#[test]
fn emit_passes_capture_heap_cell_ref_arg_as_heap_cell_mut_place() {
    let source = plan_source(escaping_lambda_capture_cell_source_ref_arg_program()).into_string();

    assert!(source.contains(&target::mut_place_heap_cell("v0")));
    assert!(!source.contains("MutPlace::local(&mut"));
    assert!(!source.contains("v0.get_copy"));
    assert!(!source.contains("v0.set"));
    assert!(!source.contains("rt.heap().with(&v0, |cell| cell.get_copy())?"));
    assert!(!source.contains("rt.heap().with(&v0, |cell| cell.set"));
    assert!(!source.contains("rt.heap().with(&v0, |cell| bump"));
    assert!(!source.contains("StackLambdaCell"));
}

#[test]
fn emit_reentrant_heap_cell_ref_arg_call_is_not_wrapped_in_cell_borrow() {
    let source = plan_source(heap_capture_cell_reentrant_source_ref_arg_program()).into_string();

    assert!(source.contains("apply(rt, types, globals, anvyx_runtime::MutPlace::heap_cell("));
    assert!(!source.contains("rt.heap().with(&v0, |cell| apply"));
    assert!(!source.contains("cell.mutate(|value| apply"));
    assert!(!source.contains("rt.heap().with(&v0, |cell| cell.get_copy())?"));
    assert!(!source.contains("rt.heap().with(&v0, |cell| cell.set"));
}

#[test]
fn emit_temps_mut_place_read_before_heap_cell_set() {
    let source = plan_source(heap_cell_set_from_mut_place_param_program()).into_string();

    assert!(source.contains(".get_copy(rt)?;"));
    assert!(source.contains("rt.heap().with(&__cell0, |cell| cell.set(v"));
    assert!(!source.contains("rt.heap().with(&__cell0, |cell| cell.set(v0_x.get_copy(rt)?))?;"));
}

#[test]
fn emit_scoped_borrowed_param_uses_scoped_mut_place_cell() {
    let source = plan_source(scoped_borrow_lambda_program()).into_string();

    assert!(
        source.contains("let __scoped0 = anvyx_runtime::ScopedMutPlaceCell::new_with_safepoint(v0.reborrow(), rt.__anvyx_safepoint_state());")
    );
    assert!(source.contains("c0: &'env anvyx_runtime::ScopedMutPlaceCell<'env, 'cx, i64>"));
    assert!(source.contains("c0: &__scoped0"));
    assert!(source.contains(&target::mut_place_scoped_cell("v0")));
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
        .push(source_ref_call(
            FunctionId::from_index(0),
            PlaceRoot::ScopedBorrow(air::ScopedBorrowId::from_index(0)),
            int,
        ));
    let source = plan_source(program).into_string();

    assert!(source.contains(&target::mut_place_scoped_cell("&__scoped0")));
    assert!(!source.contains("MutPlace::local(&mut v0)"));
    assert!(
        source.contains("let __scoped0 = anvyx_runtime::ScopedMutPlaceCell::new_with_safepoint(v0.reborrow(), rt.__anvyx_safepoint_state());")
    );
}

#[test]
fn profile_and_plan_accept_direct_local_to_native_mut_borrow() {
    assert_local_kind_to_native_mut_borrow(LocalKind::User);
}

#[test]
fn profile_accepts_temp_to_native_mut_borrow() {
    assert_local_kind_to_native_mut_borrow(LocalKind::Temp);
}

fn assert_local_kind_to_native_mut_borrow(kind: LocalKind) {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let void = program.alloc_type(TypeData::Void);
    let ext = host_mut_extern(&mut program, int, void, "touch");
    let module = program.alloc_module(root_module());
    let x = air::LocalId::from_index(0);
    let one = int_const(&mut program, int, 1);
    let caller = caller_function(
        &mut program,
        module,
        Signature::new(vec![], void),
        vec![mut_local(int, kind)],
        vec![
            Statement::Init {
                local: x,
                value: RValue::Use(Operand::Const(one)),
            },
            call_mut_ext(ext, place(x, int)),
        ],
    );
    let verified = air::verify(&program).expect("AIR verify failed");
    let config = rust_plan_config();
    RustBackendProfile::check_with_native_support(&verified, &config.native_providers)
        .expect("profile rejected native mut borrow");

    let plan = plan(&verified, config).expect("plan failed");
    let caller = rir_function_for_air(plan.program(), caller);
    assert!(matches!(only_call_arg(caller), RirCallArg::MutBorrow(_)));
}

#[test]
fn plan_lowers_projected_local_to_native_mut_place() {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let void = program.alloc_type(TypeData::Void);
    let tuple = program.alloc_type(TypeData::Tuple(vec![int]));
    let ext = host_mut_extern(&mut program, int, void, "touch_place");
    let module = program.alloc_module(root_module());
    let pair = air::LocalId::from_index(0);
    let one = int_const(&mut program, int, 1);
    let caller = caller_function(
        &mut program,
        module,
        Signature::new(vec![], void),
        vec![mut_local(tuple, LocalKind::User)],
        vec![
            Statement::Init {
                local: pair,
                value: RValue::Aggregate {
                    kind: AggregateCtor::Tuple,
                    fields: vec![Operand::Const(one)],
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
    let verified = air::verify(&program).expect("AIR verify failed");
    let plan = plan(&verified, rust_plan_config()).expect("plan failed");
    let caller = rir_function_for_air(plan.program(), caller);

    assert!(matches!(
        only_call_arg(caller),
        RirCallArg::MutPlace(RirMutPlaceArg {
            access: RirMutPlaceAccess::Handle(RirMutPlaceHandle::Local { .. }),
            projections,
            ..
        }) if matches!(projections.as_slice(), [RirProjection::TupleField(_)])
    ));
    let source = emit::emit(&plan.verified()).into_string();
    assert!(source.contains("host::touch_place(rt,"));
    assert!(source.contains("MutPlace::projected("));
    assert!(!source.contains("host::touch_place(rt, &mut"));
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
                    fields: vec![Operand::Const(one)],
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
    assert!(source.contains("anv_f0_First_value(rt, types, globals,"));
    assert!(source.contains("anv_f1_Second_value(rt, types, globals,"));
    assert!(source.contains("anv_f2_First_value(rt, types, globals)"));
    assert!(source.contains("anv_f3_value(rt, types, globals)"));
    assert!(!source.contains("impl anvT"));
    assert!(!source.contains("trait "));
}

#[test]
fn emit_renders_format_with_central_specs_and_borrowed_strings() {
    let program = format_program();
    let source = plan_source(program).into_string();

    assert!(source.contains("anvyx_runtime::AnvString::from(format!(\"{:04}\", 7))"));
    assert!(source.contains("anvyx_runtime::AnvString::from(format!(\"{:*>5}\", v1.as_str()))"));
    assert!(source.contains("format!(\"{:.2}\","));
    assert!(source.contains("anvyx_runtime::AnvString::from(format!(\"{:X}\", 255))"));
    assert!(source.contains("anvyx_runtime::AnvString::from(format!(\"{:b}\", 5))"));
    assert!(source.contains("format!(\"{:e}\","));
    assert!(source.contains("format!(\"{:E}\","));
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
    assert!(source.contains("fn register(_heap: &mut anvyx_runtime::Heap<'cx>) -> Self"));
    assert!(source.contains("struct AnvGlobals<'cx>"));
    assert!(source.contains("struct AnvGlobals<'cx>"));
    assert!(source.contains("let types = AnvTypes::register(&mut heap);"));
    assert!(source.contains("let mut runtime = AnvRuntime::new(heap);"));
    assert!(
        source.contains(
            "let mut rt = unsafe { anvyx_runtime::Ctx::__anvyx_from_raw_with_safepoint(anv_entry.heap, anv_entry.safepoint) };"
        )
    );
    assert!(source.contains("let globals = AnvGlobals::new(&safepoint);"));
    assert!(source.contains("fn anv_f0_main<'cx, 'rt>(rt: &mut anvyx_runtime::Ctx<'cx, 'rt>, _types: &AnvTypes<'cx>, _globals: &AnvGlobals<'cx>)"));
    assert!(source.contains("anvyx_core::__anvyx_native::core_runtime::_println(rt, v0.as_str())"));
    assert!(!source.contains("type AnvCtx"));
    assert_eq!(
        source
            .matches("anvyx_runtime::Ctx::__anvyx_from_raw")
            .count(),
        1
    );
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

    assert!(source.contains("fn anv_f0_leaf<'cx, 'rt>(_rt: &mut anvyx_runtime::Ctx<'cx, 'rt>, _types: &AnvTypes<'cx>, _globals: &AnvGlobals<'cx>) -> i64"));
    assert!(source.contains("fn anv_f1_caller<'cx, 'rt>(rt: &mut anvyx_runtime::Ctx<'cx, 'rt>, types: &AnvTypes<'cx>, globals: &AnvGlobals<'cx>) -> i64"));
    assert!(source.contains("anv_f0_leaf(rt, types, globals)"));
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
        "fn anvstringify_t3_point<'cx, 'rt>(_rt: &mut anvyx_runtime::Ctx<'cx, 'rt>, _types: &AnvTypes<'cx>, value: &anvT3_Point)"
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
                native_ref: false,
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
                native_ref: false,
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
        source.contains("fn stringify_inner<'cx, 'rt>(_rt: &mut anvyx_runtime::Ctx<'cx, 'rt>, _types: &AnvTypes<'cx>, value: &Inner)")
    );
    assert!(
        source.contains("fn stringify_outer<'cx, 'rt>(rt: &mut anvyx_runtime::Ctx<'cx, 'rt>, types: &AnvTypes<'cx>, value: &Outer)")
    );
    assert!(source.contains("stringify_inner(rt, types, &value.inner)"));
}

#[test]
fn emit_passes_native_string_returns() {
    let source =
        emit::emit(&rir::verify(&native_string_return_program()).expect("RIR verify failed"))
            .into_string();

    assert!(source.contains("host::string(rt)"));
    assert!(!source.contains("anvyx_runtime::AnvString::from(host::string(rt))"));
}

#[test]
fn emit_passes_native_option_string_returns() {
    let mut program = native_option_return_program(Some(RirCoreEnumKind::Option));
    let option = RirTypeId::from_index(1);
    program.functions.push(return_extern_function(option));
    program.entry = Some(RirFunctionId::from_index(0));
    let source = emit::emit(&rir::verify(&program).expect("RIR verify failed"));
    let text = source.as_str();

    assert!(!text.contains(".map(|value| anvyx_runtime::AnvString::from(value))"));

    let source = emit::RustSource::new(format!(
        "mod host {{ pub fn substring<'cx, 'rt>(_ctx: &mut anvyx_runtime::Ctx<'cx, 'rt>) -> Option<anvyx_runtime::AnvString> {{ Some(anvyx_runtime::AnvString::from(\"ok\")) }} }}\n{}",
        source.into_string()
    ));
    let output = run_source(source);

    assert_eq!(output.status, SourceJobStatus::Success, "{}", output.stderr);
}

#[test]
fn emit_uses_array_map_for_native_array_returns() {
    let string = RirTypeId::from_index(0);
    let option = RirTypeId::from_index(1);
    let array = RirTypeId::from_index(2);
    let array_abi = anvyx_runtime::ExternTypeExpr::Array {
        elem: Box::new(anvyx_runtime::ExternTypeExpr::Option(Box::new(
            anvyx_runtime::ExternTypeExpr::String,
        ))),
        len: 2,
    };
    let mut program = native_extern_rir(
        vec![
            RirType::String,
            RirType::Option(string),
            RirType::Array {
                elem: option,
                len: 2,
            },
        ],
        vec![],
        array,
        direct_rust_abi(
            vec![],
            anvyx_runtime::RustReturnAbi::Value(array_abi.clone()),
        ),
        rir_abi(vec![], array_abi),
    );
    program.functions.push(return_extern_function(array));
    program.entry = Some(RirFunctionId::from_index(0));

    let source = emit::emit(&rir::verify(&program).expect("RIR verify failed")).into_string();

    assert!(source.contains(".map(|value| { (value).map(|value| value) })"));
}

#[test]
fn emit_uses_array_map_for_native_array_params() {
    let string = RirTypeId::from_index(0);
    let option = RirTypeId::from_index(1);
    let array = RirTypeId::from_index(2);
    let void = RirTypeId::from_index(3);
    let array_abi = anvyx_runtime::ExternTypeExpr::Array {
        elem: Box::new(anvyx_runtime::ExternTypeExpr::Option(Box::new(
            anvyx_runtime::ExternTypeExpr::String,
        ))),
        len: 2,
    };
    let mut program = native_extern_rir(
        vec![
            RirType::String,
            RirType::Option(string),
            RirType::Array {
                elem: option,
                len: 2,
            },
            RirType::Void,
        ],
        vec![RirExternParam {
            ty: array,
            semantic: RirParamSemantic::Value,
            abi: RirParamAbi::Value,
            escape: RirParamEscape::NonEscaping,
        }],
        void,
        direct_rust_abi(
            vec![anvyx_runtime::RustParamAbi::Value(array_abi.clone())],
            anvyx_runtime::RustReturnAbi::Void,
        ),
        rir_abi(vec![array_abi], anvyx_runtime::ExternTypeExpr::Void),
    );
    program.functions.push(call_extern_function(
        void,
        vec![RirCallArg::Value(RirOperand::Place(RirPlace::local(
            RirLocalId::from_index(0),
            vec![],
            array,
        )))],
    ));
    program.functions[0].params.push(RirParam {
        local: RirLocalId::from_index(0),
        ty: array,
        semantic: RirParamSemantic::Value,
        abi: RirParamAbi::Value,
        escape: RirParamEscape::NonEscaping,
    });
    program.functions[0].locals.push(RirLocal {
        id: RirLocalId::from_index(0),
        ty: array,
        mutable: false,
        symbol: RirSymbol::new("v0"),
        initialized: true,
        payload_ref: false,
    });
    program.entry = Some(RirFunctionId::from_index(0));

    let source = emit::emit(&rir::verify(&program).expect("RIR verify failed")).into_string();

    assert!(source.contains(".map(|value| { (value).map(|value| value) })"));
}

#[test]
fn emit_passes_native_result_string_returns() {
    let string = RirTypeId::from_index(0);
    let result = RirTypeId::from_index(1);
    let mut program = native_extern_rir(
        vec![RirType::String, RirType::Enum(RirEnumId::from_index(0))],
        vec![],
        result,
        direct_rust_abi(
            vec![],
            anvyx_runtime::RustReturnAbi::Result(
                Box::new(anvyx_runtime::RustReturnAbi::Value(
                    anvyx_runtime::ExternTypeExpr::String,
                )),
                Box::new(anvyx_runtime::RustReturnAbi::Value(
                    anvyx_runtime::ExternTypeExpr::String,
                )),
            ),
        ),
        rir_abi(
            vec![],
            anvyx_runtime::ExternTypeExpr::Result(
                Box::new(anvyx_runtime::ExternTypeExpr::String),
                Box::new(anvyx_runtime::ExternTypeExpr::String),
            ),
        ),
    );
    program.enums.push(rir_result_enum(string, string));
    program.functions.push(return_extern_function(result));
    program.entry = Some(RirFunctionId::from_index(0));

    let source = emit::emit(&rir::verify(&program).expect("RIR verify failed")).into_string();

    assert!(source.contains("Ok(value) => Result::Ok(value)"));
    assert!(source.contains("Err(value) => Result::Err(value)"));
    assert!(!source.contains("host::native(rt)?"));
}

#[test]
fn emit_passes_native_result_string_params() {
    let string = RirTypeId::from_index(0);
    let result = RirTypeId::from_index(1);
    let void = RirTypeId::from_index(2);
    let mut program = native_extern_rir(
        vec![
            RirType::String,
            RirType::Enum(RirEnumId::from_index(0)),
            RirType::Void,
        ],
        vec![RirExternParam {
            ty: result,
            semantic: RirParamSemantic::Value,
            abi: RirParamAbi::Value,
            escape: RirParamEscape::NonEscaping,
        }],
        void,
        direct_rust_abi(
            vec![anvyx_runtime::RustParamAbi::Result(
                Box::new(anvyx_runtime::RustParamAbi::Value(
                    anvyx_runtime::ExternTypeExpr::String,
                )),
                Box::new(anvyx_runtime::RustParamAbi::Value(
                    anvyx_runtime::ExternTypeExpr::String,
                )),
            )],
            anvyx_runtime::RustReturnAbi::Void,
        ),
        rir_abi(
            vec![anvyx_runtime::ExternTypeExpr::Result(
                Box::new(anvyx_runtime::ExternTypeExpr::String),
                Box::new(anvyx_runtime::ExternTypeExpr::String),
            )],
            anvyx_runtime::ExternTypeExpr::Void,
        ),
    );
    program.enums.push(rir_result_enum(string, string));
    program.functions.push(call_extern_function(
        void,
        vec![RirCallArg::Value(RirOperand::Place(RirPlace::local(
            RirLocalId::from_index(0),
            vec![],
            result,
        )))],
    ));
    program.functions[0].params.push(RirParam {
        local: RirLocalId::from_index(0),
        ty: result,
        semantic: RirParamSemantic::Value,
        abi: RirParamAbi::Value,
        escape: RirParamEscape::NonEscaping,
    });
    program.functions[0].locals.push(RirLocal {
        id: RirLocalId::from_index(0),
        ty: result,
        mutable: false,
        symbol: RirSymbol::new("v0"),
        initialized: true,
        payload_ref: false,
    });
    program.entry = Some(RirFunctionId::from_index(0));

    let source = emit::emit(&rir::verify(&program).expect("RIR verify failed")).into_string();

    assert!(source.contains("Result::Ok(value) => Ok(value)"));
    assert!(source.contains("Result::Err(value) => Err(value)"));
}

#[test]
fn emit_adopts_owned_native_resource_returns() {
    let source =
        emit::emit(&rir::verify(&native_resource_return_program(true)).expect("RIR verify failed"))
            .into_string();

    assert!(source.contains("AnvRefType::<host::Window>::register_untracked"));
    assert!(!source.contains("register_tracked_in"));
}

#[test]
fn emit_does_not_adopt_explicit_native_resource_refs() {
    let source = emit::emit(
        &rir::verify(&native_resource_return_program(false)).expect("RIR verify failed"),
    )
    .into_string();

    assert!(!source.contains("register_untracked"));
}

#[test]
fn emit_converts_native_unit_param_and_return() {
    let unit = RirTypeId::from_index(0);
    let void = RirTypeId::from_index(1);
    let mut param_program = native_extern_rir(
        vec![RirType::Tuple(RirTupleId::from_index(0)), RirType::Void],
        vec![RirExternParam {
            ty: unit,
            semantic: RirParamSemantic::Value,
            abi: RirParamAbi::Value,
            escape: RirParamEscape::NonEscaping,
        }],
        void,
        direct_rust_abi(
            vec![anvyx_runtime::RustParamAbi::Value(
                anvyx_runtime::ExternTypeExpr::Unit,
            )],
            anvyx_runtime::RustReturnAbi::Void,
        ),
        rir_abi(
            vec![anvyx_runtime::ExternTypeExpr::Unit],
            anvyx_runtime::ExternTypeExpr::Void,
        ),
    );
    param_program.tuples.push(rir_empty_tuple());
    param_program.functions.push(call_extern_function(
        void,
        vec![RirCallArg::Value(RirOperand::Place(RirPlace::local(
            RirLocalId::from_index(0),
            vec![],
            unit,
        )))],
    ));
    param_program.functions[0].params.push(RirParam {
        local: RirLocalId::from_index(0),
        ty: unit,
        semantic: RirParamSemantic::Value,
        abi: RirParamAbi::Value,
        escape: RirParamEscape::NonEscaping,
    });
    param_program.functions[0].locals.push(RirLocal {
        id: RirLocalId::from_index(0),
        ty: unit,
        mutable: false,
        symbol: RirSymbol::new("v0"),
        initialized: true,
        payload_ref: false,
    });
    param_program.entry = Some(RirFunctionId::from_index(0));

    let source = emit::emit(&rir::verify(&param_program).expect("RIR verify failed")).into_string();
    assert!(source.contains("host::native(rt, ())"));

    let mut ret_program = native_extern_rir(
        vec![RirType::Tuple(RirTupleId::from_index(0))],
        vec![],
        unit,
        direct_rust_abi(
            vec![],
            anvyx_runtime::RustReturnAbi::Value(anvyx_runtime::ExternTypeExpr::Unit),
        ),
        rir_abi(vec![], anvyx_runtime::ExternTypeExpr::Unit),
    );
    ret_program.tuples.push(rir_empty_tuple());
    ret_program.functions.push(return_extern_function(unit));
    ret_program.entry = Some(RirFunctionId::from_index(0));

    let source = emit::emit(&rir::verify(&ret_program).expect("RIR verify failed")).into_string();
    assert!(source.contains("let _ = host::native(rt); UnitTuple {  }"));
}

#[test]
fn emit_propagates_fallible_native_calls() {
    let source = plan_source(fallible_call_program(false, false)).into_string();

    assert!(source.contains(
        "fn anv_f0_main<'cx, 'rt>(rt: &mut anvyx_runtime::Ctx<'cx, 'rt>, _types: &AnvTypes<'cx>, _globals: &AnvGlobals<'cx>) -> Result<(), anvyx_runtime::RuntimeError>"
    ));
    assert!(source.contains("host::fallible(rt, 41)?;"));
    assert!(source.contains("fn main() -> Result<(), anvyx_runtime::RuntimeError>"));
    assert!(source.contains("let _ = anv_f0_main(&mut rt, anv_entry.types, anv_entry.globals)?;"));
    assert!(!source.contains(".unwrap()"));
}

#[test]
fn emit_pins_generated_runtime_owner_before_attach() {
    let source = plan_source(fallible_call_program(false, false)).into_string();

    assert!(source.contains("struct AnvRuntime<'cx>"));
    assert!(source.contains("owner: anvyx_runtime::RuntimeOwnerHandle,"));
    assert!(source.contains("inner: std::pin::Pin<Box<AnvRuntimeInner<'cx>>>,"));
    assert!(source.contains("struct AnvRuntimeInner<'cx>"));
    assert!(source.contains("heap: anvyx_runtime::Heap<'cx>,"));
    assert!(source.contains("_pin: std::marker::PhantomPinned,"));
    assert!(source.contains(
        "types: AnvTypes<'cx>,\n    globals: AnvGlobals<'cx>,\n    heap: anvyx_runtime::Heap<'cx>,"
    ));
    assert!(source.contains("anvyx_runtime::Heap::scope_owned(|heap|"));
    assert!(source.contains("let mut runtime = AnvRuntime::new(heap);"));
    assert!(source.contains("struct AnvEntry<'entry, 'cx>"));
    assert!(source.contains("fn with_entry<R>"));
    assert!(source.contains("runtime.with_entry(|anv_entry|"));
    assert!(source.contains("let owner_entry = self.owner.__anvyx_enter_current()?;"));
    assert!(source.contains("owner_entry.owner_ptr().cast::<AnvRuntimeInner<'cx>>().as_mut()"));
    assert!(!source.contains("fn inner_mut"));
    assert!(source.contains("runtime.owner.__anvyx_attach_owner_ptr"));
    assert!(source.contains("self.owner.__anvyx_begin_shutdown()"));

    let pin_pos = source.find("inner: Box::pin(AnvRuntimeInner {").unwrap();
    let attach_pos = source.find("__anvyx_attach_owner_ptr").unwrap();
    let enter_pos = source.find("__anvyx_enter_current").unwrap();
    let cast_pos = source.find("owner_entry.owner_ptr().cast").unwrap();
    let drop_pos = source.find("impl<'cx> Drop for AnvRuntime<'cx>").unwrap();
    assert!(pin_pos < attach_pos);
    assert!(attach_pos < enter_pos);
    assert!(enter_pos < cast_pos);
    assert!(attach_pos < drop_pos);
}

#[test]
fn emit_propagates_generated_fallibility_transitively() {
    let source = plan_source(fallible_call_program(true, false)).into_string();

    assert!(source.contains(
        "fn anv_f0_leaf<'cx, 'rt>(rt: &mut anvyx_runtime::Ctx<'cx, 'rt>, _types: &AnvTypes<'cx>, _globals: &AnvGlobals<'cx>) -> Result<(), anvyx_runtime::RuntimeError>"
    ));
    assert!(source.contains(
        "fn anv_f1_main<'cx, 'rt>(rt: &mut anvyx_runtime::Ctx<'cx, 'rt>, types: &AnvTypes<'cx>, globals: &AnvGlobals<'cx>) -> Result<(), anvyx_runtime::RuntimeError>"
    ));
    assert!(source.contains("anv_f0_leaf(rt, types, globals)?;"));
}

#[test]
fn source_job_compiles_fallible_non_void_entry() {
    let source = with_fallible_host(plan_source(fallible_call_program(false, true)));
    let output = run_source(source);

    assert_eq!(output.status, SourceJobStatus::Success, "{}", output.stderr);
}

#[test]
fn emit_borrows_string_literal_call_arg_without_owned_temp() {
    let source = plan_source(borrow_string_literal_program()).into_string();

    assert!(source.contains("anvyx_core::__anvyx_native::core_runtime::_println(rt, \"ready\");"));
    assert!(!source.contains("String::from"));
    assert!(!source.contains("to_string()"));
}

#[test]
fn emit_forwards_borrowed_string_param_as_str_without_double_borrow() {
    let source = plan_source(shared_string_forward_program()).into_string();

    assert!(source.contains(": &str"));
    assert!(source.contains("anvyx_core::__anvyx_native::core_runtime::_println(rt, v0);"));
    assert!(!source.contains("anvyx_core::__anvyx_native::core_runtime::_println(rt, &v0);"));
    assert!(
        !source.contains("anvyx_core::__anvyx_native::core_runtime::_println(rt, v0.as_str());")
    );
}

#[test]
fn emit_borrows_string_constant_for_native_string_param() {
    let source = plan_source(native_str_len_const_program()).into_string();

    assert!(source.contains("anvyx_core::__anvyx_native::core_string::str_len(rt, \"abc\")"));
    assert!(!source.contains("String::from"));
    assert!(!source.contains("to_string()"));
}

#[test]
fn emit_borrows_string_local_for_native_string_param() {
    let source = plan_source(native_str_len_local_program()).into_string();

    assert!(source.contains("anvyx_core::__anvyx_native::core_string::str_len(rt, v0.as_str())"));
    assert!(!source.contains("anvyx_core::__anvyx_native::core_string::str_len(rt, &v0)"));
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
    program.functions[0].body.term = RirTerm::Return(Some(RirOperand::Place(RirPlace::local(
        RirLocalId::from_index(0),
        vec![],
        RirTypeId::from_index(0),
    ))));

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
fn rir_verify_accepts_wildcard_pattern_match() {
    let mut program = empty_rir_function(RirType::Int);
    let int = RirTypeId::from_index(0);
    let subject = RirLocalId::from_index(0);
    program.functions[0]
        .locals
        .push(rir_local(subject, int, false, "subject"));
    program.functions[0].body = RirStructuredBlock {
        stmts: vec![RirStmt::PatternMatch(RirPatternMatch {
            subject: rir_place(subject, int),
            arms: vec![RirPatternArm {
                alternatives: vec![RirPatternAlternative::default()],
                block: RirStructuredBlock {
                    stmts: vec![],
                    term: RirTerm::Return(Some(RirOperand::Const(RirConstId::from_index(0)))),
                },
            }],
        })],
        term: RirTerm::None,
    };

    rir::verify(&program).expect("RIR verify failed");
}

#[test]
fn rir_verify_rejects_pattern_binding_type_mismatch() {
    let mut program = empty_rir_function(RirType::Int);
    let int = RirTypeId::from_index(0);
    let bool_ty = RirTypeId::from_index(1);
    let subject = RirLocalId::from_index(0);
    let binding = RirLocalId::from_index(1);
    program.types.push(RirType::Bool);
    program.functions[0]
        .locals
        .push(rir_local(subject, int, false, "subject"));
    program.functions[0]
        .locals
        .push(rir_local(binding, bool_ty, false, "binding"));
    program.functions[0].body = RirStructuredBlock {
        stmts: vec![RirStmt::PatternMatch(RirPatternMatch {
            subject: rir_place(subject, int),
            arms: vec![RirPatternArm {
                alternatives: vec![RirPatternAlternative {
                    tests: vec![],
                    bindings: vec![RirPatternBinding {
                        local: binding,
                        path: RirPatternPath::default(),
                        ty: bool_ty,
                        mode: RirPatternBindingMode::Owned,
                    }],
                }],
                block: RirStructuredBlock {
                    stmts: vec![],
                    term: RirTerm::Return(Some(RirOperand::Const(RirConstId::from_index(0)))),
                },
            }],
        })],
        term: RirTerm::None,
    };

    assert_rir_type_error(program);
}

#[test]
fn rir_verify_rejects_pattern_literal_type_mismatch() {
    let mut program = empty_rir_function(RirType::Int);
    let int = RirTypeId::from_index(0);
    let bool_ty = RirTypeId::from_index(1);
    let subject = RirLocalId::from_index(0);
    program.types.push(RirType::Bool);
    program.consts.push(RirConst {
        id: RirConstId::from_index(1),
        ty: bool_ty,
        value: RirConstValue::Bool(true),
    });
    program.functions[0]
        .locals
        .push(rir_local(subject, int, false, "subject"));
    program.functions[0].body = RirStructuredBlock {
        stmts: vec![RirStmt::PatternMatch(RirPatternMatch {
            subject: rir_place(subject, int),
            arms: vec![RirPatternArm {
                alternatives: vec![RirPatternAlternative {
                    tests: vec![RirPatternTest::Literal {
                        path: RirPatternPath::default(),
                        value: RirConstId::from_index(1),
                    }],
                    bindings: vec![],
                }],
                block: RirStructuredBlock {
                    stmts: vec![],
                    term: RirTerm::Return(Some(RirOperand::Const(RirConstId::from_index(0)))),
                },
            }],
        })],
        term: RirTerm::None,
    };

    assert_rir_type_error(program);
}

#[test]
fn rir_verify_rejects_unguarded_pattern_payload_path() {
    let mut program = empty_rir_function(RirType::Int);
    let enum_ty = RirTypeId::from_index(1);
    let subject = RirLocalId::from_index(0);
    program.types.push(RirType::Enum(RirEnumId::from_index(0)));
    program.enums.push(RirEnum {
        id: RirEnumId::from_index(0),
        air_id: None,
        native_path: None,
        native_key: None,
        core: None,
        repr: rir::RirEnumRepr::Adt,
        raw_type: None,
        symbol: RirSymbol::new("Slot"),
        display: RirSymbol::new("Slot"),
        copyable: true,
        variants: vec![RirVariant {
            id: RirVariantId::from_index(0),
            symbol: RirSymbol::new("Item"),
            display: RirSymbol::new("Item"),
            kind: RirVariantKind::Tuple,
            raw_value: None,
            fields: vec![RirField {
                id: RirFieldId::from_index(0),
                symbol: RirSymbol::new("0"),
                ty: RirTypeId::from_index(0),
            }],
        }],
    });
    program.functions[0]
        .locals
        .push(rir_local(subject, enum_ty, false, "subject"));
    program.functions[0].body = RirStructuredBlock {
        stmts: vec![RirStmt::PatternMatch(RirPatternMatch {
            subject: rir_place(subject, enum_ty),
            arms: vec![RirPatternArm {
                alternatives: vec![RirPatternAlternative {
                    tests: vec![],
                    bindings: vec![RirPatternBinding {
                        local: RirLocalId::from_index(1),
                        path: RirPatternPath {
                            steps: vec![RirPatternPathStep::EnumTupleField {
                                enum_id: RirEnumId::from_index(0),
                                variant: RirVariantId::from_index(0),
                                field: 0,
                            }],
                        },
                        ty: RirTypeId::from_index(0),
                        mode: RirPatternBindingMode::Owned,
                    }],
                }],
                block: RirStructuredBlock {
                    stmts: vec![],
                    term: RirTerm::Return(Some(RirOperand::Const(RirConstId::from_index(0)))),
                },
            }],
        })],
        term: RirTerm::None,
    };

    assert_rir_error(
        program,
        RirVerifyErrorKind::PatternPayloadWithoutVariantTest,
    );
}

#[test]
fn rir_verify_rejects_pattern_payload_step_variant_shape_mismatch() {
    let mut program = empty_rir_function(RirType::Int);
    let int = RirTypeId::from_index(0);
    let enum_ty = RirTypeId::from_index(1);
    let subject = RirLocalId::from_index(0);
    let binding = RirLocalId::from_index(1);
    program.types.push(RirType::Enum(RirEnumId::from_index(0)));
    program.enums.push(RirEnum {
        id: RirEnumId::from_index(0),
        air_id: None,
        native_path: None,
        native_key: None,
        core: None,
        repr: rir::RirEnumRepr::Adt,
        raw_type: None,
        symbol: RirSymbol::new("Slot"),
        display: RirSymbol::new("Slot"),
        copyable: true,
        variants: vec![RirVariant {
            id: RirVariantId::from_index(0),
            symbol: RirSymbol::new("Item"),
            display: RirSymbol::new("Item"),
            kind: RirVariantKind::Tuple,
            raw_value: None,
            fields: vec![RirField {
                id: RirFieldId::from_index(0),
                symbol: RirSymbol::new("0"),
                ty: int,
            }],
        }],
    });
    program.functions[0]
        .locals
        .push(rir_local(subject, enum_ty, false, "subject"));
    program.functions[0]
        .locals
        .push(rir_local(binding, int, false, "binding"));
    program.functions[0].body = RirStructuredBlock {
        stmts: vec![RirStmt::PatternMatch(RirPatternMatch {
            subject: rir_place(subject, enum_ty),
            arms: vec![RirPatternArm {
                alternatives: vec![RirPatternAlternative {
                    tests: vec![RirPatternTest::EnumVariant {
                        path: RirPatternPath::default(),
                        enum_id: RirEnumId::from_index(0),
                        variant: RirVariantId::from_index(0),
                    }],
                    bindings: vec![RirPatternBinding {
                        local: binding,
                        path: RirPatternPath {
                            steps: vec![RirPatternPathStep::EnumStructField {
                                enum_id: RirEnumId::from_index(0),
                                variant: RirVariantId::from_index(0),
                                field: 0,
                            }],
                        },
                        ty: int,
                        mode: RirPatternBindingMode::Owned,
                    }],
                }],
                block: RirStructuredBlock {
                    stmts: vec![],
                    term: RirTerm::Return(Some(RirOperand::Const(RirConstId::from_index(0)))),
                },
            }],
        })],
        term: RirTerm::None,
    };

    assert_rir_error(program, RirVerifyErrorKind::BadId);
}

#[test]
fn rir_verify_rejects_pattern_owned_binding_after_arm() {
    let binding = RirLocalId::from_index(1);
    let int = RirTypeId::from_index(0);
    let program = root_pattern_binding_program(
        RirPatternBindingMode::Owned,
        false,
        false,
        RirTerm::None,
        RirTerm::Return(Some(RirOperand::Place(rir_place(binding, int)))),
    );

    assert_rir_error(program, RirVerifyErrorKind::UninitializedLocal(binding));
}

#[test]
fn rir_verify_rejects_owned_pattern_binding_to_payload_ref_local() {
    let program = root_pattern_binding_program(
        RirPatternBindingMode::Owned,
        true,
        true,
        RirTerm::Return(Some(RirOperand::Const(RirConstId::from_index(0)))),
        RirTerm::None,
    );

    assert_rir_error(program, RirVerifyErrorKind::OptionPayloadRefLocalMismatch);
}

fn root_pattern_binding_program(
    mode: RirPatternBindingMode,
    mutable: bool,
    payload_ref: bool,
    block_term: RirTerm,
    term: RirTerm,
) -> RirProgram {
    let mut program = empty_rir_function(RirType::Int);
    let int = RirTypeId::from_index(0);
    let subject = RirLocalId::from_index(0);
    let binding = RirLocalId::from_index(1);
    program.functions[0]
        .locals
        .push(rir_local(subject, int, false, "subject"));
    let mut binding_local = rir_local(binding, int, mutable, "binding");
    binding_local.initialized = false;
    binding_local.payload_ref = payload_ref;
    program.functions[0].locals.push(binding_local);
    program.functions[0].body = RirStructuredBlock {
        stmts: vec![RirStmt::PatternMatch(RirPatternMatch {
            subject: rir_place(subject, int),
            arms: vec![RirPatternArm {
                alternatives: vec![RirPatternAlternative {
                    tests: vec![],
                    bindings: vec![RirPatternBinding {
                        local: binding,
                        path: RirPatternPath::default(),
                        ty: int,
                        mode,
                    }],
                }],
                block: RirStructuredBlock {
                    stmts: vec![],
                    term: block_term,
                },
            }],
        })],
        term,
    };
    program
}

#[test]
fn rir_verify_rejects_conflicting_root_pattern_variant_tests() {
    let mut program = empty_rir_function(RirType::Int);
    let enum_id = RirEnumId::from_index(0);
    let enum_ty = RirTypeId::from_index(1);
    let subject = RirLocalId::from_index(0);
    program.types.push(RirType::Enum(enum_id));
    program.enums.push(RirEnum {
        id: enum_id,
        air_id: None,
        native_path: None,
        native_key: None,
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
    program.functions[0]
        .locals
        .push(rir_local(subject, enum_ty, false, "subject"));
    program.functions[0].body.stmts = vec![RirStmt::PatternMatch(RirPatternMatch {
        subject: rir_place(subject, enum_ty),
        arms: vec![RirPatternArm {
            alternatives: vec![RirPatternAlternative {
                tests: vec![
                    RirPatternTest::EnumVariant {
                        path: RirPatternPath::default(),
                        enum_id,
                        variant: RirVariantId::from_index(0),
                    },
                    RirPatternTest::EnumVariant {
                        path: RirPatternPath::default(),
                        enum_id,
                        variant: RirVariantId::from_index(1),
                    },
                ],
                bindings: vec![],
            }],
            block: RirStructuredBlock {
                stmts: vec![],
                term: RirTerm::Return(Some(RirOperand::Const(RirConstId::from_index(0)))),
            },
        }],
    })];

    assert_rir_error(program, RirVerifyErrorKind::UnsupportedRValueType);
}

#[test]
fn rir_verify_rejects_pattern_alias_binding_after_arm() {
    let mut program = empty_rir_function(RirType::Int);
    let int = RirTypeId::from_index(0);
    let enum_ty = RirTypeId::from_index(1);
    let subject = RirLocalId::from_index(0);
    let binding = RirLocalId::from_index(1);
    program.types.push(RirType::Enum(RirEnumId::from_index(0)));
    program.enums.push(RirEnum {
        id: RirEnumId::from_index(0),
        air_id: None,
        native_path: None,
        native_key: None,
        core: None,
        repr: rir::RirEnumRepr::Adt,
        raw_type: None,
        symbol: RirSymbol::new("Slot"),
        display: RirSymbol::new("Slot"),
        copyable: true,
        variants: vec![RirVariant {
            id: RirVariantId::from_index(0),
            symbol: RirSymbol::new("Value"),
            display: RirSymbol::new("Value"),
            kind: RirVariantKind::Tuple,
            raw_value: None,
            fields: vec![RirField {
                id: RirFieldId::from_index(0),
                symbol: RirSymbol::new("0"),
                ty: int,
            }],
        }],
    });
    program.functions[0]
        .locals
        .push(rir_local(subject, enum_ty, true, "subject"));
    let mut binding_local = rir_local(binding, int, true, "binding");
    binding_local.initialized = false;
    binding_local.payload_ref = true;
    program.functions[0].locals.push(binding_local);
    program.functions[0].body = RirStructuredBlock {
        stmts: vec![RirStmt::PatternMatch(RirPatternMatch {
            subject: rir_place(subject, enum_ty),
            arms: vec![RirPatternArm {
                alternatives: vec![RirPatternAlternative {
                    tests: vec![RirPatternTest::EnumVariant {
                        path: RirPatternPath::default(),
                        enum_id: RirEnumId::from_index(0),
                        variant: RirVariantId::from_index(0),
                    }],
                    bindings: vec![RirPatternBinding {
                        local: binding,
                        path: RirPatternPath {
                            steps: vec![RirPatternPathStep::EnumTupleField {
                                enum_id: RirEnumId::from_index(0),
                                variant: RirVariantId::from_index(0),
                                field: 0,
                            }],
                        },
                        ty: int,
                        mode: RirPatternBindingMode::Alias,
                    }],
                }],
                block: RirStructuredBlock {
                    stmts: vec![],
                    term: RirTerm::None,
                },
            }],
        })],
        term: RirTerm::Return(Some(RirOperand::Place(rir_place(binding, int)))),
    };

    assert_rir_error(
        program,
        RirVerifyErrorKind::UninitializedLocal(RirLocalId::from_index(1)),
    );
}

#[test]
fn rir_verify_rejects_unguarded_optional_pattern_binding_path() {
    let mut program = empty_rir_function(RirType::Int);
    let int = RirTypeId::from_index(0);
    let opt = RirTypeId::from_index(1);
    let subject = RirLocalId::from_index(0);
    let binding = RirLocalId::from_index(1);
    program.types.push(RirType::Option(int));
    program.functions[0]
        .locals
        .push(rir_local(subject, opt, false, "subject"));
    program.functions[0]
        .locals
        .push(rir_local(binding, int, false, "binding"));
    program.functions[0].body = RirStructuredBlock {
        stmts: vec![RirStmt::PatternMatch(RirPatternMatch {
            subject: rir_place(subject, opt),
            arms: vec![RirPatternArm {
                alternatives: vec![RirPatternAlternative {
                    tests: vec![],
                    bindings: vec![RirPatternBinding {
                        local: binding,
                        path: RirPatternPath {
                            steps: vec![RirPatternPathStep::OptionalSome],
                        },
                        ty: int,
                        mode: RirPatternBindingMode::Owned,
                    }],
                }],
                block: RirStructuredBlock {
                    stmts: vec![],
                    term: RirTerm::Return(Some(RirOperand::Const(RirConstId::from_index(0)))),
                },
            }],
        })],
        term: RirTerm::None,
    };

    assert_rir_error(
        program,
        RirVerifyErrorKind::PatternPayloadWithoutVariantTest,
    );
}

#[test]
fn rir_verify_rejects_unguarded_optional_pattern_test_path() {
    let mut program = empty_rir_function(RirType::Int);
    let int = RirTypeId::from_index(0);
    let opt = RirTypeId::from_index(1);
    let subject = RirLocalId::from_index(0);
    program.types.push(RirType::Option(int));
    program.consts.push(RirConst {
        id: RirConstId::from_index(1),
        ty: int,
        value: RirConstValue::Int(1),
    });
    program.functions[0]
        .locals
        .push(rir_local(subject, opt, false, "subject"));
    program.functions[0].body = RirStructuredBlock {
        stmts: vec![RirStmt::PatternMatch(RirPatternMatch {
            subject: rir_place(subject, opt),
            arms: vec![RirPatternArm {
                alternatives: vec![RirPatternAlternative {
                    tests: vec![RirPatternTest::Literal {
                        path: RirPatternPath {
                            steps: vec![RirPatternPathStep::OptionalSome],
                        },
                        value: RirConstId::from_index(1),
                    }],
                    bindings: vec![],
                }],
                block: RirStructuredBlock {
                    stmts: vec![],
                    term: RirTerm::Return(Some(RirOperand::Const(RirConstId::from_index(0)))),
                },
            }],
        })],
        term: RirTerm::None,
    };

    assert_rir_error(
        program,
        RirVerifyErrorKind::PatternPayloadWithoutVariantTest,
    );
}

#[test]
fn rir_verify_rejects_optional_pattern_test_before_guard() {
    let mut program = empty_rir_function(RirType::Int);
    let int = RirTypeId::from_index(0);
    let opt = RirTypeId::from_index(1);
    let subject = RirLocalId::from_index(0);
    program.types.push(RirType::Option(int));
    program.consts.push(RirConst {
        id: RirConstId::from_index(1),
        ty: int,
        value: RirConstValue::Int(1),
    });
    program.functions[0]
        .locals
        .push(rir_local(subject, opt, false, "subject"));
    program.functions[0].body = RirStructuredBlock {
        stmts: vec![RirStmt::PatternMatch(RirPatternMatch {
            subject: rir_place(subject, opt),
            arms: vec![RirPatternArm {
                alternatives: vec![RirPatternAlternative {
                    tests: vec![
                        RirPatternTest::Literal {
                            path: RirPatternPath {
                                steps: vec![RirPatternPathStep::OptionalSome],
                            },
                            value: RirConstId::from_index(1),
                        },
                        RirPatternTest::OptionalSome {
                            path: RirPatternPath::default(),
                        },
                    ],
                    bindings: vec![],
                }],
                block: RirStructuredBlock {
                    stmts: vec![],
                    term: RirTerm::Return(Some(RirOperand::Const(RirConstId::from_index(0)))),
                },
            }],
        })],
        term: RirTerm::None,
    };

    assert_rir_error(
        program,
        RirVerifyErrorKind::PatternPayloadWithoutVariantTest,
    );
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
            term: RirTerm::Return(Some(RirOperand::Place(RirPlace::local(
                RirLocalId::from_index(0),
                vec![],
                int,
            )))),
        },
    });
    program.functions[0]
        .body
        .stmts
        .push(RirStmt::Eval(RirRValue::Call {
            callee: RirCallTarget::Function(RirFunctionId::from_index(1)),
            args: vec![RirCallArg::SharedBorrow(RirPlace::local(
                RirLocalId::from_index(0),
                vec![],
                int,
            ))],
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
        native_key: None,
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
    let RirCallArg::MutPlace(arg) = &args[0] else {
        unreachable!();
    };
    let RirMutPlaceAccess::DataRef { dataref, .. } = arg.access else {
        unreachable!();
    };

    let descriptor = descriptors
        .find(dataref, &arg.projections, arg.ty)
        .expect("descriptor missing for dataref arg");

    assert_eq!(descriptor.symbol, "anvP0_Node_value_place");
}

#[test]
fn dataref_place_descriptor_inventory_finds_map_entry_descriptor() {
    let map_ty = RirTypeId::from_index(7);
    let map = dataref_projection_mut_place_arg(
        vec![RirProjection::Field(RirFieldId::from_index(6))],
        map_ty,
    );
    let program = dataref_access_rir(vec![RirStmt::MapEntryMatch(RirMapEntryMatch {
        map,
        key: RirOperand::Const(RirConstId::from_index(0)),
        payload: None,
        payload_escapes: false,
        some_block: RirStructuredBlock::default(),
        none_block: RirStructuredBlock::default(),
    })]);
    let descriptors = DataRefPlaceDescriptors::build(&program);

    assert_eq!(descriptors.all()[0].symbol, "anvP0_Node_map_place");
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

    let heap_type = target::dataref_place_heap_type_access("self");
    for needle in [
        "struct anvP0_Node_value_place<'cx>".to_string(),
        format!(
            "{}: {}",
            target::dataref_place_heap_type_field(),
            target::heap_type_ty("NodeStorage<'cx>")
        ),
        format!(
            "impl<'cx> {} for anvP0_Node_value_place<'cx>",
            target::dataref_place_ops_ty("i64")
        ),
        format!(
            "let value = {}?;",
            target::rt_heap_try_with_erased(
                "rt",
                "object",
                &heap_type,
                "storage",
                "NodeStorage<'cx>",
                "Ok(storage.value.clone())",
            )
        ),
        "f(&value)".to_string(),
        format!(
            "let mut value = {}?;",
            target::rt_heap_try_with_erased(
                "rt",
                "object",
                &heap_type,
                "storage",
                "NodeStorage<'cx>",
                "Ok(storage.value.clone())",
            )
        ),
        format!(
            "let writeback = {};",
            target::rt_heap_try_with_erased_mut(
                "rt",
                "object",
                &heap_type,
                "storage",
                "NodeStorage<'cx>",
                "{ storage.value = value; Ok(()) }",
            )
        ),
        format!(
            "let __anv_dataref_place_object_0 = {};",
            target::rt_heap_erase("rt", "&node")
        ),
        format!(
            "let __anv_dataref_place_ops_0 = anvP0_Node_value_place {{ {}: types.NodeHeapType }};",
            target::dataref_place_heap_type_field()
        ),
        format!(
            "sink(rt, types, globals, {})",
            target::mut_place_dataref("__anv_dataref_place_object_0", "&__anv_dataref_place_ops_0")
        ),
    ] {
        assert!(source.contains(&needle), "missing {needle}");
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
    assert!(source.contains("Ok(storage.point.x.clone())"));
    assert!(source.contains("f(&value)"));
    assert!(source.contains("storage.point.x = value"));
}

#[test]
fn rir_verify_accepts_direct_scalar_dataref_projection_mut_place_arg() {
    rir::verify(&dataref_projection_mut_place_call_rir(
        valid_dataref_projection_mut_place_arg(),
    ))
    .expect("RIR rejected dataref projection mut-place arg");
}

#[test]
fn rir_verify_rejects_dataref_structural_map_insert() {
    let map_ty = RirTypeId::from_index(7);
    let map = dataref_projection_mut_place_arg(
        vec![RirProjection::Field(RirFieldId::from_index(6))],
        map_ty,
    );
    let program = dataref_access_rir(vec![RirStmt::Eval(RirRValue::MapInsert {
        map: RirCollectionAccess::MutPlace(map),
        key: RirOperand::Const(RirConstId::from_index(0)),
        value: RirOperand::Const(RirConstId::from_index(0)),
        kind: RirMapWriteKind::StructuralInsert,
    })]);

    assert_rir_error(program, RirVerifyErrorKind::UnsupportedRValueType);
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
fn rir_verify_accepts_nested_dataref_handle_projection_mut_place_arg() {
    let ty = RirTypeId::from_index(1);
    let arg = dataref_projection_mut_place_arg(
        vec![
            RirProjection::Field(RirFieldId::from_index(0)),
            RirProjection::Field(RirFieldId::from_index(1)),
        ],
        ty,
    );
    let mut program = dataref_projection_mut_place_call_rir(arg);
    program.functions[1].params[0].ty = ty;
    program.functions[1].locals[0].ty = ty;

    rir::verify(&program).expect("RIR rejected nested dataref handle mut-place arg");
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
fn rir_verify_accepts_aggregate_dataref_projection_mut_place_arg_payload() {
    let ty = RirTypeId::from_index(2);
    let arg =
        dataref_projection_mut_place_arg(vec![RirProjection::Field(RirFieldId::from_index(0))], ty);
    let mut program = dataref_projection_mut_place_call_rir(arg);
    program.functions[1].params[0].ty = ty;
    program.functions[1].locals[0].ty = ty;

    rir::verify(&program).expect("RIR rejected aggregate dataref mut-place arg");
}

#[test]
fn rir_verify_rejects_container_optional_enum_dataref_projection_mut_place_arg_payloads() {
    for (field, ty) in [
        (RirFieldId::from_index(1), RirTypeId::from_index(3)),
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
    ]);

    rir::verify(&program).expect("dataref access ops should verify");
}

#[test]
fn rir_verify_rejects_dataref_mut_place_set() {
    let program = dataref_access_rir(vec![RirStmt::MutPlaceSet {
        place: valid_dataref_projection_mut_place_arg(),
        value: RirRValue::Use(RirOperand::Const(RirConstId::from_index(0))),
    }]);

    assert_rir_error(program, RirVerifyErrorKind::UnsupportedRValueType);
}

#[test]
fn rir_verify_rejects_dataref_get_set_index_projection_ops() {
    let program = dataref_access_rir(vec![
        RirStmt::Init {
            local: RirLocalId::from_index(2),
            value: RirRValue::DataRefGet {
                object: RirOperand::Place(dataref_access_place(0, 1)),
                dataref: RirDataRefId::from_index(0),
                projections: vec![
                    RirProjection::Field(RirFieldId::from_index(4)),
                    RirProjection::Index(RirLocalId::from_index(1)),
                ],
                ty: RirTypeId::from_index(0),
            },
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

    assert_rir_error(program, RirVerifyErrorKind::UnsupportedRValueType);
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
        air_id: None,
        symbol: RirSymbol::new("bad_println"),
        kind: RirExternKind::Native(rir::RirNativeExtern::new(
            vec!["host".to_string(), "println".to_string()],
            anvyx_runtime::RustExternAbi {
                params: vec![anvyx_runtime::RustParamAbi::Borrow(
                    anvyx_runtime::ExternTypeExpr::String,
                )],
                ret: anvyx_runtime::RustReturnAbi::Void,
                fallible: false,
                support: anvyx_runtime::RustAbiSupport::Direct,
                ctx: anvyx_runtime::RustWrapperCtx::HiddenRuntime,
            },
        )),
        params: vec![RirExternParam {
            ty: RirTypeId::from_index(0),
            semantic: RirParamSemantic::Value,
            abi: RirParamAbi::Value,
            escape: RirParamEscape::NonEscaping,
        }],
        ret: RirTypeId::from_index(2),
        abi: rir_abi(
            vec![anvyx_runtime::ExternTypeExpr::String],
            anvyx_runtime::ExternTypeExpr::Void,
        ),
    });

    assert_rir_error(program, RirVerifyErrorKind::UnsupportedRValueType);
}

#[test]
fn rir_verify_rejects_native_extern_param_type_mismatch() {
    let mut program = empty_rir_function(RirType::Int);
    program.types.push(RirType::Void);
    program.externs.push(RirExtern {
        id: RirExternId::from_index(0),
        air_id: None,
        symbol: RirSymbol::new("bad_bool"),
        kind: RirExternKind::Native(rir::RirNativeExtern::new(
            vec!["host".to_string(), "bad_bool".to_string()],
            anvyx_runtime::RustExternAbi {
                params: vec![anvyx_runtime::RustParamAbi::Value(
                    anvyx_runtime::ExternTypeExpr::Bool,
                )],
                ret: anvyx_runtime::RustReturnAbi::Void,
                fallible: false,
                support: anvyx_runtime::RustAbiSupport::Direct,
                ctx: anvyx_runtime::RustWrapperCtx::HiddenRuntime,
            },
        )),
        params: vec![RirExternParam {
            ty: RirTypeId::from_index(0),
            semantic: RirParamSemantic::Value,
            abi: RirParamAbi::Value,
            escape: RirParamEscape::NonEscaping,
        }],
        ret: RirTypeId::from_index(1),
        abi: rir_abi(
            vec![anvyx_runtime::ExternTypeExpr::Bool],
            anvyx_runtime::ExternTypeExpr::Void,
        ),
    });

    assert_rir_error(program, RirVerifyErrorKind::UnsupportedRValueType);
}

#[test]
fn rir_verify_rejects_native_extern_param_mode_mismatch() {
    let program = native_extern_rir(
        vec![RirType::Int, RirType::Void],
        vec![RirExternParam {
            ty: RirTypeId::from_index(0),
            semantic: RirParamSemantic::MutPlace,
            abi: RirParamAbi::MutPlace,
            escape: RirParamEscape::NonEscaping,
        }],
        RirTypeId::from_index(1),
        direct_rust_abi(
            vec![anvyx_runtime::RustParamAbi::Value(
                anvyx_runtime::ExternTypeExpr::Int,
            )],
            anvyx_runtime::RustReturnAbi::Void,
        ),
        rir_abi(
            vec![anvyx_runtime::ExternTypeExpr::Int],
            anvyx_runtime::ExternTypeExpr::Void,
        ),
    );

    assert_rir_error(program, RirVerifyErrorKind::UnsupportedRValueType);
}

#[test]
fn rir_verify_accepts_native_scoped_lambda_arg() {
    let program = native_scoped_lambda_rir();

    rir::verify(&program).expect("scoped native lambda RIR should verify");
}

#[test]
fn rir_verify_rejects_escaping_lambda_arg_to_source_call() {
    let mut program = native_escaping_lambda_rir();
    let void = RirTypeId::from_index(0);
    let lambda_ty = RirTypeId::from_index(2);
    program.externs.clear();
    program.functions.push(RirFunction {
        id: RirFunctionId::from_index(1),
        air_id: None,
        symbol: RirSymbol::new("accept"),
        params: vec![RirParam {
            local: RirLocalId::from_index(0),
            ty: lambda_ty,
            semantic: RirParamSemantic::Value,
            abi: RirParamAbi::Value,
            escape: RirParamEscape::NonEscaping,
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
        body: RirStructuredBlock {
            stmts: vec![],
            term: RirTerm::Return(None),
        },
    });
    let RirStmt::Eval(RirRValue::Call { callee, .. }) = &mut program.functions[0].body.stmts[0]
    else {
        unreachable!()
    };
    *callee = RirCallTarget::Function(RirFunctionId::from_index(1));

    assert_rir_error(program, RirVerifyErrorKind::CallArgMode);
}

#[test]
fn rir_verify_rejects_scoped_lambda_arg_to_source_call() {
    let mut program = native_scoped_lambda_rir();
    let void = RirTypeId::from_index(0);
    let lambda_ty = RirTypeId::from_index(2);
    program.externs.clear();
    program.functions.push(RirFunction {
        id: RirFunctionId::from_index(1),
        air_id: None,
        symbol: RirSymbol::new("accept"),
        params: vec![RirParam {
            local: RirLocalId::from_index(0),
            ty: lambda_ty,
            semantic: RirParamSemantic::Value,
            abi: RirParamAbi::Value,
            escape: RirParamEscape::NonEscaping,
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
        body: RirStructuredBlock {
            stmts: vec![],
            term: RirTerm::Return(None),
        },
    });
    let RirStmt::Eval(RirRValue::Call { callee, .. }) = &mut program.functions[0].body.stmts[0]
    else {
        unreachable!()
    };
    *callee = RirCallTarget::Function(RirFunctionId::from_index(1));

    assert_rir_error(program, RirVerifyErrorKind::CallArgMode);
}

#[test]
fn rir_verify_rejects_native_scoped_escaping_lambda_mismatch() {
    let mut program = native_escaping_lambda_rir();
    let RirStmt::Eval(RirRValue::Call { args, .. }) = &mut program.functions[0].body.stmts[0]
    else {
        unreachable!()
    };
    args[0] = RirCallArg::ScopedLambda {
        callee: RirOperand::Place(rir_place(
            RirLocalId::from_index(0),
            RirTypeId::from_index(2),
        )),
        sig: RirLambdaSigId::from_index(0),
    };

    assert_rir_error(program, RirVerifyErrorKind::CallArgMode);
}

#[test]
fn rir_verify_rejects_native_escaping_scoped_lambda_mismatch() {
    let mut program = native_scoped_lambda_rir();
    let RirStmt::Eval(RirRValue::Call { args, .. }) = &mut program.functions[0].body.stmts[0]
    else {
        unreachable!()
    };
    args[0] = RirCallArg::EscapingLambda {
        callee: RirOperand::Place(rir_place(
            RirLocalId::from_index(0),
            RirTypeId::from_index(2),
        )),
        sig: RirLambdaSigId::from_index(0),
    };

    assert_rir_error(program, RirVerifyErrorKind::CallArgMode);
}

#[test]
fn rir_rejects_escaping_proof_over_nonescaping_lambda_param() {
    let mut program = native_scoped_lambda_rir();
    let lambda_ty = RirTypeId::from_index(2);
    let proven = RirLocalId::from_index(program.functions[0].locals.len());
    program.functions[0].params.push(RirParam {
        local: RirLocalId::from_index(0),
        ty: lambda_ty,
        semantic: RirParamSemantic::Value,
        abi: RirParamAbi::ScopedLambda,
        escape: RirParamEscape::NonEscaping,
    });
    program.functions[0].locals.push(RirLocal {
        id: proven,
        ty: lambda_ty,
        mutable: false,
        symbol: RirSymbol::new("proven"),
        initialized: false,
        payload_ref: false,
    });
    program.functions[0].body.stmts = vec![RirStmt::Init {
        local: proven,
        value: RirRValue::FunctionValue {
            value: RirOperand::Place(rir_place(RirLocalId::from_index(0), lambda_ty)),
            escape: Some(RirLambdaEscape::Escaping),
            ty: lambda_ty,
        },
    }];

    assert_rir_error(program, RirVerifyErrorKind::LambdaEscapeProofMismatch);
}

fn native_escaping_lambda_tuple_operand() -> (RirProgram, RirTypeId, RirOperand) {
    let mut program = native_escaping_lambda_rir();
    let lambda_ty = RirTypeId::from_index(2);
    let tuple = RirTupleId::from_index(0);
    let tuple_ty = RirTypeId::from_index(program.types.len());
    program.types.push(RirType::Tuple(tuple));
    program.tuples.push(RirTuple {
        id: tuple,
        symbol: RirSymbol::new("TupleLambda"),
        display: RirSymbol::new("(fn(i64) -> i64)"),
        copyable: false,
        fields: vec![RirField {
            id: RirFieldId::from_index(0),
            symbol: RirSymbol::new("_0"),
            ty: lambda_ty,
        }],
    });
    program.functions[0].locals[0].ty = tuple_ty;
    let projection = RirOperand::Place(RirPlace::local(
        RirLocalId::from_index(0),
        vec![RirProjection::TupleField(RirFieldId::from_index(0))],
        lambda_ty,
    ));
    (program, lambda_ty, projection)
}

#[test]
fn projected_escaping_lambda_without_proof_rejected() {
    let (mut program, _, projection) = native_escaping_lambda_tuple_operand();
    let RirStmt::Eval(RirRValue::Call { args, .. }) = &mut program.functions[0].body.stmts[0]
    else {
        unreachable!()
    };
    let RirCallArg::EscapingLambda { callee, .. } = &mut args[0] else {
        unreachable!()
    };
    *callee = projection;

    assert_rir_error(program, RirVerifyErrorKind::CallArgEscape);
}

#[test]
fn rir_verify_rejects_native_escaping_lambda_signature_mismatch() {
    let mut program = native_escaping_lambda_rir();
    let sig = RirLambdaSigId::from_index(1);
    program.types.push(RirType::Lambda(sig));
    program.lambda_sigs.push(RirLambdaSig {
        id: sig,
        params: vec![],
        ret: RirTypeId::from_index(0),
    });
    let RirStmt::Eval(RirRValue::Call { args, .. }) = &mut program.functions[0].body.stmts[0]
    else {
        unreachable!()
    };
    let RirCallArg::EscapingLambda { sig: arg_sig, .. } = &mut args[0] else {
        unreachable!()
    };
    *arg_sig = sig;

    assert_rir_type_error(program);
}

#[test]
fn rir_verify_rejects_native_escaping_lambda_non_escaping_slot() {
    let mut program = native_escaping_lambda_rir();
    program.externs[0].params[0].escape = RirParamEscape::NonEscaping;

    assert_rir_error(program, RirVerifyErrorKind::UnsupportedRValueType);
}

#[test]
fn rir_verify_rejects_native_escaping_lambda_without_provider_abi() {
    let mut program = native_escaping_lambda_rir();
    program.externs[0].params[0].semantic = RirParamSemantic::Value;
    program.externs[0].params[0].abi = RirParamAbi::Value;

    assert_rir_error(program, RirVerifyErrorKind::UnsupportedAbi);
}

#[test]
fn rir_verify_rejects_native_scoped_lambda_signature_mismatch() {
    let mut program = native_scoped_lambda_rir();
    let sig = RirLambdaSigId::from_index(1);
    program.types.push(RirType::Lambda(sig));
    program.lambda_sigs.push(RirLambdaSig {
        id: sig,
        params: vec![],
        ret: RirTypeId::from_index(0),
    });
    let RirStmt::Eval(RirRValue::Call { args, .. }) = &mut program.functions[0].body.stmts[0]
    else {
        unreachable!()
    };
    let RirCallArg::ScopedLambda { sig: arg_sig, .. } = &mut args[0] else {
        unreachable!()
    };
    *arg_sig = sig;

    assert_rir_type_error(program);
}

#[test]
fn rir_verify_rejects_native_scoped_lambda_above_max_arity() {
    let mut program = native_scoped_lambda_rir();
    let int = RirTypeId::from_index(1);
    program.lambda_sigs[0].params = (0..=anvyx_runtime::CALLBACK_WRAPPER_MAX_ARITY)
        .map(|_| RirLambdaParam {
            ty: int,
            semantic: RirParamSemantic::Value,
            abi: RirParamAbi::Value,
            escape: RirParamEscape::NonEscaping,
        })
        .collect();
    let mut callback = native_callback_sig();
    callback.params = (0..=anvyx_runtime::CALLBACK_WRAPPER_MAX_ARITY)
        .map(|_| anvyx_runtime::ExternCallbackParam {
            ty: anvyx_runtime::ExternTypeExpr::Int,
            escape: anvyx_runtime::CallbackEscape::NonEscaping,
        })
        .collect();
    let RirExternKind::Native(native) = &mut program.externs[0].kind;
    native.abi.params[0] = anvyx_runtime::RustParamAbi::ScopedLambda(callback);

    assert_rir_error(program, RirVerifyErrorKind::UnsupportedRValueType);
}

#[test]
fn rir_verify_rejects_native_scoped_lambda_non_lambda_operand() {
    let mut program = native_scoped_lambda_rir();
    let int = RirTypeId::from_index(1);
    program.functions[0].locals[0].ty = int;
    let RirStmt::Eval(RirRValue::Call { args, .. }) = &mut program.functions[0].body.stmts[0]
    else {
        unreachable!()
    };
    let RirCallArg::ScopedLambda { callee, .. } = &mut args[0] else {
        unreachable!()
    };
    *callee = RirOperand::Place(rir_place(RirLocalId::from_index(0), int));

    assert_rir_type_error(program);
}

#[test]
fn rir_verify_rejects_escaping_native_scoped_lambda_slot() {
    let mut program = native_scoped_lambda_rir();
    program.externs[0].params[0].escape = RirParamEscape::Escaping;

    assert_rir_error(program, RirVerifyErrorKind::CallArgEscape);
}

#[test]
fn rir_verify_rejects_scoped_lambda_without_provider_abi() {
    let mut program = native_scoped_lambda_rir();
    program.externs[0].params[0].semantic = RirParamSemantic::Value;
    program.externs[0].params[0].abi = RirParamAbi::Value;

    assert_rir_error(program, RirVerifyErrorKind::UnsupportedAbi);
}

#[test]
fn rir_verify_rejects_direct_scoped_lambda_provider_abi() {
    let mut program = native_scoped_lambda_rir();
    let RirExternKind::Native(native) = &mut program.externs[0].kind;
    native.abi.support = anvyx_runtime::RustAbiSupport::Direct;
    native.abi.ctx = anvyx_runtime::RustWrapperCtx::HiddenRuntime;

    assert_rir_error(program, RirVerifyErrorKind::UnsupportedRValueType);
}

#[test]
fn rir_verify_accepts_direct_native_collection_carriers() {
    let void = RirTypeId::from_index(0);
    let int = RirTypeId::from_index(1);
    let list = RirTypeId::from_index(2);
    let int_list =
        anvyx_runtime::ExternTypeExpr::List(Box::new(anvyx_runtime::ExternTypeExpr::Int));
    let direct = |params, ret| anvyx_runtime::RustExternAbi {
        params,
        ret,
        fallible: false,
        support: anvyx_runtime::RustAbiSupport::Direct,
        ctx: anvyx_runtime::RustWrapperCtx::HiddenRuntime,
    };
    let cases = [
        (
            "take",
            vec![RirExternParam {
                ty: list,
                semantic: RirParamSemantic::Value,
                abi: RirParamAbi::Value,
                escape: RirParamEscape::NonEscaping,
            }],
            void,
            direct(
                vec![anvyx_runtime::RustParamAbi::Value(int_list.clone())],
                anvyx_runtime::RustReturnAbi::Void,
            ),
            rir_abi(vec![int_list.clone()], anvyx_runtime::ExternTypeExpr::Void),
        ),
        (
            "make",
            vec![],
            list,
            direct(
                vec![],
                anvyx_runtime::RustReturnAbi::Value(int_list.clone()),
            ),
            rir_abi(vec![], int_list),
        ),
    ];

    for (symbol, params, ret, abi, extern_abi) in cases {
        let program = RirProgram {
            types: vec![RirType::Void, RirType::Int, RirType::List(int)],
            collection_storages: vec![rir_list_storage(list, int)],
            externs: vec![RirExtern {
                id: RirExternId::from_index(0),
                air_id: None,
                symbol: RirSymbol::new(symbol),
                kind: RirExternKind::Native(rir::RirNativeExtern::new(
                    vec!["host".to_string(), symbol.to_string()],
                    abi,
                )),
                params,
                ret,
                abi: extern_abi,
            }],
            consts: vec![],
            ..empty_rir_function(RirType::Void)
        };
        rir::verify(&program).expect("valid direct collection RIR failed verification");
    }
}

#[test]
fn rir_verify_rejects_backend_unsupported_native_collection_shapes() {
    let void = RirTypeId::from_index(0);
    let int = RirTypeId::from_index(1);
    let list = RirTypeId::from_index(2);
    let int_list =
        anvyx_runtime::ExternTypeExpr::List(Box::new(anvyx_runtime::ExternTypeExpr::Int));
    let result_list =
        anvyx_runtime::ExternTypeExpr::List(Box::new(anvyx_runtime::ExternTypeExpr::Result(
            Box::new(anvyx_runtime::ExternTypeExpr::Int),
            Box::new(anvyx_runtime::ExternTypeExpr::String),
        )));
    let direct = |params, ret| anvyx_runtime::RustExternAbi {
        params,
        ret,
        fallible: false,
        support: anvyx_runtime::RustAbiSupport::Direct,
        ctx: anvyx_runtime::RustWrapperCtx::HiddenRuntime,
    };
    let int_slice =
        anvyx_runtime::ExternTypeExpr::Slice(Box::new(anvyx_runtime::ExternTypeExpr::Int));
    let cases = [
        (
            direct(
                vec![anvyx_runtime::RustParamAbi::MutPlace(int_list.clone())],
                anvyx_runtime::RustReturnAbi::Void,
            ),
            rir_abi(vec![int_list], anvyx_runtime::ExternTypeExpr::Void),
        ),
        (
            direct(
                vec![anvyx_runtime::RustParamAbi::Value(result_list.clone())],
                anvyx_runtime::RustReturnAbi::Void,
            ),
            rir_abi(vec![result_list], anvyx_runtime::ExternTypeExpr::Void),
        ),
        (
            direct(
                vec![anvyx_runtime::RustParamAbi::Value(int_slice.clone())],
                anvyx_runtime::RustReturnAbi::Void,
            ),
            rir_abi(vec![int_slice], anvyx_runtime::ExternTypeExpr::Void),
        ),
    ];

    for (abi, extern_abi) in cases {
        let program = RirProgram {
            types: vec![RirType::Void, RirType::Int, RirType::List(int)],
            collection_storages: vec![rir_list_storage(list, int)],
            externs: vec![RirExtern {
                id: RirExternId::from_index(0),
                air_id: None,
                symbol: RirSymbol::new("bad"),
                kind: RirExternKind::Native(rir::RirNativeExtern::new(
                    vec!["host".to_string(), "bad".to_string()],
                    abi,
                )),
                params: vec![RirExternParam {
                    ty: list,
                    semantic: RirParamSemantic::Value,
                    abi: RirParamAbi::Value,
                    escape: RirParamEscape::NonEscaping,
                }],
                ret: void,
                abi: extern_abi,
            }],
            consts: vec![],
            ..empty_rir_function(RirType::Void)
        };
        assert_rir_error(program, RirVerifyErrorKind::UnsupportedRValueType);
    }
}

#[test]
fn rir_verify_rejects_scoped_lambda_source_param_abi() {
    let mut program = native_scoped_lambda_rir();
    let lambda_ty = RirTypeId::from_index(2);
    program.functions[0].params.push(RirParam {
        local: RirLocalId::from_index(0),
        ty: lambda_ty,
        semantic: RirParamSemantic::ScopedLambda,
        abi: RirParamAbi::ScopedLambda,
        escape: RirParamEscape::NonEscaping,
    });

    assert_rir_error(program, RirVerifyErrorKind::UnsupportedAbi);
}

#[test]
fn rir_verify_rejects_scoped_lambda_signature_param_abi() {
    let mut program = native_scoped_lambda_rir();
    program.lambda_sigs[0].params[0].semantic = RirParamSemantic::ScopedLambda;
    program.lambda_sigs[0].params[0].abi = RirParamAbi::ScopedLambda;

    assert_rir_error(program, RirVerifyErrorKind::UnsupportedAbi);
}

#[test]
fn rir_verify_rejects_scoped_lambda_capture_abi() {
    let mut program = native_scoped_lambda_rir();
    let lambda_ty = RirTypeId::from_index(2);
    program.lambdas.push(RirLambda {
        id: RirLambdaId::from_index(0),
        source: RirLambdaSource::Lambda(air::LambdaId::from_index(0)),
        function: RirFunctionId::from_index(0),
        sig: RirLambdaSigId::from_index(0),
        escape: RirLambdaEscape::NonEscaping,
        storage: RirLambdaStorage::ScopedCaptures,
        captures: vec![RirLambdaCapture {
            ty: lambda_ty,
            semantic: RirParamSemantic::ScopedLambda,
            abi: RirParamAbi::ScopedLambda,
            kind: RirLambdaCaptureKind::Param,
        }],
    });

    assert_rir_error(program, RirVerifyErrorKind::UnsupportedLambdaCapture);
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
        air_id: None,
        symbol: RirSymbol::new("bad_return"),
        kind: RirExternKind::Native(rir::RirNativeExtern::new(
            vec!["host".to_string(), "bad_return".to_string()],
            anvyx_runtime::RustExternAbi {
                params: vec![],
                ret: anvyx_runtime::RustReturnAbi::Value(anvyx_runtime::ExternTypeExpr::Bool),
                fallible: false,
                support: anvyx_runtime::RustAbiSupport::Direct,
                ctx: anvyx_runtime::RustWrapperCtx::HiddenRuntime,
            },
        )),
        params: vec![],
        ret: RirTypeId::from_index(0),
        abi: rir_abi(vec![], anvyx_runtime::ExternTypeExpr::Bool),
    });

    assert_rir_error(program, RirVerifyErrorKind::UnsupportedRValueType);
}

#[test]
fn rir_verify_rejects_source_named_native_abi() {
    let point = RirTypeId::from_index(0);
    let point_abi = anvyx_runtime::ExternTypeExpr::Named {
        module: None,
        name: "Point".to_string(),
        args: vec![],
    };
    let mut program = native_extern_rir(
        vec![RirType::Struct(RirStructId::from_index(0)), RirType::Void],
        vec![RirExternParam {
            ty: point,
            semantic: RirParamSemantic::Value,
            abi: RirParamAbi::Value,
            escape: RirParamEscape::NonEscaping,
        }],
        RirTypeId::from_index(1),
        direct_rust_abi(
            vec![anvyx_runtime::RustParamAbi::Value(point_abi.clone())],
            anvyx_runtime::RustReturnAbi::Void,
        ),
        rir_abi(vec![point_abi], anvyx_runtime::ExternTypeExpr::Void),
    );
    program.structs.push(RirStruct {
        id: RirStructId::from_index(0),
        air_id: None,
        symbol: RirSymbol::new("Point"),
        display: RirSymbol::new("Point"),
        native_path: None,
        native_ref: false,
        native_key: None,
        copyable: true,
        fields: vec![],
    });

    assert_rir_error(program, RirVerifyErrorKind::UnsupportedRValueType);
}

#[test]
fn rir_verify_rejects_generic_named_native_abi() {
    let boxed = RirTypeId::from_index(0);
    let boxed_abi = anvyx_runtime::ExternTypeExpr::Named {
        module: None,
        name: "Boxed".to_string(),
        args: vec![anvyx_runtime::ExternTypeExpr::Int],
    };
    let mut program = native_extern_rir(
        vec![RirType::Struct(RirStructId::from_index(0)), RirType::Void],
        vec![RirExternParam {
            ty: boxed,
            semantic: RirParamSemantic::Value,
            abi: RirParamAbi::Value,
            escape: RirParamEscape::NonEscaping,
        }],
        RirTypeId::from_index(1),
        direct_rust_abi(
            vec![anvyx_runtime::RustParamAbi::Value(boxed_abi.clone())],
            anvyx_runtime::RustReturnAbi::Void,
        ),
        rir_abi(vec![boxed_abi], anvyx_runtime::ExternTypeExpr::Void),
    );
    program.structs.push(RirStruct {
        id: RirStructId::from_index(0),
        air_id: None,
        symbol: RirSymbol::new("Boxed"),
        display: RirSymbol::new("Boxed"),
        native_path: Some(vec!["host".to_string(), "Boxed".to_string()]),
        native_ref: false,
        native_key: Some(anvyx_runtime::ExternTypeKey {
            module: anvyx_runtime::ModulePath { segments: vec![] },
            name: "Boxed".to_string(),
        }),
        copyable: true,
        fields: vec![],
    });

    assert_rir_error(program, RirVerifyErrorKind::UnsupportedRValueType);
}

#[test]
fn rir_verify_rejects_slice_return_abi() {
    let int_slice =
        anvyx_runtime::ExternTypeExpr::Slice(Box::new(anvyx_runtime::ExternTypeExpr::Int));
    let program = native_extern_rir(
        vec![RirType::Int, RirType::Slice(RirTypeId::from_index(0))],
        vec![],
        RirTypeId::from_index(1),
        direct_rust_abi(
            vec![],
            anvyx_runtime::RustReturnAbi::Value(int_slice.clone()),
        ),
        rir_abi(vec![], int_slice),
    );

    assert_rir_error(program, RirVerifyErrorKind::UnsupportedRValueType);
}

#[test]
fn rir_verify_rejects_non_named_owned_return_abi() {
    let list = anvyx_runtime::ExternTypeExpr::List(Box::new(anvyx_runtime::ExternTypeExpr::Int));
    let program = native_extern_rir(
        vec![RirType::Int, RirType::List(RirTypeId::from_index(0))],
        vec![],
        RirTypeId::from_index(1),
        direct_rust_abi(
            vec![],
            anvyx_runtime::RustReturnAbi::OwnedNamed(list.clone()),
        ),
        rir_abi(vec![], list),
    );

    assert_rir_error(program, RirVerifyErrorKind::UnsupportedRValueType);
}

#[test]
fn rir_verify_rejects_non_named_owned_param_abi() {
    let list = anvyx_runtime::ExternTypeExpr::List(Box::new(anvyx_runtime::ExternTypeExpr::Int));
    let program = native_extern_rir(
        vec![
            RirType::Int,
            RirType::List(RirTypeId::from_index(0)),
            RirType::Void,
        ],
        vec![RirExternParam {
            ty: RirTypeId::from_index(1),
            semantic: RirParamSemantic::Value,
            abi: RirParamAbi::Value,
            escape: RirParamEscape::NonEscaping,
        }],
        RirTypeId::from_index(2),
        direct_rust_abi(
            vec![anvyx_runtime::RustParamAbi::OwnedNamed(list.clone())],
            anvyx_runtime::RustReturnAbi::Void,
        ),
        rir_abi(vec![list], anvyx_runtime::ExternTypeExpr::Void),
    );

    assert_rir_error(program, RirVerifyErrorKind::UnsupportedRValueType);
}

#[test]
fn rir_verify_rejects_callback_return_abi() {
    let sig = RirLambdaSigId::from_index(0);
    let callback = anvyx_runtime::ExternTypeExpr::Callback(native_callback_sig());
    let mut program = native_extern_rir(
        vec![RirType::Void, RirType::Int, RirType::Lambda(sig)],
        vec![],
        RirTypeId::from_index(2),
        direct_rust_abi(
            vec![],
            anvyx_runtime::RustReturnAbi::Value(callback.clone()),
        ),
        rir_abi(vec![], callback),
    );
    program.lambda_sigs.push(RirLambdaSig {
        id: sig,
        params: vec![RirLambdaParam {
            ty: RirTypeId::from_index(1),
            semantic: RirParamSemantic::Value,
            abi: RirParamAbi::Value,
            escape: RirParamEscape::NonEscaping,
        }],
        ret: RirTypeId::from_index(1),
    });

    assert_rir_error(program, RirVerifyErrorKind::UnsupportedRValueType);
}

#[test]
fn rir_verify_rejects_hidden_visible_result_confusion() {
    let result_abi = anvyx_runtime::ExternTypeExpr::Result(
        Box::new(anvyx_runtime::ExternTypeExpr::Int),
        Box::new(anvyx_runtime::ExternTypeExpr::String),
    );
    let mut rust_abi = direct_rust_abi(
        vec![],
        anvyx_runtime::RustReturnAbi::Value(anvyx_runtime::ExternTypeExpr::Int),
    );
    rust_abi.fallible = true;
    let program = native_extern_rir(
        vec![RirType::Int],
        vec![],
        RirTypeId::from_index(0),
        rust_abi,
        rir_abi(vec![], result_abi),
    );

    assert_rir_error(program, RirVerifyErrorKind::UnsupportedRValueType);
}

#[test]
fn rir_verify_accepts_final_native_abi_shapes() {
    let int = RirTypeId::from_index(0);
    let string = RirTypeId::from_index(1);
    let void = RirTypeId::from_index(2);
    let option = RirTypeId::from_index(3);
    let slice = RirTypeId::from_index(4);
    let result = RirTypeId::from_index(5);
    let types = vec![
        RirType::Int,
        RirType::String,
        RirType::Void,
        RirType::Option(int),
        RirType::Slice(int),
        RirType::Enum(RirEnumId::from_index(0)),
    ];

    let option_abi =
        anvyx_runtime::ExternTypeExpr::Option(Box::new(anvyx_runtime::ExternTypeExpr::Int));
    let slice_abi =
        anvyx_runtime::ExternTypeExpr::Slice(Box::new(anvyx_runtime::ExternTypeExpr::Int));
    let result_abi = anvyx_runtime::ExternTypeExpr::Result(
        Box::new(anvyx_runtime::ExternTypeExpr::Int),
        Box::new(anvyx_runtime::ExternTypeExpr::String),
    );

    let mut option_program = native_extern_rir(
        types.clone(),
        vec![RirExternParam {
            ty: option,
            semantic: RirParamSemantic::Value,
            abi: RirParamAbi::Value,
            escape: RirParamEscape::NonEscaping,
        }],
        void,
        direct_rust_abi(
            vec![anvyx_runtime::RustParamAbi::Option(Box::new(
                anvyx_runtime::RustParamAbi::Value(anvyx_runtime::ExternTypeExpr::Int),
            ))],
            anvyx_runtime::RustReturnAbi::Void,
        ),
        rir_abi(vec![option_abi], anvyx_runtime::ExternTypeExpr::Void),
    );
    option_program.enums.push(rir_result_enum(int, string));
    rir::verify(&option_program).expect("native option param RIR should verify");

    let mut slice_program = native_extern_rir(
        types.clone(),
        vec![RirExternParam {
            ty: slice,
            semantic: RirParamSemantic::Value,
            abi: RirParamAbi::Value,
            escape: RirParamEscape::NonEscaping,
        }],
        void,
        direct_rust_abi(
            vec![anvyx_runtime::RustParamAbi::Slice(Box::new(
                anvyx_runtime::RustParamAbi::Value(anvyx_runtime::ExternTypeExpr::Int),
            ))],
            anvyx_runtime::RustReturnAbi::Void,
        ),
        rir_abi(vec![slice_abi], anvyx_runtime::ExternTypeExpr::Void),
    );
    slice_program.enums.push(rir_result_enum(int, string));
    rir::verify(&slice_program).expect("native slice param RIR should verify");

    let mut result_program = native_extern_rir(
        types,
        vec![],
        result,
        direct_rust_abi(
            vec![],
            anvyx_runtime::RustReturnAbi::Result(
                Box::new(anvyx_runtime::RustReturnAbi::Value(
                    anvyx_runtime::ExternTypeExpr::Int,
                )),
                Box::new(anvyx_runtime::RustReturnAbi::Value(
                    anvyx_runtime::ExternTypeExpr::String,
                )),
            ),
        ),
        rir_abi(vec![], result_abi),
    );
    result_program.enums.push(rir_result_enum(int, string));
    rir::verify(&result_program).expect("native visible result return RIR should verify");
}

#[test]
fn rir_verify_rejects_forged_source_enum_as_visible_result_abi() {
    let int = RirTypeId::from_index(0);
    let string = RirTypeId::from_index(1);
    let result = RirTypeId::from_index(2);
    let result_abi = anvyx_runtime::ExternTypeExpr::Result(
        Box::new(anvyx_runtime::ExternTypeExpr::Int),
        Box::new(anvyx_runtime::ExternTypeExpr::String),
    );
    let mut program = native_extern_rir(
        vec![
            RirType::Int,
            RirType::String,
            RirType::Enum(RirEnumId::from_index(0)),
        ],
        vec![],
        result,
        direct_rust_abi(
            vec![],
            anvyx_runtime::RustReturnAbi::Result(
                Box::new(anvyx_runtime::RustReturnAbi::Value(
                    anvyx_runtime::ExternTypeExpr::Int,
                )),
                Box::new(anvyx_runtime::RustReturnAbi::Value(
                    anvyx_runtime::ExternTypeExpr::String,
                )),
            ),
        ),
        rir_abi(vec![], result_abi),
    );
    let mut forged = rir_result_enum(int, string);
    forged.core = None;
    program.enums.push(forged);

    assert_rir_error(program, RirVerifyErrorKind::UnsupportedRValueType);
}

#[test]
fn rir_verify_accepts_native_keyed_enum_and_dataref_named_abi() {
    let enum_ty = RirTypeId::from_index(0);
    let dataref_ty = RirTypeId::from_index(1);
    let void = RirTypeId::from_index(2);
    let enum_abi = native_named_abi("ProviderEnum");
    let dataref_abi = native_named_abi("ProviderRef");
    let mut program = native_extern_rir(
        vec![
            RirType::Enum(RirEnumId::from_index(0)),
            RirType::DataRef(RirDataRefId::from_index(0)),
            RirType::Void,
        ],
        vec![
            RirExternParam {
                ty: enum_ty,
                semantic: RirParamSemantic::Value,
                abi: RirParamAbi::Value,
                escape: RirParamEscape::NonEscaping,
            },
            RirExternParam {
                ty: dataref_ty,
                semantic: RirParamSemantic::Value,
                abi: RirParamAbi::Value,
                escape: RirParamEscape::NonEscaping,
            },
        ],
        void,
        direct_rust_abi(
            vec![
                anvyx_runtime::RustParamAbi::Value(enum_abi.clone()),
                anvyx_runtime::RustParamAbi::Value(dataref_abi.clone()),
            ],
            anvyx_runtime::RustReturnAbi::Void,
        ),
        rir_abi(
            vec![enum_abi, dataref_abi],
            anvyx_runtime::ExternTypeExpr::Void,
        ),
    );
    program.enums.push(rir_native_enum("ProviderEnum"));
    program.datarefs.push(rir_native_dataref("ProviderRef"));

    rir::verify(&program).expect("native named enum/dataref RIR should verify");
}

#[test]
fn rir_verify_rejects_source_enum_named_native_abi() {
    let enum_ty = RirTypeId::from_index(0);
    let enum_abi = native_named_abi("SourceEnum");
    let mut program = native_extern_rir(
        vec![RirType::Enum(RirEnumId::from_index(0)), RirType::Void],
        vec![RirExternParam {
            ty: enum_ty,
            semantic: RirParamSemantic::Value,
            abi: RirParamAbi::Value,
            escape: RirParamEscape::NonEscaping,
        }],
        RirTypeId::from_index(1),
        direct_rust_abi(
            vec![anvyx_runtime::RustParamAbi::Value(enum_abi.clone())],
            anvyx_runtime::RustReturnAbi::Void,
        ),
        rir_abi(vec![enum_abi], anvyx_runtime::ExternTypeExpr::Void),
    );
    let mut enm = rir_native_enum("SourceEnum");
    enm.native_key = None;
    program.enums.push(enm);

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
        lifetime: RirCellLifetime::Function,
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
                                value: RirOperand::Place(RirPlace::local(source, vec![], int)),
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

fn projected_cell_mut_place_arg_program(handle: RirMutPlaceHandle) -> RirProgram {
    let void = RirTypeId::from_index(0);
    let int = RirTypeId::from_index(1);
    let tuple_ty = RirTypeId::from_index(2);
    let tuple = RirTupleId::from_index(0);
    let field = RirFieldId::from_index(0);
    let storage = match handle {
        RirMutPlaceHandle::HeapCell { .. } => RirCellStorage::Heap,
        _ => RirCellStorage::StackScoped,
    };
    let mut program = RirProgram {
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
        cells: vec![RirCellDecl {
            payload_ty: tuple_ty,
            storage,
            ..valid_stack_cell_decl()
        }],
        functions: vec![rir_function(
            RirFunctionId::from_index(0),
            void,
            vec![],
            vec![rir_local(
                RirLocalId::from_index(0),
                tuple_ty,
                true,
                "source",
            )],
            vec![
                RirStmt::CellInit {
                    cell: RirCellRef::Owner(RirCellId::from_index(0)),
                    value: RirRValue::Tuple {
                        ty: tuple_ty,
                        fields: vec![RirOperand::Const(RirConstId::from_index(0))],
                    },
                },
                RirStmt::Eval(RirRValue::Call {
                    callee: RirCallTarget::Function(RirFunctionId::from_index(1)),
                    args: vec![RirCallArg::MutPlace(RirMutPlaceArg::projected(
                        handle,
                        vec![RirProjection::TupleField(field)],
                        int,
                    ))],
                    ty: void,
                }),
            ],
        )],
        consts: vec![RirConst {
            id: RirConstId::from_index(0),
            ty: int,
            value: RirConstValue::Int(1),
        }],
        ..RirProgram::default()
    };
    program
        .functions
        .push(mut_place_sink_function(RirFunctionId::from_index(1)));
    program
}

fn projected_scoped_place_cell_mut_place_arg_program() -> RirProgram {
    let mut program = projected_cell_mut_place_arg_program(RirMutPlaceHandle::ScopedPlaceCell {
        cell: RirScopedPlaceCellRef::Owner(RirScopedPlaceCellId::from_index(0)),
        ty: RirTypeId::from_index(2),
    });
    program.cells.clear();
    program.scoped_place_cells = vec![RirScopedPlaceCellDecl {
        id: RirScopedPlaceCellId::from_index(0),
        owner: RirFunctionId::from_index(0),
        source: scoped_source_param(RirLocalId::from_index(0), RirTypeId::from_index(2)),
        payload_ty: RirTypeId::from_index(2),
        symbol: RirSymbol::new("__place_cell0"),
    }];
    program.functions[0].params = vec![RirParam {
        local: RirLocalId::from_index(0),
        ty: RirTypeId::from_index(2),
        semantic: RirParamSemantic::MutPlace,
        abi: RirParamAbi::MutPlace,
        escape: RirParamEscape::NonEscaping,
    }];
    program.functions[0].body.stmts.remove(0);
    program
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
                lhs: RirOperand::Place(RirPlace::local(tmp, vec![], int)),
                rhs: RirOperand::Const(one),
                ty: int,
            },
        },
        RirStmt::CellSet {
            cell: RirCellRef::Owner(RirCellId::from_index(0)),
            value: RirRValue::Use(RirOperand::Place(RirPlace::local(next, vec![], int))),
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

fn scoped_source_ref_self(local: RirLocalId, ty: RirTypeId) -> RirScopedPlaceSource {
    RirScopedPlaceSource::RefSelf {
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
            place: RirPlace::local(RirLocalId::from_index(0), vec![], int),
        },
    )
}

fn assert_rir_error(program: RirProgram, kind: RirVerifyErrorKind) {
    let errors = rir::verify(&program).expect_err("verified invalid RIR");
    let found = errors.iter().any(|error| error.kind == kind);
    let kind = std::hint::black_box(kind);
    std::hint::black_box(program);
    assert!(found, "missing {kind:?}: {errors:?}");
}

fn assert_rir_type_error(program: RirProgram) {
    let errors = rir::verify(&program).expect_err("verified invalid RIR");
    let found = errors
        .iter()
        .any(|error| matches!(error.kind, RirVerifyErrorKind::TypeMismatch { .. }));
    std::hint::black_box(program);
    assert!(found, "missing expected type mismatch: {errors:?}");
}

fn add_global_mut_place_callee(program: &mut RirProgram) -> RirFunctionId {
    let callee = RirFunctionId::from_index(2);
    program.functions.push(RirFunction {
        id: callee,
        air_id: None,
        symbol: RirSymbol::new("bump"),
        params: vec![rir_param(
            RirLocalId::from_index(0),
            RirTypeId::from_index(0),
            RirParamSemantic::MutPlace,
            RirParamAbi::MutPlace,
        )],
        ret: RirReturn {
            ty: RirTypeId::from_index(1),
        },
        locals: vec![rir_local(
            RirLocalId::from_index(0),
            RirTypeId::from_index(0),
            true,
            "x",
        )],
        body: RirStructuredBlock {
            stmts: vec![],
            term: RirTerm::Return(None),
        },
    });
    callee
}

fn global_mut_place_call(callee: RirFunctionId, global: RirGlobalId, ty: RirTypeId) -> RirStmt {
    RirStmt::Eval(RirRValue::Call {
        callee: RirCallTarget::Function(callee),
        args: vec![RirCallArg::MutPlace(RirMutPlaceArg::global(global, ty))],
        ty: RirTypeId::from_index(1),
    })
}

fn set_global_string_payload(program: &mut RirProgram) -> (RirTypeId, RirConstId) {
    let string = RirTypeId::from_index(2);
    let value = RirConstId::from_index(1);
    program.types.push(RirType::String);
    program.consts.push(RirConst {
        id: value,
        ty: string,
        value: RirConstValue::String("ready".into()),
    });
    program.globals[0].ty = string;
    program.functions[0].ret.ty = string;
    program.functions[0].body.term = RirTerm::Return(Some(RirOperand::Const(value)));
    for stmt in &mut program.functions[1].body.stmts {
        if let RirStmt::GlobalSetRoot {
            value: root_value, ..
        } = stmt
        {
            *root_value = RirRValue::Use(RirOperand::Const(value));
        }
    }
    (string, value)
}

fn add_string_global_borrow_callee(program: &mut RirProgram) -> (RirTypeId, RirFunctionId) {
    let (string, _) = set_global_string_payload(program);
    let local = RirLocalId::from_index(0);
    let callee = RirFunctionId::from_index(2);
    program.functions.push(RirFunction {
        id: callee,
        air_id: None,
        symbol: RirSymbol::new("borrow"),
        params: vec![rir_param(
            local,
            string,
            RirParamSemantic::SharedBorrow,
            RirParamAbi::SharedBorrow,
        )],
        ret: RirReturn {
            ty: RirTypeId::from_index(1),
        },
        locals: vec![rir_local(local, string, false, "x")],
        body: RirStructuredBlock {
            stmts: vec![],
            term: RirTerm::Return(None),
        },
    });
    (string, callee)
}

fn global_list_collection_loan_rir(
    mode: RirCollectionLoanMode,
    main_locals: Vec<RirLocal>,
    consts: Vec<RirConst>,
    body: RirStructuredBlock,
) -> RirProgram {
    let int = RirTypeId::from_index(0);
    let void = RirTypeId::from_index(1);
    let list = RirTypeId::from_index(2);
    let global = RirGlobalId::from_index(0);
    let init = RirFunctionId::from_index(0);
    let main = RirFunctionId::from_index(1);
    let xs = RirLocalId::from_index(0);
    let mut init_local = rir_local(xs, list, true, "xs");
    init_local.initialized = false;

    let mut program = RirProgram {
        types: vec![RirType::Int, RirType::Void, RirType::List(int)],
        collection_storages: vec![rir_list_storage(list, int)],
        consts,
        globals: vec![RirGlobal {
            id: global,
            air_id: air::GlobalId::from_index(0),
            module: air::ModuleId::from_index(0),
            name: RirSymbol::new("game.xs"),
            slot_symbol: RirSymbol::new("g0_xs"),
            ty: list,
            mutable: true,
            init,
        }],
        functions: vec![
            rir_function(
                init,
                list,
                vec![],
                vec![init_local],
                vec![RirStmt::Init {
                    local: xs,
                    value: RirRValue::List {
                        ty: list,
                        elems: vec![],
                    },
                }],
            ),
            rir_function(
                main,
                void,
                vec![],
                main_locals,
                vec![
                    RirStmt::GlobalEnsure { global },
                    RirStmt::CollectionLoanScope(RirCollectionLoanScope {
                        root: RirCollectionAccess::Direct(RirPlace::global(global, vec![], list)),
                        root_kind: RirCollectionRootKind::List,
                        mode,
                        body,
                    }),
                ],
            ),
        ],
        ..RirProgram::default()
    };
    program.functions[0].body.term = RirTerm::Return(Some(RirOperand::Place(rir_place(xs, list))));
    program
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
    edit(&mut program);
    program
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
        native_ref: false,
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
            native_key: None,
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
    RirMutPlaceArg::dataref(
        RirOperand::Place(dataref_access_place(0, 1)),
        RirDataRefId::from_index(0),
        projections,
        ty,
    )
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
    let RirMutPlaceAccess::DataRef { object, dataref } = &mut arg.access else {
        unreachable!();
    };
    edit(object, dataref, &mut arg.projections, &mut arg.ty);
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
        collection_storages: vec![
            rir_list_storage(list, int),
            rir_map_storage(1, map, int, int),
        ],
        structs: vec![RirStruct {
            id: RirStructId::from_index(0),
            air_id: None,
            symbol: RirSymbol::new("Point"),
            display: RirSymbol::new("Point"),
            native_path: None,
            native_ref: false,
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
            native_path: None,
            native_key: None,
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
            native_key: None,
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
    RirPlace::local(
        RirLocalId::from_index(local),
        vec![],
        RirTypeId::from_index(ty),
    )
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

fn rir_field(id: usize, symbol: &str, ty: RirTypeId) -> RirField {
    RirField {
        id: RirFieldId::from_index(id),
        symbol: RirSymbol::new(symbol),
        ty,
    }
}

fn rir_struct(id: usize, symbol: &str, fields: Vec<RirField>) -> RirStruct {
    RirStruct {
        id: RirStructId::from_index(id),
        air_id: None,
        symbol: RirSymbol::new(symbol),
        display: RirSymbol::new(symbol),
        native_path: None,
        native_ref: false,
        native_key: Some(anvyx_runtime::ExternTypeKey {
            module: anvyx_runtime::ModulePath { segments: vec![] },
            name: "Window".to_string(),
        }),
        copyable: false,
        fields,
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
    RirPlace::local(local, vec![], ty)
}

fn rir_list_storage(value_ty: RirTypeId, elem_ty: RirTypeId) -> RirCollectionStorage {
    rir_list_storage_id(0, value_ty, elem_ty)
}

fn rir_list_storage_id(id: usize, value_ty: RirTypeId, elem_ty: RirTypeId) -> RirCollectionStorage {
    RirCollectionStorage {
        id: RirCollectionStorageId::from_index(id),
        value_ty,
        kind: RirCollectionStorageKind::List { elem_ty },
        symbol: RirSymbol::new(format!("list_storage{}", value_ty.index())),
    }
}

fn rir_map_storage(
    id: usize,
    value_ty: RirTypeId,
    key_ty: RirTypeId,
    value_elem_ty: RirTypeId,
) -> RirCollectionStorage {
    RirCollectionStorage {
        id: RirCollectionStorageId::from_index(id),
        value_ty,
        kind: RirCollectionStorageKind::Map {
            key_ty,
            value_ty: value_elem_ty,
        },
        symbol: RirSymbol::new(format!("map_storage{}", value_ty.index())),
    }
}

fn rir_global_place(ty: RirTypeId) -> RirPlace {
    RirPlace::global(RirGlobalId::from_index(0), vec![], ty)
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
    let list = program.alloc_type(TypeData::Slice(int));
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

fn dataref_map_index_assignment_program() -> Program {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let void = program.alloc_type(TypeData::Void);
    let map = program.alloc_type(TypeData::Map {
        key: int,
        value: int,
        order: air::MapOrder::Insertion,
    });
    let module = program.alloc_module(root_module());
    let aggregate = dataref_decl(&mut program, module, map);
    let node = program.alloc_type(TypeData::DataRef(aggregate));
    let one = program.const_arena.alloc(ConstData {
        ty: int,
        value: ConstValue::Int(1),
    });
    let two = program.const_arena.alloc(ConstData {
        ty: int,
        value: ConstValue::Int(2),
    });
    let arg = air::LocalId::from_index(0);
    let map_place = Place {
        root: PlaceRoot::Local(arg),
        projection: vec![Projection::Field(air::FieldId::from_index(0))],
        ty: map,
    };
    let main = program.alloc_function(Function {
        name: Ident::new("update"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![param("node", node, ParamMode::Value, arg)], void),
        locals: vec![Local {
            name: None,
            binding: None,
            ty: node,
            mutability: Mutability::Mutable,
            kind: LocalKind::Arg,
        }],
        body: structured_body(
            vec![Statement::Eval(RValue::MapInsert {
                map: map_place,
                key: Operand::Const(one),
                value: Operand::Const(two),
                kind: air::MapWriteKind::IndexedAssignment,
            })],
            air::AirTail::Return(None),
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

fn capture_cell_dataref_source_ref_arg_program() -> Program {
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
        lifetime: CaptureCellLifetime::Function,
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
            native_path: None,
            native_key: None,
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
            native_path: None,
            native_key: None,
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
            native_ref: false,
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
            native_path: None,
            native_key: None,
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
fn rir_option_match_accepts_non_escaping_mut_place_subject() {
    let mut program = option_match_rir();
    let option = RirTypeId::from_index(1);
    let opt = RirLocalId::from_index(0);
    let payload = RirLocalId::from_index(1);
    program.functions[0].locals[0].mutable = true;
    program.functions[0].locals[1].mutable = true;
    program.functions[0].locals[1].payload_ref = true;
    if let RirStmt::OptionMatch(match_) = &mut program.functions[0].body.stmts[0] {
        match_.subject =
            RirOptionSubject::MutPlace(RirMutPlaceArg::local(RirPlace::local(opt, vec![], option)));
        match_.payload = Some(payload);
        match_.payload_ref = true;
        match_.some_block.term = RirTerm::None;
        match_.none_block.term = RirTerm::None;
    }

    rir::verify(&program).expect("non-escaping mutable-place option subjects are supported");
}

#[test]
fn rir_option_match_accepts_escaping_mut_place_subject() {
    let mut program = option_match_rir();
    let option = RirTypeId::from_index(1);
    let opt = RirLocalId::from_index(0);
    let payload = RirLocalId::from_index(1);
    program.functions[0].locals[0].mutable = true;
    program.functions[0].locals[1].mutable = true;
    program.functions[0].locals[1].payload_ref = true;
    if let RirStmt::OptionMatch(match_) = &mut program.functions[0].body.stmts[0] {
        match_.subject =
            RirOptionSubject::MutPlace(RirMutPlaceArg::local(RirPlace::local(opt, vec![], option)));
        match_.payload = Some(payload);
        match_.payload_ref = true;
        match_.payload_escapes = true;
        match_.some_block.term = RirTerm::None;
        match_.none_block.term = RirTerm::Unreachable;
    }

    rir::verify(&program).expect("escaping mutable-place option subjects use live alias locals");
}

#[test]
fn rir_option_match_rejects_bad_mut_place_subject_type() {
    let mut program = option_match_rir();
    let int = RirTypeId::from_index(0);
    let opt = RirLocalId::from_index(0);
    program.functions[0].locals[0].ty = int;
    program.functions[0].locals[0].mutable = true;
    if let RirStmt::OptionMatch(match_) = &mut program.functions[0].body.stmts[0] {
        match_.subject =
            RirOptionSubject::MutPlace(RirMutPlaceArg::local(RirPlace::local(opt, vec![], int)));
    }

    let errors = rir::verify(&program).expect_err("option subject must have option type");
    assert!(
        errors
            .iter()
            .any(|error| error.kind == RirVerifyErrorKind::UnsupportedRValueType)
    );
}

#[test]
fn rir_option_match_rejects_bad_discriminant_payload_type_and_mutable_payload() {
    let mut bad_discr = option_match_rir();
    let int = RirTypeId::from_index(0);
    if let RirStmt::OptionMatch(match_) = &mut bad_discr.functions[0].body.stmts[0]
        && let RirOptionSubject::Place(subject) = &mut match_.subject
    {
        subject.ty = int;
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
    if let RirStmt::OptionMatch(match_) = &mut bad_payload.functions[0].body.stmts[0]
        && let RirOptionSubject::Place(subject) = &mut match_.subject
    {
        subject.ty = option;
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
fn rir_optional_some_accepts_shareable_slice_descriptor() {
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
            value: RirOperand::Place(RirPlace::local(RirLocalId::from_index(1), vec![], slice)),
            ty: option,
        })],
        term: RirTerm::Unreachable,
    };

    rir::verify(&program).expect("slice descriptor should be shareable");
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
        if let RirOptionSubject::Place(subject) = &mut match_.subject {
            subject.ty = RirTypeId::from_index(1);
        }
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
        if let RirOptionSubject::Place(subject) = &mut match_.subject {
            subject.ty = RirTypeId::from_index(1);
        }
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
        .push(RirStmt::Eval(RirRValue::Use(RirOperand::Place(
            RirPlace::local(RirLocalId::from_index(1), vec![], RirTypeId::from_index(0)),
        ))));
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
        .push(RirStmt::Eval(RirRValue::Use(RirOperand::Place(
            RirPlace::local(RirLocalId::from_index(1), vec![], RirTypeId::from_index(0)),
        ))));
    let errors = rir::verify(&scoped_ref_used_after_match)
        .expect_err("non-escaping payload_ref must not outlive match arm");
    assert!(errors.iter().any(|error| {
        error.kind == RirVerifyErrorKind::UninitializedLocal(RirLocalId::from_index(1))
    }));
}

#[test]
fn rir_accepts_map_entry_match_and_emits_map_value_ops() {
    let program = map_entry_match_rir(false);
    let source = emit::emit(&rir::verify(&program).expect("RIR verify failed")).into_string();

    assert!(source.contains("MapValueOps"));
    assert!(source.contains("begin_value_loan_by_key"));
    assert!(source.contains("ScopedMutPlaceCell::new"));
    assert!(source.contains("value.set(rt"));
}

#[test]
fn rir_accepts_escaping_map_entry_match() {
    rir::verify(&map_entry_match_rir(true)).expect("RIR verify failed");
}

#[test]
fn rir_rejects_map_entry_key_type_mismatch() {
    let mut program = map_entry_match_rir(false);
    program.consts[0].ty = RirTypeId::from_index(2);

    assert_rir_type_error(program);
}

#[test]
fn rir_rejects_map_entry_payload_type_mismatch() {
    let mut program = map_entry_match_rir(false);
    program.functions[0].locals[1].ty = RirTypeId::from_index(2);

    assert_rir_type_error(program);
}

#[test]
fn rir_rejects_map_entry_immutable_map_root() {
    let mut program = map_entry_match_rir(false);
    program.functions[0].locals[0].mutable = false;

    assert_rir_error(program, RirVerifyErrorKind::ImmutableAssign);
}

fn map_entry_match_rir(payload_escapes: bool) -> RirProgram {
    let void = RirTypeId::from_index(0);
    let int = RirTypeId::from_index(1);
    let map_ty = RirTypeId::from_index(3);
    let map = RirLocalId::from_index(0);
    let payload = RirLocalId::from_index(1);
    let mut payload_local = rir_local(payload, int, true, "value");
    payload_local.initialized = false;
    payload_local.payload_ref = true;
    RirProgram {
        types: vec![
            RirType::Void,
            RirType::Int,
            RirType::Bool,
            RirType::Map {
                key: int,
                value: int,
            },
        ],
        collection_storages: vec![rir_map_storage(0, map_ty, int, int)],
        functions: vec![RirFunction {
            id: RirFunctionId::from_index(0),
            air_id: None,
            symbol: RirSymbol::new("f"),
            params: vec![],
            ret: RirReturn { ty: void },
            locals: vec![rir_local(map, map_ty, true, "scores"), payload_local],
            body: RirStructuredBlock {
                stmts: vec![RirStmt::MapEntryMatch(RirMapEntryMatch {
                    map: RirMutPlaceArg::local(RirPlace::local(map, vec![], map_ty)),
                    key: RirOperand::Const(RirConstId::from_index(0)),
                    payload: Some(payload),
                    payload_escapes,
                    some_block: RirStructuredBlock {
                        stmts: vec![RirStmt::Assign {
                            dst: RirPlace::local(payload, vec![], int),
                            value: RirRValue::Use(RirOperand::Const(RirConstId::from_index(0))),
                        }],
                        term: RirTerm::None,
                    },
                    none_block: RirStructuredBlock {
                        stmts: vec![],
                        term: if payload_escapes {
                            RirTerm::Unreachable
                        } else {
                            RirTerm::None
                        },
                    },
                })],
                term: RirTerm::Return(None),
            },
        }],
        consts: vec![RirConst {
            id: RirConstId::from_index(0),
            ty: int,
            value: RirConstValue::Int(1),
        }],
        ..RirProgram::default()
    }
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
                    subject: RirOptionSubject::Place(RirPlace::local(opt, vec![], option)),
                    payload: Some(payload),
                    payload_ref: false,
                    payload_escapes: false,
                    some_block: RirStructuredBlock {
                        stmts: vec![],
                        term: RirTerm::Return(Some(RirOperand::Place(RirPlace::local(
                            payload,
                            vec![],
                            int,
                        )))),
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
        assert!(text.contains("(v0)[anvyx_runtime::checked_index_result(v1, 2, \"array\")?]"));
        assert!(text.contains("-> Result<i64, anvyx_runtime::RuntimeError>"));
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
    fn noncopy_array_value_copy_materializes() {
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

        let source = plan_source(program);
        assert!(source.as_str().contains("share()"));
        let output = run_source(source);
        assert_eq!(output.status, SourceJobStatus::Success);
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
                term: RirTerm::Return(Some(RirOperand::Place(RirPlace::local(
                    RirLocalId::from_index(0),
                    vec![RirProjection::Index(RirLocalId::from_index(1))],
                    int,
                )))),
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
        assert!(text.contains("anvyx_runtime::AnvList::from_elems(rt, types.list_storage"));
        assert!(!text.contains("Vec<"));
        assert!(!text.contains("vec!"));
        assert!(text.contains("-> Result<i64, anvyx_runtime::RuntimeError>"));
        assert!(text.contains("let __anv_list = &(v0); let index = anvyx_runtime::checked_index_result(v1, __anv_list.len(), \"list\")?; __anv_list.elem_at_shared(rt, index, __anv_list.structural_version())?"));
        assert!(text.contains(".len() as i64"));
        let output = run_source(source);
        assert_eq!(output.status, SourceJobStatus::Success, "{}", output.stderr);
    }

    #[test]
    fn map_structural_ops_are_fallible_checked_calls() {
        let mut program = Program::default();
        let int = program.alloc_type(TypeData::Int);
        let void = program.alloc_type(TypeData::Void);
        let optional = program.alloc_type(TypeData::Optional(int));
        let map = program.alloc_type(TypeData::Map {
            key: int,
            value: int,
            order: air::MapOrder::Insertion,
        });
        let module = program.alloc_module(root_module());
        let one = int_const(&mut program, int, 1);
        let two = int_const(&mut program, int, 2);
        let map_local = air::LocalId::from_index(0);
        let main = program.alloc_function(Function {
            name: Ident::new("main"),
            module,
            kind: FunctionKind::Normal,
            owner: None,
            specialization: None,
            signature: Signature::new(vec![], void),
            locals: vec![mut_local(map, LocalKind::Temp)],
            body: structured_body(
                vec![
                    Statement::Init {
                        local: map_local,
                        value: RValue::Aggregate {
                            kind: AggregateCtor::Map,
                            fields: vec![],
                            ty: map,
                        },
                    },
                    Statement::Eval(RValue::MapInsert {
                        map: place(map_local, map),
                        key: Operand::Const(one),
                        value: Operand::Const(two),
                        kind: air::MapWriteKind::StructuralInsert,
                    }),
                    Statement::Eval(RValue::MapRemove {
                        map: place(map_local, map),
                        key: Operand::Const(one),
                        ty: optional,
                    }),
                ],
                air::AirTail::Return(None),
            ),
        });
        program.module_mut(module).functions.push(main);
        program.set_entry(main);

        let source = plan_source(program);
        let text = source.as_str();
        assert!(text.contains("-> Result<(), anvyx_runtime::RuntimeError>"));
        assert!(text.contains(".remove(rt,"));
        let output = run_source(source);
        assert_eq!(output.status, SourceJobStatus::Success, "{}", output.stderr);
    }

    #[test]
    fn map_entry_index_oob_returns_runtime_error() {
        let mut program = Program::default();
        let int = program.alloc_type(TypeData::Int);
        let void = program.alloc_type(TypeData::Void);
        let entry = program.alloc_type(TypeData::Tuple(vec![int, int]));
        let map = program.alloc_type(TypeData::Map {
            key: int,
            value: int,
            order: air::MapOrder::Insertion,
        });
        let module = program.alloc_module(root_module());
        let one = int_const(&mut program, int, 1);
        let two = int_const(&mut program, int, 2);
        let map_local = air::LocalId::from_index(0);
        let index_local = air::LocalId::from_index(1);
        let main = program.alloc_function(Function {
            name: Ident::new("main"),
            module,
            kind: FunctionKind::Normal,
            owner: None,
            specialization: None,
            signature: Signature::new(vec![], void),
            locals: vec![local(map, LocalKind::Temp), local(int, LocalKind::Temp)],
            body: structured_body(
                vec![
                    Statement::Init {
                        local: map_local,
                        value: RValue::Aggregate {
                            kind: AggregateCtor::Map,
                            fields: vec![Operand::Const(one), Operand::Const(two)],
                            ty: map,
                        },
                    },
                    Statement::Init {
                        local: index_local,
                        value: RValue::Use(Operand::Const(one)),
                    },
                    Statement::Eval(RValue::MapEntryAt {
                        map: place(map_local, map),
                        index: index_local,
                        ty: entry,
                    }),
                ],
                air::AirTail::Return(None),
            ),
        });
        program.module_mut(module).functions.push(main);
        program.set_entry(main);

        let source = plan_source(program);
        assert!(
            source
                .as_str()
                .contains("checked_index_result(v1, __anv_map.len(), \"map entry\")?")
        );
        let output = run_source(source);
        assert!(matches!(output.status, SourceJobStatus::RunFailed(_)));
        assert!(
            output
                .stderr
                .contains("map entry index 1 out of bounds for len 1")
        );
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
        assert!(
            source
                .as_str()
                .contains("&anvyx_runtime::AnvList<'cx, i64>")
        );
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
    fn array_slice_view_emits_runtime_descriptor() {
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
        assert!(text.contains("anvyx_runtime::AnvSlice::from_raw_parts"));
        assert!(!text.contains("&[i64]"));
        let output = run_source(source);
        assert_eq!(output.status, SourceJobStatus::Success, "{}", output.stderr);
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
        native_path: None,
        native_key: None,
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
                value: RirOperand::Place(RirPlace::local(RirLocalId::from_index(0), vec![], state)),
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
        native_path: None,
        native_key: None,
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
            native_path: None,
            native_key: None,
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
        air_id: None,
        symbol: RirSymbol::new("substring"),
        kind: RirExternKind::Native(rir::RirNativeExtern::new(
            vec!["host".to_string(), "substring".to_string()],
            anvyx_runtime::RustExternAbi {
                params: vec![],
                ret: anvyx_runtime::RustReturnAbi::Option(Box::new(
                    anvyx_runtime::RustReturnAbi::Value(anvyx_runtime::ExternTypeExpr::String),
                )),
                fallible: false,
                support: anvyx_runtime::RustAbiSupport::Direct,
                ctx: anvyx_runtime::RustWrapperCtx::HiddenRuntime,
            },
        )),
        params: vec![],
        ret: option,
        abi: rir_abi(
            vec![],
            anvyx_runtime::ExternTypeExpr::Option(Box::new(anvyx_runtime::ExternTypeExpr::String)),
        ),
    });
    program
}

fn native_string_return_program() -> RirProgram {
    let mut program = empty_rir_function(RirType::String);
    program.consts.clear();
    program.externs.push(RirExtern {
        id: RirExternId::from_index(0),
        air_id: None,
        symbol: RirSymbol::new("host_string"),
        kind: RirExternKind::Native(rir::RirNativeExtern::new(
            vec!["host".to_string(), "string".to_string()],
            anvyx_runtime::RustExternAbi {
                params: vec![],
                ret: anvyx_runtime::RustReturnAbi::Value(anvyx_runtime::ExternTypeExpr::String),
                fallible: false,
                support: anvyx_runtime::RustAbiSupport::Direct,
                ctx: anvyx_runtime::RustWrapperCtx::HiddenRuntime,
            },
        )),
        params: vec![],
        ret: RirTypeId::from_index(0),
        abi: rir_abi(vec![], anvyx_runtime::ExternTypeExpr::String),
    });
    program.functions[0].body.term = RirTerm::Return(Some(RirOperand::Place(RirPlace::local(
        RirLocalId::from_index(0),
        vec![],
        RirTypeId::from_index(0),
    ))));
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

fn host_subscription_support() -> anvyx_runtime::RustProviderSupport {
    let mut callback = native_callback_sig();
    callback.policy.escape = anvyx_runtime::CallbackEscape::Escaping;
    let mut support = provider_support(
        "host_subscription",
        vec![
            function_binding(
                "host_subscription",
                "host",
                &["host", "subscribe"],
                "subscribe",
                vec![anvyx_runtime::RustParamAbi::EscapingLambda(callback)],
                anvyx_runtime::RustReturnAbi::Value(anvyx_runtime::ExternTypeExpr::Int),
                false,
            ),
            function_binding(
                "host_subscription",
                "host",
                &["host", "trigger"],
                "trigger",
                vec![anvyx_runtime::RustParamAbi::Value(
                    anvyx_runtime::ExternTypeExpr::Int,
                )],
                anvyx_runtime::RustReturnAbi::Void,
                false,
            ),
            function_binding(
                "host_subscription",
                "host",
                &["host", "close"],
                "close",
                vec![anvyx_runtime::RustParamAbi::Value(
                    anvyx_runtime::ExternTypeExpr::Int,
                )],
                anvyx_runtime::RustReturnAbi::Void,
                false,
            ),
        ],
    );
    support.modules[0].bindings[0].abi.support =
        anvyx_runtime::RustAbiSupport::NeedsWrapperConversion;
    support.modules[0].bindings[0].abi.ctx = anvyx_runtime::RustWrapperCtx::None;
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

fn host_subscription_extern(
    program: &mut Program,
    name: &str,
    params: Vec<(air::TypeId, ParamMode)>,
    ret: air::TypeId,
) -> air::ExternId {
    let id = extern_in_module(
        program,
        &["host_subscription"],
        name,
        params,
        ret,
        ExternMember::FreeFunction,
    );
    program.externs[id.index()].binding = Some(provider_binding("host_subscription", name));
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

#[derive(Clone, Copy)]
enum NativeLambdaArgKind {
    FunctionRef,
    ZeroCapture,
    EscapingZeroCapture,
    ReadonlyCapture,
    EscapingReadonlyCapture,
    CaptureCell,
    EscapingCaptureCell,
}

fn native_scoped_lambda_air() -> Program {
    native_scoped_lambda_air_with(NativeLambdaArgKind::FunctionRef)
}

fn native_escaping_lambda_air_with(kind: NativeLambdaArgKind) -> Program {
    let mut program = native_scoped_lambda_air_with(kind);
    set_extern_param_escape(
        &mut program,
        air::ExternId::from_index(0),
        0,
        ParamEscape::Escaping,
    );
    program
}

fn native_escaping_lambda_trigger_air(kind: NativeLambdaArgKind) -> Program {
    let mut program = native_escaping_lambda_air_with(kind);
    match kind {
        NativeLambdaArgKind::EscapingReadonlyCapture => {
            make_lambda_add_readonly_capture(&mut program);
        }
        NativeLambdaArgKind::EscapingCaptureCell => {
            make_lambda_increment_capture_cell(&mut program);
        }
        _ => {}
    }
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

fn native_escaping_lambda_tuple_air(kind: NativeLambdaArgKind) -> Program {
    let mut program = native_escaping_lambda_trigger_air(kind);
    let entry = program.entry.expect("entry function");
    let lambda_ty = program.externs[0].params[0].ty;
    let tuple_ty = program.alloc_type(TypeData::Tuple(vec![lambda_ty]));
    let function = program.function_mut(entry);
    let lambda_stmt = function
        .body
        .block
        .stmts
        .iter()
        .position(|stmt| matches!(stmt, Statement::Init { .. }))
        .expect("lambda init");
    let lambda_local = match &function.body.block.stmts[lambda_stmt] {
        Statement::Init { local, .. } => *local,
        _ => unreachable!(),
    };
    let tuple_local = air::LocalId::from_index(function.locals.len());
    function.locals.push(local(tuple_ty, LocalKind::Temp));
    function.body.block.stmts.insert(
        lambda_stmt + 1,
        Statement::Init {
            local: tuple_local,
            value: RValue::Aggregate {
                kind: AggregateCtor::Tuple,
                fields: vec![Operand::Place(place(lambda_local, lambda_ty))],
                ty: tuple_ty,
            },
        },
    );
    for stmt in &mut function.body.block.stmts {
        let Statement::Eval(RValue::Call { args, .. }) = stmt else {
            continue;
        };
        if args.len() == 1 {
            args[0] = CallArg::Value(Operand::Place(tuple_lambda_projection(
                tuple_local,
                lambda_ty,
            )));
        }
    }
    program
}

fn tuple_lambda_projection(local: air::LocalId, ty: air::TypeId) -> Place {
    Place {
        root: PlaceRoot::Local(local),
        projection: vec![Projection::TupleField(0)],
        ty,
    }
}

fn make_lambda_add_readonly_capture(program: &mut Program) {
    let int = program.functions[1].signature.return_mode.ty();
    let arg = air::LocalId::from_index(0);
    let sum = air::LocalId::from_index(program.functions[1].locals.len());
    program.functions[1]
        .locals
        .push(local(int, LocalKind::Temp));
    program.functions[1].body = structured_body(
        vec![Statement::Init {
            local: sum,
            value: RValue::Binary {
                op: BinaryOp::Add,
                lhs: Operand::Place(place(arg, int)),
                rhs: Operand::Place(root_place(
                    PlaceRoot::LambdaCapture(air::LambdaCaptureSlotId::from_index(0)),
                    int,
                )),
                ty: int,
            },
        }],
        air::AirTail::Return(Some(Operand::Place(place(sum, int)))),
    );
}

fn make_lambda_increment_capture_cell(program: &mut Program) {
    let int = program.functions[1].signature.return_mode.ty();
    let arg = air::LocalId::from_index(0);
    let sum = air::LocalId::from_index(program.functions[1].locals.len());
    let capture = root_place(
        PlaceRoot::LambdaCapture(air::LambdaCaptureSlotId::from_index(0)),
        int,
    );
    program.functions[1]
        .locals
        .push(local(int, LocalKind::Temp));
    program.functions[1].body = structured_body(
        vec![
            Statement::Init {
                local: sum,
                value: RValue::Binary {
                    op: BinaryOp::Add,
                    lhs: Operand::Place(capture.clone()),
                    rhs: Operand::Place(place(arg, int)),
                    ty: int,
                },
            },
            Statement::Assign {
                dst: capture.clone(),
                value: RValue::Use(Operand::Place(place(sum, int))),
            },
        ],
        air::AirTail::Return(Some(Operand::Place(capture))),
    );
}

fn native_scoped_lambda_air_with(kind: NativeLambdaArgKind) -> Program {
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
    let lambda_ty = program.alloc_type(TypeData::Function(sig.clone()));
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
    let has_source_local = !matches!(
        kind,
        NativeLambdaArgKind::ZeroCapture
            | NativeLambdaArgKind::EscapingZeroCapture
            | NativeLambdaArgKind::FunctionRef
    );
    let lambda_local = air::LocalId::from_index(usize::from(has_source_local));
    let captured = air::LocalId::from_index(0);
    let binding = BindingId::from_index(0);
    let owner = FunctionId::from_index(2);
    let cell = if matches!(
        kind,
        NativeLambdaArgKind::CaptureCell | NativeLambdaArgKind::EscapingCaptureCell
    ) {
        Some(capture_cell(&mut program, owner, captured, binding, int))
    } else {
        None
    };
    let lambda = if matches!(kind, NativeLambdaArgKind::FunctionRef) {
        None
    } else {
        let escape = match kind {
            NativeLambdaArgKind::EscapingZeroCapture
            | NativeLambdaArgKind::EscapingReadonlyCapture
            | NativeLambdaArgKind::EscapingCaptureCell => LambdaEscape::Escaping,
            _ => LambdaEscape::NonEscaping,
        };
        let captures = match kind {
            NativeLambdaArgKind::ReadonlyCapture | NativeLambdaArgKind::EscapingReadonlyCapture => {
                vec![air::LambdaCaptureDecl::ReadonlyLocal {
                    binding,
                    source: CaptureLocalSource {
                        owner,
                        local: captured,
                    },
                    ty: int,
                }]
            }
            NativeLambdaArgKind::CaptureCell | NativeLambdaArgKind::EscapingCaptureCell => {
                vec![air::LambdaCaptureDecl::CaptureCell {
                    binding,
                    cell: cell.expect("capture cell missing"),
                    ty: int,
                }]
            }
            NativeLambdaArgKind::ZeroCapture
            | NativeLambdaArgKind::EscapingZeroCapture
            | NativeLambdaArgKind::FunctionRef => vec![],
        };
        Some(program.alloc_lambda(LambdaDecl {
            source: ExprId(0),
            module,
            owner,
            body: lambda_body,
            signature: sig,
            escape,
            captures,
        }))
    };
    program.function_mut(lambda_body).kind =
        lambda.map_or(FunctionKind::Normal, FunctionKind::Lambda);

    let mut locals = vec![
        Local {
            name: None,
            binding: Some(binding),
            ty: int,
            mutability: if matches!(
                kind,
                NativeLambdaArgKind::CaptureCell | NativeLambdaArgKind::EscapingCaptureCell
            ) {
                Mutability::Mutable
            } else {
                Mutability::Immutable
            },
            kind: LocalKind::User,
        },
        local(lambda_ty, LocalKind::Temp),
    ];
    if !has_source_local {
        locals.remove(0);
    }
    let mut stmts = vec![];
    if has_source_local
        && !matches!(
            kind,
            NativeLambdaArgKind::CaptureCell | NativeLambdaArgKind::EscapingCaptureCell
        )
    {
        stmts.push(Statement::Init {
            local: captured,
            value: RValue::Use(Operand::Const(int_const(&mut program, int, 1))),
        });
    }
    if let Some(cell) = cell {
        stmts.push(init_cell(&mut program, cell, int));
    }
    let lambda_value = match kind {
        NativeLambdaArgKind::FunctionRef => RValue::FunctionRef {
            function: callback,
            ty: lambda_ty,
        },
        NativeLambdaArgKind::ZeroCapture | NativeLambdaArgKind::EscapingZeroCapture => {
            RValue::MakeLambda {
                lambda: lambda.expect("lambda missing"),
                captures: vec![],
                ty: lambda_ty,
            }
        }
        NativeLambdaArgKind::ReadonlyCapture | NativeLambdaArgKind::EscapingReadonlyCapture => {
            RValue::MakeLambda {
                lambda: lambda.expect("lambda missing"),
                captures: vec![air::LambdaCaptureArg::ReadonlyLocal {
                    value: Operand::Place(place(captured, int)),
                }],
                ty: lambda_ty,
            }
        }
        NativeLambdaArgKind::CaptureCell | NativeLambdaArgKind::EscapingCaptureCell => {
            RValue::MakeLambda {
                lambda: lambda.expect("lambda missing"),
                captures: vec![air::LambdaCaptureArg::CaptureCell {
                    cell: cell.expect("capture cell missing"),
                }],
                ty: lambda_ty,
            }
        }
    };
    stmts.extend([
        Statement::Init {
            local: lambda_local,
            value: lambda_value,
        },
        Statement::Eval(RValue::Call {
            callee: Callee::Extern(ext),
            args: vec![CallArg::Value(Operand::Place(place(
                lambda_local,
                lambda_ty,
            )))],
        }),
    ]);
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

fn native_escaping_lambda_rir() -> RirProgram {
    let mut program = native_scoped_lambda_rir();
    let mut callback = native_callback_sig();
    callback.policy.escape = anvyx_runtime::CallbackEscape::Escaping;
    let RirExternKind::Native(native) = &mut program.externs[0].kind;
    native.abi.params[0] = anvyx_runtime::RustParamAbi::EscapingLambda(callback);
    program.externs[0].params[0].semantic = RirParamSemantic::EscapingLambda;
    program.externs[0].params[0].abi = RirParamAbi::EscapingLambda;
    program.externs[0].params[0].escape = RirParamEscape::Escaping;
    let RirStmt::Eval(RirRValue::Call { args, .. }) = &mut program.functions[0].body.stmts[0]
    else {
        unreachable!()
    };
    args[0] = RirCallArg::EscapingLambda {
        callee: RirOperand::Place(rir_place(
            RirLocalId::from_index(0),
            RirTypeId::from_index(2),
        )),
        sig: RirLambdaSigId::from_index(0),
    };
    program
}

fn native_scoped_lambda_rir() -> RirProgram {
    let void = RirTypeId::from_index(0);
    let int = RirTypeId::from_index(1);
    let lambda_ty = RirTypeId::from_index(2);
    let sig = RirLambdaSigId::from_index(0);
    let local = RirLocalId::from_index(0);
    RirProgram {
        types: vec![RirType::Void, RirType::Int, RirType::Lambda(sig)],
        lambda_sigs: vec![RirLambdaSig {
            id: sig,
            params: vec![RirLambdaParam {
                ty: int,
                semantic: RirParamSemantic::Value,
                abi: RirParamAbi::Value,
                escape: RirParamEscape::NonEscaping,
            }],
            ret: int,
        }],
        externs: vec![RirExtern {
            id: RirExternId::from_index(0),
            air_id: None,
            symbol: RirSymbol::new("apply"),
            kind: RirExternKind::Native(rir::RirNativeExtern::new(
                vec!["host".to_string(), "apply".to_string()],
                anvyx_runtime::RustExternAbi {
                    params: vec![anvyx_runtime::RustParamAbi::ScopedLambda(
                        native_callback_sig(),
                    )],
                    ret: anvyx_runtime::RustReturnAbi::Void,
                    fallible: false,
                    support: anvyx_runtime::RustAbiSupport::NeedsWrapperConversion,
                    ctx: anvyx_runtime::RustWrapperCtx::None,
                },
            )),
            params: vec![RirExternParam {
                ty: lambda_ty,
                semantic: RirParamSemantic::ScopedLambda,
                abi: RirParamAbi::ScopedLambda,
                escape: RirParamEscape::NonEscaping,
            }],
            ret: void,
            abi: rir_abi(
                vec![anvyx_runtime::ExternTypeExpr::Callback(
                    native_callback_sig(),
                )],
                anvyx_runtime::ExternTypeExpr::Void,
            ),
        }],
        functions: vec![RirFunction {
            id: RirFunctionId::from_index(0),
            air_id: None,
            symbol: RirSymbol::new("main"),
            params: vec![],
            ret: RirReturn { ty: void },
            locals: vec![RirLocal {
                id: local,
                ty: lambda_ty,
                mutable: false,
                symbol: RirSymbol::new("f"),
                initialized: true,
                payload_ref: false,
            }],
            body: RirStructuredBlock {
                stmts: vec![RirStmt::Eval(RirRValue::Call {
                    callee: RirCallTarget::Extern(RirExternId::from_index(0)),
                    args: vec![RirCallArg::ScopedLambda {
                        callee: RirOperand::Place(rir_place(local, lambda_ty)),
                        sig,
                    }],
                    ty: void,
                })],
                term: RirTerm::Return(None),
            },
        }],
        entry: Some(RirFunctionId::from_index(0)),
        ..RirProgram::default()
    }
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

fn nested_scoped_borrow_lambda_program() -> Program {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let void = program.alloc_type(TypeData::Void);
    let module = program.alloc_module(root_module());
    let callee = source_ref_callee(&mut program, module, int, void);
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
            vec![source_ref_call(
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

fn heap_capture_cell_source_ref_arg_program() -> Program {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let void = program.alloc_type(TypeData::Void);
    let sig = air::SignatureType::new(vec![], air::ReturnMode::Value(void));
    let lambda_ty = program.alloc_type(TypeData::Function(sig.clone()));
    let module = program.alloc_module(root_module());
    let callee = source_ref_callee(&mut program, module, int, void);
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
                source_ref_call(callee, PlaceRoot::CaptureCell(cell), int),
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

fn capture_cell_projected_assignment_program(in_lambda: bool, escape: LambdaEscape) -> Program {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let tuple = program.alloc_type(TypeData::Tuple(vec![int]));
    let void = program.alloc_type(TypeData::Void);
    let module = program.alloc_module(root_module());
    let binding = BindingId::from_index(0);
    let source_local = air::LocalId::from_index(0);

    if !in_lambda {
        let owner = FunctionId::from_index(0);
        let cell = capture_cell(&mut program, owner, source_local, binding, tuple);
        let init = init_tuple_cell(&mut program, cell, tuple, int);
        let one = int_const(&mut program, int, 1);
        let caller = program.alloc_function(Function {
            name: Ident::new("main"),
            module,
            kind: FunctionKind::Normal,
            owner: None,
            specialization: None,
            signature: Signature::new(vec![], void),
            locals: vec![bound_source_local(binding, tuple)],
            body: structured_body(
                vec![
                    init,
                    Statement::Assign {
                        dst: projected_tuple_place(PlaceRoot::CaptureCell(cell), int),
                        value: RValue::Use(Operand::Const(one)),
                    },
                ],
                air::AirTail::Return(None),
            ),
        });
        assert_eq!(caller, owner);
        program.module_mut(module).functions.push(caller);
        program.entry = Some(caller);
        return program;
    }

    let body = FunctionId::from_index(0);
    let owner = FunctionId::from_index(1);
    let lambda = air::LambdaId::from_index(0);
    let cell = capture_cell(&mut program, owner, source_local, binding, tuple);
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
                ty: tuple,
            }],
        }),
        lambda
    );
    let one = int_const(&mut program, int, 1);
    let lambda_body = program.alloc_function(Function {
        name: Ident::new("lambda"),
        module,
        kind: FunctionKind::Lambda(lambda),
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], void),
        locals: vec![],
        body: structured_body(
            vec![Statement::Assign {
                dst: projected_tuple_place(
                    PlaceRoot::LambdaCapture(air::LambdaCaptureSlotId::from_index(0)),
                    int,
                ),
                value: RValue::Use(Operand::Const(one)),
            }],
            air::AirTail::Return(None),
        ),
    });
    assert_eq!(lambda_body, body);
    let init = init_tuple_cell(&mut program, cell, tuple, int);
    let caller = program.alloc_function(Function {
        name: Ident::new("main"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], void),
        locals: vec![
            bound_source_local(binding, tuple),
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
        .extend([lambda_body, caller]);
    program.entry = Some(caller);
    program
}

fn owner_capture_cell_source_ref_arg_program() -> Program {
    capture_cell_source_ref_arg_program(false, LambdaEscape::NonEscaping)
}

fn lambda_capture_cell_source_ref_arg_program() -> Program {
    capture_cell_source_ref_arg_program(true, LambdaEscape::NonEscaping)
}

fn escaping_lambda_capture_cell_source_ref_arg_program() -> Program {
    capture_cell_source_ref_arg_program(true, LambdaEscape::Escaping)
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

fn heap_capture_cell_reentrant_source_ref_arg_program() -> Program {
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

fn init_tuple_cell(
    program: &mut Program,
    cell: air::CaptureCellId,
    tuple: air::TypeId,
    int: air::TypeId,
) -> Statement {
    Statement::Assign {
        dst: root_place(PlaceRoot::CaptureCell(cell), tuple),
        value: RValue::Aggregate {
            kind: AggregateCtor::Tuple,
            fields: vec![Operand::Const(int_const(program, int, 0))],
            ty: tuple,
        },
    }
}

fn projected_tuple_place(root: PlaceRoot, ty: air::TypeId) -> Place {
    Place {
        root,
        projection: vec![Projection::TupleField(0)],
        ty,
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

fn collection_loan_stmt(root: Place, root_kind: air::AirCollectionRootKind) -> Statement {
    let mode = match root_kind {
        air::AirCollectionRootKind::Map => air::AirCollectionLoanMode::ReadonlyMap,
        air::AirCollectionRootKind::List
        | air::AirCollectionRootKind::FixedArray
        | air::AirCollectionRootKind::Slice => air::AirCollectionLoanMode::ReadonlySequence,
    };
    Statement::CollectionLoan(air::AirCollectionLoan {
        root,
        root_kind,
        mode,
        body: air::AirBlock::default(),
    })
}

fn init_collection_cell(
    program: &mut Program,
    cell: air::CaptureCellId,
    ty: air::TypeId,
    int: air::TypeId,
) -> Statement {
    let fields = match program.type_arena.data(ty) {
        TypeData::Map { .. } => vec![
            Operand::Const(int_const(program, int, 0)),
            Operand::Const(int_const(program, int, 1)),
        ],
        _ => vec![Operand::Const(int_const(program, int, 0))],
    };
    Statement::Assign {
        dst: root_place(PlaceRoot::CaptureCell(cell), ty),
        value: RValue::Aggregate {
            kind: match program.type_arena.data(ty) {
                TypeData::Map { .. } => AggregateCtor::Map,
                _ => AggregateCtor::List,
            },
            fields,
            ty,
        },
    }
}

fn planned_collection_loan_root(program: Program, air_id: FunctionId) -> RirCollectionAccess {
    let verified = air::verify(&program).expect("AIR verify failed");
    let plan = plan(&verified, rust_plan_config()).expect("plan failed");
    let function = rir_function_for_air(plan.program(), air_id);
    let root = function
        .body
        .stmts
        .iter()
        .find_map(|stmt| match stmt {
            RirStmt::CollectionLoanScope(scope) => Some(scope.root.clone()),
            _ => None,
        })
        .expect("missing collection loan scope");
    std::hint::black_box(program);
    root
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

fn host_escaping_lambda_plan_config() -> RustPlanConfig {
    RustPlanConfig {
        symbol_prefix: "anv".into(),
        native_providers: vec![host_escaping_lambda_support()],
    }
}

fn host_escaping_lambda_retained_plan_config() -> RustPlanConfig {
    RustPlanConfig {
        symbol_prefix: "anv".into(),
        native_providers: vec![host_escaping_lambda_retained_support()],
    }
}

fn host_scoped_and_subscription_plan_config() -> RustPlanConfig {
    RustPlanConfig {
        symbol_prefix: "anv".into(),
        native_providers: vec![host_lambda_support(), host_subscription_support()],
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

fn check(program: Program) {
    let verified = air::verify(&program).expect("AIR verify failed");
    let config = rust_plan_config();
    RustBackendProfile::check_with_native_support(&verified, &config.native_providers)
        .expect("profile rejected AIR");
    std::hint::black_box(program);
}

fn assert_plan_gap(program: Program, config: RustPlanConfig, kind: RustTargetGapKind) {
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

fn profile_errors(program: Program) -> Vec<RustBackendProfileError> {
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

fn assert_profile_error(
    errors: &[RustBackendProfileError],
    site: ProfileSite,
    kind: ProfileErrorKind,
) {
    assert!(
        errors
            .iter()
            .any(|error| error.site == site && error.kind == kind),
        "missing profile error {kind:?} at {site:?}: {errors:?}"
    );
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

fn runtime_module() -> air::Module {
    air_module(&["core_runtime"])
}

fn air_module(path: &[&str]) -> air::Module {
    air::Module {
        path: path.iter().map(|segment| Ident::new(*segment)).collect(),
        ..air::Module::default()
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

fn rir_abi(
    params: Vec<anvyx_runtime::ExternTypeExpr>,
    ret: anvyx_runtime::ExternTypeExpr,
) -> air::ExternAbi {
    air::ExternAbi { params, ret }
}

fn direct_rust_abi(
    params: Vec<anvyx_runtime::RustParamAbi>,
    ret: anvyx_runtime::RustReturnAbi,
) -> anvyx_runtime::RustExternAbi {
    anvyx_runtime::RustExternAbi {
        params,
        ret,
        fallible: false,
        support: anvyx_runtime::RustAbiSupport::Direct,
        ctx: anvyx_runtime::RustWrapperCtx::HiddenRuntime,
    }
}

fn native_extern_rir(
    types: Vec<RirType>,
    params: Vec<RirExternParam>,
    ret: RirTypeId,
    rust_abi: anvyx_runtime::RustExternAbi,
    abi: air::ExternAbi,
) -> RirProgram {
    RirProgram {
        types,
        externs: vec![RirExtern {
            id: RirExternId::from_index(0),
            air_id: None,
            symbol: RirSymbol::new("native"),
            kind: RirExternKind::Native(rir::RirNativeExtern::new(
                vec!["host".to_string(), "native".to_string()],
                rust_abi,
            )),
            params,
            ret,
            abi,
        }],
        ..RirProgram::default()
    }
}

fn native_resource_return_program(owned: bool) -> RirProgram {
    let resource = RirTypeId::from_index(0);
    let resource_abi = anvyx_runtime::ExternTypeExpr::Named {
        module: None,
        name: "Window".to_string(),
        args: vec![],
    };
    let ret_abi = if owned {
        anvyx_runtime::RustReturnAbi::OwnedNamed(resource_abi.clone())
    } else {
        anvyx_runtime::RustReturnAbi::Value(resource_abi.clone())
    };
    let mut program = native_extern_rir(
        vec![RirType::Struct(RirStructId::from_index(0))],
        vec![],
        resource,
        direct_rust_abi(vec![], ret_abi),
        rir_abi(vec![], resource_abi),
    );
    program.structs.push(RirStruct {
        id: RirStructId::from_index(0),
        air_id: None,
        symbol: RirSymbol::new("Window"),
        display: RirSymbol::new("Window"),
        native_path: Some(vec!["host".to_string(), "Window".to_string()]),
        native_ref: true,
        native_key: Some(anvyx_runtime::ExternTypeKey {
            module: anvyx_runtime::ModulePath { segments: vec![] },
            name: "Window".to_string(),
        }),
        copyable: false,
        fields: vec![],
    });
    program.functions.push(return_extern_function(resource));
    program.entry = Some(RirFunctionId::from_index(0));
    program
}

fn return_extern_function(ret: RirTypeId) -> RirFunction {
    RirFunction {
        id: RirFunctionId::from_index(0),
        air_id: None,
        symbol: RirSymbol::new("f"),
        params: vec![],
        ret: RirReturn { ty: ret },
        locals: vec![RirLocal {
            id: RirLocalId::from_index(0),
            ty: ret,
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
                    ty: ret,
                },
            }],
            term: RirTerm::Return(Some(RirOperand::Place(RirPlace::local(
                RirLocalId::from_index(0),
                vec![],
                ret,
            )))),
        },
    }
}

fn call_extern_function(ret: RirTypeId, args: Vec<RirCallArg>) -> RirFunction {
    RirFunction {
        id: RirFunctionId::from_index(0),
        air_id: None,
        symbol: RirSymbol::new("f"),
        params: vec![],
        ret: RirReturn { ty: ret },
        locals: vec![],
        body: RirStructuredBlock {
            stmts: vec![RirStmt::Eval(RirRValue::Call {
                callee: RirCallTarget::Extern(RirExternId::from_index(0)),
                args,
                ty: ret,
            })],
            term: RirTerm::Return(None),
        },
    }
}

fn native_named_abi(name: &str) -> anvyx_runtime::ExternTypeExpr {
    anvyx_runtime::ExternTypeExpr::Named {
        module: Some(anvyx_runtime::ModulePath {
            segments: vec!["host".to_string()],
        }),
        name: name.to_string(),
        args: vec![],
    }
}

fn native_type_key(name: &str) -> anvyx_runtime::ExternTypeKey {
    anvyx_runtime::ExternTypeKey {
        module: anvyx_runtime::ModulePath {
            segments: vec!["host".to_string()],
        },
        name: name.to_string(),
    }
}

fn rir_empty_tuple() -> RirTuple {
    RirTuple {
        id: RirTupleId::from_index(0),
        symbol: RirSymbol::new("UnitTuple"),
        display: RirSymbol::new("Unit"),
        copyable: true,
        fields: vec![],
    }
}

fn rir_result_enum(ok: RirTypeId, err: RirTypeId) -> RirEnum {
    RirEnum {
        id: RirEnumId::from_index(0),
        air_id: None,
        native_path: None,
        native_key: None,
        core: Some(RirCoreEnumKind::Result),
        repr: rir::RirEnumRepr::Adt,
        raw_type: None,
        symbol: RirSymbol::new("Result"),
        display: RirSymbol::new("Result"),
        copyable: false,
        variants: vec![
            rir_tuple_variant(0, "Ok", ok),
            rir_tuple_variant(1, "Err", err),
        ],
    }
}

fn rir_native_enum(name: &str) -> RirEnum {
    RirEnum {
        id: RirEnumId::from_index(0),
        air_id: None,
        native_path: None,
        native_key: Some(native_type_key(name)),
        core: None,
        repr: rir::RirEnumRepr::Adt,
        raw_type: None,
        symbol: RirSymbol::new(name),
        display: RirSymbol::new(name),
        copyable: true,
        variants: vec![],
    }
}

fn rir_tuple_variant(index: usize, name: &str, ty: RirTypeId) -> RirVariant {
    RirVariant {
        id: RirVariantId::from_index(index),
        symbol: RirSymbol::new(name),
        display: RirSymbol::new(name),
        kind: RirVariantKind::Tuple,
        raw_value: None,
        fields: vec![RirField {
            id: RirFieldId::from_index(0),
            symbol: RirSymbol::new("f0"),
            ty,
        }],
    }
}

fn rir_native_dataref(name: &str) -> RirDataRef {
    RirDataRef {
        id: RirDataRefId::from_index(0),
        air_id: air::AggregateId::from_index(0),
        native_key: Some(native_type_key(name)),
        symbol: RirSymbol::new(name),
        display: RirSymbol::new(name),
        cycle_capable: false,
        fields: vec![],
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
        TypeData::Any => ExternTypeExpr::Any,
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
        TypeData::Aggregate(_) | TypeData::Enum(_) | TypeData::DataRef(_) | TypeData::Dyn(_) => {
            ExternTypeExpr::Any
        }
    }
}
